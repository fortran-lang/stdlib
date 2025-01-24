submodule(stdlib_lapack_eig_svd_lsq) stdlib_lapack_svd_comp
  implicit none


  contains

     pure module subroutine stdlib_sgebrd( m, n, a, lda, d, e, tauq, taup, work, lwork,info )
     !! SGEBRD reduces a general real M-by-N matrix A to upper or lower
     !! bidiagonal form B by an orthogonal transformation: Q**T * A * P = B.
     !! If m >= n, B is upper bidiagonal; if m < n, B is lower bidiagonal.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, m, n
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: d(*), e(*), taup(*), tauq(*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: i, iinfo, j, ldwrkx, ldwrky, lwkopt, minmn, nb, nbmin, nx, ws
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp
           nb = max( 1_ilp, stdlib_ilaenv( 1_ilp, 'SGEBRD', ' ', m, n, -1_ilp, -1_ilp ) )
           lwkopt = ( m+n )*nb
           work( 1_ilp ) = real( lwkopt,KIND=sp)
           lquery = ( lwork==-1_ilp )
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -4_ilp
           else if( lwork<max( 1_ilp, m, n ) .and. .not.lquery ) then
              info = -10_ilp
           end if
           if( info<0_ilp ) then
              call stdlib_xerbla( 'SGEBRD', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           minmn = min( m, n )
           if( minmn==0_ilp ) then
              work( 1_ilp ) = 1_ilp
              return
           end if
           ws = max( m, n )
           ldwrkx = m
           ldwrky = n
           if( nb>1_ilp .and. nb<minmn ) then
              ! set the crossover point nx.
              nx = max( nb, stdlib_ilaenv( 3_ilp, 'SGEBRD', ' ', m, n, -1_ilp, -1_ilp ) )
              ! determine when to switch from blocked to unblocked code.
              if( nx<minmn ) then
                 ws = ( m+n )*nb
                 if( lwork<ws ) then
                    ! not enough work space for the optimal nb, consider using
                    ! a smaller block size.
                    nbmin = stdlib_ilaenv( 2_ilp, 'SGEBRD', ' ', m, n, -1_ilp, -1_ilp )
                    if( lwork>=( m+n )*nbmin ) then
                       nb = lwork / ( m+n )
                    else
                       nb = 1_ilp
                       nx = minmn
                    end if
                 end if
              end if
           else
              nx = minmn
           end if
           do i = 1, minmn - nx, nb
              ! reduce rows and columns i:i+nb-1 to bidiagonal form and return
              ! the matrices x and y which are needed to update the unreduced
              ! part of the matrix
              call stdlib_slabrd( m-i+1, n-i+1, nb, a( i, i ), lda, d( i ), e( i ),tauq( i ), &
                        taup( i ), work, ldwrkx,work( ldwrkx*nb+1 ), ldwrky )
              ! update the trailing submatrix a(i+nb:m,i+nb:n), using an update
              ! of the form  a := a - v*y**t - x*u**t
              call stdlib_sgemm( 'NO TRANSPOSE', 'TRANSPOSE', m-i-nb+1, n-i-nb+1,nb, -one, a( i+&
                        nb, i ), lda,work( ldwrkx*nb+nb+1 ), ldwrky, one,a( i+nb, i+nb ), lda )
              call stdlib_sgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', m-i-nb+1, n-i-nb+1,nb, -one, &
                        work( nb+1 ), ldwrkx, a( i, i+nb ), lda,one, a( i+nb, i+nb ), lda )
              ! copy diagonal and off-diagonal elements of b back into a
              if( m>=n ) then
                 do j = i, i + nb - 1
                    a( j, j ) = d( j )
                    a( j, j+1 ) = e( j )
                 end do
              else
                 do j = i, i + nb - 1
                    a( j, j ) = d( j )
                    a( j+1, j ) = e( j )
                 end do
              end if
           end do
           ! use unblocked code to reduce the remainder of the matrix
           call stdlib_sgebd2( m-i+1, n-i+1, a( i, i ), lda, d( i ), e( i ),tauq( i ), taup( i ), &
                     work, iinfo )
           work( 1_ilp ) = ws
           return
     end subroutine stdlib_sgebrd

     pure module subroutine stdlib_dgebrd( m, n, a, lda, d, e, tauq, taup, work, lwork,info )
     !! DGEBRD reduces a general real M-by-N matrix A to upper or lower
     !! bidiagonal form B by an orthogonal transformation: Q**T * A * P = B.
     !! If m >= n, B is upper bidiagonal; if m < n, B is lower bidiagonal.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, m, n
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: d(*), e(*), taup(*), tauq(*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: i, iinfo, j, ldwrkx, ldwrky, lwkopt, minmn, nb, nbmin, nx, ws
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp
           nb = max( 1_ilp, stdlib_ilaenv( 1_ilp, 'DGEBRD', ' ', m, n, -1_ilp, -1_ilp ) )
           lwkopt = ( m+n )*nb
           work( 1_ilp ) = real( lwkopt,KIND=dp)
           lquery = ( lwork==-1_ilp )
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -4_ilp
           else if( lwork<max( 1_ilp, m, n ) .and. .not.lquery ) then
              info = -10_ilp
           end if
           if( info<0_ilp ) then
              call stdlib_xerbla( 'DGEBRD', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           minmn = min( m, n )
           if( minmn==0_ilp ) then
              work( 1_ilp ) = 1_ilp
              return
           end if
           ws = max( m, n )
           ldwrkx = m
           ldwrky = n
           if( nb>1_ilp .and. nb<minmn ) then
              ! set the crossover point nx.
              nx = max( nb, stdlib_ilaenv( 3_ilp, 'DGEBRD', ' ', m, n, -1_ilp, -1_ilp ) )
              ! determine when to switch from blocked to unblocked code.
              if( nx<minmn ) then
                 ws = ( m+n )*nb
                 if( lwork<ws ) then
                    ! not enough work space for the optimal nb, consider using
                    ! a smaller block size.
                    nbmin = stdlib_ilaenv( 2_ilp, 'DGEBRD', ' ', m, n, -1_ilp, -1_ilp )
                    if( lwork>=( m+n )*nbmin ) then
                       nb = lwork / ( m+n )
                    else
                       nb = 1_ilp
                       nx = minmn
                    end if
                 end if
              end if
           else
              nx = minmn
           end if
           do i = 1, minmn - nx, nb
              ! reduce rows and columns i:i+nb-1 to bidiagonal form and return
              ! the matrices x and y which are needed to update the unreduced
              ! part of the matrix
              call stdlib_dlabrd( m-i+1, n-i+1, nb, a( i, i ), lda, d( i ), e( i ),tauq( i ), &
                        taup( i ), work, ldwrkx,work( ldwrkx*nb+1 ), ldwrky )
              ! update the trailing submatrix a(i+nb:m,i+nb:n), using an update
              ! of the form  a := a - v*y**t - x*u**t
              call stdlib_dgemm( 'NO TRANSPOSE', 'TRANSPOSE', m-i-nb+1, n-i-nb+1,nb, -one, a( i+&
                        nb, i ), lda,work( ldwrkx*nb+nb+1 ), ldwrky, one,a( i+nb, i+nb ), lda )
              call stdlib_dgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', m-i-nb+1, n-i-nb+1,nb, -one, &
                        work( nb+1 ), ldwrkx, a( i, i+nb ), lda,one, a( i+nb, i+nb ), lda )
              ! copy diagonal and off-diagonal elements of b back into a
              if( m>=n ) then
                 do j = i, i + nb - 1
                    a( j, j ) = d( j )
                    a( j, j+1 ) = e( j )
                 end do
              else
                 do j = i, i + nb - 1
                    a( j, j ) = d( j )
                    a( j+1, j ) = e( j )
                 end do
              end if
           end do
           ! use unblocked code to reduce the remainder of the matrix
           call stdlib_dgebd2( m-i+1, n-i+1, a( i, i ), lda, d( i ), e( i ),tauq( i ), taup( i ), &
                     work, iinfo )
           work( 1_ilp ) = ws
           return
     end subroutine stdlib_dgebrd


     pure module subroutine stdlib_cgebrd( m, n, a, lda, d, e, tauq, taup, work, lwork,info )
     !! CGEBRD reduces a general complex M-by-N matrix A to upper or lower
     !! bidiagonal form B by a unitary transformation: Q**H * A * P = B.
     !! If m >= n, B is upper bidiagonal; if m < n, B is lower bidiagonal.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, m, n
           ! Array Arguments 
           real(sp), intent(out) :: d(*), e(*)
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: taup(*), tauq(*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: i, iinfo, j, ldwrkx, ldwrky, lwkopt, minmn, nb, nbmin, nx, ws
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp
           nb = max( 1_ilp, stdlib_ilaenv( 1_ilp, 'CGEBRD', ' ', m, n, -1_ilp, -1_ilp ) )
           lwkopt = ( m+n )*nb
           work( 1_ilp ) = real( lwkopt,KIND=sp)
           lquery = ( lwork==-1_ilp )
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -4_ilp
           else if( lwork<max( 1_ilp, m, n ) .and. .not.lquery ) then
              info = -10_ilp
           end if
           if( info<0_ilp ) then
              call stdlib_xerbla( 'CGEBRD', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           minmn = min( m, n )
           if( minmn==0_ilp ) then
              work( 1_ilp ) = 1_ilp
              return
           end if
           ws = max( m, n )
           ldwrkx = m
           ldwrky = n
           if( nb>1_ilp .and. nb<minmn ) then
              ! set the crossover point nx.
              nx = max( nb, stdlib_ilaenv( 3_ilp, 'CGEBRD', ' ', m, n, -1_ilp, -1_ilp ) )
              ! determine when to switch from blocked to unblocked code.
              if( nx<minmn ) then
                 ws = ( m+n )*nb
                 if( lwork<ws ) then
                    ! not enough work space for the optimal nb, consider using
                    ! a smaller block size.
                    nbmin = stdlib_ilaenv( 2_ilp, 'CGEBRD', ' ', m, n, -1_ilp, -1_ilp )
                    if( lwork>=( m+n )*nbmin ) then
                       nb = lwork / ( m+n )
                    else
                       nb = 1_ilp
                       nx = minmn
                    end if
                 end if
              end if
           else
              nx = minmn
           end if
           do i = 1, minmn - nx, nb
              ! reduce rows and columns i:i+ib-1 to bidiagonal form and return
              ! the matrices x and y which are needed to update the unreduced
              ! part of the matrix
              call stdlib_clabrd( m-i+1, n-i+1, nb, a( i, i ), lda, d( i ), e( i ),tauq( i ), &
                        taup( i ), work, ldwrkx,work( ldwrkx*nb+1 ), ldwrky )
              ! update the trailing submatrix a(i+ib:m,i+ib:n), using
              ! an update of the form  a := a - v*y**h - x*u**h
              call stdlib_cgemm( 'NO TRANSPOSE', 'CONJUGATE TRANSPOSE', m-i-nb+1,n-i-nb+1, nb, -&
              cone, a( i+nb, i ), lda,work( ldwrkx*nb+nb+1 ), ldwrky, cone,a( i+nb, i+nb ), lda )
                        
              call stdlib_cgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', m-i-nb+1, n-i-nb+1,nb, -cone, &
                        work( nb+1 ), ldwrkx, a( i, i+nb ), lda,cone, a( i+nb, i+nb ), lda )
              ! copy diagonal and off-diagonal elements of b back into a
              if( m>=n ) then
                 do j = i, i + nb - 1
                    a( j, j ) = d( j )
                    a( j, j+1 ) = e( j )
                 end do
              else
                 do j = i, i + nb - 1
                    a( j, j ) = d( j )
                    a( j+1, j ) = e( j )
                 end do
              end if
           end do
           ! use unblocked code to reduce the remainder of the matrix
           call stdlib_cgebd2( m-i+1, n-i+1, a( i, i ), lda, d( i ), e( i ),tauq( i ), taup( i ), &
                     work, iinfo )
           work( 1_ilp ) = ws
           return
     end subroutine stdlib_cgebrd

     pure module subroutine stdlib_zgebrd( m, n, a, lda, d, e, tauq, taup, work, lwork,info )
     !! ZGEBRD reduces a general complex M-by-N matrix A to upper or lower
     !! bidiagonal form B by a unitary transformation: Q**H * A * P = B.
     !! If m >= n, B is upper bidiagonal; if m < n, B is lower bidiagonal.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, m, n
           ! Array Arguments 
           real(dp), intent(out) :: d(*), e(*)
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: taup(*), tauq(*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: i, iinfo, j, ldwrkx, ldwrky, lwkopt, minmn, nb, nbmin, nx, ws
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp
           nb = max( 1_ilp, stdlib_ilaenv( 1_ilp, 'ZGEBRD', ' ', m, n, -1_ilp, -1_ilp ) )
           lwkopt = ( m+n )*nb
           work( 1_ilp ) = real( lwkopt,KIND=dp)
           lquery = ( lwork==-1_ilp )
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -4_ilp
           else if( lwork<max( 1_ilp, m, n ) .and. .not.lquery ) then
              info = -10_ilp
           end if
           if( info<0_ilp ) then
              call stdlib_xerbla( 'ZGEBRD', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           minmn = min( m, n )
           if( minmn==0_ilp ) then
              work( 1_ilp ) = 1_ilp
              return
           end if
           ws = max( m, n )
           ldwrkx = m
           ldwrky = n
           if( nb>1_ilp .and. nb<minmn ) then
              ! set the crossover point nx.
              nx = max( nb, stdlib_ilaenv( 3_ilp, 'ZGEBRD', ' ', m, n, -1_ilp, -1_ilp ) )
              ! determine when to switch from blocked to unblocked code.
              if( nx<minmn ) then
                 ws = ( m+n )*nb
                 if( lwork<ws ) then
                    ! not enough work space for the optimal nb, consider using
                    ! a smaller block size.
                    nbmin = stdlib_ilaenv( 2_ilp, 'ZGEBRD', ' ', m, n, -1_ilp, -1_ilp )
                    if( lwork>=( m+n )*nbmin ) then
                       nb = lwork / ( m+n )
                    else
                       nb = 1_ilp
                       nx = minmn
                    end if
                 end if
              end if
           else
              nx = minmn
           end if
           do i = 1, minmn - nx, nb
              ! reduce rows and columns i:i+ib-1 to bidiagonal form and return
              ! the matrices x and y which are needed to update the unreduced
              ! part of the matrix
              call stdlib_zlabrd( m-i+1, n-i+1, nb, a( i, i ), lda, d( i ), e( i ),tauq( i ), &
                        taup( i ), work, ldwrkx,work( ldwrkx*nb+1 ), ldwrky )
              ! update the trailing submatrix a(i+ib:m,i+ib:n), using
              ! an update of the form  a := a - v*y**h - x*u**h
              call stdlib_zgemm( 'NO TRANSPOSE', 'CONJUGATE TRANSPOSE', m-i-nb+1,n-i-nb+1, nb, -&
              cone, a( i+nb, i ), lda,work( ldwrkx*nb+nb+1 ), ldwrky, cone,a( i+nb, i+nb ), lda )
                        
              call stdlib_zgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', m-i-nb+1, n-i-nb+1,nb, -cone, &
                        work( nb+1 ), ldwrkx, a( i, i+nb ), lda,cone, a( i+nb, i+nb ), lda )
              ! copy diagonal and off-diagonal elements of b back into a
              if( m>=n ) then
                 do j = i, i + nb - 1
                    a( j, j ) = d( j )
                    a( j, j+1 ) = e( j )
                 end do
              else
                 do j = i, i + nb - 1
                    a( j, j ) = d( j )
                    a( j+1, j ) = e( j )
                 end do
              end if
           end do
           ! use unblocked code to reduce the remainder of the matrix
           call stdlib_zgebd2( m-i+1, n-i+1, a( i, i ), lda, d( i ), e( i ),tauq( i ), taup( i ), &
                     work, iinfo )
           work( 1_ilp ) = ws
           return
     end subroutine stdlib_zgebrd




     pure module subroutine stdlib_sgebd2( m, n, a, lda, d, e, tauq, taup, work, info )
     !! SGEBD2 reduces a real general m by n matrix A to upper or lower
     !! bidiagonal form B by an orthogonal transformation: Q**T * A * P = B.
     !! If m >= n, B is upper bidiagonal; if m < n, B is lower bidiagonal.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: d(*), e(*), taup(*), tauq(*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i
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
           if( info<0_ilp ) then
              call stdlib_xerbla( 'SGEBD2', -info )
              return
           end if
           if( m>=n ) then
              ! reduce to upper bidiagonal form
              do i = 1, n
                 ! generate elementary reflector h(i) to annihilate a(i+1:m,i)
                 call stdlib_slarfg( m-i+1, a( i, i ), a( min( i+1, m ), i ), 1_ilp,tauq( i ) )
                           
                 d( i ) = a( i, i )
                 a( i, i ) = one
                 ! apply h(i) to a(i:m,i+1:n) from the left
                 if( i<n )call stdlib_slarf( 'LEFT', m-i+1, n-i, a( i, i ), 1_ilp, tauq( i ),a( i, i+&
                           1_ilp ), lda, work )
                 a( i, i ) = d( i )
                 if( i<n ) then
                    ! generate elementary reflector g(i) to annihilate
                    ! a(i,i+2:n)
                    call stdlib_slarfg( n-i, a( i, i+1 ), a( i, min( i+2, n ) ),lda, taup( i ) )
                              
                    e( i ) = a( i, i+1 )
                    a( i, i+1 ) = one
                    ! apply g(i) to a(i+1:m,i+1:n) from the right
                    call stdlib_slarf( 'RIGHT', m-i, n-i, a( i, i+1 ), lda,taup( i ), a( i+1, i+1 &
                              ), lda, work )
                    a( i, i+1 ) = e( i )
                 else
                    taup( i ) = zero
                 end if
              end do
           else
              ! reduce to lower bidiagonal form
              do i = 1, m
                 ! generate elementary reflector g(i) to annihilate a(i,i+1:n)
                 call stdlib_slarfg( n-i+1, a( i, i ), a( i, min( i+1, n ) ), lda,taup( i ) )
                           
                 d( i ) = a( i, i )
                 a( i, i ) = one
                 ! apply g(i) to a(i+1:m,i:n) from the right
                 if( i<m )call stdlib_slarf( 'RIGHT', m-i, n-i+1, a( i, i ), lda,taup( i ), a( i+&
                           1_ilp, i ), lda, work )
                 a( i, i ) = d( i )
                 if( i<m ) then
                    ! generate elementary reflector h(i) to annihilate
                    ! a(i+2:m,i)
                    call stdlib_slarfg( m-i, a( i+1, i ), a( min( i+2, m ), i ), 1_ilp,tauq( i ) )
                              
                    e( i ) = a( i+1, i )
                    a( i+1, i ) = one
                    ! apply h(i) to a(i+1:m,i+1:n) from the left
                    call stdlib_slarf( 'LEFT', m-i, n-i, a( i+1, i ), 1_ilp, tauq( i ),a( i+1, i+1 ), &
                              lda, work )
                    a( i+1, i ) = e( i )
                 else
                    tauq( i ) = zero
                 end if
              end do
           end if
           return
     end subroutine stdlib_sgebd2

     pure module subroutine stdlib_dgebd2( m, n, a, lda, d, e, tauq, taup, work, info )
     !! DGEBD2 reduces a real general m by n matrix A to upper or lower
     !! bidiagonal form B by an orthogonal transformation: Q**T * A * P = B.
     !! If m >= n, B is upper bidiagonal; if m < n, B is lower bidiagonal.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: d(*), e(*), taup(*), tauq(*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i
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
           if( info<0_ilp ) then
              call stdlib_xerbla( 'DGEBD2', -info )
              return
           end if
           if( m>=n ) then
              ! reduce to upper bidiagonal form
              do i = 1, n
                 ! generate elementary reflector h(i) to annihilate a(i+1:m,i)
                 call stdlib_dlarfg( m-i+1, a( i, i ), a( min( i+1, m ), i ), 1_ilp,tauq( i ) )
                           
                 d( i ) = a( i, i )
                 a( i, i ) = one
                 ! apply h(i) to a(i:m,i+1:n) from the left
                 if( i<n )call stdlib_dlarf( 'LEFT', m-i+1, n-i, a( i, i ), 1_ilp, tauq( i ),a( i, i+&
                           1_ilp ), lda, work )
                 a( i, i ) = d( i )
                 if( i<n ) then
                    ! generate elementary reflector g(i) to annihilate
                    ! a(i,i+2:n)
                    call stdlib_dlarfg( n-i, a( i, i+1 ), a( i, min( i+2, n ) ),lda, taup( i ) )
                              
                    e( i ) = a( i, i+1 )
                    a( i, i+1 ) = one
                    ! apply g(i) to a(i+1:m,i+1:n) from the right
                    call stdlib_dlarf( 'RIGHT', m-i, n-i, a( i, i+1 ), lda,taup( i ), a( i+1, i+1 &
                              ), lda, work )
                    a( i, i+1 ) = e( i )
                 else
                    taup( i ) = zero
                 end if
              end do
           else
              ! reduce to lower bidiagonal form
              do i = 1, m
                 ! generate elementary reflector g(i) to annihilate a(i,i+1:n)
                 call stdlib_dlarfg( n-i+1, a( i, i ), a( i, min( i+1, n ) ), lda,taup( i ) )
                           
                 d( i ) = a( i, i )
                 a( i, i ) = one
                 ! apply g(i) to a(i+1:m,i:n) from the right
                 if( i<m )call stdlib_dlarf( 'RIGHT', m-i, n-i+1, a( i, i ), lda,taup( i ), a( i+&
                           1_ilp, i ), lda, work )
                 a( i, i ) = d( i )
                 if( i<m ) then
                    ! generate elementary reflector h(i) to annihilate
                    ! a(i+2:m,i)
                    call stdlib_dlarfg( m-i, a( i+1, i ), a( min( i+2, m ), i ), 1_ilp,tauq( i ) )
                              
                    e( i ) = a( i+1, i )
                    a( i+1, i ) = one
                    ! apply h(i) to a(i+1:m,i+1:n) from the left
                    call stdlib_dlarf( 'LEFT', m-i, n-i, a( i+1, i ), 1_ilp, tauq( i ),a( i+1, i+1 ), &
                              lda, work )
                    a( i+1, i ) = e( i )
                 else
                    tauq( i ) = zero
                 end if
              end do
           end if
           return
     end subroutine stdlib_dgebd2


     pure module subroutine stdlib_cgebd2( m, n, a, lda, d, e, tauq, taup, work, info )
     !! CGEBD2 reduces a complex general m by n matrix A to upper or lower
     !! real bidiagonal form B by a unitary transformation: Q**H * A * P = B.
     !! If m >= n, B is upper bidiagonal; if m < n, B is lower bidiagonal.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n
           ! Array Arguments 
           real(sp), intent(out) :: d(*), e(*)
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: taup(*), tauq(*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i
           complex(sp) :: alpha
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
           if( info<0_ilp ) then
              call stdlib_xerbla( 'CGEBD2', -info )
              return
           end if
           if( m>=n ) then
              ! reduce to upper bidiagonal form
              do i = 1, n
                 ! generate elementary reflector h(i) to annihilate a(i+1:m,i)
                 alpha = a( i, i )
                 call stdlib_clarfg( m-i+1, alpha, a( min( i+1, m ), i ), 1_ilp,tauq( i ) )
                 d( i ) = real( alpha,KIND=sp)
                 a( i, i ) = cone
                 ! apply h(i)**h to a(i:m,i+1:n) from the left
                 if( i<n )call stdlib_clarf( 'LEFT', m-i+1, n-i, a( i, i ), 1_ilp,conjg( tauq( i ) ), &
                           a( i, i+1 ), lda, work )
                 a( i, i ) = d( i )
                 if( i<n ) then
                    ! generate elementary reflector g(i) to annihilate
                    ! a(i,i+2:n)
                    call stdlib_clacgv( n-i, a( i, i+1 ), lda )
                    alpha = a( i, i+1 )
                    call stdlib_clarfg( n-i, alpha, a( i, min( i+2, n ) ),lda, taup( i ) )
                    e( i ) = real( alpha,KIND=sp)
                    a( i, i+1 ) = cone
                    ! apply g(i) to a(i+1:m,i+1:n) from the right
                    call stdlib_clarf( 'RIGHT', m-i, n-i, a( i, i+1 ), lda,taup( i ), a( i+1, i+1 &
                              ), lda, work )
                    call stdlib_clacgv( n-i, a( i, i+1 ), lda )
                    a( i, i+1 ) = e( i )
                 else
                    taup( i ) = czero
                 end if
              end do
           else
              ! reduce to lower bidiagonal form
              do i = 1, m
                 ! generate elementary reflector g(i) to annihilate a(i,i+1:n)
                 call stdlib_clacgv( n-i+1, a( i, i ), lda )
                 alpha = a( i, i )
                 call stdlib_clarfg( n-i+1, alpha, a( i, min( i+1, n ) ), lda,taup( i ) )
                 d( i ) = real( alpha,KIND=sp)
                 a( i, i ) = cone
                 ! apply g(i) to a(i+1:m,i:n) from the right
                 if( i<m )call stdlib_clarf( 'RIGHT', m-i, n-i+1, a( i, i ), lda,taup( i ), a( i+&
                           1_ilp, i ), lda, work )
                 call stdlib_clacgv( n-i+1, a( i, i ), lda )
                 a( i, i ) = d( i )
                 if( i<m ) then
                    ! generate elementary reflector h(i) to annihilate
                    ! a(i+2:m,i)
                    alpha = a( i+1, i )
                    call stdlib_clarfg( m-i, alpha, a( min( i+2, m ), i ), 1_ilp,tauq( i ) )
                    e( i ) = real( alpha,KIND=sp)
                    a( i+1, i ) = cone
                    ! apply h(i)**h to a(i+1:m,i+1:n) from the left
                    call stdlib_clarf( 'LEFT', m-i, n-i, a( i+1, i ), 1_ilp,conjg( tauq( i ) ), a( i+&
                              1_ilp, i+1 ), lda,work )
                    a( i+1, i ) = e( i )
                 else
                    tauq( i ) = czero
                 end if
              end do
           end if
           return
     end subroutine stdlib_cgebd2

     pure module subroutine stdlib_zgebd2( m, n, a, lda, d, e, tauq, taup, work, info )
     !! ZGEBD2 reduces a complex general m by n matrix A to upper or lower
     !! real bidiagonal form B by a unitary transformation: Q**H * A * P = B.
     !! If m >= n, B is upper bidiagonal; if m < n, B is lower bidiagonal.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n
           ! Array Arguments 
           real(dp), intent(out) :: d(*), e(*)
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: taup(*), tauq(*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i
           complex(dp) :: alpha
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
           if( info<0_ilp ) then
              call stdlib_xerbla( 'ZGEBD2', -info )
              return
           end if
           if( m>=n ) then
              ! reduce to upper bidiagonal form
              do i = 1, n
                 ! generate elementary reflector h(i) to annihilate a(i+1:m,i)
                 alpha = a( i, i )
                 call stdlib_zlarfg( m-i+1, alpha, a( min( i+1, m ), i ), 1_ilp,tauq( i ) )
                 d( i ) = real( alpha,KIND=dp)
                 a( i, i ) = cone
                 ! apply h(i)**h to a(i:m,i+1:n) from the left
                 if( i<n )call stdlib_zlarf( 'LEFT', m-i+1, n-i, a( i, i ), 1_ilp,conjg( tauq( i ) ), &
                           a( i, i+1 ), lda, work )
                 a( i, i ) = d( i )
                 if( i<n ) then
                    ! generate elementary reflector g(i) to annihilate
                    ! a(i,i+2:n)
                    call stdlib_zlacgv( n-i, a( i, i+1 ), lda )
                    alpha = a( i, i+1 )
                    call stdlib_zlarfg( n-i, alpha, a( i, min( i+2, n ) ), lda,taup( i ) )
                    e( i ) = real( alpha,KIND=dp)
                    a( i, i+1 ) = cone
                    ! apply g(i) to a(i+1:m,i+1:n) from the right
                    call stdlib_zlarf( 'RIGHT', m-i, n-i, a( i, i+1 ), lda,taup( i ), a( i+1, i+1 &
                              ), lda, work )
                    call stdlib_zlacgv( n-i, a( i, i+1 ), lda )
                    a( i, i+1 ) = e( i )
                 else
                    taup( i ) = czero
                 end if
              end do
           else
              ! reduce to lower bidiagonal form
              do i = 1, m
                 ! generate elementary reflector g(i) to annihilate a(i,i+1:n)
                 call stdlib_zlacgv( n-i+1, a( i, i ), lda )
                 alpha = a( i, i )
                 call stdlib_zlarfg( n-i+1, alpha, a( i, min( i+1, n ) ), lda,taup( i ) )
                 d( i ) = real( alpha,KIND=dp)
                 a( i, i ) = cone
                 ! apply g(i) to a(i+1:m,i:n) from the right
                 if( i<m )call stdlib_zlarf( 'RIGHT', m-i, n-i+1, a( i, i ), lda,taup( i ), a( i+&
                           1_ilp, i ), lda, work )
                 call stdlib_zlacgv( n-i+1, a( i, i ), lda )
                 a( i, i ) = d( i )
                 if( i<m ) then
                    ! generate elementary reflector h(i) to annihilate
                    ! a(i+2:m,i)
                    alpha = a( i+1, i )
                    call stdlib_zlarfg( m-i, alpha, a( min( i+2, m ), i ), 1_ilp,tauq( i ) )
                    e( i ) = real( alpha,KIND=dp)
                    a( i+1, i ) = cone
                    ! apply h(i)**h to a(i+1:m,i+1:n) from the left
                    call stdlib_zlarf( 'LEFT', m-i, n-i, a( i+1, i ), 1_ilp,conjg( tauq( i ) ), a( i+&
                              1_ilp, i+1 ), lda,work )
                    a( i+1, i ) = e( i )
                 else
                    tauq( i ) = czero
                 end if
              end do
           end if
           return
     end subroutine stdlib_zgebd2




     pure module subroutine stdlib_sgbbrd( vect, m, n, ncc, kl, ku, ab, ldab, d, e, q,ldq, pt, ldpt, c, &
     !! SGBBRD reduces a real general m-by-n band matrix A to upper
     !! bidiagonal form B by an orthogonal transformation: Q**T * A * P = B.
     !! The routine computes B, and optionally forms Q or P**T, or computes
     !! Q**T*C for a given matrix C.
               ldc, work, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: vect
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kl, ku, ldab, ldc, ldpt, ldq, m, n, ncc
           ! Array Arguments 
           real(sp), intent(inout) :: ab(ldab,*), c(ldc,*)
           real(sp), intent(out) :: d(*), e(*), pt(ldpt,*), q(ldq,*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: wantb, wantc, wantpt, wantq
           integer(ilp) :: i, inca, j, j1, j2, kb, kb1, kk, klm, klu1, kun, l, minmn, ml, ml0, mn,&
                      mu, mu0, nr, nrt
           real(sp) :: ra, rb, rc, rs
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           wantb = stdlib_lsame( vect, 'B' )
           wantq = stdlib_lsame( vect, 'Q' ) .or. wantb
           wantpt = stdlib_lsame( vect, 'P' ) .or. wantb
           wantc = ncc>0_ilp
           klu1 = kl + ku + 1_ilp
           info = 0_ilp
           if( .not.wantq .and. .not.wantpt .and. .not.stdlib_lsame( vect, 'N' ) )then
              info = -1_ilp
           else if( m<0_ilp ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( ncc<0_ilp ) then
              info = -4_ilp
           else if( kl<0_ilp ) then
              info = -5_ilp
           else if( ku<0_ilp ) then
              info = -6_ilp
           else if( ldab<klu1 ) then
              info = -8_ilp
           else if( ldq<1_ilp .or. wantq .and. ldq<max( 1_ilp, m ) ) then
              info = -12_ilp
           else if( ldpt<1_ilp .or. wantpt .and. ldpt<max( 1_ilp, n ) ) then
              info = -14_ilp
           else if( ldc<1_ilp .or. wantc .and. ldc<max( 1_ilp, m ) ) then
              info = -16_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SGBBRD', -info )
              return
           end if
           ! initialize q and p**t to the unit matrix, if needed
           if( wantq )call stdlib_slaset( 'FULL', m, m, zero, one, q, ldq )
           if( wantpt )call stdlib_slaset( 'FULL', n, n, zero, one, pt, ldpt )
           ! quick return if possible.
           if( m==0 .or. n==0 )return
           minmn = min( m, n )
           if( kl+ku>1_ilp ) then
              ! reduce to upper bidiagonal form if ku > 0; if ku = 0, reduce
              ! first to lower bidiagonal form and then transform to upper
              ! bidiagonal
              if( ku>0_ilp ) then
                 ml0 = 1_ilp
                 mu0 = 2_ilp
              else
                 ml0 = 2_ilp
                 mu0 = 1_ilp
              end if
              ! wherever possible, plane rotations are generated and applied in
              ! vector operations of length nr over the index set j1:j2:klu1.
              ! the sines of the plane rotations are stored in work(1:max(m,n))
              ! and the cosines in work(max(m,n)+1:2*max(m,n)).
              mn = max( m, n )
              klm = min( m-1, kl )
              kun = min( n-1, ku )
              kb = klm + kun
              kb1 = kb + 1_ilp
              inca = kb1*ldab
              nr = 0_ilp
              j1 = klm + 2_ilp
              j2 = 1_ilp - kun
              loop_90: do i = 1, minmn
                 ! reduce i-th column and i-th row of matrix to bidiagonal form
                 ml = klm + 1_ilp
                 mu = kun + 1_ilp
                 loop_80: do kk = 1, kb
                    j1 = j1 + kb
                    j2 = j2 + kb
                    ! generate plane rotations to annihilate nonzero elements
                    ! which have been created below the band
                    if( nr>0_ilp )call stdlib_slargv( nr, ab( klu1, j1-klm-1 ), inca,work( j1 ), kb1, &
                              work( mn+j1 ), kb1 )
                    ! apply plane rotations from the left
                    do l = 1, kb
                       if( j2-klm+l-1>n ) then
                          nrt = nr - 1_ilp
                       else
                          nrt = nr
                       end if
                       if( nrt>0_ilp )call stdlib_slartv( nrt, ab( klu1-l, j1-klm+l-1 ), inca,ab( &
                                 klu1-l+1, j1-klm+l-1 ), inca,work( mn+j1 ), work( j1 ), kb1 )
                    end do
                    if( ml>ml0 ) then
                       if( ml<=m-i+1 ) then
                          ! generate plane rotation to annihilate a(i+ml-1,i)
                          ! within the band, and apply rotation from the left
                          call stdlib_slartg( ab( ku+ml-1, i ), ab( ku+ml, i ),work( mn+i+ml-1 ), &
                                    work( i+ml-1 ),ra )
                          ab( ku+ml-1, i ) = ra
                          if( i<n )call stdlib_srot( min( ku+ml-2, n-i ),ab( ku+ml-2, i+1 ), ldab-&
                                    1_ilp,ab( ku+ml-1, i+1 ), ldab-1,work( mn+i+ml-1 ), work( i+ml-1 ) )
                       end if
                       nr = nr + 1_ilp
                       j1 = j1 - kb1
                    end if
                    if( wantq ) then
                       ! accumulate product of plane rotations in q
                       do j = j1, j2, kb1
                          call stdlib_srot( m, q( 1_ilp, j-1 ), 1_ilp, q( 1_ilp, j ), 1_ilp,work( mn+j ), work( j &
                                    ) )
                       end do
                    end if
                    if( wantc ) then
                       ! apply plane rotations to c
                       do j = j1, j2, kb1
                          call stdlib_srot( ncc, c( j-1, 1_ilp ), ldc, c( j, 1_ilp ), ldc,work( mn+j ), &
                                    work( j ) )
                       end do
                    end if
                    if( j2+kun>n ) then
                       ! adjust j2 to keep within the bounds of the matrix
                       nr = nr - 1_ilp
                       j2 = j2 - kb1
                    end if
                    do j = j1, j2, kb1
                       ! create nonzero element a(j-1,j+ku) above the band
                       ! and store it in work(n+1:2*n)
                       work( j+kun ) = work( j )*ab( 1_ilp, j+kun )
                       ab( 1_ilp, j+kun ) = work( mn+j )*ab( 1_ilp, j+kun )
                    end do
                    ! generate plane rotations to annihilate nonzero elements
                    ! which have been generated above the band
                    if( nr>0_ilp )call stdlib_slargv( nr, ab( 1_ilp, j1+kun-1 ), inca,work( j1+kun ), kb1,&
                               work( mn+j1+kun ),kb1 )
                    ! apply plane rotations from the right
                    do l = 1, kb
                       if( j2+l-1>m ) then
                          nrt = nr - 1_ilp
                       else
                          nrt = nr
                       end if
                       if( nrt>0_ilp )call stdlib_slartv( nrt, ab( l+1, j1+kun-1 ), inca,ab( l, j1+&
                                 kun ), inca,work( mn+j1+kun ), work( j1+kun ),kb1 )
                    end do
                    if( ml==ml0 .and. mu>mu0 ) then
                       if( mu<=n-i+1 ) then
                          ! generate plane rotation to annihilate a(i,i+mu-1)
                          ! within the band, and apply rotation from the right
                          call stdlib_slartg( ab( ku-mu+3, i+mu-2 ),ab( ku-mu+2, i+mu-1 ),work( &
                                    mn+i+mu-1 ), work( i+mu-1 ),ra )
                          ab( ku-mu+3, i+mu-2 ) = ra
                          call stdlib_srot( min( kl+mu-2, m-i ),ab( ku-mu+4, i+mu-2 ), 1_ilp,ab( ku-&
                                    mu+3, i+mu-1 ), 1_ilp,work( mn+i+mu-1 ), work( i+mu-1 ) )
                       end if
                       nr = nr + 1_ilp
                       j1 = j1 - kb1
                    end if
                    if( wantpt ) then
                       ! accumulate product of plane rotations in p**t
                       do j = j1, j2, kb1
                          call stdlib_srot( n, pt( j+kun-1, 1_ilp ), ldpt,pt( j+kun, 1_ilp ), ldpt, work( &
                                    mn+j+kun ),work( j+kun ) )
                       end do
                    end if
                    if( j2+kb>m ) then
                       ! adjust j2 to keep within the bounds of the matrix
                       nr = nr - 1_ilp
                       j2 = j2 - kb1
                    end if
                    do j = j1, j2, kb1
                       ! create nonzero element a(j+kl+ku,j+ku-1) below the
                       ! band and store it in work(1:n)
                       work( j+kb ) = work( j+kun )*ab( klu1, j+kun )
                       ab( klu1, j+kun ) = work( mn+j+kun )*ab( klu1, j+kun )
                    end do
                    if( ml>ml0 ) then
                       ml = ml - 1_ilp
                    else
                       mu = mu - 1_ilp
                    end if
                 end do loop_80
              end do loop_90
           end if
           if( ku==0_ilp .and. kl>0_ilp ) then
              ! a has been reduced to lower bidiagonal form
              ! transform lower bidiagonal form to upper bidiagonal by applying
              ! plane rotations from the left, storing diagonal elements in d
              ! and off-diagonal elements in e
              do i = 1, min( m-1, n )
                 call stdlib_slartg( ab( 1_ilp, i ), ab( 2_ilp, i ), rc, rs, ra )
                 d( i ) = ra
                 if( i<n ) then
                    e( i ) = rs*ab( 1_ilp, i+1 )
                    ab( 1_ilp, i+1 ) = rc*ab( 1_ilp, i+1 )
                 end if
                 if( wantq )call stdlib_srot( m, q( 1_ilp, i ), 1_ilp, q( 1_ilp, i+1 ), 1_ilp, rc, rs )
                 if( wantc )call stdlib_srot( ncc, c( i, 1_ilp ), ldc, c( i+1, 1_ilp ), ldc, rc,rs )
                           
              end do
              if( m<=n )d( m ) = ab( 1_ilp, m )
           else if( ku>0_ilp ) then
              ! a has been reduced to upper bidiagonal form
              if( m<n ) then
                 ! annihilate a(m,m+1) by applying plane rotations from the
                 ! right, storing diagonal elements in d and off-diagonal
                 ! elements in e
                 rb = ab( ku, m+1 )
                 do i = m, 1, -1
                    call stdlib_slartg( ab( ku+1, i ), rb, rc, rs, ra )
                    d( i ) = ra
                    if( i>1_ilp ) then
                       rb = -rs*ab( ku, i )
                       e( i-1 ) = rc*ab( ku, i )
                    end if
                    if( wantpt )call stdlib_srot( n, pt( i, 1_ilp ), ldpt, pt( m+1, 1_ilp ), ldpt,rc, rs )
                              
                 end do
              else
                 ! copy off-diagonal elements to e and diagonal elements to d
                 do i = 1, minmn - 1
                    e( i ) = ab( ku, i+1 )
                 end do
                 do i = 1, minmn
                    d( i ) = ab( ku+1, i )
                 end do
              end if
           else
              ! a is diagonal. set elements of e to zero and copy diagonal
              ! elements to d.
              do i = 1, minmn - 1
                 e( i ) = zero
              end do
              do i = 1, minmn
                 d( i ) = ab( 1_ilp, i )
              end do
           end if
           return
     end subroutine stdlib_sgbbrd

     pure module subroutine stdlib_dgbbrd( vect, m, n, ncc, kl, ku, ab, ldab, d, e, q,ldq, pt, ldpt, c, &
     !! DGBBRD reduces a real general m-by-n band matrix A to upper
     !! bidiagonal form B by an orthogonal transformation: Q**T * A * P = B.
     !! The routine computes B, and optionally forms Q or P**T, or computes
     !! Q**T*C for a given matrix C.
               ldc, work, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: vect
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kl, ku, ldab, ldc, ldpt, ldq, m, n, ncc
           ! Array Arguments 
           real(dp), intent(inout) :: ab(ldab,*), c(ldc,*)
           real(dp), intent(out) :: d(*), e(*), pt(ldpt,*), q(ldq,*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: wantb, wantc, wantpt, wantq
           integer(ilp) :: i, inca, j, j1, j2, kb, kb1, kk, klm, klu1, kun, l, minmn, ml, ml0, mn,&
                      mu, mu0, nr, nrt
           real(dp) :: ra, rb, rc, rs
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           wantb = stdlib_lsame( vect, 'B' )
           wantq = stdlib_lsame( vect, 'Q' ) .or. wantb
           wantpt = stdlib_lsame( vect, 'P' ) .or. wantb
           wantc = ncc>0_ilp
           klu1 = kl + ku + 1_ilp
           info = 0_ilp
           if( .not.wantq .and. .not.wantpt .and. .not.stdlib_lsame( vect, 'N' ) )then
              info = -1_ilp
           else if( m<0_ilp ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( ncc<0_ilp ) then
              info = -4_ilp
           else if( kl<0_ilp ) then
              info = -5_ilp
           else if( ku<0_ilp ) then
              info = -6_ilp
           else if( ldab<klu1 ) then
              info = -8_ilp
           else if( ldq<1_ilp .or. wantq .and. ldq<max( 1_ilp, m ) ) then
              info = -12_ilp
           else if( ldpt<1_ilp .or. wantpt .and. ldpt<max( 1_ilp, n ) ) then
              info = -14_ilp
           else if( ldc<1_ilp .or. wantc .and. ldc<max( 1_ilp, m ) ) then
              info = -16_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DGBBRD', -info )
              return
           end if
           ! initialize q and p**t to the unit matrix, if needed
           if( wantq )call stdlib_dlaset( 'FULL', m, m, zero, one, q, ldq )
           if( wantpt )call stdlib_dlaset( 'FULL', n, n, zero, one, pt, ldpt )
           ! quick return if possible.
           if( m==0 .or. n==0 )return
           minmn = min( m, n )
           if( kl+ku>1_ilp ) then
              ! reduce to upper bidiagonal form if ku > 0; if ku = 0, reduce
              ! first to lower bidiagonal form and then transform to upper
              ! bidiagonal
              if( ku>0_ilp ) then
                 ml0 = 1_ilp
                 mu0 = 2_ilp
              else
                 ml0 = 2_ilp
                 mu0 = 1_ilp
              end if
              ! wherever possible, plane rotations are generated and applied in
              ! vector operations of length nr over the index set j1:j2:klu1.
              ! the sines of the plane rotations are stored in work(1:max(m,n))
              ! and the cosines in work(max(m,n)+1:2*max(m,n)).
              mn = max( m, n )
              klm = min( m-1, kl )
              kun = min( n-1, ku )
              kb = klm + kun
              kb1 = kb + 1_ilp
              inca = kb1*ldab
              nr = 0_ilp
              j1 = klm + 2_ilp
              j2 = 1_ilp - kun
              loop_90: do i = 1, minmn
                 ! reduce i-th column and i-th row of matrix to bidiagonal form
                 ml = klm + 1_ilp
                 mu = kun + 1_ilp
                 loop_80: do kk = 1, kb
                    j1 = j1 + kb
                    j2 = j2 + kb
                    ! generate plane rotations to annihilate nonzero elements
                    ! which have been created below the band
                    if( nr>0_ilp )call stdlib_dlargv( nr, ab( klu1, j1-klm-1 ), inca,work( j1 ), kb1, &
                              work( mn+j1 ), kb1 )
                    ! apply plane rotations from the left
                    do l = 1, kb
                       if( j2-klm+l-1>n ) then
                          nrt = nr - 1_ilp
                       else
                          nrt = nr
                       end if
                       if( nrt>0_ilp )call stdlib_dlartv( nrt, ab( klu1-l, j1-klm+l-1 ), inca,ab( &
                                 klu1-l+1, j1-klm+l-1 ), inca,work( mn+j1 ), work( j1 ), kb1 )
                    end do
                    if( ml>ml0 ) then
                       if( ml<=m-i+1 ) then
                          ! generate plane rotation to annihilate a(i+ml-1,i)
                          ! within the band, and apply rotation from the left
                          call stdlib_dlartg( ab( ku+ml-1, i ), ab( ku+ml, i ),work( mn+i+ml-1 ), &
                                    work( i+ml-1 ),ra )
                          ab( ku+ml-1, i ) = ra
                          if( i<n )call stdlib_drot( min( ku+ml-2, n-i ),ab( ku+ml-2, i+1 ), ldab-&
                                    1_ilp,ab( ku+ml-1, i+1 ), ldab-1,work( mn+i+ml-1 ), work( i+ml-1 ) )
                       end if
                       nr = nr + 1_ilp
                       j1 = j1 - kb1
                    end if
                    if( wantq ) then
                       ! accumulate product of plane rotations in q
                       do j = j1, j2, kb1
                          call stdlib_drot( m, q( 1_ilp, j-1 ), 1_ilp, q( 1_ilp, j ), 1_ilp,work( mn+j ), work( j &
                                    ) )
                       end do
                    end if
                    if( wantc ) then
                       ! apply plane rotations to c
                       do j = j1, j2, kb1
                          call stdlib_drot( ncc, c( j-1, 1_ilp ), ldc, c( j, 1_ilp ), ldc,work( mn+j ), &
                                    work( j ) )
                       end do
                    end if
                    if( j2+kun>n ) then
                       ! adjust j2 to keep within the bounds of the matrix
                       nr = nr - 1_ilp
                       j2 = j2 - kb1
                    end if
                    do j = j1, j2, kb1
                       ! create nonzero element a(j-1,j+ku) above the band
                       ! and store it in work(n+1:2*n)
                       work( j+kun ) = work( j )*ab( 1_ilp, j+kun )
                       ab( 1_ilp, j+kun ) = work( mn+j )*ab( 1_ilp, j+kun )
                    end do
                    ! generate plane rotations to annihilate nonzero elements
                    ! which have been generated above the band
                    if( nr>0_ilp )call stdlib_dlargv( nr, ab( 1_ilp, j1+kun-1 ), inca,work( j1+kun ), kb1,&
                               work( mn+j1+kun ),kb1 )
                    ! apply plane rotations from the right
                    do l = 1, kb
                       if( j2+l-1>m ) then
                          nrt = nr - 1_ilp
                       else
                          nrt = nr
                       end if
                       if( nrt>0_ilp )call stdlib_dlartv( nrt, ab( l+1, j1+kun-1 ), inca,ab( l, j1+&
                                 kun ), inca,work( mn+j1+kun ), work( j1+kun ),kb1 )
                    end do
                    if( ml==ml0 .and. mu>mu0 ) then
                       if( mu<=n-i+1 ) then
                          ! generate plane rotation to annihilate a(i,i+mu-1)
                          ! within the band, and apply rotation from the right
                          call stdlib_dlartg( ab( ku-mu+3, i+mu-2 ),ab( ku-mu+2, i+mu-1 ),work( &
                                    mn+i+mu-1 ), work( i+mu-1 ),ra )
                          ab( ku-mu+3, i+mu-2 ) = ra
                          call stdlib_drot( min( kl+mu-2, m-i ),ab( ku-mu+4, i+mu-2 ), 1_ilp,ab( ku-&
                                    mu+3, i+mu-1 ), 1_ilp,work( mn+i+mu-1 ), work( i+mu-1 ) )
                       end if
                       nr = nr + 1_ilp
                       j1 = j1 - kb1
                    end if
                    if( wantpt ) then
                       ! accumulate product of plane rotations in p**t
                       do j = j1, j2, kb1
                          call stdlib_drot( n, pt( j+kun-1, 1_ilp ), ldpt,pt( j+kun, 1_ilp ), ldpt, work( &
                                    mn+j+kun ),work( j+kun ) )
                       end do
                    end if
                    if( j2+kb>m ) then
                       ! adjust j2 to keep within the bounds of the matrix
                       nr = nr - 1_ilp
                       j2 = j2 - kb1
                    end if
                    do j = j1, j2, kb1
                       ! create nonzero element a(j+kl+ku,j+ku-1) below the
                       ! band and store it in work(1:n)
                       work( j+kb ) = work( j+kun )*ab( klu1, j+kun )
                       ab( klu1, j+kun ) = work( mn+j+kun )*ab( klu1, j+kun )
                    end do
                    if( ml>ml0 ) then
                       ml = ml - 1_ilp
                    else
                       mu = mu - 1_ilp
                    end if
                 end do loop_80
              end do loop_90
           end if
           if( ku==0_ilp .and. kl>0_ilp ) then
              ! a has been reduced to lower bidiagonal form
              ! transform lower bidiagonal form to upper bidiagonal by applying
              ! plane rotations from the left, storing diagonal elements in d
              ! and off-diagonal elements in e
              do i = 1, min( m-1, n )
                 call stdlib_dlartg( ab( 1_ilp, i ), ab( 2_ilp, i ), rc, rs, ra )
                 d( i ) = ra
                 if( i<n ) then
                    e( i ) = rs*ab( 1_ilp, i+1 )
                    ab( 1_ilp, i+1 ) = rc*ab( 1_ilp, i+1 )
                 end if
                 if( wantq )call stdlib_drot( m, q( 1_ilp, i ), 1_ilp, q( 1_ilp, i+1 ), 1_ilp, rc, rs )
                 if( wantc )call stdlib_drot( ncc, c( i, 1_ilp ), ldc, c( i+1, 1_ilp ), ldc, rc,rs )
                           
              end do
              if( m<=n )d( m ) = ab( 1_ilp, m )
           else if( ku>0_ilp ) then
              ! a has been reduced to upper bidiagonal form
              if( m<n ) then
                 ! annihilate a(m,m+1) by applying plane rotations from the
                 ! right, storing diagonal elements in d and off-diagonal
                 ! elements in e
                 rb = ab( ku, m+1 )
                 do i = m, 1, -1
                    call stdlib_dlartg( ab( ku+1, i ), rb, rc, rs, ra )
                    d( i ) = ra
                    if( i>1_ilp ) then
                       rb = -rs*ab( ku, i )
                       e( i-1 ) = rc*ab( ku, i )
                    end if
                    if( wantpt )call stdlib_drot( n, pt( i, 1_ilp ), ldpt, pt( m+1, 1_ilp ), ldpt,rc, rs )
                              
                 end do
              else
                 ! copy off-diagonal elements to e and diagonal elements to d
                 do i = 1, minmn - 1
                    e( i ) = ab( ku, i+1 )
                 end do
                 do i = 1, minmn
                    d( i ) = ab( ku+1, i )
                 end do
              end if
           else
              ! a is diagonal. set elements of e to zero and copy diagonal
              ! elements to d.
              do i = 1, minmn - 1
                 e( i ) = zero
              end do
              do i = 1, minmn
                 d( i ) = ab( 1_ilp, i )
              end do
           end if
           return
     end subroutine stdlib_dgbbrd


     pure module subroutine stdlib_cgbbrd( vect, m, n, ncc, kl, ku, ab, ldab, d, e, q,ldq, pt, ldpt, c, &
     !! CGBBRD reduces a complex general m-by-n band matrix A to real upper
     !! bidiagonal form B by a unitary transformation: Q**H * A * P = B.
     !! The routine computes B, and optionally forms Q or P**H, or computes
     !! Q**H*C for a given matrix C.
               ldc, work, rwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: vect
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kl, ku, ldab, ldc, ldpt, ldq, m, n, ncc
           ! Array Arguments 
           real(sp), intent(out) :: d(*), e(*), rwork(*)
           complex(sp), intent(inout) :: ab(ldab,*), c(ldc,*)
           complex(sp), intent(out) :: pt(ldpt,*), q(ldq,*), work(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: wantb, wantc, wantpt, wantq
           integer(ilp) :: i, inca, j, j1, j2, kb, kb1, kk, klm, klu1, kun, l, minmn, ml, ml0, mu,&
                      mu0, nr, nrt
           real(sp) :: abst, rc
           complex(sp) :: ra, rb, rs, t
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           wantb = stdlib_lsame( vect, 'B' )
           wantq = stdlib_lsame( vect, 'Q' ) .or. wantb
           wantpt = stdlib_lsame( vect, 'P' ) .or. wantb
           wantc = ncc>0_ilp
           klu1 = kl + ku + 1_ilp
           info = 0_ilp
           if( .not.wantq .and. .not.wantpt .and. .not.stdlib_lsame( vect, 'N' ) )then
              info = -1_ilp
           else if( m<0_ilp ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( ncc<0_ilp ) then
              info = -4_ilp
           else if( kl<0_ilp ) then
              info = -5_ilp
           else if( ku<0_ilp ) then
              info = -6_ilp
           else if( ldab<klu1 ) then
              info = -8_ilp
           else if( ldq<1_ilp .or. wantq .and. ldq<max( 1_ilp, m ) ) then
              info = -12_ilp
           else if( ldpt<1_ilp .or. wantpt .and. ldpt<max( 1_ilp, n ) ) then
              info = -14_ilp
           else if( ldc<1_ilp .or. wantc .and. ldc<max( 1_ilp, m ) ) then
              info = -16_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CGBBRD', -info )
              return
           end if
           ! initialize q and p**h to the unit matrix, if needed
           if( wantq )call stdlib_claset( 'FULL', m, m, czero, cone, q, ldq )
           if( wantpt )call stdlib_claset( 'FULL', n, n, czero, cone, pt, ldpt )
           ! quick return if possible.
           if( m==0 .or. n==0 )return
           minmn = min( m, n )
           if( kl+ku>1_ilp ) then
              ! reduce to upper bidiagonal form if ku > 0; if ku = 0, reduce
              ! first to lower bidiagonal form and then transform to upper
              ! bidiagonal
              if( ku>0_ilp ) then
                 ml0 = 1_ilp
                 mu0 = 2_ilp
              else
                 ml0 = 2_ilp
                 mu0 = 1_ilp
              end if
              ! wherever possible, plane rotations are generated and applied in
              ! vector operations of length nr over the index set j1:j2:klu1.
              ! the complex sines of the plane rotations are stored in work,
              ! and the real cosines in rwork.
              klm = min( m-1, kl )
              kun = min( n-1, ku )
              kb = klm + kun
              kb1 = kb + 1_ilp
              inca = kb1*ldab
              nr = 0_ilp
              j1 = klm + 2_ilp
              j2 = 1_ilp - kun
              loop_90: do i = 1, minmn
                 ! reduce i-th column and i-th row of matrix to bidiagonal form
                 ml = klm + 1_ilp
                 mu = kun + 1_ilp
                 loop_80: do kk = 1, kb
                    j1 = j1 + kb
                    j2 = j2 + kb
                    ! generate plane rotations to annihilate nonzero elements
                    ! which have been created below the band
                    if( nr>0_ilp )call stdlib_clargv( nr, ab( klu1, j1-klm-1 ), inca,work( j1 ), kb1, &
                              rwork( j1 ), kb1 )
                    ! apply plane rotations from the left
                    do l = 1, kb
                       if( j2-klm+l-1>n ) then
                          nrt = nr - 1_ilp
                       else
                          nrt = nr
                       end if
                       if( nrt>0_ilp )call stdlib_clartv( nrt, ab( klu1-l, j1-klm+l-1 ), inca,ab( &
                                 klu1-l+1, j1-klm+l-1 ), inca,rwork( j1 ), work( j1 ), kb1 )
                    end do
                    if( ml>ml0 ) then
                       if( ml<=m-i+1 ) then
                          ! generate plane rotation to annihilate a(i+ml-1,i)
                          ! within the band, and apply rotation from the left
                          call stdlib_clartg( ab( ku+ml-1, i ), ab( ku+ml, i ),rwork( i+ml-1 ), &
                                    work( i+ml-1 ), ra )
                          ab( ku+ml-1, i ) = ra
                          if( i<n )call stdlib_crot( min( ku+ml-2, n-i ),ab( ku+ml-2, i+1 ), ldab-&
                                    1_ilp,ab( ku+ml-1, i+1 ), ldab-1,rwork( i+ml-1 ), work( i+ml-1 ) )
                       end if
                       nr = nr + 1_ilp
                       j1 = j1 - kb1
                    end if
                    if( wantq ) then
                       ! accumulate product of plane rotations in q
                       do j = j1, j2, kb1
                          call stdlib_crot( m, q( 1_ilp, j-1 ), 1_ilp, q( 1_ilp, j ), 1_ilp,rwork( j ), conjg( &
                                    work( j ) ) )
                       end do
                    end if
                    if( wantc ) then
                       ! apply plane rotations to c
                       do j = j1, j2, kb1
                          call stdlib_crot( ncc, c( j-1, 1_ilp ), ldc, c( j, 1_ilp ), ldc,rwork( j ), &
                                    work( j ) )
                       end do
                    end if
                    if( j2+kun>n ) then
                       ! adjust j2 to keep within the bounds of the matrix
                       nr = nr - 1_ilp
                       j2 = j2 - kb1
                    end if
                    do j = j1, j2, kb1
                       ! create nonzero element a(j-1,j+ku) above the band
                       ! and store it in work(n+1:2*n)
                       work( j+kun ) = work( j )*ab( 1_ilp, j+kun )
                       ab( 1_ilp, j+kun ) = rwork( j )*ab( 1_ilp, j+kun )
                    end do
                    ! generate plane rotations to annihilate nonzero elements
                    ! which have been generated above the band
                    if( nr>0_ilp )call stdlib_clargv( nr, ab( 1_ilp, j1+kun-1 ), inca,work( j1+kun ), kb1,&
                               rwork( j1+kun ),kb1 )
                    ! apply plane rotations from the right
                    do l = 1, kb
                       if( j2+l-1>m ) then
                          nrt = nr - 1_ilp
                       else
                          nrt = nr
                       end if
                       if( nrt>0_ilp )call stdlib_clartv( nrt, ab( l+1, j1+kun-1 ), inca,ab( l, j1+&
                                 kun ), inca,rwork( j1+kun ), work( j1+kun ), kb1 )
                    end do
                    if( ml==ml0 .and. mu>mu0 ) then
                       if( mu<=n-i+1 ) then
                          ! generate plane rotation to annihilate a(i,i+mu-1)
                          ! within the band, and apply rotation from the right
                          call stdlib_clartg( ab( ku-mu+3, i+mu-2 ),ab( ku-mu+2, i+mu-1 ),rwork( &
                                    i+mu-1 ), work( i+mu-1 ), ra )
                          ab( ku-mu+3, i+mu-2 ) = ra
                          call stdlib_crot( min( kl+mu-2, m-i ),ab( ku-mu+4, i+mu-2 ), 1_ilp,ab( ku-&
                                    mu+3, i+mu-1 ), 1_ilp,rwork( i+mu-1 ), work( i+mu-1 ) )
                       end if
                       nr = nr + 1_ilp
                       j1 = j1 - kb1
                    end if
                    if( wantpt ) then
                       ! accumulate product of plane rotations in p**h
                       do j = j1, j2, kb1
                          call stdlib_crot( n, pt( j+kun-1, 1_ilp ), ldpt,pt( j+kun, 1_ilp ), ldpt, rwork(&
                                     j+kun ),conjg( work( j+kun ) ) )
                       end do
                    end if
                    if( j2+kb>m ) then
                       ! adjust j2 to keep within the bounds of the matrix
                       nr = nr - 1_ilp
                       j2 = j2 - kb1
                    end if
                    do j = j1, j2, kb1
                       ! create nonzero element a(j+kl+ku,j+ku-1) below the
                       ! band and store it in work(1:n)
                       work( j+kb ) = work( j+kun )*ab( klu1, j+kun )
                       ab( klu1, j+kun ) = rwork( j+kun )*ab( klu1, j+kun )
                    end do
                    if( ml>ml0 ) then
                       ml = ml - 1_ilp
                    else
                       mu = mu - 1_ilp
                    end if
                 end do loop_80
              end do loop_90
           end if
           if( ku==0_ilp .and. kl>0_ilp ) then
              ! a has been reduced to complex lower bidiagonal form
              ! transform lower bidiagonal form to upper bidiagonal by applying
              ! plane rotations from the left, overwriting superdiagonal
              ! elements on subdiagonal elements
              do i = 1, min( m-1, n )
                 call stdlib_clartg( ab( 1_ilp, i ), ab( 2_ilp, i ), rc, rs, ra )
                 ab( 1_ilp, i ) = ra
                 if( i<n ) then
                    ab( 2_ilp, i ) = rs*ab( 1_ilp, i+1 )
                    ab( 1_ilp, i+1 ) = rc*ab( 1_ilp, i+1 )
                 end if
                 if( wantq )call stdlib_crot( m, q( 1_ilp, i ), 1_ilp, q( 1_ilp, i+1 ), 1_ilp, rc,conjg( rs ) )
                           
                 if( wantc )call stdlib_crot( ncc, c( i, 1_ilp ), ldc, c( i+1, 1_ilp ), ldc, rc,rs )
                           
              end do
           else
              ! a has been reduced to complex upper bidiagonal form or is
              ! diagonal
              if( ku>0_ilp .and. m<n ) then
                 ! annihilate a(m,m+1) by applying plane rotations from the
                 ! right
                 rb = ab( ku, m+1 )
                 do i = m, 1, -1
                    call stdlib_clartg( ab( ku+1, i ), rb, rc, rs, ra )
                    ab( ku+1, i ) = ra
                    if( i>1_ilp ) then
                       rb = -conjg( rs )*ab( ku, i )
                       ab( ku, i ) = rc*ab( ku, i )
                    end if
                    if( wantpt )call stdlib_crot( n, pt( i, 1_ilp ), ldpt, pt( m+1, 1_ilp ), ldpt,rc, &
                              conjg( rs ) )
                 end do
              end if
           end if
           ! make diagonal and superdiagonal elements real, storing them in d
           ! and e
           t = ab( ku+1, 1_ilp )
           loop_120: do i = 1, minmn
              abst = abs( t )
              d( i ) = abst
              if( abst/=zero ) then
                 t = t / abst
              else
                 t = cone
              end if
              if( wantq )call stdlib_cscal( m, t, q( 1_ilp, i ), 1_ilp )
              if( wantc )call stdlib_cscal( ncc, conjg( t ), c( i, 1_ilp ), ldc )
              if( i<minmn ) then
                 if( ku==0_ilp .and. kl==0_ilp ) then
                    e( i ) = zero
                    t = ab( 1_ilp, i+1 )
                 else
                    if( ku==0_ilp ) then
                       t = ab( 2_ilp, i )*conjg( t )
                    else
                       t = ab( ku, i+1 )*conjg( t )
                    end if
                    abst = abs( t )
                    e( i ) = abst
                    if( abst/=zero ) then
                       t = t / abst
                    else
                       t = cone
                    end if
                    if( wantpt )call stdlib_cscal( n, t, pt( i+1, 1_ilp ), ldpt )
                    t = ab( ku+1, i+1 )*conjg( t )
                 end if
              end if
           end do loop_120
           return
     end subroutine stdlib_cgbbrd

     pure module subroutine stdlib_zgbbrd( vect, m, n, ncc, kl, ku, ab, ldab, d, e, q,ldq, pt, ldpt, c, &
     !! ZGBBRD reduces a complex general m-by-n band matrix A to real upper
     !! bidiagonal form B by a unitary transformation: Q**H * A * P = B.
     !! The routine computes B, and optionally forms Q or P**H, or computes
     !! Q**H*C for a given matrix C.
               ldc, work, rwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: vect
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kl, ku, ldab, ldc, ldpt, ldq, m, n, ncc
           ! Array Arguments 
           real(dp), intent(out) :: d(*), e(*), rwork(*)
           complex(dp), intent(inout) :: ab(ldab,*), c(ldc,*)
           complex(dp), intent(out) :: pt(ldpt,*), q(ldq,*), work(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: wantb, wantc, wantpt, wantq
           integer(ilp) :: i, inca, j, j1, j2, kb, kb1, kk, klm, klu1, kun, l, minmn, ml, ml0, mu,&
                      mu0, nr, nrt
           real(dp) :: abst, rc
           complex(dp) :: ra, rb, rs, t
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           wantb = stdlib_lsame( vect, 'B' )
           wantq = stdlib_lsame( vect, 'Q' ) .or. wantb
           wantpt = stdlib_lsame( vect, 'P' ) .or. wantb
           wantc = ncc>0_ilp
           klu1 = kl + ku + 1_ilp
           info = 0_ilp
           if( .not.wantq .and. .not.wantpt .and. .not.stdlib_lsame( vect, 'N' ) )then
              info = -1_ilp
           else if( m<0_ilp ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( ncc<0_ilp ) then
              info = -4_ilp
           else if( kl<0_ilp ) then
              info = -5_ilp
           else if( ku<0_ilp ) then
              info = -6_ilp
           else if( ldab<klu1 ) then
              info = -8_ilp
           else if( ldq<1_ilp .or. wantq .and. ldq<max( 1_ilp, m ) ) then
              info = -12_ilp
           else if( ldpt<1_ilp .or. wantpt .and. ldpt<max( 1_ilp, n ) ) then
              info = -14_ilp
           else if( ldc<1_ilp .or. wantc .and. ldc<max( 1_ilp, m ) ) then
              info = -16_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZGBBRD', -info )
              return
           end if
           ! initialize q and p**h to the unit matrix, if needed
           if( wantq )call stdlib_zlaset( 'FULL', m, m, czero, cone, q, ldq )
           if( wantpt )call stdlib_zlaset( 'FULL', n, n, czero, cone, pt, ldpt )
           ! quick return if possible.
           if( m==0 .or. n==0 )return
           minmn = min( m, n )
           if( kl+ku>1_ilp ) then
              ! reduce to upper bidiagonal form if ku > 0; if ku = 0, reduce
              ! first to lower bidiagonal form and then transform to upper
              ! bidiagonal
              if( ku>0_ilp ) then
                 ml0 = 1_ilp
                 mu0 = 2_ilp
              else
                 ml0 = 2_ilp
                 mu0 = 1_ilp
              end if
              ! wherever possible, plane rotations are generated and applied in
              ! vector operations of length nr over the index set j1:j2:klu1.
              ! the complex sines of the plane rotations are stored in work,
              ! and the real cosines in rwork.
              klm = min( m-1, kl )
              kun = min( n-1, ku )
              kb = klm + kun
              kb1 = kb + 1_ilp
              inca = kb1*ldab
              nr = 0_ilp
              j1 = klm + 2_ilp
              j2 = 1_ilp - kun
              loop_90: do i = 1, minmn
                 ! reduce i-th column and i-th row of matrix to bidiagonal form
                 ml = klm + 1_ilp
                 mu = kun + 1_ilp
                 loop_80: do kk = 1, kb
                    j1 = j1 + kb
                    j2 = j2 + kb
                    ! generate plane rotations to annihilate nonzero elements
                    ! which have been created below the band
                    if( nr>0_ilp )call stdlib_zlargv( nr, ab( klu1, j1-klm-1 ), inca,work( j1 ), kb1, &
                              rwork( j1 ), kb1 )
                    ! apply plane rotations from the left
                    do l = 1, kb
                       if( j2-klm+l-1>n ) then
                          nrt = nr - 1_ilp
                       else
                          nrt = nr
                       end if
                       if( nrt>0_ilp )call stdlib_zlartv( nrt, ab( klu1-l, j1-klm+l-1 ), inca,ab( &
                                 klu1-l+1, j1-klm+l-1 ), inca,rwork( j1 ), work( j1 ), kb1 )
                    end do
                    if( ml>ml0 ) then
                       if( ml<=m-i+1 ) then
                          ! generate plane rotation to annihilate a(i+ml-1,i)
                          ! within the band, and apply rotation from the left
                          call stdlib_zlartg( ab( ku+ml-1, i ), ab( ku+ml, i ),rwork( i+ml-1 ), &
                                    work( i+ml-1 ), ra )
                          ab( ku+ml-1, i ) = ra
                          if( i<n )call stdlib_zrot( min( ku+ml-2, n-i ),ab( ku+ml-2, i+1 ), ldab-&
                                    1_ilp,ab( ku+ml-1, i+1 ), ldab-1,rwork( i+ml-1 ), work( i+ml-1 ) )
                       end if
                       nr = nr + 1_ilp
                       j1 = j1 - kb1
                    end if
                    if( wantq ) then
                       ! accumulate product of plane rotations in q
                       do j = j1, j2, kb1
                          call stdlib_zrot( m, q( 1_ilp, j-1 ), 1_ilp, q( 1_ilp, j ), 1_ilp,rwork( j ), conjg( &
                                    work( j ) ) )
                       end do
                    end if
                    if( wantc ) then
                       ! apply plane rotations to c
                       do j = j1, j2, kb1
                          call stdlib_zrot( ncc, c( j-1, 1_ilp ), ldc, c( j, 1_ilp ), ldc,rwork( j ), &
                                    work( j ) )
                       end do
                    end if
                    if( j2+kun>n ) then
                       ! adjust j2 to keep within the bounds of the matrix
                       nr = nr - 1_ilp
                       j2 = j2 - kb1
                    end if
                    do j = j1, j2, kb1
                       ! create nonzero element a(j-1,j+ku) above the band
                       ! and store it in work(n+1:2*n)
                       work( j+kun ) = work( j )*ab( 1_ilp, j+kun )
                       ab( 1_ilp, j+kun ) = rwork( j )*ab( 1_ilp, j+kun )
                    end do
                    ! generate plane rotations to annihilate nonzero elements
                    ! which have been generated above the band
                    if( nr>0_ilp )call stdlib_zlargv( nr, ab( 1_ilp, j1+kun-1 ), inca,work( j1+kun ), kb1,&
                               rwork( j1+kun ),kb1 )
                    ! apply plane rotations from the right
                    do l = 1, kb
                       if( j2+l-1>m ) then
                          nrt = nr - 1_ilp
                       else
                          nrt = nr
                       end if
                       if( nrt>0_ilp )call stdlib_zlartv( nrt, ab( l+1, j1+kun-1 ), inca,ab( l, j1+&
                                 kun ), inca,rwork( j1+kun ), work( j1+kun ), kb1 )
                    end do
                    if( ml==ml0 .and. mu>mu0 ) then
                       if( mu<=n-i+1 ) then
                          ! generate plane rotation to annihilate a(i,i+mu-1)
                          ! within the band, and apply rotation from the right
                          call stdlib_zlartg( ab( ku-mu+3, i+mu-2 ),ab( ku-mu+2, i+mu-1 ),rwork( &
                                    i+mu-1 ), work( i+mu-1 ), ra )
                          ab( ku-mu+3, i+mu-2 ) = ra
                          call stdlib_zrot( min( kl+mu-2, m-i ),ab( ku-mu+4, i+mu-2 ), 1_ilp,ab( ku-&
                                    mu+3, i+mu-1 ), 1_ilp,rwork( i+mu-1 ), work( i+mu-1 ) )
                       end if
                       nr = nr + 1_ilp
                       j1 = j1 - kb1
                    end if
                    if( wantpt ) then
                       ! accumulate product of plane rotations in p**h
                       do j = j1, j2, kb1
                          call stdlib_zrot( n, pt( j+kun-1, 1_ilp ), ldpt,pt( j+kun, 1_ilp ), ldpt, rwork(&
                                     j+kun ),conjg( work( j+kun ) ) )
                       end do
                    end if
                    if( j2+kb>m ) then
                       ! adjust j2 to keep within the bounds of the matrix
                       nr = nr - 1_ilp
                       j2 = j2 - kb1
                    end if
                    do j = j1, j2, kb1
                       ! create nonzero element a(j+kl+ku,j+ku-1) below the
                       ! band and store it in work(1:n)
                       work( j+kb ) = work( j+kun )*ab( klu1, j+kun )
                       ab( klu1, j+kun ) = rwork( j+kun )*ab( klu1, j+kun )
                    end do
                    if( ml>ml0 ) then
                       ml = ml - 1_ilp
                    else
                       mu = mu - 1_ilp
                    end if
                 end do loop_80
              end do loop_90
           end if
           if( ku==0_ilp .and. kl>0_ilp ) then
              ! a has been reduced to complex lower bidiagonal form
              ! transform lower bidiagonal form to upper bidiagonal by applying
              ! plane rotations from the left, overwriting superdiagonal
              ! elements on subdiagonal elements
              do i = 1, min( m-1, n )
                 call stdlib_zlartg( ab( 1_ilp, i ), ab( 2_ilp, i ), rc, rs, ra )
                 ab( 1_ilp, i ) = ra
                 if( i<n ) then
                    ab( 2_ilp, i ) = rs*ab( 1_ilp, i+1 )
                    ab( 1_ilp, i+1 ) = rc*ab( 1_ilp, i+1 )
                 end if
                 if( wantq )call stdlib_zrot( m, q( 1_ilp, i ), 1_ilp, q( 1_ilp, i+1 ), 1_ilp, rc,conjg( rs ) )
                           
                 if( wantc )call stdlib_zrot( ncc, c( i, 1_ilp ), ldc, c( i+1, 1_ilp ), ldc, rc,rs )
                           
              end do
           else
              ! a has been reduced to complex upper bidiagonal form or is
              ! diagonal
              if( ku>0_ilp .and. m<n ) then
                 ! annihilate a(m,m+1) by applying plane rotations from the
                 ! right
                 rb = ab( ku, m+1 )
                 do i = m, 1, -1
                    call stdlib_zlartg( ab( ku+1, i ), rb, rc, rs, ra )
                    ab( ku+1, i ) = ra
                    if( i>1_ilp ) then
                       rb = -conjg( rs )*ab( ku, i )
                       ab( ku, i ) = rc*ab( ku, i )
                    end if
                    if( wantpt )call stdlib_zrot( n, pt( i, 1_ilp ), ldpt, pt( m+1, 1_ilp ), ldpt,rc, &
                              conjg( rs ) )
                 end do
              end if
           end if
           ! make diagonal and superdiagonal elements real, storing them in d
           ! and e
           t = ab( ku+1, 1_ilp )
           loop_120: do i = 1, minmn
              abst = abs( t )
              d( i ) = abst
              if( abst/=zero ) then
                 t = t / abst
              else
                 t = cone
              end if
              if( wantq )call stdlib_zscal( m, t, q( 1_ilp, i ), 1_ilp )
              if( wantc )call stdlib_zscal( ncc, conjg( t ), c( i, 1_ilp ), ldc )
              if( i<minmn ) then
                 if( ku==0_ilp .and. kl==0_ilp ) then
                    e( i ) = zero
                    t = ab( 1_ilp, i+1 )
                 else
                    if( ku==0_ilp ) then
                       t = ab( 2_ilp, i )*conjg( t )
                    else
                       t = ab( ku, i+1 )*conjg( t )
                    end if
                    abst = abs( t )
                    e( i ) = abst
                    if( abst/=zero ) then
                       t = t / abst
                    else
                       t = cone
                    end if
                    if( wantpt )call stdlib_zscal( n, t, pt( i+1, 1_ilp ), ldpt )
                    t = ab( ku+1, i+1 )*conjg( t )
                 end if
              end if
           end do loop_120
           return
     end subroutine stdlib_zgbbrd




     pure module subroutine stdlib_sgsvj0( jobv, m, n, a, lda, d, sva, mv, v, ldv, eps,sfmin, tol, &
     !! SGSVJ0 is called from SGESVJ as a pre-processor and that is its main
     !! purpose. It applies Jacobi rotations in the same way as SGESVJ does, but
     !! it does not check convergence (stopping criterion). Few tuning
     !! parameters (marked by [TP]) are available for the implementer.
               nsweep, work, lwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldv, lwork, m, mv, n, nsweep
           real(sp), intent(in) :: eps, sfmin, tol
           character, intent(in) :: jobv
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*), sva(n), d(n), v(ldv,*)
           real(sp), intent(out) :: work(lwork)
        ! =====================================================================
           
           ! Local Scalars 
           real(sp) :: aapp, aapp0, aapq, aaqq, apoaq, aqoap, big, bigtheta, cs, mxaapq, mxsinj, &
                     rootbig, rooteps, rootsfmin, roottol, small, sn, t, temp1, theta, thsign
           integer(ilp) :: blskip, emptsw, i, ibr, ierr, igl, ijblsk, ir1, iswrot, jbc, jgl, kbl, &
                     lkahead, mvl, nbl, notrot, p, pskipped, q, rowskip, swband
           logical(lk) :: applv, rotok, rsvec
           ! Local Arrays 
           real(sp) :: fastr(5_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           applv = stdlib_lsame( jobv, 'A' )
           rsvec = stdlib_lsame( jobv, 'V' )
           if( .not.( rsvec .or. applv .or. stdlib_lsame( jobv, 'N' ) ) ) then
              info = -1_ilp
           else if( m<0_ilp ) then
              info = -2_ilp
           else if( ( n<0_ilp ) .or. ( n>m ) ) then
              info = -3_ilp
           else if( lda<m ) then
              info = -5_ilp
           else if( ( rsvec.or.applv ) .and. ( mv<0_ilp ) ) then
              info = -8_ilp
           else if( ( rsvec.and.( ldv<n ) ).or.( applv.and.( ldv<mv ) ) ) then
              info = -10_ilp
           else if( tol<=eps ) then
              info = -13_ilp
           else if( nsweep<0_ilp ) then
              info = -14_ilp
           else if( lwork<m ) then
              info = -16_ilp
           else
              info = 0_ilp
           end if
           ! #:(
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SGSVJ0', -info )
              return
           end if
           if( rsvec ) then
              mvl = n
           else if( applv ) then
              mvl = mv
           end if
           rsvec = rsvec .or. applv
           rooteps = sqrt( eps )
           rootsfmin = sqrt( sfmin )
           small = sfmin / eps
           big = one / sfmin
           rootbig = one / rootsfmin
           bigtheta = one / rooteps
           roottol = sqrt( tol )
           ! .. row-cyclic jacobi svd algorithm with column pivoting ..
           emptsw = ( n*( n-1 ) ) / 2_ilp
           notrot = 0_ilp
           fastr( 1_ilp ) = zero
           ! .. row-cyclic pivot strategy with de rijk's pivoting ..
           swband = 0_ilp
      ! [tp] swband is a tuning parameter. it is meaningful and effective
           ! if stdlib_sgesvj is used as a computational routine in the preconditioned
           ! jacobi svd algorithm stdlib_sgesvj. for sweeps i=1:swband the procedure
           ! ......
           kbl = min( 8_ilp, n )
      ! [tp] kbl is a tuning parameter that defines the tile size in the
           ! tiling of the p-q loops of pivot pairs. in general, an optimal
           ! value of kbl depends on the matrix dimensions and on the
           ! parameters of the computer's memory.
           nbl = n / kbl
           if( ( nbl*kbl )/=n )nbl = nbl + 1_ilp
           blskip = ( kbl**2_ilp ) + 1_ilp
      ! [tp] blkskip is a tuning parameter that depends on swband and kbl.
           rowskip = min( 5_ilp, kbl )
      ! [tp] rowskip is a tuning parameter.
           lkahead = 1_ilp
      ! [tp] lkahead is a tuning parameter.
           swband = 0_ilp
           pskipped = 0_ilp
           loop_1993: do i = 1, nsweep
           ! .. go go go ...
              mxaapq = zero
              mxsinj = zero
              iswrot = 0_ilp
              notrot = 0_ilp
              pskipped = 0_ilp
              loop_2000: do ibr = 1, nbl
                 igl = ( ibr-1 )*kbl + 1_ilp
                 loop_1002: do ir1 = 0, min( lkahead, nbl-ibr )
                    igl = igl + ir1*kbl
                    loop_2001: do p = igl, min( igl+kbl-1, n-1 )
           ! .. de rijk's pivoting
                       q = stdlib_isamax( n-p+1, sva( p ), 1_ilp ) + p - 1_ilp
                       if( p/=q ) then
                          call stdlib_sswap( m, a( 1_ilp, p ), 1_ilp, a( 1_ilp, q ), 1_ilp )
                          if( rsvec )call stdlib_sswap( mvl, v( 1_ilp, p ), 1_ilp,v( 1_ilp, q ), 1_ilp )
                          temp1 = sva( p )
                          sva( p ) = sva( q )
                          sva( q ) = temp1
                          temp1 = d( p )
                          d( p ) = d( q )
                          d( q ) = temp1
                       end if
                       if( ir1==0_ilp ) then
              ! column norms are periodically updated by explicit
              ! norm computation.
              ! caveat:
              ! some blas implementations compute stdlib_snrm2(m,a(1,p),1)
              ! as sqrt(stdlib_sdot(m,a(1,p),1,a(1,p),1)), which may result in
              ! overflow for ||a(:,p)||_2 > sqrt(overflow_threshold), and
              ! underflow for ||a(:,p)||_2 < sqrt(underflow_threshold).
              ! hence, stdlib_snrm2 cannot be trusted, not even in the case when
              ! the true norm is far from the under(over)flow boundaries.
              ! if properly implemented stdlib_snrm2 is available, the if-then-else
              ! below should read "aapp = stdlib_snrm2( m, a(1,p), 1 ) * d(p)".
                          if( ( sva( p )<rootbig ) .and.( sva( p )>rootsfmin ) ) then
                             sva( p ) = stdlib_snrm2( m, a( 1_ilp, p ), 1_ilp )*d( p )
                          else
                             temp1 = zero
                             aapp = one
                             call stdlib_slassq( m, a( 1_ilp, p ), 1_ilp, temp1, aapp )
                             sva( p ) = temp1*sqrt( aapp )*d( p )
                          end if
                          aapp = sva( p )
                       else
                          aapp = sva( p )
                       end if
                       if( aapp>zero ) then
                          pskipped = 0_ilp
                          loop_2002: do q = p + 1, min( igl+kbl-1, n )
                             aaqq = sva( q )
                             if( aaqq>zero ) then
                                aapp0 = aapp
                                if( aaqq>=one ) then
                                   rotok = ( small*aapp )<=aaqq
                                   if( aapp<( big / aaqq ) ) then
                                      aapq = ( stdlib_sdot( m, a( 1_ilp, p ), 1_ilp, a( 1_ilp,q ), 1_ilp )*d( p )&
                                                *d( q ) / aaqq )/ aapp
                                   else
                                      call stdlib_scopy( m, a( 1_ilp, p ), 1_ilp, work, 1_ilp )
                                      call stdlib_slascl( 'G', 0_ilp, 0_ilp, aapp, d( p ),m, 1_ilp, work, lda,&
                                                 ierr )
                                      aapq = stdlib_sdot( m, work, 1_ilp, a( 1_ilp, q ),1_ilp )*d( q ) / &
                                                aaqq
                                   end if
                                else
                                   rotok = aapp<=( aaqq / small )
                                   if( aapp>( small / aaqq ) ) then
                                      aapq = ( stdlib_sdot( m, a( 1_ilp, p ), 1_ilp, a( 1_ilp,q ), 1_ilp )*d( p )&
                                                *d( q ) / aaqq )/ aapp
                                   else
                                      call stdlib_scopy( m, a( 1_ilp, q ), 1_ilp, work, 1_ilp )
                                      call stdlib_slascl( 'G', 0_ilp, 0_ilp, aaqq, d( q ),m, 1_ilp, work, lda,&
                                                 ierr )
                                      aapq = stdlib_sdot( m, work, 1_ilp, a( 1_ilp, p ),1_ilp )*d( p ) / &
                                                aapp
                                   end if
                                end if
                                mxaapq = max( mxaapq, abs( aapq ) )
              ! to rotate or not to rotate, that is the question ...
                                if( abs( aapq )>tol ) then
                 ! Rotate
                 ! rotated = rotated + one
                                   if( ir1==0_ilp ) then
                                      notrot = 0_ilp
                                      pskipped = 0_ilp
                                      iswrot = iswrot + 1_ilp
                                   end if
                                   if( rotok ) then
                                      aqoap = aaqq / aapp
                                      apoaq = aapp / aaqq
                                      theta = -half*abs( aqoap-apoaq ) / aapq
                                      if( abs( theta )>bigtheta ) then
                                         t = half / theta
                                         fastr( 3_ilp ) = t*d( p ) / d( q )
                                         fastr( 4_ilp ) = -t*d( q ) / d( p )
                                         call stdlib_srotm( m, a( 1_ilp, p ), 1_ilp,a( 1_ilp, q ), 1_ilp, fastr )
                                                   
                                         if( rsvec )call stdlib_srotm( mvl,v( 1_ilp, p ), 1_ilp,v( 1_ilp, q ),&
                                                    1_ilp,fastr )
                                         sva( q ) = aaqq*sqrt( max( zero,one+t*apoaq*aapq ) )
                                                   
                                         aapp = aapp*sqrt( max( zero,one-t*aqoap*aapq ) )
                                         mxsinj = max( mxsinj, abs( t ) )
                                      else
                       ! Choose Correct Signum For Theta And Rotate
                                         thsign = -sign( one, aapq )
                                         t = one / ( theta+thsign*sqrt( one+theta*theta ) )
                                                   
                                         cs = sqrt( one / ( one+t*t ) )
                                         sn = t*cs
                                         mxsinj = max( mxsinj, abs( sn ) )
                                         sva( q ) = aaqq*sqrt( max( zero,one+t*apoaq*aapq ) )
                                                   
                                         aapp = aapp*sqrt( max( zero,one-t*aqoap*aapq ) )
                                         apoaq = d( p ) / d( q )
                                         aqoap = d( q ) / d( p )
                                         if( d( p )>=one ) then
                                            if( d( q )>=one ) then
                                               fastr( 3_ilp ) = t*apoaq
                                               fastr( 4_ilp ) = -t*aqoap
                                               d( p ) = d( p )*cs
                                               d( q ) = d( q )*cs
                                               call stdlib_srotm( m, a( 1_ilp, p ), 1_ilp,a( 1_ilp, q ), 1_ilp,&
                                                         fastr )
                                               if( rsvec )call stdlib_srotm( mvl,v( 1_ilp, p ), 1_ilp, v( &
                                                         1_ilp, q ),1_ilp, fastr )
                                            else
                                               call stdlib_saxpy( m, -t*aqoap,a( 1_ilp, q ), 1_ilp,a( 1_ilp, &
                                                         p ), 1_ilp )
                                               call stdlib_saxpy( m, cs*sn*apoaq,a( 1_ilp, p ), 1_ilp,a( &
                                                         1_ilp, q ), 1_ilp )
                                               d( p ) = d( p )*cs
                                               d( q ) = d( q ) / cs
                                               if( rsvec ) then
                                                  call stdlib_saxpy( mvl, -t*aqoap,v( 1_ilp, q ), 1_ilp,v(&
                                                             1_ilp, p ), 1_ilp )
                                                  call stdlib_saxpy( mvl,cs*sn*apoaq,v( 1_ilp, p ), 1_ilp,&
                                                            v( 1_ilp, q ), 1_ilp )
                                               end if
                                            end if
                                         else
                                            if( d( q )>=one ) then
                                               call stdlib_saxpy( m, t*apoaq,a( 1_ilp, p ), 1_ilp,a( 1_ilp, q &
                                                         ), 1_ilp )
                                               call stdlib_saxpy( m, -cs*sn*aqoap,a( 1_ilp, q ), 1_ilp,a( &
                                                         1_ilp, p ), 1_ilp )
                                               d( p ) = d( p ) / cs
                                               d( q ) = d( q )*cs
                                               if( rsvec ) then
                                                  call stdlib_saxpy( mvl, t*apoaq,v( 1_ilp, p ), 1_ilp,v( &
                                                            1_ilp, q ), 1_ilp )
                                                  call stdlib_saxpy( mvl,-cs*sn*aqoap,v( 1_ilp, q ), &
                                                            1_ilp,v( 1_ilp, p ), 1_ilp )
                                               end if
                                            else
                                               if( d( p )>=d( q ) ) then
                                                  call stdlib_saxpy( m, -t*aqoap,a( 1_ilp, q ), 1_ilp,a( &
                                                            1_ilp, p ), 1_ilp )
                                                  call stdlib_saxpy( m, cs*sn*apoaq,a( 1_ilp, p ), 1_ilp,&
                                                            a( 1_ilp, q ), 1_ilp )
                                                  d( p ) = d( p )*cs
                                                  d( q ) = d( q ) / cs
                                                  if( rsvec ) then
                                                     call stdlib_saxpy( mvl,-t*aqoap,v( 1_ilp, q ), 1_ilp,&
                                                               v( 1_ilp, p ), 1_ilp )
                                                     call stdlib_saxpy( mvl,cs*sn*apoaq,v( 1_ilp, p ),&
                                                                1_ilp,v( 1_ilp, q ), 1_ilp )
                                                  end if
                                               else
                                                  call stdlib_saxpy( m, t*apoaq,a( 1_ilp, p ), 1_ilp,a( 1_ilp,&
                                                             q ), 1_ilp )
                                                  call stdlib_saxpy( m,-cs*sn*aqoap,a( 1_ilp, q ), 1_ilp,&
                                                            a( 1_ilp, p ), 1_ilp )
                                                  d( p ) = d( p ) / cs
                                                  d( q ) = d( q )*cs
                                                  if( rsvec ) then
                                                     call stdlib_saxpy( mvl,t*apoaq, v( 1_ilp, p ),1_ilp, &
                                                               v( 1_ilp, q ), 1_ilp )
                                                     call stdlib_saxpy( mvl,-cs*sn*aqoap,v( 1_ilp, q )&
                                                               , 1_ilp,v( 1_ilp, p ), 1_ilp )
                                                  end if
                                               end if
                                            end if
                                         end if
                                      end if
                                   else
                    ! .. have to use modified gram-schmidt like transformation
                                      call stdlib_scopy( m, a( 1_ilp, p ), 1_ilp, work, 1_ilp )
                                      call stdlib_slascl( 'G', 0_ilp, 0_ilp, aapp, one, m,1_ilp, work, lda, &
                                                ierr )
                                      call stdlib_slascl( 'G', 0_ilp, 0_ilp, aaqq, one, m,1_ilp, a( 1_ilp, q ), &
                                                lda, ierr )
                                      temp1 = -aapq*d( p ) / d( q )
                                      call stdlib_saxpy( m, temp1, work, 1_ilp,a( 1_ilp, q ), 1_ilp )
                                      call stdlib_slascl( 'G', 0_ilp, 0_ilp, one, aaqq, m,1_ilp, a( 1_ilp, q ), &
                                                lda, ierr )
                                      sva( q ) = aaqq*sqrt( max( zero,one-aapq*aapq ) )
                                      mxsinj = max( mxsinj, sfmin )
                                   end if
                 ! end if rotok then ... else
                 ! in the case of cancellation in updating sva(q), sva(p)
                 ! recompute sva(q), sva(p).
                                   if( ( sva( q ) / aaqq )**2_ilp<=rooteps )then
                                      if( ( aaqq<rootbig ) .and.( aaqq>rootsfmin ) ) then
                                         sva( q ) = stdlib_snrm2( m, a( 1_ilp, q ), 1_ilp )*d( q )
                                      else
                                         t = zero
                                         aaqq = one
                                         call stdlib_slassq( m, a( 1_ilp, q ), 1_ilp, t,aaqq )
                                         sva( q ) = t*sqrt( aaqq )*d( q )
                                      end if
                                   end if
                                   if( ( aapp / aapp0 )<=rooteps ) then
                                      if( ( aapp<rootbig ) .and.( aapp>rootsfmin ) ) then
                                         aapp = stdlib_snrm2( m, a( 1_ilp, p ), 1_ilp )*d( p )
                                      else
                                         t = zero
                                         aapp = one
                                         call stdlib_slassq( m, a( 1_ilp, p ), 1_ilp, t,aapp )
                                         aapp = t*sqrt( aapp )*d( p )
                                      end if
                                      sva( p ) = aapp
                                   end if
                                else
              ! a(:,p) and a(:,q) already numerically orthogonal
                                   if( ir1==0_ilp )notrot = notrot + 1_ilp
                                   pskipped = pskipped + 1_ilp
                                end if
                             else
              ! a(:,q) is zero column
                                if( ir1==0_ilp )notrot = notrot + 1_ilp
                                pskipped = pskipped + 1_ilp
                             end if
                             if( ( i<=swband ) .and.( pskipped>rowskip ) ) then
                                if( ir1==0_ilp )aapp = -aapp
                                notrot = 0_ilp
                                go to 2103
                             end if
                          end do loop_2002
           ! end q-loop
           2103 continue
           ! bailed out of q-loop
                          sva( p ) = aapp
                       else
                          sva( p ) = aapp
                          if( ( ir1==0_ilp ) .and. ( aapp==zero ) )notrot = notrot + min( igl+kbl-1, &
                                    n ) - p
                       end if
                    end do loop_2001
           ! end of the p-loop
           ! end of doing the block ( ibr, ibr )
                 end do loop_1002
           ! end of ir1-loop
      ! ........................................................
       ! ... go to the off diagonal blocks
                 igl = ( ibr-1 )*kbl + 1_ilp
                 loop_2010: do jbc = ibr + 1, nbl
                    jgl = ( jbc-1 )*kbl + 1_ilp
              ! doing the block at ( ibr, jbc )
                    ijblsk = 0_ilp
                    loop_2100: do p = igl, min( igl+kbl-1, n )
                       aapp = sva( p )
                       if( aapp>zero ) then
                          pskipped = 0_ilp
                          loop_2200: do q = jgl, min( jgl+kbl-1, n )
                             aaqq = sva( q )
                             if( aaqq>zero ) then
                                aapp0 = aapp
           ! M X 2 Jacobi Svd 
              ! Safe Gram Matrix Computation 
                                if( aaqq>=one ) then
                                   if( aapp>=aaqq ) then
                                      rotok = ( small*aapp )<=aaqq
                                   else
                                      rotok = ( small*aaqq )<=aapp
                                   end if
                                   if( aapp<( big / aaqq ) ) then
                                      aapq = ( stdlib_sdot( m, a( 1_ilp, p ), 1_ilp, a( 1_ilp,q ), 1_ilp )*d( p )&
                                                *d( q ) / aaqq )/ aapp
                                   else
                                      call stdlib_scopy( m, a( 1_ilp, p ), 1_ilp, work, 1_ilp )
                                      call stdlib_slascl( 'G', 0_ilp, 0_ilp, aapp, d( p ),m, 1_ilp, work, lda,&
                                                 ierr )
                                      aapq = stdlib_sdot( m, work, 1_ilp, a( 1_ilp, q ),1_ilp )*d( q ) / &
                                                aaqq
                                   end if
                                else
                                   if( aapp>=aaqq ) then
                                      rotok = aapp<=( aaqq / small )
                                   else
                                      rotok = aaqq<=( aapp / small )
                                   end if
                                   if( aapp>( small / aaqq ) ) then
                                      aapq = ( stdlib_sdot( m, a( 1_ilp, p ), 1_ilp, a( 1_ilp,q ), 1_ilp )*d( p )&
                                                *d( q ) / aaqq )/ aapp
                                   else
                                      call stdlib_scopy( m, a( 1_ilp, q ), 1_ilp, work, 1_ilp )
                                      call stdlib_slascl( 'G', 0_ilp, 0_ilp, aaqq, d( q ),m, 1_ilp, work, lda,&
                                                 ierr )
                                      aapq = stdlib_sdot( m, work, 1_ilp, a( 1_ilp, p ),1_ilp )*d( p ) / &
                                                aapp
                                   end if
                                end if
                                mxaapq = max( mxaapq, abs( aapq ) )
              ! to rotate or not to rotate, that is the question ...
                                if( abs( aapq )>tol ) then
                                   notrot = 0_ilp
                 ! rotated  = rotated + 1
                                   pskipped = 0_ilp
                                   iswrot = iswrot + 1_ilp
                                   if( rotok ) then
                                      aqoap = aaqq / aapp
                                      apoaq = aapp / aaqq
                                      theta = -half*abs( aqoap-apoaq ) / aapq
                                      if( aaqq>aapp0 )theta = -theta
                                      if( abs( theta )>bigtheta ) then
                                         t = half / theta
                                         fastr( 3_ilp ) = t*d( p ) / d( q )
                                         fastr( 4_ilp ) = -t*d( q ) / d( p )
                                         call stdlib_srotm( m, a( 1_ilp, p ), 1_ilp,a( 1_ilp, q ), 1_ilp, fastr )
                                                   
                                         if( rsvec )call stdlib_srotm( mvl,v( 1_ilp, p ), 1_ilp,v( 1_ilp, q ),&
                                                    1_ilp,fastr )
                                         sva( q ) = aaqq*sqrt( max( zero,one+t*apoaq*aapq ) )
                                                   
                                         aapp = aapp*sqrt( max( zero,one-t*aqoap*aapq ) )
                                         mxsinj = max( mxsinj, abs( t ) )
                                      else
                       ! Choose Correct Signum For Theta And Rotate
                                         thsign = -sign( one, aapq )
                                         if( aaqq>aapp0 )thsign = -thsign
                                         t = one / ( theta+thsign*sqrt( one+theta*theta ) )
                                                   
                                         cs = sqrt( one / ( one+t*t ) )
                                         sn = t*cs
                                         mxsinj = max( mxsinj, abs( sn ) )
                                         sva( q ) = aaqq*sqrt( max( zero,one+t*apoaq*aapq ) )
                                                   
                                         aapp = aapp*sqrt( max( zero,one-t*aqoap*aapq ) )
                                         apoaq = d( p ) / d( q )
                                         aqoap = d( q ) / d( p )
                                         if( d( p )>=one ) then
                                            if( d( q )>=one ) then
                                               fastr( 3_ilp ) = t*apoaq
                                               fastr( 4_ilp ) = -t*aqoap
                                               d( p ) = d( p )*cs
                                               d( q ) = d( q )*cs
                                               call stdlib_srotm( m, a( 1_ilp, p ), 1_ilp,a( 1_ilp, q ), 1_ilp,&
                                                         fastr )
                                               if( rsvec )call stdlib_srotm( mvl,v( 1_ilp, p ), 1_ilp, v( &
                                                         1_ilp, q ),1_ilp, fastr )
                                            else
                                               call stdlib_saxpy( m, -t*aqoap,a( 1_ilp, q ), 1_ilp,a( 1_ilp, &
                                                         p ), 1_ilp )
                                               call stdlib_saxpy( m, cs*sn*apoaq,a( 1_ilp, p ), 1_ilp,a( &
                                                         1_ilp, q ), 1_ilp )
                                               if( rsvec ) then
                                                  call stdlib_saxpy( mvl, -t*aqoap,v( 1_ilp, q ), 1_ilp,v(&
                                                             1_ilp, p ), 1_ilp )
                                                  call stdlib_saxpy( mvl,cs*sn*apoaq,v( 1_ilp, p ), 1_ilp,&
                                                            v( 1_ilp, q ), 1_ilp )
                                               end if
                                               d( p ) = d( p )*cs
                                               d( q ) = d( q ) / cs
                                            end if
                                         else
                                            if( d( q )>=one ) then
                                               call stdlib_saxpy( m, t*apoaq,a( 1_ilp, p ), 1_ilp,a( 1_ilp, q &
                                                         ), 1_ilp )
                                               call stdlib_saxpy( m, -cs*sn*aqoap,a( 1_ilp, q ), 1_ilp,a( &
                                                         1_ilp, p ), 1_ilp )
                                               if( rsvec ) then
                                                  call stdlib_saxpy( mvl, t*apoaq,v( 1_ilp, p ), 1_ilp,v( &
                                                            1_ilp, q ), 1_ilp )
                                                  call stdlib_saxpy( mvl,-cs*sn*aqoap,v( 1_ilp, q ), &
                                                            1_ilp,v( 1_ilp, p ), 1_ilp )
                                               end if
                                               d( p ) = d( p ) / cs
                                               d( q ) = d( q )*cs
                                            else
                                               if( d( p )>=d( q ) ) then
                                                  call stdlib_saxpy( m, -t*aqoap,a( 1_ilp, q ), 1_ilp,a( &
                                                            1_ilp, p ), 1_ilp )
                                                  call stdlib_saxpy( m, cs*sn*apoaq,a( 1_ilp, p ), 1_ilp,&
                                                            a( 1_ilp, q ), 1_ilp )
                                                  d( p ) = d( p )*cs
                                                  d( q ) = d( q ) / cs
                                                  if( rsvec ) then
                                                     call stdlib_saxpy( mvl,-t*aqoap,v( 1_ilp, q ), 1_ilp,&
                                                               v( 1_ilp, p ), 1_ilp )
                                                     call stdlib_saxpy( mvl,cs*sn*apoaq,v( 1_ilp, p ),&
                                                                1_ilp,v( 1_ilp, q ), 1_ilp )
                                                  end if
                                               else
                                                  call stdlib_saxpy( m, t*apoaq,a( 1_ilp, p ), 1_ilp,a( 1_ilp,&
                                                             q ), 1_ilp )
                                                  call stdlib_saxpy( m,-cs*sn*aqoap,a( 1_ilp, q ), 1_ilp,&
                                                            a( 1_ilp, p ), 1_ilp )
                                                  d( p ) = d( p ) / cs
                                                  d( q ) = d( q )*cs
                                                  if( rsvec ) then
                                                     call stdlib_saxpy( mvl,t*apoaq, v( 1_ilp, p ),1_ilp, &
                                                               v( 1_ilp, q ), 1_ilp )
                                                     call stdlib_saxpy( mvl,-cs*sn*aqoap,v( 1_ilp, q )&
                                                               , 1_ilp,v( 1_ilp, p ), 1_ilp )
                                                  end if
                                               end if
                                            end if
                                         end if
                                      end if
                                   else
                                      if( aapp>aaqq ) then
                                         call stdlib_scopy( m, a( 1_ilp, p ), 1_ilp, work,1_ilp )
                                         call stdlib_slascl( 'G', 0_ilp, 0_ilp, aapp, one,m, 1_ilp, work, lda,&
                                                    ierr )
                                         call stdlib_slascl( 'G', 0_ilp, 0_ilp, aaqq, one,m, 1_ilp, a( 1_ilp, q ),&
                                                    lda,ierr )
                                         temp1 = -aapq*d( p ) / d( q )
                                         call stdlib_saxpy( m, temp1, work, 1_ilp,a( 1_ilp, q ), 1_ilp )
                                                   
                                         call stdlib_slascl( 'G', 0_ilp, 0_ilp, one, aaqq,m, 1_ilp, a( 1_ilp, q ),&
                                                    lda,ierr )
                                         sva( q ) = aaqq*sqrt( max( zero,one-aapq*aapq ) )
                                         mxsinj = max( mxsinj, sfmin )
                                      else
                                         call stdlib_scopy( m, a( 1_ilp, q ), 1_ilp, work,1_ilp )
                                         call stdlib_slascl( 'G', 0_ilp, 0_ilp, aaqq, one,m, 1_ilp, work, lda,&
                                                    ierr )
                                         call stdlib_slascl( 'G', 0_ilp, 0_ilp, aapp, one,m, 1_ilp, a( 1_ilp, p ),&
                                                    lda,ierr )
                                         temp1 = -aapq*d( q ) / d( p )
                                         call stdlib_saxpy( m, temp1, work, 1_ilp,a( 1_ilp, p ), 1_ilp )
                                                   
                                         call stdlib_slascl( 'G', 0_ilp, 0_ilp, one, aapp,m, 1_ilp, a( 1_ilp, p ),&
                                                    lda,ierr )
                                         sva( p ) = aapp*sqrt( max( zero,one-aapq*aapq ) )
                                         mxsinj = max( mxsinj, sfmin )
                                      end if
                                   end if
                 ! end if rotok then ... else
                 ! in the case of cancellation in updating sva(q)
                 ! .. recompute sva(q)
                                   if( ( sva( q ) / aaqq )**2_ilp<=rooteps )then
                                      if( ( aaqq<rootbig ) .and.( aaqq>rootsfmin ) ) then
                                         sva( q ) = stdlib_snrm2( m, a( 1_ilp, q ), 1_ilp )*d( q )
                                      else
                                         t = zero
                                         aaqq = one
                                         call stdlib_slassq( m, a( 1_ilp, q ), 1_ilp, t,aaqq )
                                         sva( q ) = t*sqrt( aaqq )*d( q )
                                      end if
                                   end if
                                   if( ( aapp / aapp0 )**2_ilp<=rooteps ) then
                                      if( ( aapp<rootbig ) .and.( aapp>rootsfmin ) ) then
                                         aapp = stdlib_snrm2( m, a( 1_ilp, p ), 1_ilp )*d( p )
                                      else
                                         t = zero
                                         aapp = one
                                         call stdlib_slassq( m, a( 1_ilp, p ), 1_ilp, t,aapp )
                                         aapp = t*sqrt( aapp )*d( p )
                                      end if
                                      sva( p ) = aapp
                                   end if
                    ! end of ok rotation
                                else
                                   notrot = notrot + 1_ilp
                                   pskipped = pskipped + 1_ilp
                                   ijblsk = ijblsk + 1_ilp
                                end if
                             else
                                notrot = notrot + 1_ilp
                                pskipped = pskipped + 1_ilp
                                ijblsk = ijblsk + 1_ilp
                             end if
                             if( ( i<=swband ) .and. ( ijblsk>=blskip ) )then
                                sva( p ) = aapp
                                notrot = 0_ilp
                                go to 2011
                             end if
                             if( ( i<=swband ) .and.( pskipped>rowskip ) ) then
                                aapp = -aapp
                                notrot = 0_ilp
                                go to 2203
                             end if
                          end do loop_2200
              ! end of the q-loop
              2203 continue
                          sva( p ) = aapp
                       else
                          if( aapp==zero )notrot = notrot +min( jgl+kbl-1, n ) - jgl + 1_ilp
                          if( aapp<zero )notrot = 0_ilp
                       end if
                    end do loop_2100
           ! end of the p-loop
                 end do loop_2010
           ! end of the jbc-loop
           2011 continue
      ! 2011 bailed out of the jbc-loop
                 do p = igl, min( igl+kbl-1, n )
                    sva( p ) = abs( sva( p ) )
                 end do
              end do loop_2000
      ! 2000 :: end of the ibr-loop
           ! .. update sva(n)
              if( ( sva( n )<rootbig ) .and. ( sva( n )>rootsfmin ) )then
                 sva( n ) = stdlib_snrm2( m, a( 1_ilp, n ), 1_ilp )*d( n )
              else
                 t = zero
                 aapp = one
                 call stdlib_slassq( m, a( 1_ilp, n ), 1_ilp, t, aapp )
                 sva( n ) = t*sqrt( aapp )*d( n )
              end if
           ! additional steering devices
              if( ( i<swband ) .and. ( ( mxaapq<=roottol ) .or.( iswrot<=n ) ) )swband = i
              if( ( i>swband+1 ) .and. ( mxaapq<real( n,KIND=sp)*tol ) .and.( real( n,KIND=sp)&
                        *mxaapq*mxsinj<tol ) ) then
                 go to 1994
              end if
              if( notrot>=emptsw )go to 1994
           end do loop_1993
           ! end i=1:nsweep loop
       ! #:) reaching this point means that the procedure has completed the given
           ! number of iterations.
           info = nsweep - 1_ilp
           go to 1995
           1994 continue
       ! #:) reaching this point means that during the i-th sweep all pivots were
           ! below the given tolerance, causing early exit.
           info = 0_ilp
       ! #:) info = 0 confirms successful iterations.
       1995 continue
           ! sort the vector d.
           do p = 1, n - 1
              q = stdlib_isamax( n-p+1, sva( p ), 1_ilp ) + p - 1_ilp
              if( p/=q ) then
                 temp1 = sva( p )
                 sva( p ) = sva( q )
                 sva( q ) = temp1
                 temp1 = d( p )
                 d( p ) = d( q )
                 d( q ) = temp1
                 call stdlib_sswap( m, a( 1_ilp, p ), 1_ilp, a( 1_ilp, q ), 1_ilp )
                 if( rsvec )call stdlib_sswap( mvl, v( 1_ilp, p ), 1_ilp, v( 1_ilp, q ), 1_ilp )
              end if
           end do
           return
     end subroutine stdlib_sgsvj0

     pure module subroutine stdlib_dgsvj0( jobv, m, n, a, lda, d, sva, mv, v, ldv, eps,sfmin, tol, &
     !! DGSVJ0 is called from DGESVJ as a pre-processor and that is its main
     !! purpose. It applies Jacobi rotations in the same way as DGESVJ does, but
     !! it does not check convergence (stopping criterion). Few tuning
     !! parameters (marked by [TP]) are available for the implementer.
               nsweep, work, lwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldv, lwork, m, mv, n, nsweep
           real(dp), intent(in) :: eps, sfmin, tol
           character, intent(in) :: jobv
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*), sva(n), d(n), v(ldv,*)
           real(dp), intent(out) :: work(lwork)
        ! =====================================================================
           
           ! Local Scalars 
           real(dp) :: aapp, aapp0, aapq, aaqq, apoaq, aqoap, big, bigtheta, cs, mxaapq, mxsinj, &
                     rootbig, rooteps, rootsfmin, roottol, small, sn, t, temp1, theta, thsign
           integer(ilp) :: blskip, emptsw, i, ibr, ierr, igl, ijblsk, ir1, iswrot, jbc, jgl, kbl, &
                     lkahead, mvl, nbl, notrot, p, pskipped, q, rowskip, swband
           logical(lk) :: applv, rotok, rsvec
           ! Local Arrays 
           real(dp) :: fastr(5_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           applv = stdlib_lsame( jobv, 'A' )
           rsvec = stdlib_lsame( jobv, 'V' )
           if( .not.( rsvec .or. applv .or. stdlib_lsame( jobv, 'N' ) ) ) then
              info = -1_ilp
           else if( m<0_ilp ) then
              info = -2_ilp
           else if( ( n<0_ilp ) .or. ( n>m ) ) then
              info = -3_ilp
           else if( lda<m ) then
              info = -5_ilp
           else if( ( rsvec.or.applv ) .and. ( mv<0_ilp ) ) then
              info = -8_ilp
           else if( ( rsvec.and.( ldv<n ) ).or.( applv.and.( ldv<mv ) ) ) then
              info = -10_ilp
           else if( tol<=eps ) then
              info = -13_ilp
           else if( nsweep<0_ilp ) then
              info = -14_ilp
           else if( lwork<m ) then
              info = -16_ilp
           else
              info = 0_ilp
           end if
           ! #:(
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DGSVJ0', -info )
              return
           end if
           if( rsvec ) then
              mvl = n
           else if( applv ) then
              mvl = mv
           end if
           rsvec = rsvec .or. applv
           rooteps = sqrt( eps )
           rootsfmin = sqrt( sfmin )
           small = sfmin / eps
           big = one / sfmin
           rootbig = one / rootsfmin
           bigtheta = one / rooteps
           roottol = sqrt( tol )
           ! -#- row-cyclic jacobi svd algorithm with column pivoting -#-
           emptsw = ( n*( n-1 ) ) / 2_ilp
           notrot = 0_ilp
           fastr( 1_ilp ) = zero
           ! -#- row-cyclic pivot strategy with de rijk's pivoting -#-
           swband = 0_ilp
      ! [tp] swband is a tuning parameter. it is meaningful and effective
           ! if stdlib_sgesvj is used as a computational routine in the preconditioned
           ! jacobi svd algorithm stdlib_sgesvj. for sweeps i=1:swband the procedure
           ! ......
           kbl = min( 8_ilp, n )
      ! [tp] kbl is a tuning parameter that defines the tile size in the
           ! tiling of the p-q loops of pivot pairs. in general, an optimal
           ! value of kbl depends on the matrix dimensions and on the
           ! parameters of the computer's memory.
           nbl = n / kbl
           if( ( nbl*kbl )/=n )nbl = nbl + 1_ilp
           blskip = ( kbl**2_ilp ) + 1_ilp
      ! [tp] blkskip is a tuning parameter that depends on swband and kbl.
           rowskip = min( 5_ilp, kbl )
      ! [tp] rowskip is a tuning parameter.
           lkahead = 1_ilp
      ! [tp] lkahead is a tuning parameter.
           swband = 0_ilp
           pskipped = 0_ilp
           loop_1993: do i = 1, nsweep
           ! .. go go go ...
              mxaapq = zero
              mxsinj = zero
              iswrot = 0_ilp
              notrot = 0_ilp
              pskipped = 0_ilp
              loop_2000: do ibr = 1, nbl
                 igl = ( ibr-1 )*kbl + 1_ilp
                 loop_1002: do ir1 = 0, min( lkahead, nbl-ibr )
                    igl = igl + ir1*kbl
                    loop_2001: do p = igl, min( igl+kbl-1, n-1 )
           ! .. de rijk's pivoting
                       q = stdlib_idamax( n-p+1, sva( p ), 1_ilp ) + p - 1_ilp
                       if( p/=q ) then
                          call stdlib_dswap( m, a( 1_ilp, p ), 1_ilp, a( 1_ilp, q ), 1_ilp )
                          if( rsvec )call stdlib_dswap( mvl, v( 1_ilp, p ), 1_ilp,v( 1_ilp, q ), 1_ilp )
                          temp1 = sva( p )
                          sva( p ) = sva( q )
                          sva( q ) = temp1
                          temp1 = d( p )
                          d( p ) = d( q )
                          d( q ) = temp1
                       end if
                       if( ir1==0_ilp ) then
              ! column norms are periodically updated by explicit
              ! norm computation.
              ! caveat:
              ! some blas implementations compute stdlib_dnrm2(m,a(1,p),1)
              ! as sqrt(stdlib_ddot(m,a(1,p),1,a(1,p),1)), which may result in
              ! overflow for ||a(:,p)||_2 > sqrt(overflow_threshold), and
              ! underflow for ||a(:,p)||_2 < sqrt(underflow_threshold).
              ! hence, stdlib_dnrm2 cannot be trusted, not even in the case when
              ! the true norm is far from the under(over)flow boundaries.
              ! if properly implemented stdlib_dnrm2 is available, the if-then-else
              ! below should read "aapp = stdlib_dnrm2( m, a(1,p), 1 ) * d(p)".
                          if( ( sva( p )<rootbig ) .and.( sva( p )>rootsfmin ) ) then
                             sva( p ) = stdlib_dnrm2( m, a( 1_ilp, p ), 1_ilp )*d( p )
                          else
                             temp1 = zero
                             aapp = one
                             call stdlib_dlassq( m, a( 1_ilp, p ), 1_ilp, temp1, aapp )
                             sva( p ) = temp1*sqrt( aapp )*d( p )
                          end if
                          aapp = sva( p )
                       else
                          aapp = sva( p )
                       end if
                       if( aapp>zero ) then
                          pskipped = 0_ilp
                          loop_2002: do q = p + 1, min( igl+kbl-1, n )
                             aaqq = sva( q )
                             if( aaqq>zero ) then
                                aapp0 = aapp
                                if( aaqq>=one ) then
                                   rotok = ( small*aapp )<=aaqq
                                   if( aapp<( big / aaqq ) ) then
                                      aapq = ( stdlib_ddot( m, a( 1_ilp, p ), 1_ilp, a( 1_ilp,q ), 1_ilp )*d( p )&
                                                *d( q ) / aaqq )/ aapp
                                   else
                                      call stdlib_dcopy( m, a( 1_ilp, p ), 1_ilp, work, 1_ilp )
                                      call stdlib_dlascl( 'G', 0_ilp, 0_ilp, aapp, d( p ),m, 1_ilp, work, lda,&
                                                 ierr )
                                      aapq = stdlib_ddot( m, work, 1_ilp, a( 1_ilp, q ),1_ilp )*d( q ) / &
                                                aaqq
                                   end if
                                else
                                   rotok = aapp<=( aaqq / small )
                                   if( aapp>( small / aaqq ) ) then
                                      aapq = ( stdlib_ddot( m, a( 1_ilp, p ), 1_ilp, a( 1_ilp,q ), 1_ilp )*d( p )&
                                                *d( q ) / aaqq )/ aapp
                                   else
                                      call stdlib_dcopy( m, a( 1_ilp, q ), 1_ilp, work, 1_ilp )
                                      call stdlib_dlascl( 'G', 0_ilp, 0_ilp, aaqq, d( q ),m, 1_ilp, work, lda,&
                                                 ierr )
                                      aapq = stdlib_ddot( m, work, 1_ilp, a( 1_ilp, p ),1_ilp )*d( p ) / &
                                                aapp
                                   end if
                                end if
                                mxaapq = max( mxaapq, abs( aapq ) )
              ! to rotate or not to rotate, that is the question ...
                                if( abs( aapq )>tol ) then
                 ! Rotate
                 ! rotated = rotated + one
                                   if( ir1==0_ilp ) then
                                      notrot = 0_ilp
                                      pskipped = 0_ilp
                                      iswrot = iswrot + 1_ilp
                                   end if
                                   if( rotok ) then
                                      aqoap = aaqq / aapp
                                      apoaq = aapp / aaqq
                                      theta = -half*abs( aqoap-apoaq )/aapq
                                      if( abs( theta )>bigtheta ) then
                                         t = half / theta
                                         fastr( 3_ilp ) = t*d( p ) / d( q )
                                         fastr( 4_ilp ) = -t*d( q ) / d( p )
                                         call stdlib_drotm( m, a( 1_ilp, p ), 1_ilp,a( 1_ilp, q ), 1_ilp, fastr )
                                                   
                                         if( rsvec )call stdlib_drotm( mvl,v( 1_ilp, p ), 1_ilp,v( 1_ilp, q ),&
                                                    1_ilp,fastr )
                                         sva( q ) = aaqq*sqrt( max( zero,one+t*apoaq*aapq ) )
                                                   
                                         aapp = aapp*sqrt( max( zero,one-t*aqoap*aapq ) )
                                         mxsinj = max( mxsinj, abs( t ) )
                                      else
                       ! Choose Correct Signum For Theta And Rotate
                                         thsign = -sign( one, aapq )
                                         t = one / ( theta+thsign*sqrt( one+theta*theta ) )
                                                   
                                         cs = sqrt( one / ( one+t*t ) )
                                         sn = t*cs
                                         mxsinj = max( mxsinj, abs( sn ) )
                                         sva( q ) = aaqq*sqrt( max( zero,one+t*apoaq*aapq ) )
                                                   
                                         aapp = aapp*sqrt( max( zero,one-t*aqoap*aapq ) )
                                         apoaq = d( p ) / d( q )
                                         aqoap = d( q ) / d( p )
                                         if( d( p )>=one ) then
                                            if( d( q )>=one ) then
                                               fastr( 3_ilp ) = t*apoaq
                                               fastr( 4_ilp ) = -t*aqoap
                                               d( p ) = d( p )*cs
                                               d( q ) = d( q )*cs
                                               call stdlib_drotm( m, a( 1_ilp, p ), 1_ilp,a( 1_ilp, q ), 1_ilp,&
                                                         fastr )
                                               if( rsvec )call stdlib_drotm( mvl,v( 1_ilp, p ), 1_ilp, v( &
                                                         1_ilp, q ),1_ilp, fastr )
                                            else
                                               call stdlib_daxpy( m, -t*aqoap,a( 1_ilp, q ), 1_ilp,a( 1_ilp, &
                                                         p ), 1_ilp )
                                               call stdlib_daxpy( m, cs*sn*apoaq,a( 1_ilp, p ), 1_ilp,a( &
                                                         1_ilp, q ), 1_ilp )
                                               d( p ) = d( p )*cs
                                               d( q ) = d( q ) / cs
                                               if( rsvec ) then
                                                  call stdlib_daxpy( mvl, -t*aqoap,v( 1_ilp, q ), 1_ilp,v(&
                                                             1_ilp, p ), 1_ilp )
                                                  call stdlib_daxpy( mvl,cs*sn*apoaq,v( 1_ilp, p ), 1_ilp,&
                                                            v( 1_ilp, q ), 1_ilp )
                                               end if
                                            end if
                                         else
                                            if( d( q )>=one ) then
                                               call stdlib_daxpy( m, t*apoaq,a( 1_ilp, p ), 1_ilp,a( 1_ilp, q &
                                                         ), 1_ilp )
                                               call stdlib_daxpy( m, -cs*sn*aqoap,a( 1_ilp, q ), 1_ilp,a( &
                                                         1_ilp, p ), 1_ilp )
                                               d( p ) = d( p ) / cs
                                               d( q ) = d( q )*cs
                                               if( rsvec ) then
                                                  call stdlib_daxpy( mvl, t*apoaq,v( 1_ilp, p ), 1_ilp,v( &
                                                            1_ilp, q ), 1_ilp )
                                                  call stdlib_daxpy( mvl,-cs*sn*aqoap,v( 1_ilp, q ), &
                                                            1_ilp,v( 1_ilp, p ), 1_ilp )
                                               end if
                                            else
                                               if( d( p )>=d( q ) ) then
                                                  call stdlib_daxpy( m, -t*aqoap,a( 1_ilp, q ), 1_ilp,a( &
                                                            1_ilp, p ), 1_ilp )
                                                  call stdlib_daxpy( m, cs*sn*apoaq,a( 1_ilp, p ), 1_ilp,&
                                                            a( 1_ilp, q ), 1_ilp )
                                                  d( p ) = d( p )*cs
                                                  d( q ) = d( q ) / cs
                                                  if( rsvec ) then
                                                     call stdlib_daxpy( mvl,-t*aqoap,v( 1_ilp, q ), 1_ilp,&
                                                               v( 1_ilp, p ), 1_ilp )
                                                     call stdlib_daxpy( mvl,cs*sn*apoaq,v( 1_ilp, p ),&
                                                                1_ilp,v( 1_ilp, q ), 1_ilp )
                                                  end if
                                               else
                                                  call stdlib_daxpy( m, t*apoaq,a( 1_ilp, p ), 1_ilp,a( 1_ilp,&
                                                             q ), 1_ilp )
                                                  call stdlib_daxpy( m,-cs*sn*aqoap,a( 1_ilp, q ), 1_ilp,&
                                                            a( 1_ilp, p ), 1_ilp )
                                                  d( p ) = d( p ) / cs
                                                  d( q ) = d( q )*cs
                                                  if( rsvec ) then
                                                     call stdlib_daxpy( mvl,t*apoaq, v( 1_ilp, p ),1_ilp, &
                                                               v( 1_ilp, q ), 1_ilp )
                                                     call stdlib_daxpy( mvl,-cs*sn*aqoap,v( 1_ilp, q )&
                                                               , 1_ilp,v( 1_ilp, p ), 1_ilp )
                                                  end if
                                               end if
                                            end if
                                         end if
                                      end if
                                   else
                    ! .. have to use modified gram-schmidt like transformation
                                      call stdlib_dcopy( m, a( 1_ilp, p ), 1_ilp, work, 1_ilp )
                                      call stdlib_dlascl( 'G', 0_ilp, 0_ilp, aapp, one, m,1_ilp, work, lda, &
                                                ierr )
                                      call stdlib_dlascl( 'G', 0_ilp, 0_ilp, aaqq, one, m,1_ilp, a( 1_ilp, q ), &
                                                lda, ierr )
                                      temp1 = -aapq*d( p ) / d( q )
                                      call stdlib_daxpy( m, temp1, work, 1_ilp,a( 1_ilp, q ), 1_ilp )
                                      call stdlib_dlascl( 'G', 0_ilp, 0_ilp, one, aaqq, m,1_ilp, a( 1_ilp, q ), &
                                                lda, ierr )
                                      sva( q ) = aaqq*sqrt( max( zero,one-aapq*aapq ) )
                                      mxsinj = max( mxsinj, sfmin )
                                   end if
                 ! end if rotok then ... else
                 ! in the case of cancellation in updating sva(q), sva(p)
                 ! recompute sva(q), sva(p).
                                   if( ( sva( q ) / aaqq )**2_ilp<=rooteps )then
                                      if( ( aaqq<rootbig ) .and.( aaqq>rootsfmin ) ) then
                                         sva( q ) = stdlib_dnrm2( m, a( 1_ilp, q ), 1_ilp )*d( q )
                                      else
                                         t = zero
                                         aaqq = one
                                         call stdlib_dlassq( m, a( 1_ilp, q ), 1_ilp, t,aaqq )
                                         sva( q ) = t*sqrt( aaqq )*d( q )
                                      end if
                                   end if
                                   if( ( aapp / aapp0 )<=rooteps ) then
                                      if( ( aapp<rootbig ) .and.( aapp>rootsfmin ) ) then
                                         aapp = stdlib_dnrm2( m, a( 1_ilp, p ), 1_ilp )*d( p )
                                      else
                                         t = zero
                                         aapp = one
                                         call stdlib_dlassq( m, a( 1_ilp, p ), 1_ilp, t,aapp )
                                         aapp = t*sqrt( aapp )*d( p )
                                      end if
                                      sva( p ) = aapp
                                   end if
                                else
              ! a(:,p) and a(:,q) already numerically orthogonal
                                   if( ir1==0_ilp )notrot = notrot + 1_ilp
                                   pskipped = pskipped + 1_ilp
                                end if
                             else
              ! a(:,q) is zero column
                                if( ir1==0_ilp )notrot = notrot + 1_ilp
                                pskipped = pskipped + 1_ilp
                             end if
                             if( ( i<=swband ) .and.( pskipped>rowskip ) ) then
                                if( ir1==0_ilp )aapp = -aapp
                                notrot = 0_ilp
                                go to 2103
                             end if
                          end do loop_2002
           ! end q-loop
           2103 continue
           ! bailed out of q-loop
                          sva( p ) = aapp
                       else
                          sva( p ) = aapp
                          if( ( ir1==0_ilp ) .and. ( aapp==zero ) )notrot = notrot + min( igl+kbl-1, &
                                    n ) - p
                       end if
                    end do loop_2001
           ! end of the p-loop
           ! end of doing the block ( ibr, ibr )
                 end do loop_1002
           ! end of ir1-loop
      ! ........................................................
       ! ... go to the off diagonal blocks
                 igl = ( ibr-1 )*kbl + 1_ilp
                 loop_2010: do jbc = ibr + 1, nbl
                    jgl = ( jbc-1 )*kbl + 1_ilp
              ! doing the block at ( ibr, jbc )
                    ijblsk = 0_ilp
                    loop_2100: do p = igl, min( igl+kbl-1, n )
                       aapp = sva( p )
                       if( aapp>zero ) then
                          pskipped = 0_ilp
                          loop_2200: do q = jgl, min( jgl+kbl-1, n )
                             aaqq = sva( q )
                             if( aaqq>zero ) then
                                aapp0 = aapp
           ! -#- m x 2 jacobi svd -#-
              ! -#- safe gram matrix computation -#-
                                if( aaqq>=one ) then
                                   if( aapp>=aaqq ) then
                                      rotok = ( small*aapp )<=aaqq
                                   else
                                      rotok = ( small*aaqq )<=aapp
                                   end if
                                   if( aapp<( big / aaqq ) ) then
                                      aapq = ( stdlib_ddot( m, a( 1_ilp, p ), 1_ilp, a( 1_ilp,q ), 1_ilp )*d( p )&
                                                *d( q ) / aaqq )/ aapp
                                   else
                                      call stdlib_dcopy( m, a( 1_ilp, p ), 1_ilp, work, 1_ilp )
                                      call stdlib_dlascl( 'G', 0_ilp, 0_ilp, aapp, d( p ),m, 1_ilp, work, lda,&
                                                 ierr )
                                      aapq = stdlib_ddot( m, work, 1_ilp, a( 1_ilp, q ),1_ilp )*d( q ) / &
                                                aaqq
                                   end if
                                else
                                   if( aapp>=aaqq ) then
                                      rotok = aapp<=( aaqq / small )
                                   else
                                      rotok = aaqq<=( aapp / small )
                                   end if
                                   if( aapp>( small / aaqq ) ) then
                                      aapq = ( stdlib_ddot( m, a( 1_ilp, p ), 1_ilp, a( 1_ilp,q ), 1_ilp )*d( p )&
                                                *d( q ) / aaqq )/ aapp
                                   else
                                      call stdlib_dcopy( m, a( 1_ilp, q ), 1_ilp, work, 1_ilp )
                                      call stdlib_dlascl( 'G', 0_ilp, 0_ilp, aaqq, d( q ),m, 1_ilp, work, lda,&
                                                 ierr )
                                      aapq = stdlib_ddot( m, work, 1_ilp, a( 1_ilp, p ),1_ilp )*d( p ) / &
                                                aapp
                                   end if
                                end if
                                mxaapq = max( mxaapq, abs( aapq ) )
              ! to rotate or not to rotate, that is the question ...
                                if( abs( aapq )>tol ) then
                                   notrot = 0_ilp
                 ! rotated  = rotated + 1
                                   pskipped = 0_ilp
                                   iswrot = iswrot + 1_ilp
                                   if( rotok ) then
                                      aqoap = aaqq / aapp
                                      apoaq = aapp / aaqq
                                      theta = -half*abs( aqoap-apoaq )/aapq
                                      if( aaqq>aapp0 )theta = -theta
                                      if( abs( theta )>bigtheta ) then
                                         t = half / theta
                                         fastr( 3_ilp ) = t*d( p ) / d( q )
                                         fastr( 4_ilp ) = -t*d( q ) / d( p )
                                         call stdlib_drotm( m, a( 1_ilp, p ), 1_ilp,a( 1_ilp, q ), 1_ilp, fastr )
                                                   
                                         if( rsvec )call stdlib_drotm( mvl,v( 1_ilp, p ), 1_ilp,v( 1_ilp, q ),&
                                                    1_ilp,fastr )
                                         sva( q ) = aaqq*sqrt( max( zero,one+t*apoaq*aapq ) )
                                                   
                                         aapp = aapp*sqrt( max( zero,one-t*aqoap*aapq ) )
                                         mxsinj = max( mxsinj, abs( t ) )
                                      else
                       ! Choose Correct Signum For Theta And Rotate
                                         thsign = -sign( one, aapq )
                                         if( aaqq>aapp0 )thsign = -thsign
                                         t = one / ( theta+thsign*sqrt( one+theta*theta ) )
                                                   
                                         cs = sqrt( one / ( one+t*t ) )
                                         sn = t*cs
                                         mxsinj = max( mxsinj, abs( sn ) )
                                         sva( q ) = aaqq*sqrt( max( zero,one+t*apoaq*aapq ) )
                                                   
                                         aapp = aapp*sqrt( max( zero,one-t*aqoap*aapq ) )
                                         apoaq = d( p ) / d( q )
                                         aqoap = d( q ) / d( p )
                                         if( d( p )>=one ) then
                                            if( d( q )>=one ) then
                                               fastr( 3_ilp ) = t*apoaq
                                               fastr( 4_ilp ) = -t*aqoap
                                               d( p ) = d( p )*cs
                                               d( q ) = d( q )*cs
                                               call stdlib_drotm( m, a( 1_ilp, p ), 1_ilp,a( 1_ilp, q ), 1_ilp,&
                                                         fastr )
                                               if( rsvec )call stdlib_drotm( mvl,v( 1_ilp, p ), 1_ilp, v( &
                                                         1_ilp, q ),1_ilp, fastr )
                                            else
                                               call stdlib_daxpy( m, -t*aqoap,a( 1_ilp, q ), 1_ilp,a( 1_ilp, &
                                                         p ), 1_ilp )
                                               call stdlib_daxpy( m, cs*sn*apoaq,a( 1_ilp, p ), 1_ilp,a( &
                                                         1_ilp, q ), 1_ilp )
                                               if( rsvec ) then
                                                  call stdlib_daxpy( mvl, -t*aqoap,v( 1_ilp, q ), 1_ilp,v(&
                                                             1_ilp, p ), 1_ilp )
                                                  call stdlib_daxpy( mvl,cs*sn*apoaq,v( 1_ilp, p ), 1_ilp,&
                                                            v( 1_ilp, q ), 1_ilp )
                                               end if
                                               d( p ) = d( p )*cs
                                               d( q ) = d( q ) / cs
                                            end if
                                         else
                                            if( d( q )>=one ) then
                                               call stdlib_daxpy( m, t*apoaq,a( 1_ilp, p ), 1_ilp,a( 1_ilp, q &
                                                         ), 1_ilp )
                                               call stdlib_daxpy( m, -cs*sn*aqoap,a( 1_ilp, q ), 1_ilp,a( &
                                                         1_ilp, p ), 1_ilp )
                                               if( rsvec ) then
                                                  call stdlib_daxpy( mvl, t*apoaq,v( 1_ilp, p ), 1_ilp,v( &
                                                            1_ilp, q ), 1_ilp )
                                                  call stdlib_daxpy( mvl,-cs*sn*aqoap,v( 1_ilp, q ), &
                                                            1_ilp,v( 1_ilp, p ), 1_ilp )
                                               end if
                                               d( p ) = d( p ) / cs
                                               d( q ) = d( q )*cs
                                            else
                                               if( d( p )>=d( q ) ) then
                                                  call stdlib_daxpy( m, -t*aqoap,a( 1_ilp, q ), 1_ilp,a( &
                                                            1_ilp, p ), 1_ilp )
                                                  call stdlib_daxpy( m, cs*sn*apoaq,a( 1_ilp, p ), 1_ilp,&
                                                            a( 1_ilp, q ), 1_ilp )
                                                  d( p ) = d( p )*cs
                                                  d( q ) = d( q ) / cs
                                                  if( rsvec ) then
                                                     call stdlib_daxpy( mvl,-t*aqoap,v( 1_ilp, q ), 1_ilp,&
                                                               v( 1_ilp, p ), 1_ilp )
                                                     call stdlib_daxpy( mvl,cs*sn*apoaq,v( 1_ilp, p ),&
                                                                1_ilp,v( 1_ilp, q ), 1_ilp )
                                                  end if
                                               else
                                                  call stdlib_daxpy( m, t*apoaq,a( 1_ilp, p ), 1_ilp,a( 1_ilp,&
                                                             q ), 1_ilp )
                                                  call stdlib_daxpy( m,-cs*sn*aqoap,a( 1_ilp, q ), 1_ilp,&
                                                            a( 1_ilp, p ), 1_ilp )
                                                  d( p ) = d( p ) / cs
                                                  d( q ) = d( q )*cs
                                                  if( rsvec ) then
                                                     call stdlib_daxpy( mvl,t*apoaq, v( 1_ilp, p ),1_ilp, &
                                                               v( 1_ilp, q ), 1_ilp )
                                                     call stdlib_daxpy( mvl,-cs*sn*aqoap,v( 1_ilp, q )&
                                                               , 1_ilp,v( 1_ilp, p ), 1_ilp )
                                                  end if
                                               end if
                                            end if
                                         end if
                                      end if
                                   else
                                      if( aapp>aaqq ) then
                                         call stdlib_dcopy( m, a( 1_ilp, p ), 1_ilp, work,1_ilp )
                                         call stdlib_dlascl( 'G', 0_ilp, 0_ilp, aapp, one,m, 1_ilp, work, lda,&
                                                    ierr )
                                         call stdlib_dlascl( 'G', 0_ilp, 0_ilp, aaqq, one,m, 1_ilp, a( 1_ilp, q ),&
                                                    lda,ierr )
                                         temp1 = -aapq*d( p ) / d( q )
                                         call stdlib_daxpy( m, temp1, work, 1_ilp,a( 1_ilp, q ), 1_ilp )
                                                   
                                         call stdlib_dlascl( 'G', 0_ilp, 0_ilp, one, aaqq,m, 1_ilp, a( 1_ilp, q ),&
                                                    lda,ierr )
                                         sva( q ) = aaqq*sqrt( max( zero,one-aapq*aapq ) )
                                         mxsinj = max( mxsinj, sfmin )
                                      else
                                         call stdlib_dcopy( m, a( 1_ilp, q ), 1_ilp, work,1_ilp )
                                         call stdlib_dlascl( 'G', 0_ilp, 0_ilp, aaqq, one,m, 1_ilp, work, lda,&
                                                    ierr )
                                         call stdlib_dlascl( 'G', 0_ilp, 0_ilp, aapp, one,m, 1_ilp, a( 1_ilp, p ),&
                                                    lda,ierr )
                                         temp1 = -aapq*d( q ) / d( p )
                                         call stdlib_daxpy( m, temp1, work, 1_ilp,a( 1_ilp, p ), 1_ilp )
                                                   
                                         call stdlib_dlascl( 'G', 0_ilp, 0_ilp, one, aapp,m, 1_ilp, a( 1_ilp, p ),&
                                                    lda,ierr )
                                         sva( p ) = aapp*sqrt( max( zero,one-aapq*aapq ) )
                                         mxsinj = max( mxsinj, sfmin )
                                      end if
                                   end if
                 ! end if rotok then ... else
                 ! in the case of cancellation in updating sva(q)
                 ! .. recompute sva(q)
                                   if( ( sva( q ) / aaqq )**2_ilp<=rooteps )then
                                      if( ( aaqq<rootbig ) .and.( aaqq>rootsfmin ) ) then
                                         sva( q ) = stdlib_dnrm2( m, a( 1_ilp, q ), 1_ilp )*d( q )
                                      else
                                         t = zero
                                         aaqq = one
                                         call stdlib_dlassq( m, a( 1_ilp, q ), 1_ilp, t,aaqq )
                                         sva( q ) = t*sqrt( aaqq )*d( q )
                                      end if
                                   end if
                                   if( ( aapp / aapp0 )**2_ilp<=rooteps ) then
                                      if( ( aapp<rootbig ) .and.( aapp>rootsfmin ) ) then
                                         aapp = stdlib_dnrm2( m, a( 1_ilp, p ), 1_ilp )*d( p )
                                      else
                                         t = zero
                                         aapp = one
                                         call stdlib_dlassq( m, a( 1_ilp, p ), 1_ilp, t,aapp )
                                         aapp = t*sqrt( aapp )*d( p )
                                      end if
                                      sva( p ) = aapp
                                   end if
                    ! end of ok rotation
                                else
                                   notrot = notrot + 1_ilp
                                   pskipped = pskipped + 1_ilp
                                   ijblsk = ijblsk + 1_ilp
                                end if
                             else
                                notrot = notrot + 1_ilp
                                pskipped = pskipped + 1_ilp
                                ijblsk = ijblsk + 1_ilp
                             end if
                             if( ( i<=swband ) .and. ( ijblsk>=blskip ) )then
                                sva( p ) = aapp
                                notrot = 0_ilp
                                go to 2011
                             end if
                             if( ( i<=swband ) .and.( pskipped>rowskip ) ) then
                                aapp = -aapp
                                notrot = 0_ilp
                                go to 2203
                             end if
                          end do loop_2200
              ! end of the q-loop
              2203 continue
                          sva( p ) = aapp
                       else
                          if( aapp==zero )notrot = notrot +min( jgl+kbl-1, n ) - jgl + 1_ilp
                          if( aapp<zero )notrot = 0_ilp
                       end if
                    end do loop_2100
           ! end of the p-loop
                 end do loop_2010
           ! end of the jbc-loop
           2011 continue
      ! 2011 bailed out of the jbc-loop
                 do p = igl, min( igl+kbl-1, n )
                    sva( p ) = abs( sva( p ) )
                 end do
              end do loop_2000
      ! 2000 :: end of the ibr-loop
           ! .. update sva(n)
              if( ( sva( n )<rootbig ) .and. ( sva( n )>rootsfmin ) )then
                 sva( n ) = stdlib_dnrm2( m, a( 1_ilp, n ), 1_ilp )*d( n )
              else
                 t = zero
                 aapp = one
                 call stdlib_dlassq( m, a( 1_ilp, n ), 1_ilp, t, aapp )
                 sva( n ) = t*sqrt( aapp )*d( n )
              end if
           ! additional steering devices
              if( ( i<swband ) .and. ( ( mxaapq<=roottol ) .or.( iswrot<=n ) ) )swband = i
              if( ( i>swband+1 ) .and. ( mxaapq<real( n,KIND=dp)*tol ) .and.( real( n,KIND=dp)&
                        *mxaapq*mxsinj<tol ) ) then
                 go to 1994
              end if
              if( notrot>=emptsw )go to 1994
           end do loop_1993
           ! end i=1:nsweep loop
       ! #:) reaching this point means that the procedure has completed the given
           ! number of iterations.
           info = nsweep - 1_ilp
           go to 1995
           1994 continue
       ! #:) reaching this point means that during the i-th sweep all pivots were
           ! below the given tolerance, causing early exit.
           info = 0_ilp
       ! #:) info = 0 confirms successful iterations.
       1995 continue
           ! sort the vector d.
           do p = 1, n - 1
              q = stdlib_idamax( n-p+1, sva( p ), 1_ilp ) + p - 1_ilp
              if( p/=q ) then
                 temp1 = sva( p )
                 sva( p ) = sva( q )
                 sva( q ) = temp1
                 temp1 = d( p )
                 d( p ) = d( q )
                 d( q ) = temp1
                 call stdlib_dswap( m, a( 1_ilp, p ), 1_ilp, a( 1_ilp, q ), 1_ilp )
                 if( rsvec )call stdlib_dswap( mvl, v( 1_ilp, p ), 1_ilp, v( 1_ilp, q ), 1_ilp )
              end if
           end do
           return
     end subroutine stdlib_dgsvj0


     pure module subroutine stdlib_cgsvj0( jobv, m, n, a, lda, d, sva, mv, v, ldv, eps,sfmin, tol, &
     !! CGSVJ0 is called from CGESVJ as a pre-processor and that is its main
     !! purpose. It applies Jacobi rotations in the same way as CGESVJ does, but
     !! it does not check convergence (stopping criterion). Few tuning
     !! parameters (marked by [TP]) are available for the implementer.
               nsweep, work, lwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldv, lwork, m, mv, n, nsweep
           real(sp), intent(in) :: eps, sfmin, tol
           character, intent(in) :: jobv
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*), d(n), v(ldv,*)
           complex(sp), intent(out) :: work(lwork)
           real(sp), intent(inout) :: sva(n)
        ! =====================================================================
           
           
           ! Local Scalars 
           complex(sp) :: aapq, ompq
           real(sp) :: aapp, aapp0, aapq1, aaqq, apoaq, aqoap, big, bigtheta, cs, mxaapq, mxsinj, &
                     rootbig, rooteps, rootsfmin, roottol, small, sn, t, temp1, theta, thsign
           integer(ilp) :: blskip, emptsw, i, ibr, ierr, igl, ijblsk, ir1, iswrot, jbc, jgl, kbl, &
                     lkahead, mvl, nbl, notrot, p, pskipped, q, rowskip, swband
           logical(lk) :: applv, rotok, rsvec
           ! Intrinsic Functions 
           ! from lapack
           ! Executable Statements 
           ! test the input parameters.
           applv = stdlib_lsame( jobv, 'A' )
           rsvec = stdlib_lsame( jobv, 'V' )
           if( .not.( rsvec .or. applv .or. stdlib_lsame( jobv, 'N' ) ) ) then
              info = -1_ilp
           else if( m<0_ilp ) then
              info = -2_ilp
           else if( ( n<0_ilp ) .or. ( n>m ) ) then
              info = -3_ilp
           else if( lda<m ) then
              info = -5_ilp
           else if( ( rsvec.or.applv ) .and. ( mv<0_ilp ) ) then
              info = -8_ilp
           else if( ( rsvec.and.( ldv<n ) ).or.( applv.and.( ldv<mv ) ) ) then
              info = -10_ilp
           else if( tol<=eps ) then
              info = -13_ilp
           else if( nsweep<0_ilp ) then
              info = -14_ilp
           else if( lwork<m ) then
              info = -16_ilp
           else
              info = 0_ilp
           end if
           ! #:(
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CGSVJ0', -info )
              return
           end if
           if( rsvec ) then
              mvl = n
           else if( applv ) then
              mvl = mv
           end if
           rsvec = rsvec .or. applv
           rooteps = sqrt( eps )
           rootsfmin = sqrt( sfmin )
           small = sfmin / eps
           big = one / sfmin
           rootbig = one / rootsfmin
           bigtheta = one / rooteps
           roottol = sqrt( tol )
           ! .. row-cyclic jacobi svd algorithm with column pivoting ..
           emptsw = ( n*( n-1 ) ) / 2_ilp
           notrot = 0_ilp
           ! .. row-cyclic pivot strategy with de rijk's pivoting ..
           swband = 0_ilp
      ! [tp] swband is a tuning parameter [tp]. it is meaningful and effective
           ! if stdlib_cgesvj is used as a computational routine in the preconditioned
           ! jacobi svd algorithm stdlib_cgejsv. for sweeps i=1:swband the procedure
           ! works on pivots inside a band-like region around the diagonal.
           ! the boundaries are determined dynamically, based on the number of
           ! pivots above a threshold.
           kbl = min( 8_ilp, n )
      ! [tp] kbl is a tuning parameter that defines the tile size in the
           ! tiling of the p-q loops of pivot pairs. in general, an optimal
           ! value of kbl depends on the matrix dimensions and on the
           ! parameters of the computer's memory.
           nbl = n / kbl
           if( ( nbl*kbl )/=n )nbl = nbl + 1_ilp
           blskip = kbl**2_ilp
      ! [tp] blkskip is a tuning parameter that depends on swband and kbl.
           rowskip = min( 5_ilp, kbl )
      ! [tp] rowskip is a tuning parameter.
           lkahead = 1_ilp
      ! [tp] lkahead is a tuning parameter.
           ! quasi block transformations, using the lower (upper) triangular
           ! structure of the input matrix. the quasi-block-cycling usually
           ! invokes cubic convergence. big part of this cycle is done inside
           ! canonical subspaces of dimensions less than m.
           ! .. row-cyclic pivot strategy with de rijk's pivoting ..
           loop_1993: do i = 1, nsweep
           ! .. go go go ...
              mxaapq = zero
              mxsinj = zero
              iswrot = 0_ilp
              notrot = 0_ilp
              pskipped = 0_ilp
           ! each sweep is unrolled using kbl-by-kbl tiles over the pivot pairs
           ! 1 <= p < q <= n. this is the first step toward a blocked implementation
           ! of the rotations. new implementation, based on block transformations,
           ! is under development.
              loop_2000: do ibr = 1, nbl
                 igl = ( ibr-1 )*kbl + 1_ilp
                 loop_1002: do ir1 = 0, min( lkahead, nbl-ibr )
                    igl = igl + ir1*kbl
                    loop_2001: do p = igl, min( igl+kbl-1, n-1 )
           ! .. de rijk's pivoting
                       q = stdlib_isamax( n-p+1, sva( p ), 1_ilp ) + p - 1_ilp
                       if( p/=q ) then
                          call stdlib_cswap( m, a( 1_ilp, p ), 1_ilp, a( 1_ilp, q ), 1_ilp )
                          if( rsvec )call stdlib_cswap( mvl, v( 1_ilp, p ), 1_ilp,v( 1_ilp, q ), 1_ilp )
                          temp1 = sva( p )
                          sva( p ) = sva( q )
                          sva( q ) = temp1
                          aapq = d(p)
                          d(p) = d(q)
                          d(q) = aapq
                       end if
                       if( ir1==0_ilp ) then
              ! column norms are periodically updated by explicit
              ! norm computation.
              ! caveat:
              ! unfortunately, some blas implementations compute sncrm2(m,a(1,p),1)
              ! as sqrt(s=stdlib_cdotc(m,a(1,p),1,a(1,p),1)), which may cause the result to
              ! overflow for ||a(:,p)||_2 > sqrt(overflow_threshold), and to
              ! underflow for ||a(:,p)||_2 < sqrt(underflow_threshold).
              ! hence, stdlib_scnrm2 cannot be trusted, not even in the case when
              ! the true norm is far from the under(over)flow boundaries.
              ! if properly implemented stdlib_scnrm2 is available, the if-then-else-end if
              ! below should be replaced with "aapp = stdlib_scnrm2( m, a(1,p), 1 )".
                          if( ( sva( p )<rootbig ) .and.( sva( p )>rootsfmin ) ) then
                             sva( p ) = stdlib_scnrm2( m, a( 1_ilp, p ), 1_ilp )
                          else
                             temp1 = zero
                             aapp = one
                             call stdlib_classq( m, a( 1_ilp, p ), 1_ilp, temp1, aapp )
                             sva( p ) = temp1*sqrt( aapp )
                          end if
                          aapp = sva( p )
                       else
                          aapp = sva( p )
                       end if
                       if( aapp>zero ) then
                          pskipped = 0_ilp
                          loop_2002: do q = p + 1, min( igl+kbl-1, n )
                             aaqq = sva( q )
                             if( aaqq>zero ) then
                                aapp0 = aapp
                                if( aaqq>=one ) then
                                   rotok = ( small*aapp )<=aaqq
                                   if( aapp<( big / aaqq ) ) then
                                      aapq = ( stdlib_cdotc( m, a( 1_ilp, p ), 1_ilp,a( 1_ilp, q ), 1_ilp ) / &
                                                aaqq ) / aapp
                                   else
                                      call stdlib_ccopy( m, a( 1_ilp, p ), 1_ilp,work, 1_ilp )
                                      call stdlib_clascl( 'G', 0_ilp, 0_ilp, aapp, one,m, 1_ilp, work, lda, &
                                                ierr )
                                      aapq = stdlib_cdotc( m, work, 1_ilp,a( 1_ilp, q ), 1_ilp ) / &
                                                aaqq
                                   end if
                                else
                                   rotok = aapp<=( aaqq / small )
                                   if( aapp>( small / aaqq ) ) then
                                      aapq = ( stdlib_cdotc( m, a( 1_ilp, p ), 1_ilp,a( 1_ilp, q ), 1_ilp ) / &
                                                aapp ) / aaqq
                                   else
                                      call stdlib_ccopy( m, a( 1_ilp, q ), 1_ilp,work, 1_ilp )
                                      call stdlib_clascl( 'G', 0_ilp, 0_ilp, aaqq,one, m, 1_ilp,work, lda, &
                                                ierr )
                                      aapq = stdlib_cdotc( m, a( 1_ilp, p ), 1_ilp,work, 1_ilp ) / &
                                                aapp
                                   end if
                                end if
                                 ! aapq = aapq * conjg( cwork(p) ) * cwork(q)
                                aapq1  = -abs(aapq)
                                mxaapq = max( mxaapq, -aapq1 )
              ! to rotate or not to rotate, that is the question ...
                                if( abs( aapq1 )>tol ) then
                                   ompq = aapq / abs(aapq)
                 ! Rotate
      ! [rtd]      rotated = rotated + one
                                   if( ir1==0_ilp ) then
                                      notrot = 0_ilp
                                      pskipped = 0_ilp
                                      iswrot = iswrot + 1_ilp
                                   end if
                                   if( rotok ) then
                                      aqoap = aaqq / aapp
                                      apoaq = aapp / aaqq
                                      theta = -half*abs( aqoap-apoaq )/aapq1
                                      if( abs( theta )>bigtheta ) then
                                         t  = half / theta
                                         cs = one
                                         call stdlib_crot( m, a(1_ilp,p), 1_ilp, a(1_ilp,q), 1_ilp,cs, conjg(ompq)&
                                                   *t )
                                         if ( rsvec ) then
                                             call stdlib_crot( mvl, v(1_ilp,p), 1_ilp,v(1_ilp,q), 1_ilp, cs, &
                                                       conjg(ompq)*t )
                                         end if
                                         sva( q ) = aaqq*sqrt( max( zero,one+t*apoaq*aapq1 ) )
                                                   
                                         aapp = aapp*sqrt( max( zero,one-t*aqoap*aapq1 ) )
                                         mxsinj = max( mxsinj, abs( t ) )
                                      else
                       ! Choose Correct Signum For Theta And Rotate
                                         thsign = -sign( one, aapq1 )
                                         t = one / ( theta+thsign*sqrt( one+theta*theta ) )
                                                   
                                         cs = sqrt( one / ( one+t*t ) )
                                         sn = t*cs
                                         mxsinj = max( mxsinj, abs( sn ) )
                                         sva( q ) = aaqq*sqrt( max( zero,one+t*apoaq*aapq1 ) )
                                                   
                                         aapp = aapp*sqrt( max( zero,one-t*aqoap*aapq1 ) )
                                         call stdlib_crot( m, a(1_ilp,p), 1_ilp, a(1_ilp,q), 1_ilp,cs, conjg(ompq)&
                                                   *sn )
                                         if ( rsvec ) then
                                             call stdlib_crot( mvl, v(1_ilp,p), 1_ilp,v(1_ilp,q), 1_ilp, cs, &
                                                       conjg(ompq)*sn )
                                         end if
                                      end if
                                      d(p) = -d(q) * ompq
                                      else
                    ! .. have to use modified gram-schmidt like transformation
                                      call stdlib_ccopy( m, a( 1_ilp, p ), 1_ilp,work, 1_ilp )
                                      call stdlib_clascl( 'G', 0_ilp, 0_ilp, aapp, one, m,1_ilp, work, lda,&
                                                ierr )
                                      call stdlib_clascl( 'G', 0_ilp, 0_ilp, aaqq, one, m,1_ilp, a( 1_ilp, q ), &
                                                lda, ierr )
                                      call stdlib_caxpy( m, -aapq, work, 1_ilp,a( 1_ilp, q ), 1_ilp )
                                      call stdlib_clascl( 'G', 0_ilp, 0_ilp, one, aaqq, m,1_ilp, a( 1_ilp, q ), &
                                                lda, ierr )
                                      sva( q ) = aaqq*sqrt( max( zero,one-aapq1*aapq1 ) )
                                      mxsinj = max( mxsinj, sfmin )
                                   end if
                 ! end if rotok then ... else
                 ! in the case of cancellation in updating sva(q), sva(p)
                 ! recompute sva(q), sva(p).
                                   if( ( sva( q ) / aaqq )**2_ilp<=rooteps )then
                                      if( ( aaqq<rootbig ) .and.( aaqq>rootsfmin ) ) then
                                         sva( q ) = stdlib_scnrm2( m, a( 1_ilp, q ), 1_ilp )
                                      else
                                         t = zero
                                         aaqq = one
                                         call stdlib_classq( m, a( 1_ilp, q ), 1_ilp, t,aaqq )
                                         sva( q ) = t*sqrt( aaqq )
                                      end if
                                   end if
                                   if( ( aapp / aapp0 )<=rooteps ) then
                                      if( ( aapp<rootbig ) .and.( aapp>rootsfmin ) ) then
                                         aapp = stdlib_scnrm2( m, a( 1_ilp, p ), 1_ilp )
                                      else
                                         t = zero
                                         aapp = one
                                         call stdlib_classq( m, a( 1_ilp, p ), 1_ilp, t,aapp )
                                         aapp = t*sqrt( aapp )
                                      end if
                                      sva( p ) = aapp
                                   end if
                                else
              ! a(:,p) and a(:,q) already numerically orthogonal
                                   if( ir1==0_ilp )notrot = notrot + 1_ilp
      ! [rtd]      skipped  = skipped  + 1
                                   pskipped = pskipped + 1_ilp
                                end if
                             else
              ! a(:,q) is zero column
                                if( ir1==0_ilp )notrot = notrot + 1_ilp
                                pskipped = pskipped + 1_ilp
                             end if
                             if( ( i<=swband ) .and.( pskipped>rowskip ) ) then
                                if( ir1==0_ilp )aapp = -aapp
                                notrot = 0_ilp
                                go to 2103
                             end if
                          end do loop_2002
           ! end q-loop
           2103 continue
           ! bailed out of q-loop
                          sva( p ) = aapp
                       else
                          sva( p ) = aapp
                          if( ( ir1==0_ilp ) .and. ( aapp==zero ) )notrot = notrot + min( igl+kbl-1, &
                                    n ) - p
                       end if
                    end do loop_2001
           ! end of the p-loop
           ! end of doing the block ( ibr, ibr )
                 end do loop_1002
           ! end of ir1-loop
       ! ... go to the off diagonal blocks
                 igl = ( ibr-1 )*kbl + 1_ilp
                 loop_2010: do jbc = ibr + 1, nbl
                    jgl = ( jbc-1 )*kbl + 1_ilp
              ! doing the block at ( ibr, jbc )
                    ijblsk = 0_ilp
                    loop_2100: do p = igl, min( igl+kbl-1, n )
                       aapp = sva( p )
                       if( aapp>zero ) then
                          pskipped = 0_ilp
                          loop_2200: do q = jgl, min( jgl+kbl-1, n )
                             aaqq = sva( q )
                             if( aaqq>zero ) then
                                aapp0 = aapp
           ! M X 2 Jacobi Svd 
              ! safe gram matrix computation
                                if( aaqq>=one ) then
                                   if( aapp>=aaqq ) then
                                      rotok = ( small*aapp )<=aaqq
                                   else
                                      rotok = ( small*aaqq )<=aapp
                                   end if
                                   if( aapp<( big / aaqq ) ) then
                                      aapq = ( stdlib_cdotc( m, a( 1_ilp, p ), 1_ilp,a( 1_ilp, q ), 1_ilp ) / &
                                                aaqq ) / aapp
                                   else
                                      call stdlib_ccopy( m, a( 1_ilp, p ), 1_ilp,work, 1_ilp )
                                      call stdlib_clascl( 'G', 0_ilp, 0_ilp, aapp,one, m, 1_ilp,work, lda, &
                                                ierr )
                                      aapq = stdlib_cdotc( m, work, 1_ilp,a( 1_ilp, q ), 1_ilp ) / &
                                                aaqq
                                   end if
                                else
                                   if( aapp>=aaqq ) then
                                      rotok = aapp<=( aaqq / small )
                                   else
                                      rotok = aaqq<=( aapp / small )
                                   end if
                                   if( aapp>( small / aaqq ) ) then
                                      aapq = ( stdlib_cdotc( m, a( 1_ilp, p ), 1_ilp,a( 1_ilp, q ), 1_ilp ) / max(&
                                                aaqq,aapp) )/ min(aaqq,aapp)
                                   else
                                      call stdlib_ccopy( m, a( 1_ilp, q ), 1_ilp,work, 1_ilp )
                                      call stdlib_clascl( 'G', 0_ilp, 0_ilp, aaqq,one, m, 1_ilp,work, lda, &
                                                ierr )
                                      aapq = stdlib_cdotc( m, a( 1_ilp, p ), 1_ilp,work, 1_ilp ) / &
                                                aapp
                                   end if
                                end if
                                 ! aapq = aapq * conjg(cwork(p))*cwork(q)
                                aapq1  = -abs(aapq)
                                mxaapq = max( mxaapq, -aapq1 )
              ! to rotate or not to rotate, that is the question ...
                                if( abs( aapq1 )>tol ) then
                                   ompq = aapq / abs(aapq)
                                   notrot = 0_ilp
      ! [rtd]      rotated  = rotated + 1
                                   pskipped = 0_ilp
                                   iswrot = iswrot + 1_ilp
                                   if( rotok ) then
                                      aqoap = aaqq / aapp
                                      apoaq = aapp / aaqq
                                      theta = -half*abs( aqoap-apoaq )/ aapq1
                                      if( aaqq>aapp0 )theta = -theta
                                      if( abs( theta )>bigtheta ) then
                                         t  = half / theta
                                         cs = one
                                         call stdlib_crot( m, a(1_ilp,p), 1_ilp, a(1_ilp,q), 1_ilp,cs, conjg(ompq)&
                                                   *t )
                                         if( rsvec ) then
                                             call stdlib_crot( mvl, v(1_ilp,p), 1_ilp,v(1_ilp,q), 1_ilp, cs, &
                                                       conjg(ompq)*t )
                                         end if
                                         sva( q ) = aaqq*sqrt( max( zero,one+t*apoaq*aapq1 ) )
                                                   
                                         aapp = aapp*sqrt( max( zero,one-t*aqoap*aapq1 ) )
                                         mxsinj = max( mxsinj, abs( t ) )
                                      else
                       ! Choose Correct Signum For Theta And Rotate
                                         thsign = -sign( one, aapq1 )
                                         if( aaqq>aapp0 )thsign = -thsign
                                         t = one / ( theta+thsign*sqrt( one+theta*theta ) )
                                                   
                                         cs = sqrt( one / ( one+t*t ) )
                                         sn = t*cs
                                         mxsinj = max( mxsinj, abs( sn ) )
                                         sva( q ) = aaqq*sqrt( max( zero,one+t*apoaq*aapq1 ) )
                                                   
                                         aapp = aapp*sqrt( max( zero,one-t*aqoap*aapq1 ) )
                                         call stdlib_crot( m, a(1_ilp,p), 1_ilp, a(1_ilp,q), 1_ilp,cs, conjg(ompq)&
                                                   *sn )
                                         if( rsvec ) then
                                             call stdlib_crot( mvl, v(1_ilp,p), 1_ilp,v(1_ilp,q), 1_ilp, cs, &
                                                       conjg(ompq)*sn )
                                         end if
                                      end if
                                      d(p) = -d(q) * ompq
                                   else
                    ! .. have to use modified gram-schmidt like transformation
                                    if( aapp>aaqq ) then
                                         call stdlib_ccopy( m, a( 1_ilp, p ), 1_ilp,work, 1_ilp )
                                         call stdlib_clascl( 'G', 0_ilp, 0_ilp, aapp, one,m, 1_ilp, work,lda,&
                                                   ierr )
                                         call stdlib_clascl( 'G', 0_ilp, 0_ilp, aaqq, one,m, 1_ilp, a( 1_ilp, q ),&
                                                    lda,ierr )
                                         call stdlib_caxpy( m, -aapq, work,1_ilp, a( 1_ilp, q ), 1_ilp )
                                                   
                                         call stdlib_clascl( 'G', 0_ilp, 0_ilp, one, aaqq,m, 1_ilp, a( 1_ilp, q ),&
                                                    lda,ierr )
                                         sva( q ) = aaqq*sqrt( max( zero,one-aapq1*aapq1 ) )
                                                   
                                         mxsinj = max( mxsinj, sfmin )
                                    else
                                        call stdlib_ccopy( m, a( 1_ilp, q ), 1_ilp,work, 1_ilp )
                                         call stdlib_clascl( 'G', 0_ilp, 0_ilp, aaqq, one,m, 1_ilp, work,lda,&
                                                   ierr )
                                         call stdlib_clascl( 'G', 0_ilp, 0_ilp, aapp, one,m, 1_ilp, a( 1_ilp, p ),&
                                                    lda,ierr )
                                         call stdlib_caxpy( m, -conjg(aapq),work, 1_ilp, a( 1_ilp, p ), 1_ilp &
                                                   )
                                         call stdlib_clascl( 'G', 0_ilp, 0_ilp, one, aapp,m, 1_ilp, a( 1_ilp, p ),&
                                                    lda,ierr )
                                         sva( p ) = aapp*sqrt( max( zero,one-aapq1*aapq1 ) )
                                                   
                                         mxsinj = max( mxsinj, sfmin )
                                    end if
                                   end if
                 ! end if rotok then ... else
                 ! in the case of cancellation in updating sva(q), sva(p)
                 ! .. recompute sva(q), sva(p)
                                   if( ( sva( q ) / aaqq )**2_ilp<=rooteps )then
                                      if( ( aaqq<rootbig ) .and.( aaqq>rootsfmin ) ) then
                                         sva( q ) = stdlib_scnrm2( m, a( 1_ilp, q ), 1_ilp)
                                       else
                                         t = zero
                                         aaqq = one
                                         call stdlib_classq( m, a( 1_ilp, q ), 1_ilp, t,aaqq )
                                         sva( q ) = t*sqrt( aaqq )
                                      end if
                                   end if
                                   if( ( aapp / aapp0 )**2_ilp<=rooteps ) then
                                      if( ( aapp<rootbig ) .and.( aapp>rootsfmin ) ) then
                                         aapp = stdlib_scnrm2( m, a( 1_ilp, p ), 1_ilp )
                                      else
                                         t = zero
                                         aapp = one
                                         call stdlib_classq( m, a( 1_ilp, p ), 1_ilp, t,aapp )
                                         aapp = t*sqrt( aapp )
                                      end if
                                      sva( p ) = aapp
                                   end if
                    ! end of ok rotation
                                else
                                   notrot = notrot + 1_ilp
      ! [rtd]      skipped  = skipped  + 1
                                   pskipped = pskipped + 1_ilp
                                   ijblsk = ijblsk + 1_ilp
                                end if
                             else
                                notrot = notrot + 1_ilp
                                pskipped = pskipped + 1_ilp
                                ijblsk = ijblsk + 1_ilp
                             end if
                             if( ( i<=swband ) .and. ( ijblsk>=blskip ) )then
                                sva( p ) = aapp
                                notrot = 0_ilp
                                go to 2011
                             end if
                             if( ( i<=swband ) .and.( pskipped>rowskip ) ) then
                                aapp = -aapp
                                notrot = 0_ilp
                                go to 2203
                             end if
                          end do loop_2200
              ! end of the q-loop
              2203 continue
                          sva( p ) = aapp
                       else
                          if( aapp==zero )notrot = notrot +min( jgl+kbl-1, n ) - jgl + 1_ilp
                          if( aapp<zero )notrot = 0_ilp
                       end if
                    end do loop_2100
           ! end of the p-loop
                 end do loop_2010
           ! end of the jbc-loop
           2011 continue
      ! 2011 bailed out of the jbc-loop
                 do p = igl, min( igl+kbl-1, n )
                    sva( p ) = abs( sva( p ) )
                 end do
      ! **
              end do loop_2000
      ! 2000 :: end of the ibr-loop
           ! .. update sva(n)
              if( ( sva( n )<rootbig ) .and. ( sva( n )>rootsfmin ) )then
                 sva( n ) = stdlib_scnrm2( m, a( 1_ilp, n ), 1_ilp )
              else
                 t = zero
                 aapp = one
                 call stdlib_classq( m, a( 1_ilp, n ), 1_ilp, t, aapp )
                 sva( n ) = t*sqrt( aapp )
              end if
           ! additional steering devices
              if( ( i<swband ) .and. ( ( mxaapq<=roottol ) .or.( iswrot<=n ) ) )swband = i
              if( ( i>swband+1 ) .and. ( mxaapq<sqrt( real( n,KIND=sp) )*tol ) .and. ( real( n,&
                        KIND=sp)*mxaapq*mxsinj<tol ) ) then
                 go to 1994
              end if
              if( notrot>=emptsw )go to 1994
           end do loop_1993
           ! end i=1:nsweep loop
       ! #:( reaching this point means that the procedure has not converged.
           info = nsweep - 1_ilp
           go to 1995
           1994 continue
       ! #:) reaching this point means numerical convergence after the i-th
           ! sweep.
           info = 0_ilp
       ! #:) info = 0 confirms successful iterations.
       1995 continue
           ! sort the vector sva() of column norms.
           do p = 1, n - 1
              q = stdlib_isamax( n-p+1, sva( p ), 1_ilp ) + p - 1_ilp
              if( p/=q ) then
                 temp1 = sva( p )
                 sva( p ) = sva( q )
                 sva( q ) = temp1
                 aapq = d( p )
                 d( p ) = d( q )
                 d( q ) = aapq
                 call stdlib_cswap( m, a( 1_ilp, p ), 1_ilp, a( 1_ilp, q ), 1_ilp )
                 if( rsvec )call stdlib_cswap( mvl, v( 1_ilp, p ), 1_ilp, v( 1_ilp, q ), 1_ilp )
              end if
           end do
           return
     end subroutine stdlib_cgsvj0

     pure module subroutine stdlib_zgsvj0( jobv, m, n, a, lda, d, sva, mv, v, ldv, eps,sfmin, tol, &
     !! ZGSVJ0 is called from ZGESVJ as a pre-processor and that is its main
     !! purpose. It applies Jacobi rotations in the same way as ZGESVJ does, but
     !! it does not check convergence (stopping criterion). Few tuning
     !! parameters (marked by [TP]) are available for the implementer.
               nsweep, work, lwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldv, lwork, m, mv, n, nsweep
           real(dp), intent(in) :: eps, sfmin, tol
           character, intent(in) :: jobv
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*), d(n), v(ldv,*)
           complex(dp), intent(out) :: work(lwork)
           real(dp), intent(inout) :: sva(n)
        ! =====================================================================
           
           
           ! Local Scalars 
           complex(dp) :: aapq, ompq
           real(dp) :: aapp, aapp0, aapq1, aaqq, apoaq, aqoap, big, bigtheta, cs, mxaapq, mxsinj, &
                     rootbig, rooteps, rootsfmin, roottol, small, sn, t, temp1, theta, thsign
           integer(ilp) :: blskip, emptsw, i, ibr, ierr, igl, ijblsk, ir1, iswrot, jbc, jgl, kbl, &
                     lkahead, mvl, nbl, notrot, p, pskipped, q, rowskip, swband
           logical(lk) :: applv, rotok, rsvec
           ! Intrinsic Functions 
           ! from lapack
           ! Executable Statements 
           ! test the input parameters.
           applv = stdlib_lsame( jobv, 'A' )
           rsvec = stdlib_lsame( jobv, 'V' )
           if( .not.( rsvec .or. applv .or. stdlib_lsame( jobv, 'N' ) ) ) then
              info = -1_ilp
           else if( m<0_ilp ) then
              info = -2_ilp
           else if( ( n<0_ilp ) .or. ( n>m ) ) then
              info = -3_ilp
           else if( lda<m ) then
              info = -5_ilp
           else if( ( rsvec.or.applv ) .and. ( mv<0_ilp ) ) then
              info = -8_ilp
           else if( ( rsvec.and.( ldv<n ) ).or.( applv.and.( ldv<mv ) ) ) then
              info = -10_ilp
           else if( tol<=eps ) then
              info = -13_ilp
           else if( nsweep<0_ilp ) then
              info = -14_ilp
           else if( lwork<m ) then
              info = -16_ilp
           else
              info = 0_ilp
           end if
           ! #:(
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZGSVJ0', -info )
              return
           end if
           if( rsvec ) then
              mvl = n
           else if( applv ) then
              mvl = mv
           end if
           rsvec = rsvec .or. applv
           rooteps = sqrt( eps )
           rootsfmin = sqrt( sfmin )
           small = sfmin / eps
           big = one / sfmin
           rootbig = one / rootsfmin
           bigtheta = one / rooteps
           roottol = sqrt( tol )
           ! .. row-cyclic jacobi svd algorithm with column pivoting ..
           emptsw = ( n*( n-1 ) ) / 2_ilp
           notrot = 0_ilp
           ! .. row-cyclic pivot strategy with de rijk's pivoting ..
           swband = 0_ilp
      ! [tp] swband is a tuning parameter [tp]. it is meaningful and effective
           ! if stdlib_zgesvj is used as a computational routine in the preconditioned
           ! jacobi svd algorithm stdlib_zgejsv. for sweeps i=1:swband the procedure
           ! works on pivots inside a band-like region around the diagonal.
           ! the boundaries are determined dynamically, based on the number of
           ! pivots above a threshold.
           kbl = min( 8_ilp, n )
      ! [tp] kbl is a tuning parameter that defines the tile size in the
           ! tiling of the p-q loops of pivot pairs. in general, an optimal
           ! value of kbl depends on the matrix dimensions and on the
           ! parameters of the computer's memory.
           nbl = n / kbl
           if( ( nbl*kbl )/=n )nbl = nbl + 1_ilp
           blskip = kbl**2_ilp
      ! [tp] blkskip is a tuning parameter that depends on swband and kbl.
           rowskip = min( 5_ilp, kbl )
      ! [tp] rowskip is a tuning parameter.
           lkahead = 1_ilp
      ! [tp] lkahead is a tuning parameter.
           ! quasi block transformations, using the lower (upper) triangular
           ! structure of the input matrix. the quasi-block-cycling usually
           ! invokes cubic convergence. big part of this cycle is done inside
           ! canonical subspaces of dimensions less than m.
           ! .. row-cyclic pivot strategy with de rijk's pivoting ..
           loop_1993: do i = 1, nsweep
           ! .. go go go ...
              mxaapq = zero
              mxsinj = zero
              iswrot = 0_ilp
              notrot = 0_ilp
              pskipped = 0_ilp
           ! each sweep is unrolled using kbl-by-kbl tiles over the pivot pairs
           ! 1 <= p < q <= n. this is the first step toward a blocked implementation
           ! of the rotations. new implementation, based on block transformations,
           ! is under development.
              loop_2000: do ibr = 1, nbl
                 igl = ( ibr-1 )*kbl + 1_ilp
                 loop_1002: do ir1 = 0, min( lkahead, nbl-ibr )
                    igl = igl + ir1*kbl
                    loop_2001: do p = igl, min( igl+kbl-1, n-1 )
           ! .. de rijk's pivoting
                       q = stdlib_idamax( n-p+1, sva( p ), 1_ilp ) + p - 1_ilp
                       if( p/=q ) then
                          call stdlib_zswap( m, a( 1_ilp, p ), 1_ilp, a( 1_ilp, q ), 1_ilp )
                          if( rsvec )call stdlib_zswap( mvl, v( 1_ilp, p ), 1_ilp,v( 1_ilp, q ), 1_ilp )
                          temp1 = sva( p )
                          sva( p ) = sva( q )
                          sva( q ) = temp1
                          aapq = d(p)
                          d(p) = d(q)
                          d(q) = aapq
                       end if
                       if( ir1==0_ilp ) then
              ! column norms are periodically updated by explicit
              ! norm computation.
              ! caveat:
              ! unfortunately, some blas implementations compute sncrm2(m,a(1,p),1)
              ! as sqrt(s=stdlib_zdotc(m,a(1,p),1,a(1,p),1)), which may cause the result to
              ! overflow for ||a(:,p)||_2 > sqrt(overflow_threshold), and to
              ! underflow for ||a(:,p)||_2 < sqrt(underflow_threshold).
              ! hence, stdlib_dznrm2 cannot be trusted, not even in the case when
              ! the true norm is far from the under(over)flow boundaries.
              ! if properly implemented stdlib_dznrm2 is available, the if-then-else-end if
              ! below should be replaced with "aapp = stdlib_dznrm2( m, a(1,p), 1 )".
                          if( ( sva( p )<rootbig ) .and.( sva( p )>rootsfmin ) ) then
                             sva( p ) = stdlib_dznrm2( m, a( 1_ilp, p ), 1_ilp )
                          else
                             temp1 = zero
                             aapp = one
                             call stdlib_zlassq( m, a( 1_ilp, p ), 1_ilp, temp1, aapp )
                             sva( p ) = temp1*sqrt( aapp )
                          end if
                          aapp = sva( p )
                       else
                          aapp = sva( p )
                       end if
                       if( aapp>zero ) then
                          pskipped = 0_ilp
                          loop_2002: do q = p + 1, min( igl+kbl-1, n )
                             aaqq = sva( q )
                             if( aaqq>zero ) then
                                aapp0 = aapp
                                if( aaqq>=one ) then
                                   rotok = ( small*aapp )<=aaqq
                                   if( aapp<( big / aaqq ) ) then
                                      aapq = ( stdlib_zdotc( m, a( 1_ilp, p ), 1_ilp,a( 1_ilp, q ), 1_ilp ) / &
                                                aaqq ) / aapp
                                   else
                                      call stdlib_zcopy( m, a( 1_ilp, p ), 1_ilp,work, 1_ilp )
                                      call stdlib_zlascl( 'G', 0_ilp, 0_ilp, aapp, one,m, 1_ilp, work, lda, &
                                                ierr )
                                      aapq = stdlib_zdotc( m, work, 1_ilp,a( 1_ilp, q ), 1_ilp ) / &
                                                aaqq
                                   end if
                                else
                                   rotok = aapp<=( aaqq / small )
                                   if( aapp>( small / aaqq ) ) then
                                      aapq = ( stdlib_zdotc( m, a( 1_ilp, p ), 1_ilp,a( 1_ilp, q ), 1_ilp ) / &
                                                aapp ) / aaqq
                                   else
                                      call stdlib_zcopy( m, a( 1_ilp, q ), 1_ilp,work, 1_ilp )
                                      call stdlib_zlascl( 'G', 0_ilp, 0_ilp, aaqq,one, m, 1_ilp,work, lda, &
                                                ierr )
                                      aapq = stdlib_zdotc( m, a( 1_ilp, p ), 1_ilp,work, 1_ilp ) / &
                                                aapp
                                   end if
                                end if
                                 ! aapq = aapq * conjg( cwork(p) ) * cwork(q)
                                aapq1  = -abs(aapq)
                                mxaapq = max( mxaapq, -aapq1 )
              ! to rotate or not to rotate, that is the question ...
                                if( abs( aapq1 )>tol ) then
                                   ompq = aapq / abs(aapq)
                 ! Rotate
      ! [rtd]      rotated = rotated + one
                                   if( ir1==0_ilp ) then
                                      notrot = 0_ilp
                                      pskipped = 0_ilp
                                      iswrot = iswrot + 1_ilp
                                   end if
                                   if( rotok ) then
                                      aqoap = aaqq / aapp
                                      apoaq = aapp / aaqq
                                      theta = -half*abs( aqoap-apoaq )/aapq1
                                      if( abs( theta )>bigtheta ) then
                                         t  = half / theta
                                         cs = one
                                         call stdlib_zrot( m, a(1_ilp,p), 1_ilp, a(1_ilp,q), 1_ilp,cs, conjg(ompq)&
                                                   *t )
                                         if ( rsvec ) then
                                             call stdlib_zrot( mvl, v(1_ilp,p), 1_ilp,v(1_ilp,q), 1_ilp, cs, &
                                                       conjg(ompq)*t )
                                         end if
                                         sva( q ) = aaqq*sqrt( max( zero,one+t*apoaq*aapq1 ) )
                                                   
                                         aapp = aapp*sqrt( max( zero,one-t*aqoap*aapq1 ) )
                                         mxsinj = max( mxsinj, abs( t ) )
                                      else
                       ! Choose Correct Signum For Theta And Rotate
                                         thsign = -sign( one, aapq1 )
                                         t = one / ( theta+thsign*sqrt( one+theta*theta ) )
                                                   
                                         cs = sqrt( one / ( one+t*t ) )
                                         sn = t*cs
                                         mxsinj = max( mxsinj, abs( sn ) )
                                         sva( q ) = aaqq*sqrt( max( zero,one+t*apoaq*aapq1 ) )
                                                   
                                         aapp = aapp*sqrt( max( zero,one-t*aqoap*aapq1 ) )
                                         call stdlib_zrot( m, a(1_ilp,p), 1_ilp, a(1_ilp,q), 1_ilp,cs, conjg(ompq)&
                                                   *sn )
                                         if ( rsvec ) then
                                             call stdlib_zrot( mvl, v(1_ilp,p), 1_ilp,v(1_ilp,q), 1_ilp, cs, &
                                                       conjg(ompq)*sn )
                                         end if
                                      end if
                                      d(p) = -d(q) * ompq
                                      else
                    ! .. have to use modified gram-schmidt like transformation
                                      call stdlib_zcopy( m, a( 1_ilp, p ), 1_ilp,work, 1_ilp )
                                      call stdlib_zlascl( 'G', 0_ilp, 0_ilp, aapp, one, m,1_ilp, work, lda,&
                                                ierr )
                                      call stdlib_zlascl( 'G', 0_ilp, 0_ilp, aaqq, one, m,1_ilp, a( 1_ilp, q ), &
                                                lda, ierr )
                                      call stdlib_zaxpy( m, -aapq, work, 1_ilp,a( 1_ilp, q ), 1_ilp )
                                      call stdlib_zlascl( 'G', 0_ilp, 0_ilp, one, aaqq, m,1_ilp, a( 1_ilp, q ), &
                                                lda, ierr )
                                      sva( q ) = aaqq*sqrt( max( zero,one-aapq1*aapq1 ) )
                                      mxsinj = max( mxsinj, sfmin )
                                   end if
                 ! end if rotok then ... else
                 ! in the case of cancellation in updating sva(q), sva(p)
                 ! recompute sva(q), sva(p).
                                   if( ( sva( q ) / aaqq )**2_ilp<=rooteps )then
                                      if( ( aaqq<rootbig ) .and.( aaqq>rootsfmin ) ) then
                                         sva( q ) = stdlib_dznrm2( m, a( 1_ilp, q ), 1_ilp )
                                      else
                                         t = zero
                                         aaqq = one
                                         call stdlib_zlassq( m, a( 1_ilp, q ), 1_ilp, t,aaqq )
                                         sva( q ) = t*sqrt( aaqq )
                                      end if
                                   end if
                                   if( ( aapp / aapp0 )<=rooteps ) then
                                      if( ( aapp<rootbig ) .and.( aapp>rootsfmin ) ) then
                                         aapp = stdlib_dznrm2( m, a( 1_ilp, p ), 1_ilp )
                                      else
                                         t = zero
                                         aapp = one
                                         call stdlib_zlassq( m, a( 1_ilp, p ), 1_ilp, t,aapp )
                                         aapp = t*sqrt( aapp )
                                      end if
                                      sva( p ) = aapp
                                   end if
                                else
              ! a(:,p) and a(:,q) already numerically orthogonal
                                   if( ir1==0_ilp )notrot = notrot + 1_ilp
      ! [rtd]      skipped  = skipped  + 1
                                   pskipped = pskipped + 1_ilp
                                end if
                             else
              ! a(:,q) is zero column
                                if( ir1==0_ilp )notrot = notrot + 1_ilp
                                pskipped = pskipped + 1_ilp
                             end if
                             if( ( i<=swband ) .and.( pskipped>rowskip ) ) then
                                if( ir1==0_ilp )aapp = -aapp
                                notrot = 0_ilp
                                go to 2103
                             end if
                          end do loop_2002
           ! end q-loop
           2103 continue
           ! bailed out of q-loop
                          sva( p ) = aapp
                       else
                          sva( p ) = aapp
                          if( ( ir1==0_ilp ) .and. ( aapp==zero ) )notrot = notrot + min( igl+kbl-1, &
                                    n ) - p
                       end if
                    end do loop_2001
           ! end of the p-loop
           ! end of doing the block ( ibr, ibr )
                 end do loop_1002
           ! end of ir1-loop
       ! ... go to the off diagonal blocks
                 igl = ( ibr-1 )*kbl + 1_ilp
                 loop_2010: do jbc = ibr + 1, nbl
                    jgl = ( jbc-1 )*kbl + 1_ilp
              ! doing the block at ( ibr, jbc )
                    ijblsk = 0_ilp
                    loop_2100: do p = igl, min( igl+kbl-1, n )
                       aapp = sva( p )
                       if( aapp>zero ) then
                          pskipped = 0_ilp
                          loop_2200: do q = jgl, min( jgl+kbl-1, n )
                             aaqq = sva( q )
                             if( aaqq>zero ) then
                                aapp0 = aapp
           ! M X 2 Jacobi Svd 
              ! safe gram matrix computation
                                if( aaqq>=one ) then
                                   if( aapp>=aaqq ) then
                                      rotok = ( small*aapp )<=aaqq
                                   else
                                      rotok = ( small*aaqq )<=aapp
                                   end if
                                   if( aapp<( big / aaqq ) ) then
                                      aapq = ( stdlib_zdotc( m, a( 1_ilp, p ), 1_ilp,a( 1_ilp, q ), 1_ilp ) / &
                                                aaqq ) / aapp
                                   else
                                      call stdlib_zcopy( m, a( 1_ilp, p ), 1_ilp,work, 1_ilp )
                                      call stdlib_zlascl( 'G', 0_ilp, 0_ilp, aapp,one, m, 1_ilp,work, lda, &
                                                ierr )
                                      aapq = stdlib_zdotc( m, work, 1_ilp,a( 1_ilp, q ), 1_ilp ) / &
                                                aaqq
                                   end if
                                else
                                   if( aapp>=aaqq ) then
                                      rotok = aapp<=( aaqq / small )
                                   else
                                      rotok = aaqq<=( aapp / small )
                                   end if
                                   if( aapp>( small / aaqq ) ) then
                                      aapq = ( stdlib_zdotc( m, a( 1_ilp, p ), 1_ilp,a( 1_ilp, q ), 1_ilp ) / max(&
                                                aaqq,aapp) )/ min(aaqq,aapp)
                                   else
                                      call stdlib_zcopy( m, a( 1_ilp, q ), 1_ilp,work, 1_ilp )
                                      call stdlib_zlascl( 'G', 0_ilp, 0_ilp, aaqq,one, m, 1_ilp,work, lda, &
                                                ierr )
                                      aapq = stdlib_zdotc( m, a( 1_ilp, p ), 1_ilp,work, 1_ilp ) / &
                                                aapp
                                   end if
                                end if
                                 ! aapq = aapq * conjg(cwork(p))*cwork(q)
                                aapq1  = -abs(aapq)
                                mxaapq = max( mxaapq, -aapq1 )
              ! to rotate or not to rotate, that is the question ...
                                if( abs( aapq1 )>tol ) then
                                   ompq = aapq / abs(aapq)
                                   notrot = 0_ilp
      ! [rtd]      rotated  = rotated + 1
                                   pskipped = 0_ilp
                                   iswrot = iswrot + 1_ilp
                                   if( rotok ) then
                                      aqoap = aaqq / aapp
                                      apoaq = aapp / aaqq
                                      theta = -half*abs( aqoap-apoaq )/ aapq1
                                      if( aaqq>aapp0 )theta = -theta
                                      if( abs( theta )>bigtheta ) then
                                         t  = half / theta
                                         cs = one
                                         call stdlib_zrot( m, a(1_ilp,p), 1_ilp, a(1_ilp,q), 1_ilp,cs, conjg(ompq)&
                                                   *t )
                                         if( rsvec ) then
                                             call stdlib_zrot( mvl, v(1_ilp,p), 1_ilp,v(1_ilp,q), 1_ilp, cs, &
                                                       conjg(ompq)*t )
                                         end if
                                         sva( q ) = aaqq*sqrt( max( zero,one+t*apoaq*aapq1 ) )
                                                   
                                         aapp = aapp*sqrt( max( zero,one-t*aqoap*aapq1 ) )
                                         mxsinj = max( mxsinj, abs( t ) )
                                      else
                       ! Choose Correct Signum For Theta And Rotate
                                         thsign = -sign( one, aapq1 )
                                         if( aaqq>aapp0 )thsign = -thsign
                                         t = one / ( theta+thsign*sqrt( one+theta*theta ) )
                                                   
                                         cs = sqrt( one / ( one+t*t ) )
                                         sn = t*cs
                                         mxsinj = max( mxsinj, abs( sn ) )
                                         sva( q ) = aaqq*sqrt( max( zero,one+t*apoaq*aapq1 ) )
                                                   
                                         aapp = aapp*sqrt( max( zero,one-t*aqoap*aapq1 ) )
                                         call stdlib_zrot( m, a(1_ilp,p), 1_ilp, a(1_ilp,q), 1_ilp,cs, conjg(ompq)&
                                                   *sn )
                                         if( rsvec ) then
                                             call stdlib_zrot( mvl, v(1_ilp,p), 1_ilp,v(1_ilp,q), 1_ilp, cs, &
                                                       conjg(ompq)*sn )
                                         end if
                                      end if
                                      d(p) = -d(q) * ompq
                                   else
                    ! .. have to use modified gram-schmidt like transformation
                                    if( aapp>aaqq ) then
                                         call stdlib_zcopy( m, a( 1_ilp, p ), 1_ilp,work, 1_ilp )
                                         call stdlib_zlascl( 'G', 0_ilp, 0_ilp, aapp, one,m, 1_ilp, work,lda,&
                                                   ierr )
                                         call stdlib_zlascl( 'G', 0_ilp, 0_ilp, aaqq, one,m, 1_ilp, a( 1_ilp, q ),&
                                                    lda,ierr )
                                         call stdlib_zaxpy( m, -aapq, work,1_ilp, a( 1_ilp, q ), 1_ilp )
                                                   
                                         call stdlib_zlascl( 'G', 0_ilp, 0_ilp, one, aaqq,m, 1_ilp, a( 1_ilp, q ),&
                                                    lda,ierr )
                                         sva( q ) = aaqq*sqrt( max( zero,one-aapq1*aapq1 ) )
                                                   
                                         mxsinj = max( mxsinj, sfmin )
                                    else
                                        call stdlib_zcopy( m, a( 1_ilp, q ), 1_ilp,work, 1_ilp )
                                         call stdlib_zlascl( 'G', 0_ilp, 0_ilp, aaqq, one,m, 1_ilp, work,lda,&
                                                   ierr )
                                         call stdlib_zlascl( 'G', 0_ilp, 0_ilp, aapp, one,m, 1_ilp, a( 1_ilp, p ),&
                                                    lda,ierr )
                                         call stdlib_zaxpy( m, -conjg(aapq),work, 1_ilp, a( 1_ilp, p ), 1_ilp &
                                                   )
                                         call stdlib_zlascl( 'G', 0_ilp, 0_ilp, one, aapp,m, 1_ilp, a( 1_ilp, p ),&
                                                    lda,ierr )
                                         sva( p ) = aapp*sqrt( max( zero,one-aapq1*aapq1 ) )
                                                   
                                         mxsinj = max( mxsinj, sfmin )
                                    end if
                                   end if
                 ! end if rotok then ... else
                 ! in the case of cancellation in updating sva(q), sva(p)
                 ! .. recompute sva(q), sva(p)
                                   if( ( sva( q ) / aaqq )**2_ilp<=rooteps )then
                                      if( ( aaqq<rootbig ) .and.( aaqq>rootsfmin ) ) then
                                         sva( q ) = stdlib_dznrm2( m, a( 1_ilp, q ), 1_ilp)
                                       else
                                         t = zero
                                         aaqq = one
                                         call stdlib_zlassq( m, a( 1_ilp, q ), 1_ilp, t,aaqq )
                                         sva( q ) = t*sqrt( aaqq )
                                      end if
                                   end if
                                   if( ( aapp / aapp0 )**2_ilp<=rooteps ) then
                                      if( ( aapp<rootbig ) .and.( aapp>rootsfmin ) ) then
                                         aapp = stdlib_dznrm2( m, a( 1_ilp, p ), 1_ilp )
                                      else
                                         t = zero
                                         aapp = one
                                         call stdlib_zlassq( m, a( 1_ilp, p ), 1_ilp, t,aapp )
                                         aapp = t*sqrt( aapp )
                                      end if
                                      sva( p ) = aapp
                                   end if
                    ! end of ok rotation
                                else
                                   notrot = notrot + 1_ilp
      ! [rtd]      skipped  = skipped  + 1
                                   pskipped = pskipped + 1_ilp
                                   ijblsk = ijblsk + 1_ilp
                                end if
                             else
                                notrot = notrot + 1_ilp
                                pskipped = pskipped + 1_ilp
                                ijblsk = ijblsk + 1_ilp
                             end if
                             if( ( i<=swband ) .and. ( ijblsk>=blskip ) )then
                                sva( p ) = aapp
                                notrot = 0_ilp
                                go to 2011
                             end if
                             if( ( i<=swband ) .and.( pskipped>rowskip ) ) then
                                aapp = -aapp
                                notrot = 0_ilp
                                go to 2203
                             end if
                          end do loop_2200
              ! end of the q-loop
              2203 continue
                          sva( p ) = aapp
                       else
                          if( aapp==zero )notrot = notrot +min( jgl+kbl-1, n ) - jgl + 1_ilp
                          if( aapp<zero )notrot = 0_ilp
                       end if
                    end do loop_2100
           ! end of the p-loop
                 end do loop_2010
           ! end of the jbc-loop
           2011 continue
      ! 2011 bailed out of the jbc-loop
                 do p = igl, min( igl+kbl-1, n )
                    sva( p ) = abs( sva( p ) )
                 end do
      ! **
              end do loop_2000
      ! 2000 :: end of the ibr-loop
           ! .. update sva(n)
              if( ( sva( n )<rootbig ) .and. ( sva( n )>rootsfmin ) )then
                 sva( n ) = stdlib_dznrm2( m, a( 1_ilp, n ), 1_ilp )
              else
                 t = zero
                 aapp = one
                 call stdlib_zlassq( m, a( 1_ilp, n ), 1_ilp, t, aapp )
                 sva( n ) = t*sqrt( aapp )
              end if
           ! additional steering devices
              if( ( i<swband ) .and. ( ( mxaapq<=roottol ) .or.( iswrot<=n ) ) )swband = i
              if( ( i>swband+1 ) .and. ( mxaapq<sqrt( real( n,KIND=dp) )*tol ) .and. ( real( n,&
                        KIND=dp)*mxaapq*mxsinj<tol ) ) then
                 go to 1994
              end if
              if( notrot>=emptsw )go to 1994
           end do loop_1993
           ! end i=1:nsweep loop
       ! #:( reaching this point means that the procedure has not converged.
           info = nsweep - 1_ilp
           go to 1995
           1994 continue
       ! #:) reaching this point means numerical convergence after the i-th
           ! sweep.
           info = 0_ilp
       ! #:) info = 0 confirms successful iterations.
       1995 continue
           ! sort the vector sva() of column norms.
           do p = 1, n - 1
              q = stdlib_idamax( n-p+1, sva( p ), 1_ilp ) + p - 1_ilp
              if( p/=q ) then
                 temp1 = sva( p )
                 sva( p ) = sva( q )
                 sva( q ) = temp1
                 aapq = d( p )
                 d( p ) = d( q )
                 d( q ) = aapq
                 call stdlib_zswap( m, a( 1_ilp, p ), 1_ilp, a( 1_ilp, q ), 1_ilp )
                 if( rsvec )call stdlib_zswap( mvl, v( 1_ilp, p ), 1_ilp, v( 1_ilp, q ), 1_ilp )
              end if
           end do
           return
     end subroutine stdlib_zgsvj0




     pure module subroutine stdlib_sgsvj1( jobv, m, n, n1, a, lda, d, sva, mv, v, ldv,eps, sfmin, tol, &
     !! SGSVJ1 is called from SGESVJ as a pre-processor and that is its main
     !! purpose. It applies Jacobi rotations in the same way as SGESVJ does, but
     !! it targets only particular pivots and it does not check convergence
     !! (stopping criterion). Few tuning parameters (marked by [TP]) are
     !! available for the implementer.
     !! Further Details
     !! ~~~~~~~~~~~~~~~
     !! SGSVJ1 applies few sweeps of Jacobi rotations in the column space of
     !! the input M-by-N matrix A. The pivot pairs are taken from the (1,2)
     !! off-diagonal block in the corresponding N-by-N Gram matrix A^T * A. The
     !! block-entries (tiles) of the (1,2) off-diagonal block are marked by the
     !! [x]'s in the following scheme:
     !! | *  *  * [x] [x] [x]|
     !! | *  *  * [x] [x] [x]|    Row-cycling in the nblr-by-nblc [x] blocks.
     !! | *  *  * [x] [x] [x]|    Row-cyclic pivoting inside each [x] block.
     !! |[x] [x] [x] *  *  * |
     !! |[x] [x] [x] *  *  * |
     !! |[x] [x] [x] *  *  * |
     !! In terms of the columns of A, the first N1 columns are rotated 'against'
     !! the remaining N-N1 columns, trying to increase the angle between the
     !! corresponding subspaces. The off-diagonal block is N1-by(N-N1) and it is
     !! tiled using quadratic tiles of side KBL. Here, KBL is a tuning parameter.
     !! The number of sweeps is given in NSWEEP and the orthogonality threshold
     !! is given in TOL.
               nsweep, work, lwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           real(sp), intent(in) :: eps, sfmin, tol
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldv, lwork, m, mv, n, n1, nsweep
           character, intent(in) :: jobv
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*), d(n), sva(n), v(ldv,*)
           real(sp), intent(out) :: work(lwork)
        ! =====================================================================
           
           ! Local Scalars 
           real(sp) :: aapp, aapp0, aapq, aaqq, apoaq, aqoap, big, bigtheta, cs, large, mxaapq, &
           mxsinj, rootbig, rooteps, rootsfmin, roottol, small, sn, t, temp1, theta, &
                     thsign
           integer(ilp) :: blskip, emptsw, i, ibr, igl, ierr, ijblsk, iswrot, jbc, jgl, kbl, mvl, &
                     notrot, nblc, nblr, p, pskipped, q, rowskip, swband
           logical(lk) :: applv, rotok, rsvec
           ! Local Arrays 
           real(sp) :: fastr(5_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           applv = stdlib_lsame( jobv, 'A' )
           rsvec = stdlib_lsame( jobv, 'V' )
           if( .not.( rsvec .or. applv .or. stdlib_lsame( jobv, 'N' ) ) ) then
              info = -1_ilp
           else if( m<0_ilp ) then
              info = -2_ilp
           else if( ( n<0_ilp ) .or. ( n>m ) ) then
              info = -3_ilp
           else if( n1<0_ilp ) then
              info = -4_ilp
           else if( lda<m ) then
              info = -6_ilp
           else if( ( rsvec.or.applv ) .and. ( mv<0_ilp ) ) then
              info = -9_ilp
           else if( ( rsvec.and.( ldv<n ) ).or.( applv.and.( ldv<mv ) )  ) then
              info = -11_ilp
           else if( tol<=eps ) then
              info = -14_ilp
           else if( nsweep<0_ilp ) then
              info = -15_ilp
           else if( lwork<m ) then
              info = -17_ilp
           else
              info = 0_ilp
           end if
           ! #:(
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SGSVJ1', -info )
              return
           end if
           if( rsvec ) then
              mvl = n
           else if( applv ) then
              mvl = mv
           end if
           rsvec = rsvec .or. applv
           rooteps = sqrt( eps )
           rootsfmin = sqrt( sfmin )
           small = sfmin / eps
           big = one / sfmin
           rootbig = one / rootsfmin
           large = big / sqrt( real( m*n,KIND=sp) )
           bigtheta = one / rooteps
           roottol = sqrt( tol )
           ! Initialize The Right Singular Vector Matrix 
           ! rsvec = stdlib_lsame( jobv, 'y' )
           emptsw = n1*( n-n1 )
           notrot = 0_ilp
           fastr( 1_ilp ) = zero
           ! .. row-cyclic pivot strategy with de rijk's pivoting ..
           kbl = min( 8_ilp, n )
           nblr = n1 / kbl
           if( ( nblr*kbl )/=n1 )nblr = nblr + 1_ilp
           ! .. the tiling is nblr-by-nblc [tiles]
           nblc = ( n-n1 ) / kbl
           if( ( nblc*kbl )/=( n-n1 ) )nblc = nblc + 1_ilp
           blskip = ( kbl**2_ilp ) + 1_ilp
      ! [tp] blkskip is a tuning parameter that depends on swband and kbl.
           rowskip = min( 5_ilp, kbl )
      ! [tp] rowskip is a tuning parameter.
           swband = 0_ilp
      ! [tp] swband is a tuning parameter. it is meaningful and effective
           ! if stdlib_sgesvj is used as a computational routine in the preconditioned
           ! jacobi svd algorithm stdlib_sgesvj.
           ! | *   *   * [x] [x] [x]|
           ! | *   *   * [x] [x] [x]|    row-cycling in the nblr-by-nblc [x] blocks.
           ! | *   *   * [x] [x] [x]|    row-cyclic pivoting inside each [x] block.
           ! |[x] [x] [x] *   *   * |
           ! |[x] [x] [x] *   *   * |
           ! |[x] [x] [x] *   *   * |
           loop_1993: do i = 1, nsweep
           ! .. go go go ...
              mxaapq = zero
              mxsinj = zero
              iswrot = 0_ilp
              notrot = 0_ilp
              pskipped = 0_ilp
              loop_2000: do ibr = 1, nblr
                 igl = ( ibr-1 )*kbl + 1_ilp
      ! ........................................................
       ! ... go to the off diagonal blocks
                 igl = ( ibr-1 )*kbl + 1_ilp
                 loop_2010: do jbc = 1, nblc
                    jgl = n1 + ( jbc-1 )*kbl + 1_ilp
              ! doing the block at ( ibr, jbc )
                    ijblsk = 0_ilp
                    loop_2100: do p = igl, min( igl+kbl-1, n1 )
                       aapp = sva( p )
                       if( aapp>zero ) then
                          pskipped = 0_ilp
                          loop_2200: do q = jgl, min( jgl+kbl-1, n )
                             aaqq = sva( q )
                             if( aaqq>zero ) then
                                aapp0 = aapp
           ! M X 2 Jacobi Svd 
              ! Safe Gram Matrix Computation 
                                if( aaqq>=one ) then
                                   if( aapp>=aaqq ) then
                                      rotok = ( small*aapp )<=aaqq
                                   else
                                      rotok = ( small*aaqq )<=aapp
                                   end if
                                   if( aapp<( big / aaqq ) ) then
                                      aapq = ( stdlib_sdot( m, a( 1_ilp, p ), 1_ilp, a( 1_ilp,q ), 1_ilp )*d( p )&
                                                *d( q ) / aaqq )/ aapp
                                   else
                                      call stdlib_scopy( m, a( 1_ilp, p ), 1_ilp, work, 1_ilp )
                                      call stdlib_slascl( 'G', 0_ilp, 0_ilp, aapp, d( p ),m, 1_ilp, work, lda,&
                                                 ierr )
                                      aapq = stdlib_sdot( m, work, 1_ilp, a( 1_ilp, q ),1_ilp )*d( q ) / &
                                                aaqq
                                   end if
                                else
                                   if( aapp>=aaqq ) then
                                      rotok = aapp<=( aaqq / small )
                                   else
                                      rotok = aaqq<=( aapp / small )
                                   end if
                                   if( aapp>( small / aaqq ) ) then
                                      aapq = ( stdlib_sdot( m, a( 1_ilp, p ), 1_ilp, a( 1_ilp,q ), 1_ilp )*d( p )&
                                                *d( q ) / aaqq )/ aapp
                                   else
                                      call stdlib_scopy( m, a( 1_ilp, q ), 1_ilp, work, 1_ilp )
                                      call stdlib_slascl( 'G', 0_ilp, 0_ilp, aaqq, d( q ),m, 1_ilp, work, lda,&
                                                 ierr )
                                      aapq = stdlib_sdot( m, work, 1_ilp, a( 1_ilp, p ),1_ilp )*d( p ) / &
                                                aapp
                                   end if
                                end if
                                mxaapq = max( mxaapq, abs( aapq ) )
              ! to rotate or not to rotate, that is the question ...
                                if( abs( aapq )>tol ) then
                                   notrot = 0_ilp
                 ! rotated  = rotated + 1
                                   pskipped = 0_ilp
                                   iswrot = iswrot + 1_ilp
                                   if( rotok ) then
                                      aqoap = aaqq / aapp
                                      apoaq = aapp / aaqq
                                      theta = -half*abs( aqoap-apoaq ) / aapq
                                      if( aaqq>aapp0 )theta = -theta
                                      if( abs( theta )>bigtheta ) then
                                         t = half / theta
                                         fastr( 3_ilp ) = t*d( p ) / d( q )
                                         fastr( 4_ilp ) = -t*d( q ) / d( p )
                                         call stdlib_srotm( m, a( 1_ilp, p ), 1_ilp,a( 1_ilp, q ), 1_ilp, fastr )
                                                   
                                         if( rsvec )call stdlib_srotm( mvl,v( 1_ilp, p ), 1_ilp,v( 1_ilp, q ),&
                                                    1_ilp,fastr )
                                         sva( q ) = aaqq*sqrt( max( zero,one+t*apoaq*aapq ) )
                                                   
                                         aapp = aapp*sqrt( max( zero,one-t*aqoap*aapq ) )
                                         mxsinj = max( mxsinj, abs( t ) )
                                      else
                       ! Choose Correct Signum For Theta And Rotate
                                         thsign = -sign( one, aapq )
                                         if( aaqq>aapp0 )thsign = -thsign
                                         t = one / ( theta+thsign*sqrt( one+theta*theta ) )
                                                   
                                         cs = sqrt( one / ( one+t*t ) )
                                         sn = t*cs
                                         mxsinj = max( mxsinj, abs( sn ) )
                                         sva( q ) = aaqq*sqrt( max( zero,one+t*apoaq*aapq ) )
                                                   
                                         aapp = aapp*sqrt( max( zero,one-t*aqoap*aapq ) )
                                         apoaq = d( p ) / d( q )
                                         aqoap = d( q ) / d( p )
                                         if( d( p )>=one ) then
                                            if( d( q )>=one ) then
                                               fastr( 3_ilp ) = t*apoaq
                                               fastr( 4_ilp ) = -t*aqoap
                                               d( p ) = d( p )*cs
                                               d( q ) = d( q )*cs
                                               call stdlib_srotm( m, a( 1_ilp, p ), 1_ilp,a( 1_ilp, q ), 1_ilp,&
                                                         fastr )
                                               if( rsvec )call stdlib_srotm( mvl,v( 1_ilp, p ), 1_ilp, v( &
                                                         1_ilp, q ),1_ilp, fastr )
                                            else
                                               call stdlib_saxpy( m, -t*aqoap,a( 1_ilp, q ), 1_ilp,a( 1_ilp, &
                                                         p ), 1_ilp )
                                               call stdlib_saxpy( m, cs*sn*apoaq,a( 1_ilp, p ), 1_ilp,a( &
                                                         1_ilp, q ), 1_ilp )
                                               if( rsvec ) then
                                                  call stdlib_saxpy( mvl, -t*aqoap,v( 1_ilp, q ), 1_ilp,v(&
                                                             1_ilp, p ), 1_ilp )
                                                  call stdlib_saxpy( mvl,cs*sn*apoaq,v( 1_ilp, p ), 1_ilp,&
                                                            v( 1_ilp, q ), 1_ilp )
                                               end if
                                               d( p ) = d( p )*cs
                                               d( q ) = d( q ) / cs
                                            end if
                                         else
                                            if( d( q )>=one ) then
                                               call stdlib_saxpy( m, t*apoaq,a( 1_ilp, p ), 1_ilp,a( 1_ilp, q &
                                                         ), 1_ilp )
                                               call stdlib_saxpy( m, -cs*sn*aqoap,a( 1_ilp, q ), 1_ilp,a( &
                                                         1_ilp, p ), 1_ilp )
                                               if( rsvec ) then
                                                  call stdlib_saxpy( mvl, t*apoaq,v( 1_ilp, p ), 1_ilp,v( &
                                                            1_ilp, q ), 1_ilp )
                                                  call stdlib_saxpy( mvl,-cs*sn*aqoap,v( 1_ilp, q ), &
                                                            1_ilp,v( 1_ilp, p ), 1_ilp )
                                               end if
                                               d( p ) = d( p ) / cs
                                               d( q ) = d( q )*cs
                                            else
                                               if( d( p )>=d( q ) ) then
                                                  call stdlib_saxpy( m, -t*aqoap,a( 1_ilp, q ), 1_ilp,a( &
                                                            1_ilp, p ), 1_ilp )
                                                  call stdlib_saxpy( m, cs*sn*apoaq,a( 1_ilp, p ), 1_ilp,&
                                                            a( 1_ilp, q ), 1_ilp )
                                                  d( p ) = d( p )*cs
                                                  d( q ) = d( q ) / cs
                                                  if( rsvec ) then
                                                     call stdlib_saxpy( mvl,-t*aqoap,v( 1_ilp, q ), 1_ilp,&
                                                               v( 1_ilp, p ), 1_ilp )
                                                     call stdlib_saxpy( mvl,cs*sn*apoaq,v( 1_ilp, p ),&
                                                                1_ilp,v( 1_ilp, q ), 1_ilp )
                                                  end if
                                               else
                                                  call stdlib_saxpy( m, t*apoaq,a( 1_ilp, p ), 1_ilp,a( 1_ilp,&
                                                             q ), 1_ilp )
                                                  call stdlib_saxpy( m,-cs*sn*aqoap,a( 1_ilp, q ), 1_ilp,&
                                                            a( 1_ilp, p ), 1_ilp )
                                                  d( p ) = d( p ) / cs
                                                  d( q ) = d( q )*cs
                                                  if( rsvec ) then
                                                     call stdlib_saxpy( mvl,t*apoaq, v( 1_ilp, p ),1_ilp, &
                                                               v( 1_ilp, q ), 1_ilp )
                                                     call stdlib_saxpy( mvl,-cs*sn*aqoap,v( 1_ilp, q )&
                                                               , 1_ilp,v( 1_ilp, p ), 1_ilp )
                                                  end if
                                               end if
                                            end if
                                         end if
                                      end if
                                   else
                                      if( aapp>aaqq ) then
                                         call stdlib_scopy( m, a( 1_ilp, p ), 1_ilp, work,1_ilp )
                                         call stdlib_slascl( 'G', 0_ilp, 0_ilp, aapp, one,m, 1_ilp, work, lda,&
                                                    ierr )
                                         call stdlib_slascl( 'G', 0_ilp, 0_ilp, aaqq, one,m, 1_ilp, a( 1_ilp, q ),&
                                                    lda,ierr )
                                         temp1 = -aapq*d( p ) / d( q )
                                         call stdlib_saxpy( m, temp1, work, 1_ilp,a( 1_ilp, q ), 1_ilp )
                                                   
                                         call stdlib_slascl( 'G', 0_ilp, 0_ilp, one, aaqq,m, 1_ilp, a( 1_ilp, q ),&
                                                    lda,ierr )
                                         sva( q ) = aaqq*sqrt( max( zero,one-aapq*aapq ) )
                                         mxsinj = max( mxsinj, sfmin )
                                      else
                                         call stdlib_scopy( m, a( 1_ilp, q ), 1_ilp, work,1_ilp )
                                         call stdlib_slascl( 'G', 0_ilp, 0_ilp, aaqq, one,m, 1_ilp, work, lda,&
                                                    ierr )
                                         call stdlib_slascl( 'G', 0_ilp, 0_ilp, aapp, one,m, 1_ilp, a( 1_ilp, p ),&
                                                    lda,ierr )
                                         temp1 = -aapq*d( q ) / d( p )
                                         call stdlib_saxpy( m, temp1, work, 1_ilp,a( 1_ilp, p ), 1_ilp )
                                                   
                                         call stdlib_slascl( 'G', 0_ilp, 0_ilp, one, aapp,m, 1_ilp, a( 1_ilp, p ),&
                                                    lda,ierr )
                                         sva( p ) = aapp*sqrt( max( zero,one-aapq*aapq ) )
                                         mxsinj = max( mxsinj, sfmin )
                                      end if
                                   end if
                 ! end if rotok then ... else
                 ! in the case of cancellation in updating sva(q)
                 ! .. recompute sva(q)
                                   if( ( sva( q ) / aaqq )**2_ilp<=rooteps )then
                                      if( ( aaqq<rootbig ) .and.( aaqq>rootsfmin ) ) then
                                         sva( q ) = stdlib_snrm2( m, a( 1_ilp, q ), 1_ilp )*d( q )
                                      else
                                         t = zero
                                         aaqq = one
                                         call stdlib_slassq( m, a( 1_ilp, q ), 1_ilp, t,aaqq )
                                         sva( q ) = t*sqrt( aaqq )*d( q )
                                      end if
                                   end if
                                   if( ( aapp / aapp0 )**2_ilp<=rooteps ) then
                                      if( ( aapp<rootbig ) .and.( aapp>rootsfmin ) ) then
                                         aapp = stdlib_snrm2( m, a( 1_ilp, p ), 1_ilp )*d( p )
                                      else
                                         t = zero
                                         aapp = one
                                         call stdlib_slassq( m, a( 1_ilp, p ), 1_ilp, t,aapp )
                                         aapp = t*sqrt( aapp )*d( p )
                                      end if
                                      sva( p ) = aapp
                                   end if
                    ! end of ok rotation
                                else
                                   notrot = notrot + 1_ilp
                 ! skipped  = skipped  + 1
                                   pskipped = pskipped + 1_ilp
                                   ijblsk = ijblsk + 1_ilp
                                end if
                             else
                                notrot = notrot + 1_ilp
                                pskipped = pskipped + 1_ilp
                                ijblsk = ijblsk + 1_ilp
                             end if
            ! if ( notrot >= emptsw )  go to 2011
                             if( ( i<=swband ) .and. ( ijblsk>=blskip ) )then
                                sva( p ) = aapp
                                notrot = 0_ilp
                                go to 2011
                             end if
                             if( ( i<=swband ) .and.( pskipped>rowskip ) ) then
                                aapp = -aapp
                                notrot = 0_ilp
                                go to 2203
                             end if
                          end do loop_2200
              ! end of the q-loop
              2203 continue
                          sva( p ) = aapp
                       else
                          if( aapp==zero )notrot = notrot +min( jgl+kbl-1, n ) - jgl + 1_ilp
                          if( aapp<zero )notrot = 0_ilp
      ! **      if ( notrot >= emptsw )  go to 2011
                       end if
                    end do loop_2100
           ! end of the p-loop
                 end do loop_2010
           ! end of the jbc-loop
           2011 continue
      ! 2011 bailed out of the jbc-loop
                 do p = igl, min( igl+kbl-1, n )
                    sva( p ) = abs( sva( p ) )
                 end do
      ! **   if ( notrot >= emptsw ) go to 1994
              end do loop_2000
      ! 2000 :: end of the ibr-loop
           ! .. update sva(n)
              if( ( sva( n )<rootbig ) .and. ( sva( n )>rootsfmin ) )then
                 sva( n ) = stdlib_snrm2( m, a( 1_ilp, n ), 1_ilp )*d( n )
              else
                 t = zero
                 aapp = one
                 call stdlib_slassq( m, a( 1_ilp, n ), 1_ilp, t, aapp )
                 sva( n ) = t*sqrt( aapp )*d( n )
              end if
           ! additional steering devices
              if( ( i<swband ) .and. ( ( mxaapq<=roottol ) .or.( iswrot<=n ) ) )swband = i
              if( ( i>swband+1 ) .and. ( mxaapq<real( n,KIND=sp)*tol ) .and.( real( n,KIND=sp)&
                        *mxaapq*mxsinj<tol ) ) then
                 go to 1994
              end if
              if( notrot>=emptsw )go to 1994
           end do loop_1993
           ! end i=1:nsweep loop
       ! #:) reaching this point means that the procedure has completed the given
           ! number of sweeps.
           info = nsweep - 1_ilp
           go to 1995
           1994 continue
       ! #:) reaching this point means that during the i-th sweep all pivots were
           ! below the given threshold, causing early exit.
           info = 0_ilp
       ! #:) info = 0 confirms successful iterations.
       1995 continue
           ! sort the vector d
           do p = 1, n - 1
              q = stdlib_isamax( n-p+1, sva( p ), 1_ilp ) + p - 1_ilp
              if( p/=q ) then
                 temp1 = sva( p )
                 sva( p ) = sva( q )
                 sva( q ) = temp1
                 temp1 = d( p )
                 d( p ) = d( q )
                 d( q ) = temp1
                 call stdlib_sswap( m, a( 1_ilp, p ), 1_ilp, a( 1_ilp, q ), 1_ilp )
                 if( rsvec )call stdlib_sswap( mvl, v( 1_ilp, p ), 1_ilp, v( 1_ilp, q ), 1_ilp )
              end if
           end do
           return
     end subroutine stdlib_sgsvj1

     pure module subroutine stdlib_dgsvj1( jobv, m, n, n1, a, lda, d, sva, mv, v, ldv,eps, sfmin, tol, &
     !! DGSVJ1 is called from DGESVJ as a pre-processor and that is its main
     !! purpose. It applies Jacobi rotations in the same way as DGESVJ does, but
     !! it targets only particular pivots and it does not check convergence
     !! (stopping criterion). Few tuning parameters (marked by [TP]) are
     !! available for the implementer.
     !! Further Details
     !! ~~~~~~~~~~~~~~~
     !! DGSVJ1 applies few sweeps of Jacobi rotations in the column space of
     !! the input M-by-N matrix A. The pivot pairs are taken from the (1,2)
     !! off-diagonal block in the corresponding N-by-N Gram matrix A^T * A. The
     !! block-entries (tiles) of the (1,2) off-diagonal block are marked by the
     !! [x]'s in the following scheme:
     !! | *  *  * [x] [x] [x]|
     !! | *  *  * [x] [x] [x]|    Row-cycling in the nblr-by-nblc [x] blocks.
     !! | *  *  * [x] [x] [x]|    Row-cyclic pivoting inside each [x] block.
     !! |[x] [x] [x] *  *  * |
     !! |[x] [x] [x] *  *  * |
     !! |[x] [x] [x] *  *  * |
     !! In terms of the columns of A, the first N1 columns are rotated 'against'
     !! the remaining N-N1 columns, trying to increase the angle between the
     !! corresponding subspaces. The off-diagonal block is N1-by(N-N1) and it is
     !! tiled using quadratic tiles of side KBL. Here, KBL is a tuning parameter.
     !! The number of sweeps is given in NSWEEP and the orthogonality threshold
     !! is given in TOL.
               nsweep, work, lwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           real(dp), intent(in) :: eps, sfmin, tol
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldv, lwork, m, mv, n, n1, nsweep
           character, intent(in) :: jobv
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*), d(n), sva(n), v(ldv,*)
           real(dp), intent(out) :: work(lwork)
        ! =====================================================================
           
           ! Local Scalars 
           real(dp) :: aapp, aapp0, aapq, aaqq, apoaq, aqoap, big, bigtheta, cs, large, mxaapq, &
           mxsinj, rootbig, rooteps, rootsfmin, roottol, small, sn, t, temp1, theta, &
                     thsign
           integer(ilp) :: blskip, emptsw, i, ibr, igl, ierr, ijblsk, iswrot, jbc, jgl, kbl, mvl, &
                     notrot, nblc, nblr, p, pskipped, q, rowskip, swband
           logical(lk) :: applv, rotok, rsvec
           ! Local Arrays 
           real(dp) :: fastr(5_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           applv = stdlib_lsame( jobv, 'A' )
           rsvec = stdlib_lsame( jobv, 'V' )
           if( .not.( rsvec .or. applv .or. stdlib_lsame( jobv, 'N' ) ) ) then
              info = -1_ilp
           else if( m<0_ilp ) then
              info = -2_ilp
           else if( ( n<0_ilp ) .or. ( n>m ) ) then
              info = -3_ilp
           else if( n1<0_ilp ) then
              info = -4_ilp
           else if( lda<m ) then
              info = -6_ilp
           else if( ( rsvec.or.applv ) .and. ( mv<0_ilp ) ) then
              info = -9_ilp
           else if( ( rsvec.and.( ldv<n ) ).or.( applv.and.( ldv<mv ) )  ) then
              info = -11_ilp
           else if( tol<=eps ) then
              info = -14_ilp
           else if( nsweep<0_ilp ) then
              info = -15_ilp
           else if( lwork<m ) then
              info = -17_ilp
           else
              info = 0_ilp
           end if
           ! #:(
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DGSVJ1', -info )
              return
           end if
           if( rsvec ) then
              mvl = n
           else if( applv ) then
              mvl = mv
           end if
           rsvec = rsvec .or. applv
           rooteps = sqrt( eps )
           rootsfmin = sqrt( sfmin )
           small = sfmin / eps
           big = one / sfmin
           rootbig = one / rootsfmin
           large = big / sqrt( real( m*n,KIND=dp) )
           bigtheta = one / rooteps
           roottol = sqrt( tol )
           ! Initialize The Right Singular Vector Matrix 
           ! rsvec = stdlib_lsame( jobv, 'y' )
           emptsw = n1*( n-n1 )
           notrot = 0_ilp
           fastr( 1_ilp ) = zero
           ! .. row-cyclic pivot strategy with de rijk's pivoting ..
           kbl = min( 8_ilp, n )
           nblr = n1 / kbl
           if( ( nblr*kbl )/=n1 )nblr = nblr + 1_ilp
           ! .. the tiling is nblr-by-nblc [tiles]
           nblc = ( n-n1 ) / kbl
           if( ( nblc*kbl )/=( n-n1 ) )nblc = nblc + 1_ilp
           blskip = ( kbl**2_ilp ) + 1_ilp
      ! [tp] blkskip is a tuning parameter that depends on swband and kbl.
           rowskip = min( 5_ilp, kbl )
      ! [tp] rowskip is a tuning parameter.
           swband = 0_ilp
      ! [tp] swband is a tuning parameter. it is meaningful and effective
           ! if stdlib_sgesvj is used as a computational routine in the preconditioned
           ! jacobi svd algorithm stdlib_sgesvj.
           ! | *   *   * [x] [x] [x]|
           ! | *   *   * [x] [x] [x]|    row-cycling in the nblr-by-nblc [x] blocks.
           ! | *   *   * [x] [x] [x]|    row-cyclic pivoting inside each [x] block.
           ! |[x] [x] [x] *   *   * |
           ! |[x] [x] [x] *   *   * |
           ! |[x] [x] [x] *   *   * |
           loop_1993: do i = 1, nsweep
           ! .. go go go ...
              mxaapq = zero
              mxsinj = zero
              iswrot = 0_ilp
              notrot = 0_ilp
              pskipped = 0_ilp
              loop_2000: do ibr = 1, nblr
                 igl = ( ibr-1 )*kbl + 1_ilp
      ! ........................................................
       ! ... go to the off diagonal blocks
                 igl = ( ibr-1 )*kbl + 1_ilp
                 loop_2010: do jbc = 1, nblc
                    jgl = n1 + ( jbc-1 )*kbl + 1_ilp
              ! doing the block at ( ibr, jbc )
                    ijblsk = 0_ilp
                    loop_2100: do p = igl, min( igl+kbl-1, n1 )
                       aapp = sva( p )
                       if( aapp>zero ) then
                          pskipped = 0_ilp
                          loop_2200: do q = jgl, min( jgl+kbl-1, n )
                             aaqq = sva( q )
                             if( aaqq>zero ) then
                                aapp0 = aapp
           ! M X 2 Jacobi Svd 
              ! Safe Gram Matrix Computation 
                                if( aaqq>=one ) then
                                   if( aapp>=aaqq ) then
                                      rotok = ( small*aapp )<=aaqq
                                   else
                                      rotok = ( small*aaqq )<=aapp
                                   end if
                                   if( aapp<( big / aaqq ) ) then
                                      aapq = ( stdlib_ddot( m, a( 1_ilp, p ), 1_ilp, a( 1_ilp,q ), 1_ilp )*d( p )&
                                                *d( q ) / aaqq )/ aapp
                                   else
                                      call stdlib_dcopy( m, a( 1_ilp, p ), 1_ilp, work, 1_ilp )
                                      call stdlib_dlascl( 'G', 0_ilp, 0_ilp, aapp, d( p ),m, 1_ilp, work, lda,&
                                                 ierr )
                                      aapq = stdlib_ddot( m, work, 1_ilp, a( 1_ilp, q ),1_ilp )*d( q ) / &
                                                aaqq
                                   end if
                                else
                                   if( aapp>=aaqq ) then
                                      rotok = aapp<=( aaqq / small )
                                   else
                                      rotok = aaqq<=( aapp / small )
                                   end if
                                   if( aapp>( small / aaqq ) ) then
                                      aapq = ( stdlib_ddot( m, a( 1_ilp, p ), 1_ilp, a( 1_ilp,q ), 1_ilp )*d( p )&
                                                *d( q ) / aaqq )/ aapp
                                   else
                                      call stdlib_dcopy( m, a( 1_ilp, q ), 1_ilp, work, 1_ilp )
                                      call stdlib_dlascl( 'G', 0_ilp, 0_ilp, aaqq, d( q ),m, 1_ilp, work, lda,&
                                                 ierr )
                                      aapq = stdlib_ddot( m, work, 1_ilp, a( 1_ilp, p ),1_ilp )*d( p ) / &
                                                aapp
                                   end if
                                end if
                                mxaapq = max( mxaapq, abs( aapq ) )
              ! to rotate or not to rotate, that is the question ...
                                if( abs( aapq )>tol ) then
                                   notrot = 0_ilp
                 ! rotated  = rotated + 1
                                   pskipped = 0_ilp
                                   iswrot = iswrot + 1_ilp
                                   if( rotok ) then
                                      aqoap = aaqq / aapp
                                      apoaq = aapp / aaqq
                                      theta = -half*abs(aqoap-apoaq) / aapq
                                      if( aaqq>aapp0 )theta = -theta
                                      if( abs( theta )>bigtheta ) then
                                         t = half / theta
                                         fastr( 3_ilp ) = t*d( p ) / d( q )
                                         fastr( 4_ilp ) = -t*d( q ) / d( p )
                                         call stdlib_drotm( m, a( 1_ilp, p ), 1_ilp,a( 1_ilp, q ), 1_ilp, fastr )
                                                   
                                         if( rsvec )call stdlib_drotm( mvl,v( 1_ilp, p ), 1_ilp,v( 1_ilp, q ),&
                                                    1_ilp,fastr )
                                         sva( q ) = aaqq*sqrt( max( zero,one+t*apoaq*aapq ) )
                                                   
                                         aapp = aapp*sqrt( max( zero,one-t*aqoap*aapq ) )
                                         mxsinj = max( mxsinj, abs( t ) )
                                      else
                       ! Choose Correct Signum For Theta And Rotate
                                         thsign = -sign( one, aapq )
                                         if( aaqq>aapp0 )thsign = -thsign
                                         t = one / ( theta+thsign*sqrt( one+theta*theta ) )
                                                   
                                         cs = sqrt( one / ( one+t*t ) )
                                         sn = t*cs
                                         mxsinj = max( mxsinj, abs( sn ) )
                                         sva( q ) = aaqq*sqrt( max( zero,one+t*apoaq*aapq ) )
                                                   
                                         aapp = aapp*sqrt( max( zero,one-t*aqoap*aapq ) )
                                         apoaq = d( p ) / d( q )
                                         aqoap = d( q ) / d( p )
                                         if( d( p )>=one ) then
                                            if( d( q )>=one ) then
                                               fastr( 3_ilp ) = t*apoaq
                                               fastr( 4_ilp ) = -t*aqoap
                                               d( p ) = d( p )*cs
                                               d( q ) = d( q )*cs
                                               call stdlib_drotm( m, a( 1_ilp, p ), 1_ilp,a( 1_ilp, q ), 1_ilp,&
                                                         fastr )
                                               if( rsvec )call stdlib_drotm( mvl,v( 1_ilp, p ), 1_ilp, v( &
                                                         1_ilp, q ),1_ilp, fastr )
                                            else
                                               call stdlib_daxpy( m, -t*aqoap,a( 1_ilp, q ), 1_ilp,a( 1_ilp, &
                                                         p ), 1_ilp )
                                               call stdlib_daxpy( m, cs*sn*apoaq,a( 1_ilp, p ), 1_ilp,a( &
                                                         1_ilp, q ), 1_ilp )
                                               if( rsvec ) then
                                                  call stdlib_daxpy( mvl, -t*aqoap,v( 1_ilp, q ), 1_ilp,v(&
                                                             1_ilp, p ), 1_ilp )
                                                  call stdlib_daxpy( mvl,cs*sn*apoaq,v( 1_ilp, p ), 1_ilp,&
                                                            v( 1_ilp, q ), 1_ilp )
                                               end if
                                               d( p ) = d( p )*cs
                                               d( q ) = d( q ) / cs
                                            end if
                                         else
                                            if( d( q )>=one ) then
                                               call stdlib_daxpy( m, t*apoaq,a( 1_ilp, p ), 1_ilp,a( 1_ilp, q &
                                                         ), 1_ilp )
                                               call stdlib_daxpy( m, -cs*sn*aqoap,a( 1_ilp, q ), 1_ilp,a( &
                                                         1_ilp, p ), 1_ilp )
                                               if( rsvec ) then
                                                  call stdlib_daxpy( mvl, t*apoaq,v( 1_ilp, p ), 1_ilp,v( &
                                                            1_ilp, q ), 1_ilp )
                                                  call stdlib_daxpy( mvl,-cs*sn*aqoap,v( 1_ilp, q ), &
                                                            1_ilp,v( 1_ilp, p ), 1_ilp )
                                               end if
                                               d( p ) = d( p ) / cs
                                               d( q ) = d( q )*cs
                                            else
                                               if( d( p )>=d( q ) ) then
                                                  call stdlib_daxpy( m, -t*aqoap,a( 1_ilp, q ), 1_ilp,a( &
                                                            1_ilp, p ), 1_ilp )
                                                  call stdlib_daxpy( m, cs*sn*apoaq,a( 1_ilp, p ), 1_ilp,&
                                                            a( 1_ilp, q ), 1_ilp )
                                                  d( p ) = d( p )*cs
                                                  d( q ) = d( q ) / cs
                                                  if( rsvec ) then
                                                     call stdlib_daxpy( mvl,-t*aqoap,v( 1_ilp, q ), 1_ilp,&
                                                               v( 1_ilp, p ), 1_ilp )
                                                     call stdlib_daxpy( mvl,cs*sn*apoaq,v( 1_ilp, p ),&
                                                                1_ilp,v( 1_ilp, q ), 1_ilp )
                                                  end if
                                               else
                                                  call stdlib_daxpy( m, t*apoaq,a( 1_ilp, p ), 1_ilp,a( 1_ilp,&
                                                             q ), 1_ilp )
                                                  call stdlib_daxpy( m,-cs*sn*aqoap,a( 1_ilp, q ), 1_ilp,&
                                                            a( 1_ilp, p ), 1_ilp )
                                                  d( p ) = d( p ) / cs
                                                  d( q ) = d( q )*cs
                                                  if( rsvec ) then
                                                     call stdlib_daxpy( mvl,t*apoaq, v( 1_ilp, p ),1_ilp, &
                                                               v( 1_ilp, q ), 1_ilp )
                                                     call stdlib_daxpy( mvl,-cs*sn*aqoap,v( 1_ilp, q )&
                                                               , 1_ilp,v( 1_ilp, p ), 1_ilp )
                                                  end if
                                               end if
                                            end if
                                         end if
                                      end if
                                   else
                                      if( aapp>aaqq ) then
                                         call stdlib_dcopy( m, a( 1_ilp, p ), 1_ilp, work,1_ilp )
                                         call stdlib_dlascl( 'G', 0_ilp, 0_ilp, aapp, one,m, 1_ilp, work, lda,&
                                                    ierr )
                                         call stdlib_dlascl( 'G', 0_ilp, 0_ilp, aaqq, one,m, 1_ilp, a( 1_ilp, q ),&
                                                    lda,ierr )
                                         temp1 = -aapq*d( p ) / d( q )
                                         call stdlib_daxpy( m, temp1, work, 1_ilp,a( 1_ilp, q ), 1_ilp )
                                                   
                                         call stdlib_dlascl( 'G', 0_ilp, 0_ilp, one, aaqq,m, 1_ilp, a( 1_ilp, q ),&
                                                    lda,ierr )
                                         sva( q ) = aaqq*sqrt( max( zero,one-aapq*aapq ) )
                                         mxsinj = max( mxsinj, sfmin )
                                      else
                                         call stdlib_dcopy( m, a( 1_ilp, q ), 1_ilp, work,1_ilp )
                                         call stdlib_dlascl( 'G', 0_ilp, 0_ilp, aaqq, one,m, 1_ilp, work, lda,&
                                                    ierr )
                                         call stdlib_dlascl( 'G', 0_ilp, 0_ilp, aapp, one,m, 1_ilp, a( 1_ilp, p ),&
                                                    lda,ierr )
                                         temp1 = -aapq*d( q ) / d( p )
                                         call stdlib_daxpy( m, temp1, work, 1_ilp,a( 1_ilp, p ), 1_ilp )
                                                   
                                         call stdlib_dlascl( 'G', 0_ilp, 0_ilp, one, aapp,m, 1_ilp, a( 1_ilp, p ),&
                                                    lda,ierr )
                                         sva( p ) = aapp*sqrt( max( zero,one-aapq*aapq ) )
                                         mxsinj = max( mxsinj, sfmin )
                                      end if
                                   end if
                 ! end if rotok then ... else
                 ! in the case of cancellation in updating sva(q)
                 ! .. recompute sva(q)
                                   if( ( sva( q ) / aaqq )**2_ilp<=rooteps )then
                                      if( ( aaqq<rootbig ) .and.( aaqq>rootsfmin ) ) then
                                         sva( q ) = stdlib_dnrm2( m, a( 1_ilp, q ), 1_ilp )*d( q )
                                      else
                                         t = zero
                                         aaqq = one
                                         call stdlib_dlassq( m, a( 1_ilp, q ), 1_ilp, t,aaqq )
                                         sva( q ) = t*sqrt( aaqq )*d( q )
                                      end if
                                   end if
                                   if( ( aapp / aapp0 )**2_ilp<=rooteps ) then
                                      if( ( aapp<rootbig ) .and.( aapp>rootsfmin ) ) then
                                         aapp = stdlib_dnrm2( m, a( 1_ilp, p ), 1_ilp )*d( p )
                                      else
                                         t = zero
                                         aapp = one
                                         call stdlib_dlassq( m, a( 1_ilp, p ), 1_ilp, t,aapp )
                                         aapp = t*sqrt( aapp )*d( p )
                                      end if
                                      sva( p ) = aapp
                                   end if
                    ! end of ok rotation
                                else
                                   notrot = notrot + 1_ilp
                 ! skipped  = skipped  + 1
                                   pskipped = pskipped + 1_ilp
                                   ijblsk = ijblsk + 1_ilp
                                end if
                             else
                                notrot = notrot + 1_ilp
                                pskipped = pskipped + 1_ilp
                                ijblsk = ijblsk + 1_ilp
                             end if
            ! if ( notrot >= emptsw )  go to 2011
                             if( ( i<=swband ) .and. ( ijblsk>=blskip ) )then
                                sva( p ) = aapp
                                notrot = 0_ilp
                                go to 2011
                             end if
                             if( ( i<=swband ) .and.( pskipped>rowskip ) ) then
                                aapp = -aapp
                                notrot = 0_ilp
                                go to 2203
                             end if
                          end do loop_2200
              ! end of the q-loop
              2203 continue
                          sva( p ) = aapp
                       else
                          if( aapp==zero )notrot = notrot +min( jgl+kbl-1, n ) - jgl + 1_ilp
                          if( aapp<zero )notrot = 0_ilp
      ! **      if ( notrot >= emptsw )  go to 2011
                       end if
                    end do loop_2100
           ! end of the p-loop
                 end do loop_2010
           ! end of the jbc-loop
           2011 continue
      ! 2011 bailed out of the jbc-loop
                 do p = igl, min( igl+kbl-1, n )
                    sva( p ) = abs( sva( p ) )
                 end do
      ! **   if ( notrot >= emptsw ) go to 1994
              end do loop_2000
      ! 2000 :: end of the ibr-loop
           ! .. update sva(n)
              if( ( sva( n )<rootbig ) .and. ( sva( n )>rootsfmin ) )then
                 sva( n ) = stdlib_dnrm2( m, a( 1_ilp, n ), 1_ilp )*d( n )
              else
                 t = zero
                 aapp = one
                 call stdlib_dlassq( m, a( 1_ilp, n ), 1_ilp, t, aapp )
                 sva( n ) = t*sqrt( aapp )*d( n )
              end if
           ! additional steering devices
              if( ( i<swband ) .and. ( ( mxaapq<=roottol ) .or.( iswrot<=n ) ) )swband = i
              if( ( i>swband+1 ) .and. ( mxaapq<real( n,KIND=dp)*tol ) .and.( real( n,KIND=dp)&
                        *mxaapq*mxsinj<tol ) ) then
                 go to 1994
              end if
              if( notrot>=emptsw )go to 1994
           end do loop_1993
           ! end i=1:nsweep loop
       ! #:) reaching this point means that the procedure has completed the given
           ! number of sweeps.
           info = nsweep - 1_ilp
           go to 1995
           1994 continue
       ! #:) reaching this point means that during the i-th sweep all pivots were
           ! below the given threshold, causing early exit.
           info = 0_ilp
       ! #:) info = 0 confirms successful iterations.
       1995 continue
           ! sort the vector d
           do p = 1, n - 1
              q = stdlib_idamax( n-p+1, sva( p ), 1_ilp ) + p - 1_ilp
              if( p/=q ) then
                 temp1 = sva( p )
                 sva( p ) = sva( q )
                 sva( q ) = temp1
                 temp1 = d( p )
                 d( p ) = d( q )
                 d( q ) = temp1
                 call stdlib_dswap( m, a( 1_ilp, p ), 1_ilp, a( 1_ilp, q ), 1_ilp )
                 if( rsvec )call stdlib_dswap( mvl, v( 1_ilp, p ), 1_ilp, v( 1_ilp, q ), 1_ilp )
              end if
           end do
           return
     end subroutine stdlib_dgsvj1


     pure module subroutine stdlib_cgsvj1( jobv, m, n, n1, a, lda, d, sva, mv, v, ldv,eps, sfmin, tol, &
     !! CGSVJ1 is called from CGESVJ as a pre-processor and that is its main
     !! purpose. It applies Jacobi rotations in the same way as CGESVJ does, but
     !! it targets only particular pivots and it does not check convergence
     !! (stopping criterion). Few tuning parameters (marked by [TP]) are
     !! available for the implementer.
     !! Further Details
     !! ~~~~~~~~~~~~~~~
     !! CGSVJ1 applies few sweeps of Jacobi rotations in the column space of
     !! the input M-by-N matrix A. The pivot pairs are taken from the (1,2)
     !! off-diagonal block in the corresponding N-by-N Gram matrix A^T * A. The
     !! block-entries (tiles) of the (1,2) off-diagonal block are marked by the
     !! [x]'s in the following scheme:
     !! | *  *  * [x] [x] [x]|
     !! | *  *  * [x] [x] [x]|    Row-cycling in the nblr-by-nblc [x] blocks.
     !! | *  *  * [x] [x] [x]|    Row-cyclic pivoting inside each [x] block.
     !! |[x] [x] [x] *  *  * |
     !! |[x] [x] [x] *  *  * |
     !! |[x] [x] [x] *  *  * |
     !! In terms of the columns of A, the first N1 columns are rotated 'against'
     !! the remaining N-N1 columns, trying to increase the angle between the
     !! corresponding subspaces. The off-diagonal block is N1-by(N-N1) and it is
     !! tiled using quadratic tiles of side KBL. Here, KBL is a tuning parameter.
     !! The number of sweeps is given in NSWEEP and the orthogonality threshold
     !! is given in TOL.
               nsweep, work, lwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           real(sp), intent(in) :: eps, sfmin, tol
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldv, lwork, m, mv, n, n1, nsweep
           character, intent(in) :: jobv
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*), d(n), v(ldv,*)
           complex(sp), intent(out) :: work(lwork)
           real(sp), intent(inout) :: sva(n)
        ! =====================================================================
           
           ! Local Scalars 
           complex(sp) :: aapq, ompq
           real(sp) :: aapp, aapp0, aapq1, aaqq, apoaq, aqoap, big, bigtheta, cs, mxaapq, mxsinj, &
                     rootbig, rooteps, rootsfmin, roottol, small, sn, t, temp1, theta, thsign
           integer(ilp) :: blskip, emptsw, i, ibr, igl, ierr, ijblsk, iswrot, jbc, jgl, kbl, mvl, &
                     notrot, nblc, nblr, p, pskipped, q, rowskip, swband
           logical(lk) :: applv, rotok, rsvec
           ! Intrinsic Functions 
           ! From Lapack
           ! Executable Statements 
           ! test the input parameters.
           applv = stdlib_lsame( jobv, 'A' )
           rsvec = stdlib_lsame( jobv, 'V' )
           if( .not.( rsvec .or. applv .or. stdlib_lsame( jobv, 'N' ) ) ) then
              info = -1_ilp
           else if( m<0_ilp ) then
              info = -2_ilp
           else if( ( n<0_ilp ) .or. ( n>m ) ) then
              info = -3_ilp
           else if( n1<0_ilp ) then
              info = -4_ilp
           else if( lda<m ) then
              info = -6_ilp
           else if( ( rsvec.or.applv ) .and. ( mv<0_ilp ) ) then
              info = -9_ilp
           else if( ( rsvec.and.( ldv<n ) ).or.( applv.and.( ldv<mv ) )  ) then
              info = -11_ilp
           else if( tol<=eps ) then
              info = -14_ilp
           else if( nsweep<0_ilp ) then
              info = -15_ilp
           else if( lwork<m ) then
              info = -17_ilp
           else
              info = 0_ilp
           end if
           ! #:(
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CGSVJ1', -info )
              return
           end if
           if( rsvec ) then
              mvl = n
           else if( applv ) then
              mvl = mv
           end if
           rsvec = rsvec .or. applv
           rooteps = sqrt( eps )
           rootsfmin = sqrt( sfmin )
           small = sfmin / eps
           big = one / sfmin
           rootbig = one / rootsfmin
           ! large = big / sqrt( real( m*n,KIND=sp) )
           bigtheta = one / rooteps
           roottol = sqrt( tol )
           ! Initialize The Right Singular Vector Matrix 
           ! rsvec = stdlib_lsame( jobv, 'y' )
           emptsw = n1*( n-n1 )
           notrot = 0_ilp
           ! .. row-cyclic pivot strategy with de rijk's pivoting ..
           kbl = min( 8_ilp, n )
           nblr = n1 / kbl
           if( ( nblr*kbl )/=n1 )nblr = nblr + 1_ilp
           ! .. the tiling is nblr-by-nblc [tiles]
           nblc = ( n-n1 ) / kbl
           if( ( nblc*kbl )/=( n-n1 ) )nblc = nblc + 1_ilp
           blskip = ( kbl**2_ilp ) + 1_ilp
      ! [tp] blkskip is a tuning parameter that depends on swband and kbl.
           rowskip = min( 5_ilp, kbl )
      ! [tp] rowskip is a tuning parameter.
           swband = 0_ilp
      ! [tp] swband is a tuning parameter. it is meaningful and effective
           ! if stdlib_cgesvj is used as a computational routine in the preconditioned
           ! jacobi svd algorithm stdlib_cgejsv.
           ! | *   *   * [x] [x] [x]|
           ! | *   *   * [x] [x] [x]|    row-cycling in the nblr-by-nblc [x] blocks.
           ! | *   *   * [x] [x] [x]|    row-cyclic pivoting inside each [x] block.
           ! |[x] [x] [x] *   *   * |
           ! |[x] [x] [x] *   *   * |
           ! |[x] [x] [x] *   *   * |
           loop_1993: do i = 1, nsweep
           ! .. go go go ...
              mxaapq = zero
              mxsinj = zero
              iswrot = 0_ilp
              notrot = 0_ilp
              pskipped = 0_ilp
           ! each sweep is unrolled using kbl-by-kbl tiles over the pivot pairs
           ! 1 <= p < q <= n. this is the first step toward a blocked implementation
           ! of the rotations. new implementation, based on block transformations,
           ! is under development.
              loop_2000: do ibr = 1, nblr
                 igl = ( ibr-1 )*kbl + 1_ilp
       ! ... go to the off diagonal blocks
                 igl = ( ibr-1 )*kbl + 1_ilp
                  ! do 2010 jbc = ibr + 1, nbl
                 loop_2010: do jbc = 1, nblc
                    jgl = ( jbc-1 )*kbl + n1 + 1_ilp
              ! doing the block at ( ibr, jbc )
                    ijblsk = 0_ilp
                    loop_2100: do p = igl, min( igl+kbl-1, n1 )
                       aapp = sva( p )
                       if( aapp>zero ) then
                          pskipped = 0_ilp
                          loop_2200: do q = jgl, min( jgl+kbl-1, n )
                             aaqq = sva( q )
                             if( aaqq>zero ) then
                                aapp0 = aapp
           ! M X 2 Jacobi Svd 
              ! safe gram matrix computation
                                if( aaqq>=one ) then
                                   if( aapp>=aaqq ) then
                                      rotok = ( small*aapp )<=aaqq
                                   else
                                      rotok = ( small*aaqq )<=aapp
                                   end if
                                   if( aapp<( big / aaqq ) ) then
                                      aapq = ( stdlib_cdotc( m, a( 1_ilp, p ), 1_ilp,a( 1_ilp, q ), 1_ilp ) / &
                                                aaqq ) / aapp
                                   else
                                      call stdlib_ccopy( m, a( 1_ilp, p ), 1_ilp,work, 1_ilp )
                                      call stdlib_clascl( 'G', 0_ilp, 0_ilp, aapp,one, m, 1_ilp,work, lda, &
                                                ierr )
                                      aapq = stdlib_cdotc( m, work, 1_ilp,a( 1_ilp, q ), 1_ilp ) / &
                                                aaqq
                                   end if
                                else
                                   if( aapp>=aaqq ) then
                                      rotok = aapp<=( aaqq / small )
                                   else
                                      rotok = aaqq<=( aapp / small )
                                   end if
                                   if( aapp>( small / aaqq ) ) then
                                      aapq = ( stdlib_cdotc( m, a( 1_ilp, p ), 1_ilp,a( 1_ilp, q ), 1_ilp ) / max(&
                                                aaqq,aapp) )/ min(aaqq,aapp)
                                   else
                                      call stdlib_ccopy( m, a( 1_ilp, q ), 1_ilp,work, 1_ilp )
                                      call stdlib_clascl( 'G', 0_ilp, 0_ilp, aaqq,one, m, 1_ilp,work, lda, &
                                                ierr )
                                      aapq = stdlib_cdotc( m, a( 1_ilp, p ), 1_ilp,work, 1_ilp ) / &
                                                aapp
                                   end if
                                end if
                                 ! aapq = aapq * conjg(cwork(p))*cwork(q)
                                aapq1  = -abs(aapq)
                                mxaapq = max( mxaapq, -aapq1 )
              ! to rotate or not to rotate, that is the question ...
                                if( abs( aapq1 )>tol ) then
                                   ompq = aapq / abs(aapq)
                                   notrot = 0_ilp
      ! [rtd]      rotated  = rotated + 1
                                   pskipped = 0_ilp
                                   iswrot = iswrot + 1_ilp
                                   if( rotok ) then
                                      aqoap = aaqq / aapp
                                      apoaq = aapp / aaqq
                                      theta = -half*abs( aqoap-apoaq )/ aapq1
                                      if( aaqq>aapp0 )theta = -theta
                                      if( abs( theta )>bigtheta ) then
                                         t  = half / theta
                                         cs = one
                                         call stdlib_crot( m, a(1_ilp,p), 1_ilp, a(1_ilp,q), 1_ilp,cs, conjg(ompq)&
                                                   *t )
                                         if( rsvec ) then
                                             call stdlib_crot( mvl, v(1_ilp,p), 1_ilp,v(1_ilp,q), 1_ilp, cs, &
                                                       conjg(ompq)*t )
                                         end if
                                         sva( q ) = aaqq*sqrt( max( zero,one+t*apoaq*aapq1 ) )
                                                   
                                         aapp = aapp*sqrt( max( zero,one-t*aqoap*aapq1 ) )
                                         mxsinj = max( mxsinj, abs( t ) )
                                      else
                       ! Choose Correct Signum For Theta And Rotate
                                         thsign = -sign( one, aapq1 )
                                         if( aaqq>aapp0 )thsign = -thsign
                                         t = one / ( theta+thsign*sqrt( one+theta*theta ) )
                                                   
                                         cs = sqrt( one / ( one+t*t ) )
                                         sn = t*cs
                                         mxsinj = max( mxsinj, abs( sn ) )
                                         sva( q ) = aaqq*sqrt( max( zero,one+t*apoaq*aapq1 ) )
                                                   
                                         aapp = aapp*sqrt( max( zero,one-t*aqoap*aapq1 ) )
                                         call stdlib_crot( m, a(1_ilp,p), 1_ilp, a(1_ilp,q), 1_ilp,cs, conjg(ompq)&
                                                   *sn )
                                         if( rsvec ) then
                                             call stdlib_crot( mvl, v(1_ilp,p), 1_ilp,v(1_ilp,q), 1_ilp, cs, &
                                                       conjg(ompq)*sn )
                                         end if
                                      end if
                                      d(p) = -d(q) * ompq
                                   else
                    ! .. have to use modified gram-schmidt like transformation
                                    if( aapp>aaqq ) then
                                         call stdlib_ccopy( m, a( 1_ilp, p ), 1_ilp,work, 1_ilp )
                                         call stdlib_clascl( 'G', 0_ilp, 0_ilp, aapp, one,m, 1_ilp, work,lda,&
                                                   ierr )
                                         call stdlib_clascl( 'G', 0_ilp, 0_ilp, aaqq, one,m, 1_ilp, a( 1_ilp, q ),&
                                                    lda,ierr )
                                         call stdlib_caxpy( m, -aapq, work,1_ilp, a( 1_ilp, q ), 1_ilp )
                                                   
                                         call stdlib_clascl( 'G', 0_ilp, 0_ilp, one, aaqq,m, 1_ilp, a( 1_ilp, q ),&
                                                    lda,ierr )
                                         sva( q ) = aaqq*sqrt( max( zero,one-aapq1*aapq1 ) )
                                                   
                                         mxsinj = max( mxsinj, sfmin )
                                    else
                                        call stdlib_ccopy( m, a( 1_ilp, q ), 1_ilp,work, 1_ilp )
                                         call stdlib_clascl( 'G', 0_ilp, 0_ilp, aaqq, one,m, 1_ilp, work,lda,&
                                                   ierr )
                                         call stdlib_clascl( 'G', 0_ilp, 0_ilp, aapp, one,m, 1_ilp, a( 1_ilp, p ),&
                                                    lda,ierr )
                                         call stdlib_caxpy( m, -conjg(aapq),work, 1_ilp, a( 1_ilp, p ), 1_ilp &
                                                   )
                                         call stdlib_clascl( 'G', 0_ilp, 0_ilp, one, aapp,m, 1_ilp, a( 1_ilp, p ),&
                                                    lda,ierr )
                                         sva( p ) = aapp*sqrt( max( zero,one-aapq1*aapq1 ) )
                                                   
                                         mxsinj = max( mxsinj, sfmin )
                                    end if
                                   end if
                 ! end if rotok then ... else
                 ! in the case of cancellation in updating sva(q), sva(p)
                 ! .. recompute sva(q), sva(p)
                                   if( ( sva( q ) / aaqq )**2_ilp<=rooteps )then
                                      if( ( aaqq<rootbig ) .and.( aaqq>rootsfmin ) ) then
                                         sva( q ) = stdlib_scnrm2( m, a( 1_ilp, q ), 1_ilp)
                                       else
                                         t = zero
                                         aaqq = one
                                         call stdlib_classq( m, a( 1_ilp, q ), 1_ilp, t,aaqq )
                                         sva( q ) = t*sqrt( aaqq )
                                      end if
                                   end if
                                   if( ( aapp / aapp0 )**2_ilp<=rooteps ) then
                                      if( ( aapp<rootbig ) .and.( aapp>rootsfmin ) ) then
                                         aapp = stdlib_scnrm2( m, a( 1_ilp, p ), 1_ilp )
                                      else
                                         t = zero
                                         aapp = one
                                         call stdlib_classq( m, a( 1_ilp, p ), 1_ilp, t,aapp )
                                         aapp = t*sqrt( aapp )
                                      end if
                                      sva( p ) = aapp
                                   end if
                    ! end of ok rotation
                                else
                                   notrot = notrot + 1_ilp
      ! [rtd]      skipped  = skipped  + 1
                                   pskipped = pskipped + 1_ilp
                                   ijblsk = ijblsk + 1_ilp
                                end if
                             else
                                notrot = notrot + 1_ilp
                                pskipped = pskipped + 1_ilp
                                ijblsk = ijblsk + 1_ilp
                             end if
                             if( ( i<=swband ) .and. ( ijblsk>=blskip ) )then
                                sva( p ) = aapp
                                notrot = 0_ilp
                                go to 2011
                             end if
                             if( ( i<=swband ) .and.( pskipped>rowskip ) ) then
                                aapp = -aapp
                                notrot = 0_ilp
                                go to 2203
                             end if
                          end do loop_2200
              ! end of the q-loop
              2203 continue
                          sva( p ) = aapp
                       else
                          if( aapp==zero )notrot = notrot +min( jgl+kbl-1, n ) - jgl + 1_ilp
                          if( aapp<zero )notrot = 0_ilp
                       end if
                    end do loop_2100
           ! end of the p-loop
                 end do loop_2010
           ! end of the jbc-loop
           2011 continue
      ! 2011 bailed out of the jbc-loop
                 do p = igl, min( igl+kbl-1, n )
                    sva( p ) = abs( sva( p ) )
                 end do
      ! **
              end do loop_2000
      ! 2000 :: end of the ibr-loop
           ! .. update sva(n)
              if( ( sva( n )<rootbig ) .and. ( sva( n )>rootsfmin ) )then
                 sva( n ) = stdlib_scnrm2( m, a( 1_ilp, n ), 1_ilp )
              else
                 t = zero
                 aapp = one
                 call stdlib_classq( m, a( 1_ilp, n ), 1_ilp, t, aapp )
                 sva( n ) = t*sqrt( aapp )
              end if
           ! additional steering devices
              if( ( i<swband ) .and. ( ( mxaapq<=roottol ) .or.( iswrot<=n ) ) )swband = i
              if( ( i>swband+1 ) .and. ( mxaapq<sqrt( real( n,KIND=sp) )*tol ) .and. ( real( n,&
                        KIND=sp)*mxaapq*mxsinj<tol ) ) then
                 go to 1994
              end if
              if( notrot>=emptsw )go to 1994
           end do loop_1993
           ! end i=1:nsweep loop
       ! #:( reaching this point means that the procedure has not converged.
           info = nsweep - 1_ilp
           go to 1995
           1994 continue
       ! #:) reaching this point means numerical convergence after the i-th
           ! sweep.
           info = 0_ilp
       ! #:) info = 0 confirms successful iterations.
       1995 continue
           ! sort the vector sva() of column norms.
           do p = 1, n - 1
              q = stdlib_isamax( n-p+1, sva( p ), 1_ilp ) + p - 1_ilp
              if( p/=q ) then
                 temp1 = sva( p )
                 sva( p ) = sva( q )
                 sva( q ) = temp1
                 aapq = d( p )
                 d( p ) = d( q )
                 d( q ) = aapq
                 call stdlib_cswap( m, a( 1_ilp, p ), 1_ilp, a( 1_ilp, q ), 1_ilp )
                 if( rsvec )call stdlib_cswap( mvl, v( 1_ilp, p ), 1_ilp, v( 1_ilp, q ), 1_ilp )
              end if
           end do
           return
     end subroutine stdlib_cgsvj1

     pure module subroutine stdlib_zgsvj1( jobv, m, n, n1, a, lda, d, sva, mv, v, ldv,eps, sfmin, tol, &
     !! ZGSVJ1 is called from ZGESVJ as a pre-processor and that is its main
     !! purpose. It applies Jacobi rotations in the same way as ZGESVJ does, but
     !! it targets only particular pivots and it does not check convergence
     !! (stopping criterion). Few tuning parameters (marked by [TP]) are
     !! available for the implementer.
     !! Further Details
     !! ~~~~~~~~~~~~~~~
     !! ZGSVJ1 applies few sweeps of Jacobi rotations in the column space of
     !! the input M-by-N matrix A. The pivot pairs are taken from the (1,2)
     !! off-diagonal block in the corresponding N-by-N Gram matrix A^T * A. The
     !! block-entries (tiles) of the (1,2) off-diagonal block are marked by the
     !! [x]'s in the following scheme:
     !! | *  *  * [x] [x] [x]|
     !! | *  *  * [x] [x] [x]|    Row-cycling in the nblr-by-nblc [x] blocks.
     !! | *  *  * [x] [x] [x]|    Row-cyclic pivoting inside each [x] block.
     !! |[x] [x] [x] *  *  * |
     !! |[x] [x] [x] *  *  * |
     !! |[x] [x] [x] *  *  * |
     !! In terms of the columns of A, the first N1 columns are rotated 'against'
     !! the remaining N-N1 columns, trying to increase the angle between the
     !! corresponding subspaces. The off-diagonal block is N1-by(N-N1) and it is
     !! tiled using quadratic tiles of side KBL. Here, KBL is a tuning parameter.
     !! The number of sweeps is given in NSWEEP and the orthogonality threshold
     !! is given in TOL.
               nsweep, work, lwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           real(dp), intent(in) :: eps, sfmin, tol
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldv, lwork, m, mv, n, n1, nsweep
           character, intent(in) :: jobv
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*), d(n), v(ldv,*)
           complex(dp), intent(out) :: work(lwork)
           real(dp), intent(inout) :: sva(n)
        ! =====================================================================
           
           ! Local Scalars 
           complex(dp) :: aapq, ompq
           real(dp) :: aapp, aapp0, aapq1, aaqq, apoaq, aqoap, big, bigtheta, cs, mxaapq, mxsinj, &
                     rootbig, rooteps, rootsfmin, roottol, small, sn, t, temp1, theta, thsign
           integer(ilp) :: blskip, emptsw, i, ibr, igl, ierr, ijblsk, iswrot, jbc, jgl, kbl, mvl, &
                     notrot, nblc, nblr, p, pskipped, q, rowskip, swband
           logical(lk) :: applv, rotok, rsvec
           ! Intrinsic Functions 
           ! From Lapack
           ! Executable Statements 
           ! test the input parameters.
           applv = stdlib_lsame( jobv, 'A' )
           rsvec = stdlib_lsame( jobv, 'V' )
           if( .not.( rsvec .or. applv .or. stdlib_lsame( jobv, 'N' ) ) ) then
              info = -1_ilp
           else if( m<0_ilp ) then
              info = -2_ilp
           else if( ( n<0_ilp ) .or. ( n>m ) ) then
              info = -3_ilp
           else if( n1<0_ilp ) then
              info = -4_ilp
           else if( lda<m ) then
              info = -6_ilp
           else if( ( rsvec.or.applv ) .and. ( mv<0_ilp ) ) then
              info = -9_ilp
           else if( ( rsvec.and.( ldv<n ) ).or.( applv.and.( ldv<mv ) )  ) then
              info = -11_ilp
           else if( tol<=eps ) then
              info = -14_ilp
           else if( nsweep<0_ilp ) then
              info = -15_ilp
           else if( lwork<m ) then
              info = -17_ilp
           else
              info = 0_ilp
           end if
           ! #:(
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZGSVJ1', -info )
              return
           end if
           if( rsvec ) then
              mvl = n
           else if( applv ) then
              mvl = mv
           end if
           rsvec = rsvec .or. applv
           rooteps = sqrt( eps )
           rootsfmin = sqrt( sfmin )
           small = sfmin / eps
           big = one / sfmin
           rootbig = one / rootsfmin
           ! large = big / sqrt( real( m*n,KIND=dp) )
           bigtheta = one / rooteps
           roottol = sqrt( tol )
           ! Initialize The Right Singular Vector Matrix 
           ! rsvec = stdlib_lsame( jobv, 'y' )
           emptsw = n1*( n-n1 )
           notrot = 0_ilp
           ! .. row-cyclic pivot strategy with de rijk's pivoting ..
           kbl = min( 8_ilp, n )
           nblr = n1 / kbl
           if( ( nblr*kbl )/=n1 )nblr = nblr + 1_ilp
           ! .. the tiling is nblr-by-nblc [tiles]
           nblc = ( n-n1 ) / kbl
           if( ( nblc*kbl )/=( n-n1 ) )nblc = nblc + 1_ilp
           blskip = ( kbl**2_ilp ) + 1_ilp
      ! [tp] blkskip is a tuning parameter that depends on swband and kbl.
           rowskip = min( 5_ilp, kbl )
      ! [tp] rowskip is a tuning parameter.
           swband = 0_ilp
      ! [tp] swband is a tuning parameter. it is meaningful and effective
           ! if stdlib_zgesvj is used as a computational routine in the preconditioned
           ! jacobi svd algorithm stdlib_zgejsv.
           ! | *   *   * [x] [x] [x]|
           ! | *   *   * [x] [x] [x]|    row-cycling in the nblr-by-nblc [x] blocks.
           ! | *   *   * [x] [x] [x]|    row-cyclic pivoting inside each [x] block.
           ! |[x] [x] [x] *   *   * |
           ! |[x] [x] [x] *   *   * |
           ! |[x] [x] [x] *   *   * |
           loop_1993: do i = 1, nsweep
           ! .. go go go ...
              mxaapq = zero
              mxsinj = zero
              iswrot = 0_ilp
              notrot = 0_ilp
              pskipped = 0_ilp
           ! each sweep is unrolled using kbl-by-kbl tiles over the pivot pairs
           ! 1 <= p < q <= n. this is the first step toward a blocked implementation
           ! of the rotations. new implementation, based on block transformations,
           ! is under development.
              loop_2000: do ibr = 1, nblr
                 igl = ( ibr-1 )*kbl + 1_ilp
       ! ... go to the off diagonal blocks
                 igl = ( ibr-1 )*kbl + 1_ilp
                  ! do 2010 jbc = ibr + 1, nbl
                 loop_2010: do jbc = 1, nblc
                    jgl = ( jbc-1 )*kbl + n1 + 1_ilp
              ! doing the block at ( ibr, jbc )
                    ijblsk = 0_ilp
                    loop_2100: do p = igl, min( igl+kbl-1, n1 )
                       aapp = sva( p )
                       if( aapp>zero ) then
                          pskipped = 0_ilp
                          loop_2200: do q = jgl, min( jgl+kbl-1, n )
                             aaqq = sva( q )
                             if( aaqq>zero ) then
                                aapp0 = aapp
           ! M X 2 Jacobi Svd 
              ! safe gram matrix computation
                                if( aaqq>=one ) then
                                   if( aapp>=aaqq ) then
                                      rotok = ( small*aapp )<=aaqq
                                   else
                                      rotok = ( small*aaqq )<=aapp
                                   end if
                                   if( aapp<( big / aaqq ) ) then
                                      aapq = ( stdlib_zdotc( m, a( 1_ilp, p ), 1_ilp,a( 1_ilp, q ), 1_ilp ) / &
                                                aaqq ) / aapp
                                   else
                                      call stdlib_zcopy( m, a( 1_ilp, p ), 1_ilp,work, 1_ilp )
                                      call stdlib_zlascl( 'G', 0_ilp, 0_ilp, aapp,one, m, 1_ilp,work, lda, &
                                                ierr )
                                      aapq = stdlib_zdotc( m, work, 1_ilp,a( 1_ilp, q ), 1_ilp ) / &
                                                aaqq
                                   end if
                                else
                                   if( aapp>=aaqq ) then
                                      rotok = aapp<=( aaqq / small )
                                   else
                                      rotok = aaqq<=( aapp / small )
                                   end if
                                   if( aapp>( small / aaqq ) ) then
                                      aapq = ( stdlib_zdotc( m, a( 1_ilp, p ), 1_ilp,a( 1_ilp, q ), 1_ilp ) / max(&
                                                aaqq,aapp) )/ min(aaqq,aapp)
                                   else
                                      call stdlib_zcopy( m, a( 1_ilp, q ), 1_ilp,work, 1_ilp )
                                      call stdlib_zlascl( 'G', 0_ilp, 0_ilp, aaqq,one, m, 1_ilp,work, lda, &
                                                ierr )
                                      aapq = stdlib_zdotc( m, a( 1_ilp, p ), 1_ilp,work, 1_ilp ) / &
                                                aapp
                                   end if
                                end if
                                 ! aapq = aapq * conjg(cwork(p))*cwork(q)
                                aapq1  = -abs(aapq)
                                mxaapq = max( mxaapq, -aapq1 )
              ! to rotate or not to rotate, that is the question ...
                                if( abs( aapq1 )>tol ) then
                                   ompq = aapq / abs(aapq)
                                   notrot = 0_ilp
      ! [rtd]      rotated  = rotated + 1
                                   pskipped = 0_ilp
                                   iswrot = iswrot + 1_ilp
                                   if( rotok ) then
                                      aqoap = aaqq / aapp
                                      apoaq = aapp / aaqq
                                      theta = -half*abs( aqoap-apoaq )/ aapq1
                                      if( aaqq>aapp0 )theta = -theta
                                      if( abs( theta )>bigtheta ) then
                                         t  = half / theta
                                         cs = one
                                         call stdlib_zrot( m, a(1_ilp,p), 1_ilp, a(1_ilp,q), 1_ilp,cs, conjg(ompq)&
                                                   *t )
                                         if( rsvec ) then
                                             call stdlib_zrot( mvl, v(1_ilp,p), 1_ilp,v(1_ilp,q), 1_ilp, cs, &
                                                       conjg(ompq)*t )
                                         end if
                                         sva( q ) = aaqq*sqrt( max( zero,one+t*apoaq*aapq1 ) )
                                                   
                                         aapp = aapp*sqrt( max( zero,one-t*aqoap*aapq1 ) )
                                         mxsinj = max( mxsinj, abs( t ) )
                                      else
                       ! Choose Correct Signum For Theta And Rotate
                                         thsign = -sign( one, aapq1 )
                                         if( aaqq>aapp0 )thsign = -thsign
                                         t = one / ( theta+thsign*sqrt( one+theta*theta ) )
                                                   
                                         cs = sqrt( one / ( one+t*t ) )
                                         sn = t*cs
                                         mxsinj = max( mxsinj, abs( sn ) )
                                         sva( q ) = aaqq*sqrt( max( zero,one+t*apoaq*aapq1 ) )
                                                   
                                         aapp = aapp*sqrt( max( zero,one-t*aqoap*aapq1 ) )
                                         call stdlib_zrot( m, a(1_ilp,p), 1_ilp, a(1_ilp,q), 1_ilp,cs, conjg(ompq)&
                                                   *sn )
                                         if( rsvec ) then
                                             call stdlib_zrot( mvl, v(1_ilp,p), 1_ilp,v(1_ilp,q), 1_ilp, cs, &
                                                       conjg(ompq)*sn )
                                         end if
                                      end if
                                      d(p) = -d(q) * ompq
                                   else
                    ! .. have to use modified gram-schmidt like transformation
                                    if( aapp>aaqq ) then
                                         call stdlib_zcopy( m, a( 1_ilp, p ), 1_ilp,work, 1_ilp )
                                         call stdlib_zlascl( 'G', 0_ilp, 0_ilp, aapp, one,m, 1_ilp, work,lda,&
                                                   ierr )
                                         call stdlib_zlascl( 'G', 0_ilp, 0_ilp, aaqq, one,m, 1_ilp, a( 1_ilp, q ),&
                                                    lda,ierr )
                                         call stdlib_zaxpy( m, -aapq, work,1_ilp, a( 1_ilp, q ), 1_ilp )
                                                   
                                         call stdlib_zlascl( 'G', 0_ilp, 0_ilp, one, aaqq,m, 1_ilp, a( 1_ilp, q ),&
                                                    lda,ierr )
                                         sva( q ) = aaqq*sqrt( max( zero,one-aapq1*aapq1 ) )
                                                   
                                         mxsinj = max( mxsinj, sfmin )
                                    else
                                        call stdlib_zcopy( m, a( 1_ilp, q ), 1_ilp,work, 1_ilp )
                                         call stdlib_zlascl( 'G', 0_ilp, 0_ilp, aaqq, one,m, 1_ilp, work,lda,&
                                                   ierr )
                                         call stdlib_zlascl( 'G', 0_ilp, 0_ilp, aapp, one,m, 1_ilp, a( 1_ilp, p ),&
                                                    lda,ierr )
                                         call stdlib_zaxpy( m, -conjg(aapq),work, 1_ilp, a( 1_ilp, p ), 1_ilp &
                                                   )
                                         call stdlib_zlascl( 'G', 0_ilp, 0_ilp, one, aapp,m, 1_ilp, a( 1_ilp, p ),&
                                                    lda,ierr )
                                         sva( p ) = aapp*sqrt( max( zero,one-aapq1*aapq1 ) )
                                                   
                                         mxsinj = max( mxsinj, sfmin )
                                    end if
                                   end if
                 ! end if rotok then ... else
                 ! in the case of cancellation in updating sva(q), sva(p)
                 ! .. recompute sva(q), sva(p)
                                   if( ( sva( q ) / aaqq )**2_ilp<=rooteps )then
                                      if( ( aaqq<rootbig ) .and.( aaqq>rootsfmin ) ) then
                                         sva( q ) = stdlib_dznrm2( m, a( 1_ilp, q ), 1_ilp)
                                       else
                                         t = zero
                                         aaqq = one
                                         call stdlib_zlassq( m, a( 1_ilp, q ), 1_ilp, t,aaqq )
                                         sva( q ) = t*sqrt( aaqq )
                                      end if
                                   end if
                                   if( ( aapp / aapp0 )**2_ilp<=rooteps ) then
                                      if( ( aapp<rootbig ) .and.( aapp>rootsfmin ) ) then
                                         aapp = stdlib_dznrm2( m, a( 1_ilp, p ), 1_ilp )
                                      else
                                         t = zero
                                         aapp = one
                                         call stdlib_zlassq( m, a( 1_ilp, p ), 1_ilp, t,aapp )
                                         aapp = t*sqrt( aapp )
                                      end if
                                      sva( p ) = aapp
                                   end if
                    ! end of ok rotation
                                else
                                   notrot = notrot + 1_ilp
      ! [rtd]      skipped  = skipped  + 1
                                   pskipped = pskipped + 1_ilp
                                   ijblsk = ijblsk + 1_ilp
                                end if
                             else
                                notrot = notrot + 1_ilp
                                pskipped = pskipped + 1_ilp
                                ijblsk = ijblsk + 1_ilp
                             end if
                             if( ( i<=swband ) .and. ( ijblsk>=blskip ) )then
                                sva( p ) = aapp
                                notrot = 0_ilp
                                go to 2011
                             end if
                             if( ( i<=swband ) .and.( pskipped>rowskip ) ) then
                                aapp = -aapp
                                notrot = 0_ilp
                                go to 2203
                             end if
                          end do loop_2200
              ! end of the q-loop
              2203 continue
                          sva( p ) = aapp
                       else
                          if( aapp==zero )notrot = notrot +min( jgl+kbl-1, n ) - jgl + 1_ilp
                          if( aapp<zero )notrot = 0_ilp
                       end if
                    end do loop_2100
           ! end of the p-loop
                 end do loop_2010
           ! end of the jbc-loop
           2011 continue
      ! 2011 bailed out of the jbc-loop
                 do p = igl, min( igl+kbl-1, n )
                    sva( p ) = abs( sva( p ) )
                 end do
      ! **
              end do loop_2000
      ! 2000 :: end of the ibr-loop
           ! .. update sva(n)
              if( ( sva( n )<rootbig ) .and. ( sva( n )>rootsfmin ) )then
                 sva( n ) = stdlib_dznrm2( m, a( 1_ilp, n ), 1_ilp )
              else
                 t = zero
                 aapp = one
                 call stdlib_zlassq( m, a( 1_ilp, n ), 1_ilp, t, aapp )
                 sva( n ) = t*sqrt( aapp )
              end if
           ! additional steering devices
              if( ( i<swband ) .and. ( ( mxaapq<=roottol ) .or.( iswrot<=n ) ) )swband = i
              if( ( i>swband+1 ) .and. ( mxaapq<sqrt( real( n,KIND=dp) )*tol ) .and. ( real( n,&
                        KIND=dp)*mxaapq*mxsinj<tol ) ) then
                 go to 1994
              end if
              if( notrot>=emptsw )go to 1994
           end do loop_1993
           ! end i=1:nsweep loop
       ! #:( reaching this point means that the procedure has not converged.
           info = nsweep - 1_ilp
           go to 1995
           1994 continue
       ! #:) reaching this point means numerical convergence after the i-th
           ! sweep.
           info = 0_ilp
       ! #:) info = 0 confirms successful iterations.
       1995 continue
           ! sort the vector sva() of column norms.
           do p = 1, n - 1
              q = stdlib_idamax( n-p+1, sva( p ), 1_ilp ) + p - 1_ilp
              if( p/=q ) then
                 temp1 = sva( p )
                 sva( p ) = sva( q )
                 sva( q ) = temp1
                 aapq = d( p )
                 d( p ) = d( q )
                 d( q ) = aapq
                 call stdlib_zswap( m, a( 1_ilp, p ), 1_ilp, a( 1_ilp, q ), 1_ilp )
                 if( rsvec )call stdlib_zswap( mvl, v( 1_ilp, p ), 1_ilp, v( 1_ilp, q ), 1_ilp )
              end if
           end do
           return
     end subroutine stdlib_zgsvj1




     pure module subroutine stdlib_stgsja( jobu, jobv, jobq, m, p, n, k, l, a, lda, b,ldb, tola, tolb, &
     !! STGSJA computes the generalized singular value decomposition (GSVD)
     !! of two real upper triangular (or trapezoidal) matrices A and B.
     !! On entry, it is assumed that matrices A and B have the following
     !! forms, which may be obtained by the preprocessing subroutine SGGSVP
     !! from a general M-by-N matrix A and P-by-N matrix B:
     !! N-K-L  K    L
     !! A =    K ( 0    A12  A13 ) if M-K-L >= 0;
     !! L ( 0     0   A23 )
     !! M-K-L ( 0     0    0  )
     !! N-K-L  K    L
     !! A =  K ( 0    A12  A13 ) if M-K-L < 0;
     !! M-K ( 0     0   A23 )
     !! N-K-L  K    L
     !! B =  L ( 0     0   B13 )
     !! P-L ( 0     0    0  )
     !! where the K-by-K matrix A12 and L-by-L matrix B13 are nonsingular
     !! upper triangular; A23 is L-by-L upper triangular if M-K-L >= 0,
     !! otherwise A23 is (M-K)-by-L upper trapezoidal.
     !! On exit,
     !! U**T *A*Q = D1*( 0 R ),    V**T *B*Q = D2*( 0 R ),
     !! where U, V and Q are orthogonal matrices.
     !! R is a nonsingular upper triangular matrix, and D1 and D2 are
     !! ``diagonal'' matrices, which are of the following structures:
     !! If M-K-L >= 0,
     !! K  L
     !! D1 =     K ( I  0 )
     !! L ( 0  C )
     !! M-K-L ( 0  0 )
     !! K  L
     !! D2 = L   ( 0  S )
     !! P-L ( 0  0 )
     !! N-K-L  K    L
     !! ( 0 R ) = K (  0   R11  R12 ) K
     !! L (  0    0   R22 ) L
     !! where
     !! C = diag( ALPHA(K+1), ... , ALPHA(K+L) ),
     !! S = diag( BETA(K+1),  ... , BETA(K+L) ),
     !! C**2 + S**2 = I.
     !! R is stored in A(1:K+L,N-K-L+1:N) on exit.
     !! If M-K-L < 0,
     !! K M-K K+L-M
     !! D1 =   K ( I  0    0   )
     !! M-K ( 0  C    0   )
     !! K M-K K+L-M
     !! D2 =   M-K ( 0  S    0   )
     !! K+L-M ( 0  0    I   )
     !! P-L ( 0  0    0   )
     !! N-K-L  K   M-K  K+L-M
     !! ( 0 R ) =    K ( 0    R11  R12  R13  )
     !! M-K ( 0     0   R22  R23  )
     !! K+L-M ( 0     0    0   R33  )
     !! where
     !! C = diag( ALPHA(K+1), ... , ALPHA(M) ),
     !! S = diag( BETA(K+1),  ... , BETA(M) ),
     !! C**2 + S**2 = I.
     !! R = ( R11 R12 R13 ) is stored in A(1:M, N-K-L+1:N) and R33 is stored
     !! (  0  R22 R23 )
     !! in B(M-K+1:L,N+M-K-L+1:N) on exit.
     !! The computation of the orthogonal transformation matrices U, V or Q
     !! is optional.  These matrices may either be formed explicitly, or they
     !! may be postmultiplied into input matrices U1, V1, or Q1.
               alpha, beta, u, ldu, v, ldv,q, ldq, work, ncycle, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobq, jobu, jobv
           integer(ilp), intent(out) :: info, ncycle
           integer(ilp), intent(in) :: k, l, lda, ldb, ldq, ldu, ldv, m, n, p
           real(sp), intent(in) :: tola, tolb
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*), b(ldb,*), q(ldq,*), u(ldu,*), v(ldv,*)
           real(sp), intent(out) :: alpha(*), beta(*), work(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: maxit = 40_ilp
           real(sp), parameter :: hugenum = huge(zero)
           
           
           ! Local Scalars 
           logical(lk) :: initq, initu, initv, upper, wantq, wantu, wantv
           integer(ilp) :: i, j, kcycle
           real(sp) :: a1, a2, a3, b1, b2, b3, csq, csu, csv, error, gamma, rwk, snq, snu, snv, &
                     ssmin
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode and test the input parameters
           initu = stdlib_lsame( jobu, 'I' )
           wantu = initu .or. stdlib_lsame( jobu, 'U' )
           initv = stdlib_lsame( jobv, 'I' )
           wantv = initv .or. stdlib_lsame( jobv, 'V' )
           initq = stdlib_lsame( jobq, 'I' )
           wantq = initq .or. stdlib_lsame( jobq, 'Q' )
           info = 0_ilp
           if( .not.( initu .or. wantu .or. stdlib_lsame( jobu, 'N' ) ) ) then
              info = -1_ilp
           else if( .not.( initv .or. wantv .or. stdlib_lsame( jobv, 'N' ) ) ) then
              info = -2_ilp
           else if( .not.( initq .or. wantq .or. stdlib_lsame( jobq, 'N' ) ) ) then
              info = -3_ilp
           else if( m<0_ilp ) then
              info = -4_ilp
           else if( p<0_ilp ) then
              info = -5_ilp
           else if( n<0_ilp ) then
              info = -6_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -10_ilp
           else if( ldb<max( 1_ilp, p ) ) then
              info = -12_ilp
           else if( ldu<1_ilp .or. ( wantu .and. ldu<m ) ) then
              info = -18_ilp
           else if( ldv<1_ilp .or. ( wantv .and. ldv<p ) ) then
              info = -20_ilp
           else if( ldq<1_ilp .or. ( wantq .and. ldq<n ) ) then
              info = -22_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'STGSJA', -info )
              return
           end if
           ! initialize u, v and q, if necessary
           if( initu )call stdlib_slaset( 'FULL', m, m, zero, one, u, ldu )
           if( initv )call stdlib_slaset( 'FULL', p, p, zero, one, v, ldv )
           if( initq )call stdlib_slaset( 'FULL', n, n, zero, one, q, ldq )
           ! loop until convergence
           upper = .false.
           loop_40: do kcycle = 1, maxit
              upper = .not.upper
              loop_20: do i = 1, l - 1
                 loop_10: do j = i + 1, l
                    a1 = zero
                    a2 = zero
                    a3 = zero
                    if( k+i<=m )a1 = a( k+i, n-l+i )
                    if( k+j<=m )a3 = a( k+j, n-l+j )
                    b1 = b( i, n-l+i )
                    b3 = b( j, n-l+j )
                    if( upper ) then
                       if( k+i<=m )a2 = a( k+i, n-l+j )
                       b2 = b( i, n-l+j )
                    else
                       if( k+j<=m )a2 = a( k+j, n-l+i )
                       b2 = b( j, n-l+i )
                    end if
                    call stdlib_slags2( upper, a1, a2, a3, b1, b2, b3, csu, snu,csv, snv, csq, &
                              snq )
                    ! update (k+i)-th and (k+j)-th rows of matrix a: u**t *a
                    if( k+j<=m )call stdlib_srot( l, a( k+j, n-l+1 ), lda, a( k+i, n-l+1 ),lda, &
                              csu, snu )
                    ! update i-th and j-th rows of matrix b: v**t *b
                    call stdlib_srot( l, b( j, n-l+1 ), ldb, b( i, n-l+1 ), ldb,csv, snv )
                    ! update (n-l+i)-th and (n-l+j)-th columns of matrices
                    ! a and b: a*q and b*q
                    call stdlib_srot( min( k+l, m ), a( 1_ilp, n-l+j ), 1_ilp,a( 1_ilp, n-l+i ), 1_ilp, csq, snq )
                              
                    call stdlib_srot( l, b( 1_ilp, n-l+j ), 1_ilp, b( 1_ilp, n-l+i ), 1_ilp, csq,snq )
                    if( upper ) then
                       if( k+i<=m )a( k+i, n-l+j ) = zero
                       b( i, n-l+j ) = zero
                    else
                       if( k+j<=m )a( k+j, n-l+i ) = zero
                       b( j, n-l+i ) = zero
                    end if
                    ! update orthogonal matrices u, v, q, if desired.
                    if( wantu .and. k+j<=m )call stdlib_srot( m, u( 1_ilp, k+j ), 1_ilp, u( 1_ilp, k+i ), 1_ilp, &
                              csu,snu )
                    if( wantv )call stdlib_srot( p, v( 1_ilp, j ), 1_ilp, v( 1_ilp, i ), 1_ilp, csv, snv )
                    if( wantq )call stdlib_srot( n, q( 1_ilp, n-l+j ), 1_ilp, q( 1_ilp, n-l+i ), 1_ilp, csq,snq )
                              
                 end do loop_10
              end do loop_20
              if( .not.upper ) then
                 ! the matrices a13 and b13 were lower triangular at the start
                 ! of the cycle, and are now upper triangular.
                 ! convergence test: test the parallelism of the corresponding
                 ! rows of a and b.
                 error = zero
                 do i = 1, min( l, m-k )
                    call stdlib_scopy( l-i+1, a( k+i, n-l+i ), lda, work, 1_ilp )
                    call stdlib_scopy( l-i+1, b( i, n-l+i ), ldb, work( l+1 ), 1_ilp )
                    call stdlib_slapll( l-i+1, work, 1_ilp, work( l+1 ), 1_ilp, ssmin )
                    error = max( error, ssmin )
                 end do
                 if( abs( error )<=min( tola, tolb ) )go to 50
              end if
              ! end of cycle loop
           end do loop_40
           ! the algorithm has not converged after maxit cycles.
           info = 1_ilp
           go to 100
           50 continue
           ! if error <= min(tola,tolb), then the algorithm has converged.
           ! compute the generalized singular value pairs (alpha, beta), and
           ! set the triangular matrix r to array a.
           do i = 1, k
              alpha( i ) = one
              beta( i ) = zero
           end do
           do i = 1, min( l, m-k )
              a1 = a( k+i, n-l+i )
              b1 = b( i, n-l+i )
              gamma = b1 / a1
              if( (gamma<=hugenum).and.(gamma>=-hugenum) ) then
                 ! change sign if necessary
                 if( gamma<zero ) then
                    call stdlib_sscal( l-i+1, -one, b( i, n-l+i ), ldb )
                    if( wantv )call stdlib_sscal( p, -one, v( 1_ilp, i ), 1_ilp )
                 end if
                 call stdlib_slartg( abs( gamma ), one, beta( k+i ), alpha( k+i ),rwk )
                 if( alpha( k+i )>=beta( k+i ) ) then
                    call stdlib_sscal( l-i+1, one / alpha( k+i ), a( k+i, n-l+i ),lda )
                 else
                    call stdlib_sscal( l-i+1, one / beta( k+i ), b( i, n-l+i ),ldb )
                    call stdlib_scopy( l-i+1, b( i, n-l+i ), ldb, a( k+i, n-l+i ),lda )
                 end if
              else
                 alpha( k+i ) = zero
                 beta( k+i ) = one
                 call stdlib_scopy( l-i+1, b( i, n-l+i ), ldb, a( k+i, n-l+i ),lda )
              end if
           end do
           ! post-assignment
           do i = m + 1, k + l
              alpha( i ) = zero
              beta( i ) = one
           end do
           if( k+l<n ) then
              do i = k + l + 1, n
                 alpha( i ) = zero
                 beta( i ) = zero
              end do
           end if
           100 continue
           ncycle = kcycle
           return
     end subroutine stdlib_stgsja

     pure module subroutine stdlib_dtgsja( jobu, jobv, jobq, m, p, n, k, l, a, lda, b,ldb, tola, tolb, &
     !! DTGSJA computes the generalized singular value decomposition (GSVD)
     !! of two real upper triangular (or trapezoidal) matrices A and B.
     !! On entry, it is assumed that matrices A and B have the following
     !! forms, which may be obtained by the preprocessing subroutine DGGSVP
     !! from a general M-by-N matrix A and P-by-N matrix B:
     !! N-K-L  K    L
     !! A =    K ( 0    A12  A13 ) if M-K-L >= 0;
     !! L ( 0     0   A23 )
     !! M-K-L ( 0     0    0  )
     !! N-K-L  K    L
     !! A =  K ( 0    A12  A13 ) if M-K-L < 0;
     !! M-K ( 0     0   A23 )
     !! N-K-L  K    L
     !! B =  L ( 0     0   B13 )
     !! P-L ( 0     0    0  )
     !! where the K-by-K matrix A12 and L-by-L matrix B13 are nonsingular
     !! upper triangular; A23 is L-by-L upper triangular if M-K-L >= 0,
     !! otherwise A23 is (M-K)-by-L upper trapezoidal.
     !! On exit,
     !! U**T *A*Q = D1*( 0 R ),    V**T *B*Q = D2*( 0 R ),
     !! where U, V and Q are orthogonal matrices.
     !! R is a nonsingular upper triangular matrix, and D1 and D2 are
     !! ``diagonal'' matrices, which are of the following structures:
     !! If M-K-L >= 0,
     !! K  L
     !! D1 =     K ( I  0 )
     !! L ( 0  C )
     !! M-K-L ( 0  0 )
     !! K  L
     !! D2 = L   ( 0  S )
     !! P-L ( 0  0 )
     !! N-K-L  K    L
     !! ( 0 R ) = K (  0   R11  R12 ) K
     !! L (  0    0   R22 ) L
     !! where
     !! C = diag( ALPHA(K+1), ... , ALPHA(K+L) ),
     !! S = diag( BETA(K+1),  ... , BETA(K+L) ),
     !! C**2 + S**2 = I.
     !! R is stored in A(1:K+L,N-K-L+1:N) on exit.
     !! If M-K-L < 0,
     !! K M-K K+L-M
     !! D1 =   K ( I  0    0   )
     !! M-K ( 0  C    0   )
     !! K M-K K+L-M
     !! D2 =   M-K ( 0  S    0   )
     !! K+L-M ( 0  0    I   )
     !! P-L ( 0  0    0   )
     !! N-K-L  K   M-K  K+L-M
     !! ( 0 R ) =    K ( 0    R11  R12  R13  )
     !! M-K ( 0     0   R22  R23  )
     !! K+L-M ( 0     0    0   R33  )
     !! where
     !! C = diag( ALPHA(K+1), ... , ALPHA(M) ),
     !! S = diag( BETA(K+1),  ... , BETA(M) ),
     !! C**2 + S**2 = I.
     !! R = ( R11 R12 R13 ) is stored in A(1:M, N-K-L+1:N) and R33 is stored
     !! (  0  R22 R23 )
     !! in B(M-K+1:L,N+M-K-L+1:N) on exit.
     !! The computation of the orthogonal transformation matrices U, V or Q
     !! is optional.  These matrices may either be formed explicitly, or they
     !! may be postmultiplied into input matrices U1, V1, or Q1.
               alpha, beta, u, ldu, v, ldv,q, ldq, work, ncycle, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobq, jobu, jobv
           integer(ilp), intent(out) :: info, ncycle
           integer(ilp), intent(in) :: k, l, lda, ldb, ldq, ldu, ldv, m, n, p
           real(dp), intent(in) :: tola, tolb
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*), b(ldb,*), q(ldq,*), u(ldu,*), v(ldv,*)
           real(dp), intent(out) :: alpha(*), beta(*), work(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: maxit = 40_ilp
           real(dp), parameter :: hugenum = huge(zero)
           
           
           ! Local Scalars 
           logical(lk) :: initq, initu, initv, upper, wantq, wantu, wantv
           integer(ilp) :: i, j, kcycle
           real(dp) :: a1, a2, a3, b1, b2, b3, csq, csu, csv, error, gamma, rwk, snq, snu, snv, &
                     ssmin
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode and test the input parameters
           initu = stdlib_lsame( jobu, 'I' )
           wantu = initu .or. stdlib_lsame( jobu, 'U' )
           initv = stdlib_lsame( jobv, 'I' )
           wantv = initv .or. stdlib_lsame( jobv, 'V' )
           initq = stdlib_lsame( jobq, 'I' )
           wantq = initq .or. stdlib_lsame( jobq, 'Q' )
           info = 0_ilp
           if( .not.( initu .or. wantu .or. stdlib_lsame( jobu, 'N' ) ) ) then
              info = -1_ilp
           else if( .not.( initv .or. wantv .or. stdlib_lsame( jobv, 'N' ) ) ) then
              info = -2_ilp
           else if( .not.( initq .or. wantq .or. stdlib_lsame( jobq, 'N' ) ) ) then
              info = -3_ilp
           else if( m<0_ilp ) then
              info = -4_ilp
           else if( p<0_ilp ) then
              info = -5_ilp
           else if( n<0_ilp ) then
              info = -6_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -10_ilp
           else if( ldb<max( 1_ilp, p ) ) then
              info = -12_ilp
           else if( ldu<1_ilp .or. ( wantu .and. ldu<m ) ) then
              info = -18_ilp
           else if( ldv<1_ilp .or. ( wantv .and. ldv<p ) ) then
              info = -20_ilp
           else if( ldq<1_ilp .or. ( wantq .and. ldq<n ) ) then
              info = -22_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DTGSJA', -info )
              return
           end if
           ! initialize u, v and q, if necessary
           if( initu )call stdlib_dlaset( 'FULL', m, m, zero, one, u, ldu )
           if( initv )call stdlib_dlaset( 'FULL', p, p, zero, one, v, ldv )
           if( initq )call stdlib_dlaset( 'FULL', n, n, zero, one, q, ldq )
           ! loop until convergence
           upper = .false.
           loop_40: do kcycle = 1, maxit
              upper = .not.upper
              loop_20: do i = 1, l - 1
                 loop_10: do j = i + 1, l
                    a1 = zero
                    a2 = zero
                    a3 = zero
                    if( k+i<=m )a1 = a( k+i, n-l+i )
                    if( k+j<=m )a3 = a( k+j, n-l+j )
                    b1 = b( i, n-l+i )
                    b3 = b( j, n-l+j )
                    if( upper ) then
                       if( k+i<=m )a2 = a( k+i, n-l+j )
                       b2 = b( i, n-l+j )
                    else
                       if( k+j<=m )a2 = a( k+j, n-l+i )
                       b2 = b( j, n-l+i )
                    end if
                    call stdlib_dlags2( upper, a1, a2, a3, b1, b2, b3, csu, snu,csv, snv, csq, &
                              snq )
                    ! update (k+i)-th and (k+j)-th rows of matrix a: u**t *a
                    if( k+j<=m )call stdlib_drot( l, a( k+j, n-l+1 ), lda, a( k+i, n-l+1 ),lda, &
                              csu, snu )
                    ! update i-th and j-th rows of matrix b: v**t *b
                    call stdlib_drot( l, b( j, n-l+1 ), ldb, b( i, n-l+1 ), ldb,csv, snv )
                    ! update (n-l+i)-th and (n-l+j)-th columns of matrices
                    ! a and b: a*q and b*q
                    call stdlib_drot( min( k+l, m ), a( 1_ilp, n-l+j ), 1_ilp,a( 1_ilp, n-l+i ), 1_ilp, csq, snq )
                              
                    call stdlib_drot( l, b( 1_ilp, n-l+j ), 1_ilp, b( 1_ilp, n-l+i ), 1_ilp, csq,snq )
                    if( upper ) then
                       if( k+i<=m )a( k+i, n-l+j ) = zero
                       b( i, n-l+j ) = zero
                    else
                       if( k+j<=m )a( k+j, n-l+i ) = zero
                       b( j, n-l+i ) = zero
                    end if
                    ! update orthogonal matrices u, v, q, if desired.
                    if( wantu .and. k+j<=m )call stdlib_drot( m, u( 1_ilp, k+j ), 1_ilp, u( 1_ilp, k+i ), 1_ilp, &
                              csu,snu )
                    if( wantv )call stdlib_drot( p, v( 1_ilp, j ), 1_ilp, v( 1_ilp, i ), 1_ilp, csv, snv )
                    if( wantq )call stdlib_drot( n, q( 1_ilp, n-l+j ), 1_ilp, q( 1_ilp, n-l+i ), 1_ilp, csq,snq )
                              
                 end do loop_10
              end do loop_20
              if( .not.upper ) then
                 ! the matrices a13 and b13 were lower triangular at the start
                 ! of the cycle, and are now upper triangular.
                 ! convergence test: test the parallelism of the corresponding
                 ! rows of a and b.
                 error = zero
                 do i = 1, min( l, m-k )
                    call stdlib_dcopy( l-i+1, a( k+i, n-l+i ), lda, work, 1_ilp )
                    call stdlib_dcopy( l-i+1, b( i, n-l+i ), ldb, work( l+1 ), 1_ilp )
                    call stdlib_dlapll( l-i+1, work, 1_ilp, work( l+1 ), 1_ilp, ssmin )
                    error = max( error, ssmin )
                 end do
                 if( abs( error )<=min( tola, tolb ) )go to 50
              end if
              ! end of cycle loop
           end do loop_40
           ! the algorithm has not converged after maxit cycles.
           info = 1_ilp
           go to 100
           50 continue
           ! if error <= min(tola,tolb), then the algorithm has converged.
           ! compute the generalized singular value pairs (alpha, beta), and
           ! set the triangular matrix r to array a.
           do i = 1, k
              alpha( i ) = one
              beta( i ) = zero
           end do
           do i = 1, min( l, m-k )
              a1 = a( k+i, n-l+i )
              b1 = b( i, n-l+i )
              gamma = b1 / a1
              if( (gamma<=hugenum).and.(gamma>=-hugenum) ) then
                 ! change sign if necessary
                 if( gamma<zero ) then
                    call stdlib_dscal( l-i+1, -one, b( i, n-l+i ), ldb )
                    if( wantv )call stdlib_dscal( p, -one, v( 1_ilp, i ), 1_ilp )
                 end if
                 call stdlib_dlartg( abs( gamma ), one, beta( k+i ), alpha( k+i ),rwk )
                 if( alpha( k+i )>=beta( k+i ) ) then
                    call stdlib_dscal( l-i+1, one / alpha( k+i ), a( k+i, n-l+i ),lda )
                 else
                    call stdlib_dscal( l-i+1, one / beta( k+i ), b( i, n-l+i ),ldb )
                    call stdlib_dcopy( l-i+1, b( i, n-l+i ), ldb, a( k+i, n-l+i ),lda )
                 end if
              else
                 alpha( k+i ) = zero
                 beta( k+i ) = one
                 call stdlib_dcopy( l-i+1, b( i, n-l+i ), ldb, a( k+i, n-l+i ),lda )
              end if
           end do
           ! post-assignment
           do i = m + 1, k + l
              alpha( i ) = zero
              beta( i ) = one
           end do
           if( k+l<n ) then
              do i = k + l + 1, n
                 alpha( i ) = zero
                 beta( i ) = zero
              end do
           end if
           100 continue
           ncycle = kcycle
           return
     end subroutine stdlib_dtgsja


     pure module subroutine stdlib_ctgsja( jobu, jobv, jobq, m, p, n, k, l, a, lda, b,ldb, tola, tolb, &
     !! CTGSJA computes the generalized singular value decomposition (GSVD)
     !! of two complex upper triangular (or trapezoidal) matrices A and B.
     !! On entry, it is assumed that matrices A and B have the following
     !! forms, which may be obtained by the preprocessing subroutine CGGSVP
     !! from a general M-by-N matrix A and P-by-N matrix B:
     !! N-K-L  K    L
     !! A =    K ( 0    A12  A13 ) if M-K-L >= 0;
     !! L ( 0     0   A23 )
     !! M-K-L ( 0     0    0  )
     !! N-K-L  K    L
     !! A =  K ( 0    A12  A13 ) if M-K-L < 0;
     !! M-K ( 0     0   A23 )
     !! N-K-L  K    L
     !! B =  L ( 0     0   B13 )
     !! P-L ( 0     0    0  )
     !! where the K-by-K matrix A12 and L-by-L matrix B13 are nonsingular
     !! upper triangular; A23 is L-by-L upper triangular if M-K-L >= 0,
     !! otherwise A23 is (M-K)-by-L upper trapezoidal.
     !! On exit,
     !! U**H *A*Q = D1*( 0 R ),    V**H *B*Q = D2*( 0 R ),
     !! where U, V and Q are unitary matrices.
     !! R is a nonsingular upper triangular matrix, and D1
     !! and D2 are ``diagonal'' matrices, which are of the following
     !! structures:
     !! If M-K-L >= 0,
     !! K  L
     !! D1 =     K ( I  0 )
     !! L ( 0  C )
     !! M-K-L ( 0  0 )
     !! K  L
     !! D2 = L   ( 0  S )
     !! P-L ( 0  0 )
     !! N-K-L  K    L
     !! ( 0 R ) = K (  0   R11  R12 ) K
     !! L (  0    0   R22 ) L
     !! where
     !! C = diag( ALPHA(K+1), ... , ALPHA(K+L) ),
     !! S = diag( BETA(K+1),  ... , BETA(K+L) ),
     !! C**2 + S**2 = I.
     !! R is stored in A(1:K+L,N-K-L+1:N) on exit.
     !! If M-K-L < 0,
     !! K M-K K+L-M
     !! D1 =   K ( I  0    0   )
     !! M-K ( 0  C    0   )
     !! K M-K K+L-M
     !! D2 =   M-K ( 0  S    0   )
     !! K+L-M ( 0  0    I   )
     !! P-L ( 0  0    0   )
     !! N-K-L  K   M-K  K+L-M
     !! ( 0 R ) =    K ( 0    R11  R12  R13  )
     !! M-K ( 0     0   R22  R23  )
     !! K+L-M ( 0     0    0   R33  )
     !! where
     !! C = diag( ALPHA(K+1), ... , ALPHA(M) ),
     !! S = diag( BETA(K+1),  ... , BETA(M) ),
     !! C**2 + S**2 = I.
     !! R = ( R11 R12 R13 ) is stored in A(1:M, N-K-L+1:N) and R33 is stored
     !! (  0  R22 R23 )
     !! in B(M-K+1:L,N+M-K-L+1:N) on exit.
     !! The computation of the unitary transformation matrices U, V or Q
     !! is optional.  These matrices may either be formed explicitly, or they
     !! may be postmultiplied into input matrices U1, V1, or Q1.
               alpha, beta, u, ldu, v, ldv,q, ldq, work, ncycle, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobq, jobu, jobv
           integer(ilp), intent(out) :: info, ncycle
           integer(ilp), intent(in) :: k, l, lda, ldb, ldq, ldu, ldv, m, n, p
           real(sp), intent(in) :: tola, tolb
           ! Array Arguments 
           real(sp), intent(out) :: alpha(*), beta(*)
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*), q(ldq,*), u(ldu,*), v(ldv,*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: maxit = 40_ilp
           real(sp), parameter :: hugenum = huge(zero)
           
           
           
           ! Local Scalars 
           logical(lk) :: initq, initu, initv, upper, wantq, wantu, wantv
           integer(ilp) :: i, j, kcycle
           real(sp) :: a1, a3, b1, b3, csq, csu, csv, error, gamma, rwk, ssmin
           complex(sp) :: a2, b2, snq, snu, snv
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode and test the input parameters
           initu = stdlib_lsame( jobu, 'I' )
           wantu = initu .or. stdlib_lsame( jobu, 'U' )
           initv = stdlib_lsame( jobv, 'I' )
           wantv = initv .or. stdlib_lsame( jobv, 'V' )
           initq = stdlib_lsame( jobq, 'I' )
           wantq = initq .or. stdlib_lsame( jobq, 'Q' )
           info = 0_ilp
           if( .not.( initu .or. wantu .or. stdlib_lsame( jobu, 'N' ) ) ) then
              info = -1_ilp
           else if( .not.( initv .or. wantv .or. stdlib_lsame( jobv, 'N' ) ) ) then
              info = -2_ilp
           else if( .not.( initq .or. wantq .or. stdlib_lsame( jobq, 'N' ) ) ) then
              info = -3_ilp
           else if( m<0_ilp ) then
              info = -4_ilp
           else if( p<0_ilp ) then
              info = -5_ilp
           else if( n<0_ilp ) then
              info = -6_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -10_ilp
           else if( ldb<max( 1_ilp, p ) ) then
              info = -12_ilp
           else if( ldu<1_ilp .or. ( wantu .and. ldu<m ) ) then
              info = -18_ilp
           else if( ldv<1_ilp .or. ( wantv .and. ldv<p ) ) then
              info = -20_ilp
           else if( ldq<1_ilp .or. ( wantq .and. ldq<n ) ) then
              info = -22_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CTGSJA', -info )
              return
           end if
           ! initialize u, v and q, if necessary
           if( initu )call stdlib_claset( 'FULL', m, m, czero, cone, u, ldu )
           if( initv )call stdlib_claset( 'FULL', p, p, czero, cone, v, ldv )
           if( initq )call stdlib_claset( 'FULL', n, n, czero, cone, q, ldq )
           ! loop until convergence
           upper = .false.
           loop_40: do kcycle = 1, maxit
              upper = .not.upper
              loop_20: do i = 1, l - 1
                 loop_10: do j = i + 1, l
                    a1 = zero
                    a2 = czero
                    a3 = zero
                    if( k+i<=m )a1 = real( a( k+i, n-l+i ),KIND=sp)
                    if( k+j<=m )a3 = real( a( k+j, n-l+j ),KIND=sp)
                    b1 = real( b( i, n-l+i ),KIND=sp)
                    b3 = real( b( j, n-l+j ),KIND=sp)
                    if( upper ) then
                       if( k+i<=m )a2 = a( k+i, n-l+j )
                       b2 = b( i, n-l+j )
                    else
                       if( k+j<=m )a2 = a( k+j, n-l+i )
                       b2 = b( j, n-l+i )
                    end if
                    call stdlib_clags2( upper, a1, a2, a3, b1, b2, b3, csu, snu,csv, snv, csq, &
                              snq )
                    ! update (k+i)-th and (k+j)-th rows of matrix a: u**h *a
                    if( k+j<=m )call stdlib_crot( l, a( k+j, n-l+1 ), lda, a( k+i, n-l+1 ),lda, &
                              csu, conjg( snu ) )
                    ! update i-th and j-th rows of matrix b: v**h *b
                    call stdlib_crot( l, b( j, n-l+1 ), ldb, b( i, n-l+1 ), ldb,csv, conjg( snv ) &
                              )
                    ! update (n-l+i)-th and (n-l+j)-th columns of matrices
                    ! a and b: a*q and b*q
                    call stdlib_crot( min( k+l, m ), a( 1_ilp, n-l+j ), 1_ilp,a( 1_ilp, n-l+i ), 1_ilp, csq, snq )
                              
                    call stdlib_crot( l, b( 1_ilp, n-l+j ), 1_ilp, b( 1_ilp, n-l+i ), 1_ilp, csq,snq )
                    if( upper ) then
                       if( k+i<=m )a( k+i, n-l+j ) = czero
                       b( i, n-l+j ) = czero
                    else
                       if( k+j<=m )a( k+j, n-l+i ) = czero
                       b( j, n-l+i ) = czero
                    end if
                    ! ensure that the diagonal elements of a and b are real.
                    if( k+i<=m )a( k+i, n-l+i ) = real( a( k+i, n-l+i ),KIND=sp)
                    if( k+j<=m )a( k+j, n-l+j ) = real( a( k+j, n-l+j ),KIND=sp)
                    b( i, n-l+i ) = real( b( i, n-l+i ),KIND=sp)
                    b( j, n-l+j ) = real( b( j, n-l+j ),KIND=sp)
                    ! update unitary matrices u, v, q, if desired.
                    if( wantu .and. k+j<=m )call stdlib_crot( m, u( 1_ilp, k+j ), 1_ilp, u( 1_ilp, k+i ), 1_ilp, &
                              csu,snu )
                    if( wantv )call stdlib_crot( p, v( 1_ilp, j ), 1_ilp, v( 1_ilp, i ), 1_ilp, csv, snv )
                    if( wantq )call stdlib_crot( n, q( 1_ilp, n-l+j ), 1_ilp, q( 1_ilp, n-l+i ), 1_ilp, csq,snq )
                              
                 end do loop_10
              end do loop_20
              if( .not.upper ) then
                 ! the matrices a13 and b13 were lower triangular at the start
                 ! of the cycle, and are now upper triangular.
                 ! convergence test: test the parallelism of the corresponding
                 ! rows of a and b.
                 error = zero
                 do i = 1, min( l, m-k )
                    call stdlib_ccopy( l-i+1, a( k+i, n-l+i ), lda, work, 1_ilp )
                    call stdlib_ccopy( l-i+1, b( i, n-l+i ), ldb, work( l+1 ), 1_ilp )
                    call stdlib_clapll( l-i+1, work, 1_ilp, work( l+1 ), 1_ilp, ssmin )
                    error = max( error, ssmin )
                 end do
                 if( abs( error )<=min( tola, tolb ) )go to 50
              end if
              ! end of cycle loop
           end do loop_40
           ! the algorithm has not converged after maxit cycles.
           info = 1_ilp
           go to 100
           50 continue
           ! if error <= min(tola,tolb), then the algorithm has converged.
           ! compute the generalized singular value pairs (alpha, beta), and
           ! set the triangular matrix r to array a.
           do i = 1, k
              alpha( i ) = one
              beta( i ) = zero
           end do
           do i = 1, min( l, m-k )
              a1 = real( a( k+i, n-l+i ),KIND=sp)
              b1 = real( b( i, n-l+i ),KIND=sp)
              gamma = b1 / a1
              if( (gamma<=hugenum).and.(gamma>=-hugenum) ) then
                 if( gamma<zero ) then
                    call stdlib_csscal( l-i+1, -one, b( i, n-l+i ), ldb )
                    if( wantv )call stdlib_csscal( p, -one, v( 1_ilp, i ), 1_ilp )
                 end if
                 call stdlib_slartg( abs( gamma ), one, beta( k+i ), alpha( k+i ),rwk )
                 if( alpha( k+i )>=beta( k+i ) ) then
                    call stdlib_csscal( l-i+1, one / alpha( k+i ), a( k+i, n-l+i ),lda )
                 else
                    call stdlib_csscal( l-i+1, one / beta( k+i ), b( i, n-l+i ),ldb )
                    call stdlib_ccopy( l-i+1, b( i, n-l+i ), ldb, a( k+i, n-l+i ),lda )
                 end if
              else
                 alpha( k+i ) = zero
                 beta( k+i ) = one
                 call stdlib_ccopy( l-i+1, b( i, n-l+i ), ldb, a( k+i, n-l+i ),lda )
              end if
           end do
           ! post-assignment
           do i = m + 1, k + l
              alpha( i ) = zero
              beta( i ) = one
           end do
           if( k+l<n ) then
              do i = k + l + 1, n
                 alpha( i ) = zero
                 beta( i ) = zero
              end do
           end if
           100 continue
           ncycle = kcycle
           return
     end subroutine stdlib_ctgsja

     pure module subroutine stdlib_ztgsja( jobu, jobv, jobq, m, p, n, k, l, a, lda, b,ldb, tola, tolb, &
     !! ZTGSJA computes the generalized singular value decomposition (GSVD)
     !! of two complex upper triangular (or trapezoidal) matrices A and B.
     !! On entry, it is assumed that matrices A and B have the following
     !! forms, which may be obtained by the preprocessing subroutine ZGGSVP
     !! from a general M-by-N matrix A and P-by-N matrix B:
     !! N-K-L  K    L
     !! A =    K ( 0    A12  A13 ) if M-K-L >= 0;
     !! L ( 0     0   A23 )
     !! M-K-L ( 0     0    0  )
     !! N-K-L  K    L
     !! A =  K ( 0    A12  A13 ) if M-K-L < 0;
     !! M-K ( 0     0   A23 )
     !! N-K-L  K    L
     !! B =  L ( 0     0   B13 )
     !! P-L ( 0     0    0  )
     !! where the K-by-K matrix A12 and L-by-L matrix B13 are nonsingular
     !! upper triangular; A23 is L-by-L upper triangular if M-K-L >= 0,
     !! otherwise A23 is (M-K)-by-L upper trapezoidal.
     !! On exit,
     !! U**H *A*Q = D1*( 0 R ),    V**H *B*Q = D2*( 0 R ),
     !! where U, V and Q are unitary matrices.
     !! R is a nonsingular upper triangular matrix, and D1
     !! and D2 are ``diagonal'' matrices, which are of the following
     !! structures:
     !! If M-K-L >= 0,
     !! K  L
     !! D1 =     K ( I  0 )
     !! L ( 0  C )
     !! M-K-L ( 0  0 )
     !! K  L
     !! D2 = L   ( 0  S )
     !! P-L ( 0  0 )
     !! N-K-L  K    L
     !! ( 0 R ) = K (  0   R11  R12 ) K
     !! L (  0    0   R22 ) L
     !! where
     !! C = diag( ALPHA(K+1), ... , ALPHA(K+L) ),
     !! S = diag( BETA(K+1),  ... , BETA(K+L) ),
     !! C**2 + S**2 = I.
     !! R is stored in A(1:K+L,N-K-L+1:N) on exit.
     !! If M-K-L < 0,
     !! K M-K K+L-M
     !! D1 =   K ( I  0    0   )
     !! M-K ( 0  C    0   )
     !! K M-K K+L-M
     !! D2 =   M-K ( 0  S    0   )
     !! K+L-M ( 0  0    I   )
     !! P-L ( 0  0    0   )
     !! N-K-L  K   M-K  K+L-M
     !! ( 0 R ) =    K ( 0    R11  R12  R13  )
     !! M-K ( 0     0   R22  R23  )
     !! K+L-M ( 0     0    0   R33  )
     !! where
     !! C = diag( ALPHA(K+1), ... , ALPHA(M) ),
     !! S = diag( BETA(K+1),  ... , BETA(M) ),
     !! C**2 + S**2 = I.
     !! R = ( R11 R12 R13 ) is stored in A(1:M, N-K-L+1:N) and R33 is stored
     !! (  0  R22 R23 )
     !! in B(M-K+1:L,N+M-K-L+1:N) on exit.
     !! The computation of the unitary transformation matrices U, V or Q
     !! is optional.  These matrices may either be formed explicitly, or they
     !! may be postmultiplied into input matrices U1, V1, or Q1.
               alpha, beta, u, ldu, v, ldv,q, ldq, work, ncycle, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobq, jobu, jobv
           integer(ilp), intent(out) :: info, ncycle
           integer(ilp), intent(in) :: k, l, lda, ldb, ldq, ldu, ldv, m, n, p
           real(dp), intent(in) :: tola, tolb
           ! Array Arguments 
           real(dp), intent(out) :: alpha(*), beta(*)
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*), q(ldq,*), u(ldu,*), v(ldv,*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: maxit = 40_ilp
           real(dp), parameter :: hugenum = huge(zero)
           
           
           
           ! Local Scalars 
           logical(lk) :: initq, initu, initv, upper, wantq, wantu, wantv
           integer(ilp) :: i, j, kcycle
           real(dp) :: a1, a3, b1, b3, csq, csu, csv, error, gamma, rwk, ssmin
           complex(dp) :: a2, b2, snq, snu, snv
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode and test the input parameters
           initu = stdlib_lsame( jobu, 'I' )
           wantu = initu .or. stdlib_lsame( jobu, 'U' )
           initv = stdlib_lsame( jobv, 'I' )
           wantv = initv .or. stdlib_lsame( jobv, 'V' )
           initq = stdlib_lsame( jobq, 'I' )
           wantq = initq .or. stdlib_lsame( jobq, 'Q' )
           info = 0_ilp
           if( .not.( initu .or. wantu .or. stdlib_lsame( jobu, 'N' ) ) ) then
              info = -1_ilp
           else if( .not.( initv .or. wantv .or. stdlib_lsame( jobv, 'N' ) ) ) then
              info = -2_ilp
           else if( .not.( initq .or. wantq .or. stdlib_lsame( jobq, 'N' ) ) ) then
              info = -3_ilp
           else if( m<0_ilp ) then
              info = -4_ilp
           else if( p<0_ilp ) then
              info = -5_ilp
           else if( n<0_ilp ) then
              info = -6_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -10_ilp
           else if( ldb<max( 1_ilp, p ) ) then
              info = -12_ilp
           else if( ldu<1_ilp .or. ( wantu .and. ldu<m ) ) then
              info = -18_ilp
           else if( ldv<1_ilp .or. ( wantv .and. ldv<p ) ) then
              info = -20_ilp
           else if( ldq<1_ilp .or. ( wantq .and. ldq<n ) ) then
              info = -22_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZTGSJA', -info )
              return
           end if
           ! initialize u, v and q, if necessary
           if( initu )call stdlib_zlaset( 'FULL', m, m, czero, cone, u, ldu )
           if( initv )call stdlib_zlaset( 'FULL', p, p, czero, cone, v, ldv )
           if( initq )call stdlib_zlaset( 'FULL', n, n, czero, cone, q, ldq )
           ! loop until convergence
           upper = .false.
           loop_40: do kcycle = 1, maxit
              upper = .not.upper
              loop_20: do i = 1, l - 1
                 loop_10: do j = i + 1, l
                    a1 = zero
                    a2 = czero
                    a3 = zero
                    if( k+i<=m )a1 = real( a( k+i, n-l+i ),KIND=dp)
                    if( k+j<=m )a3 = real( a( k+j, n-l+j ),KIND=dp)
                    b1 = real( b( i, n-l+i ),KIND=dp)
                    b3 = real( b( j, n-l+j ),KIND=dp)
                    if( upper ) then
                       if( k+i<=m )a2 = a( k+i, n-l+j )
                       b2 = b( i, n-l+j )
                    else
                       if( k+j<=m )a2 = a( k+j, n-l+i )
                       b2 = b( j, n-l+i )
                    end if
                    call stdlib_zlags2( upper, a1, a2, a3, b1, b2, b3, csu, snu,csv, snv, csq, &
                              snq )
                    ! update (k+i)-th and (k+j)-th rows of matrix a: u**h *a
                    if( k+j<=m )call stdlib_zrot( l, a( k+j, n-l+1 ), lda, a( k+i, n-l+1 ),lda, &
                              csu, conjg( snu ) )
                    ! update i-th and j-th rows of matrix b: v**h *b
                    call stdlib_zrot( l, b( j, n-l+1 ), ldb, b( i, n-l+1 ), ldb,csv, conjg( snv ) &
                              )
                    ! update (n-l+i)-th and (n-l+j)-th columns of matrices
                    ! a and b: a*q and b*q
                    call stdlib_zrot( min( k+l, m ), a( 1_ilp, n-l+j ), 1_ilp,a( 1_ilp, n-l+i ), 1_ilp, csq, snq )
                              
                    call stdlib_zrot( l, b( 1_ilp, n-l+j ), 1_ilp, b( 1_ilp, n-l+i ), 1_ilp, csq,snq )
                    if( upper ) then
                       if( k+i<=m )a( k+i, n-l+j ) = czero
                       b( i, n-l+j ) = czero
                    else
                       if( k+j<=m )a( k+j, n-l+i ) = czero
                       b( j, n-l+i ) = czero
                    end if
                    ! ensure that the diagonal elements of a and b are real.
                    if( k+i<=m )a( k+i, n-l+i ) = real( a( k+i, n-l+i ),KIND=dp)
                    if( k+j<=m )a( k+j, n-l+j ) = real( a( k+j, n-l+j ),KIND=dp)
                    b( i, n-l+i ) = real( b( i, n-l+i ),KIND=dp)
                    b( j, n-l+j ) = real( b( j, n-l+j ),KIND=dp)
                    ! update unitary matrices u, v, q, if desired.
                    if( wantu .and. k+j<=m )call stdlib_zrot( m, u( 1_ilp, k+j ), 1_ilp, u( 1_ilp, k+i ), 1_ilp, &
                              csu,snu )
                    if( wantv )call stdlib_zrot( p, v( 1_ilp, j ), 1_ilp, v( 1_ilp, i ), 1_ilp, csv, snv )
                    if( wantq )call stdlib_zrot( n, q( 1_ilp, n-l+j ), 1_ilp, q( 1_ilp, n-l+i ), 1_ilp, csq,snq )
                              
                 end do loop_10
              end do loop_20
              if( .not.upper ) then
                 ! the matrices a13 and b13 were lower triangular at the start
                 ! of the cycle, and are now upper triangular.
                 ! convergence test: test the parallelism of the corresponding
                 ! rows of a and b.
                 error = zero
                 do i = 1, min( l, m-k )
                    call stdlib_zcopy( l-i+1, a( k+i, n-l+i ), lda, work, 1_ilp )
                    call stdlib_zcopy( l-i+1, b( i, n-l+i ), ldb, work( l+1 ), 1_ilp )
                    call stdlib_zlapll( l-i+1, work, 1_ilp, work( l+1 ), 1_ilp, ssmin )
                    error = max( error, ssmin )
                 end do
                 if( abs( error )<=min( tola, tolb ) )go to 50
              end if
              ! end of cycle loop
           end do loop_40
           ! the algorithm has not converged after maxit cycles.
           info = 1_ilp
           go to 100
           50 continue
           ! if error <= min(tola,tolb), then the algorithm has converged.
           ! compute the generalized singular value pairs (alpha, beta), and
           ! set the triangular matrix r to array a.
           do i = 1, k
              alpha( i ) = one
              beta( i ) = zero
           end do
           do i = 1, min( l, m-k )
              a1 = real( a( k+i, n-l+i ),KIND=dp)
              b1 = real( b( i, n-l+i ),KIND=dp)
              gamma = b1 / a1
              if( (gamma<=hugenum).and.(gamma>=-hugenum) ) then
                 if( gamma<zero ) then
                    call stdlib_zdscal( l-i+1, -one, b( i, n-l+i ), ldb )
                    if( wantv )call stdlib_zdscal( p, -one, v( 1_ilp, i ), 1_ilp )
                 end if
                 call stdlib_dlartg( abs( gamma ), one, beta( k+i ), alpha( k+i ),rwk )
                 if( alpha( k+i )>=beta( k+i ) ) then
                    call stdlib_zdscal( l-i+1, one / alpha( k+i ), a( k+i, n-l+i ),lda )
                 else
                    call stdlib_zdscal( l-i+1, one / beta( k+i ), b( i, n-l+i ),ldb )
                    call stdlib_zcopy( l-i+1, b( i, n-l+i ), ldb, a( k+i, n-l+i ),lda )
                 end if
              else
                 alpha( k+i ) = zero
                 beta( k+i ) = one
                 call stdlib_zcopy( l-i+1, b( i, n-l+i ), ldb, a( k+i, n-l+i ),lda )
              end if
           end do
           ! post-assignment
           do i = m + 1, k + l
              alpha( i ) = zero
              beta( i ) = one
           end do
           if( k+l<n ) then
              do i = k + l + 1, n
                 alpha( i ) = zero
                 beta( i ) = zero
              end do
           end if
           100 continue
           ncycle = kcycle
           return
     end subroutine stdlib_ztgsja




     pure module subroutine stdlib_cungbr( vect, m, n, k, a, lda, tau, work, lwork, info )
     !! CUNGBR generates one of the complex unitary matrices Q or P**H
     !! determined by CGEBRD when reducing a complex matrix A to bidiagonal
     !! form: A = Q * B * P**H.  Q and P**H are defined as products of
     !! elementary reflectors H(i) or G(i) respectively.
     !! If VECT = 'Q', A is assumed to have been an M-by-K matrix, and Q
     !! is of order M:
     !! if m >= k, Q = H(1) H(2) . . . H(k) and CUNGBR returns the first n
     !! columns of Q, where m >= n >= k;
     !! if m < k, Q = H(1) H(2) . . . H(m-1) and CUNGBR returns Q as an
     !! M-by-M matrix.
     !! If VECT = 'P', A is assumed to have been a K-by-N matrix, and P**H
     !! is of order N:
     !! if k < n, P**H = G(k) . . . G(2) G(1) and CUNGBR returns the first m
     !! rows of P**H, where n >= m >= k;
     !! if k >= n, P**H = G(n-1) . . . G(2) G(1) and CUNGBR returns P**H as
     !! an N-by-N matrix.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: vect
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, lwork, m, n
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(in) :: tau(*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery, wantq
           integer(ilp) :: i, iinfo, j, lwkopt, mn
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           wantq = stdlib_lsame( vect, 'Q' )
           mn = min( m, n )
           lquery = ( lwork==-1_ilp )
           if( .not.wantq .and. .not.stdlib_lsame( vect, 'P' ) ) then
              info = -1_ilp
           else if( m<0_ilp ) then
              info = -2_ilp
           else if( n<0_ilp .or. ( wantq .and. ( n>m .or. n<min( m,k ) ) ) .or. ( .not.wantq .and. ( &
                     m>n .or. m<min( n, k ) ) ) ) then
              info = -3_ilp
           else if( k<0_ilp ) then
              info = -4_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -6_ilp
           else if( lwork<max( 1_ilp, mn ) .and. .not.lquery ) then
              info = -9_ilp
           end if
           if( info==0_ilp ) then
              work( 1_ilp ) = 1_ilp
              if( wantq ) then
                 if( m>=k ) then
                    call stdlib_cungqr( m, n, k, a, lda, tau, work, -1_ilp, iinfo )
                 else
                    if( m>1_ilp ) then
                       call stdlib_cungqr( m-1, m-1, m-1, a, lda, tau, work, -1_ilp,iinfo )
                    end if
                 end if
              else
                 if( k<n ) then
                    call stdlib_cunglq( m, n, k, a, lda, tau, work, -1_ilp, iinfo )
                 else
                    if( n>1_ilp ) then
                       call stdlib_cunglq( n-1, n-1, n-1, a, lda, tau, work, -1_ilp,iinfo )
                    end if
                 end if
              end if
              lwkopt = real( work( 1_ilp ),KIND=sp)
              lwkopt = max (lwkopt, mn)
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CUNGBR', -info )
              return
           else if( lquery ) then
              work( 1_ilp ) = lwkopt
              return
           end if
           ! quick return if possible
           if( m==0_ilp .or. n==0_ilp ) then
              work( 1_ilp ) = 1_ilp
              return
           end if
           if( wantq ) then
              ! form q, determined by a call to stdlib_cgebrd to reduce an m-by-k
              ! matrix
              if( m>=k ) then
                 ! if m >= k, assume m >= n >= k
                 call stdlib_cungqr( m, n, k, a, lda, tau, work, lwork, iinfo )
              else
                 ! if m < k, assume m = n
                 ! shift the vectors which define the elementary reflectors cone
                 ! column to the right, and set the first row and column of q
                 ! to those of the unit matrix
                 do j = m, 2, -1
                    a( 1_ilp, j ) = czero
                    do i = j + 1, m
                       a( i, j ) = a( i, j-1 )
                    end do
                 end do
                 a( 1_ilp, 1_ilp ) = cone
                 do i = 2, m
                    a( i, 1_ilp ) = czero
                 end do
                 if( m>1_ilp ) then
                    ! form q(2:m,2:m)
                    call stdlib_cungqr( m-1, m-1, m-1, a( 2_ilp, 2_ilp ), lda, tau, work,lwork, iinfo )
                              
                 end if
              end if
           else
              ! form p**h, determined by a call to stdlib_cgebrd to reduce a k-by-n
              ! matrix
              if( k<n ) then
                 ! if k < n, assume k <= m <= n
                 call stdlib_cunglq( m, n, k, a, lda, tau, work, lwork, iinfo )
              else
                 ! if k >= n, assume m = n
                 ! shift the vectors which define the elementary reflectors cone
                 ! row downward, and set the first row and column of p**h to
                 ! those of the unit matrix
                 a( 1_ilp, 1_ilp ) = cone
                 do i = 2, n
                    a( i, 1_ilp ) = czero
                 end do
                 do j = 2, n
                    do i = j - 1, 2, -1
                       a( i, j ) = a( i-1, j )
                    end do
                    a( 1_ilp, j ) = czero
                 end do
                 if( n>1_ilp ) then
                    ! form p**h(2:n,2:n)
                    call stdlib_cunglq( n-1, n-1, n-1, a( 2_ilp, 2_ilp ), lda, tau, work,lwork, iinfo )
                              
                 end if
              end if
           end if
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_cungbr

     pure module subroutine stdlib_zungbr( vect, m, n, k, a, lda, tau, work, lwork, info )
     !! ZUNGBR generates one of the complex unitary matrices Q or P**H
     !! determined by ZGEBRD when reducing a complex matrix A to bidiagonal
     !! form: A = Q * B * P**H.  Q and P**H are defined as products of
     !! elementary reflectors H(i) or G(i) respectively.
     !! If VECT = 'Q', A is assumed to have been an M-by-K matrix, and Q
     !! is of order M:
     !! if m >= k, Q = H(1) H(2) . . . H(k) and ZUNGBR returns the first n
     !! columns of Q, where m >= n >= k;
     !! if m < k, Q = H(1) H(2) . . . H(m-1) and ZUNGBR returns Q as an
     !! M-by-M matrix.
     !! If VECT = 'P', A is assumed to have been a K-by-N matrix, and P**H
     !! is of order N:
     !! if k < n, P**H = G(k) . . . G(2) G(1) and ZUNGBR returns the first m
     !! rows of P**H, where n >= m >= k;
     !! if k >= n, P**H = G(n-1) . . . G(2) G(1) and ZUNGBR returns P**H as
     !! an N-by-N matrix.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: vect
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, lwork, m, n
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(in) :: tau(*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery, wantq
           integer(ilp) :: i, iinfo, j, lwkopt, mn
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           wantq = stdlib_lsame( vect, 'Q' )
           mn = min( m, n )
           lquery = ( lwork==-1_ilp )
           if( .not.wantq .and. .not.stdlib_lsame( vect, 'P' ) ) then
              info = -1_ilp
           else if( m<0_ilp ) then
              info = -2_ilp
           else if( n<0_ilp .or. ( wantq .and. ( n>m .or. n<min( m,k ) ) ) .or. ( .not.wantq .and. ( &
                     m>n .or. m<min( n, k ) ) ) ) then
              info = -3_ilp
           else if( k<0_ilp ) then
              info = -4_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -6_ilp
           else if( lwork<max( 1_ilp, mn ) .and. .not.lquery ) then
              info = -9_ilp
           end if
           if( info==0_ilp ) then
              work( 1_ilp ) = 1_ilp
              if( wantq ) then
                 if( m>=k ) then
                    call stdlib_zungqr( m, n, k, a, lda, tau, work, -1_ilp, iinfo )
                 else
                    if( m>1_ilp ) then
                       call stdlib_zungqr( m-1, m-1, m-1, a, lda, tau, work, -1_ilp,iinfo )
                    end if
                 end if
              else
                 if( k<n ) then
                    call stdlib_zunglq( m, n, k, a, lda, tau, work, -1_ilp, iinfo )
                 else
                    if( n>1_ilp ) then
                       call stdlib_zunglq( n-1, n-1, n-1, a, lda, tau, work, -1_ilp,iinfo )
                    end if
                 end if
              end if
              lwkopt = real( work( 1_ilp ),KIND=dp)
              lwkopt = max (lwkopt, mn)
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZUNGBR', -info )
              return
           else if( lquery ) then
              work( 1_ilp ) = lwkopt
              return
           end if
           ! quick return if possible
           if( m==0_ilp .or. n==0_ilp ) then
              work( 1_ilp ) = 1_ilp
              return
           end if
           if( wantq ) then
              ! form q, determined by a call to stdlib_zgebrd to reduce an m-by-k
              ! matrix
              if( m>=k ) then
                 ! if m >= k, assume m >= n >= k
                 call stdlib_zungqr( m, n, k, a, lda, tau, work, lwork, iinfo )
              else
                 ! if m < k, assume m = n
                 ! shift the vectors which define the elementary reflectors cone
                 ! column to the right, and set the first row and column of q
                 ! to those of the unit matrix
                 do j = m, 2, -1
                    a( 1_ilp, j ) = czero
                    do i = j + 1, m
                       a( i, j ) = a( i, j-1 )
                    end do
                 end do
                 a( 1_ilp, 1_ilp ) = cone
                 do i = 2, m
                    a( i, 1_ilp ) = czero
                 end do
                 if( m>1_ilp ) then
                    ! form q(2:m,2:m)
                    call stdlib_zungqr( m-1, m-1, m-1, a( 2_ilp, 2_ilp ), lda, tau, work,lwork, iinfo )
                              
                 end if
              end if
           else
              ! form p**h, determined by a call to stdlib_zgebrd to reduce a k-by-n
              ! matrix
              if( k<n ) then
                 ! if k < n, assume k <= m <= n
                 call stdlib_zunglq( m, n, k, a, lda, tau, work, lwork, iinfo )
              else
                 ! if k >= n, assume m = n
                 ! shift the vectors which define the elementary reflectors cone
                 ! row downward, and set the first row and column of p**h to
                 ! those of the unit matrix
                 a( 1_ilp, 1_ilp ) = cone
                 do i = 2, n
                    a( i, 1_ilp ) = czero
                 end do
                 do j = 2, n
                    do i = j - 1, 2, -1
                       a( i, j ) = a( i-1, j )
                    end do
                    a( 1_ilp, j ) = czero
                 end do
                 if( n>1_ilp ) then
                    ! form p**h(2:n,2:n)
                    call stdlib_zunglq( n-1, n-1, n-1, a( 2_ilp, 2_ilp ), lda, tau, work,lwork, iinfo )
                              
                 end if
              end if
           end if
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_zungbr




     pure module subroutine stdlib_sorgbr( vect, m, n, k, a, lda, tau, work, lwork, info )
     !! SORGBR generates one of the real orthogonal matrices Q or P**T
     !! determined by SGEBRD when reducing a real matrix A to bidiagonal
     !! form: A = Q * B * P**T.  Q and P**T are defined as products of
     !! elementary reflectors H(i) or G(i) respectively.
     !! If VECT = 'Q', A is assumed to have been an M-by-K matrix, and Q
     !! is of order M:
     !! if m >= k, Q = H(1) H(2) . . . H(k) and SORGBR returns the first n
     !! columns of Q, where m >= n >= k;
     !! if m < k, Q = H(1) H(2) . . . H(m-1) and SORGBR returns Q as an
     !! M-by-M matrix.
     !! If VECT = 'P', A is assumed to have been a K-by-N matrix, and P**T
     !! is of order N:
     !! if k < n, P**T = G(k) . . . G(2) G(1) and SORGBR returns the first m
     !! rows of P**T, where n >= m >= k;
     !! if k >= n, P**T = G(n-1) . . . G(2) G(1) and SORGBR returns P**T as
     !! an N-by-N matrix.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: vect
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, lwork, m, n
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(in) :: tau(*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery, wantq
           integer(ilp) :: i, iinfo, j, lwkopt, mn
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           wantq = stdlib_lsame( vect, 'Q' )
           mn = min( m, n )
           lquery = ( lwork==-1_ilp )
           if( .not.wantq .and. .not.stdlib_lsame( vect, 'P' ) ) then
              info = -1_ilp
           else if( m<0_ilp ) then
              info = -2_ilp
           else if( n<0_ilp .or. ( wantq .and. ( n>m .or. n<min( m,k ) ) ) .or. ( .not.wantq .and. ( &
                     m>n .or. m<min( n, k ) ) ) ) then
              info = -3_ilp
           else if( k<0_ilp ) then
              info = -4_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -6_ilp
           else if( lwork<max( 1_ilp, mn ) .and. .not.lquery ) then
              info = -9_ilp
           end if
           if( info==0_ilp ) then
              work( 1_ilp ) = 1_ilp
              if( wantq ) then
                 if( m>=k ) then
                    call stdlib_sorgqr( m, n, k, a, lda, tau, work, -1_ilp, iinfo )
                 else
                    if( m>1_ilp ) then
                       call stdlib_sorgqr( m-1, m-1, m-1, a, lda, tau, work, -1_ilp,iinfo )
                    end if
                 end if
              else
                 if( k<n ) then
                    call stdlib_sorglq( m, n, k, a, lda, tau, work, -1_ilp, iinfo )
                 else
                    if( n>1_ilp ) then
                       call stdlib_sorglq( n-1, n-1, n-1, a, lda, tau, work, -1_ilp,iinfo )
                    end if
                 end if
              end if
              lwkopt = work( 1_ilp )
              lwkopt = max (lwkopt, mn)
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SORGBR', -info )
              return
           else if( lquery ) then
              work( 1_ilp ) = lwkopt
              return
           end if
           ! quick return if possible
           if( m==0_ilp .or. n==0_ilp ) then
              work( 1_ilp ) = 1_ilp
              return
           end if
           if( wantq ) then
              ! form q, determined by a call to stdlib_sgebrd to reduce an m-by-k
              ! matrix
              if( m>=k ) then
                 ! if m >= k, assume m >= n >= k
                 call stdlib_sorgqr( m, n, k, a, lda, tau, work, lwork, iinfo )
              else
                 ! if m < k, assume m = n
                 ! shift the vectors which define the elementary reflectors one
                 ! column to the right, and set the first row and column of q
                 ! to those of the unit matrix
                 do j = m, 2, -1
                    a( 1_ilp, j ) = zero
                    do i = j + 1, m
                       a( i, j ) = a( i, j-1 )
                    end do
                 end do
                 a( 1_ilp, 1_ilp ) = one
                 do i = 2, m
                    a( i, 1_ilp ) = zero
                 end do
                 if( m>1_ilp ) then
                    ! form q(2:m,2:m)
                    call stdlib_sorgqr( m-1, m-1, m-1, a( 2_ilp, 2_ilp ), lda, tau, work,lwork, iinfo )
                              
                 end if
              end if
           else
              ! form p**t, determined by a call to stdlib_sgebrd to reduce a k-by-n
              ! matrix
              if( k<n ) then
                 ! if k < n, assume k <= m <= n
                 call stdlib_sorglq( m, n, k, a, lda, tau, work, lwork, iinfo )
              else
                 ! if k >= n, assume m = n
                 ! shift the vectors which define the elementary reflectors one
                 ! row downward, and set the first row and column of p**t to
                 ! those of the unit matrix
                 a( 1_ilp, 1_ilp ) = one
                 do i = 2, n
                    a( i, 1_ilp ) = zero
                 end do
                 do j = 2, n
                    do i = j - 1, 2, -1
                       a( i, j ) = a( i-1, j )
                    end do
                    a( 1_ilp, j ) = zero
                 end do
                 if( n>1_ilp ) then
                    ! form p**t(2:n,2:n)
                    call stdlib_sorglq( n-1, n-1, n-1, a( 2_ilp, 2_ilp ), lda, tau, work,lwork, iinfo )
                              
                 end if
              end if
           end if
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_sorgbr

     pure module subroutine stdlib_dorgbr( vect, m, n, k, a, lda, tau, work, lwork, info )
     !! DORGBR generates one of the real orthogonal matrices Q or P**T
     !! determined by DGEBRD when reducing a real matrix A to bidiagonal
     !! form: A = Q * B * P**T.  Q and P**T are defined as products of
     !! elementary reflectors H(i) or G(i) respectively.
     !! If VECT = 'Q', A is assumed to have been an M-by-K matrix, and Q
     !! is of order M:
     !! if m >= k, Q = H(1) H(2) . . . H(k) and DORGBR returns the first n
     !! columns of Q, where m >= n >= k;
     !! if m < k, Q = H(1) H(2) . . . H(m-1) and DORGBR returns Q as an
     !! M-by-M matrix.
     !! If VECT = 'P', A is assumed to have been a K-by-N matrix, and P**T
     !! is of order N:
     !! if k < n, P**T = G(k) . . . G(2) G(1) and DORGBR returns the first m
     !! rows of P**T, where n >= m >= k;
     !! if k >= n, P**T = G(n-1) . . . G(2) G(1) and DORGBR returns P**T as
     !! an N-by-N matrix.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: vect
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, lwork, m, n
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(in) :: tau(*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery, wantq
           integer(ilp) :: i, iinfo, j, lwkopt, mn
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           wantq = stdlib_lsame( vect, 'Q' )
           mn = min( m, n )
           lquery = ( lwork==-1_ilp )
           if( .not.wantq .and. .not.stdlib_lsame( vect, 'P' ) ) then
              info = -1_ilp
           else if( m<0_ilp ) then
              info = -2_ilp
           else if( n<0_ilp .or. ( wantq .and. ( n>m .or. n<min( m,k ) ) ) .or. ( .not.wantq .and. ( &
                     m>n .or. m<min( n, k ) ) ) ) then
              info = -3_ilp
           else if( k<0_ilp ) then
              info = -4_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -6_ilp
           else if( lwork<max( 1_ilp, mn ) .and. .not.lquery ) then
              info = -9_ilp
           end if
           if( info==0_ilp ) then
              work( 1_ilp ) = 1_ilp
              if( wantq ) then
                 if( m>=k ) then
                    call stdlib_dorgqr( m, n, k, a, lda, tau, work, -1_ilp, iinfo )
                 else
                    if( m>1_ilp ) then
                       call stdlib_dorgqr( m-1, m-1, m-1, a, lda, tau, work, -1_ilp,iinfo )
                    end if
                 end if
              else
                 if( k<n ) then
                    call stdlib_dorglq( m, n, k, a, lda, tau, work, -1_ilp, iinfo )
                 else
                    if( n>1_ilp ) then
                       call stdlib_dorglq( n-1, n-1, n-1, a, lda, tau, work, -1_ilp,iinfo )
                    end if
                 end if
              end if
              lwkopt = work( 1_ilp )
              lwkopt = max (lwkopt, mn)
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DORGBR', -info )
              return
           else if( lquery ) then
              work( 1_ilp ) = lwkopt
              return
           end if
           ! quick return if possible
           if( m==0_ilp .or. n==0_ilp ) then
              work( 1_ilp ) = 1_ilp
              return
           end if
           if( wantq ) then
              ! form q, determined by a call to stdlib_dgebrd to reduce an m-by-k
              ! matrix
              if( m>=k ) then
                 ! if m >= k, assume m >= n >= k
                 call stdlib_dorgqr( m, n, k, a, lda, tau, work, lwork, iinfo )
              else
                 ! if m < k, assume m = n
                 ! shift the vectors which define the elementary reflectors one
                 ! column to the right, and set the first row and column of q
                 ! to those of the unit matrix
                 do j = m, 2, -1
                    a( 1_ilp, j ) = zero
                    do i = j + 1, m
                       a( i, j ) = a( i, j-1 )
                    end do
                 end do
                 a( 1_ilp, 1_ilp ) = one
                 do i = 2, m
                    a( i, 1_ilp ) = zero
                 end do
                 if( m>1_ilp ) then
                    ! form q(2:m,2:m)
                    call stdlib_dorgqr( m-1, m-1, m-1, a( 2_ilp, 2_ilp ), lda, tau, work,lwork, iinfo )
                              
                 end if
              end if
           else
              ! form p**t, determined by a call to stdlib_dgebrd to reduce a k-by-n
              ! matrix
              if( k<n ) then
                 ! if k < n, assume k <= m <= n
                 call stdlib_dorglq( m, n, k, a, lda, tau, work, lwork, iinfo )
              else
                 ! if k >= n, assume m = n
                 ! shift the vectors which define the elementary reflectors one
                 ! row downward, and set the first row and column of p**t to
                 ! those of the unit matrix
                 a( 1_ilp, 1_ilp ) = one
                 do i = 2, n
                    a( i, 1_ilp ) = zero
                 end do
                 do j = 2, n
                    do i = j - 1, 2, -1
                       a( i, j ) = a( i-1, j )
                    end do
                    a( 1_ilp, j ) = zero
                 end do
                 if( n>1_ilp ) then
                    ! form p**t(2:n,2:n)
                    call stdlib_dorglq( n-1, n-1, n-1, a( 2_ilp, 2_ilp ), lda, tau, work,lwork, iinfo )
                              
                 end if
              end if
           end if
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_dorgbr




     pure module subroutine stdlib_cunmbr( vect, side, trans, m, n, k, a, lda, tau, c,ldc, work, lwork, &
     !! If VECT = 'Q', CUNMBR: overwrites the general complex M-by-N matrix C
     !! with
     !! SIDE = 'L'     SIDE = 'R'
     !! TRANS = 'N':      Q * C          C * Q
     !! TRANS = 'C':      Q**H * C       C * Q**H
     !! If VECT = 'P', CUNMBR overwrites the general complex M-by-N matrix C
     !! with
     !! SIDE = 'L'     SIDE = 'R'
     !! TRANS = 'N':      P * C          C * P
     !! TRANS = 'C':      P**H * C       C * P**H
     !! Here Q and P**H are the unitary matrices determined by CGEBRD when
     !! reducing a complex matrix A to bidiagonal form: A = Q * B * P**H. Q
     !! and P**H are defined as products of elementary reflectors H(i) and
     !! G(i) respectively.
     !! Let nq = m if SIDE = 'L' and nq = n if SIDE = 'R'. Thus nq is the
     !! order of the unitary matrix Q or P**H that is applied.
     !! If VECT = 'Q', A is assumed to have been an NQ-by-K matrix:
     !! if nq >= k, Q = H(1) H(2) . . . H(k);
     !! if nq < k, Q = H(1) H(2) . . . H(nq-1).
     !! If VECT = 'P', A is assumed to have been a K-by-NQ matrix:
     !! if k < nq, P = G(1) G(2) . . . G(k);
     !! if k >= nq, P = G(1) G(2) . . . G(nq-1).
               info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side, trans, vect
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, ldc, lwork, m, n
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*), c(ldc,*)
           complex(sp), intent(in) :: tau(*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: applyq, left, lquery, notran
           character :: transt
           integer(ilp) :: i1, i2, iinfo, lwkopt, mi, nb, ni, nq, nw
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           applyq = stdlib_lsame( vect, 'Q' )
           left = stdlib_lsame( side, 'L' )
           notran = stdlib_lsame( trans, 'N' )
           lquery = ( lwork==-1_ilp )
           ! nq is the order of q or p and nw is the minimum dimension of work
           if( left ) then
              nq = m
              nw = max( 1_ilp, n )
           else
              nq = n
              nw = max( 1_ilp, m )
           end if
           if( .not.applyq .and. .not.stdlib_lsame( vect, 'P' ) ) then
              info = -1_ilp
           else if( .not.left .and. .not.stdlib_lsame( side, 'R' ) ) then
              info = -2_ilp
           else if( .not.notran .and. .not.stdlib_lsame( trans, 'C' ) ) then
              info = -3_ilp
           else if( m<0_ilp ) then
              info = -4_ilp
           else if( n<0_ilp ) then
              info = -5_ilp
           else if( k<0_ilp ) then
              info = -6_ilp
           else if( ( applyq .and. lda<max( 1_ilp, nq ) ) .or.( .not.applyq .and. lda<max( 1_ilp, min( nq,&
                      k ) ) ) )then
              info = -8_ilp
           else if( ldc<max( 1_ilp, m ) ) then
              info = -11_ilp
           else if( lwork<nw .and. .not.lquery ) then
              info = -13_ilp
           end if
           if( info==0_ilp ) then
              if( m>0_ilp .and. n>0_ilp ) then
                 if( applyq ) then
                    if( left ) then
                       nb = stdlib_ilaenv( 1_ilp, 'CUNMQR', side // trans, m-1, n, m-1,-1_ilp )
                    else
                       nb = stdlib_ilaenv( 1_ilp, 'CUNMQR', side // trans, m, n-1, n-1,-1_ilp )
                    end if
                 else
                    if( left ) then
                       nb = stdlib_ilaenv( 1_ilp, 'CUNMLQ', side // trans, m-1, n, m-1,-1_ilp )
                    else
                       nb = stdlib_ilaenv( 1_ilp, 'CUNMLQ', side // trans, m, n-1, n-1,-1_ilp )
                    end if
                 end if
                 lwkopt = nw*nb
              else
                 lwkopt = 1_ilp
              end if
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CUNMBR', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( m==0 .or. n==0 )return
           if( applyq ) then
              ! apply q
              if( nq>=k ) then
                 ! q was determined by a call to stdlib_cgebrd with nq >= k
                 call stdlib_cunmqr( side, trans, m, n, k, a, lda, tau, c, ldc,work, lwork, iinfo &
                           )
              else if( nq>1_ilp ) then
                 ! q was determined by a call to stdlib_cgebrd with nq < k
                 if( left ) then
                    mi = m - 1_ilp
                    ni = n
                    i1 = 2_ilp
                    i2 = 1_ilp
                 else
                    mi = m
                    ni = n - 1_ilp
                    i1 = 1_ilp
                    i2 = 2_ilp
                 end if
                 call stdlib_cunmqr( side, trans, mi, ni, nq-1, a( 2_ilp, 1_ilp ), lda, tau,c( i1, i2 ), &
                           ldc, work, lwork, iinfo )
              end if
           else
              ! apply p
              if( notran ) then
                 transt = 'C'
              else
                 transt = 'N'
              end if
              if( nq>k ) then
                 ! p was determined by a call to stdlib_cgebrd with nq > k
                 call stdlib_cunmlq( side, transt, m, n, k, a, lda, tau, c, ldc,work, lwork, &
                           iinfo )
              else if( nq>1_ilp ) then
                 ! p was determined by a call to stdlib_cgebrd with nq <= k
                 if( left ) then
                    mi = m - 1_ilp
                    ni = n
                    i1 = 2_ilp
                    i2 = 1_ilp
                 else
                    mi = m
                    ni = n - 1_ilp
                    i1 = 1_ilp
                    i2 = 2_ilp
                 end if
                 call stdlib_cunmlq( side, transt, mi, ni, nq-1, a( 1_ilp, 2_ilp ), lda,tau, c( i1, i2 ), &
                           ldc, work, lwork, iinfo )
              end if
           end if
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_cunmbr

     pure module subroutine stdlib_zunmbr( vect, side, trans, m, n, k, a, lda, tau, c,ldc, work, lwork, &
     !! If VECT = 'Q', ZUNMBR: overwrites the general complex M-by-N matrix C
     !! with
     !! SIDE = 'L'     SIDE = 'R'
     !! TRANS = 'N':      Q * C          C * Q
     !! TRANS = 'C':      Q**H * C       C * Q**H
     !! If VECT = 'P', ZUNMBR overwrites the general complex M-by-N matrix C
     !! with
     !! SIDE = 'L'     SIDE = 'R'
     !! TRANS = 'N':      P * C          C * P
     !! TRANS = 'C':      P**H * C       C * P**H
     !! Here Q and P**H are the unitary matrices determined by ZGEBRD when
     !! reducing a complex matrix A to bidiagonal form: A = Q * B * P**H. Q
     !! and P**H are defined as products of elementary reflectors H(i) and
     !! G(i) respectively.
     !! Let nq = m if SIDE = 'L' and nq = n if SIDE = 'R'. Thus nq is the
     !! order of the unitary matrix Q or P**H that is applied.
     !! If VECT = 'Q', A is assumed to have been an NQ-by-K matrix:
     !! if nq >= k, Q = H(1) H(2) . . . H(k);
     !! if nq < k, Q = H(1) H(2) . . . H(nq-1).
     !! If VECT = 'P', A is assumed to have been a K-by-NQ matrix:
     !! if k < nq, P = G(1) G(2) . . . G(k);
     !! if k >= nq, P = G(1) G(2) . . . G(nq-1).
               info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side, trans, vect
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, ldc, lwork, m, n
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*), c(ldc,*)
           complex(dp), intent(in) :: tau(*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: applyq, left, lquery, notran
           character :: transt
           integer(ilp) :: i1, i2, iinfo, lwkopt, mi, nb, ni, nq, nw
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           applyq = stdlib_lsame( vect, 'Q' )
           left = stdlib_lsame( side, 'L' )
           notran = stdlib_lsame( trans, 'N' )
           lquery = ( lwork==-1_ilp )
           ! nq is the order of q or p and nw is the minimum dimension of work
           if( left ) then
              nq = m
              nw = max( 1_ilp, n )
           else
              nq = n
              nw = max( 1_ilp, m )
           end if
           if( .not.applyq .and. .not.stdlib_lsame( vect, 'P' ) ) then
              info = -1_ilp
           else if( .not.left .and. .not.stdlib_lsame( side, 'R' ) ) then
              info = -2_ilp
           else if( .not.notran .and. .not.stdlib_lsame( trans, 'C' ) ) then
              info = -3_ilp
           else if( m<0_ilp ) then
              info = -4_ilp
           else if( n<0_ilp ) then
              info = -5_ilp
           else if( k<0_ilp ) then
              info = -6_ilp
           else if( ( applyq .and. lda<max( 1_ilp, nq ) ) .or.( .not.applyq .and. lda<max( 1_ilp, min( nq,&
                      k ) ) ) )then
              info = -8_ilp
           else if( ldc<max( 1_ilp, m ) ) then
              info = -11_ilp
           else if( lwork<nw .and. .not.lquery ) then
              info = -13_ilp
           end if
           if( info==0_ilp ) then
              if( m>0_ilp .and. n>0_ilp ) then
                 if( applyq ) then
                    if( left ) then
                       nb = stdlib_ilaenv( 1_ilp, 'ZUNMQR', side // trans, m-1, n, m-1,-1_ilp )
                    else
                       nb = stdlib_ilaenv( 1_ilp, 'ZUNMQR', side // trans, m, n-1, n-1,-1_ilp )
                    end if
                 else
                    if( left ) then
                       nb = stdlib_ilaenv( 1_ilp, 'ZUNMLQ', side // trans, m-1, n, m-1,-1_ilp )
                    else
                       nb = stdlib_ilaenv( 1_ilp, 'ZUNMLQ', side // trans, m, n-1, n-1,-1_ilp )
                    end if
                 end if
                 lwkopt = nw*nb
              else
                 lwkopt = 1_ilp
              end if
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZUNMBR', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( m==0 .or. n==0 )return
           if( applyq ) then
              ! apply q
              if( nq>=k ) then
                 ! q was determined by a call to stdlib_zgebrd with nq >= k
                 call stdlib_zunmqr( side, trans, m, n, k, a, lda, tau, c, ldc,work, lwork, iinfo &
                           )
              else if( nq>1_ilp ) then
                 ! q was determined by a call to stdlib_zgebrd with nq < k
                 if( left ) then
                    mi = m - 1_ilp
                    ni = n
                    i1 = 2_ilp
                    i2 = 1_ilp
                 else
                    mi = m
                    ni = n - 1_ilp
                    i1 = 1_ilp
                    i2 = 2_ilp
                 end if
                 call stdlib_zunmqr( side, trans, mi, ni, nq-1, a( 2_ilp, 1_ilp ), lda, tau,c( i1, i2 ), &
                           ldc, work, lwork, iinfo )
              end if
           else
              ! apply p
              if( notran ) then
                 transt = 'C'
              else
                 transt = 'N'
              end if
              if( nq>k ) then
                 ! p was determined by a call to stdlib_zgebrd with nq > k
                 call stdlib_zunmlq( side, transt, m, n, k, a, lda, tau, c, ldc,work, lwork, &
                           iinfo )
              else if( nq>1_ilp ) then
                 ! p was determined by a call to stdlib_zgebrd with nq <= k
                 if( left ) then
                    mi = m - 1_ilp
                    ni = n
                    i1 = 2_ilp
                    i2 = 1_ilp
                 else
                    mi = m
                    ni = n - 1_ilp
                    i1 = 1_ilp
                    i2 = 2_ilp
                 end if
                 call stdlib_zunmlq( side, transt, mi, ni, nq-1, a( 1_ilp, 2_ilp ), lda,tau, c( i1, i2 ), &
                           ldc, work, lwork, iinfo )
              end if
           end if
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_zunmbr




     pure module subroutine stdlib_sormbr( vect, side, trans, m, n, k, a, lda, tau, c,ldc, work, lwork, &
     !! If VECT = 'Q', SORMBR: overwrites the general real M-by-N matrix C
     !! with
     !! SIDE = 'L'     SIDE = 'R'
     !! TRANS = 'N':      Q * C          C * Q
     !! TRANS = 'T':      Q**T * C       C * Q**T
     !! If VECT = 'P', SORMBR overwrites the general real M-by-N matrix C
     !! with
     !! SIDE = 'L'     SIDE = 'R'
     !! TRANS = 'N':      P * C          C * P
     !! TRANS = 'T':      P**T * C       C * P**T
     !! Here Q and P**T are the orthogonal matrices determined by SGEBRD when
     !! reducing a real matrix A to bidiagonal form: A = Q * B * P**T. Q and
     !! P**T are defined as products of elementary reflectors H(i) and G(i)
     !! respectively.
     !! Let nq = m if SIDE = 'L' and nq = n if SIDE = 'R'. Thus nq is the
     !! order of the orthogonal matrix Q or P**T that is applied.
     !! If VECT = 'Q', A is assumed to have been an NQ-by-K matrix:
     !! if nq >= k, Q = H(1) H(2) . . . H(k);
     !! if nq < k, Q = H(1) H(2) . . . H(nq-1).
     !! If VECT = 'P', A is assumed to have been a K-by-NQ matrix:
     !! if k < nq, P = G(1) G(2) . . . G(k);
     !! if k >= nq, P = G(1) G(2) . . . G(nq-1).
               info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side, trans, vect
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, ldc, lwork, m, n
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*), c(ldc,*)
           real(sp), intent(in) :: tau(*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: applyq, left, lquery, notran
           character :: transt
           integer(ilp) :: i1, i2, iinfo, lwkopt, mi, nb, ni, nq, nw
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           applyq = stdlib_lsame( vect, 'Q' )
           left = stdlib_lsame( side, 'L' )
           notran = stdlib_lsame( trans, 'N' )
           lquery = ( lwork==-1_ilp )
           ! nq is the order of q or p and nw is the minimum dimension of work
           if( left ) then
              nq = m
              nw = max( 1_ilp, n )
           else
              nq = n
              nw = max( 1_ilp, m )
           end if
           if( .not.applyq .and. .not.stdlib_lsame( vect, 'P' ) ) then
              info = -1_ilp
           else if( .not.left .and. .not.stdlib_lsame( side, 'R' ) ) then
              info = -2_ilp
           else if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) ) then
              info = -3_ilp
           else if( m<0_ilp ) then
              info = -4_ilp
           else if( n<0_ilp ) then
              info = -5_ilp
           else if( k<0_ilp ) then
              info = -6_ilp
           else if( ( applyq .and. lda<max( 1_ilp, nq ) ) .or.( .not.applyq .and. lda<max( 1_ilp, min( nq,&
                      k ) ) ) )then
              info = -8_ilp
           else if( ldc<max( 1_ilp, m ) ) then
              info = -11_ilp
           else if( lwork<nw .and. .not.lquery ) then
              info = -13_ilp
           end if
           if( info==0_ilp ) then
              if( applyq ) then
                 if( left ) then
                    nb = stdlib_ilaenv( 1_ilp, 'SORMQR', side // trans, m-1, n, m-1,-1_ilp )
                 else
                    nb = stdlib_ilaenv( 1_ilp, 'SORMQR', side // trans, m, n-1, n-1,-1_ilp )
                 end if
              else
                 if( left ) then
                    nb = stdlib_ilaenv( 1_ilp, 'SORMLQ', side // trans, m-1, n, m-1,-1_ilp )
                 else
                    nb = stdlib_ilaenv( 1_ilp, 'SORMLQ', side // trans, m, n-1, n-1,-1_ilp )
                 end if
              end if
              lwkopt = nw*nb
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SORMBR', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           work( 1_ilp ) = 1_ilp
           if( m==0 .or. n==0 )return
           if( applyq ) then
              ! apply q
              if( nq>=k ) then
                 ! q was determined by a call to stdlib_sgebrd with nq >= k
                 call stdlib_sormqr( side, trans, m, n, k, a, lda, tau, c, ldc,work, lwork, iinfo &
                           )
              else if( nq>1_ilp ) then
                 ! q was determined by a call to stdlib_sgebrd with nq < k
                 if( left ) then
                    mi = m - 1_ilp
                    ni = n
                    i1 = 2_ilp
                    i2 = 1_ilp
                 else
                    mi = m
                    ni = n - 1_ilp
                    i1 = 1_ilp
                    i2 = 2_ilp
                 end if
                 call stdlib_sormqr( side, trans, mi, ni, nq-1, a( 2_ilp, 1_ilp ), lda, tau,c( i1, i2 ), &
                           ldc, work, lwork, iinfo )
              end if
           else
              ! apply p
              if( notran ) then
                 transt = 'T'
              else
                 transt = 'N'
              end if
              if( nq>k ) then
                 ! p was determined by a call to stdlib_sgebrd with nq > k
                 call stdlib_sormlq( side, transt, m, n, k, a, lda, tau, c, ldc,work, lwork, &
                           iinfo )
              else if( nq>1_ilp ) then
                 ! p was determined by a call to stdlib_sgebrd with nq <= k
                 if( left ) then
                    mi = m - 1_ilp
                    ni = n
                    i1 = 2_ilp
                    i2 = 1_ilp
                 else
                    mi = m
                    ni = n - 1_ilp
                    i1 = 1_ilp
                    i2 = 2_ilp
                 end if
                 call stdlib_sormlq( side, transt, mi, ni, nq-1, a( 1_ilp, 2_ilp ), lda,tau, c( i1, i2 ), &
                           ldc, work, lwork, iinfo )
              end if
           end if
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_sormbr

     pure module subroutine stdlib_dormbr( vect, side, trans, m, n, k, a, lda, tau, c,ldc, work, lwork, &
     !! If VECT = 'Q', DORMBR: overwrites the general real M-by-N matrix C
     !! with
     !! SIDE = 'L'     SIDE = 'R'
     !! TRANS = 'N':      Q * C          C * Q
     !! TRANS = 'T':      Q**T * C       C * Q**T
     !! If VECT = 'P', DORMBR overwrites the general real M-by-N matrix C
     !! with
     !! SIDE = 'L'     SIDE = 'R'
     !! TRANS = 'N':      P * C          C * P
     !! TRANS = 'T':      P**T * C       C * P**T
     !! Here Q and P**T are the orthogonal matrices determined by DGEBRD when
     !! reducing a real matrix A to bidiagonal form: A = Q * B * P**T. Q and
     !! P**T are defined as products of elementary reflectors H(i) and G(i)
     !! respectively.
     !! Let nq = m if SIDE = 'L' and nq = n if SIDE = 'R'. Thus nq is the
     !! order of the orthogonal matrix Q or P**T that is applied.
     !! If VECT = 'Q', A is assumed to have been an NQ-by-K matrix:
     !! if nq >= k, Q = H(1) H(2) . . . H(k);
     !! if nq < k, Q = H(1) H(2) . . . H(nq-1).
     !! If VECT = 'P', A is assumed to have been a K-by-NQ matrix:
     !! if k < nq, P = G(1) G(2) . . . G(k);
     !! if k >= nq, P = G(1) G(2) . . . G(nq-1).
               info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side, trans, vect
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, ldc, lwork, m, n
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*), c(ldc,*)
           real(dp), intent(in) :: tau(*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: applyq, left, lquery, notran
           character :: transt
           integer(ilp) :: i1, i2, iinfo, lwkopt, mi, nb, ni, nq, nw
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           applyq = stdlib_lsame( vect, 'Q' )
           left = stdlib_lsame( side, 'L' )
           notran = stdlib_lsame( trans, 'N' )
           lquery = ( lwork==-1_ilp )
           ! nq is the order of q or p and nw is the minimum dimension of work
           if( left ) then
              nq = m
              nw = max( 1_ilp, n )
           else
              nq = n
              nw = max( 1_ilp, m )
           end if
           if( .not.applyq .and. .not.stdlib_lsame( vect, 'P' ) ) then
              info = -1_ilp
           else if( .not.left .and. .not.stdlib_lsame( side, 'R' ) ) then
              info = -2_ilp
           else if( .not.notran .and. .not.stdlib_lsame( trans, 'T' ) ) then
              info = -3_ilp
           else if( m<0_ilp ) then
              info = -4_ilp
           else if( n<0_ilp ) then
              info = -5_ilp
           else if( k<0_ilp ) then
              info = -6_ilp
           else if( ( applyq .and. lda<max( 1_ilp, nq ) ) .or.( .not.applyq .and. lda<max( 1_ilp, min( nq,&
                      k ) ) ) )then
              info = -8_ilp
           else if( ldc<max( 1_ilp, m ) ) then
              info = -11_ilp
           else if( lwork<nw .and. .not.lquery ) then
              info = -13_ilp
           end if
           if( info==0_ilp ) then
              if( applyq ) then
                 if( left ) then
                    nb = stdlib_ilaenv( 1_ilp, 'DORMQR', side // trans, m-1, n, m-1,-1_ilp )
                 else
                    nb = stdlib_ilaenv( 1_ilp, 'DORMQR', side // trans, m, n-1, n-1,-1_ilp )
                 end if
              else
                 if( left ) then
                    nb = stdlib_ilaenv( 1_ilp, 'DORMLQ', side // trans, m-1, n, m-1,-1_ilp )
                 else
                    nb = stdlib_ilaenv( 1_ilp, 'DORMLQ', side // trans, m, n-1, n-1,-1_ilp )
                 end if
              end if
              lwkopt = nw*nb
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DORMBR', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           work( 1_ilp ) = 1_ilp
           if( m==0 .or. n==0 )return
           if( applyq ) then
              ! apply q
              if( nq>=k ) then
                 ! q was determined by a call to stdlib_dgebrd with nq >= k
                 call stdlib_dormqr( side, trans, m, n, k, a, lda, tau, c, ldc,work, lwork, iinfo &
                           )
              else if( nq>1_ilp ) then
                 ! q was determined by a call to stdlib_dgebrd with nq < k
                 if( left ) then
                    mi = m - 1_ilp
                    ni = n
                    i1 = 2_ilp
                    i2 = 1_ilp
                 else
                    mi = m
                    ni = n - 1_ilp
                    i1 = 1_ilp
                    i2 = 2_ilp
                 end if
                 call stdlib_dormqr( side, trans, mi, ni, nq-1, a( 2_ilp, 1_ilp ), lda, tau,c( i1, i2 ), &
                           ldc, work, lwork, iinfo )
              end if
           else
              ! apply p
              if( notran ) then
                 transt = 'T'
              else
                 transt = 'N'
              end if
              if( nq>k ) then
                 ! p was determined by a call to stdlib_dgebrd with nq > k
                 call stdlib_dormlq( side, transt, m, n, k, a, lda, tau, c, ldc,work, lwork, &
                           iinfo )
              else if( nq>1_ilp ) then
                 ! p was determined by a call to stdlib_dgebrd with nq <= k
                 if( left ) then
                    mi = m - 1_ilp
                    ni = n
                    i1 = 2_ilp
                    i2 = 1_ilp
                 else
                    mi = m
                    ni = n - 1_ilp
                    i1 = 1_ilp
                    i2 = 2_ilp
                 end if
                 call stdlib_dormlq( side, transt, mi, ni, nq-1, a( 1_ilp, 2_ilp ), lda,tau, c( i1, i2 ), &
                           ldc, work, lwork, iinfo )
              end if
           end if
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_dormbr



end submodule stdlib_lapack_svd_comp
