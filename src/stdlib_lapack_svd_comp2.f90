submodule(stdlib_lapack_eig_svd_lsq) stdlib_lapack_svd_comp2
  implicit none


  contains

     pure module subroutine stdlib_slabrd( m, n, nb, a, lda, d, e, tauq, taup, x, ldx, y,ldy )
     !! SLABRD reduces the first NB rows and columns of a real general
     !! m by n matrix A to upper or lower bidiagonal form by an orthogonal
     !! transformation Q**T * A * P, and returns the matrices X and Y which
     !! are needed to apply the transformation to the unreduced part of A.
     !! If m >= n, A is reduced to upper bidiagonal form; if m < n, to lower
     !! bidiagonal form.
     !! This is an auxiliary routine called by SGEBRD
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: lda, ldx, ldy, m, n, nb
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: d(*), e(*), taup(*), tauq(*), x(ldx,*), y(ldy,*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i
           ! Intrinsic Functions 
           ! Executable Statements 
           ! quick return if possible
           if( m<=0 .or. n<=0 )return
           if( m>=n ) then
              ! reduce to upper bidiagonal form
              loop_10: do i = 1, nb
                 ! update a(i:m,i)
                 call stdlib_sgemv( 'NO TRANSPOSE', m-i+1, i-1, -one, a( i, 1_ilp ),lda, y( i, 1_ilp ), &
                           ldy, one, a( i, i ), 1_ilp )
                 call stdlib_sgemv( 'NO TRANSPOSE', m-i+1, i-1, -one, x( i, 1_ilp ),ldx, a( 1_ilp, i ), 1_ilp,&
                            one, a( i, i ), 1_ilp )
                 ! generate reflection q(i) to annihilate a(i+1:m,i)
                 call stdlib_slarfg( m-i+1, a( i, i ), a( min( i+1, m ), i ), 1_ilp,tauq( i ) )
                           
                 d( i ) = a( i, i )
                 if( i<n ) then
                    a( i, i ) = one
                    ! compute y(i+1:n,i)
                    call stdlib_sgemv( 'TRANSPOSE', m-i+1, n-i, one, a( i, i+1 ),lda, a( i, i ), &
                              1_ilp, zero, y( i+1, i ), 1_ilp )
                    call stdlib_sgemv( 'TRANSPOSE', m-i+1, i-1, one, a( i, 1_ilp ), lda,a( i, i ), 1_ilp, &
                              zero, y( 1_ilp, i ), 1_ilp )
                    call stdlib_sgemv( 'NO TRANSPOSE', n-i, i-1, -one, y( i+1, 1_ilp ),ldy, y( 1_ilp, i ),&
                               1_ilp, one, y( i+1, i ), 1_ilp )
                    call stdlib_sgemv( 'TRANSPOSE', m-i+1, i-1, one, x( i, 1_ilp ), ldx,a( i, i ), 1_ilp, &
                              zero, y( 1_ilp, i ), 1_ilp )
                    call stdlib_sgemv( 'TRANSPOSE', i-1, n-i, -one, a( 1_ilp, i+1 ),lda, y( 1_ilp, i ), 1_ilp,&
                               one, y( i+1, i ), 1_ilp )
                    call stdlib_sscal( n-i, tauq( i ), y( i+1, i ), 1_ilp )
                    ! update a(i,i+1:n)
                    call stdlib_sgemv( 'NO TRANSPOSE', n-i, i, -one, y( i+1, 1_ilp ),ldy, a( i, 1_ilp ), &
                              lda, one, a( i, i+1 ), lda )
                    call stdlib_sgemv( 'TRANSPOSE', i-1, n-i, -one, a( 1_ilp, i+1 ),lda, x( i, 1_ilp ), &
                              ldx, one, a( i, i+1 ), lda )
                    ! generate reflection p(i) to annihilate a(i,i+2:n)
                    call stdlib_slarfg( n-i, a( i, i+1 ), a( i, min( i+2, n ) ),lda, taup( i ) )
                              
                    e( i ) = a( i, i+1 )
                    a( i, i+1 ) = one
                    ! compute x(i+1:m,i)
                    call stdlib_sgemv( 'NO TRANSPOSE', m-i, n-i, one, a( i+1, i+1 ),lda, a( i, i+&
                              1_ilp ), lda, zero, x( i+1, i ), 1_ilp )
                    call stdlib_sgemv( 'TRANSPOSE', n-i, i, one, y( i+1, 1_ilp ), ldy,a( i, i+1 ), &
                              lda, zero, x( 1_ilp, i ), 1_ilp )
                    call stdlib_sgemv( 'NO TRANSPOSE', m-i, i, -one, a( i+1, 1_ilp ),lda, x( 1_ilp, i ), &
                              1_ilp, one, x( i+1, i ), 1_ilp )
                    call stdlib_sgemv( 'NO TRANSPOSE', i-1, n-i, one, a( 1_ilp, i+1 ),lda, a( i, i+1 )&
                              , lda, zero, x( 1_ilp, i ), 1_ilp )
                    call stdlib_sgemv( 'NO TRANSPOSE', m-i, i-1, -one, x( i+1, 1_ilp ),ldx, x( 1_ilp, i ),&
                               1_ilp, one, x( i+1, i ), 1_ilp )
                    call stdlib_sscal( m-i, taup( i ), x( i+1, i ), 1_ilp )
                 end if
              end do loop_10
           else
              ! reduce to lower bidiagonal form
              loop_20: do i = 1, nb
                 ! update a(i,i:n)
                 call stdlib_sgemv( 'NO TRANSPOSE', n-i+1, i-1, -one, y( i, 1_ilp ),ldy, a( i, 1_ilp ), &
                           lda, one, a( i, i ), lda )
                 call stdlib_sgemv( 'TRANSPOSE', i-1, n-i+1, -one, a( 1_ilp, i ), lda,x( i, 1_ilp ), ldx, &
                           one, a( i, i ), lda )
                 ! generate reflection p(i) to annihilate a(i,i+1:n)
                 call stdlib_slarfg( n-i+1, a( i, i ), a( i, min( i+1, n ) ), lda,taup( i ) )
                           
                 d( i ) = a( i, i )
                 if( i<m ) then
                    a( i, i ) = one
                    ! compute x(i+1:m,i)
                    call stdlib_sgemv( 'NO TRANSPOSE', m-i, n-i+1, one, a( i+1, i ),lda, a( i, i )&
                              , lda, zero, x( i+1, i ), 1_ilp )
                    call stdlib_sgemv( 'TRANSPOSE', n-i+1, i-1, one, y( i, 1_ilp ), ldy,a( i, i ), &
                              lda, zero, x( 1_ilp, i ), 1_ilp )
                    call stdlib_sgemv( 'NO TRANSPOSE', m-i, i-1, -one, a( i+1, 1_ilp ),lda, x( 1_ilp, i ),&
                               1_ilp, one, x( i+1, i ), 1_ilp )
                    call stdlib_sgemv( 'NO TRANSPOSE', i-1, n-i+1, one, a( 1_ilp, i ),lda, a( i, i ), &
                              lda, zero, x( 1_ilp, i ), 1_ilp )
                    call stdlib_sgemv( 'NO TRANSPOSE', m-i, i-1, -one, x( i+1, 1_ilp ),ldx, x( 1_ilp, i ),&
                               1_ilp, one, x( i+1, i ), 1_ilp )
                    call stdlib_sscal( m-i, taup( i ), x( i+1, i ), 1_ilp )
                    ! update a(i+1:m,i)
                    call stdlib_sgemv( 'NO TRANSPOSE', m-i, i-1, -one, a( i+1, 1_ilp ),lda, y( i, 1_ilp ),&
                               ldy, one, a( i+1, i ), 1_ilp )
                    call stdlib_sgemv( 'NO TRANSPOSE', m-i, i, -one, x( i+1, 1_ilp ),ldx, a( 1_ilp, i ), &
                              1_ilp, one, a( i+1, i ), 1_ilp )
                    ! generate reflection q(i) to annihilate a(i+2:m,i)
                    call stdlib_slarfg( m-i, a( i+1, i ), a( min( i+2, m ), i ), 1_ilp,tauq( i ) )
                              
                    e( i ) = a( i+1, i )
                    a( i+1, i ) = one
                    ! compute y(i+1:n,i)
                    call stdlib_sgemv( 'TRANSPOSE', m-i, n-i, one, a( i+1, i+1 ),lda, a( i+1, i ),&
                               1_ilp, zero, y( i+1, i ), 1_ilp )
                    call stdlib_sgemv( 'TRANSPOSE', m-i, i-1, one, a( i+1, 1_ilp ), lda,a( i+1, i ), &
                              1_ilp, zero, y( 1_ilp, i ), 1_ilp )
                    call stdlib_sgemv( 'NO TRANSPOSE', n-i, i-1, -one, y( i+1, 1_ilp ),ldy, y( 1_ilp, i ),&
                               1_ilp, one, y( i+1, i ), 1_ilp )
                    call stdlib_sgemv( 'TRANSPOSE', m-i, i, one, x( i+1, 1_ilp ), ldx,a( i+1, i ), 1_ilp, &
                              zero, y( 1_ilp, i ), 1_ilp )
                    call stdlib_sgemv( 'TRANSPOSE', i, n-i, -one, a( 1_ilp, i+1 ), lda,y( 1_ilp, i ), 1_ilp, &
                              one, y( i+1, i ), 1_ilp )
                    call stdlib_sscal( n-i, tauq( i ), y( i+1, i ), 1_ilp )
                 end if
              end do loop_20
           end if
           return
     end subroutine stdlib_slabrd

     pure module subroutine stdlib_dlabrd( m, n, nb, a, lda, d, e, tauq, taup, x, ldx, y,ldy )
     !! DLABRD reduces the first NB rows and columns of a real general
     !! m by n matrix A to upper or lower bidiagonal form by an orthogonal
     !! transformation Q**T * A * P, and returns the matrices X and Y which
     !! are needed to apply the transformation to the unreduced part of A.
     !! If m >= n, A is reduced to upper bidiagonal form; if m < n, to lower
     !! bidiagonal form.
     !! This is an auxiliary routine called by DGEBRD
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: lda, ldx, ldy, m, n, nb
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: d(*), e(*), taup(*), tauq(*), x(ldx,*), y(ldy,*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i
           ! Intrinsic Functions 
           ! Executable Statements 
           ! quick return if possible
           if( m<=0 .or. n<=0 )return
           if( m>=n ) then
              ! reduce to upper bidiagonal form
              loop_10: do i = 1, nb
                 ! update a(i:m,i)
                 call stdlib_dgemv( 'NO TRANSPOSE', m-i+1, i-1, -one, a( i, 1_ilp ),lda, y( i, 1_ilp ), &
                           ldy, one, a( i, i ), 1_ilp )
                 call stdlib_dgemv( 'NO TRANSPOSE', m-i+1, i-1, -one, x( i, 1_ilp ),ldx, a( 1_ilp, i ), 1_ilp,&
                            one, a( i, i ), 1_ilp )
                 ! generate reflection q(i) to annihilate a(i+1:m,i)
                 call stdlib_dlarfg( m-i+1, a( i, i ), a( min( i+1, m ), i ), 1_ilp,tauq( i ) )
                           
                 d( i ) = a( i, i )
                 if( i<n ) then
                    a( i, i ) = one
                    ! compute y(i+1:n,i)
                    call stdlib_dgemv( 'TRANSPOSE', m-i+1, n-i, one, a( i, i+1 ),lda, a( i, i ), &
                              1_ilp, zero, y( i+1, i ), 1_ilp )
                    call stdlib_dgemv( 'TRANSPOSE', m-i+1, i-1, one, a( i, 1_ilp ), lda,a( i, i ), 1_ilp, &
                              zero, y( 1_ilp, i ), 1_ilp )
                    call stdlib_dgemv( 'NO TRANSPOSE', n-i, i-1, -one, y( i+1, 1_ilp ),ldy, y( 1_ilp, i ),&
                               1_ilp, one, y( i+1, i ), 1_ilp )
                    call stdlib_dgemv( 'TRANSPOSE', m-i+1, i-1, one, x( i, 1_ilp ), ldx,a( i, i ), 1_ilp, &
                              zero, y( 1_ilp, i ), 1_ilp )
                    call stdlib_dgemv( 'TRANSPOSE', i-1, n-i, -one, a( 1_ilp, i+1 ),lda, y( 1_ilp, i ), 1_ilp,&
                               one, y( i+1, i ), 1_ilp )
                    call stdlib_dscal( n-i, tauq( i ), y( i+1, i ), 1_ilp )
                    ! update a(i,i+1:n)
                    call stdlib_dgemv( 'NO TRANSPOSE', n-i, i, -one, y( i+1, 1_ilp ),ldy, a( i, 1_ilp ), &
                              lda, one, a( i, i+1 ), lda )
                    call stdlib_dgemv( 'TRANSPOSE', i-1, n-i, -one, a( 1_ilp, i+1 ),lda, x( i, 1_ilp ), &
                              ldx, one, a( i, i+1 ), lda )
                    ! generate reflection p(i) to annihilate a(i,i+2:n)
                    call stdlib_dlarfg( n-i, a( i, i+1 ), a( i, min( i+2, n ) ),lda, taup( i ) )
                              
                    e( i ) = a( i, i+1 )
                    a( i, i+1 ) = one
                    ! compute x(i+1:m,i)
                    call stdlib_dgemv( 'NO TRANSPOSE', m-i, n-i, one, a( i+1, i+1 ),lda, a( i, i+&
                              1_ilp ), lda, zero, x( i+1, i ), 1_ilp )
                    call stdlib_dgemv( 'TRANSPOSE', n-i, i, one, y( i+1, 1_ilp ), ldy,a( i, i+1 ), &
                              lda, zero, x( 1_ilp, i ), 1_ilp )
                    call stdlib_dgemv( 'NO TRANSPOSE', m-i, i, -one, a( i+1, 1_ilp ),lda, x( 1_ilp, i ), &
                              1_ilp, one, x( i+1, i ), 1_ilp )
                    call stdlib_dgemv( 'NO TRANSPOSE', i-1, n-i, one, a( 1_ilp, i+1 ),lda, a( i, i+1 )&
                              , lda, zero, x( 1_ilp, i ), 1_ilp )
                    call stdlib_dgemv( 'NO TRANSPOSE', m-i, i-1, -one, x( i+1, 1_ilp ),ldx, x( 1_ilp, i ),&
                               1_ilp, one, x( i+1, i ), 1_ilp )
                    call stdlib_dscal( m-i, taup( i ), x( i+1, i ), 1_ilp )
                 end if
              end do loop_10
           else
              ! reduce to lower bidiagonal form
              loop_20: do i = 1, nb
                 ! update a(i,i:n)
                 call stdlib_dgemv( 'NO TRANSPOSE', n-i+1, i-1, -one, y( i, 1_ilp ),ldy, a( i, 1_ilp ), &
                           lda, one, a( i, i ), lda )
                 call stdlib_dgemv( 'TRANSPOSE', i-1, n-i+1, -one, a( 1_ilp, i ), lda,x( i, 1_ilp ), ldx, &
                           one, a( i, i ), lda )
                 ! generate reflection p(i) to annihilate a(i,i+1:n)
                 call stdlib_dlarfg( n-i+1, a( i, i ), a( i, min( i+1, n ) ), lda,taup( i ) )
                           
                 d( i ) = a( i, i )
                 if( i<m ) then
                    a( i, i ) = one
                    ! compute x(i+1:m,i)
                    call stdlib_dgemv( 'NO TRANSPOSE', m-i, n-i+1, one, a( i+1, i ),lda, a( i, i )&
                              , lda, zero, x( i+1, i ), 1_ilp )
                    call stdlib_dgemv( 'TRANSPOSE', n-i+1, i-1, one, y( i, 1_ilp ), ldy,a( i, i ), &
                              lda, zero, x( 1_ilp, i ), 1_ilp )
                    call stdlib_dgemv( 'NO TRANSPOSE', m-i, i-1, -one, a( i+1, 1_ilp ),lda, x( 1_ilp, i ),&
                               1_ilp, one, x( i+1, i ), 1_ilp )
                    call stdlib_dgemv( 'NO TRANSPOSE', i-1, n-i+1, one, a( 1_ilp, i ),lda, a( i, i ), &
                              lda, zero, x( 1_ilp, i ), 1_ilp )
                    call stdlib_dgemv( 'NO TRANSPOSE', m-i, i-1, -one, x( i+1, 1_ilp ),ldx, x( 1_ilp, i ),&
                               1_ilp, one, x( i+1, i ), 1_ilp )
                    call stdlib_dscal( m-i, taup( i ), x( i+1, i ), 1_ilp )
                    ! update a(i+1:m,i)
                    call stdlib_dgemv( 'NO TRANSPOSE', m-i, i-1, -one, a( i+1, 1_ilp ),lda, y( i, 1_ilp ),&
                               ldy, one, a( i+1, i ), 1_ilp )
                    call stdlib_dgemv( 'NO TRANSPOSE', m-i, i, -one, x( i+1, 1_ilp ),ldx, a( 1_ilp, i ), &
                              1_ilp, one, a( i+1, i ), 1_ilp )
                    ! generate reflection q(i) to annihilate a(i+2:m,i)
                    call stdlib_dlarfg( m-i, a( i+1, i ), a( min( i+2, m ), i ), 1_ilp,tauq( i ) )
                              
                    e( i ) = a( i+1, i )
                    a( i+1, i ) = one
                    ! compute y(i+1:n,i)
                    call stdlib_dgemv( 'TRANSPOSE', m-i, n-i, one, a( i+1, i+1 ),lda, a( i+1, i ),&
                               1_ilp, zero, y( i+1, i ), 1_ilp )
                    call stdlib_dgemv( 'TRANSPOSE', m-i, i-1, one, a( i+1, 1_ilp ), lda,a( i+1, i ), &
                              1_ilp, zero, y( 1_ilp, i ), 1_ilp )
                    call stdlib_dgemv( 'NO TRANSPOSE', n-i, i-1, -one, y( i+1, 1_ilp ),ldy, y( 1_ilp, i ),&
                               1_ilp, one, y( i+1, i ), 1_ilp )
                    call stdlib_dgemv( 'TRANSPOSE', m-i, i, one, x( i+1, 1_ilp ), ldx,a( i+1, i ), 1_ilp, &
                              zero, y( 1_ilp, i ), 1_ilp )
                    call stdlib_dgemv( 'TRANSPOSE', i, n-i, -one, a( 1_ilp, i+1 ), lda,y( 1_ilp, i ), 1_ilp, &
                              one, y( i+1, i ), 1_ilp )
                    call stdlib_dscal( n-i, tauq( i ), y( i+1, i ), 1_ilp )
                 end if
              end do loop_20
           end if
           return
     end subroutine stdlib_dlabrd


     pure module subroutine stdlib_clabrd( m, n, nb, a, lda, d, e, tauq, taup, x, ldx, y,ldy )
     !! CLABRD reduces the first NB rows and columns of a complex general
     !! m by n matrix A to upper or lower real bidiagonal form by a unitary
     !! transformation Q**H * A * P, and returns the matrices X and Y which
     !! are needed to apply the transformation to the unreduced part of A.
     !! If m >= n, A is reduced to upper bidiagonal form; if m < n, to lower
     !! bidiagonal form.
     !! This is an auxiliary routine called by CGEBRD
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: lda, ldx, ldy, m, n, nb
           ! Array Arguments 
           real(sp), intent(out) :: d(*), e(*)
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: taup(*), tauq(*), x(ldx,*), y(ldy,*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i
           complex(sp) :: alpha
           ! Intrinsic Functions 
           ! Executable Statements 
           ! quick return if possible
           if( m<=0 .or. n<=0 )return
           if( m>=n ) then
              ! reduce to upper bidiagonal form
              loop_10: do i = 1, nb
                 ! update a(i:m,i)
                 call stdlib_clacgv( i-1, y( i, 1_ilp ), ldy )
                 call stdlib_cgemv( 'NO TRANSPOSE', m-i+1, i-1, -cone, a( i, 1_ilp ),lda, y( i, 1_ilp ), &
                           ldy, cone, a( i, i ), 1_ilp )
                 call stdlib_clacgv( i-1, y( i, 1_ilp ), ldy )
                 call stdlib_cgemv( 'NO TRANSPOSE', m-i+1, i-1, -cone, x( i, 1_ilp ),ldx, a( 1_ilp, i ), &
                           1_ilp, cone, a( i, i ), 1_ilp )
                 ! generate reflection q(i) to annihilate a(i+1:m,i)
                 alpha = a( i, i )
                 call stdlib_clarfg( m-i+1, alpha, a( min( i+1, m ), i ), 1_ilp,tauq( i ) )
                 d( i ) = real( alpha,KIND=sp)
                 if( i<n ) then
                    a( i, i ) = cone
                    ! compute y(i+1:n,i)
                    call stdlib_cgemv( 'CONJUGATE TRANSPOSE', m-i+1, n-i, cone,a( i, i+1 ), lda, &
                              a( i, i ), 1_ilp, czero,y( i+1, i ), 1_ilp )
                    call stdlib_cgemv( 'CONJUGATE TRANSPOSE', m-i+1, i-1, cone,a( i, 1_ilp ), lda, a( &
                              i, i ), 1_ilp, czero,y( 1_ilp, i ), 1_ilp )
                    call stdlib_cgemv( 'NO TRANSPOSE', n-i, i-1, -cone, y( i+1, 1_ilp ),ldy, y( 1_ilp, i )&
                              , 1_ilp, cone, y( i+1, i ), 1_ilp )
                    call stdlib_cgemv( 'CONJUGATE TRANSPOSE', m-i+1, i-1, cone,x( i, 1_ilp ), ldx, a( &
                              i, i ), 1_ilp, czero,y( 1_ilp, i ), 1_ilp )
                    call stdlib_cgemv( 'CONJUGATE TRANSPOSE', i-1, n-i, -cone,a( 1_ilp, i+1 ), lda, y(&
                               1_ilp, i ), 1_ilp, cone,y( i+1, i ), 1_ilp )
                    call stdlib_cscal( n-i, tauq( i ), y( i+1, i ), 1_ilp )
                    ! update a(i,i+1:n)
                    call stdlib_clacgv( n-i, a( i, i+1 ), lda )
                    call stdlib_clacgv( i, a( i, 1_ilp ), lda )
                    call stdlib_cgemv( 'NO TRANSPOSE', n-i, i, -cone, y( i+1, 1_ilp ),ldy, a( i, 1_ilp ), &
                              lda, cone, a( i, i+1 ), lda )
                    call stdlib_clacgv( i, a( i, 1_ilp ), lda )
                    call stdlib_clacgv( i-1, x( i, 1_ilp ), ldx )
                    call stdlib_cgemv( 'CONJUGATE TRANSPOSE', i-1, n-i, -cone,a( 1_ilp, i+1 ), lda, x(&
                               i, 1_ilp ), ldx, cone,a( i, i+1 ), lda )
                    call stdlib_clacgv( i-1, x( i, 1_ilp ), ldx )
                    ! generate reflection p(i) to annihilate a(i,i+2:n)
                    alpha = a( i, i+1 )
                    call stdlib_clarfg( n-i, alpha, a( i, min( i+2, n ) ),lda, taup( i ) )
                    e( i ) = real( alpha,KIND=sp)
                    a( i, i+1 ) = cone
                    ! compute x(i+1:m,i)
                    call stdlib_cgemv( 'NO TRANSPOSE', m-i, n-i, cone, a( i+1, i+1 ),lda, a( i, i+&
                              1_ilp ), lda, czero, x( i+1, i ), 1_ilp )
                    call stdlib_cgemv( 'CONJUGATE TRANSPOSE', n-i, i, cone,y( i+1, 1_ilp ), ldy, a( i,&
                               i+1 ), lda, czero,x( 1_ilp, i ), 1_ilp )
                    call stdlib_cgemv( 'NO TRANSPOSE', m-i, i, -cone, a( i+1, 1_ilp ),lda, x( 1_ilp, i ), &
                              1_ilp, cone, x( i+1, i ), 1_ilp )
                    call stdlib_cgemv( 'NO TRANSPOSE', i-1, n-i, cone, a( 1_ilp, i+1 ),lda, a( i, i+1 &
                              ), lda, czero, x( 1_ilp, i ), 1_ilp )
                    call stdlib_cgemv( 'NO TRANSPOSE', m-i, i-1, -cone, x( i+1, 1_ilp ),ldx, x( 1_ilp, i )&
                              , 1_ilp, cone, x( i+1, i ), 1_ilp )
                    call stdlib_cscal( m-i, taup( i ), x( i+1, i ), 1_ilp )
                    call stdlib_clacgv( n-i, a( i, i+1 ), lda )
                 end if
              end do loop_10
           else
              ! reduce to lower bidiagonal form
              loop_20: do i = 1, nb
                 ! update a(i,i:n)
                 call stdlib_clacgv( n-i+1, a( i, i ), lda )
                 call stdlib_clacgv( i-1, a( i, 1_ilp ), lda )
                 call stdlib_cgemv( 'NO TRANSPOSE', n-i+1, i-1, -cone, y( i, 1_ilp ),ldy, a( i, 1_ilp ), &
                           lda, cone, a( i, i ), lda )
                 call stdlib_clacgv( i-1, a( i, 1_ilp ), lda )
                 call stdlib_clacgv( i-1, x( i, 1_ilp ), ldx )
                 call stdlib_cgemv( 'CONJUGATE TRANSPOSE', i-1, n-i+1, -cone,a( 1_ilp, i ), lda, x( i,&
                            1_ilp ), ldx, cone, a( i, i ),lda )
                 call stdlib_clacgv( i-1, x( i, 1_ilp ), ldx )
                 ! generate reflection p(i) to annihilate a(i,i+1:n)
                 alpha = a( i, i )
                 call stdlib_clarfg( n-i+1, alpha, a( i, min( i+1, n ) ), lda,taup( i ) )
                 d( i ) = real( alpha,KIND=sp)
                 if( i<m ) then
                    a( i, i ) = cone
                    ! compute x(i+1:m,i)
                    call stdlib_cgemv( 'NO TRANSPOSE', m-i, n-i+1, cone, a( i+1, i ),lda, a( i, i &
                              ), lda, czero, x( i+1, i ), 1_ilp )
                    call stdlib_cgemv( 'CONJUGATE TRANSPOSE', n-i+1, i-1, cone,y( i, 1_ilp ), ldy, a( &
                              i, i ), lda, czero,x( 1_ilp, i ), 1_ilp )
                    call stdlib_cgemv( 'NO TRANSPOSE', m-i, i-1, -cone, a( i+1, 1_ilp ),lda, x( 1_ilp, i )&
                              , 1_ilp, cone, x( i+1, i ), 1_ilp )
                    call stdlib_cgemv( 'NO TRANSPOSE', i-1, n-i+1, cone, a( 1_ilp, i ),lda, a( i, i ),&
                               lda, czero, x( 1_ilp, i ), 1_ilp )
                    call stdlib_cgemv( 'NO TRANSPOSE', m-i, i-1, -cone, x( i+1, 1_ilp ),ldx, x( 1_ilp, i )&
                              , 1_ilp, cone, x( i+1, i ), 1_ilp )
                    call stdlib_cscal( m-i, taup( i ), x( i+1, i ), 1_ilp )
                    call stdlib_clacgv( n-i+1, a( i, i ), lda )
                    ! update a(i+1:m,i)
                    call stdlib_clacgv( i-1, y( i, 1_ilp ), ldy )
                    call stdlib_cgemv( 'NO TRANSPOSE', m-i, i-1, -cone, a( i+1, 1_ilp ),lda, y( i, 1_ilp )&
                              , ldy, cone, a( i+1, i ), 1_ilp )
                    call stdlib_clacgv( i-1, y( i, 1_ilp ), ldy )
                    call stdlib_cgemv( 'NO TRANSPOSE', m-i, i, -cone, x( i+1, 1_ilp ),ldx, a( 1_ilp, i ), &
                              1_ilp, cone, a( i+1, i ), 1_ilp )
                    ! generate reflection q(i) to annihilate a(i+2:m,i)
                    alpha = a( i+1, i )
                    call stdlib_clarfg( m-i, alpha, a( min( i+2, m ), i ), 1_ilp,tauq( i ) )
                    e( i ) = real( alpha,KIND=sp)
                    a( i+1, i ) = cone
                    ! compute y(i+1:n,i)
                    call stdlib_cgemv( 'CONJUGATE TRANSPOSE', m-i, n-i, cone,a( i+1, i+1 ), lda, &
                              a( i+1, i ), 1_ilp, czero,y( i+1, i ), 1_ilp )
                    call stdlib_cgemv( 'CONJUGATE TRANSPOSE', m-i, i-1, cone,a( i+1, 1_ilp ), lda, a( &
                              i+1, i ), 1_ilp, czero,y( 1_ilp, i ), 1_ilp )
                    call stdlib_cgemv( 'NO TRANSPOSE', n-i, i-1, -cone, y( i+1, 1_ilp ),ldy, y( 1_ilp, i )&
                              , 1_ilp, cone, y( i+1, i ), 1_ilp )
                    call stdlib_cgemv( 'CONJUGATE TRANSPOSE', m-i, i, cone,x( i+1, 1_ilp ), ldx, a( i+&
                              1_ilp, i ), 1_ilp, czero,y( 1_ilp, i ), 1_ilp )
                    call stdlib_cgemv( 'CONJUGATE TRANSPOSE', i, n-i, -cone,a( 1_ilp, i+1 ), lda, y( &
                              1_ilp, i ), 1_ilp, cone,y( i+1, i ), 1_ilp )
                    call stdlib_cscal( n-i, tauq( i ), y( i+1, i ), 1_ilp )
                 else
                    call stdlib_clacgv( n-i+1, a( i, i ), lda )
                 end if
              end do loop_20
           end if
           return
     end subroutine stdlib_clabrd

     pure module subroutine stdlib_zlabrd( m, n, nb, a, lda, d, e, tauq, taup, x, ldx, y,ldy )
     !! ZLABRD reduces the first NB rows and columns of a complex general
     !! m by n matrix A to upper or lower real bidiagonal form by a unitary
     !! transformation Q**H * A * P, and returns the matrices X and Y which
     !! are needed to apply the transformation to the unreduced part of A.
     !! If m >= n, A is reduced to upper bidiagonal form; if m < n, to lower
     !! bidiagonal form.
     !! This is an auxiliary routine called by ZGEBRD
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: lda, ldx, ldy, m, n, nb
           ! Array Arguments 
           real(dp), intent(out) :: d(*), e(*)
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: taup(*), tauq(*), x(ldx,*), y(ldy,*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i
           complex(dp) :: alpha
           ! Intrinsic Functions 
           ! Executable Statements 
           ! quick return if possible
           if( m<=0 .or. n<=0 )return
           if( m>=n ) then
              ! reduce to upper bidiagonal form
              loop_10: do i = 1, nb
                 ! update a(i:m,i)
                 call stdlib_zlacgv( i-1, y( i, 1_ilp ), ldy )
                 call stdlib_zgemv( 'NO TRANSPOSE', m-i+1, i-1, -cone, a( i, 1_ilp ),lda, y( i, 1_ilp ), &
                           ldy, cone, a( i, i ), 1_ilp )
                 call stdlib_zlacgv( i-1, y( i, 1_ilp ), ldy )
                 call stdlib_zgemv( 'NO TRANSPOSE', m-i+1, i-1, -cone, x( i, 1_ilp ),ldx, a( 1_ilp, i ), &
                           1_ilp, cone, a( i, i ), 1_ilp )
                 ! generate reflection q(i) to annihilate a(i+1:m,i)
                 alpha = a( i, i )
                 call stdlib_zlarfg( m-i+1, alpha, a( min( i+1, m ), i ), 1_ilp,tauq( i ) )
                 d( i ) = real( alpha,KIND=dp)
                 if( i<n ) then
                    a( i, i ) = cone
                    ! compute y(i+1:n,i)
                    call stdlib_zgemv( 'CONJUGATE TRANSPOSE', m-i+1, n-i, cone,a( i, i+1 ), lda, &
                              a( i, i ), 1_ilp, czero,y( i+1, i ), 1_ilp )
                    call stdlib_zgemv( 'CONJUGATE TRANSPOSE', m-i+1, i-1, cone,a( i, 1_ilp ), lda, a( &
                              i, i ), 1_ilp, czero,y( 1_ilp, i ), 1_ilp )
                    call stdlib_zgemv( 'NO TRANSPOSE', n-i, i-1, -cone, y( i+1, 1_ilp ),ldy, y( 1_ilp, i )&
                              , 1_ilp, cone, y( i+1, i ), 1_ilp )
                    call stdlib_zgemv( 'CONJUGATE TRANSPOSE', m-i+1, i-1, cone,x( i, 1_ilp ), ldx, a( &
                              i, i ), 1_ilp, czero,y( 1_ilp, i ), 1_ilp )
                    call stdlib_zgemv( 'CONJUGATE TRANSPOSE', i-1, n-i, -cone,a( 1_ilp, i+1 ), lda, y(&
                               1_ilp, i ), 1_ilp, cone,y( i+1, i ), 1_ilp )
                    call stdlib_zscal( n-i, tauq( i ), y( i+1, i ), 1_ilp )
                    ! update a(i,i+1:n)
                    call stdlib_zlacgv( n-i, a( i, i+1 ), lda )
                    call stdlib_zlacgv( i, a( i, 1_ilp ), lda )
                    call stdlib_zgemv( 'NO TRANSPOSE', n-i, i, -cone, y( i+1, 1_ilp ),ldy, a( i, 1_ilp ), &
                              lda, cone, a( i, i+1 ), lda )
                    call stdlib_zlacgv( i, a( i, 1_ilp ), lda )
                    call stdlib_zlacgv( i-1, x( i, 1_ilp ), ldx )
                    call stdlib_zgemv( 'CONJUGATE TRANSPOSE', i-1, n-i, -cone,a( 1_ilp, i+1 ), lda, x(&
                               i, 1_ilp ), ldx, cone,a( i, i+1 ), lda )
                    call stdlib_zlacgv( i-1, x( i, 1_ilp ), ldx )
                    ! generate reflection p(i) to annihilate a(i,i+2:n)
                    alpha = a( i, i+1 )
                    call stdlib_zlarfg( n-i, alpha, a( i, min( i+2, n ) ), lda,taup( i ) )
                    e( i ) = real( alpha,KIND=dp)
                    a( i, i+1 ) = cone
                    ! compute x(i+1:m,i)
                    call stdlib_zgemv( 'NO TRANSPOSE', m-i, n-i, cone, a( i+1, i+1 ),lda, a( i, i+&
                              1_ilp ), lda, czero, x( i+1, i ), 1_ilp )
                    call stdlib_zgemv( 'CONJUGATE TRANSPOSE', n-i, i, cone,y( i+1, 1_ilp ), ldy, a( i,&
                               i+1 ), lda, czero,x( 1_ilp, i ), 1_ilp )
                    call stdlib_zgemv( 'NO TRANSPOSE', m-i, i, -cone, a( i+1, 1_ilp ),lda, x( 1_ilp, i ), &
                              1_ilp, cone, x( i+1, i ), 1_ilp )
                    call stdlib_zgemv( 'NO TRANSPOSE', i-1, n-i, cone, a( 1_ilp, i+1 ),lda, a( i, i+1 &
                              ), lda, czero, x( 1_ilp, i ), 1_ilp )
                    call stdlib_zgemv( 'NO TRANSPOSE', m-i, i-1, -cone, x( i+1, 1_ilp ),ldx, x( 1_ilp, i )&
                              , 1_ilp, cone, x( i+1, i ), 1_ilp )
                    call stdlib_zscal( m-i, taup( i ), x( i+1, i ), 1_ilp )
                    call stdlib_zlacgv( n-i, a( i, i+1 ), lda )
                 end if
              end do loop_10
           else
              ! reduce to lower bidiagonal form
              loop_20: do i = 1, nb
                 ! update a(i,i:n)
                 call stdlib_zlacgv( n-i+1, a( i, i ), lda )
                 call stdlib_zlacgv( i-1, a( i, 1_ilp ), lda )
                 call stdlib_zgemv( 'NO TRANSPOSE', n-i+1, i-1, -cone, y( i, 1_ilp ),ldy, a( i, 1_ilp ), &
                           lda, cone, a( i, i ), lda )
                 call stdlib_zlacgv( i-1, a( i, 1_ilp ), lda )
                 call stdlib_zlacgv( i-1, x( i, 1_ilp ), ldx )
                 call stdlib_zgemv( 'CONJUGATE TRANSPOSE', i-1, n-i+1, -cone,a( 1_ilp, i ), lda, x( i,&
                            1_ilp ), ldx, cone, a( i, i ),lda )
                 call stdlib_zlacgv( i-1, x( i, 1_ilp ), ldx )
                 ! generate reflection p(i) to annihilate a(i,i+1:n)
                 alpha = a( i, i )
                 call stdlib_zlarfg( n-i+1, alpha, a( i, min( i+1, n ) ), lda,taup( i ) )
                 d( i ) = real( alpha,KIND=dp)
                 if( i<m ) then
                    a( i, i ) = cone
                    ! compute x(i+1:m,i)
                    call stdlib_zgemv( 'NO TRANSPOSE', m-i, n-i+1, cone, a( i+1, i ),lda, a( i, i &
                              ), lda, czero, x( i+1, i ), 1_ilp )
                    call stdlib_zgemv( 'CONJUGATE TRANSPOSE', n-i+1, i-1, cone,y( i, 1_ilp ), ldy, a( &
                              i, i ), lda, czero,x( 1_ilp, i ), 1_ilp )
                    call stdlib_zgemv( 'NO TRANSPOSE', m-i, i-1, -cone, a( i+1, 1_ilp ),lda, x( 1_ilp, i )&
                              , 1_ilp, cone, x( i+1, i ), 1_ilp )
                    call stdlib_zgemv( 'NO TRANSPOSE', i-1, n-i+1, cone, a( 1_ilp, i ),lda, a( i, i ),&
                               lda, czero, x( 1_ilp, i ), 1_ilp )
                    call stdlib_zgemv( 'NO TRANSPOSE', m-i, i-1, -cone, x( i+1, 1_ilp ),ldx, x( 1_ilp, i )&
                              , 1_ilp, cone, x( i+1, i ), 1_ilp )
                    call stdlib_zscal( m-i, taup( i ), x( i+1, i ), 1_ilp )
                    call stdlib_zlacgv( n-i+1, a( i, i ), lda )
                    ! update a(i+1:m,i)
                    call stdlib_zlacgv( i-1, y( i, 1_ilp ), ldy )
                    call stdlib_zgemv( 'NO TRANSPOSE', m-i, i-1, -cone, a( i+1, 1_ilp ),lda, y( i, 1_ilp )&
                              , ldy, cone, a( i+1, i ), 1_ilp )
                    call stdlib_zlacgv( i-1, y( i, 1_ilp ), ldy )
                    call stdlib_zgemv( 'NO TRANSPOSE', m-i, i, -cone, x( i+1, 1_ilp ),ldx, a( 1_ilp, i ), &
                              1_ilp, cone, a( i+1, i ), 1_ilp )
                    ! generate reflection q(i) to annihilate a(i+2:m,i)
                    alpha = a( i+1, i )
                    call stdlib_zlarfg( m-i, alpha, a( min( i+2, m ), i ), 1_ilp,tauq( i ) )
                    e( i ) = real( alpha,KIND=dp)
                    a( i+1, i ) = cone
                    ! compute y(i+1:n,i)
                    call stdlib_zgemv( 'CONJUGATE TRANSPOSE', m-i, n-i, cone,a( i+1, i+1 ), lda, &
                              a( i+1, i ), 1_ilp, czero,y( i+1, i ), 1_ilp )
                    call stdlib_zgemv( 'CONJUGATE TRANSPOSE', m-i, i-1, cone,a( i+1, 1_ilp ), lda, a( &
                              i+1, i ), 1_ilp, czero,y( 1_ilp, i ), 1_ilp )
                    call stdlib_zgemv( 'NO TRANSPOSE', n-i, i-1, -cone, y( i+1, 1_ilp ),ldy, y( 1_ilp, i )&
                              , 1_ilp, cone, y( i+1, i ), 1_ilp )
                    call stdlib_zgemv( 'CONJUGATE TRANSPOSE', m-i, i, cone,x( i+1, 1_ilp ), ldx, a( i+&
                              1_ilp, i ), 1_ilp, czero,y( 1_ilp, i ), 1_ilp )
                    call stdlib_zgemv( 'CONJUGATE TRANSPOSE', i, n-i, -cone,a( 1_ilp, i+1 ), lda, y( &
                              1_ilp, i ), 1_ilp, cone,y( i+1, i ), 1_ilp )
                    call stdlib_zscal( n-i, tauq( i ), y( i+1, i ), 1_ilp )
                 else
                    call stdlib_zlacgv( n-i+1, a( i, i ), lda )
                 end if
              end do loop_20
           end if
           return
     end subroutine stdlib_zlabrd




     pure module subroutine stdlib_slas2( f, g, h, ssmin, ssmax )
     !! SLAS2 computes the singular values of the 2-by-2 matrix
     !! [  F   G  ]
     !! [  0   H  ].
     !! On return, SSMIN is the smaller singular value and SSMAX is the
     !! larger singular value.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           real(sp), intent(in) :: f, g, h
           real(sp), intent(out) :: ssmax, ssmin
        ! ====================================================================
           
           
           
           ! Local Scalars 
           real(sp) :: as, at, au, c, fa, fhmn, fhmx, ga, ha
           ! Intrinsic Functions 
           ! Executable Statements 
           fa = abs( f )
           ga = abs( g )
           ha = abs( h )
           fhmn = min( fa, ha )
           fhmx = max( fa, ha )
           if( fhmn==zero ) then
              ssmin = zero
              if( fhmx==zero ) then
                 ssmax = ga
              else
                 ssmax = max( fhmx, ga )*sqrt( one+( min( fhmx, ga ) / max( fhmx, ga ) )**2_ilp )
                           
              end if
           else
              if( ga<fhmx ) then
                 as = one + fhmn / fhmx
                 at = ( fhmx-fhmn ) / fhmx
                 au = ( ga / fhmx )**2_ilp
                 c = two / ( sqrt( as*as+au )+sqrt( at*at+au ) )
                 ssmin = fhmn*c
                 ssmax = fhmx / c
              else
                 au = fhmx / ga
                 if( au==zero ) then
                    ! avoid possible harmful underflow if exponent range
                    ! asymmetric (true ssmin may not underflow even if
                    ! au underflows)
                    ssmin = ( fhmn*fhmx ) / ga
                    ssmax = ga
                 else
                    as = one + fhmn / fhmx
                    at = ( fhmx-fhmn ) / fhmx
                    c = one / ( sqrt( one+( as*au )**2_ilp )+sqrt( one+( at*au )**2_ilp ) )
                    ssmin = ( fhmn*c )*au
                    ssmin = ssmin + ssmin
                    ssmax = ga / ( c+c )
                 end if
              end if
           end if
           return
     end subroutine stdlib_slas2

     pure module subroutine stdlib_dlas2( f, g, h, ssmin, ssmax )
     !! DLAS2 computes the singular values of the 2-by-2 matrix
     !! [  F   G  ]
     !! [  0   H  ].
     !! On return, SSMIN is the smaller singular value and SSMAX is the
     !! larger singular value.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           real(dp), intent(in) :: f, g, h
           real(dp), intent(out) :: ssmax, ssmin
        ! ====================================================================
           
           
           
           ! Local Scalars 
           real(dp) :: as, at, au, c, fa, fhmn, fhmx, ga, ha
           ! Intrinsic Functions 
           ! Executable Statements 
           fa = abs( f )
           ga = abs( g )
           ha = abs( h )
           fhmn = min( fa, ha )
           fhmx = max( fa, ha )
           if( fhmn==zero ) then
              ssmin = zero
              if( fhmx==zero ) then
                 ssmax = ga
              else
                 ssmax = max( fhmx, ga )*sqrt( one+( min( fhmx, ga ) / max( fhmx, ga ) )**2_ilp )
                           
              end if
           else
              if( ga<fhmx ) then
                 as = one + fhmn / fhmx
                 at = ( fhmx-fhmn ) / fhmx
                 au = ( ga / fhmx )**2_ilp
                 c = two / ( sqrt( as*as+au )+sqrt( at*at+au ) )
                 ssmin = fhmn*c
                 ssmax = fhmx / c
              else
                 au = fhmx / ga
                 if( au==zero ) then
                    ! avoid possible harmful underflow if exponent range
                    ! asymmetric (true ssmin may not underflow even if
                    ! au underflows)
                    ssmin = ( fhmn*fhmx ) / ga
                    ssmax = ga
                 else
                    as = one + fhmn / fhmx
                    at = ( fhmx-fhmn ) / fhmx
                    c = one / ( sqrt( one+( as*au )**2_ilp )+sqrt( one+( at*au )**2_ilp ) )
                    ssmin = ( fhmn*c )*au
                    ssmin = ssmin + ssmin
                    ssmax = ga / ( c+c )
                 end if
              end if
           end if
           return
     end subroutine stdlib_dlas2




     pure module subroutine stdlib_slasv2( f, g, h, ssmin, ssmax, snr, csr, snl, csl )
     !! SLASV2 computes the singular value decomposition of a 2-by-2
     !! triangular matrix
     !! [  F   G  ]
     !! [  0   H  ].
     !! On return, abs(SSMAX) is the larger singular value, abs(SSMIN) is the
     !! smaller singular value, and (CSL,SNL) and (CSR,SNR) are the left and
     !! right singular vectors for abs(SSMAX), giving the decomposition
     !! [ CSL  SNL ] [  F   G  ] [ CSR -SNR ]  =  [ SSMAX   0   ]
     !! [-SNL  CSL ] [  0   H  ] [ SNR  CSR ]     [  0    SSMIN ].
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           real(sp), intent(out) :: csl, csr, snl, snr, ssmax, ssmin
           real(sp), intent(in) :: f, g, h
       ! =====================================================================
           
           
           
           
           
           ! Local Scalars 
           logical(lk) :: gasmal, swap
           integer(ilp) :: pmax
           real(sp) :: a, clt, crt, d, fa, ft, ga, gt, ha, ht, l, m, mm, r, s, slt, srt, t, temp, &
                     tsign, tt
           ! Intrinsic Functions 
           ! Executable Statements 
           ft = f
           fa = abs( ft )
           ht = h
           ha = abs( h )
           ! pmax points to the maximum absolute element of matrix
             ! pmax = 1 if f largest in absolute values
             ! pmax = 2 if g largest in absolute values
             ! pmax = 3 if h largest in absolute values
           pmax = 1_ilp
           swap = ( ha>fa )
           if( swap ) then
              pmax = 3_ilp
              temp = ft
              ft = ht
              ht = temp
              temp = fa
              fa = ha
              ha = temp
              ! now fa .ge. ha
           end if
           gt = g
           ga = abs( gt )
           if( ga==zero ) then
              ! diagonal matrix
              ssmin = ha
              ssmax = fa
              clt = one
              crt = one
              slt = zero
              srt = zero
           else
              gasmal = .true.
              if( ga>fa ) then
                 pmax = 2_ilp
                 if( ( fa / ga )<stdlib_slamch( 'EPS' ) ) then
                    ! case of very large ga
                    gasmal = .false.
                    ssmax = ga
                    if( ha>one ) then
                       ssmin = fa / ( ga / ha )
                    else
                       ssmin = ( fa / ga )*ha
                    end if
                    clt = one
                    slt = ht / gt
                    srt = one
                    crt = ft / gt
                 end if
              end if
              if( gasmal ) then
                 ! normal case
                 d = fa - ha
                 if( d==fa ) then
                    ! copes with infinite f or h
                    l = one
                 else
                    l = d / fa
                 end if
                 ! note that 0 .le. l .le. 1
                 m = gt / ft
                 ! note that abs(m) .le. 1/macheps
                 t = two - l
                 ! note that t .ge. 1
                 mm = m*m
                 tt = t*t
                 s = sqrt( tt+mm )
                 ! note that 1 .le. s .le. 1 + 1/macheps
                 if( l==zero ) then
                    r = abs( m )
                 else
                    r = sqrt( l*l+mm )
                 end if
                 ! note that 0 .le. r .le. 1 + 1/macheps
                 a = half*( s+r )
                 ! note that 1 .le. a .le. 1 + abs(m)
                 ssmin = ha / a
                 ssmax = fa*a
                 if( mm==zero ) then
                    ! note that m is very tiny
                    if( l==zero ) then
                       t = sign( two, ft )*sign( one, gt )
                    else
                       t = gt / sign( d, ft ) + m / t
                    end if
                 else
                    t = ( m / ( s+t )+m / ( r+l ) )*( one+a )
                 end if
                 l = sqrt( t*t+four )
                 crt = two / l
                 srt = t / l
                 clt = ( crt+srt*m ) / a
                 slt = ( ht / ft )*srt / a
              end if
           end if
           if( swap ) then
              csl = srt
              snl = crt
              csr = slt
              snr = clt
           else
              csl = clt
              snl = slt
              csr = crt
              snr = srt
           end if
           ! correct signs of ssmax and ssmin
           if( pmax==1_ilp )tsign = sign( one, csr )*sign( one, csl )*sign( one, f )
           if( pmax==2_ilp )tsign = sign( one, snr )*sign( one, csl )*sign( one, g )
           if( pmax==3_ilp )tsign = sign( one, snr )*sign( one, snl )*sign( one, h )
           ssmax = sign( ssmax, tsign )
           ssmin = sign( ssmin, tsign*sign( one, f )*sign( one, h ) )
           return
     end subroutine stdlib_slasv2

     pure module subroutine stdlib_dlasv2( f, g, h, ssmin, ssmax, snr, csr, snl, csl )
     !! DLASV2 computes the singular value decomposition of a 2-by-2
     !! triangular matrix
     !! [  F   G  ]
     !! [  0   H  ].
     !! On return, abs(SSMAX) is the larger singular value, abs(SSMIN) is the
     !! smaller singular value, and (CSL,SNL) and (CSR,SNR) are the left and
     !! right singular vectors for abs(SSMAX), giving the decomposition
     !! [ CSL  SNL ] [  F   G  ] [ CSR -SNR ]  =  [ SSMAX   0   ]
     !! [-SNL  CSL ] [  0   H  ] [ SNR  CSR ]     [  0    SSMIN ].
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           real(dp), intent(out) :: csl, csr, snl, snr, ssmax, ssmin
           real(dp), intent(in) :: f, g, h
       ! =====================================================================
           
           
           
           
           
           ! Local Scalars 
           logical(lk) :: gasmal, swap
           integer(ilp) :: pmax
           real(dp) :: a, clt, crt, d, fa, ft, ga, gt, ha, ht, l, m, mm, r, s, slt, srt, t, temp, &
                     tsign, tt
           ! Intrinsic Functions 
           ! Executable Statements 
           ft = f
           fa = abs( ft )
           ht = h
           ha = abs( h )
           ! pmax points to the maximum absolute element of matrix
             ! pmax = 1 if f largest in absolute values
             ! pmax = 2 if g largest in absolute values
             ! pmax = 3 if h largest in absolute values
           pmax = 1_ilp
           swap = ( ha>fa )
           if( swap ) then
              pmax = 3_ilp
              temp = ft
              ft = ht
              ht = temp
              temp = fa
              fa = ha
              ha = temp
              ! now fa .ge. ha
           end if
           gt = g
           ga = abs( gt )
           if( ga==zero ) then
              ! diagonal matrix
              ssmin = ha
              ssmax = fa
              clt = one
              crt = one
              slt = zero
              srt = zero
           else
              gasmal = .true.
              if( ga>fa ) then
                 pmax = 2_ilp
                 if( ( fa / ga )<stdlib_dlamch( 'EPS' ) ) then
                    ! case of very large ga
                    gasmal = .false.
                    ssmax = ga
                    if( ha>one ) then
                       ssmin = fa / ( ga / ha )
                    else
                       ssmin = ( fa / ga )*ha
                    end if
                    clt = one
                    slt = ht / gt
                    srt = one
                    crt = ft / gt
                 end if
              end if
              if( gasmal ) then
                 ! normal case
                 d = fa - ha
                 if( d==fa ) then
                    ! copes with infinite f or h
                    l = one
                 else
                    l = d / fa
                 end if
                 ! note that 0 .le. l .le. 1
                 m = gt / ft
                 ! note that abs(m) .le. 1/macheps
                 t = two - l
                 ! note that t .ge. 1
                 mm = m*m
                 tt = t*t
                 s = sqrt( tt+mm )
                 ! note that 1 .le. s .le. 1 + 1/macheps
                 if( l==zero ) then
                    r = abs( m )
                 else
                    r = sqrt( l*l+mm )
                 end if
                 ! note that 0 .le. r .le. 1 + 1/macheps
                 a = half*( s+r )
                 ! note that 1 .le. a .le. 1 + abs(m)
                 ssmin = ha / a
                 ssmax = fa*a
                 if( mm==zero ) then
                    ! note that m is very tiny
                    if( l==zero ) then
                       t = sign( two, ft )*sign( one, gt )
                    else
                       t = gt / sign( d, ft ) + m / t
                    end if
                 else
                    t = ( m / ( s+t )+m / ( r+l ) )*( one+a )
                 end if
                 l = sqrt( t*t+four )
                 crt = two / l
                 srt = t / l
                 clt = ( crt+srt*m ) / a
                 slt = ( ht / ft )*srt / a
              end if
           end if
           if( swap ) then
              csl = srt
              snl = crt
              csr = slt
              snr = clt
           else
              csl = clt
              snl = slt
              csr = crt
              snr = srt
           end if
           ! correct signs of ssmax and ssmin
           if( pmax==1_ilp )tsign = sign( one, csr )*sign( one, csl )*sign( one, f )
           if( pmax==2_ilp )tsign = sign( one, snr )*sign( one, csl )*sign( one, g )
           if( pmax==3_ilp )tsign = sign( one, snr )*sign( one, snl )*sign( one, h )
           ssmax = sign( ssmax, tsign )
           ssmin = sign( ssmin, tsign*sign( one, f )*sign( one, h ) )
           return
     end subroutine stdlib_dlasv2




     pure module subroutine stdlib_slartgs( x, y, sigma, cs, sn )
     !! SLARTGS generates a plane rotation designed to introduce a bulge in
     !! Golub-Reinsch-style implicit QR iteration for the bidiagonal SVD
     !! problem. X and Y are the top-row entries, and SIGMA is the shift.
     !! The computed CS and SN define a plane rotation satisfying
     !! [  CS  SN  ]  .  [ X^2 - SIGMA ]  =  [ R ],
     !! [ -SN  CS  ]     [    X * Y    ]     [ 0 ]
     !! with R nonnegative.  If X^2 - SIGMA and X * Y are 0, then the
     !! rotation is by PI/2.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           real(sp), intent(out) :: cs, sn
           real(sp), intent(in) :: sigma, x, y
        ! ===================================================================
           
           ! Local Scalars 
           real(sp) :: r, s, thresh, w, z
           thresh = stdlib_slamch('E')
           ! compute the first column of b**t*b - sigma^2*i, up to a scale
           ! factor.
           if( (sigma == zero .and. abs(x) < thresh) .or.(abs(x) == sigma .and. y == zero) ) &
                     then
              z = zero
              w = zero
           else if( sigma == zero ) then
              if( x >= zero ) then
                 z = x
                 w = y
              else
                 z = -x
                 w = -y
              end if
           else if( abs(x) < thresh ) then
              z = -sigma*sigma
              w = zero
           else
              if( x >= zero ) then
                 s = one
              else
                 s = negone
              end if
              z = s * (abs(x)-sigma) * (s+sigma/x)
              w = s * y
           end if
           ! generate the rotation.
           ! call stdlib_slartgp( z, w, cs, sn, r ) might seem more natural;
           ! reordering the arguments ensures that if z = 0 then the rotation
           ! is by pi/2.
           call stdlib_slartgp( w, z, sn, cs, r )
           return
           ! end stdlib_slartgs
     end subroutine stdlib_slartgs

     pure module subroutine stdlib_dlartgs( x, y, sigma, cs, sn )
     !! DLARTGS generates a plane rotation designed to introduce a bulge in
     !! Golub-Reinsch-style implicit QR iteration for the bidiagonal SVD
     !! problem. X and Y are the top-row entries, and SIGMA is the shift.
     !! The computed CS and SN define a plane rotation satisfying
     !! [  CS  SN  ]  .  [ X^2 - SIGMA ]  =  [ R ],
     !! [ -SN  CS  ]     [    X * Y    ]     [ 0 ]
     !! with R nonnegative.  If X^2 - SIGMA and X * Y are 0, then the
     !! rotation is by PI/2.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           real(dp), intent(out) :: cs, sn
           real(dp), intent(in) :: sigma, x, y
        ! ===================================================================
           
           ! Local Scalars 
           real(dp) :: r, s, thresh, w, z
           thresh = stdlib_dlamch('E')
           ! compute the first column of b**t*b - sigma^2*i, up to a scale
           ! factor.
           if( (sigma == zero .and. abs(x) < thresh) .or.(abs(x) == sigma .and. y == zero) ) &
                     then
              z = zero
              w = zero
           else if( sigma == zero ) then
              if( x >= zero ) then
                 z = x
                 w = y
              else
                 z = -x
                 w = -y
              end if
           else if( abs(x) < thresh ) then
              z = -sigma*sigma
              w = zero
           else
              if( x >= zero ) then
                 s = one
              else
                 s = negone
              end if
              z = s * (abs(x)-sigma) * (s+sigma/x)
              w = s * y
           end if
           ! generate the rotation.
           ! call stdlib_dlartgp( z, w, cs, sn, r ) might seem more natural;
           ! reordering the arguments ensures that if z = 0 then the rotation
           ! is by pi/2.
           call stdlib_dlartgp( w, z, sn, cs, r )
           return
           ! end stdlib_dlartgs
     end subroutine stdlib_dlartgs




     pure module subroutine stdlib_slags2( upper, a1, a2, a3, b1, b2, b3, csu, snu, csv,snv, csq, snq )
     !! SLAGS2 computes 2-by-2 orthogonal matrices U, V and Q, such
     !! that if ( UPPER ) then
     !! U**T *A*Q = U**T *( A1 A2 )*Q = ( x  0  )
     !! ( 0  A3 )     ( x  x  )
     !! and
     !! V**T*B*Q = V**T *( B1 B2 )*Q = ( x  0  )
     !! ( 0  B3 )     ( x  x  )
     !! or if ( .NOT.UPPER ) then
     !! U**T *A*Q = U**T *( A1 0  )*Q = ( x  x  )
     !! ( A2 A3 )     ( 0  x  )
     !! and
     !! V**T*B*Q = V**T*( B1 0  )*Q = ( x  x  )
     !! ( B2 B3 )     ( 0  x  )
     !! The rows of the transformed A and B are parallel, where
     !! U = (  CSU  SNU ), V = (  CSV SNV ), Q = (  CSQ   SNQ )
     !! ( -SNU  CSU )      ( -SNV CSV )      ( -SNQ   CSQ )
     !! Z**T denotes the transpose of Z.
               
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           logical(lk), intent(in) :: upper
           real(sp), intent(in) :: a1, a2, a3, b1, b2, b3
           real(sp), intent(out) :: csq, csu, csv, snq, snu, snv
        ! =====================================================================
           
           ! Local Scalars 
           real(sp) :: a, aua11, aua12, aua21, aua22, avb11, avb12, avb21, avb22, csl, csr, d, s1,&
            s2, snl, snr, ua11r, ua22r, vb11r, vb22r, b, c, r, ua11, ua12, ua21, ua22, vb11, vb12,&
                       vb21, vb22
           ! Intrinsic Functions 
           ! Executable Statements 
           if( upper ) then
              ! input matrices a and b are upper triangular matrices
              ! form matrix c = a*adj(b) = ( a b )
                                         ! ( 0 d )
              a = a1*b3
              d = a3*b1
              b = a2*b1 - a1*b2
              ! the svd of real 2-by-2 triangular c
               ! ( csl -snl )*( a b )*(  csr  snr ) = ( r 0 )
               ! ( snl  csl ) ( 0 d ) ( -snr  csr )   ( 0 t )
              call stdlib_slasv2( a, b, d, s1, s2, snr, csr, snl, csl )
              if( abs( csl )>=abs( snl ) .or. abs( csr )>=abs( snr ) )then
                 ! compute the (1,1) and (1,2) elements of u**t *a and v**t *b,
                 ! and (1,2) element of |u|**t *|a| and |v|**t *|b|.
                 ua11r = csl*a1
                 ua12 = csl*a2 + snl*a3
                 vb11r = csr*b1
                 vb12 = csr*b2 + snr*b3
                 aua12 = abs( csl )*abs( a2 ) + abs( snl )*abs( a3 )
                 avb12 = abs( csr )*abs( b2 ) + abs( snr )*abs( b3 )
                 ! zero (1,2) elements of u**t *a and v**t *b
                 if( ( abs( ua11r )+abs( ua12 ) )/=zero ) then
                    if( aua12 / ( abs( ua11r )+abs( ua12 ) )<=avb12 /( abs( vb11r )+abs( vb12 ) ) &
                              ) then
                       call stdlib_slartg( -ua11r, ua12, csq, snq, r )
                    else
                       call stdlib_slartg( -vb11r, vb12, csq, snq, r )
                    end if
                 else
                    call stdlib_slartg( -vb11r, vb12, csq, snq, r )
                 end if
                 csu = csl
                 snu = -snl
                 csv = csr
                 snv = -snr
              else
                 ! compute the (2,1) and (2,2) elements of u**t *a and v**t *b,
                 ! and (2,2) element of |u|**t *|a| and |v|**t *|b|.
                 ua21 = -snl*a1
                 ua22 = -snl*a2 + csl*a3
                 vb21 = -snr*b1
                 vb22 = -snr*b2 + csr*b3
                 aua22 = abs( snl )*abs( a2 ) + abs( csl )*abs( a3 )
                 avb22 = abs( snr )*abs( b2 ) + abs( csr )*abs( b3 )
                 ! zero (2,2) elements of u**t*a and v**t*b, and then swap.
                 if( ( abs( ua21 )+abs( ua22 ) )/=zero ) then
                    if( aua22 / ( abs( ua21 )+abs( ua22 ) )<=avb22 /( abs( vb21 )+abs( vb22 ) ) ) &
                              then
                       call stdlib_slartg( -ua21, ua22, csq, snq, r )
                    else
                       call stdlib_slartg( -vb21, vb22, csq, snq, r )
                    end if
                 else
                    call stdlib_slartg( -vb21, vb22, csq, snq, r )
                 end if
                 csu = snl
                 snu = csl
                 csv = snr
                 snv = csr
              end if
           else
              ! input matrices a and b are lower triangular matrices
              ! form matrix c = a*adj(b) = ( a 0 )
                                         ! ( c d )
              a = a1*b3
              d = a3*b1
              c = a2*b3 - a3*b2
              ! the svd of real 2-by-2 triangular c
               ! ( csl -snl )*( a 0 )*(  csr  snr ) = ( r 0 )
               ! ( snl  csl ) ( c d ) ( -snr  csr )   ( 0 t )
              call stdlib_slasv2( a, c, d, s1, s2, snr, csr, snl, csl )
              if( abs( csr )>=abs( snr ) .or. abs( csl )>=abs( snl ) )then
                 ! compute the (2,1) and (2,2) elements of u**t *a and v**t *b,
                 ! and (2,1) element of |u|**t *|a| and |v|**t *|b|.
                 ua21 = -snr*a1 + csr*a2
                 ua22r = csr*a3
                 vb21 = -snl*b1 + csl*b2
                 vb22r = csl*b3
                 aua21 = abs( snr )*abs( a1 ) + abs( csr )*abs( a2 )
                 avb21 = abs( snl )*abs( b1 ) + abs( csl )*abs( b2 )
                 ! zero (2,1) elements of u**t *a and v**t *b.
                 if( ( abs( ua21 )+abs( ua22r ) )/=zero ) then
                    if( aua21 / ( abs( ua21 )+abs( ua22r ) )<=avb21 /( abs( vb21 )+abs( vb22r ) ) &
                              ) then
                       call stdlib_slartg( ua22r, ua21, csq, snq, r )
                    else
                       call stdlib_slartg( vb22r, vb21, csq, snq, r )
                    end if
                 else
                    call stdlib_slartg( vb22r, vb21, csq, snq, r )
                 end if
                 csu = csr
                 snu = -snr
                 csv = csl
                 snv = -snl
              else
                 ! compute the (1,1) and (1,2) elements of u**t *a and v**t *b,
                 ! and (1,1) element of |u|**t *|a| and |v|**t *|b|.
                 ua11 = csr*a1 + snr*a2
                 ua12 = snr*a3
                 vb11 = csl*b1 + snl*b2
                 vb12 = snl*b3
                 aua11 = abs( csr )*abs( a1 ) + abs( snr )*abs( a2 )
                 avb11 = abs( csl )*abs( b1 ) + abs( snl )*abs( b2 )
                 ! zero (1,1) elements of u**t*a and v**t*b, and then swap.
                 if( ( abs( ua11 )+abs( ua12 ) )/=zero ) then
                    if( aua11 / ( abs( ua11 )+abs( ua12 ) )<=avb11 /( abs( vb11 )+abs( vb12 ) ) ) &
                              then
                       call stdlib_slartg( ua12, ua11, csq, snq, r )
                    else
                       call stdlib_slartg( vb12, vb11, csq, snq, r )
                    end if
                 else
                    call stdlib_slartg( vb12, vb11, csq, snq, r )
                 end if
                 csu = snr
                 snu = csr
                 csv = snl
                 snv = csl
              end if
           end if
           return
     end subroutine stdlib_slags2

     pure module subroutine stdlib_dlags2( upper, a1, a2, a3, b1, b2, b3, csu, snu, csv,snv, csq, snq )
     !! DLAGS2 computes 2-by-2 orthogonal matrices U, V and Q, such
     !! that if ( UPPER ) then
     !! U**T *A*Q = U**T *( A1 A2 )*Q = ( x  0  )
     !! ( 0  A3 )     ( x  x  )
     !! and
     !! V**T*B*Q = V**T *( B1 B2 )*Q = ( x  0  )
     !! ( 0  B3 )     ( x  x  )
     !! or if ( .NOT.UPPER ) then
     !! U**T *A*Q = U**T *( A1 0  )*Q = ( x  x  )
     !! ( A2 A3 )     ( 0  x  )
     !! and
     !! V**T*B*Q = V**T*( B1 0  )*Q = ( x  x  )
     !! ( B2 B3 )     ( 0  x  )
     !! The rows of the transformed A and B are parallel, where
     !! U = (  CSU  SNU ), V = (  CSV SNV ), Q = (  CSQ   SNQ )
     !! ( -SNU  CSU )      ( -SNV CSV )      ( -SNQ   CSQ )
     !! Z**T denotes the transpose of Z.
               
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           logical(lk), intent(in) :: upper
           real(dp), intent(in) :: a1, a2, a3, b1, b2, b3
           real(dp), intent(out) :: csq, csu, csv, snq, snu, snv
        ! =====================================================================
           
           ! Local Scalars 
           real(dp) :: a, aua11, aua12, aua21, aua22, avb11, avb12, avb21, avb22, b, c, csl, csr, &
           d, r, s1, s2, snl, snr, ua11, ua11r, ua12, ua21, ua22, ua22r, vb11, vb11r, vb12, vb21, &
                     vb22, vb22r
           ! Intrinsic Functions 
           ! Executable Statements 
           if( upper ) then
              ! input matrices a and b are upper triangular matrices
              ! form matrix c = a*adj(b) = ( a b )
                                         ! ( 0 d )
              a = a1*b3
              d = a3*b1
              b = a2*b1 - a1*b2
              ! the svd of real 2-by-2 triangular c
               ! ( csl -snl )*( a b )*(  csr  snr ) = ( r 0 )
               ! ( snl  csl ) ( 0 d ) ( -snr  csr )   ( 0 t )
              call stdlib_dlasv2( a, b, d, s1, s2, snr, csr, snl, csl )
              if( abs( csl )>=abs( snl ) .or. abs( csr )>=abs( snr ) )then
                 ! compute the (1,1) and (1,2) elements of u**t *a and v**t *b,
                 ! and (1,2) element of |u|**t *|a| and |v|**t *|b|.
                 ua11r = csl*a1
                 ua12 = csl*a2 + snl*a3
                 vb11r = csr*b1
                 vb12 = csr*b2 + snr*b3
                 aua12 = abs( csl )*abs( a2 ) + abs( snl )*abs( a3 )
                 avb12 = abs( csr )*abs( b2 ) + abs( snr )*abs( b3 )
                 ! zero (1,2) elements of u**t *a and v**t *b
                 if( ( abs( ua11r )+abs( ua12 ) )/=zero ) then
                    if( aua12 / ( abs( ua11r )+abs( ua12 ) )<=avb12 /( abs( vb11r )+abs( vb12 ) ) &
                              ) then
                       call stdlib_dlartg( -ua11r, ua12, csq, snq, r )
                    else
                       call stdlib_dlartg( -vb11r, vb12, csq, snq, r )
                    end if
                 else
                    call stdlib_dlartg( -vb11r, vb12, csq, snq, r )
                 end if
                 csu = csl
                 snu = -snl
                 csv = csr
                 snv = -snr
              else
                 ! compute the (2,1) and (2,2) elements of u**t *a and v**t *b,
                 ! and (2,2) element of |u|**t *|a| and |v|**t *|b|.
                 ua21 = -snl*a1
                 ua22 = -snl*a2 + csl*a3
                 vb21 = -snr*b1
                 vb22 = -snr*b2 + csr*b3
                 aua22 = abs( snl )*abs( a2 ) + abs( csl )*abs( a3 )
                 avb22 = abs( snr )*abs( b2 ) + abs( csr )*abs( b3 )
                 ! zero (2,2) elements of u**t*a and v**t*b, and then swap.
                 if( ( abs( ua21 )+abs( ua22 ) )/=zero ) then
                    if( aua22 / ( abs( ua21 )+abs( ua22 ) )<=avb22 /( abs( vb21 )+abs( vb22 ) ) ) &
                              then
                       call stdlib_dlartg( -ua21, ua22, csq, snq, r )
                    else
                       call stdlib_dlartg( -vb21, vb22, csq, snq, r )
                    end if
                 else
                    call stdlib_dlartg( -vb21, vb22, csq, snq, r )
                 end if
                 csu = snl
                 snu = csl
                 csv = snr
                 snv = csr
              end if
           else
              ! input matrices a and b are lower triangular matrices
              ! form matrix c = a*adj(b) = ( a 0 )
                                         ! ( c d )
              a = a1*b3
              d = a3*b1
              c = a2*b3 - a3*b2
              ! the svd of real 2-by-2 triangular c
               ! ( csl -snl )*( a 0 )*(  csr  snr ) = ( r 0 )
               ! ( snl  csl ) ( c d ) ( -snr  csr )   ( 0 t )
              call stdlib_dlasv2( a, c, d, s1, s2, snr, csr, snl, csl )
              if( abs( csr )>=abs( snr ) .or. abs( csl )>=abs( snl ) )then
                 ! compute the (2,1) and (2,2) elements of u**t *a and v**t *b,
                 ! and (2,1) element of |u|**t *|a| and |v|**t *|b|.
                 ua21 = -snr*a1 + csr*a2
                 ua22r = csr*a3
                 vb21 = -snl*b1 + csl*b2
                 vb22r = csl*b3
                 aua21 = abs( snr )*abs( a1 ) + abs( csr )*abs( a2 )
                 avb21 = abs( snl )*abs( b1 ) + abs( csl )*abs( b2 )
                 ! zero (2,1) elements of u**t *a and v**t *b.
                 if( ( abs( ua21 )+abs( ua22r ) )/=zero ) then
                    if( aua21 / ( abs( ua21 )+abs( ua22r ) )<=avb21 /( abs( vb21 )+abs( vb22r ) ) &
                              ) then
                       call stdlib_dlartg( ua22r, ua21, csq, snq, r )
                    else
                       call stdlib_dlartg( vb22r, vb21, csq, snq, r )
                    end if
                 else
                    call stdlib_dlartg( vb22r, vb21, csq, snq, r )
                 end if
                 csu = csr
                 snu = -snr
                 csv = csl
                 snv = -snl
              else
                 ! compute the (1,1) and (1,2) elements of u**t *a and v**t *b,
                 ! and (1,1) element of |u|**t *|a| and |v|**t *|b|.
                 ua11 = csr*a1 + snr*a2
                 ua12 = snr*a3
                 vb11 = csl*b1 + snl*b2
                 vb12 = snl*b3
                 aua11 = abs( csr )*abs( a1 ) + abs( snr )*abs( a2 )
                 avb11 = abs( csl )*abs( b1 ) + abs( snl )*abs( b2 )
                 ! zero (1,1) elements of u**t*a and v**t*b, and then swap.
                 if( ( abs( ua11 )+abs( ua12 ) )/=zero ) then
                    if( aua11 / ( abs( ua11 )+abs( ua12 ) )<=avb11 /( abs( vb11 )+abs( vb12 ) ) ) &
                              then
                       call stdlib_dlartg( ua12, ua11, csq, snq, r )
                    else
                       call stdlib_dlartg( vb12, vb11, csq, snq, r )
                    end if
                 else
                    call stdlib_dlartg( vb12, vb11, csq, snq, r )
                 end if
                 csu = snr
                 snu = csr
                 csv = snl
                 snv = csl
              end if
           end if
           return
     end subroutine stdlib_dlags2


     pure module subroutine stdlib_clags2( upper, a1, a2, a3, b1, b2, b3, csu, snu, csv,snv, csq, snq )
     !! CLAGS2 computes 2-by-2 unitary matrices U, V and Q, such
     !! that if ( UPPER ) then
     !! U**H *A*Q = U**H *( A1 A2 )*Q = ( x  0  )
     !! ( 0  A3 )     ( x  x  )
     !! and
     !! V**H*B*Q = V**H *( B1 B2 )*Q = ( x  0  )
     !! ( 0  B3 )     ( x  x  )
     !! or if ( .NOT.UPPER ) then
     !! U**H *A*Q = U**H *( A1 0  )*Q = ( x  x  )
     !! ( A2 A3 )     ( 0  x  )
     !! and
     !! V**H *B*Q = V**H *( B1 0  )*Q = ( x  x  )
     !! ( B2 B3 )     ( 0  x  )
     !! where
     !! U = (   CSU    SNU ), V = (  CSV    SNV ),
     !! ( -SNU**H  CSU )      ( -SNV**H CSV )
     !! Q = (   CSQ    SNQ )
     !! ( -SNQ**H  CSQ )
     !! The rows of the transformed A and B are parallel. Moreover, if the
     !! input 2-by-2 matrix A is not zero, then the transformed (1,1) entry
     !! of A is not zero. If the input matrices A and B are both not zero,
     !! then the transformed (2,2) element of B is not zero, except when the
     !! first rows of input A and B are parallel and the second rows are
     !! zero.
               
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           logical(lk), intent(in) :: upper
           real(sp), intent(in) :: a1, a3, b1, b3
           real(sp), intent(out) :: csq, csu, csv
           complex(sp), intent(in) :: a2, b2
           complex(sp), intent(out) :: snq, snu, snv
        ! =====================================================================
           
           ! Local Scalars 
           real(sp) :: a, aua11, aua12, aua21, aua22, avb11, avb12, avb21, avb22, csl, csr, d, fb,&
                      fc, s1, s2, snl, snr, ua11r, ua22r, vb11r, vb22r
           complex(sp) :: b, c, d1, r, t, ua11, ua12, ua21, ua22, vb11, vb12, vb21, vb22
           ! Intrinsic Functions 
           ! Statement Functions 
           real(sp) :: abs1
           ! Statement Function Definitions 
           abs1( t ) = abs( real( t,KIND=sp) ) + abs( aimag( t ) )
           ! Executable Statements 
           if( upper ) then
              ! input matrices a and b are upper triangular matrices
              ! form matrix c = a*adj(b) = ( a b )
                                         ! ( 0 d )
              a = a1*b3
              d = a3*b1
              b = a2*b1 - a1*b2
              fb = abs( b )
              ! transform complex 2-by-2 matrix c to real matrix by unitary
              ! diagonal matrix diag(1,d1).
              d1 = one
              if( fb/=zero )d1 = b / fb
              ! the svd of real 2 by 2 triangular c
               ! ( csl -snl )*( a b )*(  csr  snr ) = ( r 0 )
               ! ( snl  csl ) ( 0 d ) ( -snr  csr )   ( 0 t )
              call stdlib_slasv2( a, fb, d, s1, s2, snr, csr, snl, csl )
              if( abs( csl )>=abs( snl ) .or. abs( csr )>=abs( snr ) )then
                 ! compute the (1,1) and (1,2) elements of u**h *a and v**h *b,
                 ! and (1,2) element of |u|**h *|a| and |v|**h *|b|.
                 ua11r = csl*a1
                 ua12 = csl*a2 + d1*snl*a3
                 vb11r = csr*b1
                 vb12 = csr*b2 + d1*snr*b3
                 aua12 = abs( csl )*abs1( a2 ) + abs( snl )*abs( a3 )
                 avb12 = abs( csr )*abs1( b2 ) + abs( snr )*abs( b3 )
                 ! zero (1,2) elements of u**h *a and v**h *b
                 if( ( abs( ua11r )+abs1( ua12 ) )==zero ) then
                    call stdlib_clartg( -cmplx( vb11r,KIND=sp), conjg( vb12 ), csq, snq,r )
                              
                 else if( ( abs( vb11r )+abs1( vb12 ) )==zero ) then
                    call stdlib_clartg( -cmplx( ua11r,KIND=sp), conjg( ua12 ), csq, snq,r )
                              
                 else if( aua12 / ( abs( ua11r )+abs1( ua12 ) )<=avb12 /( abs( vb11r )+abs1( vb12 &
                           ) ) ) then
                    call stdlib_clartg( -cmplx( ua11r,KIND=sp), conjg( ua12 ), csq, snq,r )
                              
                 else
                    call stdlib_clartg( -cmplx( vb11r,KIND=sp), conjg( vb12 ), csq, snq,r )
                              
                 end if
                 csu = csl
                 snu = -d1*snl
                 csv = csr
                 snv = -d1*snr
              else
                 ! compute the (2,1) and (2,2) elements of u**h *a and v**h *b,
                 ! and (2,2) element of |u|**h *|a| and |v|**h *|b|.
                 ua21 = -conjg( d1 )*snl*a1
                 ua22 = -conjg( d1 )*snl*a2 + csl*a3
                 vb21 = -conjg( d1 )*snr*b1
                 vb22 = -conjg( d1 )*snr*b2 + csr*b3
                 aua22 = abs( snl )*abs1( a2 ) + abs( csl )*abs( a3 )
                 avb22 = abs( snr )*abs1( b2 ) + abs( csr )*abs( b3 )
                 ! zero (2,2) elements of u**h *a and v**h *b, and then swap.
                 if( ( abs1( ua21 )+abs1( ua22 ) )==zero ) then
                    call stdlib_clartg( -conjg( vb21 ), conjg( vb22 ), csq, snq, r )
                 else if( ( abs1( vb21 )+abs( vb22 ) )==zero ) then
                    call stdlib_clartg( -conjg( ua21 ), conjg( ua22 ), csq, snq, r )
                 else if( aua22 / ( abs1( ua21 )+abs1( ua22 ) )<=avb22 /( abs1( vb21 )+abs1( vb22 &
                           ) ) ) then
                    call stdlib_clartg( -conjg( ua21 ), conjg( ua22 ), csq, snq, r )
                 else
                    call stdlib_clartg( -conjg( vb21 ), conjg( vb22 ), csq, snq, r )
                 end if
                 csu = snl
                 snu = d1*csl
                 csv = snr
                 snv = d1*csr
              end if
           else
              ! input matrices a and b are lower triangular matrices
              ! form matrix c = a*adj(b) = ( a 0 )
                                         ! ( c d )
              a = a1*b3
              d = a3*b1
              c = a2*b3 - a3*b2
              fc = abs( c )
              ! transform complex 2-by-2 matrix c to real matrix by unitary
              ! diagonal matrix diag(d1,1).
              d1 = one
              if( fc/=zero )d1 = c / fc
              ! the svd of real 2 by 2 triangular c
               ! ( csl -snl )*( a 0 )*(  csr  snr ) = ( r 0 )
               ! ( snl  csl ) ( c d ) ( -snr  csr )   ( 0 t )
              call stdlib_slasv2( a, fc, d, s1, s2, snr, csr, snl, csl )
              if( abs( csr )>=abs( snr ) .or. abs( csl )>=abs( snl ) )then
                 ! compute the (2,1) and (2,2) elements of u**h *a and v**h *b,
                 ! and (2,1) element of |u|**h *|a| and |v|**h *|b|.
                 ua21 = -d1*snr*a1 + csr*a2
                 ua22r = csr*a3
                 vb21 = -d1*snl*b1 + csl*b2
                 vb22r = csl*b3
                 aua21 = abs( snr )*abs( a1 ) + abs( csr )*abs1( a2 )
                 avb21 = abs( snl )*abs( b1 ) + abs( csl )*abs1( b2 )
                 ! zero (2,1) elements of u**h *a and v**h *b.
                 if( ( abs1( ua21 )+abs( ua22r ) )==zero ) then
                    call stdlib_clartg( cmplx( vb22r,KIND=sp), vb21, csq, snq, r )
                 else if( ( abs1( vb21 )+abs( vb22r ) )==zero ) then
                    call stdlib_clartg( cmplx( ua22r,KIND=sp), ua21, csq, snq, r )
                 else if( aua21 / ( abs1( ua21 )+abs( ua22r ) )<=avb21 /( abs1( vb21 )+abs( vb22r &
                           ) ) ) then
                    call stdlib_clartg( cmplx( ua22r,KIND=sp), ua21, csq, snq, r )
                 else
                    call stdlib_clartg( cmplx( vb22r,KIND=sp), vb21, csq, snq, r )
                 end if
                 csu = csr
                 snu = -conjg( d1 )*snr
                 csv = csl
                 snv = -conjg( d1 )*snl
              else
                 ! compute the (1,1) and (1,2) elements of u**h *a and v**h *b,
                 ! and (1,1) element of |u|**h *|a| and |v|**h *|b|.
                 ua11 = csr*a1 + conjg( d1 )*snr*a2
                 ua12 = conjg( d1 )*snr*a3
                 vb11 = csl*b1 + conjg( d1 )*snl*b2
                 vb12 = conjg( d1 )*snl*b3
                 aua11 = abs( csr )*abs( a1 ) + abs( snr )*abs1( a2 )
                 avb11 = abs( csl )*abs( b1 ) + abs( snl )*abs1( b2 )
                 ! zero (1,1) elements of u**h *a and v**h *b, and then swap.
                 if( ( abs1( ua11 )+abs1( ua12 ) )==zero ) then
                    call stdlib_clartg( vb12, vb11, csq, snq, r )
                 else if( ( abs1( vb11 )+abs1( vb12 ) )==zero ) then
                    call stdlib_clartg( ua12, ua11, csq, snq, r )
                 else if( aua11 / ( abs1( ua11 )+abs1( ua12 ) )<=avb11 /( abs1( vb11 )+abs1( vb12 &
                           ) ) ) then
                    call stdlib_clartg( ua12, ua11, csq, snq, r )
                 else
                    call stdlib_clartg( vb12, vb11, csq, snq, r )
                 end if
                 csu = snr
                 snu = conjg( d1 )*csr
                 csv = snl
                 snv = conjg( d1 )*csl
              end if
           end if
           return
     end subroutine stdlib_clags2

     pure module subroutine stdlib_zlags2( upper, a1, a2, a3, b1, b2, b3, csu, snu, csv,snv, csq, snq )
     !! ZLAGS2 computes 2-by-2 unitary matrices U, V and Q, such
     !! that if ( UPPER ) then
     !! U**H *A*Q = U**H *( A1 A2 )*Q = ( x  0  )
     !! ( 0  A3 )     ( x  x  )
     !! and
     !! V**H*B*Q = V**H *( B1 B2 )*Q = ( x  0  )
     !! ( 0  B3 )     ( x  x  )
     !! or if ( .NOT.UPPER ) then
     !! U**H *A*Q = U**H *( A1 0  )*Q = ( x  x  )
     !! ( A2 A3 )     ( 0  x  )
     !! and
     !! V**H *B*Q = V**H *( B1 0  )*Q = ( x  x  )
     !! ( B2 B3 )     ( 0  x  )
     !! where
     !! U = (   CSU    SNU ), V = (  CSV    SNV ),
     !! ( -SNU**H  CSU )      ( -SNV**H CSV )
     !! Q = (   CSQ    SNQ )
     !! ( -SNQ**H  CSQ )
     !! The rows of the transformed A and B are parallel. Moreover, if the
     !! input 2-by-2 matrix A is not zero, then the transformed (1,1) entry
     !! of A is not zero. If the input matrices A and B are both not zero,
     !! then the transformed (2,2) element of B is not zero, except when the
     !! first rows of input A and B are parallel and the second rows are
     !! zero.
               
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           logical(lk), intent(in) :: upper
           real(dp), intent(in) :: a1, a3, b1, b3
           real(dp), intent(out) :: csq, csu, csv
           complex(dp), intent(in) :: a2, b2
           complex(dp), intent(out) :: snq, snu, snv
        ! =====================================================================
           
           ! Local Scalars 
           real(dp) :: a, aua11, aua12, aua21, aua22, avb12, avb11, avb21, avb22, csl, csr, d, fb,&
                      fc, s1, s2, snl, snr, ua11r, ua22r, vb11r, vb22r
           complex(dp) :: b, c, d1, r, t, ua11, ua12, ua21, ua22, vb11, vb12, vb21, vb22
           ! Intrinsic Functions 
           ! Statement Functions 
           real(dp) :: abs1
           ! Statement Function Definitions 
           abs1( t ) = abs( real( t,KIND=dp) ) + abs( aimag( t ) )
           ! Executable Statements 
           if( upper ) then
              ! input matrices a and b are upper triangular matrices
              ! form matrix c = a*adj(b) = ( a b )
                                         ! ( 0 d )
              a = a1*b3
              d = a3*b1
              b = a2*b1 - a1*b2
              fb = abs( b )
              ! transform complex 2-by-2 matrix c to real matrix by unitary
              ! diagonal matrix diag(1,d1).
              d1 = one
              if( fb/=zero )d1 = b / fb
              ! the svd of real 2 by 2 triangular c
               ! ( csl -snl )*( a b )*(  csr  snr ) = ( r 0 )
               ! ( snl  csl ) ( 0 d ) ( -snr  csr )   ( 0 t )
              call stdlib_dlasv2( a, fb, d, s1, s2, snr, csr, snl, csl )
              if( abs( csl )>=abs( snl ) .or. abs( csr )>=abs( snr ) )then
                 ! compute the (1,1) and (1,2) elements of u**h *a and v**h *b,
                 ! and (1,2) element of |u|**h *|a| and |v|**h *|b|.
                 ua11r = csl*a1
                 ua12 = csl*a2 + d1*snl*a3
                 vb11r = csr*b1
                 vb12 = csr*b2 + d1*snr*b3
                 aua12 = abs( csl )*abs1( a2 ) + abs( snl )*abs( a3 )
                 avb12 = abs( csr )*abs1( b2 ) + abs( snr )*abs( b3 )
                 ! zero (1,2) elements of u**h *a and v**h *b
                 if( ( abs( ua11r )+abs1( ua12 ) )==zero ) then
                    call stdlib_zlartg( -cmplx( vb11r,KIND=dp), conjg( vb12 ), csq, snq,r )
                              
                 else if( ( abs( vb11r )+abs1( vb12 ) )==zero ) then
                    call stdlib_zlartg( -cmplx( ua11r,KIND=dp), conjg( ua12 ), csq, snq,r )
                              
                 else if( aua12 / ( abs( ua11r )+abs1( ua12 ) )<=avb12 /( abs( vb11r )+abs1( vb12 &
                           ) ) ) then
                    call stdlib_zlartg( -cmplx( ua11r,KIND=dp), conjg( ua12 ), csq, snq,r )
                              
                 else
                    call stdlib_zlartg( -cmplx( vb11r,KIND=dp), conjg( vb12 ), csq, snq,r )
                              
                 end if
                 csu = csl
                 snu = -d1*snl
                 csv = csr
                 snv = -d1*snr
              else
                 ! compute the (2,1) and (2,2) elements of u**h *a and v**h *b,
                 ! and (2,2) element of |u|**h *|a| and |v|**h *|b|.
                 ua21 = -conjg( d1 )*snl*a1
                 ua22 = -conjg( d1 )*snl*a2 + csl*a3
                 vb21 = -conjg( d1 )*snr*b1
                 vb22 = -conjg( d1 )*snr*b2 + csr*b3
                 aua22 = abs( snl )*abs1( a2 ) + abs( csl )*abs( a3 )
                 avb22 = abs( snr )*abs1( b2 ) + abs( csr )*abs( b3 )
                 ! zero (2,2) elements of u**h *a and v**h *b, and then swap.
                 if( ( abs1( ua21 )+abs1( ua22 ) )==zero ) then
                    call stdlib_zlartg( -conjg( vb21 ), conjg( vb22 ), csq, snq,r )
                 else if( ( abs1( vb21 )+abs( vb22 ) )==zero ) then
                    call stdlib_zlartg( -conjg( ua21 ), conjg( ua22 ), csq, snq,r )
                 else if( aua22 / ( abs1( ua21 )+abs1( ua22 ) )<=avb22 /( abs1( vb21 )+abs1( vb22 &
                           ) ) ) then
                    call stdlib_zlartg( -conjg( ua21 ), conjg( ua22 ), csq, snq,r )
                 else
                    call stdlib_zlartg( -conjg( vb21 ), conjg( vb22 ), csq, snq,r )
                 end if
                 csu = snl
                 snu = d1*csl
                 csv = snr
                 snv = d1*csr
              end if
           else
              ! input matrices a and b are lower triangular matrices
              ! form matrix c = a*adj(b) = ( a 0 )
                                         ! ( c d )
              a = a1*b3
              d = a3*b1
              c = a2*b3 - a3*b2
              fc = abs( c )
              ! transform complex 2-by-2 matrix c to real matrix by unitary
              ! diagonal matrix diag(d1,1).
              d1 = one
              if( fc/=zero )d1 = c / fc
              ! the svd of real 2 by 2 triangular c
               ! ( csl -snl )*( a 0 )*(  csr  snr ) = ( r 0 )
               ! ( snl  csl ) ( c d ) ( -snr  csr )   ( 0 t )
              call stdlib_dlasv2( a, fc, d, s1, s2, snr, csr, snl, csl )
              if( abs( csr )>=abs( snr ) .or. abs( csl )>=abs( snl ) )then
                 ! compute the (2,1) and (2,2) elements of u**h *a and v**h *b,
                 ! and (2,1) element of |u|**h *|a| and |v|**h *|b|.
                 ua21 = -d1*snr*a1 + csr*a2
                 ua22r = csr*a3
                 vb21 = -d1*snl*b1 + csl*b2
                 vb22r = csl*b3
                 aua21 = abs( snr )*abs( a1 ) + abs( csr )*abs1( a2 )
                 avb21 = abs( snl )*abs( b1 ) + abs( csl )*abs1( b2 )
                 ! zero (2,1) elements of u**h *a and v**h *b.
                 if( ( abs1( ua21 )+abs( ua22r ) )==zero ) then
                    call stdlib_zlartg( cmplx( vb22r,KIND=dp), vb21, csq, snq, r )
                 else if( ( abs1( vb21 )+abs( vb22r ) )==zero ) then
                    call stdlib_zlartg( cmplx( ua22r,KIND=dp), ua21, csq, snq, r )
                 else if( aua21 / ( abs1( ua21 )+abs( ua22r ) )<=avb21 /( abs1( vb21 )+abs( vb22r &
                           ) ) ) then
                    call stdlib_zlartg( cmplx( ua22r,KIND=dp), ua21, csq, snq, r )
                 else
                    call stdlib_zlartg( cmplx( vb22r,KIND=dp), vb21, csq, snq, r )
                 end if
                 csu = csr
                 snu = -conjg( d1 )*snr
                 csv = csl
                 snv = -conjg( d1 )*snl
              else
                 ! compute the (1,1) and (1,2) elements of u**h *a and v**h *b,
                 ! and (1,1) element of |u|**h *|a| and |v|**h *|b|.
                 ua11 = csr*a1 + conjg( d1 )*snr*a2
                 ua12 = conjg( d1 )*snr*a3
                 vb11 = csl*b1 + conjg( d1 )*snl*b2
                 vb12 = conjg( d1 )*snl*b3
                 aua11 = abs( csr )*abs( a1 ) + abs( snr )*abs1( a2 )
                 avb11 = abs( csl )*abs( b1 ) + abs( snl )*abs1( b2 )
                 ! zero (1,1) elements of u**h *a and v**h *b, and then swap.
                 if( ( abs1( ua11 )+abs1( ua12 ) )==zero ) then
                    call stdlib_zlartg( vb12, vb11, csq, snq, r )
                 else if( ( abs1( vb11 )+abs1( vb12 ) )==zero ) then
                    call stdlib_zlartg( ua12, ua11, csq, snq, r )
                 else if( aua11 / ( abs1( ua11 )+abs1( ua12 ) )<=avb11 /( abs1( vb11 )+abs1( vb12 &
                           ) ) ) then
                    call stdlib_zlartg( ua12, ua11, csq, snq, r )
                 else
                    call stdlib_zlartg( vb12, vb11, csq, snq, r )
                 end if
                 csu = snr
                 snu = conjg( d1 )*csr
                 csv = snl
                 snv = conjg( d1 )*csl
              end if
           end if
           return
     end subroutine stdlib_zlags2




     pure module subroutine stdlib_slapll( n, x, incx, y, incy, ssmin )
     !! Given two column vectors X and Y, let
     !! A = ( X Y ).
     !! The subroutine first computes the QR factorization of A = Q*R,
     !! and then computes the SVD of the 2-by-2 upper triangular matrix R.
     !! The smaller singular value of R is returned in SSMIN, which is used
     !! as the measurement of the linear dependency of the vectors X and Y.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx, incy, n
           real(sp), intent(out) :: ssmin
           ! Array Arguments 
           real(sp), intent(inout) :: x(*), y(*)
        ! =====================================================================
           
           ! Local Scalars 
           real(sp) :: a11, a12, a22, c, ssmax, tau
           ! Executable Statements 
           ! quick return if possible
           if( n<=1_ilp ) then
              ssmin = zero
              return
           end if
           ! compute the qr factorization of the n-by-2 matrix ( x y )
           call stdlib_slarfg( n, x( 1_ilp ), x( 1_ilp+incx ), incx, tau )
           a11 = x( 1_ilp )
           x( 1_ilp ) = one
           c = -tau*stdlib_sdot( n, x, incx, y, incy )
           call stdlib_saxpy( n, c, x, incx, y, incy )
           call stdlib_slarfg( n-1, y( 1_ilp+incy ), y( 1_ilp+2*incy ), incy, tau )
           a12 = y( 1_ilp )
           a22 = y( 1_ilp+incy )
           ! compute the svd of 2-by-2 upper triangular matrix.
           call stdlib_slas2( a11, a12, a22, ssmin, ssmax )
           return
     end subroutine stdlib_slapll

     pure module subroutine stdlib_dlapll( n, x, incx, y, incy, ssmin )
     !! Given two column vectors X and Y, let
     !! A = ( X Y ).
     !! The subroutine first computes the QR factorization of A = Q*R,
     !! and then computes the SVD of the 2-by-2 upper triangular matrix R.
     !! The smaller singular value of R is returned in SSMIN, which is used
     !! as the measurement of the linear dependency of the vectors X and Y.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx, incy, n
           real(dp), intent(out) :: ssmin
           ! Array Arguments 
           real(dp), intent(inout) :: x(*), y(*)
        ! =====================================================================
           
           ! Local Scalars 
           real(dp) :: a11, a12, a22, c, ssmax, tau
           ! Executable Statements 
           ! quick return if possible
           if( n<=1_ilp ) then
              ssmin = zero
              return
           end if
           ! compute the qr factorization of the n-by-2 matrix ( x y )
           call stdlib_dlarfg( n, x( 1_ilp ), x( 1_ilp+incx ), incx, tau )
           a11 = x( 1_ilp )
           x( 1_ilp ) = one
           c = -tau*stdlib_ddot( n, x, incx, y, incy )
           call stdlib_daxpy( n, c, x, incx, y, incy )
           call stdlib_dlarfg( n-1, y( 1_ilp+incy ), y( 1_ilp+2*incy ), incy, tau )
           a12 = y( 1_ilp )
           a22 = y( 1_ilp+incy )
           ! compute the svd of 2-by-2 upper triangular matrix.
           call stdlib_dlas2( a11, a12, a22, ssmin, ssmax )
           return
     end subroutine stdlib_dlapll


     pure module subroutine stdlib_clapll( n, x, incx, y, incy, ssmin )
     !! Given two column vectors X and Y, let
     !! A = ( X Y ).
     !! The subroutine first computes the QR factorization of A = Q*R,
     !! and then computes the SVD of the 2-by-2 upper triangular matrix R.
     !! The smaller singular value of R is returned in SSMIN, which is used
     !! as the measurement of the linear dependency of the vectors X and Y.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx, incy, n
           real(sp), intent(out) :: ssmin
           ! Array Arguments 
           complex(sp), intent(inout) :: x(*), y(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           real(sp) :: ssmax
           complex(sp) :: a11, a12, a22, c, tau
           ! Intrinsic Functions 
           ! Executable Statements 
           ! quick return if possible
           if( n<=1_ilp ) then
              ssmin = zero
              return
           end if
           ! compute the qr factorization of the n-by-2 matrix ( x y )
           call stdlib_clarfg( n, x( 1_ilp ), x( 1_ilp+incx ), incx, tau )
           a11 = x( 1_ilp )
           x( 1_ilp ) = cone
           c = -conjg( tau )*stdlib_cdotc( n, x, incx, y, incy )
           call stdlib_caxpy( n, c, x, incx, y, incy )
           call stdlib_clarfg( n-1, y( 1_ilp+incy ), y( 1_ilp+2*incy ), incy, tau )
           a12 = y( 1_ilp )
           a22 = y( 1_ilp+incy )
           ! compute the svd of 2-by-2 upper triangular matrix.
           call stdlib_slas2( abs( a11 ), abs( a12 ), abs( a22 ), ssmin, ssmax )
           return
     end subroutine stdlib_clapll

     pure module subroutine stdlib_zlapll( n, x, incx, y, incy, ssmin )
     !! Given two column vectors X and Y, let
     !! A = ( X Y ).
     !! The subroutine first computes the QR factorization of A = Q*R,
     !! and then computes the SVD of the 2-by-2 upper triangular matrix R.
     !! The smaller singular value of R is returned in SSMIN, which is used
     !! as the measurement of the linear dependency of the vectors X and Y.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx, incy, n
           real(dp), intent(out) :: ssmin
           ! Array Arguments 
           complex(dp), intent(inout) :: x(*), y(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           real(dp) :: ssmax
           complex(dp) :: a11, a12, a22, c, tau
           ! Intrinsic Functions 
           ! Executable Statements 
           ! quick return if possible
           if( n<=1_ilp ) then
              ssmin = zero
              return
           end if
           ! compute the qr factorization of the n-by-2 matrix ( x y )
           call stdlib_zlarfg( n, x( 1_ilp ), x( 1_ilp+incx ), incx, tau )
           a11 = x( 1_ilp )
           x( 1_ilp ) = cone
           c = -conjg( tau )*stdlib_zdotc( n, x, incx, y, incy )
           call stdlib_zaxpy( n, c, x, incx, y, incy )
           call stdlib_zlarfg( n-1, y( 1_ilp+incy ), y( 1_ilp+2*incy ), incy, tau )
           a12 = y( 1_ilp )
           a22 = y( 1_ilp+incy )
           ! compute the svd of 2-by-2 upper triangular matrix.
           call stdlib_dlas2( abs( a11 ), abs( a12 ), abs( a22 ), ssmin, ssmax )
           return
     end subroutine stdlib_zlapll



end submodule stdlib_lapack_svd_comp2
