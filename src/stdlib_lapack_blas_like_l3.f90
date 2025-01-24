submodule(stdlib_lapack_base) stdlib_lapack_blas_like_l3
  implicit none


  contains

     pure module subroutine stdlib_slagtm( trans, n, nrhs, alpha, dl, d, du, x, ldx, beta,b, ldb )
     !! SLAGTM performs a matrix-vector product of the form
     !! B := alpha * A * X + beta * B
     !! where A is a tridiagonal matrix of order N, B and X are N by NRHS
     !! matrices, and alpha and beta are real scalars, each of which may be
     !! 0., 1., or -1.
               
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: trans
           integer(ilp), intent(in) :: ldb, ldx, n, nrhs
           real(sp), intent(in) :: alpha, beta
           ! Array Arguments 
           real(sp), intent(inout) :: b(ldb,*)
           real(sp), intent(in) :: d(*), dl(*), du(*), x(ldx,*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j
           ! Executable Statements 
           if( n==0 )return
           ! multiply b by beta if beta/=1.
           if( beta==zero ) then
              do j = 1, nrhs
                 do i = 1, n
                    b( i, j ) = zero
                 end do
              end do
           else if( beta==-one ) then
              do j = 1, nrhs
                 do i = 1, n
                    b( i, j ) = -b( i, j )
                 end do
              end do
           end if
           if( alpha==one ) then
              if( stdlib_lsame( trans, 'N' ) ) then
                 ! compute b := b + a*x
                 do j = 1, nrhs
                    if( n==1_ilp ) then
                       b( 1_ilp, j ) = b( 1_ilp, j ) + d( 1_ilp )*x( 1_ilp, j )
                    else
                       b( 1_ilp, j ) = b( 1_ilp, j ) + d( 1_ilp )*x( 1_ilp, j ) +du( 1_ilp )*x( 2_ilp, j )
                       b( n, j ) = b( n, j ) + dl( n-1 )*x( n-1, j ) +d( n )*x( n, j )
                       do i = 2, n - 1
                          b( i, j ) = b( i, j ) + dl( i-1 )*x( i-1, j ) +d( i )*x( i, j ) + du( i &
                                    )*x( i+1, j )
                       end do
                    end if
                 end do
              else
                 ! compute b := b + a**t*x
                 do j = 1, nrhs
                    if( n==1_ilp ) then
                       b( 1_ilp, j ) = b( 1_ilp, j ) + d( 1_ilp )*x( 1_ilp, j )
                    else
                       b( 1_ilp, j ) = b( 1_ilp, j ) + d( 1_ilp )*x( 1_ilp, j ) +dl( 1_ilp )*x( 2_ilp, j )
                       b( n, j ) = b( n, j ) + du( n-1 )*x( n-1, j ) +d( n )*x( n, j )
                       do i = 2, n - 1
                          b( i, j ) = b( i, j ) + du( i-1 )*x( i-1, j ) +d( i )*x( i, j ) + dl( i &
                                    )*x( i+1, j )
                       end do
                    end if
                 end do
              end if
           else if( alpha==-one ) then
              if( stdlib_lsame( trans, 'N' ) ) then
                 ! compute b := b - a*x
                 do j = 1, nrhs
                    if( n==1_ilp ) then
                       b( 1_ilp, j ) = b( 1_ilp, j ) - d( 1_ilp )*x( 1_ilp, j )
                    else
                       b( 1_ilp, j ) = b( 1_ilp, j ) - d( 1_ilp )*x( 1_ilp, j ) -du( 1_ilp )*x( 2_ilp, j )
                       b( n, j ) = b( n, j ) - dl( n-1 )*x( n-1, j ) -d( n )*x( n, j )
                       do i = 2, n - 1
                          b( i, j ) = b( i, j ) - dl( i-1 )*x( i-1, j ) -d( i )*x( i, j ) - du( i &
                                    )*x( i+1, j )
                       end do
                    end if
                 end do
              else
                 ! compute b := b - a**t*x
                 do j = 1, nrhs
                    if( n==1_ilp ) then
                       b( 1_ilp, j ) = b( 1_ilp, j ) - d( 1_ilp )*x( 1_ilp, j )
                    else
                       b( 1_ilp, j ) = b( 1_ilp, j ) - d( 1_ilp )*x( 1_ilp, j ) -dl( 1_ilp )*x( 2_ilp, j )
                       b( n, j ) = b( n, j ) - du( n-1 )*x( n-1, j ) -d( n )*x( n, j )
                       do i = 2, n - 1
                          b( i, j ) = b( i, j ) - du( i-1 )*x( i-1, j ) -d( i )*x( i, j ) - dl( i &
                                    )*x( i+1, j )
                       end do
                    end if
                 end do
              end if
           end if
           return
     end subroutine stdlib_slagtm

     pure module subroutine stdlib_dlagtm( trans, n, nrhs, alpha, dl, d, du, x, ldx, beta,b, ldb )
     !! DLAGTM performs a matrix-vector product of the form
     !! B := alpha * A * X + beta * B
     !! where A is a tridiagonal matrix of order N, B and X are N by NRHS
     !! matrices, and alpha and beta are real scalars, each of which may be
     !! 0., 1., or -1.
               
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: trans
           integer(ilp), intent(in) :: ldb, ldx, n, nrhs
           real(dp), intent(in) :: alpha, beta
           ! Array Arguments 
           real(dp), intent(inout) :: b(ldb,*)
           real(dp), intent(in) :: d(*), dl(*), du(*), x(ldx,*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j
           ! Executable Statements 
           if( n==0 )return
           ! multiply b by beta if beta/=1.
           if( beta==zero ) then
              do j = 1, nrhs
                 do i = 1, n
                    b( i, j ) = zero
                 end do
              end do
           else if( beta==-one ) then
              do j = 1, nrhs
                 do i = 1, n
                    b( i, j ) = -b( i, j )
                 end do
              end do
           end if
           if( alpha==one ) then
              if( stdlib_lsame( trans, 'N' ) ) then
                 ! compute b := b + a*x
                 do j = 1, nrhs
                    if( n==1_ilp ) then
                       b( 1_ilp, j ) = b( 1_ilp, j ) + d( 1_ilp )*x( 1_ilp, j )
                    else
                       b( 1_ilp, j ) = b( 1_ilp, j ) + d( 1_ilp )*x( 1_ilp, j ) +du( 1_ilp )*x( 2_ilp, j )
                       b( n, j ) = b( n, j ) + dl( n-1 )*x( n-1, j ) +d( n )*x( n, j )
                       do i = 2, n - 1
                          b( i, j ) = b( i, j ) + dl( i-1 )*x( i-1, j ) +d( i )*x( i, j ) + du( i &
                                    )*x( i+1, j )
                       end do
                    end if
                 end do
              else
                 ! compute b := b + a**t*x
                 do j = 1, nrhs
                    if( n==1_ilp ) then
                       b( 1_ilp, j ) = b( 1_ilp, j ) + d( 1_ilp )*x( 1_ilp, j )
                    else
                       b( 1_ilp, j ) = b( 1_ilp, j ) + d( 1_ilp )*x( 1_ilp, j ) +dl( 1_ilp )*x( 2_ilp, j )
                       b( n, j ) = b( n, j ) + du( n-1 )*x( n-1, j ) +d( n )*x( n, j )
                       do i = 2, n - 1
                          b( i, j ) = b( i, j ) + du( i-1 )*x( i-1, j ) +d( i )*x( i, j ) + dl( i &
                                    )*x( i+1, j )
                       end do
                    end if
                 end do
              end if
           else if( alpha==-one ) then
              if( stdlib_lsame( trans, 'N' ) ) then
                 ! compute b := b - a*x
                 do j = 1, nrhs
                    if( n==1_ilp ) then
                       b( 1_ilp, j ) = b( 1_ilp, j ) - d( 1_ilp )*x( 1_ilp, j )
                    else
                       b( 1_ilp, j ) = b( 1_ilp, j ) - d( 1_ilp )*x( 1_ilp, j ) -du( 1_ilp )*x( 2_ilp, j )
                       b( n, j ) = b( n, j ) - dl( n-1 )*x( n-1, j ) -d( n )*x( n, j )
                       do i = 2, n - 1
                          b( i, j ) = b( i, j ) - dl( i-1 )*x( i-1, j ) -d( i )*x( i, j ) - du( i &
                                    )*x( i+1, j )
                       end do
                    end if
                 end do
              else
                 ! compute b := b - a**t*x
                 do j = 1, nrhs
                    if( n==1_ilp ) then
                       b( 1_ilp, j ) = b( 1_ilp, j ) - d( 1_ilp )*x( 1_ilp, j )
                    else
                       b( 1_ilp, j ) = b( 1_ilp, j ) - d( 1_ilp )*x( 1_ilp, j ) -dl( 1_ilp )*x( 2_ilp, j )
                       b( n, j ) = b( n, j ) - du( n-1 )*x( n-1, j ) -d( n )*x( n, j )
                       do i = 2, n - 1
                          b( i, j ) = b( i, j ) - du( i-1 )*x( i-1, j ) -d( i )*x( i, j ) - dl( i &
                                    )*x( i+1, j )
                       end do
                    end if
                 end do
              end if
           end if
           return
     end subroutine stdlib_dlagtm


     pure module subroutine stdlib_clagtm( trans, n, nrhs, alpha, dl, d, du, x, ldx, beta,b, ldb )
     !! CLAGTM performs a matrix-vector product of the form
     !! B := alpha * A * X + beta * B
     !! where A is a tridiagonal matrix of order N, B and X are N by NRHS
     !! matrices, and alpha and beta are real scalars, each of which may be
     !! 0., 1., or -1.
               
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: trans
           integer(ilp), intent(in) :: ldb, ldx, n, nrhs
           real(sp), intent(in) :: alpha, beta
           ! Array Arguments 
           complex(sp), intent(inout) :: b(ldb,*)
           complex(sp), intent(in) :: d(*), dl(*), du(*), x(ldx,*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j
           ! Intrinsic Functions 
           ! Executable Statements 
           if( n==0 )return
           ! multiply b by beta if beta/=1.
           if( beta==zero ) then
              do j = 1, nrhs
                 do i = 1, n
                    b( i, j ) = zero
                 end do
              end do
           else if( beta==-one ) then
              do j = 1, nrhs
                 do i = 1, n
                    b( i, j ) = -b( i, j )
                 end do
              end do
           end if
           if( alpha==one ) then
              if( stdlib_lsame( trans, 'N' ) ) then
                 ! compute b := b + a*x
                 do j = 1, nrhs
                    if( n==1_ilp ) then
                       b( 1_ilp, j ) = b( 1_ilp, j ) + d( 1_ilp )*x( 1_ilp, j )
                    else
                       b( 1_ilp, j ) = b( 1_ilp, j ) + d( 1_ilp )*x( 1_ilp, j ) +du( 1_ilp )*x( 2_ilp, j )
                       b( n, j ) = b( n, j ) + dl( n-1 )*x( n-1, j ) +d( n )*x( n, j )
                       do i = 2, n - 1
                          b( i, j ) = b( i, j ) + dl( i-1 )*x( i-1, j ) +d( i )*x( i, j ) + du( i &
                                    )*x( i+1, j )
                       end do
                    end if
                 end do
              else if( stdlib_lsame( trans, 'T' ) ) then
                 ! compute b := b + a**t * x
                 do j = 1, nrhs
                    if( n==1_ilp ) then
                       b( 1_ilp, j ) = b( 1_ilp, j ) + d( 1_ilp )*x( 1_ilp, j )
                    else
                       b( 1_ilp, j ) = b( 1_ilp, j ) + d( 1_ilp )*x( 1_ilp, j ) +dl( 1_ilp )*x( 2_ilp, j )
                       b( n, j ) = b( n, j ) + du( n-1 )*x( n-1, j ) +d( n )*x( n, j )
                       do i = 2, n - 1
                          b( i, j ) = b( i, j ) + du( i-1 )*x( i-1, j ) +d( i )*x( i, j ) + dl( i &
                                    )*x( i+1, j )
                       end do
                    end if
                 end do
              else if( stdlib_lsame( trans, 'C' ) ) then
                 ! compute b := b + a**h * x
                 do j = 1, nrhs
                    if( n==1_ilp ) then
                       b( 1_ilp, j ) = b( 1_ilp, j ) + conjg( d( 1_ilp ) )*x( 1_ilp, j )
                    else
                       b( 1_ilp, j ) = b( 1_ilp, j ) + conjg( d( 1_ilp ) )*x( 1_ilp, j ) +conjg( dl( 1_ilp ) )*x( 2_ilp, &
                                 j )
                       b( n, j ) = b( n, j ) + conjg( du( n-1 ) )*x( n-1, j ) + conjg( d( n ) )*x(&
                                  n, j )
                       do i = 2, n - 1
                          b( i, j ) = b( i, j ) + conjg( du( i-1 ) )*x( i-1, j ) + conjg( d( i ) )&
                                    *x( i, j ) + conjg( dl( i ) )*x( i+1, j )
                       end do
                    end if
                 end do
              end if
           else if( alpha==-one ) then
              if( stdlib_lsame( trans, 'N' ) ) then
                 ! compute b := b - a*x
                 do j = 1, nrhs
                    if( n==1_ilp ) then
                       b( 1_ilp, j ) = b( 1_ilp, j ) - d( 1_ilp )*x( 1_ilp, j )
                    else
                       b( 1_ilp, j ) = b( 1_ilp, j ) - d( 1_ilp )*x( 1_ilp, j ) -du( 1_ilp )*x( 2_ilp, j )
                       b( n, j ) = b( n, j ) - dl( n-1 )*x( n-1, j ) -d( n )*x( n, j )
                       do i = 2, n - 1
                          b( i, j ) = b( i, j ) - dl( i-1 )*x( i-1, j ) -d( i )*x( i, j ) - du( i &
                                    )*x( i+1, j )
                       end do
                    end if
                 end do
              else if( stdlib_lsame( trans, 'T' ) ) then
                 ! compute b := b - a**t*x
                 do j = 1, nrhs
                    if( n==1_ilp ) then
                       b( 1_ilp, j ) = b( 1_ilp, j ) - d( 1_ilp )*x( 1_ilp, j )
                    else
                       b( 1_ilp, j ) = b( 1_ilp, j ) - d( 1_ilp )*x( 1_ilp, j ) -dl( 1_ilp )*x( 2_ilp, j )
                       b( n, j ) = b( n, j ) - du( n-1 )*x( n-1, j ) -d( n )*x( n, j )
                       do i = 2, n - 1
                          b( i, j ) = b( i, j ) - du( i-1 )*x( i-1, j ) -d( i )*x( i, j ) - dl( i &
                                    )*x( i+1, j )
                       end do
                    end if
                 end do
              else if( stdlib_lsame( trans, 'C' ) ) then
                 ! compute b := b - a**h*x
                 do j = 1, nrhs
                    if( n==1_ilp ) then
                       b( 1_ilp, j ) = b( 1_ilp, j ) - conjg( d( 1_ilp ) )*x( 1_ilp, j )
                    else
                       b( 1_ilp, j ) = b( 1_ilp, j ) - conjg( d( 1_ilp ) )*x( 1_ilp, j ) -conjg( dl( 1_ilp ) )*x( 2_ilp, &
                                 j )
                       b( n, j ) = b( n, j ) - conjg( du( n-1 ) )*x( n-1, j ) - conjg( d( n ) )*x(&
                                  n, j )
                       do i = 2, n - 1
                          b( i, j ) = b( i, j ) - conjg( du( i-1 ) )*x( i-1, j ) - conjg( d( i ) )&
                                    *x( i, j ) - conjg( dl( i ) )*x( i+1, j )
                       end do
                    end if
                 end do
              end if
           end if
           return
     end subroutine stdlib_clagtm

     pure module subroutine stdlib_zlagtm( trans, n, nrhs, alpha, dl, d, du, x, ldx, beta,b, ldb )
     !! ZLAGTM performs a matrix-vector product of the form
     !! B := alpha * A * X + beta * B
     !! where A is a tridiagonal matrix of order N, B and X are N by NRHS
     !! matrices, and alpha and beta are real scalars, each of which may be
     !! 0., 1., or -1.
               
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: trans
           integer(ilp), intent(in) :: ldb, ldx, n, nrhs
           real(dp), intent(in) :: alpha, beta
           ! Array Arguments 
           complex(dp), intent(inout) :: b(ldb,*)
           complex(dp), intent(in) :: d(*), dl(*), du(*), x(ldx,*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j
           ! Intrinsic Functions 
           ! Executable Statements 
           if( n==0 )return
           ! multiply b by beta if beta/=1.
           if( beta==zero ) then
              do j = 1, nrhs
                 do i = 1, n
                    b( i, j ) = zero
                 end do
              end do
           else if( beta==-one ) then
              do j = 1, nrhs
                 do i = 1, n
                    b( i, j ) = -b( i, j )
                 end do
              end do
           end if
           if( alpha==one ) then
              if( stdlib_lsame( trans, 'N' ) ) then
                 ! compute b := b + a*x
                 do j = 1, nrhs
                    if( n==1_ilp ) then
                       b( 1_ilp, j ) = b( 1_ilp, j ) + d( 1_ilp )*x( 1_ilp, j )
                    else
                       b( 1_ilp, j ) = b( 1_ilp, j ) + d( 1_ilp )*x( 1_ilp, j ) +du( 1_ilp )*x( 2_ilp, j )
                       b( n, j ) = b( n, j ) + dl( n-1 )*x( n-1, j ) +d( n )*x( n, j )
                       do i = 2, n - 1
                          b( i, j ) = b( i, j ) + dl( i-1 )*x( i-1, j ) +d( i )*x( i, j ) + du( i &
                                    )*x( i+1, j )
                       end do
                    end if
                 end do
              else if( stdlib_lsame( trans, 'T' ) ) then
                 ! compute b := b + a**t * x
                 do j = 1, nrhs
                    if( n==1_ilp ) then
                       b( 1_ilp, j ) = b( 1_ilp, j ) + d( 1_ilp )*x( 1_ilp, j )
                    else
                       b( 1_ilp, j ) = b( 1_ilp, j ) + d( 1_ilp )*x( 1_ilp, j ) +dl( 1_ilp )*x( 2_ilp, j )
                       b( n, j ) = b( n, j ) + du( n-1 )*x( n-1, j ) +d( n )*x( n, j )
                       do i = 2, n - 1
                          b( i, j ) = b( i, j ) + du( i-1 )*x( i-1, j ) +d( i )*x( i, j ) + dl( i &
                                    )*x( i+1, j )
                       end do
                    end if
                 end do
              else if( stdlib_lsame( trans, 'C' ) ) then
                 ! compute b := b + a**h * x
                 do j = 1, nrhs
                    if( n==1_ilp ) then
                       b( 1_ilp, j ) = b( 1_ilp, j ) + conjg( d( 1_ilp ) )*x( 1_ilp, j )
                    else
                       b( 1_ilp, j ) = b( 1_ilp, j ) + conjg( d( 1_ilp ) )*x( 1_ilp, j ) +conjg( dl( 1_ilp ) )*x( 2_ilp, &
                                 j )
                       b( n, j ) = b( n, j ) + conjg( du( n-1 ) )*x( n-1, j ) + conjg( d( n ) )*x(&
                                  n, j )
                       do i = 2, n - 1
                          b( i, j ) = b( i, j ) + conjg( du( i-1 ) )*x( i-1, j ) + conjg( d( i ) )&
                                    *x( i, j ) + conjg( dl( i ) )*x( i+1, j )
                       end do
                    end if
                 end do
              end if
           else if( alpha==-one ) then
              if( stdlib_lsame( trans, 'N' ) ) then
                 ! compute b := b - a*x
                 do j = 1, nrhs
                    if( n==1_ilp ) then
                       b( 1_ilp, j ) = b( 1_ilp, j ) - d( 1_ilp )*x( 1_ilp, j )
                    else
                       b( 1_ilp, j ) = b( 1_ilp, j ) - d( 1_ilp )*x( 1_ilp, j ) -du( 1_ilp )*x( 2_ilp, j )
                       b( n, j ) = b( n, j ) - dl( n-1 )*x( n-1, j ) -d( n )*x( n, j )
                       do i = 2, n - 1
                          b( i, j ) = b( i, j ) - dl( i-1 )*x( i-1, j ) -d( i )*x( i, j ) - du( i &
                                    )*x( i+1, j )
                       end do
                    end if
                 end do
              else if( stdlib_lsame( trans, 'T' ) ) then
                 ! compute b := b - a**t *x
                 do j = 1, nrhs
                    if( n==1_ilp ) then
                       b( 1_ilp, j ) = b( 1_ilp, j ) - d( 1_ilp )*x( 1_ilp, j )
                    else
                       b( 1_ilp, j ) = b( 1_ilp, j ) - d( 1_ilp )*x( 1_ilp, j ) -dl( 1_ilp )*x( 2_ilp, j )
                       b( n, j ) = b( n, j ) - du( n-1 )*x( n-1, j ) -d( n )*x( n, j )
                       do i = 2, n - 1
                          b( i, j ) = b( i, j ) - du( i-1 )*x( i-1, j ) -d( i )*x( i, j ) - dl( i &
                                    )*x( i+1, j )
                       end do
                    end if
                 end do
              else if( stdlib_lsame( trans, 'C' ) ) then
                 ! compute b := b - a**h *x
                 do j = 1, nrhs
                    if( n==1_ilp ) then
                       b( 1_ilp, j ) = b( 1_ilp, j ) - conjg( d( 1_ilp ) )*x( 1_ilp, j )
                    else
                       b( 1_ilp, j ) = b( 1_ilp, j ) - conjg( d( 1_ilp ) )*x( 1_ilp, j ) -conjg( dl( 1_ilp ) )*x( 2_ilp, &
                                 j )
                       b( n, j ) = b( n, j ) - conjg( du( n-1 ) )*x( n-1, j ) - conjg( d( n ) )*x(&
                                  n, j )
                       do i = 2, n - 1
                          b( i, j ) = b( i, j ) - conjg( du( i-1 ) )*x( i-1, j ) - conjg( d( i ) )&
                                    *x( i, j ) - conjg( dl( i ) )*x( i+1, j )
                       end do
                    end if
                 end do
              end if
           end if
           return
     end subroutine stdlib_zlagtm




     pure module subroutine stdlib_clacrm( m, n, a, lda, b, ldb, c, ldc, rwork )
     !! CLACRM performs a very simple matrix-matrix multiplication:
     !! C := A * B,
     !! where A is M by N and complex; B is N by N and real;
     !! C is M by N and complex.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: lda, ldb, ldc, m, n
           ! Array Arguments 
           real(sp), intent(in) :: b(ldb,*)
           real(sp), intent(out) :: rwork(*)
           complex(sp), intent(in) :: a(lda,*)
           complex(sp), intent(out) :: c(ldc,*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j, l
           ! Intrinsic Functions 
           ! Executable Statements 
           ! quick return if possible.
           if( ( m==0 ) .or. ( n==0 ) )return
           do j = 1, n
              do i = 1, m
                 rwork( ( j-1 )*m+i ) = real( a( i, j ),KIND=sp)
              end do
           end do
           l = m*n + 1_ilp
           call stdlib_sgemm( 'N', 'N', m, n, n, one, rwork, m, b, ldb, zero,rwork( l ), m )
                     
           do j = 1, n
              do i = 1, m
                 c( i, j ) = rwork( l+( j-1 )*m+i-1 )
              end do
           end do
           do j = 1, n
              do i = 1, m
                 rwork( ( j-1 )*m+i ) = aimag( a( i, j ) )
              end do
           end do
           call stdlib_sgemm( 'N', 'N', m, n, n, one, rwork, m, b, ldb, zero,rwork( l ), m )
                     
           do j = 1, n
              do i = 1, m
                 c( i, j ) = cmplx( real( c( i, j ),KIND=sp),rwork( l+( j-1 )*m+i-1 ),KIND=sp)
                           
              end do
           end do
           return
     end subroutine stdlib_clacrm

     pure module subroutine stdlib_zlacrm( m, n, a, lda, b, ldb, c, ldc, rwork )
     !! ZLACRM performs a very simple matrix-matrix multiplication:
     !! C := A * B,
     !! where A is M by N and complex; B is N by N and real;
     !! C is M by N and complex.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: lda, ldb, ldc, m, n
           ! Array Arguments 
           real(dp), intent(in) :: b(ldb,*)
           real(dp), intent(out) :: rwork(*)
           complex(dp), intent(in) :: a(lda,*)
           complex(dp), intent(out) :: c(ldc,*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j, l
           ! Intrinsic Functions 
           ! Executable Statements 
           ! quick return if possible.
           if( ( m==0 ) .or. ( n==0 ) )return
           do j = 1, n
              do i = 1, m
                 rwork( ( j-1 )*m+i ) = real( a( i, j ),KIND=dp)
              end do
           end do
           l = m*n + 1_ilp
           call stdlib_dgemm( 'N', 'N', m, n, n, one, rwork, m, b, ldb, zero,rwork( l ), m )
                     
           do j = 1, n
              do i = 1, m
                 c( i, j ) = rwork( l+( j-1 )*m+i-1 )
              end do
           end do
           do j = 1, n
              do i = 1, m
                 rwork( ( j-1 )*m+i ) = aimag( a( i, j ) )
              end do
           end do
           call stdlib_dgemm( 'N', 'N', m, n, n, one, rwork, m, b, ldb, zero,rwork( l ), m )
                     
           do j = 1, n
              do i = 1, m
                 c( i, j ) = cmplx( real( c( i, j ),KIND=dp),rwork( l+( j-1 )*m+i-1 ),KIND=dp)
                           
              end do
           end do
           return
     end subroutine stdlib_zlacrm




     pure module subroutine stdlib_clarcm( m, n, a, lda, b, ldb, c, ldc, rwork )
     !! CLARCM performs a very simple matrix-matrix multiplication:
     !! C := A * B,
     !! where A is M by M and real; B is M by N and complex;
     !! C is M by N and complex.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: lda, ldb, ldc, m, n
           ! Array Arguments 
           real(sp), intent(in) :: a(lda,*)
           real(sp), intent(out) :: rwork(*)
           complex(sp), intent(in) :: b(ldb,*)
           complex(sp), intent(out) :: c(ldc,*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j, l
           ! Intrinsic Functions 
           ! Executable Statements 
           ! quick return if possible.
           if( ( m==0 ) .or. ( n==0 ) )return
           do j = 1, n
              do i = 1, m
                 rwork( ( j-1 )*m+i ) = real( b( i, j ),KIND=sp)
              end do
           end do
           l = m*n + 1_ilp
           call stdlib_sgemm( 'N', 'N', m, n, m, one, a, lda, rwork, m, zero,rwork( l ), m )
                     
           do j = 1, n
              do i = 1, m
                 c( i, j ) = rwork( l+( j-1 )*m+i-1 )
              end do
           end do
           do j = 1, n
              do i = 1, m
                 rwork( ( j-1 )*m+i ) = aimag( b( i, j ) )
              end do
           end do
           call stdlib_sgemm( 'N', 'N', m, n, m, one, a, lda, rwork, m, zero,rwork( l ), m )
                     
           do j = 1, n
              do i = 1, m
                 c( i, j ) = cmplx( real( c( i, j ),KIND=sp),rwork( l+( j-1 )*m+i-1 ),KIND=sp)
                           
              end do
           end do
           return
     end subroutine stdlib_clarcm

     pure module subroutine stdlib_zlarcm( m, n, a, lda, b, ldb, c, ldc, rwork )
     !! ZLARCM performs a very simple matrix-matrix multiplication:
     !! C := A * B,
     !! where A is M by M and real; B is M by N and complex;
     !! C is M by N and complex.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: lda, ldb, ldc, m, n
           ! Array Arguments 
           real(dp), intent(in) :: a(lda,*)
           real(dp), intent(out) :: rwork(*)
           complex(dp), intent(in) :: b(ldb,*)
           complex(dp), intent(out) :: c(ldc,*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j, l
           ! Intrinsic Functions 
           ! Executable Statements 
           ! quick return if possible.
           if( ( m==0 ) .or. ( n==0 ) )return
           do j = 1, n
              do i = 1, m
                 rwork( ( j-1 )*m+i ) = real( b( i, j ),KIND=dp)
              end do
           end do
           l = m*n + 1_ilp
           call stdlib_dgemm( 'N', 'N', m, n, m, one, a, lda, rwork, m, zero,rwork( l ), m )
                     
           do j = 1, n
              do i = 1, m
                 c( i, j ) = rwork( l+( j-1 )*m+i-1 )
              end do
           end do
           do j = 1, n
              do i = 1, m
                 rwork( ( j-1 )*m+i ) = aimag( b( i, j ) )
              end do
           end do
           call stdlib_dgemm( 'N', 'N', m, n, m, one, a, lda, rwork, m, zero,rwork( l ), m )
                     
           do j = 1, n
              do i = 1, m
                 c( i, j ) = cmplx( real( c( i, j ),KIND=dp),rwork( l+( j-1 )*m+i-1 ),KIND=dp)
                           
              end do
           end do
           return
     end subroutine stdlib_zlarcm




     pure module subroutine stdlib_chfrk( transr, uplo, trans, n, k, alpha, a, lda, beta,c )
     !! Level 3 BLAS like routine for C in RFP Format.
     !! CHFRK performs one of the Hermitian rank--k operations
     !! C := alpha*A*A**H + beta*C,
     !! or
     !! C := alpha*A**H*A + beta*C,
     !! where alpha and beta are real scalars, C is an n--by--n Hermitian
     !! matrix and A is an n--by--k matrix in the first case and a k--by--n
     !! matrix in the second case.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           real(sp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: k, lda, n
           character, intent(in) :: trans, transr, uplo
           ! Array Arguments 
           complex(sp), intent(in) :: a(lda,*)
           complex(sp), intent(inout) :: c(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: lower, normaltransr, nisodd, notrans
           integer(ilp) :: info, nrowa, j, nk, n1, n2
           complex(sp) :: calpha, cbeta
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           normaltransr = stdlib_lsame( transr, 'N' )
           lower = stdlib_lsame( uplo, 'L' )
           notrans = stdlib_lsame( trans, 'N' )
           if( notrans ) then
              nrowa = n
           else
              nrowa = k
           end if
           if( .not.normaltransr .and. .not.stdlib_lsame( transr, 'C' ) ) then
              info = -1_ilp
           else if( .not.lower .and. .not.stdlib_lsame( uplo, 'U' ) ) then
              info = -2_ilp
           else if( .not.notrans .and. .not.stdlib_lsame( trans, 'C' ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( k<0_ilp ) then
              info = -5_ilp
           else if( lda<max( 1_ilp, nrowa ) ) then
              info = -8_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CHFRK ', -info )
              return
           end if
           ! quick return if possible.
           ! the quick return case: ((alpha==0).and.(beta/=zero)) is not
           ! done (it is in stdlib_cherk for example) and left in the general case.
           if( ( n==0_ilp ) .or. ( ( ( alpha==zero ) .or. ( k==0_ilp ) ) .and.( beta==one ) ) )&
                     return
           if( ( alpha==zero ) .and. ( beta==zero ) ) then
              do j = 1, ( ( n*( n+1 ) ) / 2 )
                 c( j ) = czero
              end do
              return
           end if
           calpha = cmplx( alpha, zero,KIND=sp)
           cbeta = cmplx( beta, zero,KIND=sp)
           ! c is n-by-n.
           ! if n is odd, set nisodd = .true., and n1 and n2.
           ! if n is even, nisodd = .false., and nk.
           if( mod( n, 2_ilp )==0_ilp ) then
              nisodd = .false.
              nk = n / 2_ilp
           else
              nisodd = .true.
              if( lower ) then
                 n2 = n / 2_ilp
                 n1 = n - n2
              else
                 n1 = n / 2_ilp
                 n2 = n - n1
              end if
           end if
           if( nisodd ) then
              ! n is odd
              if( normaltransr ) then
                 ! n is odd and transr = 'n'
                 if( lower ) then
                    ! n is odd, transr = 'n', and uplo = 'l'
                    if( notrans ) then
                       ! n is odd, transr = 'n', uplo = 'l', and trans = 'n'
                       call stdlib_cherk( 'L', 'N', n1, k, alpha, a( 1_ilp, 1_ilp ), lda,beta, c( 1_ilp ), n )
                                 
                       call stdlib_cherk( 'U', 'N', n2, k, alpha, a( n1+1, 1_ilp ), lda,beta, c( n+1 )&
                                 , n )
                       call stdlib_cgemm( 'N', 'C', n2, n1, k, calpha, a( n1+1, 1_ilp ),lda, a( 1_ilp, 1_ilp )&
                                 , lda, cbeta, c( n1+1 ), n )
                    else
                       ! n is odd, transr = 'n', uplo = 'l', and trans = 'c'
                       call stdlib_cherk( 'L', 'C', n1, k, alpha, a( 1_ilp, 1_ilp ), lda,beta, c( 1_ilp ), n )
                                 
                       call stdlib_cherk( 'U', 'C', n2, k, alpha, a( 1_ilp, n1+1 ), lda,beta, c( n+1 )&
                                 , n )
                       call stdlib_cgemm( 'C', 'N', n2, n1, k, calpha, a( 1_ilp, n1+1 ),lda, a( 1_ilp, 1_ilp )&
                                 , lda, cbeta, c( n1+1 ), n )
                    end if
                 else
                    ! n is odd, transr = 'n', and uplo = 'u'
                    if( notrans ) then
                       ! n is odd, transr = 'n', uplo = 'u', and trans = 'n'
                       call stdlib_cherk( 'L', 'N', n1, k, alpha, a( 1_ilp, 1_ilp ), lda,beta, c( n2+1 ), &
                                 n )
                       call stdlib_cherk( 'U', 'N', n2, k, alpha, a( n2, 1_ilp ), lda,beta, c( n1+1 ),&
                                  n )
                       call stdlib_cgemm( 'N', 'C', n1, n2, k, calpha, a( 1_ilp, 1_ilp ),lda, a( n2, 1_ilp ), &
                                 lda, cbeta, c( 1_ilp ), n )
                    else
                       ! n is odd, transr = 'n', uplo = 'u', and trans = 'c'
                       call stdlib_cherk( 'L', 'C', n1, k, alpha, a( 1_ilp, 1_ilp ), lda,beta, c( n2+1 ), &
                                 n )
                       call stdlib_cherk( 'U', 'C', n2, k, alpha, a( 1_ilp, n2 ), lda,beta, c( n1+1 ),&
                                  n )
                       call stdlib_cgemm( 'C', 'N', n1, n2, k, calpha, a( 1_ilp, 1_ilp ),lda, a( 1_ilp, n2 ), &
                                 lda, cbeta, c( 1_ilp ), n )
                    end if
                 end if
              else
                 ! n is odd, and transr = 'c'
                 if( lower ) then
                    ! n is odd, transr = 'c', and uplo = 'l'
                    if( notrans ) then
                       ! n is odd, transr = 'c', uplo = 'l', and trans = 'n'
                       call stdlib_cherk( 'U', 'N', n1, k, alpha, a( 1_ilp, 1_ilp ), lda,beta, c( 1_ilp ), n1 &
                                 )
                       call stdlib_cherk( 'L', 'N', n2, k, alpha, a( n1+1, 1_ilp ), lda,beta, c( 2_ilp ), &
                                 n1 )
                       call stdlib_cgemm( 'N', 'C', n1, n2, k, calpha, a( 1_ilp, 1_ilp ),lda, a( n1+1, 1_ilp )&
                                 , lda, cbeta,c( n1*n1+1 ), n1 )
                    else
                       ! n is odd, transr = 'c', uplo = 'l', and trans = 'c'
                       call stdlib_cherk( 'U', 'C', n1, k, alpha, a( 1_ilp, 1_ilp ), lda,beta, c( 1_ilp ), n1 &
                                 )
                       call stdlib_cherk( 'L', 'C', n2, k, alpha, a( 1_ilp, n1+1 ), lda,beta, c( 2_ilp ), &
                                 n1 )
                       call stdlib_cgemm( 'C', 'N', n1, n2, k, calpha, a( 1_ilp, 1_ilp ),lda, a( 1_ilp, n1+1 )&
                                 , lda, cbeta,c( n1*n1+1 ), n1 )
                    end if
                 else
                    ! n is odd, transr = 'c', and uplo = 'u'
                    if( notrans ) then
                       ! n is odd, transr = 'c', uplo = 'u', and trans = 'n'
                       call stdlib_cherk( 'U', 'N', n1, k, alpha, a( 1_ilp, 1_ilp ), lda,beta, c( n2*n2+1 &
                                 ), n2 )
                       call stdlib_cherk( 'L', 'N', n2, k, alpha, a( n1+1, 1_ilp ), lda,beta, c( &
                                 n1*n2+1 ), n2 )
                       call stdlib_cgemm( 'N', 'C', n2, n1, k, calpha, a( n1+1, 1_ilp ),lda, a( 1_ilp, 1_ilp )&
                                 , lda, cbeta, c( 1_ilp ), n2 )
                    else
                       ! n is odd, transr = 'c', uplo = 'u', and trans = 'c'
                       call stdlib_cherk( 'U', 'C', n1, k, alpha, a( 1_ilp, 1_ilp ), lda,beta, c( n2*n2+1 &
                                 ), n2 )
                       call stdlib_cherk( 'L', 'C', n2, k, alpha, a( 1_ilp, n1+1 ), lda,beta, c( &
                                 n1*n2+1 ), n2 )
                       call stdlib_cgemm( 'C', 'N', n2, n1, k, calpha, a( 1_ilp, n1+1 ),lda, a( 1_ilp, 1_ilp )&
                                 , lda, cbeta, c( 1_ilp ), n2 )
                    end if
                 end if
              end if
           else
              ! n is even
              if( normaltransr ) then
                 ! n is even and transr = 'n'
                 if( lower ) then
                    ! n is even, transr = 'n', and uplo = 'l'
                    if( notrans ) then
                       ! n is even, transr = 'n', uplo = 'l', and trans = 'n'
                       call stdlib_cherk( 'L', 'N', nk, k, alpha, a( 1_ilp, 1_ilp ), lda,beta, c( 2_ilp ), n+&
                                 1_ilp )
                       call stdlib_cherk( 'U', 'N', nk, k, alpha, a( nk+1, 1_ilp ), lda,beta, c( 1_ilp ), &
                                 n+1 )
                       call stdlib_cgemm( 'N', 'C', nk, nk, k, calpha, a( nk+1, 1_ilp ),lda, a( 1_ilp, 1_ilp )&
                                 , lda, cbeta, c( nk+2 ),n+1 )
                    else
                       ! n is even, transr = 'n', uplo = 'l', and trans = 'c'
                       call stdlib_cherk( 'L', 'C', nk, k, alpha, a( 1_ilp, 1_ilp ), lda,beta, c( 2_ilp ), n+&
                                 1_ilp )
                       call stdlib_cherk( 'U', 'C', nk, k, alpha, a( 1_ilp, nk+1 ), lda,beta, c( 1_ilp ), &
                                 n+1 )
                       call stdlib_cgemm( 'C', 'N', nk, nk, k, calpha, a( 1_ilp, nk+1 ),lda, a( 1_ilp, 1_ilp )&
                                 , lda, cbeta, c( nk+2 ),n+1 )
                    end if
                 else
                    ! n is even, transr = 'n', and uplo = 'u'
                    if( notrans ) then
                       ! n is even, transr = 'n', uplo = 'u', and trans = 'n'
                       call stdlib_cherk( 'L', 'N', nk, k, alpha, a( 1_ilp, 1_ilp ), lda,beta, c( nk+2 ), &
                                 n+1 )
                       call stdlib_cherk( 'U', 'N', nk, k, alpha, a( nk+1, 1_ilp ), lda,beta, c( nk+1 &
                                 ), n+1 )
                       call stdlib_cgemm( 'N', 'C', nk, nk, k, calpha, a( 1_ilp, 1_ilp ),lda, a( nk+1, 1_ilp )&
                                 , lda, cbeta, c( 1_ilp ),n+1 )
                    else
                       ! n is even, transr = 'n', uplo = 'u', and trans = 'c'
                       call stdlib_cherk( 'L', 'C', nk, k, alpha, a( 1_ilp, 1_ilp ), lda,beta, c( nk+2 ), &
                                 n+1 )
                       call stdlib_cherk( 'U', 'C', nk, k, alpha, a( 1_ilp, nk+1 ), lda,beta, c( nk+1 &
                                 ), n+1 )
                       call stdlib_cgemm( 'C', 'N', nk, nk, k, calpha, a( 1_ilp, 1_ilp ),lda, a( 1_ilp, nk+1 )&
                                 , lda, cbeta, c( 1_ilp ),n+1 )
                    end if
                 end if
              else
                 ! n is even, and transr = 'c'
                 if( lower ) then
                    ! n is even, transr = 'c', and uplo = 'l'
                    if( notrans ) then
                       ! n is even, transr = 'c', uplo = 'l', and trans = 'n'
                       call stdlib_cherk( 'U', 'N', nk, k, alpha, a( 1_ilp, 1_ilp ), lda,beta, c( nk+1 ), &
                                 nk )
                       call stdlib_cherk( 'L', 'N', nk, k, alpha, a( nk+1, 1_ilp ), lda,beta, c( 1_ilp ), &
                                 nk )
                       call stdlib_cgemm( 'N', 'C', nk, nk, k, calpha, a( 1_ilp, 1_ilp ),lda, a( nk+1, 1_ilp )&
                                 , lda, cbeta,c( ( ( nk+1 )*nk )+1_ilp ), nk )
                    else
                       ! n is even, transr = 'c', uplo = 'l', and trans = 'c'
                       call stdlib_cherk( 'U', 'C', nk, k, alpha, a( 1_ilp, 1_ilp ), lda,beta, c( nk+1 ), &
                                 nk )
                       call stdlib_cherk( 'L', 'C', nk, k, alpha, a( 1_ilp, nk+1 ), lda,beta, c( 1_ilp ), &
                                 nk )
                       call stdlib_cgemm( 'C', 'N', nk, nk, k, calpha, a( 1_ilp, 1_ilp ),lda, a( 1_ilp, nk+1 )&
                                 , lda, cbeta,c( ( ( nk+1 )*nk )+1_ilp ), nk )
                    end if
                 else
                    ! n is even, transr = 'c', and uplo = 'u'
                    if( notrans ) then
                       ! n is even, transr = 'c', uplo = 'u', and trans = 'n'
                       call stdlib_cherk( 'U', 'N', nk, k, alpha, a( 1_ilp, 1_ilp ), lda,beta, c( nk*( nk+&
                                 1_ilp )+1_ilp ), nk )
                       call stdlib_cherk( 'L', 'N', nk, k, alpha, a( nk+1, 1_ilp ), lda,beta, c( &
                                 nk*nk+1 ), nk )
                       call stdlib_cgemm( 'N', 'C', nk, nk, k, calpha, a( nk+1, 1_ilp ),lda, a( 1_ilp, 1_ilp )&
                                 , lda, cbeta, c( 1_ilp ), nk )
                    else
                       ! n is even, transr = 'c', uplo = 'u', and trans = 'c'
                       call stdlib_cherk( 'U', 'C', nk, k, alpha, a( 1_ilp, 1_ilp ), lda,beta, c( nk*( nk+&
                                 1_ilp )+1_ilp ), nk )
                       call stdlib_cherk( 'L', 'C', nk, k, alpha, a( 1_ilp, nk+1 ), lda,beta, c( &
                                 nk*nk+1 ), nk )
                       call stdlib_cgemm( 'C', 'N', nk, nk, k, calpha, a( 1_ilp, nk+1 ),lda, a( 1_ilp, 1_ilp )&
                                 , lda, cbeta, c( 1_ilp ), nk )
                    end if
                 end if
              end if
           end if
           return
     end subroutine stdlib_chfrk

     pure module subroutine stdlib_zhfrk( transr, uplo, trans, n, k, alpha, a, lda, beta,c )
     !! Level 3 BLAS like routine for C in RFP Format.
     !! ZHFRK performs one of the Hermitian rank--k operations
     !! C := alpha*A*A**H + beta*C,
     !! or
     !! C := alpha*A**H*A + beta*C,
     !! where alpha and beta are real scalars, C is an n--by--n Hermitian
     !! matrix and A is an n--by--k matrix in the first case and a k--by--n
     !! matrix in the second case.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           real(dp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: k, lda, n
           character, intent(in) :: trans, transr, uplo
           ! Array Arguments 
           complex(dp), intent(in) :: a(lda,*)
           complex(dp), intent(inout) :: c(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: lower, normaltransr, nisodd, notrans
           integer(ilp) :: info, nrowa, j, nk, n1, n2
           complex(dp) :: calpha, cbeta
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           normaltransr = stdlib_lsame( transr, 'N' )
           lower = stdlib_lsame( uplo, 'L' )
           notrans = stdlib_lsame( trans, 'N' )
           if( notrans ) then
              nrowa = n
           else
              nrowa = k
           end if
           if( .not.normaltransr .and. .not.stdlib_lsame( transr, 'C' ) ) then
              info = -1_ilp
           else if( .not.lower .and. .not.stdlib_lsame( uplo, 'U' ) ) then
              info = -2_ilp
           else if( .not.notrans .and. .not.stdlib_lsame( trans, 'C' ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( k<0_ilp ) then
              info = -5_ilp
           else if( lda<max( 1_ilp, nrowa ) ) then
              info = -8_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZHFRK ', -info )
              return
           end if
           ! quick return if possible.
           ! the quick return case: ((alpha==0).and.(beta/=zero)) is not
           ! done (it is in stdlib_zherk for example) and left in the general case.
           if( ( n==0_ilp ) .or. ( ( ( alpha==zero ) .or. ( k==0_ilp ) ) .and.( beta==one ) ) )&
                     return
           if( ( alpha==zero ) .and. ( beta==zero ) ) then
              do j = 1, ( ( n*( n+1 ) ) / 2 )
                 c( j ) = czero
              end do
              return
           end if
           calpha = cmplx( alpha, zero,KIND=dp)
           cbeta = cmplx( beta, zero,KIND=dp)
           ! c is n-by-n.
           ! if n is odd, set nisodd = .true., and n1 and n2.
           ! if n is even, nisodd = .false., and nk.
           if( mod( n, 2_ilp )==0_ilp ) then
              nisodd = .false.
              nk = n / 2_ilp
           else
              nisodd = .true.
              if( lower ) then
                 n2 = n / 2_ilp
                 n1 = n - n2
              else
                 n1 = n / 2_ilp
                 n2 = n - n1
              end if
           end if
           if( nisodd ) then
              ! n is odd
              if( normaltransr ) then
                 ! n is odd and transr = 'n'
                 if( lower ) then
                    ! n is odd, transr = 'n', and uplo = 'l'
                    if( notrans ) then
                       ! n is odd, transr = 'n', uplo = 'l', and trans = 'n'
                       call stdlib_zherk( 'L', 'N', n1, k, alpha, a( 1_ilp, 1_ilp ), lda,beta, c( 1_ilp ), n )
                                 
                       call stdlib_zherk( 'U', 'N', n2, k, alpha, a( n1+1, 1_ilp ), lda,beta, c( n+1 )&
                                 , n )
                       call stdlib_zgemm( 'N', 'C', n2, n1, k, calpha, a( n1+1, 1_ilp ),lda, a( 1_ilp, 1_ilp )&
                                 , lda, cbeta, c( n1+1 ), n )
                    else
                       ! n is odd, transr = 'n', uplo = 'l', and trans = 'c'
                       call stdlib_zherk( 'L', 'C', n1, k, alpha, a( 1_ilp, 1_ilp ), lda,beta, c( 1_ilp ), n )
                                 
                       call stdlib_zherk( 'U', 'C', n2, k, alpha, a( 1_ilp, n1+1 ), lda,beta, c( n+1 )&
                                 , n )
                       call stdlib_zgemm( 'C', 'N', n2, n1, k, calpha, a( 1_ilp, n1+1 ),lda, a( 1_ilp, 1_ilp )&
                                 , lda, cbeta, c( n1+1 ), n )
                    end if
                 else
                    ! n is odd, transr = 'n', and uplo = 'u'
                    if( notrans ) then
                       ! n is odd, transr = 'n', uplo = 'u', and trans = 'n'
                       call stdlib_zherk( 'L', 'N', n1, k, alpha, a( 1_ilp, 1_ilp ), lda,beta, c( n2+1 ), &
                                 n )
                       call stdlib_zherk( 'U', 'N', n2, k, alpha, a( n2, 1_ilp ), lda,beta, c( n1+1 ),&
                                  n )
                       call stdlib_zgemm( 'N', 'C', n1, n2, k, calpha, a( 1_ilp, 1_ilp ),lda, a( n2, 1_ilp ), &
                                 lda, cbeta, c( 1_ilp ), n )
                    else
                       ! n is odd, transr = 'n', uplo = 'u', and trans = 'c'
                       call stdlib_zherk( 'L', 'C', n1, k, alpha, a( 1_ilp, 1_ilp ), lda,beta, c( n2+1 ), &
                                 n )
                       call stdlib_zherk( 'U', 'C', n2, k, alpha, a( 1_ilp, n2 ), lda,beta, c( n1+1 ),&
                                  n )
                       call stdlib_zgemm( 'C', 'N', n1, n2, k, calpha, a( 1_ilp, 1_ilp ),lda, a( 1_ilp, n2 ), &
                                 lda, cbeta, c( 1_ilp ), n )
                    end if
                 end if
              else
                 ! n is odd, and transr = 'c'
                 if( lower ) then
                    ! n is odd, transr = 'c', and uplo = 'l'
                    if( notrans ) then
                       ! n is odd, transr = 'c', uplo = 'l', and trans = 'n'
                       call stdlib_zherk( 'U', 'N', n1, k, alpha, a( 1_ilp, 1_ilp ), lda,beta, c( 1_ilp ), n1 &
                                 )
                       call stdlib_zherk( 'L', 'N', n2, k, alpha, a( n1+1, 1_ilp ), lda,beta, c( 2_ilp ), &
                                 n1 )
                       call stdlib_zgemm( 'N', 'C', n1, n2, k, calpha, a( 1_ilp, 1_ilp ),lda, a( n1+1, 1_ilp )&
                                 , lda, cbeta,c( n1*n1+1 ), n1 )
                    else
                       ! n is odd, transr = 'c', uplo = 'l', and trans = 'c'
                       call stdlib_zherk( 'U', 'C', n1, k, alpha, a( 1_ilp, 1_ilp ), lda,beta, c( 1_ilp ), n1 &
                                 )
                       call stdlib_zherk( 'L', 'C', n2, k, alpha, a( 1_ilp, n1+1 ), lda,beta, c( 2_ilp ), &
                                 n1 )
                       call stdlib_zgemm( 'C', 'N', n1, n2, k, calpha, a( 1_ilp, 1_ilp ),lda, a( 1_ilp, n1+1 )&
                                 , lda, cbeta,c( n1*n1+1 ), n1 )
                    end if
                 else
                    ! n is odd, transr = 'c', and uplo = 'u'
                    if( notrans ) then
                       ! n is odd, transr = 'c', uplo = 'u', and trans = 'n'
                       call stdlib_zherk( 'U', 'N', n1, k, alpha, a( 1_ilp, 1_ilp ), lda,beta, c( n2*n2+1 &
                                 ), n2 )
                       call stdlib_zherk( 'L', 'N', n2, k, alpha, a( n1+1, 1_ilp ), lda,beta, c( &
                                 n1*n2+1 ), n2 )
                       call stdlib_zgemm( 'N', 'C', n2, n1, k, calpha, a( n1+1, 1_ilp ),lda, a( 1_ilp, 1_ilp )&
                                 , lda, cbeta, c( 1_ilp ), n2 )
                    else
                       ! n is odd, transr = 'c', uplo = 'u', and trans = 'c'
                       call stdlib_zherk( 'U', 'C', n1, k, alpha, a( 1_ilp, 1_ilp ), lda,beta, c( n2*n2+1 &
                                 ), n2 )
                       call stdlib_zherk( 'L', 'C', n2, k, alpha, a( 1_ilp, n1+1 ), lda,beta, c( &
                                 n1*n2+1 ), n2 )
                       call stdlib_zgemm( 'C', 'N', n2, n1, k, calpha, a( 1_ilp, n1+1 ),lda, a( 1_ilp, 1_ilp )&
                                 , lda, cbeta, c( 1_ilp ), n2 )
                    end if
                 end if
              end if
           else
              ! n is even
              if( normaltransr ) then
                 ! n is even and transr = 'n'
                 if( lower ) then
                    ! n is even, transr = 'n', and uplo = 'l'
                    if( notrans ) then
                       ! n is even, transr = 'n', uplo = 'l', and trans = 'n'
                       call stdlib_zherk( 'L', 'N', nk, k, alpha, a( 1_ilp, 1_ilp ), lda,beta, c( 2_ilp ), n+&
                                 1_ilp )
                       call stdlib_zherk( 'U', 'N', nk, k, alpha, a( nk+1, 1_ilp ), lda,beta, c( 1_ilp ), &
                                 n+1 )
                       call stdlib_zgemm( 'N', 'C', nk, nk, k, calpha, a( nk+1, 1_ilp ),lda, a( 1_ilp, 1_ilp )&
                                 , lda, cbeta, c( nk+2 ),n+1 )
                    else
                       ! n is even, transr = 'n', uplo = 'l', and trans = 'c'
                       call stdlib_zherk( 'L', 'C', nk, k, alpha, a( 1_ilp, 1_ilp ), lda,beta, c( 2_ilp ), n+&
                                 1_ilp )
                       call stdlib_zherk( 'U', 'C', nk, k, alpha, a( 1_ilp, nk+1 ), lda,beta, c( 1_ilp ), &
                                 n+1 )
                       call stdlib_zgemm( 'C', 'N', nk, nk, k, calpha, a( 1_ilp, nk+1 ),lda, a( 1_ilp, 1_ilp )&
                                 , lda, cbeta, c( nk+2 ),n+1 )
                    end if
                 else
                    ! n is even, transr = 'n', and uplo = 'u'
                    if( notrans ) then
                       ! n is even, transr = 'n', uplo = 'u', and trans = 'n'
                       call stdlib_zherk( 'L', 'N', nk, k, alpha, a( 1_ilp, 1_ilp ), lda,beta, c( nk+2 ), &
                                 n+1 )
                       call stdlib_zherk( 'U', 'N', nk, k, alpha, a( nk+1, 1_ilp ), lda,beta, c( nk+1 &
                                 ), n+1 )
                       call stdlib_zgemm( 'N', 'C', nk, nk, k, calpha, a( 1_ilp, 1_ilp ),lda, a( nk+1, 1_ilp )&
                                 , lda, cbeta, c( 1_ilp ),n+1 )
                    else
                       ! n is even, transr = 'n', uplo = 'u', and trans = 'c'
                       call stdlib_zherk( 'L', 'C', nk, k, alpha, a( 1_ilp, 1_ilp ), lda,beta, c( nk+2 ), &
                                 n+1 )
                       call stdlib_zherk( 'U', 'C', nk, k, alpha, a( 1_ilp, nk+1 ), lda,beta, c( nk+1 &
                                 ), n+1 )
                       call stdlib_zgemm( 'C', 'N', nk, nk, k, calpha, a( 1_ilp, 1_ilp ),lda, a( 1_ilp, nk+1 )&
                                 , lda, cbeta, c( 1_ilp ),n+1 )
                    end if
                 end if
              else
                 ! n is even, and transr = 'c'
                 if( lower ) then
                    ! n is even, transr = 'c', and uplo = 'l'
                    if( notrans ) then
                       ! n is even, transr = 'c', uplo = 'l', and trans = 'n'
                       call stdlib_zherk( 'U', 'N', nk, k, alpha, a( 1_ilp, 1_ilp ), lda,beta, c( nk+1 ), &
                                 nk )
                       call stdlib_zherk( 'L', 'N', nk, k, alpha, a( nk+1, 1_ilp ), lda,beta, c( 1_ilp ), &
                                 nk )
                       call stdlib_zgemm( 'N', 'C', nk, nk, k, calpha, a( 1_ilp, 1_ilp ),lda, a( nk+1, 1_ilp )&
                                 , lda, cbeta,c( ( ( nk+1 )*nk )+1_ilp ), nk )
                    else
                       ! n is even, transr = 'c', uplo = 'l', and trans = 'c'
                       call stdlib_zherk( 'U', 'C', nk, k, alpha, a( 1_ilp, 1_ilp ), lda,beta, c( nk+1 ), &
                                 nk )
                       call stdlib_zherk( 'L', 'C', nk, k, alpha, a( 1_ilp, nk+1 ), lda,beta, c( 1_ilp ), &
                                 nk )
                       call stdlib_zgemm( 'C', 'N', nk, nk, k, calpha, a( 1_ilp, 1_ilp ),lda, a( 1_ilp, nk+1 )&
                                 , lda, cbeta,c( ( ( nk+1 )*nk )+1_ilp ), nk )
                    end if
                 else
                    ! n is even, transr = 'c', and uplo = 'u'
                    if( notrans ) then
                       ! n is even, transr = 'c', uplo = 'u', and trans = 'n'
                       call stdlib_zherk( 'U', 'N', nk, k, alpha, a( 1_ilp, 1_ilp ), lda,beta, c( nk*( nk+&
                                 1_ilp )+1_ilp ), nk )
                       call stdlib_zherk( 'L', 'N', nk, k, alpha, a( nk+1, 1_ilp ), lda,beta, c( &
                                 nk*nk+1 ), nk )
                       call stdlib_zgemm( 'N', 'C', nk, nk, k, calpha, a( nk+1, 1_ilp ),lda, a( 1_ilp, 1_ilp )&
                                 , lda, cbeta, c( 1_ilp ), nk )
                    else
                       ! n is even, transr = 'c', uplo = 'u', and trans = 'c'
                       call stdlib_zherk( 'U', 'C', nk, k, alpha, a( 1_ilp, 1_ilp ), lda,beta, c( nk*( nk+&
                                 1_ilp )+1_ilp ), nk )
                       call stdlib_zherk( 'L', 'C', nk, k, alpha, a( 1_ilp, nk+1 ), lda,beta, c( &
                                 nk*nk+1 ), nk )
                       call stdlib_zgemm( 'C', 'N', nk, nk, k, calpha, a( 1_ilp, nk+1 ),lda, a( 1_ilp, 1_ilp )&
                                 , lda, cbeta, c( 1_ilp ), nk )
                    end if
                 end if
              end if
           end if
           return
     end subroutine stdlib_zhfrk




     pure module subroutine stdlib_stfsm( transr, side, uplo, trans, diag, m, n, alpha, a,b, ldb )
     !! Level 3 BLAS like routine for A in RFP Format.
     !! STFSM solves the matrix equation
     !! op( A )*X = alpha*B  or  X*op( A ) = alpha*B
     !! where alpha is a scalar, X and B are m by n matrices, A is a unit, or
     !! non-unit,  upper or lower triangular matrix  and  op( A )  is one  of
     !! op( A ) = A   or   op( A ) = A**T.
     !! A is in Rectangular Full Packed (RFP) Format.
     !! The matrix X is overwritten on B.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: transr, diag, side, trans, uplo
           integer(ilp), intent(in) :: ldb, m, n
           real(sp), intent(in) :: alpha
           ! Array Arguments 
           real(sp), intent(in) :: a(0_ilp:*)
           real(sp), intent(inout) :: b(0_ilp:ldb-1,0_ilp:*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lower, lside, misodd, nisodd, normaltransr, notrans
           integer(ilp) :: m1, m2, n1, n2, k, info, i, j
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           normaltransr = stdlib_lsame( transr, 'N' )
           lside = stdlib_lsame( side, 'L' )
           lower = stdlib_lsame( uplo, 'L' )
           notrans = stdlib_lsame( trans, 'N' )
           if( .not.normaltransr .and. .not.stdlib_lsame( transr, 'T' ) ) then
              info = -1_ilp
           else if( .not.lside .and. .not.stdlib_lsame( side, 'R' ) ) then
              info = -2_ilp
           else if( .not.lower .and. .not.stdlib_lsame( uplo, 'U' ) ) then
              info = -3_ilp
           else if( .not.notrans .and. .not.stdlib_lsame( trans, 'T' ) ) then
              info = -4_ilp
           else if( .not.stdlib_lsame( diag, 'N' ) .and. .not.stdlib_lsame( diag, 'U' ) )&
                     then
              info = -5_ilp
           else if( m<0_ilp ) then
              info = -6_ilp
           else if( n<0_ilp ) then
              info = -7_ilp
           else if( ldb<max( 1_ilp, m ) ) then
              info = -11_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'STFSM ', -info )
              return
           end if
           ! quick return when ( (n==0).or.(m==0) )
           if( ( m==0 ) .or. ( n==0 ) )return
           ! quick return when alpha==(0e+0_sp)
           if( alpha==zero ) then
              do j = 0, n - 1
                 do i = 0, m - 1
                    b( i, j ) = zero
                 end do
              end do
              return
           end if
           if( lside ) then
              ! side = 'l'
              ! a is m-by-m.
              ! if m is odd, set nisodd = .true., and m1 and m2.
              ! if m is even, nisodd = .false., and m.
              if( mod( m, 2_ilp )==0_ilp ) then
                 misodd = .false.
                 k = m / 2_ilp
              else
                 misodd = .true.
                 if( lower ) then
                    m2 = m / 2_ilp
                    m1 = m - m2
                 else
                    m1 = m / 2_ilp
                    m2 = m - m1
                 end if
              end if
              if( misodd ) then
                 ! side = 'l' and n is odd
                 if( normaltransr ) then
                    ! side = 'l', n is odd, and transr = 'n'
                    if( lower ) then
                       ! side  ='l', n is odd, transr = 'n', and uplo = 'l'
                       if( notrans ) then
                          ! side  ='l', n is odd, transr = 'n', uplo = 'l', and
                          ! trans = 'n'
                          if( m==1_ilp ) then
                             call stdlib_strsm( 'L', 'L', 'N', diag, m1, n, alpha,a, m, b, ldb )
                                       
                          else
                             call stdlib_strsm( 'L', 'L', 'N', diag, m1, n, alpha,a( 0_ilp ), m, b, &
                                       ldb )
                             call stdlib_sgemm( 'N', 'N', m2, n, m1, -one, a( m1 ),m, b, ldb, &
                                       alpha, b( m1, 0_ilp ), ldb )
                             call stdlib_strsm( 'L', 'U', 'T', diag, m2, n, one,a( m ), m, b( m1, &
                                       0_ilp ), ldb )
                          end if
                       else
                          ! side  ='l', n is odd, transr = 'n', uplo = 'l', and
                          ! trans = 't'
                          if( m==1_ilp ) then
                             call stdlib_strsm( 'L', 'L', 'T', diag, m1, n, alpha,a( 0_ilp ), m, b, &
                                       ldb )
                          else
                             call stdlib_strsm( 'L', 'U', 'N', diag, m2, n, alpha,a( m ), m, b( &
                                       m1, 0_ilp ), ldb )
                             call stdlib_sgemm( 'T', 'N', m1, n, m2, -one, a( m1 ),m, b( m1, 0_ilp ), &
                                       ldb, alpha, b, ldb )
                             call stdlib_strsm( 'L', 'L', 'T', diag, m1, n, one,a( 0_ilp ), m, b, ldb &
                                       )
                          end if
                       end if
                    else
                       ! side  ='l', n is odd, transr = 'n', and uplo = 'u'
                       if( .not.notrans ) then
                          ! side  ='l', n is odd, transr = 'n', uplo = 'u', and
                          ! trans = 'n'
                          call stdlib_strsm( 'L', 'L', 'N', diag, m1, n, alpha,a( m2 ), m, b, ldb &
                                    )
                          call stdlib_sgemm( 'T', 'N', m2, n, m1, -one, a( 0_ilp ), m,b, ldb, alpha, &
                                    b( m1, 0_ilp ), ldb )
                          call stdlib_strsm( 'L', 'U', 'T', diag, m2, n, one,a( m1 ), m, b( m1, 0_ilp &
                                    ), ldb )
                       else
                          ! side  ='l', n is odd, transr = 'n', uplo = 'u', and
                          ! trans = 't'
                          call stdlib_strsm( 'L', 'U', 'N', diag, m2, n, alpha,a( m1 ), m, b( m1, &
                                    0_ilp ), ldb )
                          call stdlib_sgemm( 'N', 'N', m1, n, m2, -one, a( 0_ilp ), m,b( m1, 0_ilp ), ldb,&
                                     alpha, b, ldb )
                          call stdlib_strsm( 'L', 'L', 'T', diag, m1, n, one,a( m2 ), m, b, ldb )
                                    
                       end if
                    end if
                 else
                    ! side = 'l', n is odd, and transr = 't'
                    if( lower ) then
                       ! side  ='l', n is odd, transr = 't', and uplo = 'l'
                       if( notrans ) then
                          ! side  ='l', n is odd, transr = 't', uplo = 'l', and
                          ! trans = 'n'
                          if( m==1_ilp ) then
                             call stdlib_strsm( 'L', 'U', 'T', diag, m1, n, alpha,a( 0_ilp ), m1, b, &
                                       ldb )
                          else
                             call stdlib_strsm( 'L', 'U', 'T', diag, m1, n, alpha,a( 0_ilp ), m1, b, &
                                       ldb )
                             call stdlib_sgemm( 'T', 'N', m2, n, m1, -one,a( m1*m1 ), m1, b, ldb, &
                                       alpha,b( m1, 0_ilp ), ldb )
                             call stdlib_strsm( 'L', 'L', 'N', diag, m2, n, one,a( 1_ilp ), m1, b( m1,&
                                        0_ilp ), ldb )
                          end if
                       else
                          ! side  ='l', n is odd, transr = 't', uplo = 'l', and
                          ! trans = 't'
                          if( m==1_ilp ) then
                             call stdlib_strsm( 'L', 'U', 'N', diag, m1, n, alpha,a( 0_ilp ), m1, b, &
                                       ldb )
                          else
                             call stdlib_strsm( 'L', 'L', 'T', diag, m2, n, alpha,a( 1_ilp ), m1, b( &
                                       m1, 0_ilp ), ldb )
                             call stdlib_sgemm( 'N', 'N', m1, n, m2, -one,a( m1*m1 ), m1, b( m1, &
                                       0_ilp ), ldb,alpha, b, ldb )
                             call stdlib_strsm( 'L', 'U', 'N', diag, m1, n, one,a( 0_ilp ), m1, b, &
                                       ldb )
                          end if
                       end if
                    else
                       ! side  ='l', n is odd, transr = 't', and uplo = 'u'
                       if( .not.notrans ) then
                          ! side  ='l', n is odd, transr = 't', uplo = 'u', and
                          ! trans = 'n'
                          call stdlib_strsm( 'L', 'U', 'T', diag, m1, n, alpha,a( m2*m2 ), m2, b, &
                                    ldb )
                          call stdlib_sgemm( 'N', 'N', m2, n, m1, -one, a( 0_ilp ), m2,b, ldb, alpha, &
                                    b( m1, 0_ilp ), ldb )
                          call stdlib_strsm( 'L', 'L', 'N', diag, m2, n, one,a( m1*m2 ), m2, b( &
                                    m1, 0_ilp ), ldb )
                       else
                          ! side  ='l', n is odd, transr = 't', uplo = 'u', and
                          ! trans = 't'
                          call stdlib_strsm( 'L', 'L', 'T', diag, m2, n, alpha,a( m1*m2 ), m2, b( &
                                    m1, 0_ilp ), ldb )
                          call stdlib_sgemm( 'T', 'N', m1, n, m2, -one, a( 0_ilp ), m2,b( m1, 0_ilp ), &
                                    ldb, alpha, b, ldb )
                          call stdlib_strsm( 'L', 'U', 'N', diag, m1, n, one,a( m2*m2 ), m2, b, &
                                    ldb )
                       end if
                    end if
                 end if
              else
                 ! side = 'l' and n is even
                 if( normaltransr ) then
                    ! side = 'l', n is even, and transr = 'n'
                    if( lower ) then
                       ! side  ='l', n is even, transr = 'n', and uplo = 'l'
                       if( notrans ) then
                          ! side  ='l', n is even, transr = 'n', uplo = 'l',
                          ! and trans = 'n'
                          call stdlib_strsm( 'L', 'L', 'N', diag, k, n, alpha,a( 1_ilp ), m+1, b, ldb &
                                    )
                          call stdlib_sgemm( 'N', 'N', k, n, k, -one, a( k+1 ),m+1, b, ldb, alpha,&
                                     b( k, 0_ilp ), ldb )
                          call stdlib_strsm( 'L', 'U', 'T', diag, k, n, one,a( 0_ilp ), m+1, b( k, 0_ilp )&
                                    , ldb )
                       else
                          ! side  ='l', n is even, transr = 'n', uplo = 'l',
                          ! and trans = 't'
                          call stdlib_strsm( 'L', 'U', 'N', diag, k, n, alpha,a( 0_ilp ), m+1, b( k, &
                                    0_ilp ), ldb )
                          call stdlib_sgemm( 'T', 'N', k, n, k, -one, a( k+1 ),m+1, b( k, 0_ilp ), &
                                    ldb, alpha, b, ldb )
                          call stdlib_strsm( 'L', 'L', 'T', diag, k, n, one,a( 1_ilp ), m+1, b, ldb )
                                    
                       end if
                    else
                       ! side  ='l', n is even, transr = 'n', and uplo = 'u'
                       if( .not.notrans ) then
                          ! side  ='l', n is even, transr = 'n', uplo = 'u',
                          ! and trans = 'n'
                          call stdlib_strsm( 'L', 'L', 'N', diag, k, n, alpha,a( k+1 ), m+1, b, &
                                    ldb )
                          call stdlib_sgemm( 'T', 'N', k, n, k, -one, a( 0_ilp ), m+1,b, ldb, alpha, &
                                    b( k, 0_ilp ), ldb )
                          call stdlib_strsm( 'L', 'U', 'T', diag, k, n, one,a( k ), m+1, b( k, 0_ilp )&
                                    , ldb )
                       else
                          ! side  ='l', n is even, transr = 'n', uplo = 'u',
                          ! and trans = 't'
                          call stdlib_strsm( 'L', 'U', 'N', diag, k, n, alpha,a( k ), m+1, b( k, &
                                    0_ilp ), ldb )
                          call stdlib_sgemm( 'N', 'N', k, n, k, -one, a( 0_ilp ), m+1,b( k, 0_ilp ), ldb, &
                                    alpha, b, ldb )
                          call stdlib_strsm( 'L', 'L', 'T', diag, k, n, one,a( k+1 ), m+1, b, ldb &
                                    )
                       end if
                    end if
                 else
                    ! side = 'l', n is even, and transr = 't'
                    if( lower ) then
                       ! side  ='l', n is even, transr = 't', and uplo = 'l'
                       if( notrans ) then
                          ! side  ='l', n is even, transr = 't', uplo = 'l',
                          ! and trans = 'n'
                          call stdlib_strsm( 'L', 'U', 'T', diag, k, n, alpha,a( k ), k, b, ldb )
                                    
                          call stdlib_sgemm( 'T', 'N', k, n, k, -one,a( k*( k+1 ) ), k, b, ldb, &
                                    alpha,b( k, 0_ilp ), ldb )
                          call stdlib_strsm( 'L', 'L', 'N', diag, k, n, one,a( 0_ilp ), k, b( k, 0_ilp ), &
                                    ldb )
                       else
                          ! side  ='l', n is even, transr = 't', uplo = 'l',
                          ! and trans = 't'
                          call stdlib_strsm( 'L', 'L', 'T', diag, k, n, alpha,a( 0_ilp ), k, b( k, 0_ilp )&
                                    , ldb )
                          call stdlib_sgemm( 'N', 'N', k, n, k, -one,a( k*( k+1 ) ), k, b( k, 0_ilp ),&
                                     ldb,alpha, b, ldb )
                          call stdlib_strsm( 'L', 'U', 'N', diag, k, n, one,a( k ), k, b, ldb )
                                    
                       end if
                    else
                       ! side  ='l', n is even, transr = 't', and uplo = 'u'
                       if( .not.notrans ) then
                          ! side  ='l', n is even, transr = 't', uplo = 'u',
                          ! and trans = 'n'
                          call stdlib_strsm( 'L', 'U', 'T', diag, k, n, alpha,a( k*( k+1 ) ), k, &
                                    b, ldb )
                          call stdlib_sgemm( 'N', 'N', k, n, k, -one, a( 0_ilp ), k, b,ldb, alpha, b( &
                                    k, 0_ilp ), ldb )
                          call stdlib_strsm( 'L', 'L', 'N', diag, k, n, one,a( k*k ), k, b( k, 0_ilp )&
                                    , ldb )
                       else
                          ! side  ='l', n is even, transr = 't', uplo = 'u',
                          ! and trans = 't'
                          call stdlib_strsm( 'L', 'L', 'T', diag, k, n, alpha,a( k*k ), k, b( k, &
                                    0_ilp ), ldb )
                          call stdlib_sgemm( 'T', 'N', k, n, k, -one, a( 0_ilp ), k,b( k, 0_ilp ), ldb, &
                                    alpha, b, ldb )
                          call stdlib_strsm( 'L', 'U', 'N', diag, k, n, one,a( k*( k+1 ) ), k, b, &
                                    ldb )
                       end if
                    end if
                 end if
              end if
           else
              ! side = 'r'
              ! a is n-by-n.
              ! if n is odd, set nisodd = .true., and n1 and n2.
              ! if n is even, nisodd = .false., and k.
              if( mod( n, 2_ilp )==0_ilp ) then
                 nisodd = .false.
                 k = n / 2_ilp
              else
                 nisodd = .true.
                 if( lower ) then
                    n2 = n / 2_ilp
                    n1 = n - n2
                 else
                    n1 = n / 2_ilp
                    n2 = n - n1
                 end if
              end if
              if( nisodd ) then
                 ! side = 'r' and n is odd
                 if( normaltransr ) then
                    ! side = 'r', n is odd, and transr = 'n'
                    if( lower ) then
                       ! side  ='r', n is odd, transr = 'n', and uplo = 'l'
                       if( notrans ) then
                          ! side  ='r', n is odd, transr = 'n', uplo = 'l', and
                          ! trans = 'n'
                          call stdlib_strsm( 'R', 'U', 'T', diag, m, n2, alpha,a( n ), n, b( 0_ilp, &
                                    n1 ), ldb )
                          call stdlib_sgemm( 'N', 'N', m, n1, n2, -one, b( 0_ilp, n1 ),ldb, a( n1 ), &
                                    n, alpha, b( 0_ilp, 0_ilp ),ldb )
                          call stdlib_strsm( 'R', 'L', 'N', diag, m, n1, one,a( 0_ilp ), n, b( 0_ilp, 0_ilp ),&
                                     ldb )
                       else
                          ! side  ='r', n is odd, transr = 'n', uplo = 'l', and
                          ! trans = 't'
                          call stdlib_strsm( 'R', 'L', 'T', diag, m, n1, alpha,a( 0_ilp ), n, b( 0_ilp, 0_ilp &
                                    ), ldb )
                          call stdlib_sgemm( 'N', 'T', m, n2, n1, -one, b( 0_ilp, 0_ilp ),ldb, a( n1 ), n,&
                                     alpha, b( 0_ilp, n1 ),ldb )
                          call stdlib_strsm( 'R', 'U', 'N', diag, m, n2, one,a( n ), n, b( 0_ilp, n1 )&
                                    , ldb )
                       end if
                    else
                       ! side  ='r', n is odd, transr = 'n', and uplo = 'u'
                       if( notrans ) then
                          ! side  ='r', n is odd, transr = 'n', uplo = 'u', and
                          ! trans = 'n'
                          call stdlib_strsm( 'R', 'L', 'T', diag, m, n1, alpha,a( n2 ), n, b( 0_ilp, &
                                    0_ilp ), ldb )
                          call stdlib_sgemm( 'N', 'N', m, n2, n1, -one, b( 0_ilp, 0_ilp ),ldb, a( 0_ilp ), n, &
                                    alpha, b( 0_ilp, n1 ),ldb )
                          call stdlib_strsm( 'R', 'U', 'N', diag, m, n2, one,a( n1 ), n, b( 0_ilp, n1 &
                                    ), ldb )
                       else
                          ! side  ='r', n is odd, transr = 'n', uplo = 'u', and
                          ! trans = 't'
                          call stdlib_strsm( 'R', 'U', 'T', diag, m, n2, alpha,a( n1 ), n, b( 0_ilp, &
                                    n1 ), ldb )
                          call stdlib_sgemm( 'N', 'T', m, n1, n2, -one, b( 0_ilp, n1 ),ldb, a( 0_ilp ), n,&
                                     alpha, b( 0_ilp, 0_ilp ), ldb )
                          call stdlib_strsm( 'R', 'L', 'N', diag, m, n1, one,a( n2 ), n, b( 0_ilp, 0_ilp )&
                                    , ldb )
                       end if
                    end if
                 else
                    ! side = 'r', n is odd, and transr = 't'
                    if( lower ) then
                       ! side  ='r', n is odd, transr = 't', and uplo = 'l'
                       if( notrans ) then
                          ! side  ='r', n is odd, transr = 't', uplo = 'l', and
                          ! trans = 'n'
                          call stdlib_strsm( 'R', 'L', 'N', diag, m, n2, alpha,a( 1_ilp ), n1, b( 0_ilp, &
                                    n1 ), ldb )
                          call stdlib_sgemm( 'N', 'T', m, n1, n2, -one, b( 0_ilp, n1 ),ldb, a( n1*n1 )&
                                    , n1, alpha, b( 0_ilp, 0_ilp ),ldb )
                          call stdlib_strsm( 'R', 'U', 'T', diag, m, n1, one,a( 0_ilp ), n1, b( 0_ilp, 0_ilp )&
                                    , ldb )
                       else
                          ! side  ='r', n is odd, transr = 't', uplo = 'l', and
                          ! trans = 't'
                          call stdlib_strsm( 'R', 'U', 'N', diag, m, n1, alpha,a( 0_ilp ), n1, b( 0_ilp, &
                                    0_ilp ), ldb )
                          call stdlib_sgemm( 'N', 'N', m, n2, n1, -one, b( 0_ilp, 0_ilp ),ldb, a( n1*n1 ),&
                                     n1, alpha, b( 0_ilp, n1 ),ldb )
                          call stdlib_strsm( 'R', 'L', 'T', diag, m, n2, one,a( 1_ilp ), n1, b( 0_ilp, n1 &
                                    ), ldb )
                       end if
                    else
                       ! side  ='r', n is odd, transr = 't', and uplo = 'u'
                       if( notrans ) then
                          ! side  ='r', n is odd, transr = 't', uplo = 'u', and
                          ! trans = 'n'
                          call stdlib_strsm( 'R', 'U', 'N', diag, m, n1, alpha,a( n2*n2 ), n2, b( &
                                    0_ilp, 0_ilp ), ldb )
                          call stdlib_sgemm( 'N', 'T', m, n2, n1, -one, b( 0_ilp, 0_ilp ),ldb, a( 0_ilp ), n2,&
                                     alpha, b( 0_ilp, n1 ),ldb )
                          call stdlib_strsm( 'R', 'L', 'T', diag, m, n2, one,a( n1*n2 ), n2, b( 0_ilp,&
                                     n1 ), ldb )
                       else
                          ! side  ='r', n is odd, transr = 't', uplo = 'u', and
                          ! trans = 't'
                          call stdlib_strsm( 'R', 'L', 'N', diag, m, n2, alpha,a( n1*n2 ), n2, b( &
                                    0_ilp, n1 ), ldb )
                          call stdlib_sgemm( 'N', 'N', m, n1, n2, -one, b( 0_ilp, n1 ),ldb, a( 0_ilp ), &
                                    n2, alpha, b( 0_ilp, 0_ilp ),ldb )
                          call stdlib_strsm( 'R', 'U', 'T', diag, m, n1, one,a( n2*n2 ), n2, b( 0_ilp,&
                                     0_ilp ), ldb )
                       end if
                    end if
                 end if
              else
                 ! side = 'r' and n is even
                 if( normaltransr ) then
                    ! side = 'r', n is even, and transr = 'n'
                    if( lower ) then
                       ! side  ='r', n is even, transr = 'n', and uplo = 'l'
                       if( notrans ) then
                          ! side  ='r', n is even, transr = 'n', uplo = 'l',
                          ! and trans = 'n'
                          call stdlib_strsm( 'R', 'U', 'T', diag, m, k, alpha,a( 0_ilp ), n+1, b( 0_ilp, &
                                    k ), ldb )
                          call stdlib_sgemm( 'N', 'N', m, k, k, -one, b( 0_ilp, k ),ldb, a( k+1 ), n+&
                                    1_ilp, alpha, b( 0_ilp, 0_ilp ),ldb )
                          call stdlib_strsm( 'R', 'L', 'N', diag, m, k, one,a( 1_ilp ), n+1, b( 0_ilp, 0_ilp )&
                                    , ldb )
                       else
                          ! side  ='r', n is even, transr = 'n', uplo = 'l',
                          ! and trans = 't'
                          call stdlib_strsm( 'R', 'L', 'T', diag, m, k, alpha,a( 1_ilp ), n+1, b( 0_ilp, &
                                    0_ilp ), ldb )
                          call stdlib_sgemm( 'N', 'T', m, k, k, -one, b( 0_ilp, 0_ilp ),ldb, a( k+1 ), n+&
                                    1_ilp, alpha, b( 0_ilp, k ),ldb )
                          call stdlib_strsm( 'R', 'U', 'N', diag, m, k, one,a( 0_ilp ), n+1, b( 0_ilp, k )&
                                    , ldb )
                       end if
                    else
                       ! side  ='r', n is even, transr = 'n', and uplo = 'u'
                       if( notrans ) then
                          ! side  ='r', n is even, transr = 'n', uplo = 'u',
                          ! and trans = 'n'
                          call stdlib_strsm( 'R', 'L', 'T', diag, m, k, alpha,a( k+1 ), n+1, b( 0_ilp,&
                                     0_ilp ), ldb )
                          call stdlib_sgemm( 'N', 'N', m, k, k, -one, b( 0_ilp, 0_ilp ),ldb, a( 0_ilp ), n+1, &
                                    alpha, b( 0_ilp, k ),ldb )
                          call stdlib_strsm( 'R', 'U', 'N', diag, m, k, one,a( k ), n+1, b( 0_ilp, k )&
                                    , ldb )
                       else
                          ! side  ='r', n is even, transr = 'n', uplo = 'u',
                          ! and trans = 't'
                          call stdlib_strsm( 'R', 'U', 'T', diag, m, k, alpha,a( k ), n+1, b( 0_ilp, &
                                    k ), ldb )
                          call stdlib_sgemm( 'N', 'T', m, k, k, -one, b( 0_ilp, k ),ldb, a( 0_ilp ), n+1, &
                                    alpha, b( 0_ilp, 0_ilp ),ldb )
                          call stdlib_strsm( 'R', 'L', 'N', diag, m, k, one,a( k+1 ), n+1, b( 0_ilp, &
                                    0_ilp ), ldb )
                       end if
                    end if
                 else
                    ! side = 'r', n is even, and transr = 't'
                    if( lower ) then
                       ! side  ='r', n is even, transr = 't', and uplo = 'l'
                       if( notrans ) then
                          ! side  ='r', n is even, transr = 't', uplo = 'l',
                          ! and trans = 'n'
                          call stdlib_strsm( 'R', 'L', 'N', diag, m, k, alpha,a( 0_ilp ), k, b( 0_ilp, k )&
                                    , ldb )
                          call stdlib_sgemm( 'N', 'T', m, k, k, -one, b( 0_ilp, k ),ldb, a( ( k+1 )*k &
                                    ), k, alpha,b( 0_ilp, 0_ilp ), ldb )
                          call stdlib_strsm( 'R', 'U', 'T', diag, m, k, one,a( k ), k, b( 0_ilp, 0_ilp ), &
                                    ldb )
                       else
                          ! side  ='r', n is even, transr = 't', uplo = 'l',
                          ! and trans = 't'
                          call stdlib_strsm( 'R', 'U', 'N', diag, m, k, alpha,a( k ), k, b( 0_ilp, 0_ilp )&
                                    , ldb )
                          call stdlib_sgemm( 'N', 'N', m, k, k, -one, b( 0_ilp, 0_ilp ),ldb, a( ( k+1 )*k &
                                    ), k, alpha,b( 0_ilp, k ), ldb )
                          call stdlib_strsm( 'R', 'L', 'T', diag, m, k, one,a( 0_ilp ), k, b( 0_ilp, k ), &
                                    ldb )
                       end if
                    else
                       ! side  ='r', n is even, transr = 't', and uplo = 'u'
                       if( notrans ) then
                          ! side  ='r', n is even, transr = 't', uplo = 'u',
                          ! and trans = 'n'
                          call stdlib_strsm( 'R', 'U', 'N', diag, m, k, alpha,a( ( k+1 )*k ), k, &
                                    b( 0_ilp, 0_ilp ), ldb )
                          call stdlib_sgemm( 'N', 'T', m, k, k, -one, b( 0_ilp, 0_ilp ),ldb, a( 0_ilp ), k, &
                                    alpha, b( 0_ilp, k ), ldb )
                          call stdlib_strsm( 'R', 'L', 'T', diag, m, k, one,a( k*k ), k, b( 0_ilp, k )&
                                    , ldb )
                       else
                          ! side  ='r', n is even, transr = 't', uplo = 'u',
                          ! and trans = 't'
                          call stdlib_strsm( 'R', 'L', 'N', diag, m, k, alpha,a( k*k ), k, b( 0_ilp, &
                                    k ), ldb )
                          call stdlib_sgemm( 'N', 'N', m, k, k, -one, b( 0_ilp, k ),ldb, a( 0_ilp ), k, &
                                    alpha, b( 0_ilp, 0_ilp ), ldb )
                          call stdlib_strsm( 'R', 'U', 'T', diag, m, k, one,a( ( k+1 )*k ), k, b( &
                                    0_ilp, 0_ilp ), ldb )
                       end if
                    end if
                 end if
              end if
           end if
           return
     end subroutine stdlib_stfsm

     pure module subroutine stdlib_dtfsm( transr, side, uplo, trans, diag, m, n, alpha, a,b, ldb )
     !! Level 3 BLAS like routine for A in RFP Format.
     !! DTFSM solves the matrix equation
     !! op( A )*X = alpha*B  or  X*op( A ) = alpha*B
     !! where alpha is a scalar, X and B are m by n matrices, A is a unit, or
     !! non-unit,  upper or lower triangular matrix  and  op( A )  is one  of
     !! op( A ) = A   or   op( A ) = A**T.
     !! A is in Rectangular Full Packed (RFP) Format.
     !! The matrix X is overwritten on B.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: transr, diag, side, trans, uplo
           integer(ilp), intent(in) :: ldb, m, n
           real(dp), intent(in) :: alpha
           ! Array Arguments 
           real(dp), intent(in) :: a(0_ilp:*)
           real(dp), intent(inout) :: b(0_ilp:ldb-1,0_ilp:*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lower, lside, misodd, nisodd, normaltransr, notrans
           integer(ilp) :: m1, m2, n1, n2, k, info, i, j
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           normaltransr = stdlib_lsame( transr, 'N' )
           lside = stdlib_lsame( side, 'L' )
           lower = stdlib_lsame( uplo, 'L' )
           notrans = stdlib_lsame( trans, 'N' )
           if( .not.normaltransr .and. .not.stdlib_lsame( transr, 'T' ) ) then
              info = -1_ilp
           else if( .not.lside .and. .not.stdlib_lsame( side, 'R' ) ) then
              info = -2_ilp
           else if( .not.lower .and. .not.stdlib_lsame( uplo, 'U' ) ) then
              info = -3_ilp
           else if( .not.notrans .and. .not.stdlib_lsame( trans, 'T' ) ) then
              info = -4_ilp
           else if( .not.stdlib_lsame( diag, 'N' ) .and. .not.stdlib_lsame( diag, 'U' ) )&
                     then
              info = -5_ilp
           else if( m<0_ilp ) then
              info = -6_ilp
           else if( n<0_ilp ) then
              info = -7_ilp
           else if( ldb<max( 1_ilp, m ) ) then
              info = -11_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DTFSM ', -info )
              return
           end if
           ! quick return when ( (n==0).or.(m==0) )
           if( ( m==0 ) .or. ( n==0 ) )return
           ! quick return when alpha==(0e+0_dp)
           if( alpha==zero ) then
              do j = 0, n - 1
                 do i = 0, m - 1
                    b( i, j ) = zero
                 end do
              end do
              return
           end if
           if( lside ) then
              ! side = 'l'
              ! a is m-by-m.
              ! if m is odd, set nisodd = .true., and m1 and m2.
              ! if m is even, nisodd = .false., and m.
              if( mod( m, 2_ilp )==0_ilp ) then
                 misodd = .false.
                 k = m / 2_ilp
              else
                 misodd = .true.
                 if( lower ) then
                    m2 = m / 2_ilp
                    m1 = m - m2
                 else
                    m1 = m / 2_ilp
                    m2 = m - m1
                 end if
              end if
              if( misodd ) then
                 ! side = 'l' and n is odd
                 if( normaltransr ) then
                    ! side = 'l', n is odd, and transr = 'n'
                    if( lower ) then
                       ! side  ='l', n is odd, transr = 'n', and uplo = 'l'
                       if( notrans ) then
                          ! side  ='l', n is odd, transr = 'n', uplo = 'l', and
                          ! trans = 'n'
                          if( m==1_ilp ) then
                             call stdlib_dtrsm( 'L', 'L', 'N', diag, m1, n, alpha,a, m, b, ldb )
                                       
                          else
                             call stdlib_dtrsm( 'L', 'L', 'N', diag, m1, n, alpha,a( 0_ilp ), m, b, &
                                       ldb )
                             call stdlib_dgemm( 'N', 'N', m2, n, m1, -one, a( m1 ),m, b, ldb, &
                                       alpha, b( m1, 0_ilp ), ldb )
                             call stdlib_dtrsm( 'L', 'U', 'T', diag, m2, n, one,a( m ), m, b( m1, &
                                       0_ilp ), ldb )
                          end if
                       else
                          ! side  ='l', n is odd, transr = 'n', uplo = 'l', and
                          ! trans = 't'
                          if( m==1_ilp ) then
                             call stdlib_dtrsm( 'L', 'L', 'T', diag, m1, n, alpha,a( 0_ilp ), m, b, &
                                       ldb )
                          else
                             call stdlib_dtrsm( 'L', 'U', 'N', diag, m2, n, alpha,a( m ), m, b( &
                                       m1, 0_ilp ), ldb )
                             call stdlib_dgemm( 'T', 'N', m1, n, m2, -one, a( m1 ),m, b( m1, 0_ilp ), &
                                       ldb, alpha, b, ldb )
                             call stdlib_dtrsm( 'L', 'L', 'T', diag, m1, n, one,a( 0_ilp ), m, b, ldb &
                                       )
                          end if
                       end if
                    else
                       ! side  ='l', n is odd, transr = 'n', and uplo = 'u'
                       if( .not.notrans ) then
                          ! side  ='l', n is odd, transr = 'n', uplo = 'u', and
                          ! trans = 'n'
                          call stdlib_dtrsm( 'L', 'L', 'N', diag, m1, n, alpha,a( m2 ), m, b, ldb &
                                    )
                          call stdlib_dgemm( 'T', 'N', m2, n, m1, -one, a( 0_ilp ), m,b, ldb, alpha, &
                                    b( m1, 0_ilp ), ldb )
                          call stdlib_dtrsm( 'L', 'U', 'T', diag, m2, n, one,a( m1 ), m, b( m1, 0_ilp &
                                    ), ldb )
                       else
                          ! side  ='l', n is odd, transr = 'n', uplo = 'u', and
                          ! trans = 't'
                          call stdlib_dtrsm( 'L', 'U', 'N', diag, m2, n, alpha,a( m1 ), m, b( m1, &
                                    0_ilp ), ldb )
                          call stdlib_dgemm( 'N', 'N', m1, n, m2, -one, a( 0_ilp ), m,b( m1, 0_ilp ), ldb,&
                                     alpha, b, ldb )
                          call stdlib_dtrsm( 'L', 'L', 'T', diag, m1, n, one,a( m2 ), m, b, ldb )
                                    
                       end if
                    end if
                 else
                    ! side = 'l', n is odd, and transr = 't'
                    if( lower ) then
                       ! side  ='l', n is odd, transr = 't', and uplo = 'l'
                       if( notrans ) then
                          ! side  ='l', n is odd, transr = 't', uplo = 'l', and
                          ! trans = 'n'
                          if( m==1_ilp ) then
                             call stdlib_dtrsm( 'L', 'U', 'T', diag, m1, n, alpha,a( 0_ilp ), m1, b, &
                                       ldb )
                          else
                             call stdlib_dtrsm( 'L', 'U', 'T', diag, m1, n, alpha,a( 0_ilp ), m1, b, &
                                       ldb )
                             call stdlib_dgemm( 'T', 'N', m2, n, m1, -one,a( m1*m1 ), m1, b, ldb, &
                                       alpha,b( m1, 0_ilp ), ldb )
                             call stdlib_dtrsm( 'L', 'L', 'N', diag, m2, n, one,a( 1_ilp ), m1, b( m1,&
                                        0_ilp ), ldb )
                          end if
                       else
                          ! side  ='l', n is odd, transr = 't', uplo = 'l', and
                          ! trans = 't'
                          if( m==1_ilp ) then
                             call stdlib_dtrsm( 'L', 'U', 'N', diag, m1, n, alpha,a( 0_ilp ), m1, b, &
                                       ldb )
                          else
                             call stdlib_dtrsm( 'L', 'L', 'T', diag, m2, n, alpha,a( 1_ilp ), m1, b( &
                                       m1, 0_ilp ), ldb )
                             call stdlib_dgemm( 'N', 'N', m1, n, m2, -one,a( m1*m1 ), m1, b( m1, &
                                       0_ilp ), ldb,alpha, b, ldb )
                             call stdlib_dtrsm( 'L', 'U', 'N', diag, m1, n, one,a( 0_ilp ), m1, b, &
                                       ldb )
                          end if
                       end if
                    else
                       ! side  ='l', n is odd, transr = 't', and uplo = 'u'
                       if( .not.notrans ) then
                          ! side  ='l', n is odd, transr = 't', uplo = 'u', and
                          ! trans = 'n'
                          call stdlib_dtrsm( 'L', 'U', 'T', diag, m1, n, alpha,a( m2*m2 ), m2, b, &
                                    ldb )
                          call stdlib_dgemm( 'N', 'N', m2, n, m1, -one, a( 0_ilp ), m2,b, ldb, alpha, &
                                    b( m1, 0_ilp ), ldb )
                          call stdlib_dtrsm( 'L', 'L', 'N', diag, m2, n, one,a( m1*m2 ), m2, b( &
                                    m1, 0_ilp ), ldb )
                       else
                          ! side  ='l', n is odd, transr = 't', uplo = 'u', and
                          ! trans = 't'
                          call stdlib_dtrsm( 'L', 'L', 'T', diag, m2, n, alpha,a( m1*m2 ), m2, b( &
                                    m1, 0_ilp ), ldb )
                          call stdlib_dgemm( 'T', 'N', m1, n, m2, -one, a( 0_ilp ), m2,b( m1, 0_ilp ), &
                                    ldb, alpha, b, ldb )
                          call stdlib_dtrsm( 'L', 'U', 'N', diag, m1, n, one,a( m2*m2 ), m2, b, &
                                    ldb )
                       end if
                    end if
                 end if
              else
                 ! side = 'l' and n is even
                 if( normaltransr ) then
                    ! side = 'l', n is even, and transr = 'n'
                    if( lower ) then
                       ! side  ='l', n is even, transr = 'n', and uplo = 'l'
                       if( notrans ) then
                          ! side  ='l', n is even, transr = 'n', uplo = 'l',
                          ! and trans = 'n'
                          call stdlib_dtrsm( 'L', 'L', 'N', diag, k, n, alpha,a( 1_ilp ), m+1, b, ldb &
                                    )
                          call stdlib_dgemm( 'N', 'N', k, n, k, -one, a( k+1 ),m+1, b, ldb, alpha,&
                                     b( k, 0_ilp ), ldb )
                          call stdlib_dtrsm( 'L', 'U', 'T', diag, k, n, one,a( 0_ilp ), m+1, b( k, 0_ilp )&
                                    , ldb )
                       else
                          ! side  ='l', n is even, transr = 'n', uplo = 'l',
                          ! and trans = 't'
                          call stdlib_dtrsm( 'L', 'U', 'N', diag, k, n, alpha,a( 0_ilp ), m+1, b( k, &
                                    0_ilp ), ldb )
                          call stdlib_dgemm( 'T', 'N', k, n, k, -one, a( k+1 ),m+1, b( k, 0_ilp ), &
                                    ldb, alpha, b, ldb )
                          call stdlib_dtrsm( 'L', 'L', 'T', diag, k, n, one,a( 1_ilp ), m+1, b, ldb )
                                    
                       end if
                    else
                       ! side  ='l', n is even, transr = 'n', and uplo = 'u'
                       if( .not.notrans ) then
                          ! side  ='l', n is even, transr = 'n', uplo = 'u',
                          ! and trans = 'n'
                          call stdlib_dtrsm( 'L', 'L', 'N', diag, k, n, alpha,a( k+1 ), m+1, b, &
                                    ldb )
                          call stdlib_dgemm( 'T', 'N', k, n, k, -one, a( 0_ilp ), m+1,b, ldb, alpha, &
                                    b( k, 0_ilp ), ldb )
                          call stdlib_dtrsm( 'L', 'U', 'T', diag, k, n, one,a( k ), m+1, b( k, 0_ilp )&
                                    , ldb )
                       else
                          ! side  ='l', n is even, transr = 'n', uplo = 'u',
                          ! and trans = 't'
                          call stdlib_dtrsm( 'L', 'U', 'N', diag, k, n, alpha,a( k ), m+1, b( k, &
                                    0_ilp ), ldb )
                          call stdlib_dgemm( 'N', 'N', k, n, k, -one, a( 0_ilp ), m+1,b( k, 0_ilp ), ldb, &
                                    alpha, b, ldb )
                          call stdlib_dtrsm( 'L', 'L', 'T', diag, k, n, one,a( k+1 ), m+1, b, ldb &
                                    )
                       end if
                    end if
                 else
                    ! side = 'l', n is even, and transr = 't'
                    if( lower ) then
                       ! side  ='l', n is even, transr = 't', and uplo = 'l'
                       if( notrans ) then
                          ! side  ='l', n is even, transr = 't', uplo = 'l',
                          ! and trans = 'n'
                          call stdlib_dtrsm( 'L', 'U', 'T', diag, k, n, alpha,a( k ), k, b, ldb )
                                    
                          call stdlib_dgemm( 'T', 'N', k, n, k, -one,a( k*( k+1 ) ), k, b, ldb, &
                                    alpha,b( k, 0_ilp ), ldb )
                          call stdlib_dtrsm( 'L', 'L', 'N', diag, k, n, one,a( 0_ilp ), k, b( k, 0_ilp ), &
                                    ldb )
                       else
                          ! side  ='l', n is even, transr = 't', uplo = 'l',
                          ! and trans = 't'
                          call stdlib_dtrsm( 'L', 'L', 'T', diag, k, n, alpha,a( 0_ilp ), k, b( k, 0_ilp )&
                                    , ldb )
                          call stdlib_dgemm( 'N', 'N', k, n, k, -one,a( k*( k+1 ) ), k, b( k, 0_ilp ),&
                                     ldb,alpha, b, ldb )
                          call stdlib_dtrsm( 'L', 'U', 'N', diag, k, n, one,a( k ), k, b, ldb )
                                    
                       end if
                    else
                       ! side  ='l', n is even, transr = 't', and uplo = 'u'
                       if( .not.notrans ) then
                          ! side  ='l', n is even, transr = 't', uplo = 'u',
                          ! and trans = 'n'
                          call stdlib_dtrsm( 'L', 'U', 'T', diag, k, n, alpha,a( k*( k+1 ) ), k, &
                                    b, ldb )
                          call stdlib_dgemm( 'N', 'N', k, n, k, -one, a( 0_ilp ), k, b,ldb, alpha, b( &
                                    k, 0_ilp ), ldb )
                          call stdlib_dtrsm( 'L', 'L', 'N', diag, k, n, one,a( k*k ), k, b( k, 0_ilp )&
                                    , ldb )
                       else
                          ! side  ='l', n is even, transr = 't', uplo = 'u',
                          ! and trans = 't'
                          call stdlib_dtrsm( 'L', 'L', 'T', diag, k, n, alpha,a( k*k ), k, b( k, &
                                    0_ilp ), ldb )
                          call stdlib_dgemm( 'T', 'N', k, n, k, -one, a( 0_ilp ), k,b( k, 0_ilp ), ldb, &
                                    alpha, b, ldb )
                          call stdlib_dtrsm( 'L', 'U', 'N', diag, k, n, one,a( k*( k+1 ) ), k, b, &
                                    ldb )
                       end if
                    end if
                 end if
              end if
           else
              ! side = 'r'
              ! a is n-by-n.
              ! if n is odd, set nisodd = .true., and n1 and n2.
              ! if n is even, nisodd = .false., and k.
              if( mod( n, 2_ilp )==0_ilp ) then
                 nisodd = .false.
                 k = n / 2_ilp
              else
                 nisodd = .true.
                 if( lower ) then
                    n2 = n / 2_ilp
                    n1 = n - n2
                 else
                    n1 = n / 2_ilp
                    n2 = n - n1
                 end if
              end if
              if( nisodd ) then
                 ! side = 'r' and n is odd
                 if( normaltransr ) then
                    ! side = 'r', n is odd, and transr = 'n'
                    if( lower ) then
                       ! side  ='r', n is odd, transr = 'n', and uplo = 'l'
                       if( notrans ) then
                          ! side  ='r', n is odd, transr = 'n', uplo = 'l', and
                          ! trans = 'n'
                          call stdlib_dtrsm( 'R', 'U', 'T', diag, m, n2, alpha,a( n ), n, b( 0_ilp, &
                                    n1 ), ldb )
                          call stdlib_dgemm( 'N', 'N', m, n1, n2, -one, b( 0_ilp, n1 ),ldb, a( n1 ), &
                                    n, alpha, b( 0_ilp, 0_ilp ),ldb )
                          call stdlib_dtrsm( 'R', 'L', 'N', diag, m, n1, one,a( 0_ilp ), n, b( 0_ilp, 0_ilp ),&
                                     ldb )
                       else
                          ! side  ='r', n is odd, transr = 'n', uplo = 'l', and
                          ! trans = 't'
                          call stdlib_dtrsm( 'R', 'L', 'T', diag, m, n1, alpha,a( 0_ilp ), n, b( 0_ilp, 0_ilp &
                                    ), ldb )
                          call stdlib_dgemm( 'N', 'T', m, n2, n1, -one, b( 0_ilp, 0_ilp ),ldb, a( n1 ), n,&
                                     alpha, b( 0_ilp, n1 ),ldb )
                          call stdlib_dtrsm( 'R', 'U', 'N', diag, m, n2, one,a( n ), n, b( 0_ilp, n1 )&
                                    , ldb )
                       end if
                    else
                       ! side  ='r', n is odd, transr = 'n', and uplo = 'u'
                       if( notrans ) then
                          ! side  ='r', n is odd, transr = 'n', uplo = 'u', and
                          ! trans = 'n'
                          call stdlib_dtrsm( 'R', 'L', 'T', diag, m, n1, alpha,a( n2 ), n, b( 0_ilp, &
                                    0_ilp ), ldb )
                          call stdlib_dgemm( 'N', 'N', m, n2, n1, -one, b( 0_ilp, 0_ilp ),ldb, a( 0_ilp ), n, &
                                    alpha, b( 0_ilp, n1 ),ldb )
                          call stdlib_dtrsm( 'R', 'U', 'N', diag, m, n2, one,a( n1 ), n, b( 0_ilp, n1 &
                                    ), ldb )
                       else
                          ! side  ='r', n is odd, transr = 'n', uplo = 'u', and
                          ! trans = 't'
                          call stdlib_dtrsm( 'R', 'U', 'T', diag, m, n2, alpha,a( n1 ), n, b( 0_ilp, &
                                    n1 ), ldb )
                          call stdlib_dgemm( 'N', 'T', m, n1, n2, -one, b( 0_ilp, n1 ),ldb, a( 0_ilp ), n,&
                                     alpha, b( 0_ilp, 0_ilp ), ldb )
                          call stdlib_dtrsm( 'R', 'L', 'N', diag, m, n1, one,a( n2 ), n, b( 0_ilp, 0_ilp )&
                                    , ldb )
                       end if
                    end if
                 else
                    ! side = 'r', n is odd, and transr = 't'
                    if( lower ) then
                       ! side  ='r', n is odd, transr = 't', and uplo = 'l'
                       if( notrans ) then
                          ! side  ='r', n is odd, transr = 't', uplo = 'l', and
                          ! trans = 'n'
                          call stdlib_dtrsm( 'R', 'L', 'N', diag, m, n2, alpha,a( 1_ilp ), n1, b( 0_ilp, &
                                    n1 ), ldb )
                          call stdlib_dgemm( 'N', 'T', m, n1, n2, -one, b( 0_ilp, n1 ),ldb, a( n1*n1 )&
                                    , n1, alpha, b( 0_ilp, 0_ilp ),ldb )
                          call stdlib_dtrsm( 'R', 'U', 'T', diag, m, n1, one,a( 0_ilp ), n1, b( 0_ilp, 0_ilp )&
                                    , ldb )
                       else
                          ! side  ='r', n is odd, transr = 't', uplo = 'l', and
                          ! trans = 't'
                          call stdlib_dtrsm( 'R', 'U', 'N', diag, m, n1, alpha,a( 0_ilp ), n1, b( 0_ilp, &
                                    0_ilp ), ldb )
                          call stdlib_dgemm( 'N', 'N', m, n2, n1, -one, b( 0_ilp, 0_ilp ),ldb, a( n1*n1 ),&
                                     n1, alpha, b( 0_ilp, n1 ),ldb )
                          call stdlib_dtrsm( 'R', 'L', 'T', diag, m, n2, one,a( 1_ilp ), n1, b( 0_ilp, n1 &
                                    ), ldb )
                       end if
                    else
                       ! side  ='r', n is odd, transr = 't', and uplo = 'u'
                       if( notrans ) then
                          ! side  ='r', n is odd, transr = 't', uplo = 'u', and
                          ! trans = 'n'
                          call stdlib_dtrsm( 'R', 'U', 'N', diag, m, n1, alpha,a( n2*n2 ), n2, b( &
                                    0_ilp, 0_ilp ), ldb )
                          call stdlib_dgemm( 'N', 'T', m, n2, n1, -one, b( 0_ilp, 0_ilp ),ldb, a( 0_ilp ), n2,&
                                     alpha, b( 0_ilp, n1 ),ldb )
                          call stdlib_dtrsm( 'R', 'L', 'T', diag, m, n2, one,a( n1*n2 ), n2, b( 0_ilp,&
                                     n1 ), ldb )
                       else
                          ! side  ='r', n is odd, transr = 't', uplo = 'u', and
                          ! trans = 't'
                          call stdlib_dtrsm( 'R', 'L', 'N', diag, m, n2, alpha,a( n1*n2 ), n2, b( &
                                    0_ilp, n1 ), ldb )
                          call stdlib_dgemm( 'N', 'N', m, n1, n2, -one, b( 0_ilp, n1 ),ldb, a( 0_ilp ), &
                                    n2, alpha, b( 0_ilp, 0_ilp ),ldb )
                          call stdlib_dtrsm( 'R', 'U', 'T', diag, m, n1, one,a( n2*n2 ), n2, b( 0_ilp,&
                                     0_ilp ), ldb )
                       end if
                    end if
                 end if
              else
                 ! side = 'r' and n is even
                 if( normaltransr ) then
                    ! side = 'r', n is even, and transr = 'n'
                    if( lower ) then
                       ! side  ='r', n is even, transr = 'n', and uplo = 'l'
                       if( notrans ) then
                          ! side  ='r', n is even, transr = 'n', uplo = 'l',
                          ! and trans = 'n'
                          call stdlib_dtrsm( 'R', 'U', 'T', diag, m, k, alpha,a( 0_ilp ), n+1, b( 0_ilp, &
                                    k ), ldb )
                          call stdlib_dgemm( 'N', 'N', m, k, k, -one, b( 0_ilp, k ),ldb, a( k+1 ), n+&
                                    1_ilp, alpha, b( 0_ilp, 0_ilp ),ldb )
                          call stdlib_dtrsm( 'R', 'L', 'N', diag, m, k, one,a( 1_ilp ), n+1, b( 0_ilp, 0_ilp )&
                                    , ldb )
                       else
                          ! side  ='r', n is even, transr = 'n', uplo = 'l',
                          ! and trans = 't'
                          call stdlib_dtrsm( 'R', 'L', 'T', diag, m, k, alpha,a( 1_ilp ), n+1, b( 0_ilp, &
                                    0_ilp ), ldb )
                          call stdlib_dgemm( 'N', 'T', m, k, k, -one, b( 0_ilp, 0_ilp ),ldb, a( k+1 ), n+&
                                    1_ilp, alpha, b( 0_ilp, k ),ldb )
                          call stdlib_dtrsm( 'R', 'U', 'N', diag, m, k, one,a( 0_ilp ), n+1, b( 0_ilp, k )&
                                    , ldb )
                       end if
                    else
                       ! side  ='r', n is even, transr = 'n', and uplo = 'u'
                       if( notrans ) then
                          ! side  ='r', n is even, transr = 'n', uplo = 'u',
                          ! and trans = 'n'
                          call stdlib_dtrsm( 'R', 'L', 'T', diag, m, k, alpha,a( k+1 ), n+1, b( 0_ilp,&
                                     0_ilp ), ldb )
                          call stdlib_dgemm( 'N', 'N', m, k, k, -one, b( 0_ilp, 0_ilp ),ldb, a( 0_ilp ), n+1, &
                                    alpha, b( 0_ilp, k ),ldb )
                          call stdlib_dtrsm( 'R', 'U', 'N', diag, m, k, one,a( k ), n+1, b( 0_ilp, k )&
                                    , ldb )
                       else
                          ! side  ='r', n is even, transr = 'n', uplo = 'u',
                          ! and trans = 't'
                          call stdlib_dtrsm( 'R', 'U', 'T', diag, m, k, alpha,a( k ), n+1, b( 0_ilp, &
                                    k ), ldb )
                          call stdlib_dgemm( 'N', 'T', m, k, k, -one, b( 0_ilp, k ),ldb, a( 0_ilp ), n+1, &
                                    alpha, b( 0_ilp, 0_ilp ),ldb )
                          call stdlib_dtrsm( 'R', 'L', 'N', diag, m, k, one,a( k+1 ), n+1, b( 0_ilp, &
                                    0_ilp ), ldb )
                       end if
                    end if
                 else
                    ! side = 'r', n is even, and transr = 't'
                    if( lower ) then
                       ! side  ='r', n is even, transr = 't', and uplo = 'l'
                       if( notrans ) then
                          ! side  ='r', n is even, transr = 't', uplo = 'l',
                          ! and trans = 'n'
                          call stdlib_dtrsm( 'R', 'L', 'N', diag, m, k, alpha,a( 0_ilp ), k, b( 0_ilp, k )&
                                    , ldb )
                          call stdlib_dgemm( 'N', 'T', m, k, k, -one, b( 0_ilp, k ),ldb, a( ( k+1 )*k &
                                    ), k, alpha,b( 0_ilp, 0_ilp ), ldb )
                          call stdlib_dtrsm( 'R', 'U', 'T', diag, m, k, one,a( k ), k, b( 0_ilp, 0_ilp ), &
                                    ldb )
                       else
                          ! side  ='r', n is even, transr = 't', uplo = 'l',
                          ! and trans = 't'
                          call stdlib_dtrsm( 'R', 'U', 'N', diag, m, k, alpha,a( k ), k, b( 0_ilp, 0_ilp )&
                                    , ldb )
                          call stdlib_dgemm( 'N', 'N', m, k, k, -one, b( 0_ilp, 0_ilp ),ldb, a( ( k+1 )*k &
                                    ), k, alpha,b( 0_ilp, k ), ldb )
                          call stdlib_dtrsm( 'R', 'L', 'T', diag, m, k, one,a( 0_ilp ), k, b( 0_ilp, k ), &
                                    ldb )
                       end if
                    else
                       ! side  ='r', n is even, transr = 't', and uplo = 'u'
                       if( notrans ) then
                          ! side  ='r', n is even, transr = 't', uplo = 'u',
                          ! and trans = 'n'
                          call stdlib_dtrsm( 'R', 'U', 'N', diag, m, k, alpha,a( ( k+1 )*k ), k, &
                                    b( 0_ilp, 0_ilp ), ldb )
                          call stdlib_dgemm( 'N', 'T', m, k, k, -one, b( 0_ilp, 0_ilp ),ldb, a( 0_ilp ), k, &
                                    alpha, b( 0_ilp, k ), ldb )
                          call stdlib_dtrsm( 'R', 'L', 'T', diag, m, k, one,a( k*k ), k, b( 0_ilp, k )&
                                    , ldb )
                       else
                          ! side  ='r', n is even, transr = 't', uplo = 'u',
                          ! and trans = 't'
                          call stdlib_dtrsm( 'R', 'L', 'N', diag, m, k, alpha,a( k*k ), k, b( 0_ilp, &
                                    k ), ldb )
                          call stdlib_dgemm( 'N', 'N', m, k, k, -one, b( 0_ilp, k ),ldb, a( 0_ilp ), k, &
                                    alpha, b( 0_ilp, 0_ilp ), ldb )
                          call stdlib_dtrsm( 'R', 'U', 'T', diag, m, k, one,a( ( k+1 )*k ), k, b( &
                                    0_ilp, 0_ilp ), ldb )
                       end if
                    end if
                 end if
              end if
           end if
           return
     end subroutine stdlib_dtfsm


     pure module subroutine stdlib_ctfsm( transr, side, uplo, trans, diag, m, n, alpha, a,b, ldb )
     !! Level 3 BLAS like routine for A in RFP Format.
     !! CTFSM solves the matrix equation
     !! op( A )*X = alpha*B  or  X*op( A ) = alpha*B
     !! where alpha is a scalar, X and B are m by n matrices, A is a unit, or
     !! non-unit,  upper or lower triangular matrix  and  op( A )  is one  of
     !! op( A ) = A   or   op( A ) = A**H.
     !! A is in Rectangular Full Packed (RFP) Format.
     !! The matrix X is overwritten on B.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: transr, diag, side, trans, uplo
           integer(ilp), intent(in) :: ldb, m, n
           complex(sp), intent(in) :: alpha
           ! Array Arguments 
           complex(sp), intent(in) :: a(0_ilp:*)
           complex(sp), intent(inout) :: b(0_ilp:ldb-1,0_ilp:*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lower, lside, misodd, nisodd, normaltransr, notrans
           integer(ilp) :: m1, m2, n1, n2, k, info, i, j
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           normaltransr = stdlib_lsame( transr, 'N' )
           lside = stdlib_lsame( side, 'L' )
           lower = stdlib_lsame( uplo, 'L' )
           notrans = stdlib_lsame( trans, 'N' )
           if( .not.normaltransr .and. .not.stdlib_lsame( transr, 'C' ) ) then
              info = -1_ilp
           else if( .not.lside .and. .not.stdlib_lsame( side, 'R' ) ) then
              info = -2_ilp
           else if( .not.lower .and. .not.stdlib_lsame( uplo, 'U' ) ) then
              info = -3_ilp
           else if( .not.notrans .and. .not.stdlib_lsame( trans, 'C' ) ) then
              info = -4_ilp
           else if( .not.stdlib_lsame( diag, 'N' ) .and. .not.stdlib_lsame( diag, 'U' ) )&
                     then
              info = -5_ilp
           else if( m<0_ilp ) then
              info = -6_ilp
           else if( n<0_ilp ) then
              info = -7_ilp
           else if( ldb<max( 1_ilp, m ) ) then
              info = -11_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CTFSM ', -info )
              return
           end if
           ! quick return when ( (n==0).or.(m==0) )
           if( ( m==0 ) .or. ( n==0 ) )return
           ! quick return when alpha==(0e+0_sp,0e+0_sp)
           if( alpha==czero ) then
              do j = 0, n - 1
                 do i = 0, m - 1
                    b( i, j ) = czero
                 end do
              end do
              return
           end if
           if( lside ) then
              ! side = 'l'
              ! a is m-by-m.
              ! if m is odd, set nisodd = .true., and m1 and m2.
              ! if m is even, nisodd = .false., and m.
              if( mod( m, 2_ilp )==0_ilp ) then
                 misodd = .false.
                 k = m / 2_ilp
              else
                 misodd = .true.
                 if( lower ) then
                    m2 = m / 2_ilp
                    m1 = m - m2
                 else
                    m1 = m / 2_ilp
                    m2 = m - m1
                 end if
              end if
              if( misodd ) then
                 ! side = 'l' and n is odd
                 if( normaltransr ) then
                    ! side = 'l', n is odd, and transr = 'n'
                    if( lower ) then
                       ! side  ='l', n is odd, transr = 'n', and uplo = 'l'
                       if( notrans ) then
                          ! side  ='l', n is odd, transr = 'n', uplo = 'l', and
                          ! trans = 'n'
                          if( m==1_ilp ) then
                             call stdlib_ctrsm( 'L', 'L', 'N', diag, m1, n, alpha,a, m, b, ldb )
                                       
                          else
                             call stdlib_ctrsm( 'L', 'L', 'N', diag, m1, n, alpha,a( 0_ilp ), m, b, &
                                       ldb )
                             call stdlib_cgemm( 'N', 'N', m2, n, m1, -cone, a( m1 ),m, b, ldb, &
                                       alpha, b( m1, 0_ilp ), ldb )
                             call stdlib_ctrsm( 'L', 'U', 'C', diag, m2, n, cone,a( m ), m, b( m1,&
                                        0_ilp ), ldb )
                          end if
                       else
                          ! side  ='l', n is odd, transr = 'n', uplo = 'l', and
                          ! trans = 'c'
                          if( m==1_ilp ) then
                             call stdlib_ctrsm( 'L', 'L', 'C', diag, m1, n, alpha,a( 0_ilp ), m, b, &
                                       ldb )
                          else
                             call stdlib_ctrsm( 'L', 'U', 'N', diag, m2, n, alpha,a( m ), m, b( &
                                       m1, 0_ilp ), ldb )
                             call stdlib_cgemm( 'C', 'N', m1, n, m2, -cone, a( m1 ),m, b( m1, 0_ilp ),&
                                        ldb, alpha, b, ldb )
                             call stdlib_ctrsm( 'L', 'L', 'C', diag, m1, n, cone,a( 0_ilp ), m, b, &
                                       ldb )
                          end if
                       end if
                    else
                       ! side  ='l', n is odd, transr = 'n', and uplo = 'u'
                       if( .not.notrans ) then
                          ! side  ='l', n is odd, transr = 'n', uplo = 'u', and
                          ! trans = 'n'
                          call stdlib_ctrsm( 'L', 'L', 'N', diag, m1, n, alpha,a( m2 ), m, b, ldb &
                                    )
                          call stdlib_cgemm( 'C', 'N', m2, n, m1, -cone, a( 0_ilp ), m,b, ldb, alpha, &
                                    b( m1, 0_ilp ), ldb )
                          call stdlib_ctrsm( 'L', 'U', 'C', diag, m2, n, cone,a( m1 ), m, b( m1, &
                                    0_ilp ), ldb )
                       else
                          ! side  ='l', n is odd, transr = 'n', uplo = 'u', and
                          ! trans = 'c'
                          call stdlib_ctrsm( 'L', 'U', 'N', diag, m2, n, alpha,a( m1 ), m, b( m1, &
                                    0_ilp ), ldb )
                          call stdlib_cgemm( 'N', 'N', m1, n, m2, -cone, a( 0_ilp ), m,b( m1, 0_ilp ), &
                                    ldb, alpha, b, ldb )
                          call stdlib_ctrsm( 'L', 'L', 'C', diag, m1, n, cone,a( m2 ), m, b, ldb )
                                    
                       end if
                    end if
                 else
                    ! side = 'l', n is odd, and transr = 'c'
                    if( lower ) then
                       ! side  ='l', n is odd, transr = 'c', and uplo = 'l'
                       if( notrans ) then
                          ! side  ='l', n is odd, transr = 'c', uplo = 'l', and
                          ! trans = 'n'
                          if( m==1_ilp ) then
                             call stdlib_ctrsm( 'L', 'U', 'C', diag, m1, n, alpha,a( 0_ilp ), m1, b, &
                                       ldb )
                          else
                             call stdlib_ctrsm( 'L', 'U', 'C', diag, m1, n, alpha,a( 0_ilp ), m1, b, &
                                       ldb )
                             call stdlib_cgemm( 'C', 'N', m2, n, m1, -cone,a( m1*m1 ), m1, b, ldb,&
                                        alpha,b( m1, 0_ilp ), ldb )
                             call stdlib_ctrsm( 'L', 'L', 'N', diag, m2, n, cone,a( 1_ilp ), m1, b( &
                                       m1, 0_ilp ), ldb )
                          end if
                       else
                          ! side  ='l', n is odd, transr = 'c', uplo = 'l', and
                          ! trans = 'c'
                          if( m==1_ilp ) then
                             call stdlib_ctrsm( 'L', 'U', 'N', diag, m1, n, alpha,a( 0_ilp ), m1, b, &
                                       ldb )
                          else
                             call stdlib_ctrsm( 'L', 'L', 'C', diag, m2, n, alpha,a( 1_ilp ), m1, b( &
                                       m1, 0_ilp ), ldb )
                             call stdlib_cgemm( 'N', 'N', m1, n, m2, -cone,a( m1*m1 ), m1, b( m1, &
                                       0_ilp ), ldb,alpha, b, ldb )
                             call stdlib_ctrsm( 'L', 'U', 'N', diag, m1, n, cone,a( 0_ilp ), m1, b, &
                                       ldb )
                          end if
                       end if
                    else
                       ! side  ='l', n is odd, transr = 'c', and uplo = 'u'
                       if( .not.notrans ) then
                          ! side  ='l', n is odd, transr = 'c', uplo = 'u', and
                          ! trans = 'n'
                          call stdlib_ctrsm( 'L', 'U', 'C', diag, m1, n, alpha,a( m2*m2 ), m2, b, &
                                    ldb )
                          call stdlib_cgemm( 'N', 'N', m2, n, m1, -cone, a( 0_ilp ), m2,b, ldb, alpha,&
                                     b( m1, 0_ilp ), ldb )
                          call stdlib_ctrsm( 'L', 'L', 'N', diag, m2, n, cone,a( m1*m2 ), m2, b( &
                                    m1, 0_ilp ), ldb )
                       else
                          ! side  ='l', n is odd, transr = 'c', uplo = 'u', and
                          ! trans = 'c'
                          call stdlib_ctrsm( 'L', 'L', 'C', diag, m2, n, alpha,a( m1*m2 ), m2, b( &
                                    m1, 0_ilp ), ldb )
                          call stdlib_cgemm( 'C', 'N', m1, n, m2, -cone, a( 0_ilp ), m2,b( m1, 0_ilp ), &
                                    ldb, alpha, b, ldb )
                          call stdlib_ctrsm( 'L', 'U', 'N', diag, m1, n, cone,a( m2*m2 ), m2, b, &
                                    ldb )
                       end if
                    end if
                 end if
              else
                 ! side = 'l' and n is even
                 if( normaltransr ) then
                    ! side = 'l', n is even, and transr = 'n'
                    if( lower ) then
                       ! side  ='l', n is even, transr = 'n', and uplo = 'l'
                       if( notrans ) then
                          ! side  ='l', n is even, transr = 'n', uplo = 'l',
                          ! and trans = 'n'
                          call stdlib_ctrsm( 'L', 'L', 'N', diag, k, n, alpha,a( 1_ilp ), m+1, b, ldb &
                                    )
                          call stdlib_cgemm( 'N', 'N', k, n, k, -cone, a( k+1 ),m+1, b, ldb, &
                                    alpha, b( k, 0_ilp ), ldb )
                          call stdlib_ctrsm( 'L', 'U', 'C', diag, k, n, cone,a( 0_ilp ), m+1, b( k, 0_ilp &
                                    ), ldb )
                       else
                          ! side  ='l', n is even, transr = 'n', uplo = 'l',
                          ! and trans = 'c'
                          call stdlib_ctrsm( 'L', 'U', 'N', diag, k, n, alpha,a( 0_ilp ), m+1, b( k, &
                                    0_ilp ), ldb )
                          call stdlib_cgemm( 'C', 'N', k, n, k, -cone, a( k+1 ),m+1, b( k, 0_ilp ), &
                                    ldb, alpha, b, ldb )
                          call stdlib_ctrsm( 'L', 'L', 'C', diag, k, n, cone,a( 1_ilp ), m+1, b, ldb )
                                    
                       end if
                    else
                       ! side  ='l', n is even, transr = 'n', and uplo = 'u'
                       if( .not.notrans ) then
                          ! side  ='l', n is even, transr = 'n', uplo = 'u',
                          ! and trans = 'n'
                          call stdlib_ctrsm( 'L', 'L', 'N', diag, k, n, alpha,a( k+1 ), m+1, b, &
                                    ldb )
                          call stdlib_cgemm( 'C', 'N', k, n, k, -cone, a( 0_ilp ), m+1,b, ldb, alpha, &
                                    b( k, 0_ilp ), ldb )
                          call stdlib_ctrsm( 'L', 'U', 'C', diag, k, n, cone,a( k ), m+1, b( k, 0_ilp &
                                    ), ldb )
                       else
                          ! side  ='l', n is even, transr = 'n', uplo = 'u',
                          ! and trans = 'c'
                          call stdlib_ctrsm( 'L', 'U', 'N', diag, k, n, alpha,a( k ), m+1, b( k, &
                                    0_ilp ), ldb )
                          call stdlib_cgemm( 'N', 'N', k, n, k, -cone, a( 0_ilp ), m+1,b( k, 0_ilp ), ldb,&
                                     alpha, b, ldb )
                          call stdlib_ctrsm( 'L', 'L', 'C', diag, k, n, cone,a( k+1 ), m+1, b, &
                                    ldb )
                       end if
                    end if
                 else
                    ! side = 'l', n is even, and transr = 'c'
                    if( lower ) then
                       ! side  ='l', n is even, transr = 'c', and uplo = 'l'
                       if( notrans ) then
                          ! side  ='l', n is even, transr = 'c', uplo = 'l',
                          ! and trans = 'n'
                          call stdlib_ctrsm( 'L', 'U', 'C', diag, k, n, alpha,a( k ), k, b, ldb )
                                    
                          call stdlib_cgemm( 'C', 'N', k, n, k, -cone,a( k*( k+1 ) ), k, b, ldb, &
                                    alpha,b( k, 0_ilp ), ldb )
                          call stdlib_ctrsm( 'L', 'L', 'N', diag, k, n, cone,a( 0_ilp ), k, b( k, 0_ilp ),&
                                     ldb )
                       else
                          ! side  ='l', n is even, transr = 'c', uplo = 'l',
                          ! and trans = 'c'
                          call stdlib_ctrsm( 'L', 'L', 'C', diag, k, n, alpha,a( 0_ilp ), k, b( k, 0_ilp )&
                                    , ldb )
                          call stdlib_cgemm( 'N', 'N', k, n, k, -cone,a( k*( k+1 ) ), k, b( k, 0_ilp )&
                                    , ldb,alpha, b, ldb )
                          call stdlib_ctrsm( 'L', 'U', 'N', diag, k, n, cone,a( k ), k, b, ldb )
                                    
                       end if
                    else
                       ! side  ='l', n is even, transr = 'c', and uplo = 'u'
                       if( .not.notrans ) then
                          ! side  ='l', n is even, transr = 'c', uplo = 'u',
                          ! and trans = 'n'
                          call stdlib_ctrsm( 'L', 'U', 'C', diag, k, n, alpha,a( k*( k+1 ) ), k, &
                                    b, ldb )
                          call stdlib_cgemm( 'N', 'N', k, n, k, -cone, a( 0_ilp ), k, b,ldb, alpha, b(&
                                     k, 0_ilp ), ldb )
                          call stdlib_ctrsm( 'L', 'L', 'N', diag, k, n, cone,a( k*k ), k, b( k, 0_ilp &
                                    ), ldb )
                       else
                          ! side  ='l', n is even, transr = 'c', uplo = 'u',
                          ! and trans = 'c'
                          call stdlib_ctrsm( 'L', 'L', 'C', diag, k, n, alpha,a( k*k ), k, b( k, &
                                    0_ilp ), ldb )
                          call stdlib_cgemm( 'C', 'N', k, n, k, -cone, a( 0_ilp ), k,b( k, 0_ilp ), ldb, &
                                    alpha, b, ldb )
                          call stdlib_ctrsm( 'L', 'U', 'N', diag, k, n, cone,a( k*( k+1 ) ), k, b,&
                                     ldb )
                       end if
                    end if
                 end if
              end if
           else
              ! side = 'r'
              ! a is n-by-n.
              ! if n is odd, set nisodd = .true., and n1 and n2.
              ! if n is even, nisodd = .false., and k.
              if( mod( n, 2_ilp )==0_ilp ) then
                 nisodd = .false.
                 k = n / 2_ilp
              else
                 nisodd = .true.
                 if( lower ) then
                    n2 = n / 2_ilp
                    n1 = n - n2
                 else
                    n1 = n / 2_ilp
                    n2 = n - n1
                 end if
              end if
              if( nisodd ) then
                 ! side = 'r' and n is odd
                 if( normaltransr ) then
                    ! side = 'r', n is odd, and transr = 'n'
                    if( lower ) then
                       ! side  ='r', n is odd, transr = 'n', and uplo = 'l'
                       if( notrans ) then
                          ! side  ='r', n is odd, transr = 'n', uplo = 'l', and
                          ! trans = 'n'
                          call stdlib_ctrsm( 'R', 'U', 'C', diag, m, n2, alpha,a( n ), n, b( 0_ilp, &
                                    n1 ), ldb )
                          call stdlib_cgemm( 'N', 'N', m, n1, n2, -cone, b( 0_ilp, n1 ),ldb, a( n1 ), &
                                    n, alpha, b( 0_ilp, 0_ilp ),ldb )
                          call stdlib_ctrsm( 'R', 'L', 'N', diag, m, n1, cone,a( 0_ilp ), n, b( 0_ilp, 0_ilp )&
                                    , ldb )
                       else
                          ! side  ='r', n is odd, transr = 'n', uplo = 'l', and
                          ! trans = 'c'
                          call stdlib_ctrsm( 'R', 'L', 'C', diag, m, n1, alpha,a( 0_ilp ), n, b( 0_ilp, 0_ilp &
                                    ), ldb )
                          call stdlib_cgemm( 'N', 'C', m, n2, n1, -cone, b( 0_ilp, 0_ilp ),ldb, a( n1 ), &
                                    n, alpha, b( 0_ilp, n1 ),ldb )
                          call stdlib_ctrsm( 'R', 'U', 'N', diag, m, n2, cone,a( n ), n, b( 0_ilp, n1 &
                                    ), ldb )
                       end if
                    else
                       ! side  ='r', n is odd, transr = 'n', and uplo = 'u'
                       if( notrans ) then
                          ! side  ='r', n is odd, transr = 'n', uplo = 'u', and
                          ! trans = 'n'
                          call stdlib_ctrsm( 'R', 'L', 'C', diag, m, n1, alpha,a( n2 ), n, b( 0_ilp, &
                                    0_ilp ), ldb )
                          call stdlib_cgemm( 'N', 'N', m, n2, n1, -cone, b( 0_ilp, 0_ilp ),ldb, a( 0_ilp ), n,&
                                     alpha, b( 0_ilp, n1 ),ldb )
                          call stdlib_ctrsm( 'R', 'U', 'N', diag, m, n2, cone,a( n1 ), n, b( 0_ilp, &
                                    n1 ), ldb )
                       else
                          ! side  ='r', n is odd, transr = 'n', uplo = 'u', and
                          ! trans = 'c'
                          call stdlib_ctrsm( 'R', 'U', 'C', diag, m, n2, alpha,a( n1 ), n, b( 0_ilp, &
                                    n1 ), ldb )
                          call stdlib_cgemm( 'N', 'C', m, n1, n2, -cone, b( 0_ilp, n1 ),ldb, a( 0_ilp ), &
                                    n, alpha, b( 0_ilp, 0_ilp ), ldb )
                          call stdlib_ctrsm( 'R', 'L', 'N', diag, m, n1, cone,a( n2 ), n, b( 0_ilp, 0_ilp &
                                    ), ldb )
                       end if
                    end if
                 else
                    ! side = 'r', n is odd, and transr = 'c'
                    if( lower ) then
                       ! side  ='r', n is odd, transr = 'c', and uplo = 'l'
                       if( notrans ) then
                          ! side  ='r', n is odd, transr = 'c', uplo = 'l', and
                          ! trans = 'n'
                          call stdlib_ctrsm( 'R', 'L', 'N', diag, m, n2, alpha,a( 1_ilp ), n1, b( 0_ilp, &
                                    n1 ), ldb )
                          call stdlib_cgemm( 'N', 'C', m, n1, n2, -cone, b( 0_ilp, n1 ),ldb, a( n1*n1 &
                                    ), n1, alpha, b( 0_ilp, 0_ilp ),ldb )
                          call stdlib_ctrsm( 'R', 'U', 'C', diag, m, n1, cone,a( 0_ilp ), n1, b( 0_ilp, 0_ilp &
                                    ), ldb )
                       else
                          ! side  ='r', n is odd, transr = 'c', uplo = 'l', and
                          ! trans = 'c'
                          call stdlib_ctrsm( 'R', 'U', 'N', diag, m, n1, alpha,a( 0_ilp ), n1, b( 0_ilp, &
                                    0_ilp ), ldb )
                          call stdlib_cgemm( 'N', 'N', m, n2, n1, -cone, b( 0_ilp, 0_ilp ),ldb, a( n1*n1 )&
                                    , n1, alpha, b( 0_ilp, n1 ),ldb )
                          call stdlib_ctrsm( 'R', 'L', 'C', diag, m, n2, cone,a( 1_ilp ), n1, b( 0_ilp, &
                                    n1 ), ldb )
                       end if
                    else
                       ! side  ='r', n is odd, transr = 'c', and uplo = 'u'
                       if( notrans ) then
                          ! side  ='r', n is odd, transr = 'c', uplo = 'u', and
                          ! trans = 'n'
                          call stdlib_ctrsm( 'R', 'U', 'N', diag, m, n1, alpha,a( n2*n2 ), n2, b( &
                                    0_ilp, 0_ilp ), ldb )
                          call stdlib_cgemm( 'N', 'C', m, n2, n1, -cone, b( 0_ilp, 0_ilp ),ldb, a( 0_ilp ), &
                                    n2, alpha, b( 0_ilp, n1 ),ldb )
                          call stdlib_ctrsm( 'R', 'L', 'C', diag, m, n2, cone,a( n1*n2 ), n2, b( &
                                    0_ilp, n1 ), ldb )
                       else
                          ! side  ='r', n is odd, transr = 'c', uplo = 'u', and
                          ! trans = 'c'
                          call stdlib_ctrsm( 'R', 'L', 'N', diag, m, n2, alpha,a( n1*n2 ), n2, b( &
                                    0_ilp, n1 ), ldb )
                          call stdlib_cgemm( 'N', 'N', m, n1, n2, -cone, b( 0_ilp, n1 ),ldb, a( 0_ilp ), &
                                    n2, alpha, b( 0_ilp, 0_ilp ),ldb )
                          call stdlib_ctrsm( 'R', 'U', 'C', diag, m, n1, cone,a( n2*n2 ), n2, b( &
                                    0_ilp, 0_ilp ), ldb )
                       end if
                    end if
                 end if
              else
                 ! side = 'r' and n is even
                 if( normaltransr ) then
                    ! side = 'r', n is even, and transr = 'n'
                    if( lower ) then
                       ! side  ='r', n is even, transr = 'n', and uplo = 'l'
                       if( notrans ) then
                          ! side  ='r', n is even, transr = 'n', uplo = 'l',
                          ! and trans = 'n'
                          call stdlib_ctrsm( 'R', 'U', 'C', diag, m, k, alpha,a( 0_ilp ), n+1, b( 0_ilp, &
                                    k ), ldb )
                          call stdlib_cgemm( 'N', 'N', m, k, k, -cone, b( 0_ilp, k ),ldb, a( k+1 ), n+&
                                    1_ilp, alpha, b( 0_ilp, 0_ilp ),ldb )
                          call stdlib_ctrsm( 'R', 'L', 'N', diag, m, k, cone,a( 1_ilp ), n+1, b( 0_ilp, 0_ilp &
                                    ), ldb )
                       else
                          ! side  ='r', n is even, transr = 'n', uplo = 'l',
                          ! and trans = 'c'
                          call stdlib_ctrsm( 'R', 'L', 'C', diag, m, k, alpha,a( 1_ilp ), n+1, b( 0_ilp, &
                                    0_ilp ), ldb )
                          call stdlib_cgemm( 'N', 'C', m, k, k, -cone, b( 0_ilp, 0_ilp ),ldb, a( k+1 ), n+&
                                    1_ilp, alpha, b( 0_ilp, k ),ldb )
                          call stdlib_ctrsm( 'R', 'U', 'N', diag, m, k, cone,a( 0_ilp ), n+1, b( 0_ilp, k &
                                    ), ldb )
                       end if
                    else
                       ! side  ='r', n is even, transr = 'n', and uplo = 'u'
                       if( notrans ) then
                          ! side  ='r', n is even, transr = 'n', uplo = 'u',
                          ! and trans = 'n'
                          call stdlib_ctrsm( 'R', 'L', 'C', diag, m, k, alpha,a( k+1 ), n+1, b( 0_ilp,&
                                     0_ilp ), ldb )
                          call stdlib_cgemm( 'N', 'N', m, k, k, -cone, b( 0_ilp, 0_ilp ),ldb, a( 0_ilp ), n+1,&
                                     alpha, b( 0_ilp, k ),ldb )
                          call stdlib_ctrsm( 'R', 'U', 'N', diag, m, k, cone,a( k ), n+1, b( 0_ilp, k &
                                    ), ldb )
                       else
                          ! side  ='r', n is even, transr = 'n', uplo = 'u',
                          ! and trans = 'c'
                          call stdlib_ctrsm( 'R', 'U', 'C', diag, m, k, alpha,a( k ), n+1, b( 0_ilp, &
                                    k ), ldb )
                          call stdlib_cgemm( 'N', 'C', m, k, k, -cone, b( 0_ilp, k ),ldb, a( 0_ilp ), n+1,&
                                     alpha, b( 0_ilp, 0_ilp ),ldb )
                          call stdlib_ctrsm( 'R', 'L', 'N', diag, m, k, cone,a( k+1 ), n+1, b( 0_ilp, &
                                    0_ilp ), ldb )
                       end if
                    end if
                 else
                    ! side = 'r', n is even, and transr = 'c'
                    if( lower ) then
                       ! side  ='r', n is even, transr = 'c', and uplo = 'l'
                       if( notrans ) then
                          ! side  ='r', n is even, transr = 'c', uplo = 'l',
                          ! and trans = 'n'
                          call stdlib_ctrsm( 'R', 'L', 'N', diag, m, k, alpha,a( 0_ilp ), k, b( 0_ilp, k )&
                                    , ldb )
                          call stdlib_cgemm( 'N', 'C', m, k, k, -cone, b( 0_ilp, k ),ldb, a( ( k+1 )&
                                    *k ), k, alpha,b( 0_ilp, 0_ilp ), ldb )
                          call stdlib_ctrsm( 'R', 'U', 'C', diag, m, k, cone,a( k ), k, b( 0_ilp, 0_ilp ),&
                                     ldb )
                       else
                          ! side  ='r', n is even, transr = 'c', uplo = 'l',
                          ! and trans = 'c'
                          call stdlib_ctrsm( 'R', 'U', 'N', diag, m, k, alpha,a( k ), k, b( 0_ilp, 0_ilp )&
                                    , ldb )
                          call stdlib_cgemm( 'N', 'N', m, k, k, -cone, b( 0_ilp, 0_ilp ),ldb, a( ( k+1 )&
                                    *k ), k, alpha,b( 0_ilp, k ), ldb )
                          call stdlib_ctrsm( 'R', 'L', 'C', diag, m, k, cone,a( 0_ilp ), k, b( 0_ilp, k ),&
                                     ldb )
                       end if
                    else
                       ! side  ='r', n is even, transr = 'c', and uplo = 'u'
                       if( notrans ) then
                          ! side  ='r', n is even, transr = 'c', uplo = 'u',
                          ! and trans = 'n'
                          call stdlib_ctrsm( 'R', 'U', 'N', diag, m, k, alpha,a( ( k+1 )*k ), k, &
                                    b( 0_ilp, 0_ilp ), ldb )
                          call stdlib_cgemm( 'N', 'C', m, k, k, -cone, b( 0_ilp, 0_ilp ),ldb, a( 0_ilp ), k, &
                                    alpha, b( 0_ilp, k ), ldb )
                          call stdlib_ctrsm( 'R', 'L', 'C', diag, m, k, cone,a( k*k ), k, b( 0_ilp, k &
                                    ), ldb )
                       else
                          ! side  ='r', n is even, transr = 'c', uplo = 'u',
                          ! and trans = 'c'
                          call stdlib_ctrsm( 'R', 'L', 'N', diag, m, k, alpha,a( k*k ), k, b( 0_ilp, &
                                    k ), ldb )
                          call stdlib_cgemm( 'N', 'N', m, k, k, -cone, b( 0_ilp, k ),ldb, a( 0_ilp ), k, &
                                    alpha, b( 0_ilp, 0_ilp ), ldb )
                          call stdlib_ctrsm( 'R', 'U', 'C', diag, m, k, cone,a( ( k+1 )*k ), k, b(&
                                     0_ilp, 0_ilp ), ldb )
                       end if
                    end if
                 end if
              end if
           end if
           return
     end subroutine stdlib_ctfsm

     pure module subroutine stdlib_ztfsm( transr, side, uplo, trans, diag, m, n, alpha, a,b, ldb )
     !! Level 3 BLAS like routine for A in RFP Format.
     !! ZTFSM solves the matrix equation
     !! op( A )*X = alpha*B  or  X*op( A ) = alpha*B
     !! where alpha is a scalar, X and B are m by n matrices, A is a unit, or
     !! non-unit,  upper or lower triangular matrix  and  op( A )  is one  of
     !! op( A ) = A   or   op( A ) = A**H.
     !! A is in Rectangular Full Packed (RFP) Format.
     !! The matrix X is overwritten on B.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: transr, diag, side, trans, uplo
           integer(ilp), intent(in) :: ldb, m, n
           complex(dp), intent(in) :: alpha
           ! Array Arguments 
           complex(dp), intent(in) :: a(0_ilp:*)
           complex(dp), intent(inout) :: b(0_ilp:ldb-1,0_ilp:*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lower, lside, misodd, nisodd, normaltransr, notrans
           integer(ilp) :: m1, m2, n1, n2, k, info, i, j
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           normaltransr = stdlib_lsame( transr, 'N' )
           lside = stdlib_lsame( side, 'L' )
           lower = stdlib_lsame( uplo, 'L' )
           notrans = stdlib_lsame( trans, 'N' )
           if( .not.normaltransr .and. .not.stdlib_lsame( transr, 'C' ) ) then
              info = -1_ilp
           else if( .not.lside .and. .not.stdlib_lsame( side, 'R' ) ) then
              info = -2_ilp
           else if( .not.lower .and. .not.stdlib_lsame( uplo, 'U' ) ) then
              info = -3_ilp
           else if( .not.notrans .and. .not.stdlib_lsame( trans, 'C' ) ) then
              info = -4_ilp
           else if( .not.stdlib_lsame( diag, 'N' ) .and. .not.stdlib_lsame( diag, 'U' ) )&
                     then
              info = -5_ilp
           else if( m<0_ilp ) then
              info = -6_ilp
           else if( n<0_ilp ) then
              info = -7_ilp
           else if( ldb<max( 1_ilp, m ) ) then
              info = -11_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZTFSM ', -info )
              return
           end if
           ! quick return when ( (n==0).or.(m==0) )
           if( ( m==0 ) .or. ( n==0 ) )return
           ! quick return when alpha==(0e+0_dp,0e+0_dp)
           if( alpha==czero ) then
              do j = 0, n - 1
                 do i = 0, m - 1
                    b( i, j ) = czero
                 end do
              end do
              return
           end if
           if( lside ) then
              ! side = 'l'
              ! a is m-by-m.
              ! if m is odd, set nisodd = .true., and m1 and m2.
              ! if m is even, nisodd = .false., and m.
              if( mod( m, 2_ilp )==0_ilp ) then
                 misodd = .false.
                 k = m / 2_ilp
              else
                 misodd = .true.
                 if( lower ) then
                    m2 = m / 2_ilp
                    m1 = m - m2
                 else
                    m1 = m / 2_ilp
                    m2 = m - m1
                 end if
              end if
              if( misodd ) then
                 ! side = 'l' and n is odd
                 if( normaltransr ) then
                    ! side = 'l', n is odd, and transr = 'n'
                    if( lower ) then
                       ! side  ='l', n is odd, transr = 'n', and uplo = 'l'
                       if( notrans ) then
                          ! side  ='l', n is odd, transr = 'n', uplo = 'l', and
                          ! trans = 'n'
                          if( m==1_ilp ) then
                             call stdlib_ztrsm( 'L', 'L', 'N', diag, m1, n, alpha,a, m, b, ldb )
                                       
                          else
                             call stdlib_ztrsm( 'L', 'L', 'N', diag, m1, n, alpha,a( 0_ilp ), m, b, &
                                       ldb )
                             call stdlib_zgemm( 'N', 'N', m2, n, m1, -cone, a( m1 ),m, b, ldb, &
                                       alpha, b( m1, 0_ilp ), ldb )
                             call stdlib_ztrsm( 'L', 'U', 'C', diag, m2, n, cone,a( m ), m, b( m1,&
                                        0_ilp ), ldb )
                          end if
                       else
                          ! side  ='l', n is odd, transr = 'n', uplo = 'l', and
                          ! trans = 'c'
                          if( m==1_ilp ) then
                             call stdlib_ztrsm( 'L', 'L', 'C', diag, m1, n, alpha,a( 0_ilp ), m, b, &
                                       ldb )
                          else
                             call stdlib_ztrsm( 'L', 'U', 'N', diag, m2, n, alpha,a( m ), m, b( &
                                       m1, 0_ilp ), ldb )
                             call stdlib_zgemm( 'C', 'N', m1, n, m2, -cone, a( m1 ),m, b( m1, 0_ilp ),&
                                        ldb, alpha, b, ldb )
                             call stdlib_ztrsm( 'L', 'L', 'C', diag, m1, n, cone,a( 0_ilp ), m, b, &
                                       ldb )
                          end if
                       end if
                    else
                       ! side  ='l', n is odd, transr = 'n', and uplo = 'u'
                       if( .not.notrans ) then
                          ! side  ='l', n is odd, transr = 'n', uplo = 'u', and
                          ! trans = 'n'
                          call stdlib_ztrsm( 'L', 'L', 'N', diag, m1, n, alpha,a( m2 ), m, b, ldb &
                                    )
                          call stdlib_zgemm( 'C', 'N', m2, n, m1, -cone, a( 0_ilp ), m,b, ldb, alpha, &
                                    b( m1, 0_ilp ), ldb )
                          call stdlib_ztrsm( 'L', 'U', 'C', diag, m2, n, cone,a( m1 ), m, b( m1, &
                                    0_ilp ), ldb )
                       else
                          ! side  ='l', n is odd, transr = 'n', uplo = 'u', and
                          ! trans = 'c'
                          call stdlib_ztrsm( 'L', 'U', 'N', diag, m2, n, alpha,a( m1 ), m, b( m1, &
                                    0_ilp ), ldb )
                          call stdlib_zgemm( 'N', 'N', m1, n, m2, -cone, a( 0_ilp ), m,b( m1, 0_ilp ), &
                                    ldb, alpha, b, ldb )
                          call stdlib_ztrsm( 'L', 'L', 'C', diag, m1, n, cone,a( m2 ), m, b, ldb )
                                    
                       end if
                    end if
                 else
                    ! side = 'l', n is odd, and transr = 'c'
                    if( lower ) then
                       ! side  ='l', n is odd, transr = 'c', and uplo = 'l'
                       if( notrans ) then
                          ! side  ='l', n is odd, transr = 'c', uplo = 'l', and
                          ! trans = 'n'
                          if( m==1_ilp ) then
                             call stdlib_ztrsm( 'L', 'U', 'C', diag, m1, n, alpha,a( 0_ilp ), m1, b, &
                                       ldb )
                          else
                             call stdlib_ztrsm( 'L', 'U', 'C', diag, m1, n, alpha,a( 0_ilp ), m1, b, &
                                       ldb )
                             call stdlib_zgemm( 'C', 'N', m2, n, m1, -cone,a( m1*m1 ), m1, b, ldb,&
                                        alpha,b( m1, 0_ilp ), ldb )
                             call stdlib_ztrsm( 'L', 'L', 'N', diag, m2, n, cone,a( 1_ilp ), m1, b( &
                                       m1, 0_ilp ), ldb )
                          end if
                       else
                          ! side  ='l', n is odd, transr = 'c', uplo = 'l', and
                          ! trans = 'c'
                          if( m==1_ilp ) then
                             call stdlib_ztrsm( 'L', 'U', 'N', diag, m1, n, alpha,a( 0_ilp ), m1, b, &
                                       ldb )
                          else
                             call stdlib_ztrsm( 'L', 'L', 'C', diag, m2, n, alpha,a( 1_ilp ), m1, b( &
                                       m1, 0_ilp ), ldb )
                             call stdlib_zgemm( 'N', 'N', m1, n, m2, -cone,a( m1*m1 ), m1, b( m1, &
                                       0_ilp ), ldb,alpha, b, ldb )
                             call stdlib_ztrsm( 'L', 'U', 'N', diag, m1, n, cone,a( 0_ilp ), m1, b, &
                                       ldb )
                          end if
                       end if
                    else
                       ! side  ='l', n is odd, transr = 'c', and uplo = 'u'
                       if( .not.notrans ) then
                          ! side  ='l', n is odd, transr = 'c', uplo = 'u', and
                          ! trans = 'n'
                          call stdlib_ztrsm( 'L', 'U', 'C', diag, m1, n, alpha,a( m2*m2 ), m2, b, &
                                    ldb )
                          call stdlib_zgemm( 'N', 'N', m2, n, m1, -cone, a( 0_ilp ), m2,b, ldb, alpha,&
                                     b( m1, 0_ilp ), ldb )
                          call stdlib_ztrsm( 'L', 'L', 'N', diag, m2, n, cone,a( m1*m2 ), m2, b( &
                                    m1, 0_ilp ), ldb )
                       else
                          ! side  ='l', n is odd, transr = 'c', uplo = 'u', and
                          ! trans = 'c'
                          call stdlib_ztrsm( 'L', 'L', 'C', diag, m2, n, alpha,a( m1*m2 ), m2, b( &
                                    m1, 0_ilp ), ldb )
                          call stdlib_zgemm( 'C', 'N', m1, n, m2, -cone, a( 0_ilp ), m2,b( m1, 0_ilp ), &
                                    ldb, alpha, b, ldb )
                          call stdlib_ztrsm( 'L', 'U', 'N', diag, m1, n, cone,a( m2*m2 ), m2, b, &
                                    ldb )
                       end if
                    end if
                 end if
              else
                 ! side = 'l' and n is even
                 if( normaltransr ) then
                    ! side = 'l', n is even, and transr = 'n'
                    if( lower ) then
                       ! side  ='l', n is even, transr = 'n', and uplo = 'l'
                       if( notrans ) then
                          ! side  ='l', n is even, transr = 'n', uplo = 'l',
                          ! and trans = 'n'
                          call stdlib_ztrsm( 'L', 'L', 'N', diag, k, n, alpha,a( 1_ilp ), m+1, b, ldb &
                                    )
                          call stdlib_zgemm( 'N', 'N', k, n, k, -cone, a( k+1 ),m+1, b, ldb, &
                                    alpha, b( k, 0_ilp ), ldb )
                          call stdlib_ztrsm( 'L', 'U', 'C', diag, k, n, cone,a( 0_ilp ), m+1, b( k, 0_ilp &
                                    ), ldb )
                       else
                          ! side  ='l', n is even, transr = 'n', uplo = 'l',
                          ! and trans = 'c'
                          call stdlib_ztrsm( 'L', 'U', 'N', diag, k, n, alpha,a( 0_ilp ), m+1, b( k, &
                                    0_ilp ), ldb )
                          call stdlib_zgemm( 'C', 'N', k, n, k, -cone, a( k+1 ),m+1, b( k, 0_ilp ), &
                                    ldb, alpha, b, ldb )
                          call stdlib_ztrsm( 'L', 'L', 'C', diag, k, n, cone,a( 1_ilp ), m+1, b, ldb )
                                    
                       end if
                    else
                       ! side  ='l', n is even, transr = 'n', and uplo = 'u'
                       if( .not.notrans ) then
                          ! side  ='l', n is even, transr = 'n', uplo = 'u',
                          ! and trans = 'n'
                          call stdlib_ztrsm( 'L', 'L', 'N', diag, k, n, alpha,a( k+1 ), m+1, b, &
                                    ldb )
                          call stdlib_zgemm( 'C', 'N', k, n, k, -cone, a( 0_ilp ), m+1,b, ldb, alpha, &
                                    b( k, 0_ilp ), ldb )
                          call stdlib_ztrsm( 'L', 'U', 'C', diag, k, n, cone,a( k ), m+1, b( k, 0_ilp &
                                    ), ldb )
                       else
                          ! side  ='l', n is even, transr = 'n', uplo = 'u',
                          ! and trans = 'c'
                          call stdlib_ztrsm( 'L', 'U', 'N', diag, k, n, alpha,a( k ), m+1, b( k, &
                                    0_ilp ), ldb )
                          call stdlib_zgemm( 'N', 'N', k, n, k, -cone, a( 0_ilp ), m+1,b( k, 0_ilp ), ldb,&
                                     alpha, b, ldb )
                          call stdlib_ztrsm( 'L', 'L', 'C', diag, k, n, cone,a( k+1 ), m+1, b, &
                                    ldb )
                       end if
                    end if
                 else
                    ! side = 'l', n is even, and transr = 'c'
                    if( lower ) then
                       ! side  ='l', n is even, transr = 'c', and uplo = 'l'
                       if( notrans ) then
                          ! side  ='l', n is even, transr = 'c', uplo = 'l',
                          ! and trans = 'n'
                          call stdlib_ztrsm( 'L', 'U', 'C', diag, k, n, alpha,a( k ), k, b, ldb )
                                    
                          call stdlib_zgemm( 'C', 'N', k, n, k, -cone,a( k*( k+1 ) ), k, b, ldb, &
                                    alpha,b( k, 0_ilp ), ldb )
                          call stdlib_ztrsm( 'L', 'L', 'N', diag, k, n, cone,a( 0_ilp ), k, b( k, 0_ilp ),&
                                     ldb )
                       else
                          ! side  ='l', n is even, transr = 'c', uplo = 'l',
                          ! and trans = 'c'
                          call stdlib_ztrsm( 'L', 'L', 'C', diag, k, n, alpha,a( 0_ilp ), k, b( k, 0_ilp )&
                                    , ldb )
                          call stdlib_zgemm( 'N', 'N', k, n, k, -cone,a( k*( k+1 ) ), k, b( k, 0_ilp )&
                                    , ldb,alpha, b, ldb )
                          call stdlib_ztrsm( 'L', 'U', 'N', diag, k, n, cone,a( k ), k, b, ldb )
                                    
                       end if
                    else
                       ! side  ='l', n is even, transr = 'c', and uplo = 'u'
                       if( .not.notrans ) then
                          ! side  ='l', n is even, transr = 'c', uplo = 'u',
                          ! and trans = 'n'
                          call stdlib_ztrsm( 'L', 'U', 'C', diag, k, n, alpha,a( k*( k+1 ) ), k, &
                                    b, ldb )
                          call stdlib_zgemm( 'N', 'N', k, n, k, -cone, a( 0_ilp ), k, b,ldb, alpha, b(&
                                     k, 0_ilp ), ldb )
                          call stdlib_ztrsm( 'L', 'L', 'N', diag, k, n, cone,a( k*k ), k, b( k, 0_ilp &
                                    ), ldb )
                       else
                          ! side  ='l', n is even, transr = 'c', uplo = 'u',
                          ! and trans = 'c'
                          call stdlib_ztrsm( 'L', 'L', 'C', diag, k, n, alpha,a( k*k ), k, b( k, &
                                    0_ilp ), ldb )
                          call stdlib_zgemm( 'C', 'N', k, n, k, -cone, a( 0_ilp ), k,b( k, 0_ilp ), ldb, &
                                    alpha, b, ldb )
                          call stdlib_ztrsm( 'L', 'U', 'N', diag, k, n, cone,a( k*( k+1 ) ), k, b,&
                                     ldb )
                       end if
                    end if
                 end if
              end if
           else
              ! side = 'r'
              ! a is n-by-n.
              ! if n is odd, set nisodd = .true., and n1 and n2.
              ! if n is even, nisodd = .false., and k.
              if( mod( n, 2_ilp )==0_ilp ) then
                 nisodd = .false.
                 k = n / 2_ilp
              else
                 nisodd = .true.
                 if( lower ) then
                    n2 = n / 2_ilp
                    n1 = n - n2
                 else
                    n1 = n / 2_ilp
                    n2 = n - n1
                 end if
              end if
              if( nisodd ) then
                 ! side = 'r' and n is odd
                 if( normaltransr ) then
                    ! side = 'r', n is odd, and transr = 'n'
                    if( lower ) then
                       ! side  ='r', n is odd, transr = 'n', and uplo = 'l'
                       if( notrans ) then
                          ! side  ='r', n is odd, transr = 'n', uplo = 'l', and
                          ! trans = 'n'
                          call stdlib_ztrsm( 'R', 'U', 'C', diag, m, n2, alpha,a( n ), n, b( 0_ilp, &
                                    n1 ), ldb )
                          call stdlib_zgemm( 'N', 'N', m, n1, n2, -cone, b( 0_ilp, n1 ),ldb, a( n1 ), &
                                    n, alpha, b( 0_ilp, 0_ilp ),ldb )
                          call stdlib_ztrsm( 'R', 'L', 'N', diag, m, n1, cone,a( 0_ilp ), n, b( 0_ilp, 0_ilp )&
                                    , ldb )
                       else
                          ! side  ='r', n is odd, transr = 'n', uplo = 'l', and
                          ! trans = 'c'
                          call stdlib_ztrsm( 'R', 'L', 'C', diag, m, n1, alpha,a( 0_ilp ), n, b( 0_ilp, 0_ilp &
                                    ), ldb )
                          call stdlib_zgemm( 'N', 'C', m, n2, n1, -cone, b( 0_ilp, 0_ilp ),ldb, a( n1 ), &
                                    n, alpha, b( 0_ilp, n1 ),ldb )
                          call stdlib_ztrsm( 'R', 'U', 'N', diag, m, n2, cone,a( n ), n, b( 0_ilp, n1 &
                                    ), ldb )
                       end if
                    else
                       ! side  ='r', n is odd, transr = 'n', and uplo = 'u'
                       if( notrans ) then
                          ! side  ='r', n is odd, transr = 'n', uplo = 'u', and
                          ! trans = 'n'
                          call stdlib_ztrsm( 'R', 'L', 'C', diag, m, n1, alpha,a( n2 ), n, b( 0_ilp, &
                                    0_ilp ), ldb )
                          call stdlib_zgemm( 'N', 'N', m, n2, n1, -cone, b( 0_ilp, 0_ilp ),ldb, a( 0_ilp ), n,&
                                     alpha, b( 0_ilp, n1 ),ldb )
                          call stdlib_ztrsm( 'R', 'U', 'N', diag, m, n2, cone,a( n1 ), n, b( 0_ilp, &
                                    n1 ), ldb )
                       else
                          ! side  ='r', n is odd, transr = 'n', uplo = 'u', and
                          ! trans = 'c'
                          call stdlib_ztrsm( 'R', 'U', 'C', diag, m, n2, alpha,a( n1 ), n, b( 0_ilp, &
                                    n1 ), ldb )
                          call stdlib_zgemm( 'N', 'C', m, n1, n2, -cone, b( 0_ilp, n1 ),ldb, a( 0_ilp ), &
                                    n, alpha, b( 0_ilp, 0_ilp ), ldb )
                          call stdlib_ztrsm( 'R', 'L', 'N', diag, m, n1, cone,a( n2 ), n, b( 0_ilp, 0_ilp &
                                    ), ldb )
                       end if
                    end if
                 else
                    ! side = 'r', n is odd, and transr = 'c'
                    if( lower ) then
                       ! side  ='r', n is odd, transr = 'c', and uplo = 'l'
                       if( notrans ) then
                          ! side  ='r', n is odd, transr = 'c', uplo = 'l', and
                          ! trans = 'n'
                          call stdlib_ztrsm( 'R', 'L', 'N', diag, m, n2, alpha,a( 1_ilp ), n1, b( 0_ilp, &
                                    n1 ), ldb )
                          call stdlib_zgemm( 'N', 'C', m, n1, n2, -cone, b( 0_ilp, n1 ),ldb, a( n1*n1 &
                                    ), n1, alpha, b( 0_ilp, 0_ilp ),ldb )
                          call stdlib_ztrsm( 'R', 'U', 'C', diag, m, n1, cone,a( 0_ilp ), n1, b( 0_ilp, 0_ilp &
                                    ), ldb )
                       else
                          ! side  ='r', n is odd, transr = 'c', uplo = 'l', and
                          ! trans = 'c'
                          call stdlib_ztrsm( 'R', 'U', 'N', diag, m, n1, alpha,a( 0_ilp ), n1, b( 0_ilp, &
                                    0_ilp ), ldb )
                          call stdlib_zgemm( 'N', 'N', m, n2, n1, -cone, b( 0_ilp, 0_ilp ),ldb, a( n1*n1 )&
                                    , n1, alpha, b( 0_ilp, n1 ),ldb )
                          call stdlib_ztrsm( 'R', 'L', 'C', diag, m, n2, cone,a( 1_ilp ), n1, b( 0_ilp, &
                                    n1 ), ldb )
                       end if
                    else
                       ! side  ='r', n is odd, transr = 'c', and uplo = 'u'
                       if( notrans ) then
                          ! side  ='r', n is odd, transr = 'c', uplo = 'u', and
                          ! trans = 'n'
                          call stdlib_ztrsm( 'R', 'U', 'N', diag, m, n1, alpha,a( n2*n2 ), n2, b( &
                                    0_ilp, 0_ilp ), ldb )
                          call stdlib_zgemm( 'N', 'C', m, n2, n1, -cone, b( 0_ilp, 0_ilp ),ldb, a( 0_ilp ), &
                                    n2, alpha, b( 0_ilp, n1 ),ldb )
                          call stdlib_ztrsm( 'R', 'L', 'C', diag, m, n2, cone,a( n1*n2 ), n2, b( &
                                    0_ilp, n1 ), ldb )
                       else
                          ! side  ='r', n is odd, transr = 'c', uplo = 'u', and
                          ! trans = 'c'
                          call stdlib_ztrsm( 'R', 'L', 'N', diag, m, n2, alpha,a( n1*n2 ), n2, b( &
                                    0_ilp, n1 ), ldb )
                          call stdlib_zgemm( 'N', 'N', m, n1, n2, -cone, b( 0_ilp, n1 ),ldb, a( 0_ilp ), &
                                    n2, alpha, b( 0_ilp, 0_ilp ),ldb )
                          call stdlib_ztrsm( 'R', 'U', 'C', diag, m, n1, cone,a( n2*n2 ), n2, b( &
                                    0_ilp, 0_ilp ), ldb )
                       end if
                    end if
                 end if
              else
                 ! side = 'r' and n is even
                 if( normaltransr ) then
                    ! side = 'r', n is even, and transr = 'n'
                    if( lower ) then
                       ! side  ='r', n is even, transr = 'n', and uplo = 'l'
                       if( notrans ) then
                          ! side  ='r', n is even, transr = 'n', uplo = 'l',
                          ! and trans = 'n'
                          call stdlib_ztrsm( 'R', 'U', 'C', diag, m, k, alpha,a( 0_ilp ), n+1, b( 0_ilp, &
                                    k ), ldb )
                          call stdlib_zgemm( 'N', 'N', m, k, k, -cone, b( 0_ilp, k ),ldb, a( k+1 ), n+&
                                    1_ilp, alpha, b( 0_ilp, 0_ilp ),ldb )
                          call stdlib_ztrsm( 'R', 'L', 'N', diag, m, k, cone,a( 1_ilp ), n+1, b( 0_ilp, 0_ilp &
                                    ), ldb )
                       else
                          ! side  ='r', n is even, transr = 'n', uplo = 'l',
                          ! and trans = 'c'
                          call stdlib_ztrsm( 'R', 'L', 'C', diag, m, k, alpha,a( 1_ilp ), n+1, b( 0_ilp, &
                                    0_ilp ), ldb )
                          call stdlib_zgemm( 'N', 'C', m, k, k, -cone, b( 0_ilp, 0_ilp ),ldb, a( k+1 ), n+&
                                    1_ilp, alpha, b( 0_ilp, k ),ldb )
                          call stdlib_ztrsm( 'R', 'U', 'N', diag, m, k, cone,a( 0_ilp ), n+1, b( 0_ilp, k &
                                    ), ldb )
                       end if
                    else
                       ! side  ='r', n is even, transr = 'n', and uplo = 'u'
                       if( notrans ) then
                          ! side  ='r', n is even, transr = 'n', uplo = 'u',
                          ! and trans = 'n'
                          call stdlib_ztrsm( 'R', 'L', 'C', diag, m, k, alpha,a( k+1 ), n+1, b( 0_ilp,&
                                     0_ilp ), ldb )
                          call stdlib_zgemm( 'N', 'N', m, k, k, -cone, b( 0_ilp, 0_ilp ),ldb, a( 0_ilp ), n+1,&
                                     alpha, b( 0_ilp, k ),ldb )
                          call stdlib_ztrsm( 'R', 'U', 'N', diag, m, k, cone,a( k ), n+1, b( 0_ilp, k &
                                    ), ldb )
                       else
                          ! side  ='r', n is even, transr = 'n', uplo = 'u',
                          ! and trans = 'c'
                          call stdlib_ztrsm( 'R', 'U', 'C', diag, m, k, alpha,a( k ), n+1, b( 0_ilp, &
                                    k ), ldb )
                          call stdlib_zgemm( 'N', 'C', m, k, k, -cone, b( 0_ilp, k ),ldb, a( 0_ilp ), n+1,&
                                     alpha, b( 0_ilp, 0_ilp ),ldb )
                          call stdlib_ztrsm( 'R', 'L', 'N', diag, m, k, cone,a( k+1 ), n+1, b( 0_ilp, &
                                    0_ilp ), ldb )
                       end if
                    end if
                 else
                    ! side = 'r', n is even, and transr = 'c'
                    if( lower ) then
                       ! side  ='r', n is even, transr = 'c', and uplo = 'l'
                       if( notrans ) then
                          ! side  ='r', n is even, transr = 'c', uplo = 'l',
                          ! and trans = 'n'
                          call stdlib_ztrsm( 'R', 'L', 'N', diag, m, k, alpha,a( 0_ilp ), k, b( 0_ilp, k )&
                                    , ldb )
                          call stdlib_zgemm( 'N', 'C', m, k, k, -cone, b( 0_ilp, k ),ldb, a( ( k+1 )&
                                    *k ), k, alpha,b( 0_ilp, 0_ilp ), ldb )
                          call stdlib_ztrsm( 'R', 'U', 'C', diag, m, k, cone,a( k ), k, b( 0_ilp, 0_ilp ),&
                                     ldb )
                       else
                          ! side  ='r', n is even, transr = 'c', uplo = 'l',
                          ! and trans = 'c'
                          call stdlib_ztrsm( 'R', 'U', 'N', diag, m, k, alpha,a( k ), k, b( 0_ilp, 0_ilp )&
                                    , ldb )
                          call stdlib_zgemm( 'N', 'N', m, k, k, -cone, b( 0_ilp, 0_ilp ),ldb, a( ( k+1 )&
                                    *k ), k, alpha,b( 0_ilp, k ), ldb )
                          call stdlib_ztrsm( 'R', 'L', 'C', diag, m, k, cone,a( 0_ilp ), k, b( 0_ilp, k ),&
                                     ldb )
                       end if
                    else
                       ! side  ='r', n is even, transr = 'c', and uplo = 'u'
                       if( notrans ) then
                          ! side  ='r', n is even, transr = 'c', uplo = 'u',
                          ! and trans = 'n'
                          call stdlib_ztrsm( 'R', 'U', 'N', diag, m, k, alpha,a( ( k+1 )*k ), k, &
                                    b( 0_ilp, 0_ilp ), ldb )
                          call stdlib_zgemm( 'N', 'C', m, k, k, -cone, b( 0_ilp, 0_ilp ),ldb, a( 0_ilp ), k, &
                                    alpha, b( 0_ilp, k ), ldb )
                          call stdlib_ztrsm( 'R', 'L', 'C', diag, m, k, cone,a( k*k ), k, b( 0_ilp, k &
                                    ), ldb )
                       else
                          ! side  ='r', n is even, transr = 'c', uplo = 'u',
                          ! and trans = 'c'
                          call stdlib_ztrsm( 'R', 'L', 'N', diag, m, k, alpha,a( k*k ), k, b( 0_ilp, &
                                    k ), ldb )
                          call stdlib_zgemm( 'N', 'N', m, k, k, -cone, b( 0_ilp, k ),ldb, a( 0_ilp ), k, &
                                    alpha, b( 0_ilp, 0_ilp ), ldb )
                          call stdlib_ztrsm( 'R', 'U', 'C', diag, m, k, cone,a( ( k+1 )*k ), k, b(&
                                     0_ilp, 0_ilp ), ldb )
                       end if
                    end if
                 end if
              end if
           end if
           return
     end subroutine stdlib_ztfsm




     pure module subroutine stdlib_ssfrk( transr, uplo, trans, n, k, alpha, a, lda, beta,c )
     !! Level 3 BLAS like routine for C in RFP Format.
     !! SSFRK performs one of the symmetric rank--k operations
     !! C := alpha*A*A**T + beta*C,
     !! or
     !! C := alpha*A**T*A + beta*C,
     !! where alpha and beta are real scalars, C is an n--by--n symmetric
     !! matrix and A is an n--by--k matrix in the first case and a k--by--n
     !! matrix in the second case.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           real(sp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: k, lda, n
           character, intent(in) :: trans, transr, uplo
           ! Array Arguments 
           real(sp), intent(in) :: a(lda,*)
           real(sp), intent(inout) :: c(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lower, normaltransr, nisodd, notrans
           integer(ilp) :: info, nrowa, j, nk, n1, n2
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           normaltransr = stdlib_lsame( transr, 'N' )
           lower = stdlib_lsame( uplo, 'L' )
           notrans = stdlib_lsame( trans, 'N' )
           if( notrans ) then
              nrowa = n
           else
              nrowa = k
           end if
           if( .not.normaltransr .and. .not.stdlib_lsame( transr, 'T' ) ) then
              info = -1_ilp
           else if( .not.lower .and. .not.stdlib_lsame( uplo, 'U' ) ) then
              info = -2_ilp
           else if( .not.notrans .and. .not.stdlib_lsame( trans, 'T' ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( k<0_ilp ) then
              info = -5_ilp
           else if( lda<max( 1_ilp, nrowa ) ) then
              info = -8_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SSFRK ', -info )
              return
           end if
           ! quick return if possible.
           ! the quick return case: ((alpha==0).and.(beta/=zero)) is not
           ! done (it is in stdlib_ssyrk for example) and left in the general case.
           if( ( n==0_ilp ) .or. ( ( ( alpha==zero ) .or. ( k==0_ilp ) ) .and.( beta==one ) ) )&
                     return
           if( ( alpha==zero ) .and. ( beta==zero ) ) then
              do j = 1, ( ( n*( n+1 ) ) / 2 )
                 c( j ) = zero
              end do
              return
           end if
           ! c is n-by-n.
           ! if n is odd, set nisodd = .true., and n1 and n2.
           ! if n is even, nisodd = .false., and nk.
           if( mod( n, 2_ilp )==0_ilp ) then
              nisodd = .false.
              nk = n / 2_ilp
           else
              nisodd = .true.
              if( lower ) then
                 n2 = n / 2_ilp
                 n1 = n - n2
              else
                 n1 = n / 2_ilp
                 n2 = n - n1
              end if
           end if
           if( nisodd ) then
              ! n is odd
              if( normaltransr ) then
                 ! n is odd and transr = 'n'
                 if( lower ) then
                    ! n is odd, transr = 'n', and uplo = 'l'
                    if( notrans ) then
                       ! n is odd, transr = 'n', uplo = 'l', and trans = 'n'
                       call stdlib_ssyrk( 'L', 'N', n1, k, alpha, a( 1_ilp, 1_ilp ), lda,beta, c( 1_ilp ), n )
                                 
                       call stdlib_ssyrk( 'U', 'N', n2, k, alpha, a( n1+1, 1_ilp ), lda,beta, c( n+1 )&
                                 , n )
                       call stdlib_sgemm( 'N', 'T', n2, n1, k, alpha, a( n1+1, 1_ilp ),lda, a( 1_ilp, 1_ilp ),&
                                  lda, beta, c( n1+1 ), n )
                    else
                       ! n is odd, transr = 'n', uplo = 'l', and trans = 't'
                       call stdlib_ssyrk( 'L', 'T', n1, k, alpha, a( 1_ilp, 1_ilp ), lda,beta, c( 1_ilp ), n )
                                 
                       call stdlib_ssyrk( 'U', 'T', n2, k, alpha, a( 1_ilp, n1+1 ), lda,beta, c( n+1 )&
                                 , n )
                       call stdlib_sgemm( 'T', 'N', n2, n1, k, alpha, a( 1_ilp, n1+1 ),lda, a( 1_ilp, 1_ilp ),&
                                  lda, beta, c( n1+1 ), n )
                    end if
                 else
                    ! n is odd, transr = 'n', and uplo = 'u'
                    if( notrans ) then
                       ! n is odd, transr = 'n', uplo = 'u', and trans = 'n'
                       call stdlib_ssyrk( 'L', 'N', n1, k, alpha, a( 1_ilp, 1_ilp ), lda,beta, c( n2+1 ), &
                                 n )
                       call stdlib_ssyrk( 'U', 'N', n2, k, alpha, a( n2, 1_ilp ), lda,beta, c( n1+1 ),&
                                  n )
                       call stdlib_sgemm( 'N', 'T', n1, n2, k, alpha, a( 1_ilp, 1_ilp ),lda, a( n2, 1_ilp ), &
                                 lda, beta, c( 1_ilp ), n )
                    else
                       ! n is odd, transr = 'n', uplo = 'u', and trans = 't'
                       call stdlib_ssyrk( 'L', 'T', n1, k, alpha, a( 1_ilp, 1_ilp ), lda,beta, c( n2+1 ), &
                                 n )
                       call stdlib_ssyrk( 'U', 'T', n2, k, alpha, a( 1_ilp, n2 ), lda,beta, c( n1+1 ),&
                                  n )
                       call stdlib_sgemm( 'T', 'N', n1, n2, k, alpha, a( 1_ilp, 1_ilp ),lda, a( 1_ilp, n2 ), &
                                 lda, beta, c( 1_ilp ), n )
                    end if
                 end if
              else
                 ! n is odd, and transr = 't'
                 if( lower ) then
                    ! n is odd, transr = 't', and uplo = 'l'
                    if( notrans ) then
                       ! n is odd, transr = 't', uplo = 'l', and trans = 'n'
                       call stdlib_ssyrk( 'U', 'N', n1, k, alpha, a( 1_ilp, 1_ilp ), lda,beta, c( 1_ilp ), n1 &
                                 )
                       call stdlib_ssyrk( 'L', 'N', n2, k, alpha, a( n1+1, 1_ilp ), lda,beta, c( 2_ilp ), &
                                 n1 )
                       call stdlib_sgemm( 'N', 'T', n1, n2, k, alpha, a( 1_ilp, 1_ilp ),lda, a( n1+1, 1_ilp ),&
                                  lda, beta,c( n1*n1+1 ), n1 )
                    else
                       ! n is odd, transr = 't', uplo = 'l', and trans = 't'
                       call stdlib_ssyrk( 'U', 'T', n1, k, alpha, a( 1_ilp, 1_ilp ), lda,beta, c( 1_ilp ), n1 &
                                 )
                       call stdlib_ssyrk( 'L', 'T', n2, k, alpha, a( 1_ilp, n1+1 ), lda,beta, c( 2_ilp ), &
                                 n1 )
                       call stdlib_sgemm( 'T', 'N', n1, n2, k, alpha, a( 1_ilp, 1_ilp ),lda, a( 1_ilp, n1+1 ),&
                                  lda, beta,c( n1*n1+1 ), n1 )
                    end if
                 else
                    ! n is odd, transr = 't', and uplo = 'u'
                    if( notrans ) then
                       ! n is odd, transr = 't', uplo = 'u', and trans = 'n'
                       call stdlib_ssyrk( 'U', 'N', n1, k, alpha, a( 1_ilp, 1_ilp ), lda,beta, c( n2*n2+1 &
                                 ), n2 )
                       call stdlib_ssyrk( 'L', 'N', n2, k, alpha, a( n1+1, 1_ilp ), lda,beta, c( &
                                 n1*n2+1 ), n2 )
                       call stdlib_sgemm( 'N', 'T', n2, n1, k, alpha, a( n1+1, 1_ilp ),lda, a( 1_ilp, 1_ilp ),&
                                  lda, beta, c( 1_ilp ), n2 )
                    else
                       ! n is odd, transr = 't', uplo = 'u', and trans = 't'
                       call stdlib_ssyrk( 'U', 'T', n1, k, alpha, a( 1_ilp, 1_ilp ), lda,beta, c( n2*n2+1 &
                                 ), n2 )
                       call stdlib_ssyrk( 'L', 'T', n2, k, alpha, a( 1_ilp, n1+1 ), lda,beta, c( &
                                 n1*n2+1 ), n2 )
                       call stdlib_sgemm( 'T', 'N', n2, n1, k, alpha, a( 1_ilp, n1+1 ),lda, a( 1_ilp, 1_ilp ),&
                                  lda, beta, c( 1_ilp ), n2 )
                    end if
                 end if
              end if
           else
              ! n is even
              if( normaltransr ) then
                 ! n is even and transr = 'n'
                 if( lower ) then
                    ! n is even, transr = 'n', and uplo = 'l'
                    if( notrans ) then
                       ! n is even, transr = 'n', uplo = 'l', and trans = 'n'
                       call stdlib_ssyrk( 'L', 'N', nk, k, alpha, a( 1_ilp, 1_ilp ), lda,beta, c( 2_ilp ), n+&
                                 1_ilp )
                       call stdlib_ssyrk( 'U', 'N', nk, k, alpha, a( nk+1, 1_ilp ), lda,beta, c( 1_ilp ), &
                                 n+1 )
                       call stdlib_sgemm( 'N', 'T', nk, nk, k, alpha, a( nk+1, 1_ilp ),lda, a( 1_ilp, 1_ilp ),&
                                  lda, beta, c( nk+2 ),n+1 )
                    else
                       ! n is even, transr = 'n', uplo = 'l', and trans = 't'
                       call stdlib_ssyrk( 'L', 'T', nk, k, alpha, a( 1_ilp, 1_ilp ), lda,beta, c( 2_ilp ), n+&
                                 1_ilp )
                       call stdlib_ssyrk( 'U', 'T', nk, k, alpha, a( 1_ilp, nk+1 ), lda,beta, c( 1_ilp ), &
                                 n+1 )
                       call stdlib_sgemm( 'T', 'N', nk, nk, k, alpha, a( 1_ilp, nk+1 ),lda, a( 1_ilp, 1_ilp ),&
                                  lda, beta, c( nk+2 ),n+1 )
                    end if
                 else
                    ! n is even, transr = 'n', and uplo = 'u'
                    if( notrans ) then
                       ! n is even, transr = 'n', uplo = 'u', and trans = 'n'
                       call stdlib_ssyrk( 'L', 'N', nk, k, alpha, a( 1_ilp, 1_ilp ), lda,beta, c( nk+2 ), &
                                 n+1 )
                       call stdlib_ssyrk( 'U', 'N', nk, k, alpha, a( nk+1, 1_ilp ), lda,beta, c( nk+1 &
                                 ), n+1 )
                       call stdlib_sgemm( 'N', 'T', nk, nk, k, alpha, a( 1_ilp, 1_ilp ),lda, a( nk+1, 1_ilp ),&
                                  lda, beta, c( 1_ilp ),n+1 )
                    else
                       ! n is even, transr = 'n', uplo = 'u', and trans = 't'
                       call stdlib_ssyrk( 'L', 'T', nk, k, alpha, a( 1_ilp, 1_ilp ), lda,beta, c( nk+2 ), &
                                 n+1 )
                       call stdlib_ssyrk( 'U', 'T', nk, k, alpha, a( 1_ilp, nk+1 ), lda,beta, c( nk+1 &
                                 ), n+1 )
                       call stdlib_sgemm( 'T', 'N', nk, nk, k, alpha, a( 1_ilp, 1_ilp ),lda, a( 1_ilp, nk+1 ),&
                                  lda, beta, c( 1_ilp ),n+1 )
                    end if
                 end if
              else
                 ! n is even, and transr = 't'
                 if( lower ) then
                    ! n is even, transr = 't', and uplo = 'l'
                    if( notrans ) then
                       ! n is even, transr = 't', uplo = 'l', and trans = 'n'
                       call stdlib_ssyrk( 'U', 'N', nk, k, alpha, a( 1_ilp, 1_ilp ), lda,beta, c( nk+1 ), &
                                 nk )
                       call stdlib_ssyrk( 'L', 'N', nk, k, alpha, a( nk+1, 1_ilp ), lda,beta, c( 1_ilp ), &
                                 nk )
                       call stdlib_sgemm( 'N', 'T', nk, nk, k, alpha, a( 1_ilp, 1_ilp ),lda, a( nk+1, 1_ilp ),&
                                  lda, beta,c( ( ( nk+1 )*nk )+1_ilp ), nk )
                    else
                       ! n is even, transr = 't', uplo = 'l', and trans = 't'
                       call stdlib_ssyrk( 'U', 'T', nk, k, alpha, a( 1_ilp, 1_ilp ), lda,beta, c( nk+1 ), &
                                 nk )
                       call stdlib_ssyrk( 'L', 'T', nk, k, alpha, a( 1_ilp, nk+1 ), lda,beta, c( 1_ilp ), &
                                 nk )
                       call stdlib_sgemm( 'T', 'N', nk, nk, k, alpha, a( 1_ilp, 1_ilp ),lda, a( 1_ilp, nk+1 ),&
                                  lda, beta,c( ( ( nk+1 )*nk )+1_ilp ), nk )
                    end if
                 else
                    ! n is even, transr = 't', and uplo = 'u'
                    if( notrans ) then
                       ! n is even, transr = 't', uplo = 'u', and trans = 'n'
                       call stdlib_ssyrk( 'U', 'N', nk, k, alpha, a( 1_ilp, 1_ilp ), lda,beta, c( nk*( nk+&
                                 1_ilp )+1_ilp ), nk )
                       call stdlib_ssyrk( 'L', 'N', nk, k, alpha, a( nk+1, 1_ilp ), lda,beta, c( &
                                 nk*nk+1 ), nk )
                       call stdlib_sgemm( 'N', 'T', nk, nk, k, alpha, a( nk+1, 1_ilp ),lda, a( 1_ilp, 1_ilp ),&
                                  lda, beta, c( 1_ilp ), nk )
                    else
                       ! n is even, transr = 't', uplo = 'u', and trans = 't'
                       call stdlib_ssyrk( 'U', 'T', nk, k, alpha, a( 1_ilp, 1_ilp ), lda,beta, c( nk*( nk+&
                                 1_ilp )+1_ilp ), nk )
                       call stdlib_ssyrk( 'L', 'T', nk, k, alpha, a( 1_ilp, nk+1 ), lda,beta, c( &
                                 nk*nk+1 ), nk )
                       call stdlib_sgemm( 'T', 'N', nk, nk, k, alpha, a( 1_ilp, nk+1 ),lda, a( 1_ilp, 1_ilp ),&
                                  lda, beta, c( 1_ilp ), nk )
                    end if
                 end if
              end if
           end if
           return
     end subroutine stdlib_ssfrk

     pure module subroutine stdlib_dsfrk( transr, uplo, trans, n, k, alpha, a, lda, beta,c )
     !! Level 3 BLAS like routine for C in RFP Format.
     !! DSFRK performs one of the symmetric rank--k operations
     !! C := alpha*A*A**T + beta*C,
     !! or
     !! C := alpha*A**T*A + beta*C,
     !! where alpha and beta are real scalars, C is an n--by--n symmetric
     !! matrix and A is an n--by--k matrix in the first case and a k--by--n
     !! matrix in the second case.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           real(dp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: k, lda, n
           character, intent(in) :: trans, transr, uplo
           ! Array Arguments 
           real(dp), intent(in) :: a(lda,*)
           real(dp), intent(inout) :: c(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lower, normaltransr, nisodd, notrans
           integer(ilp) :: info, nrowa, j, nk, n1, n2
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           normaltransr = stdlib_lsame( transr, 'N' )
           lower = stdlib_lsame( uplo, 'L' )
           notrans = stdlib_lsame( trans, 'N' )
           if( notrans ) then
              nrowa = n
           else
              nrowa = k
           end if
           if( .not.normaltransr .and. .not.stdlib_lsame( transr, 'T' ) ) then
              info = -1_ilp
           else if( .not.lower .and. .not.stdlib_lsame( uplo, 'U' ) ) then
              info = -2_ilp
           else if( .not.notrans .and. .not.stdlib_lsame( trans, 'T' ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( k<0_ilp ) then
              info = -5_ilp
           else if( lda<max( 1_ilp, nrowa ) ) then
              info = -8_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DSFRK ', -info )
              return
           end if
           ! quick return if possible.
           ! the quick return case: ((alpha==0).and.(beta/=zero)) is not
           ! done (it is in stdlib_dsyrk for example) and left in the general case.
           if( ( n==0_ilp ) .or. ( ( ( alpha==zero ) .or. ( k==0_ilp ) ) .and.( beta==one ) ) )&
                     return
           if( ( alpha==zero ) .and. ( beta==zero ) ) then
              do j = 1, ( ( n*( n+1 ) ) / 2 )
                 c( j ) = zero
              end do
              return
           end if
           ! c is n-by-n.
           ! if n is odd, set nisodd = .true., and n1 and n2.
           ! if n is even, nisodd = .false., and nk.
           if( mod( n, 2_ilp )==0_ilp ) then
              nisodd = .false.
              nk = n / 2_ilp
           else
              nisodd = .true.
              if( lower ) then
                 n2 = n / 2_ilp
                 n1 = n - n2
              else
                 n1 = n / 2_ilp
                 n2 = n - n1
              end if
           end if
           if( nisodd ) then
              ! n is odd
              if( normaltransr ) then
                 ! n is odd and transr = 'n'
                 if( lower ) then
                    ! n is odd, transr = 'n', and uplo = 'l'
                    if( notrans ) then
                       ! n is odd, transr = 'n', uplo = 'l', and trans = 'n'
                       call stdlib_dsyrk( 'L', 'N', n1, k, alpha, a( 1_ilp, 1_ilp ), lda,beta, c( 1_ilp ), n )
                                 
                       call stdlib_dsyrk( 'U', 'N', n2, k, alpha, a( n1+1, 1_ilp ), lda,beta, c( n+1 )&
                                 , n )
                       call stdlib_dgemm( 'N', 'T', n2, n1, k, alpha, a( n1+1, 1_ilp ),lda, a( 1_ilp, 1_ilp ),&
                                  lda, beta, c( n1+1 ), n )
                    else
                       ! n is odd, transr = 'n', uplo = 'l', and trans = 't'
                       call stdlib_dsyrk( 'L', 'T', n1, k, alpha, a( 1_ilp, 1_ilp ), lda,beta, c( 1_ilp ), n )
                                 
                       call stdlib_dsyrk( 'U', 'T', n2, k, alpha, a( 1_ilp, n1+1 ), lda,beta, c( n+1 )&
                                 , n )
                       call stdlib_dgemm( 'T', 'N', n2, n1, k, alpha, a( 1_ilp, n1+1 ),lda, a( 1_ilp, 1_ilp ),&
                                  lda, beta, c( n1+1 ), n )
                    end if
                 else
                    ! n is odd, transr = 'n', and uplo = 'u'
                    if( notrans ) then
                       ! n is odd, transr = 'n', uplo = 'u', and trans = 'n'
                       call stdlib_dsyrk( 'L', 'N', n1, k, alpha, a( 1_ilp, 1_ilp ), lda,beta, c( n2+1 ), &
                                 n )
                       call stdlib_dsyrk( 'U', 'N', n2, k, alpha, a( n2, 1_ilp ), lda,beta, c( n1+1 ),&
                                  n )
                       call stdlib_dgemm( 'N', 'T', n1, n2, k, alpha, a( 1_ilp, 1_ilp ),lda, a( n2, 1_ilp ), &
                                 lda, beta, c( 1_ilp ), n )
                    else
                       ! n is odd, transr = 'n', uplo = 'u', and trans = 't'
                       call stdlib_dsyrk( 'L', 'T', n1, k, alpha, a( 1_ilp, 1_ilp ), lda,beta, c( n2+1 ), &
                                 n )
                       call stdlib_dsyrk( 'U', 'T', n2, k, alpha, a( 1_ilp, n2 ), lda,beta, c( n1+1 ),&
                                  n )
                       call stdlib_dgemm( 'T', 'N', n1, n2, k, alpha, a( 1_ilp, 1_ilp ),lda, a( 1_ilp, n2 ), &
                                 lda, beta, c( 1_ilp ), n )
                    end if
                 end if
              else
                 ! n is odd, and transr = 't'
                 if( lower ) then
                    ! n is odd, transr = 't', and uplo = 'l'
                    if( notrans ) then
                       ! n is odd, transr = 't', uplo = 'l', and trans = 'n'
                       call stdlib_dsyrk( 'U', 'N', n1, k, alpha, a( 1_ilp, 1_ilp ), lda,beta, c( 1_ilp ), n1 &
                                 )
                       call stdlib_dsyrk( 'L', 'N', n2, k, alpha, a( n1+1, 1_ilp ), lda,beta, c( 2_ilp ), &
                                 n1 )
                       call stdlib_dgemm( 'N', 'T', n1, n2, k, alpha, a( 1_ilp, 1_ilp ),lda, a( n1+1, 1_ilp ),&
                                  lda, beta,c( n1*n1+1 ), n1 )
                    else
                       ! n is odd, transr = 't', uplo = 'l', and trans = 't'
                       call stdlib_dsyrk( 'U', 'T', n1, k, alpha, a( 1_ilp, 1_ilp ), lda,beta, c( 1_ilp ), n1 &
                                 )
                       call stdlib_dsyrk( 'L', 'T', n2, k, alpha, a( 1_ilp, n1+1 ), lda,beta, c( 2_ilp ), &
                                 n1 )
                       call stdlib_dgemm( 'T', 'N', n1, n2, k, alpha, a( 1_ilp, 1_ilp ),lda, a( 1_ilp, n1+1 ),&
                                  lda, beta,c( n1*n1+1 ), n1 )
                    end if
                 else
                    ! n is odd, transr = 't', and uplo = 'u'
                    if( notrans ) then
                       ! n is odd, transr = 't', uplo = 'u', and trans = 'n'
                       call stdlib_dsyrk( 'U', 'N', n1, k, alpha, a( 1_ilp, 1_ilp ), lda,beta, c( n2*n2+1 &
                                 ), n2 )
                       call stdlib_dsyrk( 'L', 'N', n2, k, alpha, a( n1+1, 1_ilp ), lda,beta, c( &
                                 n1*n2+1 ), n2 )
                       call stdlib_dgemm( 'N', 'T', n2, n1, k, alpha, a( n1+1, 1_ilp ),lda, a( 1_ilp, 1_ilp ),&
                                  lda, beta, c( 1_ilp ), n2 )
                    else
                       ! n is odd, transr = 't', uplo = 'u', and trans = 't'
                       call stdlib_dsyrk( 'U', 'T', n1, k, alpha, a( 1_ilp, 1_ilp ), lda,beta, c( n2*n2+1 &
                                 ), n2 )
                       call stdlib_dsyrk( 'L', 'T', n2, k, alpha, a( 1_ilp, n1+1 ), lda,beta, c( &
                                 n1*n2+1 ), n2 )
                       call stdlib_dgemm( 'T', 'N', n2, n1, k, alpha, a( 1_ilp, n1+1 ),lda, a( 1_ilp, 1_ilp ),&
                                  lda, beta, c( 1_ilp ), n2 )
                    end if
                 end if
              end if
           else
              ! n is even
              if( normaltransr ) then
                 ! n is even and transr = 'n'
                 if( lower ) then
                    ! n is even, transr = 'n', and uplo = 'l'
                    if( notrans ) then
                       ! n is even, transr = 'n', uplo = 'l', and trans = 'n'
                       call stdlib_dsyrk( 'L', 'N', nk, k, alpha, a( 1_ilp, 1_ilp ), lda,beta, c( 2_ilp ), n+&
                                 1_ilp )
                       call stdlib_dsyrk( 'U', 'N', nk, k, alpha, a( nk+1, 1_ilp ), lda,beta, c( 1_ilp ), &
                                 n+1 )
                       call stdlib_dgemm( 'N', 'T', nk, nk, k, alpha, a( nk+1, 1_ilp ),lda, a( 1_ilp, 1_ilp ),&
                                  lda, beta, c( nk+2 ),n+1 )
                    else
                       ! n is even, transr = 'n', uplo = 'l', and trans = 't'
                       call stdlib_dsyrk( 'L', 'T', nk, k, alpha, a( 1_ilp, 1_ilp ), lda,beta, c( 2_ilp ), n+&
                                 1_ilp )
                       call stdlib_dsyrk( 'U', 'T', nk, k, alpha, a( 1_ilp, nk+1 ), lda,beta, c( 1_ilp ), &
                                 n+1 )
                       call stdlib_dgemm( 'T', 'N', nk, nk, k, alpha, a( 1_ilp, nk+1 ),lda, a( 1_ilp, 1_ilp ),&
                                  lda, beta, c( nk+2 ),n+1 )
                    end if
                 else
                    ! n is even, transr = 'n', and uplo = 'u'
                    if( notrans ) then
                       ! n is even, transr = 'n', uplo = 'u', and trans = 'n'
                       call stdlib_dsyrk( 'L', 'N', nk, k, alpha, a( 1_ilp, 1_ilp ), lda,beta, c( nk+2 ), &
                                 n+1 )
                       call stdlib_dsyrk( 'U', 'N', nk, k, alpha, a( nk+1, 1_ilp ), lda,beta, c( nk+1 &
                                 ), n+1 )
                       call stdlib_dgemm( 'N', 'T', nk, nk, k, alpha, a( 1_ilp, 1_ilp ),lda, a( nk+1, 1_ilp ),&
                                  lda, beta, c( 1_ilp ),n+1 )
                    else
                       ! n is even, transr = 'n', uplo = 'u', and trans = 't'
                       call stdlib_dsyrk( 'L', 'T', nk, k, alpha, a( 1_ilp, 1_ilp ), lda,beta, c( nk+2 ), &
                                 n+1 )
                       call stdlib_dsyrk( 'U', 'T', nk, k, alpha, a( 1_ilp, nk+1 ), lda,beta, c( nk+1 &
                                 ), n+1 )
                       call stdlib_dgemm( 'T', 'N', nk, nk, k, alpha, a( 1_ilp, 1_ilp ),lda, a( 1_ilp, nk+1 ),&
                                  lda, beta, c( 1_ilp ),n+1 )
                    end if
                 end if
              else
                 ! n is even, and transr = 't'
                 if( lower ) then
                    ! n is even, transr = 't', and uplo = 'l'
                    if( notrans ) then
                       ! n is even, transr = 't', uplo = 'l', and trans = 'n'
                       call stdlib_dsyrk( 'U', 'N', nk, k, alpha, a( 1_ilp, 1_ilp ), lda,beta, c( nk+1 ), &
                                 nk )
                       call stdlib_dsyrk( 'L', 'N', nk, k, alpha, a( nk+1, 1_ilp ), lda,beta, c( 1_ilp ), &
                                 nk )
                       call stdlib_dgemm( 'N', 'T', nk, nk, k, alpha, a( 1_ilp, 1_ilp ),lda, a( nk+1, 1_ilp ),&
                                  lda, beta,c( ( ( nk+1 )*nk )+1_ilp ), nk )
                    else
                       ! n is even, transr = 't', uplo = 'l', and trans = 't'
                       call stdlib_dsyrk( 'U', 'T', nk, k, alpha, a( 1_ilp, 1_ilp ), lda,beta, c( nk+1 ), &
                                 nk )
                       call stdlib_dsyrk( 'L', 'T', nk, k, alpha, a( 1_ilp, nk+1 ), lda,beta, c( 1_ilp ), &
                                 nk )
                       call stdlib_dgemm( 'T', 'N', nk, nk, k, alpha, a( 1_ilp, 1_ilp ),lda, a( 1_ilp, nk+1 ),&
                                  lda, beta,c( ( ( nk+1 )*nk )+1_ilp ), nk )
                    end if
                 else
                    ! n is even, transr = 't', and uplo = 'u'
                    if( notrans ) then
                       ! n is even, transr = 't', uplo = 'u', and trans = 'n'
                       call stdlib_dsyrk( 'U', 'N', nk, k, alpha, a( 1_ilp, 1_ilp ), lda,beta, c( nk*( nk+&
                                 1_ilp )+1_ilp ), nk )
                       call stdlib_dsyrk( 'L', 'N', nk, k, alpha, a( nk+1, 1_ilp ), lda,beta, c( &
                                 nk*nk+1 ), nk )
                       call stdlib_dgemm( 'N', 'T', nk, nk, k, alpha, a( nk+1, 1_ilp ),lda, a( 1_ilp, 1_ilp ),&
                                  lda, beta, c( 1_ilp ), nk )
                    else
                       ! n is even, transr = 't', uplo = 'u', and trans = 't'
                       call stdlib_dsyrk( 'U', 'T', nk, k, alpha, a( 1_ilp, 1_ilp ), lda,beta, c( nk*( nk+&
                                 1_ilp )+1_ilp ), nk )
                       call stdlib_dsyrk( 'L', 'T', nk, k, alpha, a( 1_ilp, nk+1 ), lda,beta, c( &
                                 nk*nk+1 ), nk )
                       call stdlib_dgemm( 'T', 'N', nk, nk, k, alpha, a( 1_ilp, nk+1 ),lda, a( 1_ilp, 1_ilp ),&
                                  lda, beta, c( 1_ilp ), nk )
                    end if
                 end if
              end if
           end if
           return
     end subroutine stdlib_dsfrk



end submodule stdlib_lapack_blas_like_l3
