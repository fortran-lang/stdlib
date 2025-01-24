submodule(stdlib_lapack_solve) stdlib_lapack_solve_aux
  implicit none


  contains

     pure module subroutine stdlib_slacn2( n, v, x, isgn, est, kase, isave )
     !! SLACN2 estimates the 1-norm of a square, real matrix A.
     !! Reverse communication is used for evaluating matrix-vector products.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(inout) :: kase
           integer(ilp), intent(in) :: n
           real(sp), intent(inout) :: est
           ! Array Arguments 
           integer(ilp), intent(out) :: isgn(*)
           integer(ilp), intent(inout) :: isave(3_ilp)
           real(sp), intent(out) :: v(*)
           real(sp), intent(inout) :: x(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: itmax = 5_ilp
           
           
           ! Local Scalars 
           integer(ilp) :: i, jlast
           real(sp) :: altsgn, estold, temp, xs
           ! Intrinsic Functions 
           ! Executable Statements 
           if( kase==0_ilp ) then
              do i = 1, n
                 x( i ) = one / real( n,KIND=sp)
              end do
              kase = 1_ilp
              isave( 1_ilp ) = 1_ilp
              return
           end if
           go to ( 20, 40, 70, 110, 140 )isave( 1 )
           ! ................ entry   (isave( 1 ) = 1)
           ! first iteration.  x has been overwritten by a*x.
           20 continue
           if( n==1_ilp ) then
              v( 1_ilp ) = x( 1_ilp )
              est = abs( v( 1_ilp ) )
              ! ... quit
              go to 150
           end if
           est = stdlib_sasum( n, x, 1_ilp )
           do i = 1, n
              if( x(i)>=zero ) then
                 x(i) = one
              else
                 x(i) = -one
              end if
              isgn( i ) = nint( x( i ),KIND=ilp)
           end do
           kase = 2_ilp
           isave( 1_ilp ) = 2_ilp
           return
           ! ................ entry   (isave( 1 ) = 2)
           ! first iteration.  x has been overwritten by transpose(a)*x.
           40 continue
           isave( 2_ilp ) = stdlib_isamax( n, x, 1_ilp )
           isave( 3_ilp ) = 2_ilp
           ! main loop - iterations 2,3,...,itmax.
           50 continue
           do i = 1, n
              x( i ) = zero
           end do
           x( isave( 2_ilp ) ) = one
           kase = 1_ilp
           isave( 1_ilp ) = 3_ilp
           return
           ! ................ entry   (isave( 1 ) = 3)
           ! x has been overwritten by a*x.
           70 continue
           call stdlib_scopy( n, x, 1_ilp, v, 1_ilp )
           estold = est
           est = stdlib_sasum( n, v, 1_ilp )
           do i = 1, n
              if( x(i)>=zero ) then
                 xs = one
              else
                 xs = -one
              end if
              if( nint( xs,KIND=ilp)/=isgn( i ) )go to 90
           end do
           ! repeated sign vector detected, hence algorithm has converged.
           go to 120
           90 continue
           ! test for cycling.
           if( est<=estold )go to 120
           do i = 1, n
              if( x(i)>=zero ) then
                 x(i) = one
              else
                 x(i) = -one
              end if
              isgn( i ) = nint( x( i ),KIND=ilp)
           end do
           kase = 2_ilp
           isave( 1_ilp ) = 4_ilp
           return
           ! ................ entry   (isave( 1 ) = 4)
           ! x has been overwritten by transpose(a)*x.
           110 continue
           jlast = isave( 2_ilp )
           isave( 2_ilp ) = stdlib_isamax( n, x, 1_ilp )
           if( ( x( jlast )/=abs( x( isave( 2_ilp ) ) ) ) .and.( isave( 3_ilp )<itmax ) ) then
              isave( 3_ilp ) = isave( 3_ilp ) + 1_ilp
              go to 50
           end if
           ! iteration complete.  final stage.
           120 continue
           altsgn = one
           do i = 1, n
              x( i ) = altsgn*( one+real( i-1,KIND=sp) / real( n-1,KIND=sp) )
              altsgn = -altsgn
           end do
           kase = 1_ilp
           isave( 1_ilp ) = 5_ilp
           return
           ! ................ entry   (isave( 1 ) = 5)
           ! x has been overwritten by a*x.
           140 continue
           temp = two*( stdlib_sasum( n, x, 1_ilp ) / real( 3_ilp*n,KIND=sp) )
           if( temp>est ) then
              call stdlib_scopy( n, x, 1_ilp, v, 1_ilp )
              est = temp
           end if
           150 continue
           kase = 0_ilp
           return
     end subroutine stdlib_slacn2

     pure module subroutine stdlib_dlacn2( n, v, x, isgn, est, kase, isave )
     !! DLACN2 estimates the 1-norm of a square, real matrix A.
     !! Reverse communication is used for evaluating matrix-vector products.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(inout) :: kase
           integer(ilp), intent(in) :: n
           real(dp), intent(inout) :: est
           ! Array Arguments 
           integer(ilp), intent(out) :: isgn(*)
           integer(ilp), intent(inout) :: isave(3_ilp)
           real(dp), intent(out) :: v(*)
           real(dp), intent(inout) :: x(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: itmax = 5_ilp
           
           
           ! Local Scalars 
           integer(ilp) :: i, jlast
           real(dp) :: altsgn, estold, temp, xs
           ! Intrinsic Functions 
           ! Executable Statements 
           if( kase==0_ilp ) then
              do i = 1, n
                 x( i ) = one / real( n,KIND=dp)
              end do
              kase = 1_ilp
              isave( 1_ilp ) = 1_ilp
              return
           end if
           go to ( 20, 40, 70, 110, 140 )isave( 1 )
           ! ................ entry   (isave( 1 ) = 1)
           ! first iteration.  x has been overwritten by a*x.
           20 continue
           if( n==1_ilp ) then
              v( 1_ilp ) = x( 1_ilp )
              est = abs( v( 1_ilp ) )
              ! ... quit
              go to 150
           end if
           est = stdlib_dasum( n, x, 1_ilp )
           do i = 1, n
              if( x(i)>=zero ) then
                 x(i) = one
              else
                 x(i) = -one
              end if
              isgn( i ) = nint( x( i ),KIND=ilp)
           end do
           kase = 2_ilp
           isave( 1_ilp ) = 2_ilp
           return
           ! ................ entry   (isave( 1 ) = 2)
           ! first iteration.  x has been overwritten by transpose(a)*x.
           40 continue
           isave( 2_ilp ) = stdlib_idamax( n, x, 1_ilp )
           isave( 3_ilp ) = 2_ilp
           ! main loop - iterations 2,3,...,itmax.
           50 continue
           do i = 1, n
              x( i ) = zero
           end do
           x( isave( 2_ilp ) ) = one
           kase = 1_ilp
           isave( 1_ilp ) = 3_ilp
           return
           ! ................ entry   (isave( 1 ) = 3)
           ! x has been overwritten by a*x.
           70 continue
           call stdlib_dcopy( n, x, 1_ilp, v, 1_ilp )
           estold = est
           est = stdlib_dasum( n, v, 1_ilp )
           do i = 1, n
              if( x(i)>=zero ) then
                 xs = one
              else
                 xs = -one
              end if
              if( nint( xs,KIND=ilp)/=isgn( i ) )go to 90
           end do
           ! repeated sign vector detected, hence algorithm has converged.
           go to 120
           90 continue
           ! test for cycling.
           if( est<=estold )go to 120
           do i = 1, n
              if( x(i)>=zero ) then
                 x(i) = one
              else
                 x(i) = -one
              end if
              isgn( i ) = nint( x( i ),KIND=ilp)
           end do
           kase = 2_ilp
           isave( 1_ilp ) = 4_ilp
           return
           ! ................ entry   (isave( 1 ) = 4)
           ! x has been overwritten by transpose(a)*x.
           110 continue
           jlast = isave( 2_ilp )
           isave( 2_ilp ) = stdlib_idamax( n, x, 1_ilp )
           if( ( x( jlast )/=abs( x( isave( 2_ilp ) ) ) ) .and.( isave( 3_ilp )<itmax ) ) then
              isave( 3_ilp ) = isave( 3_ilp ) + 1_ilp
              go to 50
           end if
           ! iteration complete.  final stage.
           120 continue
           altsgn = one
           do i = 1, n
              x( i ) = altsgn*( one+real( i-1,KIND=dp) / real( n-1,KIND=dp) )
              altsgn = -altsgn
           end do
           kase = 1_ilp
           isave( 1_ilp ) = 5_ilp
           return
           ! ................ entry   (isave( 1 ) = 5)
           ! x has been overwritten by a*x.
           140 continue
           temp = two*( stdlib_dasum( n, x, 1_ilp ) / real( 3_ilp*n,KIND=dp) )
           if( temp>est ) then
              call stdlib_dcopy( n, x, 1_ilp, v, 1_ilp )
              est = temp
           end if
           150 continue
           kase = 0_ilp
           return
     end subroutine stdlib_dlacn2


     pure module subroutine stdlib_clacn2( n, v, x, est, kase, isave )
     !! CLACN2 estimates the 1-norm of a square, complex matrix A.
     !! Reverse communication is used for evaluating matrix-vector products.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(inout) :: kase
           integer(ilp), intent(in) :: n
           real(sp), intent(inout) :: est
           ! Array Arguments 
           integer(ilp), intent(inout) :: isave(3_ilp)
           complex(sp), intent(out) :: v(*)
           complex(sp), intent(inout) :: x(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: itmax = 5_ilp
           
           
           
           ! Local Scalars 
           integer(ilp) :: i, jlast
           real(sp) :: absxi, altsgn, estold, safmin, temp
           ! Intrinsic Functions 
           ! Executable Statements 
           safmin = stdlib_slamch( 'SAFE MINIMUM' )
           if( kase==0_ilp ) then
              do i = 1, n
                 x( i ) = cmplx( one / real( n,KIND=sp),KIND=sp)
              end do
              kase = 1_ilp
              isave( 1_ilp ) = 1_ilp
              return
           end if
           go to ( 20, 40, 70, 90, 120 )isave( 1 )
           ! ................ entry   (isave( 1 ) = 1)
           ! first iteration.  x has been overwritten by a*x.
           20 continue
           if( n==1_ilp ) then
              v( 1_ilp ) = x( 1_ilp )
              est = abs( v( 1_ilp ) )
              ! ... quit
              go to 130
           end if
           est = stdlib_scsum1( n, x, 1_ilp )
           do i = 1, n
              absxi = abs( x( i ) )
              if( absxi>safmin ) then
                 x( i ) = cmplx( real( x( i ),KIND=sp) / absxi,aimag( x( i ) ) / absxi,KIND=sp)
                           
              else
                 x( i ) = cone
              end if
           end do
           kase = 2_ilp
           isave( 1_ilp ) = 2_ilp
           return
           ! ................ entry   (isave( 1 ) = 2)
           ! first iteration.  x has been overwritten by ctrans(a)*x.
           40 continue
           isave( 2_ilp ) = stdlib_icmax1( n, x, 1_ilp )
           isave( 3_ilp ) = 2_ilp
           ! main loop - iterations 2,3,...,itmax.
           50 continue
           do i = 1, n
              x( i ) = czero
           end do
           x( isave( 2_ilp ) ) = cone
           kase = 1_ilp
           isave( 1_ilp ) = 3_ilp
           return
           ! ................ entry   (isave( 1 ) = 3)
           ! x has been overwritten by a*x.
           70 continue
           call stdlib_ccopy( n, x, 1_ilp, v, 1_ilp )
           estold = est
           est = stdlib_scsum1( n, v, 1_ilp )
           ! test for cycling.
           if( est<=estold )go to 100
           do i = 1, n
              absxi = abs( x( i ) )
              if( absxi>safmin ) then
                 x( i ) = cmplx( real( x( i ),KIND=sp) / absxi,aimag( x( i ) ) / absxi,KIND=sp)
                           
              else
                 x( i ) = cone
              end if
           end do
           kase = 2_ilp
           isave( 1_ilp ) = 4_ilp
           return
           ! ................ entry   (isave( 1 ) = 4)
           ! x has been overwritten by ctrans(a)*x.
           90 continue
           jlast = isave( 2_ilp )
           isave( 2_ilp ) = stdlib_icmax1( n, x, 1_ilp )
           if( ( abs( x( jlast ) )/=abs( x( isave( 2_ilp ) ) ) ) .and.( isave( 3_ilp )<itmax ) ) &
                     then
              isave( 3_ilp ) = isave( 3_ilp ) + 1_ilp
              go to 50
           end if
           ! iteration complete.  final stage.
           100 continue
           altsgn = one
           do i = 1, n
              x( i ) = cmplx( altsgn*( one + real( i-1,KIND=sp) / real( n-1,KIND=sp) ),KIND=sp)
                        
              altsgn = -altsgn
           end do
           kase = 1_ilp
           isave( 1_ilp ) = 5_ilp
           return
           ! ................ entry   (isave( 1 ) = 5)
           ! x has been overwritten by a*x.
           120 continue
           temp = two*( stdlib_scsum1( n, x, 1_ilp ) / real( 3_ilp*n,KIND=sp) )
           if( temp>est ) then
              call stdlib_ccopy( n, x, 1_ilp, v, 1_ilp )
              est = temp
           end if
           130 continue
           kase = 0_ilp
           return
     end subroutine stdlib_clacn2

     pure module subroutine stdlib_zlacn2( n, v, x, est, kase, isave )
     !! ZLACN2 estimates the 1-norm of a square, complex matrix A.
     !! Reverse communication is used for evaluating matrix-vector products.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(inout) :: kase
           integer(ilp), intent(in) :: n
           real(dp), intent(inout) :: est
           ! Array Arguments 
           integer(ilp), intent(inout) :: isave(3_ilp)
           complex(dp), intent(out) :: v(*)
           complex(dp), intent(inout) :: x(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: itmax = 5_ilp
           
           
           
           ! Local Scalars 
           integer(ilp) :: i, jlast
           real(dp) :: absxi, altsgn, estold, safmin, temp
           ! Intrinsic Functions 
           ! Executable Statements 
           safmin = stdlib_dlamch( 'SAFE MINIMUM' )
           if( kase==0_ilp ) then
              do i = 1, n
                 x( i ) = cmplx( one / real( n,KIND=dp),KIND=dp)
              end do
              kase = 1_ilp
              isave( 1_ilp ) = 1_ilp
              return
           end if
           go to ( 20, 40, 70, 90, 120 )isave( 1 )
           ! ................ entry   (isave( 1 ) = 1)
           ! first iteration.  x has been overwritten by a*x.
           20 continue
           if( n==1_ilp ) then
              v( 1_ilp ) = x( 1_ilp )
              est = abs( v( 1_ilp ) )
              ! ... quit
              go to 130
           end if
           est = stdlib_dzsum1( n, x, 1_ilp )
           do i = 1, n
              absxi = abs( x( i ) )
              if( absxi>safmin ) then
                 x( i ) = cmplx( real( x( i ),KIND=dp) / absxi,aimag( x( i ) ) / absxi,KIND=dp)
                           
              else
                 x( i ) = cone
              end if
           end do
           kase = 2_ilp
           isave( 1_ilp ) = 2_ilp
           return
           ! ................ entry   (isave( 1 ) = 2)
           ! first iteration.  x has been overwritten by ctrans(a)*x.
           40 continue
           isave( 2_ilp ) = stdlib_izmax1( n, x, 1_ilp )
           isave( 3_ilp ) = 2_ilp
           ! main loop - iterations 2,3,...,itmax.
           50 continue
           do i = 1, n
              x( i ) = czero
           end do
           x( isave( 2_ilp ) ) = cone
           kase = 1_ilp
           isave( 1_ilp ) = 3_ilp
           return
           ! ................ entry   (isave( 1 ) = 3)
           ! x has been overwritten by a*x.
           70 continue
           call stdlib_zcopy( n, x, 1_ilp, v, 1_ilp )
           estold = est
           est = stdlib_dzsum1( n, v, 1_ilp )
           ! test for cycling.
           if( est<=estold )go to 100
           do i = 1, n
              absxi = abs( x( i ) )
              if( absxi>safmin ) then
                 x( i ) = cmplx( real( x( i ),KIND=dp) / absxi,aimag( x( i ) ) / absxi,KIND=dp)
                           
              else
                 x( i ) = cone
              end if
           end do
           kase = 2_ilp
           isave( 1_ilp ) = 4_ilp
           return
           ! ................ entry   (isave( 1 ) = 4)
           ! x has been overwritten by ctrans(a)*x.
           90 continue
           jlast = isave( 2_ilp )
           isave( 2_ilp ) = stdlib_izmax1( n, x, 1_ilp )
           if( ( abs( x( jlast ) )/=abs( x( isave( 2_ilp ) ) ) ) .and.( isave( 3_ilp )<itmax ) ) &
                     then
              isave( 3_ilp ) = isave( 3_ilp ) + 1_ilp
              go to 50
           end if
           ! iteration complete.  final stage.
           100 continue
           altsgn = one
           do i = 1, n
              x( i ) = cmplx( altsgn*( one+real( i-1,KIND=dp) / real( n-1,KIND=dp) ),KIND=dp)
                        
              altsgn = -altsgn
           end do
           kase = 1_ilp
           isave( 1_ilp ) = 5_ilp
           return
           ! ................ entry   (isave( 1 ) = 5)
           ! x has been overwritten by a*x.
           120 continue
           temp = two*( stdlib_dzsum1( n, x, 1_ilp ) / real( 3_ilp*n,KIND=dp) )
           if( temp>est ) then
              call stdlib_zcopy( n, x, 1_ilp, v, 1_ilp )
              est = temp
           end if
           130 continue
           kase = 0_ilp
           return
     end subroutine stdlib_zlacn2




     module subroutine stdlib_slacon( n, v, x, isgn, est, kase )
     !! SLACON estimates the 1-norm of a square, real matrix A.
     !! Reverse communication is used for evaluating matrix-vector products.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(inout) :: kase
           integer(ilp), intent(in) :: n
           real(sp), intent(inout) :: est
           ! Array Arguments 
           integer(ilp), intent(out) :: isgn(*)
           real(sp), intent(out) :: v(*)
           real(sp), intent(inout) :: x(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: itmax = 5_ilp
           
           
           ! Local Scalars 
           integer(ilp) :: i, iter, j, jlast, jump
           real(sp) :: altsgn, estold, temp
           ! Intrinsic Functions 
           ! Save Statement 
           save
           ! Executable Statements 
           if( kase==0_ilp ) then
              do i = 1, n
                 x( i ) = one / real( n,KIND=sp)
              end do
              kase = 1_ilp
              jump = 1_ilp
              return
           end if
           go to ( 20, 40, 70, 110, 140 )jump
           ! ................ entry   (jump = 1)
           ! first iteration.  x has been overwritten by a*x.
           20 continue
           if( n==1_ilp ) then
              v( 1_ilp ) = x( 1_ilp )
              est = abs( v( 1_ilp ) )
              ! ... quit
              go to 150
           end if
           est = stdlib_sasum( n, x, 1_ilp )
           do i = 1, n
              x( i ) = sign( one, x( i ) )
              isgn( i ) = nint( x( i ),KIND=ilp)
           end do
           kase = 2_ilp
           jump = 2_ilp
           return
           ! ................ entry   (jump = 2)
           ! first iteration.  x has been overwritten by transpose(a)*x.
           40 continue
           j = stdlib_isamax( n, x, 1_ilp )
           iter = 2_ilp
           ! main loop - iterations 2,3,...,itmax.
           50 continue
           do i = 1, n
              x( i ) = zero
           end do
           x( j ) = one
           kase = 1_ilp
           jump = 3_ilp
           return
           ! ................ entry   (jump = 3)
           ! x has been overwritten by a*x.
           70 continue
           call stdlib_scopy( n, x, 1_ilp, v, 1_ilp )
           estold = est
           est = stdlib_sasum( n, v, 1_ilp )
           do i = 1, n
              if( nint( sign( one, x( i ) ),KIND=ilp)/=isgn( i ) )go to 90
           end do
           ! repeated sign vector detected, hence algorithm has converged.
           go to 120
           90 continue
           ! test for cycling.
           if( est<=estold )go to 120
           do i = 1, n
              x( i ) = sign( one, x( i ) )
              isgn( i ) = nint( x( i ),KIND=ilp)
           end do
           kase = 2_ilp
           jump = 4_ilp
           return
           ! ................ entry   (jump = 4)
           ! x has been overwritten by transpose(a)*x.
           110 continue
           jlast = j
           j = stdlib_isamax( n, x, 1_ilp )
           if( ( x( jlast )/=abs( x( j ) ) ) .and. ( iter<itmax ) ) then
              iter = iter + 1_ilp
              go to 50
           end if
           ! iteration complete.  final stage.
           120 continue
           altsgn = one
           do i = 1, n
              x( i ) = altsgn*( one+real( i-1,KIND=sp) / real( n-1,KIND=sp) )
              altsgn = -altsgn
           end do
           kase = 1_ilp
           jump = 5_ilp
           return
           ! ................ entry   (jump = 5)
           ! x has been overwritten by a*x.
           140 continue
           temp = two*( stdlib_sasum( n, x, 1_ilp ) / real( 3_ilp*n,KIND=sp) )
           if( temp>est ) then
              call stdlib_scopy( n, x, 1_ilp, v, 1_ilp )
              est = temp
           end if
           150 continue
           kase = 0_ilp
           return
     end subroutine stdlib_slacon

     module subroutine stdlib_dlacon( n, v, x, isgn, est, kase )
     !! DLACON estimates the 1-norm of a square, real matrix A.
     !! Reverse communication is used for evaluating matrix-vector products.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(inout) :: kase
           integer(ilp), intent(in) :: n
           real(dp), intent(inout) :: est
           ! Array Arguments 
           integer(ilp), intent(out) :: isgn(*)
           real(dp), intent(out) :: v(*)
           real(dp), intent(inout) :: x(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: itmax = 5_ilp
           
           
           ! Local Scalars 
           integer(ilp) :: i, iter, j, jlast, jump
           real(dp) :: altsgn, estold, temp
           ! Intrinsic Functions 
           ! Save Statement 
           save
           ! Executable Statements 
           if( kase==0_ilp ) then
              do i = 1, n
                 x( i ) = one / real( n,KIND=dp)
              end do
              kase = 1_ilp
              jump = 1_ilp
              return
           end if
           go to ( 20, 40, 70, 110, 140 )jump
           ! ................ entry   (jump = 1)
           ! first iteration.  x has been overwritten by a*x.
           20 continue
           if( n==1_ilp ) then
              v( 1_ilp ) = x( 1_ilp )
              est = abs( v( 1_ilp ) )
              ! ... quit
              go to 150
           end if
           est = stdlib_dasum( n, x, 1_ilp )
           do i = 1, n
              x( i ) = sign( one, x( i ) )
              isgn( i ) = nint( x( i ),KIND=ilp)
           end do
           kase = 2_ilp
           jump = 2_ilp
           return
           ! ................ entry   (jump = 2)
           ! first iteration.  x has been overwritten by transpose(a)*x.
           40 continue
           j = stdlib_idamax( n, x, 1_ilp )
           iter = 2_ilp
           ! main loop - iterations 2,3,...,itmax.
           50 continue
           do i = 1, n
              x( i ) = zero
           end do
           x( j ) = one
           kase = 1_ilp
           jump = 3_ilp
           return
           ! ................ entry   (jump = 3)
           ! x has been overwritten by a*x.
           70 continue
           call stdlib_dcopy( n, x, 1_ilp, v, 1_ilp )
           estold = est
           est = stdlib_dasum( n, v, 1_ilp )
           do i = 1, n
              if( nint( sign( one, x( i ) ),KIND=ilp)/=isgn( i ) )go to 90
           end do
           ! repeated sign vector detected, hence algorithm has converged.
           go to 120
           90 continue
           ! test for cycling.
           if( est<=estold )go to 120
           do i = 1, n
              x( i ) = sign( one, x( i ) )
              isgn( i ) = nint( x( i ),KIND=ilp)
           end do
           kase = 2_ilp
           jump = 4_ilp
           return
           ! ................ entry   (jump = 4)
           ! x has been overwritten by transpose(a)*x.
           110 continue
           jlast = j
           j = stdlib_idamax( n, x, 1_ilp )
           if( ( x( jlast )/=abs( x( j ) ) ) .and. ( iter<itmax ) ) then
              iter = iter + 1_ilp
              go to 50
           end if
           ! iteration complete.  final stage.
           120 continue
           altsgn = one
           do i = 1, n
              x( i ) = altsgn*( one+real( i-1,KIND=dp) / real( n-1,KIND=dp) )
              altsgn = -altsgn
           end do
           kase = 1_ilp
           jump = 5_ilp
           return
           ! ................ entry   (jump = 5)
           ! x has been overwritten by a*x.
           140 continue
           temp = two*( stdlib_dasum( n, x, 1_ilp ) / real( 3_ilp*n,KIND=dp) )
           if( temp>est ) then
              call stdlib_dcopy( n, x, 1_ilp, v, 1_ilp )
              est = temp
           end if
           150 continue
           kase = 0_ilp
           return
     end subroutine stdlib_dlacon


     module subroutine stdlib_clacon( n, v, x, est, kase )
     !! CLACON estimates the 1-norm of a square, complex matrix A.
     !! Reverse communication is used for evaluating matrix-vector products.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(inout) :: kase
           integer(ilp), intent(in) :: n
           real(sp), intent(inout) :: est
           ! Array Arguments 
           complex(sp), intent(out) :: v(n)
           complex(sp), intent(inout) :: x(n)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: itmax = 5_ilp
           
           
           
           ! Local Scalars 
           integer(ilp) :: i, iter, j, jlast, jump
           real(sp) :: absxi, altsgn, estold, safmin, temp
           ! Intrinsic Functions 
           ! Save Statement 
           save
           ! Executable Statements 
           safmin = stdlib_slamch( 'SAFE MINIMUM' )
           if( kase==0_ilp ) then
              do i = 1, n
                 x( i ) = cmplx( one / real( n,KIND=sp),KIND=sp)
              end do
              kase = 1_ilp
              jump = 1_ilp
              return
           end if
           go to ( 20, 40, 70, 90, 120 )jump
           ! ................ entry   (jump = 1)
           ! first iteration.  x has been overwritten by a*x.
           20 continue
           if( n==1_ilp ) then
              v( 1_ilp ) = x( 1_ilp )
              est = abs( v( 1_ilp ) )
              ! ... quit
              go to 130
           end if
           est = stdlib_scsum1( n, x, 1_ilp )
           do i = 1, n
              absxi = abs( x( i ) )
              if( absxi>safmin ) then
                 x( i ) = cmplx( real( x( i ),KIND=sp) / absxi,aimag( x( i ) ) / absxi,KIND=sp)
                           
              else
                 x( i ) = cone
              end if
           end do
           kase = 2_ilp
           jump = 2_ilp
           return
           ! ................ entry   (jump = 2)
           ! first iteration.  x has been overwritten by ctrans(a)*x.
           40 continue
           j = stdlib_icmax1( n, x, 1_ilp )
           iter = 2_ilp
           ! main loop - iterations 2,3,...,itmax.
           50 continue
           do i = 1, n
              x( i ) = czero
           end do
           x( j ) = cone
           kase = 1_ilp
           jump = 3_ilp
           return
           ! ................ entry   (jump = 3)
           ! x has been overwritten by a*x.
           70 continue
           call stdlib_ccopy( n, x, 1_ilp, v, 1_ilp )
           estold = est
           est = stdlib_scsum1( n, v, 1_ilp )
           ! test for cycling.
           if( est<=estold )go to 100
           do i = 1, n
              absxi = abs( x( i ) )
              if( absxi>safmin ) then
                 x( i ) = cmplx( real( x( i ),KIND=sp) / absxi,aimag( x( i ) ) / absxi,KIND=sp)
                           
              else
                 x( i ) = cone
              end if
           end do
           kase = 2_ilp
           jump = 4_ilp
           return
           ! ................ entry   (jump = 4)
           ! x has been overwritten by ctrans(a)*x.
           90 continue
           jlast = j
           j = stdlib_icmax1( n, x, 1_ilp )
           if( ( abs( x( jlast ) )/=abs( x( j ) ) ) .and.( iter<itmax ) ) then
              iter = iter + 1_ilp
              go to 50
           end if
           ! iteration complete.  final stage.
           100 continue
           altsgn = one
           do i = 1, n
              x( i ) = cmplx( altsgn*( one+real( i-1,KIND=sp) / real( n-1,KIND=sp) ),KIND=sp)
                        
              altsgn = -altsgn
           end do
           kase = 1_ilp
           jump = 5_ilp
           return
           ! ................ entry   (jump = 5)
           ! x has been overwritten by a*x.
           120 continue
           temp = two*( stdlib_scsum1( n, x, 1_ilp ) / real( 3_ilp*n,KIND=sp) )
           if( temp>est ) then
              call stdlib_ccopy( n, x, 1_ilp, v, 1_ilp )
              est = temp
           end if
           130 continue
           kase = 0_ilp
           return
     end subroutine stdlib_clacon

     module subroutine stdlib_zlacon( n, v, x, est, kase )
     !! ZLACON estimates the 1-norm of a square, complex matrix A.
     !! Reverse communication is used for evaluating matrix-vector products.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(inout) :: kase
           integer(ilp), intent(in) :: n
           real(dp), intent(inout) :: est
           ! Array Arguments 
           complex(dp), intent(out) :: v(n)
           complex(dp), intent(inout) :: x(n)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: itmax = 5_ilp
           
           
           
           ! Local Scalars 
           integer(ilp) :: i, iter, j, jlast, jump
           real(dp) :: absxi, altsgn, estold, safmin, temp
           ! Intrinsic Functions 
           ! Save Statement 
           save
           ! Executable Statements 
           safmin = stdlib_dlamch( 'SAFE MINIMUM' )
           if( kase==0_ilp ) then
              do i = 1, n
                 x( i ) = cmplx( one / real( n,KIND=dp),KIND=dp)
              end do
              kase = 1_ilp
              jump = 1_ilp
              return
           end if
           go to ( 20, 40, 70, 90, 120 )jump
           ! ................ entry   (jump = 1)
           ! first iteration.  x has been overwritten by a*x.
           20 continue
           if( n==1_ilp ) then
              v( 1_ilp ) = x( 1_ilp )
              est = abs( v( 1_ilp ) )
              ! ... quit
              go to 130
           end if
           est = stdlib_dzsum1( n, x, 1_ilp )
           do i = 1, n
              absxi = abs( x( i ) )
              if( absxi>safmin ) then
                 x( i ) = cmplx( real( x( i ),KIND=dp) / absxi,aimag( x( i ) ) / absxi,KIND=dp)
                           
              else
                 x( i ) = cone
              end if
           end do
           kase = 2_ilp
           jump = 2_ilp
           return
           ! ................ entry   (jump = 2)
           ! first iteration.  x has been overwritten by ctrans(a)*x.
           40 continue
           j = stdlib_izmax1( n, x, 1_ilp )
           iter = 2_ilp
           ! main loop - iterations 2,3,...,itmax.
           50 continue
           do i = 1, n
              x( i ) = czero
           end do
           x( j ) = cone
           kase = 1_ilp
           jump = 3_ilp
           return
           ! ................ entry   (jump = 3)
           ! x has been overwritten by a*x.
           70 continue
           call stdlib_zcopy( n, x, 1_ilp, v, 1_ilp )
           estold = est
           est = stdlib_dzsum1( n, v, 1_ilp )
           ! test for cycling.
           if( est<=estold )go to 100
           do i = 1, n
              absxi = abs( x( i ) )
              if( absxi>safmin ) then
                 x( i ) = cmplx( real( x( i ),KIND=dp) / absxi,aimag( x( i ) ) / absxi,KIND=dp)
                           
              else
                 x( i ) = cone
              end if
           end do
           kase = 2_ilp
           jump = 4_ilp
           return
           ! ................ entry   (jump = 4)
           ! x has been overwritten by ctrans(a)*x.
           90 continue
           jlast = j
           j = stdlib_izmax1( n, x, 1_ilp )
           if( ( abs( x( jlast ) )/=abs( x( j ) ) ) .and.( iter<itmax ) ) then
              iter = iter + 1_ilp
              go to 50
           end if
           ! iteration complete.  final stage.
           100 continue
           altsgn = one
           do i = 1, n
              x( i ) = cmplx( altsgn*( one+real( i-1,KIND=dp) / real( n-1,KIND=dp) ),KIND=dp)
                        
              altsgn = -altsgn
           end do
           kase = 1_ilp
           jump = 5_ilp
           return
           ! ................ entry   (jump = 5)
           ! x has been overwritten by a*x.
           120 continue
           temp = two*( stdlib_dzsum1( n, x, 1_ilp ) / real( 3_ilp*n,KIND=dp) )
           if( temp>est ) then
              call stdlib_zcopy( n, x, 1_ilp, v, 1_ilp )
              est = temp
           end if
           130 continue
           kase = 0_ilp
           return
     end subroutine stdlib_zlacon




     pure module subroutine stdlib_sla_lin_berr( n, nz, nrhs, res, ayb, berr )
     !! SLA_LIN_BERR computes componentwise relative backward error from
     !! the formula
     !! max(i) ( abs(R(i)) / ( abs(op(A_s))*abs(Y) + abs(B_s) )(i) )
     !! where abs(Z) is the componentwise absolute value of the matrix
     !! or vector Z.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: n, nz, nrhs
           ! Array Arguments 
           real(sp), intent(in) :: ayb(n,nrhs)
           real(sp), intent(out) :: berr(nrhs)
           real(sp), intent(in) :: res(n,nrhs)
        ! =====================================================================
           ! Local Scalars 
           real(sp) :: tmp,safe1
           integer(ilp) :: i, j
           ! Intrinsic Functions 
           ! Executable Statements 
           ! adding safe1 to the numerator guards against spuriously zero
           ! residuals.  a similar safeguard is in the sla_yyamv routine used
           ! to compute ayb.
           safe1 = stdlib_slamch( 'SAFE MINIMUM' )
           safe1 = (nz+1)*safe1
           do j = 1, nrhs
              berr(j) = zero
              do i = 1, n
                 if (ayb(i,j) /= 0.0_sp) then
                    tmp = (safe1+abs(res(i,j)))/ayb(i,j)
                    berr(j) = max( berr(j), tmp )
                 end if
           ! if ayb is exactly 0.0_sp (and if computed by sla_yyamv), then we know
           ! the true residual also must be exactly zero.
              end do
           end do
     end subroutine stdlib_sla_lin_berr

     pure module subroutine stdlib_dla_lin_berr ( n, nz, nrhs, res, ayb, berr )
     !! DLA_LIN_BERR computes component-wise relative backward error from
     !! the formula
     !! max(i) ( abs(R(i)) / ( abs(op(A_s))*abs(Y) + abs(B_s) )(i) )
     !! where abs(Z) is the component-wise absolute value of the matrix
     !! or vector Z.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: n, nz, nrhs
           ! Array Arguments 
           real(dp), intent(in) :: ayb(n,nrhs)
           real(dp), intent(out) :: berr(nrhs)
           real(dp), intent(in) :: res(n,nrhs)
        ! =====================================================================
           ! Local Scalars 
           real(dp) :: tmp,safe1
           integer(ilp) :: i, j
           ! Intrinsic Functions 
           ! Executable Statements 
           ! adding safe1 to the numerator guards against spuriously zero
           ! residuals.  a similar safeguard is in the sla_yyamv routine used
           ! to compute ayb.
           safe1 = stdlib_dlamch( 'SAFE MINIMUM' )
           safe1 = (nz+1)*safe1
           do j = 1, nrhs
              berr(j) = zero
              do i = 1, n
                 if (ayb(i,j) /= zero) then
                    tmp = (safe1+abs(res(i,j)))/ayb(i,j)
                    berr(j) = max( berr(j), tmp )
                 end if
           ! if ayb is exactly 0.0_dp (and if computed by sla_yyamv), then we know
           ! the true residual also must be exactly zero.
              end do
           end do
     end subroutine stdlib_dla_lin_berr


     pure module subroutine stdlib_cla_lin_berr( n, nz, nrhs, res, ayb, berr )
     !! CLA_LIN_BERR computes componentwise relative backward error from
     !! the formula
     !! max(i) ( abs(R(i)) / ( abs(op(A_s))*abs(Y) + abs(B_s) )(i) )
     !! where abs(Z) is the componentwise absolute value of the matrix
     !! or vector Z.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: n, nz, nrhs
           ! Array Arguments 
           real(sp), intent(in) :: ayb(n,nrhs)
           real(sp), intent(out) :: berr(nrhs)
           complex(sp), intent(in) :: res(n,nrhs)
        ! =====================================================================
           ! Local Scalars 
           real(sp) :: tmp,safe1
           integer(ilp) :: i, j
           complex(sp) :: cdum
           ! Intrinsic Functions 
           ! Statement Functions 
           complex(sp) :: cabs1
           ! Statement Function Definitions 
           cabs1( cdum ) = abs( real( cdum,KIND=sp) ) + abs( aimag( cdum ) )
           ! Executable Statements 
           ! adding safe1 to the numerator guards against spuriously zero
           ! residuals.  a similar safeguard is in the cla_yyamv routine used
           ! to compute ayb.
           safe1 = stdlib_slamch( 'SAFE MINIMUM' )
           safe1 = (nz+1)*safe1
           do j = 1, nrhs
              berr(j) = zero
              do i = 1, n
                 if (ayb(i,j) /= 0.0_sp) then
                    tmp = (safe1 + cabs1(res(i,j)))/ayb(i,j)
                    berr(j) = max( berr(j), tmp )
                 end if
           ! if ayb is exactly 0.0_sp (and if computed by cla_yyamv), then we know
           ! the true residual also must be exactly zero.
              end do
           end do
     end subroutine stdlib_cla_lin_berr

     pure module subroutine stdlib_zla_lin_berr( n, nz, nrhs, res, ayb, berr )
     !! ZLA_LIN_BERR computes componentwise relative backward error from
     !! the formula
     !! max(i) ( abs(R(i)) / ( abs(op(A_s))*abs(Y) + abs(B_s) )(i) )
     !! where abs(Z) is the componentwise absolute value of the matrix
     !! or vector Z.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: n, nz, nrhs
           ! Array Arguments 
           real(dp), intent(in) :: ayb(n,nrhs)
           real(dp), intent(out) :: berr(nrhs)
           complex(dp), intent(in) :: res(n,nrhs)
        ! =====================================================================
           ! Local Scalars 
           real(dp) :: tmp,safe1
           integer(ilp) :: i, j
           complex(dp) :: cdum
           ! Intrinsic Functions 
           ! Statement Functions 
           complex(dp) :: cabs1
           ! Statement Function Definitions 
           cabs1( cdum ) = abs( real( cdum,KIND=dp) ) + abs( aimag( cdum ) )
           ! Executable Statements 
           ! adding safe1 to the numerator guards against spuriously zero
           ! residuals.  a similar safeguard is in the cla_yyamv routine used
           ! to compute ayb.
           safe1 = stdlib_dlamch( 'SAFE MINIMUM' )
           safe1 = (nz+1)*safe1
           do j = 1, nrhs
              berr(j) = zero
              do i = 1, n
                 if (ayb(i,j) /= zero) then
                    tmp = (safe1 + cabs1(res(i,j)))/ayb(i,j)
                    berr(j) = max( berr(j), tmp )
                 end if
           ! if ayb is exactly 0.0_dp (and if computed by cla_yyamv), then we know
           ! the true residual also must be exactly zero.
              end do
           end do
     end subroutine stdlib_zla_lin_berr




     pure module subroutine stdlib_I64_slacn2( n, v, x, isgn, est, kase, isave )
     !! SLACN2 estimates the 1-norm of a square, real matrix A.
     !! Reverse communication is used for evaluating matrix-vector products.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(inout) :: kase
           integer(ilp64), intent(in) :: n
           real(sp), intent(inout) :: est
           ! Array Arguments 
           integer(ilp64), intent(out) :: isgn(*)
           integer(ilp64), intent(inout) :: isave(3_ilp64)
           real(sp), intent(out) :: v(*)
           real(sp), intent(inout) :: x(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp64), parameter :: itmax = 5_ilp64
           
           
           ! Local Scalars 
           integer(ilp64) :: i, jlast
           real(sp) :: altsgn, estold, temp, xs
           ! Intrinsic Functions 
           ! Executable Statements 
           if( kase==0_ilp64 ) then
              do i = 1, n
                 x( i ) = one / real( n,KIND=sp)
              end do
              kase = 1_ilp64
              isave( 1_ilp64 ) = 1_ilp64
              return
           end if
           go to ( 20, 40, 70, 110, 140 )isave( 1 )
           ! ................ entry   (isave( 1 ) = 1)
           ! first iteration.  x has been overwritten by a*x.
           20 continue
           if( n==1_ilp64 ) then
              v( 1_ilp64 ) = x( 1_ilp64 )
              est = abs( v( 1_ilp64 ) )
              ! ... quit
              go to 150
           end if
           est = stdlib_I64_sasum( n, x, 1_ilp64 )
           do i = 1, n
              if( x(i)>=zero ) then
                 x(i) = one
              else
                 x(i) = -one
              end if
              isgn( i ) = nint( x( i ),KIND=ilp64)
           end do
           kase = 2_ilp64
           isave( 1_ilp64 ) = 2_ilp64
           return
           ! ................ entry   (isave( 1 ) = 2)
           ! first iteration.  x has been overwritten by transpose(a)*x.
           40 continue
           isave( 2_ilp64 ) = stdlib_I64_isamax( n, x, 1_ilp64 )
           isave( 3_ilp64 ) = 2_ilp64
           ! main loop - iterations 2,3,...,itmax.
           50 continue
           do i = 1, n
              x( i ) = zero
           end do
           x( isave( 2_ilp64 ) ) = one
           kase = 1_ilp64
           isave( 1_ilp64 ) = 3_ilp64
           return
           ! ................ entry   (isave( 1 ) = 3)
           ! x has been overwritten by a*x.
           70 continue
           call stdlib_I64_scopy( n, x, 1_ilp64, v, 1_ilp64 )
           estold = est
           est = stdlib_I64_sasum( n, v, 1_ilp64 )
           do i = 1, n
              if( x(i)>=zero ) then
                 xs = one
              else
                 xs = -one
              end if
              if( nint( xs,KIND=ilp64)/=isgn( i ) )go to 90
           end do
           ! repeated sign vector detected, hence algorithm has converged.
           go to 120
           90 continue
           ! test for cycling.
           if( est<=estold )go to 120
           do i = 1, n
              if( x(i)>=zero ) then
                 x(i) = one
              else
                 x(i) = -one
              end if
              isgn( i ) = nint( x( i ),KIND=ilp64)
           end do
           kase = 2_ilp64
           isave( 1_ilp64 ) = 4_ilp64
           return
           ! ................ entry   (isave( 1 ) = 4)
           ! x has been overwritten by transpose(a)*x.
           110 continue
           jlast = isave( 2_ilp64 )
           isave( 2_ilp64 ) = stdlib_I64_isamax( n, x, 1_ilp64 )
           if( ( x( jlast )/=abs( x( isave( 2_ilp64 ) ) ) ) .and.( isave( 3_ilp64 )<itmax ) ) then
              isave( 3_ilp64 ) = isave( 3_ilp64 ) + 1_ilp64
              go to 50
           end if
           ! iteration complete.  final stage.
           120 continue
           altsgn = one
           do i = 1, n
              x( i ) = altsgn*( one+real( i-1,KIND=sp) / real( n-1,KIND=sp) )
              altsgn = -altsgn
           end do
           kase = 1_ilp64
           isave( 1_ilp64 ) = 5_ilp64
           return
           ! ................ entry   (isave( 1 ) = 5)
           ! x has been overwritten by a*x.
           140 continue
           temp = two*( stdlib_I64_sasum( n, x, 1_ilp64 ) / real( 3_ilp64*n,KIND=sp) )
           if( temp>est ) then
              call stdlib_I64_scopy( n, x, 1_ilp64, v, 1_ilp64 )
              est = temp
           end if
           150 continue
           kase = 0_ilp64
           return
     end subroutine stdlib_I64_slacn2

     pure module subroutine stdlib_I64_dlacn2( n, v, x, isgn, est, kase, isave )
     !! DLACN2 estimates the 1-norm of a square, real matrix A.
     !! Reverse communication is used for evaluating matrix-vector products.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(inout) :: kase
           integer(ilp64), intent(in) :: n
           real(dp), intent(inout) :: est
           ! Array Arguments 
           integer(ilp64), intent(out) :: isgn(*)
           integer(ilp64), intent(inout) :: isave(3_ilp64)
           real(dp), intent(out) :: v(*)
           real(dp), intent(inout) :: x(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp64), parameter :: itmax = 5_ilp64
           
           
           ! Local Scalars 
           integer(ilp64) :: i, jlast
           real(dp) :: altsgn, estold, temp, xs
           ! Intrinsic Functions 
           ! Executable Statements 
           if( kase==0_ilp64 ) then
              do i = 1, n
                 x( i ) = one / real( n,KIND=dp)
              end do
              kase = 1_ilp64
              isave( 1_ilp64 ) = 1_ilp64
              return
           end if
           go to ( 20, 40, 70, 110, 140 )isave( 1 )
           ! ................ entry   (isave( 1 ) = 1)
           ! first iteration.  x has been overwritten by a*x.
           20 continue
           if( n==1_ilp64 ) then
              v( 1_ilp64 ) = x( 1_ilp64 )
              est = abs( v( 1_ilp64 ) )
              ! ... quit
              go to 150
           end if
           est = stdlib_I64_dasum( n, x, 1_ilp64 )
           do i = 1, n
              if( x(i)>=zero ) then
                 x(i) = one
              else
                 x(i) = -one
              end if
              isgn( i ) = nint( x( i ),KIND=ilp64)
           end do
           kase = 2_ilp64
           isave( 1_ilp64 ) = 2_ilp64
           return
           ! ................ entry   (isave( 1 ) = 2)
           ! first iteration.  x has been overwritten by transpose(a)*x.
           40 continue
           isave( 2_ilp64 ) = stdlib_I64_idamax( n, x, 1_ilp64 )
           isave( 3_ilp64 ) = 2_ilp64
           ! main loop - iterations 2,3,...,itmax.
           50 continue
           do i = 1, n
              x( i ) = zero
           end do
           x( isave( 2_ilp64 ) ) = one
           kase = 1_ilp64
           isave( 1_ilp64 ) = 3_ilp64
           return
           ! ................ entry   (isave( 1 ) = 3)
           ! x has been overwritten by a*x.
           70 continue
           call stdlib_I64_dcopy( n, x, 1_ilp64, v, 1_ilp64 )
           estold = est
           est = stdlib_I64_dasum( n, v, 1_ilp64 )
           do i = 1, n
              if( x(i)>=zero ) then
                 xs = one
              else
                 xs = -one
              end if
              if( nint( xs,KIND=ilp64)/=isgn( i ) )go to 90
           end do
           ! repeated sign vector detected, hence algorithm has converged.
           go to 120
           90 continue
           ! test for cycling.
           if( est<=estold )go to 120
           do i = 1, n
              if( x(i)>=zero ) then
                 x(i) = one
              else
                 x(i) = -one
              end if
              isgn( i ) = nint( x( i ),KIND=ilp64)
           end do
           kase = 2_ilp64
           isave( 1_ilp64 ) = 4_ilp64
           return
           ! ................ entry   (isave( 1 ) = 4)
           ! x has been overwritten by transpose(a)*x.
           110 continue
           jlast = isave( 2_ilp64 )
           isave( 2_ilp64 ) = stdlib_I64_idamax( n, x, 1_ilp64 )
           if( ( x( jlast )/=abs( x( isave( 2_ilp64 ) ) ) ) .and.( isave( 3_ilp64 )<itmax ) ) then
              isave( 3_ilp64 ) = isave( 3_ilp64 ) + 1_ilp64
              go to 50
           end if
           ! iteration complete.  final stage.
           120 continue
           altsgn = one
           do i = 1, n
              x( i ) = altsgn*( one+real( i-1,KIND=dp) / real( n-1,KIND=dp) )
              altsgn = -altsgn
           end do
           kase = 1_ilp64
           isave( 1_ilp64 ) = 5_ilp64
           return
           ! ................ entry   (isave( 1 ) = 5)
           ! x has been overwritten by a*x.
           140 continue
           temp = two*( stdlib_I64_dasum( n, x, 1_ilp64 ) / real( 3_ilp64*n,KIND=dp) )
           if( temp>est ) then
              call stdlib_I64_dcopy( n, x, 1_ilp64, v, 1_ilp64 )
              est = temp
           end if
           150 continue
           kase = 0_ilp64
           return
     end subroutine stdlib_I64_dlacn2


     pure module subroutine stdlib_I64_clacn2( n, v, x, est, kase, isave )
     !! CLACN2 estimates the 1-norm of a square, complex matrix A.
     !! Reverse communication is used for evaluating matrix-vector products.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(inout) :: kase
           integer(ilp64), intent(in) :: n
           real(sp), intent(inout) :: est
           ! Array Arguments 
           integer(ilp64), intent(inout) :: isave(3_ilp64)
           complex(sp), intent(out) :: v(*)
           complex(sp), intent(inout) :: x(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp64), parameter :: itmax = 5_ilp64
           
           
           
           ! Local Scalars 
           integer(ilp64) :: i, jlast
           real(sp) :: absxi, altsgn, estold, safmin, temp
           ! Intrinsic Functions 
           ! Executable Statements 
           safmin = stdlib_I64_slamch( 'SAFE MINIMUM' )
           if( kase==0_ilp64 ) then
              do i = 1, n
                 x( i ) = cmplx( one / real( n,KIND=sp),KIND=sp)
              end do
              kase = 1_ilp64
              isave( 1_ilp64 ) = 1_ilp64
              return
           end if
           go to ( 20, 40, 70, 90, 120 )isave( 1 )
           ! ................ entry   (isave( 1 ) = 1)
           ! first iteration.  x has been overwritten by a*x.
           20 continue
           if( n==1_ilp64 ) then
              v( 1_ilp64 ) = x( 1_ilp64 )
              est = abs( v( 1_ilp64 ) )
              ! ... quit
              go to 130
           end if
           est = stdlib_I64_scsum1( n, x, 1_ilp64 )
           do i = 1, n
              absxi = abs( x( i ) )
              if( absxi>safmin ) then
                 x( i ) = cmplx( real( x( i ),KIND=sp) / absxi,aimag( x( i ) ) / absxi,KIND=sp)
                           
              else
                 x( i ) = cone
              end if
           end do
           kase = 2_ilp64
           isave( 1_ilp64 ) = 2_ilp64
           return
           ! ................ entry   (isave( 1 ) = 2)
           ! first iteration.  x has been overwritten by ctrans(a)*x.
           40 continue
           isave( 2_ilp64 ) = stdlib_I64_icmax1( n, x, 1_ilp64 )
           isave( 3_ilp64 ) = 2_ilp64
           ! main loop - iterations 2,3,...,itmax.
           50 continue
           do i = 1, n
              x( i ) = czero
           end do
           x( isave( 2_ilp64 ) ) = cone
           kase = 1_ilp64
           isave( 1_ilp64 ) = 3_ilp64
           return
           ! ................ entry   (isave( 1 ) = 3)
           ! x has been overwritten by a*x.
           70 continue
           call stdlib_I64_ccopy( n, x, 1_ilp64, v, 1_ilp64 )
           estold = est
           est = stdlib_I64_scsum1( n, v, 1_ilp64 )
           ! test for cycling.
           if( est<=estold )go to 100
           do i = 1, n
              absxi = abs( x( i ) )
              if( absxi>safmin ) then
                 x( i ) = cmplx( real( x( i ),KIND=sp) / absxi,aimag( x( i ) ) / absxi,KIND=sp)
                           
              else
                 x( i ) = cone
              end if
           end do
           kase = 2_ilp64
           isave( 1_ilp64 ) = 4_ilp64
           return
           ! ................ entry   (isave( 1 ) = 4)
           ! x has been overwritten by ctrans(a)*x.
           90 continue
           jlast = isave( 2_ilp64 )
           isave( 2_ilp64 ) = stdlib_I64_icmax1( n, x, 1_ilp64 )
           if( ( abs( x( jlast ) )/=abs( x( isave( 2_ilp64 ) ) ) ) .and.( isave( 3_ilp64 )<itmax ) ) &
                     then
              isave( 3_ilp64 ) = isave( 3_ilp64 ) + 1_ilp64
              go to 50
           end if
           ! iteration complete.  final stage.
           100 continue
           altsgn = one
           do i = 1, n
              x( i ) = cmplx( altsgn*( one + real( i-1,KIND=sp) / real( n-1,KIND=sp) ),KIND=sp)
                        
              altsgn = -altsgn
           end do
           kase = 1_ilp64
           isave( 1_ilp64 ) = 5_ilp64
           return
           ! ................ entry   (isave( 1 ) = 5)
           ! x has been overwritten by a*x.
           120 continue
           temp = two*( stdlib_I64_scsum1( n, x, 1_ilp64 ) / real( 3_ilp64*n,KIND=sp) )
           if( temp>est ) then
              call stdlib_I64_ccopy( n, x, 1_ilp64, v, 1_ilp64 )
              est = temp
           end if
           130 continue
           kase = 0_ilp64
           return
     end subroutine stdlib_I64_clacn2

     pure module subroutine stdlib_I64_zlacn2( n, v, x, est, kase, isave )
     !! ZLACN2 estimates the 1-norm of a square, complex matrix A.
     !! Reverse communication is used for evaluating matrix-vector products.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(inout) :: kase
           integer(ilp64), intent(in) :: n
           real(dp), intent(inout) :: est
           ! Array Arguments 
           integer(ilp64), intent(inout) :: isave(3_ilp64)
           complex(dp), intent(out) :: v(*)
           complex(dp), intent(inout) :: x(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp64), parameter :: itmax = 5_ilp64
           
           
           
           ! Local Scalars 
           integer(ilp64) :: i, jlast
           real(dp) :: absxi, altsgn, estold, safmin, temp
           ! Intrinsic Functions 
           ! Executable Statements 
           safmin = stdlib_I64_dlamch( 'SAFE MINIMUM' )
           if( kase==0_ilp64 ) then
              do i = 1, n
                 x( i ) = cmplx( one / real( n,KIND=dp),KIND=dp)
              end do
              kase = 1_ilp64
              isave( 1_ilp64 ) = 1_ilp64
              return
           end if
           go to ( 20, 40, 70, 90, 120 )isave( 1 )
           ! ................ entry   (isave( 1 ) = 1)
           ! first iteration.  x has been overwritten by a*x.
           20 continue
           if( n==1_ilp64 ) then
              v( 1_ilp64 ) = x( 1_ilp64 )
              est = abs( v( 1_ilp64 ) )
              ! ... quit
              go to 130
           end if
           est = stdlib_I64_dzsum1( n, x, 1_ilp64 )
           do i = 1, n
              absxi = abs( x( i ) )
              if( absxi>safmin ) then
                 x( i ) = cmplx( real( x( i ),KIND=dp) / absxi,aimag( x( i ) ) / absxi,KIND=dp)
                           
              else
                 x( i ) = cone
              end if
           end do
           kase = 2_ilp64
           isave( 1_ilp64 ) = 2_ilp64
           return
           ! ................ entry   (isave( 1 ) = 2)
           ! first iteration.  x has been overwritten by ctrans(a)*x.
           40 continue
           isave( 2_ilp64 ) = stdlib_I64_izmax1( n, x, 1_ilp64 )
           isave( 3_ilp64 ) = 2_ilp64
           ! main loop - iterations 2,3,...,itmax.
           50 continue
           do i = 1, n
              x( i ) = czero
           end do
           x( isave( 2_ilp64 ) ) = cone
           kase = 1_ilp64
           isave( 1_ilp64 ) = 3_ilp64
           return
           ! ................ entry   (isave( 1 ) = 3)
           ! x has been overwritten by a*x.
           70 continue
           call stdlib_I64_zcopy( n, x, 1_ilp64, v, 1_ilp64 )
           estold = est
           est = stdlib_I64_dzsum1( n, v, 1_ilp64 )
           ! test for cycling.
           if( est<=estold )go to 100
           do i = 1, n
              absxi = abs( x( i ) )
              if( absxi>safmin ) then
                 x( i ) = cmplx( real( x( i ),KIND=dp) / absxi,aimag( x( i ) ) / absxi,KIND=dp)
                           
              else
                 x( i ) = cone
              end if
           end do
           kase = 2_ilp64
           isave( 1_ilp64 ) = 4_ilp64
           return
           ! ................ entry   (isave( 1 ) = 4)
           ! x has been overwritten by ctrans(a)*x.
           90 continue
           jlast = isave( 2_ilp64 )
           isave( 2_ilp64 ) = stdlib_I64_izmax1( n, x, 1_ilp64 )
           if( ( abs( x( jlast ) )/=abs( x( isave( 2_ilp64 ) ) ) ) .and.( isave( 3_ilp64 )<itmax ) ) &
                     then
              isave( 3_ilp64 ) = isave( 3_ilp64 ) + 1_ilp64
              go to 50
           end if
           ! iteration complete.  final stage.
           100 continue
           altsgn = one
           do i = 1, n
              x( i ) = cmplx( altsgn*( one+real( i-1,KIND=dp) / real( n-1,KIND=dp) ),KIND=dp)
                        
              altsgn = -altsgn
           end do
           kase = 1_ilp64
           isave( 1_ilp64 ) = 5_ilp64
           return
           ! ................ entry   (isave( 1 ) = 5)
           ! x has been overwritten by a*x.
           120 continue
           temp = two*( stdlib_I64_dzsum1( n, x, 1_ilp64 ) / real( 3_ilp64*n,KIND=dp) )
           if( temp>est ) then
              call stdlib_I64_zcopy( n, x, 1_ilp64, v, 1_ilp64 )
              est = temp
           end if
           130 continue
           kase = 0_ilp64
           return
     end subroutine stdlib_I64_zlacn2




     module subroutine stdlib_I64_slacon( n, v, x, isgn, est, kase )
     !! SLACON estimates the 1-norm of a square, real matrix A.
     !! Reverse communication is used for evaluating matrix-vector products.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(inout) :: kase
           integer(ilp64), intent(in) :: n
           real(sp), intent(inout) :: est
           ! Array Arguments 
           integer(ilp64), intent(out) :: isgn(*)
           real(sp), intent(out) :: v(*)
           real(sp), intent(inout) :: x(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp64), parameter :: itmax = 5_ilp64
           
           
           ! Local Scalars 
           integer(ilp64) :: i, iter, j, jlast, jump
           real(sp) :: altsgn, estold, temp
           ! Intrinsic Functions 
           ! Save Statement 
           save
           ! Executable Statements 
           if( kase==0_ilp64 ) then
              do i = 1, n
                 x( i ) = one / real( n,KIND=sp)
              end do
              kase = 1_ilp64
              jump = 1_ilp64
              return
           end if
           go to ( 20, 40, 70, 110, 140 )jump
           ! ................ entry   (jump = 1)
           ! first iteration.  x has been overwritten by a*x.
           20 continue
           if( n==1_ilp64 ) then
              v( 1_ilp64 ) = x( 1_ilp64 )
              est = abs( v( 1_ilp64 ) )
              ! ... quit
              go to 150
           end if
           est = stdlib_I64_sasum( n, x, 1_ilp64 )
           do i = 1, n
              x( i ) = sign( one, x( i ) )
              isgn( i ) = nint( x( i ),KIND=ilp64)
           end do
           kase = 2_ilp64
           jump = 2_ilp64
           return
           ! ................ entry   (jump = 2)
           ! first iteration.  x has been overwritten by transpose(a)*x.
           40 continue
           j = stdlib_I64_isamax( n, x, 1_ilp64 )
           iter = 2_ilp64
           ! main loop - iterations 2,3,...,itmax.
           50 continue
           do i = 1, n
              x( i ) = zero
           end do
           x( j ) = one
           kase = 1_ilp64
           jump = 3_ilp64
           return
           ! ................ entry   (jump = 3)
           ! x has been overwritten by a*x.
           70 continue
           call stdlib_I64_scopy( n, x, 1_ilp64, v, 1_ilp64 )
           estold = est
           est = stdlib_I64_sasum( n, v, 1_ilp64 )
           do i = 1, n
              if( nint( sign( one, x( i ) ),KIND=ilp64)/=isgn( i ) )go to 90
           end do
           ! repeated sign vector detected, hence algorithm has converged.
           go to 120
           90 continue
           ! test for cycling.
           if( est<=estold )go to 120
           do i = 1, n
              x( i ) = sign( one, x( i ) )
              isgn( i ) = nint( x( i ),KIND=ilp64)
           end do
           kase = 2_ilp64
           jump = 4_ilp64
           return
           ! ................ entry   (jump = 4)
           ! x has been overwritten by transpose(a)*x.
           110 continue
           jlast = j
           j = stdlib_I64_isamax( n, x, 1_ilp64 )
           if( ( x( jlast )/=abs( x( j ) ) ) .and. ( iter<itmax ) ) then
              iter = iter + 1_ilp64
              go to 50
           end if
           ! iteration complete.  final stage.
           120 continue
           altsgn = one
           do i = 1, n
              x( i ) = altsgn*( one+real( i-1,KIND=sp) / real( n-1,KIND=sp) )
              altsgn = -altsgn
           end do
           kase = 1_ilp64
           jump = 5_ilp64
           return
           ! ................ entry   (jump = 5)
           ! x has been overwritten by a*x.
           140 continue
           temp = two*( stdlib_I64_sasum( n, x, 1_ilp64 ) / real( 3_ilp64*n,KIND=sp) )
           if( temp>est ) then
              call stdlib_I64_scopy( n, x, 1_ilp64, v, 1_ilp64 )
              est = temp
           end if
           150 continue
           kase = 0_ilp64
           return
     end subroutine stdlib_I64_slacon

     module subroutine stdlib_I64_dlacon( n, v, x, isgn, est, kase )
     !! DLACON estimates the 1-norm of a square, real matrix A.
     !! Reverse communication is used for evaluating matrix-vector products.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(inout) :: kase
           integer(ilp64), intent(in) :: n
           real(dp), intent(inout) :: est
           ! Array Arguments 
           integer(ilp64), intent(out) :: isgn(*)
           real(dp), intent(out) :: v(*)
           real(dp), intent(inout) :: x(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp64), parameter :: itmax = 5_ilp64
           
           
           ! Local Scalars 
           integer(ilp64) :: i, iter, j, jlast, jump
           real(dp) :: altsgn, estold, temp
           ! Intrinsic Functions 
           ! Save Statement 
           save
           ! Executable Statements 
           if( kase==0_ilp64 ) then
              do i = 1, n
                 x( i ) = one / real( n,KIND=dp)
              end do
              kase = 1_ilp64
              jump = 1_ilp64
              return
           end if
           go to ( 20, 40, 70, 110, 140 )jump
           ! ................ entry   (jump = 1)
           ! first iteration.  x has been overwritten by a*x.
           20 continue
           if( n==1_ilp64 ) then
              v( 1_ilp64 ) = x( 1_ilp64 )
              est = abs( v( 1_ilp64 ) )
              ! ... quit
              go to 150
           end if
           est = stdlib_I64_dasum( n, x, 1_ilp64 )
           do i = 1, n
              x( i ) = sign( one, x( i ) )
              isgn( i ) = nint( x( i ),KIND=ilp64)
           end do
           kase = 2_ilp64
           jump = 2_ilp64
           return
           ! ................ entry   (jump = 2)
           ! first iteration.  x has been overwritten by transpose(a)*x.
           40 continue
           j = stdlib_I64_idamax( n, x, 1_ilp64 )
           iter = 2_ilp64
           ! main loop - iterations 2,3,...,itmax.
           50 continue
           do i = 1, n
              x( i ) = zero
           end do
           x( j ) = one
           kase = 1_ilp64
           jump = 3_ilp64
           return
           ! ................ entry   (jump = 3)
           ! x has been overwritten by a*x.
           70 continue
           call stdlib_I64_dcopy( n, x, 1_ilp64, v, 1_ilp64 )
           estold = est
           est = stdlib_I64_dasum( n, v, 1_ilp64 )
           do i = 1, n
              if( nint( sign( one, x( i ) ),KIND=ilp64)/=isgn( i ) )go to 90
           end do
           ! repeated sign vector detected, hence algorithm has converged.
           go to 120
           90 continue
           ! test for cycling.
           if( est<=estold )go to 120
           do i = 1, n
              x( i ) = sign( one, x( i ) )
              isgn( i ) = nint( x( i ),KIND=ilp64)
           end do
           kase = 2_ilp64
           jump = 4_ilp64
           return
           ! ................ entry   (jump = 4)
           ! x has been overwritten by transpose(a)*x.
           110 continue
           jlast = j
           j = stdlib_I64_idamax( n, x, 1_ilp64 )
           if( ( x( jlast )/=abs( x( j ) ) ) .and. ( iter<itmax ) ) then
              iter = iter + 1_ilp64
              go to 50
           end if
           ! iteration complete.  final stage.
           120 continue
           altsgn = one
           do i = 1, n
              x( i ) = altsgn*( one+real( i-1,KIND=dp) / real( n-1,KIND=dp) )
              altsgn = -altsgn
           end do
           kase = 1_ilp64
           jump = 5_ilp64
           return
           ! ................ entry   (jump = 5)
           ! x has been overwritten by a*x.
           140 continue
           temp = two*( stdlib_I64_dasum( n, x, 1_ilp64 ) / real( 3_ilp64*n,KIND=dp) )
           if( temp>est ) then
              call stdlib_I64_dcopy( n, x, 1_ilp64, v, 1_ilp64 )
              est = temp
           end if
           150 continue
           kase = 0_ilp64
           return
     end subroutine stdlib_I64_dlacon


     module subroutine stdlib_I64_clacon( n, v, x, est, kase )
     !! CLACON estimates the 1-norm of a square, complex matrix A.
     !! Reverse communication is used for evaluating matrix-vector products.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(inout) :: kase
           integer(ilp64), intent(in) :: n
           real(sp), intent(inout) :: est
           ! Array Arguments 
           complex(sp), intent(out) :: v(n)
           complex(sp), intent(inout) :: x(n)
        ! =====================================================================
           ! Parameters 
           integer(ilp64), parameter :: itmax = 5_ilp64
           
           
           
           ! Local Scalars 
           integer(ilp64) :: i, iter, j, jlast, jump
           real(sp) :: absxi, altsgn, estold, safmin, temp
           ! Intrinsic Functions 
           ! Save Statement 
           save
           ! Executable Statements 
           safmin = stdlib_I64_slamch( 'SAFE MINIMUM' )
           if( kase==0_ilp64 ) then
              do i = 1, n
                 x( i ) = cmplx( one / real( n,KIND=sp),KIND=sp)
              end do
              kase = 1_ilp64
              jump = 1_ilp64
              return
           end if
           go to ( 20, 40, 70, 90, 120 )jump
           ! ................ entry   (jump = 1)
           ! first iteration.  x has been overwritten by a*x.
           20 continue
           if( n==1_ilp64 ) then
              v( 1_ilp64 ) = x( 1_ilp64 )
              est = abs( v( 1_ilp64 ) )
              ! ... quit
              go to 130
           end if
           est = stdlib_I64_scsum1( n, x, 1_ilp64 )
           do i = 1, n
              absxi = abs( x( i ) )
              if( absxi>safmin ) then
                 x( i ) = cmplx( real( x( i ),KIND=sp) / absxi,aimag( x( i ) ) / absxi,KIND=sp)
                           
              else
                 x( i ) = cone
              end if
           end do
           kase = 2_ilp64
           jump = 2_ilp64
           return
           ! ................ entry   (jump = 2)
           ! first iteration.  x has been overwritten by ctrans(a)*x.
           40 continue
           j = stdlib_I64_icmax1( n, x, 1_ilp64 )
           iter = 2_ilp64
           ! main loop - iterations 2,3,...,itmax.
           50 continue
           do i = 1, n
              x( i ) = czero
           end do
           x( j ) = cone
           kase = 1_ilp64
           jump = 3_ilp64
           return
           ! ................ entry   (jump = 3)
           ! x has been overwritten by a*x.
           70 continue
           call stdlib_I64_ccopy( n, x, 1_ilp64, v, 1_ilp64 )
           estold = est
           est = stdlib_I64_scsum1( n, v, 1_ilp64 )
           ! test for cycling.
           if( est<=estold )go to 100
           do i = 1, n
              absxi = abs( x( i ) )
              if( absxi>safmin ) then
                 x( i ) = cmplx( real( x( i ),KIND=sp) / absxi,aimag( x( i ) ) / absxi,KIND=sp)
                           
              else
                 x( i ) = cone
              end if
           end do
           kase = 2_ilp64
           jump = 4_ilp64
           return
           ! ................ entry   (jump = 4)
           ! x has been overwritten by ctrans(a)*x.
           90 continue
           jlast = j
           j = stdlib_I64_icmax1( n, x, 1_ilp64 )
           if( ( abs( x( jlast ) )/=abs( x( j ) ) ) .and.( iter<itmax ) ) then
              iter = iter + 1_ilp64
              go to 50
           end if
           ! iteration complete.  final stage.
           100 continue
           altsgn = one
           do i = 1, n
              x( i ) = cmplx( altsgn*( one+real( i-1,KIND=sp) / real( n-1,KIND=sp) ),KIND=sp)
                        
              altsgn = -altsgn
           end do
           kase = 1_ilp64
           jump = 5_ilp64
           return
           ! ................ entry   (jump = 5)
           ! x has been overwritten by a*x.
           120 continue
           temp = two*( stdlib_I64_scsum1( n, x, 1_ilp64 ) / real( 3_ilp64*n,KIND=sp) )
           if( temp>est ) then
              call stdlib_I64_ccopy( n, x, 1_ilp64, v, 1_ilp64 )
              est = temp
           end if
           130 continue
           kase = 0_ilp64
           return
     end subroutine stdlib_I64_clacon

     module subroutine stdlib_I64_zlacon( n, v, x, est, kase )
     !! ZLACON estimates the 1-norm of a square, complex matrix A.
     !! Reverse communication is used for evaluating matrix-vector products.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(inout) :: kase
           integer(ilp64), intent(in) :: n
           real(dp), intent(inout) :: est
           ! Array Arguments 
           complex(dp), intent(out) :: v(n)
           complex(dp), intent(inout) :: x(n)
        ! =====================================================================
           ! Parameters 
           integer(ilp64), parameter :: itmax = 5_ilp64
           
           
           
           ! Local Scalars 
           integer(ilp64) :: i, iter, j, jlast, jump
           real(dp) :: absxi, altsgn, estold, safmin, temp
           ! Intrinsic Functions 
           ! Save Statement 
           save
           ! Executable Statements 
           safmin = stdlib_I64_dlamch( 'SAFE MINIMUM' )
           if( kase==0_ilp64 ) then
              do i = 1, n
                 x( i ) = cmplx( one / real( n,KIND=dp),KIND=dp)
              end do
              kase = 1_ilp64
              jump = 1_ilp64
              return
           end if
           go to ( 20, 40, 70, 90, 120 )jump
           ! ................ entry   (jump = 1)
           ! first iteration.  x has been overwritten by a*x.
           20 continue
           if( n==1_ilp64 ) then
              v( 1_ilp64 ) = x( 1_ilp64 )
              est = abs( v( 1_ilp64 ) )
              ! ... quit
              go to 130
           end if
           est = stdlib_I64_dzsum1( n, x, 1_ilp64 )
           do i = 1, n
              absxi = abs( x( i ) )
              if( absxi>safmin ) then
                 x( i ) = cmplx( real( x( i ),KIND=dp) / absxi,aimag( x( i ) ) / absxi,KIND=dp)
                           
              else
                 x( i ) = cone
              end if
           end do
           kase = 2_ilp64
           jump = 2_ilp64
           return
           ! ................ entry   (jump = 2)
           ! first iteration.  x has been overwritten by ctrans(a)*x.
           40 continue
           j = stdlib_I64_izmax1( n, x, 1_ilp64 )
           iter = 2_ilp64
           ! main loop - iterations 2,3,...,itmax.
           50 continue
           do i = 1, n
              x( i ) = czero
           end do
           x( j ) = cone
           kase = 1_ilp64
           jump = 3_ilp64
           return
           ! ................ entry   (jump = 3)
           ! x has been overwritten by a*x.
           70 continue
           call stdlib_I64_zcopy( n, x, 1_ilp64, v, 1_ilp64 )
           estold = est
           est = stdlib_I64_dzsum1( n, v, 1_ilp64 )
           ! test for cycling.
           if( est<=estold )go to 100
           do i = 1, n
              absxi = abs( x( i ) )
              if( absxi>safmin ) then
                 x( i ) = cmplx( real( x( i ),KIND=dp) / absxi,aimag( x( i ) ) / absxi,KIND=dp)
                           
              else
                 x( i ) = cone
              end if
           end do
           kase = 2_ilp64
           jump = 4_ilp64
           return
           ! ................ entry   (jump = 4)
           ! x has been overwritten by ctrans(a)*x.
           90 continue
           jlast = j
           j = stdlib_I64_izmax1( n, x, 1_ilp64 )
           if( ( abs( x( jlast ) )/=abs( x( j ) ) ) .and.( iter<itmax ) ) then
              iter = iter + 1_ilp64
              go to 50
           end if
           ! iteration complete.  final stage.
           100 continue
           altsgn = one
           do i = 1, n
              x( i ) = cmplx( altsgn*( one+real( i-1,KIND=dp) / real( n-1,KIND=dp) ),KIND=dp)
                        
              altsgn = -altsgn
           end do
           kase = 1_ilp64
           jump = 5_ilp64
           return
           ! ................ entry   (jump = 5)
           ! x has been overwritten by a*x.
           120 continue
           temp = two*( stdlib_I64_dzsum1( n, x, 1_ilp64 ) / real( 3_ilp64*n,KIND=dp) )
           if( temp>est ) then
              call stdlib_I64_zcopy( n, x, 1_ilp64, v, 1_ilp64 )
              est = temp
           end if
           130 continue
           kase = 0_ilp64
           return
     end subroutine stdlib_I64_zlacon




     pure module subroutine stdlib_I64_sla_lin_berr( n, nz, nrhs, res, ayb, berr )
     !! SLA_LIN_BERR computes componentwise relative backward error from
     !! the formula
     !! max(i) ( abs(R(i)) / ( abs(op(A_s))*abs(Y) + abs(B_s) )(i) )
     !! where abs(Z) is the componentwise absolute value of the matrix
     !! or vector Z.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: n, nz, nrhs
           ! Array Arguments 
           real(sp), intent(in) :: ayb(n,nrhs)
           real(sp), intent(out) :: berr(nrhs)
           real(sp), intent(in) :: res(n,nrhs)
        ! =====================================================================
           ! Local Scalars 
           real(sp) :: tmp,safe1
           integer(ilp64) :: i, j
           ! Intrinsic Functions 
           ! Executable Statements 
           ! adding safe1 to the numerator guards against spuriously zero
           ! residuals.  a similar safeguard is in the sla_yyamv routine used
           ! to compute ayb.
           safe1 = stdlib_I64_slamch( 'SAFE MINIMUM' )
           safe1 = (nz+1)*safe1
           do j = 1, nrhs
              berr(j) = zero
              do i = 1, n
                 if (ayb(i,j) /= 0.0_sp) then
                    tmp = (safe1+abs(res(i,j)))/ayb(i,j)
                    berr(j) = max( berr(j), tmp )
                 end if
           ! if ayb is exactly 0.0_sp (and if computed by sla_yyamv), then we know
           ! the true residual also must be exactly zero.
              end do
           end do
     end subroutine stdlib_I64_sla_lin_berr

     pure module subroutine stdlib_I64_dla_lin_berr ( n, nz, nrhs, res, ayb, berr )
     !! DLA_LIN_BERR computes component-wise relative backward error from
     !! the formula
     !! max(i) ( abs(R(i)) / ( abs(op(A_s))*abs(Y) + abs(B_s) )(i) )
     !! where abs(Z) is the component-wise absolute value of the matrix
     !! or vector Z.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: n, nz, nrhs
           ! Array Arguments 
           real(dp), intent(in) :: ayb(n,nrhs)
           real(dp), intent(out) :: berr(nrhs)
           real(dp), intent(in) :: res(n,nrhs)
        ! =====================================================================
           ! Local Scalars 
           real(dp) :: tmp,safe1
           integer(ilp64) :: i, j
           ! Intrinsic Functions 
           ! Executable Statements 
           ! adding safe1 to the numerator guards against spuriously zero
           ! residuals.  a similar safeguard is in the sla_yyamv routine used
           ! to compute ayb.
           safe1 = stdlib_I64_dlamch( 'SAFE MINIMUM' )
           safe1 = (nz+1)*safe1
           do j = 1, nrhs
              berr(j) = zero
              do i = 1, n
                 if (ayb(i,j) /= zero) then
                    tmp = (safe1+abs(res(i,j)))/ayb(i,j)
                    berr(j) = max( berr(j), tmp )
                 end if
           ! if ayb is exactly 0.0_dp (and if computed by sla_yyamv), then we know
           ! the true residual also must be exactly zero.
              end do
           end do
     end subroutine stdlib_I64_dla_lin_berr


     pure module subroutine stdlib_I64_cla_lin_berr( n, nz, nrhs, res, ayb, berr )
     !! CLA_LIN_BERR computes componentwise relative backward error from
     !! the formula
     !! max(i) ( abs(R(i)) / ( abs(op(A_s))*abs(Y) + abs(B_s) )(i) )
     !! where abs(Z) is the componentwise absolute value of the matrix
     !! or vector Z.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: n, nz, nrhs
           ! Array Arguments 
           real(sp), intent(in) :: ayb(n,nrhs)
           real(sp), intent(out) :: berr(nrhs)
           complex(sp), intent(in) :: res(n,nrhs)
        ! =====================================================================
           ! Local Scalars 
           real(sp) :: tmp,safe1
           integer(ilp64) :: i, j
           complex(sp) :: cdum
           ! Intrinsic Functions 
           ! Statement Functions 
           complex(sp) :: cabs1
           ! Statement Function Definitions 
           cabs1( cdum ) = abs( real( cdum,KIND=sp) ) + abs( aimag( cdum ) )
           ! Executable Statements 
           ! adding safe1 to the numerator guards against spuriously zero
           ! residuals.  a similar safeguard is in the cla_yyamv routine used
           ! to compute ayb.
           safe1 = stdlib_I64_slamch( 'SAFE MINIMUM' )
           safe1 = (nz+1)*safe1
           do j = 1, nrhs
              berr(j) = zero
              do i = 1, n
                 if (ayb(i,j) /= 0.0_sp) then
                    tmp = (safe1 + cabs1(res(i,j)))/ayb(i,j)
                    berr(j) = max( berr(j), tmp )
                 end if
           ! if ayb is exactly 0.0_sp (and if computed by cla_yyamv), then we know
           ! the true residual also must be exactly zero.
              end do
           end do
     end subroutine stdlib_I64_cla_lin_berr

     pure module subroutine stdlib_I64_zla_lin_berr( n, nz, nrhs, res, ayb, berr )
     !! ZLA_LIN_BERR computes componentwise relative backward error from
     !! the formula
     !! max(i) ( abs(R(i)) / ( abs(op(A_s))*abs(Y) + abs(B_s) )(i) )
     !! where abs(Z) is the componentwise absolute value of the matrix
     !! or vector Z.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: n, nz, nrhs
           ! Array Arguments 
           real(dp), intent(in) :: ayb(n,nrhs)
           real(dp), intent(out) :: berr(nrhs)
           complex(dp), intent(in) :: res(n,nrhs)
        ! =====================================================================
           ! Local Scalars 
           real(dp) :: tmp,safe1
           integer(ilp64) :: i, j
           complex(dp) :: cdum
           ! Intrinsic Functions 
           ! Statement Functions 
           complex(dp) :: cabs1
           ! Statement Function Definitions 
           cabs1( cdum ) = abs( real( cdum,KIND=dp) ) + abs( aimag( cdum ) )
           ! Executable Statements 
           ! adding safe1 to the numerator guards against spuriously zero
           ! residuals.  a similar safeguard is in the cla_yyamv routine used
           ! to compute ayb.
           safe1 = stdlib_I64_dlamch( 'SAFE MINIMUM' )
           safe1 = (nz+1)*safe1
           do j = 1, nrhs
              berr(j) = zero
              do i = 1, n
                 if (ayb(i,j) /= zero) then
                    tmp = (safe1 + cabs1(res(i,j)))/ayb(i,j)
                    berr(j) = max( berr(j), tmp )
                 end if
           ! if ayb is exactly 0.0_dp (and if computed by cla_yyamv), then we know
           ! the true residual also must be exactly zero.
              end do
           end do
     end subroutine stdlib_I64_zla_lin_berr



end submodule stdlib_lapack_solve_aux
