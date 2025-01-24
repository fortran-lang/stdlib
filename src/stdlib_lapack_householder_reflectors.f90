submodule(stdlib_lapack_base) stdlib_lapack_householder_reflectors
  implicit none


  contains

     pure module subroutine stdlib_slarf( side, m, n, v, incv, tau, c, ldc, work )
     !! SLARF applies a real elementary reflector H to a real m by n matrix
     !! C, from either the left or the right. H is represented in the form
     !! H = I - tau * v * v**T
     !! where tau is a real scalar and v is a real vector.
     !! If tau = 0, then H is taken to be the unit matrix.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side
           integer(ilp), intent(in) :: incv, ldc, m, n
           real(sp), intent(in) :: tau
           ! Array Arguments 
           real(sp), intent(inout) :: c(ldc,*)
           real(sp), intent(in) :: v(*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: applyleft
           integer(ilp) :: i, lastv, lastc
           ! Executable Statements 
           applyleft = stdlib_lsame( side, 'L' )
           lastv = 0_ilp
           lastc = 0_ilp
           if( tau/=zero ) then
           ! set up variables for scanning v.  lastv begins pointing to the end
           ! of v.
              if( applyleft ) then
                 lastv = m
              else
                 lastv = n
              end if
              if( incv>0_ilp ) then
                 i = 1_ilp + (lastv-1) * incv
              else
                 i = 1_ilp
              end if
           ! look for the last non-zero row in v.
              do while( lastv>0 .and. v( i )==zero )
                 lastv = lastv - 1_ilp
                 i = i - incv
              end do
              if( applyleft ) then
           ! scan for the last non-zero column in c(1:lastv,:).
                 lastc = stdlib_ilaslc(lastv, n, c, ldc)
              else
           ! scan for the last non-zero row in c(:,1:lastv).
                 lastc = stdlib_ilaslr(m, lastv, c, ldc)
              end if
           end if
           ! note that lastc.eq.0_sp renders the blas operations null; no special
           ! case is needed at this level.
           if( applyleft ) then
              ! form  h * c
              if( lastv>0_ilp ) then
                 ! w(1:lastc,1) := c(1:lastv,1:lastc)**t * v(1:lastv,1)
                 call stdlib_sgemv( 'TRANSPOSE', lastv, lastc, one, c, ldc, v, incv,zero, work, 1_ilp &
                           )
                 ! c(1:lastv,1:lastc) := c(...) - v(1:lastv,1) * w(1:lastc,1)**t
                 call stdlib_sger( lastv, lastc, -tau, v, incv, work, 1_ilp, c, ldc )
              end if
           else
              ! form  c * h
              if( lastv>0_ilp ) then
                 ! w(1:lastc,1) := c(1:lastc,1:lastv) * v(1:lastv,1)
                 call stdlib_sgemv( 'NO TRANSPOSE', lastc, lastv, one, c, ldc,v, incv, zero, work,&
                            1_ilp )
                 ! c(1:lastc,1:lastv) := c(...) - w(1:lastc,1) * v(1:lastv,1)**t
                 call stdlib_sger( lastc, lastv, -tau, work, 1_ilp, v, incv, c, ldc )
              end if
           end if
           return
     end subroutine stdlib_slarf

     pure module subroutine stdlib_dlarf( side, m, n, v, incv, tau, c, ldc, work )
     !! DLARF applies a real elementary reflector H to a real m by n matrix
     !! C, from either the left or the right. H is represented in the form
     !! H = I - tau * v * v**T
     !! where tau is a real scalar and v is a real vector.
     !! If tau = 0, then H is taken to be the unit matrix.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side
           integer(ilp), intent(in) :: incv, ldc, m, n
           real(dp), intent(in) :: tau
           ! Array Arguments 
           real(dp), intent(inout) :: c(ldc,*)
           real(dp), intent(in) :: v(*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: applyleft
           integer(ilp) :: i, lastv, lastc
           ! Executable Statements 
           applyleft = stdlib_lsame( side, 'L' )
           lastv = 0_ilp
           lastc = 0_ilp
           if( tau/=zero ) then
           ! set up variables for scanning v.  lastv begins pointing to the end
           ! of v.
              if( applyleft ) then
                 lastv = m
              else
                 lastv = n
              end if
              if( incv>0_ilp ) then
                 i = 1_ilp + (lastv-1) * incv
              else
                 i = 1_ilp
              end if
           ! look for the last non-zero row in v.
              do while( lastv>0 .and. v( i )==zero )
                 lastv = lastv - 1_ilp
                 i = i - incv
              end do
              if( applyleft ) then
           ! scan for the last non-zero column in c(1:lastv,:).
                 lastc = stdlib_iladlc(lastv, n, c, ldc)
              else
           ! scan for the last non-zero row in c(:,1:lastv).
                 lastc = stdlib_iladlr(m, lastv, c, ldc)
              end if
           end if
           ! note that lastc.eq.0_dp renders the blas operations null; no special
           ! case is needed at this level.
           if( applyleft ) then
              ! form  h * c
              if( lastv>0_ilp ) then
                 ! w(1:lastc,1) := c(1:lastv,1:lastc)**t * v(1:lastv,1)
                 call stdlib_dgemv( 'TRANSPOSE', lastv, lastc, one, c, ldc, v, incv,zero, work, 1_ilp &
                           )
                 ! c(1:lastv,1:lastc) := c(...) - v(1:lastv,1) * w(1:lastc,1)**t
                 call stdlib_dger( lastv, lastc, -tau, v, incv, work, 1_ilp, c, ldc )
              end if
           else
              ! form  c * h
              if( lastv>0_ilp ) then
                 ! w(1:lastc,1) := c(1:lastc,1:lastv) * v(1:lastv,1)
                 call stdlib_dgemv( 'NO TRANSPOSE', lastc, lastv, one, c, ldc,v, incv, zero, work,&
                            1_ilp )
                 ! c(1:lastc,1:lastv) := c(...) - w(1:lastc,1) * v(1:lastv,1)**t
                 call stdlib_dger( lastc, lastv, -tau, work, 1_ilp, v, incv, c, ldc )
              end if
           end if
           return
     end subroutine stdlib_dlarf


     pure module subroutine stdlib_clarf( side, m, n, v, incv, tau, c, ldc, work )
     !! CLARF applies a complex elementary reflector H to a complex M-by-N
     !! matrix C, from either the left or the right. H is represented in the
     !! form
     !! H = I - tau * v * v**H
     !! where tau is a complex scalar and v is a complex vector.
     !! If tau = 0, then H is taken to be the unit matrix.
     !! To apply H**H (the conjugate transpose of H), supply conjg(tau) instead
     !! tau.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side
           integer(ilp), intent(in) :: incv, ldc, m, n
           complex(sp), intent(in) :: tau
           ! Array Arguments 
           complex(sp), intent(inout) :: c(ldc,*)
           complex(sp), intent(in) :: v(*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: applyleft
           integer(ilp) :: i, lastv, lastc
           ! Executable Statements 
           applyleft = stdlib_lsame( side, 'L' )
           lastv = 0_ilp
           lastc = 0_ilp
           if( tau/=czero ) then
           ! set up variables for scanning v.  lastv begins pointing to the end
           ! of v.
              if( applyleft ) then
                 lastv = m
              else
                 lastv = n
              end if
              if( incv>0_ilp ) then
                 i = 1_ilp + (lastv-1) * incv
              else
                 i = 1_ilp
              end if
           ! look for the last non-czero row in v.
              do while( lastv>0 .and. v( i )==czero )
                 lastv = lastv - 1_ilp
                 i = i - incv
              end do
              if( applyleft ) then
           ! scan for the last non-czero column in c(1:lastv,:).
                 lastc = stdlib_ilaclc(lastv, n, c, ldc)
              else
           ! scan for the last non-czero row in c(:,1:lastv).
                 lastc = stdlib_ilaclr(m, lastv, c, ldc)
              end if
           end if
           ! note that lastc.eq.0_sp renders the blas operations null; no special
           ! case is needed at this level.
           if( applyleft ) then
              ! form  h * c
              if( lastv>0_ilp ) then
                 ! w(1:lastc,1) := c(1:lastv,1:lastc)**h * v(1:lastv,1)
                 call stdlib_cgemv( 'CONJUGATE TRANSPOSE', lastv, lastc, cone,c, ldc, v, incv, &
                           czero, work, 1_ilp )
                 ! c(1:lastv,1:lastc) := c(...) - v(1:lastv,1) * w(1:lastc,1)**h
                 call stdlib_cgerc( lastv, lastc, -tau, v, incv, work, 1_ilp, c, ldc )
              end if
           else
              ! form  c * h
              if( lastv>0_ilp ) then
                 ! w(1:lastc,1) := c(1:lastc,1:lastv) * v(1:lastv,1)
                 call stdlib_cgemv( 'NO TRANSPOSE', lastc, lastv, cone, c, ldc,v, incv, czero, &
                           work, 1_ilp )
                 ! c(1:lastc,1:lastv) := c(...) - w(1:lastc,1) * v(1:lastv,1)**h
                 call stdlib_cgerc( lastc, lastv, -tau, work, 1_ilp, v, incv, c, ldc )
              end if
           end if
           return
     end subroutine stdlib_clarf

     pure module subroutine stdlib_zlarf( side, m, n, v, incv, tau, c, ldc, work )
     !! ZLARF applies a complex elementary reflector H to a complex M-by-N
     !! matrix C, from either the left or the right. H is represented in the
     !! form
     !! H = I - tau * v * v**H
     !! where tau is a complex scalar and v is a complex vector.
     !! If tau = 0, then H is taken to be the unit matrix.
     !! To apply H**H, supply conjg(tau) instead
     !! tau.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side
           integer(ilp), intent(in) :: incv, ldc, m, n
           complex(dp), intent(in) :: tau
           ! Array Arguments 
           complex(dp), intent(inout) :: c(ldc,*)
           complex(dp), intent(in) :: v(*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: applyleft
           integer(ilp) :: i, lastv, lastc
           ! Executable Statements 
           applyleft = stdlib_lsame( side, 'L' )
           lastv = 0_ilp
           lastc = 0_ilp
           if( tau/=czero ) then
           ! set up variables for scanning v.  lastv begins pointing to the end
           ! of v.
              if( applyleft ) then
                 lastv = m
              else
                 lastv = n
              end if
              if( incv>0_ilp ) then
                 i = 1_ilp + (lastv-1) * incv
              else
                 i = 1_ilp
              end if
           ! look for the last non-czero row in v.
              do while( lastv>0 .and. v( i )==czero )
                 lastv = lastv - 1_ilp
                 i = i - incv
              end do
              if( applyleft ) then
           ! scan for the last non-czero column in c(1:lastv,:).
                 lastc = stdlib_ilazlc(lastv, n, c, ldc)
              else
           ! scan for the last non-czero row in c(:,1:lastv).
                 lastc = stdlib_ilazlr(m, lastv, c, ldc)
              end if
           end if
           ! note that lastc.eq.0_dp renders the blas operations null; no special
           ! case is needed at this level.
           if( applyleft ) then
              ! form  h * c
              if( lastv>0_ilp ) then
                 ! w(1:lastc,1) := c(1:lastv,1:lastc)**h * v(1:lastv,1)
                 call stdlib_zgemv( 'CONJUGATE TRANSPOSE', lastv, lastc, cone,c, ldc, v, incv, &
                           czero, work, 1_ilp )
                 ! c(1:lastv,1:lastc) := c(...) - v(1:lastv,1) * w(1:lastc,1)**h
                 call stdlib_zgerc( lastv, lastc, -tau, v, incv, work, 1_ilp, c, ldc )
              end if
           else
              ! form  c * h
              if( lastv>0_ilp ) then
                 ! w(1:lastc,1) := c(1:lastc,1:lastv) * v(1:lastv,1)
                 call stdlib_zgemv( 'NO TRANSPOSE', lastc, lastv, cone, c, ldc,v, incv, czero, &
                           work, 1_ilp )
                 ! c(1:lastc,1:lastv) := c(...) - w(1:lastc,1) * v(1:lastv,1)**h
                 call stdlib_zgerc( lastc, lastv, -tau, work, 1_ilp, v, incv, c, ldc )
              end if
           end if
           return
     end subroutine stdlib_zlarf




     pure module subroutine stdlib_slarfx( side, m, n, v, tau, c, ldc, work )
     !! SLARFX applies a real elementary reflector H to a real m by n
     !! matrix C, from either the left or the right. H is represented in the
     !! form
     !! H = I - tau * v * v**T
     !! where tau is a real scalar and v is a real vector.
     !! If tau = 0, then H is taken to be the unit matrix
     !! This version uses inline code if H has order < 11.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side
           integer(ilp), intent(in) :: ldc, m, n
           real(sp), intent(in) :: tau
           ! Array Arguments 
           real(sp), intent(inout) :: c(ldc,*)
           real(sp), intent(in) :: v(*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: j
           real(sp) :: sum, t1, t10, t2, t3, t4, t5, t6, t7, t8, t9, v1, v10, v2, v3, v4, v5, v6, &
                     v7, v8, v9
           ! Executable Statements 
           if( tau==zero )return
           if( stdlib_lsame( side, 'L' ) ) then
              ! form  h * c, where h has order m.
              go to ( 10, 30, 50, 70, 90, 110, 130, 150,170, 190 )m
              ! code for general m
              call stdlib_slarf( side, m, n, v, 1_ilp, tau, c, ldc, work )
              go to 410
              10 continue
              ! special code for 1 x 1 householder
              t1 = one - tau*v( 1_ilp )*v( 1_ilp )
              do j = 1, n
                 c( 1_ilp, j ) = t1*c( 1_ilp, j )
              end do
              go to 410
              30 continue
              ! special code for 2 x 2 householder
              v1 = v( 1_ilp )
              t1 = tau*v1
              v2 = v( 2_ilp )
              t2 = tau*v2
              do j = 1, n
                 sum = v1*c( 1_ilp, j ) + v2*c( 2_ilp, j )
                 c( 1_ilp, j ) = c( 1_ilp, j ) - sum*t1
                 c( 2_ilp, j ) = c( 2_ilp, j ) - sum*t2
              end do
              go to 410
              50 continue
              ! special code for 3 x 3 householder
              v1 = v( 1_ilp )
              t1 = tau*v1
              v2 = v( 2_ilp )
              t2 = tau*v2
              v3 = v( 3_ilp )
              t3 = tau*v3
              do j = 1, n
                 sum = v1*c( 1_ilp, j ) + v2*c( 2_ilp, j ) + v3*c( 3_ilp, j )
                 c( 1_ilp, j ) = c( 1_ilp, j ) - sum*t1
                 c( 2_ilp, j ) = c( 2_ilp, j ) - sum*t2
                 c( 3_ilp, j ) = c( 3_ilp, j ) - sum*t3
              end do
              go to 410
              70 continue
              ! special code for 4 x 4 householder
              v1 = v( 1_ilp )
              t1 = tau*v1
              v2 = v( 2_ilp )
              t2 = tau*v2
              v3 = v( 3_ilp )
              t3 = tau*v3
              v4 = v( 4_ilp )
              t4 = tau*v4
              do j = 1, n
                 sum = v1*c( 1_ilp, j ) + v2*c( 2_ilp, j ) + v3*c( 3_ilp, j ) +v4*c( 4_ilp, j )
                 c( 1_ilp, j ) = c( 1_ilp, j ) - sum*t1
                 c( 2_ilp, j ) = c( 2_ilp, j ) - sum*t2
                 c( 3_ilp, j ) = c( 3_ilp, j ) - sum*t3
                 c( 4_ilp, j ) = c( 4_ilp, j ) - sum*t4
              end do
              go to 410
              90 continue
              ! special code for 5 x 5 householder
              v1 = v( 1_ilp )
              t1 = tau*v1
              v2 = v( 2_ilp )
              t2 = tau*v2
              v3 = v( 3_ilp )
              t3 = tau*v3
              v4 = v( 4_ilp )
              t4 = tau*v4
              v5 = v( 5_ilp )
              t5 = tau*v5
              do j = 1, n
                 sum = v1*c( 1_ilp, j ) + v2*c( 2_ilp, j ) + v3*c( 3_ilp, j ) +v4*c( 4_ilp, j ) + v5*c( 5_ilp, j )
                           
                 c( 1_ilp, j ) = c( 1_ilp, j ) - sum*t1
                 c( 2_ilp, j ) = c( 2_ilp, j ) - sum*t2
                 c( 3_ilp, j ) = c( 3_ilp, j ) - sum*t3
                 c( 4_ilp, j ) = c( 4_ilp, j ) - sum*t4
                 c( 5_ilp, j ) = c( 5_ilp, j ) - sum*t5
              end do
              go to 410
              110 continue
              ! special code for 6 x 6 householder
              v1 = v( 1_ilp )
              t1 = tau*v1
              v2 = v( 2_ilp )
              t2 = tau*v2
              v3 = v( 3_ilp )
              t3 = tau*v3
              v4 = v( 4_ilp )
              t4 = tau*v4
              v5 = v( 5_ilp )
              t5 = tau*v5
              v6 = v( 6_ilp )
              t6 = tau*v6
              do j = 1, n
                 sum = v1*c( 1_ilp, j ) + v2*c( 2_ilp, j ) + v3*c( 3_ilp, j ) +v4*c( 4_ilp, j ) + v5*c( 5_ilp, j ) + &
                           v6*c( 6_ilp, j )
                 c( 1_ilp, j ) = c( 1_ilp, j ) - sum*t1
                 c( 2_ilp, j ) = c( 2_ilp, j ) - sum*t2
                 c( 3_ilp, j ) = c( 3_ilp, j ) - sum*t3
                 c( 4_ilp, j ) = c( 4_ilp, j ) - sum*t4
                 c( 5_ilp, j ) = c( 5_ilp, j ) - sum*t5
                 c( 6_ilp, j ) = c( 6_ilp, j ) - sum*t6
              end do
              go to 410
              130 continue
              ! special code for 7 x 7 householder
              v1 = v( 1_ilp )
              t1 = tau*v1
              v2 = v( 2_ilp )
              t2 = tau*v2
              v3 = v( 3_ilp )
              t3 = tau*v3
              v4 = v( 4_ilp )
              t4 = tau*v4
              v5 = v( 5_ilp )
              t5 = tau*v5
              v6 = v( 6_ilp )
              t6 = tau*v6
              v7 = v( 7_ilp )
              t7 = tau*v7
              do j = 1, n
                 sum = v1*c( 1_ilp, j ) + v2*c( 2_ilp, j ) + v3*c( 3_ilp, j ) +v4*c( 4_ilp, j ) + v5*c( 5_ilp, j ) + &
                           v6*c( 6_ilp, j ) +v7*c( 7_ilp, j )
                 c( 1_ilp, j ) = c( 1_ilp, j ) - sum*t1
                 c( 2_ilp, j ) = c( 2_ilp, j ) - sum*t2
                 c( 3_ilp, j ) = c( 3_ilp, j ) - sum*t3
                 c( 4_ilp, j ) = c( 4_ilp, j ) - sum*t4
                 c( 5_ilp, j ) = c( 5_ilp, j ) - sum*t5
                 c( 6_ilp, j ) = c( 6_ilp, j ) - sum*t6
                 c( 7_ilp, j ) = c( 7_ilp, j ) - sum*t7
              end do
              go to 410
              150 continue
              ! special code for 8 x 8 householder
              v1 = v( 1_ilp )
              t1 = tau*v1
              v2 = v( 2_ilp )
              t2 = tau*v2
              v3 = v( 3_ilp )
              t3 = tau*v3
              v4 = v( 4_ilp )
              t4 = tau*v4
              v5 = v( 5_ilp )
              t5 = tau*v5
              v6 = v( 6_ilp )
              t6 = tau*v6
              v7 = v( 7_ilp )
              t7 = tau*v7
              v8 = v( 8_ilp )
              t8 = tau*v8
              do j = 1, n
                 sum = v1*c( 1_ilp, j ) + v2*c( 2_ilp, j ) + v3*c( 3_ilp, j ) +v4*c( 4_ilp, j ) + v5*c( 5_ilp, j ) + &
                           v6*c( 6_ilp, j ) +v7*c( 7_ilp, j ) + v8*c( 8_ilp, j )
                 c( 1_ilp, j ) = c( 1_ilp, j ) - sum*t1
                 c( 2_ilp, j ) = c( 2_ilp, j ) - sum*t2
                 c( 3_ilp, j ) = c( 3_ilp, j ) - sum*t3
                 c( 4_ilp, j ) = c( 4_ilp, j ) - sum*t4
                 c( 5_ilp, j ) = c( 5_ilp, j ) - sum*t5
                 c( 6_ilp, j ) = c( 6_ilp, j ) - sum*t6
                 c( 7_ilp, j ) = c( 7_ilp, j ) - sum*t7
                 c( 8_ilp, j ) = c( 8_ilp, j ) - sum*t8
              end do
              go to 410
              170 continue
              ! special code for 9 x 9 householder
              v1 = v( 1_ilp )
              t1 = tau*v1
              v2 = v( 2_ilp )
              t2 = tau*v2
              v3 = v( 3_ilp )
              t3 = tau*v3
              v4 = v( 4_ilp )
              t4 = tau*v4
              v5 = v( 5_ilp )
              t5 = tau*v5
              v6 = v( 6_ilp )
              t6 = tau*v6
              v7 = v( 7_ilp )
              t7 = tau*v7
              v8 = v( 8_ilp )
              t8 = tau*v8
              v9 = v( 9_ilp )
              t9 = tau*v9
              do j = 1, n
                 sum = v1*c( 1_ilp, j ) + v2*c( 2_ilp, j ) + v3*c( 3_ilp, j ) +v4*c( 4_ilp, j ) + v5*c( 5_ilp, j ) + &
                           v6*c( 6_ilp, j ) +v7*c( 7_ilp, j ) + v8*c( 8_ilp, j ) + v9*c( 9_ilp, j )
                 c( 1_ilp, j ) = c( 1_ilp, j ) - sum*t1
                 c( 2_ilp, j ) = c( 2_ilp, j ) - sum*t2
                 c( 3_ilp, j ) = c( 3_ilp, j ) - sum*t3
                 c( 4_ilp, j ) = c( 4_ilp, j ) - sum*t4
                 c( 5_ilp, j ) = c( 5_ilp, j ) - sum*t5
                 c( 6_ilp, j ) = c( 6_ilp, j ) - sum*t6
                 c( 7_ilp, j ) = c( 7_ilp, j ) - sum*t7
                 c( 8_ilp, j ) = c( 8_ilp, j ) - sum*t8
                 c( 9_ilp, j ) = c( 9_ilp, j ) - sum*t9
              end do
              go to 410
              190 continue
              ! special code for 10 x 10 householder
              v1 = v( 1_ilp )
              t1 = tau*v1
              v2 = v( 2_ilp )
              t2 = tau*v2
              v3 = v( 3_ilp )
              t3 = tau*v3
              v4 = v( 4_ilp )
              t4 = tau*v4
              v5 = v( 5_ilp )
              t5 = tau*v5
              v6 = v( 6_ilp )
              t6 = tau*v6
              v7 = v( 7_ilp )
              t7 = tau*v7
              v8 = v( 8_ilp )
              t8 = tau*v8
              v9 = v( 9_ilp )
              t9 = tau*v9
              v10 = v( 10_ilp )
              t10 = tau*v10
              do j = 1, n
                 sum = v1*c( 1_ilp, j ) + v2*c( 2_ilp, j ) + v3*c( 3_ilp, j ) +v4*c( 4_ilp, j ) + v5*c( 5_ilp, j ) + &
                           v6*c( 6_ilp, j ) +v7*c( 7_ilp, j ) + v8*c( 8_ilp, j ) + v9*c( 9_ilp, j ) +v10*c( 10_ilp, j )
                 c( 1_ilp, j ) = c( 1_ilp, j ) - sum*t1
                 c( 2_ilp, j ) = c( 2_ilp, j ) - sum*t2
                 c( 3_ilp, j ) = c( 3_ilp, j ) - sum*t3
                 c( 4_ilp, j ) = c( 4_ilp, j ) - sum*t4
                 c( 5_ilp, j ) = c( 5_ilp, j ) - sum*t5
                 c( 6_ilp, j ) = c( 6_ilp, j ) - sum*t6
                 c( 7_ilp, j ) = c( 7_ilp, j ) - sum*t7
                 c( 8_ilp, j ) = c( 8_ilp, j ) - sum*t8
                 c( 9_ilp, j ) = c( 9_ilp, j ) - sum*t9
                 c( 10_ilp, j ) = c( 10_ilp, j ) - sum*t10
              end do
              go to 410
           else
              ! form  c * h, where h has order n.
              go to ( 210, 230, 250, 270, 290, 310, 330, 350,370, 390 )n
              ! code for general n
              call stdlib_slarf( side, m, n, v, 1_ilp, tau, c, ldc, work )
              go to 410
              210 continue
              ! special code for 1 x 1 householder
              t1 = one - tau*v( 1_ilp )*v( 1_ilp )
              do j = 1, m
                 c( j, 1_ilp ) = t1*c( j, 1_ilp )
              end do
              go to 410
              230 continue
              ! special code for 2 x 2 householder
              v1 = v( 1_ilp )
              t1 = tau*v1
              v2 = v( 2_ilp )
              t2 = tau*v2
              do j = 1, m
                 sum = v1*c( j, 1_ilp ) + v2*c( j, 2_ilp )
                 c( j, 1_ilp ) = c( j, 1_ilp ) - sum*t1
                 c( j, 2_ilp ) = c( j, 2_ilp ) - sum*t2
              end do
              go to 410
              250 continue
              ! special code for 3 x 3 householder
              v1 = v( 1_ilp )
              t1 = tau*v1
              v2 = v( 2_ilp )
              t2 = tau*v2
              v3 = v( 3_ilp )
              t3 = tau*v3
              do j = 1, m
                 sum = v1*c( j, 1_ilp ) + v2*c( j, 2_ilp ) + v3*c( j, 3_ilp )
                 c( j, 1_ilp ) = c( j, 1_ilp ) - sum*t1
                 c( j, 2_ilp ) = c( j, 2_ilp ) - sum*t2
                 c( j, 3_ilp ) = c( j, 3_ilp ) - sum*t3
              end do
              go to 410
              270 continue
              ! special code for 4 x 4 householder
              v1 = v( 1_ilp )
              t1 = tau*v1
              v2 = v( 2_ilp )
              t2 = tau*v2
              v3 = v( 3_ilp )
              t3 = tau*v3
              v4 = v( 4_ilp )
              t4 = tau*v4
              do j = 1, m
                 sum = v1*c( j, 1_ilp ) + v2*c( j, 2_ilp ) + v3*c( j, 3_ilp ) +v4*c( j, 4_ilp )
                 c( j, 1_ilp ) = c( j, 1_ilp ) - sum*t1
                 c( j, 2_ilp ) = c( j, 2_ilp ) - sum*t2
                 c( j, 3_ilp ) = c( j, 3_ilp ) - sum*t3
                 c( j, 4_ilp ) = c( j, 4_ilp ) - sum*t4
              end do
              go to 410
              290 continue
              ! special code for 5 x 5 householder
              v1 = v( 1_ilp )
              t1 = tau*v1
              v2 = v( 2_ilp )
              t2 = tau*v2
              v3 = v( 3_ilp )
              t3 = tau*v3
              v4 = v( 4_ilp )
              t4 = tau*v4
              v5 = v( 5_ilp )
              t5 = tau*v5
              do j = 1, m
                 sum = v1*c( j, 1_ilp ) + v2*c( j, 2_ilp ) + v3*c( j, 3_ilp ) +v4*c( j, 4_ilp ) + v5*c( j, 5_ilp )
                           
                 c( j, 1_ilp ) = c( j, 1_ilp ) - sum*t1
                 c( j, 2_ilp ) = c( j, 2_ilp ) - sum*t2
                 c( j, 3_ilp ) = c( j, 3_ilp ) - sum*t3
                 c( j, 4_ilp ) = c( j, 4_ilp ) - sum*t4
                 c( j, 5_ilp ) = c( j, 5_ilp ) - sum*t5
              end do
              go to 410
              310 continue
              ! special code for 6 x 6 householder
              v1 = v( 1_ilp )
              t1 = tau*v1
              v2 = v( 2_ilp )
              t2 = tau*v2
              v3 = v( 3_ilp )
              t3 = tau*v3
              v4 = v( 4_ilp )
              t4 = tau*v4
              v5 = v( 5_ilp )
              t5 = tau*v5
              v6 = v( 6_ilp )
              t6 = tau*v6
              do j = 1, m
                 sum = v1*c( j, 1_ilp ) + v2*c( j, 2_ilp ) + v3*c( j, 3_ilp ) +v4*c( j, 4_ilp ) + v5*c( j, 5_ilp ) + &
                           v6*c( j, 6_ilp )
                 c( j, 1_ilp ) = c( j, 1_ilp ) - sum*t1
                 c( j, 2_ilp ) = c( j, 2_ilp ) - sum*t2
                 c( j, 3_ilp ) = c( j, 3_ilp ) - sum*t3
                 c( j, 4_ilp ) = c( j, 4_ilp ) - sum*t4
                 c( j, 5_ilp ) = c( j, 5_ilp ) - sum*t5
                 c( j, 6_ilp ) = c( j, 6_ilp ) - sum*t6
              end do
              go to 410
              330 continue
              ! special code for 7 x 7 householder
              v1 = v( 1_ilp )
              t1 = tau*v1
              v2 = v( 2_ilp )
              t2 = tau*v2
              v3 = v( 3_ilp )
              t3 = tau*v3
              v4 = v( 4_ilp )
              t4 = tau*v4
              v5 = v( 5_ilp )
              t5 = tau*v5
              v6 = v( 6_ilp )
              t6 = tau*v6
              v7 = v( 7_ilp )
              t7 = tau*v7
              do j = 1, m
                 sum = v1*c( j, 1_ilp ) + v2*c( j, 2_ilp ) + v3*c( j, 3_ilp ) +v4*c( j, 4_ilp ) + v5*c( j, 5_ilp ) + &
                           v6*c( j, 6_ilp ) +v7*c( j, 7_ilp )
                 c( j, 1_ilp ) = c( j, 1_ilp ) - sum*t1
                 c( j, 2_ilp ) = c( j, 2_ilp ) - sum*t2
                 c( j, 3_ilp ) = c( j, 3_ilp ) - sum*t3
                 c( j, 4_ilp ) = c( j, 4_ilp ) - sum*t4
                 c( j, 5_ilp ) = c( j, 5_ilp ) - sum*t5
                 c( j, 6_ilp ) = c( j, 6_ilp ) - sum*t6
                 c( j, 7_ilp ) = c( j, 7_ilp ) - sum*t7
              end do
              go to 410
              350 continue
              ! special code for 8 x 8 householder
              v1 = v( 1_ilp )
              t1 = tau*v1
              v2 = v( 2_ilp )
              t2 = tau*v2
              v3 = v( 3_ilp )
              t3 = tau*v3
              v4 = v( 4_ilp )
              t4 = tau*v4
              v5 = v( 5_ilp )
              t5 = tau*v5
              v6 = v( 6_ilp )
              t6 = tau*v6
              v7 = v( 7_ilp )
              t7 = tau*v7
              v8 = v( 8_ilp )
              t8 = tau*v8
              do j = 1, m
                 sum = v1*c( j, 1_ilp ) + v2*c( j, 2_ilp ) + v3*c( j, 3_ilp ) +v4*c( j, 4_ilp ) + v5*c( j, 5_ilp ) + &
                           v6*c( j, 6_ilp ) +v7*c( j, 7_ilp ) + v8*c( j, 8_ilp )
                 c( j, 1_ilp ) = c( j, 1_ilp ) - sum*t1
                 c( j, 2_ilp ) = c( j, 2_ilp ) - sum*t2
                 c( j, 3_ilp ) = c( j, 3_ilp ) - sum*t3
                 c( j, 4_ilp ) = c( j, 4_ilp ) - sum*t4
                 c( j, 5_ilp ) = c( j, 5_ilp ) - sum*t5
                 c( j, 6_ilp ) = c( j, 6_ilp ) - sum*t6
                 c( j, 7_ilp ) = c( j, 7_ilp ) - sum*t7
                 c( j, 8_ilp ) = c( j, 8_ilp ) - sum*t8
              end do
              go to 410
              370 continue
              ! special code for 9 x 9 householder
              v1 = v( 1_ilp )
              t1 = tau*v1
              v2 = v( 2_ilp )
              t2 = tau*v2
              v3 = v( 3_ilp )
              t3 = tau*v3
              v4 = v( 4_ilp )
              t4 = tau*v4
              v5 = v( 5_ilp )
              t5 = tau*v5
              v6 = v( 6_ilp )
              t6 = tau*v6
              v7 = v( 7_ilp )
              t7 = tau*v7
              v8 = v( 8_ilp )
              t8 = tau*v8
              v9 = v( 9_ilp )
              t9 = tau*v9
              do j = 1, m
                 sum = v1*c( j, 1_ilp ) + v2*c( j, 2_ilp ) + v3*c( j, 3_ilp ) +v4*c( j, 4_ilp ) + v5*c( j, 5_ilp ) + &
                           v6*c( j, 6_ilp ) +v7*c( j, 7_ilp ) + v8*c( j, 8_ilp ) + v9*c( j, 9_ilp )
                 c( j, 1_ilp ) = c( j, 1_ilp ) - sum*t1
                 c( j, 2_ilp ) = c( j, 2_ilp ) - sum*t2
                 c( j, 3_ilp ) = c( j, 3_ilp ) - sum*t3
                 c( j, 4_ilp ) = c( j, 4_ilp ) - sum*t4
                 c( j, 5_ilp ) = c( j, 5_ilp ) - sum*t5
                 c( j, 6_ilp ) = c( j, 6_ilp ) - sum*t6
                 c( j, 7_ilp ) = c( j, 7_ilp ) - sum*t7
                 c( j, 8_ilp ) = c( j, 8_ilp ) - sum*t8
                 c( j, 9_ilp ) = c( j, 9_ilp ) - sum*t9
              end do
              go to 410
              390 continue
              ! special code for 10 x 10 householder
              v1 = v( 1_ilp )
              t1 = tau*v1
              v2 = v( 2_ilp )
              t2 = tau*v2
              v3 = v( 3_ilp )
              t3 = tau*v3
              v4 = v( 4_ilp )
              t4 = tau*v4
              v5 = v( 5_ilp )
              t5 = tau*v5
              v6 = v( 6_ilp )
              t6 = tau*v6
              v7 = v( 7_ilp )
              t7 = tau*v7
              v8 = v( 8_ilp )
              t8 = tau*v8
              v9 = v( 9_ilp )
              t9 = tau*v9
              v10 = v( 10_ilp )
              t10 = tau*v10
              do j = 1, m
                 sum = v1*c( j, 1_ilp ) + v2*c( j, 2_ilp ) + v3*c( j, 3_ilp ) +v4*c( j, 4_ilp ) + v5*c( j, 5_ilp ) + &
                           v6*c( j, 6_ilp ) +v7*c( j, 7_ilp ) + v8*c( j, 8_ilp ) + v9*c( j, 9_ilp ) +v10*c( j, 10_ilp )
                 c( j, 1_ilp ) = c( j, 1_ilp ) - sum*t1
                 c( j, 2_ilp ) = c( j, 2_ilp ) - sum*t2
                 c( j, 3_ilp ) = c( j, 3_ilp ) - sum*t3
                 c( j, 4_ilp ) = c( j, 4_ilp ) - sum*t4
                 c( j, 5_ilp ) = c( j, 5_ilp ) - sum*t5
                 c( j, 6_ilp ) = c( j, 6_ilp ) - sum*t6
                 c( j, 7_ilp ) = c( j, 7_ilp ) - sum*t7
                 c( j, 8_ilp ) = c( j, 8_ilp ) - sum*t8
                 c( j, 9_ilp ) = c( j, 9_ilp ) - sum*t9
                 c( j, 10_ilp ) = c( j, 10_ilp ) - sum*t10
              end do
              go to 410
           end if
       410 return
     end subroutine stdlib_slarfx

     pure module subroutine stdlib_dlarfx( side, m, n, v, tau, c, ldc, work )
     !! DLARFX applies a real elementary reflector H to a real m by n
     !! matrix C, from either the left or the right. H is represented in the
     !! form
     !! H = I - tau * v * v**T
     !! where tau is a real scalar and v is a real vector.
     !! If tau = 0, then H is taken to be the unit matrix
     !! This version uses inline code if H has order < 11.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side
           integer(ilp), intent(in) :: ldc, m, n
           real(dp), intent(in) :: tau
           ! Array Arguments 
           real(dp), intent(inout) :: c(ldc,*)
           real(dp), intent(in) :: v(*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: j
           real(dp) :: sum, t1, t10, t2, t3, t4, t5, t6, t7, t8, t9, v1, v10, v2, v3, v4, v5, v6, &
                     v7, v8, v9
           ! Executable Statements 
           if( tau==zero )return
           if( stdlib_lsame( side, 'L' ) ) then
              ! form  h * c, where h has order m.
              go to ( 10, 30, 50, 70, 90, 110, 130, 150,170, 190 )m
              ! code for general m
              call stdlib_dlarf( side, m, n, v, 1_ilp, tau, c, ldc, work )
              go to 410
              10 continue
              ! special code for 1 x 1 householder
              t1 = one - tau*v( 1_ilp )*v( 1_ilp )
              do j = 1, n
                 c( 1_ilp, j ) = t1*c( 1_ilp, j )
              end do
              go to 410
              30 continue
              ! special code for 2 x 2 householder
              v1 = v( 1_ilp )
              t1 = tau*v1
              v2 = v( 2_ilp )
              t2 = tau*v2
              do j = 1, n
                 sum = v1*c( 1_ilp, j ) + v2*c( 2_ilp, j )
                 c( 1_ilp, j ) = c( 1_ilp, j ) - sum*t1
                 c( 2_ilp, j ) = c( 2_ilp, j ) - sum*t2
              end do
              go to 410
              50 continue
              ! special code for 3 x 3 householder
              v1 = v( 1_ilp )
              t1 = tau*v1
              v2 = v( 2_ilp )
              t2 = tau*v2
              v3 = v( 3_ilp )
              t3 = tau*v3
              do j = 1, n
                 sum = v1*c( 1_ilp, j ) + v2*c( 2_ilp, j ) + v3*c( 3_ilp, j )
                 c( 1_ilp, j ) = c( 1_ilp, j ) - sum*t1
                 c( 2_ilp, j ) = c( 2_ilp, j ) - sum*t2
                 c( 3_ilp, j ) = c( 3_ilp, j ) - sum*t3
              end do
              go to 410
              70 continue
              ! special code for 4 x 4 householder
              v1 = v( 1_ilp )
              t1 = tau*v1
              v2 = v( 2_ilp )
              t2 = tau*v2
              v3 = v( 3_ilp )
              t3 = tau*v3
              v4 = v( 4_ilp )
              t4 = tau*v4
              do j = 1, n
                 sum = v1*c( 1_ilp, j ) + v2*c( 2_ilp, j ) + v3*c( 3_ilp, j ) +v4*c( 4_ilp, j )
                 c( 1_ilp, j ) = c( 1_ilp, j ) - sum*t1
                 c( 2_ilp, j ) = c( 2_ilp, j ) - sum*t2
                 c( 3_ilp, j ) = c( 3_ilp, j ) - sum*t3
                 c( 4_ilp, j ) = c( 4_ilp, j ) - sum*t4
              end do
              go to 410
              90 continue
              ! special code for 5 x 5 householder
              v1 = v( 1_ilp )
              t1 = tau*v1
              v2 = v( 2_ilp )
              t2 = tau*v2
              v3 = v( 3_ilp )
              t3 = tau*v3
              v4 = v( 4_ilp )
              t4 = tau*v4
              v5 = v( 5_ilp )
              t5 = tau*v5
              do j = 1, n
                 sum = v1*c( 1_ilp, j ) + v2*c( 2_ilp, j ) + v3*c( 3_ilp, j ) +v4*c( 4_ilp, j ) + v5*c( 5_ilp, j )
                           
                 c( 1_ilp, j ) = c( 1_ilp, j ) - sum*t1
                 c( 2_ilp, j ) = c( 2_ilp, j ) - sum*t2
                 c( 3_ilp, j ) = c( 3_ilp, j ) - sum*t3
                 c( 4_ilp, j ) = c( 4_ilp, j ) - sum*t4
                 c( 5_ilp, j ) = c( 5_ilp, j ) - sum*t5
              end do
              go to 410
              110 continue
              ! special code for 6 x 6 householder
              v1 = v( 1_ilp )
              t1 = tau*v1
              v2 = v( 2_ilp )
              t2 = tau*v2
              v3 = v( 3_ilp )
              t3 = tau*v3
              v4 = v( 4_ilp )
              t4 = tau*v4
              v5 = v( 5_ilp )
              t5 = tau*v5
              v6 = v( 6_ilp )
              t6 = tau*v6
              do j = 1, n
                 sum = v1*c( 1_ilp, j ) + v2*c( 2_ilp, j ) + v3*c( 3_ilp, j ) +v4*c( 4_ilp, j ) + v5*c( 5_ilp, j ) + &
                           v6*c( 6_ilp, j )
                 c( 1_ilp, j ) = c( 1_ilp, j ) - sum*t1
                 c( 2_ilp, j ) = c( 2_ilp, j ) - sum*t2
                 c( 3_ilp, j ) = c( 3_ilp, j ) - sum*t3
                 c( 4_ilp, j ) = c( 4_ilp, j ) - sum*t4
                 c( 5_ilp, j ) = c( 5_ilp, j ) - sum*t5
                 c( 6_ilp, j ) = c( 6_ilp, j ) - sum*t6
              end do
              go to 410
              130 continue
              ! special code for 7 x 7 householder
              v1 = v( 1_ilp )
              t1 = tau*v1
              v2 = v( 2_ilp )
              t2 = tau*v2
              v3 = v( 3_ilp )
              t3 = tau*v3
              v4 = v( 4_ilp )
              t4 = tau*v4
              v5 = v( 5_ilp )
              t5 = tau*v5
              v6 = v( 6_ilp )
              t6 = tau*v6
              v7 = v( 7_ilp )
              t7 = tau*v7
              do j = 1, n
                 sum = v1*c( 1_ilp, j ) + v2*c( 2_ilp, j ) + v3*c( 3_ilp, j ) +v4*c( 4_ilp, j ) + v5*c( 5_ilp, j ) + &
                           v6*c( 6_ilp, j ) +v7*c( 7_ilp, j )
                 c( 1_ilp, j ) = c( 1_ilp, j ) - sum*t1
                 c( 2_ilp, j ) = c( 2_ilp, j ) - sum*t2
                 c( 3_ilp, j ) = c( 3_ilp, j ) - sum*t3
                 c( 4_ilp, j ) = c( 4_ilp, j ) - sum*t4
                 c( 5_ilp, j ) = c( 5_ilp, j ) - sum*t5
                 c( 6_ilp, j ) = c( 6_ilp, j ) - sum*t6
                 c( 7_ilp, j ) = c( 7_ilp, j ) - sum*t7
              end do
              go to 410
              150 continue
              ! special code for 8 x 8 householder
              v1 = v( 1_ilp )
              t1 = tau*v1
              v2 = v( 2_ilp )
              t2 = tau*v2
              v3 = v( 3_ilp )
              t3 = tau*v3
              v4 = v( 4_ilp )
              t4 = tau*v4
              v5 = v( 5_ilp )
              t5 = tau*v5
              v6 = v( 6_ilp )
              t6 = tau*v6
              v7 = v( 7_ilp )
              t7 = tau*v7
              v8 = v( 8_ilp )
              t8 = tau*v8
              do j = 1, n
                 sum = v1*c( 1_ilp, j ) + v2*c( 2_ilp, j ) + v3*c( 3_ilp, j ) +v4*c( 4_ilp, j ) + v5*c( 5_ilp, j ) + &
                           v6*c( 6_ilp, j ) +v7*c( 7_ilp, j ) + v8*c( 8_ilp, j )
                 c( 1_ilp, j ) = c( 1_ilp, j ) - sum*t1
                 c( 2_ilp, j ) = c( 2_ilp, j ) - sum*t2
                 c( 3_ilp, j ) = c( 3_ilp, j ) - sum*t3
                 c( 4_ilp, j ) = c( 4_ilp, j ) - sum*t4
                 c( 5_ilp, j ) = c( 5_ilp, j ) - sum*t5
                 c( 6_ilp, j ) = c( 6_ilp, j ) - sum*t6
                 c( 7_ilp, j ) = c( 7_ilp, j ) - sum*t7
                 c( 8_ilp, j ) = c( 8_ilp, j ) - sum*t8
              end do
              go to 410
              170 continue
              ! special code for 9 x 9 householder
              v1 = v( 1_ilp )
              t1 = tau*v1
              v2 = v( 2_ilp )
              t2 = tau*v2
              v3 = v( 3_ilp )
              t3 = tau*v3
              v4 = v( 4_ilp )
              t4 = tau*v4
              v5 = v( 5_ilp )
              t5 = tau*v5
              v6 = v( 6_ilp )
              t6 = tau*v6
              v7 = v( 7_ilp )
              t7 = tau*v7
              v8 = v( 8_ilp )
              t8 = tau*v8
              v9 = v( 9_ilp )
              t9 = tau*v9
              do j = 1, n
                 sum = v1*c( 1_ilp, j ) + v2*c( 2_ilp, j ) + v3*c( 3_ilp, j ) +v4*c( 4_ilp, j ) + v5*c( 5_ilp, j ) + &
                           v6*c( 6_ilp, j ) +v7*c( 7_ilp, j ) + v8*c( 8_ilp, j ) + v9*c( 9_ilp, j )
                 c( 1_ilp, j ) = c( 1_ilp, j ) - sum*t1
                 c( 2_ilp, j ) = c( 2_ilp, j ) - sum*t2
                 c( 3_ilp, j ) = c( 3_ilp, j ) - sum*t3
                 c( 4_ilp, j ) = c( 4_ilp, j ) - sum*t4
                 c( 5_ilp, j ) = c( 5_ilp, j ) - sum*t5
                 c( 6_ilp, j ) = c( 6_ilp, j ) - sum*t6
                 c( 7_ilp, j ) = c( 7_ilp, j ) - sum*t7
                 c( 8_ilp, j ) = c( 8_ilp, j ) - sum*t8
                 c( 9_ilp, j ) = c( 9_ilp, j ) - sum*t9
              end do
              go to 410
              190 continue
              ! special code for 10 x 10 householder
              v1 = v( 1_ilp )
              t1 = tau*v1
              v2 = v( 2_ilp )
              t2 = tau*v2
              v3 = v( 3_ilp )
              t3 = tau*v3
              v4 = v( 4_ilp )
              t4 = tau*v4
              v5 = v( 5_ilp )
              t5 = tau*v5
              v6 = v( 6_ilp )
              t6 = tau*v6
              v7 = v( 7_ilp )
              t7 = tau*v7
              v8 = v( 8_ilp )
              t8 = tau*v8
              v9 = v( 9_ilp )
              t9 = tau*v9
              v10 = v( 10_ilp )
              t10 = tau*v10
              do j = 1, n
                 sum = v1*c( 1_ilp, j ) + v2*c( 2_ilp, j ) + v3*c( 3_ilp, j ) +v4*c( 4_ilp, j ) + v5*c( 5_ilp, j ) + &
                           v6*c( 6_ilp, j ) +v7*c( 7_ilp, j ) + v8*c( 8_ilp, j ) + v9*c( 9_ilp, j ) +v10*c( 10_ilp, j )
                 c( 1_ilp, j ) = c( 1_ilp, j ) - sum*t1
                 c( 2_ilp, j ) = c( 2_ilp, j ) - sum*t2
                 c( 3_ilp, j ) = c( 3_ilp, j ) - sum*t3
                 c( 4_ilp, j ) = c( 4_ilp, j ) - sum*t4
                 c( 5_ilp, j ) = c( 5_ilp, j ) - sum*t5
                 c( 6_ilp, j ) = c( 6_ilp, j ) - sum*t6
                 c( 7_ilp, j ) = c( 7_ilp, j ) - sum*t7
                 c( 8_ilp, j ) = c( 8_ilp, j ) - sum*t8
                 c( 9_ilp, j ) = c( 9_ilp, j ) - sum*t9
                 c( 10_ilp, j ) = c( 10_ilp, j ) - sum*t10
              end do
              go to 410
           else
              ! form  c * h, where h has order n.
              go to ( 210, 230, 250, 270, 290, 310, 330, 350,370, 390 )n
              ! code for general n
              call stdlib_dlarf( side, m, n, v, 1_ilp, tau, c, ldc, work )
              go to 410
              210 continue
              ! special code for 1 x 1 householder
              t1 = one - tau*v( 1_ilp )*v( 1_ilp )
              do j = 1, m
                 c( j, 1_ilp ) = t1*c( j, 1_ilp )
              end do
              go to 410
              230 continue
              ! special code for 2 x 2 householder
              v1 = v( 1_ilp )
              t1 = tau*v1
              v2 = v( 2_ilp )
              t2 = tau*v2
              do j = 1, m
                 sum = v1*c( j, 1_ilp ) + v2*c( j, 2_ilp )
                 c( j, 1_ilp ) = c( j, 1_ilp ) - sum*t1
                 c( j, 2_ilp ) = c( j, 2_ilp ) - sum*t2
              end do
              go to 410
              250 continue
              ! special code for 3 x 3 householder
              v1 = v( 1_ilp )
              t1 = tau*v1
              v2 = v( 2_ilp )
              t2 = tau*v2
              v3 = v( 3_ilp )
              t3 = tau*v3
              do j = 1, m
                 sum = v1*c( j, 1_ilp ) + v2*c( j, 2_ilp ) + v3*c( j, 3_ilp )
                 c( j, 1_ilp ) = c( j, 1_ilp ) - sum*t1
                 c( j, 2_ilp ) = c( j, 2_ilp ) - sum*t2
                 c( j, 3_ilp ) = c( j, 3_ilp ) - sum*t3
              end do
              go to 410
              270 continue
              ! special code for 4 x 4 householder
              v1 = v( 1_ilp )
              t1 = tau*v1
              v2 = v( 2_ilp )
              t2 = tau*v2
              v3 = v( 3_ilp )
              t3 = tau*v3
              v4 = v( 4_ilp )
              t4 = tau*v4
              do j = 1, m
                 sum = v1*c( j, 1_ilp ) + v2*c( j, 2_ilp ) + v3*c( j, 3_ilp ) +v4*c( j, 4_ilp )
                 c( j, 1_ilp ) = c( j, 1_ilp ) - sum*t1
                 c( j, 2_ilp ) = c( j, 2_ilp ) - sum*t2
                 c( j, 3_ilp ) = c( j, 3_ilp ) - sum*t3
                 c( j, 4_ilp ) = c( j, 4_ilp ) - sum*t4
              end do
              go to 410
              290 continue
              ! special code for 5 x 5 householder
              v1 = v( 1_ilp )
              t1 = tau*v1
              v2 = v( 2_ilp )
              t2 = tau*v2
              v3 = v( 3_ilp )
              t3 = tau*v3
              v4 = v( 4_ilp )
              t4 = tau*v4
              v5 = v( 5_ilp )
              t5 = tau*v5
              do j = 1, m
                 sum = v1*c( j, 1_ilp ) + v2*c( j, 2_ilp ) + v3*c( j, 3_ilp ) +v4*c( j, 4_ilp ) + v5*c( j, 5_ilp )
                           
                 c( j, 1_ilp ) = c( j, 1_ilp ) - sum*t1
                 c( j, 2_ilp ) = c( j, 2_ilp ) - sum*t2
                 c( j, 3_ilp ) = c( j, 3_ilp ) - sum*t3
                 c( j, 4_ilp ) = c( j, 4_ilp ) - sum*t4
                 c( j, 5_ilp ) = c( j, 5_ilp ) - sum*t5
              end do
              go to 410
              310 continue
              ! special code for 6 x 6 householder
              v1 = v( 1_ilp )
              t1 = tau*v1
              v2 = v( 2_ilp )
              t2 = tau*v2
              v3 = v( 3_ilp )
              t3 = tau*v3
              v4 = v( 4_ilp )
              t4 = tau*v4
              v5 = v( 5_ilp )
              t5 = tau*v5
              v6 = v( 6_ilp )
              t6 = tau*v6
              do j = 1, m
                 sum = v1*c( j, 1_ilp ) + v2*c( j, 2_ilp ) + v3*c( j, 3_ilp ) +v4*c( j, 4_ilp ) + v5*c( j, 5_ilp ) + &
                           v6*c( j, 6_ilp )
                 c( j, 1_ilp ) = c( j, 1_ilp ) - sum*t1
                 c( j, 2_ilp ) = c( j, 2_ilp ) - sum*t2
                 c( j, 3_ilp ) = c( j, 3_ilp ) - sum*t3
                 c( j, 4_ilp ) = c( j, 4_ilp ) - sum*t4
                 c( j, 5_ilp ) = c( j, 5_ilp ) - sum*t5
                 c( j, 6_ilp ) = c( j, 6_ilp ) - sum*t6
              end do
              go to 410
              330 continue
              ! special code for 7 x 7 householder
              v1 = v( 1_ilp )
              t1 = tau*v1
              v2 = v( 2_ilp )
              t2 = tau*v2
              v3 = v( 3_ilp )
              t3 = tau*v3
              v4 = v( 4_ilp )
              t4 = tau*v4
              v5 = v( 5_ilp )
              t5 = tau*v5
              v6 = v( 6_ilp )
              t6 = tau*v6
              v7 = v( 7_ilp )
              t7 = tau*v7
              do j = 1, m
                 sum = v1*c( j, 1_ilp ) + v2*c( j, 2_ilp ) + v3*c( j, 3_ilp ) +v4*c( j, 4_ilp ) + v5*c( j, 5_ilp ) + &
                           v6*c( j, 6_ilp ) +v7*c( j, 7_ilp )
                 c( j, 1_ilp ) = c( j, 1_ilp ) - sum*t1
                 c( j, 2_ilp ) = c( j, 2_ilp ) - sum*t2
                 c( j, 3_ilp ) = c( j, 3_ilp ) - sum*t3
                 c( j, 4_ilp ) = c( j, 4_ilp ) - sum*t4
                 c( j, 5_ilp ) = c( j, 5_ilp ) - sum*t5
                 c( j, 6_ilp ) = c( j, 6_ilp ) - sum*t6
                 c( j, 7_ilp ) = c( j, 7_ilp ) - sum*t7
              end do
              go to 410
              350 continue
              ! special code for 8 x 8 householder
              v1 = v( 1_ilp )
              t1 = tau*v1
              v2 = v( 2_ilp )
              t2 = tau*v2
              v3 = v( 3_ilp )
              t3 = tau*v3
              v4 = v( 4_ilp )
              t4 = tau*v4
              v5 = v( 5_ilp )
              t5 = tau*v5
              v6 = v( 6_ilp )
              t6 = tau*v6
              v7 = v( 7_ilp )
              t7 = tau*v7
              v8 = v( 8_ilp )
              t8 = tau*v8
              do j = 1, m
                 sum = v1*c( j, 1_ilp ) + v2*c( j, 2_ilp ) + v3*c( j, 3_ilp ) +v4*c( j, 4_ilp ) + v5*c( j, 5_ilp ) + &
                           v6*c( j, 6_ilp ) +v7*c( j, 7_ilp ) + v8*c( j, 8_ilp )
                 c( j, 1_ilp ) = c( j, 1_ilp ) - sum*t1
                 c( j, 2_ilp ) = c( j, 2_ilp ) - sum*t2
                 c( j, 3_ilp ) = c( j, 3_ilp ) - sum*t3
                 c( j, 4_ilp ) = c( j, 4_ilp ) - sum*t4
                 c( j, 5_ilp ) = c( j, 5_ilp ) - sum*t5
                 c( j, 6_ilp ) = c( j, 6_ilp ) - sum*t6
                 c( j, 7_ilp ) = c( j, 7_ilp ) - sum*t7
                 c( j, 8_ilp ) = c( j, 8_ilp ) - sum*t8
              end do
              go to 410
              370 continue
              ! special code for 9 x 9 householder
              v1 = v( 1_ilp )
              t1 = tau*v1
              v2 = v( 2_ilp )
              t2 = tau*v2
              v3 = v( 3_ilp )
              t3 = tau*v3
              v4 = v( 4_ilp )
              t4 = tau*v4
              v5 = v( 5_ilp )
              t5 = tau*v5
              v6 = v( 6_ilp )
              t6 = tau*v6
              v7 = v( 7_ilp )
              t7 = tau*v7
              v8 = v( 8_ilp )
              t8 = tau*v8
              v9 = v( 9_ilp )
              t9 = tau*v9
              do j = 1, m
                 sum = v1*c( j, 1_ilp ) + v2*c( j, 2_ilp ) + v3*c( j, 3_ilp ) +v4*c( j, 4_ilp ) + v5*c( j, 5_ilp ) + &
                           v6*c( j, 6_ilp ) +v7*c( j, 7_ilp ) + v8*c( j, 8_ilp ) + v9*c( j, 9_ilp )
                 c( j, 1_ilp ) = c( j, 1_ilp ) - sum*t1
                 c( j, 2_ilp ) = c( j, 2_ilp ) - sum*t2
                 c( j, 3_ilp ) = c( j, 3_ilp ) - sum*t3
                 c( j, 4_ilp ) = c( j, 4_ilp ) - sum*t4
                 c( j, 5_ilp ) = c( j, 5_ilp ) - sum*t5
                 c( j, 6_ilp ) = c( j, 6_ilp ) - sum*t6
                 c( j, 7_ilp ) = c( j, 7_ilp ) - sum*t7
                 c( j, 8_ilp ) = c( j, 8_ilp ) - sum*t8
                 c( j, 9_ilp ) = c( j, 9_ilp ) - sum*t9
              end do
              go to 410
              390 continue
              ! special code for 10 x 10 householder
              v1 = v( 1_ilp )
              t1 = tau*v1
              v2 = v( 2_ilp )
              t2 = tau*v2
              v3 = v( 3_ilp )
              t3 = tau*v3
              v4 = v( 4_ilp )
              t4 = tau*v4
              v5 = v( 5_ilp )
              t5 = tau*v5
              v6 = v( 6_ilp )
              t6 = tau*v6
              v7 = v( 7_ilp )
              t7 = tau*v7
              v8 = v( 8_ilp )
              t8 = tau*v8
              v9 = v( 9_ilp )
              t9 = tau*v9
              v10 = v( 10_ilp )
              t10 = tau*v10
              do j = 1, m
                 sum = v1*c( j, 1_ilp ) + v2*c( j, 2_ilp ) + v3*c( j, 3_ilp ) +v4*c( j, 4_ilp ) + v5*c( j, 5_ilp ) + &
                           v6*c( j, 6_ilp ) +v7*c( j, 7_ilp ) + v8*c( j, 8_ilp ) + v9*c( j, 9_ilp ) +v10*c( j, 10_ilp )
                 c( j, 1_ilp ) = c( j, 1_ilp ) - sum*t1
                 c( j, 2_ilp ) = c( j, 2_ilp ) - sum*t2
                 c( j, 3_ilp ) = c( j, 3_ilp ) - sum*t3
                 c( j, 4_ilp ) = c( j, 4_ilp ) - sum*t4
                 c( j, 5_ilp ) = c( j, 5_ilp ) - sum*t5
                 c( j, 6_ilp ) = c( j, 6_ilp ) - sum*t6
                 c( j, 7_ilp ) = c( j, 7_ilp ) - sum*t7
                 c( j, 8_ilp ) = c( j, 8_ilp ) - sum*t8
                 c( j, 9_ilp ) = c( j, 9_ilp ) - sum*t9
                 c( j, 10_ilp ) = c( j, 10_ilp ) - sum*t10
              end do
              go to 410
           end if
           410 continue
           return
     end subroutine stdlib_dlarfx


     pure module subroutine stdlib_clarfx( side, m, n, v, tau, c, ldc, work )
     !! CLARFX applies a complex elementary reflector H to a complex m by n
     !! matrix C, from either the left or the right. H is represented in the
     !! form
     !! H = I - tau * v * v**H
     !! where tau is a complex scalar and v is a complex vector.
     !! If tau = 0, then H is taken to be the unit matrix
     !! This version uses inline code if H has order < 11.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side
           integer(ilp), intent(in) :: ldc, m, n
           complex(sp), intent(in) :: tau
           ! Array Arguments 
           complex(sp), intent(inout) :: c(ldc,*)
           complex(sp), intent(in) :: v(*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: j
           complex(sp) :: sum, t1, t10, t2, t3, t4, t5, t6, t7, t8, t9, v1, v10, v2, v3, v4, v5, &
                     v6, v7, v8, v9
           ! Intrinsic Functions 
           ! Executable Statements 
           if( tau==czero )return
           if( stdlib_lsame( side, 'L' ) ) then
              ! form  h * c, where h has order m.
              go to ( 10, 30, 50, 70, 90, 110, 130, 150,170, 190 )m
              ! code for general m
              call stdlib_clarf( side, m, n, v, 1_ilp, tau, c, ldc, work )
              go to 410
              10 continue
              ! special code for 1 x 1 householder
              t1 = cone - tau*v( 1_ilp )*conjg( v( 1_ilp ) )
              do j = 1, n
                 c( 1_ilp, j ) = t1*c( 1_ilp, j )
              end do
              go to 410
              30 continue
              ! special code for 2 x 2 householder
              v1 = conjg( v( 1_ilp ) )
              t1 = tau*conjg( v1 )
              v2 = conjg( v( 2_ilp ) )
              t2 = tau*conjg( v2 )
              do j = 1, n
                 sum = v1*c( 1_ilp, j ) + v2*c( 2_ilp, j )
                 c( 1_ilp, j ) = c( 1_ilp, j ) - sum*t1
                 c( 2_ilp, j ) = c( 2_ilp, j ) - sum*t2
              end do
              go to 410
              50 continue
              ! special code for 3 x 3 householder
              v1 = conjg( v( 1_ilp ) )
              t1 = tau*conjg( v1 )
              v2 = conjg( v( 2_ilp ) )
              t2 = tau*conjg( v2 )
              v3 = conjg( v( 3_ilp ) )
              t3 = tau*conjg( v3 )
              do j = 1, n
                 sum = v1*c( 1_ilp, j ) + v2*c( 2_ilp, j ) + v3*c( 3_ilp, j )
                 c( 1_ilp, j ) = c( 1_ilp, j ) - sum*t1
                 c( 2_ilp, j ) = c( 2_ilp, j ) - sum*t2
                 c( 3_ilp, j ) = c( 3_ilp, j ) - sum*t3
              end do
              go to 410
              70 continue
              ! special code for 4 x 4 householder
              v1 = conjg( v( 1_ilp ) )
              t1 = tau*conjg( v1 )
              v2 = conjg( v( 2_ilp ) )
              t2 = tau*conjg( v2 )
              v3 = conjg( v( 3_ilp ) )
              t3 = tau*conjg( v3 )
              v4 = conjg( v( 4_ilp ) )
              t4 = tau*conjg( v4 )
              do j = 1, n
                 sum = v1*c( 1_ilp, j ) + v2*c( 2_ilp, j ) + v3*c( 3_ilp, j ) +v4*c( 4_ilp, j )
                 c( 1_ilp, j ) = c( 1_ilp, j ) - sum*t1
                 c( 2_ilp, j ) = c( 2_ilp, j ) - sum*t2
                 c( 3_ilp, j ) = c( 3_ilp, j ) - sum*t3
                 c( 4_ilp, j ) = c( 4_ilp, j ) - sum*t4
              end do
              go to 410
              90 continue
              ! special code for 5 x 5 householder
              v1 = conjg( v( 1_ilp ) )
              t1 = tau*conjg( v1 )
              v2 = conjg( v( 2_ilp ) )
              t2 = tau*conjg( v2 )
              v3 = conjg( v( 3_ilp ) )
              t3 = tau*conjg( v3 )
              v4 = conjg( v( 4_ilp ) )
              t4 = tau*conjg( v4 )
              v5 = conjg( v( 5_ilp ) )
              t5 = tau*conjg( v5 )
              do j = 1, n
                 sum = v1*c( 1_ilp, j ) + v2*c( 2_ilp, j ) + v3*c( 3_ilp, j ) +v4*c( 4_ilp, j ) + v5*c( 5_ilp, j )
                           
                 c( 1_ilp, j ) = c( 1_ilp, j ) - sum*t1
                 c( 2_ilp, j ) = c( 2_ilp, j ) - sum*t2
                 c( 3_ilp, j ) = c( 3_ilp, j ) - sum*t3
                 c( 4_ilp, j ) = c( 4_ilp, j ) - sum*t4
                 c( 5_ilp, j ) = c( 5_ilp, j ) - sum*t5
              end do
              go to 410
              110 continue
              ! special code for 6 x 6 householder
              v1 = conjg( v( 1_ilp ) )
              t1 = tau*conjg( v1 )
              v2 = conjg( v( 2_ilp ) )
              t2 = tau*conjg( v2 )
              v3 = conjg( v( 3_ilp ) )
              t3 = tau*conjg( v3 )
              v4 = conjg( v( 4_ilp ) )
              t4 = tau*conjg( v4 )
              v5 = conjg( v( 5_ilp ) )
              t5 = tau*conjg( v5 )
              v6 = conjg( v( 6_ilp ) )
              t6 = tau*conjg( v6 )
              do j = 1, n
                 sum = v1*c( 1_ilp, j ) + v2*c( 2_ilp, j ) + v3*c( 3_ilp, j ) +v4*c( 4_ilp, j ) + v5*c( 5_ilp, j ) + &
                           v6*c( 6_ilp, j )
                 c( 1_ilp, j ) = c( 1_ilp, j ) - sum*t1
                 c( 2_ilp, j ) = c( 2_ilp, j ) - sum*t2
                 c( 3_ilp, j ) = c( 3_ilp, j ) - sum*t3
                 c( 4_ilp, j ) = c( 4_ilp, j ) - sum*t4
                 c( 5_ilp, j ) = c( 5_ilp, j ) - sum*t5
                 c( 6_ilp, j ) = c( 6_ilp, j ) - sum*t6
              end do
              go to 410
              130 continue
              ! special code for 7 x 7 householder
              v1 = conjg( v( 1_ilp ) )
              t1 = tau*conjg( v1 )
              v2 = conjg( v( 2_ilp ) )
              t2 = tau*conjg( v2 )
              v3 = conjg( v( 3_ilp ) )
              t3 = tau*conjg( v3 )
              v4 = conjg( v( 4_ilp ) )
              t4 = tau*conjg( v4 )
              v5 = conjg( v( 5_ilp ) )
              t5 = tau*conjg( v5 )
              v6 = conjg( v( 6_ilp ) )
              t6 = tau*conjg( v6 )
              v7 = conjg( v( 7_ilp ) )
              t7 = tau*conjg( v7 )
              do j = 1, n
                 sum = v1*c( 1_ilp, j ) + v2*c( 2_ilp, j ) + v3*c( 3_ilp, j ) +v4*c( 4_ilp, j ) + v5*c( 5_ilp, j ) + &
                           v6*c( 6_ilp, j ) +v7*c( 7_ilp, j )
                 c( 1_ilp, j ) = c( 1_ilp, j ) - sum*t1
                 c( 2_ilp, j ) = c( 2_ilp, j ) - sum*t2
                 c( 3_ilp, j ) = c( 3_ilp, j ) - sum*t3
                 c( 4_ilp, j ) = c( 4_ilp, j ) - sum*t4
                 c( 5_ilp, j ) = c( 5_ilp, j ) - sum*t5
                 c( 6_ilp, j ) = c( 6_ilp, j ) - sum*t6
                 c( 7_ilp, j ) = c( 7_ilp, j ) - sum*t7
              end do
              go to 410
              150 continue
              ! special code for 8 x 8 householder
              v1 = conjg( v( 1_ilp ) )
              t1 = tau*conjg( v1 )
              v2 = conjg( v( 2_ilp ) )
              t2 = tau*conjg( v2 )
              v3 = conjg( v( 3_ilp ) )
              t3 = tau*conjg( v3 )
              v4 = conjg( v( 4_ilp ) )
              t4 = tau*conjg( v4 )
              v5 = conjg( v( 5_ilp ) )
              t5 = tau*conjg( v5 )
              v6 = conjg( v( 6_ilp ) )
              t6 = tau*conjg( v6 )
              v7 = conjg( v( 7_ilp ) )
              t7 = tau*conjg( v7 )
              v8 = conjg( v( 8_ilp ) )
              t8 = tau*conjg( v8 )
              do j = 1, n
                 sum = v1*c( 1_ilp, j ) + v2*c( 2_ilp, j ) + v3*c( 3_ilp, j ) +v4*c( 4_ilp, j ) + v5*c( 5_ilp, j ) + &
                           v6*c( 6_ilp, j ) +v7*c( 7_ilp, j ) + v8*c( 8_ilp, j )
                 c( 1_ilp, j ) = c( 1_ilp, j ) - sum*t1
                 c( 2_ilp, j ) = c( 2_ilp, j ) - sum*t2
                 c( 3_ilp, j ) = c( 3_ilp, j ) - sum*t3
                 c( 4_ilp, j ) = c( 4_ilp, j ) - sum*t4
                 c( 5_ilp, j ) = c( 5_ilp, j ) - sum*t5
                 c( 6_ilp, j ) = c( 6_ilp, j ) - sum*t6
                 c( 7_ilp, j ) = c( 7_ilp, j ) - sum*t7
                 c( 8_ilp, j ) = c( 8_ilp, j ) - sum*t8
              end do
              go to 410
              170 continue
              ! special code for 9 x 9 householder
              v1 = conjg( v( 1_ilp ) )
              t1 = tau*conjg( v1 )
              v2 = conjg( v( 2_ilp ) )
              t2 = tau*conjg( v2 )
              v3 = conjg( v( 3_ilp ) )
              t3 = tau*conjg( v3 )
              v4 = conjg( v( 4_ilp ) )
              t4 = tau*conjg( v4 )
              v5 = conjg( v( 5_ilp ) )
              t5 = tau*conjg( v5 )
              v6 = conjg( v( 6_ilp ) )
              t6 = tau*conjg( v6 )
              v7 = conjg( v( 7_ilp ) )
              t7 = tau*conjg( v7 )
              v8 = conjg( v( 8_ilp ) )
              t8 = tau*conjg( v8 )
              v9 = conjg( v( 9_ilp ) )
              t9 = tau*conjg( v9 )
              do j = 1, n
                 sum = v1*c( 1_ilp, j ) + v2*c( 2_ilp, j ) + v3*c( 3_ilp, j ) +v4*c( 4_ilp, j ) + v5*c( 5_ilp, j ) + &
                           v6*c( 6_ilp, j ) +v7*c( 7_ilp, j ) + v8*c( 8_ilp, j ) + v9*c( 9_ilp, j )
                 c( 1_ilp, j ) = c( 1_ilp, j ) - sum*t1
                 c( 2_ilp, j ) = c( 2_ilp, j ) - sum*t2
                 c( 3_ilp, j ) = c( 3_ilp, j ) - sum*t3
                 c( 4_ilp, j ) = c( 4_ilp, j ) - sum*t4
                 c( 5_ilp, j ) = c( 5_ilp, j ) - sum*t5
                 c( 6_ilp, j ) = c( 6_ilp, j ) - sum*t6
                 c( 7_ilp, j ) = c( 7_ilp, j ) - sum*t7
                 c( 8_ilp, j ) = c( 8_ilp, j ) - sum*t8
                 c( 9_ilp, j ) = c( 9_ilp, j ) - sum*t9
              end do
              go to 410
              190 continue
              ! special code for 10 x 10 householder
              v1 = conjg( v( 1_ilp ) )
              t1 = tau*conjg( v1 )
              v2 = conjg( v( 2_ilp ) )
              t2 = tau*conjg( v2 )
              v3 = conjg( v( 3_ilp ) )
              t3 = tau*conjg( v3 )
              v4 = conjg( v( 4_ilp ) )
              t4 = tau*conjg( v4 )
              v5 = conjg( v( 5_ilp ) )
              t5 = tau*conjg( v5 )
              v6 = conjg( v( 6_ilp ) )
              t6 = tau*conjg( v6 )
              v7 = conjg( v( 7_ilp ) )
              t7 = tau*conjg( v7 )
              v8 = conjg( v( 8_ilp ) )
              t8 = tau*conjg( v8 )
              v9 = conjg( v( 9_ilp ) )
              t9 = tau*conjg( v9 )
              v10 = conjg( v( 10_ilp ) )
              t10 = tau*conjg( v10 )
              do j = 1, n
                 sum = v1*c( 1_ilp, j ) + v2*c( 2_ilp, j ) + v3*c( 3_ilp, j ) +v4*c( 4_ilp, j ) + v5*c( 5_ilp, j ) + &
                           v6*c( 6_ilp, j ) +v7*c( 7_ilp, j ) + v8*c( 8_ilp, j ) + v9*c( 9_ilp, j ) +v10*c( 10_ilp, j )
                 c( 1_ilp, j ) = c( 1_ilp, j ) - sum*t1
                 c( 2_ilp, j ) = c( 2_ilp, j ) - sum*t2
                 c( 3_ilp, j ) = c( 3_ilp, j ) - sum*t3
                 c( 4_ilp, j ) = c( 4_ilp, j ) - sum*t4
                 c( 5_ilp, j ) = c( 5_ilp, j ) - sum*t5
                 c( 6_ilp, j ) = c( 6_ilp, j ) - sum*t6
                 c( 7_ilp, j ) = c( 7_ilp, j ) - sum*t7
                 c( 8_ilp, j ) = c( 8_ilp, j ) - sum*t8
                 c( 9_ilp, j ) = c( 9_ilp, j ) - sum*t9
                 c( 10_ilp, j ) = c( 10_ilp, j ) - sum*t10
              end do
              go to 410
           else
              ! form  c * h, where h has order n.
              go to ( 210, 230, 250, 270, 290, 310, 330, 350,370, 390 )n
              ! code for general n
              call stdlib_clarf( side, m, n, v, 1_ilp, tau, c, ldc, work )
              go to 410
              210 continue
              ! special code for 1 x 1 householder
              t1 = cone - tau*v( 1_ilp )*conjg( v( 1_ilp ) )
              do j = 1, m
                 c( j, 1_ilp ) = t1*c( j, 1_ilp )
              end do
              go to 410
              230 continue
              ! special code for 2 x 2 householder
              v1 = v( 1_ilp )
              t1 = tau*conjg( v1 )
              v2 = v( 2_ilp )
              t2 = tau*conjg( v2 )
              do j = 1, m
                 sum = v1*c( j, 1_ilp ) + v2*c( j, 2_ilp )
                 c( j, 1_ilp ) = c( j, 1_ilp ) - sum*t1
                 c( j, 2_ilp ) = c( j, 2_ilp ) - sum*t2
              end do
              go to 410
              250 continue
              ! special code for 3 x 3 householder
              v1 = v( 1_ilp )
              t1 = tau*conjg( v1 )
              v2 = v( 2_ilp )
              t2 = tau*conjg( v2 )
              v3 = v( 3_ilp )
              t3 = tau*conjg( v3 )
              do j = 1, m
                 sum = v1*c( j, 1_ilp ) + v2*c( j, 2_ilp ) + v3*c( j, 3_ilp )
                 c( j, 1_ilp ) = c( j, 1_ilp ) - sum*t1
                 c( j, 2_ilp ) = c( j, 2_ilp ) - sum*t2
                 c( j, 3_ilp ) = c( j, 3_ilp ) - sum*t3
              end do
              go to 410
              270 continue
              ! special code for 4 x 4 householder
              v1 = v( 1_ilp )
              t1 = tau*conjg( v1 )
              v2 = v( 2_ilp )
              t2 = tau*conjg( v2 )
              v3 = v( 3_ilp )
              t3 = tau*conjg( v3 )
              v4 = v( 4_ilp )
              t4 = tau*conjg( v4 )
              do j = 1, m
                 sum = v1*c( j, 1_ilp ) + v2*c( j, 2_ilp ) + v3*c( j, 3_ilp ) +v4*c( j, 4_ilp )
                 c( j, 1_ilp ) = c( j, 1_ilp ) - sum*t1
                 c( j, 2_ilp ) = c( j, 2_ilp ) - sum*t2
                 c( j, 3_ilp ) = c( j, 3_ilp ) - sum*t3
                 c( j, 4_ilp ) = c( j, 4_ilp ) - sum*t4
              end do
              go to 410
              290 continue
              ! special code for 5 x 5 householder
              v1 = v( 1_ilp )
              t1 = tau*conjg( v1 )
              v2 = v( 2_ilp )
              t2 = tau*conjg( v2 )
              v3 = v( 3_ilp )
              t3 = tau*conjg( v3 )
              v4 = v( 4_ilp )
              t4 = tau*conjg( v4 )
              v5 = v( 5_ilp )
              t5 = tau*conjg( v5 )
              do j = 1, m
                 sum = v1*c( j, 1_ilp ) + v2*c( j, 2_ilp ) + v3*c( j, 3_ilp ) +v4*c( j, 4_ilp ) + v5*c( j, 5_ilp )
                           
                 c( j, 1_ilp ) = c( j, 1_ilp ) - sum*t1
                 c( j, 2_ilp ) = c( j, 2_ilp ) - sum*t2
                 c( j, 3_ilp ) = c( j, 3_ilp ) - sum*t3
                 c( j, 4_ilp ) = c( j, 4_ilp ) - sum*t4
                 c( j, 5_ilp ) = c( j, 5_ilp ) - sum*t5
              end do
              go to 410
              310 continue
              ! special code for 6 x 6 householder
              v1 = v( 1_ilp )
              t1 = tau*conjg( v1 )
              v2 = v( 2_ilp )
              t2 = tau*conjg( v2 )
              v3 = v( 3_ilp )
              t3 = tau*conjg( v3 )
              v4 = v( 4_ilp )
              t4 = tau*conjg( v4 )
              v5 = v( 5_ilp )
              t5 = tau*conjg( v5 )
              v6 = v( 6_ilp )
              t6 = tau*conjg( v6 )
              do j = 1, m
                 sum = v1*c( j, 1_ilp ) + v2*c( j, 2_ilp ) + v3*c( j, 3_ilp ) +v4*c( j, 4_ilp ) + v5*c( j, 5_ilp ) + &
                           v6*c( j, 6_ilp )
                 c( j, 1_ilp ) = c( j, 1_ilp ) - sum*t1
                 c( j, 2_ilp ) = c( j, 2_ilp ) - sum*t2
                 c( j, 3_ilp ) = c( j, 3_ilp ) - sum*t3
                 c( j, 4_ilp ) = c( j, 4_ilp ) - sum*t4
                 c( j, 5_ilp ) = c( j, 5_ilp ) - sum*t5
                 c( j, 6_ilp ) = c( j, 6_ilp ) - sum*t6
              end do
              go to 410
              330 continue
              ! special code for 7 x 7 householder
              v1 = v( 1_ilp )
              t1 = tau*conjg( v1 )
              v2 = v( 2_ilp )
              t2 = tau*conjg( v2 )
              v3 = v( 3_ilp )
              t3 = tau*conjg( v3 )
              v4 = v( 4_ilp )
              t4 = tau*conjg( v4 )
              v5 = v( 5_ilp )
              t5 = tau*conjg( v5 )
              v6 = v( 6_ilp )
              t6 = tau*conjg( v6 )
              v7 = v( 7_ilp )
              t7 = tau*conjg( v7 )
              do j = 1, m
                 sum = v1*c( j, 1_ilp ) + v2*c( j, 2_ilp ) + v3*c( j, 3_ilp ) +v4*c( j, 4_ilp ) + v5*c( j, 5_ilp ) + &
                           v6*c( j, 6_ilp ) +v7*c( j, 7_ilp )
                 c( j, 1_ilp ) = c( j, 1_ilp ) - sum*t1
                 c( j, 2_ilp ) = c( j, 2_ilp ) - sum*t2
                 c( j, 3_ilp ) = c( j, 3_ilp ) - sum*t3
                 c( j, 4_ilp ) = c( j, 4_ilp ) - sum*t4
                 c( j, 5_ilp ) = c( j, 5_ilp ) - sum*t5
                 c( j, 6_ilp ) = c( j, 6_ilp ) - sum*t6
                 c( j, 7_ilp ) = c( j, 7_ilp ) - sum*t7
              end do
              go to 410
              350 continue
              ! special code for 8 x 8 householder
              v1 = v( 1_ilp )
              t1 = tau*conjg( v1 )
              v2 = v( 2_ilp )
              t2 = tau*conjg( v2 )
              v3 = v( 3_ilp )
              t3 = tau*conjg( v3 )
              v4 = v( 4_ilp )
              t4 = tau*conjg( v4 )
              v5 = v( 5_ilp )
              t5 = tau*conjg( v5 )
              v6 = v( 6_ilp )
              t6 = tau*conjg( v6 )
              v7 = v( 7_ilp )
              t7 = tau*conjg( v7 )
              v8 = v( 8_ilp )
              t8 = tau*conjg( v8 )
              do j = 1, m
                 sum = v1*c( j, 1_ilp ) + v2*c( j, 2_ilp ) + v3*c( j, 3_ilp ) +v4*c( j, 4_ilp ) + v5*c( j, 5_ilp ) + &
                           v6*c( j, 6_ilp ) +v7*c( j, 7_ilp ) + v8*c( j, 8_ilp )
                 c( j, 1_ilp ) = c( j, 1_ilp ) - sum*t1
                 c( j, 2_ilp ) = c( j, 2_ilp ) - sum*t2
                 c( j, 3_ilp ) = c( j, 3_ilp ) - sum*t3
                 c( j, 4_ilp ) = c( j, 4_ilp ) - sum*t4
                 c( j, 5_ilp ) = c( j, 5_ilp ) - sum*t5
                 c( j, 6_ilp ) = c( j, 6_ilp ) - sum*t6
                 c( j, 7_ilp ) = c( j, 7_ilp ) - sum*t7
                 c( j, 8_ilp ) = c( j, 8_ilp ) - sum*t8
              end do
              go to 410
              370 continue
              ! special code for 9 x 9 householder
              v1 = v( 1_ilp )
              t1 = tau*conjg( v1 )
              v2 = v( 2_ilp )
              t2 = tau*conjg( v2 )
              v3 = v( 3_ilp )
              t3 = tau*conjg( v3 )
              v4 = v( 4_ilp )
              t4 = tau*conjg( v4 )
              v5 = v( 5_ilp )
              t5 = tau*conjg( v5 )
              v6 = v( 6_ilp )
              t6 = tau*conjg( v6 )
              v7 = v( 7_ilp )
              t7 = tau*conjg( v7 )
              v8 = v( 8_ilp )
              t8 = tau*conjg( v8 )
              v9 = v( 9_ilp )
              t9 = tau*conjg( v9 )
              do j = 1, m
                 sum = v1*c( j, 1_ilp ) + v2*c( j, 2_ilp ) + v3*c( j, 3_ilp ) +v4*c( j, 4_ilp ) + v5*c( j, 5_ilp ) + &
                           v6*c( j, 6_ilp ) +v7*c( j, 7_ilp ) + v8*c( j, 8_ilp ) + v9*c( j, 9_ilp )
                 c( j, 1_ilp ) = c( j, 1_ilp ) - sum*t1
                 c( j, 2_ilp ) = c( j, 2_ilp ) - sum*t2
                 c( j, 3_ilp ) = c( j, 3_ilp ) - sum*t3
                 c( j, 4_ilp ) = c( j, 4_ilp ) - sum*t4
                 c( j, 5_ilp ) = c( j, 5_ilp ) - sum*t5
                 c( j, 6_ilp ) = c( j, 6_ilp ) - sum*t6
                 c( j, 7_ilp ) = c( j, 7_ilp ) - sum*t7
                 c( j, 8_ilp ) = c( j, 8_ilp ) - sum*t8
                 c( j, 9_ilp ) = c( j, 9_ilp ) - sum*t9
              end do
              go to 410
              390 continue
              ! special code for 10 x 10 householder
              v1 = v( 1_ilp )
              t1 = tau*conjg( v1 )
              v2 = v( 2_ilp )
              t2 = tau*conjg( v2 )
              v3 = v( 3_ilp )
              t3 = tau*conjg( v3 )
              v4 = v( 4_ilp )
              t4 = tau*conjg( v4 )
              v5 = v( 5_ilp )
              t5 = tau*conjg( v5 )
              v6 = v( 6_ilp )
              t6 = tau*conjg( v6 )
              v7 = v( 7_ilp )
              t7 = tau*conjg( v7 )
              v8 = v( 8_ilp )
              t8 = tau*conjg( v8 )
              v9 = v( 9_ilp )
              t9 = tau*conjg( v9 )
              v10 = v( 10_ilp )
              t10 = tau*conjg( v10 )
              do j = 1, m
                 sum = v1*c( j, 1_ilp ) + v2*c( j, 2_ilp ) + v3*c( j, 3_ilp ) +v4*c( j, 4_ilp ) + v5*c( j, 5_ilp ) + &
                           v6*c( j, 6_ilp ) +v7*c( j, 7_ilp ) + v8*c( j, 8_ilp ) + v9*c( j, 9_ilp ) +v10*c( j, 10_ilp )
                 c( j, 1_ilp ) = c( j, 1_ilp ) - sum*t1
                 c( j, 2_ilp ) = c( j, 2_ilp ) - sum*t2
                 c( j, 3_ilp ) = c( j, 3_ilp ) - sum*t3
                 c( j, 4_ilp ) = c( j, 4_ilp ) - sum*t4
                 c( j, 5_ilp ) = c( j, 5_ilp ) - sum*t5
                 c( j, 6_ilp ) = c( j, 6_ilp ) - sum*t6
                 c( j, 7_ilp ) = c( j, 7_ilp ) - sum*t7
                 c( j, 8_ilp ) = c( j, 8_ilp ) - sum*t8
                 c( j, 9_ilp ) = c( j, 9_ilp ) - sum*t9
                 c( j, 10_ilp ) = c( j, 10_ilp ) - sum*t10
              end do
              go to 410
           end if
       410 return
     end subroutine stdlib_clarfx

     pure module subroutine stdlib_zlarfx( side, m, n, v, tau, c, ldc, work )
     !! ZLARFX applies a complex elementary reflector H to a complex m by n
     !! matrix C, from either the left or the right. H is represented in the
     !! form
     !! H = I - tau * v * v**H
     !! where tau is a complex scalar and v is a complex vector.
     !! If tau = 0, then H is taken to be the unit matrix
     !! This version uses inline code if H has order < 11.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side
           integer(ilp), intent(in) :: ldc, m, n
           complex(dp), intent(in) :: tau
           ! Array Arguments 
           complex(dp), intent(inout) :: c(ldc,*)
           complex(dp), intent(in) :: v(*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: j
           complex(dp) :: sum, t1, t10, t2, t3, t4, t5, t6, t7, t8, t9, v1, v10, v2, v3, v4, v5, &
                     v6, v7, v8, v9
           ! Intrinsic Functions 
           ! Executable Statements 
           if( tau==czero )return
           if( stdlib_lsame( side, 'L' ) ) then
              ! form  h * c, where h has order m.
              go to ( 10, 30, 50, 70, 90, 110, 130, 150,170, 190 )m
              ! code for general m
              call stdlib_zlarf( side, m, n, v, 1_ilp, tau, c, ldc, work )
              go to 410
              10 continue
              ! special code for 1 x 1 householder
              t1 = cone - tau*v( 1_ilp )*conjg( v( 1_ilp ) )
              do j = 1, n
                 c( 1_ilp, j ) = t1*c( 1_ilp, j )
              end do
              go to 410
              30 continue
              ! special code for 2 x 2 householder
              v1 = conjg( v( 1_ilp ) )
              t1 = tau*conjg( v1 )
              v2 = conjg( v( 2_ilp ) )
              t2 = tau*conjg( v2 )
              do j = 1, n
                 sum = v1*c( 1_ilp, j ) + v2*c( 2_ilp, j )
                 c( 1_ilp, j ) = c( 1_ilp, j ) - sum*t1
                 c( 2_ilp, j ) = c( 2_ilp, j ) - sum*t2
              end do
              go to 410
              50 continue
              ! special code for 3 x 3 householder
              v1 = conjg( v( 1_ilp ) )
              t1 = tau*conjg( v1 )
              v2 = conjg( v( 2_ilp ) )
              t2 = tau*conjg( v2 )
              v3 = conjg( v( 3_ilp ) )
              t3 = tau*conjg( v3 )
              do j = 1, n
                 sum = v1*c( 1_ilp, j ) + v2*c( 2_ilp, j ) + v3*c( 3_ilp, j )
                 c( 1_ilp, j ) = c( 1_ilp, j ) - sum*t1
                 c( 2_ilp, j ) = c( 2_ilp, j ) - sum*t2
                 c( 3_ilp, j ) = c( 3_ilp, j ) - sum*t3
              end do
              go to 410
              70 continue
              ! special code for 4 x 4 householder
              v1 = conjg( v( 1_ilp ) )
              t1 = tau*conjg( v1 )
              v2 = conjg( v( 2_ilp ) )
              t2 = tau*conjg( v2 )
              v3 = conjg( v( 3_ilp ) )
              t3 = tau*conjg( v3 )
              v4 = conjg( v( 4_ilp ) )
              t4 = tau*conjg( v4 )
              do j = 1, n
                 sum = v1*c( 1_ilp, j ) + v2*c( 2_ilp, j ) + v3*c( 3_ilp, j ) +v4*c( 4_ilp, j )
                 c( 1_ilp, j ) = c( 1_ilp, j ) - sum*t1
                 c( 2_ilp, j ) = c( 2_ilp, j ) - sum*t2
                 c( 3_ilp, j ) = c( 3_ilp, j ) - sum*t3
                 c( 4_ilp, j ) = c( 4_ilp, j ) - sum*t4
              end do
              go to 410
              90 continue
              ! special code for 5 x 5 householder
              v1 = conjg( v( 1_ilp ) )
              t1 = tau*conjg( v1 )
              v2 = conjg( v( 2_ilp ) )
              t2 = tau*conjg( v2 )
              v3 = conjg( v( 3_ilp ) )
              t3 = tau*conjg( v3 )
              v4 = conjg( v( 4_ilp ) )
              t4 = tau*conjg( v4 )
              v5 = conjg( v( 5_ilp ) )
              t5 = tau*conjg( v5 )
              do j = 1, n
                 sum = v1*c( 1_ilp, j ) + v2*c( 2_ilp, j ) + v3*c( 3_ilp, j ) +v4*c( 4_ilp, j ) + v5*c( 5_ilp, j )
                           
                 c( 1_ilp, j ) = c( 1_ilp, j ) - sum*t1
                 c( 2_ilp, j ) = c( 2_ilp, j ) - sum*t2
                 c( 3_ilp, j ) = c( 3_ilp, j ) - sum*t3
                 c( 4_ilp, j ) = c( 4_ilp, j ) - sum*t4
                 c( 5_ilp, j ) = c( 5_ilp, j ) - sum*t5
              end do
              go to 410
              110 continue
              ! special code for 6 x 6 householder
              v1 = conjg( v( 1_ilp ) )
              t1 = tau*conjg( v1 )
              v2 = conjg( v( 2_ilp ) )
              t2 = tau*conjg( v2 )
              v3 = conjg( v( 3_ilp ) )
              t3 = tau*conjg( v3 )
              v4 = conjg( v( 4_ilp ) )
              t4 = tau*conjg( v4 )
              v5 = conjg( v( 5_ilp ) )
              t5 = tau*conjg( v5 )
              v6 = conjg( v( 6_ilp ) )
              t6 = tau*conjg( v6 )
              do j = 1, n
                 sum = v1*c( 1_ilp, j ) + v2*c( 2_ilp, j ) + v3*c( 3_ilp, j ) +v4*c( 4_ilp, j ) + v5*c( 5_ilp, j ) + &
                           v6*c( 6_ilp, j )
                 c( 1_ilp, j ) = c( 1_ilp, j ) - sum*t1
                 c( 2_ilp, j ) = c( 2_ilp, j ) - sum*t2
                 c( 3_ilp, j ) = c( 3_ilp, j ) - sum*t3
                 c( 4_ilp, j ) = c( 4_ilp, j ) - sum*t4
                 c( 5_ilp, j ) = c( 5_ilp, j ) - sum*t5
                 c( 6_ilp, j ) = c( 6_ilp, j ) - sum*t6
              end do
              go to 410
              130 continue
              ! special code for 7 x 7 householder
              v1 = conjg( v( 1_ilp ) )
              t1 = tau*conjg( v1 )
              v2 = conjg( v( 2_ilp ) )
              t2 = tau*conjg( v2 )
              v3 = conjg( v( 3_ilp ) )
              t3 = tau*conjg( v3 )
              v4 = conjg( v( 4_ilp ) )
              t4 = tau*conjg( v4 )
              v5 = conjg( v( 5_ilp ) )
              t5 = tau*conjg( v5 )
              v6 = conjg( v( 6_ilp ) )
              t6 = tau*conjg( v6 )
              v7 = conjg( v( 7_ilp ) )
              t7 = tau*conjg( v7 )
              do j = 1, n
                 sum = v1*c( 1_ilp, j ) + v2*c( 2_ilp, j ) + v3*c( 3_ilp, j ) +v4*c( 4_ilp, j ) + v5*c( 5_ilp, j ) + &
                           v6*c( 6_ilp, j ) +v7*c( 7_ilp, j )
                 c( 1_ilp, j ) = c( 1_ilp, j ) - sum*t1
                 c( 2_ilp, j ) = c( 2_ilp, j ) - sum*t2
                 c( 3_ilp, j ) = c( 3_ilp, j ) - sum*t3
                 c( 4_ilp, j ) = c( 4_ilp, j ) - sum*t4
                 c( 5_ilp, j ) = c( 5_ilp, j ) - sum*t5
                 c( 6_ilp, j ) = c( 6_ilp, j ) - sum*t6
                 c( 7_ilp, j ) = c( 7_ilp, j ) - sum*t7
              end do
              go to 410
              150 continue
              ! special code for 8 x 8 householder
              v1 = conjg( v( 1_ilp ) )
              t1 = tau*conjg( v1 )
              v2 = conjg( v( 2_ilp ) )
              t2 = tau*conjg( v2 )
              v3 = conjg( v( 3_ilp ) )
              t3 = tau*conjg( v3 )
              v4 = conjg( v( 4_ilp ) )
              t4 = tau*conjg( v4 )
              v5 = conjg( v( 5_ilp ) )
              t5 = tau*conjg( v5 )
              v6 = conjg( v( 6_ilp ) )
              t6 = tau*conjg( v6 )
              v7 = conjg( v( 7_ilp ) )
              t7 = tau*conjg( v7 )
              v8 = conjg( v( 8_ilp ) )
              t8 = tau*conjg( v8 )
              do j = 1, n
                 sum = v1*c( 1_ilp, j ) + v2*c( 2_ilp, j ) + v3*c( 3_ilp, j ) +v4*c( 4_ilp, j ) + v5*c( 5_ilp, j ) + &
                           v6*c( 6_ilp, j ) +v7*c( 7_ilp, j ) + v8*c( 8_ilp, j )
                 c( 1_ilp, j ) = c( 1_ilp, j ) - sum*t1
                 c( 2_ilp, j ) = c( 2_ilp, j ) - sum*t2
                 c( 3_ilp, j ) = c( 3_ilp, j ) - sum*t3
                 c( 4_ilp, j ) = c( 4_ilp, j ) - sum*t4
                 c( 5_ilp, j ) = c( 5_ilp, j ) - sum*t5
                 c( 6_ilp, j ) = c( 6_ilp, j ) - sum*t6
                 c( 7_ilp, j ) = c( 7_ilp, j ) - sum*t7
                 c( 8_ilp, j ) = c( 8_ilp, j ) - sum*t8
              end do
              go to 410
              170 continue
              ! special code for 9 x 9 householder
              v1 = conjg( v( 1_ilp ) )
              t1 = tau*conjg( v1 )
              v2 = conjg( v( 2_ilp ) )
              t2 = tau*conjg( v2 )
              v3 = conjg( v( 3_ilp ) )
              t3 = tau*conjg( v3 )
              v4 = conjg( v( 4_ilp ) )
              t4 = tau*conjg( v4 )
              v5 = conjg( v( 5_ilp ) )
              t5 = tau*conjg( v5 )
              v6 = conjg( v( 6_ilp ) )
              t6 = tau*conjg( v6 )
              v7 = conjg( v( 7_ilp ) )
              t7 = tau*conjg( v7 )
              v8 = conjg( v( 8_ilp ) )
              t8 = tau*conjg( v8 )
              v9 = conjg( v( 9_ilp ) )
              t9 = tau*conjg( v9 )
              do j = 1, n
                 sum = v1*c( 1_ilp, j ) + v2*c( 2_ilp, j ) + v3*c( 3_ilp, j ) +v4*c( 4_ilp, j ) + v5*c( 5_ilp, j ) + &
                           v6*c( 6_ilp, j ) +v7*c( 7_ilp, j ) + v8*c( 8_ilp, j ) + v9*c( 9_ilp, j )
                 c( 1_ilp, j ) = c( 1_ilp, j ) - sum*t1
                 c( 2_ilp, j ) = c( 2_ilp, j ) - sum*t2
                 c( 3_ilp, j ) = c( 3_ilp, j ) - sum*t3
                 c( 4_ilp, j ) = c( 4_ilp, j ) - sum*t4
                 c( 5_ilp, j ) = c( 5_ilp, j ) - sum*t5
                 c( 6_ilp, j ) = c( 6_ilp, j ) - sum*t6
                 c( 7_ilp, j ) = c( 7_ilp, j ) - sum*t7
                 c( 8_ilp, j ) = c( 8_ilp, j ) - sum*t8
                 c( 9_ilp, j ) = c( 9_ilp, j ) - sum*t9
              end do
              go to 410
              190 continue
              ! special code for 10 x 10 householder
              v1 = conjg( v( 1_ilp ) )
              t1 = tau*conjg( v1 )
              v2 = conjg( v( 2_ilp ) )
              t2 = tau*conjg( v2 )
              v3 = conjg( v( 3_ilp ) )
              t3 = tau*conjg( v3 )
              v4 = conjg( v( 4_ilp ) )
              t4 = tau*conjg( v4 )
              v5 = conjg( v( 5_ilp ) )
              t5 = tau*conjg( v5 )
              v6 = conjg( v( 6_ilp ) )
              t6 = tau*conjg( v6 )
              v7 = conjg( v( 7_ilp ) )
              t7 = tau*conjg( v7 )
              v8 = conjg( v( 8_ilp ) )
              t8 = tau*conjg( v8 )
              v9 = conjg( v( 9_ilp ) )
              t9 = tau*conjg( v9 )
              v10 = conjg( v( 10_ilp ) )
              t10 = tau*conjg( v10 )
              do j = 1, n
                 sum = v1*c( 1_ilp, j ) + v2*c( 2_ilp, j ) + v3*c( 3_ilp, j ) +v4*c( 4_ilp, j ) + v5*c( 5_ilp, j ) + &
                           v6*c( 6_ilp, j ) +v7*c( 7_ilp, j ) + v8*c( 8_ilp, j ) + v9*c( 9_ilp, j ) +v10*c( 10_ilp, j )
                 c( 1_ilp, j ) = c( 1_ilp, j ) - sum*t1
                 c( 2_ilp, j ) = c( 2_ilp, j ) - sum*t2
                 c( 3_ilp, j ) = c( 3_ilp, j ) - sum*t3
                 c( 4_ilp, j ) = c( 4_ilp, j ) - sum*t4
                 c( 5_ilp, j ) = c( 5_ilp, j ) - sum*t5
                 c( 6_ilp, j ) = c( 6_ilp, j ) - sum*t6
                 c( 7_ilp, j ) = c( 7_ilp, j ) - sum*t7
                 c( 8_ilp, j ) = c( 8_ilp, j ) - sum*t8
                 c( 9_ilp, j ) = c( 9_ilp, j ) - sum*t9
                 c( 10_ilp, j ) = c( 10_ilp, j ) - sum*t10
              end do
              go to 410
           else
              ! form  c * h, where h has order n.
              go to ( 210, 230, 250, 270, 290, 310, 330, 350,370, 390 )n
              ! code for general n
              call stdlib_zlarf( side, m, n, v, 1_ilp, tau, c, ldc, work )
              go to 410
              210 continue
              ! special code for 1 x 1 householder
              t1 = cone - tau*v( 1_ilp )*conjg( v( 1_ilp ) )
              do j = 1, m
                 c( j, 1_ilp ) = t1*c( j, 1_ilp )
              end do
              go to 410
              230 continue
              ! special code for 2 x 2 householder
              v1 = v( 1_ilp )
              t1 = tau*conjg( v1 )
              v2 = v( 2_ilp )
              t2 = tau*conjg( v2 )
              do j = 1, m
                 sum = v1*c( j, 1_ilp ) + v2*c( j, 2_ilp )
                 c( j, 1_ilp ) = c( j, 1_ilp ) - sum*t1
                 c( j, 2_ilp ) = c( j, 2_ilp ) - sum*t2
              end do
              go to 410
              250 continue
              ! special code for 3 x 3 householder
              v1 = v( 1_ilp )
              t1 = tau*conjg( v1 )
              v2 = v( 2_ilp )
              t2 = tau*conjg( v2 )
              v3 = v( 3_ilp )
              t3 = tau*conjg( v3 )
              do j = 1, m
                 sum = v1*c( j, 1_ilp ) + v2*c( j, 2_ilp ) + v3*c( j, 3_ilp )
                 c( j, 1_ilp ) = c( j, 1_ilp ) - sum*t1
                 c( j, 2_ilp ) = c( j, 2_ilp ) - sum*t2
                 c( j, 3_ilp ) = c( j, 3_ilp ) - sum*t3
              end do
              go to 410
              270 continue
              ! special code for 4 x 4 householder
              v1 = v( 1_ilp )
              t1 = tau*conjg( v1 )
              v2 = v( 2_ilp )
              t2 = tau*conjg( v2 )
              v3 = v( 3_ilp )
              t3 = tau*conjg( v3 )
              v4 = v( 4_ilp )
              t4 = tau*conjg( v4 )
              do j = 1, m
                 sum = v1*c( j, 1_ilp ) + v2*c( j, 2_ilp ) + v3*c( j, 3_ilp ) +v4*c( j, 4_ilp )
                 c( j, 1_ilp ) = c( j, 1_ilp ) - sum*t1
                 c( j, 2_ilp ) = c( j, 2_ilp ) - sum*t2
                 c( j, 3_ilp ) = c( j, 3_ilp ) - sum*t3
                 c( j, 4_ilp ) = c( j, 4_ilp ) - sum*t4
              end do
              go to 410
              290 continue
              ! special code for 5 x 5 householder
              v1 = v( 1_ilp )
              t1 = tau*conjg( v1 )
              v2 = v( 2_ilp )
              t2 = tau*conjg( v2 )
              v3 = v( 3_ilp )
              t3 = tau*conjg( v3 )
              v4 = v( 4_ilp )
              t4 = tau*conjg( v4 )
              v5 = v( 5_ilp )
              t5 = tau*conjg( v5 )
              do j = 1, m
                 sum = v1*c( j, 1_ilp ) + v2*c( j, 2_ilp ) + v3*c( j, 3_ilp ) +v4*c( j, 4_ilp ) + v5*c( j, 5_ilp )
                           
                 c( j, 1_ilp ) = c( j, 1_ilp ) - sum*t1
                 c( j, 2_ilp ) = c( j, 2_ilp ) - sum*t2
                 c( j, 3_ilp ) = c( j, 3_ilp ) - sum*t3
                 c( j, 4_ilp ) = c( j, 4_ilp ) - sum*t4
                 c( j, 5_ilp ) = c( j, 5_ilp ) - sum*t5
              end do
              go to 410
              310 continue
              ! special code for 6 x 6 householder
              v1 = v( 1_ilp )
              t1 = tau*conjg( v1 )
              v2 = v( 2_ilp )
              t2 = tau*conjg( v2 )
              v3 = v( 3_ilp )
              t3 = tau*conjg( v3 )
              v4 = v( 4_ilp )
              t4 = tau*conjg( v4 )
              v5 = v( 5_ilp )
              t5 = tau*conjg( v5 )
              v6 = v( 6_ilp )
              t6 = tau*conjg( v6 )
              do j = 1, m
                 sum = v1*c( j, 1_ilp ) + v2*c( j, 2_ilp ) + v3*c( j, 3_ilp ) +v4*c( j, 4_ilp ) + v5*c( j, 5_ilp ) + &
                           v6*c( j, 6_ilp )
                 c( j, 1_ilp ) = c( j, 1_ilp ) - sum*t1
                 c( j, 2_ilp ) = c( j, 2_ilp ) - sum*t2
                 c( j, 3_ilp ) = c( j, 3_ilp ) - sum*t3
                 c( j, 4_ilp ) = c( j, 4_ilp ) - sum*t4
                 c( j, 5_ilp ) = c( j, 5_ilp ) - sum*t5
                 c( j, 6_ilp ) = c( j, 6_ilp ) - sum*t6
              end do
              go to 410
              330 continue
              ! special code for 7 x 7 householder
              v1 = v( 1_ilp )
              t1 = tau*conjg( v1 )
              v2 = v( 2_ilp )
              t2 = tau*conjg( v2 )
              v3 = v( 3_ilp )
              t3 = tau*conjg( v3 )
              v4 = v( 4_ilp )
              t4 = tau*conjg( v4 )
              v5 = v( 5_ilp )
              t5 = tau*conjg( v5 )
              v6 = v( 6_ilp )
              t6 = tau*conjg( v6 )
              v7 = v( 7_ilp )
              t7 = tau*conjg( v7 )
              do j = 1, m
                 sum = v1*c( j, 1_ilp ) + v2*c( j, 2_ilp ) + v3*c( j, 3_ilp ) +v4*c( j, 4_ilp ) + v5*c( j, 5_ilp ) + &
                           v6*c( j, 6_ilp ) +v7*c( j, 7_ilp )
                 c( j, 1_ilp ) = c( j, 1_ilp ) - sum*t1
                 c( j, 2_ilp ) = c( j, 2_ilp ) - sum*t2
                 c( j, 3_ilp ) = c( j, 3_ilp ) - sum*t3
                 c( j, 4_ilp ) = c( j, 4_ilp ) - sum*t4
                 c( j, 5_ilp ) = c( j, 5_ilp ) - sum*t5
                 c( j, 6_ilp ) = c( j, 6_ilp ) - sum*t6
                 c( j, 7_ilp ) = c( j, 7_ilp ) - sum*t7
              end do
              go to 410
              350 continue
              ! special code for 8 x 8 householder
              v1 = v( 1_ilp )
              t1 = tau*conjg( v1 )
              v2 = v( 2_ilp )
              t2 = tau*conjg( v2 )
              v3 = v( 3_ilp )
              t3 = tau*conjg( v3 )
              v4 = v( 4_ilp )
              t4 = tau*conjg( v4 )
              v5 = v( 5_ilp )
              t5 = tau*conjg( v5 )
              v6 = v( 6_ilp )
              t6 = tau*conjg( v6 )
              v7 = v( 7_ilp )
              t7 = tau*conjg( v7 )
              v8 = v( 8_ilp )
              t8 = tau*conjg( v8 )
              do j = 1, m
                 sum = v1*c( j, 1_ilp ) + v2*c( j, 2_ilp ) + v3*c( j, 3_ilp ) +v4*c( j, 4_ilp ) + v5*c( j, 5_ilp ) + &
                           v6*c( j, 6_ilp ) +v7*c( j, 7_ilp ) + v8*c( j, 8_ilp )
                 c( j, 1_ilp ) = c( j, 1_ilp ) - sum*t1
                 c( j, 2_ilp ) = c( j, 2_ilp ) - sum*t2
                 c( j, 3_ilp ) = c( j, 3_ilp ) - sum*t3
                 c( j, 4_ilp ) = c( j, 4_ilp ) - sum*t4
                 c( j, 5_ilp ) = c( j, 5_ilp ) - sum*t5
                 c( j, 6_ilp ) = c( j, 6_ilp ) - sum*t6
                 c( j, 7_ilp ) = c( j, 7_ilp ) - sum*t7
                 c( j, 8_ilp ) = c( j, 8_ilp ) - sum*t8
              end do
              go to 410
              370 continue
              ! special code for 9 x 9 householder
              v1 = v( 1_ilp )
              t1 = tau*conjg( v1 )
              v2 = v( 2_ilp )
              t2 = tau*conjg( v2 )
              v3 = v( 3_ilp )
              t3 = tau*conjg( v3 )
              v4 = v( 4_ilp )
              t4 = tau*conjg( v4 )
              v5 = v( 5_ilp )
              t5 = tau*conjg( v5 )
              v6 = v( 6_ilp )
              t6 = tau*conjg( v6 )
              v7 = v( 7_ilp )
              t7 = tau*conjg( v7 )
              v8 = v( 8_ilp )
              t8 = tau*conjg( v8 )
              v9 = v( 9_ilp )
              t9 = tau*conjg( v9 )
              do j = 1, m
                 sum = v1*c( j, 1_ilp ) + v2*c( j, 2_ilp ) + v3*c( j, 3_ilp ) +v4*c( j, 4_ilp ) + v5*c( j, 5_ilp ) + &
                           v6*c( j, 6_ilp ) +v7*c( j, 7_ilp ) + v8*c( j, 8_ilp ) + v9*c( j, 9_ilp )
                 c( j, 1_ilp ) = c( j, 1_ilp ) - sum*t1
                 c( j, 2_ilp ) = c( j, 2_ilp ) - sum*t2
                 c( j, 3_ilp ) = c( j, 3_ilp ) - sum*t3
                 c( j, 4_ilp ) = c( j, 4_ilp ) - sum*t4
                 c( j, 5_ilp ) = c( j, 5_ilp ) - sum*t5
                 c( j, 6_ilp ) = c( j, 6_ilp ) - sum*t6
                 c( j, 7_ilp ) = c( j, 7_ilp ) - sum*t7
                 c( j, 8_ilp ) = c( j, 8_ilp ) - sum*t8
                 c( j, 9_ilp ) = c( j, 9_ilp ) - sum*t9
              end do
              go to 410
              390 continue
              ! special code for 10 x 10 householder
              v1 = v( 1_ilp )
              t1 = tau*conjg( v1 )
              v2 = v( 2_ilp )
              t2 = tau*conjg( v2 )
              v3 = v( 3_ilp )
              t3 = tau*conjg( v3 )
              v4 = v( 4_ilp )
              t4 = tau*conjg( v4 )
              v5 = v( 5_ilp )
              t5 = tau*conjg( v5 )
              v6 = v( 6_ilp )
              t6 = tau*conjg( v6 )
              v7 = v( 7_ilp )
              t7 = tau*conjg( v7 )
              v8 = v( 8_ilp )
              t8 = tau*conjg( v8 )
              v9 = v( 9_ilp )
              t9 = tau*conjg( v9 )
              v10 = v( 10_ilp )
              t10 = tau*conjg( v10 )
              do j = 1, m
                 sum = v1*c( j, 1_ilp ) + v2*c( j, 2_ilp ) + v3*c( j, 3_ilp ) +v4*c( j, 4_ilp ) + v5*c( j, 5_ilp ) + &
                           v6*c( j, 6_ilp ) +v7*c( j, 7_ilp ) + v8*c( j, 8_ilp ) + v9*c( j, 9_ilp ) +v10*c( j, 10_ilp )
                 c( j, 1_ilp ) = c( j, 1_ilp ) - sum*t1
                 c( j, 2_ilp ) = c( j, 2_ilp ) - sum*t2
                 c( j, 3_ilp ) = c( j, 3_ilp ) - sum*t3
                 c( j, 4_ilp ) = c( j, 4_ilp ) - sum*t4
                 c( j, 5_ilp ) = c( j, 5_ilp ) - sum*t5
                 c( j, 6_ilp ) = c( j, 6_ilp ) - sum*t6
                 c( j, 7_ilp ) = c( j, 7_ilp ) - sum*t7
                 c( j, 8_ilp ) = c( j, 8_ilp ) - sum*t8
                 c( j, 9_ilp ) = c( j, 9_ilp ) - sum*t9
                 c( j, 10_ilp ) = c( j, 10_ilp ) - sum*t10
              end do
              go to 410
           end if
           410 continue
           return
     end subroutine stdlib_zlarfx




     pure module subroutine stdlib_slarfy( uplo, n, v, incv, tau, c, ldc, work )
     !! SLARFY applies an elementary reflector, or Householder matrix, H,
     !! to an n x n symmetric matrix C, from both the left and the right.
     !! H is represented in the form
     !! H = I - tau * v * v'
     !! where  tau  is a scalar and  v  is a vector.
     !! If  tau  is  zero, then  H  is taken to be the unit matrix.
        ! -- lapack test routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: incv, ldc, n
           real(sp), intent(in) :: tau
           ! Array Arguments 
           real(sp), intent(inout) :: c(ldc,*)
           real(sp), intent(in) :: v(*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           real(sp) :: alpha
           ! Executable Statements 
           if( tau==zero )return
           ! form  w:= c * v
           call stdlib_ssymv( uplo, n, one, c, ldc, v, incv, zero, work, 1_ilp )
           alpha = -half*tau*stdlib_sdot( n, work, 1_ilp, v, incv )
           call stdlib_saxpy( n, alpha, v, incv, work, 1_ilp )
           ! c := c - v * w' - w * v'
           call stdlib_ssyr2( uplo, n, -tau, v, incv, work, 1_ilp, c, ldc )
           return
     end subroutine stdlib_slarfy

     pure module subroutine stdlib_dlarfy( uplo, n, v, incv, tau, c, ldc, work )
     !! DLARFY applies an elementary reflector, or Householder matrix, H,
     !! to an n x n symmetric matrix C, from both the left and the right.
     !! H is represented in the form
     !! H = I - tau * v * v'
     !! where  tau  is a scalar and  v  is a vector.
     !! If  tau  is  zero, then  H  is taken to be the unit matrix.
        ! -- lapack test routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: incv, ldc, n
           real(dp), intent(in) :: tau
           ! Array Arguments 
           real(dp), intent(inout) :: c(ldc,*)
           real(dp), intent(in) :: v(*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           real(dp) :: alpha
           ! Executable Statements 
           if( tau==zero )return
           ! form  w:= c * v
           call stdlib_dsymv( uplo, n, one, c, ldc, v, incv, zero, work, 1_ilp )
           alpha = -half*tau*stdlib_ddot( n, work, 1_ilp, v, incv )
           call stdlib_daxpy( n, alpha, v, incv, work, 1_ilp )
           ! c := c - v * w' - w * v'
           call stdlib_dsyr2( uplo, n, -tau, v, incv, work, 1_ilp, c, ldc )
           return
     end subroutine stdlib_dlarfy


     pure module subroutine stdlib_clarfy( uplo, n, v, incv, tau, c, ldc, work )
     !! CLARFY applies an elementary reflector, or Householder matrix, H,
     !! to an n x n Hermitian matrix C, from both the left and the right.
     !! H is represented in the form
     !! H = I - tau * v * v'
     !! where  tau  is a scalar and  v  is a vector.
     !! If  tau  is  zero, then  H  is taken to be the unit matrix.
        ! -- lapack test routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: incv, ldc, n
           complex(sp), intent(in) :: tau
           ! Array Arguments 
           complex(sp), intent(inout) :: c(ldc,*)
           complex(sp), intent(in) :: v(*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           complex(sp) :: alpha
           ! Executable Statements 
           if( tau==czero )return
           ! form  w:= c * v
           call stdlib_chemv( uplo, n, cone, c, ldc, v, incv, czero, work, 1_ilp )
           alpha = -chalf*tau*stdlib_cdotc( n, work, 1_ilp, v, incv )
           call stdlib_caxpy( n, alpha, v, incv, work, 1_ilp )
           ! c := c - v * w' - w * v'
           call stdlib_cher2( uplo, n, -tau, v, incv, work, 1_ilp, c, ldc )
           return
     end subroutine stdlib_clarfy

     pure module subroutine stdlib_zlarfy( uplo, n, v, incv, tau, c, ldc, work )
     !! ZLARFY applies an elementary reflector, or Householder matrix, H,
     !! to an n x n Hermitian matrix C, from both the left and the right.
     !! H is represented in the form
     !! H = I - tau * v * v'
     !! where  tau  is a scalar and  v  is a vector.
     !! If  tau  is  zero, then  H  is taken to be the unit matrix.
        ! -- lapack test routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: incv, ldc, n
           complex(dp), intent(in) :: tau
           ! Array Arguments 
           complex(dp), intent(inout) :: c(ldc,*)
           complex(dp), intent(in) :: v(*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           complex(dp) :: alpha
           ! Executable Statements 
           if( tau==czero )return
           ! form  w:= c * v
           call stdlib_zhemv( uplo, n, cone, c, ldc, v, incv, czero, work, 1_ilp )
           alpha = -chalf*tau*stdlib_zdotc( n, work, 1_ilp, v, incv )
           call stdlib_zaxpy( n, alpha, v, incv, work, 1_ilp )
           ! c := c - v * w' - w * v'
           call stdlib_zher2( uplo, n, -tau, v, incv, work, 1_ilp, c, ldc )
           return
     end subroutine stdlib_zlarfy




     pure module subroutine stdlib_slarfb( side, trans, direct, storev, m, n, k, v, ldv,t, ldt, c, ldc, &
     !! SLARFB applies a real block reflector H or its transpose H**T to a
     !! real m by n matrix C, from either the left or the right.
               work, ldwork )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: direct, side, storev, trans
           integer(ilp), intent(in) :: k, ldc, ldt, ldv, ldwork, m, n
           ! Array Arguments 
           real(sp), intent(inout) :: c(ldc,*)
           real(sp), intent(in) :: t(ldt,*), v(ldv,*)
           real(sp), intent(out) :: work(ldwork,*)
        ! =====================================================================
           
           ! Local Scalars 
           character :: transt
           integer(ilp) :: i, j
           ! Executable Statements 
           ! quick return if possible
           if( m<=0 .or. n<=0 )return
           if( stdlib_lsame( trans, 'N' ) ) then
              transt = 'T'
           else
              transt = 'N'
           end if
           if( stdlib_lsame( storev, 'C' ) ) then
              if( stdlib_lsame( direct, 'F' ) ) then
                 ! let  v =  ( v1 )    (first k rows)
                           ! ( v2 )
                 ! where  v1  is unit lower triangular.
                 if( stdlib_lsame( side, 'L' ) ) then
                    ! form  h * c  or  h**t * c  where  c = ( c1 )
                                                          ! ( c2 )
                    ! w := c**t * v  =  (c1**t * v1 + c2**t * v2)  (stored in work)
                    ! w := c1**t
                    do j = 1, k
                       call stdlib_scopy( n, c( j, 1_ilp ), ldc, work( 1_ilp, j ), 1_ilp )
                    end do
                    ! w := w * v1
                    call stdlib_strmm( 'RIGHT', 'LOWER', 'NO TRANSPOSE', 'UNIT', n,k, one, v, ldv,&
                               work, ldwork )
                    if( m>k ) then
                       ! w := w + c2**t * v2
                       call stdlib_sgemm( 'TRANSPOSE', 'NO TRANSPOSE', n, k, m-k,one, c( k+1, 1_ilp ),&
                                  ldc, v( k+1, 1_ilp ), ldv,one, work, ldwork )
                    end if
                    ! w := w * t**t  or  w * t
                    call stdlib_strmm( 'RIGHT', 'UPPER', transt, 'NON-UNIT', n, k,one, t, ldt, &
                              work, ldwork )
                    ! c := c - v * w**t
                    if( m>k ) then
                       ! c2 := c2 - v2 * w**t
                       call stdlib_sgemm( 'NO TRANSPOSE', 'TRANSPOSE', m-k, n, k,-one, v( k+1, 1_ilp )&
                                 , ldv, work, ldwork, one,c( k+1, 1_ilp ), ldc )
                    end if
                    ! w := w * v1**t
                    call stdlib_strmm( 'RIGHT', 'LOWER', 'TRANSPOSE', 'UNIT', n, k,one, v, ldv, &
                              work, ldwork )
                    ! c1 := c1 - w**t
                    do j = 1, k
                       do i = 1, n
                          c( j, i ) = c( j, i ) - work( i, j )
                       end do
                    end do
                 else if( stdlib_lsame( side, 'R' ) ) then
                    ! form  c * h  or  c * h**t  where  c = ( c1  c2 )
                    ! w := c * v  =  (c1*v1 + c2*v2)  (stored in work)
                    ! w := c1
                    do j = 1, k
                       call stdlib_scopy( m, c( 1_ilp, j ), 1_ilp, work( 1_ilp, j ), 1_ilp )
                    end do
                    ! w := w * v1
                    call stdlib_strmm( 'RIGHT', 'LOWER', 'NO TRANSPOSE', 'UNIT', m,k, one, v, ldv,&
                               work, ldwork )
                    if( n>k ) then
                       ! w := w + c2 * v2
                       call stdlib_sgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', m, k, n-k,one, c( 1_ilp, k+&
                                 1_ilp ), ldc, v( k+1, 1_ilp ), ldv,one, work, ldwork )
                    end if
                    ! w := w * t  or  w * t**t
                    call stdlib_strmm( 'RIGHT', 'UPPER', trans, 'NON-UNIT', m, k,one, t, ldt, &
                              work, ldwork )
                    ! c := c - w * v**t
                    if( n>k ) then
                       ! c2 := c2 - w * v2**t
                       call stdlib_sgemm( 'NO TRANSPOSE', 'TRANSPOSE', m, n-k, k,-one, work, &
                                 ldwork, v( k+1, 1_ilp ), ldv, one,c( 1_ilp, k+1 ), ldc )
                    end if
                    ! w := w * v1**t
                    call stdlib_strmm( 'RIGHT', 'LOWER', 'TRANSPOSE', 'UNIT', m, k,one, v, ldv, &
                              work, ldwork )
                    ! c1 := c1 - w
                    do j = 1, k
                       do i = 1, m
                          c( i, j ) = c( i, j ) - work( i, j )
                       end do
                    end do
                 end if
              else
                 ! let  v =  ( v1 )
                           ! ( v2 )    (last k rows)
                 ! where  v2  is unit upper triangular.
                 if( stdlib_lsame( side, 'L' ) ) then
                    ! form  h * c  or  h**t * c  where  c = ( c1 )
                                                          ! ( c2 )
                    ! w := c**t * v  =  (c1**t * v1 + c2**t * v2)  (stored in work)
                    ! w := c2**t
                    do j = 1, k
                       call stdlib_scopy( n, c( m-k+j, 1_ilp ), ldc, work( 1_ilp, j ), 1_ilp )
                    end do
                    ! w := w * v2
                    call stdlib_strmm( 'RIGHT', 'UPPER', 'NO TRANSPOSE', 'UNIT', n,k, one, v( m-k+&
                              1_ilp, 1_ilp ), ldv, work, ldwork )
                    if( m>k ) then
                       ! w := w + c1**t * v1
                       call stdlib_sgemm( 'TRANSPOSE', 'NO TRANSPOSE', n, k, m-k,one, c, ldc, v, &
                                 ldv, one, work, ldwork )
                    end if
                    ! w := w * t**t  or  w * t
                    call stdlib_strmm( 'RIGHT', 'LOWER', transt, 'NON-UNIT', n, k,one, t, ldt, &
                              work, ldwork )
                    ! c := c - v * w**t
                    if( m>k ) then
                       ! c1 := c1 - v1 * w**t
                       call stdlib_sgemm( 'NO TRANSPOSE', 'TRANSPOSE', m-k, n, k,-one, v, ldv, &
                                 work, ldwork, one, c, ldc )
                    end if
                    ! w := w * v2**t
                    call stdlib_strmm( 'RIGHT', 'UPPER', 'TRANSPOSE', 'UNIT', n, k,one, v( m-k+1, &
                              1_ilp ), ldv, work, ldwork )
                    ! c2 := c2 - w**t
                    do j = 1, k
                       do i = 1, n
                          c( m-k+j, i ) = c( m-k+j, i ) - work( i, j )
                       end do
                    end do
                 else if( stdlib_lsame( side, 'R' ) ) then
                    ! form  c * h  or  c * h'  where  c = ( c1  c2 )
                    ! w := c * v  =  (c1*v1 + c2*v2)  (stored in work)
                    ! w := c2
                    do j = 1, k
                       call stdlib_scopy( m, c( 1_ilp, n-k+j ), 1_ilp, work( 1_ilp, j ), 1_ilp )
                    end do
                    ! w := w * v2
                    call stdlib_strmm( 'RIGHT', 'UPPER', 'NO TRANSPOSE', 'UNIT', m,k, one, v( n-k+&
                              1_ilp, 1_ilp ), ldv, work, ldwork )
                    if( n>k ) then
                       ! w := w + c1 * v1
                       call stdlib_sgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', m, k, n-k,one, c, ldc, &
                                 v, ldv, one, work, ldwork )
                    end if
                    ! w := w * t  or  w * t**t
                    call stdlib_strmm( 'RIGHT', 'LOWER', trans, 'NON-UNIT', m, k,one, t, ldt, &
                              work, ldwork )
                    ! c := c - w * v**t
                    if( n>k ) then
                       ! c1 := c1 - w * v1**t
                       call stdlib_sgemm( 'NO TRANSPOSE', 'TRANSPOSE', m, n-k, k,-one, work, &
                                 ldwork, v, ldv, one, c, ldc )
                    end if
                    ! w := w * v2**t
                    call stdlib_strmm( 'RIGHT', 'UPPER', 'TRANSPOSE', 'UNIT', m, k,one, v( n-k+1, &
                              1_ilp ), ldv, work, ldwork )
                    ! c2 := c2 - w
                    do j = 1, k
                       do i = 1, m
                          c( i, n-k+j ) = c( i, n-k+j ) - work( i, j )
                       end do
                    end do
                 end if
              end if
           else if( stdlib_lsame( storev, 'R' ) ) then
              if( stdlib_lsame( direct, 'F' ) ) then
                 ! let  v =  ( v1  v2 )    (v1: first k columns)
                 ! where  v1  is unit upper triangular.
                 if( stdlib_lsame( side, 'L' ) ) then
                    ! form  h * c  or  h**t * c  where  c = ( c1 )
                                                          ! ( c2 )
                    ! w := c**t * v**t  =  (c1**t * v1**t + c2**t * v2**t) (stored in work)
                    ! w := c1**t
                    do j = 1, k
                       call stdlib_scopy( n, c( j, 1_ilp ), ldc, work( 1_ilp, j ), 1_ilp )
                    end do
                    ! w := w * v1**t
                    call stdlib_strmm( 'RIGHT', 'UPPER', 'TRANSPOSE', 'UNIT', n, k,one, v, ldv, &
                              work, ldwork )
                    if( m>k ) then
                       ! w := w + c2**t * v2**t
                       call stdlib_sgemm( 'TRANSPOSE', 'TRANSPOSE', n, k, m-k, one,c( k+1, 1_ilp ), &
                                 ldc, v( 1_ilp, k+1 ), ldv, one,work, ldwork )
                    end if
                    ! w := w * t**t  or  w * t
                    call stdlib_strmm( 'RIGHT', 'UPPER', transt, 'NON-UNIT', n, k,one, t, ldt, &
                              work, ldwork )
                    ! c := c - v**t * w**t
                    if( m>k ) then
                       ! c2 := c2 - v2**t * w**t
                       call stdlib_sgemm( 'TRANSPOSE', 'TRANSPOSE', m-k, n, k, -one,v( 1_ilp, k+1 ), &
                                 ldv, work, ldwork, one,c( k+1, 1_ilp ), ldc )
                    end if
                    ! w := w * v1
                    call stdlib_strmm( 'RIGHT', 'UPPER', 'NO TRANSPOSE', 'UNIT', n,k, one, v, ldv,&
                               work, ldwork )
                    ! c1 := c1 - w**t
                    do j = 1, k
                       do i = 1, n
                          c( j, i ) = c( j, i ) - work( i, j )
                       end do
                    end do
                 else if( stdlib_lsame( side, 'R' ) ) then
                    ! form  c * h  or  c * h**t  where  c = ( c1  c2 )
                    ! w := c * v**t  =  (c1*v1**t + c2*v2**t)  (stored in work)
                    ! w := c1
                    do j = 1, k
                       call stdlib_scopy( m, c( 1_ilp, j ), 1_ilp, work( 1_ilp, j ), 1_ilp )
                    end do
                    ! w := w * v1**t
                    call stdlib_strmm( 'RIGHT', 'UPPER', 'TRANSPOSE', 'UNIT', m, k,one, v, ldv, &
                              work, ldwork )
                    if( n>k ) then
                       ! w := w + c2 * v2**t
                       call stdlib_sgemm( 'NO TRANSPOSE', 'TRANSPOSE', m, k, n-k,one, c( 1_ilp, k+1 ),&
                                  ldc, v( 1_ilp, k+1 ), ldv,one, work, ldwork )
                    end if
                    ! w := w * t  or  w * t**t
                    call stdlib_strmm( 'RIGHT', 'UPPER', trans, 'NON-UNIT', m, k,one, t, ldt, &
                              work, ldwork )
                    ! c := c - w * v
                    if( n>k ) then
                       ! c2 := c2 - w * v2
                       call stdlib_sgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', m, n-k, k,-one, work, &
                                 ldwork, v( 1_ilp, k+1 ), ldv, one,c( 1_ilp, k+1 ), ldc )
                    end if
                    ! w := w * v1
                    call stdlib_strmm( 'RIGHT', 'UPPER', 'NO TRANSPOSE', 'UNIT', m,k, one, v, ldv,&
                               work, ldwork )
                    ! c1 := c1 - w
                    do j = 1, k
                       do i = 1, m
                          c( i, j ) = c( i, j ) - work( i, j )
                       end do
                    end do
                 end if
              else
                 ! let  v =  ( v1  v2 )    (v2: last k columns)
                 ! where  v2  is unit lower triangular.
                 if( stdlib_lsame( side, 'L' ) ) then
                    ! form  h * c  or  h**t * c  where  c = ( c1 )
                                                          ! ( c2 )
                    ! w := c**t * v**t  =  (c1**t * v1**t + c2**t * v2**t) (stored in work)
                    ! w := c2**t
                    do j = 1, k
                       call stdlib_scopy( n, c( m-k+j, 1_ilp ), ldc, work( 1_ilp, j ), 1_ilp )
                    end do
                    ! w := w * v2**t
                    call stdlib_strmm( 'RIGHT', 'LOWER', 'TRANSPOSE', 'UNIT', n, k,one, v( 1_ilp, m-k+&
                              1_ilp ), ldv, work, ldwork )
                    if( m>k ) then
                       ! w := w + c1**t * v1**t
                       call stdlib_sgemm( 'TRANSPOSE', 'TRANSPOSE', n, k, m-k, one,c, ldc, v, ldv,&
                                  one, work, ldwork )
                    end if
                    ! w := w * t**t  or  w * t
                    call stdlib_strmm( 'RIGHT', 'LOWER', transt, 'NON-UNIT', n, k,one, t, ldt, &
                              work, ldwork )
                    ! c := c - v**t * w**t
                    if( m>k ) then
                       ! c1 := c1 - v1**t * w**t
                       call stdlib_sgemm( 'TRANSPOSE', 'TRANSPOSE', m-k, n, k, -one,v, ldv, work, &
                                 ldwork, one, c, ldc )
                    end if
                    ! w := w * v2
                    call stdlib_strmm( 'RIGHT', 'LOWER', 'NO TRANSPOSE', 'UNIT', n,k, one, v( 1_ilp, &
                              m-k+1 ), ldv, work, ldwork )
                    ! c2 := c2 - w**t
                    do j = 1, k
                       do i = 1, n
                          c( m-k+j, i ) = c( m-k+j, i ) - work( i, j )
                       end do
                    end do
                 else if( stdlib_lsame( side, 'R' ) ) then
                    ! form  c * h  or  c * h**t  where  c = ( c1  c2 )
                    ! w := c * v**t  =  (c1*v1**t + c2*v2**t)  (stored in work)
                    ! w := c2
                    do j = 1, k
                       call stdlib_scopy( m, c( 1_ilp, n-k+j ), 1_ilp, work( 1_ilp, j ), 1_ilp )
                    end do
                    ! w := w * v2**t
                    call stdlib_strmm( 'RIGHT', 'LOWER', 'TRANSPOSE', 'UNIT', m, k,one, v( 1_ilp, n-k+&
                              1_ilp ), ldv, work, ldwork )
                    if( n>k ) then
                       ! w := w + c1 * v1**t
                       call stdlib_sgemm( 'NO TRANSPOSE', 'TRANSPOSE', m, k, n-k,one, c, ldc, v, &
                                 ldv, one, work, ldwork )
                    end if
                    ! w := w * t  or  w * t**t
                    call stdlib_strmm( 'RIGHT', 'LOWER', trans, 'NON-UNIT', m, k,one, t, ldt, &
                              work, ldwork )
                    ! c := c - w * v
                    if( n>k ) then
                       ! c1 := c1 - w * v1
                       call stdlib_sgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', m, n-k, k,-one, work, &
                                 ldwork, v, ldv, one, c, ldc )
                    end if
                    ! w := w * v2
                    call stdlib_strmm( 'RIGHT', 'LOWER', 'NO TRANSPOSE', 'UNIT', m,k, one, v( 1_ilp, &
                              n-k+1 ), ldv, work, ldwork )
                    ! c1 := c1 - w
                    do j = 1, k
                       do i = 1, m
                          c( i, n-k+j ) = c( i, n-k+j ) - work( i, j )
                       end do
                    end do
                 end if
              end if
           end if
           return
     end subroutine stdlib_slarfb

     pure module subroutine stdlib_dlarfb( side, trans, direct, storev, m, n, k, v, ldv,t, ldt, c, ldc, &
     !! DLARFB applies a real block reflector H or its transpose H**T to a
     !! real m by n matrix C, from either the left or the right.
               work, ldwork )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: direct, side, storev, trans
           integer(ilp), intent(in) :: k, ldc, ldt, ldv, ldwork, m, n
           ! Array Arguments 
           real(dp), intent(inout) :: c(ldc,*)
           real(dp), intent(in) :: t(ldt,*), v(ldv,*)
           real(dp), intent(out) :: work(ldwork,*)
        ! =====================================================================
           
           ! Local Scalars 
           character :: transt
           integer(ilp) :: i, j
           ! Executable Statements 
           ! quick return if possible
           if( m<=0 .or. n<=0 )return
           if( stdlib_lsame( trans, 'N' ) ) then
              transt = 'T'
           else
              transt = 'N'
           end if
           if( stdlib_lsame( storev, 'C' ) ) then
              if( stdlib_lsame( direct, 'F' ) ) then
                 ! let  v =  ( v1 )    (first k rows)
                           ! ( v2 )
                 ! where  v1  is unit lower triangular.
                 if( stdlib_lsame( side, 'L' ) ) then
                    ! form  h * c  or  h**t * c  where  c = ( c1 )
                                                          ! ( c2 )
                    ! w := c**t * v  =  (c1**t * v1 + c2**t * v2)  (stored in work)
                    ! w := c1**t
                    do j = 1, k
                       call stdlib_dcopy( n, c( j, 1_ilp ), ldc, work( 1_ilp, j ), 1_ilp )
                    end do
                    ! w := w * v1
                    call stdlib_dtrmm( 'RIGHT', 'LOWER', 'NO TRANSPOSE', 'UNIT', n,k, one, v, ldv,&
                               work, ldwork )
                    if( m>k ) then
                       ! w := w + c2**t * v2
                       call stdlib_dgemm( 'TRANSPOSE', 'NO TRANSPOSE', n, k, m-k,one, c( k+1, 1_ilp ),&
                                  ldc, v( k+1, 1_ilp ), ldv,one, work, ldwork )
                    end if
                    ! w := w * t**t  or  w * t
                    call stdlib_dtrmm( 'RIGHT', 'UPPER', transt, 'NON-UNIT', n, k,one, t, ldt, &
                              work, ldwork )
                    ! c := c - v * w**t
                    if( m>k ) then
                       ! c2 := c2 - v2 * w**t
                       call stdlib_dgemm( 'NO TRANSPOSE', 'TRANSPOSE', m-k, n, k,-one, v( k+1, 1_ilp )&
                                 , ldv, work, ldwork, one,c( k+1, 1_ilp ), ldc )
                    end if
                    ! w := w * v1**t
                    call stdlib_dtrmm( 'RIGHT', 'LOWER', 'TRANSPOSE', 'UNIT', n, k,one, v, ldv, &
                              work, ldwork )
                    ! c1 := c1 - w**t
                    do j = 1, k
                       do i = 1, n
                          c( j, i ) = c( j, i ) - work( i, j )
                       end do
                    end do
                 else if( stdlib_lsame( side, 'R' ) ) then
                    ! form  c * h  or  c * h**t  where  c = ( c1  c2 )
                    ! w := c * v  =  (c1*v1 + c2*v2)  (stored in work)
                    ! w := c1
                    do j = 1, k
                       call stdlib_dcopy( m, c( 1_ilp, j ), 1_ilp, work( 1_ilp, j ), 1_ilp )
                    end do
                    ! w := w * v1
                    call stdlib_dtrmm( 'RIGHT', 'LOWER', 'NO TRANSPOSE', 'UNIT', m,k, one, v, ldv,&
                               work, ldwork )
                    if( n>k ) then
                       ! w := w + c2 * v2
                       call stdlib_dgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', m, k, n-k,one, c( 1_ilp, k+&
                                 1_ilp ), ldc, v( k+1, 1_ilp ), ldv,one, work, ldwork )
                    end if
                    ! w := w * t  or  w * t**t
                    call stdlib_dtrmm( 'RIGHT', 'UPPER', trans, 'NON-UNIT', m, k,one, t, ldt, &
                              work, ldwork )
                    ! c := c - w * v**t
                    if( n>k ) then
                       ! c2 := c2 - w * v2**t
                       call stdlib_dgemm( 'NO TRANSPOSE', 'TRANSPOSE', m, n-k, k,-one, work, &
                                 ldwork, v( k+1, 1_ilp ), ldv, one,c( 1_ilp, k+1 ), ldc )
                    end if
                    ! w := w * v1**t
                    call stdlib_dtrmm( 'RIGHT', 'LOWER', 'TRANSPOSE', 'UNIT', m, k,one, v, ldv, &
                              work, ldwork )
                    ! c1 := c1 - w
                    do j = 1, k
                       do i = 1, m
                          c( i, j ) = c( i, j ) - work( i, j )
                       end do
                    end do
                 end if
              else
                 ! let  v =  ( v1 )
                           ! ( v2 )    (last k rows)
                 ! where  v2  is unit upper triangular.
                 if( stdlib_lsame( side, 'L' ) ) then
                    ! form  h * c  or  h**t * c  where  c = ( c1 )
                                                          ! ( c2 )
                    ! w := c**t * v  =  (c1**t * v1 + c2**t * v2)  (stored in work)
                    ! w := c2**t
                    do j = 1, k
                       call stdlib_dcopy( n, c( m-k+j, 1_ilp ), ldc, work( 1_ilp, j ), 1_ilp )
                    end do
                    ! w := w * v2
                    call stdlib_dtrmm( 'RIGHT', 'UPPER', 'NO TRANSPOSE', 'UNIT', n,k, one, v( m-k+&
                              1_ilp, 1_ilp ), ldv, work, ldwork )
                    if( m>k ) then
                       ! w := w + c1**t * v1
                       call stdlib_dgemm( 'TRANSPOSE', 'NO TRANSPOSE', n, k, m-k,one, c, ldc, v, &
                                 ldv, one, work, ldwork )
                    end if
                    ! w := w * t**t  or  w * t
                    call stdlib_dtrmm( 'RIGHT', 'LOWER', transt, 'NON-UNIT', n, k,one, t, ldt, &
                              work, ldwork )
                    ! c := c - v * w**t
                    if( m>k ) then
                       ! c1 := c1 - v1 * w**t
                       call stdlib_dgemm( 'NO TRANSPOSE', 'TRANSPOSE', m-k, n, k,-one, v, ldv, &
                                 work, ldwork, one, c, ldc )
                    end if
                    ! w := w * v2**t
                    call stdlib_dtrmm( 'RIGHT', 'UPPER', 'TRANSPOSE', 'UNIT', n, k,one, v( m-k+1, &
                              1_ilp ), ldv, work, ldwork )
                    ! c2 := c2 - w**t
                    do j = 1, k
                       do i = 1, n
                          c( m-k+j, i ) = c( m-k+j, i ) - work( i, j )
                       end do
                    end do
                 else if( stdlib_lsame( side, 'R' ) ) then
                    ! form  c * h  or  c * h**t  where  c = ( c1  c2 )
                    ! w := c * v  =  (c1*v1 + c2*v2)  (stored in work)
                    ! w := c2
                    do j = 1, k
                       call stdlib_dcopy( m, c( 1_ilp, n-k+j ), 1_ilp, work( 1_ilp, j ), 1_ilp )
                    end do
                    ! w := w * v2
                    call stdlib_dtrmm( 'RIGHT', 'UPPER', 'NO TRANSPOSE', 'UNIT', m,k, one, v( n-k+&
                              1_ilp, 1_ilp ), ldv, work, ldwork )
                    if( n>k ) then
                       ! w := w + c1 * v1
                       call stdlib_dgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', m, k, n-k,one, c, ldc, &
                                 v, ldv, one, work, ldwork )
                    end if
                    ! w := w * t  or  w * t**t
                    call stdlib_dtrmm( 'RIGHT', 'LOWER', trans, 'NON-UNIT', m, k,one, t, ldt, &
                              work, ldwork )
                    ! c := c - w * v**t
                    if( n>k ) then
                       ! c1 := c1 - w * v1**t
                       call stdlib_dgemm( 'NO TRANSPOSE', 'TRANSPOSE', m, n-k, k,-one, work, &
                                 ldwork, v, ldv, one, c, ldc )
                    end if
                    ! w := w * v2**t
                    call stdlib_dtrmm( 'RIGHT', 'UPPER', 'TRANSPOSE', 'UNIT', m, k,one, v( n-k+1, &
                              1_ilp ), ldv, work, ldwork )
                    ! c2 := c2 - w
                    do j = 1, k
                       do i = 1, m
                          c( i, n-k+j ) = c( i, n-k+j ) - work( i, j )
                       end do
                    end do
                 end if
              end if
           else if( stdlib_lsame( storev, 'R' ) ) then
              if( stdlib_lsame( direct, 'F' ) ) then
                 ! let  v =  ( v1  v2 )    (v1: first k columns)
                 ! where  v1  is unit upper triangular.
                 if( stdlib_lsame( side, 'L' ) ) then
                    ! form  h * c  or  h**t * c  where  c = ( c1 )
                                                          ! ( c2 )
                    ! w := c**t * v**t  =  (c1**t * v1**t + c2**t * v2**t) (stored in work)
                    ! w := c1**t
                    do j = 1, k
                       call stdlib_dcopy( n, c( j, 1_ilp ), ldc, work( 1_ilp, j ), 1_ilp )
                    end do
                    ! w := w * v1**t
                    call stdlib_dtrmm( 'RIGHT', 'UPPER', 'TRANSPOSE', 'UNIT', n, k,one, v, ldv, &
                              work, ldwork )
                    if( m>k ) then
                       ! w := w + c2**t * v2**t
                       call stdlib_dgemm( 'TRANSPOSE', 'TRANSPOSE', n, k, m-k, one,c( k+1, 1_ilp ), &
                                 ldc, v( 1_ilp, k+1 ), ldv, one,work, ldwork )
                    end if
                    ! w := w * t**t  or  w * t
                    call stdlib_dtrmm( 'RIGHT', 'UPPER', transt, 'NON-UNIT', n, k,one, t, ldt, &
                              work, ldwork )
                    ! c := c - v**t * w**t
                    if( m>k ) then
                       ! c2 := c2 - v2**t * w**t
                       call stdlib_dgemm( 'TRANSPOSE', 'TRANSPOSE', m-k, n, k, -one,v( 1_ilp, k+1 ), &
                                 ldv, work, ldwork, one,c( k+1, 1_ilp ), ldc )
                    end if
                    ! w := w * v1
                    call stdlib_dtrmm( 'RIGHT', 'UPPER', 'NO TRANSPOSE', 'UNIT', n,k, one, v, ldv,&
                               work, ldwork )
                    ! c1 := c1 - w**t
                    do j = 1, k
                       do i = 1, n
                          c( j, i ) = c( j, i ) - work( i, j )
                       end do
                    end do
                 else if( stdlib_lsame( side, 'R' ) ) then
                    ! form  c * h  or  c * h**t  where  c = ( c1  c2 )
                    ! w := c * v**t  =  (c1*v1**t + c2*v2**t)  (stored in work)
                    ! w := c1
                    do j = 1, k
                       call stdlib_dcopy( m, c( 1_ilp, j ), 1_ilp, work( 1_ilp, j ), 1_ilp )
                    end do
                    ! w := w * v1**t
                    call stdlib_dtrmm( 'RIGHT', 'UPPER', 'TRANSPOSE', 'UNIT', m, k,one, v, ldv, &
                              work, ldwork )
                    if( n>k ) then
                       ! w := w + c2 * v2**t
                       call stdlib_dgemm( 'NO TRANSPOSE', 'TRANSPOSE', m, k, n-k,one, c( 1_ilp, k+1 ),&
                                  ldc, v( 1_ilp, k+1 ), ldv,one, work, ldwork )
                    end if
                    ! w := w * t  or  w * t**t
                    call stdlib_dtrmm( 'RIGHT', 'UPPER', trans, 'NON-UNIT', m, k,one, t, ldt, &
                              work, ldwork )
                    ! c := c - w * v
                    if( n>k ) then
                       ! c2 := c2 - w * v2
                       call stdlib_dgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', m, n-k, k,-one, work, &
                                 ldwork, v( 1_ilp, k+1 ), ldv, one,c( 1_ilp, k+1 ), ldc )
                    end if
                    ! w := w * v1
                    call stdlib_dtrmm( 'RIGHT', 'UPPER', 'NO TRANSPOSE', 'UNIT', m,k, one, v, ldv,&
                               work, ldwork )
                    ! c1 := c1 - w
                    do j = 1, k
                       do i = 1, m
                          c( i, j ) = c( i, j ) - work( i, j )
                       end do
                    end do
                 end if
              else
                 ! let  v =  ( v1  v2 )    (v2: last k columns)
                 ! where  v2  is unit lower triangular.
                 if( stdlib_lsame( side, 'L' ) ) then
                    ! form  h * c  or  h**t * c  where  c = ( c1 )
                                                          ! ( c2 )
                    ! w := c**t * v**t  =  (c1**t * v1**t + c2**t * v2**t) (stored in work)
                    ! w := c2**t
                    do j = 1, k
                       call stdlib_dcopy( n, c( m-k+j, 1_ilp ), ldc, work( 1_ilp, j ), 1_ilp )
                    end do
                    ! w := w * v2**t
                    call stdlib_dtrmm( 'RIGHT', 'LOWER', 'TRANSPOSE', 'UNIT', n, k,one, v( 1_ilp, m-k+&
                              1_ilp ), ldv, work, ldwork )
                    if( m>k ) then
                       ! w := w + c1**t * v1**t
                       call stdlib_dgemm( 'TRANSPOSE', 'TRANSPOSE', n, k, m-k, one,c, ldc, v, ldv,&
                                  one, work, ldwork )
                    end if
                    ! w := w * t**t  or  w * t
                    call stdlib_dtrmm( 'RIGHT', 'LOWER', transt, 'NON-UNIT', n, k,one, t, ldt, &
                              work, ldwork )
                    ! c := c - v**t * w**t
                    if( m>k ) then
                       ! c1 := c1 - v1**t * w**t
                       call stdlib_dgemm( 'TRANSPOSE', 'TRANSPOSE', m-k, n, k, -one,v, ldv, work, &
                                 ldwork, one, c, ldc )
                    end if
                    ! w := w * v2
                    call stdlib_dtrmm( 'RIGHT', 'LOWER', 'NO TRANSPOSE', 'UNIT', n,k, one, v( 1_ilp, &
                              m-k+1 ), ldv, work, ldwork )
                    ! c2 := c2 - w**t
                    do j = 1, k
                       do i = 1, n
                          c( m-k+j, i ) = c( m-k+j, i ) - work( i, j )
                       end do
                    end do
                 else if( stdlib_lsame( side, 'R' ) ) then
                    ! form  c * h  or  c * h'  where  c = ( c1  c2 )
                    ! w := c * v**t  =  (c1*v1**t + c2*v2**t)  (stored in work)
                    ! w := c2
                    do j = 1, k
                       call stdlib_dcopy( m, c( 1_ilp, n-k+j ), 1_ilp, work( 1_ilp, j ), 1_ilp )
                    end do
                    ! w := w * v2**t
                    call stdlib_dtrmm( 'RIGHT', 'LOWER', 'TRANSPOSE', 'UNIT', m, k,one, v( 1_ilp, n-k+&
                              1_ilp ), ldv, work, ldwork )
                    if( n>k ) then
                       ! w := w + c1 * v1**t
                       call stdlib_dgemm( 'NO TRANSPOSE', 'TRANSPOSE', m, k, n-k,one, c, ldc, v, &
                                 ldv, one, work, ldwork )
                    end if
                    ! w := w * t  or  w * t**t
                    call stdlib_dtrmm( 'RIGHT', 'LOWER', trans, 'NON-UNIT', m, k,one, t, ldt, &
                              work, ldwork )
                    ! c := c - w * v
                    if( n>k ) then
                       ! c1 := c1 - w * v1
                       call stdlib_dgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', m, n-k, k,-one, work, &
                                 ldwork, v, ldv, one, c, ldc )
                    end if
                    ! w := w * v2
                    call stdlib_dtrmm( 'RIGHT', 'LOWER', 'NO TRANSPOSE', 'UNIT', m,k, one, v( 1_ilp, &
                              n-k+1 ), ldv, work, ldwork )
                    ! c1 := c1 - w
                    do j = 1, k
                       do i = 1, m
                          c( i, n-k+j ) = c( i, n-k+j ) - work( i, j )
                       end do
                    end do
                 end if
              end if
           end if
           return
     end subroutine stdlib_dlarfb


     pure module subroutine stdlib_clarfb( side, trans, direct, storev, m, n, k, v, ldv,t, ldt, c, ldc, &
     work, ldwork )
     !! CLARFB applies a complex block reflector H or its transpose H**H to a
     !! complex M-by-N matrix C, from either the left or the right.
               
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: direct, side, storev, trans
           integer(ilp), intent(in) :: k, ldc, ldt, ldv, ldwork, m, n
           ! Array Arguments 
           complex(sp), intent(inout) :: c(ldc,*)
           complex(sp), intent(in) :: t(ldt,*), v(ldv,*)
           complex(sp), intent(out) :: work(ldwork,*)
        ! =====================================================================
           
           ! Local Scalars 
           character :: transt
           integer(ilp) :: i, j
           ! Intrinsic Functions 
           ! Executable Statements 
           ! quick return if possible
           if( m<=0 .or. n<=0 )return
           if( stdlib_lsame( trans, 'N' ) ) then
              transt = 'C'
           else
              transt = 'N'
           end if
           if( stdlib_lsame( storev, 'C' ) ) then
              if( stdlib_lsame( direct, 'F' ) ) then
                 ! let  v =  ( v1 )    (first k rows)
                           ! ( v2 )
                 ! where  v1  is unit lower triangular.
                 if( stdlib_lsame( side, 'L' ) ) then
                    ! form  h * c  or  h**h * c  where  c = ( c1 )
                                                          ! ( c2 )
                    ! w := c**h * v  =  (c1**h * v1 + c2**h * v2)  (stored in work)
                    ! w := c1**h
                    do j = 1, k
                       call stdlib_ccopy( n, c( j, 1_ilp ), ldc, work( 1_ilp, j ), 1_ilp )
                       call stdlib_clacgv( n, work( 1_ilp, j ), 1_ilp )
                    end do
                    ! w := w * v1
                    call stdlib_ctrmm( 'RIGHT', 'LOWER', 'NO TRANSPOSE', 'UNIT', n,k, cone, v, &
                              ldv, work, ldwork )
                    if( m>k ) then
                       ! w := w + c2**h *v2
                       call stdlib_cgemm( 'CONJUGATE TRANSPOSE', 'NO TRANSPOSE', n,k, m-k, cone, &
                                 c( k+1, 1_ilp ), ldc,v( k+1, 1_ilp ), ldv, cone, work, ldwork )
                    end if
                    ! w := w * t**h  or  w * t
                    call stdlib_ctrmm( 'RIGHT', 'UPPER', transt, 'NON-UNIT', n, k,cone, t, ldt, &
                              work, ldwork )
                    ! c := c - v * w**h
                    if( m>k ) then
                       ! c2 := c2 - v2 * w**h
                       call stdlib_cgemm( 'NO TRANSPOSE', 'CONJUGATE TRANSPOSE',m-k, n, k, -cone, &
                                 v( k+1, 1_ilp ), ldv, work,ldwork, cone, c( k+1, 1_ilp ), ldc )
                    end if
                    ! w := w * v1**h
                    call stdlib_ctrmm( 'RIGHT', 'LOWER', 'CONJUGATE TRANSPOSE','UNIT', n, k, cone,&
                               v, ldv, work, ldwork )
                    ! c1 := c1 - w**h
                    do j = 1, k
                       do i = 1, n
                          c( j, i ) = c( j, i ) - conjg( work( i, j ) )
                       end do
                    end do
                 else if( stdlib_lsame( side, 'R' ) ) then
                    ! form  c * h  or  c * h**h  where  c = ( c1  c2 )
                    ! w := c * v  =  (c1*v1 + c2*v2)  (stored in work)
                    ! w := c1
                    do j = 1, k
                       call stdlib_ccopy( m, c( 1_ilp, j ), 1_ilp, work( 1_ilp, j ), 1_ilp )
                    end do
                    ! w := w * v1
                    call stdlib_ctrmm( 'RIGHT', 'LOWER', 'NO TRANSPOSE', 'UNIT', m,k, cone, v, &
                              ldv, work, ldwork )
                    if( n>k ) then
                       ! w := w + c2 * v2
                       call stdlib_cgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', m, k, n-k,cone, c( 1_ilp, k+&
                                 1_ilp ), ldc, v( k+1, 1_ilp ), ldv,cone, work, ldwork )
                    end if
                    ! w := w * t  or  w * t**h
                    call stdlib_ctrmm( 'RIGHT', 'UPPER', trans, 'NON-UNIT', m, k,cone, t, ldt, &
                              work, ldwork )
                    ! c := c - w * v**h
                    if( n>k ) then
                       ! c2 := c2 - w * v2**h
                       call stdlib_cgemm( 'NO TRANSPOSE', 'CONJUGATE TRANSPOSE', m,n-k, k, -cone, &
                                 work, ldwork, v( k+1, 1_ilp ),ldv, cone, c( 1_ilp, k+1 ), ldc )
                    end if
                    ! w := w * v1**h
                    call stdlib_ctrmm( 'RIGHT', 'LOWER', 'CONJUGATE TRANSPOSE','UNIT', m, k, cone,&
                               v, ldv, work, ldwork )
                    ! c1 := c1 - w
                    do j = 1, k
                       do i = 1, m
                          c( i, j ) = c( i, j ) - work( i, j )
                       end do
                    end do
                 end if
              else
                 ! let  v =  ( v1 )
                           ! ( v2 )    (last k rows)
                 ! where  v2  is unit upper triangular.
                 if( stdlib_lsame( side, 'L' ) ) then
                    ! form  h * c  or  h**h * c  where  c = ( c1 )
                                                        ! ( c2 )
                    ! w := c**h * v  =  (c1**h * v1 + c2**h * v2)  (stored in work)
                    ! w := c2**h
                    do j = 1, k
                       call stdlib_ccopy( n, c( m-k+j, 1_ilp ), ldc, work( 1_ilp, j ), 1_ilp )
                       call stdlib_clacgv( n, work( 1_ilp, j ), 1_ilp )
                    end do
                    ! w := w * v2
                    call stdlib_ctrmm( 'RIGHT', 'UPPER', 'NO TRANSPOSE', 'UNIT', n,k, cone, v( m-&
                              k+1, 1_ilp ), ldv, work, ldwork )
                    if( m>k ) then
                       ! w := w + c1**h * v1
                       call stdlib_cgemm( 'CONJUGATE TRANSPOSE', 'NO TRANSPOSE', n,k, m-k, cone, &
                                 c, ldc, v, ldv, cone, work,ldwork )
                    end if
                    ! w := w * t**h  or  w * t
                    call stdlib_ctrmm( 'RIGHT', 'LOWER', transt, 'NON-UNIT', n, k,cone, t, ldt, &
                              work, ldwork )
                    ! c := c - v * w**h
                    if( m>k ) then
                       ! c1 := c1 - v1 * w**h
                       call stdlib_cgemm( 'NO TRANSPOSE', 'CONJUGATE TRANSPOSE',m-k, n, k, -cone, &
                                 v, ldv, work, ldwork,cone, c, ldc )
                    end if
                    ! w := w * v2**h
                    call stdlib_ctrmm( 'RIGHT', 'UPPER', 'CONJUGATE TRANSPOSE','UNIT', n, k, cone,&
                               v( m-k+1, 1_ilp ), ldv, work,ldwork )
                    ! c2 := c2 - w**h
                    do j = 1, k
                       do i = 1, n
                          c( m-k+j, i ) = c( m-k+j, i ) -conjg( work( i, j ) )
                       end do
                    end do
                 else if( stdlib_lsame( side, 'R' ) ) then
                    ! form  c * h  or  c * h**h  where  c = ( c1  c2 )
                    ! w := c * v  =  (c1*v1 + c2*v2)  (stored in work)
                    ! w := c2
                    do j = 1, k
                       call stdlib_ccopy( m, c( 1_ilp, n-k+j ), 1_ilp, work( 1_ilp, j ), 1_ilp )
                    end do
                    ! w := w * v2
                    call stdlib_ctrmm( 'RIGHT', 'UPPER', 'NO TRANSPOSE', 'UNIT', m,k, cone, v( n-&
                              k+1, 1_ilp ), ldv, work, ldwork )
                    if( n>k ) then
                       ! w := w + c1 * v1
                       call stdlib_cgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', m, k, n-k,cone, c, ldc, &
                                 v, ldv, cone, work, ldwork )
                    end if
                    ! w := w * t  or  w * t**h
                    call stdlib_ctrmm( 'RIGHT', 'LOWER', trans, 'NON-UNIT', m, k,cone, t, ldt, &
                              work, ldwork )
                    ! c := c - w * v**h
                    if( n>k ) then
                       ! c1 := c1 - w * v1**h
                       call stdlib_cgemm( 'NO TRANSPOSE', 'CONJUGATE TRANSPOSE', m,n-k, k, -cone, &
                                 work, ldwork, v, ldv, cone,c, ldc )
                    end if
                    ! w := w * v2**h
                    call stdlib_ctrmm( 'RIGHT', 'UPPER', 'CONJUGATE TRANSPOSE','UNIT', m, k, cone,&
                               v( n-k+1, 1_ilp ), ldv, work,ldwork )
                    ! c2 := c2 - w
                    do j = 1, k
                       do i = 1, m
                          c( i, n-k+j ) = c( i, n-k+j ) - work( i, j )
                       end do
                    end do
                 end if
              end if
           else if( stdlib_lsame( storev, 'R' ) ) then
              if( stdlib_lsame( direct, 'F' ) ) then
                 ! let  v =  ( v1  v2 )    (v1: first k columns)
                 ! where  v1  is unit upper triangular.
                 if( stdlib_lsame( side, 'L' ) ) then
                    ! form  h * c  or  h**h * c  where  c = ( c1 )
                                                          ! ( c2 )
                    ! w := c**h * v**h  =  (c1**h * v1**h + c2**h * v2**h) (stored in work)
                    ! w := c1**h
                    do j = 1, k
                       call stdlib_ccopy( n, c( j, 1_ilp ), ldc, work( 1_ilp, j ), 1_ilp )
                       call stdlib_clacgv( n, work( 1_ilp, j ), 1_ilp )
                    end do
                    ! w := w * v1**h
                    call stdlib_ctrmm( 'RIGHT', 'UPPER', 'CONJUGATE TRANSPOSE','UNIT', n, k, cone,&
                               v, ldv, work, ldwork )
                    if( m>k ) then
                       ! w := w + c2**h * v2**h
                       call stdlib_cgemm( 'CONJUGATE TRANSPOSE','CONJUGATE TRANSPOSE', n, k, m-k, &
                                 cone,c( k+1, 1_ilp ), ldc, v( 1_ilp, k+1 ), ldv, cone,work, ldwork )
                    end if
                    ! w := w * t**h  or  w * t
                    call stdlib_ctrmm( 'RIGHT', 'UPPER', transt, 'NON-UNIT', n, k,cone, t, ldt, &
                              work, ldwork )
                    ! c := c - v**h * w**h
                    if( m>k ) then
                       ! c2 := c2 - v2**h * w**h
                       call stdlib_cgemm( 'CONJUGATE TRANSPOSE','CONJUGATE TRANSPOSE', m-k, n, k, &
                                 -cone,v( 1_ilp, k+1 ), ldv, work, ldwork, cone,c( k+1, 1_ilp ), ldc )
                    end if
                    ! w := w * v1
                    call stdlib_ctrmm( 'RIGHT', 'UPPER', 'NO TRANSPOSE', 'UNIT', n,k, cone, v, &
                              ldv, work, ldwork )
                    ! c1 := c1 - w**h
                    do j = 1, k
                       do i = 1, n
                          c( j, i ) = c( j, i ) - conjg( work( i, j ) )
                       end do
                    end do
                 else if( stdlib_lsame( side, 'R' ) ) then
                    ! form  c * h  or  c * h**h  where  c = ( c1  c2 )
                    ! w := c * v**h  =  (c1*v1**h + c2*v2**h)  (stored in work)
                    ! w := c1
                    do j = 1, k
                       call stdlib_ccopy( m, c( 1_ilp, j ), 1_ilp, work( 1_ilp, j ), 1_ilp )
                    end do
                    ! w := w * v1**h
                    call stdlib_ctrmm( 'RIGHT', 'UPPER', 'CONJUGATE TRANSPOSE','UNIT', m, k, cone,&
                               v, ldv, work, ldwork )
                    if( n>k ) then
                       ! w := w + c2 * v2**h
                       call stdlib_cgemm( 'NO TRANSPOSE', 'CONJUGATE TRANSPOSE', m,k, n-k, cone, &
                                 c( 1_ilp, k+1 ), ldc,v( 1_ilp, k+1 ), ldv, cone, work, ldwork )
                    end if
                    ! w := w * t  or  w * t**h
                    call stdlib_ctrmm( 'RIGHT', 'UPPER', trans, 'NON-UNIT', m, k,cone, t, ldt, &
                              work, ldwork )
                    ! c := c - w * v
                    if( n>k ) then
                       ! c2 := c2 - w * v2
                       call stdlib_cgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', m, n-k, k,-cone, work, &
                                 ldwork, v( 1_ilp, k+1 ), ldv, cone,c( 1_ilp, k+1 ), ldc )
                    end if
                    ! w := w * v1
                    call stdlib_ctrmm( 'RIGHT', 'UPPER', 'NO TRANSPOSE', 'UNIT', m,k, cone, v, &
                              ldv, work, ldwork )
                    ! c1 := c1 - w
                    do j = 1, k
                       do i = 1, m
                          c( i, j ) = c( i, j ) - work( i, j )
                       end do
                    end do
                 end if
              else
                 ! let  v =  ( v1  v2 )    (v2: last k columns)
                 ! where  v2  is unit lower triangular.
                 if( stdlib_lsame( side, 'L' ) ) then
                    ! form  h * c  or  h**h * c  where  c = ( c1 )
                                                          ! ( c2 )
                    ! w := c**h * v**h  =  (c1**h * v1**h + c2**h * v2**h) (stored in work)
                    ! w := c2**h
                    do j = 1, k
                       call stdlib_ccopy( n, c( m-k+j, 1_ilp ), ldc, work( 1_ilp, j ), 1_ilp )
                       call stdlib_clacgv( n, work( 1_ilp, j ), 1_ilp )
                    end do
                    ! w := w * v2**h
                    call stdlib_ctrmm( 'RIGHT', 'LOWER', 'CONJUGATE TRANSPOSE','UNIT', n, k, cone,&
                               v( 1_ilp, m-k+1 ), ldv, work,ldwork )
                    if( m>k ) then
                       ! w := w + c1**h * v1**h
                       call stdlib_cgemm( 'CONJUGATE TRANSPOSE','CONJUGATE TRANSPOSE', n, k, m-k, &
                                 cone, c,ldc, v, ldv, cone, work, ldwork )
                    end if
                    ! w := w * t**h  or  w * t
                    call stdlib_ctrmm( 'RIGHT', 'LOWER', transt, 'NON-UNIT', n, k,cone, t, ldt, &
                              work, ldwork )
                    ! c := c - v**h * w**h
                    if( m>k ) then
                       ! c1 := c1 - v1**h * w**h
                       call stdlib_cgemm( 'CONJUGATE TRANSPOSE','CONJUGATE TRANSPOSE', m-k, n, k, &
                                 -cone, v,ldv, work, ldwork, cone, c, ldc )
                    end if
                    ! w := w * v2
                    call stdlib_ctrmm( 'RIGHT', 'LOWER', 'NO TRANSPOSE', 'UNIT', n,k, cone, v( 1_ilp, &
                              m-k+1 ), ldv, work, ldwork )
                    ! c2 := c2 - w**h
                    do j = 1, k
                       do i = 1, n
                          c( m-k+j, i ) = c( m-k+j, i ) -conjg( work( i, j ) )
                       end do
                    end do
                 else if( stdlib_lsame( side, 'R' ) ) then
                    ! form  c * h  or  c * h**h  where  c = ( c1  c2 )
                    ! w := c * v**h  =  (c1*v1**h + c2*v2**h)  (stored in work)
                    ! w := c2
                    do j = 1, k
                       call stdlib_ccopy( m, c( 1_ilp, n-k+j ), 1_ilp, work( 1_ilp, j ), 1_ilp )
                    end do
                    ! w := w * v2**h
                    call stdlib_ctrmm( 'RIGHT', 'LOWER', 'CONJUGATE TRANSPOSE','UNIT', m, k, cone,&
                               v( 1_ilp, n-k+1 ), ldv, work,ldwork )
                    if( n>k ) then
                       ! w := w + c1 * v1**h
                       call stdlib_cgemm( 'NO TRANSPOSE', 'CONJUGATE TRANSPOSE', m,k, n-k, cone, &
                                 c, ldc, v, ldv, cone, work,ldwork )
                    end if
                    ! w := w * t  or  w * t**h
                    call stdlib_ctrmm( 'RIGHT', 'LOWER', trans, 'NON-UNIT', m, k,cone, t, ldt, &
                              work, ldwork )
                    ! c := c - w * v
                    if( n>k ) then
                       ! c1 := c1 - w * v1
                       call stdlib_cgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', m, n-k, k,-cone, work, &
                                 ldwork, v, ldv, cone, c, ldc )
                    end if
                    ! w := w * v2
                    call stdlib_ctrmm( 'RIGHT', 'LOWER', 'NO TRANSPOSE', 'UNIT', m,k, cone, v( 1_ilp, &
                              n-k+1 ), ldv, work, ldwork )
                    ! c1 := c1 - w
                    do j = 1, k
                       do i = 1, m
                          c( i, n-k+j ) = c( i, n-k+j ) - work( i, j )
                       end do
                    end do
                 end if
              end if
           end if
           return
     end subroutine stdlib_clarfb

     pure module subroutine stdlib_zlarfb( side, trans, direct, storev, m, n, k, v, ldv,t, ldt, c, ldc, &
     !! ZLARFB applies a complex block reflector H or its transpose H**H to a
     !! complex M-by-N matrix C, from either the left or the right.
               work, ldwork )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: direct, side, storev, trans
           integer(ilp), intent(in) :: k, ldc, ldt, ldv, ldwork, m, n
           ! Array Arguments 
           complex(dp), intent(inout) :: c(ldc,*)
           complex(dp), intent(in) :: t(ldt,*), v(ldv,*)
           complex(dp), intent(out) :: work(ldwork,*)
        ! =====================================================================
           
           ! Local Scalars 
           character :: transt
           integer(ilp) :: i, j
           ! Intrinsic Functions 
           ! Executable Statements 
           ! quick return if possible
           if( m<=0 .or. n<=0 )return
           if( stdlib_lsame( trans, 'N' ) ) then
              transt = 'C'
           else
              transt = 'N'
           end if
           if( stdlib_lsame( storev, 'C' ) ) then
              if( stdlib_lsame( direct, 'F' ) ) then
                 ! let  v =  ( v1 )    (first k rows)
                           ! ( v2 )
                 ! where  v1  is unit lower triangular.
                 if( stdlib_lsame( side, 'L' ) ) then
                    ! form  h * c  or  h**h * c  where  c = ( c1 )
                                                          ! ( c2 )
                    ! w := c**h * v  =  (c1**h * v1 + c2**h * v2)  (stored in work)
                    ! w := c1**h
                    do j = 1, k
                       call stdlib_zcopy( n, c( j, 1_ilp ), ldc, work( 1_ilp, j ), 1_ilp )
                       call stdlib_zlacgv( n, work( 1_ilp, j ), 1_ilp )
                    end do
                    ! w := w * v1
                    call stdlib_ztrmm( 'RIGHT', 'LOWER', 'NO TRANSPOSE', 'UNIT', n,k, cone, v, &
                              ldv, work, ldwork )
                    if( m>k ) then
                       ! w := w + c2**h * v2
                       call stdlib_zgemm( 'CONJUGATE TRANSPOSE', 'NO TRANSPOSE', n,k, m-k, cone, &
                                 c( k+1, 1_ilp ), ldc,v( k+1, 1_ilp ), ldv, cone, work, ldwork )
                    end if
                    ! w := w * t**h  or  w * t
                    call stdlib_ztrmm( 'RIGHT', 'UPPER', transt, 'NON-UNIT', n, k,cone, t, ldt, &
                              work, ldwork )
                    ! c := c - v * w**h
                    if( m>k ) then
                       ! c2 := c2 - v2 * w**h
                       call stdlib_zgemm( 'NO TRANSPOSE', 'CONJUGATE TRANSPOSE',m-k, n, k, -cone, &
                                 v( k+1, 1_ilp ), ldv, work,ldwork, cone, c( k+1, 1_ilp ), ldc )
                    end if
                    ! w := w * v1**h
                    call stdlib_ztrmm( 'RIGHT', 'LOWER', 'CONJUGATE TRANSPOSE','UNIT', n, k, cone,&
                               v, ldv, work, ldwork )
                    ! c1 := c1 - w**h
                    do j = 1, k
                       do i = 1, n
                          c( j, i ) = c( j, i ) - conjg( work( i, j ) )
                       end do
                    end do
                 else if( stdlib_lsame( side, 'R' ) ) then
                    ! form  c * h  or  c * h**h  where  c = ( c1  c2 )
                    ! w := c * v  =  (c1*v1 + c2*v2)  (stored in work)
                    ! w := c1
                    do j = 1, k
                       call stdlib_zcopy( m, c( 1_ilp, j ), 1_ilp, work( 1_ilp, j ), 1_ilp )
                    end do
                    ! w := w * v1
                    call stdlib_ztrmm( 'RIGHT', 'LOWER', 'NO TRANSPOSE', 'UNIT', m,k, cone, v, &
                              ldv, work, ldwork )
                    if( n>k ) then
                       ! w := w + c2 * v2
                       call stdlib_zgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', m, k, n-k,cone, c( 1_ilp, k+&
                                 1_ilp ), ldc, v( k+1, 1_ilp ), ldv,cone, work, ldwork )
                    end if
                    ! w := w * t  or  w * t**h
                    call stdlib_ztrmm( 'RIGHT', 'UPPER', trans, 'NON-UNIT', m, k,cone, t, ldt, &
                              work, ldwork )
                    ! c := c - w * v**h
                    if( n>k ) then
                       ! c2 := c2 - w * v2**h
                       call stdlib_zgemm( 'NO TRANSPOSE', 'CONJUGATE TRANSPOSE', m,n-k, k, -cone, &
                                 work, ldwork, v( k+1, 1_ilp ),ldv, cone, c( 1_ilp, k+1 ), ldc )
                    end if
                    ! w := w * v1**h
                    call stdlib_ztrmm( 'RIGHT', 'LOWER', 'CONJUGATE TRANSPOSE','UNIT', m, k, cone,&
                               v, ldv, work, ldwork )
                    ! c1 := c1 - w
                    do j = 1, k
                       do i = 1, m
                          c( i, j ) = c( i, j ) - work( i, j )
                       end do
                    end do
                 end if
              else
                 ! let  v =  ( v1 )
                           ! ( v2 )    (last k rows)
                 ! where  v2  is unit upper triangular.
                 if( stdlib_lsame( side, 'L' ) ) then
                    ! form  h * c  or  h**h * c  where  c = ( c1 )
                                                          ! ( c2 )
                    ! w := c**h * v  =  (c1**h * v1 + c2**h * v2)  (stored in work)
                    ! w := c2**h
                    do j = 1, k
                       call stdlib_zcopy( n, c( m-k+j, 1_ilp ), ldc, work( 1_ilp, j ), 1_ilp )
                       call stdlib_zlacgv( n, work( 1_ilp, j ), 1_ilp )
                    end do
                    ! w := w * v2
                    call stdlib_ztrmm( 'RIGHT', 'UPPER', 'NO TRANSPOSE', 'UNIT', n,k, cone, v( m-&
                              k+1, 1_ilp ), ldv, work, ldwork )
                    if( m>k ) then
                       ! w := w + c1**h * v1
                       call stdlib_zgemm( 'CONJUGATE TRANSPOSE', 'NO TRANSPOSE', n,k, m-k, cone, &
                                 c, ldc, v, ldv, cone, work,ldwork )
                    end if
                    ! w := w * t**h  or  w * t
                    call stdlib_ztrmm( 'RIGHT', 'LOWER', transt, 'NON-UNIT', n, k,cone, t, ldt, &
                              work, ldwork )
                    ! c := c - v * w**h
                    if( m>k ) then
                       ! c1 := c1 - v1 * w**h
                       call stdlib_zgemm( 'NO TRANSPOSE', 'CONJUGATE TRANSPOSE',m-k, n, k, -cone, &
                                 v, ldv, work, ldwork,cone, c, ldc )
                    end if
                    ! w := w * v2**h
                    call stdlib_ztrmm( 'RIGHT', 'UPPER', 'CONJUGATE TRANSPOSE','UNIT', n, k, cone,&
                               v( m-k+1, 1_ilp ), ldv, work,ldwork )
                    ! c2 := c2 - w**h
                    do j = 1, k
                       do i = 1, n
                          c( m-k+j, i ) = c( m-k+j, i ) -conjg( work( i, j ) )
                       end do
                    end do
                 else if( stdlib_lsame( side, 'R' ) ) then
                    ! form  c * h  or  c * h**h  where  c = ( c1  c2 )
                    ! w := c * v  =  (c1*v1 + c2*v2)  (stored in work)
                    ! w := c2
                    do j = 1, k
                       call stdlib_zcopy( m, c( 1_ilp, n-k+j ), 1_ilp, work( 1_ilp, j ), 1_ilp )
                    end do
                    ! w := w * v2
                    call stdlib_ztrmm( 'RIGHT', 'UPPER', 'NO TRANSPOSE', 'UNIT', m,k, cone, v( n-&
                              k+1, 1_ilp ), ldv, work, ldwork )
                    if( n>k ) then
                       ! w := w + c1 * v1
                       call stdlib_zgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', m, k, n-k,cone, c, ldc, &
                                 v, ldv, cone, work, ldwork )
                    end if
                    ! w := w * t  or  w * t**h
                    call stdlib_ztrmm( 'RIGHT', 'LOWER', trans, 'NON-UNIT', m, k,cone, t, ldt, &
                              work, ldwork )
                    ! c := c - w * v**h
                    if( n>k ) then
                       ! c1 := c1 - w * v1**h
                       call stdlib_zgemm( 'NO TRANSPOSE', 'CONJUGATE TRANSPOSE', m,n-k, k, -cone, &
                                 work, ldwork, v, ldv, cone,c, ldc )
                    end if
                    ! w := w * v2**h
                    call stdlib_ztrmm( 'RIGHT', 'UPPER', 'CONJUGATE TRANSPOSE','UNIT', m, k, cone,&
                               v( n-k+1, 1_ilp ), ldv, work,ldwork )
                    ! c2 := c2 - w
                    do j = 1, k
                       do i = 1, m
                          c( i, n-k+j ) = c( i, n-k+j ) - work( i, j )
                       end do
                    end do
                 end if
              end if
           else if( stdlib_lsame( storev, 'R' ) ) then
              if( stdlib_lsame( direct, 'F' ) ) then
                 ! let  v =  ( v1  v2 )    (v1: first k columns)
                 ! where  v1  is unit upper triangular.
                 if( stdlib_lsame( side, 'L' ) ) then
                    ! form  h * c  or  h**h * c  where  c = ( c1 )
                                                          ! ( c2 )
                    ! w := c**h * v**h  =  (c1**h * v1**h + c2**h * v2**h) (stored in work)
                    ! w := c1**h
                    do j = 1, k
                       call stdlib_zcopy( n, c( j, 1_ilp ), ldc, work( 1_ilp, j ), 1_ilp )
                       call stdlib_zlacgv( n, work( 1_ilp, j ), 1_ilp )
                    end do
                    ! w := w * v1**h
                    call stdlib_ztrmm( 'RIGHT', 'UPPER', 'CONJUGATE TRANSPOSE','UNIT', n, k, cone,&
                               v, ldv, work, ldwork )
                    if( m>k ) then
                       ! w := w + c2**h * v2**h
                       call stdlib_zgemm( 'CONJUGATE TRANSPOSE','CONJUGATE TRANSPOSE', n, k, m-k, &
                                 cone,c( k+1, 1_ilp ), ldc, v( 1_ilp, k+1 ), ldv, cone,work, ldwork )
                    end if
                    ! w := w * t**h  or  w * t
                    call stdlib_ztrmm( 'RIGHT', 'UPPER', transt, 'NON-UNIT', n, k,cone, t, ldt, &
                              work, ldwork )
                    ! c := c - v**h * w**h
                    if( m>k ) then
                       ! c2 := c2 - v2**h * w**h
                       call stdlib_zgemm( 'CONJUGATE TRANSPOSE','CONJUGATE TRANSPOSE', m-k, n, k, &
                                 -cone,v( 1_ilp, k+1 ), ldv, work, ldwork, cone,c( k+1, 1_ilp ), ldc )
                    end if
                    ! w := w * v1
                    call stdlib_ztrmm( 'RIGHT', 'UPPER', 'NO TRANSPOSE', 'UNIT', n,k, cone, v, &
                              ldv, work, ldwork )
                    ! c1 := c1 - w**h
                    do j = 1, k
                       do i = 1, n
                          c( j, i ) = c( j, i ) - conjg( work( i, j ) )
                       end do
                    end do
                 else if( stdlib_lsame( side, 'R' ) ) then
                    ! form  c * h  or  c * h**h  where  c = ( c1  c2 )
                    ! w := c * v**h  =  (c1*v1**h + c2*v2**h)  (stored in work)
                    ! w := c1
                    do j = 1, k
                       call stdlib_zcopy( m, c( 1_ilp, j ), 1_ilp, work( 1_ilp, j ), 1_ilp )
                    end do
                    ! w := w * v1**h
                    call stdlib_ztrmm( 'RIGHT', 'UPPER', 'CONJUGATE TRANSPOSE','UNIT', m, k, cone,&
                               v, ldv, work, ldwork )
                    if( n>k ) then
                       ! w := w + c2 * v2**h
                       call stdlib_zgemm( 'NO TRANSPOSE', 'CONJUGATE TRANSPOSE', m,k, n-k, cone, &
                                 c( 1_ilp, k+1 ), ldc,v( 1_ilp, k+1 ), ldv, cone, work, ldwork )
                    end if
                    ! w := w * t  or  w * t**h
                    call stdlib_ztrmm( 'RIGHT', 'UPPER', trans, 'NON-UNIT', m, k,cone, t, ldt, &
                              work, ldwork )
                    ! c := c - w * v
                    if( n>k ) then
                       ! c2 := c2 - w * v2
                       call stdlib_zgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', m, n-k, k,-cone, work, &
                                 ldwork, v( 1_ilp, k+1 ), ldv, cone,c( 1_ilp, k+1 ), ldc )
                    end if
                    ! w := w * v1
                    call stdlib_ztrmm( 'RIGHT', 'UPPER', 'NO TRANSPOSE', 'UNIT', m,k, cone, v, &
                              ldv, work, ldwork )
                    ! c1 := c1 - w
                    do j = 1, k
                       do i = 1, m
                          c( i, j ) = c( i, j ) - work( i, j )
                       end do
                    end do
                 end if
              else
                 ! let  v =  ( v1  v2 )    (v2: last k columns)
                 ! where  v2  is unit lower triangular.
                 if( stdlib_lsame( side, 'L' ) ) then
                    ! form  h * c  or  h**h * c  where  c = ( c1 )
                                                          ! ( c2 )
                    ! w := c**h * v**h  =  (c1**h * v1**h + c2**h * v2**h) (stored in work)
                    ! w := c2**h
                    do j = 1, k
                       call stdlib_zcopy( n, c( m-k+j, 1_ilp ), ldc, work( 1_ilp, j ), 1_ilp )
                       call stdlib_zlacgv( n, work( 1_ilp, j ), 1_ilp )
                    end do
                    ! w := w * v2**h
                    call stdlib_ztrmm( 'RIGHT', 'LOWER', 'CONJUGATE TRANSPOSE','UNIT', n, k, cone,&
                               v( 1_ilp, m-k+1 ), ldv, work,ldwork )
                    if( m>k ) then
                       ! w := w + c1**h * v1**h
                       call stdlib_zgemm( 'CONJUGATE TRANSPOSE','CONJUGATE TRANSPOSE', n, k, m-k, &
                                 cone, c,ldc, v, ldv, cone, work, ldwork )
                    end if
                    ! w := w * t**h  or  w * t
                    call stdlib_ztrmm( 'RIGHT', 'LOWER', transt, 'NON-UNIT', n, k,cone, t, ldt, &
                              work, ldwork )
                    ! c := c - v**h * w**h
                    if( m>k ) then
                       ! c1 := c1 - v1**h * w**h
                       call stdlib_zgemm( 'CONJUGATE TRANSPOSE','CONJUGATE TRANSPOSE', m-k, n, k, &
                                 -cone, v,ldv, work, ldwork, cone, c, ldc )
                    end if
                    ! w := w * v2
                    call stdlib_ztrmm( 'RIGHT', 'LOWER', 'NO TRANSPOSE', 'UNIT', n,k, cone, v( 1_ilp, &
                              m-k+1 ), ldv, work, ldwork )
                    ! c2 := c2 - w**h
                    do j = 1, k
                       do i = 1, n
                          c( m-k+j, i ) = c( m-k+j, i ) -conjg( work( i, j ) )
                       end do
                    end do
                 else if( stdlib_lsame( side, 'R' ) ) then
                    ! form  c * h  or  c * h**h  where  c = ( c1  c2 )
                    ! w := c * v**h  =  (c1*v1**h + c2*v2**h)  (stored in work)
                    ! w := c2
                    do j = 1, k
                       call stdlib_zcopy( m, c( 1_ilp, n-k+j ), 1_ilp, work( 1_ilp, j ), 1_ilp )
                    end do
                    ! w := w * v2**h
                    call stdlib_ztrmm( 'RIGHT', 'LOWER', 'CONJUGATE TRANSPOSE','UNIT', m, k, cone,&
                               v( 1_ilp, n-k+1 ), ldv, work,ldwork )
                    if( n>k ) then
                       ! w := w + c1 * v1**h
                       call stdlib_zgemm( 'NO TRANSPOSE', 'CONJUGATE TRANSPOSE', m,k, n-k, cone, &
                                 c, ldc, v, ldv, cone, work,ldwork )
                    end if
                    ! w := w * t  or  w * t**h
                    call stdlib_ztrmm( 'RIGHT', 'LOWER', trans, 'NON-UNIT', m, k,cone, t, ldt, &
                              work, ldwork )
                    ! c := c - w * v
                    if( n>k ) then
                       ! c1 := c1 - w * v1
                       call stdlib_zgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', m, n-k, k,-cone, work, &
                                 ldwork, v, ldv, cone, c, ldc )
                    end if
                    ! w := w * v2
                    call stdlib_ztrmm( 'RIGHT', 'LOWER', 'NO TRANSPOSE', 'UNIT', m,k, cone, v( 1_ilp, &
                              n-k+1 ), ldv, work, ldwork )
                    ! c1 := c1 - w
                    do j = 1, k
                       do i = 1, m
                          c( i, n-k+j ) = c( i, n-k+j ) - work( i, j )
                       end do
                    end do
                 end if
              end if
           end if
           return
     end subroutine stdlib_zlarfb




     pure module subroutine stdlib_slarfg( n, alpha, x, incx, tau )
     !! SLARFG generates a real elementary reflector H of order n, such
     !! that
     !! H * ( alpha ) = ( beta ),   H**T * H = I.
     !! (   x   )   (   0  )
     !! where alpha and beta are scalars, and x is an (n-1)-element real
     !! vector. H is represented in the form
     !! H = I - tau * ( 1 ) * ( 1 v**T ) ,
     !! ( v )
     !! where tau is a real scalar and v is a real (n-1)-element
     !! vector.
     !! If the elements of x are all zero, then tau = 0 and H is taken to be
     !! the unit matrix.
     !! Otherwise  1 <= tau <= 2.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx, n
           real(sp), intent(inout) :: alpha
           real(sp), intent(out) :: tau
           ! Array Arguments 
           real(sp), intent(inout) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: j, knt
           real(sp) :: beta, rsafmn, safmin, xnorm
           ! Intrinsic Functions 
           ! Executable Statements 
           if( n<=1_ilp ) then
              tau = zero
              return
           end if
           xnorm = stdlib_snrm2( n-1, x, incx )
           if( xnorm==zero ) then
              ! h  =  i
              tau = zero
           else
              ! general case
              beta = -sign( stdlib_slapy2( alpha, xnorm ), alpha )
              safmin = stdlib_slamch( 'S' ) / stdlib_slamch( 'E' )
              knt = 0_ilp
              if( abs( beta )<safmin ) then
                 ! xnorm, beta may be inaccurate; scale x and recompute them
                 rsafmn = one / safmin
                 10 continue
                 knt = knt + 1_ilp
                 call stdlib_sscal( n-1, rsafmn, x, incx )
                 beta = beta*rsafmn
                 alpha = alpha*rsafmn
                 if( (abs( beta )<safmin) .and. (knt < 20) )go to 10
                 ! new beta is at most 1, at least safmin
                 xnorm = stdlib_snrm2( n-1, x, incx )
                 beta = -sign( stdlib_slapy2( alpha, xnorm ), alpha )
              end if
              tau = ( beta-alpha ) / beta
              call stdlib_sscal( n-1, one / ( alpha-beta ), x, incx )
              ! if alpha is subnormal, it may lose relative accuracy
              do j = 1, knt
                 beta = beta*safmin
              end do
              alpha = beta
           end if
           return
     end subroutine stdlib_slarfg

     pure module subroutine stdlib_dlarfg( n, alpha, x, incx, tau )
     !! DLARFG generates a real elementary reflector H of order n, such
     !! that
     !! H * ( alpha ) = ( beta ),   H**T * H = I.
     !! (   x   )   (   0  )
     !! where alpha and beta are scalars, and x is an (n-1)-element real
     !! vector. H is represented in the form
     !! H = I - tau * ( 1 ) * ( 1 v**T ) ,
     !! ( v )
     !! where tau is a real scalar and v is a real (n-1)-element
     !! vector.
     !! If the elements of x are all zero, then tau = 0 and H is taken to be
     !! the unit matrix.
     !! Otherwise  1 <= tau <= 2.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx, n
           real(dp), intent(inout) :: alpha
           real(dp), intent(out) :: tau
           ! Array Arguments 
           real(dp), intent(inout) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: j, knt
           real(dp) :: beta, rsafmn, safmin, xnorm
           ! Intrinsic Functions 
           ! Executable Statements 
           if( n<=1_ilp ) then
              tau = zero
              return
           end if
           xnorm = stdlib_dnrm2( n-1, x, incx )
           if( xnorm==zero ) then
              ! h  =  i
              tau = zero
           else
              ! general case
              beta = -sign( stdlib_dlapy2( alpha, xnorm ), alpha )
              safmin = stdlib_dlamch( 'S' ) / stdlib_dlamch( 'E' )
              knt = 0_ilp
              if( abs( beta )<safmin ) then
                 ! xnorm, beta may be inaccurate; scale x and recompute them
                 rsafmn = one / safmin
                 10 continue
                 knt = knt + 1_ilp
                 call stdlib_dscal( n-1, rsafmn, x, incx )
                 beta = beta*rsafmn
                 alpha = alpha*rsafmn
                 if( (abs( beta )<safmin) .and. (knt < 20) )go to 10
                 ! new beta is at most 1, at least safmin
                 xnorm = stdlib_dnrm2( n-1, x, incx )
                 beta = -sign( stdlib_dlapy2( alpha, xnorm ), alpha )
              end if
              tau = ( beta-alpha ) / beta
              call stdlib_dscal( n-1, one / ( alpha-beta ), x, incx )
              ! if alpha is subnormal, it may lose relative accuracy
              do j = 1, knt
                 beta = beta*safmin
              end do
              alpha = beta
           end if
           return
     end subroutine stdlib_dlarfg


     pure module subroutine stdlib_clarfg( n, alpha, x, incx, tau )
     !! CLARFG generates a complex elementary reflector H of order n, such
     !! that
     !! H**H * ( alpha ) = ( beta ),   H**H * H = I.
     !! (   x   )   (   0  )
     !! where alpha and beta are scalars, with beta real, and x is an
     !! (n-1)-element complex vector. H is represented in the form
     !! H = I - tau * ( 1 ) * ( 1 v**H ) ,
     !! ( v )
     !! where tau is a complex scalar and v is a complex (n-1)-element
     !! vector. Note that H is not hermitian.
     !! If the elements of x are all zero and alpha is real, then tau = 0
     !! and H is taken to be the unit matrix.
     !! Otherwise  1 <= real(tau) <= 2  and  abs(tau-1) <= 1 .
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx, n
           complex(sp), intent(inout) :: alpha
           complex(sp), intent(out) :: tau
           ! Array Arguments 
           complex(sp), intent(inout) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: j, knt
           real(sp) :: alphi, alphr, beta, rsafmn, safmin, xnorm
           ! Intrinsic Functions 
           ! Executable Statements 
           if( n<=0_ilp ) then
              tau = zero
              return
           end if
           xnorm = stdlib_scnrm2( n-1, x, incx )
           alphr = real( alpha,KIND=sp)
           alphi = aimag( alpha )
           if( xnorm==zero .and. alphi==zero ) then
              ! h  =  i
              tau = zero
           else
              ! general case
              beta = -sign( stdlib_slapy3( alphr, alphi, xnorm ), alphr )
              safmin = stdlib_slamch( 'S' ) / stdlib_slamch( 'E' )
              rsafmn = one / safmin
              knt = 0_ilp
              if( abs( beta )<safmin ) then
                 ! xnorm, beta may be inaccurate; scale x and recompute them
                 10 continue
                 knt = knt + 1_ilp
                 call stdlib_csscal( n-1, rsafmn, x, incx )
                 beta = beta*rsafmn
                 alphi = alphi*rsafmn
                 alphr = alphr*rsafmn
                 if( (abs( beta )<safmin) .and. (knt < 20) )go to 10
                 ! new beta is at most 1, at least safmin
                 xnorm = stdlib_scnrm2( n-1, x, incx )
                 alpha = cmplx( alphr, alphi,KIND=sp)
                 beta = -sign( stdlib_slapy3( alphr, alphi, xnorm ), alphr )
              end if
              tau = cmplx( ( beta-alphr ) / beta, -alphi / beta,KIND=sp)
              alpha = stdlib_cladiv( cmplx( one,KIND=sp), alpha-beta )
              call stdlib_cscal( n-1, alpha, x, incx )
              ! if alpha is subnormal, it may lose relative accuracy
              do j = 1, knt
                 beta = beta*safmin
              end do
              alpha = beta
           end if
           return
     end subroutine stdlib_clarfg

     pure module subroutine stdlib_zlarfg( n, alpha, x, incx, tau )
     !! ZLARFG generates a complex elementary reflector H of order n, such
     !! that
     !! H**H * ( alpha ) = ( beta ),   H**H * H = I.
     !! (   x   )   (   0  )
     !! where alpha and beta are scalars, with beta real, and x is an
     !! (n-1)-element complex vector. H is represented in the form
     !! H = I - tau * ( 1 ) * ( 1 v**H ) ,
     !! ( v )
     !! where tau is a complex scalar and v is a complex (n-1)-element
     !! vector. Note that H is not hermitian.
     !! If the elements of x are all zero and alpha is real, then tau = 0
     !! and H is taken to be the unit matrix.
     !! Otherwise  1 <= real(tau) <= 2  and  abs(tau-1) <= 1 .
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx, n
           complex(dp), intent(inout) :: alpha
           complex(dp), intent(out) :: tau
           ! Array Arguments 
           complex(dp), intent(inout) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: j, knt
           real(dp) :: alphi, alphr, beta, rsafmn, safmin, xnorm
           ! Intrinsic Functions 
           ! Executable Statements 
           if( n<=0_ilp ) then
              tau = zero
              return
           end if
           xnorm = stdlib_dznrm2( n-1, x, incx )
           alphr = real( alpha,KIND=dp)
           alphi = aimag( alpha )
           if( xnorm==zero .and. alphi==zero ) then
              ! h  =  i
              tau = zero
           else
              ! general case
              beta = -sign( stdlib_dlapy3( alphr, alphi, xnorm ), alphr )
              safmin = stdlib_dlamch( 'S' ) / stdlib_dlamch( 'E' )
              rsafmn = one / safmin
              knt = 0_ilp
              if( abs( beta )<safmin ) then
                 ! xnorm, beta may be inaccurate; scale x and recompute them
                 10 continue
                 knt = knt + 1_ilp
                 call stdlib_zdscal( n-1, rsafmn, x, incx )
                 beta = beta*rsafmn
                 alphi = alphi*rsafmn
                 alphr = alphr*rsafmn
                 if( (abs( beta )<safmin) .and. (knt < 20) )go to 10
                 ! new beta is at most 1, at least safmin
                 xnorm = stdlib_dznrm2( n-1, x, incx )
                 alpha = cmplx( alphr, alphi,KIND=dp)
                 beta = -sign( stdlib_dlapy3( alphr, alphi, xnorm ), alphr )
              end if
              tau = cmplx( ( beta-alphr ) / beta, -alphi / beta,KIND=dp)
              alpha = stdlib_zladiv( cmplx( one,KIND=dp), alpha-beta )
              call stdlib_zscal( n-1, alpha, x, incx )
              ! if alpha is subnormal, it may lose relative accuracy
              do j = 1, knt
                 beta = beta*safmin
              end do
              alpha = beta
           end if
           return
     end subroutine stdlib_zlarfg




     module subroutine stdlib_slarfgp( n, alpha, x, incx, tau )
     !! SLARFGP generates a real elementary reflector H of order n, such
     !! that
     !! H * ( alpha ) = ( beta ),   H**T * H = I.
     !! (   x   )   (   0  )
     !! where alpha and beta are scalars, beta is non-negative, and x is
     !! an (n-1)-element real vector.  H is represented in the form
     !! H = I - tau * ( 1 ) * ( 1 v**T ) ,
     !! ( v )
     !! where tau is a real scalar and v is a real (n-1)-element
     !! vector.
     !! If the elements of x are all zero, then tau = 0 and H is taken to be
     !! the unit matrix.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx, n
           real(sp), intent(inout) :: alpha
           real(sp), intent(out) :: tau
           ! Array Arguments 
           real(sp), intent(inout) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: j, knt
           real(sp) :: beta, bignum, savealpha, smlnum, xnorm
           ! Intrinsic Functions 
           ! Executable Statements 
           if( n<=0_ilp ) then
              tau = zero
              return
           end if
           xnorm = stdlib_snrm2( n-1, x, incx )
           if( xnorm==zero ) then
              ! h  =  [+/-1, 0; i], sign chosen so alpha >= 0.
              if( alpha>=zero ) then
                 ! when tau.eq.zero, the vector is special-cased to be
                 ! all zeros in the application routines.  we do not need
                 ! to clear it.
                 tau = zero
              else
                 ! however, the application routines rely on explicit
                 ! zero checks when tau.ne.zero, and we must clear x.
                 tau = two
                 do j = 1, n-1
                    x( 1_ilp + (j-1)*incx ) = 0_ilp
                 end do
                 alpha = -alpha
              end if
           else
              ! general case
              beta = sign( stdlib_slapy2( alpha, xnorm ), alpha )
              smlnum = stdlib_slamch( 'S' ) / stdlib_slamch( 'E' )
              knt = 0_ilp
              if( abs( beta )<smlnum ) then
                 ! xnorm, beta may be inaccurate; scale x and recompute them
                 bignum = one / smlnum
                 10 continue
                 knt = knt + 1_ilp
                 call stdlib_sscal( n-1, bignum, x, incx )
                 beta = beta*bignum
                 alpha = alpha*bignum
                 if( (abs( beta )<smlnum) .and. (knt < 20) )go to 10
                 ! new beta is at most 1, at least smlnum
                 xnorm = stdlib_snrm2( n-1, x, incx )
                 beta = sign( stdlib_slapy2( alpha, xnorm ), alpha )
              end if
              savealpha = alpha
              alpha = alpha + beta
              if( beta<zero ) then
                 beta = -beta
                 tau = -alpha / beta
              else
                 alpha = xnorm * (xnorm/alpha)
                 tau = alpha / beta
                 alpha = -alpha
              end if
              if ( abs(tau)<=smlnum ) then
                 ! in the case where the computed tau ends up being a denormalized number,
                 ! it loses relative accuracy. this is a big problem. solution: flush tau
                 ! to zero. this explains the next if statement.
                 ! (bug report provided by pat quillen from mathworks on jul 29, 2009.)
                 ! (thanks pat. thanks mathworks.)
                 if( savealpha>=zero ) then
                    tau = zero
                 else
                    tau = two
                    do j = 1, n-1
                       x( 1_ilp + (j-1)*incx ) = 0_ilp
                    end do
                    beta = -savealpha
                 end if
              else
                 ! this is the general case.
                 call stdlib_sscal( n-1, one / alpha, x, incx )
              end if
              ! if beta is subnormal, it may lose relative accuracy
              do j = 1, knt
                 beta = beta*smlnum
              end do
              alpha = beta
           end if
           return
     end subroutine stdlib_slarfgp

     module subroutine stdlib_dlarfgp( n, alpha, x, incx, tau )
     !! DLARFGP generates a real elementary reflector H of order n, such
     !! that
     !! H * ( alpha ) = ( beta ),   H**T * H = I.
     !! (   x   )   (   0  )
     !! where alpha and beta are scalars, beta is non-negative, and x is
     !! an (n-1)-element real vector.  H is represented in the form
     !! H = I - tau * ( 1 ) * ( 1 v**T ) ,
     !! ( v )
     !! where tau is a real scalar and v is a real (n-1)-element
     !! vector.
     !! If the elements of x are all zero, then tau = 0 and H is taken to be
     !! the unit matrix.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx, n
           real(dp), intent(inout) :: alpha
           real(dp), intent(out) :: tau
           ! Array Arguments 
           real(dp), intent(inout) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: j, knt
           real(dp) :: beta, bignum, savealpha, smlnum, xnorm
           ! Intrinsic Functions 
           ! Executable Statements 
           if( n<=0_ilp ) then
              tau = zero
              return
           end if
           xnorm = stdlib_dnrm2( n-1, x, incx )
           if( xnorm==zero ) then
              ! h  =  [+/-1, 0; i], sign chosen so alpha >= 0
              if( alpha>=zero ) then
                 ! when tau.eq.zero, the vector is special-cased to be
                 ! all zeros in the application routines.  we do not need
                 ! to clear it.
                 tau = zero
              else
                 ! however, the application routines rely on explicit
                 ! zero checks when tau.ne.zero, and we must clear x.
                 tau = two
                 do j = 1, n-1
                    x( 1_ilp + (j-1)*incx ) = 0_ilp
                 end do
                 alpha = -alpha
              end if
           else
              ! general case
              beta = sign( stdlib_dlapy2( alpha, xnorm ), alpha )
              smlnum = stdlib_dlamch( 'S' ) / stdlib_dlamch( 'E' )
              knt = 0_ilp
              if( abs( beta )<smlnum ) then
                 ! xnorm, beta may be inaccurate; scale x and recompute them
                 bignum = one / smlnum
                 10 continue
                 knt = knt + 1_ilp
                 call stdlib_dscal( n-1, bignum, x, incx )
                 beta = beta*bignum
                 alpha = alpha*bignum
                 if( (abs( beta )<smlnum) .and. (knt < 20) )go to 10
                 ! new beta is at most 1, at least smlnum
                 xnorm = stdlib_dnrm2( n-1, x, incx )
                 beta = sign( stdlib_dlapy2( alpha, xnorm ), alpha )
              end if
              savealpha = alpha
              alpha = alpha + beta
              if( beta<zero ) then
                 beta = -beta
                 tau = -alpha / beta
              else
                 alpha = xnorm * (xnorm/alpha)
                 tau = alpha / beta
                 alpha = -alpha
              end if
              if ( abs(tau)<=smlnum ) then
                 ! in the case where the computed tau ends up being a denormalized number,
                 ! it loses relative accuracy. this is a big problem. solution: flush tau
                 ! to zero. this explains the next if statement.
                 ! (bug report provided by pat quillen from mathworks on jul 29, 2009.)
                 ! (thanks pat. thanks mathworks.)
                 if( savealpha>=zero ) then
                    tau = zero
                 else
                    tau = two
                    do j = 1, n-1
                       x( 1_ilp + (j-1)*incx ) = 0_ilp
                    end do
                    beta = -savealpha
                 end if
              else
                 ! this is the general case.
                 call stdlib_dscal( n-1, one / alpha, x, incx )
              end if
              ! if beta is subnormal, it may lose relative accuracy
              do j = 1, knt
                 beta = beta*smlnum
              end do
              alpha = beta
           end if
           return
     end subroutine stdlib_dlarfgp


     module subroutine stdlib_clarfgp( n, alpha, x, incx, tau )
     !! CLARFGP generates a complex elementary reflector H of order n, such
     !! that
     !! H**H * ( alpha ) = ( beta ),   H**H * H = I.
     !! (   x   )   (   0  )
     !! where alpha and beta are scalars, beta is real and non-negative, and
     !! x is an (n-1)-element complex vector.  H is represented in the form
     !! H = I - tau * ( 1 ) * ( 1 v**H ) ,
     !! ( v )
     !! where tau is a complex scalar and v is a complex (n-1)-element
     !! vector. Note that H is not hermitian.
     !! If the elements of x are all zero and alpha is real, then tau = 0
     !! and H is taken to be the unit matrix.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx, n
           complex(sp), intent(inout) :: alpha
           complex(sp), intent(out) :: tau
           ! Array Arguments 
           complex(sp), intent(inout) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: j, knt
           real(sp) :: alphi, alphr, beta, bignum, smlnum, xnorm
           complex(sp) :: savealpha
           ! Intrinsic Functions 
           ! Executable Statements 
           if( n<=0_ilp ) then
              tau = zero
              return
           end if
           xnorm = stdlib_scnrm2( n-1, x, incx )
           alphr = real( alpha,KIND=sp)
           alphi = aimag( alpha )
           if( xnorm==zero ) then
              ! h  =  [1-alpha/abs(alpha) 0; 0 i], sign chosen so alpha >= 0.
              if( alphi==zero ) then
                 if( alphr>=zero ) then
                    ! when tau.eq.zero, the vector is special-cased to be
                    ! all zeros in the application routines.  we do not need
                    ! to clear it.
                    tau = zero
                 else
                    ! however, the application routines rely on explicit
                    ! zero checks when tau.ne.zero, and we must clear x.
                    tau = two
                    do j = 1, n-1
                       x( 1_ilp + (j-1)*incx ) = zero
                    end do
                    alpha = -alpha
                 end if
              else
                 ! only "reflecting" the diagonal entry to be real and non-negative.
                 xnorm = stdlib_slapy2( alphr, alphi )
                 tau = cmplx( one - alphr / xnorm, -alphi / xnorm,KIND=sp)
                 do j = 1, n-1
                    x( 1_ilp + (j-1)*incx ) = zero
                 end do
                 alpha = xnorm
              end if
           else
              ! general case
              beta = sign( stdlib_slapy3( alphr, alphi, xnorm ), alphr )
              smlnum = stdlib_slamch( 'S' ) / stdlib_slamch( 'E' )
              bignum = one / smlnum
              knt = 0_ilp
              if( abs( beta )<smlnum ) then
                 ! xnorm, beta may be inaccurate; scale x and recompute them
                 10 continue
                 knt = knt + 1_ilp
                 call stdlib_csscal( n-1, bignum, x, incx )
                 beta = beta*bignum
                 alphi = alphi*bignum
                 alphr = alphr*bignum
                 if( (abs( beta )<smlnum) .and. (knt < 20) )go to 10
                 ! new beta is at most 1, at least smlnum
                 xnorm = stdlib_scnrm2( n-1, x, incx )
                 alpha = cmplx( alphr, alphi,KIND=sp)
                 beta = sign( stdlib_slapy3( alphr, alphi, xnorm ), alphr )
              end if
              savealpha = alpha
              alpha = alpha + beta
              if( beta<zero ) then
                 beta = -beta
                 tau = -alpha / beta
              else
                 alphr = alphi * (alphi/real( alpha,KIND=sp))
                 alphr = alphr + xnorm * (xnorm/real( alpha,KIND=sp))
                 tau = cmplx( alphr/beta, -alphi/beta,KIND=sp)
                 alpha = cmplx( -alphr, alphi,KIND=sp)
              end if
              alpha = stdlib_cladiv( cmplx( one,KIND=sp), alpha )
              if ( abs(tau)<=smlnum ) then
                 ! in the case where the computed tau ends up being a denormalized number,
                 ! it loses relative accuracy. this is a big problem. solution: flush tau
                 ! to zero (or two or whatever makes a nonnegative real number for beta).
                 ! (bug report provided by pat quillen from mathworks on jul 29, 2009.)
                 ! (thanks pat. thanks mathworks.)
                 alphr = real( savealpha,KIND=sp)
                 alphi = aimag( savealpha )
                 if( alphi==zero ) then
                    if( alphr>=zero ) then
                       tau = zero
                    else
                       tau = two
                       do j = 1, n-1
                          x( 1_ilp + (j-1)*incx ) = zero
                       end do
                       beta = real( -savealpha,KIND=sp)
                    end if
                 else
                    xnorm = stdlib_slapy2( alphr, alphi )
                    tau = cmplx( one - alphr / xnorm, -alphi / xnorm,KIND=sp)
                    do j = 1, n-1
                       x( 1_ilp + (j-1)*incx ) = zero
                    end do
                    beta = xnorm
                 end if
              else
                 ! this is the general case.
                 call stdlib_cscal( n-1, alpha, x, incx )
              end if
              ! if beta is subnormal, it may lose relative accuracy
              do j = 1, knt
                 beta = beta*smlnum
              end do
              alpha = beta
           end if
           return
     end subroutine stdlib_clarfgp

     module subroutine stdlib_zlarfgp( n, alpha, x, incx, tau )
     !! ZLARFGP generates a complex elementary reflector H of order n, such
     !! that
     !! H**H * ( alpha ) = ( beta ),   H**H * H = I.
     !! (   x   )   (   0  )
     !! where alpha and beta are scalars, beta is real and non-negative, and
     !! x is an (n-1)-element complex vector.  H is represented in the form
     !! H = I - tau * ( 1 ) * ( 1 v**H ) ,
     !! ( v )
     !! where tau is a complex scalar and v is a complex (n-1)-element
     !! vector. Note that H is not hermitian.
     !! If the elements of x are all zero and alpha is real, then tau = 0
     !! and H is taken to be the unit matrix.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx, n
           complex(dp), intent(inout) :: alpha
           complex(dp), intent(out) :: tau
           ! Array Arguments 
           complex(dp), intent(inout) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: j, knt
           real(dp) :: alphi, alphr, beta, bignum, smlnum, xnorm
           complex(dp) :: savealpha
           ! Intrinsic Functions 
           ! Executable Statements 
           if( n<=0_ilp ) then
              tau = zero
              return
           end if
           xnorm = stdlib_dznrm2( n-1, x, incx )
           alphr = real( alpha,KIND=dp)
           alphi = aimag( alpha )
           if( xnorm==zero ) then
              ! h  =  [1-alpha/abs(alpha) 0; 0 i], sign chosen so alpha >= 0.
              if( alphi==zero ) then
                 if( alphr>=zero ) then
                    ! when tau.eq.zero, the vector is special-cased to be
                    ! all zeros in the application routines.  we do not need
                    ! to clear it.
                    tau = zero
                 else
                    ! however, the application routines rely on explicit
                    ! zero checks when tau.ne.zero, and we must clear x.
                    tau = two
                    do j = 1, n-1
                       x( 1_ilp + (j-1)*incx ) = zero
                    end do
                    alpha = -alpha
                 end if
              else
                 ! only "reflecting" the diagonal entry to be real and non-negative.
                 xnorm = stdlib_dlapy2( alphr, alphi )
                 tau = cmplx( one - alphr / xnorm, -alphi / xnorm,KIND=dp)
                 do j = 1, n-1
                    x( 1_ilp + (j-1)*incx ) = zero
                 end do
                 alpha = xnorm
              end if
           else
              ! general case
              beta = sign( stdlib_dlapy3( alphr, alphi, xnorm ), alphr )
              smlnum = stdlib_dlamch( 'S' ) / stdlib_dlamch( 'E' )
              bignum = one / smlnum
              knt = 0_ilp
              if( abs( beta )<smlnum ) then
                 ! xnorm, beta may be inaccurate; scale x and recompute them
                 10 continue
                 knt = knt + 1_ilp
                 call stdlib_zdscal( n-1, bignum, x, incx )
                 beta = beta*bignum
                 alphi = alphi*bignum
                 alphr = alphr*bignum
                 if( (abs( beta )<smlnum) .and. (knt < 20) )go to 10
                 ! new beta is at most 1, at least smlnum
                 xnorm = stdlib_dznrm2( n-1, x, incx )
                 alpha = cmplx( alphr, alphi,KIND=dp)
                 beta = sign( stdlib_dlapy3( alphr, alphi, xnorm ), alphr )
              end if
              savealpha = alpha
              alpha = alpha + beta
              if( beta<zero ) then
                 beta = -beta
                 tau = -alpha / beta
              else
                 alphr = alphi * (alphi/real( alpha,KIND=dp))
                 alphr = alphr + xnorm * (xnorm/real( alpha,KIND=dp))
                 tau = cmplx( alphr/beta, -alphi/beta,KIND=dp)
                 alpha = cmplx( -alphr, alphi,KIND=dp)
              end if
              alpha = stdlib_zladiv( cmplx( one,KIND=dp), alpha )
              if ( abs(tau)<=smlnum ) then
                 ! in the case where the computed tau ends up being a denormalized number,
                 ! it loses relative accuracy. this is a big problem. solution: flush tau
                 ! to zero (or two or whatever makes a nonnegative real number for beta).
                 ! (bug report provided by pat quillen from mathworks on jul 29, 2009.)
                 ! (thanks pat. thanks mathworks.)
                 alphr = real( savealpha,KIND=dp)
                 alphi = aimag( savealpha )
                 if( alphi==zero ) then
                    if( alphr>=zero ) then
                       tau = zero
                    else
                       tau = two
                       do j = 1, n-1
                          x( 1_ilp + (j-1)*incx ) = zero
                       end do
                       beta = real( -savealpha,KIND=dp)
                    end if
                 else
                    xnorm = stdlib_dlapy2( alphr, alphi )
                    tau = cmplx( one - alphr / xnorm, -alphi / xnorm,KIND=dp)
                    do j = 1, n-1
                       x( 1_ilp + (j-1)*incx ) = zero
                    end do
                    beta = xnorm
                 end if
              else
                 ! this is the general case.
                 call stdlib_zscal( n-1, alpha, x, incx )
              end if
              ! if beta is subnormal, it may lose relative accuracy
              do j = 1, knt
                 beta = beta*smlnum
              end do
              alpha = beta
           end if
           return
     end subroutine stdlib_zlarfgp




     pure module subroutine stdlib_slarft( direct, storev, n, k, v, ldv, tau, t, ldt )
     !! SLARFT forms the triangular factor T of a real block reflector H
     !! of order n, which is defined as a product of k elementary reflectors.
     !! If DIRECT = 'F', H = H(1) H(2) . . . H(k) and T is upper triangular;
     !! If DIRECT = 'B', H = H(k) . . . H(2) H(1) and T is lower triangular.
     !! If STOREV = 'C', the vector which defines the elementary reflector
     !! H(i) is stored in the i-th column of the array V, and
     !! H  =  I - V * T * V**T
     !! If STOREV = 'R', the vector which defines the elementary reflector
     !! H(i) is stored in the i-th row of the array V, and
     !! H  =  I - V**T * T * V
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: direct, storev
           integer(ilp), intent(in) :: k, ldt, ldv, n
           ! Array Arguments 
           real(sp), intent(out) :: t(ldt,*)
           real(sp), intent(in) :: tau(*), v(ldv,*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j, prevlastv, lastv
           ! Executable Statements 
           ! quick return if possible
           if( n==0 )return
           if( stdlib_lsame( direct, 'F' ) ) then
              prevlastv = n
              do i = 1, k
                 prevlastv = max( i, prevlastv )
                 if( tau( i )==zero ) then
                    ! h(i)  =  i
                    do j = 1, i
                       t( j, i ) = zero
                    end do
                 else
                    ! general case
                    if( stdlib_lsame( storev, 'C' ) ) then
                       ! skip any trailing zeros.
                       do lastv = n, i+1, -1
                          if( v( lastv, i )/=zero ) exit
                       end do
                       do j = 1, i-1
                          t( j, i ) = -tau( i ) * v( i , j )
                       end do
                       j = min( lastv, prevlastv )
                       ! t(1:i-1,i) := - tau(i) * v(i:j,1:i-1)**t * v(i:j,i)
                       call stdlib_sgemv( 'TRANSPOSE', j-i, i-1, -tau( i ),v( i+1, 1_ilp ), ldv, v( i+&
                                 1_ilp, i ), 1_ilp, one,t( 1_ilp, i ), 1_ilp )
                    else
                       ! skip any trailing zeros.
                       do lastv = n, i+1, -1
                          if( v( i, lastv )/=zero ) exit
                       end do
                       do j = 1, i-1
                          t( j, i ) = -tau( i ) * v( j , i )
                       end do
                       j = min( lastv, prevlastv )
                       ! t(1:i-1,i) := - tau(i) * v(1:i-1,i:j) * v(i,i:j)**t
                       call stdlib_sgemv( 'NO TRANSPOSE', i-1, j-i, -tau( i ),v( 1_ilp, i+1 ), ldv, v(&
                                  i, i+1 ), ldv,one, t( 1_ilp, i ), 1_ilp )
                    end if
                    ! t(1:i-1,i) := t(1:i-1,1:i-1) * t(1:i-1,i)
                    call stdlib_strmv( 'UPPER', 'NO TRANSPOSE', 'NON-UNIT', i-1, t,ldt, t( 1_ilp, i ),&
                               1_ilp )
                    t( i, i ) = tau( i )
                    if( i>1_ilp ) then
                       prevlastv = max( prevlastv, lastv )
                    else
                       prevlastv = lastv
                    end if
                 end if
              end do
           else
              prevlastv = 1_ilp
              do i = k, 1, -1
                 if( tau( i )==zero ) then
                    ! h(i)  =  i
                    do j = i, k
                       t( j, i ) = zero
                    end do
                 else
                    ! general case
                    if( i<k ) then
                       if( stdlib_lsame( storev, 'C' ) ) then
                          ! skip any leading zeros.
                          do lastv = 1, i-1
                             if( v( lastv, i )/=zero ) exit
                          end do
                          do j = i+1, k
                             t( j, i ) = -tau( i ) * v( n-k+i , j )
                          end do
                          j = max( lastv, prevlastv )
                          ! t(i+1:k,i) = -tau(i) * v(j:n-k+i,i+1:k)**t * v(j:n-k+i,i)
                          call stdlib_sgemv( 'TRANSPOSE', n-k+i-j, k-i, -tau( i ),v( j, i+1 ), &
                                    ldv, v( j, i ), 1_ilp, one,t( i+1, i ), 1_ilp )
                       else
                          ! skip any leading zeros.
                          do lastv = 1, i-1
                             if( v( i, lastv )/=zero ) exit
                          end do
                          do j = i+1, k
                             t( j, i ) = -tau( i ) * v( j, n-k+i )
                          end do
                          j = max( lastv, prevlastv )
                          ! t(i+1:k,i) = -tau(i) * v(i+1:k,j:n-k+i) * v(i,j:n-k+i)**t
                          call stdlib_sgemv( 'NO TRANSPOSE', k-i, n-k+i-j,-tau( i ), v( i+1, j ), &
                                    ldv, v( i, j ), ldv,one, t( i+1, i ), 1_ilp )
                       end if
                       ! t(i+1:k,i) := t(i+1:k,i+1:k) * t(i+1:k,i)
                       call stdlib_strmv( 'LOWER', 'NO TRANSPOSE', 'NON-UNIT', k-i,t( i+1, i+1 ), &
                                 ldt, t( i+1, i ), 1_ilp )
                       if( i>1_ilp ) then
                          prevlastv = min( prevlastv, lastv )
                       else
                          prevlastv = lastv
                       end if
                    end if
                    t( i, i ) = tau( i )
                 end if
              end do
           end if
           return
     end subroutine stdlib_slarft

     pure module subroutine stdlib_dlarft( direct, storev, n, k, v, ldv, tau, t, ldt )
     !! DLARFT forms the triangular factor T of a real block reflector H
     !! of order n, which is defined as a product of k elementary reflectors.
     !! If DIRECT = 'F', H = H(1) H(2) . . . H(k) and T is upper triangular;
     !! If DIRECT = 'B', H = H(k) . . . H(2) H(1) and T is lower triangular.
     !! If STOREV = 'C', the vector which defines the elementary reflector
     !! H(i) is stored in the i-th column of the array V, and
     !! H  =  I - V * T * V**T
     !! If STOREV = 'R', the vector which defines the elementary reflector
     !! H(i) is stored in the i-th row of the array V, and
     !! H  =  I - V**T * T * V
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: direct, storev
           integer(ilp), intent(in) :: k, ldt, ldv, n
           ! Array Arguments 
           real(dp), intent(out) :: t(ldt,*)
           real(dp), intent(in) :: tau(*), v(ldv,*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j, prevlastv, lastv
           ! Executable Statements 
           ! quick return if possible
           if( n==0 )return
           if( stdlib_lsame( direct, 'F' ) ) then
              prevlastv = n
              do i = 1, k
                 prevlastv = max( i, prevlastv )
                 if( tau( i )==zero ) then
                    ! h(i)  =  i
                    do j = 1, i
                       t( j, i ) = zero
                    end do
                 else
                    ! general case
                    if( stdlib_lsame( storev, 'C' ) ) then
                       ! skip any trailing zeros.
                       do lastv = n, i+1, -1
                          if( v( lastv, i )/=zero ) exit
                       end do
                       do j = 1, i-1
                          t( j, i ) = -tau( i ) * v( i , j )
                       end do
                       j = min( lastv, prevlastv )
                       ! t(1:i-1,i) := - tau(i) * v(i:j,1:i-1)**t * v(i:j,i)
                       call stdlib_dgemv( 'TRANSPOSE', j-i, i-1, -tau( i ),v( i+1, 1_ilp ), ldv, v( i+&
                                 1_ilp, i ), 1_ilp, one,t( 1_ilp, i ), 1_ilp )
                    else
                       ! skip any trailing zeros.
                       do lastv = n, i+1, -1
                          if( v( i, lastv )/=zero ) exit
                       end do
                       do j = 1, i-1
                          t( j, i ) = -tau( i ) * v( j , i )
                       end do
                       j = min( lastv, prevlastv )
                       ! t(1:i-1,i) := - tau(i) * v(1:i-1,i:j) * v(i,i:j)**t
                       call stdlib_dgemv( 'NO TRANSPOSE', i-1, j-i, -tau( i ),v( 1_ilp, i+1 ), ldv, v(&
                                  i, i+1 ), ldv, one,t( 1_ilp, i ), 1_ilp )
                    end if
                    ! t(1:i-1,i) := t(1:i-1,1:i-1) * t(1:i-1,i)
                    call stdlib_dtrmv( 'UPPER', 'NO TRANSPOSE', 'NON-UNIT', i-1, t,ldt, t( 1_ilp, i ),&
                               1_ilp )
                    t( i, i ) = tau( i )
                    if( i>1_ilp ) then
                       prevlastv = max( prevlastv, lastv )
                    else
                       prevlastv = lastv
                    end if
                 end if
              end do
           else
              prevlastv = 1_ilp
              do i = k, 1, -1
                 if( tau( i )==zero ) then
                    ! h(i)  =  i
                    do j = i, k
                       t( j, i ) = zero
                    end do
                 else
                    ! general case
                    if( i<k ) then
                       if( stdlib_lsame( storev, 'C' ) ) then
                          ! skip any leading zeros.
                          do lastv = 1, i-1
                             if( v( lastv, i )/=zero ) exit
                          end do
                          do j = i+1, k
                             t( j, i ) = -tau( i ) * v( n-k+i , j )
                          end do
                          j = max( lastv, prevlastv )
                          ! t(i+1:k,i) = -tau(i) * v(j:n-k+i,i+1:k)**t * v(j:n-k+i,i)
                          call stdlib_dgemv( 'TRANSPOSE', n-k+i-j, k-i, -tau( i ),v( j, i+1 ), &
                                    ldv, v( j, i ), 1_ilp, one,t( i+1, i ), 1_ilp )
                       else
                          ! skip any leading zeros.
                          do lastv = 1, i-1
                             if( v( i, lastv )/=zero ) exit
                          end do
                          do j = i+1, k
                             t( j, i ) = -tau( i ) * v( j, n-k+i )
                          end do
                          j = max( lastv, prevlastv )
                          ! t(i+1:k,i) = -tau(i) * v(i+1:k,j:n-k+i) * v(i,j:n-k+i)**t
                          call stdlib_dgemv( 'NO TRANSPOSE', k-i, n-k+i-j,-tau( i ), v( i+1, j ), &
                                    ldv, v( i, j ), ldv,one, t( i+1, i ), 1_ilp )
                       end if
                       ! t(i+1:k,i) := t(i+1:k,i+1:k) * t(i+1:k,i)
                       call stdlib_dtrmv( 'LOWER', 'NO TRANSPOSE', 'NON-UNIT', k-i,t( i+1, i+1 ), &
                                 ldt, t( i+1, i ), 1_ilp )
                       if( i>1_ilp ) then
                          prevlastv = min( prevlastv, lastv )
                       else
                          prevlastv = lastv
                       end if
                    end if
                    t( i, i ) = tau( i )
                 end if
              end do
           end if
           return
     end subroutine stdlib_dlarft


     pure module subroutine stdlib_clarft( direct, storev, n, k, v, ldv, tau, t, ldt )
     !! CLARFT forms the triangular factor T of a complex block reflector H
     !! of order n, which is defined as a product of k elementary reflectors.
     !! If DIRECT = 'F', H = H(1) H(2) . . . H(k) and T is upper triangular;
     !! If DIRECT = 'B', H = H(k) . . . H(2) H(1) and T is lower triangular.
     !! If STOREV = 'C', the vector which defines the elementary reflector
     !! H(i) is stored in the i-th column of the array V, and
     !! H  =  I - V * T * V**H
     !! If STOREV = 'R', the vector which defines the elementary reflector
     !! H(i) is stored in the i-th row of the array V, and
     !! H  =  I - V**H * T * V
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: direct, storev
           integer(ilp), intent(in) :: k, ldt, ldv, n
           ! Array Arguments 
           complex(sp), intent(out) :: t(ldt,*)
           complex(sp), intent(in) :: tau(*), v(ldv,*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j, prevlastv, lastv
           ! Executable Statements 
           ! quick return if possible
           if( n==0 )return
           if( stdlib_lsame( direct, 'F' ) ) then
              prevlastv = n
              do i = 1, k
                 prevlastv = max( prevlastv, i )
                 if( tau( i )==czero ) then
                    ! h(i)  =  i
                    do j = 1, i
                       t( j, i ) = czero
                    end do
                 else
                    ! general case
                    if( stdlib_lsame( storev, 'C' ) ) then
                       ! skip any trailing zeros.
                       do lastv = n, i+1, -1
                          if( v( lastv, i )/=czero ) exit
                       end do
                       do j = 1, i-1
                          t( j, i ) = -tau( i ) * conjg( v( i , j ) )
                       end do
                       j = min( lastv, prevlastv )
                       ! t(1:i-1,i) := - tau(i) * v(i:j,1:i-1)**h * v(i:j,i)
                       call stdlib_cgemv( 'CONJUGATE TRANSPOSE', j-i, i-1,-tau( i ), v( i+1, 1_ilp ), &
                                 ldv,v( i+1, i ), 1_ilp,cone, t( 1_ilp, i ), 1_ilp )
                    else
                       ! skip any trailing zeros.
                       do lastv = n, i+1, -1
                          if( v( i, lastv )/=czero ) exit
                       end do
                       do j = 1, i-1
                          t( j, i ) = -tau( i ) * v( j , i )
                       end do
                       j = min( lastv, prevlastv )
                       ! t(1:i-1,i) := - tau(i) * v(1:i-1,i:j) * v(i,i:j)**h
                       call stdlib_cgemm( 'N', 'C', i-1, 1_ilp, j-i, -tau( i ),v( 1_ilp, i+1 ), ldv, v( i,&
                                  i+1 ), ldv,cone, t( 1_ilp, i ), ldt )
                    end if
                    ! t(1:i-1,i) := t(1:i-1,1:i-1) * t(1:i-1,i)
                    call stdlib_ctrmv( 'UPPER', 'NO TRANSPOSE', 'NON-UNIT', i-1, t,ldt, t( 1_ilp, i ),&
                               1_ilp )
                    t( i, i ) = tau( i )
                    if( i>1_ilp ) then
                       prevlastv = max( prevlastv, lastv )
                    else
                       prevlastv = lastv
                    end if
                 end if
              end do
           else
              prevlastv = 1_ilp
              do i = k, 1, -1
                 if( tau( i )==czero ) then
                    ! h(i)  =  i
                    do j = i, k
                       t( j, i ) = czero
                    end do
                 else
                    ! general case
                    if( i<k ) then
                       if( stdlib_lsame( storev, 'C' ) ) then
                          ! skip any leading zeros.
                          do lastv = 1, i-1
                             if( v( lastv, i )/=czero ) exit
                          end do
                          do j = i+1, k
                             t( j, i ) = -tau( i ) * conjg( v( n-k+i , j ) )
                          end do
                          j = max( lastv, prevlastv )
                          ! t(i+1:k,i) = -tau(i) * v(j:n-k+i,i+1:k)**h * v(j:n-k+i,i)
                          call stdlib_cgemv( 'CONJUGATE TRANSPOSE', n-k+i-j, k-i,-tau( i ), v( j, &
                                    i+1 ), ldv, v( j, i ),1_ilp, cone, t( i+1, i ), 1_ilp )
                       else
                          ! skip any leading zeros.
                          do lastv = 1, i-1
                             if( v( i, lastv )/=czero ) exit
                          end do
                          do j = i+1, k
                             t( j, i ) = -tau( i ) * v( j, n-k+i )
                          end do
                          j = max( lastv, prevlastv )
                          ! t(i+1:k,i) = -tau(i) * v(i+1:k,j:n-k+i) * v(i,j:n-k+i)**h
                          call stdlib_cgemm( 'N', 'C', k-i, 1_ilp, n-k+i-j, -tau( i ),v( i+1, j ), &
                                    ldv, v( i, j ), ldv,cone, t( i+1, i ), ldt )
                       end if
                       ! t(i+1:k,i) := t(i+1:k,i+1:k) * t(i+1:k,i)
                       call stdlib_ctrmv( 'LOWER', 'NO TRANSPOSE', 'NON-UNIT', k-i,t( i+1, i+1 ), &
                                 ldt, t( i+1, i ), 1_ilp )
                       if( i>1_ilp ) then
                          prevlastv = min( prevlastv, lastv )
                       else
                          prevlastv = lastv
                       end if
                    end if
                    t( i, i ) = tau( i )
                 end if
              end do
           end if
           return
     end subroutine stdlib_clarft

     pure module subroutine stdlib_zlarft( direct, storev, n, k, v, ldv, tau, t, ldt )
     !! ZLARFT forms the triangular factor T of a complex block reflector H
     !! of order n, which is defined as a product of k elementary reflectors.
     !! If DIRECT = 'F', H = H(1) H(2) . . . H(k) and T is upper triangular;
     !! If DIRECT = 'B', H = H(k) . . . H(2) H(1) and T is lower triangular.
     !! If STOREV = 'C', the vector which defines the elementary reflector
     !! H(i) is stored in the i-th column of the array V, and
     !! H  =  I - V * T * V**H
     !! If STOREV = 'R', the vector which defines the elementary reflector
     !! H(i) is stored in the i-th row of the array V, and
     !! H  =  I - V**H * T * V
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: direct, storev
           integer(ilp), intent(in) :: k, ldt, ldv, n
           ! Array Arguments 
           complex(dp), intent(out) :: t(ldt,*)
           complex(dp), intent(in) :: tau(*), v(ldv,*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j, prevlastv, lastv
           ! Executable Statements 
           ! quick return if possible
           if( n==0 )return
           if( stdlib_lsame( direct, 'F' ) ) then
              prevlastv = n
              do i = 1, k
                 prevlastv = max( prevlastv, i )
                 if( tau( i )==czero ) then
                    ! h(i)  =  i
                    do j = 1, i
                       t( j, i ) = czero
                    end do
                 else
                    ! general case
                    if( stdlib_lsame( storev, 'C' ) ) then
                       ! skip any trailing zeros.
                       do lastv = n, i+1, -1
                          if( v( lastv, i )/=czero ) exit
                       end do
                       do j = 1, i-1
                          t( j, i ) = -tau( i ) * conjg( v( i , j ) )
                       end do
                       j = min( lastv, prevlastv )
                       ! t(1:i-1,i) := - tau(i) * v(i:j,1:i-1)**h * v(i:j,i)
                       call stdlib_zgemv( 'CONJUGATE TRANSPOSE', j-i, i-1,-tau( i ), v( i+1, 1_ilp ), &
                                 ldv,v( i+1, i ), 1_ilp, cone, t( 1_ilp, i ), 1_ilp )
                    else
                       ! skip any trailing zeros.
                       do lastv = n, i+1, -1
                          if( v( i, lastv )/=czero ) exit
                       end do
                       do j = 1, i-1
                          t( j, i ) = -tau( i ) * v( j , i )
                       end do
                       j = min( lastv, prevlastv )
                       ! t(1:i-1,i) := - tau(i) * v(1:i-1,i:j) * v(i,i:j)**h
                       call stdlib_zgemm( 'N', 'C', i-1, 1_ilp, j-i, -tau( i ),v( 1_ilp, i+1 ), ldv, v( i,&
                                  i+1 ), ldv,cone, t( 1_ilp, i ), ldt )
                    end if
                    ! t(1:i-1,i) := t(1:i-1,1:i-1) * t(1:i-1,i)
                    call stdlib_ztrmv( 'UPPER', 'NO TRANSPOSE', 'NON-UNIT', i-1, t,ldt, t( 1_ilp, i ),&
                               1_ilp )
                    t( i, i ) = tau( i )
                    if( i>1_ilp ) then
                       prevlastv = max( prevlastv, lastv )
                    else
                       prevlastv = lastv
                    end if
                  end if
              end do
           else
              prevlastv = 1_ilp
              do i = k, 1, -1
                 if( tau( i )==czero ) then
                    ! h(i)  =  i
                    do j = i, k
                       t( j, i ) = czero
                    end do
                 else
                    ! general case
                    if( i<k ) then
                       if( stdlib_lsame( storev, 'C' ) ) then
                          ! skip any leading zeros.
                          do lastv = 1, i-1
                             if( v( lastv, i )/=czero ) exit
                          end do
                          do j = i+1, k
                             t( j, i ) = -tau( i ) * conjg( v( n-k+i , j ) )
                          end do
                          j = max( lastv, prevlastv )
                          ! t(i+1:k,i) = -tau(i) * v(j:n-k+i,i+1:k)**h * v(j:n-k+i,i)
                          call stdlib_zgemv( 'CONJUGATE TRANSPOSE', n-k+i-j, k-i,-tau( i ), v( j, &
                                    i+1 ), ldv, v( j, i ),1_ilp, cone, t( i+1, i ), 1_ilp )
                       else
                          ! skip any leading zeros.
                          do lastv = 1, i-1
                             if( v( i, lastv )/=czero ) exit
                          end do
                          do j = i+1, k
                             t( j, i ) = -tau( i ) * v( j, n-k+i )
                          end do
                          j = max( lastv, prevlastv )
                          ! t(i+1:k,i) = -tau(i) * v(i+1:k,j:n-k+i) * v(i,j:n-k+i)**h
                          call stdlib_zgemm( 'N', 'C', k-i, 1_ilp, n-k+i-j, -tau( i ),v( i+1, j ), &
                                    ldv, v( i, j ), ldv,cone, t( i+1, i ), ldt )
                       end if
                       ! t(i+1:k,i) := t(i+1:k,i+1:k) * t(i+1:k,i)
                       call stdlib_ztrmv( 'LOWER', 'NO TRANSPOSE', 'NON-UNIT', k-i,t( i+1, i+1 ), &
                                 ldt, t( i+1, i ), 1_ilp )
                       if( i>1_ilp ) then
                          prevlastv = min( prevlastv, lastv )
                       else
                          prevlastv = lastv
                       end if
                    end if
                    t( i, i ) = tau( i )
                 end if
              end do
           end if
           return
     end subroutine stdlib_zlarft




     pure module subroutine stdlib_I64_slarf( side, m, n, v, incv, tau, c, ldc, work )
     !! SLARF applies a real elementary reflector H to a real m by n matrix
     !! C, from either the left or the right. H is represented in the form
     !! H = I - tau * v * v**T
     !! where tau is a real scalar and v is a real vector.
     !! If tau = 0, then H is taken to be the unit matrix.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side
           integer(ilp64), intent(in) :: incv, ldc, m, n
           real(sp), intent(in) :: tau
           ! Array Arguments 
           real(sp), intent(inout) :: c(ldc,*)
           real(sp), intent(in) :: v(*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: applyleft
           integer(ilp64) :: i, lastv, lastc
           ! Executable Statements 
           applyleft = stdlib_lsame( side, 'L' )
           lastv = 0_ilp64
           lastc = 0_ilp64
           if( tau/=zero ) then
           ! set up variables for scanning v.  lastv begins pointing to the end
           ! of v.
              if( applyleft ) then
                 lastv = m
              else
                 lastv = n
              end if
              if( incv>0_ilp64 ) then
                 i = 1_ilp64 + (lastv-1) * incv
              else
                 i = 1_ilp64
              end if
           ! look for the last non-zero row in v.
              do while( lastv>0 .and. v( i )==zero )
                 lastv = lastv - 1_ilp64
                 i = i - incv
              end do
              if( applyleft ) then
           ! scan for the last non-zero column in c(1:lastv,:).
                 lastc = stdlib_I64_ilaslc(lastv, n, c, ldc)
              else
           ! scan for the last non-zero row in c(:,1:lastv).
                 lastc = stdlib_I64_ilaslr(m, lastv, c, ldc)
              end if
           end if
           ! note that lastc.eq.0_sp renders the blas operations null; no special
           ! case is needed at this level.
           if( applyleft ) then
              ! form  h * c
              if( lastv>0_ilp64 ) then
                 ! w(1:lastc,1) := c(1:lastv,1:lastc)**t * v(1:lastv,1)
                 call stdlib_I64_sgemv( 'TRANSPOSE', lastv, lastc, one, c, ldc, v, incv,zero, work, 1_ilp64 &
                           )
                 ! c(1:lastv,1:lastc) := c(...) - v(1:lastv,1) * w(1:lastc,1)**t
                 call stdlib_I64_sger( lastv, lastc, -tau, v, incv, work, 1_ilp64, c, ldc )
              end if
           else
              ! form  c * h
              if( lastv>0_ilp64 ) then
                 ! w(1:lastc,1) := c(1:lastc,1:lastv) * v(1:lastv,1)
                 call stdlib_I64_sgemv( 'NO TRANSPOSE', lastc, lastv, one, c, ldc,v, incv, zero, work,&
                            1_ilp64 )
                 ! c(1:lastc,1:lastv) := c(...) - w(1:lastc,1) * v(1:lastv,1)**t
                 call stdlib_I64_sger( lastc, lastv, -tau, work, 1_ilp64, v, incv, c, ldc )
              end if
           end if
           return
     end subroutine stdlib_I64_slarf

     pure module subroutine stdlib_I64_dlarf( side, m, n, v, incv, tau, c, ldc, work )
     !! DLARF applies a real elementary reflector H to a real m by n matrix
     !! C, from either the left or the right. H is represented in the form
     !! H = I - tau * v * v**T
     !! where tau is a real scalar and v is a real vector.
     !! If tau = 0, then H is taken to be the unit matrix.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side
           integer(ilp64), intent(in) :: incv, ldc, m, n
           real(dp), intent(in) :: tau
           ! Array Arguments 
           real(dp), intent(inout) :: c(ldc,*)
           real(dp), intent(in) :: v(*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: applyleft
           integer(ilp64) :: i, lastv, lastc
           ! Executable Statements 
           applyleft = stdlib_lsame( side, 'L' )
           lastv = 0_ilp64
           lastc = 0_ilp64
           if( tau/=zero ) then
           ! set up variables for scanning v.  lastv begins pointing to the end
           ! of v.
              if( applyleft ) then
                 lastv = m
              else
                 lastv = n
              end if
              if( incv>0_ilp64 ) then
                 i = 1_ilp64 + (lastv-1) * incv
              else
                 i = 1_ilp64
              end if
           ! look for the last non-zero row in v.
              do while( lastv>0 .and. v( i )==zero )
                 lastv = lastv - 1_ilp64
                 i = i - incv
              end do
              if( applyleft ) then
           ! scan for the last non-zero column in c(1:lastv,:).
                 lastc = stdlib_I64_iladlc(lastv, n, c, ldc)
              else
           ! scan for the last non-zero row in c(:,1:lastv).
                 lastc = stdlib_I64_iladlr(m, lastv, c, ldc)
              end if
           end if
           ! note that lastc.eq.0_dp renders the blas operations null; no special
           ! case is needed at this level.
           if( applyleft ) then
              ! form  h * c
              if( lastv>0_ilp64 ) then
                 ! w(1:lastc,1) := c(1:lastv,1:lastc)**t * v(1:lastv,1)
                 call stdlib_I64_dgemv( 'TRANSPOSE', lastv, lastc, one, c, ldc, v, incv,zero, work, 1_ilp64 &
                           )
                 ! c(1:lastv,1:lastc) := c(...) - v(1:lastv,1) * w(1:lastc,1)**t
                 call stdlib_I64_dger( lastv, lastc, -tau, v, incv, work, 1_ilp64, c, ldc )
              end if
           else
              ! form  c * h
              if( lastv>0_ilp64 ) then
                 ! w(1:lastc,1) := c(1:lastc,1:lastv) * v(1:lastv,1)
                 call stdlib_I64_dgemv( 'NO TRANSPOSE', lastc, lastv, one, c, ldc,v, incv, zero, work,&
                            1_ilp64 )
                 ! c(1:lastc,1:lastv) := c(...) - w(1:lastc,1) * v(1:lastv,1)**t
                 call stdlib_I64_dger( lastc, lastv, -tau, work, 1_ilp64, v, incv, c, ldc )
              end if
           end if
           return
     end subroutine stdlib_I64_dlarf


     pure module subroutine stdlib_I64_clarf( side, m, n, v, incv, tau, c, ldc, work )
     !! CLARF applies a complex elementary reflector H to a complex M-by-N
     !! matrix C, from either the left or the right. H is represented in the
     !! form
     !! H = I - tau * v * v**H
     !! where tau is a complex scalar and v is a complex vector.
     !! If tau = 0, then H is taken to be the unit matrix.
     !! To apply H**H (the conjugate transpose of H), supply conjg(tau) instead
     !! tau.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side
           integer(ilp64), intent(in) :: incv, ldc, m, n
           complex(sp), intent(in) :: tau
           ! Array Arguments 
           complex(sp), intent(inout) :: c(ldc,*)
           complex(sp), intent(in) :: v(*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: applyleft
           integer(ilp64) :: i, lastv, lastc
           ! Executable Statements 
           applyleft = stdlib_lsame( side, 'L' )
           lastv = 0_ilp64
           lastc = 0_ilp64
           if( tau/=czero ) then
           ! set up variables for scanning v.  lastv begins pointing to the end
           ! of v.
              if( applyleft ) then
                 lastv = m
              else
                 lastv = n
              end if
              if( incv>0_ilp64 ) then
                 i = 1_ilp64 + (lastv-1) * incv
              else
                 i = 1_ilp64
              end if
           ! look for the last non-czero row in v.
              do while( lastv>0 .and. v( i )==czero )
                 lastv = lastv - 1_ilp64
                 i = i - incv
              end do
              if( applyleft ) then
           ! scan for the last non-czero column in c(1:lastv,:).
                 lastc = stdlib_I64_ilaclc(lastv, n, c, ldc)
              else
           ! scan for the last non-czero row in c(:,1:lastv).
                 lastc = stdlib_I64_ilaclr(m, lastv, c, ldc)
              end if
           end if
           ! note that lastc.eq.0_sp renders the blas operations null; no special
           ! case is needed at this level.
           if( applyleft ) then
              ! form  h * c
              if( lastv>0_ilp64 ) then
                 ! w(1:lastc,1) := c(1:lastv,1:lastc)**h * v(1:lastv,1)
                 call stdlib_I64_cgemv( 'CONJUGATE TRANSPOSE', lastv, lastc, cone,c, ldc, v, incv, &
                           czero, work, 1_ilp64 )
                 ! c(1:lastv,1:lastc) := c(...) - v(1:lastv,1) * w(1:lastc,1)**h
                 call stdlib_I64_cgerc( lastv, lastc, -tau, v, incv, work, 1_ilp64, c, ldc )
              end if
           else
              ! form  c * h
              if( lastv>0_ilp64 ) then
                 ! w(1:lastc,1) := c(1:lastc,1:lastv) * v(1:lastv,1)
                 call stdlib_I64_cgemv( 'NO TRANSPOSE', lastc, lastv, cone, c, ldc,v, incv, czero, &
                           work, 1_ilp64 )
                 ! c(1:lastc,1:lastv) := c(...) - w(1:lastc,1) * v(1:lastv,1)**h
                 call stdlib_I64_cgerc( lastc, lastv, -tau, work, 1_ilp64, v, incv, c, ldc )
              end if
           end if
           return
     end subroutine stdlib_I64_clarf

     pure module subroutine stdlib_I64_zlarf( side, m, n, v, incv, tau, c, ldc, work )
     !! ZLARF applies a complex elementary reflector H to a complex M-by-N
     !! matrix C, from either the left or the right. H is represented in the
     !! form
     !! H = I - tau * v * v**H
     !! where tau is a complex scalar and v is a complex vector.
     !! If tau = 0, then H is taken to be the unit matrix.
     !! To apply H**H, supply conjg(tau) instead
     !! tau.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side
           integer(ilp64), intent(in) :: incv, ldc, m, n
           complex(dp), intent(in) :: tau
           ! Array Arguments 
           complex(dp), intent(inout) :: c(ldc,*)
           complex(dp), intent(in) :: v(*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: applyleft
           integer(ilp64) :: i, lastv, lastc
           ! Executable Statements 
           applyleft = stdlib_lsame( side, 'L' )
           lastv = 0_ilp64
           lastc = 0_ilp64
           if( tau/=czero ) then
           ! set up variables for scanning v.  lastv begins pointing to the end
           ! of v.
              if( applyleft ) then
                 lastv = m
              else
                 lastv = n
              end if
              if( incv>0_ilp64 ) then
                 i = 1_ilp64 + (lastv-1) * incv
              else
                 i = 1_ilp64
              end if
           ! look for the last non-czero row in v.
              do while( lastv>0 .and. v( i )==czero )
                 lastv = lastv - 1_ilp64
                 i = i - incv
              end do
              if( applyleft ) then
           ! scan for the last non-czero column in c(1:lastv,:).
                 lastc = stdlib_I64_ilazlc(lastv, n, c, ldc)
              else
           ! scan for the last non-czero row in c(:,1:lastv).
                 lastc = stdlib_I64_ilazlr(m, lastv, c, ldc)
              end if
           end if
           ! note that lastc.eq.0_dp renders the blas operations null; no special
           ! case is needed at this level.
           if( applyleft ) then
              ! form  h * c
              if( lastv>0_ilp64 ) then
                 ! w(1:lastc,1) := c(1:lastv,1:lastc)**h * v(1:lastv,1)
                 call stdlib_I64_zgemv( 'CONJUGATE TRANSPOSE', lastv, lastc, cone,c, ldc, v, incv, &
                           czero, work, 1_ilp64 )
                 ! c(1:lastv,1:lastc) := c(...) - v(1:lastv,1) * w(1:lastc,1)**h
                 call stdlib_I64_zgerc( lastv, lastc, -tau, v, incv, work, 1_ilp64, c, ldc )
              end if
           else
              ! form  c * h
              if( lastv>0_ilp64 ) then
                 ! w(1:lastc,1) := c(1:lastc,1:lastv) * v(1:lastv,1)
                 call stdlib_I64_zgemv( 'NO TRANSPOSE', lastc, lastv, cone, c, ldc,v, incv, czero, &
                           work, 1_ilp64 )
                 ! c(1:lastc,1:lastv) := c(...) - w(1:lastc,1) * v(1:lastv,1)**h
                 call stdlib_I64_zgerc( lastc, lastv, -tau, work, 1_ilp64, v, incv, c, ldc )
              end if
           end if
           return
     end subroutine stdlib_I64_zlarf




     pure module subroutine stdlib_I64_slarfx( side, m, n, v, tau, c, ldc, work )
     !! SLARFX applies a real elementary reflector H to a real m by n
     !! matrix C, from either the left or the right. H is represented in the
     !! form
     !! H = I - tau * v * v**T
     !! where tau is a real scalar and v is a real vector.
     !! If tau = 0, then H is taken to be the unit matrix
     !! This version uses inline code if H has order < 11.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side
           integer(ilp64), intent(in) :: ldc, m, n
           real(sp), intent(in) :: tau
           ! Array Arguments 
           real(sp), intent(inout) :: c(ldc,*)
           real(sp), intent(in) :: v(*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp64) :: j
           real(sp) :: sum, t1, t10, t2, t3, t4, t5, t6, t7, t8, t9, v1, v10, v2, v3, v4, v5, v6, &
                     v7, v8, v9
           ! Executable Statements 
           if( tau==zero )return
           if( stdlib_lsame( side, 'L' ) ) then
              ! form  h * c, where h has order m.
              go to ( 10, 30, 50, 70, 90, 110, 130, 150,170, 190 )m
              ! code for general m
              call stdlib_I64_slarf( side, m, n, v, 1_ilp64, tau, c, ldc, work )
              go to 410
              10 continue
              ! special code for 1 x 1 householder
              t1 = one - tau*v( 1_ilp64 )*v( 1_ilp64 )
              do j = 1, n
                 c( 1_ilp64, j ) = t1*c( 1_ilp64, j )
              end do
              go to 410
              30 continue
              ! special code for 2 x 2 householder
              v1 = v( 1_ilp64 )
              t1 = tau*v1
              v2 = v( 2_ilp64 )
              t2 = tau*v2
              do j = 1, n
                 sum = v1*c( 1_ilp64, j ) + v2*c( 2_ilp64, j )
                 c( 1_ilp64, j ) = c( 1_ilp64, j ) - sum*t1
                 c( 2_ilp64, j ) = c( 2_ilp64, j ) - sum*t2
              end do
              go to 410
              50 continue
              ! special code for 3 x 3 householder
              v1 = v( 1_ilp64 )
              t1 = tau*v1
              v2 = v( 2_ilp64 )
              t2 = tau*v2
              v3 = v( 3_ilp64 )
              t3 = tau*v3
              do j = 1, n
                 sum = v1*c( 1_ilp64, j ) + v2*c( 2_ilp64, j ) + v3*c( 3_ilp64, j )
                 c( 1_ilp64, j ) = c( 1_ilp64, j ) - sum*t1
                 c( 2_ilp64, j ) = c( 2_ilp64, j ) - sum*t2
                 c( 3_ilp64, j ) = c( 3_ilp64, j ) - sum*t3
              end do
              go to 410
              70 continue
              ! special code for 4 x 4 householder
              v1 = v( 1_ilp64 )
              t1 = tau*v1
              v2 = v( 2_ilp64 )
              t2 = tau*v2
              v3 = v( 3_ilp64 )
              t3 = tau*v3
              v4 = v( 4_ilp64 )
              t4 = tau*v4
              do j = 1, n
                 sum = v1*c( 1_ilp64, j ) + v2*c( 2_ilp64, j ) + v3*c( 3_ilp64, j ) +v4*c( 4_ilp64, j )
                 c( 1_ilp64, j ) = c( 1_ilp64, j ) - sum*t1
                 c( 2_ilp64, j ) = c( 2_ilp64, j ) - sum*t2
                 c( 3_ilp64, j ) = c( 3_ilp64, j ) - sum*t3
                 c( 4_ilp64, j ) = c( 4_ilp64, j ) - sum*t4
              end do
              go to 410
              90 continue
              ! special code for 5 x 5 householder
              v1 = v( 1_ilp64 )
              t1 = tau*v1
              v2 = v( 2_ilp64 )
              t2 = tau*v2
              v3 = v( 3_ilp64 )
              t3 = tau*v3
              v4 = v( 4_ilp64 )
              t4 = tau*v4
              v5 = v( 5_ilp64 )
              t5 = tau*v5
              do j = 1, n
                 sum = v1*c( 1_ilp64, j ) + v2*c( 2_ilp64, j ) + v3*c( 3_ilp64, j ) +v4*c( 4_ilp64, j ) + v5*c( 5_ilp64, j )
                           
                 c( 1_ilp64, j ) = c( 1_ilp64, j ) - sum*t1
                 c( 2_ilp64, j ) = c( 2_ilp64, j ) - sum*t2
                 c( 3_ilp64, j ) = c( 3_ilp64, j ) - sum*t3
                 c( 4_ilp64, j ) = c( 4_ilp64, j ) - sum*t4
                 c( 5_ilp64, j ) = c( 5_ilp64, j ) - sum*t5
              end do
              go to 410
              110 continue
              ! special code for 6 x 6 householder
              v1 = v( 1_ilp64 )
              t1 = tau*v1
              v2 = v( 2_ilp64 )
              t2 = tau*v2
              v3 = v( 3_ilp64 )
              t3 = tau*v3
              v4 = v( 4_ilp64 )
              t4 = tau*v4
              v5 = v( 5_ilp64 )
              t5 = tau*v5
              v6 = v( 6_ilp64 )
              t6 = tau*v6
              do j = 1, n
                 sum = v1*c( 1_ilp64, j ) + v2*c( 2_ilp64, j ) + v3*c( 3_ilp64, j ) +v4*c( 4_ilp64, j ) + v5*c( 5_ilp64, j ) + &
                           v6*c( 6_ilp64, j )
                 c( 1_ilp64, j ) = c( 1_ilp64, j ) - sum*t1
                 c( 2_ilp64, j ) = c( 2_ilp64, j ) - sum*t2
                 c( 3_ilp64, j ) = c( 3_ilp64, j ) - sum*t3
                 c( 4_ilp64, j ) = c( 4_ilp64, j ) - sum*t4
                 c( 5_ilp64, j ) = c( 5_ilp64, j ) - sum*t5
                 c( 6_ilp64, j ) = c( 6_ilp64, j ) - sum*t6
              end do
              go to 410
              130 continue
              ! special code for 7 x 7 householder
              v1 = v( 1_ilp64 )
              t1 = tau*v1
              v2 = v( 2_ilp64 )
              t2 = tau*v2
              v3 = v( 3_ilp64 )
              t3 = tau*v3
              v4 = v( 4_ilp64 )
              t4 = tau*v4
              v5 = v( 5_ilp64 )
              t5 = tau*v5
              v6 = v( 6_ilp64 )
              t6 = tau*v6
              v7 = v( 7_ilp64 )
              t7 = tau*v7
              do j = 1, n
                 sum = v1*c( 1_ilp64, j ) + v2*c( 2_ilp64, j ) + v3*c( 3_ilp64, j ) +v4*c( 4_ilp64, j ) + v5*c( 5_ilp64, j ) + &
                           v6*c( 6_ilp64, j ) +v7*c( 7_ilp64, j )
                 c( 1_ilp64, j ) = c( 1_ilp64, j ) - sum*t1
                 c( 2_ilp64, j ) = c( 2_ilp64, j ) - sum*t2
                 c( 3_ilp64, j ) = c( 3_ilp64, j ) - sum*t3
                 c( 4_ilp64, j ) = c( 4_ilp64, j ) - sum*t4
                 c( 5_ilp64, j ) = c( 5_ilp64, j ) - sum*t5
                 c( 6_ilp64, j ) = c( 6_ilp64, j ) - sum*t6
                 c( 7_ilp64, j ) = c( 7_ilp64, j ) - sum*t7
              end do
              go to 410
              150 continue
              ! special code for 8 x 8 householder
              v1 = v( 1_ilp64 )
              t1 = tau*v1
              v2 = v( 2_ilp64 )
              t2 = tau*v2
              v3 = v( 3_ilp64 )
              t3 = tau*v3
              v4 = v( 4_ilp64 )
              t4 = tau*v4
              v5 = v( 5_ilp64 )
              t5 = tau*v5
              v6 = v( 6_ilp64 )
              t6 = tau*v6
              v7 = v( 7_ilp64 )
              t7 = tau*v7
              v8 = v( 8_ilp64 )
              t8 = tau*v8
              do j = 1, n
                 sum = v1*c( 1_ilp64, j ) + v2*c( 2_ilp64, j ) + v3*c( 3_ilp64, j ) +v4*c( 4_ilp64, j ) + v5*c( 5_ilp64, j ) + &
                           v6*c( 6_ilp64, j ) +v7*c( 7_ilp64, j ) + v8*c( 8_ilp64, j )
                 c( 1_ilp64, j ) = c( 1_ilp64, j ) - sum*t1
                 c( 2_ilp64, j ) = c( 2_ilp64, j ) - sum*t2
                 c( 3_ilp64, j ) = c( 3_ilp64, j ) - sum*t3
                 c( 4_ilp64, j ) = c( 4_ilp64, j ) - sum*t4
                 c( 5_ilp64, j ) = c( 5_ilp64, j ) - sum*t5
                 c( 6_ilp64, j ) = c( 6_ilp64, j ) - sum*t6
                 c( 7_ilp64, j ) = c( 7_ilp64, j ) - sum*t7
                 c( 8_ilp64, j ) = c( 8_ilp64, j ) - sum*t8
              end do
              go to 410
              170 continue
              ! special code for 9 x 9 householder
              v1 = v( 1_ilp64 )
              t1 = tau*v1
              v2 = v( 2_ilp64 )
              t2 = tau*v2
              v3 = v( 3_ilp64 )
              t3 = tau*v3
              v4 = v( 4_ilp64 )
              t4 = tau*v4
              v5 = v( 5_ilp64 )
              t5 = tau*v5
              v6 = v( 6_ilp64 )
              t6 = tau*v6
              v7 = v( 7_ilp64 )
              t7 = tau*v7
              v8 = v( 8_ilp64 )
              t8 = tau*v8
              v9 = v( 9_ilp64 )
              t9 = tau*v9
              do j = 1, n
                 sum = v1*c( 1_ilp64, j ) + v2*c( 2_ilp64, j ) + v3*c( 3_ilp64, j ) +v4*c( 4_ilp64, j ) + v5*c( 5_ilp64, j ) + &
                           v6*c( 6_ilp64, j ) +v7*c( 7_ilp64, j ) + v8*c( 8_ilp64, j ) + v9*c( 9_ilp64, j )
                 c( 1_ilp64, j ) = c( 1_ilp64, j ) - sum*t1
                 c( 2_ilp64, j ) = c( 2_ilp64, j ) - sum*t2
                 c( 3_ilp64, j ) = c( 3_ilp64, j ) - sum*t3
                 c( 4_ilp64, j ) = c( 4_ilp64, j ) - sum*t4
                 c( 5_ilp64, j ) = c( 5_ilp64, j ) - sum*t5
                 c( 6_ilp64, j ) = c( 6_ilp64, j ) - sum*t6
                 c( 7_ilp64, j ) = c( 7_ilp64, j ) - sum*t7
                 c( 8_ilp64, j ) = c( 8_ilp64, j ) - sum*t8
                 c( 9_ilp64, j ) = c( 9_ilp64, j ) - sum*t9
              end do
              go to 410
              190 continue
              ! special code for 10 x 10 householder
              v1 = v( 1_ilp64 )
              t1 = tau*v1
              v2 = v( 2_ilp64 )
              t2 = tau*v2
              v3 = v( 3_ilp64 )
              t3 = tau*v3
              v4 = v( 4_ilp64 )
              t4 = tau*v4
              v5 = v( 5_ilp64 )
              t5 = tau*v5
              v6 = v( 6_ilp64 )
              t6 = tau*v6
              v7 = v( 7_ilp64 )
              t7 = tau*v7
              v8 = v( 8_ilp64 )
              t8 = tau*v8
              v9 = v( 9_ilp64 )
              t9 = tau*v9
              v10 = v( 10_ilp64 )
              t10 = tau*v10
              do j = 1, n
                 sum = v1*c( 1_ilp64, j ) + v2*c( 2_ilp64, j ) + v3*c( 3_ilp64, j ) +v4*c( 4_ilp64, j ) + v5*c( 5_ilp64, j ) + &
                           v6*c( 6_ilp64, j ) +v7*c( 7_ilp64, j ) + v8*c( 8_ilp64, j ) + v9*c( 9_ilp64, j ) +v10*c( 10_ilp64, j )
                 c( 1_ilp64, j ) = c( 1_ilp64, j ) - sum*t1
                 c( 2_ilp64, j ) = c( 2_ilp64, j ) - sum*t2
                 c( 3_ilp64, j ) = c( 3_ilp64, j ) - sum*t3
                 c( 4_ilp64, j ) = c( 4_ilp64, j ) - sum*t4
                 c( 5_ilp64, j ) = c( 5_ilp64, j ) - sum*t5
                 c( 6_ilp64, j ) = c( 6_ilp64, j ) - sum*t6
                 c( 7_ilp64, j ) = c( 7_ilp64, j ) - sum*t7
                 c( 8_ilp64, j ) = c( 8_ilp64, j ) - sum*t8
                 c( 9_ilp64, j ) = c( 9_ilp64, j ) - sum*t9
                 c( 10_ilp64, j ) = c( 10_ilp64, j ) - sum*t10
              end do
              go to 410
           else
              ! form  c * h, where h has order n.
              go to ( 210, 230, 250, 270, 290, 310, 330, 350,370, 390 )n
              ! code for general n
              call stdlib_I64_slarf( side, m, n, v, 1_ilp64, tau, c, ldc, work )
              go to 410
              210 continue
              ! special code for 1 x 1 householder
              t1 = one - tau*v( 1_ilp64 )*v( 1_ilp64 )
              do j = 1, m
                 c( j, 1_ilp64 ) = t1*c( j, 1_ilp64 )
              end do
              go to 410
              230 continue
              ! special code for 2 x 2 householder
              v1 = v( 1_ilp64 )
              t1 = tau*v1
              v2 = v( 2_ilp64 )
              t2 = tau*v2
              do j = 1, m
                 sum = v1*c( j, 1_ilp64 ) + v2*c( j, 2_ilp64 )
                 c( j, 1_ilp64 ) = c( j, 1_ilp64 ) - sum*t1
                 c( j, 2_ilp64 ) = c( j, 2_ilp64 ) - sum*t2
              end do
              go to 410
              250 continue
              ! special code for 3 x 3 householder
              v1 = v( 1_ilp64 )
              t1 = tau*v1
              v2 = v( 2_ilp64 )
              t2 = tau*v2
              v3 = v( 3_ilp64 )
              t3 = tau*v3
              do j = 1, m
                 sum = v1*c( j, 1_ilp64 ) + v2*c( j, 2_ilp64 ) + v3*c( j, 3_ilp64 )
                 c( j, 1_ilp64 ) = c( j, 1_ilp64 ) - sum*t1
                 c( j, 2_ilp64 ) = c( j, 2_ilp64 ) - sum*t2
                 c( j, 3_ilp64 ) = c( j, 3_ilp64 ) - sum*t3
              end do
              go to 410
              270 continue
              ! special code for 4 x 4 householder
              v1 = v( 1_ilp64 )
              t1 = tau*v1
              v2 = v( 2_ilp64 )
              t2 = tau*v2
              v3 = v( 3_ilp64 )
              t3 = tau*v3
              v4 = v( 4_ilp64 )
              t4 = tau*v4
              do j = 1, m
                 sum = v1*c( j, 1_ilp64 ) + v2*c( j, 2_ilp64 ) + v3*c( j, 3_ilp64 ) +v4*c( j, 4_ilp64 )
                 c( j, 1_ilp64 ) = c( j, 1_ilp64 ) - sum*t1
                 c( j, 2_ilp64 ) = c( j, 2_ilp64 ) - sum*t2
                 c( j, 3_ilp64 ) = c( j, 3_ilp64 ) - sum*t3
                 c( j, 4_ilp64 ) = c( j, 4_ilp64 ) - sum*t4
              end do
              go to 410
              290 continue
              ! special code for 5 x 5 householder
              v1 = v( 1_ilp64 )
              t1 = tau*v1
              v2 = v( 2_ilp64 )
              t2 = tau*v2
              v3 = v( 3_ilp64 )
              t3 = tau*v3
              v4 = v( 4_ilp64 )
              t4 = tau*v4
              v5 = v( 5_ilp64 )
              t5 = tau*v5
              do j = 1, m
                 sum = v1*c( j, 1_ilp64 ) + v2*c( j, 2_ilp64 ) + v3*c( j, 3_ilp64 ) +v4*c( j, 4_ilp64 ) + v5*c( j, 5_ilp64 )
                           
                 c( j, 1_ilp64 ) = c( j, 1_ilp64 ) - sum*t1
                 c( j, 2_ilp64 ) = c( j, 2_ilp64 ) - sum*t2
                 c( j, 3_ilp64 ) = c( j, 3_ilp64 ) - sum*t3
                 c( j, 4_ilp64 ) = c( j, 4_ilp64 ) - sum*t4
                 c( j, 5_ilp64 ) = c( j, 5_ilp64 ) - sum*t5
              end do
              go to 410
              310 continue
              ! special code for 6 x 6 householder
              v1 = v( 1_ilp64 )
              t1 = tau*v1
              v2 = v( 2_ilp64 )
              t2 = tau*v2
              v3 = v( 3_ilp64 )
              t3 = tau*v3
              v4 = v( 4_ilp64 )
              t4 = tau*v4
              v5 = v( 5_ilp64 )
              t5 = tau*v5
              v6 = v( 6_ilp64 )
              t6 = tau*v6
              do j = 1, m
                 sum = v1*c( j, 1_ilp64 ) + v2*c( j, 2_ilp64 ) + v3*c( j, 3_ilp64 ) +v4*c( j, 4_ilp64 ) + v5*c( j, 5_ilp64 ) + &
                           v6*c( j, 6_ilp64 )
                 c( j, 1_ilp64 ) = c( j, 1_ilp64 ) - sum*t1
                 c( j, 2_ilp64 ) = c( j, 2_ilp64 ) - sum*t2
                 c( j, 3_ilp64 ) = c( j, 3_ilp64 ) - sum*t3
                 c( j, 4_ilp64 ) = c( j, 4_ilp64 ) - sum*t4
                 c( j, 5_ilp64 ) = c( j, 5_ilp64 ) - sum*t5
                 c( j, 6_ilp64 ) = c( j, 6_ilp64 ) - sum*t6
              end do
              go to 410
              330 continue
              ! special code for 7 x 7 householder
              v1 = v( 1_ilp64 )
              t1 = tau*v1
              v2 = v( 2_ilp64 )
              t2 = tau*v2
              v3 = v( 3_ilp64 )
              t3 = tau*v3
              v4 = v( 4_ilp64 )
              t4 = tau*v4
              v5 = v( 5_ilp64 )
              t5 = tau*v5
              v6 = v( 6_ilp64 )
              t6 = tau*v6
              v7 = v( 7_ilp64 )
              t7 = tau*v7
              do j = 1, m
                 sum = v1*c( j, 1_ilp64 ) + v2*c( j, 2_ilp64 ) + v3*c( j, 3_ilp64 ) +v4*c( j, 4_ilp64 ) + v5*c( j, 5_ilp64 ) + &
                           v6*c( j, 6_ilp64 ) +v7*c( j, 7_ilp64 )
                 c( j, 1_ilp64 ) = c( j, 1_ilp64 ) - sum*t1
                 c( j, 2_ilp64 ) = c( j, 2_ilp64 ) - sum*t2
                 c( j, 3_ilp64 ) = c( j, 3_ilp64 ) - sum*t3
                 c( j, 4_ilp64 ) = c( j, 4_ilp64 ) - sum*t4
                 c( j, 5_ilp64 ) = c( j, 5_ilp64 ) - sum*t5
                 c( j, 6_ilp64 ) = c( j, 6_ilp64 ) - sum*t6
                 c( j, 7_ilp64 ) = c( j, 7_ilp64 ) - sum*t7
              end do
              go to 410
              350 continue
              ! special code for 8 x 8 householder
              v1 = v( 1_ilp64 )
              t1 = tau*v1
              v2 = v( 2_ilp64 )
              t2 = tau*v2
              v3 = v( 3_ilp64 )
              t3 = tau*v3
              v4 = v( 4_ilp64 )
              t4 = tau*v4
              v5 = v( 5_ilp64 )
              t5 = tau*v5
              v6 = v( 6_ilp64 )
              t6 = tau*v6
              v7 = v( 7_ilp64 )
              t7 = tau*v7
              v8 = v( 8_ilp64 )
              t8 = tau*v8
              do j = 1, m
                 sum = v1*c( j, 1_ilp64 ) + v2*c( j, 2_ilp64 ) + v3*c( j, 3_ilp64 ) +v4*c( j, 4_ilp64 ) + v5*c( j, 5_ilp64 ) + &
                           v6*c( j, 6_ilp64 ) +v7*c( j, 7_ilp64 ) + v8*c( j, 8_ilp64 )
                 c( j, 1_ilp64 ) = c( j, 1_ilp64 ) - sum*t1
                 c( j, 2_ilp64 ) = c( j, 2_ilp64 ) - sum*t2
                 c( j, 3_ilp64 ) = c( j, 3_ilp64 ) - sum*t3
                 c( j, 4_ilp64 ) = c( j, 4_ilp64 ) - sum*t4
                 c( j, 5_ilp64 ) = c( j, 5_ilp64 ) - sum*t5
                 c( j, 6_ilp64 ) = c( j, 6_ilp64 ) - sum*t6
                 c( j, 7_ilp64 ) = c( j, 7_ilp64 ) - sum*t7
                 c( j, 8_ilp64 ) = c( j, 8_ilp64 ) - sum*t8
              end do
              go to 410
              370 continue
              ! special code for 9 x 9 householder
              v1 = v( 1_ilp64 )
              t1 = tau*v1
              v2 = v( 2_ilp64 )
              t2 = tau*v2
              v3 = v( 3_ilp64 )
              t3 = tau*v3
              v4 = v( 4_ilp64 )
              t4 = tau*v4
              v5 = v( 5_ilp64 )
              t5 = tau*v5
              v6 = v( 6_ilp64 )
              t6 = tau*v6
              v7 = v( 7_ilp64 )
              t7 = tau*v7
              v8 = v( 8_ilp64 )
              t8 = tau*v8
              v9 = v( 9_ilp64 )
              t9 = tau*v9
              do j = 1, m
                 sum = v1*c( j, 1_ilp64 ) + v2*c( j, 2_ilp64 ) + v3*c( j, 3_ilp64 ) +v4*c( j, 4_ilp64 ) + v5*c( j, 5_ilp64 ) + &
                           v6*c( j, 6_ilp64 ) +v7*c( j, 7_ilp64 ) + v8*c( j, 8_ilp64 ) + v9*c( j, 9_ilp64 )
                 c( j, 1_ilp64 ) = c( j, 1_ilp64 ) - sum*t1
                 c( j, 2_ilp64 ) = c( j, 2_ilp64 ) - sum*t2
                 c( j, 3_ilp64 ) = c( j, 3_ilp64 ) - sum*t3
                 c( j, 4_ilp64 ) = c( j, 4_ilp64 ) - sum*t4
                 c( j, 5_ilp64 ) = c( j, 5_ilp64 ) - sum*t5
                 c( j, 6_ilp64 ) = c( j, 6_ilp64 ) - sum*t6
                 c( j, 7_ilp64 ) = c( j, 7_ilp64 ) - sum*t7
                 c( j, 8_ilp64 ) = c( j, 8_ilp64 ) - sum*t8
                 c( j, 9_ilp64 ) = c( j, 9_ilp64 ) - sum*t9
              end do
              go to 410
              390 continue
              ! special code for 10 x 10 householder
              v1 = v( 1_ilp64 )
              t1 = tau*v1
              v2 = v( 2_ilp64 )
              t2 = tau*v2
              v3 = v( 3_ilp64 )
              t3 = tau*v3
              v4 = v( 4_ilp64 )
              t4 = tau*v4
              v5 = v( 5_ilp64 )
              t5 = tau*v5
              v6 = v( 6_ilp64 )
              t6 = tau*v6
              v7 = v( 7_ilp64 )
              t7 = tau*v7
              v8 = v( 8_ilp64 )
              t8 = tau*v8
              v9 = v( 9_ilp64 )
              t9 = tau*v9
              v10 = v( 10_ilp64 )
              t10 = tau*v10
              do j = 1, m
                 sum = v1*c( j, 1_ilp64 ) + v2*c( j, 2_ilp64 ) + v3*c( j, 3_ilp64 ) +v4*c( j, 4_ilp64 ) + v5*c( j, 5_ilp64 ) + &
                           v6*c( j, 6_ilp64 ) +v7*c( j, 7_ilp64 ) + v8*c( j, 8_ilp64 ) + v9*c( j, 9_ilp64 ) +v10*c( j, 10_ilp64 )
                 c( j, 1_ilp64 ) = c( j, 1_ilp64 ) - sum*t1
                 c( j, 2_ilp64 ) = c( j, 2_ilp64 ) - sum*t2
                 c( j, 3_ilp64 ) = c( j, 3_ilp64 ) - sum*t3
                 c( j, 4_ilp64 ) = c( j, 4_ilp64 ) - sum*t4
                 c( j, 5_ilp64 ) = c( j, 5_ilp64 ) - sum*t5
                 c( j, 6_ilp64 ) = c( j, 6_ilp64 ) - sum*t6
                 c( j, 7_ilp64 ) = c( j, 7_ilp64 ) - sum*t7
                 c( j, 8_ilp64 ) = c( j, 8_ilp64 ) - sum*t8
                 c( j, 9_ilp64 ) = c( j, 9_ilp64 ) - sum*t9
                 c( j, 10_ilp64 ) = c( j, 10_ilp64 ) - sum*t10
              end do
              go to 410
           end if
       410 return
     end subroutine stdlib_I64_slarfx

     pure module subroutine stdlib_I64_dlarfx( side, m, n, v, tau, c, ldc, work )
     !! DLARFX applies a real elementary reflector H to a real m by n
     !! matrix C, from either the left or the right. H is represented in the
     !! form
     !! H = I - tau * v * v**T
     !! where tau is a real scalar and v is a real vector.
     !! If tau = 0, then H is taken to be the unit matrix
     !! This version uses inline code if H has order < 11.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side
           integer(ilp64), intent(in) :: ldc, m, n
           real(dp), intent(in) :: tau
           ! Array Arguments 
           real(dp), intent(inout) :: c(ldc,*)
           real(dp), intent(in) :: v(*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp64) :: j
           real(dp) :: sum, t1, t10, t2, t3, t4, t5, t6, t7, t8, t9, v1, v10, v2, v3, v4, v5, v6, &
                     v7, v8, v9
           ! Executable Statements 
           if( tau==zero )return
           if( stdlib_lsame( side, 'L' ) ) then
              ! form  h * c, where h has order m.
              go to ( 10, 30, 50, 70, 90, 110, 130, 150,170, 190 )m
              ! code for general m
              call stdlib_I64_dlarf( side, m, n, v, 1_ilp64, tau, c, ldc, work )
              go to 410
              10 continue
              ! special code for 1 x 1 householder
              t1 = one - tau*v( 1_ilp64 )*v( 1_ilp64 )
              do j = 1, n
                 c( 1_ilp64, j ) = t1*c( 1_ilp64, j )
              end do
              go to 410
              30 continue
              ! special code for 2 x 2 householder
              v1 = v( 1_ilp64 )
              t1 = tau*v1
              v2 = v( 2_ilp64 )
              t2 = tau*v2
              do j = 1, n
                 sum = v1*c( 1_ilp64, j ) + v2*c( 2_ilp64, j )
                 c( 1_ilp64, j ) = c( 1_ilp64, j ) - sum*t1
                 c( 2_ilp64, j ) = c( 2_ilp64, j ) - sum*t2
              end do
              go to 410
              50 continue
              ! special code for 3 x 3 householder
              v1 = v( 1_ilp64 )
              t1 = tau*v1
              v2 = v( 2_ilp64 )
              t2 = tau*v2
              v3 = v( 3_ilp64 )
              t3 = tau*v3
              do j = 1, n
                 sum = v1*c( 1_ilp64, j ) + v2*c( 2_ilp64, j ) + v3*c( 3_ilp64, j )
                 c( 1_ilp64, j ) = c( 1_ilp64, j ) - sum*t1
                 c( 2_ilp64, j ) = c( 2_ilp64, j ) - sum*t2
                 c( 3_ilp64, j ) = c( 3_ilp64, j ) - sum*t3
              end do
              go to 410
              70 continue
              ! special code for 4 x 4 householder
              v1 = v( 1_ilp64 )
              t1 = tau*v1
              v2 = v( 2_ilp64 )
              t2 = tau*v2
              v3 = v( 3_ilp64 )
              t3 = tau*v3
              v4 = v( 4_ilp64 )
              t4 = tau*v4
              do j = 1, n
                 sum = v1*c( 1_ilp64, j ) + v2*c( 2_ilp64, j ) + v3*c( 3_ilp64, j ) +v4*c( 4_ilp64, j )
                 c( 1_ilp64, j ) = c( 1_ilp64, j ) - sum*t1
                 c( 2_ilp64, j ) = c( 2_ilp64, j ) - sum*t2
                 c( 3_ilp64, j ) = c( 3_ilp64, j ) - sum*t3
                 c( 4_ilp64, j ) = c( 4_ilp64, j ) - sum*t4
              end do
              go to 410
              90 continue
              ! special code for 5 x 5 householder
              v1 = v( 1_ilp64 )
              t1 = tau*v1
              v2 = v( 2_ilp64 )
              t2 = tau*v2
              v3 = v( 3_ilp64 )
              t3 = tau*v3
              v4 = v( 4_ilp64 )
              t4 = tau*v4
              v5 = v( 5_ilp64 )
              t5 = tau*v5
              do j = 1, n
                 sum = v1*c( 1_ilp64, j ) + v2*c( 2_ilp64, j ) + v3*c( 3_ilp64, j ) +v4*c( 4_ilp64, j ) + v5*c( 5_ilp64, j )
                           
                 c( 1_ilp64, j ) = c( 1_ilp64, j ) - sum*t1
                 c( 2_ilp64, j ) = c( 2_ilp64, j ) - sum*t2
                 c( 3_ilp64, j ) = c( 3_ilp64, j ) - sum*t3
                 c( 4_ilp64, j ) = c( 4_ilp64, j ) - sum*t4
                 c( 5_ilp64, j ) = c( 5_ilp64, j ) - sum*t5
              end do
              go to 410
              110 continue
              ! special code for 6 x 6 householder
              v1 = v( 1_ilp64 )
              t1 = tau*v1
              v2 = v( 2_ilp64 )
              t2 = tau*v2
              v3 = v( 3_ilp64 )
              t3 = tau*v3
              v4 = v( 4_ilp64 )
              t4 = tau*v4
              v5 = v( 5_ilp64 )
              t5 = tau*v5
              v6 = v( 6_ilp64 )
              t6 = tau*v6
              do j = 1, n
                 sum = v1*c( 1_ilp64, j ) + v2*c( 2_ilp64, j ) + v3*c( 3_ilp64, j ) +v4*c( 4_ilp64, j ) + v5*c( 5_ilp64, j ) + &
                           v6*c( 6_ilp64, j )
                 c( 1_ilp64, j ) = c( 1_ilp64, j ) - sum*t1
                 c( 2_ilp64, j ) = c( 2_ilp64, j ) - sum*t2
                 c( 3_ilp64, j ) = c( 3_ilp64, j ) - sum*t3
                 c( 4_ilp64, j ) = c( 4_ilp64, j ) - sum*t4
                 c( 5_ilp64, j ) = c( 5_ilp64, j ) - sum*t5
                 c( 6_ilp64, j ) = c( 6_ilp64, j ) - sum*t6
              end do
              go to 410
              130 continue
              ! special code for 7 x 7 householder
              v1 = v( 1_ilp64 )
              t1 = tau*v1
              v2 = v( 2_ilp64 )
              t2 = tau*v2
              v3 = v( 3_ilp64 )
              t3 = tau*v3
              v4 = v( 4_ilp64 )
              t4 = tau*v4
              v5 = v( 5_ilp64 )
              t5 = tau*v5
              v6 = v( 6_ilp64 )
              t6 = tau*v6
              v7 = v( 7_ilp64 )
              t7 = tau*v7
              do j = 1, n
                 sum = v1*c( 1_ilp64, j ) + v2*c( 2_ilp64, j ) + v3*c( 3_ilp64, j ) +v4*c( 4_ilp64, j ) + v5*c( 5_ilp64, j ) + &
                           v6*c( 6_ilp64, j ) +v7*c( 7_ilp64, j )
                 c( 1_ilp64, j ) = c( 1_ilp64, j ) - sum*t1
                 c( 2_ilp64, j ) = c( 2_ilp64, j ) - sum*t2
                 c( 3_ilp64, j ) = c( 3_ilp64, j ) - sum*t3
                 c( 4_ilp64, j ) = c( 4_ilp64, j ) - sum*t4
                 c( 5_ilp64, j ) = c( 5_ilp64, j ) - sum*t5
                 c( 6_ilp64, j ) = c( 6_ilp64, j ) - sum*t6
                 c( 7_ilp64, j ) = c( 7_ilp64, j ) - sum*t7
              end do
              go to 410
              150 continue
              ! special code for 8 x 8 householder
              v1 = v( 1_ilp64 )
              t1 = tau*v1
              v2 = v( 2_ilp64 )
              t2 = tau*v2
              v3 = v( 3_ilp64 )
              t3 = tau*v3
              v4 = v( 4_ilp64 )
              t4 = tau*v4
              v5 = v( 5_ilp64 )
              t5 = tau*v5
              v6 = v( 6_ilp64 )
              t6 = tau*v6
              v7 = v( 7_ilp64 )
              t7 = tau*v7
              v8 = v( 8_ilp64 )
              t8 = tau*v8
              do j = 1, n
                 sum = v1*c( 1_ilp64, j ) + v2*c( 2_ilp64, j ) + v3*c( 3_ilp64, j ) +v4*c( 4_ilp64, j ) + v5*c( 5_ilp64, j ) + &
                           v6*c( 6_ilp64, j ) +v7*c( 7_ilp64, j ) + v8*c( 8_ilp64, j )
                 c( 1_ilp64, j ) = c( 1_ilp64, j ) - sum*t1
                 c( 2_ilp64, j ) = c( 2_ilp64, j ) - sum*t2
                 c( 3_ilp64, j ) = c( 3_ilp64, j ) - sum*t3
                 c( 4_ilp64, j ) = c( 4_ilp64, j ) - sum*t4
                 c( 5_ilp64, j ) = c( 5_ilp64, j ) - sum*t5
                 c( 6_ilp64, j ) = c( 6_ilp64, j ) - sum*t6
                 c( 7_ilp64, j ) = c( 7_ilp64, j ) - sum*t7
                 c( 8_ilp64, j ) = c( 8_ilp64, j ) - sum*t8
              end do
              go to 410
              170 continue
              ! special code for 9 x 9 householder
              v1 = v( 1_ilp64 )
              t1 = tau*v1
              v2 = v( 2_ilp64 )
              t2 = tau*v2
              v3 = v( 3_ilp64 )
              t3 = tau*v3
              v4 = v( 4_ilp64 )
              t4 = tau*v4
              v5 = v( 5_ilp64 )
              t5 = tau*v5
              v6 = v( 6_ilp64 )
              t6 = tau*v6
              v7 = v( 7_ilp64 )
              t7 = tau*v7
              v8 = v( 8_ilp64 )
              t8 = tau*v8
              v9 = v( 9_ilp64 )
              t9 = tau*v9
              do j = 1, n
                 sum = v1*c( 1_ilp64, j ) + v2*c( 2_ilp64, j ) + v3*c( 3_ilp64, j ) +v4*c( 4_ilp64, j ) + v5*c( 5_ilp64, j ) + &
                           v6*c( 6_ilp64, j ) +v7*c( 7_ilp64, j ) + v8*c( 8_ilp64, j ) + v9*c( 9_ilp64, j )
                 c( 1_ilp64, j ) = c( 1_ilp64, j ) - sum*t1
                 c( 2_ilp64, j ) = c( 2_ilp64, j ) - sum*t2
                 c( 3_ilp64, j ) = c( 3_ilp64, j ) - sum*t3
                 c( 4_ilp64, j ) = c( 4_ilp64, j ) - sum*t4
                 c( 5_ilp64, j ) = c( 5_ilp64, j ) - sum*t5
                 c( 6_ilp64, j ) = c( 6_ilp64, j ) - sum*t6
                 c( 7_ilp64, j ) = c( 7_ilp64, j ) - sum*t7
                 c( 8_ilp64, j ) = c( 8_ilp64, j ) - sum*t8
                 c( 9_ilp64, j ) = c( 9_ilp64, j ) - sum*t9
              end do
              go to 410
              190 continue
              ! special code for 10 x 10 householder
              v1 = v( 1_ilp64 )
              t1 = tau*v1
              v2 = v( 2_ilp64 )
              t2 = tau*v2
              v3 = v( 3_ilp64 )
              t3 = tau*v3
              v4 = v( 4_ilp64 )
              t4 = tau*v4
              v5 = v( 5_ilp64 )
              t5 = tau*v5
              v6 = v( 6_ilp64 )
              t6 = tau*v6
              v7 = v( 7_ilp64 )
              t7 = tau*v7
              v8 = v( 8_ilp64 )
              t8 = tau*v8
              v9 = v( 9_ilp64 )
              t9 = tau*v9
              v10 = v( 10_ilp64 )
              t10 = tau*v10
              do j = 1, n
                 sum = v1*c( 1_ilp64, j ) + v2*c( 2_ilp64, j ) + v3*c( 3_ilp64, j ) +v4*c( 4_ilp64, j ) + v5*c( 5_ilp64, j ) + &
                           v6*c( 6_ilp64, j ) +v7*c( 7_ilp64, j ) + v8*c( 8_ilp64, j ) + v9*c( 9_ilp64, j ) +v10*c( 10_ilp64, j )
                 c( 1_ilp64, j ) = c( 1_ilp64, j ) - sum*t1
                 c( 2_ilp64, j ) = c( 2_ilp64, j ) - sum*t2
                 c( 3_ilp64, j ) = c( 3_ilp64, j ) - sum*t3
                 c( 4_ilp64, j ) = c( 4_ilp64, j ) - sum*t4
                 c( 5_ilp64, j ) = c( 5_ilp64, j ) - sum*t5
                 c( 6_ilp64, j ) = c( 6_ilp64, j ) - sum*t6
                 c( 7_ilp64, j ) = c( 7_ilp64, j ) - sum*t7
                 c( 8_ilp64, j ) = c( 8_ilp64, j ) - sum*t8
                 c( 9_ilp64, j ) = c( 9_ilp64, j ) - sum*t9
                 c( 10_ilp64, j ) = c( 10_ilp64, j ) - sum*t10
              end do
              go to 410
           else
              ! form  c * h, where h has order n.
              go to ( 210, 230, 250, 270, 290, 310, 330, 350,370, 390 )n
              ! code for general n
              call stdlib_I64_dlarf( side, m, n, v, 1_ilp64, tau, c, ldc, work )
              go to 410
              210 continue
              ! special code for 1 x 1 householder
              t1 = one - tau*v( 1_ilp64 )*v( 1_ilp64 )
              do j = 1, m
                 c( j, 1_ilp64 ) = t1*c( j, 1_ilp64 )
              end do
              go to 410
              230 continue
              ! special code for 2 x 2 householder
              v1 = v( 1_ilp64 )
              t1 = tau*v1
              v2 = v( 2_ilp64 )
              t2 = tau*v2
              do j = 1, m
                 sum = v1*c( j, 1_ilp64 ) + v2*c( j, 2_ilp64 )
                 c( j, 1_ilp64 ) = c( j, 1_ilp64 ) - sum*t1
                 c( j, 2_ilp64 ) = c( j, 2_ilp64 ) - sum*t2
              end do
              go to 410
              250 continue
              ! special code for 3 x 3 householder
              v1 = v( 1_ilp64 )
              t1 = tau*v1
              v2 = v( 2_ilp64 )
              t2 = tau*v2
              v3 = v( 3_ilp64 )
              t3 = tau*v3
              do j = 1, m
                 sum = v1*c( j, 1_ilp64 ) + v2*c( j, 2_ilp64 ) + v3*c( j, 3_ilp64 )
                 c( j, 1_ilp64 ) = c( j, 1_ilp64 ) - sum*t1
                 c( j, 2_ilp64 ) = c( j, 2_ilp64 ) - sum*t2
                 c( j, 3_ilp64 ) = c( j, 3_ilp64 ) - sum*t3
              end do
              go to 410
              270 continue
              ! special code for 4 x 4 householder
              v1 = v( 1_ilp64 )
              t1 = tau*v1
              v2 = v( 2_ilp64 )
              t2 = tau*v2
              v3 = v( 3_ilp64 )
              t3 = tau*v3
              v4 = v( 4_ilp64 )
              t4 = tau*v4
              do j = 1, m
                 sum = v1*c( j, 1_ilp64 ) + v2*c( j, 2_ilp64 ) + v3*c( j, 3_ilp64 ) +v4*c( j, 4_ilp64 )
                 c( j, 1_ilp64 ) = c( j, 1_ilp64 ) - sum*t1
                 c( j, 2_ilp64 ) = c( j, 2_ilp64 ) - sum*t2
                 c( j, 3_ilp64 ) = c( j, 3_ilp64 ) - sum*t3
                 c( j, 4_ilp64 ) = c( j, 4_ilp64 ) - sum*t4
              end do
              go to 410
              290 continue
              ! special code for 5 x 5 householder
              v1 = v( 1_ilp64 )
              t1 = tau*v1
              v2 = v( 2_ilp64 )
              t2 = tau*v2
              v3 = v( 3_ilp64 )
              t3 = tau*v3
              v4 = v( 4_ilp64 )
              t4 = tau*v4
              v5 = v( 5_ilp64 )
              t5 = tau*v5
              do j = 1, m
                 sum = v1*c( j, 1_ilp64 ) + v2*c( j, 2_ilp64 ) + v3*c( j, 3_ilp64 ) +v4*c( j, 4_ilp64 ) + v5*c( j, 5_ilp64 )
                           
                 c( j, 1_ilp64 ) = c( j, 1_ilp64 ) - sum*t1
                 c( j, 2_ilp64 ) = c( j, 2_ilp64 ) - sum*t2
                 c( j, 3_ilp64 ) = c( j, 3_ilp64 ) - sum*t3
                 c( j, 4_ilp64 ) = c( j, 4_ilp64 ) - sum*t4
                 c( j, 5_ilp64 ) = c( j, 5_ilp64 ) - sum*t5
              end do
              go to 410
              310 continue
              ! special code for 6 x 6 householder
              v1 = v( 1_ilp64 )
              t1 = tau*v1
              v2 = v( 2_ilp64 )
              t2 = tau*v2
              v3 = v( 3_ilp64 )
              t3 = tau*v3
              v4 = v( 4_ilp64 )
              t4 = tau*v4
              v5 = v( 5_ilp64 )
              t5 = tau*v5
              v6 = v( 6_ilp64 )
              t6 = tau*v6
              do j = 1, m
                 sum = v1*c( j, 1_ilp64 ) + v2*c( j, 2_ilp64 ) + v3*c( j, 3_ilp64 ) +v4*c( j, 4_ilp64 ) + v5*c( j, 5_ilp64 ) + &
                           v6*c( j, 6_ilp64 )
                 c( j, 1_ilp64 ) = c( j, 1_ilp64 ) - sum*t1
                 c( j, 2_ilp64 ) = c( j, 2_ilp64 ) - sum*t2
                 c( j, 3_ilp64 ) = c( j, 3_ilp64 ) - sum*t3
                 c( j, 4_ilp64 ) = c( j, 4_ilp64 ) - sum*t4
                 c( j, 5_ilp64 ) = c( j, 5_ilp64 ) - sum*t5
                 c( j, 6_ilp64 ) = c( j, 6_ilp64 ) - sum*t6
              end do
              go to 410
              330 continue
              ! special code for 7 x 7 householder
              v1 = v( 1_ilp64 )
              t1 = tau*v1
              v2 = v( 2_ilp64 )
              t2 = tau*v2
              v3 = v( 3_ilp64 )
              t3 = tau*v3
              v4 = v( 4_ilp64 )
              t4 = tau*v4
              v5 = v( 5_ilp64 )
              t5 = tau*v5
              v6 = v( 6_ilp64 )
              t6 = tau*v6
              v7 = v( 7_ilp64 )
              t7 = tau*v7
              do j = 1, m
                 sum = v1*c( j, 1_ilp64 ) + v2*c( j, 2_ilp64 ) + v3*c( j, 3_ilp64 ) +v4*c( j, 4_ilp64 ) + v5*c( j, 5_ilp64 ) + &
                           v6*c( j, 6_ilp64 ) +v7*c( j, 7_ilp64 )
                 c( j, 1_ilp64 ) = c( j, 1_ilp64 ) - sum*t1
                 c( j, 2_ilp64 ) = c( j, 2_ilp64 ) - sum*t2
                 c( j, 3_ilp64 ) = c( j, 3_ilp64 ) - sum*t3
                 c( j, 4_ilp64 ) = c( j, 4_ilp64 ) - sum*t4
                 c( j, 5_ilp64 ) = c( j, 5_ilp64 ) - sum*t5
                 c( j, 6_ilp64 ) = c( j, 6_ilp64 ) - sum*t6
                 c( j, 7_ilp64 ) = c( j, 7_ilp64 ) - sum*t7
              end do
              go to 410
              350 continue
              ! special code for 8 x 8 householder
              v1 = v( 1_ilp64 )
              t1 = tau*v1
              v2 = v( 2_ilp64 )
              t2 = tau*v2
              v3 = v( 3_ilp64 )
              t3 = tau*v3
              v4 = v( 4_ilp64 )
              t4 = tau*v4
              v5 = v( 5_ilp64 )
              t5 = tau*v5
              v6 = v( 6_ilp64 )
              t6 = tau*v6
              v7 = v( 7_ilp64 )
              t7 = tau*v7
              v8 = v( 8_ilp64 )
              t8 = tau*v8
              do j = 1, m
                 sum = v1*c( j, 1_ilp64 ) + v2*c( j, 2_ilp64 ) + v3*c( j, 3_ilp64 ) +v4*c( j, 4_ilp64 ) + v5*c( j, 5_ilp64 ) + &
                           v6*c( j, 6_ilp64 ) +v7*c( j, 7_ilp64 ) + v8*c( j, 8_ilp64 )
                 c( j, 1_ilp64 ) = c( j, 1_ilp64 ) - sum*t1
                 c( j, 2_ilp64 ) = c( j, 2_ilp64 ) - sum*t2
                 c( j, 3_ilp64 ) = c( j, 3_ilp64 ) - sum*t3
                 c( j, 4_ilp64 ) = c( j, 4_ilp64 ) - sum*t4
                 c( j, 5_ilp64 ) = c( j, 5_ilp64 ) - sum*t5
                 c( j, 6_ilp64 ) = c( j, 6_ilp64 ) - sum*t6
                 c( j, 7_ilp64 ) = c( j, 7_ilp64 ) - sum*t7
                 c( j, 8_ilp64 ) = c( j, 8_ilp64 ) - sum*t8
              end do
              go to 410
              370 continue
              ! special code for 9 x 9 householder
              v1 = v( 1_ilp64 )
              t1 = tau*v1
              v2 = v( 2_ilp64 )
              t2 = tau*v2
              v3 = v( 3_ilp64 )
              t3 = tau*v3
              v4 = v( 4_ilp64 )
              t4 = tau*v4
              v5 = v( 5_ilp64 )
              t5 = tau*v5
              v6 = v( 6_ilp64 )
              t6 = tau*v6
              v7 = v( 7_ilp64 )
              t7 = tau*v7
              v8 = v( 8_ilp64 )
              t8 = tau*v8
              v9 = v( 9_ilp64 )
              t9 = tau*v9
              do j = 1, m
                 sum = v1*c( j, 1_ilp64 ) + v2*c( j, 2_ilp64 ) + v3*c( j, 3_ilp64 ) +v4*c( j, 4_ilp64 ) + v5*c( j, 5_ilp64 ) + &
                           v6*c( j, 6_ilp64 ) +v7*c( j, 7_ilp64 ) + v8*c( j, 8_ilp64 ) + v9*c( j, 9_ilp64 )
                 c( j, 1_ilp64 ) = c( j, 1_ilp64 ) - sum*t1
                 c( j, 2_ilp64 ) = c( j, 2_ilp64 ) - sum*t2
                 c( j, 3_ilp64 ) = c( j, 3_ilp64 ) - sum*t3
                 c( j, 4_ilp64 ) = c( j, 4_ilp64 ) - sum*t4
                 c( j, 5_ilp64 ) = c( j, 5_ilp64 ) - sum*t5
                 c( j, 6_ilp64 ) = c( j, 6_ilp64 ) - sum*t6
                 c( j, 7_ilp64 ) = c( j, 7_ilp64 ) - sum*t7
                 c( j, 8_ilp64 ) = c( j, 8_ilp64 ) - sum*t8
                 c( j, 9_ilp64 ) = c( j, 9_ilp64 ) - sum*t9
              end do
              go to 410
              390 continue
              ! special code for 10 x 10 householder
              v1 = v( 1_ilp64 )
              t1 = tau*v1
              v2 = v( 2_ilp64 )
              t2 = tau*v2
              v3 = v( 3_ilp64 )
              t3 = tau*v3
              v4 = v( 4_ilp64 )
              t4 = tau*v4
              v5 = v( 5_ilp64 )
              t5 = tau*v5
              v6 = v( 6_ilp64 )
              t6 = tau*v6
              v7 = v( 7_ilp64 )
              t7 = tau*v7
              v8 = v( 8_ilp64 )
              t8 = tau*v8
              v9 = v( 9_ilp64 )
              t9 = tau*v9
              v10 = v( 10_ilp64 )
              t10 = tau*v10
              do j = 1, m
                 sum = v1*c( j, 1_ilp64 ) + v2*c( j, 2_ilp64 ) + v3*c( j, 3_ilp64 ) +v4*c( j, 4_ilp64 ) + v5*c( j, 5_ilp64 ) + &
                           v6*c( j, 6_ilp64 ) +v7*c( j, 7_ilp64 ) + v8*c( j, 8_ilp64 ) + v9*c( j, 9_ilp64 ) +v10*c( j, 10_ilp64 )
                 c( j, 1_ilp64 ) = c( j, 1_ilp64 ) - sum*t1
                 c( j, 2_ilp64 ) = c( j, 2_ilp64 ) - sum*t2
                 c( j, 3_ilp64 ) = c( j, 3_ilp64 ) - sum*t3
                 c( j, 4_ilp64 ) = c( j, 4_ilp64 ) - sum*t4
                 c( j, 5_ilp64 ) = c( j, 5_ilp64 ) - sum*t5
                 c( j, 6_ilp64 ) = c( j, 6_ilp64 ) - sum*t6
                 c( j, 7_ilp64 ) = c( j, 7_ilp64 ) - sum*t7
                 c( j, 8_ilp64 ) = c( j, 8_ilp64 ) - sum*t8
                 c( j, 9_ilp64 ) = c( j, 9_ilp64 ) - sum*t9
                 c( j, 10_ilp64 ) = c( j, 10_ilp64 ) - sum*t10
              end do
              go to 410
           end if
           410 continue
           return
     end subroutine stdlib_I64_dlarfx


     pure module subroutine stdlib_I64_clarfx( side, m, n, v, tau, c, ldc, work )
     !! CLARFX applies a complex elementary reflector H to a complex m by n
     !! matrix C, from either the left or the right. H is represented in the
     !! form
     !! H = I - tau * v * v**H
     !! where tau is a complex scalar and v is a complex vector.
     !! If tau = 0, then H is taken to be the unit matrix
     !! This version uses inline code if H has order < 11.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side
           integer(ilp64), intent(in) :: ldc, m, n
           complex(sp), intent(in) :: tau
           ! Array Arguments 
           complex(sp), intent(inout) :: c(ldc,*)
           complex(sp), intent(in) :: v(*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp64) :: j
           complex(sp) :: sum, t1, t10, t2, t3, t4, t5, t6, t7, t8, t9, v1, v10, v2, v3, v4, v5, &
                     v6, v7, v8, v9
           ! Intrinsic Functions 
           ! Executable Statements 
           if( tau==czero )return
           if( stdlib_lsame( side, 'L' ) ) then
              ! form  h * c, where h has order m.
              go to ( 10, 30, 50, 70, 90, 110, 130, 150,170, 190 )m
              ! code for general m
              call stdlib_I64_clarf( side, m, n, v, 1_ilp64, tau, c, ldc, work )
              go to 410
              10 continue
              ! special code for 1 x 1 householder
              t1 = cone - tau*v( 1_ilp64 )*conjg( v( 1_ilp64 ) )
              do j = 1, n
                 c( 1_ilp64, j ) = t1*c( 1_ilp64, j )
              end do
              go to 410
              30 continue
              ! special code for 2 x 2 householder
              v1 = conjg( v( 1_ilp64 ) )
              t1 = tau*conjg( v1 )
              v2 = conjg( v( 2_ilp64 ) )
              t2 = tau*conjg( v2 )
              do j = 1, n
                 sum = v1*c( 1_ilp64, j ) + v2*c( 2_ilp64, j )
                 c( 1_ilp64, j ) = c( 1_ilp64, j ) - sum*t1
                 c( 2_ilp64, j ) = c( 2_ilp64, j ) - sum*t2
              end do
              go to 410
              50 continue
              ! special code for 3 x 3 householder
              v1 = conjg( v( 1_ilp64 ) )
              t1 = tau*conjg( v1 )
              v2 = conjg( v( 2_ilp64 ) )
              t2 = tau*conjg( v2 )
              v3 = conjg( v( 3_ilp64 ) )
              t3 = tau*conjg( v3 )
              do j = 1, n
                 sum = v1*c( 1_ilp64, j ) + v2*c( 2_ilp64, j ) + v3*c( 3_ilp64, j )
                 c( 1_ilp64, j ) = c( 1_ilp64, j ) - sum*t1
                 c( 2_ilp64, j ) = c( 2_ilp64, j ) - sum*t2
                 c( 3_ilp64, j ) = c( 3_ilp64, j ) - sum*t3
              end do
              go to 410
              70 continue
              ! special code for 4 x 4 householder
              v1 = conjg( v( 1_ilp64 ) )
              t1 = tau*conjg( v1 )
              v2 = conjg( v( 2_ilp64 ) )
              t2 = tau*conjg( v2 )
              v3 = conjg( v( 3_ilp64 ) )
              t3 = tau*conjg( v3 )
              v4 = conjg( v( 4_ilp64 ) )
              t4 = tau*conjg( v4 )
              do j = 1, n
                 sum = v1*c( 1_ilp64, j ) + v2*c( 2_ilp64, j ) + v3*c( 3_ilp64, j ) +v4*c( 4_ilp64, j )
                 c( 1_ilp64, j ) = c( 1_ilp64, j ) - sum*t1
                 c( 2_ilp64, j ) = c( 2_ilp64, j ) - sum*t2
                 c( 3_ilp64, j ) = c( 3_ilp64, j ) - sum*t3
                 c( 4_ilp64, j ) = c( 4_ilp64, j ) - sum*t4
              end do
              go to 410
              90 continue
              ! special code for 5 x 5 householder
              v1 = conjg( v( 1_ilp64 ) )
              t1 = tau*conjg( v1 )
              v2 = conjg( v( 2_ilp64 ) )
              t2 = tau*conjg( v2 )
              v3 = conjg( v( 3_ilp64 ) )
              t3 = tau*conjg( v3 )
              v4 = conjg( v( 4_ilp64 ) )
              t4 = tau*conjg( v4 )
              v5 = conjg( v( 5_ilp64 ) )
              t5 = tau*conjg( v5 )
              do j = 1, n
                 sum = v1*c( 1_ilp64, j ) + v2*c( 2_ilp64, j ) + v3*c( 3_ilp64, j ) +v4*c( 4_ilp64, j ) + v5*c( 5_ilp64, j )
                           
                 c( 1_ilp64, j ) = c( 1_ilp64, j ) - sum*t1
                 c( 2_ilp64, j ) = c( 2_ilp64, j ) - sum*t2
                 c( 3_ilp64, j ) = c( 3_ilp64, j ) - sum*t3
                 c( 4_ilp64, j ) = c( 4_ilp64, j ) - sum*t4
                 c( 5_ilp64, j ) = c( 5_ilp64, j ) - sum*t5
              end do
              go to 410
              110 continue
              ! special code for 6 x 6 householder
              v1 = conjg( v( 1_ilp64 ) )
              t1 = tau*conjg( v1 )
              v2 = conjg( v( 2_ilp64 ) )
              t2 = tau*conjg( v2 )
              v3 = conjg( v( 3_ilp64 ) )
              t3 = tau*conjg( v3 )
              v4 = conjg( v( 4_ilp64 ) )
              t4 = tau*conjg( v4 )
              v5 = conjg( v( 5_ilp64 ) )
              t5 = tau*conjg( v5 )
              v6 = conjg( v( 6_ilp64 ) )
              t6 = tau*conjg( v6 )
              do j = 1, n
                 sum = v1*c( 1_ilp64, j ) + v2*c( 2_ilp64, j ) + v3*c( 3_ilp64, j ) +v4*c( 4_ilp64, j ) + v5*c( 5_ilp64, j ) + &
                           v6*c( 6_ilp64, j )
                 c( 1_ilp64, j ) = c( 1_ilp64, j ) - sum*t1
                 c( 2_ilp64, j ) = c( 2_ilp64, j ) - sum*t2
                 c( 3_ilp64, j ) = c( 3_ilp64, j ) - sum*t3
                 c( 4_ilp64, j ) = c( 4_ilp64, j ) - sum*t4
                 c( 5_ilp64, j ) = c( 5_ilp64, j ) - sum*t5
                 c( 6_ilp64, j ) = c( 6_ilp64, j ) - sum*t6
              end do
              go to 410
              130 continue
              ! special code for 7 x 7 householder
              v1 = conjg( v( 1_ilp64 ) )
              t1 = tau*conjg( v1 )
              v2 = conjg( v( 2_ilp64 ) )
              t2 = tau*conjg( v2 )
              v3 = conjg( v( 3_ilp64 ) )
              t3 = tau*conjg( v3 )
              v4 = conjg( v( 4_ilp64 ) )
              t4 = tau*conjg( v4 )
              v5 = conjg( v( 5_ilp64 ) )
              t5 = tau*conjg( v5 )
              v6 = conjg( v( 6_ilp64 ) )
              t6 = tau*conjg( v6 )
              v7 = conjg( v( 7_ilp64 ) )
              t7 = tau*conjg( v7 )
              do j = 1, n
                 sum = v1*c( 1_ilp64, j ) + v2*c( 2_ilp64, j ) + v3*c( 3_ilp64, j ) +v4*c( 4_ilp64, j ) + v5*c( 5_ilp64, j ) + &
                           v6*c( 6_ilp64, j ) +v7*c( 7_ilp64, j )
                 c( 1_ilp64, j ) = c( 1_ilp64, j ) - sum*t1
                 c( 2_ilp64, j ) = c( 2_ilp64, j ) - sum*t2
                 c( 3_ilp64, j ) = c( 3_ilp64, j ) - sum*t3
                 c( 4_ilp64, j ) = c( 4_ilp64, j ) - sum*t4
                 c( 5_ilp64, j ) = c( 5_ilp64, j ) - sum*t5
                 c( 6_ilp64, j ) = c( 6_ilp64, j ) - sum*t6
                 c( 7_ilp64, j ) = c( 7_ilp64, j ) - sum*t7
              end do
              go to 410
              150 continue
              ! special code for 8 x 8 householder
              v1 = conjg( v( 1_ilp64 ) )
              t1 = tau*conjg( v1 )
              v2 = conjg( v( 2_ilp64 ) )
              t2 = tau*conjg( v2 )
              v3 = conjg( v( 3_ilp64 ) )
              t3 = tau*conjg( v3 )
              v4 = conjg( v( 4_ilp64 ) )
              t4 = tau*conjg( v4 )
              v5 = conjg( v( 5_ilp64 ) )
              t5 = tau*conjg( v5 )
              v6 = conjg( v( 6_ilp64 ) )
              t6 = tau*conjg( v6 )
              v7 = conjg( v( 7_ilp64 ) )
              t7 = tau*conjg( v7 )
              v8 = conjg( v( 8_ilp64 ) )
              t8 = tau*conjg( v8 )
              do j = 1, n
                 sum = v1*c( 1_ilp64, j ) + v2*c( 2_ilp64, j ) + v3*c( 3_ilp64, j ) +v4*c( 4_ilp64, j ) + v5*c( 5_ilp64, j ) + &
                           v6*c( 6_ilp64, j ) +v7*c( 7_ilp64, j ) + v8*c( 8_ilp64, j )
                 c( 1_ilp64, j ) = c( 1_ilp64, j ) - sum*t1
                 c( 2_ilp64, j ) = c( 2_ilp64, j ) - sum*t2
                 c( 3_ilp64, j ) = c( 3_ilp64, j ) - sum*t3
                 c( 4_ilp64, j ) = c( 4_ilp64, j ) - sum*t4
                 c( 5_ilp64, j ) = c( 5_ilp64, j ) - sum*t5
                 c( 6_ilp64, j ) = c( 6_ilp64, j ) - sum*t6
                 c( 7_ilp64, j ) = c( 7_ilp64, j ) - sum*t7
                 c( 8_ilp64, j ) = c( 8_ilp64, j ) - sum*t8
              end do
              go to 410
              170 continue
              ! special code for 9 x 9 householder
              v1 = conjg( v( 1_ilp64 ) )
              t1 = tau*conjg( v1 )
              v2 = conjg( v( 2_ilp64 ) )
              t2 = tau*conjg( v2 )
              v3 = conjg( v( 3_ilp64 ) )
              t3 = tau*conjg( v3 )
              v4 = conjg( v( 4_ilp64 ) )
              t4 = tau*conjg( v4 )
              v5 = conjg( v( 5_ilp64 ) )
              t5 = tau*conjg( v5 )
              v6 = conjg( v( 6_ilp64 ) )
              t6 = tau*conjg( v6 )
              v7 = conjg( v( 7_ilp64 ) )
              t7 = tau*conjg( v7 )
              v8 = conjg( v( 8_ilp64 ) )
              t8 = tau*conjg( v8 )
              v9 = conjg( v( 9_ilp64 ) )
              t9 = tau*conjg( v9 )
              do j = 1, n
                 sum = v1*c( 1_ilp64, j ) + v2*c( 2_ilp64, j ) + v3*c( 3_ilp64, j ) +v4*c( 4_ilp64, j ) + v5*c( 5_ilp64, j ) + &
                           v6*c( 6_ilp64, j ) +v7*c( 7_ilp64, j ) + v8*c( 8_ilp64, j ) + v9*c( 9_ilp64, j )
                 c( 1_ilp64, j ) = c( 1_ilp64, j ) - sum*t1
                 c( 2_ilp64, j ) = c( 2_ilp64, j ) - sum*t2
                 c( 3_ilp64, j ) = c( 3_ilp64, j ) - sum*t3
                 c( 4_ilp64, j ) = c( 4_ilp64, j ) - sum*t4
                 c( 5_ilp64, j ) = c( 5_ilp64, j ) - sum*t5
                 c( 6_ilp64, j ) = c( 6_ilp64, j ) - sum*t6
                 c( 7_ilp64, j ) = c( 7_ilp64, j ) - sum*t7
                 c( 8_ilp64, j ) = c( 8_ilp64, j ) - sum*t8
                 c( 9_ilp64, j ) = c( 9_ilp64, j ) - sum*t9
              end do
              go to 410
              190 continue
              ! special code for 10 x 10 householder
              v1 = conjg( v( 1_ilp64 ) )
              t1 = tau*conjg( v1 )
              v2 = conjg( v( 2_ilp64 ) )
              t2 = tau*conjg( v2 )
              v3 = conjg( v( 3_ilp64 ) )
              t3 = tau*conjg( v3 )
              v4 = conjg( v( 4_ilp64 ) )
              t4 = tau*conjg( v4 )
              v5 = conjg( v( 5_ilp64 ) )
              t5 = tau*conjg( v5 )
              v6 = conjg( v( 6_ilp64 ) )
              t6 = tau*conjg( v6 )
              v7 = conjg( v( 7_ilp64 ) )
              t7 = tau*conjg( v7 )
              v8 = conjg( v( 8_ilp64 ) )
              t8 = tau*conjg( v8 )
              v9 = conjg( v( 9_ilp64 ) )
              t9 = tau*conjg( v9 )
              v10 = conjg( v( 10_ilp64 ) )
              t10 = tau*conjg( v10 )
              do j = 1, n
                 sum = v1*c( 1_ilp64, j ) + v2*c( 2_ilp64, j ) + v3*c( 3_ilp64, j ) +v4*c( 4_ilp64, j ) + v5*c( 5_ilp64, j ) + &
                           v6*c( 6_ilp64, j ) +v7*c( 7_ilp64, j ) + v8*c( 8_ilp64, j ) + v9*c( 9_ilp64, j ) +v10*c( 10_ilp64, j )
                 c( 1_ilp64, j ) = c( 1_ilp64, j ) - sum*t1
                 c( 2_ilp64, j ) = c( 2_ilp64, j ) - sum*t2
                 c( 3_ilp64, j ) = c( 3_ilp64, j ) - sum*t3
                 c( 4_ilp64, j ) = c( 4_ilp64, j ) - sum*t4
                 c( 5_ilp64, j ) = c( 5_ilp64, j ) - sum*t5
                 c( 6_ilp64, j ) = c( 6_ilp64, j ) - sum*t6
                 c( 7_ilp64, j ) = c( 7_ilp64, j ) - sum*t7
                 c( 8_ilp64, j ) = c( 8_ilp64, j ) - sum*t8
                 c( 9_ilp64, j ) = c( 9_ilp64, j ) - sum*t9
                 c( 10_ilp64, j ) = c( 10_ilp64, j ) - sum*t10
              end do
              go to 410
           else
              ! form  c * h, where h has order n.
              go to ( 210, 230, 250, 270, 290, 310, 330, 350,370, 390 )n
              ! code for general n
              call stdlib_I64_clarf( side, m, n, v, 1_ilp64, tau, c, ldc, work )
              go to 410
              210 continue
              ! special code for 1 x 1 householder
              t1 = cone - tau*v( 1_ilp64 )*conjg( v( 1_ilp64 ) )
              do j = 1, m
                 c( j, 1_ilp64 ) = t1*c( j, 1_ilp64 )
              end do
              go to 410
              230 continue
              ! special code for 2 x 2 householder
              v1 = v( 1_ilp64 )
              t1 = tau*conjg( v1 )
              v2 = v( 2_ilp64 )
              t2 = tau*conjg( v2 )
              do j = 1, m
                 sum = v1*c( j, 1_ilp64 ) + v2*c( j, 2_ilp64 )
                 c( j, 1_ilp64 ) = c( j, 1_ilp64 ) - sum*t1
                 c( j, 2_ilp64 ) = c( j, 2_ilp64 ) - sum*t2
              end do
              go to 410
              250 continue
              ! special code for 3 x 3 householder
              v1 = v( 1_ilp64 )
              t1 = tau*conjg( v1 )
              v2 = v( 2_ilp64 )
              t2 = tau*conjg( v2 )
              v3 = v( 3_ilp64 )
              t3 = tau*conjg( v3 )
              do j = 1, m
                 sum = v1*c( j, 1_ilp64 ) + v2*c( j, 2_ilp64 ) + v3*c( j, 3_ilp64 )
                 c( j, 1_ilp64 ) = c( j, 1_ilp64 ) - sum*t1
                 c( j, 2_ilp64 ) = c( j, 2_ilp64 ) - sum*t2
                 c( j, 3_ilp64 ) = c( j, 3_ilp64 ) - sum*t3
              end do
              go to 410
              270 continue
              ! special code for 4 x 4 householder
              v1 = v( 1_ilp64 )
              t1 = tau*conjg( v1 )
              v2 = v( 2_ilp64 )
              t2 = tau*conjg( v2 )
              v3 = v( 3_ilp64 )
              t3 = tau*conjg( v3 )
              v4 = v( 4_ilp64 )
              t4 = tau*conjg( v4 )
              do j = 1, m
                 sum = v1*c( j, 1_ilp64 ) + v2*c( j, 2_ilp64 ) + v3*c( j, 3_ilp64 ) +v4*c( j, 4_ilp64 )
                 c( j, 1_ilp64 ) = c( j, 1_ilp64 ) - sum*t1
                 c( j, 2_ilp64 ) = c( j, 2_ilp64 ) - sum*t2
                 c( j, 3_ilp64 ) = c( j, 3_ilp64 ) - sum*t3
                 c( j, 4_ilp64 ) = c( j, 4_ilp64 ) - sum*t4
              end do
              go to 410
              290 continue
              ! special code for 5 x 5 householder
              v1 = v( 1_ilp64 )
              t1 = tau*conjg( v1 )
              v2 = v( 2_ilp64 )
              t2 = tau*conjg( v2 )
              v3 = v( 3_ilp64 )
              t3 = tau*conjg( v3 )
              v4 = v( 4_ilp64 )
              t4 = tau*conjg( v4 )
              v5 = v( 5_ilp64 )
              t5 = tau*conjg( v5 )
              do j = 1, m
                 sum = v1*c( j, 1_ilp64 ) + v2*c( j, 2_ilp64 ) + v3*c( j, 3_ilp64 ) +v4*c( j, 4_ilp64 ) + v5*c( j, 5_ilp64 )
                           
                 c( j, 1_ilp64 ) = c( j, 1_ilp64 ) - sum*t1
                 c( j, 2_ilp64 ) = c( j, 2_ilp64 ) - sum*t2
                 c( j, 3_ilp64 ) = c( j, 3_ilp64 ) - sum*t3
                 c( j, 4_ilp64 ) = c( j, 4_ilp64 ) - sum*t4
                 c( j, 5_ilp64 ) = c( j, 5_ilp64 ) - sum*t5
              end do
              go to 410
              310 continue
              ! special code for 6 x 6 householder
              v1 = v( 1_ilp64 )
              t1 = tau*conjg( v1 )
              v2 = v( 2_ilp64 )
              t2 = tau*conjg( v2 )
              v3 = v( 3_ilp64 )
              t3 = tau*conjg( v3 )
              v4 = v( 4_ilp64 )
              t4 = tau*conjg( v4 )
              v5 = v( 5_ilp64 )
              t5 = tau*conjg( v5 )
              v6 = v( 6_ilp64 )
              t6 = tau*conjg( v6 )
              do j = 1, m
                 sum = v1*c( j, 1_ilp64 ) + v2*c( j, 2_ilp64 ) + v3*c( j, 3_ilp64 ) +v4*c( j, 4_ilp64 ) + v5*c( j, 5_ilp64 ) + &
                           v6*c( j, 6_ilp64 )
                 c( j, 1_ilp64 ) = c( j, 1_ilp64 ) - sum*t1
                 c( j, 2_ilp64 ) = c( j, 2_ilp64 ) - sum*t2
                 c( j, 3_ilp64 ) = c( j, 3_ilp64 ) - sum*t3
                 c( j, 4_ilp64 ) = c( j, 4_ilp64 ) - sum*t4
                 c( j, 5_ilp64 ) = c( j, 5_ilp64 ) - sum*t5
                 c( j, 6_ilp64 ) = c( j, 6_ilp64 ) - sum*t6
              end do
              go to 410
              330 continue
              ! special code for 7 x 7 householder
              v1 = v( 1_ilp64 )
              t1 = tau*conjg( v1 )
              v2 = v( 2_ilp64 )
              t2 = tau*conjg( v2 )
              v3 = v( 3_ilp64 )
              t3 = tau*conjg( v3 )
              v4 = v( 4_ilp64 )
              t4 = tau*conjg( v4 )
              v5 = v( 5_ilp64 )
              t5 = tau*conjg( v5 )
              v6 = v( 6_ilp64 )
              t6 = tau*conjg( v6 )
              v7 = v( 7_ilp64 )
              t7 = tau*conjg( v7 )
              do j = 1, m
                 sum = v1*c( j, 1_ilp64 ) + v2*c( j, 2_ilp64 ) + v3*c( j, 3_ilp64 ) +v4*c( j, 4_ilp64 ) + v5*c( j, 5_ilp64 ) + &
                           v6*c( j, 6_ilp64 ) +v7*c( j, 7_ilp64 )
                 c( j, 1_ilp64 ) = c( j, 1_ilp64 ) - sum*t1
                 c( j, 2_ilp64 ) = c( j, 2_ilp64 ) - sum*t2
                 c( j, 3_ilp64 ) = c( j, 3_ilp64 ) - sum*t3
                 c( j, 4_ilp64 ) = c( j, 4_ilp64 ) - sum*t4
                 c( j, 5_ilp64 ) = c( j, 5_ilp64 ) - sum*t5
                 c( j, 6_ilp64 ) = c( j, 6_ilp64 ) - sum*t6
                 c( j, 7_ilp64 ) = c( j, 7_ilp64 ) - sum*t7
              end do
              go to 410
              350 continue
              ! special code for 8 x 8 householder
              v1 = v( 1_ilp64 )
              t1 = tau*conjg( v1 )
              v2 = v( 2_ilp64 )
              t2 = tau*conjg( v2 )
              v3 = v( 3_ilp64 )
              t3 = tau*conjg( v3 )
              v4 = v( 4_ilp64 )
              t4 = tau*conjg( v4 )
              v5 = v( 5_ilp64 )
              t5 = tau*conjg( v5 )
              v6 = v( 6_ilp64 )
              t6 = tau*conjg( v6 )
              v7 = v( 7_ilp64 )
              t7 = tau*conjg( v7 )
              v8 = v( 8_ilp64 )
              t8 = tau*conjg( v8 )
              do j = 1, m
                 sum = v1*c( j, 1_ilp64 ) + v2*c( j, 2_ilp64 ) + v3*c( j, 3_ilp64 ) +v4*c( j, 4_ilp64 ) + v5*c( j, 5_ilp64 ) + &
                           v6*c( j, 6_ilp64 ) +v7*c( j, 7_ilp64 ) + v8*c( j, 8_ilp64 )
                 c( j, 1_ilp64 ) = c( j, 1_ilp64 ) - sum*t1
                 c( j, 2_ilp64 ) = c( j, 2_ilp64 ) - sum*t2
                 c( j, 3_ilp64 ) = c( j, 3_ilp64 ) - sum*t3
                 c( j, 4_ilp64 ) = c( j, 4_ilp64 ) - sum*t4
                 c( j, 5_ilp64 ) = c( j, 5_ilp64 ) - sum*t5
                 c( j, 6_ilp64 ) = c( j, 6_ilp64 ) - sum*t6
                 c( j, 7_ilp64 ) = c( j, 7_ilp64 ) - sum*t7
                 c( j, 8_ilp64 ) = c( j, 8_ilp64 ) - sum*t8
              end do
              go to 410
              370 continue
              ! special code for 9 x 9 householder
              v1 = v( 1_ilp64 )
              t1 = tau*conjg( v1 )
              v2 = v( 2_ilp64 )
              t2 = tau*conjg( v2 )
              v3 = v( 3_ilp64 )
              t3 = tau*conjg( v3 )
              v4 = v( 4_ilp64 )
              t4 = tau*conjg( v4 )
              v5 = v( 5_ilp64 )
              t5 = tau*conjg( v5 )
              v6 = v( 6_ilp64 )
              t6 = tau*conjg( v6 )
              v7 = v( 7_ilp64 )
              t7 = tau*conjg( v7 )
              v8 = v( 8_ilp64 )
              t8 = tau*conjg( v8 )
              v9 = v( 9_ilp64 )
              t9 = tau*conjg( v9 )
              do j = 1, m
                 sum = v1*c( j, 1_ilp64 ) + v2*c( j, 2_ilp64 ) + v3*c( j, 3_ilp64 ) +v4*c( j, 4_ilp64 ) + v5*c( j, 5_ilp64 ) + &
                           v6*c( j, 6_ilp64 ) +v7*c( j, 7_ilp64 ) + v8*c( j, 8_ilp64 ) + v9*c( j, 9_ilp64 )
                 c( j, 1_ilp64 ) = c( j, 1_ilp64 ) - sum*t1
                 c( j, 2_ilp64 ) = c( j, 2_ilp64 ) - sum*t2
                 c( j, 3_ilp64 ) = c( j, 3_ilp64 ) - sum*t3
                 c( j, 4_ilp64 ) = c( j, 4_ilp64 ) - sum*t4
                 c( j, 5_ilp64 ) = c( j, 5_ilp64 ) - sum*t5
                 c( j, 6_ilp64 ) = c( j, 6_ilp64 ) - sum*t6
                 c( j, 7_ilp64 ) = c( j, 7_ilp64 ) - sum*t7
                 c( j, 8_ilp64 ) = c( j, 8_ilp64 ) - sum*t8
                 c( j, 9_ilp64 ) = c( j, 9_ilp64 ) - sum*t9
              end do
              go to 410
              390 continue
              ! special code for 10 x 10 householder
              v1 = v( 1_ilp64 )
              t1 = tau*conjg( v1 )
              v2 = v( 2_ilp64 )
              t2 = tau*conjg( v2 )
              v3 = v( 3_ilp64 )
              t3 = tau*conjg( v3 )
              v4 = v( 4_ilp64 )
              t4 = tau*conjg( v4 )
              v5 = v( 5_ilp64 )
              t5 = tau*conjg( v5 )
              v6 = v( 6_ilp64 )
              t6 = tau*conjg( v6 )
              v7 = v( 7_ilp64 )
              t7 = tau*conjg( v7 )
              v8 = v( 8_ilp64 )
              t8 = tau*conjg( v8 )
              v9 = v( 9_ilp64 )
              t9 = tau*conjg( v9 )
              v10 = v( 10_ilp64 )
              t10 = tau*conjg( v10 )
              do j = 1, m
                 sum = v1*c( j, 1_ilp64 ) + v2*c( j, 2_ilp64 ) + v3*c( j, 3_ilp64 ) +v4*c( j, 4_ilp64 ) + v5*c( j, 5_ilp64 ) + &
                           v6*c( j, 6_ilp64 ) +v7*c( j, 7_ilp64 ) + v8*c( j, 8_ilp64 ) + v9*c( j, 9_ilp64 ) +v10*c( j, 10_ilp64 )
                 c( j, 1_ilp64 ) = c( j, 1_ilp64 ) - sum*t1
                 c( j, 2_ilp64 ) = c( j, 2_ilp64 ) - sum*t2
                 c( j, 3_ilp64 ) = c( j, 3_ilp64 ) - sum*t3
                 c( j, 4_ilp64 ) = c( j, 4_ilp64 ) - sum*t4
                 c( j, 5_ilp64 ) = c( j, 5_ilp64 ) - sum*t5
                 c( j, 6_ilp64 ) = c( j, 6_ilp64 ) - sum*t6
                 c( j, 7_ilp64 ) = c( j, 7_ilp64 ) - sum*t7
                 c( j, 8_ilp64 ) = c( j, 8_ilp64 ) - sum*t8
                 c( j, 9_ilp64 ) = c( j, 9_ilp64 ) - sum*t9
                 c( j, 10_ilp64 ) = c( j, 10_ilp64 ) - sum*t10
              end do
              go to 410
           end if
       410 return
     end subroutine stdlib_I64_clarfx

     pure module subroutine stdlib_I64_zlarfx( side, m, n, v, tau, c, ldc, work )
     !! ZLARFX applies a complex elementary reflector H to a complex m by n
     !! matrix C, from either the left or the right. H is represented in the
     !! form
     !! H = I - tau * v * v**H
     !! where tau is a complex scalar and v is a complex vector.
     !! If tau = 0, then H is taken to be the unit matrix
     !! This version uses inline code if H has order < 11.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side
           integer(ilp64), intent(in) :: ldc, m, n
           complex(dp), intent(in) :: tau
           ! Array Arguments 
           complex(dp), intent(inout) :: c(ldc,*)
           complex(dp), intent(in) :: v(*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp64) :: j
           complex(dp) :: sum, t1, t10, t2, t3, t4, t5, t6, t7, t8, t9, v1, v10, v2, v3, v4, v5, &
                     v6, v7, v8, v9
           ! Intrinsic Functions 
           ! Executable Statements 
           if( tau==czero )return
           if( stdlib_lsame( side, 'L' ) ) then
              ! form  h * c, where h has order m.
              go to ( 10, 30, 50, 70, 90, 110, 130, 150,170, 190 )m
              ! code for general m
              call stdlib_I64_zlarf( side, m, n, v, 1_ilp64, tau, c, ldc, work )
              go to 410
              10 continue
              ! special code for 1 x 1 householder
              t1 = cone - tau*v( 1_ilp64 )*conjg( v( 1_ilp64 ) )
              do j = 1, n
                 c( 1_ilp64, j ) = t1*c( 1_ilp64, j )
              end do
              go to 410
              30 continue
              ! special code for 2 x 2 householder
              v1 = conjg( v( 1_ilp64 ) )
              t1 = tau*conjg( v1 )
              v2 = conjg( v( 2_ilp64 ) )
              t2 = tau*conjg( v2 )
              do j = 1, n
                 sum = v1*c( 1_ilp64, j ) + v2*c( 2_ilp64, j )
                 c( 1_ilp64, j ) = c( 1_ilp64, j ) - sum*t1
                 c( 2_ilp64, j ) = c( 2_ilp64, j ) - sum*t2
              end do
              go to 410
              50 continue
              ! special code for 3 x 3 householder
              v1 = conjg( v( 1_ilp64 ) )
              t1 = tau*conjg( v1 )
              v2 = conjg( v( 2_ilp64 ) )
              t2 = tau*conjg( v2 )
              v3 = conjg( v( 3_ilp64 ) )
              t3 = tau*conjg( v3 )
              do j = 1, n
                 sum = v1*c( 1_ilp64, j ) + v2*c( 2_ilp64, j ) + v3*c( 3_ilp64, j )
                 c( 1_ilp64, j ) = c( 1_ilp64, j ) - sum*t1
                 c( 2_ilp64, j ) = c( 2_ilp64, j ) - sum*t2
                 c( 3_ilp64, j ) = c( 3_ilp64, j ) - sum*t3
              end do
              go to 410
              70 continue
              ! special code for 4 x 4 householder
              v1 = conjg( v( 1_ilp64 ) )
              t1 = tau*conjg( v1 )
              v2 = conjg( v( 2_ilp64 ) )
              t2 = tau*conjg( v2 )
              v3 = conjg( v( 3_ilp64 ) )
              t3 = tau*conjg( v3 )
              v4 = conjg( v( 4_ilp64 ) )
              t4 = tau*conjg( v4 )
              do j = 1, n
                 sum = v1*c( 1_ilp64, j ) + v2*c( 2_ilp64, j ) + v3*c( 3_ilp64, j ) +v4*c( 4_ilp64, j )
                 c( 1_ilp64, j ) = c( 1_ilp64, j ) - sum*t1
                 c( 2_ilp64, j ) = c( 2_ilp64, j ) - sum*t2
                 c( 3_ilp64, j ) = c( 3_ilp64, j ) - sum*t3
                 c( 4_ilp64, j ) = c( 4_ilp64, j ) - sum*t4
              end do
              go to 410
              90 continue
              ! special code for 5 x 5 householder
              v1 = conjg( v( 1_ilp64 ) )
              t1 = tau*conjg( v1 )
              v2 = conjg( v( 2_ilp64 ) )
              t2 = tau*conjg( v2 )
              v3 = conjg( v( 3_ilp64 ) )
              t3 = tau*conjg( v3 )
              v4 = conjg( v( 4_ilp64 ) )
              t4 = tau*conjg( v4 )
              v5 = conjg( v( 5_ilp64 ) )
              t5 = tau*conjg( v5 )
              do j = 1, n
                 sum = v1*c( 1_ilp64, j ) + v2*c( 2_ilp64, j ) + v3*c( 3_ilp64, j ) +v4*c( 4_ilp64, j ) + v5*c( 5_ilp64, j )
                           
                 c( 1_ilp64, j ) = c( 1_ilp64, j ) - sum*t1
                 c( 2_ilp64, j ) = c( 2_ilp64, j ) - sum*t2
                 c( 3_ilp64, j ) = c( 3_ilp64, j ) - sum*t3
                 c( 4_ilp64, j ) = c( 4_ilp64, j ) - sum*t4
                 c( 5_ilp64, j ) = c( 5_ilp64, j ) - sum*t5
              end do
              go to 410
              110 continue
              ! special code for 6 x 6 householder
              v1 = conjg( v( 1_ilp64 ) )
              t1 = tau*conjg( v1 )
              v2 = conjg( v( 2_ilp64 ) )
              t2 = tau*conjg( v2 )
              v3 = conjg( v( 3_ilp64 ) )
              t3 = tau*conjg( v3 )
              v4 = conjg( v( 4_ilp64 ) )
              t4 = tau*conjg( v4 )
              v5 = conjg( v( 5_ilp64 ) )
              t5 = tau*conjg( v5 )
              v6 = conjg( v( 6_ilp64 ) )
              t6 = tau*conjg( v6 )
              do j = 1, n
                 sum = v1*c( 1_ilp64, j ) + v2*c( 2_ilp64, j ) + v3*c( 3_ilp64, j ) +v4*c( 4_ilp64, j ) + v5*c( 5_ilp64, j ) + &
                           v6*c( 6_ilp64, j )
                 c( 1_ilp64, j ) = c( 1_ilp64, j ) - sum*t1
                 c( 2_ilp64, j ) = c( 2_ilp64, j ) - sum*t2
                 c( 3_ilp64, j ) = c( 3_ilp64, j ) - sum*t3
                 c( 4_ilp64, j ) = c( 4_ilp64, j ) - sum*t4
                 c( 5_ilp64, j ) = c( 5_ilp64, j ) - sum*t5
                 c( 6_ilp64, j ) = c( 6_ilp64, j ) - sum*t6
              end do
              go to 410
              130 continue
              ! special code for 7 x 7 householder
              v1 = conjg( v( 1_ilp64 ) )
              t1 = tau*conjg( v1 )
              v2 = conjg( v( 2_ilp64 ) )
              t2 = tau*conjg( v2 )
              v3 = conjg( v( 3_ilp64 ) )
              t3 = tau*conjg( v3 )
              v4 = conjg( v( 4_ilp64 ) )
              t4 = tau*conjg( v4 )
              v5 = conjg( v( 5_ilp64 ) )
              t5 = tau*conjg( v5 )
              v6 = conjg( v( 6_ilp64 ) )
              t6 = tau*conjg( v6 )
              v7 = conjg( v( 7_ilp64 ) )
              t7 = tau*conjg( v7 )
              do j = 1, n
                 sum = v1*c( 1_ilp64, j ) + v2*c( 2_ilp64, j ) + v3*c( 3_ilp64, j ) +v4*c( 4_ilp64, j ) + v5*c( 5_ilp64, j ) + &
                           v6*c( 6_ilp64, j ) +v7*c( 7_ilp64, j )
                 c( 1_ilp64, j ) = c( 1_ilp64, j ) - sum*t1
                 c( 2_ilp64, j ) = c( 2_ilp64, j ) - sum*t2
                 c( 3_ilp64, j ) = c( 3_ilp64, j ) - sum*t3
                 c( 4_ilp64, j ) = c( 4_ilp64, j ) - sum*t4
                 c( 5_ilp64, j ) = c( 5_ilp64, j ) - sum*t5
                 c( 6_ilp64, j ) = c( 6_ilp64, j ) - sum*t6
                 c( 7_ilp64, j ) = c( 7_ilp64, j ) - sum*t7
              end do
              go to 410
              150 continue
              ! special code for 8 x 8 householder
              v1 = conjg( v( 1_ilp64 ) )
              t1 = tau*conjg( v1 )
              v2 = conjg( v( 2_ilp64 ) )
              t2 = tau*conjg( v2 )
              v3 = conjg( v( 3_ilp64 ) )
              t3 = tau*conjg( v3 )
              v4 = conjg( v( 4_ilp64 ) )
              t4 = tau*conjg( v4 )
              v5 = conjg( v( 5_ilp64 ) )
              t5 = tau*conjg( v5 )
              v6 = conjg( v( 6_ilp64 ) )
              t6 = tau*conjg( v6 )
              v7 = conjg( v( 7_ilp64 ) )
              t7 = tau*conjg( v7 )
              v8 = conjg( v( 8_ilp64 ) )
              t8 = tau*conjg( v8 )
              do j = 1, n
                 sum = v1*c( 1_ilp64, j ) + v2*c( 2_ilp64, j ) + v3*c( 3_ilp64, j ) +v4*c( 4_ilp64, j ) + v5*c( 5_ilp64, j ) + &
                           v6*c( 6_ilp64, j ) +v7*c( 7_ilp64, j ) + v8*c( 8_ilp64, j )
                 c( 1_ilp64, j ) = c( 1_ilp64, j ) - sum*t1
                 c( 2_ilp64, j ) = c( 2_ilp64, j ) - sum*t2
                 c( 3_ilp64, j ) = c( 3_ilp64, j ) - sum*t3
                 c( 4_ilp64, j ) = c( 4_ilp64, j ) - sum*t4
                 c( 5_ilp64, j ) = c( 5_ilp64, j ) - sum*t5
                 c( 6_ilp64, j ) = c( 6_ilp64, j ) - sum*t6
                 c( 7_ilp64, j ) = c( 7_ilp64, j ) - sum*t7
                 c( 8_ilp64, j ) = c( 8_ilp64, j ) - sum*t8
              end do
              go to 410
              170 continue
              ! special code for 9 x 9 householder
              v1 = conjg( v( 1_ilp64 ) )
              t1 = tau*conjg( v1 )
              v2 = conjg( v( 2_ilp64 ) )
              t2 = tau*conjg( v2 )
              v3 = conjg( v( 3_ilp64 ) )
              t3 = tau*conjg( v3 )
              v4 = conjg( v( 4_ilp64 ) )
              t4 = tau*conjg( v4 )
              v5 = conjg( v( 5_ilp64 ) )
              t5 = tau*conjg( v5 )
              v6 = conjg( v( 6_ilp64 ) )
              t6 = tau*conjg( v6 )
              v7 = conjg( v( 7_ilp64 ) )
              t7 = tau*conjg( v7 )
              v8 = conjg( v( 8_ilp64 ) )
              t8 = tau*conjg( v8 )
              v9 = conjg( v( 9_ilp64 ) )
              t9 = tau*conjg( v9 )
              do j = 1, n
                 sum = v1*c( 1_ilp64, j ) + v2*c( 2_ilp64, j ) + v3*c( 3_ilp64, j ) +v4*c( 4_ilp64, j ) + v5*c( 5_ilp64, j ) + &
                           v6*c( 6_ilp64, j ) +v7*c( 7_ilp64, j ) + v8*c( 8_ilp64, j ) + v9*c( 9_ilp64, j )
                 c( 1_ilp64, j ) = c( 1_ilp64, j ) - sum*t1
                 c( 2_ilp64, j ) = c( 2_ilp64, j ) - sum*t2
                 c( 3_ilp64, j ) = c( 3_ilp64, j ) - sum*t3
                 c( 4_ilp64, j ) = c( 4_ilp64, j ) - sum*t4
                 c( 5_ilp64, j ) = c( 5_ilp64, j ) - sum*t5
                 c( 6_ilp64, j ) = c( 6_ilp64, j ) - sum*t6
                 c( 7_ilp64, j ) = c( 7_ilp64, j ) - sum*t7
                 c( 8_ilp64, j ) = c( 8_ilp64, j ) - sum*t8
                 c( 9_ilp64, j ) = c( 9_ilp64, j ) - sum*t9
              end do
              go to 410
              190 continue
              ! special code for 10 x 10 householder
              v1 = conjg( v( 1_ilp64 ) )
              t1 = tau*conjg( v1 )
              v2 = conjg( v( 2_ilp64 ) )
              t2 = tau*conjg( v2 )
              v3 = conjg( v( 3_ilp64 ) )
              t3 = tau*conjg( v3 )
              v4 = conjg( v( 4_ilp64 ) )
              t4 = tau*conjg( v4 )
              v5 = conjg( v( 5_ilp64 ) )
              t5 = tau*conjg( v5 )
              v6 = conjg( v( 6_ilp64 ) )
              t6 = tau*conjg( v6 )
              v7 = conjg( v( 7_ilp64 ) )
              t7 = tau*conjg( v7 )
              v8 = conjg( v( 8_ilp64 ) )
              t8 = tau*conjg( v8 )
              v9 = conjg( v( 9_ilp64 ) )
              t9 = tau*conjg( v9 )
              v10 = conjg( v( 10_ilp64 ) )
              t10 = tau*conjg( v10 )
              do j = 1, n
                 sum = v1*c( 1_ilp64, j ) + v2*c( 2_ilp64, j ) + v3*c( 3_ilp64, j ) +v4*c( 4_ilp64, j ) + v5*c( 5_ilp64, j ) + &
                           v6*c( 6_ilp64, j ) +v7*c( 7_ilp64, j ) + v8*c( 8_ilp64, j ) + v9*c( 9_ilp64, j ) +v10*c( 10_ilp64, j )
                 c( 1_ilp64, j ) = c( 1_ilp64, j ) - sum*t1
                 c( 2_ilp64, j ) = c( 2_ilp64, j ) - sum*t2
                 c( 3_ilp64, j ) = c( 3_ilp64, j ) - sum*t3
                 c( 4_ilp64, j ) = c( 4_ilp64, j ) - sum*t4
                 c( 5_ilp64, j ) = c( 5_ilp64, j ) - sum*t5
                 c( 6_ilp64, j ) = c( 6_ilp64, j ) - sum*t6
                 c( 7_ilp64, j ) = c( 7_ilp64, j ) - sum*t7
                 c( 8_ilp64, j ) = c( 8_ilp64, j ) - sum*t8
                 c( 9_ilp64, j ) = c( 9_ilp64, j ) - sum*t9
                 c( 10_ilp64, j ) = c( 10_ilp64, j ) - sum*t10
              end do
              go to 410
           else
              ! form  c * h, where h has order n.
              go to ( 210, 230, 250, 270, 290, 310, 330, 350,370, 390 )n
              ! code for general n
              call stdlib_I64_zlarf( side, m, n, v, 1_ilp64, tau, c, ldc, work )
              go to 410
              210 continue
              ! special code for 1 x 1 householder
              t1 = cone - tau*v( 1_ilp64 )*conjg( v( 1_ilp64 ) )
              do j = 1, m
                 c( j, 1_ilp64 ) = t1*c( j, 1_ilp64 )
              end do
              go to 410
              230 continue
              ! special code for 2 x 2 householder
              v1 = v( 1_ilp64 )
              t1 = tau*conjg( v1 )
              v2 = v( 2_ilp64 )
              t2 = tau*conjg( v2 )
              do j = 1, m
                 sum = v1*c( j, 1_ilp64 ) + v2*c( j, 2_ilp64 )
                 c( j, 1_ilp64 ) = c( j, 1_ilp64 ) - sum*t1
                 c( j, 2_ilp64 ) = c( j, 2_ilp64 ) - sum*t2
              end do
              go to 410
              250 continue
              ! special code for 3 x 3 householder
              v1 = v( 1_ilp64 )
              t1 = tau*conjg( v1 )
              v2 = v( 2_ilp64 )
              t2 = tau*conjg( v2 )
              v3 = v( 3_ilp64 )
              t3 = tau*conjg( v3 )
              do j = 1, m
                 sum = v1*c( j, 1_ilp64 ) + v2*c( j, 2_ilp64 ) + v3*c( j, 3_ilp64 )
                 c( j, 1_ilp64 ) = c( j, 1_ilp64 ) - sum*t1
                 c( j, 2_ilp64 ) = c( j, 2_ilp64 ) - sum*t2
                 c( j, 3_ilp64 ) = c( j, 3_ilp64 ) - sum*t3
              end do
              go to 410
              270 continue
              ! special code for 4 x 4 householder
              v1 = v( 1_ilp64 )
              t1 = tau*conjg( v1 )
              v2 = v( 2_ilp64 )
              t2 = tau*conjg( v2 )
              v3 = v( 3_ilp64 )
              t3 = tau*conjg( v3 )
              v4 = v( 4_ilp64 )
              t4 = tau*conjg( v4 )
              do j = 1, m
                 sum = v1*c( j, 1_ilp64 ) + v2*c( j, 2_ilp64 ) + v3*c( j, 3_ilp64 ) +v4*c( j, 4_ilp64 )
                 c( j, 1_ilp64 ) = c( j, 1_ilp64 ) - sum*t1
                 c( j, 2_ilp64 ) = c( j, 2_ilp64 ) - sum*t2
                 c( j, 3_ilp64 ) = c( j, 3_ilp64 ) - sum*t3
                 c( j, 4_ilp64 ) = c( j, 4_ilp64 ) - sum*t4
              end do
              go to 410
              290 continue
              ! special code for 5 x 5 householder
              v1 = v( 1_ilp64 )
              t1 = tau*conjg( v1 )
              v2 = v( 2_ilp64 )
              t2 = tau*conjg( v2 )
              v3 = v( 3_ilp64 )
              t3 = tau*conjg( v3 )
              v4 = v( 4_ilp64 )
              t4 = tau*conjg( v4 )
              v5 = v( 5_ilp64 )
              t5 = tau*conjg( v5 )
              do j = 1, m
                 sum = v1*c( j, 1_ilp64 ) + v2*c( j, 2_ilp64 ) + v3*c( j, 3_ilp64 ) +v4*c( j, 4_ilp64 ) + v5*c( j, 5_ilp64 )
                           
                 c( j, 1_ilp64 ) = c( j, 1_ilp64 ) - sum*t1
                 c( j, 2_ilp64 ) = c( j, 2_ilp64 ) - sum*t2
                 c( j, 3_ilp64 ) = c( j, 3_ilp64 ) - sum*t3
                 c( j, 4_ilp64 ) = c( j, 4_ilp64 ) - sum*t4
                 c( j, 5_ilp64 ) = c( j, 5_ilp64 ) - sum*t5
              end do
              go to 410
              310 continue
              ! special code for 6 x 6 householder
              v1 = v( 1_ilp64 )
              t1 = tau*conjg( v1 )
              v2 = v( 2_ilp64 )
              t2 = tau*conjg( v2 )
              v3 = v( 3_ilp64 )
              t3 = tau*conjg( v3 )
              v4 = v( 4_ilp64 )
              t4 = tau*conjg( v4 )
              v5 = v( 5_ilp64 )
              t5 = tau*conjg( v5 )
              v6 = v( 6_ilp64 )
              t6 = tau*conjg( v6 )
              do j = 1, m
                 sum = v1*c( j, 1_ilp64 ) + v2*c( j, 2_ilp64 ) + v3*c( j, 3_ilp64 ) +v4*c( j, 4_ilp64 ) + v5*c( j, 5_ilp64 ) + &
                           v6*c( j, 6_ilp64 )
                 c( j, 1_ilp64 ) = c( j, 1_ilp64 ) - sum*t1
                 c( j, 2_ilp64 ) = c( j, 2_ilp64 ) - sum*t2
                 c( j, 3_ilp64 ) = c( j, 3_ilp64 ) - sum*t3
                 c( j, 4_ilp64 ) = c( j, 4_ilp64 ) - sum*t4
                 c( j, 5_ilp64 ) = c( j, 5_ilp64 ) - sum*t5
                 c( j, 6_ilp64 ) = c( j, 6_ilp64 ) - sum*t6
              end do
              go to 410
              330 continue
              ! special code for 7 x 7 householder
              v1 = v( 1_ilp64 )
              t1 = tau*conjg( v1 )
              v2 = v( 2_ilp64 )
              t2 = tau*conjg( v2 )
              v3 = v( 3_ilp64 )
              t3 = tau*conjg( v3 )
              v4 = v( 4_ilp64 )
              t4 = tau*conjg( v4 )
              v5 = v( 5_ilp64 )
              t5 = tau*conjg( v5 )
              v6 = v( 6_ilp64 )
              t6 = tau*conjg( v6 )
              v7 = v( 7_ilp64 )
              t7 = tau*conjg( v7 )
              do j = 1, m
                 sum = v1*c( j, 1_ilp64 ) + v2*c( j, 2_ilp64 ) + v3*c( j, 3_ilp64 ) +v4*c( j, 4_ilp64 ) + v5*c( j, 5_ilp64 ) + &
                           v6*c( j, 6_ilp64 ) +v7*c( j, 7_ilp64 )
                 c( j, 1_ilp64 ) = c( j, 1_ilp64 ) - sum*t1
                 c( j, 2_ilp64 ) = c( j, 2_ilp64 ) - sum*t2
                 c( j, 3_ilp64 ) = c( j, 3_ilp64 ) - sum*t3
                 c( j, 4_ilp64 ) = c( j, 4_ilp64 ) - sum*t4
                 c( j, 5_ilp64 ) = c( j, 5_ilp64 ) - sum*t5
                 c( j, 6_ilp64 ) = c( j, 6_ilp64 ) - sum*t6
                 c( j, 7_ilp64 ) = c( j, 7_ilp64 ) - sum*t7
              end do
              go to 410
              350 continue
              ! special code for 8 x 8 householder
              v1 = v( 1_ilp64 )
              t1 = tau*conjg( v1 )
              v2 = v( 2_ilp64 )
              t2 = tau*conjg( v2 )
              v3 = v( 3_ilp64 )
              t3 = tau*conjg( v3 )
              v4 = v( 4_ilp64 )
              t4 = tau*conjg( v4 )
              v5 = v( 5_ilp64 )
              t5 = tau*conjg( v5 )
              v6 = v( 6_ilp64 )
              t6 = tau*conjg( v6 )
              v7 = v( 7_ilp64 )
              t7 = tau*conjg( v7 )
              v8 = v( 8_ilp64 )
              t8 = tau*conjg( v8 )
              do j = 1, m
                 sum = v1*c( j, 1_ilp64 ) + v2*c( j, 2_ilp64 ) + v3*c( j, 3_ilp64 ) +v4*c( j, 4_ilp64 ) + v5*c( j, 5_ilp64 ) + &
                           v6*c( j, 6_ilp64 ) +v7*c( j, 7_ilp64 ) + v8*c( j, 8_ilp64 )
                 c( j, 1_ilp64 ) = c( j, 1_ilp64 ) - sum*t1
                 c( j, 2_ilp64 ) = c( j, 2_ilp64 ) - sum*t2
                 c( j, 3_ilp64 ) = c( j, 3_ilp64 ) - sum*t3
                 c( j, 4_ilp64 ) = c( j, 4_ilp64 ) - sum*t4
                 c( j, 5_ilp64 ) = c( j, 5_ilp64 ) - sum*t5
                 c( j, 6_ilp64 ) = c( j, 6_ilp64 ) - sum*t6
                 c( j, 7_ilp64 ) = c( j, 7_ilp64 ) - sum*t7
                 c( j, 8_ilp64 ) = c( j, 8_ilp64 ) - sum*t8
              end do
              go to 410
              370 continue
              ! special code for 9 x 9 householder
              v1 = v( 1_ilp64 )
              t1 = tau*conjg( v1 )
              v2 = v( 2_ilp64 )
              t2 = tau*conjg( v2 )
              v3 = v( 3_ilp64 )
              t3 = tau*conjg( v3 )
              v4 = v( 4_ilp64 )
              t4 = tau*conjg( v4 )
              v5 = v( 5_ilp64 )
              t5 = tau*conjg( v5 )
              v6 = v( 6_ilp64 )
              t6 = tau*conjg( v6 )
              v7 = v( 7_ilp64 )
              t7 = tau*conjg( v7 )
              v8 = v( 8_ilp64 )
              t8 = tau*conjg( v8 )
              v9 = v( 9_ilp64 )
              t9 = tau*conjg( v9 )
              do j = 1, m
                 sum = v1*c( j, 1_ilp64 ) + v2*c( j, 2_ilp64 ) + v3*c( j, 3_ilp64 ) +v4*c( j, 4_ilp64 ) + v5*c( j, 5_ilp64 ) + &
                           v6*c( j, 6_ilp64 ) +v7*c( j, 7_ilp64 ) + v8*c( j, 8_ilp64 ) + v9*c( j, 9_ilp64 )
                 c( j, 1_ilp64 ) = c( j, 1_ilp64 ) - sum*t1
                 c( j, 2_ilp64 ) = c( j, 2_ilp64 ) - sum*t2
                 c( j, 3_ilp64 ) = c( j, 3_ilp64 ) - sum*t3
                 c( j, 4_ilp64 ) = c( j, 4_ilp64 ) - sum*t4
                 c( j, 5_ilp64 ) = c( j, 5_ilp64 ) - sum*t5
                 c( j, 6_ilp64 ) = c( j, 6_ilp64 ) - sum*t6
                 c( j, 7_ilp64 ) = c( j, 7_ilp64 ) - sum*t7
                 c( j, 8_ilp64 ) = c( j, 8_ilp64 ) - sum*t8
                 c( j, 9_ilp64 ) = c( j, 9_ilp64 ) - sum*t9
              end do
              go to 410
              390 continue
              ! special code for 10 x 10 householder
              v1 = v( 1_ilp64 )
              t1 = tau*conjg( v1 )
              v2 = v( 2_ilp64 )
              t2 = tau*conjg( v2 )
              v3 = v( 3_ilp64 )
              t3 = tau*conjg( v3 )
              v4 = v( 4_ilp64 )
              t4 = tau*conjg( v4 )
              v5 = v( 5_ilp64 )
              t5 = tau*conjg( v5 )
              v6 = v( 6_ilp64 )
              t6 = tau*conjg( v6 )
              v7 = v( 7_ilp64 )
              t7 = tau*conjg( v7 )
              v8 = v( 8_ilp64 )
              t8 = tau*conjg( v8 )
              v9 = v( 9_ilp64 )
              t9 = tau*conjg( v9 )
              v10 = v( 10_ilp64 )
              t10 = tau*conjg( v10 )
              do j = 1, m
                 sum = v1*c( j, 1_ilp64 ) + v2*c( j, 2_ilp64 ) + v3*c( j, 3_ilp64 ) +v4*c( j, 4_ilp64 ) + v5*c( j, 5_ilp64 ) + &
                           v6*c( j, 6_ilp64 ) +v7*c( j, 7_ilp64 ) + v8*c( j, 8_ilp64 ) + v9*c( j, 9_ilp64 ) +v10*c( j, 10_ilp64 )
                 c( j, 1_ilp64 ) = c( j, 1_ilp64 ) - sum*t1
                 c( j, 2_ilp64 ) = c( j, 2_ilp64 ) - sum*t2
                 c( j, 3_ilp64 ) = c( j, 3_ilp64 ) - sum*t3
                 c( j, 4_ilp64 ) = c( j, 4_ilp64 ) - sum*t4
                 c( j, 5_ilp64 ) = c( j, 5_ilp64 ) - sum*t5
                 c( j, 6_ilp64 ) = c( j, 6_ilp64 ) - sum*t6
                 c( j, 7_ilp64 ) = c( j, 7_ilp64 ) - sum*t7
                 c( j, 8_ilp64 ) = c( j, 8_ilp64 ) - sum*t8
                 c( j, 9_ilp64 ) = c( j, 9_ilp64 ) - sum*t9
                 c( j, 10_ilp64 ) = c( j, 10_ilp64 ) - sum*t10
              end do
              go to 410
           end if
           410 continue
           return
     end subroutine stdlib_I64_zlarfx




     pure module subroutine stdlib_I64_slarfy( uplo, n, v, incv, tau, c, ldc, work )
     !! SLARFY applies an elementary reflector, or Householder matrix, H,
     !! to an n x n symmetric matrix C, from both the left and the right.
     !! H is represented in the form
     !! H = I - tau * v * v'
     !! where  tau  is a scalar and  v  is a vector.
     !! If  tau  is  zero, then  H  is taken to be the unit matrix.
        ! -- lapack test routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(in) :: incv, ldc, n
           real(sp), intent(in) :: tau
           ! Array Arguments 
           real(sp), intent(inout) :: c(ldc,*)
           real(sp), intent(in) :: v(*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           real(sp) :: alpha
           ! Executable Statements 
           if( tau==zero )return
           ! form  w:= c * v
           call stdlib_I64_ssymv( uplo, n, one, c, ldc, v, incv, zero, work, 1_ilp64 )
           alpha = -half*tau*stdlib_I64_sdot( n, work, 1_ilp64, v, incv )
           call stdlib_I64_saxpy( n, alpha, v, incv, work, 1_ilp64 )
           ! c := c - v * w' - w * v'
           call stdlib_I64_ssyr2( uplo, n, -tau, v, incv, work, 1_ilp64, c, ldc )
           return
     end subroutine stdlib_I64_slarfy

     pure module subroutine stdlib_I64_dlarfy( uplo, n, v, incv, tau, c, ldc, work )
     !! DLARFY applies an elementary reflector, or Householder matrix, H,
     !! to an n x n symmetric matrix C, from both the left and the right.
     !! H is represented in the form
     !! H = I - tau * v * v'
     !! where  tau  is a scalar and  v  is a vector.
     !! If  tau  is  zero, then  H  is taken to be the unit matrix.
        ! -- lapack test routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(in) :: incv, ldc, n
           real(dp), intent(in) :: tau
           ! Array Arguments 
           real(dp), intent(inout) :: c(ldc,*)
           real(dp), intent(in) :: v(*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           real(dp) :: alpha
           ! Executable Statements 
           if( tau==zero )return
           ! form  w:= c * v
           call stdlib_I64_dsymv( uplo, n, one, c, ldc, v, incv, zero, work, 1_ilp64 )
           alpha = -half*tau*stdlib_I64_ddot( n, work, 1_ilp64, v, incv )
           call stdlib_I64_daxpy( n, alpha, v, incv, work, 1_ilp64 )
           ! c := c - v * w' - w * v'
           call stdlib_I64_dsyr2( uplo, n, -tau, v, incv, work, 1_ilp64, c, ldc )
           return
     end subroutine stdlib_I64_dlarfy


     pure module subroutine stdlib_I64_clarfy( uplo, n, v, incv, tau, c, ldc, work )
     !! CLARFY applies an elementary reflector, or Householder matrix, H,
     !! to an n x n Hermitian matrix C, from both the left and the right.
     !! H is represented in the form
     !! H = I - tau * v * v'
     !! where  tau  is a scalar and  v  is a vector.
     !! If  tau  is  zero, then  H  is taken to be the unit matrix.
        ! -- lapack test routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(in) :: incv, ldc, n
           complex(sp), intent(in) :: tau
           ! Array Arguments 
           complex(sp), intent(inout) :: c(ldc,*)
           complex(sp), intent(in) :: v(*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           complex(sp) :: alpha
           ! Executable Statements 
           if( tau==czero )return
           ! form  w:= c * v
           call stdlib_I64_chemv( uplo, n, cone, c, ldc, v, incv, czero, work, 1_ilp64 )
           alpha = -chalf*tau*stdlib_I64_cdotc( n, work, 1_ilp64, v, incv )
           call stdlib_I64_caxpy( n, alpha, v, incv, work, 1_ilp64 )
           ! c := c - v * w' - w * v'
           call stdlib_I64_cher2( uplo, n, -tau, v, incv, work, 1_ilp64, c, ldc )
           return
     end subroutine stdlib_I64_clarfy

     pure module subroutine stdlib_I64_zlarfy( uplo, n, v, incv, tau, c, ldc, work )
     !! ZLARFY applies an elementary reflector, or Householder matrix, H,
     !! to an n x n Hermitian matrix C, from both the left and the right.
     !! H is represented in the form
     !! H = I - tau * v * v'
     !! where  tau  is a scalar and  v  is a vector.
     !! If  tau  is  zero, then  H  is taken to be the unit matrix.
        ! -- lapack test routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(in) :: incv, ldc, n
           complex(dp), intent(in) :: tau
           ! Array Arguments 
           complex(dp), intent(inout) :: c(ldc,*)
           complex(dp), intent(in) :: v(*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           complex(dp) :: alpha
           ! Executable Statements 
           if( tau==czero )return
           ! form  w:= c * v
           call stdlib_I64_zhemv( uplo, n, cone, c, ldc, v, incv, czero, work, 1_ilp64 )
           alpha = -chalf*tau*stdlib_I64_zdotc( n, work, 1_ilp64, v, incv )
           call stdlib_I64_zaxpy( n, alpha, v, incv, work, 1_ilp64 )
           ! c := c - v * w' - w * v'
           call stdlib_I64_zher2( uplo, n, -tau, v, incv, work, 1_ilp64, c, ldc )
           return
     end subroutine stdlib_I64_zlarfy




     pure module subroutine stdlib_I64_slarfb( side, trans, direct, storev, m, n, k, v, ldv,t, ldt, c, ldc, &
     !! SLARFB applies a real block reflector H or its transpose H**T to a
     !! real m by n matrix C, from either the left or the right.
               work, ldwork )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: direct, side, storev, trans
           integer(ilp64), intent(in) :: k, ldc, ldt, ldv, ldwork, m, n
           ! Array Arguments 
           real(sp), intent(inout) :: c(ldc,*)
           real(sp), intent(in) :: t(ldt,*), v(ldv,*)
           real(sp), intent(out) :: work(ldwork,*)
        ! =====================================================================
           
           ! Local Scalars 
           character :: transt
           integer(ilp64) :: i, j
           ! Executable Statements 
           ! quick return if possible
           if( m<=0 .or. n<=0 )return
           if( stdlib_lsame( trans, 'N' ) ) then
              transt = 'T'
           else
              transt = 'N'
           end if
           if( stdlib_lsame( storev, 'C' ) ) then
              if( stdlib_lsame( direct, 'F' ) ) then
                 ! let  v =  ( v1 )    (first k rows)
                           ! ( v2 )
                 ! where  v1  is unit lower triangular.
                 if( stdlib_lsame( side, 'L' ) ) then
                    ! form  h * c  or  h**t * c  where  c = ( c1 )
                                                          ! ( c2 )
                    ! w := c**t * v  =  (c1**t * v1 + c2**t * v2)  (stored in work)
                    ! w := c1**t
                    do j = 1, k
                       call stdlib_I64_scopy( n, c( j, 1_ilp64 ), ldc, work( 1_ilp64, j ), 1_ilp64 )
                    end do
                    ! w := w * v1
                    call stdlib_I64_strmm( 'RIGHT', 'LOWER', 'NO TRANSPOSE', 'UNIT', n,k, one, v, ldv,&
                               work, ldwork )
                    if( m>k ) then
                       ! w := w + c2**t * v2
                       call stdlib_I64_sgemm( 'TRANSPOSE', 'NO TRANSPOSE', n, k, m-k,one, c( k+1, 1_ilp64 ),&
                                  ldc, v( k+1, 1_ilp64 ), ldv,one, work, ldwork )
                    end if
                    ! w := w * t**t  or  w * t
                    call stdlib_I64_strmm( 'RIGHT', 'UPPER', transt, 'NON-UNIT', n, k,one, t, ldt, &
                              work, ldwork )
                    ! c := c - v * w**t
                    if( m>k ) then
                       ! c2 := c2 - v2 * w**t
                       call stdlib_I64_sgemm( 'NO TRANSPOSE', 'TRANSPOSE', m-k, n, k,-one, v( k+1, 1_ilp64 )&
                                 , ldv, work, ldwork, one,c( k+1, 1_ilp64 ), ldc )
                    end if
                    ! w := w * v1**t
                    call stdlib_I64_strmm( 'RIGHT', 'LOWER', 'TRANSPOSE', 'UNIT', n, k,one, v, ldv, &
                              work, ldwork )
                    ! c1 := c1 - w**t
                    do j = 1, k
                       do i = 1, n
                          c( j, i ) = c( j, i ) - work( i, j )
                       end do
                    end do
                 else if( stdlib_lsame( side, 'R' ) ) then
                    ! form  c * h  or  c * h**t  where  c = ( c1  c2 )
                    ! w := c * v  =  (c1*v1 + c2*v2)  (stored in work)
                    ! w := c1
                    do j = 1, k
                       call stdlib_I64_scopy( m, c( 1_ilp64, j ), 1_ilp64, work( 1_ilp64, j ), 1_ilp64 )
                    end do
                    ! w := w * v1
                    call stdlib_I64_strmm( 'RIGHT', 'LOWER', 'NO TRANSPOSE', 'UNIT', m,k, one, v, ldv,&
                               work, ldwork )
                    if( n>k ) then
                       ! w := w + c2 * v2
                       call stdlib_I64_sgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', m, k, n-k,one, c( 1_ilp64, k+&
                                 1_ilp64 ), ldc, v( k+1, 1_ilp64 ), ldv,one, work, ldwork )
                    end if
                    ! w := w * t  or  w * t**t
                    call stdlib_I64_strmm( 'RIGHT', 'UPPER', trans, 'NON-UNIT', m, k,one, t, ldt, &
                              work, ldwork )
                    ! c := c - w * v**t
                    if( n>k ) then
                       ! c2 := c2 - w * v2**t
                       call stdlib_I64_sgemm( 'NO TRANSPOSE', 'TRANSPOSE', m, n-k, k,-one, work, &
                                 ldwork, v( k+1, 1_ilp64 ), ldv, one,c( 1_ilp64, k+1 ), ldc )
                    end if
                    ! w := w * v1**t
                    call stdlib_I64_strmm( 'RIGHT', 'LOWER', 'TRANSPOSE', 'UNIT', m, k,one, v, ldv, &
                              work, ldwork )
                    ! c1 := c1 - w
                    do j = 1, k
                       do i = 1, m
                          c( i, j ) = c( i, j ) - work( i, j )
                       end do
                    end do
                 end if
              else
                 ! let  v =  ( v1 )
                           ! ( v2 )    (last k rows)
                 ! where  v2  is unit upper triangular.
                 if( stdlib_lsame( side, 'L' ) ) then
                    ! form  h * c  or  h**t * c  where  c = ( c1 )
                                                          ! ( c2 )
                    ! w := c**t * v  =  (c1**t * v1 + c2**t * v2)  (stored in work)
                    ! w := c2**t
                    do j = 1, k
                       call stdlib_I64_scopy( n, c( m-k+j, 1_ilp64 ), ldc, work( 1_ilp64, j ), 1_ilp64 )
                    end do
                    ! w := w * v2
                    call stdlib_I64_strmm( 'RIGHT', 'UPPER', 'NO TRANSPOSE', 'UNIT', n,k, one, v( m-k+&
                              1_ilp64, 1_ilp64 ), ldv, work, ldwork )
                    if( m>k ) then
                       ! w := w + c1**t * v1
                       call stdlib_I64_sgemm( 'TRANSPOSE', 'NO TRANSPOSE', n, k, m-k,one, c, ldc, v, &
                                 ldv, one, work, ldwork )
                    end if
                    ! w := w * t**t  or  w * t
                    call stdlib_I64_strmm( 'RIGHT', 'LOWER', transt, 'NON-UNIT', n, k,one, t, ldt, &
                              work, ldwork )
                    ! c := c - v * w**t
                    if( m>k ) then
                       ! c1 := c1 - v1 * w**t
                       call stdlib_I64_sgemm( 'NO TRANSPOSE', 'TRANSPOSE', m-k, n, k,-one, v, ldv, &
                                 work, ldwork, one, c, ldc )
                    end if
                    ! w := w * v2**t
                    call stdlib_I64_strmm( 'RIGHT', 'UPPER', 'TRANSPOSE', 'UNIT', n, k,one, v( m-k+1, &
                              1_ilp64 ), ldv, work, ldwork )
                    ! c2 := c2 - w**t
                    do j = 1, k
                       do i = 1, n
                          c( m-k+j, i ) = c( m-k+j, i ) - work( i, j )
                       end do
                    end do
                 else if( stdlib_lsame( side, 'R' ) ) then
                    ! form  c * h  or  c * h'  where  c = ( c1  c2 )
                    ! w := c * v  =  (c1*v1 + c2*v2)  (stored in work)
                    ! w := c2
                    do j = 1, k
                       call stdlib_I64_scopy( m, c( 1_ilp64, n-k+j ), 1_ilp64, work( 1_ilp64, j ), 1_ilp64 )
                    end do
                    ! w := w * v2
                    call stdlib_I64_strmm( 'RIGHT', 'UPPER', 'NO TRANSPOSE', 'UNIT', m,k, one, v( n-k+&
                              1_ilp64, 1_ilp64 ), ldv, work, ldwork )
                    if( n>k ) then
                       ! w := w + c1 * v1
                       call stdlib_I64_sgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', m, k, n-k,one, c, ldc, &
                                 v, ldv, one, work, ldwork )
                    end if
                    ! w := w * t  or  w * t**t
                    call stdlib_I64_strmm( 'RIGHT', 'LOWER', trans, 'NON-UNIT', m, k,one, t, ldt, &
                              work, ldwork )
                    ! c := c - w * v**t
                    if( n>k ) then
                       ! c1 := c1 - w * v1**t
                       call stdlib_I64_sgemm( 'NO TRANSPOSE', 'TRANSPOSE', m, n-k, k,-one, work, &
                                 ldwork, v, ldv, one, c, ldc )
                    end if
                    ! w := w * v2**t
                    call stdlib_I64_strmm( 'RIGHT', 'UPPER', 'TRANSPOSE', 'UNIT', m, k,one, v( n-k+1, &
                              1_ilp64 ), ldv, work, ldwork )
                    ! c2 := c2 - w
                    do j = 1, k
                       do i = 1, m
                          c( i, n-k+j ) = c( i, n-k+j ) - work( i, j )
                       end do
                    end do
                 end if
              end if
           else if( stdlib_lsame( storev, 'R' ) ) then
              if( stdlib_lsame( direct, 'F' ) ) then
                 ! let  v =  ( v1  v2 )    (v1: first k columns)
                 ! where  v1  is unit upper triangular.
                 if( stdlib_lsame( side, 'L' ) ) then
                    ! form  h * c  or  h**t * c  where  c = ( c1 )
                                                          ! ( c2 )
                    ! w := c**t * v**t  =  (c1**t * v1**t + c2**t * v2**t) (stored in work)
                    ! w := c1**t
                    do j = 1, k
                       call stdlib_I64_scopy( n, c( j, 1_ilp64 ), ldc, work( 1_ilp64, j ), 1_ilp64 )
                    end do
                    ! w := w * v1**t
                    call stdlib_I64_strmm( 'RIGHT', 'UPPER', 'TRANSPOSE', 'UNIT', n, k,one, v, ldv, &
                              work, ldwork )
                    if( m>k ) then
                       ! w := w + c2**t * v2**t
                       call stdlib_I64_sgemm( 'TRANSPOSE', 'TRANSPOSE', n, k, m-k, one,c( k+1, 1_ilp64 ), &
                                 ldc, v( 1_ilp64, k+1 ), ldv, one,work, ldwork )
                    end if
                    ! w := w * t**t  or  w * t
                    call stdlib_I64_strmm( 'RIGHT', 'UPPER', transt, 'NON-UNIT', n, k,one, t, ldt, &
                              work, ldwork )
                    ! c := c - v**t * w**t
                    if( m>k ) then
                       ! c2 := c2 - v2**t * w**t
                       call stdlib_I64_sgemm( 'TRANSPOSE', 'TRANSPOSE', m-k, n, k, -one,v( 1_ilp64, k+1 ), &
                                 ldv, work, ldwork, one,c( k+1, 1_ilp64 ), ldc )
                    end if
                    ! w := w * v1
                    call stdlib_I64_strmm( 'RIGHT', 'UPPER', 'NO TRANSPOSE', 'UNIT', n,k, one, v, ldv,&
                               work, ldwork )
                    ! c1 := c1 - w**t
                    do j = 1, k
                       do i = 1, n
                          c( j, i ) = c( j, i ) - work( i, j )
                       end do
                    end do
                 else if( stdlib_lsame( side, 'R' ) ) then
                    ! form  c * h  or  c * h**t  where  c = ( c1  c2 )
                    ! w := c * v**t  =  (c1*v1**t + c2*v2**t)  (stored in work)
                    ! w := c1
                    do j = 1, k
                       call stdlib_I64_scopy( m, c( 1_ilp64, j ), 1_ilp64, work( 1_ilp64, j ), 1_ilp64 )
                    end do
                    ! w := w * v1**t
                    call stdlib_I64_strmm( 'RIGHT', 'UPPER', 'TRANSPOSE', 'UNIT', m, k,one, v, ldv, &
                              work, ldwork )
                    if( n>k ) then
                       ! w := w + c2 * v2**t
                       call stdlib_I64_sgemm( 'NO TRANSPOSE', 'TRANSPOSE', m, k, n-k,one, c( 1_ilp64, k+1 ),&
                                  ldc, v( 1_ilp64, k+1 ), ldv,one, work, ldwork )
                    end if
                    ! w := w * t  or  w * t**t
                    call stdlib_I64_strmm( 'RIGHT', 'UPPER', trans, 'NON-UNIT', m, k,one, t, ldt, &
                              work, ldwork )
                    ! c := c - w * v
                    if( n>k ) then
                       ! c2 := c2 - w * v2
                       call stdlib_I64_sgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', m, n-k, k,-one, work, &
                                 ldwork, v( 1_ilp64, k+1 ), ldv, one,c( 1_ilp64, k+1 ), ldc )
                    end if
                    ! w := w * v1
                    call stdlib_I64_strmm( 'RIGHT', 'UPPER', 'NO TRANSPOSE', 'UNIT', m,k, one, v, ldv,&
                               work, ldwork )
                    ! c1 := c1 - w
                    do j = 1, k
                       do i = 1, m
                          c( i, j ) = c( i, j ) - work( i, j )
                       end do
                    end do
                 end if
              else
                 ! let  v =  ( v1  v2 )    (v2: last k columns)
                 ! where  v2  is unit lower triangular.
                 if( stdlib_lsame( side, 'L' ) ) then
                    ! form  h * c  or  h**t * c  where  c = ( c1 )
                                                          ! ( c2 )
                    ! w := c**t * v**t  =  (c1**t * v1**t + c2**t * v2**t) (stored in work)
                    ! w := c2**t
                    do j = 1, k
                       call stdlib_I64_scopy( n, c( m-k+j, 1_ilp64 ), ldc, work( 1_ilp64, j ), 1_ilp64 )
                    end do
                    ! w := w * v2**t
                    call stdlib_I64_strmm( 'RIGHT', 'LOWER', 'TRANSPOSE', 'UNIT', n, k,one, v( 1_ilp64, m-k+&
                              1_ilp64 ), ldv, work, ldwork )
                    if( m>k ) then
                       ! w := w + c1**t * v1**t
                       call stdlib_I64_sgemm( 'TRANSPOSE', 'TRANSPOSE', n, k, m-k, one,c, ldc, v, ldv,&
                                  one, work, ldwork )
                    end if
                    ! w := w * t**t  or  w * t
                    call stdlib_I64_strmm( 'RIGHT', 'LOWER', transt, 'NON-UNIT', n, k,one, t, ldt, &
                              work, ldwork )
                    ! c := c - v**t * w**t
                    if( m>k ) then
                       ! c1 := c1 - v1**t * w**t
                       call stdlib_I64_sgemm( 'TRANSPOSE', 'TRANSPOSE', m-k, n, k, -one,v, ldv, work, &
                                 ldwork, one, c, ldc )
                    end if
                    ! w := w * v2
                    call stdlib_I64_strmm( 'RIGHT', 'LOWER', 'NO TRANSPOSE', 'UNIT', n,k, one, v( 1_ilp64, &
                              m-k+1 ), ldv, work, ldwork )
                    ! c2 := c2 - w**t
                    do j = 1, k
                       do i = 1, n
                          c( m-k+j, i ) = c( m-k+j, i ) - work( i, j )
                       end do
                    end do
                 else if( stdlib_lsame( side, 'R' ) ) then
                    ! form  c * h  or  c * h**t  where  c = ( c1  c2 )
                    ! w := c * v**t  =  (c1*v1**t + c2*v2**t)  (stored in work)
                    ! w := c2
                    do j = 1, k
                       call stdlib_I64_scopy( m, c( 1_ilp64, n-k+j ), 1_ilp64, work( 1_ilp64, j ), 1_ilp64 )
                    end do
                    ! w := w * v2**t
                    call stdlib_I64_strmm( 'RIGHT', 'LOWER', 'TRANSPOSE', 'UNIT', m, k,one, v( 1_ilp64, n-k+&
                              1_ilp64 ), ldv, work, ldwork )
                    if( n>k ) then
                       ! w := w + c1 * v1**t
                       call stdlib_I64_sgemm( 'NO TRANSPOSE', 'TRANSPOSE', m, k, n-k,one, c, ldc, v, &
                                 ldv, one, work, ldwork )
                    end if
                    ! w := w * t  or  w * t**t
                    call stdlib_I64_strmm( 'RIGHT', 'LOWER', trans, 'NON-UNIT', m, k,one, t, ldt, &
                              work, ldwork )
                    ! c := c - w * v
                    if( n>k ) then
                       ! c1 := c1 - w * v1
                       call stdlib_I64_sgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', m, n-k, k,-one, work, &
                                 ldwork, v, ldv, one, c, ldc )
                    end if
                    ! w := w * v2
                    call stdlib_I64_strmm( 'RIGHT', 'LOWER', 'NO TRANSPOSE', 'UNIT', m,k, one, v( 1_ilp64, &
                              n-k+1 ), ldv, work, ldwork )
                    ! c1 := c1 - w
                    do j = 1, k
                       do i = 1, m
                          c( i, n-k+j ) = c( i, n-k+j ) - work( i, j )
                       end do
                    end do
                 end if
              end if
           end if
           return
     end subroutine stdlib_I64_slarfb

     pure module subroutine stdlib_I64_dlarfb( side, trans, direct, storev, m, n, k, v, ldv,t, ldt, c, ldc, &
     !! DLARFB applies a real block reflector H or its transpose H**T to a
     !! real m by n matrix C, from either the left or the right.
               work, ldwork )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: direct, side, storev, trans
           integer(ilp64), intent(in) :: k, ldc, ldt, ldv, ldwork, m, n
           ! Array Arguments 
           real(dp), intent(inout) :: c(ldc,*)
           real(dp), intent(in) :: t(ldt,*), v(ldv,*)
           real(dp), intent(out) :: work(ldwork,*)
        ! =====================================================================
           
           ! Local Scalars 
           character :: transt
           integer(ilp64) :: i, j
           ! Executable Statements 
           ! quick return if possible
           if( m<=0 .or. n<=0 )return
           if( stdlib_lsame( trans, 'N' ) ) then
              transt = 'T'
           else
              transt = 'N'
           end if
           if( stdlib_lsame( storev, 'C' ) ) then
              if( stdlib_lsame( direct, 'F' ) ) then
                 ! let  v =  ( v1 )    (first k rows)
                           ! ( v2 )
                 ! where  v1  is unit lower triangular.
                 if( stdlib_lsame( side, 'L' ) ) then
                    ! form  h * c  or  h**t * c  where  c = ( c1 )
                                                          ! ( c2 )
                    ! w := c**t * v  =  (c1**t * v1 + c2**t * v2)  (stored in work)
                    ! w := c1**t
                    do j = 1, k
                       call stdlib_I64_dcopy( n, c( j, 1_ilp64 ), ldc, work( 1_ilp64, j ), 1_ilp64 )
                    end do
                    ! w := w * v1
                    call stdlib_I64_dtrmm( 'RIGHT', 'LOWER', 'NO TRANSPOSE', 'UNIT', n,k, one, v, ldv,&
                               work, ldwork )
                    if( m>k ) then
                       ! w := w + c2**t * v2
                       call stdlib_I64_dgemm( 'TRANSPOSE', 'NO TRANSPOSE', n, k, m-k,one, c( k+1, 1_ilp64 ),&
                                  ldc, v( k+1, 1_ilp64 ), ldv,one, work, ldwork )
                    end if
                    ! w := w * t**t  or  w * t
                    call stdlib_I64_dtrmm( 'RIGHT', 'UPPER', transt, 'NON-UNIT', n, k,one, t, ldt, &
                              work, ldwork )
                    ! c := c - v * w**t
                    if( m>k ) then
                       ! c2 := c2 - v2 * w**t
                       call stdlib_I64_dgemm( 'NO TRANSPOSE', 'TRANSPOSE', m-k, n, k,-one, v( k+1, 1_ilp64 )&
                                 , ldv, work, ldwork, one,c( k+1, 1_ilp64 ), ldc )
                    end if
                    ! w := w * v1**t
                    call stdlib_I64_dtrmm( 'RIGHT', 'LOWER', 'TRANSPOSE', 'UNIT', n, k,one, v, ldv, &
                              work, ldwork )
                    ! c1 := c1 - w**t
                    do j = 1, k
                       do i = 1, n
                          c( j, i ) = c( j, i ) - work( i, j )
                       end do
                    end do
                 else if( stdlib_lsame( side, 'R' ) ) then
                    ! form  c * h  or  c * h**t  where  c = ( c1  c2 )
                    ! w := c * v  =  (c1*v1 + c2*v2)  (stored in work)
                    ! w := c1
                    do j = 1, k
                       call stdlib_I64_dcopy( m, c( 1_ilp64, j ), 1_ilp64, work( 1_ilp64, j ), 1_ilp64 )
                    end do
                    ! w := w * v1
                    call stdlib_I64_dtrmm( 'RIGHT', 'LOWER', 'NO TRANSPOSE', 'UNIT', m,k, one, v, ldv,&
                               work, ldwork )
                    if( n>k ) then
                       ! w := w + c2 * v2
                       call stdlib_I64_dgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', m, k, n-k,one, c( 1_ilp64, k+&
                                 1_ilp64 ), ldc, v( k+1, 1_ilp64 ), ldv,one, work, ldwork )
                    end if
                    ! w := w * t  or  w * t**t
                    call stdlib_I64_dtrmm( 'RIGHT', 'UPPER', trans, 'NON-UNIT', m, k,one, t, ldt, &
                              work, ldwork )
                    ! c := c - w * v**t
                    if( n>k ) then
                       ! c2 := c2 - w * v2**t
                       call stdlib_I64_dgemm( 'NO TRANSPOSE', 'TRANSPOSE', m, n-k, k,-one, work, &
                                 ldwork, v( k+1, 1_ilp64 ), ldv, one,c( 1_ilp64, k+1 ), ldc )
                    end if
                    ! w := w * v1**t
                    call stdlib_I64_dtrmm( 'RIGHT', 'LOWER', 'TRANSPOSE', 'UNIT', m, k,one, v, ldv, &
                              work, ldwork )
                    ! c1 := c1 - w
                    do j = 1, k
                       do i = 1, m
                          c( i, j ) = c( i, j ) - work( i, j )
                       end do
                    end do
                 end if
              else
                 ! let  v =  ( v1 )
                           ! ( v2 )    (last k rows)
                 ! where  v2  is unit upper triangular.
                 if( stdlib_lsame( side, 'L' ) ) then
                    ! form  h * c  or  h**t * c  where  c = ( c1 )
                                                          ! ( c2 )
                    ! w := c**t * v  =  (c1**t * v1 + c2**t * v2)  (stored in work)
                    ! w := c2**t
                    do j = 1, k
                       call stdlib_I64_dcopy( n, c( m-k+j, 1_ilp64 ), ldc, work( 1_ilp64, j ), 1_ilp64 )
                    end do
                    ! w := w * v2
                    call stdlib_I64_dtrmm( 'RIGHT', 'UPPER', 'NO TRANSPOSE', 'UNIT', n,k, one, v( m-k+&
                              1_ilp64, 1_ilp64 ), ldv, work, ldwork )
                    if( m>k ) then
                       ! w := w + c1**t * v1
                       call stdlib_I64_dgemm( 'TRANSPOSE', 'NO TRANSPOSE', n, k, m-k,one, c, ldc, v, &
                                 ldv, one, work, ldwork )
                    end if
                    ! w := w * t**t  or  w * t
                    call stdlib_I64_dtrmm( 'RIGHT', 'LOWER', transt, 'NON-UNIT', n, k,one, t, ldt, &
                              work, ldwork )
                    ! c := c - v * w**t
                    if( m>k ) then
                       ! c1 := c1 - v1 * w**t
                       call stdlib_I64_dgemm( 'NO TRANSPOSE', 'TRANSPOSE', m-k, n, k,-one, v, ldv, &
                                 work, ldwork, one, c, ldc )
                    end if
                    ! w := w * v2**t
                    call stdlib_I64_dtrmm( 'RIGHT', 'UPPER', 'TRANSPOSE', 'UNIT', n, k,one, v( m-k+1, &
                              1_ilp64 ), ldv, work, ldwork )
                    ! c2 := c2 - w**t
                    do j = 1, k
                       do i = 1, n
                          c( m-k+j, i ) = c( m-k+j, i ) - work( i, j )
                       end do
                    end do
                 else if( stdlib_lsame( side, 'R' ) ) then
                    ! form  c * h  or  c * h**t  where  c = ( c1  c2 )
                    ! w := c * v  =  (c1*v1 + c2*v2)  (stored in work)
                    ! w := c2
                    do j = 1, k
                       call stdlib_I64_dcopy( m, c( 1_ilp64, n-k+j ), 1_ilp64, work( 1_ilp64, j ), 1_ilp64 )
                    end do
                    ! w := w * v2
                    call stdlib_I64_dtrmm( 'RIGHT', 'UPPER', 'NO TRANSPOSE', 'UNIT', m,k, one, v( n-k+&
                              1_ilp64, 1_ilp64 ), ldv, work, ldwork )
                    if( n>k ) then
                       ! w := w + c1 * v1
                       call stdlib_I64_dgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', m, k, n-k,one, c, ldc, &
                                 v, ldv, one, work, ldwork )
                    end if
                    ! w := w * t  or  w * t**t
                    call stdlib_I64_dtrmm( 'RIGHT', 'LOWER', trans, 'NON-UNIT', m, k,one, t, ldt, &
                              work, ldwork )
                    ! c := c - w * v**t
                    if( n>k ) then
                       ! c1 := c1 - w * v1**t
                       call stdlib_I64_dgemm( 'NO TRANSPOSE', 'TRANSPOSE', m, n-k, k,-one, work, &
                                 ldwork, v, ldv, one, c, ldc )
                    end if
                    ! w := w * v2**t
                    call stdlib_I64_dtrmm( 'RIGHT', 'UPPER', 'TRANSPOSE', 'UNIT', m, k,one, v( n-k+1, &
                              1_ilp64 ), ldv, work, ldwork )
                    ! c2 := c2 - w
                    do j = 1, k
                       do i = 1, m
                          c( i, n-k+j ) = c( i, n-k+j ) - work( i, j )
                       end do
                    end do
                 end if
              end if
           else if( stdlib_lsame( storev, 'R' ) ) then
              if( stdlib_lsame( direct, 'F' ) ) then
                 ! let  v =  ( v1  v2 )    (v1: first k columns)
                 ! where  v1  is unit upper triangular.
                 if( stdlib_lsame( side, 'L' ) ) then
                    ! form  h * c  or  h**t * c  where  c = ( c1 )
                                                          ! ( c2 )
                    ! w := c**t * v**t  =  (c1**t * v1**t + c2**t * v2**t) (stored in work)
                    ! w := c1**t
                    do j = 1, k
                       call stdlib_I64_dcopy( n, c( j, 1_ilp64 ), ldc, work( 1_ilp64, j ), 1_ilp64 )
                    end do
                    ! w := w * v1**t
                    call stdlib_I64_dtrmm( 'RIGHT', 'UPPER', 'TRANSPOSE', 'UNIT', n, k,one, v, ldv, &
                              work, ldwork )
                    if( m>k ) then
                       ! w := w + c2**t * v2**t
                       call stdlib_I64_dgemm( 'TRANSPOSE', 'TRANSPOSE', n, k, m-k, one,c( k+1, 1_ilp64 ), &
                                 ldc, v( 1_ilp64, k+1 ), ldv, one,work, ldwork )
                    end if
                    ! w := w * t**t  or  w * t
                    call stdlib_I64_dtrmm( 'RIGHT', 'UPPER', transt, 'NON-UNIT', n, k,one, t, ldt, &
                              work, ldwork )
                    ! c := c - v**t * w**t
                    if( m>k ) then
                       ! c2 := c2 - v2**t * w**t
                       call stdlib_I64_dgemm( 'TRANSPOSE', 'TRANSPOSE', m-k, n, k, -one,v( 1_ilp64, k+1 ), &
                                 ldv, work, ldwork, one,c( k+1, 1_ilp64 ), ldc )
                    end if
                    ! w := w * v1
                    call stdlib_I64_dtrmm( 'RIGHT', 'UPPER', 'NO TRANSPOSE', 'UNIT', n,k, one, v, ldv,&
                               work, ldwork )
                    ! c1 := c1 - w**t
                    do j = 1, k
                       do i = 1, n
                          c( j, i ) = c( j, i ) - work( i, j )
                       end do
                    end do
                 else if( stdlib_lsame( side, 'R' ) ) then
                    ! form  c * h  or  c * h**t  where  c = ( c1  c2 )
                    ! w := c * v**t  =  (c1*v1**t + c2*v2**t)  (stored in work)
                    ! w := c1
                    do j = 1, k
                       call stdlib_I64_dcopy( m, c( 1_ilp64, j ), 1_ilp64, work( 1_ilp64, j ), 1_ilp64 )
                    end do
                    ! w := w * v1**t
                    call stdlib_I64_dtrmm( 'RIGHT', 'UPPER', 'TRANSPOSE', 'UNIT', m, k,one, v, ldv, &
                              work, ldwork )
                    if( n>k ) then
                       ! w := w + c2 * v2**t
                       call stdlib_I64_dgemm( 'NO TRANSPOSE', 'TRANSPOSE', m, k, n-k,one, c( 1_ilp64, k+1 ),&
                                  ldc, v( 1_ilp64, k+1 ), ldv,one, work, ldwork )
                    end if
                    ! w := w * t  or  w * t**t
                    call stdlib_I64_dtrmm( 'RIGHT', 'UPPER', trans, 'NON-UNIT', m, k,one, t, ldt, &
                              work, ldwork )
                    ! c := c - w * v
                    if( n>k ) then
                       ! c2 := c2 - w * v2
                       call stdlib_I64_dgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', m, n-k, k,-one, work, &
                                 ldwork, v( 1_ilp64, k+1 ), ldv, one,c( 1_ilp64, k+1 ), ldc )
                    end if
                    ! w := w * v1
                    call stdlib_I64_dtrmm( 'RIGHT', 'UPPER', 'NO TRANSPOSE', 'UNIT', m,k, one, v, ldv,&
                               work, ldwork )
                    ! c1 := c1 - w
                    do j = 1, k
                       do i = 1, m
                          c( i, j ) = c( i, j ) - work( i, j )
                       end do
                    end do
                 end if
              else
                 ! let  v =  ( v1  v2 )    (v2: last k columns)
                 ! where  v2  is unit lower triangular.
                 if( stdlib_lsame( side, 'L' ) ) then
                    ! form  h * c  or  h**t * c  where  c = ( c1 )
                                                          ! ( c2 )
                    ! w := c**t * v**t  =  (c1**t * v1**t + c2**t * v2**t) (stored in work)
                    ! w := c2**t
                    do j = 1, k
                       call stdlib_I64_dcopy( n, c( m-k+j, 1_ilp64 ), ldc, work( 1_ilp64, j ), 1_ilp64 )
                    end do
                    ! w := w * v2**t
                    call stdlib_I64_dtrmm( 'RIGHT', 'LOWER', 'TRANSPOSE', 'UNIT', n, k,one, v( 1_ilp64, m-k+&
                              1_ilp64 ), ldv, work, ldwork )
                    if( m>k ) then
                       ! w := w + c1**t * v1**t
                       call stdlib_I64_dgemm( 'TRANSPOSE', 'TRANSPOSE', n, k, m-k, one,c, ldc, v, ldv,&
                                  one, work, ldwork )
                    end if
                    ! w := w * t**t  or  w * t
                    call stdlib_I64_dtrmm( 'RIGHT', 'LOWER', transt, 'NON-UNIT', n, k,one, t, ldt, &
                              work, ldwork )
                    ! c := c - v**t * w**t
                    if( m>k ) then
                       ! c1 := c1 - v1**t * w**t
                       call stdlib_I64_dgemm( 'TRANSPOSE', 'TRANSPOSE', m-k, n, k, -one,v, ldv, work, &
                                 ldwork, one, c, ldc )
                    end if
                    ! w := w * v2
                    call stdlib_I64_dtrmm( 'RIGHT', 'LOWER', 'NO TRANSPOSE', 'UNIT', n,k, one, v( 1_ilp64, &
                              m-k+1 ), ldv, work, ldwork )
                    ! c2 := c2 - w**t
                    do j = 1, k
                       do i = 1, n
                          c( m-k+j, i ) = c( m-k+j, i ) - work( i, j )
                       end do
                    end do
                 else if( stdlib_lsame( side, 'R' ) ) then
                    ! form  c * h  or  c * h'  where  c = ( c1  c2 )
                    ! w := c * v**t  =  (c1*v1**t + c2*v2**t)  (stored in work)
                    ! w := c2
                    do j = 1, k
                       call stdlib_I64_dcopy( m, c( 1_ilp64, n-k+j ), 1_ilp64, work( 1_ilp64, j ), 1_ilp64 )
                    end do
                    ! w := w * v2**t
                    call stdlib_I64_dtrmm( 'RIGHT', 'LOWER', 'TRANSPOSE', 'UNIT', m, k,one, v( 1_ilp64, n-k+&
                              1_ilp64 ), ldv, work, ldwork )
                    if( n>k ) then
                       ! w := w + c1 * v1**t
                       call stdlib_I64_dgemm( 'NO TRANSPOSE', 'TRANSPOSE', m, k, n-k,one, c, ldc, v, &
                                 ldv, one, work, ldwork )
                    end if
                    ! w := w * t  or  w * t**t
                    call stdlib_I64_dtrmm( 'RIGHT', 'LOWER', trans, 'NON-UNIT', m, k,one, t, ldt, &
                              work, ldwork )
                    ! c := c - w * v
                    if( n>k ) then
                       ! c1 := c1 - w * v1
                       call stdlib_I64_dgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', m, n-k, k,-one, work, &
                                 ldwork, v, ldv, one, c, ldc )
                    end if
                    ! w := w * v2
                    call stdlib_I64_dtrmm( 'RIGHT', 'LOWER', 'NO TRANSPOSE', 'UNIT', m,k, one, v( 1_ilp64, &
                              n-k+1 ), ldv, work, ldwork )
                    ! c1 := c1 - w
                    do j = 1, k
                       do i = 1, m
                          c( i, n-k+j ) = c( i, n-k+j ) - work( i, j )
                       end do
                    end do
                 end if
              end if
           end if
           return
     end subroutine stdlib_I64_dlarfb


     pure module subroutine stdlib_I64_clarfb( side, trans, direct, storev, m, n, k, v, ldv,t, ldt, c, ldc, &
     work, ldwork )
     !! CLARFB applies a complex block reflector H or its transpose H**H to a
     !! complex M-by-N matrix C, from either the left or the right.
               
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: direct, side, storev, trans
           integer(ilp64), intent(in) :: k, ldc, ldt, ldv, ldwork, m, n
           ! Array Arguments 
           complex(sp), intent(inout) :: c(ldc,*)
           complex(sp), intent(in) :: t(ldt,*), v(ldv,*)
           complex(sp), intent(out) :: work(ldwork,*)
        ! =====================================================================
           
           ! Local Scalars 
           character :: transt
           integer(ilp64) :: i, j
           ! Intrinsic Functions 
           ! Executable Statements 
           ! quick return if possible
           if( m<=0 .or. n<=0 )return
           if( stdlib_lsame( trans, 'N' ) ) then
              transt = 'C'
           else
              transt = 'N'
           end if
           if( stdlib_lsame( storev, 'C' ) ) then
              if( stdlib_lsame( direct, 'F' ) ) then
                 ! let  v =  ( v1 )    (first k rows)
                           ! ( v2 )
                 ! where  v1  is unit lower triangular.
                 if( stdlib_lsame( side, 'L' ) ) then
                    ! form  h * c  or  h**h * c  where  c = ( c1 )
                                                          ! ( c2 )
                    ! w := c**h * v  =  (c1**h * v1 + c2**h * v2)  (stored in work)
                    ! w := c1**h
                    do j = 1, k
                       call stdlib_I64_ccopy( n, c( j, 1_ilp64 ), ldc, work( 1_ilp64, j ), 1_ilp64 )
                       call stdlib_I64_clacgv( n, work( 1_ilp64, j ), 1_ilp64 )
                    end do
                    ! w := w * v1
                    call stdlib_I64_ctrmm( 'RIGHT', 'LOWER', 'NO TRANSPOSE', 'UNIT', n,k, cone, v, &
                              ldv, work, ldwork )
                    if( m>k ) then
                       ! w := w + c2**h *v2
                       call stdlib_I64_cgemm( 'CONJUGATE TRANSPOSE', 'NO TRANSPOSE', n,k, m-k, cone, &
                                 c( k+1, 1_ilp64 ), ldc,v( k+1, 1_ilp64 ), ldv, cone, work, ldwork )
                    end if
                    ! w := w * t**h  or  w * t
                    call stdlib_I64_ctrmm( 'RIGHT', 'UPPER', transt, 'NON-UNIT', n, k,cone, t, ldt, &
                              work, ldwork )
                    ! c := c - v * w**h
                    if( m>k ) then
                       ! c2 := c2 - v2 * w**h
                       call stdlib_I64_cgemm( 'NO TRANSPOSE', 'CONJUGATE TRANSPOSE',m-k, n, k, -cone, &
                                 v( k+1, 1_ilp64 ), ldv, work,ldwork, cone, c( k+1, 1_ilp64 ), ldc )
                    end if
                    ! w := w * v1**h
                    call stdlib_I64_ctrmm( 'RIGHT', 'LOWER', 'CONJUGATE TRANSPOSE','UNIT', n, k, cone,&
                               v, ldv, work, ldwork )
                    ! c1 := c1 - w**h
                    do j = 1, k
                       do i = 1, n
                          c( j, i ) = c( j, i ) - conjg( work( i, j ) )
                       end do
                    end do
                 else if( stdlib_lsame( side, 'R' ) ) then
                    ! form  c * h  or  c * h**h  where  c = ( c1  c2 )
                    ! w := c * v  =  (c1*v1 + c2*v2)  (stored in work)
                    ! w := c1
                    do j = 1, k
                       call stdlib_I64_ccopy( m, c( 1_ilp64, j ), 1_ilp64, work( 1_ilp64, j ), 1_ilp64 )
                    end do
                    ! w := w * v1
                    call stdlib_I64_ctrmm( 'RIGHT', 'LOWER', 'NO TRANSPOSE', 'UNIT', m,k, cone, v, &
                              ldv, work, ldwork )
                    if( n>k ) then
                       ! w := w + c2 * v2
                       call stdlib_I64_cgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', m, k, n-k,cone, c( 1_ilp64, k+&
                                 1_ilp64 ), ldc, v( k+1, 1_ilp64 ), ldv,cone, work, ldwork )
                    end if
                    ! w := w * t  or  w * t**h
                    call stdlib_I64_ctrmm( 'RIGHT', 'UPPER', trans, 'NON-UNIT', m, k,cone, t, ldt, &
                              work, ldwork )
                    ! c := c - w * v**h
                    if( n>k ) then
                       ! c2 := c2 - w * v2**h
                       call stdlib_I64_cgemm( 'NO TRANSPOSE', 'CONJUGATE TRANSPOSE', m,n-k, k, -cone, &
                                 work, ldwork, v( k+1, 1_ilp64 ),ldv, cone, c( 1_ilp64, k+1 ), ldc )
                    end if
                    ! w := w * v1**h
                    call stdlib_I64_ctrmm( 'RIGHT', 'LOWER', 'CONJUGATE TRANSPOSE','UNIT', m, k, cone,&
                               v, ldv, work, ldwork )
                    ! c1 := c1 - w
                    do j = 1, k
                       do i = 1, m
                          c( i, j ) = c( i, j ) - work( i, j )
                       end do
                    end do
                 end if
              else
                 ! let  v =  ( v1 )
                           ! ( v2 )    (last k rows)
                 ! where  v2  is unit upper triangular.
                 if( stdlib_lsame( side, 'L' ) ) then
                    ! form  h * c  or  h**h * c  where  c = ( c1 )
                                                        ! ( c2 )
                    ! w := c**h * v  =  (c1**h * v1 + c2**h * v2)  (stored in work)
                    ! w := c2**h
                    do j = 1, k
                       call stdlib_I64_ccopy( n, c( m-k+j, 1_ilp64 ), ldc, work( 1_ilp64, j ), 1_ilp64 )
                       call stdlib_I64_clacgv( n, work( 1_ilp64, j ), 1_ilp64 )
                    end do
                    ! w := w * v2
                    call stdlib_I64_ctrmm( 'RIGHT', 'UPPER', 'NO TRANSPOSE', 'UNIT', n,k, cone, v( m-&
                              k+1, 1_ilp64 ), ldv, work, ldwork )
                    if( m>k ) then
                       ! w := w + c1**h * v1
                       call stdlib_I64_cgemm( 'CONJUGATE TRANSPOSE', 'NO TRANSPOSE', n,k, m-k, cone, &
                                 c, ldc, v, ldv, cone, work,ldwork )
                    end if
                    ! w := w * t**h  or  w * t
                    call stdlib_I64_ctrmm( 'RIGHT', 'LOWER', transt, 'NON-UNIT', n, k,cone, t, ldt, &
                              work, ldwork )
                    ! c := c - v * w**h
                    if( m>k ) then
                       ! c1 := c1 - v1 * w**h
                       call stdlib_I64_cgemm( 'NO TRANSPOSE', 'CONJUGATE TRANSPOSE',m-k, n, k, -cone, &
                                 v, ldv, work, ldwork,cone, c, ldc )
                    end if
                    ! w := w * v2**h
                    call stdlib_I64_ctrmm( 'RIGHT', 'UPPER', 'CONJUGATE TRANSPOSE','UNIT', n, k, cone,&
                               v( m-k+1, 1_ilp64 ), ldv, work,ldwork )
                    ! c2 := c2 - w**h
                    do j = 1, k
                       do i = 1, n
                          c( m-k+j, i ) = c( m-k+j, i ) -conjg( work( i, j ) )
                       end do
                    end do
                 else if( stdlib_lsame( side, 'R' ) ) then
                    ! form  c * h  or  c * h**h  where  c = ( c1  c2 )
                    ! w := c * v  =  (c1*v1 + c2*v2)  (stored in work)
                    ! w := c2
                    do j = 1, k
                       call stdlib_I64_ccopy( m, c( 1_ilp64, n-k+j ), 1_ilp64, work( 1_ilp64, j ), 1_ilp64 )
                    end do
                    ! w := w * v2
                    call stdlib_I64_ctrmm( 'RIGHT', 'UPPER', 'NO TRANSPOSE', 'UNIT', m,k, cone, v( n-&
                              k+1, 1_ilp64 ), ldv, work, ldwork )
                    if( n>k ) then
                       ! w := w + c1 * v1
                       call stdlib_I64_cgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', m, k, n-k,cone, c, ldc, &
                                 v, ldv, cone, work, ldwork )
                    end if
                    ! w := w * t  or  w * t**h
                    call stdlib_I64_ctrmm( 'RIGHT', 'LOWER', trans, 'NON-UNIT', m, k,cone, t, ldt, &
                              work, ldwork )
                    ! c := c - w * v**h
                    if( n>k ) then
                       ! c1 := c1 - w * v1**h
                       call stdlib_I64_cgemm( 'NO TRANSPOSE', 'CONJUGATE TRANSPOSE', m,n-k, k, -cone, &
                                 work, ldwork, v, ldv, cone,c, ldc )
                    end if
                    ! w := w * v2**h
                    call stdlib_I64_ctrmm( 'RIGHT', 'UPPER', 'CONJUGATE TRANSPOSE','UNIT', m, k, cone,&
                               v( n-k+1, 1_ilp64 ), ldv, work,ldwork )
                    ! c2 := c2 - w
                    do j = 1, k
                       do i = 1, m
                          c( i, n-k+j ) = c( i, n-k+j ) - work( i, j )
                       end do
                    end do
                 end if
              end if
           else if( stdlib_lsame( storev, 'R' ) ) then
              if( stdlib_lsame( direct, 'F' ) ) then
                 ! let  v =  ( v1  v2 )    (v1: first k columns)
                 ! where  v1  is unit upper triangular.
                 if( stdlib_lsame( side, 'L' ) ) then
                    ! form  h * c  or  h**h * c  where  c = ( c1 )
                                                          ! ( c2 )
                    ! w := c**h * v**h  =  (c1**h * v1**h + c2**h * v2**h) (stored in work)
                    ! w := c1**h
                    do j = 1, k
                       call stdlib_I64_ccopy( n, c( j, 1_ilp64 ), ldc, work( 1_ilp64, j ), 1_ilp64 )
                       call stdlib_I64_clacgv( n, work( 1_ilp64, j ), 1_ilp64 )
                    end do
                    ! w := w * v1**h
                    call stdlib_I64_ctrmm( 'RIGHT', 'UPPER', 'CONJUGATE TRANSPOSE','UNIT', n, k, cone,&
                               v, ldv, work, ldwork )
                    if( m>k ) then
                       ! w := w + c2**h * v2**h
                       call stdlib_I64_cgemm( 'CONJUGATE TRANSPOSE','CONJUGATE TRANSPOSE', n, k, m-k, &
                                 cone,c( k+1, 1_ilp64 ), ldc, v( 1_ilp64, k+1 ), ldv, cone,work, ldwork )
                    end if
                    ! w := w * t**h  or  w * t
                    call stdlib_I64_ctrmm( 'RIGHT', 'UPPER', transt, 'NON-UNIT', n, k,cone, t, ldt, &
                              work, ldwork )
                    ! c := c - v**h * w**h
                    if( m>k ) then
                       ! c2 := c2 - v2**h * w**h
                       call stdlib_I64_cgemm( 'CONJUGATE TRANSPOSE','CONJUGATE TRANSPOSE', m-k, n, k, &
                                 -cone,v( 1_ilp64, k+1 ), ldv, work, ldwork, cone,c( k+1, 1_ilp64 ), ldc )
                    end if
                    ! w := w * v1
                    call stdlib_I64_ctrmm( 'RIGHT', 'UPPER', 'NO TRANSPOSE', 'UNIT', n,k, cone, v, &
                              ldv, work, ldwork )
                    ! c1 := c1 - w**h
                    do j = 1, k
                       do i = 1, n
                          c( j, i ) = c( j, i ) - conjg( work( i, j ) )
                       end do
                    end do
                 else if( stdlib_lsame( side, 'R' ) ) then
                    ! form  c * h  or  c * h**h  where  c = ( c1  c2 )
                    ! w := c * v**h  =  (c1*v1**h + c2*v2**h)  (stored in work)
                    ! w := c1
                    do j = 1, k
                       call stdlib_I64_ccopy( m, c( 1_ilp64, j ), 1_ilp64, work( 1_ilp64, j ), 1_ilp64 )
                    end do
                    ! w := w * v1**h
                    call stdlib_I64_ctrmm( 'RIGHT', 'UPPER', 'CONJUGATE TRANSPOSE','UNIT', m, k, cone,&
                               v, ldv, work, ldwork )
                    if( n>k ) then
                       ! w := w + c2 * v2**h
                       call stdlib_I64_cgemm( 'NO TRANSPOSE', 'CONJUGATE TRANSPOSE', m,k, n-k, cone, &
                                 c( 1_ilp64, k+1 ), ldc,v( 1_ilp64, k+1 ), ldv, cone, work, ldwork )
                    end if
                    ! w := w * t  or  w * t**h
                    call stdlib_I64_ctrmm( 'RIGHT', 'UPPER', trans, 'NON-UNIT', m, k,cone, t, ldt, &
                              work, ldwork )
                    ! c := c - w * v
                    if( n>k ) then
                       ! c2 := c2 - w * v2
                       call stdlib_I64_cgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', m, n-k, k,-cone, work, &
                                 ldwork, v( 1_ilp64, k+1 ), ldv, cone,c( 1_ilp64, k+1 ), ldc )
                    end if
                    ! w := w * v1
                    call stdlib_I64_ctrmm( 'RIGHT', 'UPPER', 'NO TRANSPOSE', 'UNIT', m,k, cone, v, &
                              ldv, work, ldwork )
                    ! c1 := c1 - w
                    do j = 1, k
                       do i = 1, m
                          c( i, j ) = c( i, j ) - work( i, j )
                       end do
                    end do
                 end if
              else
                 ! let  v =  ( v1  v2 )    (v2: last k columns)
                 ! where  v2  is unit lower triangular.
                 if( stdlib_lsame( side, 'L' ) ) then
                    ! form  h * c  or  h**h * c  where  c = ( c1 )
                                                          ! ( c2 )
                    ! w := c**h * v**h  =  (c1**h * v1**h + c2**h * v2**h) (stored in work)
                    ! w := c2**h
                    do j = 1, k
                       call stdlib_I64_ccopy( n, c( m-k+j, 1_ilp64 ), ldc, work( 1_ilp64, j ), 1_ilp64 )
                       call stdlib_I64_clacgv( n, work( 1_ilp64, j ), 1_ilp64 )
                    end do
                    ! w := w * v2**h
                    call stdlib_I64_ctrmm( 'RIGHT', 'LOWER', 'CONJUGATE TRANSPOSE','UNIT', n, k, cone,&
                               v( 1_ilp64, m-k+1 ), ldv, work,ldwork )
                    if( m>k ) then
                       ! w := w + c1**h * v1**h
                       call stdlib_I64_cgemm( 'CONJUGATE TRANSPOSE','CONJUGATE TRANSPOSE', n, k, m-k, &
                                 cone, c,ldc, v, ldv, cone, work, ldwork )
                    end if
                    ! w := w * t**h  or  w * t
                    call stdlib_I64_ctrmm( 'RIGHT', 'LOWER', transt, 'NON-UNIT', n, k,cone, t, ldt, &
                              work, ldwork )
                    ! c := c - v**h * w**h
                    if( m>k ) then
                       ! c1 := c1 - v1**h * w**h
                       call stdlib_I64_cgemm( 'CONJUGATE TRANSPOSE','CONJUGATE TRANSPOSE', m-k, n, k, &
                                 -cone, v,ldv, work, ldwork, cone, c, ldc )
                    end if
                    ! w := w * v2
                    call stdlib_I64_ctrmm( 'RIGHT', 'LOWER', 'NO TRANSPOSE', 'UNIT', n,k, cone, v( 1_ilp64, &
                              m-k+1 ), ldv, work, ldwork )
                    ! c2 := c2 - w**h
                    do j = 1, k
                       do i = 1, n
                          c( m-k+j, i ) = c( m-k+j, i ) -conjg( work( i, j ) )
                       end do
                    end do
                 else if( stdlib_lsame( side, 'R' ) ) then
                    ! form  c * h  or  c * h**h  where  c = ( c1  c2 )
                    ! w := c * v**h  =  (c1*v1**h + c2*v2**h)  (stored in work)
                    ! w := c2
                    do j = 1, k
                       call stdlib_I64_ccopy( m, c( 1_ilp64, n-k+j ), 1_ilp64, work( 1_ilp64, j ), 1_ilp64 )
                    end do
                    ! w := w * v2**h
                    call stdlib_I64_ctrmm( 'RIGHT', 'LOWER', 'CONJUGATE TRANSPOSE','UNIT', m, k, cone,&
                               v( 1_ilp64, n-k+1 ), ldv, work,ldwork )
                    if( n>k ) then
                       ! w := w + c1 * v1**h
                       call stdlib_I64_cgemm( 'NO TRANSPOSE', 'CONJUGATE TRANSPOSE', m,k, n-k, cone, &
                                 c, ldc, v, ldv, cone, work,ldwork )
                    end if
                    ! w := w * t  or  w * t**h
                    call stdlib_I64_ctrmm( 'RIGHT', 'LOWER', trans, 'NON-UNIT', m, k,cone, t, ldt, &
                              work, ldwork )
                    ! c := c - w * v
                    if( n>k ) then
                       ! c1 := c1 - w * v1
                       call stdlib_I64_cgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', m, n-k, k,-cone, work, &
                                 ldwork, v, ldv, cone, c, ldc )
                    end if
                    ! w := w * v2
                    call stdlib_I64_ctrmm( 'RIGHT', 'LOWER', 'NO TRANSPOSE', 'UNIT', m,k, cone, v( 1_ilp64, &
                              n-k+1 ), ldv, work, ldwork )
                    ! c1 := c1 - w
                    do j = 1, k
                       do i = 1, m
                          c( i, n-k+j ) = c( i, n-k+j ) - work( i, j )
                       end do
                    end do
                 end if
              end if
           end if
           return
     end subroutine stdlib_I64_clarfb

     pure module subroutine stdlib_I64_zlarfb( side, trans, direct, storev, m, n, k, v, ldv,t, ldt, c, ldc, &
     !! ZLARFB applies a complex block reflector H or its transpose H**H to a
     !! complex M-by-N matrix C, from either the left or the right.
               work, ldwork )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: direct, side, storev, trans
           integer(ilp64), intent(in) :: k, ldc, ldt, ldv, ldwork, m, n
           ! Array Arguments 
           complex(dp), intent(inout) :: c(ldc,*)
           complex(dp), intent(in) :: t(ldt,*), v(ldv,*)
           complex(dp), intent(out) :: work(ldwork,*)
        ! =====================================================================
           
           ! Local Scalars 
           character :: transt
           integer(ilp64) :: i, j
           ! Intrinsic Functions 
           ! Executable Statements 
           ! quick return if possible
           if( m<=0 .or. n<=0 )return
           if( stdlib_lsame( trans, 'N' ) ) then
              transt = 'C'
           else
              transt = 'N'
           end if
           if( stdlib_lsame( storev, 'C' ) ) then
              if( stdlib_lsame( direct, 'F' ) ) then
                 ! let  v =  ( v1 )    (first k rows)
                           ! ( v2 )
                 ! where  v1  is unit lower triangular.
                 if( stdlib_lsame( side, 'L' ) ) then
                    ! form  h * c  or  h**h * c  where  c = ( c1 )
                                                          ! ( c2 )
                    ! w := c**h * v  =  (c1**h * v1 + c2**h * v2)  (stored in work)
                    ! w := c1**h
                    do j = 1, k
                       call stdlib_I64_zcopy( n, c( j, 1_ilp64 ), ldc, work( 1_ilp64, j ), 1_ilp64 )
                       call stdlib_I64_zlacgv( n, work( 1_ilp64, j ), 1_ilp64 )
                    end do
                    ! w := w * v1
                    call stdlib_I64_ztrmm( 'RIGHT', 'LOWER', 'NO TRANSPOSE', 'UNIT', n,k, cone, v, &
                              ldv, work, ldwork )
                    if( m>k ) then
                       ! w := w + c2**h * v2
                       call stdlib_I64_zgemm( 'CONJUGATE TRANSPOSE', 'NO TRANSPOSE', n,k, m-k, cone, &
                                 c( k+1, 1_ilp64 ), ldc,v( k+1, 1_ilp64 ), ldv, cone, work, ldwork )
                    end if
                    ! w := w * t**h  or  w * t
                    call stdlib_I64_ztrmm( 'RIGHT', 'UPPER', transt, 'NON-UNIT', n, k,cone, t, ldt, &
                              work, ldwork )
                    ! c := c - v * w**h
                    if( m>k ) then
                       ! c2 := c2 - v2 * w**h
                       call stdlib_I64_zgemm( 'NO TRANSPOSE', 'CONJUGATE TRANSPOSE',m-k, n, k, -cone, &
                                 v( k+1, 1_ilp64 ), ldv, work,ldwork, cone, c( k+1, 1_ilp64 ), ldc )
                    end if
                    ! w := w * v1**h
                    call stdlib_I64_ztrmm( 'RIGHT', 'LOWER', 'CONJUGATE TRANSPOSE','UNIT', n, k, cone,&
                               v, ldv, work, ldwork )
                    ! c1 := c1 - w**h
                    do j = 1, k
                       do i = 1, n
                          c( j, i ) = c( j, i ) - conjg( work( i, j ) )
                       end do
                    end do
                 else if( stdlib_lsame( side, 'R' ) ) then
                    ! form  c * h  or  c * h**h  where  c = ( c1  c2 )
                    ! w := c * v  =  (c1*v1 + c2*v2)  (stored in work)
                    ! w := c1
                    do j = 1, k
                       call stdlib_I64_zcopy( m, c( 1_ilp64, j ), 1_ilp64, work( 1_ilp64, j ), 1_ilp64 )
                    end do
                    ! w := w * v1
                    call stdlib_I64_ztrmm( 'RIGHT', 'LOWER', 'NO TRANSPOSE', 'UNIT', m,k, cone, v, &
                              ldv, work, ldwork )
                    if( n>k ) then
                       ! w := w + c2 * v2
                       call stdlib_I64_zgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', m, k, n-k,cone, c( 1_ilp64, k+&
                                 1_ilp64 ), ldc, v( k+1, 1_ilp64 ), ldv,cone, work, ldwork )
                    end if
                    ! w := w * t  or  w * t**h
                    call stdlib_I64_ztrmm( 'RIGHT', 'UPPER', trans, 'NON-UNIT', m, k,cone, t, ldt, &
                              work, ldwork )
                    ! c := c - w * v**h
                    if( n>k ) then
                       ! c2 := c2 - w * v2**h
                       call stdlib_I64_zgemm( 'NO TRANSPOSE', 'CONJUGATE TRANSPOSE', m,n-k, k, -cone, &
                                 work, ldwork, v( k+1, 1_ilp64 ),ldv, cone, c( 1_ilp64, k+1 ), ldc )
                    end if
                    ! w := w * v1**h
                    call stdlib_I64_ztrmm( 'RIGHT', 'LOWER', 'CONJUGATE TRANSPOSE','UNIT', m, k, cone,&
                               v, ldv, work, ldwork )
                    ! c1 := c1 - w
                    do j = 1, k
                       do i = 1, m
                          c( i, j ) = c( i, j ) - work( i, j )
                       end do
                    end do
                 end if
              else
                 ! let  v =  ( v1 )
                           ! ( v2 )    (last k rows)
                 ! where  v2  is unit upper triangular.
                 if( stdlib_lsame( side, 'L' ) ) then
                    ! form  h * c  or  h**h * c  where  c = ( c1 )
                                                          ! ( c2 )
                    ! w := c**h * v  =  (c1**h * v1 + c2**h * v2)  (stored in work)
                    ! w := c2**h
                    do j = 1, k
                       call stdlib_I64_zcopy( n, c( m-k+j, 1_ilp64 ), ldc, work( 1_ilp64, j ), 1_ilp64 )
                       call stdlib_I64_zlacgv( n, work( 1_ilp64, j ), 1_ilp64 )
                    end do
                    ! w := w * v2
                    call stdlib_I64_ztrmm( 'RIGHT', 'UPPER', 'NO TRANSPOSE', 'UNIT', n,k, cone, v( m-&
                              k+1, 1_ilp64 ), ldv, work, ldwork )
                    if( m>k ) then
                       ! w := w + c1**h * v1
                       call stdlib_I64_zgemm( 'CONJUGATE TRANSPOSE', 'NO TRANSPOSE', n,k, m-k, cone, &
                                 c, ldc, v, ldv, cone, work,ldwork )
                    end if
                    ! w := w * t**h  or  w * t
                    call stdlib_I64_ztrmm( 'RIGHT', 'LOWER', transt, 'NON-UNIT', n, k,cone, t, ldt, &
                              work, ldwork )
                    ! c := c - v * w**h
                    if( m>k ) then
                       ! c1 := c1 - v1 * w**h
                       call stdlib_I64_zgemm( 'NO TRANSPOSE', 'CONJUGATE TRANSPOSE',m-k, n, k, -cone, &
                                 v, ldv, work, ldwork,cone, c, ldc )
                    end if
                    ! w := w * v2**h
                    call stdlib_I64_ztrmm( 'RIGHT', 'UPPER', 'CONJUGATE TRANSPOSE','UNIT', n, k, cone,&
                               v( m-k+1, 1_ilp64 ), ldv, work,ldwork )
                    ! c2 := c2 - w**h
                    do j = 1, k
                       do i = 1, n
                          c( m-k+j, i ) = c( m-k+j, i ) -conjg( work( i, j ) )
                       end do
                    end do
                 else if( stdlib_lsame( side, 'R' ) ) then
                    ! form  c * h  or  c * h**h  where  c = ( c1  c2 )
                    ! w := c * v  =  (c1*v1 + c2*v2)  (stored in work)
                    ! w := c2
                    do j = 1, k
                       call stdlib_I64_zcopy( m, c( 1_ilp64, n-k+j ), 1_ilp64, work( 1_ilp64, j ), 1_ilp64 )
                    end do
                    ! w := w * v2
                    call stdlib_I64_ztrmm( 'RIGHT', 'UPPER', 'NO TRANSPOSE', 'UNIT', m,k, cone, v( n-&
                              k+1, 1_ilp64 ), ldv, work, ldwork )
                    if( n>k ) then
                       ! w := w + c1 * v1
                       call stdlib_I64_zgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', m, k, n-k,cone, c, ldc, &
                                 v, ldv, cone, work, ldwork )
                    end if
                    ! w := w * t  or  w * t**h
                    call stdlib_I64_ztrmm( 'RIGHT', 'LOWER', trans, 'NON-UNIT', m, k,cone, t, ldt, &
                              work, ldwork )
                    ! c := c - w * v**h
                    if( n>k ) then
                       ! c1 := c1 - w * v1**h
                       call stdlib_I64_zgemm( 'NO TRANSPOSE', 'CONJUGATE TRANSPOSE', m,n-k, k, -cone, &
                                 work, ldwork, v, ldv, cone,c, ldc )
                    end if
                    ! w := w * v2**h
                    call stdlib_I64_ztrmm( 'RIGHT', 'UPPER', 'CONJUGATE TRANSPOSE','UNIT', m, k, cone,&
                               v( n-k+1, 1_ilp64 ), ldv, work,ldwork )
                    ! c2 := c2 - w
                    do j = 1, k
                       do i = 1, m
                          c( i, n-k+j ) = c( i, n-k+j ) - work( i, j )
                       end do
                    end do
                 end if
              end if
           else if( stdlib_lsame( storev, 'R' ) ) then
              if( stdlib_lsame( direct, 'F' ) ) then
                 ! let  v =  ( v1  v2 )    (v1: first k columns)
                 ! where  v1  is unit upper triangular.
                 if( stdlib_lsame( side, 'L' ) ) then
                    ! form  h * c  or  h**h * c  where  c = ( c1 )
                                                          ! ( c2 )
                    ! w := c**h * v**h  =  (c1**h * v1**h + c2**h * v2**h) (stored in work)
                    ! w := c1**h
                    do j = 1, k
                       call stdlib_I64_zcopy( n, c( j, 1_ilp64 ), ldc, work( 1_ilp64, j ), 1_ilp64 )
                       call stdlib_I64_zlacgv( n, work( 1_ilp64, j ), 1_ilp64 )
                    end do
                    ! w := w * v1**h
                    call stdlib_I64_ztrmm( 'RIGHT', 'UPPER', 'CONJUGATE TRANSPOSE','UNIT', n, k, cone,&
                               v, ldv, work, ldwork )
                    if( m>k ) then
                       ! w := w + c2**h * v2**h
                       call stdlib_I64_zgemm( 'CONJUGATE TRANSPOSE','CONJUGATE TRANSPOSE', n, k, m-k, &
                                 cone,c( k+1, 1_ilp64 ), ldc, v( 1_ilp64, k+1 ), ldv, cone,work, ldwork )
                    end if
                    ! w := w * t**h  or  w * t
                    call stdlib_I64_ztrmm( 'RIGHT', 'UPPER', transt, 'NON-UNIT', n, k,cone, t, ldt, &
                              work, ldwork )
                    ! c := c - v**h * w**h
                    if( m>k ) then
                       ! c2 := c2 - v2**h * w**h
                       call stdlib_I64_zgemm( 'CONJUGATE TRANSPOSE','CONJUGATE TRANSPOSE', m-k, n, k, &
                                 -cone,v( 1_ilp64, k+1 ), ldv, work, ldwork, cone,c( k+1, 1_ilp64 ), ldc )
                    end if
                    ! w := w * v1
                    call stdlib_I64_ztrmm( 'RIGHT', 'UPPER', 'NO TRANSPOSE', 'UNIT', n,k, cone, v, &
                              ldv, work, ldwork )
                    ! c1 := c1 - w**h
                    do j = 1, k
                       do i = 1, n
                          c( j, i ) = c( j, i ) - conjg( work( i, j ) )
                       end do
                    end do
                 else if( stdlib_lsame( side, 'R' ) ) then
                    ! form  c * h  or  c * h**h  where  c = ( c1  c2 )
                    ! w := c * v**h  =  (c1*v1**h + c2*v2**h)  (stored in work)
                    ! w := c1
                    do j = 1, k
                       call stdlib_I64_zcopy( m, c( 1_ilp64, j ), 1_ilp64, work( 1_ilp64, j ), 1_ilp64 )
                    end do
                    ! w := w * v1**h
                    call stdlib_I64_ztrmm( 'RIGHT', 'UPPER', 'CONJUGATE TRANSPOSE','UNIT', m, k, cone,&
                               v, ldv, work, ldwork )
                    if( n>k ) then
                       ! w := w + c2 * v2**h
                       call stdlib_I64_zgemm( 'NO TRANSPOSE', 'CONJUGATE TRANSPOSE', m,k, n-k, cone, &
                                 c( 1_ilp64, k+1 ), ldc,v( 1_ilp64, k+1 ), ldv, cone, work, ldwork )
                    end if
                    ! w := w * t  or  w * t**h
                    call stdlib_I64_ztrmm( 'RIGHT', 'UPPER', trans, 'NON-UNIT', m, k,cone, t, ldt, &
                              work, ldwork )
                    ! c := c - w * v
                    if( n>k ) then
                       ! c2 := c2 - w * v2
                       call stdlib_I64_zgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', m, n-k, k,-cone, work, &
                                 ldwork, v( 1_ilp64, k+1 ), ldv, cone,c( 1_ilp64, k+1 ), ldc )
                    end if
                    ! w := w * v1
                    call stdlib_I64_ztrmm( 'RIGHT', 'UPPER', 'NO TRANSPOSE', 'UNIT', m,k, cone, v, &
                              ldv, work, ldwork )
                    ! c1 := c1 - w
                    do j = 1, k
                       do i = 1, m
                          c( i, j ) = c( i, j ) - work( i, j )
                       end do
                    end do
                 end if
              else
                 ! let  v =  ( v1  v2 )    (v2: last k columns)
                 ! where  v2  is unit lower triangular.
                 if( stdlib_lsame( side, 'L' ) ) then
                    ! form  h * c  or  h**h * c  where  c = ( c1 )
                                                          ! ( c2 )
                    ! w := c**h * v**h  =  (c1**h * v1**h + c2**h * v2**h) (stored in work)
                    ! w := c2**h
                    do j = 1, k
                       call stdlib_I64_zcopy( n, c( m-k+j, 1_ilp64 ), ldc, work( 1_ilp64, j ), 1_ilp64 )
                       call stdlib_I64_zlacgv( n, work( 1_ilp64, j ), 1_ilp64 )
                    end do
                    ! w := w * v2**h
                    call stdlib_I64_ztrmm( 'RIGHT', 'LOWER', 'CONJUGATE TRANSPOSE','UNIT', n, k, cone,&
                               v( 1_ilp64, m-k+1 ), ldv, work,ldwork )
                    if( m>k ) then
                       ! w := w + c1**h * v1**h
                       call stdlib_I64_zgemm( 'CONJUGATE TRANSPOSE','CONJUGATE TRANSPOSE', n, k, m-k, &
                                 cone, c,ldc, v, ldv, cone, work, ldwork )
                    end if
                    ! w := w * t**h  or  w * t
                    call stdlib_I64_ztrmm( 'RIGHT', 'LOWER', transt, 'NON-UNIT', n, k,cone, t, ldt, &
                              work, ldwork )
                    ! c := c - v**h * w**h
                    if( m>k ) then
                       ! c1 := c1 - v1**h * w**h
                       call stdlib_I64_zgemm( 'CONJUGATE TRANSPOSE','CONJUGATE TRANSPOSE', m-k, n, k, &
                                 -cone, v,ldv, work, ldwork, cone, c, ldc )
                    end if
                    ! w := w * v2
                    call stdlib_I64_ztrmm( 'RIGHT', 'LOWER', 'NO TRANSPOSE', 'UNIT', n,k, cone, v( 1_ilp64, &
                              m-k+1 ), ldv, work, ldwork )
                    ! c2 := c2 - w**h
                    do j = 1, k
                       do i = 1, n
                          c( m-k+j, i ) = c( m-k+j, i ) -conjg( work( i, j ) )
                       end do
                    end do
                 else if( stdlib_lsame( side, 'R' ) ) then
                    ! form  c * h  or  c * h**h  where  c = ( c1  c2 )
                    ! w := c * v**h  =  (c1*v1**h + c2*v2**h)  (stored in work)
                    ! w := c2
                    do j = 1, k
                       call stdlib_I64_zcopy( m, c( 1_ilp64, n-k+j ), 1_ilp64, work( 1_ilp64, j ), 1_ilp64 )
                    end do
                    ! w := w * v2**h
                    call stdlib_I64_ztrmm( 'RIGHT', 'LOWER', 'CONJUGATE TRANSPOSE','UNIT', m, k, cone,&
                               v( 1_ilp64, n-k+1 ), ldv, work,ldwork )
                    if( n>k ) then
                       ! w := w + c1 * v1**h
                       call stdlib_I64_zgemm( 'NO TRANSPOSE', 'CONJUGATE TRANSPOSE', m,k, n-k, cone, &
                                 c, ldc, v, ldv, cone, work,ldwork )
                    end if
                    ! w := w * t  or  w * t**h
                    call stdlib_I64_ztrmm( 'RIGHT', 'LOWER', trans, 'NON-UNIT', m, k,cone, t, ldt, &
                              work, ldwork )
                    ! c := c - w * v
                    if( n>k ) then
                       ! c1 := c1 - w * v1
                       call stdlib_I64_zgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', m, n-k, k,-cone, work, &
                                 ldwork, v, ldv, cone, c, ldc )
                    end if
                    ! w := w * v2
                    call stdlib_I64_ztrmm( 'RIGHT', 'LOWER', 'NO TRANSPOSE', 'UNIT', m,k, cone, v( 1_ilp64, &
                              n-k+1 ), ldv, work, ldwork )
                    ! c1 := c1 - w
                    do j = 1, k
                       do i = 1, m
                          c( i, n-k+j ) = c( i, n-k+j ) - work( i, j )
                       end do
                    end do
                 end if
              end if
           end if
           return
     end subroutine stdlib_I64_zlarfb




     pure module subroutine stdlib_I64_slarfg( n, alpha, x, incx, tau )
     !! SLARFG generates a real elementary reflector H of order n, such
     !! that
     !! H * ( alpha ) = ( beta ),   H**T * H = I.
     !! (   x   )   (   0  )
     !! where alpha and beta are scalars, and x is an (n-1)-element real
     !! vector. H is represented in the form
     !! H = I - tau * ( 1 ) * ( 1 v**T ) ,
     !! ( v )
     !! where tau is a real scalar and v is a real (n-1)-element
     !! vector.
     !! If the elements of x are all zero, then tau = 0 and H is taken to be
     !! the unit matrix.
     !! Otherwise  1 <= tau <= 2.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: incx, n
           real(sp), intent(inout) :: alpha
           real(sp), intent(out) :: tau
           ! Array Arguments 
           real(sp), intent(inout) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp64) :: j, knt
           real(sp) :: beta, rsafmn, safmin, xnorm
           ! Intrinsic Functions 
           ! Executable Statements 
           if( n<=1_ilp64 ) then
              tau = zero
              return
           end if
           xnorm = stdlib_I64_snrm2( n-1, x, incx )
           if( xnorm==zero ) then
              ! h  =  i
              tau = zero
           else
              ! general case
              beta = -sign( stdlib_I64_slapy2( alpha, xnorm ), alpha )
              safmin = stdlib_I64_slamch( 'S' ) / stdlib_I64_slamch( 'E' )
              knt = 0_ilp64
              if( abs( beta )<safmin ) then
                 ! xnorm, beta may be inaccurate; scale x and recompute them
                 rsafmn = one / safmin
                 10 continue
                 knt = knt + 1_ilp64
                 call stdlib_I64_sscal( n-1, rsafmn, x, incx )
                 beta = beta*rsafmn
                 alpha = alpha*rsafmn
                 if( (abs( beta )<safmin) .and. (knt < 20) )go to 10
                 ! new beta is at most 1, at least safmin
                 xnorm = stdlib_I64_snrm2( n-1, x, incx )
                 beta = -sign( stdlib_I64_slapy2( alpha, xnorm ), alpha )
              end if
              tau = ( beta-alpha ) / beta
              call stdlib_I64_sscal( n-1, one / ( alpha-beta ), x, incx )
              ! if alpha is subnormal, it may lose relative accuracy
              do j = 1, knt
                 beta = beta*safmin
              end do
              alpha = beta
           end if
           return
     end subroutine stdlib_I64_slarfg

     pure module subroutine stdlib_I64_dlarfg( n, alpha, x, incx, tau )
     !! DLARFG generates a real elementary reflector H of order n, such
     !! that
     !! H * ( alpha ) = ( beta ),   H**T * H = I.
     !! (   x   )   (   0  )
     !! where alpha and beta are scalars, and x is an (n-1)-element real
     !! vector. H is represented in the form
     !! H = I - tau * ( 1 ) * ( 1 v**T ) ,
     !! ( v )
     !! where tau is a real scalar and v is a real (n-1)-element
     !! vector.
     !! If the elements of x are all zero, then tau = 0 and H is taken to be
     !! the unit matrix.
     !! Otherwise  1 <= tau <= 2.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: incx, n
           real(dp), intent(inout) :: alpha
           real(dp), intent(out) :: tau
           ! Array Arguments 
           real(dp), intent(inout) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp64) :: j, knt
           real(dp) :: beta, rsafmn, safmin, xnorm
           ! Intrinsic Functions 
           ! Executable Statements 
           if( n<=1_ilp64 ) then
              tau = zero
              return
           end if
           xnorm = stdlib_I64_dnrm2( n-1, x, incx )
           if( xnorm==zero ) then
              ! h  =  i
              tau = zero
           else
              ! general case
              beta = -sign( stdlib_I64_dlapy2( alpha, xnorm ), alpha )
              safmin = stdlib_I64_dlamch( 'S' ) / stdlib_I64_dlamch( 'E' )
              knt = 0_ilp64
              if( abs( beta )<safmin ) then
                 ! xnorm, beta may be inaccurate; scale x and recompute them
                 rsafmn = one / safmin
                 10 continue
                 knt = knt + 1_ilp64
                 call stdlib_I64_dscal( n-1, rsafmn, x, incx )
                 beta = beta*rsafmn
                 alpha = alpha*rsafmn
                 if( (abs( beta )<safmin) .and. (knt < 20) )go to 10
                 ! new beta is at most 1, at least safmin
                 xnorm = stdlib_I64_dnrm2( n-1, x, incx )
                 beta = -sign( stdlib_I64_dlapy2( alpha, xnorm ), alpha )
              end if
              tau = ( beta-alpha ) / beta
              call stdlib_I64_dscal( n-1, one / ( alpha-beta ), x, incx )
              ! if alpha is subnormal, it may lose relative accuracy
              do j = 1, knt
                 beta = beta*safmin
              end do
              alpha = beta
           end if
           return
     end subroutine stdlib_I64_dlarfg


     pure module subroutine stdlib_I64_clarfg( n, alpha, x, incx, tau )
     !! CLARFG generates a complex elementary reflector H of order n, such
     !! that
     !! H**H * ( alpha ) = ( beta ),   H**H * H = I.
     !! (   x   )   (   0  )
     !! where alpha and beta are scalars, with beta real, and x is an
     !! (n-1)-element complex vector. H is represented in the form
     !! H = I - tau * ( 1 ) * ( 1 v**H ) ,
     !! ( v )
     !! where tau is a complex scalar and v is a complex (n-1)-element
     !! vector. Note that H is not hermitian.
     !! If the elements of x are all zero and alpha is real, then tau = 0
     !! and H is taken to be the unit matrix.
     !! Otherwise  1 <= real(tau) <= 2  and  abs(tau-1) <= 1 .
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: incx, n
           complex(sp), intent(inout) :: alpha
           complex(sp), intent(out) :: tau
           ! Array Arguments 
           complex(sp), intent(inout) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp64) :: j, knt
           real(sp) :: alphi, alphr, beta, rsafmn, safmin, xnorm
           ! Intrinsic Functions 
           ! Executable Statements 
           if( n<=0_ilp64 ) then
              tau = zero
              return
           end if
           xnorm = stdlib_I64_scnrm2( n-1, x, incx )
           alphr = real( alpha,KIND=sp)
           alphi = aimag( alpha )
           if( xnorm==zero .and. alphi==zero ) then
              ! h  =  i
              tau = zero
           else
              ! general case
              beta = -sign( stdlib_I64_slapy3( alphr, alphi, xnorm ), alphr )
              safmin = stdlib_I64_slamch( 'S' ) / stdlib_I64_slamch( 'E' )
              rsafmn = one / safmin
              knt = 0_ilp64
              if( abs( beta )<safmin ) then
                 ! xnorm, beta may be inaccurate; scale x and recompute them
                 10 continue
                 knt = knt + 1_ilp64
                 call stdlib_I64_csscal( n-1, rsafmn, x, incx )
                 beta = beta*rsafmn
                 alphi = alphi*rsafmn
                 alphr = alphr*rsafmn
                 if( (abs( beta )<safmin) .and. (knt < 20) )go to 10
                 ! new beta is at most 1, at least safmin
                 xnorm = stdlib_I64_scnrm2( n-1, x, incx )
                 alpha = cmplx( alphr, alphi,KIND=sp)
                 beta = -sign( stdlib_I64_slapy3( alphr, alphi, xnorm ), alphr )
              end if
              tau = cmplx( ( beta-alphr ) / beta, -alphi / beta,KIND=sp)
              alpha = stdlib_I64_cladiv( cmplx( one,KIND=sp), alpha-beta )
              call stdlib_I64_cscal( n-1, alpha, x, incx )
              ! if alpha is subnormal, it may lose relative accuracy
              do j = 1, knt
                 beta = beta*safmin
              end do
              alpha = beta
           end if
           return
     end subroutine stdlib_I64_clarfg

     pure module subroutine stdlib_I64_zlarfg( n, alpha, x, incx, tau )
     !! ZLARFG generates a complex elementary reflector H of order n, such
     !! that
     !! H**H * ( alpha ) = ( beta ),   H**H * H = I.
     !! (   x   )   (   0  )
     !! where alpha and beta are scalars, with beta real, and x is an
     !! (n-1)-element complex vector. H is represented in the form
     !! H = I - tau * ( 1 ) * ( 1 v**H ) ,
     !! ( v )
     !! where tau is a complex scalar and v is a complex (n-1)-element
     !! vector. Note that H is not hermitian.
     !! If the elements of x are all zero and alpha is real, then tau = 0
     !! and H is taken to be the unit matrix.
     !! Otherwise  1 <= real(tau) <= 2  and  abs(tau-1) <= 1 .
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: incx, n
           complex(dp), intent(inout) :: alpha
           complex(dp), intent(out) :: tau
           ! Array Arguments 
           complex(dp), intent(inout) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp64) :: j, knt
           real(dp) :: alphi, alphr, beta, rsafmn, safmin, xnorm
           ! Intrinsic Functions 
           ! Executable Statements 
           if( n<=0_ilp64 ) then
              tau = zero
              return
           end if
           xnorm = stdlib_I64_dznrm2( n-1, x, incx )
           alphr = real( alpha,KIND=dp)
           alphi = aimag( alpha )
           if( xnorm==zero .and. alphi==zero ) then
              ! h  =  i
              tau = zero
           else
              ! general case
              beta = -sign( stdlib_I64_dlapy3( alphr, alphi, xnorm ), alphr )
              safmin = stdlib_I64_dlamch( 'S' ) / stdlib_I64_dlamch( 'E' )
              rsafmn = one / safmin
              knt = 0_ilp64
              if( abs( beta )<safmin ) then
                 ! xnorm, beta may be inaccurate; scale x and recompute them
                 10 continue
                 knt = knt + 1_ilp64
                 call stdlib_I64_zdscal( n-1, rsafmn, x, incx )
                 beta = beta*rsafmn
                 alphi = alphi*rsafmn
                 alphr = alphr*rsafmn
                 if( (abs( beta )<safmin) .and. (knt < 20) )go to 10
                 ! new beta is at most 1, at least safmin
                 xnorm = stdlib_I64_dznrm2( n-1, x, incx )
                 alpha = cmplx( alphr, alphi,KIND=dp)
                 beta = -sign( stdlib_I64_dlapy3( alphr, alphi, xnorm ), alphr )
              end if
              tau = cmplx( ( beta-alphr ) / beta, -alphi / beta,KIND=dp)
              alpha = stdlib_I64_zladiv( cmplx( one,KIND=dp), alpha-beta )
              call stdlib_I64_zscal( n-1, alpha, x, incx )
              ! if alpha is subnormal, it may lose relative accuracy
              do j = 1, knt
                 beta = beta*safmin
              end do
              alpha = beta
           end if
           return
     end subroutine stdlib_I64_zlarfg




     module subroutine stdlib_I64_slarfgp( n, alpha, x, incx, tau )
     !! SLARFGP generates a real elementary reflector H of order n, such
     !! that
     !! H * ( alpha ) = ( beta ),   H**T * H = I.
     !! (   x   )   (   0  )
     !! where alpha and beta are scalars, beta is non-negative, and x is
     !! an (n-1)-element real vector.  H is represented in the form
     !! H = I - tau * ( 1 ) * ( 1 v**T ) ,
     !! ( v )
     !! where tau is a real scalar and v is a real (n-1)-element
     !! vector.
     !! If the elements of x are all zero, then tau = 0 and H is taken to be
     !! the unit matrix.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: incx, n
           real(sp), intent(inout) :: alpha
           real(sp), intent(out) :: tau
           ! Array Arguments 
           real(sp), intent(inout) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp64) :: j, knt
           real(sp) :: beta, bignum, savealpha, smlnum, xnorm
           ! Intrinsic Functions 
           ! Executable Statements 
           if( n<=0_ilp64 ) then
              tau = zero
              return
           end if
           xnorm = stdlib_I64_snrm2( n-1, x, incx )
           if( xnorm==zero ) then
              ! h  =  [+/-1, 0; i], sign chosen so alpha >= 0.
              if( alpha>=zero ) then
                 ! when tau.eq.zero, the vector is special-cased to be
                 ! all zeros in the application routines.  we do not need
                 ! to clear it.
                 tau = zero
              else
                 ! however, the application routines rely on explicit
                 ! zero checks when tau.ne.zero, and we must clear x.
                 tau = two
                 do j = 1, n-1
                    x( 1_ilp64 + (j-1)*incx ) = 0_ilp64
                 end do
                 alpha = -alpha
              end if
           else
              ! general case
              beta = sign( stdlib_I64_slapy2( alpha, xnorm ), alpha )
              smlnum = stdlib_I64_slamch( 'S' ) / stdlib_I64_slamch( 'E' )
              knt = 0_ilp64
              if( abs( beta )<smlnum ) then
                 ! xnorm, beta may be inaccurate; scale x and recompute them
                 bignum = one / smlnum
                 10 continue
                 knt = knt + 1_ilp64
                 call stdlib_I64_sscal( n-1, bignum, x, incx )
                 beta = beta*bignum
                 alpha = alpha*bignum
                 if( (abs( beta )<smlnum) .and. (knt < 20) )go to 10
                 ! new beta is at most 1, at least smlnum
                 xnorm = stdlib_I64_snrm2( n-1, x, incx )
                 beta = sign( stdlib_I64_slapy2( alpha, xnorm ), alpha )
              end if
              savealpha = alpha
              alpha = alpha + beta
              if( beta<zero ) then
                 beta = -beta
                 tau = -alpha / beta
              else
                 alpha = xnorm * (xnorm/alpha)
                 tau = alpha / beta
                 alpha = -alpha
              end if
              if ( abs(tau)<=smlnum ) then
                 ! in the case where the computed tau ends up being a denormalized number,
                 ! it loses relative accuracy. this is a big problem. solution: flush tau
                 ! to zero. this explains the next if statement.
                 ! (bug report provided by pat quillen from mathworks on jul 29, 2009.)
                 ! (thanks pat. thanks mathworks.)
                 if( savealpha>=zero ) then
                    tau = zero
                 else
                    tau = two
                    do j = 1, n-1
                       x( 1_ilp64 + (j-1)*incx ) = 0_ilp64
                    end do
                    beta = -savealpha
                 end if
              else
                 ! this is the general case.
                 call stdlib_I64_sscal( n-1, one / alpha, x, incx )
              end if
              ! if beta is subnormal, it may lose relative accuracy
              do j = 1, knt
                 beta = beta*smlnum
              end do
              alpha = beta
           end if
           return
     end subroutine stdlib_I64_slarfgp

     module subroutine stdlib_I64_dlarfgp( n, alpha, x, incx, tau )
     !! DLARFGP generates a real elementary reflector H of order n, such
     !! that
     !! H * ( alpha ) = ( beta ),   H**T * H = I.
     !! (   x   )   (   0  )
     !! where alpha and beta are scalars, beta is non-negative, and x is
     !! an (n-1)-element real vector.  H is represented in the form
     !! H = I - tau * ( 1 ) * ( 1 v**T ) ,
     !! ( v )
     !! where tau is a real scalar and v is a real (n-1)-element
     !! vector.
     !! If the elements of x are all zero, then tau = 0 and H is taken to be
     !! the unit matrix.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: incx, n
           real(dp), intent(inout) :: alpha
           real(dp), intent(out) :: tau
           ! Array Arguments 
           real(dp), intent(inout) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp64) :: j, knt
           real(dp) :: beta, bignum, savealpha, smlnum, xnorm
           ! Intrinsic Functions 
           ! Executable Statements 
           if( n<=0_ilp64 ) then
              tau = zero
              return
           end if
           xnorm = stdlib_I64_dnrm2( n-1, x, incx )
           if( xnorm==zero ) then
              ! h  =  [+/-1, 0; i], sign chosen so alpha >= 0
              if( alpha>=zero ) then
                 ! when tau.eq.zero, the vector is special-cased to be
                 ! all zeros in the application routines.  we do not need
                 ! to clear it.
                 tau = zero
              else
                 ! however, the application routines rely on explicit
                 ! zero checks when tau.ne.zero, and we must clear x.
                 tau = two
                 do j = 1, n-1
                    x( 1_ilp64 + (j-1)*incx ) = 0_ilp64
                 end do
                 alpha = -alpha
              end if
           else
              ! general case
              beta = sign( stdlib_I64_dlapy2( alpha, xnorm ), alpha )
              smlnum = stdlib_I64_dlamch( 'S' ) / stdlib_I64_dlamch( 'E' )
              knt = 0_ilp64
              if( abs( beta )<smlnum ) then
                 ! xnorm, beta may be inaccurate; scale x and recompute them
                 bignum = one / smlnum
                 10 continue
                 knt = knt + 1_ilp64
                 call stdlib_I64_dscal( n-1, bignum, x, incx )
                 beta = beta*bignum
                 alpha = alpha*bignum
                 if( (abs( beta )<smlnum) .and. (knt < 20) )go to 10
                 ! new beta is at most 1, at least smlnum
                 xnorm = stdlib_I64_dnrm2( n-1, x, incx )
                 beta = sign( stdlib_I64_dlapy2( alpha, xnorm ), alpha )
              end if
              savealpha = alpha
              alpha = alpha + beta
              if( beta<zero ) then
                 beta = -beta
                 tau = -alpha / beta
              else
                 alpha = xnorm * (xnorm/alpha)
                 tau = alpha / beta
                 alpha = -alpha
              end if
              if ( abs(tau)<=smlnum ) then
                 ! in the case where the computed tau ends up being a denormalized number,
                 ! it loses relative accuracy. this is a big problem. solution: flush tau
                 ! to zero. this explains the next if statement.
                 ! (bug report provided by pat quillen from mathworks on jul 29, 2009.)
                 ! (thanks pat. thanks mathworks.)
                 if( savealpha>=zero ) then
                    tau = zero
                 else
                    tau = two
                    do j = 1, n-1
                       x( 1_ilp64 + (j-1)*incx ) = 0_ilp64
                    end do
                    beta = -savealpha
                 end if
              else
                 ! this is the general case.
                 call stdlib_I64_dscal( n-1, one / alpha, x, incx )
              end if
              ! if beta is subnormal, it may lose relative accuracy
              do j = 1, knt
                 beta = beta*smlnum
              end do
              alpha = beta
           end if
           return
     end subroutine stdlib_I64_dlarfgp


     module subroutine stdlib_I64_clarfgp( n, alpha, x, incx, tau )
     !! CLARFGP generates a complex elementary reflector H of order n, such
     !! that
     !! H**H * ( alpha ) = ( beta ),   H**H * H = I.
     !! (   x   )   (   0  )
     !! where alpha and beta are scalars, beta is real and non-negative, and
     !! x is an (n-1)-element complex vector.  H is represented in the form
     !! H = I - tau * ( 1 ) * ( 1 v**H ) ,
     !! ( v )
     !! where tau is a complex scalar and v is a complex (n-1)-element
     !! vector. Note that H is not hermitian.
     !! If the elements of x are all zero and alpha is real, then tau = 0
     !! and H is taken to be the unit matrix.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: incx, n
           complex(sp), intent(inout) :: alpha
           complex(sp), intent(out) :: tau
           ! Array Arguments 
           complex(sp), intent(inout) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp64) :: j, knt
           real(sp) :: alphi, alphr, beta, bignum, smlnum, xnorm
           complex(sp) :: savealpha
           ! Intrinsic Functions 
           ! Executable Statements 
           if( n<=0_ilp64 ) then
              tau = zero
              return
           end if
           xnorm = stdlib_I64_scnrm2( n-1, x, incx )
           alphr = real( alpha,KIND=sp)
           alphi = aimag( alpha )
           if( xnorm==zero ) then
              ! h  =  [1-alpha/abs(alpha) 0; 0 i], sign chosen so alpha >= 0.
              if( alphi==zero ) then
                 if( alphr>=zero ) then
                    ! when tau.eq.zero, the vector is special-cased to be
                    ! all zeros in the application routines.  we do not need
                    ! to clear it.
                    tau = zero
                 else
                    ! however, the application routines rely on explicit
                    ! zero checks when tau.ne.zero, and we must clear x.
                    tau = two
                    do j = 1, n-1
                       x( 1_ilp64 + (j-1)*incx ) = zero
                    end do
                    alpha = -alpha
                 end if
              else
                 ! only "reflecting" the diagonal entry to be real and non-negative.
                 xnorm = stdlib_I64_slapy2( alphr, alphi )
                 tau = cmplx( one - alphr / xnorm, -alphi / xnorm,KIND=sp)
                 do j = 1, n-1
                    x( 1_ilp64 + (j-1)*incx ) = zero
                 end do
                 alpha = xnorm
              end if
           else
              ! general case
              beta = sign( stdlib_I64_slapy3( alphr, alphi, xnorm ), alphr )
              smlnum = stdlib_I64_slamch( 'S' ) / stdlib_I64_slamch( 'E' )
              bignum = one / smlnum
              knt = 0_ilp64
              if( abs( beta )<smlnum ) then
                 ! xnorm, beta may be inaccurate; scale x and recompute them
                 10 continue
                 knt = knt + 1_ilp64
                 call stdlib_I64_csscal( n-1, bignum, x, incx )
                 beta = beta*bignum
                 alphi = alphi*bignum
                 alphr = alphr*bignum
                 if( (abs( beta )<smlnum) .and. (knt < 20) )go to 10
                 ! new beta is at most 1, at least smlnum
                 xnorm = stdlib_I64_scnrm2( n-1, x, incx )
                 alpha = cmplx( alphr, alphi,KIND=sp)
                 beta = sign( stdlib_I64_slapy3( alphr, alphi, xnorm ), alphr )
              end if
              savealpha = alpha
              alpha = alpha + beta
              if( beta<zero ) then
                 beta = -beta
                 tau = -alpha / beta
              else
                 alphr = alphi * (alphi/real( alpha,KIND=sp))
                 alphr = alphr + xnorm * (xnorm/real( alpha,KIND=sp))
                 tau = cmplx( alphr/beta, -alphi/beta,KIND=sp)
                 alpha = cmplx( -alphr, alphi,KIND=sp)
              end if
              alpha = stdlib_I64_cladiv( cmplx( one,KIND=sp), alpha )
              if ( abs(tau)<=smlnum ) then
                 ! in the case where the computed tau ends up being a denormalized number,
                 ! it loses relative accuracy. this is a big problem. solution: flush tau
                 ! to zero (or two or whatever makes a nonnegative real number for beta).
                 ! (bug report provided by pat quillen from mathworks on jul 29, 2009.)
                 ! (thanks pat. thanks mathworks.)
                 alphr = real( savealpha,KIND=sp)
                 alphi = aimag( savealpha )
                 if( alphi==zero ) then
                    if( alphr>=zero ) then
                       tau = zero
                    else
                       tau = two
                       do j = 1, n-1
                          x( 1_ilp64 + (j-1)*incx ) = zero
                       end do
                       beta = real( -savealpha,KIND=sp)
                    end if
                 else
                    xnorm = stdlib_I64_slapy2( alphr, alphi )
                    tau = cmplx( one - alphr / xnorm, -alphi / xnorm,KIND=sp)
                    do j = 1, n-1
                       x( 1_ilp64 + (j-1)*incx ) = zero
                    end do
                    beta = xnorm
                 end if
              else
                 ! this is the general case.
                 call stdlib_I64_cscal( n-1, alpha, x, incx )
              end if
              ! if beta is subnormal, it may lose relative accuracy
              do j = 1, knt
                 beta = beta*smlnum
              end do
              alpha = beta
           end if
           return
     end subroutine stdlib_I64_clarfgp

     module subroutine stdlib_I64_zlarfgp( n, alpha, x, incx, tau )
     !! ZLARFGP generates a complex elementary reflector H of order n, such
     !! that
     !! H**H * ( alpha ) = ( beta ),   H**H * H = I.
     !! (   x   )   (   0  )
     !! where alpha and beta are scalars, beta is real and non-negative, and
     !! x is an (n-1)-element complex vector.  H is represented in the form
     !! H = I - tau * ( 1 ) * ( 1 v**H ) ,
     !! ( v )
     !! where tau is a complex scalar and v is a complex (n-1)-element
     !! vector. Note that H is not hermitian.
     !! If the elements of x are all zero and alpha is real, then tau = 0
     !! and H is taken to be the unit matrix.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: incx, n
           complex(dp), intent(inout) :: alpha
           complex(dp), intent(out) :: tau
           ! Array Arguments 
           complex(dp), intent(inout) :: x(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp64) :: j, knt
           real(dp) :: alphi, alphr, beta, bignum, smlnum, xnorm
           complex(dp) :: savealpha
           ! Intrinsic Functions 
           ! Executable Statements 
           if( n<=0_ilp64 ) then
              tau = zero
              return
           end if
           xnorm = stdlib_I64_dznrm2( n-1, x, incx )
           alphr = real( alpha,KIND=dp)
           alphi = aimag( alpha )
           if( xnorm==zero ) then
              ! h  =  [1-alpha/abs(alpha) 0; 0 i], sign chosen so alpha >= 0.
              if( alphi==zero ) then
                 if( alphr>=zero ) then
                    ! when tau.eq.zero, the vector is special-cased to be
                    ! all zeros in the application routines.  we do not need
                    ! to clear it.
                    tau = zero
                 else
                    ! however, the application routines rely on explicit
                    ! zero checks when tau.ne.zero, and we must clear x.
                    tau = two
                    do j = 1, n-1
                       x( 1_ilp64 + (j-1)*incx ) = zero
                    end do
                    alpha = -alpha
                 end if
              else
                 ! only "reflecting" the diagonal entry to be real and non-negative.
                 xnorm = stdlib_I64_dlapy2( alphr, alphi )
                 tau = cmplx( one - alphr / xnorm, -alphi / xnorm,KIND=dp)
                 do j = 1, n-1
                    x( 1_ilp64 + (j-1)*incx ) = zero
                 end do
                 alpha = xnorm
              end if
           else
              ! general case
              beta = sign( stdlib_I64_dlapy3( alphr, alphi, xnorm ), alphr )
              smlnum = stdlib_I64_dlamch( 'S' ) / stdlib_I64_dlamch( 'E' )
              bignum = one / smlnum
              knt = 0_ilp64
              if( abs( beta )<smlnum ) then
                 ! xnorm, beta may be inaccurate; scale x and recompute them
                 10 continue
                 knt = knt + 1_ilp64
                 call stdlib_I64_zdscal( n-1, bignum, x, incx )
                 beta = beta*bignum
                 alphi = alphi*bignum
                 alphr = alphr*bignum
                 if( (abs( beta )<smlnum) .and. (knt < 20) )go to 10
                 ! new beta is at most 1, at least smlnum
                 xnorm = stdlib_I64_dznrm2( n-1, x, incx )
                 alpha = cmplx( alphr, alphi,KIND=dp)
                 beta = sign( stdlib_I64_dlapy3( alphr, alphi, xnorm ), alphr )
              end if
              savealpha = alpha
              alpha = alpha + beta
              if( beta<zero ) then
                 beta = -beta
                 tau = -alpha / beta
              else
                 alphr = alphi * (alphi/real( alpha,KIND=dp))
                 alphr = alphr + xnorm * (xnorm/real( alpha,KIND=dp))
                 tau = cmplx( alphr/beta, -alphi/beta,KIND=dp)
                 alpha = cmplx( -alphr, alphi,KIND=dp)
              end if
              alpha = stdlib_I64_zladiv( cmplx( one,KIND=dp), alpha )
              if ( abs(tau)<=smlnum ) then
                 ! in the case where the computed tau ends up being a denormalized number,
                 ! it loses relative accuracy. this is a big problem. solution: flush tau
                 ! to zero (or two or whatever makes a nonnegative real number for beta).
                 ! (bug report provided by pat quillen from mathworks on jul 29, 2009.)
                 ! (thanks pat. thanks mathworks.)
                 alphr = real( savealpha,KIND=dp)
                 alphi = aimag( savealpha )
                 if( alphi==zero ) then
                    if( alphr>=zero ) then
                       tau = zero
                    else
                       tau = two
                       do j = 1, n-1
                          x( 1_ilp64 + (j-1)*incx ) = zero
                       end do
                       beta = real( -savealpha,KIND=dp)
                    end if
                 else
                    xnorm = stdlib_I64_dlapy2( alphr, alphi )
                    tau = cmplx( one - alphr / xnorm, -alphi / xnorm,KIND=dp)
                    do j = 1, n-1
                       x( 1_ilp64 + (j-1)*incx ) = zero
                    end do
                    beta = xnorm
                 end if
              else
                 ! this is the general case.
                 call stdlib_I64_zscal( n-1, alpha, x, incx )
              end if
              ! if beta is subnormal, it may lose relative accuracy
              do j = 1, knt
                 beta = beta*smlnum
              end do
              alpha = beta
           end if
           return
     end subroutine stdlib_I64_zlarfgp




     pure module subroutine stdlib_I64_slarft( direct, storev, n, k, v, ldv, tau, t, ldt )
     !! SLARFT forms the triangular factor T of a real block reflector H
     !! of order n, which is defined as a product of k elementary reflectors.
     !! If DIRECT = 'F', H = H(1) H(2) . . . H(k) and T is upper triangular;
     !! If DIRECT = 'B', H = H(k) . . . H(2) H(1) and T is lower triangular.
     !! If STOREV = 'C', the vector which defines the elementary reflector
     !! H(i) is stored in the i-th column of the array V, and
     !! H  =  I - V * T * V**T
     !! If STOREV = 'R', the vector which defines the elementary reflector
     !! H(i) is stored in the i-th row of the array V, and
     !! H  =  I - V**T * T * V
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: direct, storev
           integer(ilp64), intent(in) :: k, ldt, ldv, n
           ! Array Arguments 
           real(sp), intent(out) :: t(ldt,*)
           real(sp), intent(in) :: tau(*), v(ldv,*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp64) :: i, j, prevlastv, lastv
           ! Executable Statements 
           ! quick return if possible
           if( n==0 )return
           if( stdlib_lsame( direct, 'F' ) ) then
              prevlastv = n
              do i = 1, k
                 prevlastv = max( i, prevlastv )
                 if( tau( i )==zero ) then
                    ! h(i)  =  i
                    do j = 1, i
                       t( j, i ) = zero
                    end do
                 else
                    ! general case
                    if( stdlib_lsame( storev, 'C' ) ) then
                       ! skip any trailing zeros.
                       do lastv = n, i+1, -1
                          if( v( lastv, i )/=zero ) exit
                       end do
                       do j = 1, i-1
                          t( j, i ) = -tau( i ) * v( i , j )
                       end do
                       j = min( lastv, prevlastv )
                       ! t(1:i-1,i) := - tau(i) * v(i:j,1:i-1)**t * v(i:j,i)
                       call stdlib_I64_sgemv( 'TRANSPOSE', j-i, i-1, -tau( i ),v( i+1, 1_ilp64 ), ldv, v( i+&
                                 1_ilp64, i ), 1_ilp64, one,t( 1_ilp64, i ), 1_ilp64 )
                    else
                       ! skip any trailing zeros.
                       do lastv = n, i+1, -1
                          if( v( i, lastv )/=zero ) exit
                       end do
                       do j = 1, i-1
                          t( j, i ) = -tau( i ) * v( j , i )
                       end do
                       j = min( lastv, prevlastv )
                       ! t(1:i-1,i) := - tau(i) * v(1:i-1,i:j) * v(i,i:j)**t
                       call stdlib_I64_sgemv( 'NO TRANSPOSE', i-1, j-i, -tau( i ),v( 1_ilp64, i+1 ), ldv, v(&
                                  i, i+1 ), ldv,one, t( 1_ilp64, i ), 1_ilp64 )
                    end if
                    ! t(1:i-1,i) := t(1:i-1,1:i-1) * t(1:i-1,i)
                    call stdlib_I64_strmv( 'UPPER', 'NO TRANSPOSE', 'NON-UNIT', i-1, t,ldt, t( 1_ilp64, i ),&
                               1_ilp64 )
                    t( i, i ) = tau( i )
                    if( i>1_ilp64 ) then
                       prevlastv = max( prevlastv, lastv )
                    else
                       prevlastv = lastv
                    end if
                 end if
              end do
           else
              prevlastv = 1_ilp64
              do i = k, 1, -1
                 if( tau( i )==zero ) then
                    ! h(i)  =  i
                    do j = i, k
                       t( j, i ) = zero
                    end do
                 else
                    ! general case
                    if( i<k ) then
                       if( stdlib_lsame( storev, 'C' ) ) then
                          ! skip any leading zeros.
                          do lastv = 1, i-1
                             if( v( lastv, i )/=zero ) exit
                          end do
                          do j = i+1, k
                             t( j, i ) = -tau( i ) * v( n-k+i , j )
                          end do
                          j = max( lastv, prevlastv )
                          ! t(i+1:k,i) = -tau(i) * v(j:n-k+i,i+1:k)**t * v(j:n-k+i,i)
                          call stdlib_I64_sgemv( 'TRANSPOSE', n-k+i-j, k-i, -tau( i ),v( j, i+1 ), &
                                    ldv, v( j, i ), 1_ilp64, one,t( i+1, i ), 1_ilp64 )
                       else
                          ! skip any leading zeros.
                          do lastv = 1, i-1
                             if( v( i, lastv )/=zero ) exit
                          end do
                          do j = i+1, k
                             t( j, i ) = -tau( i ) * v( j, n-k+i )
                          end do
                          j = max( lastv, prevlastv )
                          ! t(i+1:k,i) = -tau(i) * v(i+1:k,j:n-k+i) * v(i,j:n-k+i)**t
                          call stdlib_I64_sgemv( 'NO TRANSPOSE', k-i, n-k+i-j,-tau( i ), v( i+1, j ), &
                                    ldv, v( i, j ), ldv,one, t( i+1, i ), 1_ilp64 )
                       end if
                       ! t(i+1:k,i) := t(i+1:k,i+1:k) * t(i+1:k,i)
                       call stdlib_I64_strmv( 'LOWER', 'NO TRANSPOSE', 'NON-UNIT', k-i,t( i+1, i+1 ), &
                                 ldt, t( i+1, i ), 1_ilp64 )
                       if( i>1_ilp64 ) then
                          prevlastv = min( prevlastv, lastv )
                       else
                          prevlastv = lastv
                       end if
                    end if
                    t( i, i ) = tau( i )
                 end if
              end do
           end if
           return
     end subroutine stdlib_I64_slarft

     pure module subroutine stdlib_I64_dlarft( direct, storev, n, k, v, ldv, tau, t, ldt )
     !! DLARFT forms the triangular factor T of a real block reflector H
     !! of order n, which is defined as a product of k elementary reflectors.
     !! If DIRECT = 'F', H = H(1) H(2) . . . H(k) and T is upper triangular;
     !! If DIRECT = 'B', H = H(k) . . . H(2) H(1) and T is lower triangular.
     !! If STOREV = 'C', the vector which defines the elementary reflector
     !! H(i) is stored in the i-th column of the array V, and
     !! H  =  I - V * T * V**T
     !! If STOREV = 'R', the vector which defines the elementary reflector
     !! H(i) is stored in the i-th row of the array V, and
     !! H  =  I - V**T * T * V
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: direct, storev
           integer(ilp64), intent(in) :: k, ldt, ldv, n
           ! Array Arguments 
           real(dp), intent(out) :: t(ldt,*)
           real(dp), intent(in) :: tau(*), v(ldv,*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp64) :: i, j, prevlastv, lastv
           ! Executable Statements 
           ! quick return if possible
           if( n==0 )return
           if( stdlib_lsame( direct, 'F' ) ) then
              prevlastv = n
              do i = 1, k
                 prevlastv = max( i, prevlastv )
                 if( tau( i )==zero ) then
                    ! h(i)  =  i
                    do j = 1, i
                       t( j, i ) = zero
                    end do
                 else
                    ! general case
                    if( stdlib_lsame( storev, 'C' ) ) then
                       ! skip any trailing zeros.
                       do lastv = n, i+1, -1
                          if( v( lastv, i )/=zero ) exit
                       end do
                       do j = 1, i-1
                          t( j, i ) = -tau( i ) * v( i , j )
                       end do
                       j = min( lastv, prevlastv )
                       ! t(1:i-1,i) := - tau(i) * v(i:j,1:i-1)**t * v(i:j,i)
                       call stdlib_I64_dgemv( 'TRANSPOSE', j-i, i-1, -tau( i ),v( i+1, 1_ilp64 ), ldv, v( i+&
                                 1_ilp64, i ), 1_ilp64, one,t( 1_ilp64, i ), 1_ilp64 )
                    else
                       ! skip any trailing zeros.
                       do lastv = n, i+1, -1
                          if( v( i, lastv )/=zero ) exit
                       end do
                       do j = 1, i-1
                          t( j, i ) = -tau( i ) * v( j , i )
                       end do
                       j = min( lastv, prevlastv )
                       ! t(1:i-1,i) := - tau(i) * v(1:i-1,i:j) * v(i,i:j)**t
                       call stdlib_I64_dgemv( 'NO TRANSPOSE', i-1, j-i, -tau( i ),v( 1_ilp64, i+1 ), ldv, v(&
                                  i, i+1 ), ldv, one,t( 1_ilp64, i ), 1_ilp64 )
                    end if
                    ! t(1:i-1,i) := t(1:i-1,1:i-1) * t(1:i-1,i)
                    call stdlib_I64_dtrmv( 'UPPER', 'NO TRANSPOSE', 'NON-UNIT', i-1, t,ldt, t( 1_ilp64, i ),&
                               1_ilp64 )
                    t( i, i ) = tau( i )
                    if( i>1_ilp64 ) then
                       prevlastv = max( prevlastv, lastv )
                    else
                       prevlastv = lastv
                    end if
                 end if
              end do
           else
              prevlastv = 1_ilp64
              do i = k, 1, -1
                 if( tau( i )==zero ) then
                    ! h(i)  =  i
                    do j = i, k
                       t( j, i ) = zero
                    end do
                 else
                    ! general case
                    if( i<k ) then
                       if( stdlib_lsame( storev, 'C' ) ) then
                          ! skip any leading zeros.
                          do lastv = 1, i-1
                             if( v( lastv, i )/=zero ) exit
                          end do
                          do j = i+1, k
                             t( j, i ) = -tau( i ) * v( n-k+i , j )
                          end do
                          j = max( lastv, prevlastv )
                          ! t(i+1:k,i) = -tau(i) * v(j:n-k+i,i+1:k)**t * v(j:n-k+i,i)
                          call stdlib_I64_dgemv( 'TRANSPOSE', n-k+i-j, k-i, -tau( i ),v( j, i+1 ), &
                                    ldv, v( j, i ), 1_ilp64, one,t( i+1, i ), 1_ilp64 )
                       else
                          ! skip any leading zeros.
                          do lastv = 1, i-1
                             if( v( i, lastv )/=zero ) exit
                          end do
                          do j = i+1, k
                             t( j, i ) = -tau( i ) * v( j, n-k+i )
                          end do
                          j = max( lastv, prevlastv )
                          ! t(i+1:k,i) = -tau(i) * v(i+1:k,j:n-k+i) * v(i,j:n-k+i)**t
                          call stdlib_I64_dgemv( 'NO TRANSPOSE', k-i, n-k+i-j,-tau( i ), v( i+1, j ), &
                                    ldv, v( i, j ), ldv,one, t( i+1, i ), 1_ilp64 )
                       end if
                       ! t(i+1:k,i) := t(i+1:k,i+1:k) * t(i+1:k,i)
                       call stdlib_I64_dtrmv( 'LOWER', 'NO TRANSPOSE', 'NON-UNIT', k-i,t( i+1, i+1 ), &
                                 ldt, t( i+1, i ), 1_ilp64 )
                       if( i>1_ilp64 ) then
                          prevlastv = min( prevlastv, lastv )
                       else
                          prevlastv = lastv
                       end if
                    end if
                    t( i, i ) = tau( i )
                 end if
              end do
           end if
           return
     end subroutine stdlib_I64_dlarft


     pure module subroutine stdlib_I64_clarft( direct, storev, n, k, v, ldv, tau, t, ldt )
     !! CLARFT forms the triangular factor T of a complex block reflector H
     !! of order n, which is defined as a product of k elementary reflectors.
     !! If DIRECT = 'F', H = H(1) H(2) . . . H(k) and T is upper triangular;
     !! If DIRECT = 'B', H = H(k) . . . H(2) H(1) and T is lower triangular.
     !! If STOREV = 'C', the vector which defines the elementary reflector
     !! H(i) is stored in the i-th column of the array V, and
     !! H  =  I - V * T * V**H
     !! If STOREV = 'R', the vector which defines the elementary reflector
     !! H(i) is stored in the i-th row of the array V, and
     !! H  =  I - V**H * T * V
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: direct, storev
           integer(ilp64), intent(in) :: k, ldt, ldv, n
           ! Array Arguments 
           complex(sp), intent(out) :: t(ldt,*)
           complex(sp), intent(in) :: tau(*), v(ldv,*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp64) :: i, j, prevlastv, lastv
           ! Executable Statements 
           ! quick return if possible
           if( n==0 )return
           if( stdlib_lsame( direct, 'F' ) ) then
              prevlastv = n
              do i = 1, k
                 prevlastv = max( prevlastv, i )
                 if( tau( i )==czero ) then
                    ! h(i)  =  i
                    do j = 1, i
                       t( j, i ) = czero
                    end do
                 else
                    ! general case
                    if( stdlib_lsame( storev, 'C' ) ) then
                       ! skip any trailing zeros.
                       do lastv = n, i+1, -1
                          if( v( lastv, i )/=czero ) exit
                       end do
                       do j = 1, i-1
                          t( j, i ) = -tau( i ) * conjg( v( i , j ) )
                       end do
                       j = min( lastv, prevlastv )
                       ! t(1:i-1,i) := - tau(i) * v(i:j,1:i-1)**h * v(i:j,i)
                       call stdlib_I64_cgemv( 'CONJUGATE TRANSPOSE', j-i, i-1,-tau( i ), v( i+1, 1_ilp64 ), &
                                 ldv,v( i+1, i ), 1_ilp64,cone, t( 1_ilp64, i ), 1_ilp64 )
                    else
                       ! skip any trailing zeros.
                       do lastv = n, i+1, -1
                          if( v( i, lastv )/=czero ) exit
                       end do
                       do j = 1, i-1
                          t( j, i ) = -tau( i ) * v( j , i )
                       end do
                       j = min( lastv, prevlastv )
                       ! t(1:i-1,i) := - tau(i) * v(1:i-1,i:j) * v(i,i:j)**h
                       call stdlib_I64_cgemm( 'N', 'C', i-1, 1_ilp64, j-i, -tau( i ),v( 1_ilp64, i+1 ), ldv, v( i,&
                                  i+1 ), ldv,cone, t( 1_ilp64, i ), ldt )
                    end if
                    ! t(1:i-1,i) := t(1:i-1,1:i-1) * t(1:i-1,i)
                    call stdlib_I64_ctrmv( 'UPPER', 'NO TRANSPOSE', 'NON-UNIT', i-1, t,ldt, t( 1_ilp64, i ),&
                               1_ilp64 )
                    t( i, i ) = tau( i )
                    if( i>1_ilp64 ) then
                       prevlastv = max( prevlastv, lastv )
                    else
                       prevlastv = lastv
                    end if
                 end if
              end do
           else
              prevlastv = 1_ilp64
              do i = k, 1, -1
                 if( tau( i )==czero ) then
                    ! h(i)  =  i
                    do j = i, k
                       t( j, i ) = czero
                    end do
                 else
                    ! general case
                    if( i<k ) then
                       if( stdlib_lsame( storev, 'C' ) ) then
                          ! skip any leading zeros.
                          do lastv = 1, i-1
                             if( v( lastv, i )/=czero ) exit
                          end do
                          do j = i+1, k
                             t( j, i ) = -tau( i ) * conjg( v( n-k+i , j ) )
                          end do
                          j = max( lastv, prevlastv )
                          ! t(i+1:k,i) = -tau(i) * v(j:n-k+i,i+1:k)**h * v(j:n-k+i,i)
                          call stdlib_I64_cgemv( 'CONJUGATE TRANSPOSE', n-k+i-j, k-i,-tau( i ), v( j, &
                                    i+1 ), ldv, v( j, i ),1_ilp64, cone, t( i+1, i ), 1_ilp64 )
                       else
                          ! skip any leading zeros.
                          do lastv = 1, i-1
                             if( v( i, lastv )/=czero ) exit
                          end do
                          do j = i+1, k
                             t( j, i ) = -tau( i ) * v( j, n-k+i )
                          end do
                          j = max( lastv, prevlastv )
                          ! t(i+1:k,i) = -tau(i) * v(i+1:k,j:n-k+i) * v(i,j:n-k+i)**h
                          call stdlib_I64_cgemm( 'N', 'C', k-i, 1_ilp64, n-k+i-j, -tau( i ),v( i+1, j ), &
                                    ldv, v( i, j ), ldv,cone, t( i+1, i ), ldt )
                       end if
                       ! t(i+1:k,i) := t(i+1:k,i+1:k) * t(i+1:k,i)
                       call stdlib_I64_ctrmv( 'LOWER', 'NO TRANSPOSE', 'NON-UNIT', k-i,t( i+1, i+1 ), &
                                 ldt, t( i+1, i ), 1_ilp64 )
                       if( i>1_ilp64 ) then
                          prevlastv = min( prevlastv, lastv )
                       else
                          prevlastv = lastv
                       end if
                    end if
                    t( i, i ) = tau( i )
                 end if
              end do
           end if
           return
     end subroutine stdlib_I64_clarft

     pure module subroutine stdlib_I64_zlarft( direct, storev, n, k, v, ldv, tau, t, ldt )
     !! ZLARFT forms the triangular factor T of a complex block reflector H
     !! of order n, which is defined as a product of k elementary reflectors.
     !! If DIRECT = 'F', H = H(1) H(2) . . . H(k) and T is upper triangular;
     !! If DIRECT = 'B', H = H(k) . . . H(2) H(1) and T is lower triangular.
     !! If STOREV = 'C', the vector which defines the elementary reflector
     !! H(i) is stored in the i-th column of the array V, and
     !! H  =  I - V * T * V**H
     !! If STOREV = 'R', the vector which defines the elementary reflector
     !! H(i) is stored in the i-th row of the array V, and
     !! H  =  I - V**H * T * V
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: direct, storev
           integer(ilp64), intent(in) :: k, ldt, ldv, n
           ! Array Arguments 
           complex(dp), intent(out) :: t(ldt,*)
           complex(dp), intent(in) :: tau(*), v(ldv,*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp64) :: i, j, prevlastv, lastv
           ! Executable Statements 
           ! quick return if possible
           if( n==0 )return
           if( stdlib_lsame( direct, 'F' ) ) then
              prevlastv = n
              do i = 1, k
                 prevlastv = max( prevlastv, i )
                 if( tau( i )==czero ) then
                    ! h(i)  =  i
                    do j = 1, i
                       t( j, i ) = czero
                    end do
                 else
                    ! general case
                    if( stdlib_lsame( storev, 'C' ) ) then
                       ! skip any trailing zeros.
                       do lastv = n, i+1, -1
                          if( v( lastv, i )/=czero ) exit
                       end do
                       do j = 1, i-1
                          t( j, i ) = -tau( i ) * conjg( v( i , j ) )
                       end do
                       j = min( lastv, prevlastv )
                       ! t(1:i-1,i) := - tau(i) * v(i:j,1:i-1)**h * v(i:j,i)
                       call stdlib_I64_zgemv( 'CONJUGATE TRANSPOSE', j-i, i-1,-tau( i ), v( i+1, 1_ilp64 ), &
                                 ldv,v( i+1, i ), 1_ilp64, cone, t( 1_ilp64, i ), 1_ilp64 )
                    else
                       ! skip any trailing zeros.
                       do lastv = n, i+1, -1
                          if( v( i, lastv )/=czero ) exit
                       end do
                       do j = 1, i-1
                          t( j, i ) = -tau( i ) * v( j , i )
                       end do
                       j = min( lastv, prevlastv )
                       ! t(1:i-1,i) := - tau(i) * v(1:i-1,i:j) * v(i,i:j)**h
                       call stdlib_I64_zgemm( 'N', 'C', i-1, 1_ilp64, j-i, -tau( i ),v( 1_ilp64, i+1 ), ldv, v( i,&
                                  i+1 ), ldv,cone, t( 1_ilp64, i ), ldt )
                    end if
                    ! t(1:i-1,i) := t(1:i-1,1:i-1) * t(1:i-1,i)
                    call stdlib_I64_ztrmv( 'UPPER', 'NO TRANSPOSE', 'NON-UNIT', i-1, t,ldt, t( 1_ilp64, i ),&
                               1_ilp64 )
                    t( i, i ) = tau( i )
                    if( i>1_ilp64 ) then
                       prevlastv = max( prevlastv, lastv )
                    else
                       prevlastv = lastv
                    end if
                  end if
              end do
           else
              prevlastv = 1_ilp64
              do i = k, 1, -1
                 if( tau( i )==czero ) then
                    ! h(i)  =  i
                    do j = i, k
                       t( j, i ) = czero
                    end do
                 else
                    ! general case
                    if( i<k ) then
                       if( stdlib_lsame( storev, 'C' ) ) then
                          ! skip any leading zeros.
                          do lastv = 1, i-1
                             if( v( lastv, i )/=czero ) exit
                          end do
                          do j = i+1, k
                             t( j, i ) = -tau( i ) * conjg( v( n-k+i , j ) )
                          end do
                          j = max( lastv, prevlastv )
                          ! t(i+1:k,i) = -tau(i) * v(j:n-k+i,i+1:k)**h * v(j:n-k+i,i)
                          call stdlib_I64_zgemv( 'CONJUGATE TRANSPOSE', n-k+i-j, k-i,-tau( i ), v( j, &
                                    i+1 ), ldv, v( j, i ),1_ilp64, cone, t( i+1, i ), 1_ilp64 )
                       else
                          ! skip any leading zeros.
                          do lastv = 1, i-1
                             if( v( i, lastv )/=czero ) exit
                          end do
                          do j = i+1, k
                             t( j, i ) = -tau( i ) * v( j, n-k+i )
                          end do
                          j = max( lastv, prevlastv )
                          ! t(i+1:k,i) = -tau(i) * v(i+1:k,j:n-k+i) * v(i,j:n-k+i)**h
                          call stdlib_I64_zgemm( 'N', 'C', k-i, 1_ilp64, n-k+i-j, -tau( i ),v( i+1, j ), &
                                    ldv, v( i, j ), ldv,cone, t( i+1, i ), ldt )
                       end if
                       ! t(i+1:k,i) := t(i+1:k,i+1:k) * t(i+1:k,i)
                       call stdlib_I64_ztrmv( 'LOWER', 'NO TRANSPOSE', 'NON-UNIT', k-i,t( i+1, i+1 ), &
                                 ldt, t( i+1, i ), 1_ilp64 )
                       if( i>1_ilp64 ) then
                          prevlastv = min( prevlastv, lastv )
                       else
                          prevlastv = lastv
                       end if
                    end if
                    t( i, i ) = tau( i )
                 end if
              end do
           end if
           return
     end subroutine stdlib_I64_zlarft



end submodule stdlib_lapack_householder_reflectors
