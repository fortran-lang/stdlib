submodule(stdlib_lapack_base) stdlib_lapack_auxiliary
  implicit none


  contains

     pure real(sp) module function stdlib_slamch( cmach )
     !! SLAMCH determines single precision machine parameters.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: zero, one, eps
           ! Scalar Arguments 
           character, intent(in) :: cmach
       ! =====================================================================
           
           ! Local Scalars 
           real(sp) :: sfmin, small, rmach
           ! Intrinsic Functions 
           ! Executable Statements 
           ! assume rounding, not chopping. always.
           if( stdlib_lsame( cmach, 'E' ) ) then
              rmach = eps
           else if( stdlib_lsame( cmach, 'S' ) ) then
              sfmin = tiny(zero)
              small = one / huge(zero)
              if( small>=sfmin ) then
                 ! use small plus a bit, to avoid the possibility of rounding
                 ! causing overflow when computing  1/sfmin.
                 sfmin = small*( one+eps )
              end if
              rmach = sfmin
           else if( stdlib_lsame( cmach, 'B' ) ) then
              rmach = radix(zero)
           else if( stdlib_lsame( cmach, 'P' ) ) then
              rmach = eps * radix(zero)
           else if( stdlib_lsame( cmach, 'N' ) ) then
              rmach = digits(zero)
           else if( stdlib_lsame( cmach, 'R' ) ) then
              rmach = one
           else if( stdlib_lsame( cmach, 'M' ) ) then
              rmach = minexponent(zero)
           else if( stdlib_lsame( cmach, 'U' ) ) then
              rmach = tiny(zero)
           else if( stdlib_lsame( cmach, 'L' ) ) then
              rmach = maxexponent(zero)
           else if( stdlib_lsame( cmach, 'O' ) ) then
              rmach = huge(zero)
           else
              rmach = zero
           end if
           stdlib_slamch = rmach
           return
     end function stdlib_slamch

     pure real(dp) module function stdlib_dlamch( cmach )
     !! DLAMCH determines double precision machine parameters.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: zero, one, eps
           ! Scalar Arguments 
           character, intent(in) :: cmach
       ! =====================================================================
           
           ! Local Scalars 
           real(dp) :: sfmin, small, rmach
           ! Intrinsic Functions 
           ! Executable Statements 
           ! assume rounding, not chopping. always.
           if( stdlib_lsame( cmach, 'E' ) ) then
              rmach = eps
           else if( stdlib_lsame( cmach, 'S' ) ) then
              sfmin = tiny(zero)
              small = one / huge(zero)
              if( small>=sfmin ) then
                 ! use small plus a bit, to avoid the possibility of rounding
                 ! causing overflow when computing  1/sfmin.
                 sfmin = small*( one+eps )
              end if
              rmach = sfmin
           else if( stdlib_lsame( cmach, 'B' ) ) then
              rmach = radix(zero)
           else if( stdlib_lsame( cmach, 'P' ) ) then
              rmach = eps * radix(zero)
           else if( stdlib_lsame( cmach, 'N' ) ) then
              rmach = digits(zero)
           else if( stdlib_lsame( cmach, 'R' ) ) then
              rmach = one
           else if( stdlib_lsame( cmach, 'M' ) ) then
              rmach = minexponent(zero)
           else if( stdlib_lsame( cmach, 'U' ) ) then
              rmach = tiny(zero)
           else if( stdlib_lsame( cmach, 'L' ) ) then
              rmach = maxexponent(zero)
           else if( stdlib_lsame( cmach, 'O' ) ) then
              rmach = huge(zero)
           else
              rmach = zero
           end if
           stdlib_dlamch = rmach
           return
     end function stdlib_dlamch




     pure real(sp) module function stdlib_slamc3( a, b )
        ! -- lapack auxiliary routine --
           ! univ. of tennessee, univ. of california berkeley and nag ltd..
           ! Scalar Arguments 
           real(sp), intent(in) :: a, b
       ! =====================================================================
           ! Executable Statements 
           stdlib_slamc3 = a + b
           return
     end function stdlib_slamc3

     pure real(dp) module function stdlib_dlamc3( a, b )
        ! -- lapack auxiliary routine --
           ! univ. of tennessee, univ. of california berkeley and nag ltd..
           ! Scalar Arguments 
           real(dp), intent(in) :: a, b
       ! =====================================================================
           ! Executable Statements 
           stdlib_dlamc3 = a + b
           return
     end function stdlib_dlamc3




     pure module subroutine stdlib_slabad( small, large )
     !! SLABAD takes as input the values computed by SLAMCH for underflow and
     !! overflow, and returns the square root of each of these values if the
     !! log of LARGE is sufficiently large.  This subroutine is intended to
     !! identify machines with a large exponent range, such as the Crays, and
     !! redefine the underflow and overflow limits to be the square roots of
     !! the values computed by SLAMCH.  This subroutine is needed because
     !! SLAMCH does not compensate for poor arithmetic in the upper half of
     !! the exponent range, as is found on a Cray.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           real(sp), intent(inout) :: large, small
        ! =====================================================================
           ! Intrinsic Functions 
           ! Executable Statements 
           ! if it looks like we're on a cray, take the square root of
           ! small and large to avoid overflow and underflow problems.
           if( log10( large )>2000. ) then
              small = sqrt( small )
              large = sqrt( large )
           end if
           return
     end subroutine stdlib_slabad

     pure module subroutine stdlib_dlabad( small, large )
     !! DLABAD takes as input the values computed by DLAMCH for underflow and
     !! overflow, and returns the square root of each of these values if the
     !! log of LARGE is sufficiently large.  This subroutine is intended to
     !! identify machines with a large exponent range, such as the Crays, and
     !! redefine the underflow and overflow limits to be the square roots of
     !! the values computed by DLAMCH.  This subroutine is needed because
     !! DLAMCH does not compensate for poor arithmetic in the upper half of
     !! the exponent range, as is found on a Cray.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           real(dp), intent(inout) :: large, small
        ! =====================================================================
           ! Intrinsic Functions 
           ! Executable Statements 
           ! if it looks like we're on a cray, take the square root of
           ! small and large to avoid overflow and underflow problems.
           if( log10( large )>2000._dp ) then
              small = sqrt( small )
              large = sqrt( large )
           end if
           return
     end subroutine stdlib_dlabad




     pure real(sp) module function stdlib_scsum1( n, cx, incx )
     !! SCSUM1 takes the sum of the absolute values of a complex
     !! vector and returns a single precision result.
     !! Based on SCASUM from the Level 1 BLAS.
     !! The change is to use the 'genuine' absolute value.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: zero
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx, n
           ! Array Arguments 
           complex(sp), intent(in) :: cx(*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, nincx
           real(sp) :: stemp
           ! Intrinsic Functions 
           ! Executable Statements 
           stdlib_scsum1 = zero
           stemp = zero
           if( n<=0 )return
           if( incx==1 )go to 20
           ! code for increment not equal to 1
           nincx = n*incx
           do i = 1, nincx, incx
              ! next line modified.
              stemp = stemp + abs( cx( i ) )
           end do
           stdlib_scsum1 = stemp
           return
           ! code for increment equal to 1
           20 continue
           do i = 1, n
              ! next line modified.
              stemp = stemp + abs( cx( i ) )
           end do
           stdlib_scsum1 = stemp
           return
     end function stdlib_scsum1



     pure real(dp) module function stdlib_dzsum1( n, cx, incx )
     !! DZSUM1 takes the sum of the absolute values of a complex
     !! vector and returns a double precision result.
     !! Based on DZASUM from the Level 1 BLAS.
     !! The change is to use the 'genuine' absolute value.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: zero
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx, n
           ! Array Arguments 
           complex(dp), intent(in) :: cx(*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, nincx
           real(dp) :: stemp
           ! Intrinsic Functions 
           ! Executable Statements 
           stdlib_dzsum1 = zero
           stemp = zero
           if( n<=0 )return
           if( incx==1 )go to 20
           ! code for increment not equal to 1
           nincx = n*incx
           do i = 1, nincx, incx
              ! next line modified.
              stemp = stemp + abs( cx( i ) )
           end do
           stdlib_dzsum1 = stemp
           return
           ! code for increment equal to 1
           20 continue
           do i = 1, n
              ! next line modified.
              stemp = stemp + abs( cx( i ) )
           end do
           stdlib_dzsum1 = stemp
           return
     end function stdlib_dzsum1




     pure module subroutine stdlib_slaqsb( uplo, n, kd, ab, ldab, s, scond, amax, equed )
     !! SLAQSB equilibrates a symmetric band matrix A using the scaling
     !! factors in the vector S.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: one
           ! Scalar Arguments 
           character, intent(out) :: equed
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: kd, ldab, n
           real(sp), intent(in) :: amax, scond
           ! Array Arguments 
           real(sp), intent(inout) :: ab(ldab,*)
           real(sp), intent(in) :: s(*)
        ! =====================================================================
           ! Parameters 
           real(sp), parameter :: thresh = 0.1e+0_sp
           
           ! Local Scalars 
           integer(ilp) :: i, j
           real(sp) :: cj, large, small
           ! Intrinsic Functions 
           ! Executable Statements 
           ! quick return if possible
           if( n<=0_ilp ) then
              equed = 'N'
              return
           end if
           ! initialize large and small.
           small = stdlib_slamch( 'SAFE MINIMUM' ) / stdlib_slamch( 'PRECISION' )
           large = one / small
           if( scond>=thresh .and. amax>=small .and. amax<=large ) then
              ! no equilibration
              equed = 'N'
           else
              ! replace a by diag(s) * a * diag(s).
              if( stdlib_lsame( uplo, 'U' ) ) then
                 ! upper triangle of a is stored in band format.
                 do j = 1, n
                    cj = s( j )
                    do i = max( 1, j-kd ), j
                       ab( kd+1+i-j, j ) = cj*s( i )*ab( kd+1+i-j, j )
                    end do
                 end do
              else
                 ! lower triangle of a is stored.
                 do j = 1, n
                    cj = s( j )
                    do i = j, min( n, j+kd )
                       ab( 1_ilp+i-j, j ) = cj*s( i )*ab( 1_ilp+i-j, j )
                    end do
                 end do
              end if
              equed = 'Y'
           end if
           return
     end subroutine stdlib_slaqsb

     pure module subroutine stdlib_dlaqsb( uplo, n, kd, ab, ldab, s, scond, amax, equed )
     !! DLAQSB equilibrates a symmetric band matrix A using the scaling
     !! factors in the vector S.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: one
           ! Scalar Arguments 
           character, intent(out) :: equed
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: kd, ldab, n
           real(dp), intent(in) :: amax, scond
           ! Array Arguments 
           real(dp), intent(inout) :: ab(ldab,*)
           real(dp), intent(in) :: s(*)
        ! =====================================================================
           ! Parameters 
           real(dp), parameter :: thresh = 0.1e+0_dp
           
           ! Local Scalars 
           integer(ilp) :: i, j
           real(dp) :: cj, large, small
           ! Intrinsic Functions 
           ! Executable Statements 
           ! quick return if possible
           if( n<=0_ilp ) then
              equed = 'N'
              return
           end if
           ! initialize large and small.
           small = stdlib_dlamch( 'SAFE MINIMUM' ) / stdlib_dlamch( 'PRECISION' )
           large = one / small
           if( scond>=thresh .and. amax>=small .and. amax<=large ) then
              ! no equilibration
              equed = 'N'
           else
              ! replace a by diag(s) * a * diag(s).
              if( stdlib_lsame( uplo, 'U' ) ) then
                 ! upper triangle of a is stored in band format.
                 do j = 1, n
                    cj = s( j )
                    do i = max( 1, j-kd ), j
                       ab( kd+1+i-j, j ) = cj*s( i )*ab( kd+1+i-j, j )
                    end do
                 end do
              else
                 ! lower triangle of a is stored.
                 do j = 1, n
                    cj = s( j )
                    do i = j, min( n, j+kd )
                       ab( 1_ilp+i-j, j ) = cj*s( i )*ab( 1_ilp+i-j, j )
                    end do
                 end do
              end if
              equed = 'Y'
           end if
           return
     end subroutine stdlib_dlaqsb


     pure module subroutine stdlib_claqsb( uplo, n, kd, ab, ldab, s, scond, amax, equed )
     !! CLAQSB equilibrates a symmetric band matrix A using the scaling
     !! factors in the vector S.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: one
           ! Scalar Arguments 
           character, intent(out) :: equed
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: kd, ldab, n
           real(sp), intent(in) :: amax, scond
           ! Array Arguments 
           real(sp), intent(in) :: s(*)
           complex(sp), intent(inout) :: ab(ldab,*)
        ! =====================================================================
           ! Parameters 
           real(sp), parameter :: thresh = 0.1e+0_sp
           
           ! Local Scalars 
           integer(ilp) :: i, j
           real(sp) :: cj, large, small
           ! Intrinsic Functions 
           ! Executable Statements 
           ! quick return if possible
           if( n<=0_ilp ) then
              equed = 'N'
              return
           end if
           ! initialize large and small.
           small = stdlib_slamch( 'SAFE MINIMUM' ) / stdlib_slamch( 'PRECISION' )
           large = one / small
           if( scond>=thresh .and. amax>=small .and. amax<=large ) then
              ! no equilibration
              equed = 'N'
           else
              ! replace a by diag(s) * a * diag(s).
              if( stdlib_lsame( uplo, 'U' ) ) then
                 ! upper triangle of a is stored in band format.
                 do j = 1, n
                    cj = s( j )
                    do i = max( 1, j-kd ), j
                       ab( kd+1+i-j, j ) = cj*s( i )*ab( kd+1+i-j, j )
                    end do
                 end do
              else
                 ! lower triangle of a is stored.
                 do j = 1, n
                    cj = s( j )
                    do i = j, min( n, j+kd )
                       ab( 1_ilp+i-j, j ) = cj*s( i )*ab( 1_ilp+i-j, j )
                    end do
                 end do
              end if
              equed = 'Y'
           end if
           return
     end subroutine stdlib_claqsb

     pure module subroutine stdlib_zlaqsb( uplo, n, kd, ab, ldab, s, scond, amax, equed )
     !! ZLAQSB equilibrates a symmetric band matrix A using the scaling
     !! factors in the vector S.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: one
           ! Scalar Arguments 
           character, intent(out) :: equed
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: kd, ldab, n
           real(dp), intent(in) :: amax, scond
           ! Array Arguments 
           real(dp), intent(in) :: s(*)
           complex(dp), intent(inout) :: ab(ldab,*)
        ! =====================================================================
           ! Parameters 
           real(dp), parameter :: thresh = 0.1e+0_dp
           
           ! Local Scalars 
           integer(ilp) :: i, j
           real(dp) :: cj, large, small
           ! Intrinsic Functions 
           ! Executable Statements 
           ! quick return if possible
           if( n<=0_ilp ) then
              equed = 'N'
              return
           end if
           ! initialize large and small.
           small = stdlib_dlamch( 'SAFE MINIMUM' ) / stdlib_dlamch( 'PRECISION' )
           large = one / small
           if( scond>=thresh .and. amax>=small .and. amax<=large ) then
              ! no equilibration
              equed = 'N'
           else
              ! replace a by diag(s) * a * diag(s).
              if( stdlib_lsame( uplo, 'U' ) ) then
                 ! upper triangle of a is stored in band format.
                 do j = 1, n
                    cj = s( j )
                    do i = max( 1, j-kd ), j
                       ab( kd+1+i-j, j ) = cj*s( i )*ab( kd+1+i-j, j )
                    end do
                 end do
              else
                 ! lower triangle of a is stored.
                 do j = 1, n
                    cj = s( j )
                    do i = j, min( n, j+kd )
                       ab( 1_ilp+i-j, j ) = cj*s( i )*ab( 1_ilp+i-j, j )
                    end do
                 end do
              end if
              equed = 'Y'
           end if
           return
     end subroutine stdlib_zlaqsb




     pure module subroutine stdlib_sladiv1( a, b, c, d, p, q )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: one
           ! Scalar Arguments 
           real(sp), intent(inout) :: a
           real(sp), intent(in) :: b, c, d
           real(sp), intent(out) :: p, q
        ! =====================================================================
           
           ! Local Scalars 
           real(sp) :: r, t
           ! Executable Statements 
           r = d / c
           t = one / (c + d * r)
           p = stdlib_sladiv2(a, b, c, d, r, t)
           a = -a
           q = stdlib_sladiv2(b, a, c, d, r, t)
           return
     end subroutine stdlib_sladiv1

     pure module subroutine stdlib_dladiv1( a, b, c, d, p, q )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: one
           ! Scalar Arguments 
           real(dp), intent(inout) :: a
           real(dp), intent(in) :: b, c, d
           real(dp), intent(out) :: p, q
        ! =====================================================================
           
           ! Local Scalars 
           real(dp) :: r, t
           ! Executable Statements 
           r = d / c
           t = one / (c + d * r)
           p = stdlib_dladiv2(a, b, c, d, r, t)
           a = -a
           q = stdlib_dladiv2(b, a, c, d, r, t)
           return
     end subroutine stdlib_dladiv1




     pure real(sp) module function stdlib_sladiv2( a, b, c, d, r, t )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: zero
           ! Scalar Arguments 
           real(sp), intent(in) :: a, b, c, d, r, t
        ! =====================================================================
           
           ! Local Scalars 
           real(sp) :: br
           ! Executable Statements 
           if( r/=zero ) then
              br = b * r
              if( br/=zero ) then
                 stdlib_sladiv2 = (a + br) * t
              else
                 stdlib_sladiv2 = a * t + (b * t) * r
              end if
           else
              stdlib_sladiv2 = (a + d * (b / c)) * t
           end if
           return
     end function stdlib_sladiv2

     pure real(dp) module function stdlib_dladiv2( a, b, c, d, r, t )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: zero
           ! Scalar Arguments 
           real(dp), intent(in) :: a, b, c, d, r, t
        ! =====================================================================
           
           ! Local Scalars 
           real(dp) :: br
           ! Executable Statements 
           if( r/=zero ) then
              br = b * r
              if( br/=zero ) then
                 stdlib_dladiv2 = (a + br) * t
              else
                 stdlib_dladiv2 = a * t + (b * t) * r
              end if
           else
              stdlib_dladiv2 = (a + d * (b / c)) * t
           end if
           return
     end function stdlib_dladiv2




     pure module subroutine stdlib_crot( n, cx, incx, cy, incy, c, s )
     !! CROT applies a plane rotation, where the cos (C) is real and the
     !! sin (S) is complex, and the vectors CX and CY are complex.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx, incy, n
           real(sp), intent(in) :: c
           complex(sp), intent(in) :: s
           ! Array Arguments 
           complex(sp), intent(inout) :: cx(*), cy(*)
       ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, ix, iy
           complex(sp) :: stemp
           ! Intrinsic Functions 
           ! Executable Statements 
           if( n<=0 )return
           if( incx==1 .and. incy==1 )go to 20
           ! code for unequal increments or equal increments not equal to 1
           ix = 1_ilp
           iy = 1_ilp
           if( incx<0_ilp )ix = ( -n+1 )*incx + 1_ilp
           if( incy<0_ilp )iy = ( -n+1 )*incy + 1_ilp
           do i = 1, n
              stemp = c*cx( ix ) + s*cy( iy )
              cy( iy ) = c*cy( iy ) - conjg( s )*cx( ix )
              cx( ix ) = stemp
              ix = ix + incx
              iy = iy + incy
           end do
           return
           ! code for both increments equal to 1
           20 continue
           do i = 1, n
              stemp = c*cx( i ) + s*cy( i )
              cy( i ) = c*cy( i ) - conjg( s )*cx( i )
              cx( i ) = stemp
           end do
           return
     end subroutine stdlib_crot

     pure module subroutine stdlib_zrot( n, cx, incx, cy, incy, c, s )
     !! ZROT applies a plane rotation, where the cos (C) is real and the
     !! sin (S) is complex, and the vectors CX and CY are complex.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx, incy, n
           real(dp), intent(in) :: c
           complex(dp), intent(in) :: s
           ! Array Arguments 
           complex(dp), intent(inout) :: cx(*), cy(*)
       ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, ix, iy
           complex(dp) :: stemp
           ! Intrinsic Functions 
           ! Executable Statements 
           if( n<=0 )return
           if( incx==1 .and. incy==1 )go to 20
           ! code for unequal increments or equal increments not equal to 1
           ix = 1_ilp
           iy = 1_ilp
           if( incx<0_ilp )ix = ( -n+1 )*incx + 1_ilp
           if( incy<0_ilp )iy = ( -n+1 )*incy + 1_ilp
           do i = 1, n
              stemp = c*cx( ix ) + s*cy( iy )
              cy( iy ) = c*cy( iy ) - conjg( s )*cx( ix )
              cx( ix ) = stemp
              ix = ix + incx
              iy = iy + incy
           end do
           return
           ! code for both increments equal to 1
           20 continue
           do i = 1, n
              stemp = c*cx( i ) + s*cy( i )
              cy( i ) = c*cy( i ) - conjg( s )*cx( i )
              cx( i ) = stemp
           end do
           return
     end subroutine stdlib_zrot




     pure real(sp) module function stdlib_I64_slamch( cmach )
     !! SLAMCH determines single precision machine parameters.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: zero, one, eps
           ! Scalar Arguments 
           character, intent(in) :: cmach
       ! =====================================================================
           
           ! Local Scalars 
           real(sp) :: sfmin, small, rmach
           ! Intrinsic Functions 
           ! Executable Statements 
           ! assume rounding, not chopping. always.
           if( stdlib_lsame( cmach, 'E' ) ) then
              rmach = eps
           else if( stdlib_lsame( cmach, 'S' ) ) then
              sfmin = tiny(zero)
              small = one / huge(zero)
              if( small>=sfmin ) then
                 ! use small plus a bit, to avoid the possibility of rounding
                 ! causing overflow when computing  1/sfmin.
                 sfmin = small*( one+eps )
              end if
              rmach = sfmin
           else if( stdlib_lsame( cmach, 'B' ) ) then
              rmach = radix(zero)
           else if( stdlib_lsame( cmach, 'P' ) ) then
              rmach = eps * radix(zero)
           else if( stdlib_lsame( cmach, 'N' ) ) then
              rmach = digits(zero)
           else if( stdlib_lsame( cmach, 'R' ) ) then
              rmach = one
           else if( stdlib_lsame( cmach, 'M' ) ) then
              rmach = minexponent(zero)
           else if( stdlib_lsame( cmach, 'U' ) ) then
              rmach = tiny(zero)
           else if( stdlib_lsame( cmach, 'L' ) ) then
              rmach = maxexponent(zero)
           else if( stdlib_lsame( cmach, 'O' ) ) then
              rmach = huge(zero)
           else
              rmach = zero
           end if
           stdlib_I64_slamch = rmach
           return
     end function stdlib_I64_slamch

     pure real(dp) module function stdlib_I64_dlamch( cmach )
     !! DLAMCH determines double precision machine parameters.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: zero, one, eps
           ! Scalar Arguments 
           character, intent(in) :: cmach
       ! =====================================================================
           
           ! Local Scalars 
           real(dp) :: sfmin, small, rmach
           ! Intrinsic Functions 
           ! Executable Statements 
           ! assume rounding, not chopping. always.
           if( stdlib_lsame( cmach, 'E' ) ) then
              rmach = eps
           else if( stdlib_lsame( cmach, 'S' ) ) then
              sfmin = tiny(zero)
              small = one / huge(zero)
              if( small>=sfmin ) then
                 ! use small plus a bit, to avoid the possibility of rounding
                 ! causing overflow when computing  1/sfmin.
                 sfmin = small*( one+eps )
              end if
              rmach = sfmin
           else if( stdlib_lsame( cmach, 'B' ) ) then
              rmach = radix(zero)
           else if( stdlib_lsame( cmach, 'P' ) ) then
              rmach = eps * radix(zero)
           else if( stdlib_lsame( cmach, 'N' ) ) then
              rmach = digits(zero)
           else if( stdlib_lsame( cmach, 'R' ) ) then
              rmach = one
           else if( stdlib_lsame( cmach, 'M' ) ) then
              rmach = minexponent(zero)
           else if( stdlib_lsame( cmach, 'U' ) ) then
              rmach = tiny(zero)
           else if( stdlib_lsame( cmach, 'L' ) ) then
              rmach = maxexponent(zero)
           else if( stdlib_lsame( cmach, 'O' ) ) then
              rmach = huge(zero)
           else
              rmach = zero
           end if
           stdlib_I64_dlamch = rmach
           return
     end function stdlib_I64_dlamch




     pure real(sp) module function stdlib_I64_slamc3( a, b )
        ! -- lapack auxiliary routine --
           ! univ. of tennessee, univ. of california berkeley and nag ltd..
           ! Scalar Arguments 
           real(sp), intent(in) :: a, b
       ! =====================================================================
           ! Executable Statements 
           stdlib_I64_slamc3 = a + b
           return
     end function stdlib_I64_slamc3

     pure real(dp) module function stdlib_I64_dlamc3( a, b )
        ! -- lapack auxiliary routine --
           ! univ. of tennessee, univ. of california berkeley and nag ltd..
           ! Scalar Arguments 
           real(dp), intent(in) :: a, b
       ! =====================================================================
           ! Executable Statements 
           stdlib_I64_dlamc3 = a + b
           return
     end function stdlib_I64_dlamc3




     pure module subroutine stdlib_I64_slabad( small, large )
     !! SLABAD takes as input the values computed by SLAMCH for underflow and
     !! overflow, and returns the square root of each of these values if the
     !! log of LARGE is sufficiently large.  This subroutine is intended to
     !! identify machines with a large exponent range, such as the Crays, and
     !! redefine the underflow and overflow limits to be the square roots of
     !! the values computed by SLAMCH.  This subroutine is needed because
     !! SLAMCH does not compensate for poor arithmetic in the upper half of
     !! the exponent range, as is found on a Cray.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           real(sp), intent(inout) :: large, small
        ! =====================================================================
           ! Intrinsic Functions 
           ! Executable Statements 
           ! if it looks like we're on a cray, take the square root of
           ! small and large to avoid overflow and underflow problems.
           if( log10( large )>2000. ) then
              small = sqrt( small )
              large = sqrt( large )
           end if
           return
     end subroutine stdlib_I64_slabad

     pure module subroutine stdlib_I64_dlabad( small, large )
     !! DLABAD takes as input the values computed by DLAMCH for underflow and
     !! overflow, and returns the square root of each of these values if the
     !! log of LARGE is sufficiently large.  This subroutine is intended to
     !! identify machines with a large exponent range, such as the Crays, and
     !! redefine the underflow and overflow limits to be the square roots of
     !! the values computed by DLAMCH.  This subroutine is needed because
     !! DLAMCH does not compensate for poor arithmetic in the upper half of
     !! the exponent range, as is found on a Cray.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           real(dp), intent(inout) :: large, small
        ! =====================================================================
           ! Intrinsic Functions 
           ! Executable Statements 
           ! if it looks like we're on a cray, take the square root of
           ! small and large to avoid overflow and underflow problems.
           if( log10( large )>2000._dp ) then
              small = sqrt( small )
              large = sqrt( large )
           end if
           return
     end subroutine stdlib_I64_dlabad




     pure real(sp) module function stdlib_I64_scsum1( n, cx, incx )
     !! SCSUM1 takes the sum of the absolute values of a complex
     !! vector and returns a single precision result.
     !! Based on SCASUM from the Level 1 BLAS.
     !! The change is to use the 'genuine' absolute value.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: zero
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: incx, n
           ! Array Arguments 
           complex(sp), intent(in) :: cx(*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp64) :: i, nincx
           real(sp) :: stemp
           ! Intrinsic Functions 
           ! Executable Statements 
           stdlib_I64_scsum1 = zero
           stemp = zero
           if( n<=0 )return
           if( incx==1 )go to 20
           ! code for increment not equal to 1
           nincx = n*incx
           do i = 1, nincx, incx
              ! next line modified.
              stemp = stemp + abs( cx( i ) )
           end do
           stdlib_I64_scsum1 = stemp
           return
           ! code for increment equal to 1
           20 continue
           do i = 1, n
              ! next line modified.
              stemp = stemp + abs( cx( i ) )
           end do
           stdlib_I64_scsum1 = stemp
           return
     end function stdlib_I64_scsum1



     pure real(dp) module function stdlib_I64_dzsum1( n, cx, incx )
     !! DZSUM1 takes the sum of the absolute values of a complex
     !! vector and returns a double precision result.
     !! Based on DZASUM from the Level 1 BLAS.
     !! The change is to use the 'genuine' absolute value.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: zero
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: incx, n
           ! Array Arguments 
           complex(dp), intent(in) :: cx(*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp64) :: i, nincx
           real(dp) :: stemp
           ! Intrinsic Functions 
           ! Executable Statements 
           stdlib_I64_dzsum1 = zero
           stemp = zero
           if( n<=0 )return
           if( incx==1 )go to 20
           ! code for increment not equal to 1
           nincx = n*incx
           do i = 1, nincx, incx
              ! next line modified.
              stemp = stemp + abs( cx( i ) )
           end do
           stdlib_I64_dzsum1 = stemp
           return
           ! code for increment equal to 1
           20 continue
           do i = 1, n
              ! next line modified.
              stemp = stemp + abs( cx( i ) )
           end do
           stdlib_I64_dzsum1 = stemp
           return
     end function stdlib_I64_dzsum1




     pure module subroutine stdlib_I64_slaqsb( uplo, n, kd, ab, ldab, s, scond, amax, equed )
     !! SLAQSB equilibrates a symmetric band matrix A using the scaling
     !! factors in the vector S.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: one
           ! Scalar Arguments 
           character, intent(out) :: equed
           character, intent(in) :: uplo
           integer(ilp64), intent(in) :: kd, ldab, n
           real(sp), intent(in) :: amax, scond
           ! Array Arguments 
           real(sp), intent(inout) :: ab(ldab,*)
           real(sp), intent(in) :: s(*)
        ! =====================================================================
           ! Parameters 
           real(sp), parameter :: thresh = 0.1e+0_sp
           
           ! Local Scalars 
           integer(ilp64) :: i, j
           real(sp) :: cj, large, small
           ! Intrinsic Functions 
           ! Executable Statements 
           ! quick return if possible
           if( n<=0_ilp64 ) then
              equed = 'N'
              return
           end if
           ! initialize large and small.
           small = stdlib_I64_slamch( 'SAFE MINIMUM' ) / stdlib_I64_slamch( 'PRECISION' )
           large = one / small
           if( scond>=thresh .and. amax>=small .and. amax<=large ) then
              ! no equilibration
              equed = 'N'
           else
              ! replace a by diag(s) * a * diag(s).
              if( stdlib_lsame( uplo, 'U' ) ) then
                 ! upper triangle of a is stored in band format.
                 do j = 1, n
                    cj = s( j )
                    do i = max( 1, j-kd ), j
                       ab( kd+1+i-j, j ) = cj*s( i )*ab( kd+1+i-j, j )
                    end do
                 end do
              else
                 ! lower triangle of a is stored.
                 do j = 1, n
                    cj = s( j )
                    do i = j, min( n, j+kd )
                       ab( 1_ilp64+i-j, j ) = cj*s( i )*ab( 1_ilp64+i-j, j )
                    end do
                 end do
              end if
              equed = 'Y'
           end if
           return
     end subroutine stdlib_I64_slaqsb

     pure module subroutine stdlib_I64_dlaqsb( uplo, n, kd, ab, ldab, s, scond, amax, equed )
     !! DLAQSB equilibrates a symmetric band matrix A using the scaling
     !! factors in the vector S.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: one
           ! Scalar Arguments 
           character, intent(out) :: equed
           character, intent(in) :: uplo
           integer(ilp64), intent(in) :: kd, ldab, n
           real(dp), intent(in) :: amax, scond
           ! Array Arguments 
           real(dp), intent(inout) :: ab(ldab,*)
           real(dp), intent(in) :: s(*)
        ! =====================================================================
           ! Parameters 
           real(dp), parameter :: thresh = 0.1e+0_dp
           
           ! Local Scalars 
           integer(ilp64) :: i, j
           real(dp) :: cj, large, small
           ! Intrinsic Functions 
           ! Executable Statements 
           ! quick return if possible
           if( n<=0_ilp64 ) then
              equed = 'N'
              return
           end if
           ! initialize large and small.
           small = stdlib_I64_dlamch( 'SAFE MINIMUM' ) / stdlib_I64_dlamch( 'PRECISION' )
           large = one / small
           if( scond>=thresh .and. amax>=small .and. amax<=large ) then
              ! no equilibration
              equed = 'N'
           else
              ! replace a by diag(s) * a * diag(s).
              if( stdlib_lsame( uplo, 'U' ) ) then
                 ! upper triangle of a is stored in band format.
                 do j = 1, n
                    cj = s( j )
                    do i = max( 1, j-kd ), j
                       ab( kd+1+i-j, j ) = cj*s( i )*ab( kd+1+i-j, j )
                    end do
                 end do
              else
                 ! lower triangle of a is stored.
                 do j = 1, n
                    cj = s( j )
                    do i = j, min( n, j+kd )
                       ab( 1_ilp64+i-j, j ) = cj*s( i )*ab( 1_ilp64+i-j, j )
                    end do
                 end do
              end if
              equed = 'Y'
           end if
           return
     end subroutine stdlib_I64_dlaqsb


     pure module subroutine stdlib_I64_claqsb( uplo, n, kd, ab, ldab, s, scond, amax, equed )
     !! CLAQSB equilibrates a symmetric band matrix A using the scaling
     !! factors in the vector S.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: one
           ! Scalar Arguments 
           character, intent(out) :: equed
           character, intent(in) :: uplo
           integer(ilp64), intent(in) :: kd, ldab, n
           real(sp), intent(in) :: amax, scond
           ! Array Arguments 
           real(sp), intent(in) :: s(*)
           complex(sp), intent(inout) :: ab(ldab,*)
        ! =====================================================================
           ! Parameters 
           real(sp), parameter :: thresh = 0.1e+0_sp
           
           ! Local Scalars 
           integer(ilp64) :: i, j
           real(sp) :: cj, large, small
           ! Intrinsic Functions 
           ! Executable Statements 
           ! quick return if possible
           if( n<=0_ilp64 ) then
              equed = 'N'
              return
           end if
           ! initialize large and small.
           small = stdlib_I64_slamch( 'SAFE MINIMUM' ) / stdlib_I64_slamch( 'PRECISION' )
           large = one / small
           if( scond>=thresh .and. amax>=small .and. amax<=large ) then
              ! no equilibration
              equed = 'N'
           else
              ! replace a by diag(s) * a * diag(s).
              if( stdlib_lsame( uplo, 'U' ) ) then
                 ! upper triangle of a is stored in band format.
                 do j = 1, n
                    cj = s( j )
                    do i = max( 1, j-kd ), j
                       ab( kd+1+i-j, j ) = cj*s( i )*ab( kd+1+i-j, j )
                    end do
                 end do
              else
                 ! lower triangle of a is stored.
                 do j = 1, n
                    cj = s( j )
                    do i = j, min( n, j+kd )
                       ab( 1_ilp64+i-j, j ) = cj*s( i )*ab( 1_ilp64+i-j, j )
                    end do
                 end do
              end if
              equed = 'Y'
           end if
           return
     end subroutine stdlib_I64_claqsb

     pure module subroutine stdlib_I64_zlaqsb( uplo, n, kd, ab, ldab, s, scond, amax, equed )
     !! ZLAQSB equilibrates a symmetric band matrix A using the scaling
     !! factors in the vector S.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: one
           ! Scalar Arguments 
           character, intent(out) :: equed
           character, intent(in) :: uplo
           integer(ilp64), intent(in) :: kd, ldab, n
           real(dp), intent(in) :: amax, scond
           ! Array Arguments 
           real(dp), intent(in) :: s(*)
           complex(dp), intent(inout) :: ab(ldab,*)
        ! =====================================================================
           ! Parameters 
           real(dp), parameter :: thresh = 0.1e+0_dp
           
           ! Local Scalars 
           integer(ilp64) :: i, j
           real(dp) :: cj, large, small
           ! Intrinsic Functions 
           ! Executable Statements 
           ! quick return if possible
           if( n<=0_ilp64 ) then
              equed = 'N'
              return
           end if
           ! initialize large and small.
           small = stdlib_I64_dlamch( 'SAFE MINIMUM' ) / stdlib_I64_dlamch( 'PRECISION' )
           large = one / small
           if( scond>=thresh .and. amax>=small .and. amax<=large ) then
              ! no equilibration
              equed = 'N'
           else
              ! replace a by diag(s) * a * diag(s).
              if( stdlib_lsame( uplo, 'U' ) ) then
                 ! upper triangle of a is stored in band format.
                 do j = 1, n
                    cj = s( j )
                    do i = max( 1, j-kd ), j
                       ab( kd+1+i-j, j ) = cj*s( i )*ab( kd+1+i-j, j )
                    end do
                 end do
              else
                 ! lower triangle of a is stored.
                 do j = 1, n
                    cj = s( j )
                    do i = j, min( n, j+kd )
                       ab( 1_ilp64+i-j, j ) = cj*s( i )*ab( 1_ilp64+i-j, j )
                    end do
                 end do
              end if
              equed = 'Y'
           end if
           return
     end subroutine stdlib_I64_zlaqsb




     pure module subroutine stdlib_I64_sladiv1( a, b, c, d, p, q )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: one
           ! Scalar Arguments 
           real(sp), intent(inout) :: a
           real(sp), intent(in) :: b, c, d
           real(sp), intent(out) :: p, q
        ! =====================================================================
           
           ! Local Scalars 
           real(sp) :: r, t
           ! Executable Statements 
           r = d / c
           t = one / (c + d * r)
           p = stdlib_I64_sladiv2(a, b, c, d, r, t)
           a = -a
           q = stdlib_I64_sladiv2(b, a, c, d, r, t)
           return
     end subroutine stdlib_I64_sladiv1

     pure module subroutine stdlib_I64_dladiv1( a, b, c, d, p, q )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: one
           ! Scalar Arguments 
           real(dp), intent(inout) :: a
           real(dp), intent(in) :: b, c, d
           real(dp), intent(out) :: p, q
        ! =====================================================================
           
           ! Local Scalars 
           real(dp) :: r, t
           ! Executable Statements 
           r = d / c
           t = one / (c + d * r)
           p = stdlib_I64_dladiv2(a, b, c, d, r, t)
           a = -a
           q = stdlib_I64_dladiv2(b, a, c, d, r, t)
           return
     end subroutine stdlib_I64_dladiv1




     pure real(sp) module function stdlib_I64_sladiv2( a, b, c, d, r, t )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: zero
           ! Scalar Arguments 
           real(sp), intent(in) :: a, b, c, d, r, t
        ! =====================================================================
           
           ! Local Scalars 
           real(sp) :: br
           ! Executable Statements 
           if( r/=zero ) then
              br = b * r
              if( br/=zero ) then
                 stdlib_I64_sladiv2 = (a + br) * t
              else
                 stdlib_I64_sladiv2 = a * t + (b * t) * r
              end if
           else
              stdlib_I64_sladiv2 = (a + d * (b / c)) * t
           end if
           return
     end function stdlib_I64_sladiv2

     pure real(dp) module function stdlib_I64_dladiv2( a, b, c, d, r, t )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: zero
           ! Scalar Arguments 
           real(dp), intent(in) :: a, b, c, d, r, t
        ! =====================================================================
           
           ! Local Scalars 
           real(dp) :: br
           ! Executable Statements 
           if( r/=zero ) then
              br = b * r
              if( br/=zero ) then
                 stdlib_I64_dladiv2 = (a + br) * t
              else
                 stdlib_I64_dladiv2 = a * t + (b * t) * r
              end if
           else
              stdlib_I64_dladiv2 = (a + d * (b / c)) * t
           end if
           return
     end function stdlib_I64_dladiv2




     pure module subroutine stdlib_I64_crot( n, cx, incx, cy, incy, c, s )
     !! CROT applies a plane rotation, where the cos (C) is real and the
     !! sin (S) is complex, and the vectors CX and CY are complex.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: incx, incy, n
           real(sp), intent(in) :: c
           complex(sp), intent(in) :: s
           ! Array Arguments 
           complex(sp), intent(inout) :: cx(*), cy(*)
       ! =====================================================================
           ! Local Scalars 
           integer(ilp64) :: i, ix, iy
           complex(sp) :: stemp
           ! Intrinsic Functions 
           ! Executable Statements 
           if( n<=0 )return
           if( incx==1 .and. incy==1 )go to 20
           ! code for unequal increments or equal increments not equal to 1
           ix = 1_ilp64
           iy = 1_ilp64
           if( incx<0_ilp64 )ix = ( -n+1 )*incx + 1_ilp64
           if( incy<0_ilp64 )iy = ( -n+1 )*incy + 1_ilp64
           do i = 1, n
              stemp = c*cx( ix ) + s*cy( iy )
              cy( iy ) = c*cy( iy ) - conjg( s )*cx( ix )
              cx( ix ) = stemp
              ix = ix + incx
              iy = iy + incy
           end do
           return
           ! code for both increments equal to 1
           20 continue
           do i = 1, n
              stemp = c*cx( i ) + s*cy( i )
              cy( i ) = c*cy( i ) - conjg( s )*cx( i )
              cx( i ) = stemp
           end do
           return
     end subroutine stdlib_I64_crot

     pure module subroutine stdlib_I64_zrot( n, cx, incx, cy, incy, c, s )
     !! ZROT applies a plane rotation, where the cos (C) is real and the
     !! sin (S) is complex, and the vectors CX and CY are complex.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: incx, incy, n
           real(dp), intent(in) :: c
           complex(dp), intent(in) :: s
           ! Array Arguments 
           complex(dp), intent(inout) :: cx(*), cy(*)
       ! =====================================================================
           ! Local Scalars 
           integer(ilp64) :: i, ix, iy
           complex(dp) :: stemp
           ! Intrinsic Functions 
           ! Executable Statements 
           if( n<=0 )return
           if( incx==1 .and. incy==1 )go to 20
           ! code for unequal increments or equal increments not equal to 1
           ix = 1_ilp64
           iy = 1_ilp64
           if( incx<0_ilp64 )ix = ( -n+1 )*incx + 1_ilp64
           if( incy<0_ilp64 )iy = ( -n+1 )*incy + 1_ilp64
           do i = 1, n
              stemp = c*cx( ix ) + s*cy( iy )
              cy( iy ) = c*cy( iy ) - conjg( s )*cx( ix )
              cx( ix ) = stemp
              ix = ix + incx
              iy = iy + incy
           end do
           return
           ! code for both increments equal to 1
           20 continue
           do i = 1, n
              stemp = c*cx( i ) + s*cy( i )
              cy( i ) = c*cy( i ) - conjg( s )*cx( i )
              cx( i ) = stemp
           end do
           return
     end subroutine stdlib_I64_zrot



end submodule stdlib_lapack_auxiliary
