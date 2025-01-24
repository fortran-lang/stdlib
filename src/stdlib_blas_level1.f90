submodule(stdlib_blas) stdlib_blas_level1
  implicit none


  contains

     pure real(sp) module function stdlib_sasum(n,sx,incx)
     use stdlib_blas_constants_sp
     !! SASUM takes the sum of the absolute values.
     !! uses unrolled loops for increment equal to one.
        ! -- reference blas level1 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx, n
           ! Array Arguments 
           real(sp), intent(in) :: sx(*)
        ! =====================================================================
           ! Local Scalars 
           real(sp) :: stemp
           integer(ilp) :: i, m, mp1, nincx
           ! Intrinsic Functions 
           intrinsic :: abs,mod
           stdlib_sasum = zero
           stemp = zero
           if (n<=0 .or. incx<=0) return
           if (incx==1) then
              ! code for increment equal to 1
              ! clean-up loop
              m = mod(n,6)
              if (m/=0) then
                 do i = 1,m
                    stemp = stemp + abs(sx(i))
                 end do
                 if (n<6) then
                    stdlib_sasum = stemp
                    return
                 end if
              end if
              mp1 = m + 1
              do i = mp1,n,6
                 stemp = stemp + abs(sx(i)) + abs(sx(i+1)) +abs(sx(i+2)) + abs(sx(i+3)) +abs(sx(i+&
                           4)) + abs(sx(i+5))
              end do
           else
              ! code for increment not equal to 1
              nincx = n*incx
              do i = 1,nincx,incx
                 stemp = stemp + abs(sx(i))
              end do
           end if
           stdlib_sasum = stemp
           return
     end function stdlib_sasum

     pure real(dp) module function stdlib_dasum(n,dx,incx)
     use stdlib_blas_constants_dp
     !! DASUM takes the sum of the absolute values.
        ! -- reference blas level1 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx, n
           ! Array Arguments 
           real(dp), intent(in) :: dx(*)
        ! =====================================================================
           ! Local Scalars 
           real(dp) :: dtemp
           integer(ilp) :: i, m, mp1, nincx
           ! Intrinsic Functions 
           intrinsic :: abs,mod
           stdlib_dasum = zero
           dtemp = zero
           if (n<=0 .or. incx<=0) return
           if (incx==1) then
              ! code for increment equal to 1
              ! clean-up loop
              m = mod(n,6)
              if (m/=0) then
                 do i = 1,m
                    dtemp = dtemp + abs(dx(i))
                 end do
                 if (n<6) then
                    stdlib_dasum = dtemp
                    return
                 end if
              end if
              mp1 = m + 1
              do i = mp1,n,6
                 dtemp = dtemp + abs(dx(i)) + abs(dx(i+1)) +abs(dx(i+2)) + abs(dx(i+3)) +abs(dx(i+&
                           4)) + abs(dx(i+5))
              end do
           else
              ! code for increment not equal to 1
              nincx = n*incx
              do i = 1,nincx,incx
                 dtemp = dtemp + abs(dx(i))
              end do
           end if
           stdlib_dasum = dtemp
           return
     end function stdlib_dasum




     pure real(sp) module function stdlib_scasum(n,cx,incx)
     use stdlib_blas_constants_sp
     !! SCASUM takes the sum of the (|Re(.)| + |Im(.)|)'s of a complex vector and
     !! returns a single precision result.
        ! -- reference blas level1 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx, n
           ! Array Arguments 
           complex(sp), intent(in) :: cx(*)
        ! =====================================================================
           ! Local Scalars 
           real(sp) :: stemp
           integer(ilp) :: i, nincx
           ! Intrinsic Functions 
           intrinsic :: abs,aimag,real
           stdlib_scasum = zero
           stemp = zero
           if (n<=0 .or. incx<=0) return
           if (incx==1) then
              ! code for increment equal to 1
              do i = 1,n
                 stemp = stemp + abs(real(cx(i),KIND=sp)) + abs(aimag(cx(i)))
              end do
           else
              ! code for increment not equal to 1
              nincx = n*incx
              do i = 1,nincx,incx
                 stemp = stemp + abs(real(cx(i),KIND=sp)) + abs(aimag(cx(i)))
              end do
           end if
           stdlib_scasum = stemp
           return
     end function stdlib_scasum



     pure real(dp) module function stdlib_dzasum(n,zx,incx)
     use stdlib_blas_constants_dp
     !! DZASUM takes the sum of the (|Re(.)| + |Im(.)|)'s of a complex vector and
     !! returns a double precision result.
        ! -- reference blas level1 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx, n
           ! Array Arguments 
           complex(dp), intent(in) :: zx(*)
        ! =====================================================================
           ! Local Scalars 
           real(dp) :: stemp
           integer(ilp) :: i, nincx
           stdlib_dzasum = zero
           stemp = zero
           if (n<=0 .or. incx<=0) return
           if (incx==1) then
              ! code for increment equal to 1
              do i = 1,n
                 stemp = stemp + stdlib_cabs1(zx(i))
              end do
           else
              ! code for increment not equal to 1
              nincx = n*incx
              do i = 1,nincx,incx
                 stemp = stemp + stdlib_cabs1(zx(i))
              end do
           end if
           stdlib_dzasum = stemp
           return
     end function stdlib_dzasum




     pure module subroutine stdlib_saxpy(n,sa,sx,incx,sy,incy)
     use stdlib_blas_constants_sp
     !! SAXPY constant times a vector plus a vector.
     !! uses unrolled loops for increments equal to one.
        ! -- reference blas level1 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           real(sp), intent(in) :: sa
           integer(ilp), intent(in) :: incx, incy, n
           ! Array Arguments 
           real(sp), intent(in) :: sx(*)
           real(sp), intent(inout) :: sy(*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, ix, iy, m, mp1
           ! Intrinsic Functions 
           intrinsic :: mod
           if (n<=0) return
           if (sa==0.0_sp) return
           if (incx==1 .and. incy==1) then
              ! code for both increments equal to 1
              ! clean-up loop
              m = mod(n,4)
              if (m/=0) then
                 do i = 1,m
                    sy(i) = sy(i) + sa*sx(i)
                 end do
              end if
              if (n<4) return
              mp1 = m + 1
              do i = mp1,n,4
                 sy(i) = sy(i) + sa*sx(i)
                 sy(i+1) = sy(i+1) + sa*sx(i+1)
                 sy(i+2) = sy(i+2) + sa*sx(i+2)
                 sy(i+3) = sy(i+3) + sa*sx(i+3)
              end do
           else
              ! code for unequal increments or equal increments
                ! not equal to 1
              ix = 1
              iy = 1
              if (incx<0) ix = (-n+1)*incx + 1
              if (incy<0) iy = (-n+1)*incy + 1
              do i = 1,n
               sy(iy) = sy(iy) + sa*sx(ix)
               ix = ix + incx
               iy = iy + incy
              end do
           end if
           return
     end subroutine stdlib_saxpy

     pure module subroutine stdlib_daxpy(n,da,dx,incx,dy,incy)
     use stdlib_blas_constants_dp
     !! DAXPY constant times a vector plus a vector.
     !! uses unrolled loops for increments equal to one.
        ! -- reference blas level1 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           real(dp), intent(in) :: da
           integer(ilp), intent(in) :: incx, incy, n
           ! Array Arguments 
           real(dp), intent(in) :: dx(*)
           real(dp), intent(inout) :: dy(*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, ix, iy, m, mp1
           ! Intrinsic Functions 
           intrinsic :: mod
           if (n<=0) return
           if (da==0.0_dp) return
           if (incx==1 .and. incy==1) then
              ! code for both increments equal to 1
              ! clean-up loop
              m = mod(n,4)
              if (m/=0) then
                 do i = 1,m
                    dy(i) = dy(i) + da*dx(i)
                 end do
              end if
              if (n<4) return
              mp1 = m + 1
              do i = mp1,n,4
                 dy(i) = dy(i) + da*dx(i)
                 dy(i+1) = dy(i+1) + da*dx(i+1)
                 dy(i+2) = dy(i+2) + da*dx(i+2)
                 dy(i+3) = dy(i+3) + da*dx(i+3)
              end do
           else
              ! code for unequal increments or equal increments
                ! not equal to 1
              ix = 1
              iy = 1
              if (incx<0) ix = (-n+1)*incx + 1
              if (incy<0) iy = (-n+1)*incy + 1
              do i = 1,n
               dy(iy) = dy(iy) + da*dx(ix)
               ix = ix + incx
               iy = iy + incy
              end do
           end if
           return
     end subroutine stdlib_daxpy


     pure module subroutine stdlib_caxpy(n,ca,cx,incx,cy,incy)
     use stdlib_blas_constants_sp
     !! CAXPY constant times a vector plus a vector.
        ! -- reference blas level1 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           complex(sp), intent(in) :: ca
           integer(ilp), intent(in) :: incx, incy, n
           ! Array Arguments 
           complex(sp), intent(in) :: cx(*)
           complex(sp), intent(inout) :: cy(*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, ix, iy
           if (n<=0) return
           if (stdlib_cabs1(ca)==0.0e+0_sp) return
           if (incx==1 .and. incy==1) then
              ! code for both increments equal to 1
              do i = 1,n
                 cy(i) = cy(i) + ca*cx(i)
              end do
           else
              ! code for unequal increments or equal increments
                ! not equal to 1
              ix = 1
              iy = 1
              if (incx<0) ix = (-n+1)*incx + 1
              if (incy<0) iy = (-n+1)*incy + 1
              do i = 1,n
                 cy(iy) = cy(iy) + ca*cx(ix)
                 ix = ix + incx
                 iy = iy + incy
              end do
           end if
           return
     end subroutine stdlib_caxpy

     pure module subroutine stdlib_zaxpy(n,za,zx,incx,zy,incy)
     use stdlib_blas_constants_dp
     !! ZAXPY constant times a vector plus a vector.
        ! -- reference blas level1 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           complex(dp), intent(in) :: za
           integer(ilp), intent(in) :: incx, incy, n
           ! Array Arguments 
           complex(dp), intent(in) :: zx(*)
           complex(dp), intent(inout) :: zy(*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, ix, iy
           if (n<=0) return
           if (stdlib_cabs1(za)==0.0_dp) return
           if (incx==1 .and. incy==1) then
              ! code for both increments equal to 1
              do i = 1,n
                 zy(i) = zy(i) + za*zx(i)
              end do
           else
              ! code for unequal increments or equal increments
                ! not equal to 1
              ix = 1
              iy = 1
              if (incx<0) ix = (-n+1)*incx + 1
              if (incy<0) iy = (-n+1)*incy + 1
              do i = 1,n
                 zy(iy) = zy(iy) + za*zx(ix)
                 ix = ix + incx
                 iy = iy + incy
              end do
           end if
           return
     end subroutine stdlib_zaxpy




     pure module subroutine stdlib_scopy(n,sx,incx,sy,incy)
     use stdlib_blas_constants_sp
     !! SCOPY copies a vector, x, to a vector, y.
     !! uses unrolled loops for increments equal to 1.
        ! -- reference blas level1 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx, incy, n
           ! Array Arguments 
           real(sp), intent(in) :: sx(*)
           real(sp), intent(out) :: sy(*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, ix, iy, m, mp1
           ! Intrinsic Functions 
           intrinsic :: mod
           if (n<=0) return
           if (incx==1 .and. incy==1) then
              ! code for both increments equal to 1
              ! clean-up loop
              m = mod(n,7)
              if (m/=0) then
                 do i = 1,m
                    sy(i) = sx(i)
                 end do
                 if (n<7) return
              end if
              mp1 = m + 1
              do i = mp1,n,7
                 sy(i) = sx(i)
                 sy(i+1) = sx(i+1)
                 sy(i+2) = sx(i+2)
                 sy(i+3) = sx(i+3)
                 sy(i+4) = sx(i+4)
                 sy(i+5) = sx(i+5)
                 sy(i+6) = sx(i+6)
              end do
           else
              ! code for unequal increments or equal increments
                ! not equal to 1
              ix = 1
              iy = 1
              if (incx<0) ix = (-n+1)*incx + 1
              if (incy<0) iy = (-n+1)*incy + 1
              do i = 1,n
                 sy(iy) = sx(ix)
                 ix = ix + incx
                 iy = iy + incy
              end do
           end if
           return
     end subroutine stdlib_scopy

     pure module subroutine stdlib_dcopy(n,dx,incx,dy,incy)
     use stdlib_blas_constants_dp
     !! DCOPY copies a vector, x, to a vector, y.
     !! uses unrolled loops for increments equal to 1.
        ! -- reference blas level1 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx, incy, n
           ! Array Arguments 
           real(dp), intent(in) :: dx(*)
           real(dp), intent(out) :: dy(*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, ix, iy, m, mp1
           ! Intrinsic Functions 
           intrinsic :: mod
           if (n<=0) return
           if (incx==1 .and. incy==1) then
              ! code for both increments equal to 1
              ! clean-up loop
              m = mod(n,7)
              if (m/=0) then
                 do i = 1,m
                    dy(i) = dx(i)
                 end do
                 if (n<7) return
              end if
              mp1 = m + 1
              do i = mp1,n,7
                 dy(i) = dx(i)
                 dy(i+1) = dx(i+1)
                 dy(i+2) = dx(i+2)
                 dy(i+3) = dx(i+3)
                 dy(i+4) = dx(i+4)
                 dy(i+5) = dx(i+5)
                 dy(i+6) = dx(i+6)
              end do
           else
              ! code for unequal increments or equal increments
                ! not equal to 1
              ix = 1
              iy = 1
              if (incx<0) ix = (-n+1)*incx + 1
              if (incy<0) iy = (-n+1)*incy + 1
              do i = 1,n
                 dy(iy) = dx(ix)
                 ix = ix + incx
                 iy = iy + incy
              end do
           end if
           return
     end subroutine stdlib_dcopy


     pure module subroutine stdlib_ccopy(n,cx,incx,cy,incy)
     use stdlib_blas_constants_sp
     !! CCOPY copies a vector x to a vector y.
        ! -- reference blas level1 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx, incy, n
           ! Array Arguments 
           complex(sp), intent(in) :: cx(*)
           complex(sp), intent(out) :: cy(*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, ix, iy
           if (n<=0) return
           if (incx==1 .and. incy==1) then
              ! code for both increments equal to 1
              do i = 1,n
                 cy(i) = cx(i)
              end do
           else
              ! code for unequal increments or equal increments
                ! not equal to 1
              ix = 1
              iy = 1
              if (incx<0) ix = (-n+1)*incx + 1
              if (incy<0) iy = (-n+1)*incy + 1
              do i = 1,n
                 cy(iy) = cx(ix)
                 ix = ix + incx
                 iy = iy + incy
              end do
           end if
           return
     end subroutine stdlib_ccopy

     pure module subroutine stdlib_zcopy(n,zx,incx,zy,incy)
     use stdlib_blas_constants_dp
     !! ZCOPY copies a vector, x, to a vector, y.
        ! -- reference blas level1 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx, incy, n
           ! Array Arguments 
           complex(dp), intent(in) :: zx(*)
           complex(dp), intent(out) :: zy(*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, ix, iy
           if (n<=0) return
           if (incx==1 .and. incy==1) then
              ! code for both increments equal to 1
              do i = 1,n
               zy(i) = zx(i)
              end do
           else
              ! code for unequal increments or equal increments
                ! not equal to 1
              ix = 1
              iy = 1
              if (incx<0) ix = (-n+1)*incx + 1
              if (incy<0) iy = (-n+1)*incy + 1
              do i = 1,n
                 zy(iy) = zx(ix)
                 ix = ix + incx
                 iy = iy + incy
              end do
           end if
           return
     end subroutine stdlib_zcopy




     pure real(sp) module function stdlib_sdot(n,sx,incx,sy,incy)
     use stdlib_blas_constants_sp
     !! SDOT forms the dot product of two vectors.
     !! uses unrolled loops for increments equal to one.
        ! -- reference blas level1 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx, incy, n
           ! Array Arguments 
           real(sp), intent(in) :: sx(*), sy(*)
        ! =====================================================================
           ! Local Scalars 
           real(sp) :: stemp
           integer(ilp) :: i, ix, iy, m, mp1
           ! Intrinsic Functions 
           intrinsic :: mod
           stemp = zero
           stdlib_sdot = zero
           if (n<=0) return
           if (incx==1 .and. incy==1) then
              ! code for both increments equal to 1
              ! clean-up loop
              m = mod(n,5)
              if (m/=0) then
                 do i = 1,m
                    stemp = stemp + sx(i)*sy(i)
                 end do
                 if (n<5) then
                    stdlib_sdot=stemp
                 return
                 end if
              end if
              mp1 = m + 1
              do i = mp1,n,5
               stemp = stemp + sx(i)*sy(i) + sx(i+1)*sy(i+1) +sx(i+2)*sy(i+2) + sx(i+3)*sy(i+3) + &
                         sx(i+4)*sy(i+4)
              end do
           else
              ! code for unequal increments or equal increments
                ! not equal to 1
              ix = 1
              iy = 1
              if (incx<0) ix = (-n+1)*incx + 1
              if (incy<0) iy = (-n+1)*incy + 1
              do i = 1,n
                 stemp = stemp + sx(ix)*sy(iy)
                 ix = ix + incx
                 iy = iy + incy
              end do
           end if
           stdlib_sdot = stemp
           return
     end function stdlib_sdot

     pure real(dp) module function stdlib_ddot(n,dx,incx,dy,incy)
     use stdlib_blas_constants_dp
     !! DDOT forms the dot product of two vectors.
     !! uses unrolled loops for increments equal to one.
        ! -- reference blas level1 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx, incy, n
           ! Array Arguments 
           real(dp), intent(in) :: dx(*), dy(*)
        ! =====================================================================
           ! Local Scalars 
           real(dp) :: dtemp
           integer(ilp) :: i, ix, iy, m, mp1
           ! Intrinsic Functions 
           intrinsic :: mod
           stdlib_ddot = zero
           dtemp = zero
           if (n<=0) return
           if (incx==1 .and. incy==1) then
              ! code for both increments equal to 1
              ! clean-up loop
              m = mod(n,5)
              if (m/=0) then
                 do i = 1,m
                    dtemp = dtemp + dx(i)*dy(i)
                 end do
                 if (n<5) then
                    stdlib_ddot=dtemp
                 return
                 end if
              end if
              mp1 = m + 1
              do i = mp1,n,5
               dtemp = dtemp + dx(i)*dy(i) + dx(i+1)*dy(i+1) +dx(i+2)*dy(i+2) + dx(i+3)*dy(i+3) + &
                         dx(i+4)*dy(i+4)
              end do
           else
              ! code for unequal increments or equal increments
                ! not equal to 1
              ix = 1
              iy = 1
              if (incx<0) ix = (-n+1)*incx + 1
              if (incy<0) iy = (-n+1)*incy + 1
              do i = 1,n
                 dtemp = dtemp + dx(ix)*dy(iy)
                 ix = ix + incx
                 iy = iy + incy
              end do
           end if
           stdlib_ddot = dtemp
           return
     end function stdlib_ddot




     pure real(dp) module function stdlib_dsdot(n,sx,incx,sy,incy)
     use stdlib_blas_constants_dp
     !! Compute the inner product of two vectors with extended
     !! precision accumulation and result.
     !! Returns D.P. dot product accumulated in D.P., for S.P. SX and SY
     !! DSDOT = sum for I = 0 to N-1 of  SX(LX+I*INCX) * SY(LY+I*INCY),
     !! where LX = 1 if INCX >= 0, else LX = 1+(1-N)*INCX, and LY is
     !! defined in a similar way using INCY.
        ! -- reference blas level1 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx, incy, n
           ! Array Arguments 
           real(sp), intent(in) :: sx(*), sy(*)
        ! authors:
        ! ========
        ! lawson, c. l., (jpl), hanson, r. j., (snla),
        ! kincaid, d. r., (u. of texas), krogh, f. t., (jpl)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, kx, ky, ns
           ! Intrinsic Functions 
           intrinsic :: real
           stdlib_dsdot = zero
           if (n<=0) return
           if (incx==incy .and. incx>0) then
           ! code for equal, positive, non-unit increments.
              ns = n*incx
              do i = 1,ns,incx
                 stdlib_dsdot = stdlib_dsdot + real(sx(i),KIND=dp)*real(sy(i),KIND=dp)
              end do
           else
           ! code for unequal or nonpositive increments.
              kx = 1
              ky = 1
              if (incx<0) kx = 1 + (1-n)*incx
              if (incy<0) ky = 1 + (1-n)*incy
              do i = 1,n
                 stdlib_dsdot = stdlib_dsdot + real(sx(kx),KIND=dp)*real(sy(ky),KIND=dp)
                 kx = kx + incx
                 ky = ky + incy
              end do
           end if
           return
     end function stdlib_dsdot




     pure complex(sp) module function stdlib_cdotc(n,cx,incx,cy,incy)
     use stdlib_blas_constants_sp
     !! CDOTC forms the dot product of two complex vectors
     !! CDOTC = X^H * Y
        ! -- reference blas level1 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx, incy, n
           ! Array Arguments 
           complex(sp), intent(in) :: cx(*), cy(*)
        ! =====================================================================
           ! Local Scalars 
           complex(sp) :: ctemp
           integer(ilp) :: i, ix, iy
           ! Intrinsic Functions 
           intrinsic :: conjg
           ctemp = (0.0_sp,0.0_sp)
           stdlib_cdotc = (0.0_sp,0.0_sp)
           if (n<=0) return
           if (incx==1 .and. incy==1) then
              ! code for both increments equal to 1
              do i = 1,n
                 ctemp = ctemp + conjg(cx(i))*cy(i)
              end do
           else
              ! code for unequal increments or equal increments
                ! not equal to 1
              ix = 1
              iy = 1
              if (incx<0) ix = (-n+1)*incx + 1
              if (incy<0) iy = (-n+1)*incy + 1
              do i = 1,n
                 ctemp = ctemp + conjg(cx(ix))*cy(iy)
                 ix = ix + incx
                 iy = iy + incy
              end do
           end if
           stdlib_cdotc = ctemp
           return
     end function stdlib_cdotc

     pure complex(dp) module function stdlib_zdotc(n,zx,incx,zy,incy)
     use stdlib_blas_constants_dp
     !! ZDOTC forms the dot product of two complex vectors
     !! ZDOTC = X^H * Y
        ! -- reference blas level1 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx, incy, n
           ! Array Arguments 
           complex(dp), intent(in) :: zx(*), zy(*)
        ! =====================================================================
           ! Local Scalars 
           complex(dp) :: ztemp
           integer(ilp) :: i, ix, iy
           ! Intrinsic Functions 
           intrinsic :: conjg
           ztemp = (0.0_dp,0.0_dp)
           stdlib_zdotc = (0.0_dp,0.0_dp)
           if (n<=0) return
           if (incx==1 .and. incy==1) then
              ! code for both increments equal to 1
              do i = 1,n
                 ztemp = ztemp + conjg(zx(i))*zy(i)
              end do
           else
              ! code for unequal increments or equal increments
                ! not equal to 1
              ix = 1
              iy = 1
              if (incx<0) ix = (-n+1)*incx + 1
              if (incy<0) iy = (-n+1)*incy + 1
              do i = 1,n
                 ztemp = ztemp + conjg(zx(ix))*zy(iy)
                 ix = ix + incx
                 iy = iy + incy
              end do
           end if
           stdlib_zdotc = ztemp
           return
     end function stdlib_zdotc




     pure complex(sp) module function stdlib_cdotu(n,cx,incx,cy,incy)
     use stdlib_blas_constants_sp
     !! CDOTU forms the dot product of two complex vectors
     !! CDOTU = X^T * Y
        ! -- reference blas level1 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx, incy, n
           ! Array Arguments 
           complex(sp), intent(in) :: cx(*), cy(*)
        ! =====================================================================
           ! Local Scalars 
           complex(sp) :: ctemp
           integer(ilp) :: i, ix, iy
           ctemp = (0.0_sp,0.0_sp)
           stdlib_cdotu = (0.0_sp,0.0_sp)
           if (n<=0) return
           if (incx==1 .and. incy==1) then
              ! code for both increments equal to 1
              do i = 1,n
                 ctemp = ctemp + cx(i)*cy(i)
              end do
           else
              ! code for unequal increments or equal increments
                ! not equal to 1
              ix = 1
              iy = 1
              if (incx<0) ix = (-n+1)*incx + 1
              if (incy<0) iy = (-n+1)*incy + 1
              do i = 1,n
                 ctemp = ctemp + cx(ix)*cy(iy)
                 ix = ix + incx
                 iy = iy + incy
              end do
           end if
           stdlib_cdotu = ctemp
           return
     end function stdlib_cdotu

     pure complex(dp) module function stdlib_zdotu(n,zx,incx,zy,incy)
     use stdlib_blas_constants_dp
     !! ZDOTU forms the dot product of two complex vectors
     !! ZDOTU = X^T * Y
        ! -- reference blas level1 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx, incy, n
           ! Array Arguments 
           complex(dp), intent(in) :: zx(*), zy(*)
        ! =====================================================================
           ! Local Scalars 
           complex(dp) :: ztemp
           integer(ilp) :: i, ix, iy
           ztemp = (0.0_dp,0.0_dp)
           stdlib_zdotu = (0.0_dp,0.0_dp)
           if (n<=0) return
           if (incx==1 .and. incy==1) then
              ! code for both increments equal to 1
              do i = 1,n
                 ztemp = ztemp + zx(i)*zy(i)
              end do
           else
              ! code for unequal increments or equal increments
                ! not equal to 1
              ix = 1
              iy = 1
              if (incx<0) ix = (-n+1)*incx + 1
              if (incy<0) iy = (-n+1)*incy + 1
              do i = 1,n
                 ztemp = ztemp + zx(ix)*zy(iy)
                 ix = ix + incx
                 iy = iy + incy
              end do
           end if
           stdlib_zdotu = ztemp
           return
     end function stdlib_zdotu




     pure real(sp) module function stdlib_snrm2( n, x, incx )
     use stdlib_blas_constants_sp
     !! SNRM2 returns the euclidean norm of a vector via the function
     !! name, so that
     !! SNRM2 := sqrt( x'*x ).
        ! -- reference blas level1 routine (version 3.9.1_sp) --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! march 2021
        ! Scalar Arguments 
        integer(ilp), intent(in) :: incx, n
        ! Array Arguments 
        real(sp), intent(in) :: x(*)
        ! =====================================================================
        ! Constants 
        real(sp), parameter :: maxn = huge(0.0_sp)
        ! .. blue's scaling constants ..
        ! Local Scalars 
     integer(ilp) :: i, ix
     logical(lk) :: notbig
        real(sp) :: abig, amed, asml, ax, scl, sumsq, ymax, ymin
        ! quick return if possible
        stdlib_snrm2 = zero
        if( n <= 0 ) return
        scl = one
        sumsq = zero
        ! compute the sum of squares in 3 accumulators:
           ! abig -- sums of squares scaled down to avoid overflow
           ! asml -- sums of squares scaled up to avoid underflow
           ! amed -- sums of squares that do not require scaling
        ! the thresholds and multipliers are
           ! tbig -- values bigger than this are scaled down by sbig
           ! tsml -- values smaller than this are scaled up by ssml
        notbig = .true.
        asml = zero
        amed = zero
        abig = zero
        ix = 1
        if( incx < 0 ) ix = 1 - (n-1)*incx
        do i = 1, n
           ax = abs(x(ix))
           if (ax > tbig) then
              abig = abig + (ax*sbig)**2
              notbig = .false.
           else if (ax < tsml) then
              if (notbig) asml = asml + (ax*ssml)**2
           else
              amed = amed + ax**2
           end if
           ix = ix + incx
        end do
        ! combine abig and amed or amed and asml if more than one
        ! accumulator was used.
        if (abig > zero) then
           ! combine abig and amed if abig > 0.
           if ( (amed > zero) .or. (amed > maxn) .or. (amed /= amed) ) then
              abig = abig + (amed*sbig)*sbig
           end if
           scl = one / sbig
           sumsq = abig
        else if (asml > zero) then
           ! combine amed and asml if asml > 0.
           if ( (amed > zero) .or. (amed > maxn) .or. (amed /= amed) ) then
              amed = sqrt(amed)
              asml = sqrt(asml) / ssml
              if (asml > amed) then
                 ymin = amed
                 ymax = asml
              else
                 ymin = asml
                 ymax = amed
              end if
              scl = one
              sumsq = ymax**2*( one + (ymin/ymax)**2 )
           else
              scl = one / ssml
              sumsq = asml
           end if
        else
           ! otherwise all values are mid-range
           scl = one
           sumsq = amed
        end if
        stdlib_snrm2 = scl*sqrt( sumsq )
        return
     end function stdlib_snrm2

     pure real(dp) module function stdlib_dnrm2( n, x, incx )
     use stdlib_blas_constants_dp
     !! DNRM2 returns the euclidean norm of a vector via the function
     !! name, so that
     !! DNRM2 := sqrt( x'*x )
        ! -- reference blas level1 routine (version 3.9.1_dp) --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! march 2021
        ! Scalar Arguments 
        integer(ilp), intent(in) :: incx, n
        ! Array Arguments 
        real(dp), intent(in) :: x(*)
        ! =====================================================================
        ! Constants 
        real(dp), parameter :: maxn = huge(0.0_dp)
        ! .. blue's scaling constants ..
        ! Local Scalars 
     integer(ilp) :: i, ix
     logical(lk) :: notbig
        real(dp) :: abig, amed, asml, ax, scl, sumsq, ymax, ymin
        ! quick return if possible
        stdlib_dnrm2 = zero
        if( n <= 0 ) return
        scl = one
        sumsq = zero
        ! compute the sum of squares in 3 accumulators:
           ! abig -- sums of squares scaled down to avoid overflow
           ! asml -- sums of squares scaled up to avoid underflow
           ! amed -- sums of squares that do not require scaling
        ! the thresholds and multipliers are
           ! tbig -- values bigger than this are scaled down by sbig
           ! tsml -- values smaller than this are scaled up by ssml
        notbig = .true.
        asml = zero
        amed = zero
        abig = zero
        ix = 1
        if( incx < 0 ) ix = 1 - (n-1)*incx
        do i = 1, n
           ax = abs(x(ix))
           if (ax > tbig) then
              abig = abig + (ax*sbig)**2
              notbig = .false.
           else if (ax < tsml) then
              if (notbig) asml = asml + (ax*ssml)**2
           else
              amed = amed + ax**2
           end if
           ix = ix + incx
        end do
        ! combine abig and amed or amed and asml if more than one
        ! accumulator was used.
        if (abig > zero) then
           ! combine abig and amed if abig > 0.
           if ( (amed > zero) .or. (amed > maxn) .or. (amed /= amed) ) then
              abig = abig + (amed*sbig)*sbig
           end if
           scl = one / sbig
           sumsq = abig
        else if (asml > zero) then
           ! combine amed and asml if asml > 0.
           if ( (amed > zero) .or. (amed > maxn) .or. (amed /= amed) ) then
              amed = sqrt(amed)
              asml = sqrt(asml) / ssml
              if (asml > amed) then
                 ymin = amed
                 ymax = asml
              else
                 ymin = asml
                 ymax = amed
              end if
              scl = one
              sumsq = ymax**2*( one + (ymin/ymax)**2 )
           else
              scl = one / ssml
              sumsq = asml
           end if
        else
           ! otherwise all values are mid-range
           scl = one
           sumsq = amed
        end if
        stdlib_dnrm2 = scl*sqrt( sumsq )
        return
     end function stdlib_dnrm2




     pure real(sp) module function stdlib_scnrm2( n, x, incx )
     use stdlib_blas_constants_sp
     !! SCNRM2 returns the euclidean norm of a vector via the function
     !! name, so that
     !! SCNRM2 := sqrt( x**H*x )
        ! -- reference blas level1 routine (version 3.9.1_sp) --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! march 2021
        ! Scalar Arguments 
        integer(ilp), intent(in) :: incx, n
        ! Array Arguments 
        complex(sp), intent(in) :: x(*)
        ! =====================================================================
        ! Constants 
        real(sp), parameter :: maxn = huge(0.0_sp)
        ! .. blue's scaling constants ..
        ! Local Scalars 
     integer(ilp) :: i, ix
     logical(lk) :: notbig
        real(sp) :: abig, amed, asml, ax, scl, sumsq, ymax, ymin
        ! quick return if possible
        stdlib_scnrm2 = zero
        if( n <= 0 ) return
        scl = one
        sumsq = zero
        ! compute the sum of squares in 3 accumulators:
           ! abig -- sums of squares scaled down to avoid overflow
           ! asml -- sums of squares scaled up to avoid underflow
           ! amed -- sums of squares that do not require scaling
        ! the thresholds and multipliers are
           ! tbig -- values bigger than this are scaled down by sbig
           ! tsml -- values smaller than this are scaled up by ssml
        notbig = .true.
        asml = zero
        amed = zero
        abig = zero
        ix = 1
        if( incx < 0 ) ix = 1 - (n-1)*incx
        do i = 1, n
           ax = abs(real(x(ix),KIND=sp))
           if (ax > tbig) then
              abig = abig + (ax*sbig)**2
              notbig = .false.
           else if (ax < tsml) then
              if (notbig) asml = asml + (ax*ssml)**2
           else
              amed = amed + ax**2
           end if
           ax = abs(aimag(x(ix)))
           if (ax > tbig) then
              abig = abig + (ax*sbig)**2
              notbig = .false.
           else if (ax < tsml) then
              if (notbig) asml = asml + (ax*ssml)**2
           else
              amed = amed + ax**2
           end if
           ix = ix + incx
        end do
        ! combine abig and amed or amed and asml if more than one
        ! accumulator was used.
        if (abig > zero) then
           ! combine abig and amed if abig > 0.
           if ( (amed > zero) .or. (amed > maxn) .or. (amed /= amed) ) then
              abig = abig + (amed*sbig)*sbig
           end if
           scl = one / sbig
           sumsq = abig
        else if (asml > zero) then
           ! combine amed and asml if asml > 0.
           if ( (amed > zero) .or. (amed > maxn) .or. (amed /= amed) ) then
              amed = sqrt(amed)
              asml = sqrt(asml) / ssml
              if (asml > amed) then
                 ymin = amed
                 ymax = asml
              else
                 ymin = asml
                 ymax = amed
              end if
              scl = one
              sumsq = ymax**2*( one + (ymin/ymax)**2 )
           else
              scl = one / ssml
              sumsq = asml
           end if
        else
           ! otherwise all values are mid-range
           scl = one
           sumsq = amed
        end if
        stdlib_scnrm2 = scl*sqrt( sumsq )
        return
     end function stdlib_scnrm2



     pure real(dp) module function stdlib_dznrm2( n, x, incx )
     use stdlib_blas_constants_dp
     !! DZNRM2 returns the euclidean norm of a vector via the function
     !! name, so that
     !! DZNRM2 := sqrt( x**H*x )
        ! -- reference blas level1 routine (version 3.9.1_dp) --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! march 2021
        ! Scalar Arguments 
        integer(ilp), intent(in) :: incx, n
        ! Array Arguments 
        complex(dp), intent(in) :: x(*)
        ! =====================================================================
        ! Constants 
        real(dp), parameter :: maxn = huge(0.0_dp)
        ! .. blue's scaling constants ..
        ! Local Scalars 
        integer(ilp) :: i, ix
        logical(lk) :: notbig
        real(dp) :: abig, amed, asml, ax, scl, sumsq, ymax, ymin
        ! quick return if possible
        stdlib_dznrm2 = zero
        if( n <= 0 ) return
        scl = one
        sumsq = zero
        ! compute the sum of squares in 3 accumulators:
           ! abig -- sums of squares scaled down to avoid overflow
           ! asml -- sums of squares scaled up to avoid underflow
           ! amed -- sums of squares that do not require scaling
        ! the thresholds and multipliers are
           ! tbig -- values bigger than this are scaled down by sbig
           ! tsml -- values smaller than this are scaled up by ssml
        notbig = .true.
        asml = zero
        amed = zero
        abig = zero
        ix = 1
        if( incx < 0 ) ix = 1 - (n-1)*incx
        do i = 1, n
           ax = abs(real(x(ix),KIND=dp))
           if (ax > tbig) then
              abig = abig + (ax*sbig)**2
              notbig = .false.
           else if (ax < tsml) then
              if (notbig) asml = asml + (ax*ssml)**2
           else
              amed = amed + ax**2
           end if
           ax = abs(aimag(x(ix)))
           if (ax > tbig) then
              abig = abig + (ax*sbig)**2
              notbig = .false.
           else if (ax < tsml) then
              if (notbig) asml = asml + (ax*ssml)**2
           else
              amed = amed + ax**2
           end if
           ix = ix + incx
        end do
        ! combine abig and amed or amed and asml if more than one
        ! accumulator was used.
        if (abig > zero) then
           ! combine abig and amed if abig > 0.
           if ( (amed > zero) .or. (amed > maxn) .or. (amed /= amed) ) then
              abig = abig + (amed*sbig)*sbig
           end if
           scl = one / sbig
           sumsq = abig
        else if (asml > zero) then
           ! combine amed and asml if asml > 0.
           if ( (amed > zero) .or. (amed > maxn) .or. (amed /= amed) ) then
              amed = sqrt(amed)
              asml = sqrt(asml) / ssml
              if (asml > amed) then
                 ymin = amed
                 ymax = asml
              else
                 ymin = asml
                 ymax = amed
              end if
              scl = one
              sumsq = ymax**2*( one + (ymin/ymax)**2 )
           else
              scl = one / ssml
              sumsq = asml
           end if
        else
           ! otherwise all values are mid-range
           scl = one
           sumsq = amed
        end if
        stdlib_dznrm2 = scl*sqrt( sumsq )
        return
     end function stdlib_dznrm2




     pure module subroutine stdlib_srot(n,sx,incx,sy,incy,c,s)
     use stdlib_blas_constants_sp
     !! applies a plane rotation.
        ! -- reference blas level1 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           real(sp), intent(in) :: c, s
           integer(ilp), intent(in) :: incx, incy, n
           ! Array Arguments 
           real(sp), intent(inout) :: sx(*), sy(*)
        ! =====================================================================
           ! Local Scalars 
           real(sp) :: stemp
           integer(ilp) :: i, ix, iy
           if (n<=0) return
           if (incx==1 .and. incy==1) then
             ! code for both increments equal to 1
              do i = 1,n
                 stemp = c*sx(i) + s*sy(i)
                 sy(i) = c*sy(i) - s*sx(i)
                 sx(i) = stemp
              end do
           else
             ! code for unequal increments or equal increments not equal
               ! to 1
              ix = 1
              iy = 1
              if (incx<0) ix = (-n+1)*incx + 1
              if (incy<0) iy = (-n+1)*incy + 1
              do i = 1,n
                 stemp = c*sx(ix) + s*sy(iy)
                 sy(iy) = c*sy(iy) - s*sx(ix)
                 sx(ix) = stemp
                 ix = ix + incx
                 iy = iy + incy
              end do
           end if
           return
     end subroutine stdlib_srot

     pure module subroutine stdlib_drot(n,dx,incx,dy,incy,c,s)
     use stdlib_blas_constants_dp
     !! DROT applies a plane rotation.
        ! -- reference blas level1 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           real(dp), intent(in) :: c, s
           integer(ilp), intent(in) :: incx, incy, n
           ! Array Arguments 
           real(dp), intent(inout) :: dx(*), dy(*)
        ! =====================================================================
           ! Local Scalars 
           real(dp) :: dtemp
           integer(ilp) :: i, ix, iy
           if (n<=0) return
           if (incx==1 .and. incy==1) then
             ! code for both increments equal to 1
              do i = 1,n
                 dtemp = c*dx(i) + s*dy(i)
                 dy(i) = c*dy(i) - s*dx(i)
                 dx(i) = dtemp
              end do
           else
             ! code for unequal increments or equal increments not equal
               ! to 1
              ix = 1
              iy = 1
              if (incx<0) ix = (-n+1)*incx + 1
              if (incy<0) iy = (-n+1)*incy + 1
              do i = 1,n
                 dtemp = c*dx(ix) + s*dy(iy)
                 dy(iy) = c*dy(iy) - s*dx(ix)
                 dx(ix) = dtemp
                 ix = ix + incx
                 iy = iy + incy
              end do
           end if
           return
     end subroutine stdlib_drot




     pure module subroutine stdlib_zdrot( n, zx, incx, zy, incy, c, s )
     use stdlib_blas_constants_dp
     !! Applies a plane rotation, where the cos and sin (c and s) are real
     !! and the vectors cx and cy are complex.
     !! jack dongarra, linpack, 3/11/78.
        ! -- reference blas level1 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx, incy, n
           real(dp), intent(in) :: c, s
           ! Array Arguments 
           complex(dp), intent(inout) :: zx(*), zy(*)
       ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, ix, iy
           complex(dp) :: ctemp
           ! Executable Statements 
           if( n<=0 )return
           if( incx==1 .and. incy==1 ) then
              ! code for both increments equal to 1
              do i = 1, n
                 ctemp = c*zx( i ) + s*zy( i )
                 zy( i ) = c*zy( i ) - s*zx( i )
                 zx( i ) = ctemp
              end do
           else
              ! code for unequal increments or equal increments not equal
                ! to 1
              ix = 1
              iy = 1
              if( incx<0 )ix = ( -n+1 )*incx + 1
              if( incy<0 )iy = ( -n+1 )*incy + 1
              do i = 1, n
                 ctemp = c*zx( ix ) + s*zy( iy )
                 zy( iy ) = c*zy( iy ) - s*zx( ix )
                 zx( ix ) = ctemp
                 ix = ix + incx
                 iy = iy + incy
              end do
           end if
           return
     end subroutine stdlib_zdrot




     pure module subroutine stdlib_srotg( a, b, c, s )
     use stdlib_blas_constants_sp
     !! The computation uses the formulas
     !! sigma = sgn(a)    if |a| >  |b|
     !! = sgn(b)    if |b| >= |a|
     !! r = sigma*sqrt( a**2 + b**2 )
     !! c = 1; s = 0      if r = 0
     !! c = a/r; s = b/r  if r != 0
     !! The subroutine also computes
     !! z = s    if |a| > |b|,
     !! = 1/c  if |b| >= |a| and c != 0
     !! = 1    if c = 0
     !! This allows c and s to be reconstructed from z as follows:
     !! If z = 1, set c = 0, s = 1.
     !! If |z| < 1, set c = sqrt(1 - z**2) and s = z.
     !! If |z| > 1, set c = 1/z and s = sqrt( 1 - c**2).
        ! -- reference blas level1 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
        ! Constants 
        ! Scaling Constants 
        ! Scalar Arguments 
        real(sp), intent(inout) :: a, b
        real(sp), intent(out) :: c, s
        ! =====================================================================
        ! Local Scalars 
        real(sp) :: anorm, bnorm, scl, sigma, r, z
        anorm = abs(a)
        bnorm = abs(b)
        if( bnorm == zero ) then
           c = one
           s = zero
           b = zero
        else if( anorm == zero ) then
           c = zero
           s = one
           a = b
           b = one
        else
           scl = min( safmax, max( safmin, anorm, bnorm ) )
           if( anorm > bnorm ) then
              sigma = sign(one,a)
           else
              sigma = sign(one,b)
           end if
           r = sigma*( scl*sqrt((a/scl)**2 + (b/scl)**2) )
           c = a/r
           s = b/r
           if( anorm > bnorm ) then
              z = s
           else if( c /= zero ) then
              z = one/c
           else
              z = one
           end if
           a = r
           b = z
        end if
        return
     end subroutine stdlib_srotg

     pure module subroutine stdlib_drotg( a, b, c, s )
     use stdlib_blas_constants_dp
     !! The computation uses the formulas
     !! sigma = sgn(a)    if |a| >  |b|
     !! = sgn(b)    if |b| >= |a|
     !! r = sigma*sqrt( a**2 + b**2 )
     !! c = 1; s = 0      if r = 0
     !! c = a/r; s = b/r  if r != 0
     !! The subroutine also computes
     !! z = s    if |a| > |b|,
     !! = 1/c  if |b| >= |a| and c != 0
     !! = 1    if c = 0
     !! This allows c and s to be reconstructed from z as follows:
     !! If z = 1, set c = 0, s = 1.
     !! If |z| < 1, set c = sqrt(1 - z**2) and s = z.
     !! If |z| > 1, set c = 1/z and s = sqrt( 1 - c**2).
        ! -- reference blas level1 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
        ! Scaling Constants 
        ! Scalar Arguments 
        real(dp), intent(inout) :: a, b
        real(dp), intent(out) :: c, s
        ! =====================================================================
        ! Local Scalars 
        real(dp) :: anorm, bnorm, scl, sigma, r, z
        anorm = abs(a)
        bnorm = abs(b)
        if( bnorm == zero ) then
           c = one
           s = zero
           b = zero
        else if( anorm == zero ) then
           c = zero
           s = one
           a = b
           b = one
        else
           scl = min( safmax, max( safmin, anorm, bnorm ) )
           if( anorm > bnorm ) then
              sigma = sign(one,a)
           else
              sigma = sign(one,b)
           end if
           r = sigma*( scl*sqrt((a/scl)**2 + (b/scl)**2) )
           c = a/r
           s = b/r
           if( anorm > bnorm ) then
              z = s
           else if( c /= zero ) then
              z = one/c
           else
              z = one
           end if
           a = r
           b = z
        end if
        return
     end subroutine stdlib_drotg


     pure module subroutine stdlib_crotg( a, b, c, s )
     use stdlib_blas_constants_sp
     !! The computation uses the formulas
     !! |x| = sqrt( Re(x)**2 + Im(x)**2 )
     !! sgn(x) = x / |x|  if x /= 0
     !! = 1        if x  = 0
     !! c = |a| / sqrt(|a|**2 + |b|**2)
     !! s = sgn(a) * conjg(b) / sqrt(|a|**2 + |b|**2)
     !! When a and b are real and r /= 0, the formulas simplify to
     !! r = sgn(a)*sqrt(|a|**2 + |b|**2)
     !! c = a / r
     !! s = b / r
     !! the same as in SROTG when |a| > |b|.  When |b| >= |a|, the
     !! sign of c and s will be different from those computed by SROTG
     !! if the signs of a and b are not the same.
        ! -- reference blas level1 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
        ! Scaling Constants 
        ! Scalar Arguments 
        real(sp), intent(out) :: c
        complex(sp), intent(inout) :: a
        complex(sp), intent(in) :: b
        complex(sp), intent(out) :: s
        ! =====================================================================
        ! Local Scalars 
        real(sp) :: d, f1, f2, g1, g2, h2, p, u, uu, v, vv, w
        complex(sp) :: f, fs, g, gs, r, t
        ! Intrinsic Functions 
        intrinsic :: abs,aimag,conjg,max,min,real,sqrt
        ! Statement Functions 
        real(sp) :: abssq
        ! Statement Function Definitions 
        abssq( t ) = real( t,KIND=sp)**2 + aimag( t )**2
        ! Executable Statements 
        f = a
        g = b
        if( g == czero ) then
           c = one
           s = czero
           r = f
        else if( f == czero ) then
           c = zero
           g1 = max( abs(real(g,KIND=sp)), abs(aimag(g)) )
           if( g1 > rtmin .and. g1 < rtmax ) then
              ! use unscaled algorithm
              g2 = abssq( g )
              d = sqrt( g2 )
              s = conjg( g ) / d
              r = d
           else
              ! use scaled algorithm
              u = min( safmax, max( safmin, g1 ) )
              uu = one / u
              gs = g*uu
              g2 = abssq( gs )
              d = sqrt( g2 )
              s = conjg( gs ) / d
              r = d*u
           end if
        else
           f1 = max( abs(real(f,KIND=sp)), abs(aimag(f)) )
           g1 = max( abs(real(g,KIND=sp)), abs(aimag(g)) )
     if( f1 > rtmin .and. f1 < rtmax .and.          g1 > rtmin .and. g1 < rtmax ) then
              ! use unscaled algorithm
              f2 = abssq( f )
              g2 = abssq( g )
              h2 = f2 + g2
              if( f2 > rtmin .and. h2 < rtmax ) then
                 d = sqrt( f2*h2 )
              else
                 d = sqrt( f2 )*sqrt( h2 )
              end if
              p = 1 / d
              c = f2*p
              s = conjg( g )*( f*p )
              r = f*( h2*p )
           else
              ! use scaled algorithm
              u = min( safmax, max( safmin, f1, g1 ) )
              uu = one / u
              gs = g*uu
              g2 = abssq( gs )
              if( f1*uu < rtmin ) then
                 ! f is not well-scaled when scaled by g1.
                 ! use a different scaling for f.
                 v = min( safmax, max( safmin, f1 ) )
                 vv = one / v
                 w = v * uu
                 fs = f*vv
                 f2 = abssq( fs )
                 h2 = f2*w**2 + g2
              else
                 ! otherwise use the same scaling for f and g.
                 w = one
                 fs = f*uu
                 f2 = abssq( fs )
                 h2 = f2 + g2
              end if
              if( f2 > rtmin .and. h2 < rtmax ) then
                 d = sqrt( f2*h2 )
              else
                 d = sqrt( f2 )*sqrt( h2 )
              end if
              p = 1 / d
              c = ( f2*p )*w
              s = conjg( gs )*( fs*p )
              r = ( fs*( h2*p ) )*u
           end if
        end if
        a = r
        return
     end subroutine stdlib_crotg

     pure module subroutine stdlib_zrotg( a, b, c, s )
     use stdlib_blas_constants_dp
     !! The computation uses the formulas
     !! |x| = sqrt( Re(x)**2 + Im(x)**2 )
     !! sgn(x) = x / |x|  if x /= 0
     !! = 1        if x  = 0
     !! c = |a| / sqrt(|a|**2 + |b|**2)
     !! s = sgn(a) * conjg(b) / sqrt(|a|**2 + |b|**2)
     !! When a and b are real and r /= 0, the formulas simplify to
     !! r = sgn(a)*sqrt(|a|**2 + |b|**2)
     !! c = a / r
     !! s = b / r
     !! the same as in DROTG when |a| > |b|.  When |b| >= |a|, the
     !! sign of c and s will be different from those computed by DROTG
     !! if the signs of a and b are not the same.
        ! -- reference blas level1 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
        ! Scaling Constants 
        ! Scalar Arguments 
        real(dp), intent(out) :: c
        complex(dp), intent(inout) :: a
        complex(dp), intent(in) :: b
        complex(dp), intent(out) :: s
        ! =====================================================================
        ! Local Scalars 
        real(dp) :: d, f1, f2, g1, g2, h2, p, u, uu, v, vv, w
        complex(dp) :: f, fs, g, gs, r, t
        ! Intrinsic Functions 
        intrinsic :: abs,aimag,conjg,max,min,real,sqrt
        ! Statement Functions 
        real(dp) :: abssq
        ! Statement Function Definitions 
        abssq( t ) = real( t,KIND=dp)**2 + aimag( t )**2
        ! Executable Statements 
        f = a
        g = b
        if( g == czero ) then
           c = one
           s = czero
           r = f
        else if( f == czero ) then
           c = zero
           g1 = max( abs(real(g,KIND=dp)), abs(aimag(g)) )
           if( g1 > rtmin .and. g1 < rtmax ) then
              ! use unscaled algorithm
              g2 = abssq( g )
              d = sqrt( g2 )
              s = conjg( g ) / d
              r = d
           else
              ! use scaled algorithm
              u = min( safmax, max( safmin, g1 ) )
              uu = one / u
              gs = g*uu
              g2 = abssq( gs )
              d = sqrt( g2 )
              s = conjg( gs ) / d
              r = d*u
           end if
        else
           f1 = max( abs(real(f,KIND=dp)), abs(aimag(f)) )
           g1 = max( abs(real(g,KIND=dp)), abs(aimag(g)) )
     if( f1 > rtmin .and. f1 < rtmax .and.          g1 > rtmin .and. g1 < rtmax ) then
              ! use unscaled algorithm
              f2 = abssq( f )
              g2 = abssq( g )
              h2 = f2 + g2
              if( f2 > rtmin .and. h2 < rtmax ) then
                 d = sqrt( f2*h2 )
              else
                 d = sqrt( f2 )*sqrt( h2 )
              end if
              p = 1 / d
              c = f2*p
              s = conjg( g )*( f*p )
              r = f*( h2*p )
           else
              ! use scaled algorithm
              u = min( safmax, max( safmin, f1, g1 ) )
              uu = one / u
              gs = g*uu
              g2 = abssq( gs )
              if( f1*uu < rtmin ) then
                 ! f is not well-scaled when scaled by g1.
                 ! use a different scaling for f.
                 v = min( safmax, max( safmin, f1 ) )
                 vv = one / v
                 w = v * uu
                 fs = f*vv
                 f2 = abssq( fs )
                 h2 = f2*w**2 + g2
              else
                 ! otherwise use the same scaling for f and g.
                 w = one
                 fs = f*uu
                 f2 = abssq( fs )
                 h2 = f2 + g2
              end if
              if( f2 > rtmin .and. h2 < rtmax ) then
                 d = sqrt( f2*h2 )
              else
                 d = sqrt( f2 )*sqrt( h2 )
              end if
              p = 1 / d
              c = ( f2*p )*w
              s = conjg( gs )*( fs*p )
              r = ( fs*( h2*p ) )*u
           end if
        end if
        a = r
        return
     end subroutine stdlib_zrotg




     pure module subroutine stdlib_srotm(n,sx,incx,sy,incy,sparam)
     use stdlib_blas_constants_sp
     !! SROTM applies the modified Givens transformation, \(H\), to the 2-by-N matrix 
     !! $$ \left[ \begin{array}{c}SX^T\\SY^T\\ \end{array} \right], $$ 
     !! where \(^T\) indicates transpose. The elements of \(SX\) are in 
     !! SX(LX+I*INCX), I = 0:N-1, where LX = 1 if INCX >= 0, else LX = (-INCX)*N, 
     !! and similarly for SY using LY and INCY. 
     !! With SPARAM(1)=SFLAG, \(H\) has one of the following forms: 
     !! $$ H=\underbrace{\begin{bmatrix}SH_{11} & SH_{12}\\SH_{21} & SH_{22}\end{bmatrix}}_{SFLAG=-1},
     !!      \underbrace{\begin{bmatrix}1 & SH_{12}\\SH_{21} & 1\end{bmatrix}}_{SFLAG=0}, 
     !!      \underbrace{\begin{bmatrix}SH_{11} & 1\\-1 & SH_{22}\end{bmatrix}}_{SFLAG=1},  
     !!      \underbrace{\begin{bmatrix}1 & 0\\0 & 1\end{bmatrix}}_{SFLAG=-2}. $$     
     !! See SROTMG for a description of data storage in SPARAM. 
        ! -- reference blas level1 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx, incy, n
           ! Array Arguments 
           real(sp), intent(in) :: sparam(5)
           real(sp), intent(inout) :: sx(*), sy(*)
        ! =====================================================================
           ! Local Scalars 
           real(sp) :: sflag, sh11, sh12, sh21, sh22, w, z
           integer(ilp) :: i, kx, ky, nsteps
           ! Data Statements 
           sflag = sparam(1)
           if (n<=0 .or. (sflag+two==zero)) return
           if (incx==incy.and.incx>0) then
              nsteps = n*incx
              if (sflag<zero) then
                 sh11 = sparam(2)
                 sh12 = sparam(4)
                 sh21 = sparam(3)
                 sh22 = sparam(5)
                 do i = 1,nsteps,incx
                    w = sx(i)
                    z = sy(i)
                    sx(i) = w*sh11 + z*sh12
                    sy(i) = w*sh21 + z*sh22
                 end do
              else if (sflag==zero) then
                 sh12 = sparam(4)
                 sh21 = sparam(3)
                 do i = 1,nsteps,incx
                    w = sx(i)
                    z = sy(i)
                    sx(i) = w + z*sh12
                    sy(i) = w*sh21 + z
                 end do
              else
                 sh11 = sparam(2)
                 sh22 = sparam(5)
                 do i = 1,nsteps,incx
                    w = sx(i)
                    z = sy(i)
                    sx(i) = w*sh11 + z
                    sy(i) = -w + sh22*z
                 end do
              end if
           else
              kx = 1
              ky = 1
              if (incx<0) kx = 1 + (1-n)*incx
              if (incy<0) ky = 1 + (1-n)*incy
              if (sflag<zero) then
                 sh11 = sparam(2)
                 sh12 = sparam(4)
                 sh21 = sparam(3)
                 sh22 = sparam(5)
                 do i = 1,n
                    w = sx(kx)
                    z = sy(ky)
                    sx(kx) = w*sh11 + z*sh12
                    sy(ky) = w*sh21 + z*sh22
                    kx = kx + incx
                    ky = ky + incy
                 end do
              else if (sflag==zero) then
                 sh12 = sparam(4)
                 sh21 = sparam(3)
                 do i = 1,n
                    w = sx(kx)
                    z = sy(ky)
                    sx(kx) = w + z*sh12
                    sy(ky) = w*sh21 + z
                    kx = kx + incx
                    ky = ky + incy
                 end do
              else
                  sh11 = sparam(2)
                  sh22 = sparam(5)
                  do i = 1,n
                     w = sx(kx)
                     z = sy(ky)
                     sx(kx) = w*sh11 + z
                     sy(ky) = -w + sh22*z
                     kx = kx + incx
                     ky = ky + incy
                 end do
              end if
           end if
           return
     end subroutine stdlib_srotm

     pure module subroutine stdlib_drotm(n,dx,incx,dy,incy,dparam)    
     use stdlib_blas_constants_dp
     !! DROTM applies the modified Givens transformation, \(H\), to the 2-by-N matrix 
     !! $$ \left[ \begin{array}{c}DX^T\\DY^T\\ \end{array} \right], $$ 
     !! where \(^T\) indicates transpose. The elements of \(DX\) are in 
     !! DX(LX+I*INCX), I = 0:N-1, where LX = 1 if INCX >= 0, else LX = (-INCX)*N, 
     !! and similarly for DY using LY and INCY. 
     !! With DPARAM(1)=DFLAG, \(H\) has one of the following forms: 
     !! $$ H=\underbrace{\begin{bmatrix}DH_{11} & DH_{12}\\DH_{21} & DH_{22}\end{bmatrix}}_{DFLAG=-1},
     !!      \underbrace{\begin{bmatrix}1 & DH_{12}\\DH_{21} & 1\end{bmatrix}}_{DFLAG=0}, 
     !!      \underbrace{\begin{bmatrix}DH_{11} & 1\\-1 & DH_{22}\end{bmatrix}}_{DFLAG=1},  
     !!      \underbrace{\begin{bmatrix}1 & 0\\0 & 1\end{bmatrix}}_{DFLAG=-2}. $$     
     !! See DROTMG for a description of data storage in DPARAM.   
        ! -- reference blas level1 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx, incy, n
           ! Array Arguments 
           real(dp), intent(in) :: dparam(5)
           real(dp), intent(inout) :: dx(*), dy(*)
        ! =====================================================================
           ! Local Scalars 
           real(dp) :: dflag, dh11, dh12, dh21, dh22, w, z
           integer(ilp) :: i, kx, ky, nsteps
           ! Data Statements 
           dflag = dparam(1)
           if (n<=0 .or. (dflag+two==zero)) return
           if (incx==incy.and.incx>0) then
              nsteps = n*incx
              if (dflag<zero) then
                 dh11 = dparam(2)
                 dh12 = dparam(4)
                 dh21 = dparam(3)
                 dh22 = dparam(5)
                 do i = 1,nsteps,incx
                    w = dx(i)
                    z = dy(i)
                    dx(i) = w*dh11 + z*dh12
                    dy(i) = w*dh21 + z*dh22
                 end do
              else if (dflag==zero) then
                 dh12 = dparam(4)
                 dh21 = dparam(3)
                 do i = 1,nsteps,incx
                    w = dx(i)
                    z = dy(i)
                    dx(i) = w + z*dh12
                    dy(i) = w*dh21 + z
                 end do
              else
                 dh11 = dparam(2)
                 dh22 = dparam(5)
                 do i = 1,nsteps,incx
                    w = dx(i)
                    z = dy(i)
                    dx(i) = w*dh11 + z
                    dy(i) = -w + dh22*z
                 end do
              end if
           else
              kx = 1
              ky = 1
              if (incx<0) kx = 1 + (1-n)*incx
              if (incy<0) ky = 1 + (1-n)*incy
              if (dflag<zero) then
                 dh11 = dparam(2)
                 dh12 = dparam(4)
                 dh21 = dparam(3)
                 dh22 = dparam(5)
                 do i = 1,n
                    w = dx(kx)
                    z = dy(ky)
                    dx(kx) = w*dh11 + z*dh12
                    dy(ky) = w*dh21 + z*dh22
                    kx = kx + incx
                    ky = ky + incy
                 end do
              else if (dflag==zero) then
                 dh12 = dparam(4)
                 dh21 = dparam(3)
                 do i = 1,n
                    w = dx(kx)
                    z = dy(ky)
                    dx(kx) = w + z*dh12
                    dy(ky) = w*dh21 + z
                    kx = kx + incx
                    ky = ky + incy
                 end do
              else
                  dh11 = dparam(2)
                  dh22 = dparam(5)
                  do i = 1,n
                     w = dx(kx)
                     z = dy(ky)
                     dx(kx) = w*dh11 + z
                     dy(ky) = -w + dh22*z
                     kx = kx + incx
                     ky = ky + incy
                 end do
              end if
           end if
           return
     end subroutine stdlib_drotm




     pure module subroutine stdlib_srotmg(sd1,sd2,sx1,sy1,sparam)
     use stdlib_blas_constants_sp
     !! SROTMG Constructs the modified Givens transformation matrix \(H\) which zeros the 
     !! second component of the 2-vector 
     !! $$ \left[ {\sqrt{SD_1}\cdot SX_1,\sqrt{SD_2}\cdot SY_2} \right]^T. $$
     !! With SPARAM(1)=SFLAG, \(H\) has one of the following forms:          
     !! $$ H=\underbrace{\begin{bmatrix}SH_{11} & SD_{12}\\SH_{21} & SH_{22}\end{bmatrix}}_{SFLAG=-1},
     !!      \underbrace{\begin{bmatrix}1 & SH_{12}\\SH_{21} & 1\end{bmatrix}}_{SFLAG=0}, 
     !!      \underbrace{\begin{bmatrix}SH_{11} & 1\\-1 & SH_{22}\end{bmatrix}}_{SFLAG=1},  
     !!      \underbrace{\begin{bmatrix}1 & 0\\0 & 1\end{bmatrix}}_{SFLAG=2}. $$
     !! Locations 2-4 of SPARAM contain SH11, SH21, SH12 and SH22 respectively.
     !! (Values of 1.0, -1.0, or 0.0 implied by the value of SPARAM(1) are not stored in SPARAM.)
     !! The values of parameters GAMSQ and RGAMSQ may be inexact. This is OK as they are only 
     !! used for testing the size of DD1 and DD2. All actual scaling of data is done using GAM.     
        ! -- reference blas level1 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           real(sp), intent(inout) :: sd1, sd2, sx1
           real(sp), intent(in) :: sy1
           ! Array Arguments 
           real(sp), intent(out) :: sparam(5)
        ! =====================================================================
           ! Local Scalars 
           real(sp) :: gam, gamsq, rgamsq, sflag, sh11, sh12, sh21, sh22, sp1, sp2, sq1, sq2,&
                      stemp, su
           ! Intrinsic Functions 
           intrinsic :: abs
           ! Data Statements 
           gam = 4096.0_sp
           gamsq = 1.67772e7_sp
           rgamsq = 5.96046e-8_sp
           if (sd1<zero) then
              ! go zero-h-d-and-sx1..
              sflag = -one
              sh11 = zero
              sh12 = zero
              sh21 = zero
              sh22 = zero
              sd1 = zero
              sd2 = zero
              sx1 = zero
           else
              ! case-sd1-nonnegative
              sp2 = sd2*sy1
              if (sp2==zero) then
                 sflag = -two
                 sparam(1) = sflag
                 return
              end if
              ! regular-case..
              sp1 = sd1*sx1
              sq2 = sp2*sy1
              sq1 = sp1*sx1
              if (abs(sq1)>abs(sq2)) then
                 sh21 = -sy1/sx1
                 sh12 = sp2/sp1
                 su = one - sh12*sh21
                if (su>zero) then
                  sflag = zero
                  sd1 = sd1/su
                  sd2 = sd2/su
                  sx1 = sx1*su
                else
                  ! this code path if here for safety. we do not expect this
                  ! condition to ever hold except in edge cases with rounding
                  ! errors. see doi: 10.1145/355841.355847
                  sflag = -one
                  sh11 = zero
                  sh12 = zero
                  sh21 = zero
                  sh22 = zero
                  sd1 = zero
                  sd2 = zero
                  sx1 = zero
                end if
              else
                 if (sq2<zero) then
                    ! go zero-h-d-and-sx1..
                    sflag = -one
                    sh11 = zero
                    sh12 = zero
                    sh21 = zero
                    sh22 = zero
                    sd1 = zero
                    sd2 = zero
                    sx1 = zero
                 else
                    sflag = one
                    sh11 = sp1/sp2
                    sh22 = sx1/sy1
                    su = one + sh11*sh22
                    stemp = sd2/su
                    sd2 = sd1/su
                    sd1 = stemp
                    sx1 = sy1*su
                 end if
              end if
           ! procedure..scale-check
              if (sd1/=zero) then
                 do while ((sd1<=rgamsq) .or. (sd1>=gamsq))
                    if (sflag==zero) then
                       sh11 = one
                       sh22 = one
                       sflag = -one
                    else
                       sh21 = -one
                       sh12 = one
                       sflag = -one
                    end if
                    if (sd1<=rgamsq) then
                       sd1 = sd1*gam**2
                       sx1 = sx1/gam
                       sh11 = sh11/gam
                       sh12 = sh12/gam
                    else
                       sd1 = sd1/gam**2
                       sx1 = sx1*gam
                       sh11 = sh11*gam
                       sh12 = sh12*gam
                    end if
                 enddo
              end if
              if (sd2/=zero) then
                 do while ( (abs(sd2)<=rgamsq) .or. (abs(sd2)>=gamsq) )
                    if (sflag==zero) then
                       sh11 = one
                       sh22 = one
                       sflag = -one
                    else
                       sh21 = -one
                       sh12 = one
                       sflag = -one
                    end if
                    if (abs(sd2)<=rgamsq) then
                       sd2 = sd2*gam**2
                       sh21 = sh21/gam
                       sh22 = sh22/gam
                    else
                       sd2 = sd2/gam**2
                       sh21 = sh21*gam
                       sh22 = sh22*gam
                    end if
                 end do
              end if
           end if
           if (sflag<zero) then
              sparam(2) = sh11
              sparam(3) = sh21
              sparam(4) = sh12
              sparam(5) = sh22
           else if (sflag==zero) then
              sparam(3) = sh21
              sparam(4) = sh12
           else
              sparam(2) = sh11
              sparam(5) = sh22
           end if
           sparam(1) = sflag
           return
     end subroutine stdlib_srotmg

     pure module subroutine stdlib_drotmg(dd1,dd2,dx1,dy1,dparam)
     use stdlib_blas_constants_dp
     !! DROTMG Constructs the modified Givens transformation matrix \(H\) which zeros the 
     !! second component of the 2-vector 
     !! $$ \left[ {\sqrt{DD_1}\cdot DX_1,\sqrt{DD_2}\cdot DY_2} \right]^T. $$
     !! With DPARAM(1)=DFLAG, \(H\) has one of the following forms:          
     !! $$ H=\underbrace{\begin{bmatrix}DH_{11} & DH_{12}\\DH_{21} & DH_{22}\end{bmatrix}}_{DFLAG=-1},
     !!      \underbrace{\begin{bmatrix}1 & DH_{12}\\DH_{21} & 1\end{bmatrix}}_{DFLAG=0}, 
     !!      \underbrace{\begin{bmatrix}DH_{11} & 1\\-1 & DH_{22}\end{bmatrix}}_{DFLAG=1},  
     !!      \underbrace{\begin{bmatrix}1 & 0\\0 & 1\end{bmatrix}}_{DFLAG=-2}. $$
     !! Locations 2-4 of DPARAM contain DH11, DH21, DH12 and DH22 respectively.
     !! (Values of 1.0, -1.0, or 0.0 implied by the value of DPARAM(1) are not stored in DPARAM.)
     !! The values of parameters GAMSQ and RGAMSQ may be inexact. This is OK as they are only 
     !! used for testing the size of DD1 and DD2. All actual scaling of data is done using GAM.          
        ! -- reference blas level1 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           real(dp), intent(inout) :: dd1, dd2, dx1
           real(dp), intent(in) :: dy1
           ! Array Arguments 
           real(dp), intent(out) :: dparam(5)
        ! =====================================================================
           ! Local Scalars 
           real(dp) :: dflag, dh11, dh12, dh21, dh22, dp1, dp2, dq1, dq2, dtemp, du, gam, gamsq, &
                     rgamsq
           ! Intrinsic Functions 
           intrinsic :: abs
           ! Data Statements 
           gam = 4096.0_dp
           gamsq = 16777216.0_dp
           rgamsq = 5.9604645e-8_dp
           if (dd1<zero) then
              ! go zero-h-d-and-dx1..
              dflag = -one
              dh11 = zero
              dh12 = zero
              dh21 = zero
              dh22 = zero
              dd1 = zero
              dd2 = zero
              dx1 = zero
           else
              ! case-dd1-nonnegative
              dp2 = dd2*dy1
              if (dp2==zero) then
                 dflag = -two
                 dparam(1) = dflag
                 return
              end if
              ! regular-case..
              dp1 = dd1*dx1
              dq2 = dp2*dy1
              dq1 = dp1*dx1
              if (abs(dq1)>abs(dq2)) then
                 dh21 = -dy1/dx1
                 dh12 = dp2/dp1
                 du = one - dh12*dh21
                if (du>zero) then
                  dflag = zero
                  dd1 = dd1/du
                  dd2 = dd2/du
                  dx1 = dx1*du
                else
                  ! this code path if here for safety. we do not expect this
                  ! condition to ever hold except in edge cases with rounding
                  ! errors. see doi: 10.1145/355841.355847
                  dflag = -one
                  dh11 = zero
                  dh12 = zero
                  dh21 = zero
                  dh22 = zero
                  dd1 = zero
                  dd2 = zero
                  dx1 = zero
                end if
              else
                 if (dq2<zero) then
                    ! go zero-h-d-and-dx1..
                    dflag = -one
                    dh11 = zero
                    dh12 = zero
                    dh21 = zero
                    dh22 = zero
                    dd1 = zero
                    dd2 = zero
                    dx1 = zero
                 else
                    dflag = one
                    dh11 = dp1/dp2
                    dh22 = dx1/dy1
                    du = one + dh11*dh22
                    dtemp = dd2/du
                    dd2 = dd1/du
                    dd1 = dtemp
                    dx1 = dy1*du
                 end if
              end if
           ! procedure..scale-check
              if (dd1/=zero) then
                 do while ((dd1<=rgamsq) .or. (dd1>=gamsq))
                    if (dflag==zero) then
                       dh11 = one
                       dh22 = one
                       dflag = -one
                    else
                       dh21 = -one
                       dh12 = one
                       dflag = -one
                    end if
                    if (dd1<=rgamsq) then
                       dd1 = dd1*gam**2
                       dx1 = dx1/gam
                       dh11 = dh11/gam
                       dh12 = dh12/gam
                    else
                       dd1 = dd1/gam**2
                       dx1 = dx1*gam
                       dh11 = dh11*gam
                       dh12 = dh12*gam
                    end if
                 enddo
              end if
              if (dd2/=zero) then
                 do while ( (abs(dd2)<=rgamsq) .or. (abs(dd2)>=gamsq) )
                    if (dflag==zero) then
                       dh11 = one
                       dh22 = one
                       dflag = -one
                    else
                       dh21 = -one
                       dh12 = one
                       dflag = -one
                    end if
                    if (abs(dd2)<=rgamsq) then
                       dd2 = dd2*gam**2
                       dh21 = dh21/gam
                       dh22 = dh22/gam
                    else
                       dd2 = dd2/gam**2
                       dh21 = dh21*gam
                       dh22 = dh22*gam
                    end if
                 end do
              end if
           end if
           if (dflag<zero) then
              dparam(2) = dh11
              dparam(3) = dh21
              dparam(4) = dh12
              dparam(5) = dh22
           else if (dflag==zero) then
              dparam(3) = dh21
              dparam(4) = dh12
           else
              dparam(2) = dh11
              dparam(5) = dh22
           end if
           dparam(1) = dflag
           return
     end subroutine stdlib_drotmg




     pure module subroutine stdlib_csrot( n, cx, incx, cy, incy, c, s )
     use stdlib_blas_constants_sp
     !! CSROT applies a plane rotation, where the cos and sin (c and s) are real
     !! and the vectors cx and cy are complex.
     !! jack dongarra, linpack, 3/11/78.
        ! -- reference blas level1 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx, incy, n
           real(sp), intent(in) :: c, s
           ! Array Arguments 
           complex(sp), intent(inout) :: cx(*), cy(*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, ix, iy
           complex(sp) :: ctemp
           ! Executable Statements 
           if( n<=0 )return
           if( incx==1 .and. incy==1 ) then
              ! code for both increments equal to 1
              do i = 1, n
                 ctemp = c*cx( i ) + s*cy( i )
                 cy( i ) = c*cy( i ) - s*cx( i )
                 cx( i ) = ctemp
              end do
           else
              ! code for unequal increments or equal increments not equal
                ! to 1
              ix = 1
              iy = 1
              if( incx<0 )ix = ( -n+1 )*incx + 1
              if( incy<0 )iy = ( -n+1 )*incy + 1
              do i = 1, n
                 ctemp = c*cx( ix ) + s*cy( iy )
                 cy( iy ) = c*cy( iy ) - s*cx( ix )
                 cx( ix ) = ctemp
                 ix = ix + incx
                 iy = iy + incy
              end do
           end if
           return
     end subroutine stdlib_csrot



     pure module subroutine stdlib_sscal(n,sa,sx,incx)
     use stdlib_blas_constants_sp
     !! SSCAL scales a vector by a constant.
     !! uses unrolled loops for increment equal to 1.
        ! -- reference blas level1 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           real(sp), intent(in) :: sa
           integer(ilp), intent(in) :: incx, n
           ! Array Arguments 
           real(sp), intent(inout) :: sx(*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, m, mp1, nincx
           ! Intrinsic Functions 
           intrinsic :: mod
           if (n<=0 .or. incx<=0) return
           if (incx==1) then
              ! code for increment equal to 1
              ! clean-up loop
              m = mod(n,5)
              if (m/=0) then
                 do i = 1,m
                    sx(i) = sa*sx(i)
                 end do
                 if (n<5) return
              end if
              mp1 = m + 1
              do i = mp1,n,5
                 sx(i) = sa*sx(i)
                 sx(i+1) = sa*sx(i+1)
                 sx(i+2) = sa*sx(i+2)
                 sx(i+3) = sa*sx(i+3)
                 sx(i+4) = sa*sx(i+4)
              end do
           else
              ! code for increment not equal to 1
              nincx = n*incx
              do i = 1,nincx,incx
                 sx(i) = sa*sx(i)
              end do
           end if
           return
     end subroutine stdlib_sscal

     pure module subroutine stdlib_dscal(n,da,dx,incx)
     use stdlib_blas_constants_dp
     !! DSCAL scales a vector by a constant.
     !! uses unrolled loops for increment equal to 1.
        ! -- reference blas level1 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           real(dp), intent(in) :: da
           integer(ilp), intent(in) :: incx, n
           ! Array Arguments 
           real(dp), intent(inout) :: dx(*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, m, mp1, nincx
           ! Intrinsic Functions 
           intrinsic :: mod
           if (n<=0 .or. incx<=0) return
           if (incx==1) then
              ! code for increment equal to 1
              ! clean-up loop
              m = mod(n,5)
              if (m/=0) then
                 do i = 1,m
                    dx(i) = da*dx(i)
                 end do
                 if (n<5) return
              end if
              mp1 = m + 1
              do i = mp1,n,5
                 dx(i) = da*dx(i)
                 dx(i+1) = da*dx(i+1)
                 dx(i+2) = da*dx(i+2)
                 dx(i+3) = da*dx(i+3)
                 dx(i+4) = da*dx(i+4)
              end do
           else
              ! code for increment not equal to 1
              nincx = n*incx
              do i = 1,nincx,incx
                 dx(i) = da*dx(i)
              end do
           end if
           return
     end subroutine stdlib_dscal


     pure module subroutine stdlib_cscal(n,ca,cx,incx)
     use stdlib_blas_constants_sp
     !! CSCAL scales a vector by a constant.
        ! -- reference blas level1 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           complex(sp), intent(in) :: ca
           integer(ilp), intent(in) :: incx, n
           ! Array Arguments 
           complex(sp), intent(inout) :: cx(*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, nincx
           if (n<=0 .or. incx<=0) return
           if (incx==1) then
              ! code for increment equal to 1
              do i = 1,n
                 cx(i) = ca*cx(i)
              end do
           else
              ! code for increment not equal to 1
              nincx = n*incx
              do i = 1,nincx,incx
                 cx(i) = ca*cx(i)
              end do
           end if
           return
     end subroutine stdlib_cscal

     pure module subroutine stdlib_zscal(n,za,zx,incx)
     use stdlib_blas_constants_dp
     !! ZSCAL scales a vector by a constant.
        ! -- reference blas level1 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           complex(dp), intent(in) :: za
           integer(ilp), intent(in) :: incx, n
           ! Array Arguments 
           complex(dp), intent(inout) :: zx(*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, nincx
           if (n<=0 .or. incx<=0) return
           if (incx==1) then
              ! code for increment equal to 1
              do i = 1,n
                 zx(i) = za*zx(i)
              end do
           else
              ! code for increment not equal to 1
              nincx = n*incx
              do i = 1,nincx,incx
                 zx(i) = za*zx(i)
              end do
           end if
           return
     end subroutine stdlib_zscal




     pure module subroutine stdlib_csscal(n,sa,cx,incx)
     use stdlib_blas_constants_sp
     !! CSSCAL scales a complex vector by a real constant.
        ! -- reference blas level1 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           real(sp), intent(in) :: sa
           integer(ilp), intent(in) :: incx, n
           ! Array Arguments 
           complex(sp), intent(inout) :: cx(*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, nincx
           ! Intrinsic Functions 
           intrinsic :: aimag,cmplx,real
           if (n<=0 .or. incx<=0) return
           if (incx==1) then
              ! code for increment equal to 1
              do i = 1,n
                 cx(i) = cmplx(sa*real(cx(i),KIND=sp),sa*aimag(cx(i)),KIND=sp)
              end do
           else
              ! code for increment not equal to 1
              nincx = n*incx
              do i = 1,nincx,incx
                 cx(i) = cmplx(sa*real(cx(i),KIND=sp),sa*aimag(cx(i)),KIND=sp)
              end do
           end if
           return
     end subroutine stdlib_csscal



     pure module subroutine stdlib_zdscal(n,da,zx,incx)
     use stdlib_blas_constants_dp
     !! ZDSCAL scales a vector by a constant.
        ! -- reference blas level1 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           real(dp), intent(in) :: da
           integer(ilp), intent(in) :: incx, n
           ! Array Arguments 
           complex(dp), intent(inout) :: zx(*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, nincx
           ! Intrinsic Functions 
           intrinsic :: cmplx
           if (n<=0 .or. incx<=0) return
           if (incx==1) then
              ! code for increment equal to 1
              do i = 1,n
                 zx(i) = cmplx(da,0.0_dp,KIND=dp)*zx(i)
              end do
           else
              ! code for increment not equal to 1
              nincx = n*incx
              do i = 1,nincx,incx
                 zx(i) = cmplx(da,0.0_dp,KIND=dp)*zx(i)
              end do
           end if
           return
     end subroutine stdlib_zdscal




     pure module subroutine stdlib_sswap(n,sx,incx,sy,incy)
     use stdlib_blas_constants_sp
     !! SSWAP interchanges two vectors.
     !! uses unrolled loops for increments equal to 1.
        ! -- reference blas level1 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx, incy, n
           ! Array Arguments 
           real(sp), intent(inout) :: sx(*), sy(*)
        ! =====================================================================
           ! Local Scalars 
           real(sp) :: stemp
           integer(ilp) :: i, ix, iy, m, mp1
           ! Intrinsic Functions 
           intrinsic :: mod
           if (n<=0) return
           if (incx==1 .and. incy==1) then
             ! code for both increments equal to 1
             ! clean-up loop
              m = mod(n,3)
              if (m/=0) then
                 do i = 1,m
                    stemp = sx(i)
                    sx(i) = sy(i)
                    sy(i) = stemp
                 end do
                 if (n<3) return
              end if
              mp1 = m + 1
              do i = mp1,n,3
                 stemp = sx(i)
                 sx(i) = sy(i)
                 sy(i) = stemp
                 stemp = sx(i+1)
                 sx(i+1) = sy(i+1)
                 sy(i+1) = stemp
                 stemp = sx(i+2)
                 sx(i+2) = sy(i+2)
                 sy(i+2) = stemp
              end do
           else
             ! code for unequal increments or equal increments not equal
               ! to 1
              ix = 1
              iy = 1
              if (incx<0) ix = (-n+1)*incx + 1
              if (incy<0) iy = (-n+1)*incy + 1
              do i = 1,n
                 stemp = sx(ix)
                 sx(ix) = sy(iy)
                 sy(iy) = stemp
                 ix = ix + incx
                 iy = iy + incy
              end do
           end if
           return
     end subroutine stdlib_sswap

     pure module subroutine stdlib_dswap(n,dx,incx,dy,incy)
     use stdlib_blas_constants_dp
     !! DSWAP interchanges two vectors.
     !! uses unrolled loops for increments equal to 1.
        ! -- reference blas level1 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx, incy, n
           ! Array Arguments 
           real(dp), intent(inout) :: dx(*), dy(*)
        ! =====================================================================
           ! Local Scalars 
           real(dp) :: dtemp
           integer(ilp) :: i, ix, iy, m, mp1
           ! Intrinsic Functions 
           intrinsic :: mod
           if (n<=0) return
           if (incx==1 .and. incy==1) then
             ! code for both increments equal to 1
             ! clean-up loop
              m = mod(n,3)
              if (m/=0) then
                 do i = 1,m
                    dtemp = dx(i)
                    dx(i) = dy(i)
                    dy(i) = dtemp
                 end do
                 if (n<3) return
              end if
              mp1 = m + 1
              do i = mp1,n,3
                 dtemp = dx(i)
                 dx(i) = dy(i)
                 dy(i) = dtemp
                 dtemp = dx(i+1)
                 dx(i+1) = dy(i+1)
                 dy(i+1) = dtemp
                 dtemp = dx(i+2)
                 dx(i+2) = dy(i+2)
                 dy(i+2) = dtemp
              end do
           else
             ! code for unequal increments or equal increments not equal
               ! to 1
              ix = 1
              iy = 1
              if (incx<0) ix = (-n+1)*incx + 1
              if (incy<0) iy = (-n+1)*incy + 1
              do i = 1,n
                 dtemp = dx(ix)
                 dx(ix) = dy(iy)
                 dy(iy) = dtemp
                 ix = ix + incx
                 iy = iy + incy
              end do
           end if
           return
     end subroutine stdlib_dswap


     pure module subroutine stdlib_cswap(n,cx,incx,cy,incy)
     use stdlib_blas_constants_sp
     !! CSWAP interchanges two vectors.
        ! -- reference blas level1 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx, incy, n
           ! Array Arguments 
           complex(sp), intent(inout) :: cx(*), cy(*)
        ! =====================================================================
           ! Local Scalars 
           complex(sp) :: ctemp
           integer(ilp) :: i, ix, iy
           if (n<=0) return
           if (incx==1 .and. incy==1) then
             ! code for both increments equal to 1
              do i = 1,n
                 ctemp = cx(i)
                 cx(i) = cy(i)
                 cy(i) = ctemp
              end do
           else
             ! code for unequal increments or equal increments not equal
               ! to 1
              ix = 1
              iy = 1
              if (incx<0) ix = (-n+1)*incx + 1
              if (incy<0) iy = (-n+1)*incy + 1
              do i = 1,n
                 ctemp = cx(ix)
                 cx(ix) = cy(iy)
                 cy(iy) = ctemp
                 ix = ix + incx
                 iy = iy + incy
              end do
           end if
           return
     end subroutine stdlib_cswap

     pure module subroutine stdlib_zswap(n,zx,incx,zy,incy)
     use stdlib_blas_constants_dp
     !! ZSWAP interchanges two vectors.
        ! -- reference blas level1 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx, incy, n
           ! Array Arguments 
           complex(dp), intent(inout) :: zx(*), zy(*)
        ! =====================================================================
           ! Local Scalars 
           complex(dp) :: ztemp
           integer(ilp) :: i, ix, iy
           if (n<=0) return
           if (incx==1 .and. incy==1) then
             ! code for both increments equal to 1
              do i = 1,n
                 ztemp = zx(i)
                 zx(i) = zy(i)
                 zy(i) = ztemp
              end do
           else
             ! code for unequal increments or equal increments not equal
               ! to 1
              ix = 1
              iy = 1
              if (incx<0) ix = (-n+1)*incx + 1
              if (incy<0) iy = (-n+1)*incy + 1
              do i = 1,n
                 ztemp = zx(ix)
                 zx(ix) = zy(iy)
                 zy(iy) = ztemp
                 ix = ix + incx
                 iy = iy + incy
              end do
           end if
           return
     end subroutine stdlib_zswap



end submodule stdlib_blas_level1
