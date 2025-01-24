module stdlib_linalg_blas_aux
     use stdlib_linalg_constants
     implicit none
     private


     public :: sp,dp,qp,lk,ilp
     public :: stdlib_cabs1
     public :: stdlib_lsame
     public :: stdlib_isamax
     public :: stdlib_idamax
     public :: stdlib_icamax
     public :: stdlib_izamax
     public :: stdlib_xerbla
     public :: stdlib_xerbla_array

     interface stdlib_cabs1
        module procedure stdlib_scabs1
        module procedure stdlib_dcabs1
     end interface stdlib_cabs1

     contains


     pure elemental real(sp) function stdlib_scabs1(z)
     !! DCABS1 computes |Re(.)| + |Im(.)| of a double complex number
        ! -- reference blas level1 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           complex(sp), intent(in) :: z
        ! =====================================================================
           ! Intrinsic Functions 
           intrinsic :: abs,real,aimag
           stdlib_scabs1 = abs(real(z,KIND=sp)) + abs(aimag(z))
           return
     end function stdlib_scabs1
     pure elemental real(dp) function stdlib_dcabs1(z)
     !! DCABS1 computes |Re(.)| + |Im(.)| of a double complex number
        ! -- reference blas level1 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           complex(dp), intent(in) :: z
        ! =====================================================================
           ! Intrinsic Functions 
           intrinsic :: abs,real,aimag
           stdlib_dcabs1 = abs(real(z,KIND=dp)) + abs(aimag(z))
           return
     end function stdlib_dcabs1

     pure elemental logical(lk) function stdlib_lsame(ca,cb)
     !! LSAME returns .TRUE. if CA is the same letter as CB regardless of
     !! case.
        ! -- reference blas level1 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character, intent(in) :: ca, cb
       ! =====================================================================
           ! Intrinsic Functions 
           intrinsic :: ichar
           ! Local Scalars 
           integer(ilp) :: inta, intb, zcode
           ! test if the characters are equal
           stdlib_lsame = ca == cb
           if (stdlib_lsame) return
           ! now test for equivalence if both characters are alphabetic.
           zcode = ichar('Z',kind=ilp)
           ! use 'z' rather than 'a' so that ascii can be detected on prime
           ! machines, on which ichar returns a value with bit 8 set.
           ! ichar('a') on prime machines returns 193 which is the same as
           ! ichar('a') on an ebcdic machine.
           inta = ichar(ca,kind=ilp)
           intb = ichar(cb,kind=ilp)
           if (zcode==90 .or. zcode==122) then
              ! ascii is assumed - zcode is the ascii code of either lower or
              ! upper case 'z'.
               if (inta>=97 .and. inta<=122) inta = inta - 32
               if (intb>=97 .and. intb<=122) intb = intb - 32
           else if (zcode==233 .or. zcode==169) then
              ! ebcdic is assumed - zcode is the ebcdic code of either lower or
              ! upper case 'z'.
               if (inta>=129 .and. inta<=137 .or.inta>=145 .and. inta<=153 .or.inta>=162 .and. &
                         inta<=169) inta = inta + 64
               if (intb>=129 .and. intb<=137 .or.intb>=145 .and. intb<=153 .or.intb>=162 .and. &
                         intb<=169) intb = intb + 64
           else if (zcode==218 .or. zcode==250) then
              ! ascii is assumed, on prime machines - zcode is the ascii code
              ! plus 128 of either lower or upper case 'z'.
               if (inta>=225 .and. inta<=250) inta = inta - 32
               if (intb>=225 .and. intb<=250) intb = intb - 32
           end if
           stdlib_lsame = inta == intb
           ! return
     end function stdlib_lsame

     pure subroutine stdlib_xerbla( srname, info )
     !! XERBLA is an error handler for the LAPACK routines.
     !! It is called by an LAPACK routine if an input parameter has an
     !! invalid value.  A message is printed and execution stops.
     !! Installers may consider modifying the STOP statement in order to
     !! call system-specific exception-handling facilities.
        ! -- reference blas level1 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character(len=*), intent(in) :: srname
           integer(ilp), intent(in) :: info
       ! =====================================================================
           ! Intrinsic Functions 
           intrinsic :: len_trim
           ! Executable Statements 
      9999 format( ' ** ON ENTRY TO ', a, ' PARAMETER NUMBER ', i2, ' HAD ','AN ILLEGAL VALUE' )
                
     end subroutine stdlib_xerbla


     pure subroutine stdlib_xerbla_array(srname_array, srname_len, info)
     !! XERBLA_ARRAY assists other languages in calling XERBLA, the LAPACK
     !! and BLAS error handler.  Rather than taking a Fortran string argument
     !! as the function's name, XERBLA_ARRAY takes an array of single
     !! characters along with the array's length.  XERBLA_ARRAY then copies
     !! up to 32 characters of that array into a Fortran string and passes
     !! that to XERBLA.  If called with a non-positive SRNAME_LEN,
     !! XERBLA_ARRAY will call XERBLA with a string of all blank characters.
     !! Say some macro or other device makes XERBLA_ARRAY available to C99
     !! by a name lapack_xerbla and with a common Fortran calling convention.
     !! Then a C99 program could invoke XERBLA via:
     !! {
     !! int flen = strlen(__func__);
     !! lapack_xerbla(__func__,
     !! }
     !! Providing XERBLA_ARRAY is not necessary for intercepting LAPACK
     !! errors.  XERBLA_ARRAY calls XERBLA.
        ! -- reference blas level1 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp), intent(in) :: srname_len, info
           ! Array Arguments 
           character(1), intent(in) :: srname_array(srname_len)
       ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i
           ! Local Arrays 
           character*32 srname
           ! Intrinsic Functions 
           intrinsic :: min,len
           ! Executable Statements 
           srname = ''
           do i = 1, min( srname_len, len( srname ) )
              srname( i:i ) = srname_array( i )
           end do
           call stdlib_xerbla( srname, info )
           return
     end subroutine stdlib_xerbla_array

     pure integer(ilp) function stdlib_isamax(n,dx,incx) result(iamax)
     !! IDAMAX: finds the index of the first element having maximum absolute value.
        ! -- reference blas level1 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx, n
           ! Array Arguments 
           real(sp), intent(in) :: dx(*)
        ! =====================================================================
           ! Local Scalars 
           real(sp) :: dmax
           integer(ilp) :: i, ix
           ! Intrinsic Functions 
           intrinsic :: abs
           iamax = 0
           if (n<1 .or. incx<=0) return
           iamax = 1
           if (n==1) return
           if (incx==1) then
              ! code for increment equal to 1
              dmax = abs(dx(1))
              do i = 2,n
                 if (abs(dx(i))>dmax) then
                    iamax = i
                    dmax = abs(dx(i))
                 end if
              end do
           else
              ! code for increment not equal to 1
              ix = 1
              dmax = abs(dx(1))
              ix = ix + incx
              do i = 2,n
                 if (abs(dx(ix))>dmax) then
                    iamax = i
                    dmax = abs(dx(ix))
                 end if
                 ix = ix + incx
              end do
           end if
           return
     end function stdlib_isamax
     
     pure integer(ilp) function stdlib_idamax(n,dx,incx) result(iamax)
     !! IDAMAX: finds the index of the first element having maximum absolute value.
        ! -- reference blas level1 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx, n
           ! Array Arguments 
           real(dp), intent(in) :: dx(*)
        ! =====================================================================
           ! Local Scalars 
           real(dp) :: dmax
           integer(ilp) :: i, ix
           ! Intrinsic Functions 
           intrinsic :: abs
           iamax = 0
           if (n<1 .or. incx<=0) return
           iamax = 1
           if (n==1) return
           if (incx==1) then
              ! code for increment equal to 1
              dmax = abs(dx(1))
              do i = 2,n
                 if (abs(dx(i))>dmax) then
                    iamax = i
                    dmax = abs(dx(i))
                 end if
              end do
           else
              ! code for increment not equal to 1
              ix = 1
              dmax = abs(dx(1))
              ix = ix + incx
              do i = 2,n
                 if (abs(dx(ix))>dmax) then
                    iamax = i
                    dmax = abs(dx(ix))
                 end if
                 ix = ix + incx
              end do
           end if
           return
     end function stdlib_idamax
     
     pure integer(ilp) function stdlib_icamax(n,zx,incx) result(iamax)
     !! IZAMAX: finds the index of the first element having maximum |Re(.)| + |Im(.)|
        ! -- reference blas level1 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx, n
           ! Array Arguments 
           complex(sp), intent(in) :: zx(*)
        ! =====================================================================
           ! Local Scalars 
           real(sp) :: dmax
           integer(ilp) :: i, ix
           iamax = 0
           if (n<1 .or. incx<=0) return
           iamax = 1
           if (n==1) return
           if (incx==1) then
              ! code for increment equal to 1
              dmax = stdlib_cabs1(zx(1))
              do i = 2,n
                 if (stdlib_cabs1(zx(i))>dmax) then
                    iamax = i
                    dmax = stdlib_cabs1(zx(i))
                 end if
              end do
           else
              ! code for increment not equal to 1
              ix = 1
              dmax = stdlib_cabs1(zx(1))
              ix = ix + incx
              do i = 2,n
                 if (stdlib_cabs1(zx(ix))>dmax) then
                    iamax = i
                    dmax = stdlib_cabs1(zx(ix))
                 end if
                 ix = ix + incx
              end do
           end if
           return
     end function stdlib_icamax
     
     pure integer(ilp) function stdlib_izamax(n,zx,incx) result(iamax)
     !! IZAMAX: finds the index of the first element having maximum |Re(.)| + |Im(.)|
        ! -- reference blas level1 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx, n
           ! Array Arguments 
           complex(dp), intent(in) :: zx(*)
        ! =====================================================================
           ! Local Scalars 
           real(dp) :: dmax
           integer(ilp) :: i, ix
           iamax = 0
           if (n<1 .or. incx<=0) return
           iamax = 1
           if (n==1) return
           if (incx==1) then
              ! code for increment equal to 1
              dmax = stdlib_cabs1(zx(1))
              do i = 2,n
                 if (stdlib_cabs1(zx(i))>dmax) then
                    iamax = i
                    dmax = stdlib_cabs1(zx(i))
                 end if
              end do
           else
              ! code for increment not equal to 1
              ix = 1
              dmax = stdlib_cabs1(zx(1))
              ix = ix + incx
              do i = 2,n
                 if (stdlib_cabs1(zx(ix))>dmax) then
                    iamax = i
                    dmax = stdlib_cabs1(zx(ix))
                 end if
                 ix = ix + incx
              end do
           end if
           return
     end function stdlib_izamax
     

end module stdlib_linalg_blas_aux
