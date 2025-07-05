module stdlib_linalg_lapack_aux
     use stdlib_linalg_constants
     use stdlib_linalg_blas
     use stdlib_linalg_state, only: linalg_state_type, linalg_error_handling, LINALG_ERROR, &
         LINALG_INTERNAL_ERROR, LINALG_VALUE_ERROR, LINALG_SUCCESS
     use ieee_arithmetic, only: ieee_support_inf, ieee_support_nan
     implicit none
     private


     public :: sp,dp,qp,lk,ilp,ilp64
     public :: stdlib_chla_transtype          
     public :: stdlib_ieeeck
     public :: stdlib_iladiag     
     public :: stdlib_ilaenv
     public :: stdlib_ilaenv2stage
     public :: stdlib_ilaprec     
     public :: stdlib_ilatrans
     public :: stdlib_ilauplo
     public :: stdlib_iparam2stage
     public :: stdlib_iparmq     
     public :: stdlib_lsamen
     public :: stdlib_xerbla
     public :: stdlib_xerbla_array     
     public :: stdlib_sroundup_lwork
     public :: stdlib_droundup_lwork
     public :: stdlib_icmax1
     public :: stdlib_izmax1
     public :: stdlib_ilaslc
     public :: stdlib_ilaslr
     public :: stdlib_iladlc
     public :: stdlib_iladlr
     public :: stdlib_ilaclc
     public :: stdlib_ilaclr
     public :: stdlib_ilazlc
     public :: stdlib_ilazlr
     public :: stdlib_I64_chla_transtype          
     public :: stdlib_I64_ieeeck
     public :: stdlib_I64_iladiag     
     public :: stdlib_I64_ilaenv
     public :: stdlib_I64_ilaenv2stage
     public :: stdlib_I64_ilaprec     
     public :: stdlib_I64_ilatrans
     public :: stdlib_I64_ilauplo
     public :: stdlib_I64_iparam2stage
     public :: stdlib_I64_iparmq     
     public :: stdlib_I64_lsamen
     public :: stdlib_I64_xerbla
     public :: stdlib_I64_xerbla_array     
     public :: stdlib_I64_sroundup_lwork
     public :: stdlib_I64_droundup_lwork
     public :: stdlib_I64_icmax1
     public :: stdlib_I64_izmax1
     public :: stdlib_I64_ilaslc
     public :: stdlib_I64_ilaslr
     public :: stdlib_I64_iladlc
     public :: stdlib_I64_iladlr
     public :: stdlib_I64_ilaclc
     public :: stdlib_I64_ilaclr
     public :: stdlib_I64_ilazlc
     public :: stdlib_I64_ilazlr
     public :: stdlib_select_s
     public :: stdlib_selctg_s     
     public :: stdlib_select_d
     public :: stdlib_selctg_d     
     public :: stdlib_select_c
     public :: stdlib_selctg_c     
     public :: stdlib_select_z
     public :: stdlib_selctg_z     
     public :: handle_potrf_info
     public :: handle_getri_info
     public :: handle_gesdd_info
     public :: handle_gesv_info
     public :: handle_gees_info
     public :: handle_geqrf_info
     public :: handle_orgqr_info
     public :: handle_gelsd_info
     public :: handle_geev_info
     public :: handle_ggev_info
     public :: handle_heev_info
     
     ! SELCTG is a LOGICAL FUNCTION of three DOUBLE PRECISION arguments 
     ! used to select eigenvalues to sort to the top left of the Schur form. 
     ! An eigenvalue (ALPHAR(j)+ALPHAI(j))/BETA(j) is selected if SELCTG is true, i.e., 
     abstract interface 
        pure logical(lk) function stdlib_selctg_s(alphar,alphai,beta) 
            import sp,lk 
            implicit none 
            real(sp), intent(in) :: alphar,alphai,beta 
        end function stdlib_selctg_s
        pure logical(lk) function stdlib_select_s(alphar,alphai) 
            import sp,lk
            implicit none 
            real(sp), intent(in) :: alphar,alphai 
        end function stdlib_select_s 
        pure logical(lk) function stdlib_selctg_d(alphar,alphai,beta) 
            import dp,lk 
            implicit none 
            real(dp), intent(in) :: alphar,alphai,beta 
        end function stdlib_selctg_d
        pure logical(lk) function stdlib_select_d(alphar,alphai) 
            import dp,lk
            implicit none 
            real(dp), intent(in) :: alphar,alphai 
        end function stdlib_select_d 
        pure logical(lk) function stdlib_selctg_c(alpha,beta) 
            import sp,lk 
            implicit none 
            complex(sp), intent(in) :: alpha,beta 
        end function stdlib_selctg_c
        pure logical(lk) function stdlib_select_c(alpha) 
            import sp,lk 
            implicit none 
            complex(sp), intent(in) :: alpha 
        end function stdlib_select_c        
        pure logical(lk) function stdlib_selctg_z(alpha,beta) 
            import dp,lk 
            implicit none 
            complex(dp), intent(in) :: alpha,beta 
        end function stdlib_selctg_z
        pure logical(lk) function stdlib_select_z(alpha) 
            import dp,lk 
            implicit none 
            complex(dp), intent(in) :: alpha 
        end function stdlib_select_z        
     end interface 

     contains

     pure character function stdlib_chla_transtype( trans )
     !! This subroutine translates from a BLAST-specified integer constant to
     !! the character string specifying a transposition operation.
     !! CHLA_TRANSTYPE returns an CHARACTER*1.  If CHLA_TRANSTYPE: is 'X',
     !! then input is not an integer indicating a transposition operator.
     !! Otherwise CHLA_TRANSTYPE returns the constant value corresponding to
     !! TRANS.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp), intent(in) :: trans
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: blas_no_trans = 111
           integer(ilp), parameter :: blas_trans = 112
           integer(ilp), parameter :: blas_conj_trans = 113
           
           ! Executable Statements 
           if( trans==blas_no_trans ) then
              stdlib_chla_transtype = 'N'
           else if( trans==blas_trans ) then
              stdlib_chla_transtype = 'T'
           else if( trans==blas_conj_trans ) then
              stdlib_chla_transtype = 'C'
           else
              stdlib_chla_transtype = 'X'
           end if
           return
     end function stdlib_chla_transtype

     pure integer(ilp) function stdlib_ieeeck( ispec, zero, one )
     !! IEEECK is called from the ILAENV to verify that Infinity and
     !! possibly NaN arithmetic is safe (i.e. will not trap).
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp), intent(in) :: ispec
           real(sp), intent(in) :: one, zero
           
        ! =====================================================================
           ! Executable Statements
           stdlib_ieeeck = 1
           
           ! Test support for infinity values
           if (.not.ieee_support_inf(one)) then 
              stdlib_ieeeck = 0
              return
           end if
           
           ! return if we were only asked to check infinity arithmetic
           if (ispec == 0) return
           
           if (.not.ieee_support_nan(one)) then
              stdlib_ieeeck = 0
              return
           end if
           
           return
     end function stdlib_ieeeck




     integer(ilp) function stdlib_iladiag( diag )
     !! This subroutine translated from a character string specifying if a
     !! matrix has unit diagonal or not to the relevant BLAST-specified
     !! integer constant.
     !! ILADIAG returns an INTEGER.  If ILADIAG: < 0, then the input is not a
     !! character indicating a unit or non-unit diagonal.  Otherwise ILADIAG
     !! returns the constant value corresponding to DIAG.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character :: diag
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: blas_non_unit_diag = 131
           integer(ilp), parameter :: blas_unit_diag = 132
           
           ! Executable Statements 
           if( stdlib_lsame( diag, 'N' ) ) then
              stdlib_iladiag = blas_non_unit_diag
           else if( stdlib_lsame( diag, 'U' ) ) then
              stdlib_iladiag = blas_unit_diag
           else
              stdlib_iladiag = -1
           end if
           return
     end function stdlib_iladiag




     integer(ilp) function stdlib_ilaprec( prec )
     !! This subroutine translated from a character string specifying an
     !! intermediate precision to the relevant BLAST-specified integer
     !! constant.
     !! ILAPREC returns an INTEGER.  If ILAPREC: < 0, then the input is not a
     !! character indicating a supported intermediate precision.  Otherwise
     !! ILAPREC returns the constant value corresponding to PREC.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character :: prec
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: blas_prec_single = 211
           integer(ilp), parameter :: blas_prec_double = 212
           integer(ilp), parameter :: blas_prec_indigenous = 213
           integer(ilp), parameter :: blas_prec_extra = 214
           
           ! Executable Statements 
           if( stdlib_lsame( prec, 'S' ) ) then
              stdlib_ilaprec = blas_prec_single
           else if( stdlib_lsame( prec, 'D' ) ) then
              stdlib_ilaprec = blas_prec_double
           else if( stdlib_lsame( prec, 'I' ) ) then
              stdlib_ilaprec = blas_prec_indigenous
           else if( stdlib_lsame( prec, 'X' ) .or. stdlib_lsame( prec, 'E' ) ) then
              stdlib_ilaprec = blas_prec_extra
           else
              stdlib_ilaprec = -1
           end if
           return
     end function stdlib_ilaprec



     integer(ilp) function stdlib_ilatrans( trans )
     !! This subroutine translates from a character string specifying a
     !! transposition operation to the relevant BLAST-specified integer
     !! constant.
     !! ILATRANS returns an INTEGER.  If ILATRANS: < 0, then the input is not
     !! a character indicating a transposition operator.  Otherwise ILATRANS
     !! returns the constant value corresponding to TRANS.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character :: trans
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: blas_no_trans = 111
           integer(ilp), parameter :: blas_trans = 112
           integer(ilp), parameter :: blas_conj_trans = 113
           
           ! Executable Statements 
           if( stdlib_lsame( trans, 'N' ) ) then
              stdlib_ilatrans = blas_no_trans
           else if( stdlib_lsame( trans, 'T' ) ) then
              stdlib_ilatrans = blas_trans
           else if( stdlib_lsame( trans, 'C' ) ) then
              stdlib_ilatrans = blas_conj_trans
           else
              stdlib_ilatrans = -1
           end if
           return
     end function stdlib_ilatrans


     integer(ilp) function stdlib_ilauplo( uplo )
     !! This subroutine translated from a character string specifying a
     !! upper- or lower-triangular matrix to the relevant BLAST-specified
     !! integer constant.
     !! ILAUPLO returns an INTEGER.  If ILAUPLO: < 0, then the input is not
     !! a character indicating an upper- or lower-triangular matrix.
     !! Otherwise ILAUPLO returns the constant value corresponding to UPLO.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character :: uplo
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: blas_upper = 121
           integer(ilp), parameter :: blas_lower = 122
           
           ! Executable Statements 
           if( stdlib_lsame( uplo, 'U' ) ) then
              stdlib_ilauplo = blas_upper
           else if( stdlib_lsame( uplo, 'L' ) ) then
              stdlib_ilauplo = blas_lower
           else
              stdlib_ilauplo = -1
           end if
           return
     end function stdlib_ilauplo


     pure integer(ilp) function stdlib_iparmq( ispec, name, opts, n, ilo, ihi, lwork )
     !! This program sets problem and machine dependent parameters
     !! useful for xHSEQR and related subroutines for eigenvalue
     !! problems. It is called whenever
     !! IPARMQ is called with 12 <= ISPEC <= 16
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp), intent(in) :: ihi, ilo, ispec, lwork, n
           character, intent(in) :: name*(*), opts*(*)
        ! ================================================================
           ! Parameters 
           integer(ilp), parameter :: inmin = 12
           integer(ilp), parameter :: inwin = 13
           integer(ilp), parameter :: inibl = 14
           integer(ilp), parameter :: ishfts = 15
           integer(ilp), parameter :: iacc22 = 16
           integer(ilp), parameter :: icost = 17
           integer(ilp), parameter :: nmin = 75
           integer(ilp), parameter :: k22min = 14
           integer(ilp), parameter :: kacmin = 14
           integer(ilp), parameter :: nibble = 14
           integer(ilp), parameter :: knwswp = 500
           integer(ilp), parameter :: rcost = 10
           real(sp), parameter :: two = 2.0
           
           
           
           ! Local Scalars 
           integer(ilp) :: nh, ns
           integer(ilp) :: i, ic, iz
           character :: subnam*6
           ! Intrinsic Functions 
           intrinsic :: log,max,mod,nint,real
           ! Executable Statements 
           if( ( ispec==ishfts ) .or. ( ispec==inwin ) .or.( ispec==iacc22 ) ) then
              ! ==== set the number simultaneous shifts ====
              nh = ihi - ilo + 1
              ns = 2
              if( nh>=30 )ns = 4
              if( nh>=60 )ns = 10
              if( nh>=150 )ns = max( 10, nh / nint( log( real( nh,KIND=dp) ) / log( two ),&
                        KIND=ilp) )
              if( nh>=590 )ns = 64
              if( nh>=3000 )ns = 128
              if( nh>=6000 )ns = 256
              ns = max( 2, ns-mod( ns, 2 ) )
           end if
           if( ispec==inmin ) then
              ! ===== matrices of order smaller than nmin get sent
              ! .     to xlahqr, the classic double shift algorithm.
              ! .     this must be at least 11. ====
              stdlib_iparmq = nmin
           else if( ispec==inibl ) then
              ! ==== inibl: skip a multi-shift qr iteration and
              ! .    whenever aggressive early deflation finds
              ! .    at least (nibble*(window size)/100) deflations. ====
              stdlib_iparmq = nibble
           else if( ispec==ishfts ) then
              ! ==== nshfts: the number of simultaneous shifts =====
              stdlib_iparmq = ns
           else if( ispec==inwin ) then
              ! ==== nw: deflation window size.  ====
              if( nh<=knwswp ) then
                 stdlib_iparmq = ns
              else
                 stdlib_iparmq = 3*ns / 2
              end if
           else if( ispec==iacc22 ) then
              ! ==== iacc22: whether to accumulate reflections
              ! .     before updating the far-from-diagonal elements
              ! .     and whether to use 2-by-2 block structure while
              ! .     doing it.  a small amount of work could be saved
              ! .     by making this choice dependent also upon the
              ! .     nh=ihi-ilo+1.
              ! convert name to upper case if the first character is lower case.
              stdlib_iparmq = 0
              subnam = name
              ic = ichar( subnam( 1: 1 ) )
              iz = ichar( 'Z' )
              if( iz==90 .or. iz==122 ) then
                 ! ascii character set
                 if( ic>=97 .and. ic<=122 ) then
                    subnam( 1: 1 ) = char( ic-32 )
                    do i = 2, 6
                       ic = ichar( subnam( i: i ) )
                       if( ic>=97 .and. ic<=122 )subnam( i: i ) = char( ic-32 )
                    end do
                 end if
              else if( iz==233 .or. iz==169 ) then
                 ! ebcdic character set
                 if( ( ic>=129 .and. ic<=137 ) .or.( ic>=145 .and. ic<=153 ) .or.( ic>=162 .and. &
                           ic<=169 ) ) then
                    subnam( 1: 1 ) = char( ic+64 )
                    do i = 2, 6
                       ic = ichar( subnam( i: i ) )
                       if( ( ic>=129 .and. ic<=137 ) .or.( ic>=145 .and. ic<=153 ) .or.( ic>=162 &
                                 .and. ic<=169 ) )subnam( i:i ) = char( ic+64 )
                    end do
                 end if
              else if( iz==218 .or. iz==250 ) then
                 ! prime machines:  ascii+128
                 if( ic>=225 .and. ic<=250 ) then
                    subnam( 1: 1 ) = char( ic-32 )
                    do i = 2, 6
                       ic = ichar( subnam( i: i ) )
                       if( ic>=225 .and. ic<=250 )subnam( i: i ) = char( ic-32 )
                    end do
                 end if
              end if
              if( subnam( 2:6 )=='GGHRD' .or.subnam( 2:6 )=='GGHD3' ) then
                 stdlib_iparmq = 1
                 if( nh>=k22min )stdlib_iparmq = 2
              else if ( subnam( 4:6 )=='EXC' ) then
                 if( nh>=kacmin )stdlib_iparmq = 1
                 if( nh>=k22min )stdlib_iparmq = 2
              else if ( subnam( 2:6 )=='HSEQR' .or.subnam( 2:5 )=='LAQR' ) then
                 if( ns>=kacmin )stdlib_iparmq = 1
                 if( ns>=k22min )stdlib_iparmq = 2
              end if
           else if( ispec==icost ) then
              ! === relative cost of near-the-diagonal chase vs
                  ! blas updates ===
              stdlib_iparmq = rcost
           else
              ! ===== invalid value of ispec =====
              stdlib_iparmq = -1
           end if
     end function stdlib_iparmq

     pure logical(lk) function stdlib_lsamen( n, ca, cb )
     !! LSAMEN tests if the first N letters of CA are the same as the
     !! first N letters of CB, regardless of case.
     !! LSAMEN returns .TRUE. if CA and CB are equivalent except for case
     !! and .FALSE. otherwise.  LSAMEN also returns .FALSE. if LEN( CA )
     !! or LEN( CB ) is less than N.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character(len=*), intent(in) :: ca, cb
           integer(ilp), intent(in) :: n
       ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i
           ! Intrinsic Functions 
           intrinsic :: len
           ! Executable Statements 
           stdlib_lsamen = .false.
           if( len( ca )<n .or. len( cb )<n )go to 20
           ! do for each character in the two strings.
           do i = 1, n
              ! test if the characters are equal using stdlib_lsame.
              if( .not.stdlib_lsame( ca( i: i ), cb( i: i ) ) )go to 20
           end do
           stdlib_lsamen = .true.
           20 continue
           return
     end function stdlib_lsamen

     pure real(sp) function stdlib_sroundup_lwork( lwork )
     !! ROUNDUP_LWORK >= LWORK.
     !! ROUNDUP_LWORK is guaranteed to have zero decimal part.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp), intent(in) :: lwork
       ! =====================================================================
           ! Intrinsic Functions 
           intrinsic :: epsilon,real,int
           ! Executable Statements 
           stdlib_sroundup_lwork = real(lwork,KIND=sp)
           if (int( stdlib_sroundup_lwork,KIND=ilp)<lwork) then
               ! force round up of lwork
               stdlib_sroundup_lwork = stdlib_sroundup_lwork * (1.0e+0_sp + epsilon(0.0e+0_sp))
           endif
           return
     end function stdlib_sroundup_lwork
     
     pure real(dp) function stdlib_droundup_lwork( lwork )
     !! ROUNDUP_LWORK >= LWORK.
     !! ROUNDUP_LWORK is guaranteed to have zero decimal part.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp), intent(in) :: lwork
       ! =====================================================================
           ! Intrinsic Functions 
           intrinsic :: epsilon,real,int
           ! Executable Statements 
           stdlib_droundup_lwork = real(lwork,KIND=dp)
           if (int( stdlib_droundup_lwork,KIND=ilp)<lwork) then
               ! force round up of lwork
               stdlib_droundup_lwork = stdlib_droundup_lwork * (1.0e+0_dp + epsilon(0.0e+0_dp))
           endif
           return
     end function stdlib_droundup_lwork
     

     pure integer(ilp) function stdlib_ilaslc( m, n, a, lda )
     !! ILADLC: scans A for its last non-zero column.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp), intent(in) :: m, n, lda
           ! Array Arguments 
           real(sp), intent(in) :: a(lda,*)
        ! =====================================================================
           ! Parameters 
           real(sp), parameter :: zero = 0.0_sp
           
           ! Local Scalars 
           integer(ilp) :: i
           ! Executable Statements 
           ! quick test for the common case where one corner is non-zero.
           if( n==0 ) then
              stdlib_ilaslc = n
           else if (a(1, n)/=zero .or. a(m, n)/=zero) then
              stdlib_ilaslc = n
           else
           ! now scan each column from the end, returning with the first non-zero.
              do stdlib_ilaslc = n, 1, -1
                 do i = 1, m
                    if (a(i, stdlib_ilaslc)/=zero) return
                 end do
              end do
           end if
           return
     end function stdlib_ilaslc
     
     pure integer(ilp) function stdlib_ilaslr( m, n, a, lda )
     !! ILADLR: scans A for its last non-zero row.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp), intent(in) :: m, n, lda
           ! Array Arguments 
           real(sp), intent(in) :: a(lda,*)
        ! =====================================================================
           ! Parameters 
           real(sp), parameter :: zero = 0.0_sp
           
           ! Local Scalars 
           integer(ilp) :: i, j
           ! Executable Statements 
           ! quick test for the common case where one corner is non-zero.
           if( m==0 ) then
              stdlib_ilaslr = m
           else if( a(m, 1)/=zero .or. a(m, n)/=zero ) then
              stdlib_ilaslr = m
           else
           ! scan up each column tracking the last zero row seen.
              stdlib_ilaslr = 0
              do j = 1, n
                 i=m
                 do while((a(max(i,1),j)==zero).and.(i>=1))
                    i=i-1
                 enddo
                 stdlib_ilaslr = max( stdlib_ilaslr, i )
              end do
           end if
           return
     end function stdlib_ilaslr     
     
     pure integer(ilp) function stdlib_iladlc( m, n, a, lda )
     !! ILADLC: scans A for its last non-zero column.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp), intent(in) :: m, n, lda
           ! Array Arguments 
           real(dp), intent(in) :: a(lda,*)
        ! =====================================================================
           ! Parameters 
           real(dp), parameter :: zero = 0.0_dp
           
           ! Local Scalars 
           integer(ilp) :: i
           ! Executable Statements 
           ! quick test for the common case where one corner is non-zero.
           if( n==0 ) then
              stdlib_iladlc = n
           else if (a(1, n)/=zero .or. a(m, n)/=zero) then
              stdlib_iladlc = n
           else
           ! now scan each column from the end, returning with the first non-zero.
              do stdlib_iladlc = n, 1, -1
                 do i = 1, m
                    if (a(i, stdlib_iladlc)/=zero) return
                 end do
              end do
           end if
           return
     end function stdlib_iladlc
     
     pure integer(ilp) function stdlib_iladlr( m, n, a, lda )
     !! ILADLR: scans A for its last non-zero row.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp), intent(in) :: m, n, lda
           ! Array Arguments 
           real(dp), intent(in) :: a(lda,*)
        ! =====================================================================
           ! Parameters 
           real(dp), parameter :: zero = 0.0_dp
           
           ! Local Scalars 
           integer(ilp) :: i, j
           ! Executable Statements 
           ! quick test for the common case where one corner is non-zero.
           if( m==0 ) then
              stdlib_iladlr = m
           else if( a(m, 1)/=zero .or. a(m, n)/=zero ) then
              stdlib_iladlr = m
           else
           ! scan up each column tracking the last zero row seen.
              stdlib_iladlr = 0
              do j = 1, n
                 i=m
                 do while((a(max(i,1),j)==zero).and.(i>=1))
                    i=i-1
                 enddo
                 stdlib_iladlr = max( stdlib_iladlr, i )
              end do
           end if
           return
     end function stdlib_iladlr     
     
     pure integer(ilp) function stdlib_ilaclc( m, n, a, lda )
     !! ILADLC: scans A for its last non-zero column.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp), intent(in) :: m, n, lda
           ! Array Arguments 
           complex(sp), intent(in) :: a(lda,*)
        ! =====================================================================
           ! Parameters 
           complex(sp), parameter :: zero = 0.0_sp
           
           ! Local Scalars 
           integer(ilp) :: i
           ! Executable Statements 
           ! quick test for the common case where one corner is non-zero.
           if( n==0 ) then
              stdlib_ilaclc = n
           else if (a(1, n)/=zero .or. a(m, n)/=zero) then
              stdlib_ilaclc = n
           else
           ! now scan each column from the end, returning with the first non-zero.
              do stdlib_ilaclc = n, 1, -1
                 do i = 1, m
                    if (a(i, stdlib_ilaclc)/=zero) return
                 end do
              end do
           end if
           return
     end function stdlib_ilaclc
     
     pure integer(ilp) function stdlib_ilaclr( m, n, a, lda )
     !! ILADLR: scans A for its last non-zero row.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp), intent(in) :: m, n, lda
           ! Array Arguments 
           complex(sp), intent(in) :: a(lda,*)
        ! =====================================================================
           ! Parameters 
           complex(sp), parameter :: zero = 0.0_sp
           
           ! Local Scalars 
           integer(ilp) :: i, j
           ! Executable Statements 
           ! quick test for the common case where one corner is non-zero.
           if( m==0 ) then
              stdlib_ilaclr = m
           else if( a(m, 1)/=zero .or. a(m, n)/=zero ) then
              stdlib_ilaclr = m
           else
           ! scan up each column tracking the last zero row seen.
              stdlib_ilaclr = 0
              do j = 1, n
                 i=m
                 do while((a(max(i,1),j)==zero).and.(i>=1))
                    i=i-1
                 enddo
                 stdlib_ilaclr = max( stdlib_ilaclr, i )
              end do
           end if
           return
     end function stdlib_ilaclr     
     
     pure integer(ilp) function stdlib_ilazlc( m, n, a, lda )
     !! ILADLC: scans A for its last non-zero column.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp), intent(in) :: m, n, lda
           ! Array Arguments 
           complex(dp), intent(in) :: a(lda,*)
        ! =====================================================================
           ! Parameters 
           complex(dp), parameter :: zero = 0.0_dp
           
           ! Local Scalars 
           integer(ilp) :: i
           ! Executable Statements 
           ! quick test for the common case where one corner is non-zero.
           if( n==0 ) then
              stdlib_ilazlc = n
           else if (a(1, n)/=zero .or. a(m, n)/=zero) then
              stdlib_ilazlc = n
           else
           ! now scan each column from the end, returning with the first non-zero.
              do stdlib_ilazlc = n, 1, -1
                 do i = 1, m
                    if (a(i, stdlib_ilazlc)/=zero) return
                 end do
              end do
           end if
           return
     end function stdlib_ilazlc
     
     pure integer(ilp) function stdlib_ilazlr( m, n, a, lda )
     !! ILADLR: scans A for its last non-zero row.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp), intent(in) :: m, n, lda
           ! Array Arguments 
           complex(dp), intent(in) :: a(lda,*)
        ! =====================================================================
           ! Parameters 
           complex(dp), parameter :: zero = 0.0_dp
           
           ! Local Scalars 
           integer(ilp) :: i, j
           ! Executable Statements 
           ! quick test for the common case where one corner is non-zero.
           if( m==0 ) then
              stdlib_ilazlr = m
           else if( a(m, 1)/=zero .or. a(m, n)/=zero ) then
              stdlib_ilazlr = m
           else
           ! scan up each column tracking the last zero row seen.
              stdlib_ilazlr = 0
              do j = 1, n
                 i=m
                 do while((a(max(i,1),j)==zero).and.(i>=1))
                    i=i-1
                 enddo
                 stdlib_ilazlr = max( stdlib_ilazlr, i )
              end do
           end if
           return
     end function stdlib_ilazlr     
     

     pure integer(ilp) function stdlib_icmax1( n, zx, incx )
     !! I*MAX1: finds the index of the first vector element of maximum absolute value.
     !! Based on I*AMAX from Level 1 BLAS.
     !! The change is to use the 'genuine' absolute value.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx, n
           ! Array Arguments 
           complex(sp), intent(in) :: zx(*)
        ! =====================================================================
           ! Local Scalars 
           real(sp) :: dmax
           integer(ilp) :: i, ix
           ! Intrinsic Functions 
           intrinsic :: abs
           ! Executable Statements 
           stdlib_icmax1 = 0
           if (n<1 .or. incx<=0) return
           stdlib_icmax1 = 1
           if (n==1) return
           if (incx==1) then
              ! code for increment equal to 1
              dmax = abs(zx(1))
              do i = 2,n
                 if (abs(zx(i))>dmax) then
                    stdlib_icmax1 = i
                    dmax = abs(zx(i))
                 end if
              end do
           else
              ! code for increment not equal to 1
              ix = 1
              dmax = abs(zx(1))
              ix = ix + incx
              do i = 2,n
                 if (abs(zx(ix))>dmax) then
                    stdlib_icmax1 = i
                    dmax = abs(zx(ix))
                 end if
                 ix = ix + incx
              end do
           end if
           return
     end function stdlib_icmax1
     pure integer(ilp) function stdlib_izmax1( n, zx, incx )
     !! I*MAX1: finds the index of the first vector element of maximum absolute value.
     !! Based on I*AMAX from Level 1 BLAS.
     !! The change is to use the 'genuine' absolute value.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx, n
           ! Array Arguments 
           complex(dp), intent(in) :: zx(*)
        ! =====================================================================
           ! Local Scalars 
           real(dp) :: dmax
           integer(ilp) :: i, ix
           ! Intrinsic Functions 
           intrinsic :: abs
           ! Executable Statements 
           stdlib_izmax1 = 0
           if (n<1 .or. incx<=0) return
           stdlib_izmax1 = 1
           if (n==1) return
           if (incx==1) then
              ! code for increment equal to 1
              dmax = abs(zx(1))
              do i = 2,n
                 if (abs(zx(i))>dmax) then
                    stdlib_izmax1 = i
                    dmax = abs(zx(i))
                 end if
              end do
           else
              ! code for increment not equal to 1
              ix = 1
              dmax = abs(zx(1))
              ix = ix + incx
              do i = 2,n
                 if (abs(zx(ix))>dmax) then
                    stdlib_izmax1 = i
                    dmax = abs(zx(ix))
                 end if
                 ix = ix + incx
              end do
           end if
           return
     end function stdlib_izmax1


     pure integer(ilp) function stdlib_ilaenv( ispec, name, opts, n1, n2, n3, n4 )
     !! ILAENV is called from the LAPACK routines to choose problem-dependent
     !! parameters for the local environment.  See ISPEC for a description of
     !! the parameters.
     !! ILAENV returns an INTEGER
     !! if ILAENV >= 0: ILAENV returns the value of the parameter specified by ISPEC
     !! if ILAENV < 0:  if ILAENV = -k, the k-th argument had an illegal value.
     !! This version provides a set of parameters which should give good,
     !! but not optimal, performance on many of the currently available
     !! computers.  Users are encouraged to modify this subroutine to set
     !! the tuning parameters for their particular machine using the option
     !! and problem size information in the arguments.
     !! This routine will not function correctly if it is converted to all
     !! lower case.  Converting it to all upper case is allowed.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character(len=*), intent(in) :: name, opts
           integer(ilp), intent(in) :: ispec, n1, n2, n3, n4
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, ic, iz, nb, nbmin, nx
           logical(lk) :: cname, sname, twostage
           character :: c1*1, c2*2, c4*2, c3*3, subnam*16
           ! Intrinsic Functions 
           intrinsic :: char,ichar,int,min,real
           ! Executable Statements 
           go to ( 10, 10, 10, 80, 90, 100, 110, 120,130, 140, 150, 160, 160, 160, 160, 160, 160)&
                     ispec
           ! invalid value for ispec
           stdlib_ilaenv = -1
           return
           10 continue
           ! convert name to upper case if the first character is lower case.
           stdlib_ilaenv = 1
           subnam = name
           ic = ichar( subnam( 1: 1 ) )
           iz = ichar( 'Z' )
           if( iz==90 .or. iz==122 ) then
              ! ascii character set
              if( ic>=97 .and. ic<=122 ) then
                 subnam( 1: 1 ) = char( ic-32 )
                 do i = 2, 6
                    ic = ichar( subnam( i: i ) )
                    if( ic>=97 .and. ic<=122 )subnam( i: i ) = char( ic-32 )
                 end do
              end if
           else if( iz==233 .or. iz==169 ) then
              ! ebcdic character set
              if( ( ic>=129 .and. ic<=137 ) .or.( ic>=145 .and. ic<=153 ) .or.( ic>=162 .and. &
                        ic<=169 ) ) then
                 subnam( 1: 1 ) = char( ic+64 )
                 do i = 2, 6
                    ic = ichar( subnam( i: i ) )
                    if( ( ic>=129 .and. ic<=137 ) .or.( ic>=145 .and. ic<=153 ) .or.( ic>=162 &
                              .and. ic<=169 ) )subnam( i:i ) = char( ic+64 )
                 end do
              end if
           else if( iz==218 .or. iz==250 ) then
              ! prime machines:  ascii+128
              if( ic>=225 .and. ic<=250 ) then
                 subnam( 1: 1 ) = char( ic-32 )
                 do i = 2, 6
                    ic = ichar( subnam( i: i ) )
                    if( ic>=225 .and. ic<=250 )subnam( i: i ) = char( ic-32 )
                 end do
              end if
           end if
           c1 = subnam( 1: 1 )
           sname = c1=='S' .or. c1=='D'
           cname = c1=='C' .or. c1=='Z'
           if( .not.( cname .or. sname ) )return
           c2 = subnam( 2: 3 )
           c3 = subnam( 4: 6 )
           c4 = c3( 2: 3 )
           twostage = len( subnam )>=11.and. subnam( 11: 11 )=='2'
           go to ( 50, 60, 70 )ispec
           50 continue
           ! ispec = 1:  block size
           ! in these examples, separate code is provided for setting nb for
           ! real and complex.  we assume that nb will take the same value in
           ! single or double precision.
           nb = 1
           if( subnam(2:6)=='LAORH' ) then
              ! this is for *laorhr_getrfnp routine
              if( sname ) then
                  nb = 32
              else
                  nb = 32
              end if
           else if( c2=='GE' ) then
              if( c3=='TRF' ) then
                 if( sname ) then
                    nb = 64
                 else
                    nb = 64
                 end if
              else if( c3=='QRF' .or. c3=='RQF' .or. c3=='LQF' .or.c3=='QLF' ) then
                 if( sname ) then
                    nb = 32
                 else
                    nb = 32
                 end if
              else if( c3=='QR ') then
                 if( n3 == 1) then
                    if( sname ) then
           ! m*n
                       if ((n1*n2<=131072).or.(n1<=8192)) then
                          nb = n1
                       else
                          nb = 32768/n2
                       end if
                    else
                       if ((n1*n2<=131072).or.(n1<=8192)) then
                          nb = n1
                       else
                          nb = 32768/n2
                       end if
                    end if
                 else
                    if( sname ) then
                       nb = 1
                    else
                       nb = 1
                    end if
                 end if
              else if( c3=='LQ ') then
                 if( n3 == 2) then
                    if( sname ) then
           ! m*n
                       if ((n1*n2<=131072).or.(n1<=8192)) then
                          nb = n1
                       else
                          nb = 32768/n2
                       end if
                    else
                       if ((n1*n2<=131072).or.(n1<=8192)) then
                          nb = n1
                       else
                          nb = 32768/n2
                       end if
                    end if
                 else
                    if( sname ) then
                       nb = 1
                    else
                       nb = 1
                    end if
                 end if
              else if( c3=='HRD' ) then
                 if( sname ) then
                    nb = 32
                 else
                    nb = 32
                 end if
              else if( c3=='BRD' ) then
                 if( sname ) then
                    nb = 32
                 else
                    nb = 32
                 end if
              else if( c3=='TRI' ) then
                 if( sname ) then
                    nb = 64
                 else
                    nb = 64
                 end if
              end if
           else if( c2=='PO' ) then
              if( c3=='TRF' ) then
                 if( sname ) then
                    nb = 64
                 else
                    nb = 64
                 end if
              end if
           else if( c2=='SY' ) then
              if( c3=='TRF' ) then
                 if( sname ) then
                    if( twostage ) then
                       nb = 192
                    else
                       nb = 64
                    end if
                 else
                    if( twostage ) then
                       nb = 192
                    else
                       nb = 64
                    end if
                 end if
              else if( sname .and. c3=='TRD' ) then
                 nb = 32
              else if( sname .and. c3=='GST' ) then
                 nb = 64
              end if
           else if( cname .and. c2=='HE' ) then
              if( c3=='TRF' ) then
                 if( twostage ) then
                    nb = 192
                 else
                    nb = 64
                 end if
              else if( c3=='TRD' ) then
                 nb = 32
              else if( c3=='GST' ) then
                 nb = 64
              end if
           else if( sname .and. c2=='OR' ) then
              if( c3( 1: 1 )=='G' ) then
                 if( c4=='QR' .or. c4=='RQ' .or. c4=='LQ' .or. c4=='QL' .or. c4=='HR' .or. &
                           c4=='TR' .or. c4=='BR' )then
                    nb = 32
                 end if
              else if( c3( 1: 1 )=='M' ) then
                 if( c4=='QR' .or. c4=='RQ' .or. c4=='LQ' .or. c4=='QL' .or. c4=='HR' .or. &
                           c4=='TR' .or. c4=='BR' )then
                    nb = 32
                 end if
              end if
           else if( cname .and. c2=='UN' ) then
              if( c3( 1: 1 )=='G' ) then
                 if( c4=='QR' .or. c4=='RQ' .or. c4=='LQ' .or. c4=='QL' .or. c4=='HR' .or. &
                           c4=='TR' .or. c4=='BR' )then
                    nb = 32
                 end if
              else if( c3( 1: 1 )=='M' ) then
                 if( c4=='QR' .or. c4=='RQ' .or. c4=='LQ' .or. c4=='QL' .or. c4=='HR' .or. &
                           c4=='TR' .or. c4=='BR' )then
                    nb = 32
                 end if
              end if
           else if( c2=='GB' ) then
              if( c3=='TRF' ) then
                 if( sname ) then
                    if( n4<=64 ) then
                       nb = 1
                    else
                       nb = 32
                    end if
                 else
                    if( n4<=64 ) then
                       nb = 1
                    else
                       nb = 32
                    end if
                 end if
              end if
           else if( c2=='PB' ) then
              if( c3=='TRF' ) then
                 if( sname ) then
                    if( n2<=64 ) then
                       nb = 1
                    else
                       nb = 32
                    end if
                 else
                    if( n2<=64 ) then
                       nb = 1
                    else
                       nb = 32
                    end if
                 end if
              end if
           else if( c2=='TR' ) then
              if( c3=='TRI' ) then
                 if( sname ) then
                    nb = 64
                 else
                    nb = 64
                 end if
              else if ( c3=='EVC' ) then
                 if( sname ) then
                    nb = 64
                 else
                    nb = 64
                 end if
              end if
           else if( c2=='LA' ) then
              if( c3=='UUM' ) then
                 if( sname ) then
                    nb = 64
                 else
                    nb = 64
                 end if
              end if
           else if( sname .and. c2=='ST' ) then
              if( c3=='EBZ' ) then
                 nb = 1
              end if
           else if( c2=='GG' ) then
              nb = 32
              if( c3=='HD3' ) then
                 if( sname ) then
                    nb = 32
                 else
                    nb = 32
                 end if
              end if
           end if
           stdlib_ilaenv = nb
           return
           60 continue
           ! ispec = 2:  minimum block size
           nbmin = 2
           if( c2=='GE' ) then
              if( c3=='QRF' .or. c3=='RQF' .or. c3=='LQF' .or. c3=='QLF' ) then
                 if( sname ) then
                    nbmin = 2
                 else
                    nbmin = 2
                 end if
              else if( c3=='HRD' ) then
                 if( sname ) then
                    nbmin = 2
                 else
                    nbmin = 2
                 end if
              else if( c3=='BRD' ) then
                 if( sname ) then
                    nbmin = 2
                 else
                    nbmin = 2
                 end if
              else if( c3=='TRI' ) then
                 if( sname ) then
                    nbmin = 2
                 else
                    nbmin = 2
                 end if
              end if
           else if( c2=='SY' ) then
              if( c3=='TRF' ) then
                 if( sname ) then
                    nbmin = 8
                 else
                    nbmin = 8
                 end if
              else if( sname .and. c3=='TRD' ) then
                 nbmin = 2
              end if
           else if( cname .and. c2=='HE' ) then
              if( c3=='TRD' ) then
                 nbmin = 2
              end if
           else if( sname .and. c2=='OR' ) then
              if( c3( 1: 1 )=='G' ) then
                 if( c4=='QR' .or. c4=='RQ' .or. c4=='LQ' .or. c4=='QL' .or. c4=='HR' .or. &
                           c4=='TR' .or. c4=='BR' )then
                    nbmin = 2
                 end if
              else if( c3( 1: 1 )=='M' ) then
                 if( c4=='QR' .or. c4=='RQ' .or. c4=='LQ' .or. c4=='QL' .or. c4=='HR' .or. &
                           c4=='TR' .or. c4=='BR' )then
                    nbmin = 2
                 end if
              end if
           else if( cname .and. c2=='UN' ) then
              if( c3( 1: 1 )=='G' ) then
                 if( c4=='QR' .or. c4=='RQ' .or. c4=='LQ' .or. c4=='QL' .or. c4=='HR' .or. &
                           c4=='TR' .or. c4=='BR' )then
                    nbmin = 2
                 end if
              else if( c3( 1: 1 )=='M' ) then
                 if( c4=='QR' .or. c4=='RQ' .or. c4=='LQ' .or. c4=='QL' .or. c4=='HR' .or. &
                           c4=='TR' .or. c4=='BR' )then
                    nbmin = 2
                 end if
              end if
           else if( c2=='GG' ) then
              nbmin = 2
              if( c3=='HD3' ) then
                 nbmin = 2
              end if
           end if
           stdlib_ilaenv = nbmin
           return
           70 continue
           ! ispec = 3:  crossover point
           nx = 0
           if( c2=='GE' ) then
              if( c3=='QRF' .or. c3=='RQF' .or. c3=='LQF' .or. c3=='QLF' ) then
                 if( sname ) then
                    nx = 128
                 else
                    nx = 128
                 end if
              else if( c3=='HRD' ) then
                 if( sname ) then
                    nx = 128
                 else
                    nx = 128
                 end if
              else if( c3=='BRD' ) then
                 if( sname ) then
                    nx = 128
                 else
                    nx = 128
                 end if
              end if
           else if( c2=='SY' ) then
              if( sname .and. c3=='TRD' ) then
                 nx = 32
              end if
           else if( cname .and. c2=='HE' ) then
              if( c3=='TRD' ) then
                 nx = 32
              end if
           else if( sname .and. c2=='OR' ) then
              if( c3( 1: 1 )=='G' ) then
                 if( c4=='QR' .or. c4=='RQ' .or. c4=='LQ' .or. c4=='QL' .or. c4=='HR' .or. &
                           c4=='TR' .or. c4=='BR' )then
                    nx = 128
                 end if
              end if
           else if( cname .and. c2=='UN' ) then
              if( c3( 1: 1 )=='G' ) then
                 if( c4=='QR' .or. c4=='RQ' .or. c4=='LQ' .or. c4=='QL' .or. c4=='HR' .or. &
                           c4=='TR' .or. c4=='BR' )then
                    nx = 128
                 end if
              end if
           else if( c2=='GG' ) then
              nx = 128
              if( c3=='HD3' ) then
                 nx = 128
              end if
           end if
           stdlib_ilaenv = nx
           return
           80 continue
           ! ispec = 4:  number of shifts (used by xhseqr)
           stdlib_ilaenv = 6
           return
           90 continue
           ! ispec = 5:  minimum column dimension (not used)
           stdlib_ilaenv = 2
           return
           100 continue
           ! ispec = 6:  crossover point for svd (used by xgelss and xgesvd)
           stdlib_ilaenv = int( real( min( n1, n2 ),KIND=dp)*1.6e0,KIND=ilp)
           return
           110 continue
           ! ispec = 7:  number of processors (not used)
           stdlib_ilaenv = 1
           return
           120 continue
           ! ispec = 8:  crossover point for multishift (used by xhseqr)
           stdlib_ilaenv = 50
           return
           130 continue
           ! ispec = 9:  maximum size of the subproblems at the bottom of the
                       ! computation tree in the divide-and-conquer algorithm
                       ! (used by xgelsd and xgesdd)
           stdlib_ilaenv = 25
           return
           140 continue
           ! ispec = 10: ieee and infinity nan arithmetic can be trusted not to trap
           ! stdlib_ilaenv = 0
           stdlib_ilaenv = 1
           if( stdlib_ilaenv==1 ) then
              stdlib_ilaenv = stdlib_ieeeck( 1_ilp, 0.0, 1.0 )
           end if
           return
           150 continue
           ! ispec = 11: ieee infinity arithmetic can be trusted not to trap
           ! stdlib_ilaenv = 0
           stdlib_ilaenv = 1
           if( stdlib_ilaenv==1 ) then
              stdlib_ilaenv = stdlib_ieeeck( 0_ilp, 0.0, 1.0 )
           end if
           return
           160 continue
           ! 12 <= ispec <= 17: xhseqr or related subroutines.
           stdlib_ilaenv = stdlib_iparmq( ispec, name, opts, n1, n2, n3, n4 )
           return
     end function stdlib_ilaenv


     integer(ilp) function stdlib_iparam2stage( ispec, name, opts,ni, nbi, ibi, nxi )
     !! This program sets problem and machine dependent parameters
     !! useful for xHETRD_2STAGE, xHETRD_HE2HB, xHETRD_HB2ST,
     !! xGEBRD_2STAGE, xGEBRD_GE2GB, xGEBRD_GB2BD
     !! and related subroutines for eigenvalue problems.
     !! It is called whenever ILAENV is called with 17 <= ISPEC <= 21.
     !! It is called whenever ILAENV2STAGE is called with 1 <= ISPEC <= 5
     !! with a direct conversion ISPEC + 16.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character(len=*), intent(in) :: name, opts
           integer(ilp), intent(in) :: ispec, ni, nbi, ibi, nxi
        ! ================================================================
           ! Local Scalars 
           integer(ilp) :: i, ic, iz, kd, ib, lhous, lwork, nthreads, factoptnb, qroptnb, &
                     lqoptnb
           logical(lk) :: rprec, cprec
           character :: prec*1, algo*3, stag*5, subnam*12, vect*1
           ! Intrinsic Functions 
           intrinsic :: char,ichar,max
           ! Executable Statements 
           ! invalid value for ispec
           if( (ispec<17).or.(ispec>21) ) then
               stdlib_iparam2stage = -1
               return
           endif
           ! get the number of threads
           nthreads = 1
           !$ nthreads = omp_get_num_threads()

           ! write(*,*) 'iparam voici nthreads ispec ',nthreads, ispec
           if( ispec /= 19 ) then
              ! convert name to upper case if the first character is lower case.
              stdlib_iparam2stage = -1
              subnam = name
              ic = ichar( subnam( 1: 1 ) )
              iz = ichar( 'Z' )
              if( iz==90 .or. iz==122 ) then
                 ! ascii character set
                 if( ic>=97 .and. ic<=122 ) then
                    subnam( 1: 1 ) = char( ic-32 )
                    do i = 2, 12
                       ic = ichar( subnam( i: i ) )
                       if( ic>=97 .and. ic<=122 )subnam( i: i ) = char( ic-32 )
                    end do
                 end if
              else if( iz==233 .or. iz==169 ) then
                 ! ebcdic character set
                 if( ( ic>=129 .and. ic<=137 ) .or.( ic>=145 .and. ic<=153 ) .or.( ic>=162 .and. &
                           ic<=169 ) ) then
                    subnam( 1: 1 ) = char( ic+64 )
                    do i = 2, 12
                       ic = ichar( subnam( i: i ) )
                       if( ( ic>=129 .and. ic<=137 ) .or.( ic>=145 .and. ic<=153 ) .or.( ic>=162 &
                                 .and. ic<=169 ) )subnam( i:i ) = char( ic+64 )
                    end do
                 end if
              else if( iz==218 .or. iz==250 ) then
                 ! prime machines:  ascii+128
                 if( ic>=225 .and. ic<=250 ) then
                    subnam( 1: 1 ) = char( ic-32 )
                    do i = 2, 12
                      ic = ichar( subnam( i: i ) )
                      if( ic>=225 .and. ic<=250 )subnam( i: i ) = char( ic-32 )
                    end do
                 end if
              end if
              prec  = subnam( 1: 1 )
              algo  = subnam( 4: 6 )
              stag  = subnam( 8:12 )
              rprec = prec=='S' .or. prec=='D'
              cprec = prec=='C' .or. prec=='Z'
              ! invalid value for precision
              if( .not.( rprec .or. cprec ) ) then
                  stdlib_iparam2stage = -1
                  return
              endif
           endif
            ! write(*,*),'rprec,cprec ',rprec,cprec,
           ! $           '   algo ',algo,'    stage ',stag
           if (( ispec == 17 ) .or. ( ispec == 18 )) then
           ! ispec = 17, 18:  block size kd, ib
           ! could be also dependent from n but for now it
           ! depend only on sequential or parallel
              if( nthreads>4 ) then
                 if( cprec ) then
                    kd = 128
                    ib = 32
                 else
                    kd = 160
                    ib = 40
                 endif
              else if( nthreads>1 ) then
                 if( cprec ) then
                    kd = 64
                    ib = 32
                 else
                    kd = 64
                    ib = 32
                 endif
              else
                 if( cprec ) then
                    kd = 16
                    ib = 16
                 else
                    kd = 32
                    ib = 16
                 endif
              endif
              if( ispec==17 ) stdlib_iparam2stage = kd
              if( ispec==18 ) stdlib_iparam2stage = ib
           else if ( ispec == 19 ) then
           ! ispec = 19:
           ! lhous length of the houselholder representation
           ! matrix (v,t) of the second stage. should be >= 1.
           ! will add the vect option here next release
              vect  = opts(1:1)
              if( vect=='N' ) then
                 lhous = max( 1, 4*ni )
              else
                 ! this is not correct, it need to call the algo and the stage2
                 lhous = max( 1, 4*ni ) + ibi
              endif
              if( lhous>=0 ) then
                 stdlib_iparam2stage = lhous
              else
                 stdlib_iparam2stage = -1
              endif
           else if ( ispec == 20 ) then
           ! ispec = 20: (21 for future use)
           ! lwork length of the workspace for
           ! either or both stages for trd and brd. should be >= 1.
           ! trd:
           ! trd_stage 1: = lt + lw + ls1 + ls2
                        ! = ldt*kd + n*kd + n*max(kd,factoptnb) + lds2*kd
                          ! where ldt=lds2=kd
                        ! = n*kd + n*max(kd,factoptnb) + 2*kd*kd
           ! trd_stage 2: = (2nb+1)*n + kd*nthreads
           ! trd_both   : = max(stage1,stage2) + ab ( ab=(kd+1)*n )
                        ! = n*kd + n*max(kd+1,factoptnb)
                          ! + max(2*kd*kd, kd*nthreads)
                          ! + (kd+1)*n
              lwork        = -1
              subnam(1:1)  = prec
              subnam(2:6)  = 'GEQRF'
              qroptnb      = stdlib_ilaenv( 1_ilp, subnam, ' ', ni, nbi, -1_ilp, -1_ilp )
              subnam(2:6)  = 'GELQF'
              lqoptnb      = stdlib_ilaenv( 1_ilp, subnam, ' ', nbi, ni, -1_ilp, -1_ilp )
              ! could be qr or lq for trd and the max for brd
              factoptnb    = max(qroptnb, lqoptnb)
              if( algo=='TRD' ) then
                 if( stag=='2STAG' ) then
                    lwork = ni*nbi + ni*max(nbi+1,factoptnb)+ max(2*nbi*nbi, nbi*nthreads)+ (nbi+&
                              1)*ni
                 else if( (stag=='HE2HB').or.(stag=='SY2SB') ) then
                    lwork = ni*nbi + ni*max(nbi,factoptnb) + 2*nbi*nbi
                 else if( (stag=='HB2ST').or.(stag=='SB2ST') ) then
                    lwork = (2*nbi+1)*ni + nbi*nthreads
                 endif
              else if( algo=='BRD' ) then
                 if( stag=='2STAG' ) then
                    lwork = 2*ni*nbi + ni*max(nbi+1,factoptnb)+ max(2*nbi*nbi, nbi*nthreads)+ (&
                              nbi+1)*ni
                 else if( stag=='GE2GB' ) then
                    lwork = ni*nbi + ni*max(nbi,factoptnb) + 2*nbi*nbi
                 else if( stag=='GB2BD' ) then
                    lwork = (3*nbi+1)*ni + nbi*nthreads
                 endif
              endif
              lwork = max ( 1, lwork )
              if( lwork>0 ) then
                 stdlib_iparam2stage = lwork
              else
                 stdlib_iparam2stage = -1
              endif
           else if ( ispec == 21 ) then
           ! ispec = 21 for future use
              stdlib_iparam2stage = nxi
           endif
     end function stdlib_iparam2stage


     integer(ilp) function stdlib_ilaenv2stage( ispec, name, opts, n1, n2, n3, n4 )
     !! ILAENV2STAGE is called from the LAPACK routines to choose problem-dependent
     !! parameters for the local environment.  See ISPEC for a description of
     !! the parameters.
     !! It sets problem and machine dependent parameters useful for *_2STAGE and
     !! related subroutines.
     !! ILAENV2STAGE returns an INTEGER
     !! if ILAENV2STAGE >= 0: ILAENV2STAGE returns the value of the parameter
     !! specified by ISPEC
     !! if ILAENV2STAGE < 0:  if ILAENV2STAGE = -k, the k-th argument had an
     !! illegal value.
     !! This version provides a set of parameters which should give good,
     !! but not optimal, performance on many of the currently available
     !! computers for the 2-stage solvers. Users are encouraged to modify this
     !! subroutine to set the tuning parameters for their particular machine using
     !! the option and problem size information in the arguments.
     !! This routine will not function correctly if it is converted to all
     !! lower case.  Converting it to all upper case is allowed.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! july 2017
           ! Scalar Arguments 
           character(len=*), intent(in) :: name, opts
           integer(ilp), intent(in) :: ispec, n1, n2, n3, n4
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: iispec
           ! Executable Statements 
           go to ( 10, 10, 10, 10, 10 )ispec
           ! invalid value for ispec
           stdlib_ilaenv2stage = -1
           return
           10 continue
           ! 2stage eigenvalues and svd or related subroutines.
           iispec = 16 + ispec
           stdlib_ilaenv2stage = stdlib_iparam2stage( iispec, name, opts,n1, n2, n3, n4 )
           return
     end function stdlib_ilaenv2stage

     pure character function stdlib_I64_chla_transtype( trans )
     !! This subroutine translates from a BLAST-specified integer constant to
     !! the character string specifying a transposition operation.
     !! CHLA_TRANSTYPE returns an CHARACTER*1.  If CHLA_TRANSTYPE: is 'X',
     !! then input is not an integer indicating a transposition operator.
     !! Otherwise CHLA_TRANSTYPE returns the constant value corresponding to
     !! TRANS.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: trans
        ! =====================================================================
           ! Parameters 
           integer(ilp64), parameter :: blas_no_trans = 111
           integer(ilp64), parameter :: blas_trans = 112
           integer(ilp64), parameter :: blas_conj_trans = 113
           
           ! Executable Statements 
           if( trans==blas_no_trans ) then
              stdlib_I64_chla_transtype = 'N'
           else if( trans==blas_trans ) then
              stdlib_I64_chla_transtype = 'T'
           else if( trans==blas_conj_trans ) then
              stdlib_I64_chla_transtype = 'C'
           else
              stdlib_I64_chla_transtype = 'X'
           end if
           return
     end function stdlib_I64_chla_transtype

     pure integer(ilp64) function stdlib_I64_ieeeck( ispec, zero, one )
     !! IEEECK is called from the ILAENV to verify that Infinity and
     !! possibly NaN arithmetic is safe (i.e. will not trap).
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: ispec
           real(sp), intent(in) :: one, zero
           
        ! =====================================================================
           ! Executable Statements
           stdlib_I64_ieeeck = 1
           
           ! Test support for infinity values
           if (.not.ieee_support_inf(one)) then 
              stdlib_I64_ieeeck = 0
              return
           end if
           
           ! return if we were only asked to check infinity arithmetic
           if (ispec == 0) return
           
           if (.not.ieee_support_nan(one)) then
              stdlib_I64_ieeeck = 0
              return
           end if
           
           return
     end function stdlib_I64_ieeeck




     integer(ilp64) function stdlib_I64_iladiag( diag )
     !! This subroutine translated from a character string specifying if a
     !! matrix has unit diagonal or not to the relevant BLAST-specified
     !! integer constant.
     !! ILADIAG returns an INTEGER.  If ILADIAG: < 0, then the input is not a
     !! character indicating a unit or non-unit diagonal.  Otherwise ILADIAG
     !! returns the constant value corresponding to DIAG.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character :: diag
        ! =====================================================================
           ! Parameters 
           integer(ilp64), parameter :: blas_non_unit_diag = 131
           integer(ilp64), parameter :: blas_unit_diag = 132
           
           ! Executable Statements 
           if( stdlib_lsame( diag, 'N' ) ) then
              stdlib_I64_iladiag = blas_non_unit_diag
           else if( stdlib_lsame( diag, 'U' ) ) then
              stdlib_I64_iladiag = blas_unit_diag
           else
              stdlib_I64_iladiag = -1
           end if
           return
     end function stdlib_I64_iladiag




     integer(ilp64) function stdlib_I64_ilaprec( prec )
     !! This subroutine translated from a character string specifying an
     !! intermediate precision to the relevant BLAST-specified integer
     !! constant.
     !! ILAPREC returns an INTEGER.  If ILAPREC: < 0, then the input is not a
     !! character indicating a supported intermediate precision.  Otherwise
     !! ILAPREC returns the constant value corresponding to PREC.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character :: prec
        ! =====================================================================
           ! Parameters 
           integer(ilp64), parameter :: blas_prec_single = 211
           integer(ilp64), parameter :: blas_prec_double = 212
           integer(ilp64), parameter :: blas_prec_indigenous = 213
           integer(ilp64), parameter :: blas_prec_extra = 214
           
           ! Executable Statements 
           if( stdlib_lsame( prec, 'S' ) ) then
              stdlib_I64_ilaprec = blas_prec_single
           else if( stdlib_lsame( prec, 'D' ) ) then
              stdlib_I64_ilaprec = blas_prec_double
           else if( stdlib_lsame( prec, 'I' ) ) then
              stdlib_I64_ilaprec = blas_prec_indigenous
           else if( stdlib_lsame( prec, 'X' ) .or. stdlib_lsame( prec, 'E' ) ) then
              stdlib_I64_ilaprec = blas_prec_extra
           else
              stdlib_I64_ilaprec = -1
           end if
           return
     end function stdlib_I64_ilaprec



     integer(ilp64) function stdlib_I64_ilatrans( trans )
     !! This subroutine translates from a character string specifying a
     !! transposition operation to the relevant BLAST-specified integer
     !! constant.
     !! ILATRANS returns an INTEGER.  If ILATRANS: < 0, then the input is not
     !! a character indicating a transposition operator.  Otherwise ILATRANS
     !! returns the constant value corresponding to TRANS.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character :: trans
        ! =====================================================================
           ! Parameters 
           integer(ilp64), parameter :: blas_no_trans = 111
           integer(ilp64), parameter :: blas_trans = 112
           integer(ilp64), parameter :: blas_conj_trans = 113
           
           ! Executable Statements 
           if( stdlib_lsame( trans, 'N' ) ) then
              stdlib_I64_ilatrans = blas_no_trans
           else if( stdlib_lsame( trans, 'T' ) ) then
              stdlib_I64_ilatrans = blas_trans
           else if( stdlib_lsame( trans, 'C' ) ) then
              stdlib_I64_ilatrans = blas_conj_trans
           else
              stdlib_I64_ilatrans = -1
           end if
           return
     end function stdlib_I64_ilatrans


     integer(ilp64) function stdlib_I64_ilauplo( uplo )
     !! This subroutine translated from a character string specifying a
     !! upper- or lower-triangular matrix to the relevant BLAST-specified
     !! integer constant.
     !! ILAUPLO returns an INTEGER.  If ILAUPLO: < 0, then the input is not
     !! a character indicating an upper- or lower-triangular matrix.
     !! Otherwise ILAUPLO returns the constant value corresponding to UPLO.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character :: uplo
        ! =====================================================================
           ! Parameters 
           integer(ilp64), parameter :: blas_upper = 121
           integer(ilp64), parameter :: blas_lower = 122
           
           ! Executable Statements 
           if( stdlib_lsame( uplo, 'U' ) ) then
              stdlib_I64_ilauplo = blas_upper
           else if( stdlib_lsame( uplo, 'L' ) ) then
              stdlib_I64_ilauplo = blas_lower
           else
              stdlib_I64_ilauplo = -1
           end if
           return
     end function stdlib_I64_ilauplo


     pure integer(ilp64) function stdlib_I64_iparmq( ispec, name, opts, n, ilo, ihi, lwork )
     !! This program sets problem and machine dependent parameters
     !! useful for xHSEQR and related subroutines for eigenvalue
     !! problems. It is called whenever
     !! IPARMQ is called with 12 <= ISPEC <= 16
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: ihi, ilo, ispec, lwork, n
           character, intent(in) :: name*(*), opts*(*)
        ! ================================================================
           ! Parameters 
           integer(ilp64), parameter :: inmin = 12
           integer(ilp64), parameter :: inwin = 13
           integer(ilp64), parameter :: inibl = 14
           integer(ilp64), parameter :: ishfts = 15
           integer(ilp64), parameter :: iacc22 = 16
           integer(ilp64), parameter :: icost = 17
           integer(ilp64), parameter :: nmin = 75
           integer(ilp64), parameter :: k22min = 14
           integer(ilp64), parameter :: kacmin = 14
           integer(ilp64), parameter :: nibble = 14
           integer(ilp64), parameter :: knwswp = 500
           integer(ilp64), parameter :: rcost = 10
           real(sp), parameter :: two = 2.0
           
           
           
           ! Local Scalars 
           integer(ilp64) :: nh, ns
           integer(ilp64) :: i, ic, iz
           character :: subnam*6
           ! Intrinsic Functions 
           intrinsic :: log,max,mod,nint,real
           ! Executable Statements 
           if( ( ispec==ishfts ) .or. ( ispec==inwin ) .or.( ispec==iacc22 ) ) then
              ! ==== set the number simultaneous shifts ====
              nh = ihi - ilo + 1
              ns = 2
              if( nh>=30 )ns = 4
              if( nh>=60 )ns = 10
              if( nh>=150 )ns = max( 10, nh / nint( log( real( nh,KIND=dp) ) / log( two ),&
                        KIND=ilp) )
              if( nh>=590 )ns = 64
              if( nh>=3000 )ns = 128
              if( nh>=6000 )ns = 256
              ns = max( 2, ns-mod( ns, 2 ) )
           end if
           if( ispec==inmin ) then
              ! ===== matrices of order smaller than nmin get sent
              ! .     to xlahqr, the classic double shift algorithm.
              ! .     this must be at least 11. ====
              stdlib_I64_iparmq = nmin
           else if( ispec==inibl ) then
              ! ==== inibl: skip a multi-shift qr iteration and
              ! .    whenever aggressive early deflation finds
              ! .    at least (nibble*(window size)/100) deflations. ====
              stdlib_I64_iparmq = nibble
           else if( ispec==ishfts ) then
              ! ==== nshfts: the number of simultaneous shifts =====
              stdlib_I64_iparmq = ns
           else if( ispec==inwin ) then
              ! ==== nw: deflation window size.  ====
              if( nh<=knwswp ) then
                 stdlib_I64_iparmq = ns
              else
                 stdlib_I64_iparmq = 3*ns / 2
              end if
           else if( ispec==iacc22 ) then
              ! ==== iacc22: whether to accumulate reflections
              ! .     before updating the far-from-diagonal elements
              ! .     and whether to use 2-by-2 block structure while
              ! .     doing it.  a small amount of work could be saved
              ! .     by making this choice dependent also upon the
              ! .     nh=ihi-ilo+1.
              ! convert name to upper case if the first character is lower case.
              stdlib_I64_iparmq = 0
              subnam = name
              ic = ichar( subnam( 1: 1 ) )
              iz = ichar( 'Z' )
              if( iz==90 .or. iz==122 ) then
                 ! ascii character set
                 if( ic>=97 .and. ic<=122 ) then
                    subnam( 1: 1 ) = char( ic-32 )
                    do i = 2, 6
                       ic = ichar( subnam( i: i ) )
                       if( ic>=97 .and. ic<=122 )subnam( i: i ) = char( ic-32 )
                    end do
                 end if
              else if( iz==233 .or. iz==169 ) then
                 ! ebcdic character set
                 if( ( ic>=129 .and. ic<=137 ) .or.( ic>=145 .and. ic<=153 ) .or.( ic>=162 .and. &
                           ic<=169 ) ) then
                    subnam( 1: 1 ) = char( ic+64 )
                    do i = 2, 6
                       ic = ichar( subnam( i: i ) )
                       if( ( ic>=129 .and. ic<=137 ) .or.( ic>=145 .and. ic<=153 ) .or.( ic>=162 &
                                 .and. ic<=169 ) )subnam( i:i ) = char( ic+64 )
                    end do
                 end if
              else if( iz==218 .or. iz==250 ) then
                 ! prime machines:  ascii+128
                 if( ic>=225 .and. ic<=250 ) then
                    subnam( 1: 1 ) = char( ic-32 )
                    do i = 2, 6
                       ic = ichar( subnam( i: i ) )
                       if( ic>=225 .and. ic<=250 )subnam( i: i ) = char( ic-32 )
                    end do
                 end if
              end if
              if( subnam( 2:6 )=='GGHRD' .or.subnam( 2:6 )=='GGHD3' ) then
                 stdlib_I64_iparmq = 1
                 if( nh>=k22min )stdlib_I64_iparmq = 2
              else if ( subnam( 4:6 )=='EXC' ) then
                 if( nh>=kacmin )stdlib_I64_iparmq = 1
                 if( nh>=k22min )stdlib_I64_iparmq = 2
              else if ( subnam( 2:6 )=='HSEQR' .or.subnam( 2:5 )=='LAQR' ) then
                 if( ns>=kacmin )stdlib_I64_iparmq = 1
                 if( ns>=k22min )stdlib_I64_iparmq = 2
              end if
           else if( ispec==icost ) then
              ! === relative cost of near-the-diagonal chase vs
                  ! blas updates ===
              stdlib_I64_iparmq = rcost
           else
              ! ===== invalid value of ispec =====
              stdlib_I64_iparmq = -1
           end if
     end function stdlib_I64_iparmq

     pure logical(lk) function stdlib_I64_lsamen( n, ca, cb )
     !! LSAMEN tests if the first N letters of CA are the same as the
     !! first N letters of CB, regardless of case.
     !! LSAMEN returns .TRUE. if CA and CB are equivalent except for case
     !! and .FALSE. otherwise.  LSAMEN also returns .FALSE. if LEN( CA )
     !! or LEN( CB ) is less than N.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character(len=*), intent(in) :: ca, cb
           integer(ilp64), intent(in) :: n
       ! =====================================================================
           ! Local Scalars 
           integer(ilp64) :: i
           ! Intrinsic Functions 
           intrinsic :: len
           ! Executable Statements 
           stdlib_I64_lsamen = .false.
           if( len( ca )<n .or. len( cb )<n )go to 20
           ! do for each character in the two strings.
           do i = 1, n
              ! test if the characters are equal using stdlib_I64_lsame.
              if( .not.stdlib_lsame( ca( i: i ), cb( i: i ) ) )go to 20
           end do
           stdlib_I64_lsamen = .true.
           20 continue
           return
     end function stdlib_I64_lsamen

     pure real(sp) function stdlib_I64_sroundup_lwork( lwork )
     !! ROUNDUP_LWORK >= LWORK.
     !! ROUNDUP_LWORK is guaranteed to have zero decimal part.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: lwork
       ! =====================================================================
           ! Intrinsic Functions 
           intrinsic :: epsilon,real,int
           ! Executable Statements 
           stdlib_I64_sroundup_lwork = real(lwork,KIND=sp)
           if (int( stdlib_I64_sroundup_lwork,KIND=ilp)<lwork) then
               ! force round up of lwork
               stdlib_I64_sroundup_lwork = stdlib_I64_sroundup_lwork * (1.0e+0_sp + epsilon(0.0e+0_sp))
           endif
           return
     end function stdlib_I64_sroundup_lwork
     
     pure real(dp) function stdlib_I64_droundup_lwork( lwork )
     !! ROUNDUP_LWORK >= LWORK.
     !! ROUNDUP_LWORK is guaranteed to have zero decimal part.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: lwork
       ! =====================================================================
           ! Intrinsic Functions 
           intrinsic :: epsilon,real,int
           ! Executable Statements 
           stdlib_I64_droundup_lwork = real(lwork,KIND=dp)
           if (int( stdlib_I64_droundup_lwork,KIND=ilp)<lwork) then
               ! force round up of lwork
               stdlib_I64_droundup_lwork = stdlib_I64_droundup_lwork * (1.0e+0_dp + epsilon(0.0e+0_dp))
           endif
           return
     end function stdlib_I64_droundup_lwork
     

     pure integer(ilp64) function stdlib_I64_ilaslc( m, n, a, lda )
     !! ILADLC: scans A for its last non-zero column.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: m, n, lda
           ! Array Arguments 
           real(sp), intent(in) :: a(lda,*)
        ! =====================================================================
           ! Parameters 
           real(sp), parameter :: zero = 0.0_sp
           
           ! Local Scalars 
           integer(ilp64) :: i
           ! Executable Statements 
           ! quick test for the common case where one corner is non-zero.
           if( n==0 ) then
              stdlib_I64_ilaslc = n
           else if (a(1, n)/=zero .or. a(m, n)/=zero) then
              stdlib_I64_ilaslc = n
           else
           ! now scan each column from the end, returning with the first non-zero.
              do stdlib_I64_ilaslc = n, 1, -1
                 do i = 1, m
                    if (a(i, stdlib_I64_ilaslc)/=zero) return
                 end do
              end do
           end if
           return
     end function stdlib_I64_ilaslc
     
     pure integer(ilp64) function stdlib_I64_ilaslr( m, n, a, lda )
     !! ILADLR: scans A for its last non-zero row.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: m, n, lda
           ! Array Arguments 
           real(sp), intent(in) :: a(lda,*)
        ! =====================================================================
           ! Parameters 
           real(sp), parameter :: zero = 0.0_sp
           
           ! Local Scalars 
           integer(ilp64) :: i, j
           ! Executable Statements 
           ! quick test for the common case where one corner is non-zero.
           if( m==0 ) then
              stdlib_I64_ilaslr = m
           else if( a(m, 1)/=zero .or. a(m, n)/=zero ) then
              stdlib_I64_ilaslr = m
           else
           ! scan up each column tracking the last zero row seen.
              stdlib_I64_ilaslr = 0
              do j = 1, n
                 i=m
                 do while((a(max(i,1),j)==zero).and.(i>=1))
                    i=i-1
                 enddo
                 stdlib_I64_ilaslr = max( stdlib_I64_ilaslr, i )
              end do
           end if
           return
     end function stdlib_I64_ilaslr     
     
     pure integer(ilp64) function stdlib_I64_iladlc( m, n, a, lda )
     !! ILADLC: scans A for its last non-zero column.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: m, n, lda
           ! Array Arguments 
           real(dp), intent(in) :: a(lda,*)
        ! =====================================================================
           ! Parameters 
           real(dp), parameter :: zero = 0.0_dp
           
           ! Local Scalars 
           integer(ilp64) :: i
           ! Executable Statements 
           ! quick test for the common case where one corner is non-zero.
           if( n==0 ) then
              stdlib_I64_iladlc = n
           else if (a(1, n)/=zero .or. a(m, n)/=zero) then
              stdlib_I64_iladlc = n
           else
           ! now scan each column from the end, returning with the first non-zero.
              do stdlib_I64_iladlc = n, 1, -1
                 do i = 1, m
                    if (a(i, stdlib_I64_iladlc)/=zero) return
                 end do
              end do
           end if
           return
     end function stdlib_I64_iladlc
     
     pure integer(ilp64) function stdlib_I64_iladlr( m, n, a, lda )
     !! ILADLR: scans A for its last non-zero row.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: m, n, lda
           ! Array Arguments 
           real(dp), intent(in) :: a(lda,*)
        ! =====================================================================
           ! Parameters 
           real(dp), parameter :: zero = 0.0_dp
           
           ! Local Scalars 
           integer(ilp64) :: i, j
           ! Executable Statements 
           ! quick test for the common case where one corner is non-zero.
           if( m==0 ) then
              stdlib_I64_iladlr = m
           else if( a(m, 1)/=zero .or. a(m, n)/=zero ) then
              stdlib_I64_iladlr = m
           else
           ! scan up each column tracking the last zero row seen.
              stdlib_I64_iladlr = 0
              do j = 1, n
                 i=m
                 do while((a(max(i,1),j)==zero).and.(i>=1))
                    i=i-1
                 enddo
                 stdlib_I64_iladlr = max( stdlib_I64_iladlr, i )
              end do
           end if
           return
     end function stdlib_I64_iladlr     
     
     pure integer(ilp64) function stdlib_I64_ilaclc( m, n, a, lda )
     !! ILADLC: scans A for its last non-zero column.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: m, n, lda
           ! Array Arguments 
           complex(sp), intent(in) :: a(lda,*)
        ! =====================================================================
           ! Parameters 
           complex(sp), parameter :: zero = 0.0_sp
           
           ! Local Scalars 
           integer(ilp64) :: i
           ! Executable Statements 
           ! quick test for the common case where one corner is non-zero.
           if( n==0 ) then
              stdlib_I64_ilaclc = n
           else if (a(1, n)/=zero .or. a(m, n)/=zero) then
              stdlib_I64_ilaclc = n
           else
           ! now scan each column from the end, returning with the first non-zero.
              do stdlib_I64_ilaclc = n, 1, -1
                 do i = 1, m
                    if (a(i, stdlib_I64_ilaclc)/=zero) return
                 end do
              end do
           end if
           return
     end function stdlib_I64_ilaclc
     
     pure integer(ilp64) function stdlib_I64_ilaclr( m, n, a, lda )
     !! ILADLR: scans A for its last non-zero row.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: m, n, lda
           ! Array Arguments 
           complex(sp), intent(in) :: a(lda,*)
        ! =====================================================================
           ! Parameters 
           complex(sp), parameter :: zero = 0.0_sp
           
           ! Local Scalars 
           integer(ilp64) :: i, j
           ! Executable Statements 
           ! quick test for the common case where one corner is non-zero.
           if( m==0 ) then
              stdlib_I64_ilaclr = m
           else if( a(m, 1)/=zero .or. a(m, n)/=zero ) then
              stdlib_I64_ilaclr = m
           else
           ! scan up each column tracking the last zero row seen.
              stdlib_I64_ilaclr = 0
              do j = 1, n
                 i=m
                 do while((a(max(i,1),j)==zero).and.(i>=1))
                    i=i-1
                 enddo
                 stdlib_I64_ilaclr = max( stdlib_I64_ilaclr, i )
              end do
           end if
           return
     end function stdlib_I64_ilaclr     
     
     pure integer(ilp64) function stdlib_I64_ilazlc( m, n, a, lda )
     !! ILADLC: scans A for its last non-zero column.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: m, n, lda
           ! Array Arguments 
           complex(dp), intent(in) :: a(lda,*)
        ! =====================================================================
           ! Parameters 
           complex(dp), parameter :: zero = 0.0_dp
           
           ! Local Scalars 
           integer(ilp64) :: i
           ! Executable Statements 
           ! quick test for the common case where one corner is non-zero.
           if( n==0 ) then
              stdlib_I64_ilazlc = n
           else if (a(1, n)/=zero .or. a(m, n)/=zero) then
              stdlib_I64_ilazlc = n
           else
           ! now scan each column from the end, returning with the first non-zero.
              do stdlib_I64_ilazlc = n, 1, -1
                 do i = 1, m
                    if (a(i, stdlib_I64_ilazlc)/=zero) return
                 end do
              end do
           end if
           return
     end function stdlib_I64_ilazlc
     
     pure integer(ilp64) function stdlib_I64_ilazlr( m, n, a, lda )
     !! ILADLR: scans A for its last non-zero row.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: m, n, lda
           ! Array Arguments 
           complex(dp), intent(in) :: a(lda,*)
        ! =====================================================================
           ! Parameters 
           complex(dp), parameter :: zero = 0.0_dp
           
           ! Local Scalars 
           integer(ilp64) :: i, j
           ! Executable Statements 
           ! quick test for the common case where one corner is non-zero.
           if( m==0 ) then
              stdlib_I64_ilazlr = m
           else if( a(m, 1)/=zero .or. a(m, n)/=zero ) then
              stdlib_I64_ilazlr = m
           else
           ! scan up each column tracking the last zero row seen.
              stdlib_I64_ilazlr = 0
              do j = 1, n
                 i=m
                 do while((a(max(i,1),j)==zero).and.(i>=1))
                    i=i-1
                 enddo
                 stdlib_I64_ilazlr = max( stdlib_I64_ilazlr, i )
              end do
           end if
           return
     end function stdlib_I64_ilazlr     
     

     pure integer(ilp64) function stdlib_I64_icmax1( n, zx, incx )
     !! I*MAX1: finds the index of the first vector element of maximum absolute value.
     !! Based on I*AMAX from Level 1 BLAS.
     !! The change is to use the 'genuine' absolute value.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: incx, n
           ! Array Arguments 
           complex(sp), intent(in) :: zx(*)
        ! =====================================================================
           ! Local Scalars 
           real(sp) :: dmax
           integer(ilp64) :: i, ix
           ! Intrinsic Functions 
           intrinsic :: abs
           ! Executable Statements 
           stdlib_I64_icmax1 = 0
           if (n<1 .or. incx<=0) return
           stdlib_I64_icmax1 = 1
           if (n==1) return
           if (incx==1) then
              ! code for increment equal to 1
              dmax = abs(zx(1))
              do i = 2,n
                 if (abs(zx(i))>dmax) then
                    stdlib_I64_icmax1 = i
                    dmax = abs(zx(i))
                 end if
              end do
           else
              ! code for increment not equal to 1
              ix = 1
              dmax = abs(zx(1))
              ix = ix + incx
              do i = 2,n
                 if (abs(zx(ix))>dmax) then
                    stdlib_I64_icmax1 = i
                    dmax = abs(zx(ix))
                 end if
                 ix = ix + incx
              end do
           end if
           return
     end function stdlib_I64_icmax1
     pure integer(ilp64) function stdlib_I64_izmax1( n, zx, incx )
     !! I*MAX1: finds the index of the first vector element of maximum absolute value.
     !! Based on I*AMAX from Level 1 BLAS.
     !! The change is to use the 'genuine' absolute value.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: incx, n
           ! Array Arguments 
           complex(dp), intent(in) :: zx(*)
        ! =====================================================================
           ! Local Scalars 
           real(dp) :: dmax
           integer(ilp64) :: i, ix
           ! Intrinsic Functions 
           intrinsic :: abs
           ! Executable Statements 
           stdlib_I64_izmax1 = 0
           if (n<1 .or. incx<=0) return
           stdlib_I64_izmax1 = 1
           if (n==1) return
           if (incx==1) then
              ! code for increment equal to 1
              dmax = abs(zx(1))
              do i = 2,n
                 if (abs(zx(i))>dmax) then
                    stdlib_I64_izmax1 = i
                    dmax = abs(zx(i))
                 end if
              end do
           else
              ! code for increment not equal to 1
              ix = 1
              dmax = abs(zx(1))
              ix = ix + incx
              do i = 2,n
                 if (abs(zx(ix))>dmax) then
                    stdlib_I64_izmax1 = i
                    dmax = abs(zx(ix))
                 end if
                 ix = ix + incx
              end do
           end if
           return
     end function stdlib_I64_izmax1


     pure integer(ilp64) function stdlib_I64_ilaenv( ispec, name, opts, n1, n2, n3, n4 )
     !! ILAENV is called from the LAPACK routines to choose problem-dependent
     !! parameters for the local environment.  See ISPEC for a description of
     !! the parameters.
     !! ILAENV returns an INTEGER
     !! if ILAENV >= 0: ILAENV returns the value of the parameter specified by ISPEC
     !! if ILAENV < 0:  if ILAENV = -k, the k-th argument had an illegal value.
     !! This version provides a set of parameters which should give good,
     !! but not optimal, performance on many of the currently available
     !! computers.  Users are encouraged to modify this subroutine to set
     !! the tuning parameters for their particular machine using the option
     !! and problem size information in the arguments.
     !! This routine will not function correctly if it is converted to all
     !! lower case.  Converting it to all upper case is allowed.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character(len=*), intent(in) :: name, opts
           integer(ilp64), intent(in) :: ispec, n1, n2, n3, n4
        ! =====================================================================
           ! Local Scalars 
           integer(ilp64) :: i, ic, iz, nb, nbmin, nx
           logical(lk) :: cname, sname, twostage
           character :: c1*1, c2*2, c4*2, c3*3, subnam*16
           ! Intrinsic Functions 
           intrinsic :: char,ichar,int,min,real
           ! Executable Statements 
           go to ( 10, 10, 10, 80, 90, 100, 110, 120,130, 140, 150, 160, 160, 160, 160, 160, 160)&
                     ispec
           ! invalid value for ispec
           stdlib_I64_ilaenv = -1
           return
           10 continue
           ! convert name to upper case if the first character is lower case.
           stdlib_I64_ilaenv = 1
           subnam = name
           ic = ichar( subnam( 1: 1 ) )
           iz = ichar( 'Z' )
           if( iz==90 .or. iz==122 ) then
              ! ascii character set
              if( ic>=97 .and. ic<=122 ) then
                 subnam( 1: 1 ) = char( ic-32 )
                 do i = 2, 6
                    ic = ichar( subnam( i: i ) )
                    if( ic>=97 .and. ic<=122 )subnam( i: i ) = char( ic-32 )
                 end do
              end if
           else if( iz==233 .or. iz==169 ) then
              ! ebcdic character set
              if( ( ic>=129 .and. ic<=137 ) .or.( ic>=145 .and. ic<=153 ) .or.( ic>=162 .and. &
                        ic<=169 ) ) then
                 subnam( 1: 1 ) = char( ic+64 )
                 do i = 2, 6
                    ic = ichar( subnam( i: i ) )
                    if( ( ic>=129 .and. ic<=137 ) .or.( ic>=145 .and. ic<=153 ) .or.( ic>=162 &
                              .and. ic<=169 ) )subnam( i:i ) = char( ic+64 )
                 end do
              end if
           else if( iz==218 .or. iz==250 ) then
              ! prime machines:  ascii+128
              if( ic>=225 .and. ic<=250 ) then
                 subnam( 1: 1 ) = char( ic-32 )
                 do i = 2, 6
                    ic = ichar( subnam( i: i ) )
                    if( ic>=225 .and. ic<=250 )subnam( i: i ) = char( ic-32 )
                 end do
              end if
           end if
           c1 = subnam( 1: 1 )
           sname = c1=='S' .or. c1=='D'
           cname = c1=='C' .or. c1=='Z'
           if( .not.( cname .or. sname ) )return
           c2 = subnam( 2: 3 )
           c3 = subnam( 4: 6 )
           c4 = c3( 2: 3 )
           twostage = len( subnam )>=11.and. subnam( 11: 11 )=='2'
           go to ( 50, 60, 70 )ispec
           50 continue
           ! ispec = 1:  block size
           ! in these examples, separate code is provided for setting nb for
           ! real and complex.  we assume that nb will take the same value in
           ! single or double precision.
           nb = 1
           if( subnam(2:6)=='LAORH' ) then
              ! this is for *laorhr_getrfnp routine
              if( sname ) then
                  nb = 32
              else
                  nb = 32
              end if
           else if( c2=='GE' ) then
              if( c3=='TRF' ) then
                 if( sname ) then
                    nb = 64
                 else
                    nb = 64
                 end if
              else if( c3=='QRF' .or. c3=='RQF' .or. c3=='LQF' .or.c3=='QLF' ) then
                 if( sname ) then
                    nb = 32
                 else
                    nb = 32
                 end if
              else if( c3=='QR ') then
                 if( n3 == 1) then
                    if( sname ) then
           ! m*n
                       if ((n1*n2<=131072).or.(n1<=8192)) then
                          nb = n1
                       else
                          nb = 32768/n2
                       end if
                    else
                       if ((n1*n2<=131072).or.(n1<=8192)) then
                          nb = n1
                       else
                          nb = 32768/n2
                       end if
                    end if
                 else
                    if( sname ) then
                       nb = 1
                    else
                       nb = 1
                    end if
                 end if
              else if( c3=='LQ ') then
                 if( n3 == 2) then
                    if( sname ) then
           ! m*n
                       if ((n1*n2<=131072).or.(n1<=8192)) then
                          nb = n1
                       else
                          nb = 32768/n2
                       end if
                    else
                       if ((n1*n2<=131072).or.(n1<=8192)) then
                          nb = n1
                       else
                          nb = 32768/n2
                       end if
                    end if
                 else
                    if( sname ) then
                       nb = 1
                    else
                       nb = 1
                    end if
                 end if
              else if( c3=='HRD' ) then
                 if( sname ) then
                    nb = 32
                 else
                    nb = 32
                 end if
              else if( c3=='BRD' ) then
                 if( sname ) then
                    nb = 32
                 else
                    nb = 32
                 end if
              else if( c3=='TRI' ) then
                 if( sname ) then
                    nb = 64
                 else
                    nb = 64
                 end if
              end if
           else if( c2=='PO' ) then
              if( c3=='TRF' ) then
                 if( sname ) then
                    nb = 64
                 else
                    nb = 64
                 end if
              end if
           else if( c2=='SY' ) then
              if( c3=='TRF' ) then
                 if( sname ) then
                    if( twostage ) then
                       nb = 192
                    else
                       nb = 64
                    end if
                 else
                    if( twostage ) then
                       nb = 192
                    else
                       nb = 64
                    end if
                 end if
              else if( sname .and. c3=='TRD' ) then
                 nb = 32
              else if( sname .and. c3=='GST' ) then
                 nb = 64
              end if
           else if( cname .and. c2=='HE' ) then
              if( c3=='TRF' ) then
                 if( twostage ) then
                    nb = 192
                 else
                    nb = 64
                 end if
              else if( c3=='TRD' ) then
                 nb = 32
              else if( c3=='GST' ) then
                 nb = 64
              end if
           else if( sname .and. c2=='OR' ) then
              if( c3( 1: 1 )=='G' ) then
                 if( c4=='QR' .or. c4=='RQ' .or. c4=='LQ' .or. c4=='QL' .or. c4=='HR' .or. &
                           c4=='TR' .or. c4=='BR' )then
                    nb = 32
                 end if
              else if( c3( 1: 1 )=='M' ) then
                 if( c4=='QR' .or. c4=='RQ' .or. c4=='LQ' .or. c4=='QL' .or. c4=='HR' .or. &
                           c4=='TR' .or. c4=='BR' )then
                    nb = 32
                 end if
              end if
           else if( cname .and. c2=='UN' ) then
              if( c3( 1: 1 )=='G' ) then
                 if( c4=='QR' .or. c4=='RQ' .or. c4=='LQ' .or. c4=='QL' .or. c4=='HR' .or. &
                           c4=='TR' .or. c4=='BR' )then
                    nb = 32
                 end if
              else if( c3( 1: 1 )=='M' ) then
                 if( c4=='QR' .or. c4=='RQ' .or. c4=='LQ' .or. c4=='QL' .or. c4=='HR' .or. &
                           c4=='TR' .or. c4=='BR' )then
                    nb = 32
                 end if
              end if
           else if( c2=='GB' ) then
              if( c3=='TRF' ) then
                 if( sname ) then
                    if( n4<=64 ) then
                       nb = 1
                    else
                       nb = 32
                    end if
                 else
                    if( n4<=64 ) then
                       nb = 1
                    else
                       nb = 32
                    end if
                 end if
              end if
           else if( c2=='PB' ) then
              if( c3=='TRF' ) then
                 if( sname ) then
                    if( n2<=64 ) then
                       nb = 1
                    else
                       nb = 32
                    end if
                 else
                    if( n2<=64 ) then
                       nb = 1
                    else
                       nb = 32
                    end if
                 end if
              end if
           else if( c2=='TR' ) then
              if( c3=='TRI' ) then
                 if( sname ) then
                    nb = 64
                 else
                    nb = 64
                 end if
              else if ( c3=='EVC' ) then
                 if( sname ) then
                    nb = 64
                 else
                    nb = 64
                 end if
              end if
           else if( c2=='LA' ) then
              if( c3=='UUM' ) then
                 if( sname ) then
                    nb = 64
                 else
                    nb = 64
                 end if
              end if
           else if( sname .and. c2=='ST' ) then
              if( c3=='EBZ' ) then
                 nb = 1
              end if
           else if( c2=='GG' ) then
              nb = 32
              if( c3=='HD3' ) then
                 if( sname ) then
                    nb = 32
                 else
                    nb = 32
                 end if
              end if
           end if
           stdlib_I64_ilaenv = nb
           return
           60 continue
           ! ispec = 2:  minimum block size
           nbmin = 2
           if( c2=='GE' ) then
              if( c3=='QRF' .or. c3=='RQF' .or. c3=='LQF' .or. c3=='QLF' ) then
                 if( sname ) then
                    nbmin = 2
                 else
                    nbmin = 2
                 end if
              else if( c3=='HRD' ) then
                 if( sname ) then
                    nbmin = 2
                 else
                    nbmin = 2
                 end if
              else if( c3=='BRD' ) then
                 if( sname ) then
                    nbmin = 2
                 else
                    nbmin = 2
                 end if
              else if( c3=='TRI' ) then
                 if( sname ) then
                    nbmin = 2
                 else
                    nbmin = 2
                 end if
              end if
           else if( c2=='SY' ) then
              if( c3=='TRF' ) then
                 if( sname ) then
                    nbmin = 8
                 else
                    nbmin = 8
                 end if
              else if( sname .and. c3=='TRD' ) then
                 nbmin = 2
              end if
           else if( cname .and. c2=='HE' ) then
              if( c3=='TRD' ) then
                 nbmin = 2
              end if
           else if( sname .and. c2=='OR' ) then
              if( c3( 1: 1 )=='G' ) then
                 if( c4=='QR' .or. c4=='RQ' .or. c4=='LQ' .or. c4=='QL' .or. c4=='HR' .or. &
                           c4=='TR' .or. c4=='BR' )then
                    nbmin = 2
                 end if
              else if( c3( 1: 1 )=='M' ) then
                 if( c4=='QR' .or. c4=='RQ' .or. c4=='LQ' .or. c4=='QL' .or. c4=='HR' .or. &
                           c4=='TR' .or. c4=='BR' )then
                    nbmin = 2
                 end if
              end if
           else if( cname .and. c2=='UN' ) then
              if( c3( 1: 1 )=='G' ) then
                 if( c4=='QR' .or. c4=='RQ' .or. c4=='LQ' .or. c4=='QL' .or. c4=='HR' .or. &
                           c4=='TR' .or. c4=='BR' )then
                    nbmin = 2
                 end if
              else if( c3( 1: 1 )=='M' ) then
                 if( c4=='QR' .or. c4=='RQ' .or. c4=='LQ' .or. c4=='QL' .or. c4=='HR' .or. &
                           c4=='TR' .or. c4=='BR' )then
                    nbmin = 2
                 end if
              end if
           else if( c2=='GG' ) then
              nbmin = 2
              if( c3=='HD3' ) then
                 nbmin = 2
              end if
           end if
           stdlib_I64_ilaenv = nbmin
           return
           70 continue
           ! ispec = 3:  crossover point
           nx = 0
           if( c2=='GE' ) then
              if( c3=='QRF' .or. c3=='RQF' .or. c3=='LQF' .or. c3=='QLF' ) then
                 if( sname ) then
                    nx = 128
                 else
                    nx = 128
                 end if
              else if( c3=='HRD' ) then
                 if( sname ) then
                    nx = 128
                 else
                    nx = 128
                 end if
              else if( c3=='BRD' ) then
                 if( sname ) then
                    nx = 128
                 else
                    nx = 128
                 end if
              end if
           else if( c2=='SY' ) then
              if( sname .and. c3=='TRD' ) then
                 nx = 32
              end if
           else if( cname .and. c2=='HE' ) then
              if( c3=='TRD' ) then
                 nx = 32
              end if
           else if( sname .and. c2=='OR' ) then
              if( c3( 1: 1 )=='G' ) then
                 if( c4=='QR' .or. c4=='RQ' .or. c4=='LQ' .or. c4=='QL' .or. c4=='HR' .or. &
                           c4=='TR' .or. c4=='BR' )then
                    nx = 128
                 end if
              end if
           else if( cname .and. c2=='UN' ) then
              if( c3( 1: 1 )=='G' ) then
                 if( c4=='QR' .or. c4=='RQ' .or. c4=='LQ' .or. c4=='QL' .or. c4=='HR' .or. &
                           c4=='TR' .or. c4=='BR' )then
                    nx = 128
                 end if
              end if
           else if( c2=='GG' ) then
              nx = 128
              if( c3=='HD3' ) then
                 nx = 128
              end if
           end if
           stdlib_I64_ilaenv = nx
           return
           80 continue
           ! ispec = 4:  number of shifts (used by xhseqr)
           stdlib_I64_ilaenv = 6
           return
           90 continue
           ! ispec = 5:  minimum column dimension (not used)
           stdlib_I64_ilaenv = 2
           return
           100 continue
           ! ispec = 6:  crossover point for svd (used by xgelss and xgesvd)
           stdlib_I64_ilaenv = int( real( min( n1, n2 ),KIND=dp)*1.6e0,KIND=ilp)
           return
           110 continue
           ! ispec = 7:  number of processors (not used)
           stdlib_I64_ilaenv = 1
           return
           120 continue
           ! ispec = 8:  crossover point for multishift (used by xhseqr)
           stdlib_I64_ilaenv = 50
           return
           130 continue
           ! ispec = 9:  maximum size of the subproblems at the bottom of the
                       ! computation tree in the divide-and-conquer algorithm
                       ! (used by xgelsd and xgesdd)
           stdlib_I64_ilaenv = 25
           return
           140 continue
           ! ispec = 10: ieee and infinity nan arithmetic can be trusted not to trap
           ! stdlib_I64_ilaenv = 0
           stdlib_I64_ilaenv = 1
           if( stdlib_I64_ilaenv==1 ) then
              stdlib_I64_ilaenv = stdlib_I64_ieeeck( 1_ilp64, 0.0, 1.0 )
           end if
           return
           150 continue
           ! ispec = 11: ieee infinity arithmetic can be trusted not to trap
           ! stdlib_I64_ilaenv = 0
           stdlib_I64_ilaenv = 1
           if( stdlib_I64_ilaenv==1 ) then
              stdlib_I64_ilaenv = stdlib_I64_ieeeck( 0_ilp64, 0.0, 1.0 )
           end if
           return
           160 continue
           ! 12 <= ispec <= 17: xhseqr or related subroutines.
           stdlib_I64_ilaenv = stdlib_I64_iparmq( ispec, name, opts, n1, n2, n3, n4 )
           return
     end function stdlib_I64_ilaenv


     integer(ilp64) function stdlib_I64_iparam2stage( ispec, name, opts,ni, nbi, ibi, nxi )
     !! This program sets problem and machine dependent parameters
     !! useful for xHETRD_2STAGE, xHETRD_HE2HB, xHETRD_HB2ST,
     !! xGEBRD_2STAGE, xGEBRD_GE2GB, xGEBRD_GB2BD
     !! and related subroutines for eigenvalue problems.
     !! It is called whenever ILAENV is called with 17 <= ISPEC <= 21.
     !! It is called whenever ILAENV2STAGE is called with 1 <= ISPEC <= 5
     !! with a direct conversion ISPEC + 16.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character(len=*), intent(in) :: name, opts
           integer(ilp64), intent(in) :: ispec, ni, nbi, ibi, nxi
        ! ================================================================
           ! Local Scalars 
           integer(ilp64) :: i, ic, iz, kd, ib, lhous, lwork, nthreads, factoptnb, qroptnb, &
                     lqoptnb
           logical(lk) :: rprec, cprec
           character :: prec*1, algo*3, stag*5, subnam*12, vect*1
           ! Intrinsic Functions 
           intrinsic :: char,ichar,max
           ! Executable Statements 
           ! invalid value for ispec
           if( (ispec<17).or.(ispec>21) ) then
               stdlib_I64_iparam2stage = -1
               return
           endif
           ! get the number of threads
           nthreads = 1
           !$ nthreads = omp_get_num_threads()

           ! write(*,*) 'iparam voici nthreads ispec ',nthreads, ispec
           if( ispec /= 19 ) then
              ! convert name to upper case if the first character is lower case.
              stdlib_I64_iparam2stage = -1
              subnam = name
              ic = ichar( subnam( 1: 1 ) )
              iz = ichar( 'Z' )
              if( iz==90 .or. iz==122 ) then
                 ! ascii character set
                 if( ic>=97 .and. ic<=122 ) then
                    subnam( 1: 1 ) = char( ic-32 )
                    do i = 2, 12
                       ic = ichar( subnam( i: i ) )
                       if( ic>=97 .and. ic<=122 )subnam( i: i ) = char( ic-32 )
                    end do
                 end if
              else if( iz==233 .or. iz==169 ) then
                 ! ebcdic character set
                 if( ( ic>=129 .and. ic<=137 ) .or.( ic>=145 .and. ic<=153 ) .or.( ic>=162 .and. &
                           ic<=169 ) ) then
                    subnam( 1: 1 ) = char( ic+64 )
                    do i = 2, 12
                       ic = ichar( subnam( i: i ) )
                       if( ( ic>=129 .and. ic<=137 ) .or.( ic>=145 .and. ic<=153 ) .or.( ic>=162 &
                                 .and. ic<=169 ) )subnam( i:i ) = char( ic+64 )
                    end do
                 end if
              else if( iz==218 .or. iz==250 ) then
                 ! prime machines:  ascii+128
                 if( ic>=225 .and. ic<=250 ) then
                    subnam( 1: 1 ) = char( ic-32 )
                    do i = 2, 12
                      ic = ichar( subnam( i: i ) )
                      if( ic>=225 .and. ic<=250 )subnam( i: i ) = char( ic-32 )
                    end do
                 end if
              end if
              prec  = subnam( 1: 1 )
              algo  = subnam( 4: 6 )
              stag  = subnam( 8:12 )
              rprec = prec=='S' .or. prec=='D'
              cprec = prec=='C' .or. prec=='Z'
              ! invalid value for precision
              if( .not.( rprec .or. cprec ) ) then
                  stdlib_I64_iparam2stage = -1
                  return
              endif
           endif
            ! write(*,*),'rprec,cprec ',rprec,cprec,
           ! $           '   algo ',algo,'    stage ',stag
           if (( ispec == 17 ) .or. ( ispec == 18 )) then
           ! ispec = 17, 18:  block size kd, ib
           ! could be also dependent from n but for now it
           ! depend only on sequential or parallel
              if( nthreads>4 ) then
                 if( cprec ) then
                    kd = 128
                    ib = 32
                 else
                    kd = 160
                    ib = 40
                 endif
              else if( nthreads>1 ) then
                 if( cprec ) then
                    kd = 64
                    ib = 32
                 else
                    kd = 64
                    ib = 32
                 endif
              else
                 if( cprec ) then
                    kd = 16
                    ib = 16
                 else
                    kd = 32
                    ib = 16
                 endif
              endif
              if( ispec==17 ) stdlib_I64_iparam2stage = kd
              if( ispec==18 ) stdlib_I64_iparam2stage = ib
           else if ( ispec == 19 ) then
           ! ispec = 19:
           ! lhous length of the houselholder representation
           ! matrix (v,t) of the second stage. should be >= 1.
           ! will add the vect option here next release
              vect  = opts(1:1)
              if( vect=='N' ) then
                 lhous = max( 1, 4*ni )
              else
                 ! this is not correct, it need to call the algo and the stage2
                 lhous = max( 1, 4*ni ) + ibi
              endif
              if( lhous>=0 ) then
                 stdlib_I64_iparam2stage = lhous
              else
                 stdlib_I64_iparam2stage = -1
              endif
           else if ( ispec == 20 ) then
           ! ispec = 20: (21 for future use)
           ! lwork length of the workspace for
           ! either or both stages for trd and brd. should be >= 1.
           ! trd:
           ! trd_stage 1: = lt + lw + ls1 + ls2
                        ! = ldt*kd + n*kd + n*max(kd,factoptnb) + lds2*kd
                          ! where ldt=lds2=kd
                        ! = n*kd + n*max(kd,factoptnb) + 2*kd*kd
           ! trd_stage 2: = (2nb+1)*n + kd*nthreads
           ! trd_both   : = max(stage1,stage2) + ab ( ab=(kd+1)*n )
                        ! = n*kd + n*max(kd+1,factoptnb)
                          ! + max(2*kd*kd, kd*nthreads)
                          ! + (kd+1)*n
              lwork        = -1
              subnam(1:1)  = prec
              subnam(2:6)  = 'GEQRF'
              qroptnb      = stdlib_I64_ilaenv( 1_ilp64, subnam, ' ', ni, nbi, -1_ilp64, -1_ilp64 )
              subnam(2:6)  = 'GELQF'
              lqoptnb      = stdlib_I64_ilaenv( 1_ilp64, subnam, ' ', nbi, ni, -1_ilp64, -1_ilp64 )
              ! could be qr or lq for trd and the max for brd
              factoptnb    = max(qroptnb, lqoptnb)
              if( algo=='TRD' ) then
                 if( stag=='2STAG' ) then
                    lwork = ni*nbi + ni*max(nbi+1,factoptnb)+ max(2*nbi*nbi, nbi*nthreads)+ (nbi+&
                              1)*ni
                 else if( (stag=='HE2HB').or.(stag=='SY2SB') ) then
                    lwork = ni*nbi + ni*max(nbi,factoptnb) + 2*nbi*nbi
                 else if( (stag=='HB2ST').or.(stag=='SB2ST') ) then
                    lwork = (2*nbi+1)*ni + nbi*nthreads
                 endif
              else if( algo=='BRD' ) then
                 if( stag=='2STAG' ) then
                    lwork = 2*ni*nbi + ni*max(nbi+1,factoptnb)+ max(2*nbi*nbi, nbi*nthreads)+ (&
                              nbi+1)*ni
                 else if( stag=='GE2GB' ) then
                    lwork = ni*nbi + ni*max(nbi,factoptnb) + 2*nbi*nbi
                 else if( stag=='GB2BD' ) then
                    lwork = (3*nbi+1)*ni + nbi*nthreads
                 endif
              endif
              lwork = max ( 1, lwork )
              if( lwork>0 ) then
                 stdlib_I64_iparam2stage = lwork
              else
                 stdlib_I64_iparam2stage = -1
              endif
           else if ( ispec == 21 ) then
           ! ispec = 21 for future use
              stdlib_I64_iparam2stage = nxi
           endif
     end function stdlib_I64_iparam2stage


     integer(ilp64) function stdlib_I64_ilaenv2stage( ispec, name, opts, n1, n2, n3, n4 )
     !! ILAENV2STAGE is called from the LAPACK routines to choose problem-dependent
     !! parameters for the local environment.  See ISPEC for a description of
     !! the parameters.
     !! It sets problem and machine dependent parameters useful for *_2STAGE and
     !! related subroutines.
     !! ILAENV2STAGE returns an INTEGER
     !! if ILAENV2STAGE >= 0: ILAENV2STAGE returns the value of the parameter
     !! specified by ISPEC
     !! if ILAENV2STAGE < 0:  if ILAENV2STAGE = -k, the k-th argument had an
     !! illegal value.
     !! This version provides a set of parameters which should give good,
     !! but not optimal, performance on many of the currently available
     !! computers for the 2-stage solvers. Users are encouraged to modify this
     !! subroutine to set the tuning parameters for their particular machine using
     !! the option and problem size information in the arguments.
     !! This routine will not function correctly if it is converted to all
     !! lower case.  Converting it to all upper case is allowed.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! july 2017
           ! Scalar Arguments 
           character(len=*), intent(in) :: name, opts
           integer(ilp64), intent(in) :: ispec, n1, n2, n3, n4
        ! =====================================================================
           ! Local Scalars 
           integer(ilp64) :: iispec
           ! Executable Statements 
           go to ( 10, 10, 10, 10, 10 )ispec
           ! invalid value for ispec
           stdlib_I64_ilaenv2stage = -1
           return
           10 continue
           ! 2stage eigenvalues and svd or related subroutines.
           iispec = 16 + ispec
           stdlib_I64_ilaenv2stage = stdlib_I64_iparam2stage( iispec, name, opts,n1, n2, n3, n4 )
           return
     end function stdlib_I64_ilaenv2stage


   !----------------------------------------------------------------------------
   !-----                                                                  -----
   !-----     AUXILIARY INFO HANDLING FUNCTIONS FOR LAPACK SUBROUTINES     -----
   !-----                                                                  -----
   !----------------------------------------------------------------------------
   
   ! Cholesky factorization
   elemental subroutine handle_potrf_info(this,info,triangle,lda,n,err)
      character(len=*), intent(in) :: this
      character, intent(in) :: triangle
      integer(ilp), intent(in) :: info,lda,n
      type(linalg_state_type), intent(out) :: err

      ! Process output
      select case (info)
      case (0)
         ! Success
      case (-1)
         err = linalg_state_type(this,LINALG_INTERNAL_ERROR,'invalid triangle selection: ', &
                  triangle,'. should be U/L')
      case (-2)
         err = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix size n=',n)
      case (-4)
         err = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid lda=',lda,': is < n = ',n)
      case (1:)
         err = linalg_state_type(this,LINALG_ERROR,'cannot complete factorization:',info, &
               '-th order leading minor is not positive definite')
      case default
         err = linalg_state_type(this,LINALG_INTERNAL_ERROR,'catastrophic error')
      end select

   end subroutine handle_potrf_info

   elemental subroutine handle_getri_info(this,info,lda,n,err)
      character(len=*), intent(in) :: this
      integer(ilp), intent(in) :: info,lda,n
      type(linalg_state_type), intent(out) :: err

      ! Process output
      select case (info)
         case (0)
             ! Success
         case (:-1)
             err = linalg_state_type(this,LINALG_ERROR,'invalid matrix size a=',[lda,n])
         case (1:)
             ! Matrix is singular
             err = linalg_state_type(this,LINALG_ERROR,'singular matrix')
         case default
             err = linalg_state_type(this,LINALG_INTERNAL_ERROR,'catastrophic error')
      end select
   end subroutine handle_getri_info

   elemental subroutine handle_gesdd_info(this,err,info,m,n)
        character(len=*), intent(in) :: this
        !> Error handler
        type(linalg_state_type), intent(inout) :: err
        !> GESDD return flag
        integer(ilp), intent(in) :: info
        !> Input matrix size
        integer(ilp), intent(in) :: m,n

        select case (info)
           case (0)
               ! Success!
               err%state = LINALG_SUCCESS
           case (-1)
               err = linalg_state_type(this,LINALG_INTERNAL_ERROR,'Invalid task ID on input to GESDD.')
           case (-5,-3:-2)
               err = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix size: a=',[m,n])
           case (-8)
               err = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix U size, with a=',[m,n])
           case (-10)
               err = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix V size, with a=',[m,n])
           case (-4)
               err = linalg_state_type(this,LINALG_VALUE_ERROR,'A contains invalid/NaN values.')
           case (1:)
               err = linalg_state_type(this,LINALG_ERROR,'SVD computation did not converge.')
           case default
               err = linalg_state_type(this,LINALG_INTERNAL_ERROR,'Unknown error returned by GESDD.')
        end select

     end subroutine handle_gesdd_info

     elemental subroutine handle_gesv_info(this,info,lda,n,nrhs,err)
         character(len=*), intent(in) :: this
         integer(ilp), intent(in) :: info,lda,n,nrhs
         type(linalg_state_type), intent(out) :: err

         ! Process output
         select case (info)
            case (0)
                ! Success
            case (-1)
                err = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid problem size n=',n)
            case (-2)
                err = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid rhs size n=',nrhs)
            case (-4)
                err = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix size a=',[lda,n])
            case (-7)
                err = linalg_state_type(this,LINALG_ERROR,'invalid matrix size a=',[lda,n])
            case (1:)
                err = linalg_state_type(this,LINALG_ERROR,'singular matrix')
            case default
                err = linalg_state_type(this,LINALG_INTERNAL_ERROR,'catastrophic error')
         end select
         
     end subroutine handle_gesv_info

   !> Wrapper function to handle GEES error codes
    elemental subroutine handle_gees_info(this, info, m, n, ldvs, err)
        character(len=*), intent(in) :: this
        integer(ilp), intent(in) :: info, m, n, ldvs
        type(linalg_state_type), intent(out) :: err

        ! Process GEES output
        select case (info)
        case (0_ilp)
            ! Success
        case (-1_ilp)
            ! Vector not wanted, but task is wrong
            err = linalg_state_type(this, LINALG_INTERNAL_ERROR,'Invalid Schur vector task request')
        case (-2_ilp)
            ! Vector not wanted, but task is wrong
            err = linalg_state_type(this, LINALG_INTERNAL_ERROR,'Invalid sorting task request')            
        case (-4_ilp,-6_ilp)
            ! Vector not wanted, but task is wrong
            err = linalg_state_type(this, LINALG_VALUE_ERROR,'Invalid/non-square input matrix size:',[m,n])  
        case (-11_ilp)
            err = linalg_state_type(this, LINALG_VALUE_ERROR,'Schur vector matrix has insufficient size',[ldvs,n])
        case (-13_ilp)
            err = linalg_state_type(this, LINALG_INTERNAL_ERROR,'Insufficient working storage size')
        case (1_ilp:)
            
            if (info==n+2) then 
                err = linalg_state_type(this, LINALG_ERROR, 'Ill-conditioned problem: could not sort eigenvalues')
            elseif (info==n+1) then 
                err = linalg_state_type(this, LINALG_ERROR, 'Some selected eigenvalues lost property due to sorting')                
            elseif (info==n) then 
                err = linalg_state_type(this, LINALG_ERROR, 'Convergence failure: no converged eigenvalues')
            else
                err = linalg_state_type(this, LINALG_ERROR, 'Convergence failure; converged range is',[info,n])                            
            end if
            
        case default
            
            err = linalg_state_type(this, LINALG_INTERNAL_ERROR, 'GEES catastrophic error: info=', info)

        end select
        
    end subroutine handle_gees_info

   elemental subroutine handle_geqrf_info(this,info,m,n,lwork,err)
         character(len=*), intent(in) :: this
         integer(ilp), intent(in) :: info,m,n,lwork
         type(linalg_state_type), intent(out) :: err

         ! Process output
         select case (info)
            case (0)
                ! Success
            case (-1)
                err = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix size m=',m)
            case (-2)
                err = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix size n=',n)
            case (-4)
                err = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix size a=',[m,n])
            case (-7)
                err = linalg_state_type(this,LINALG_ERROR,'invalid input for lwork=',lwork)
            case default
                err = linalg_state_type(this,LINALG_INTERNAL_ERROR,'catastrophic error')
         end select

     end subroutine handle_geqrf_info

     elemental subroutine handle_orgqr_info(this,info,m,n,k,lwork,err)
         character(len=*), intent(in) :: this
         integer(ilp), intent(in) :: info,m,n,k,lwork
         type(linalg_state_type), intent(out) :: err

         ! Process output
         select case (info)
            case (0)
                ! Success
            case (-1)
                err = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix size m=',m)
            case (-2)
                err = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix size n=',n)
            case (-4)
                err = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid k=min(m,n)=',k)
            case (-5)
                err = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix size a=',[m,n])                
            case (-8)
                err = linalg_state_type(this,LINALG_ERROR,'invalid input for lwork=',lwork)
            case default
                err = linalg_state_type(this,LINALG_INTERNAL_ERROR,'catastrophic error')
         end select

     end subroutine handle_orgqr_info     

     elemental subroutine handle_gelsd_info(this,info,lda,n,ldb,nrhs,err)
          character(len=*), intent(in) :: this
          integer(ilp), intent(in) :: info,lda,n,ldb,nrhs
          type(linalg_state_type), intent(out) :: err

          ! Process output
          select case (info)
             case (0)
                 ! Success
             case (:-1)
                 err = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid problem size a=',[lda,n], &
                                                                                    ', b=',[ldb,nrhs])
             case (1:)
                 err = linalg_state_type(this,LINALG_ERROR,'SVD did not converge.')
             case default
                 err = linalg_state_type(this,LINALG_INTERNAL_ERROR,'catastrophic error')
          end select

      end subroutine handle_gelsd_info

     !> Process GEEV output flags
     pure subroutine handle_geev_info(this,err,info,shapea)
        character(len=*), intent(in) :: this
        !> Error handler
        type(linalg_state_type), intent(inout) :: err
        !> GEEV return flag
        integer(ilp), intent(in) :: info
        !> Input matrix size
        integer(ilp), intent(in) :: shapea(2)

        select case (info)
           case (0)
               ! Success!
               err%state = LINALG_SUCCESS
           case (-1)
               err = linalg_state_type(this,LINALG_INTERNAL_ERROR,'Invalid task ID: left eigenvectors.')
           case (-2)
               err = linalg_state_type(this,LINALG_INTERNAL_ERROR,'Invalid task ID: right eigenvectors.')
           case (-5,-3)
               err = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix size: a=',shapea)
           case (-9)
               err = linalg_state_type(this,LINALG_VALUE_ERROR,'insufficient left vector matrix size.')
           case (-11)
               err = linalg_state_type(this,LINALG_VALUE_ERROR,'insufficient right vector matrix size.')
           case (-13)
               err = linalg_state_type(this,LINALG_INTERNAL_ERROR,'Insufficient work array size.')
           case (1:)
               err = linalg_state_type(this,LINALG_ERROR,'Eigenvalue computation did not converge.')
           case default
               err = linalg_state_type(this,LINALG_INTERNAL_ERROR,'Unknown error returned by geev.')
        end select

     end subroutine handle_geev_info

     !> Process GGEV output flags
     pure subroutine handle_ggev_info(this,err,info,shapea,shapeb)
        character(len=*), intent(in) :: this
        !> Error handler
        type(linalg_state_type), intent(inout) :: err
        !> GEEV return flag
        integer(ilp), intent(in) :: info
        !> Input matrix size
        integer(ilp), intent(in) :: shapea(2),shapeb(2)

        select case (info)
           case (0)
               ! Success!
               err%state = LINALG_SUCCESS
           case (-1)
               err = linalg_state_type(this,LINALG_INTERNAL_ERROR,'Invalid task ID: left eigenvectors.')
           case (-2)
               err = linalg_state_type(this,LINALG_INTERNAL_ERROR,'Invalid task ID: right eigenvectors.')
           case (-5,-3)
               err = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix size: a=',shapea)
           case (-7)
               err = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix size: b=',shapeb)               
           case (-12)
               err = linalg_state_type(this,LINALG_VALUE_ERROR,'insufficient left vector matrix size.')
           case (-14)
               err = linalg_state_type(this,LINALG_VALUE_ERROR,'insufficient right vector matrix size.')
           case (-16)
               err = linalg_state_type(this,LINALG_INTERNAL_ERROR,'Insufficient work array size.')
           case (1:)
               err = linalg_state_type(this,LINALG_ERROR,'Eigenvalue computation did not converge.')
           case default
               err = linalg_state_type(this,LINALG_INTERNAL_ERROR,'Unknown error returned by ggev.')
        end select

     end subroutine handle_ggev_info

     !> Process SYEV/HEEV output flags
     elemental subroutine handle_heev_info(this,err,info,m,n)
        character(len=*), intent(in) :: this
        !> Error handler
        type(linalg_state_type), intent(inout) :: err
        !> SYEV/HEEV return flag
        integer(ilp), intent(in) :: info
        !> Input matrix size
        integer(ilp), intent(in) :: m,n

        select case (info)
           case (0)
               ! Success!
               err%state = LINALG_SUCCESS
           case (-1)
               err = linalg_state_type(this,LINALG_INTERNAL_ERROR,'Invalid eigenvector request.')
           case (-2)
               err = linalg_state_type(this,LINALG_INTERNAL_ERROR,'Invalid triangular section request.')
           case (-5,-3)
               err = linalg_state_type(this,LINALG_VALUE_ERROR,'invalid matrix size: a=',[m,n])
           case (-8)
               err = linalg_state_type(this,LINALG_INTERNAL_ERROR,'insufficient workspace size.')
           case (1:)
               err = linalg_state_type(this,LINALG_ERROR,'Eigenvalue computation did not converge.')
           case default
               err = linalg_state_type(this,LINALG_INTERNAL_ERROR,'Unknown error returned by syev/heev.')
        end select

     end subroutine handle_heev_info

end module stdlib_linalg_lapack_aux
