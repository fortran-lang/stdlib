submodule(stdlib_lapack_base) stdlib_lapack_blas_like_l2
  implicit none


  contains

     pure module subroutine stdlib_slascl( type, kl, ku, cfrom, cto, m, n, a, lda, info )
     !! SLASCL multiplies the M by N real matrix A by the real scalar
     !! CTO/CFROM.  This is done without over/underflow as long as the final
     !! result CTO*A(I,J)/CFROM does not over/underflow. TYPE specifies that
     !! A may be full, upper triangular, lower triangular, upper Hessenberg,
     !! or banded.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: zero, half, one
           ! Scalar Arguments 
           character, intent(in) :: type
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kl, ku, lda, m, n
           real(sp), intent(in) :: cfrom, cto
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: done
           integer(ilp) :: i, itype, j, k1, k2, k3, k4
           real(sp) :: bignum, cfrom1, cfromc, cto1, ctoc, mul, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           if( stdlib_lsame( type, 'G' ) ) then
              itype = 0_ilp
           else if( stdlib_lsame( type, 'L' ) ) then
              itype = 1_ilp
           else if( stdlib_lsame( type, 'U' ) ) then
              itype = 2_ilp
           else if( stdlib_lsame( type, 'H' ) ) then
              itype = 3_ilp
           else if( stdlib_lsame( type, 'B' ) ) then
              itype = 4_ilp
           else if( stdlib_lsame( type, 'Q' ) ) then
              itype = 5_ilp
           else if( stdlib_lsame( type, 'Z' ) ) then
              itype = 6_ilp
           else
              itype = -1_ilp
           end if
           if( itype==-1_ilp ) then
              info = -1_ilp
           else if( cfrom==zero .or. stdlib_sisnan(cfrom) ) then
              info = -4_ilp
           else if( stdlib_sisnan(cto) ) then
              info = -5_ilp
           else if( m<0_ilp ) then
              info = -6_ilp
           else if( n<0_ilp .or. ( itype==4_ilp .and. n/=m ) .or.( itype==5_ilp .and. n/=m ) ) then
              info = -7_ilp
           else if( itype<=3_ilp .and. lda<max( 1_ilp, m ) ) then
              info = -9_ilp
           else if( itype>=4_ilp ) then
              if( kl<0_ilp .or. kl>max( m-1, 0_ilp ) ) then
                 info = -2_ilp
              else if( ku<0_ilp .or. ku>max( n-1, 0_ilp ) .or.( ( itype==4_ilp .or. itype==5_ilp ) .and. kl/=ku ) &
                        )then
                 info = -3_ilp
              else if( ( itype==4_ilp .and. lda<kl+1 ) .or.( itype==5_ilp .and. lda<ku+1 ) .or.( itype==6_ilp &
                        .and. lda<2_ilp*kl+ku+1 ) ) then
                 info = -9_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SLASCL', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. m==0 )return
           ! get machine parameters
           smlnum = stdlib_slamch( 'S' )
           bignum = one / smlnum
           cfromc = cfrom
           ctoc = cto
           10 continue
           cfrom1 = cfromc*smlnum
           if( cfrom1==cfromc ) then
              ! cfromc is an inf.  multiply by a correctly signed zero for
              ! finite ctoc, or a nan if ctoc is infinite.
              mul = ctoc / cfromc
              done = .true.
              cto1 = ctoc
           else
              cto1 = ctoc / bignum
              if( cto1==ctoc ) then
                 ! ctoc is either 0 or an inf.  in both cases, ctoc itself
                 ! serves as the correct multiplication factor.
                 mul = ctoc
                 done = .true.
                 cfromc = one
              else if( abs( cfrom1 )>abs( ctoc ) .and. ctoc/=zero ) then
                 mul = smlnum
                 done = .false.
                 cfromc = cfrom1
              else if( abs( cto1 )>abs( cfromc ) ) then
                 mul = bignum
                 done = .false.
                 ctoc = cto1
              else
                 mul = ctoc / cfromc
                 done = .true.
              end if
           end if
           if( itype==0_ilp ) then
              ! full matrix
              do j = 1, n
                 do i = 1, m
                    a( i, j ) = a( i, j )*mul
                 end do
              end do
           else if( itype==1_ilp ) then
              ! lower triangular matrix
              do j = 1, n
                 do i = j, m
                    a( i, j ) = a( i, j )*mul
                 end do
              end do
           else if( itype==2_ilp ) then
              ! upper triangular matrix
              do j = 1, n
                 do i = 1, min( j, m )
                    a( i, j ) = a( i, j )*mul
                 end do
              end do
           else if( itype==3_ilp ) then
              ! upper hessenberg matrix
              do j = 1, n
                 do i = 1, min( j+1, m )
                    a( i, j ) = a( i, j )*mul
                 end do
              end do
           else if( itype==4_ilp ) then
              ! lower half of a symmetric band matrix
              k3 = kl + 1_ilp
              k4 = n + 1_ilp
              do j = 1, n
                 do i = 1, min( k3, k4-j )
                    a( i, j ) = a( i, j )*mul
                 end do
              end do
           else if( itype==5_ilp ) then
              ! upper half of a symmetric band matrix
              k1 = ku + 2_ilp
              k3 = ku + 1_ilp
              do j = 1, n
                 do i = max( k1-j, 1 ), k3
                    a( i, j ) = a( i, j )*mul
                 end do
              end do
           else if( itype==6_ilp ) then
              ! band matrix
              k1 = kl + ku + 2_ilp
              k2 = kl + 1_ilp
              k3 = 2_ilp*kl + ku + 1_ilp
              k4 = kl + ku + 1_ilp + m
              do j = 1, n
                 do i = max( k1-j, k2 ), min( k3, k4-j )
                    a( i, j ) = a( i, j )*mul
                 end do
              end do
           end if
           if( .not.done )go to 10
           return
     end subroutine stdlib_slascl

     pure module subroutine stdlib_dlascl( type, kl, ku, cfrom, cto, m, n, a, lda, info )
     !! DLASCL multiplies the M by N real matrix A by the real scalar
     !! CTO/CFROM.  This is done without over/underflow as long as the final
     !! result CTO*A(I,J)/CFROM does not over/underflow. TYPE specifies that
     !! A may be full, upper triangular, lower triangular, upper Hessenberg,
     !! or banded.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: zero, half, one
           ! Scalar Arguments 
           character, intent(in) :: type
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kl, ku, lda, m, n
           real(dp), intent(in) :: cfrom, cto
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: done
           integer(ilp) :: i, itype, j, k1, k2, k3, k4
           real(dp) :: bignum, cfrom1, cfromc, cto1, ctoc, mul, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           if( stdlib_lsame( type, 'G' ) ) then
              itype = 0_ilp
           else if( stdlib_lsame( type, 'L' ) ) then
              itype = 1_ilp
           else if( stdlib_lsame( type, 'U' ) ) then
              itype = 2_ilp
           else if( stdlib_lsame( type, 'H' ) ) then
              itype = 3_ilp
           else if( stdlib_lsame( type, 'B' ) ) then
              itype = 4_ilp
           else if( stdlib_lsame( type, 'Q' ) ) then
              itype = 5_ilp
           else if( stdlib_lsame( type, 'Z' ) ) then
              itype = 6_ilp
           else
              itype = -1_ilp
           end if
           if( itype==-1_ilp ) then
              info = -1_ilp
           else if( cfrom==zero .or. stdlib_disnan(cfrom) ) then
              info = -4_ilp
           else if( stdlib_disnan(cto) ) then
              info = -5_ilp
           else if( m<0_ilp ) then
              info = -6_ilp
           else if( n<0_ilp .or. ( itype==4_ilp .and. n/=m ) .or.( itype==5_ilp .and. n/=m ) ) then
              info = -7_ilp
           else if( itype<=3_ilp .and. lda<max( 1_ilp, m ) ) then
              info = -9_ilp
           else if( itype>=4_ilp ) then
              if( kl<0_ilp .or. kl>max( m-1, 0_ilp ) ) then
                 info = -2_ilp
              else if( ku<0_ilp .or. ku>max( n-1, 0_ilp ) .or.( ( itype==4_ilp .or. itype==5_ilp ) .and. kl/=ku ) &
                        )then
                 info = -3_ilp
              else if( ( itype==4_ilp .and. lda<kl+1 ) .or.( itype==5_ilp .and. lda<ku+1 ) .or.( itype==6_ilp &
                        .and. lda<2_ilp*kl+ku+1 ) ) then
                 info = -9_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DLASCL', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. m==0 )return
           ! get machine parameters
           smlnum = stdlib_dlamch( 'S' )
           bignum = one / smlnum
           cfromc = cfrom
           ctoc = cto
           10 continue
           cfrom1 = cfromc*smlnum
           if( cfrom1==cfromc ) then
              ! cfromc is an inf.  multiply by a correctly signed zero for
              ! finite ctoc, or a nan if ctoc is infinite.
              mul = ctoc / cfromc
              done = .true.
              cto1 = ctoc
           else
              cto1 = ctoc / bignum
              if( cto1==ctoc ) then
                 ! ctoc is either 0 or an inf.  in both cases, ctoc itself
                 ! serves as the correct multiplication factor.
                 mul = ctoc
                 done = .true.
                 cfromc = one
              else if( abs( cfrom1 )>abs( ctoc ) .and. ctoc/=zero ) then
                 mul = smlnum
                 done = .false.
                 cfromc = cfrom1
              else if( abs( cto1 )>abs( cfromc ) ) then
                 mul = bignum
                 done = .false.
                 ctoc = cto1
              else
                 mul = ctoc / cfromc
                 done = .true.
              end if
           end if
           if( itype==0_ilp ) then
              ! full matrix
              do j = 1, n
                 do i = 1, m
                    a( i, j ) = a( i, j )*mul
                 end do
              end do
           else if( itype==1_ilp ) then
              ! lower triangular matrix
              do j = 1, n
                 do i = j, m
                    a( i, j ) = a( i, j )*mul
                 end do
              end do
           else if( itype==2_ilp ) then
              ! upper triangular matrix
              do j = 1, n
                 do i = 1, min( j, m )
                    a( i, j ) = a( i, j )*mul
                 end do
              end do
           else if( itype==3_ilp ) then
              ! upper hessenberg matrix
              do j = 1, n
                 do i = 1, min( j+1, m )
                    a( i, j ) = a( i, j )*mul
                 end do
              end do
           else if( itype==4_ilp ) then
              ! lower half of a symmetric band matrix
              k3 = kl + 1_ilp
              k4 = n + 1_ilp
              do j = 1, n
                 do i = 1, min( k3, k4-j )
                    a( i, j ) = a( i, j )*mul
                 end do
              end do
           else if( itype==5_ilp ) then
              ! upper half of a symmetric band matrix
              k1 = ku + 2_ilp
              k3 = ku + 1_ilp
              do j = 1, n
                 do i = max( k1-j, 1 ), k3
                    a( i, j ) = a( i, j )*mul
                 end do
              end do
           else if( itype==6_ilp ) then
              ! band matrix
              k1 = kl + ku + 2_ilp
              k2 = kl + 1_ilp
              k3 = 2_ilp*kl + ku + 1_ilp
              k4 = kl + ku + 1_ilp + m
              do j = 1, n
                 do i = max( k1-j, k2 ), min( k3, k4-j )
                    a( i, j ) = a( i, j )*mul
                 end do
              end do
           end if
           if( .not.done )go to 10
           return
     end subroutine stdlib_dlascl


     pure module subroutine stdlib_clascl( type, kl, ku, cfrom, cto, m, n, a, lda, info )
     !! CLASCL multiplies the M by N complex matrix A by the real scalar
     !! CTO/CFROM.  This is done without over/underflow as long as the final
     !! result CTO*A(I,J)/CFROM does not over/underflow. TYPE specifies that
     !! A may be full, upper triangular, lower triangular, upper Hessenberg,
     !! or banded.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: zero, half, one
           ! Scalar Arguments 
           character, intent(in) :: type
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kl, ku, lda, m, n
           real(sp), intent(in) :: cfrom, cto
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: done
           integer(ilp) :: i, itype, j, k1, k2, k3, k4
           real(sp) :: bignum, cfrom1, cfromc, cto1, ctoc, mul, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           if( stdlib_lsame( type, 'G' ) ) then
              itype = 0_ilp
           else if( stdlib_lsame( type, 'L' ) ) then
              itype = 1_ilp
           else if( stdlib_lsame( type, 'U' ) ) then
              itype = 2_ilp
           else if( stdlib_lsame( type, 'H' ) ) then
              itype = 3_ilp
           else if( stdlib_lsame( type, 'B' ) ) then
              itype = 4_ilp
           else if( stdlib_lsame( type, 'Q' ) ) then
              itype = 5_ilp
           else if( stdlib_lsame( type, 'Z' ) ) then
              itype = 6_ilp
           else
              itype = -1_ilp
           end if
           if( itype==-1_ilp ) then
              info = -1_ilp
           else if( cfrom==zero .or. stdlib_sisnan(cfrom) ) then
              info = -4_ilp
           else if( stdlib_sisnan(cto) ) then
              info = -5_ilp
           else if( m<0_ilp ) then
              info = -6_ilp
           else if( n<0_ilp .or. ( itype==4_ilp .and. n/=m ) .or.( itype==5_ilp .and. n/=m ) ) then
              info = -7_ilp
           else if( itype<=3_ilp .and. lda<max( 1_ilp, m ) ) then
              info = -9_ilp
           else if( itype>=4_ilp ) then
              if( kl<0_ilp .or. kl>max( m-1, 0_ilp ) ) then
                 info = -2_ilp
              else if( ku<0_ilp .or. ku>max( n-1, 0_ilp ) .or.( ( itype==4_ilp .or. itype==5_ilp ) .and. kl/=ku ) &
                        )then
                 info = -3_ilp
              else if( ( itype==4_ilp .and. lda<kl+1 ) .or.( itype==5_ilp .and. lda<ku+1 ) .or.( itype==6_ilp &
                        .and. lda<2_ilp*kl+ku+1 ) ) then
                 info = -9_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CLASCL', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. m==0 )return
           ! get machine parameters
           smlnum = stdlib_slamch( 'S' )
           bignum = one / smlnum
           cfromc = cfrom
           ctoc = cto
           10 continue
           cfrom1 = cfromc*smlnum
           if( cfrom1==cfromc ) then
              ! cfromc is an inf.  multiply by a correctly signed zero for
              ! finite ctoc, or a nan if ctoc is infinite.
              mul = ctoc / cfromc
              done = .true.
              cto1 = ctoc
           else
              cto1 = ctoc / bignum
              if( cto1==ctoc ) then
                 ! ctoc is either 0 or an inf.  in both cases, ctoc itself
                 ! serves as the correct multiplication factor.
                 mul = ctoc
                 done = .true.
                 cfromc = one
              else if( abs( cfrom1 )>abs( ctoc ) .and. ctoc/=zero ) then
                 mul = smlnum
                 done = .false.
                 cfromc = cfrom1
              else if( abs( cto1 )>abs( cfromc ) ) then
                 mul = bignum
                 done = .false.
                 ctoc = cto1
              else
                 mul = ctoc / cfromc
                 done = .true.
              end if
           end if
           if( itype==0_ilp ) then
              ! full matrix
              do j = 1, n
                 do i = 1, m
                    a( i, j ) = a( i, j )*mul
                 end do
              end do
           else if( itype==1_ilp ) then
              ! lower triangular matrix
              do j = 1, n
                 do i = j, m
                    a( i, j ) = a( i, j )*mul
                 end do
              end do
           else if( itype==2_ilp ) then
              ! upper triangular matrix
              do j = 1, n
                 do i = 1, min( j, m )
                    a( i, j ) = a( i, j )*mul
                 end do
              end do
           else if( itype==3_ilp ) then
              ! upper hessenberg matrix
              do j = 1, n
                 do i = 1, min( j+1, m )
                    a( i, j ) = a( i, j )*mul
                 end do
              end do
           else if( itype==4_ilp ) then
              ! lower chalf of a symmetric band matrix
              k3 = kl + 1_ilp
              k4 = n + 1_ilp
              do j = 1, n
                 do i = 1, min( k3, k4-j )
                    a( i, j ) = a( i, j )*mul
                 end do
              end do
           else if( itype==5_ilp ) then
              ! upper chalf of a symmetric band matrix
              k1 = ku + 2_ilp
              k3 = ku + 1_ilp
              do j = 1, n
                 do i = max( k1-j, 1 ), k3
                    a( i, j ) = a( i, j )*mul
                 end do
              end do
           else if( itype==6_ilp ) then
              ! band matrix
              k1 = kl + ku + 2_ilp
              k2 = kl + 1_ilp
              k3 = 2_ilp*kl + ku + 1_ilp
              k4 = kl + ku + 1_ilp + m
              do j = 1, n
                 do i = max( k1-j, k2 ), min( k3, k4-j )
                    a( i, j ) = a( i, j )*mul
                 end do
              end do
           end if
           if( .not.done )go to 10
           return
     end subroutine stdlib_clascl

     pure module subroutine stdlib_zlascl( type, kl, ku, cfrom, cto, m, n, a, lda, info )
     !! ZLASCL multiplies the M by N complex matrix A by the real scalar
     !! CTO/CFROM.  This is done without over/underflow as long as the final
     !! result CTO*A(I,J)/CFROM does not over/underflow. TYPE specifies that
     !! A may be full, upper triangular, lower triangular, upper Hessenberg,
     !! or banded.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: zero, half, one
           ! Scalar Arguments 
           character, intent(in) :: type
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kl, ku, lda, m, n
           real(dp), intent(in) :: cfrom, cto
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: done
           integer(ilp) :: i, itype, j, k1, k2, k3, k4
           real(dp) :: bignum, cfrom1, cfromc, cto1, ctoc, mul, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           if( stdlib_lsame( type, 'G' ) ) then
              itype = 0_ilp
           else if( stdlib_lsame( type, 'L' ) ) then
              itype = 1_ilp
           else if( stdlib_lsame( type, 'U' ) ) then
              itype = 2_ilp
           else if( stdlib_lsame( type, 'H' ) ) then
              itype = 3_ilp
           else if( stdlib_lsame( type, 'B' ) ) then
              itype = 4_ilp
           else if( stdlib_lsame( type, 'Q' ) ) then
              itype = 5_ilp
           else if( stdlib_lsame( type, 'Z' ) ) then
              itype = 6_ilp
           else
              itype = -1_ilp
           end if
           if( itype==-1_ilp ) then
              info = -1_ilp
           else if( cfrom==zero .or. stdlib_disnan(cfrom) ) then
              info = -4_ilp
           else if( stdlib_disnan(cto) ) then
              info = -5_ilp
           else if( m<0_ilp ) then
              info = -6_ilp
           else if( n<0_ilp .or. ( itype==4_ilp .and. n/=m ) .or.( itype==5_ilp .and. n/=m ) ) then
              info = -7_ilp
           else if( itype<=3_ilp .and. lda<max( 1_ilp, m ) ) then
              info = -9_ilp
           else if( itype>=4_ilp ) then
              if( kl<0_ilp .or. kl>max( m-1, 0_ilp ) ) then
                 info = -2_ilp
              else if( ku<0_ilp .or. ku>max( n-1, 0_ilp ) .or.( ( itype==4_ilp .or. itype==5_ilp ) .and. kl/=ku ) &
                        )then
                 info = -3_ilp
              else if( ( itype==4_ilp .and. lda<kl+1 ) .or.( itype==5_ilp .and. lda<ku+1 ) .or.( itype==6_ilp &
                        .and. lda<2_ilp*kl+ku+1 ) ) then
                 info = -9_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZLASCL', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. m==0 )return
           ! get machine parameters
           smlnum = stdlib_dlamch( 'S' )
           bignum = one / smlnum
           cfromc = cfrom
           ctoc = cto
           10 continue
           cfrom1 = cfromc*smlnum
           if( cfrom1==cfromc ) then
              ! cfromc is an inf.  multiply by a correctly signed zero for
              ! finite ctoc, or a nan if ctoc is infinite.
              mul = ctoc / cfromc
              done = .true.
              cto1 = ctoc
           else
              cto1 = ctoc / bignum
              if( cto1==ctoc ) then
                 ! ctoc is either 0 or an inf.  in both cases, ctoc itself
                 ! serves as the correct multiplication factor.
                 mul = ctoc
                 done = .true.
                 cfromc = one
              else if( abs( cfrom1 )>abs( ctoc ) .and. ctoc/=zero ) then
                 mul = smlnum
                 done = .false.
                 cfromc = cfrom1
              else if( abs( cto1 )>abs( cfromc ) ) then
                 mul = bignum
                 done = .false.
                 ctoc = cto1
              else
                 mul = ctoc / cfromc
                 done = .true.
              end if
           end if
           if( itype==0_ilp ) then
              ! full matrix
              do j = 1, n
                 do i = 1, m
                    a( i, j ) = a( i, j )*mul
                 end do
              end do
           else if( itype==1_ilp ) then
              ! lower triangular matrix
              do j = 1, n
                 do i = j, m
                    a( i, j ) = a( i, j )*mul
                 end do
              end do
           else if( itype==2_ilp ) then
              ! upper triangular matrix
              do j = 1, n
                 do i = 1, min( j, m )
                    a( i, j ) = a( i, j )*mul
                 end do
              end do
           else if( itype==3_ilp ) then
              ! upper hessenberg matrix
              do j = 1, n
                 do i = 1, min( j+1, m )
                    a( i, j ) = a( i, j )*mul
                 end do
              end do
           else if( itype==4_ilp ) then
              ! lower chalf of a symmetric band matrix
              k3 = kl + 1_ilp
              k4 = n + 1_ilp
              do j = 1, n
                 do i = 1, min( k3, k4-j )
                    a( i, j ) = a( i, j )*mul
                 end do
              end do
           else if( itype==5_ilp ) then
              ! upper chalf of a symmetric band matrix
              k1 = ku + 2_ilp
              k3 = ku + 1_ilp
              do j = 1, n
                 do i = max( k1-j, 1 ), k3
                    a( i, j ) = a( i, j )*mul
                 end do
              end do
           else if( itype==6_ilp ) then
              ! band matrix
              k1 = kl + ku + 2_ilp
              k2 = kl + 1_ilp
              k3 = 2_ilp*kl + ku + 1_ilp
              k4 = kl + ku + 1_ilp + m
              do j = 1, n
                 do i = max( k1-j, k2 ), min( k3, k4-j )
                    a( i, j ) = a( i, j )*mul
                 end do
              end do
           end if
           if( .not.done )go to 10
           return
     end subroutine stdlib_zlascl




     module subroutine stdlib_sla_geamv( trans, m, n, alpha, a, lda, x, incx, beta,y, incy )
     !! SLA_GEAMV performs one of the matrix-vector operations
     !! y := alpha*abs(A)*abs(x) + beta*abs(y),
     !! or   y := alpha*abs(A)**T*abs(x) + beta*abs(y),
     !! where alpha and beta are scalars, x and y are vectors and A is an
     !! m by n matrix.
     !! This function is primarily used in calculating error bounds.
     !! To protect against underflow during evaluation, components in
     !! the resulting vector are perturbed away from zero by (N+1)
     !! times the underflow threshold.  To prevent unnecessarily large
     !! errors for block-structure embedded in general matrices,
     !! "symbolically" zero components are not perturbed.  A zero
     !! entry is considered "symbolic" if all multiplications involved
     !! in computing that entry have at least one zero multiplicand.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           real(sp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, lda, m, n, trans
           ! Array Arguments 
           real(sp), intent(in) :: a(lda,*), x(*)
           real(sp), intent(inout) :: y(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: symb_zero
           real(sp) :: temp, safe1
           integer(ilp) :: i, info, iy, j, jx, kx, ky, lenx, leny
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if     ( .not.( ( trans==stdlib_ilatrans( 'N' ) ).or. ( trans==stdlib_ilatrans( 'T' ) )&
                     .or. ( trans==stdlib_ilatrans( 'C' )) ) ) then
              info = 1_ilp
           else if( m<0_ilp )then
              info = 2_ilp
           else if( n<0_ilp )then
              info = 3_ilp
           else if( lda<max( 1_ilp, m ) )then
              info = 6_ilp
           else if( incx==0_ilp )then
              info = 8_ilp
           else if( incy==0_ilp )then
              info = 11_ilp
           end if
           if( info/=0_ilp )then
              call stdlib_xerbla( 'SLA_GEAMV ', info )
              return
           end if
           ! quick return if possible.
           if( ( m==0 ).or.( n==0 ).or.( ( alpha==zero ).and.( beta==one ) ) )return
           ! set  lenx  and  leny, the lengths of the vectors x and y, and set
           ! up the start points in  x  and  y.
           if( trans==stdlib_ilatrans( 'N' ) )then
              lenx = n
              leny = m
           else
              lenx = m
              leny = n
           end if
           if( incx>0_ilp )then
              kx = 1_ilp
           else
              kx = 1_ilp - ( lenx - 1_ilp )*incx
           end if
           if( incy>0_ilp )then
              ky = 1_ilp
           else
              ky = 1_ilp - ( leny - 1_ilp )*incy
           end if
           ! set safe1 essentially to be the underflow threshold times the
           ! number of additions in each row.
           safe1 = stdlib_slamch( 'SAFE MINIMUM' )
           safe1 = (n+1)*safe1
           ! form  y := alpha*abs(a)*abs(x) + beta*abs(y).
           ! the o(m*n) symb_zero tests could be replaced by o(n) queries to
           ! the inexact flag.  still doesn't help change the iteration order
           ! to per-column.
           iy = ky
           if ( incx==1_ilp ) then
              if( trans==stdlib_ilatrans( 'N' ) )then
                 do i = 1, leny
                    if ( beta == zero ) then
                       symb_zero = .true.
                       y( iy ) = zero
                    else if ( y( iy ) == zero ) then
                       symb_zero = .true.
                    else
                       symb_zero = .false.
                       y( iy ) = beta * abs( y( iy ) )
                    end if
                    if ( alpha /= zero ) then
                       do j = 1, lenx
                          temp = abs( a( i, j ) )
                          symb_zero = symb_zero .and.( x( j ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*abs( x( j ) )*temp
                       end do
                    end if
                    if ( .not.symb_zero )y( iy ) = y( iy ) + sign( safe1, y( iy ) )
                    iy = iy + incy
                 end do
              else
                 do i = 1, leny
                    if ( beta == zero ) then
                       symb_zero = .true.
                       y( iy ) = zero
                    else if ( y( iy ) == zero ) then
                       symb_zero = .true.
                    else
                       symb_zero = .false.
                       y( iy ) = beta * abs( y( iy ) )
                    end if
                    if ( alpha /= zero ) then
                       do j = 1, lenx
                          temp = abs( a( j, i ) )
                          symb_zero = symb_zero .and.( x( j ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*abs( x( j ) )*temp
                       end do
                    end if
                    if ( .not.symb_zero )y( iy ) = y( iy ) + sign( safe1, y( iy ) )
                    iy = iy + incy
                 end do
              end if
           else
              if( trans==stdlib_ilatrans( 'N' ) )then
                 do i = 1, leny
                    if ( beta == zero ) then
                       symb_zero = .true.
                       y( iy ) = zero
                    else if ( y( iy ) == zero ) then
                       symb_zero = .true.
                    else
                       symb_zero = .false.
                       y( iy ) = beta * abs( y( iy ) )
                    end if
                    if ( alpha /= zero ) then
                       jx = kx
                       do j = 1, lenx
                          temp = abs( a( i, j ) )
                          symb_zero = symb_zero .and.( x( jx ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*abs( x( jx ) )*temp
                          jx = jx + incx
                       end do
                    end if
                    if (.not.symb_zero)y( iy ) = y( iy ) + sign( safe1, y( iy ) )
                    iy = iy + incy
                 end do
              else
                 do i = 1, leny
                    if ( beta == zero ) then
                       symb_zero = .true.
                       y( iy ) = zero
                    else if ( y( iy ) == zero ) then
                       symb_zero = .true.
                    else
                       symb_zero = .false.
                       y( iy ) = beta * abs( y( iy ) )
                    end if
                    if ( alpha /= zero ) then
                       jx = kx
                       do j = 1, lenx
                          temp = abs( a( j, i ) )
                          symb_zero = symb_zero .and.( x( jx ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*abs( x( jx ) )*temp
                          jx = jx + incx
                       end do
                    end if
                    if (.not.symb_zero)y( iy ) = y( iy ) + sign( safe1, y( iy ) )
                    iy = iy + incy
                 end do
              end if
           end if
           return
     end subroutine stdlib_sla_geamv

     module subroutine stdlib_dla_geamv ( trans, m, n, alpha, a, lda, x, incx, beta,y, incy )
     !! DLA_GEAMV performs one of the matrix-vector operations
     !! y := alpha*abs(A)*abs(x) + beta*abs(y),
     !! or   y := alpha*abs(A)**T*abs(x) + beta*abs(y),
     !! where alpha and beta are scalars, x and y are vectors and A is an
     !! m by n matrix.
     !! This function is primarily used in calculating error bounds.
     !! To protect against underflow during evaluation, components in
     !! the resulting vector are perturbed away from zero by (N+1)
     !! times the underflow threshold.  To prevent unnecessarily large
     !! errors for block-structure embedded in general matrices,
     !! "symbolically" zero components are not perturbed.  A zero
     !! entry is considered "symbolic" if all multiplications involved
     !! in computing that entry have at least one zero multiplicand.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           real(dp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, lda, m, n, trans
           ! Array Arguments 
           real(dp), intent(in) :: a(lda,*), x(*)
           real(dp), intent(inout) :: y(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: symb_zero
           real(dp) :: temp, safe1
           integer(ilp) :: i, info, iy, j, jx, kx, ky, lenx, leny
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if     ( .not.( ( trans==stdlib_ilatrans( 'N' ) ).or. ( trans==stdlib_ilatrans( 'T' ) )&
                     .or. ( trans==stdlib_ilatrans( 'C' )) ) ) then
              info = 1_ilp
           else if( m<0_ilp )then
              info = 2_ilp
           else if( n<0_ilp )then
              info = 3_ilp
           else if( lda<max( 1_ilp, m ) )then
              info = 6_ilp
           else if( incx==0_ilp )then
              info = 8_ilp
           else if( incy==0_ilp )then
              info = 11_ilp
           end if
           if( info/=0_ilp )then
              call stdlib_xerbla( 'DLA_GEAMV ', info )
              return
           end if
           ! quick return if possible.
           if( ( m==0 ).or.( n==0 ).or.( ( alpha==zero ).and.( beta==one ) ) )return
           ! set  lenx  and  leny, the lengths of the vectors x and y, and set
           ! up the start points in  x  and  y.
           if( trans==stdlib_ilatrans( 'N' ) )then
              lenx = n
              leny = m
           else
              lenx = m
              leny = n
           end if
           if( incx>0_ilp )then
              kx = 1_ilp
           else
              kx = 1_ilp - ( lenx - 1_ilp )*incx
           end if
           if( incy>0_ilp )then
              ky = 1_ilp
           else
              ky = 1_ilp - ( leny - 1_ilp )*incy
           end if
           ! set safe1 essentially to be the underflow threshold times the
           ! number of additions in each row.
           safe1 = stdlib_dlamch( 'SAFE MINIMUM' )
           safe1 = (n+1)*safe1
           ! form  y := alpha*abs(a)*abs(x) + beta*abs(y).
           ! the o(m*n) symb_zero tests could be replaced by o(n) queries to
           ! the inexact flag.  still doesn't help change the iteration order
           ! to per-column.
           iy = ky
           if ( incx==1_ilp ) then
              if( trans==stdlib_ilatrans( 'N' ) )then
                 do i = 1, leny
                    if ( beta == zero ) then
                       symb_zero = .true.
                       y( iy ) = zero
                    else if ( y( iy ) == zero ) then
                       symb_zero = .true.
                    else
                       symb_zero = .false.
                       y( iy ) = beta * abs( y( iy ) )
                    end if
                    if ( alpha /= zero ) then
                       do j = 1, lenx
                          temp = abs( a( i, j ) )
                          symb_zero = symb_zero .and.( x( j ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*abs( x( j ) )*temp
                       end do
                    end if
                    if ( .not.symb_zero )y( iy ) = y( iy ) + sign( safe1, y( iy ) )
                    iy = iy + incy
                 end do
              else
                 do i = 1, leny
                    if ( beta == zero ) then
                       symb_zero = .true.
                       y( iy ) = zero
                    else if ( y( iy ) == zero ) then
                       symb_zero = .true.
                    else
                       symb_zero = .false.
                       y( iy ) = beta * abs( y( iy ) )
                    end if
                    if ( alpha /= zero ) then
                       do j = 1, lenx
                          temp = abs( a( j, i ) )
                          symb_zero = symb_zero .and.( x( j ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*abs( x( j ) )*temp
                       end do
                    end if
                    if ( .not.symb_zero )y( iy ) = y( iy ) + sign( safe1, y( iy ) )
                    iy = iy + incy
                 end do
              end if
           else
              if( trans==stdlib_ilatrans( 'N' ) )then
                 do i = 1, leny
                    if ( beta == zero ) then
                       symb_zero = .true.
                       y( iy ) = zero
                    else if ( y( iy ) == zero ) then
                       symb_zero = .true.
                    else
                       symb_zero = .false.
                       y( iy ) = beta * abs( y( iy ) )
                    end if
                    if ( alpha /= zero ) then
                       jx = kx
                       do j = 1, lenx
                          temp = abs( a( i, j ) )
                          symb_zero = symb_zero .and.( x( jx ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*abs( x( jx ) )*temp
                          jx = jx + incx
                       end do
                    end if
                    if (.not.symb_zero)y( iy ) = y( iy ) + sign( safe1, y( iy ) )
                    iy = iy + incy
                 end do
              else
                 do i = 1, leny
                    if ( beta == zero ) then
                       symb_zero = .true.
                       y( iy ) = zero
                    else if ( y( iy ) == zero ) then
                       symb_zero = .true.
                    else
                       symb_zero = .false.
                       y( iy ) = beta * abs( y( iy ) )
                    end if
                    if ( alpha /= zero ) then
                       jx = kx
                       do j = 1, lenx
                          temp = abs( a( j, i ) )
                          symb_zero = symb_zero .and.( x( jx ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*abs( x( jx ) )*temp
                          jx = jx + incx
                       end do
                    end if
                    if (.not.symb_zero)y( iy ) = y( iy ) + sign( safe1, y( iy ) )
                    iy = iy + incy
                 end do
              end if
           end if
           return
     end subroutine stdlib_dla_geamv


     module subroutine stdlib_cla_geamv( trans, m, n, alpha, a, lda, x, incx, beta,y, incy )
     !! CLA_GEAMV performs one of the matrix-vector operations
     !! y := alpha*abs(A)*abs(x) + beta*abs(y),
     !! or   y := alpha*abs(A)**T*abs(x) + beta*abs(y),
     !! where alpha and beta are scalars, x and y are vectors and A is an
     !! m by n matrix.
     !! This function is primarily used in calculating error bounds.
     !! To protect against underflow during evaluation, components in
     !! the resulting vector are perturbed away from zero by (N+1)
     !! times the underflow threshold.  To prevent unnecessarily large
     !! errors for block-structure embedded in general matrices,
     !! "symbolically" zero components are not perturbed.  A zero
     !! entry is considered "symbolic" if all multiplications involved
     !! in computing that entry have at least one zero multiplicand.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           real(sp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, lda, m, n
           integer(ilp), intent(in) :: trans
           ! Array Arguments 
           complex(sp), intent(in) :: a(lda,*), x(*)
           real(sp), intent(inout) :: y(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: symb_zero
           real(sp) :: temp, safe1
           integer(ilp) :: i, info, iy, j, jx, kx, ky, lenx, leny
           complex(sp) :: cdum
           ! Intrinsic Functions 
           ! Statement Functions 
           real(sp) :: cabs1
           ! Statement Function Definitions 
           cabs1( cdum ) = abs( real( cdum,KIND=sp) ) + abs( aimag( cdum ) )
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if     ( .not.( ( trans==stdlib_ilatrans( 'N' ) ).or. ( trans==stdlib_ilatrans( 'T' ) )&
                     .or. ( trans==stdlib_ilatrans( 'C' ) ) ) ) then
              info = 1_ilp
           else if( m<0_ilp )then
              info = 2_ilp
           else if( n<0_ilp )then
              info = 3_ilp
           else if( lda<max( 1_ilp, m ) )then
              info = 6_ilp
           else if( incx==0_ilp )then
              info = 8_ilp
           else if( incy==0_ilp )then
              info = 11_ilp
           end if
           if( info/=0_ilp )then
              call stdlib_xerbla( 'CLA_GEAMV ', info )
              return
           end if
           ! quick return if possible.
           if( ( m==0 ).or.( n==0 ).or.( ( alpha==czero ).and.( beta==cone ) ) )return
           ! set  lenx  and  leny, the lengths of the vectors x and y, and set
           ! up the start points in  x  and  y.
           if( trans==stdlib_ilatrans( 'N' ) )then
              lenx = n
              leny = m
           else
              lenx = m
              leny = n
           end if
           if( incx>0_ilp )then
              kx = 1_ilp
           else
              kx = 1_ilp - ( lenx - 1_ilp )*incx
           end if
           if( incy>0_ilp )then
              ky = 1_ilp
           else
              ky = 1_ilp - ( leny - 1_ilp )*incy
           end if
           ! set safe1 essentially to be the underflow threshold times the
           ! number of additions in each row.
           safe1 = stdlib_slamch( 'SAFE MINIMUM' )
           safe1 = (n+1)*safe1
           ! form  y := alpha*abs(a)*abs(x) + beta*abs(y).
           ! the o(m*n) symb_zero tests could be replaced by o(n) queries to
           ! the inexact flag.  still doesn't help change the iteration order
           ! to per-column.
           iy = ky
           if ( incx==1_ilp ) then
              if( trans==stdlib_ilatrans( 'N' ) )then
                 do i = 1, leny
                    if ( beta == 0.0_sp ) then
                       symb_zero = .true.
                       y( iy ) = czero
                    else if ( y( iy ) == 0.0_sp ) then
                       symb_zero = .true.
                    else
                       symb_zero = .false.
                       y( iy ) = beta * abs( y( iy ) )
                    end if
                    if ( alpha /= 0.0_sp ) then
                       do j = 1, lenx
                          temp = cabs1( a( i, j ) )
                          symb_zero = symb_zero .and.( x( j ) == czero .or. temp == czero )
                                    
                          y( iy ) = y( iy ) + alpha*cabs1( x( j ) )*temp
                       end do
                    end if
                    if ( .not.symb_zero ) y( iy ) =y( iy ) + sign( safe1, y( iy ) )
                    iy = iy + incy
                 end do
              else
                 do i = 1, leny
                    if ( beta == 0.0_sp ) then
                       symb_zero = .true.
                       y( iy ) = czero
                    else if ( y( iy ) == 0.0_sp ) then
                       symb_zero = .true.
                    else
                       symb_zero = .false.
                       y( iy ) = beta * abs( y( iy ) )
                    end if
                    if ( alpha /= 0.0_sp ) then
                       do j = 1, lenx
                          temp = cabs1( a( j, i ) )
                          symb_zero = symb_zero .and.( x( j ) == czero .or. temp == czero )
                                    
                          y( iy ) = y( iy ) + alpha*cabs1( x( j ) )*temp
                       end do
                    end if
                    if ( .not.symb_zero ) y( iy ) =y( iy ) + sign( safe1, y( iy ) )
                    iy = iy + incy
                 end do
              end if
           else
              if( trans==stdlib_ilatrans( 'N' ) )then
                 do i = 1, leny
                    if ( beta == 0.0_sp ) then
                       symb_zero = .true.
                       y( iy ) = czero
                    else if ( y( iy ) == 0.0_sp ) then
                       symb_zero = .true.
                    else
                       symb_zero = .false.
                       y( iy ) = beta * abs( y( iy ) )
                    end if
                    if ( alpha /= 0.0_sp ) then
                       jx = kx
                       do j = 1, lenx
                          temp = cabs1( a( i, j ) )
                          symb_zero = symb_zero .and.( x( jx ) == czero .or. temp == czero )
                                    
                          y( iy ) = y( iy ) + alpha*cabs1( x( jx ) )*temp
                          jx = jx + incx
                       end do
                    end if
                    if ( .not.symb_zero ) y( iy ) =y( iy ) + sign( safe1, y( iy ) )
                    iy = iy + incy
                 end do
              else
                 do i = 1, leny
                    if ( beta == 0.0_sp ) then
                       symb_zero = .true.
                       y( iy ) = czero
                    else if ( y( iy ) == 0.0_sp ) then
                       symb_zero = .true.
                    else
                       symb_zero = .false.
                       y( iy ) = beta * abs( y( iy ) )
                    end if
                    if ( alpha /= 0.0_sp ) then
                       jx = kx
                       do j = 1, lenx
                          temp = cabs1( a( j, i ) )
                          symb_zero = symb_zero .and.( x( jx ) == czero .or. temp == czero )
                                    
                          y( iy ) = y( iy ) + alpha*cabs1( x( jx ) )*temp
                          jx = jx + incx
                       end do
                    end if
                    if ( .not.symb_zero ) y( iy ) =y( iy ) + sign( safe1, y( iy ) )
                    iy = iy + incy
                 end do
              end if
           end if
           return
     end subroutine stdlib_cla_geamv

     module subroutine stdlib_zla_geamv( trans, m, n, alpha, a, lda, x, incx, beta,y, incy )
     !! ZLA_GEAMV performs one of the matrix-vector operations
     !! y := alpha*abs(A)*abs(x) + beta*abs(y),
     !! or   y := alpha*abs(A)**T*abs(x) + beta*abs(y),
     !! where alpha and beta are scalars, x and y are vectors and A is an
     !! m by n matrix.
     !! This function is primarily used in calculating error bounds.
     !! To protect against underflow during evaluation, components in
     !! the resulting vector are perturbed away from zero by (N+1)
     !! times the underflow threshold.  To prevent unnecessarily large
     !! errors for block-structure embedded in general matrices,
     !! "symbolically" zero components are not perturbed.  A zero
     !! entry is considered "symbolic" if all multiplications involved
     !! in computing that entry have at least one zero multiplicand.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           real(dp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, lda, m, n
           integer(ilp), intent(in) :: trans
           ! Array Arguments 
           complex(dp), intent(in) :: a(lda,*), x(*)
           real(dp), intent(inout) :: y(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: symb_zero
           real(dp) :: temp, safe1
           integer(ilp) :: i, info, iy, j, jx, kx, ky, lenx, leny
           complex(dp) :: cdum
           ! Intrinsic Functions 
           ! Statement Functions 
           real(dp) :: cabs1
           ! Statement Function Definitions 
           cabs1( cdum ) = abs( real( cdum,KIND=dp) ) + abs( aimag( cdum ) )
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if     ( .not.( ( trans==stdlib_ilatrans( 'N' ) ).or. ( trans==stdlib_ilatrans( 'T' ) )&
                     .or. ( trans==stdlib_ilatrans( 'C' ) ) ) ) then
              info = 1_ilp
           else if( m<0_ilp )then
              info = 2_ilp
           else if( n<0_ilp )then
              info = 3_ilp
           else if( lda<max( 1_ilp, m ) )then
              info = 6_ilp
           else if( incx==0_ilp )then
              info = 8_ilp
           else if( incy==0_ilp )then
              info = 11_ilp
           end if
           if( info/=0_ilp )then
              call stdlib_xerbla( 'ZLA_GEAMV ', info )
              return
           end if
           ! quick return if possible.
           if( ( m==0 ).or.( n==0 ).or.( ( alpha==czero ).and.( beta==cone ) ) )return
           ! set  lenx  and  leny, the lengths of the vectors x and y, and set
           ! up the start points in  x  and  y.
           if( trans==stdlib_ilatrans( 'N' ) )then
              lenx = n
              leny = m
           else
              lenx = m
              leny = n
           end if
           if( incx>0_ilp )then
              kx = 1_ilp
           else
              kx = 1_ilp - ( lenx - 1_ilp )*incx
           end if
           if( incy>0_ilp )then
              ky = 1_ilp
           else
              ky = 1_ilp - ( leny - 1_ilp )*incy
           end if
           ! set safe1 essentially to be the underflow threshold times the
           ! number of additions in each row.
           safe1 = stdlib_dlamch( 'SAFE MINIMUM' )
           safe1 = (n+1)*safe1
           ! form  y := alpha*abs(a)*abs(x) + beta*abs(y).
           ! the o(m*n) symb_zero tests could be replaced by o(n) queries to
           ! the inexact flag.  still doesn't help change the iteration order
           ! to per-column.
           iy = ky
           if ( incx==1_ilp ) then
              if( trans==stdlib_ilatrans( 'N' ) )then
                 do i = 1, leny
                    if ( beta == czero ) then
                       symb_zero = .true.
                       y( iy ) = czero
                    else if ( y( iy ) == czero ) then
                       symb_zero = .true.
                    else
                       symb_zero = .false.
                       y( iy ) = beta * abs( y( iy ) )
                    end if
                    if ( alpha /= czero ) then
                       do j = 1, lenx
                          temp = cabs1( a( i, j ) )
                          symb_zero = symb_zero .and.( x( j ) == czero .or. temp == czero )
                                    
                          y( iy ) = y( iy ) + alpha*cabs1( x( j ) )*temp
                       end do
                    end if
                    if ( .not.symb_zero ) y( iy ) =y( iy ) + sign( safe1, y( iy ) )
                    iy = iy + incy
                 end do
              else
                 do i = 1, leny
                    if ( beta == czero ) then
                       symb_zero = .true.
                       y( iy ) = czero
                    else if ( y( iy ) == czero ) then
                       symb_zero = .true.
                    else
                       symb_zero = .false.
                       y( iy ) = beta * abs( y( iy ) )
                    end if
                    if ( alpha /= czero ) then
                       do j = 1, lenx
                          temp = cabs1( a( j, i ) )
                          symb_zero = symb_zero .and.( x( j ) == czero .or. temp == czero )
                                    
                          y( iy ) = y( iy ) + alpha*cabs1( x( j ) )*temp
                       end do
                    end if
                    if ( .not.symb_zero ) y( iy ) =y( iy ) + sign( safe1, y( iy ) )
                    iy = iy + incy
                 end do
              end if
           else
              if( trans==stdlib_ilatrans( 'N' ) )then
                 do i = 1, leny
                    if ( beta == czero ) then
                       symb_zero = .true.
                       y( iy ) = czero
                    else if ( y( iy ) == czero ) then
                       symb_zero = .true.
                    else
                       symb_zero = .false.
                       y( iy ) = beta * abs( y( iy ) )
                    end if
                    if ( alpha /= czero ) then
                       jx = kx
                       do j = 1, lenx
                          temp = cabs1( a( i, j ) )
                          symb_zero = symb_zero .and.( x( jx ) == czero .or. temp == czero )
                                    
                          y( iy ) = y( iy ) + alpha*cabs1( x( jx ) )*temp
                          jx = jx + incx
                       end do
                    end if
                    if ( .not.symb_zero ) y( iy ) =y( iy ) + sign( safe1, y( iy ) )
                    iy = iy + incy
                 end do
              else
                 do i = 1, leny
                    if ( beta == czero ) then
                       symb_zero = .true.
                       y( iy ) = czero
                    else if ( y( iy ) == czero ) then
                       symb_zero = .true.
                    else
                       symb_zero = .false.
                       y( iy ) = beta * abs( y( iy ) )
                    end if
                    if ( alpha /= czero ) then
                       jx = kx
                       do j = 1, lenx
                          temp = cabs1( a( j, i ) )
                          symb_zero = symb_zero .and.( x( jx ) == czero .or. temp == czero )
                                    
                          y( iy ) = y( iy ) + alpha*cabs1( x( jx ) )*temp
                          jx = jx + incx
                       end do
                    end if
                    if ( .not.symb_zero ) y( iy ) =y( iy ) + sign( safe1, y( iy ) )
                    iy = iy + incy
                 end do
              end if
           end if
           return
     end subroutine stdlib_zla_geamv




     module subroutine stdlib_sla_gbamv( trans, m, n, kl, ku, alpha, ab, ldab, x,incx, beta, y, incy )
     !! SLA_GBAMV performs one of the matrix-vector operations
     !! y := alpha*abs(A)*abs(x) + beta*abs(y),
     !! or   y := alpha*abs(A)**T*abs(x) + beta*abs(y),
     !! where alpha and beta are scalars, x and y are vectors and A is an
     !! m by n matrix.
     !! This function is primarily used in calculating error bounds.
     !! To protect against underflow during evaluation, components in
     !! the resulting vector are perturbed away from zero by (N+1)
     !! times the underflow threshold.  To prevent unnecessarily large
     !! errors for block-structure embedded in general matrices,
     !! "symbolically" zero components are not perturbed.  A zero
     !! entry is considered "symbolic" if all multiplications involved
     !! in computing that entry have at least one zero multiplicand.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           real(sp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, ldab, m, n, kl, ku, trans
           ! Array Arguments 
           real(sp), intent(in) :: ab(ldab,*), x(*)
           real(sp), intent(inout) :: y(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: symb_zero
           real(sp) :: temp, safe1
           integer(ilp) :: i, info, iy, j, jx, kx, ky, lenx, leny, kd, ke
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if     ( .not.( ( trans==stdlib_ilatrans( 'N' ) ).or. ( trans==stdlib_ilatrans( 'T' ) )&
                     .or. ( trans==stdlib_ilatrans( 'C' ) ) ) ) then
              info = 1_ilp
           else if( m<0_ilp )then
              info = 2_ilp
           else if( n<0_ilp )then
              info = 3_ilp
           else if( kl<0_ilp .or. kl>m-1 ) then
              info = 4_ilp
           else if( ku<0_ilp .or. ku>n-1 ) then
              info = 5_ilp
           else if( ldab<kl+ku+1 )then
              info = 6_ilp
           else if( incx==0_ilp )then
              info = 8_ilp
           else if( incy==0_ilp )then
              info = 11_ilp
           end if
           if( info/=0_ilp )then
              call stdlib_xerbla( 'SLA_GBAMV ', info )
              return
           end if
           ! quick return if possible.
           if( ( m==0 ).or.( n==0 ).or.( ( alpha==zero ).and.( beta==one ) ) )return
           ! set  lenx  and  leny, the lengths of the vectors x and y, and set
           ! up the start points in  x  and  y.
           if( trans==stdlib_ilatrans( 'N' ) )then
              lenx = n
              leny = m
           else
              lenx = m
              leny = n
           end if
           if( incx>0_ilp )then
              kx = 1_ilp
           else
              kx = 1_ilp - ( lenx - 1_ilp )*incx
           end if
           if( incy>0_ilp )then
              ky = 1_ilp
           else
              ky = 1_ilp - ( leny - 1_ilp )*incy
           end if
           ! set safe1 essentially to be the underflow threshold times the
           ! number of additions in each row.
           safe1 = stdlib_slamch( 'SAFE MINIMUM' )
           safe1 = (n+1)*safe1
           ! form  y := alpha*abs(a)*abs(x) + beta*abs(y).
           ! the o(m*n) symb_zero tests could be replaced by o(n) queries to
           ! the inexact flag.  still doesn't help change the iteration order
           ! to per-column.
           kd = ku + 1_ilp
           ke = kl + 1_ilp
           iy = ky
           if ( incx==1_ilp ) then
              if( trans==stdlib_ilatrans( 'N' ) )then
                 do i = 1, leny
                    if ( beta == zero ) then
                       symb_zero = .true.
                       y( iy ) = zero
                    else if ( y( iy ) == zero ) then
                       symb_zero = .true.
                    else
                       symb_zero = .false.
                       y( iy ) = beta * abs( y( iy ) )
                    end if
                    if ( alpha /= zero ) then
                       do j = max( i-kl, 1 ), min( i+ku, lenx )
                          temp = abs( ab( kd+i-j, j ) )
                          symb_zero = symb_zero .and.( x( j ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*abs( x( j ) )*temp
                       end do
                    end if
                    if ( .not.symb_zero )y( iy ) = y( iy ) + sign( safe1, y( iy ) )
                    iy = iy + incy
                 end do
              else
                 do i = 1, leny
                    if ( beta == zero ) then
                       symb_zero = .true.
                       y( iy ) = zero
                    else if ( y( iy ) == zero ) then
                       symb_zero = .true.
                    else
                       symb_zero = .false.
                       y( iy ) = beta * abs( y( iy ) )
                    end if
                    if ( alpha /= zero ) then
                       do j = max( i-kl, 1 ), min( i+ku, lenx )
                          temp = abs( ab( ke-i+j, i ) )
                          symb_zero = symb_zero .and.( x( j ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*abs( x( j ) )*temp
                       end do
                    end if
                    if ( .not.symb_zero )y( iy ) = y( iy ) + sign( safe1, y( iy ) )
                    iy = iy + incy
                 end do
              end if
           else
              if( trans==stdlib_ilatrans( 'N' ) )then
                 do i = 1, leny
                    if ( beta == zero ) then
                       symb_zero = .true.
                       y( iy ) = zero
                    else if ( y( iy ) == zero ) then
                       symb_zero = .true.
                    else
                       symb_zero = .false.
                       y( iy ) = beta * abs( y( iy ) )
                    end if
                    if ( alpha /= zero ) then
                       jx = kx
                       do j = max( i-kl, 1 ), min( i+ku, lenx )
                          temp = abs( ab( kd+i-j, j ) )
                          symb_zero = symb_zero .and.( x( jx ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*abs( x( jx ) )*temp
                          jx = jx + incx
                       end do
                    end if
                    if ( .not.symb_zero )y( iy ) = y( iy ) + sign( safe1, y( iy ) )
                    iy = iy + incy
                 end do
              else
                 do i = 1, leny
                    if ( beta == zero ) then
                       symb_zero = .true.
                       y( iy ) = zero
                    else if ( y( iy ) == zero ) then
                       symb_zero = .true.
                    else
                       symb_zero = .false.
                       y( iy ) = beta * abs( y( iy ) )
                    end if
                    if ( alpha /= zero ) then
                       jx = kx
                       do j = max( i-kl, 1 ), min( i+ku, lenx )
                          temp = abs( ab( ke-i+j, i ) )
                          symb_zero = symb_zero .and.( x( jx ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*abs( x( jx ) )*temp
                          jx = jx + incx
                       end do
                    end if
                    if ( .not.symb_zero )y( iy ) = y( iy ) + sign( safe1, y( iy ) )
                    iy = iy + incy
                 end do
              end if
           end if
           return
     end subroutine stdlib_sla_gbamv

     module subroutine stdlib_dla_gbamv( trans, m, n, kl, ku, alpha, ab, ldab, x,incx, beta, y, incy )
     !! DLA_GBAMV performs one of the matrix-vector operations
     !! y := alpha*abs(A)*abs(x) + beta*abs(y),
     !! or   y := alpha*abs(A)**T*abs(x) + beta*abs(y),
     !! where alpha and beta are scalars, x and y are vectors and A is an
     !! m by n matrix.
     !! This function is primarily used in calculating error bounds.
     !! To protect against underflow during evaluation, components in
     !! the resulting vector are perturbed away from zero by (N+1)
     !! times the underflow threshold.  To prevent unnecessarily large
     !! errors for block-structure embedded in general matrices,
     !! "symbolically" zero components are not perturbed.  A zero
     !! entry is considered "symbolic" if all multiplications involved
     !! in computing that entry have at least one zero multiplicand.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           real(dp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, ldab, m, n, kl, ku, trans
           ! Array Arguments 
           real(dp), intent(in) :: ab(ldab,*), x(*)
           real(dp), intent(inout) :: y(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: symb_zero
           real(dp) :: temp, safe1
           integer(ilp) :: i, info, iy, j, jx, kx, ky, lenx, leny, kd, ke
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if     ( .not.( ( trans==stdlib_ilatrans( 'N' ) ).or. ( trans==stdlib_ilatrans( 'T' ) )&
                     .or. ( trans==stdlib_ilatrans( 'C' ) ) ) ) then
              info = 1_ilp
           else if( m<0_ilp )then
              info = 2_ilp
           else if( n<0_ilp )then
              info = 3_ilp
           else if( kl<0_ilp .or. kl>m-1 ) then
              info = 4_ilp
           else if( ku<0_ilp .or. ku>n-1 ) then
              info = 5_ilp
           else if( ldab<kl+ku+1 )then
              info = 6_ilp
           else if( incx==0_ilp )then
              info = 8_ilp
           else if( incy==0_ilp )then
              info = 11_ilp
           end if
           if( info/=0_ilp )then
              call stdlib_xerbla( 'DLA_GBAMV ', info )
              return
           end if
           ! quick return if possible.
           if( ( m==0 ).or.( n==0 ).or.( ( alpha==zero ).and.( beta==one ) ) )return
           ! set  lenx  and  leny, the lengths of the vectors x and y, and set
           ! up the start points in  x  and  y.
           if( trans==stdlib_ilatrans( 'N' ) )then
              lenx = n
              leny = m
           else
              lenx = m
              leny = n
           end if
           if( incx>0_ilp )then
              kx = 1_ilp
           else
              kx = 1_ilp - ( lenx - 1_ilp )*incx
           end if
           if( incy>0_ilp )then
              ky = 1_ilp
           else
              ky = 1_ilp - ( leny - 1_ilp )*incy
           end if
           ! set safe1 essentially to be the underflow threshold times the
           ! number of additions in each row.
           safe1 = stdlib_dlamch( 'SAFE MINIMUM' )
           safe1 = (n+1)*safe1
           ! form  y := alpha*abs(a)*abs(x) + beta*abs(y).
           ! the o(m*n) symb_zero tests could be replaced by o(n) queries to
           ! the inexact flag.  still doesn't help change the iteration order
           ! to per-column.
           kd = ku + 1_ilp
           ke = kl + 1_ilp
           iy = ky
           if ( incx==1_ilp ) then
              if( trans==stdlib_ilatrans( 'N' ) )then
                 do i = 1, leny
                    if ( beta == zero ) then
                       symb_zero = .true.
                       y( iy ) = zero
                    else if ( y( iy ) == zero ) then
                       symb_zero = .true.
                    else
                       symb_zero = .false.
                       y( iy ) = beta * abs( y( iy ) )
                    end if
                    if ( alpha /= zero ) then
                       do j = max( i-kl, 1 ), min( i+ku, lenx )
                          temp = abs( ab( kd+i-j, j ) )
                          symb_zero = symb_zero .and.( x( j ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*abs( x( j ) )*temp
                       end do
                    end if
                    if ( .not.symb_zero )y( iy ) = y( iy ) + sign( safe1, y( iy ) )
                    iy = iy + incy
                 end do
              else
                 do i = 1, leny
                    if ( beta == zero ) then
                       symb_zero = .true.
                       y( iy ) = zero
                    else if ( y( iy ) == zero ) then
                       symb_zero = .true.
                    else
                       symb_zero = .false.
                       y( iy ) = beta * abs( y( iy ) )
                    end if
                    if ( alpha /= zero ) then
                       do j = max( i-kl, 1 ), min( i+ku, lenx )
                          temp = abs( ab( ke-i+j, i ) )
                          symb_zero = symb_zero .and.( x( j ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*abs( x( j ) )*temp
                       end do
                    end if
                    if ( .not.symb_zero )y( iy ) = y( iy ) + sign( safe1, y( iy ) )
                    iy = iy + incy
                 end do
              end if
           else
              if( trans==stdlib_ilatrans( 'N' ) )then
                 do i = 1, leny
                    if ( beta == zero ) then
                       symb_zero = .true.
                       y( iy ) = zero
                    else if ( y( iy ) == zero ) then
                       symb_zero = .true.
                    else
                       symb_zero = .false.
                       y( iy ) = beta * abs( y( iy ) )
                    end if
                    if ( alpha /= zero ) then
                       jx = kx
                       do j = max( i-kl, 1 ), min( i+ku, lenx )
                          temp = abs( ab( kd+i-j, j ) )
                          symb_zero = symb_zero .and.( x( jx ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*abs( x( jx ) )*temp
                          jx = jx + incx
                       end do
                    end if
                    if ( .not.symb_zero )y( iy ) = y( iy ) + sign( safe1, y( iy ) )
                    iy = iy + incy
                 end do
              else
                 do i = 1, leny
                    if ( beta == zero ) then
                       symb_zero = .true.
                       y( iy ) = zero
                    else if ( y( iy ) == zero ) then
                       symb_zero = .true.
                    else
                       symb_zero = .false.
                       y( iy ) = beta * abs( y( iy ) )
                    end if
                    if ( alpha /= zero ) then
                       jx = kx
                       do j = max( i-kl, 1 ), min( i+ku, lenx )
                          temp = abs( ab( ke-i+j, i ) )
                          symb_zero = symb_zero .and.( x( jx ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*abs( x( jx ) )*temp
                          jx = jx + incx
                       end do
                    end if
                    if ( .not.symb_zero )y( iy ) = y( iy ) + sign( safe1, y( iy ) )
                    iy = iy + incy
                 end do
              end if
           end if
           return
     end subroutine stdlib_dla_gbamv


     module subroutine stdlib_cla_gbamv( trans, m, n, kl, ku, alpha, ab, ldab, x,incx, beta, y, incy )
     !! CLA_GBAMV performs one of the matrix-vector operations
     !! y := alpha*abs(A)*abs(x) + beta*abs(y),
     !! or   y := alpha*abs(A)**T*abs(x) + beta*abs(y),
     !! where alpha and beta are scalars, x and y are vectors and A is an
     !! m by n matrix.
     !! This function is primarily used in calculating error bounds.
     !! To protect against underflow during evaluation, components in
     !! the resulting vector are perturbed away from zero by (N+1)
     !! times the underflow threshold.  To prevent unnecessarily large
     !! errors for block-structure embedded in general matrices,
     !! "symbolically" zero components are not perturbed.  A zero
     !! entry is considered "symbolic" if all multiplications involved
     !! in computing that entry have at least one zero multiplicand.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           real(sp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, ldab, m, n, kl, ku, trans
           ! Array Arguments 
           complex(sp), intent(in) :: ab(ldab,*), x(*)
           real(sp), intent(inout) :: y(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: symb_zero
           real(sp) :: temp, safe1
           integer(ilp) :: i, info, iy, j, jx, kx, ky, lenx, leny, kd, ke
           complex(sp) :: cdum
           ! Intrinsic Functions 
           ! Statement Functions
           real(sp) :: cabs1
           ! Statement Function Definitions 
           cabs1( cdum ) = abs( real( cdum,KIND=sp) ) + abs( aimag( cdum ) )
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if     ( .not.( ( trans==stdlib_ilatrans( 'N' ) ).or. ( trans==stdlib_ilatrans( 'T' ) )&
                     .or. ( trans==stdlib_ilatrans( 'C' ) ) ) ) then
              info = 1_ilp
           else if( m<0_ilp )then
              info = 2_ilp
           else if( n<0_ilp )then
              info = 3_ilp
           else if( kl<0_ilp .or. kl>m-1 ) then
              info = 4_ilp
           else if( ku<0_ilp .or. ku>n-1 ) then
              info = 5_ilp
           else if( ldab<kl+ku+1 )then
              info = 6_ilp
           else if( incx==0_ilp )then
              info = 8_ilp
           else if( incy==0_ilp )then
              info = 11_ilp
           end if
           if( info/=0_ilp )then
              call stdlib_xerbla( 'CLA_GBAMV ', info )
              return
           end if
           ! quick return if possible.
           if( ( m==0 ).or.( n==0 ).or.( ( alpha==czero ).and.( beta==cone ) ) )return
           ! set  lenx  and  leny, the lengths of the vectors x and y, and set
           ! up the start points in  x  and  y.
           if( trans==stdlib_ilatrans( 'N' ) )then
              lenx = n
              leny = m
           else
              lenx = m
              leny = n
           end if
           if( incx>0_ilp )then
              kx = 1_ilp
           else
              kx = 1_ilp - ( lenx - 1_ilp )*incx
           end if
           if( incy>0_ilp )then
              ky = 1_ilp
           else
              ky = 1_ilp - ( leny - 1_ilp )*incy
           end if
           ! set safe1 essentially to be the underflow threshold times the
           ! number of additions in each row.
           safe1 = stdlib_slamch( 'SAFE MINIMUM' )
           safe1 = (n+1)*safe1
           ! form  y := alpha*abs(a)*abs(x) + beta*abs(y).
           ! the o(m*n) symb_zero tests could be replaced by o(n) queries to
           ! the inexact flag.  still doesn't help change the iteration order
           ! to per-column.
           kd = ku + 1_ilp
           ke = kl + 1_ilp
           iy = ky
           if ( incx==1_ilp ) then
              if( trans==stdlib_ilatrans( 'N' ) )then
                 do i = 1, leny
                    if ( beta == 0.0_sp ) then
                       symb_zero = .true.
                       y( iy ) = czero
                    else if ( y( iy ) == 0.0_sp ) then
                       symb_zero = .true.
                    else
                       symb_zero = .false.
                       y( iy ) = beta * abs( y( iy ) )
                    end if
                    if ( alpha /= 0.0_sp ) then
                       do j = max( i-kl, 1 ), min( i+ku, lenx )
                          temp = cabs1( ab( kd+i-j, j ) )
                          symb_zero = symb_zero .and.( x( j ) == czero .or. temp == czero )
                                    
                          y( iy ) = y( iy ) + alpha*cabs1( x( j ) )*temp
                       end do
                    end if
                    if ( .not.symb_zero)y( iy ) = y( iy ) + sign( safe1, y( iy ) )
                    iy = iy + incy
                 end do
              else
                 do i = 1, leny
                    if ( beta == 0.0_sp ) then
                       symb_zero = .true.
                       y( iy ) = czero
                    else if ( y( iy ) == 0.0_sp ) then
                       symb_zero = .true.
                    else
                       symb_zero = .false.
                       y( iy ) = beta * abs( y( iy ) )
                    end if
                    if ( alpha /= 0.0_sp ) then
                       do j = max( i-kl, 1 ), min( i+ku, lenx )
                          temp = cabs1( ab( ke-i+j, i ) )
                          symb_zero = symb_zero .and.( x( j ) == czero .or. temp == czero )
                                    
                          y( iy ) = y( iy ) + alpha*cabs1( x( j ) )*temp
                       end do
                    end if
                    if ( .not.symb_zero)y( iy ) = y( iy ) + sign( safe1, y( iy ) )
                    iy = iy + incy
                 end do
              end if
           else
              if( trans==stdlib_ilatrans( 'N' ) )then
                 do i = 1, leny
                    if ( beta == 0.0_sp ) then
                       symb_zero = .true.
                       y( iy ) = czero
                    else if ( y( iy ) == 0.0_sp ) then
                       symb_zero = .true.
                    else
                       symb_zero = .false.
                       y( iy ) = beta * abs( y( iy ) )
                    end if
                    if ( alpha /= 0.0_sp ) then
                       jx = kx
                       do j = max( i-kl, 1 ), min( i+ku, lenx )
                          temp = cabs1( ab( kd+i-j, j ) )
                          symb_zero = symb_zero .and.( x( jx ) == czero .or. temp == czero )
                                    
                          y( iy ) = y( iy ) + alpha*cabs1( x( jx ) )*temp
                          jx = jx + incx
                       end do
                    end if
                    if ( .not.symb_zero )y( iy ) = y( iy ) + sign( safe1, y( iy ) )
                    iy = iy + incy
                 end do
              else
                 do i = 1, leny
                    if ( beta == 0.0_sp ) then
                       symb_zero = .true.
                       y( iy ) = czero
                    else if ( y( iy ) == 0.0_sp ) then
                       symb_zero = .true.
                    else
                       symb_zero = .false.
                       y( iy ) = beta * abs( y( iy ) )
                    end if
                    if ( alpha /= 0.0_sp ) then
                       jx = kx
                       do j = max( i-kl, 1 ), min( i+ku, lenx )
                          temp = cabs1( ab( ke-i+j, i ) )
                          symb_zero = symb_zero .and.( x( jx ) == czero .or. temp == czero )
                                    
                          y( iy ) = y( iy ) + alpha*cabs1( x( jx ) )*temp
                          jx = jx + incx
                       end do
                    end if
                    if ( .not.symb_zero )y( iy ) = y( iy ) + sign( safe1, y( iy ) )
                    iy = iy + incy
                 end do
              end if
           end if
           return
     end subroutine stdlib_cla_gbamv

     module subroutine stdlib_zla_gbamv( trans, m, n, kl, ku, alpha, ab, ldab, x,incx, beta, y, incy )
     !! ZLA_GBAMV performs one of the matrix-vector operations
     !! y := alpha*abs(A)*abs(x) + beta*abs(y),
     !! or   y := alpha*abs(A)**T*abs(x) + beta*abs(y),
     !! where alpha and beta are scalars, x and y are vectors and A is an
     !! m by n matrix.
     !! This function is primarily used in calculating error bounds.
     !! To protect against underflow during evaluation, components in
     !! the resulting vector are perturbed away from zero by (N+1)
     !! times the underflow threshold.  To prevent unnecessarily large
     !! errors for block-structure embedded in general matrices,
     !! "symbolically" zero components are not perturbed.  A zero
     !! entry is considered "symbolic" if all multiplications involved
     !! in computing that entry have at least one zero multiplicand.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           real(dp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, ldab, m, n, kl, ku, trans
           ! Array Arguments 
           complex(dp), intent(in) :: ab(ldab,*), x(*)
           real(dp), intent(inout) :: y(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: symb_zero
           real(dp) :: temp, safe1
           integer(ilp) :: i, info, iy, j, jx, kx, ky, lenx, leny, kd, ke
           complex(dp) :: cdum
           ! Intrinsic Functions 
           ! Statement Functions
           real(dp) :: cabs1
           ! Statement Function Definitions 
           cabs1( cdum ) = abs( real( cdum,KIND=dp) ) + abs( aimag( cdum ) )
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if     ( .not.( ( trans==stdlib_ilatrans( 'N' ) ).or. ( trans==stdlib_ilatrans( 'T' ) )&
                     .or. ( trans==stdlib_ilatrans( 'C' ) ) ) ) then
              info = 1_ilp
           else if( m<0_ilp )then
              info = 2_ilp
           else if( n<0_ilp )then
              info = 3_ilp
           else if( kl<0_ilp .or. kl>m-1 ) then
              info = 4_ilp
           else if( ku<0_ilp .or. ku>n-1 ) then
              info = 5_ilp
           else if( ldab<kl+ku+1 )then
              info = 6_ilp
           else if( incx==0_ilp )then
              info = 8_ilp
           else if( incy==0_ilp )then
              info = 11_ilp
           end if
           if( info/=0_ilp )then
              call stdlib_xerbla( 'ZLA_GBAMV ', info )
              return
           end if
           ! quick return if possible.
           if( ( m==0 ).or.( n==0 ).or.( ( alpha==czero ).and.( beta==cone ) ) )return
           ! set  lenx  and  leny, the lengths of the vectors x and y, and set
           ! up the start points in  x  and  y.
           if( trans==stdlib_ilatrans( 'N' ) )then
              lenx = n
              leny = m
           else
              lenx = m
              leny = n
           end if
           if( incx>0_ilp )then
              kx = 1_ilp
           else
              kx = 1_ilp - ( lenx - 1_ilp )*incx
           end if
           if( incy>0_ilp )then
              ky = 1_ilp
           else
              ky = 1_ilp - ( leny - 1_ilp )*incy
           end if
           ! set safe1 essentially to be the underflow threshold times the
           ! number of additions in each row.
           safe1 = stdlib_dlamch( 'SAFE MINIMUM' )
           safe1 = (n+1)*safe1
           ! form  y := alpha*abs(a)*abs(x) + beta*abs(y).
           ! the o(m*n) symb_zero tests could be replaced by o(n) queries to
           ! the inexact flag.  still doesn't help change the iteration order
           ! to per-column.
           kd = ku + 1_ilp
           ke = kl + 1_ilp
           iy = ky
           if ( incx==1_ilp ) then
              if( trans==stdlib_ilatrans( 'N' ) )then
                 do i = 1, leny
                    if ( beta == czero ) then
                       symb_zero = .true.
                       y( iy ) = czero
                    else if ( y( iy ) == czero ) then
                       symb_zero = .true.
                    else
                       symb_zero = .false.
                       y( iy ) = beta * abs( y( iy ) )
                    end if
                    if ( alpha /= czero ) then
                       do j = max( i-kl, 1 ), min( i+ku, lenx )
                          temp = cabs1( ab( kd+i-j, j ) )
                          symb_zero = symb_zero .and.( x( j ) == czero .or. temp == czero )
                                    
                          y( iy ) = y( iy ) + alpha*cabs1( x( j ) )*temp
                       end do
                    end if
                    if ( .not.symb_zero)y( iy ) = y( iy ) + sign( safe1, y( iy ) )
                    iy = iy + incy
                 end do
              else
                 do i = 1, leny
                    if ( beta == czero ) then
                       symb_zero = .true.
                       y( iy ) = czero
                    else if ( y( iy ) == czero ) then
                       symb_zero = .true.
                    else
                       symb_zero = .false.
                       y( iy ) = beta * abs( y( iy ) )
                    end if
                    if ( alpha /= czero ) then
                       do j = max( i-kl, 1 ), min( i+ku, lenx )
                          temp = cabs1( ab( ke-i+j, i ) )
                          symb_zero = symb_zero .and.( x( j ) == czero .or. temp == czero )
                                    
                          y( iy ) = y( iy ) + alpha*cabs1( x( j ) )*temp
                       end do
                    end if
                    if ( .not.symb_zero)y( iy ) = y( iy ) + sign( safe1, y( iy ) )
                    iy = iy + incy
                 end do
              end if
           else
              if( trans==stdlib_ilatrans( 'N' ) )then
                 do i = 1, leny
                    if ( beta == czero ) then
                       symb_zero = .true.
                       y( iy ) = czero
                    else if ( y( iy ) == czero ) then
                       symb_zero = .true.
                    else
                       symb_zero = .false.
                       y( iy ) = beta * abs( y( iy ) )
                    end if
                    if ( alpha /= czero ) then
                       jx = kx
                       do j = max( i-kl, 1 ), min( i+ku, lenx )
                          temp = cabs1( ab( kd+i-j, j ) )
                          symb_zero = symb_zero .and.( x( jx ) == czero .or. temp == czero )
                                    
                          y( iy ) = y( iy ) + alpha*cabs1( x( jx ) )*temp
                          jx = jx + incx
                       end do
                    end if
                    if ( .not.symb_zero )y( iy ) = y( iy ) + sign( safe1, y( iy ) )
                    iy = iy + incy
                 end do
              else
                 do i = 1, leny
                    if ( beta == czero ) then
                       symb_zero = .true.
                       y( iy ) = czero
                    else if ( y( iy ) == czero ) then
                       symb_zero = .true.
                    else
                       symb_zero = .false.
                       y( iy ) = beta * abs( y( iy ) )
                    end if
                    if ( alpha /= czero ) then
                       jx = kx
                       do j = max( i-kl, 1 ), min( i+ku, lenx )
                          temp = cabs1( ab( ke-i+j, i ) )
                          symb_zero = symb_zero .and.( x( jx ) == czero .or. temp == czero )
                                    
                          y( iy ) = y( iy ) + alpha*cabs1( x( jx ) )*temp
                          jx = jx + incx
                       end do
                    end if
                    if ( .not.symb_zero )y( iy ) = y( iy ) + sign( safe1, y( iy ) )
                    iy = iy + incy
                 end do
              end if
           end if
           return
     end subroutine stdlib_zla_gbamv




     module subroutine stdlib_cla_heamv( uplo, n, alpha, a, lda, x, incx, beta, y,incy )
     !! CLA_SYAMV  performs the matrix-vector operation
     !! y := alpha*abs(A)*abs(x) + beta*abs(y),
     !! where alpha and beta are scalars, x and y are vectors and A is an
     !! n by n symmetric matrix.
     !! This function is primarily used in calculating error bounds.
     !! To protect against underflow during evaluation, components in
     !! the resulting vector are perturbed away from zero by (N+1)
     !! times the underflow threshold.  To prevent unnecessarily large
     !! errors for block-structure embedded in general matrices,
     !! "symbolically" zero components are not perturbed.  A zero
     !! entry is considered "symbolic" if all multiplications involved
     !! in computing that entry have at least one zero multiplicand.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           real(sp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, lda, n, uplo
           ! Array Arguments 
           complex(sp), intent(in) :: a(lda,*), x(*)
           real(sp), intent(inout) :: y(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: symb_zero
           real(sp) :: temp, safe1
           integer(ilp) :: i, info, iy, j, jx, kx, ky
           complex(sp) :: zdum
           ! Intrinsic Functions 
           ! Statement Functions 
           real(sp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=sp) ) + abs( aimag ( zdum ) )
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if     ( uplo/=stdlib_ilauplo( 'U' ) .and.uplo/=stdlib_ilauplo( 'L' ) )then
              info = 1_ilp
           else if( n<0_ilp )then
              info = 2_ilp
           else if( lda<max( 1_ilp, n ) )then
              info = 5_ilp
           else if( incx==0_ilp )then
              info = 7_ilp
           else if( incy==0_ilp )then
              info = 10_ilp
           end if
           if( info/=0_ilp )then
              call stdlib_xerbla( 'CHEMV ', info )
              return
           end if
           ! quick return if possible.
           if( ( n==0 ).or.( ( alpha==zero ).and.( beta==one ) ) )return
           ! set up the start points in  x  and  y.
           if( incx>0_ilp )then
              kx = 1_ilp
           else
              kx = 1_ilp - ( n - 1_ilp )*incx
           end if
           if( incy>0_ilp )then
              ky = 1_ilp
           else
              ky = 1_ilp - ( n - 1_ilp )*incy
           end if
           ! set safe1 essentially to be the underflow threshold times the
           ! number of additions in each row.
           safe1 = stdlib_slamch( 'SAFE MINIMUM' )
           safe1 = (n+1)*safe1
           ! form  y := alpha*abs(a)*abs(x) + beta*abs(y).
           ! the o(n^2) symb_zero tests could be replaced by o(n) queries to
           ! the inexact flag.  still doesn't help change the iteration order
           ! to per-column.
           iy = ky
           if ( incx==1_ilp ) then
              if ( uplo == stdlib_ilauplo( 'U' ) ) then
                 do i = 1, n
                    if ( beta == zero ) then
                       symb_zero = .true.
                       y( iy ) = zero
                    else if ( y( iy ) == zero ) then
                       symb_zero = .true.
                    else
                       symb_zero = .false.
                       y( iy ) = beta * abs( y( iy ) )
                    end if
                    if ( alpha /= zero ) then
                       do j = 1, i
                          temp = cabs1( a( j, i ) )
                          symb_zero = symb_zero .and.( x( j ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*cabs1( x( j ) )*temp
                       end do
                       do j = i+1, n
                          temp = cabs1( a( i, j ) )
                          symb_zero = symb_zero .and.( x( j ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*cabs1( x( j ) )*temp
                       end do
                    end if
                    if (.not.symb_zero)y( iy ) = y( iy ) + sign( safe1, y( iy ) )
                    iy = iy + incy
                 end do
              else
                 do i = 1, n
                    if ( beta == zero ) then
                       symb_zero = .true.
                       y( iy ) = zero
                    else if ( y( iy ) == zero ) then
                       symb_zero = .true.
                    else
                       symb_zero = .false.
                       y( iy ) = beta * abs( y( iy ) )
                    end if
                    if ( alpha /= zero ) then
                       do j = 1, i
                          temp = cabs1( a( i, j ) )
                          symb_zero = symb_zero .and.( x( j ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*cabs1( x( j ) )*temp
                       end do
                       do j = i+1, n
                          temp = cabs1( a( j, i ) )
                          symb_zero = symb_zero .and.( x( j ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*cabs1( x( j ) )*temp
                       end do
                    end if
                    if (.not.symb_zero)y( iy ) = y( iy ) + sign( safe1, y( iy ) )
                    iy = iy + incy
                 end do
              end if
           else
              if ( uplo == stdlib_ilauplo( 'U' ) ) then
                 do i = 1, n
                    if ( beta == zero ) then
                       symb_zero = .true.
                       y( iy ) = zero
                    else if ( y( iy ) == zero ) then
                       symb_zero = .true.
                    else
                       symb_zero = .false.
                       y( iy ) = beta * abs( y( iy ) )
                    end if
                    jx = kx
                    if ( alpha /= zero ) then
                       do j = 1, i
                          temp = cabs1( a( j, i ) )
                          symb_zero = symb_zero .and.( x( j ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*cabs1( x( jx ) )*temp
                          jx = jx + incx
                       end do
                       do j = i+1, n
                          temp = cabs1( a( i, j ) )
                          symb_zero = symb_zero .and.( x( j ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*cabs1( x( jx ) )*temp
                          jx = jx + incx
                       end do
                    end if
                    if ( .not.symb_zero )y( iy ) = y( iy ) + sign( safe1, y( iy ) )
                    iy = iy + incy
                 end do
              else
                 do i = 1, n
                    if ( beta == zero ) then
                       symb_zero = .true.
                       y( iy ) = zero
                    else if ( y( iy ) == zero ) then
                       symb_zero = .true.
                    else
                       symb_zero = .false.
                       y( iy ) = beta * abs( y( iy ) )
                    end if
                    jx = kx
                    if ( alpha /= zero ) then
                       do j = 1, i
                          temp = cabs1( a( i, j ) )
                          symb_zero = symb_zero .and.( x( j ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*cabs1( x( jx ) )*temp
                          jx = jx + incx
                       end do
                       do j = i+1, n
                          temp = cabs1( a( j, i ) )
                          symb_zero = symb_zero .and.( x( j ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*cabs1( x( jx ) )*temp
                          jx = jx + incx
                       end do
                    end if
                    if ( .not.symb_zero )y( iy ) = y( iy ) + sign( safe1, y( iy ) )
                    iy = iy + incy
                 end do
              end if
           end if
           return
     end subroutine stdlib_cla_heamv

     module subroutine stdlib_zla_heamv( uplo, n, alpha, a, lda, x, incx, beta, y,incy )
     !! ZLA_SYAMV  performs the matrix-vector operation
     !! y := alpha*abs(A)*abs(x) + beta*abs(y),
     !! where alpha and beta are scalars, x and y are vectors and A is an
     !! n by n symmetric matrix.
     !! This function is primarily used in calculating error bounds.
     !! To protect against underflow during evaluation, components in
     !! the resulting vector are perturbed away from zero by (N+1)
     !! times the underflow threshold.  To prevent unnecessarily large
     !! errors for block-structure embedded in general matrices,
     !! "symbolically" zero components are not perturbed.  A zero
     !! entry is considered "symbolic" if all multiplications involved
     !! in computing that entry have at least one zero multiplicand.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           real(dp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, lda, n, uplo
           ! Array Arguments 
           complex(dp), intent(in) :: a(lda,*), x(*)
           real(dp), intent(inout) :: y(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: symb_zero
           real(dp) :: temp, safe1
           integer(ilp) :: i, info, iy, j, jx, kx, ky
           complex(dp) :: zdum
           ! Intrinsic Functions 
           ! Statement Functions 
           real(dp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=dp) ) + abs( aimag ( zdum ) )
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if     ( uplo/=stdlib_ilauplo( 'U' ) .and.uplo/=stdlib_ilauplo( 'L' ) )then
              info = 1_ilp
           else if( n<0_ilp )then
              info = 2_ilp
           else if( lda<max( 1_ilp, n ) )then
              info = 5_ilp
           else if( incx==0_ilp )then
              info = 7_ilp
           else if( incy==0_ilp )then
              info = 10_ilp
           end if
           if( info/=0_ilp )then
              call stdlib_xerbla( 'ZHEMV ', info )
              return
           end if
           ! quick return if possible.
           if( ( n==0 ).or.( ( alpha==zero ).and.( beta==one ) ) )return
           ! set up the start points in  x  and  y.
           if( incx>0_ilp )then
              kx = 1_ilp
           else
              kx = 1_ilp - ( n - 1_ilp )*incx
           end if
           if( incy>0_ilp )then
              ky = 1_ilp
           else
              ky = 1_ilp - ( n - 1_ilp )*incy
           end if
           ! set safe1 essentially to be the underflow threshold times the
           ! number of additions in each row.
           safe1 = stdlib_dlamch( 'SAFE MINIMUM' )
           safe1 = (n+1)*safe1
           ! form  y := alpha*abs(a)*abs(x) + beta*abs(y).
           ! the o(n^2) symb_zero tests could be replaced by o(n) queries to
           ! the inexact flag.  still doesn't help change the iteration order
           ! to per-column.
           iy = ky
           if ( incx==1_ilp ) then
              if ( uplo == stdlib_ilauplo( 'U' ) ) then
                 do i = 1, n
                    if ( beta == zero ) then
                       symb_zero = .true.
                       y( iy ) = zero
                    else if ( y( iy ) == zero ) then
                       symb_zero = .true.
                    else
                       symb_zero = .false.
                       y( iy ) = beta * abs( y( iy ) )
                    end if
                    if ( alpha /= zero ) then
                       do j = 1, i
                          temp = cabs1( a( j, i ) )
                          symb_zero = symb_zero .and.( x( j ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*cabs1( x( j ) )*temp
                       end do
                       do j = i+1, n
                          temp = cabs1( a( i, j ) )
                          symb_zero = symb_zero .and.( x( j ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*cabs1( x( j ) )*temp
                       end do
                    end if
                    if (.not.symb_zero)y( iy ) = y( iy ) + sign( safe1, y( iy ) )
                    iy = iy + incy
                 end do
              else
                 do i = 1, n
                    if ( beta == zero ) then
                       symb_zero = .true.
                       y( iy ) = zero
                    else if ( y( iy ) == zero ) then
                       symb_zero = .true.
                    else
                       symb_zero = .false.
                       y( iy ) = beta * abs( y( iy ) )
                    end if
                    if ( alpha /= zero ) then
                       do j = 1, i
                          temp = cabs1( a( i, j ) )
                          symb_zero = symb_zero .and.( x( j ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*cabs1( x( j ) )*temp
                       end do
                       do j = i+1, n
                          temp = cabs1( a( j, i ) )
                          symb_zero = symb_zero .and.( x( j ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*cabs1( x( j ) )*temp
                       end do
                    end if
                    if (.not.symb_zero)y( iy ) = y( iy ) + sign( safe1, y( iy ) )
                    iy = iy + incy
                 end do
              end if
           else
              if ( uplo == stdlib_ilauplo( 'U' ) ) then
                 do i = 1, n
                    if ( beta == zero ) then
                       symb_zero = .true.
                       y( iy ) = zero
                    else if ( y( iy ) == zero ) then
                       symb_zero = .true.
                    else
                       symb_zero = .false.
                       y( iy ) = beta * abs( y( iy ) )
                    end if
                    jx = kx
                    if ( alpha /= zero ) then
                       do j = 1, i
                          temp = cabs1( a( j, i ) )
                          symb_zero = symb_zero .and.( x( j ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*cabs1( x( jx ) )*temp
                          jx = jx + incx
                       end do
                       do j = i+1, n
                          temp = cabs1( a( i, j ) )
                          symb_zero = symb_zero .and.( x( j ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*cabs1( x( jx ) )*temp
                          jx = jx + incx
                       end do
                    end if
                    if ( .not.symb_zero )y( iy ) = y( iy ) + sign( safe1, y( iy ) )
                    iy = iy + incy
                 end do
              else
                 do i = 1, n
                    if ( beta == zero ) then
                       symb_zero = .true.
                       y( iy ) = zero
                    else if ( y( iy ) == zero ) then
                       symb_zero = .true.
                    else
                       symb_zero = .false.
                       y( iy ) = beta * abs( y( iy ) )
                    end if
                    jx = kx
                    if ( alpha /= zero ) then
                       do j = 1, i
                          temp = cabs1( a( i, j ) )
                          symb_zero = symb_zero .and.( x( j ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*cabs1( x( jx ) )*temp
                          jx = jx + incx
                       end do
                       do j = i+1, n
                          temp = cabs1( a( j, i ) )
                          symb_zero = symb_zero .and.( x( j ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*cabs1( x( jx ) )*temp
                          jx = jx + incx
                       end do
                    end if
                    if ( .not.symb_zero )y( iy ) = y( iy ) + sign( safe1, y( iy ) )
                    iy = iy + incy
                 end do
              end if
           end if
           return
     end subroutine stdlib_zla_heamv




     pure module subroutine stdlib_sla_wwaddw( n, x, y, w )
     !! SLA_WWADDW adds a vector W into a doubled-single vector (X, Y).
     !! This works for all extant IBM's hex and binary floating point
     !! arithmetic, but not for decimal.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           real(sp), intent(inout) :: x(*), y(*)
           real(sp), intent(in) :: w(*)
        ! =====================================================================
           ! Local Scalars 
           real(sp) :: s
           integer(ilp) :: i
           ! Executable Statements 
           do 10 i = 1, n
             s = x(i) + w(i)
             s = (s + s) - s
             y(i) = ((x(i) - s) + w(i)) + y(i)
             x(i) = s
             10 continue
           return
     end subroutine stdlib_sla_wwaddw

     pure module subroutine stdlib_dla_wwaddw( n, x, y, w )
     !! DLA_WWADDW adds a vector W into a doubled-single vector (X, Y).
     !! This works for all extant IBM's hex and binary floating point
     !! arithmetic, but not for decimal.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           real(dp), intent(inout) :: x(*), y(*)
           real(dp), intent(in) :: w(*)
        ! =====================================================================
           ! Local Scalars 
           real(dp) :: s
           integer(ilp) :: i
           ! Executable Statements 
           do 10 i = 1, n
             s = x(i) + w(i)
             s = (s + s) - s
             y(i) = ((x(i) - s) + w(i)) + y(i)
             x(i) = s
             10 continue
           return
     end subroutine stdlib_dla_wwaddw


     pure module subroutine stdlib_cla_wwaddw( n, x, y, w )
     !! CLA_WWADDW adds a vector W into a doubled-single vector (X, Y).
     !! This works for all extant IBM's hex and binary floating point
     !! arithmetic, but not for decimal.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           complex(sp), intent(inout) :: x(*), y(*)
           complex(sp), intent(in) :: w(*)
        ! =====================================================================
           ! Local Scalars 
           complex(sp) :: s
           integer(ilp) :: i
           ! Executable Statements 
           do 10 i = 1, n
             s = x(i) + w(i)
             s = (s + s) - s
             y(i) = ((x(i) - s) + w(i)) + y(i)
             x(i) = s
             10 continue
           return
     end subroutine stdlib_cla_wwaddw

     pure module subroutine stdlib_zla_wwaddw( n, x, y, w )
     !! ZLA_WWADDW adds a vector W into a doubled-single vector (X, Y).
     !! This works for all extant IBM's hex and binary floating point
     !! arithmetic, but not for decimal.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           complex(dp), intent(inout) :: x(*), y(*)
           complex(dp), intent(in) :: w(*)
        ! =====================================================================
           ! Local Scalars 
           complex(dp) :: s
           integer(ilp) :: i
           ! Executable Statements 
           do 10 i = 1, n
             s = x(i) + w(i)
             s = (s + s) - s
             y(i) = ((x(i) - s) + w(i)) + y(i)
             x(i) = s
             10 continue
           return
     end subroutine stdlib_zla_wwaddw




     pure module subroutine stdlib_cspmv( uplo, n, alpha, ap, x, incx, beta, y, incy )
     !! CSPMV performs the matrix-vector operation
     !! y := alpha*A*x + beta*y,
     !! where alpha and beta are scalars, x and y are n element vectors and
     !! A is an n by n symmetric matrix, supplied in packed form.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: incx, incy, n
           complex(sp), intent(in) :: alpha, beta
           ! Array Arguments 
           complex(sp), intent(in) :: ap(*), x(*)
           complex(sp), intent(inout) :: y(*)
       ! =====================================================================
           
           
           ! Local Scalars 
           integer(ilp) :: i, info, ix, iy, j, jx, jy, k, kk, kx, ky
           complex(sp) :: temp1, temp2
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = 1_ilp
           else if( n<0_ilp ) then
              info = 2_ilp
           else if( incx==0_ilp ) then
              info = 6_ilp
           else if( incy==0_ilp ) then
              info = 9_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CSPMV ', info )
              return
           end if
           ! quick return if possible.
           if( ( n==0 ) .or. ( ( alpha==czero ) .and. ( beta==cone ) ) )return
           ! set up the start points in  x  and  y.
           if( incx>0_ilp ) then
              kx = 1_ilp
           else
              kx = 1_ilp - ( n-1 )*incx
           end if
           if( incy>0_ilp ) then
              ky = 1_ilp
           else
              ky = 1_ilp - ( n-1 )*incy
           end if
           ! start the operations. in this version the elements of the array ap
           ! are accessed sequentially with cone pass through ap.
           ! first form  y := beta*y.
           if( beta/=cone ) then
              if( incy==1_ilp ) then
                 if( beta==czero ) then
                    do i = 1, n
                       y( i ) = czero
                    end do
                 else
                    do i = 1, n
                       y( i ) = beta*y( i )
                    end do
                 end if
              else
                 iy = ky
                 if( beta==czero ) then
                    do i = 1, n
                       y( iy ) = czero
                       iy = iy + incy
                    end do
                 else
                    do i = 1, n
                       y( iy ) = beta*y( iy )
                       iy = iy + incy
                    end do
                 end if
              end if
           end if
           if( alpha==czero )return
           kk = 1_ilp
           if( stdlib_lsame( uplo, 'U' ) ) then
              ! form  y  when ap contains the upper triangle.
              if( ( incx==1_ilp ) .and. ( incy==1_ilp ) ) then
                 do j = 1, n
                    temp1 = alpha*x( j )
                    temp2 = czero
                    k = kk
                    do i = 1, j - 1
                       y( i ) = y( i ) + temp1*ap( k )
                       temp2 = temp2 + ap( k )*x( i )
                       k = k + 1_ilp
                    end do
                    y( j ) = y( j ) + temp1*ap( kk+j-1 ) + alpha*temp2
                    kk = kk + j
                 end do
              else
                 jx = kx
                 jy = ky
                 do j = 1, n
                    temp1 = alpha*x( jx )
                    temp2 = czero
                    ix = kx
                    iy = ky
                    do k = kk, kk + j - 2
                       y( iy ) = y( iy ) + temp1*ap( k )
                       temp2 = temp2 + ap( k )*x( ix )
                       ix = ix + incx
                       iy = iy + incy
                    end do
                    y( jy ) = y( jy ) + temp1*ap( kk+j-1 ) + alpha*temp2
                    jx = jx + incx
                    jy = jy + incy
                    kk = kk + j
                 end do
              end if
           else
              ! form  y  when ap contains the lower triangle.
              if( ( incx==1_ilp ) .and. ( incy==1_ilp ) ) then
                 do j = 1, n
                    temp1 = alpha*x( j )
                    temp2 = czero
                    y( j ) = y( j ) + temp1*ap( kk )
                    k = kk + 1_ilp
                    do i = j + 1, n
                       y( i ) = y( i ) + temp1*ap( k )
                       temp2 = temp2 + ap( k )*x( i )
                       k = k + 1_ilp
                    end do
                    y( j ) = y( j ) + alpha*temp2
                    kk = kk + ( n-j+1 )
                 end do
              else
                 jx = kx
                 jy = ky
                 do j = 1, n
                    temp1 = alpha*x( jx )
                    temp2 = czero
                    y( jy ) = y( jy ) + temp1*ap( kk )
                    ix = jx
                    iy = jy
                    do k = kk + 1, kk + n - j
                       ix = ix + incx
                       iy = iy + incy
                       y( iy ) = y( iy ) + temp1*ap( k )
                       temp2 = temp2 + ap( k )*x( ix )
                    end do
                    y( jy ) = y( jy ) + alpha*temp2
                    jx = jx + incx
                    jy = jy + incy
                    kk = kk + ( n-j+1 )
                 end do
              end if
           end if
           return
     end subroutine stdlib_cspmv

     pure module subroutine stdlib_zspmv( uplo, n, alpha, ap, x, incx, beta, y, incy )
     !! ZSPMV performs the matrix-vector operation
     !! y := alpha*A*x + beta*y,
     !! where alpha and beta are scalars, x and y are n element vectors and
     !! A is an n by n symmetric matrix, supplied in packed form.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: incx, incy, n
           complex(dp), intent(in) :: alpha, beta
           ! Array Arguments 
           complex(dp), intent(in) :: ap(*), x(*)
           complex(dp), intent(inout) :: y(*)
       ! =====================================================================
           
           
           ! Local Scalars 
           integer(ilp) :: i, info, ix, iy, j, jx, jy, k, kk, kx, ky
           complex(dp) :: temp1, temp2
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = 1_ilp
           else if( n<0_ilp ) then
              info = 2_ilp
           else if( incx==0_ilp ) then
              info = 6_ilp
           else if( incy==0_ilp ) then
              info = 9_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZSPMV ', info )
              return
           end if
           ! quick return if possible.
           if( ( n==0 ) .or. ( ( alpha==czero ) .and. ( beta==cone ) ) )return
           ! set up the start points in  x  and  y.
           if( incx>0_ilp ) then
              kx = 1_ilp
           else
              kx = 1_ilp - ( n-1 )*incx
           end if
           if( incy>0_ilp ) then
              ky = 1_ilp
           else
              ky = 1_ilp - ( n-1 )*incy
           end if
           ! start the operations. in this version the elements of the array ap
           ! are accessed sequentially with cone pass through ap.
           ! first form  y := beta*y.
           if( beta/=cone ) then
              if( incy==1_ilp ) then
                 if( beta==czero ) then
                    do i = 1, n
                       y( i ) = czero
                    end do
                 else
                    do i = 1, n
                       y( i ) = beta*y( i )
                    end do
                 end if
              else
                 iy = ky
                 if( beta==czero ) then
                    do i = 1, n
                       y( iy ) = czero
                       iy = iy + incy
                    end do
                 else
                    do i = 1, n
                       y( iy ) = beta*y( iy )
                       iy = iy + incy
                    end do
                 end if
              end if
           end if
           if( alpha==czero )return
           kk = 1_ilp
           if( stdlib_lsame( uplo, 'U' ) ) then
              ! form  y  when ap contains the upper triangle.
              if( ( incx==1_ilp ) .and. ( incy==1_ilp ) ) then
                 do j = 1, n
                    temp1 = alpha*x( j )
                    temp2 = czero
                    k = kk
                    do i = 1, j - 1
                       y( i ) = y( i ) + temp1*ap( k )
                       temp2 = temp2 + ap( k )*x( i )
                       k = k + 1_ilp
                    end do
                    y( j ) = y( j ) + temp1*ap( kk+j-1 ) + alpha*temp2
                    kk = kk + j
                 end do
              else
                 jx = kx
                 jy = ky
                 do j = 1, n
                    temp1 = alpha*x( jx )
                    temp2 = czero
                    ix = kx
                    iy = ky
                    do k = kk, kk + j - 2
                       y( iy ) = y( iy ) + temp1*ap( k )
                       temp2 = temp2 + ap( k )*x( ix )
                       ix = ix + incx
                       iy = iy + incy
                    end do
                    y( jy ) = y( jy ) + temp1*ap( kk+j-1 ) + alpha*temp2
                    jx = jx + incx
                    jy = jy + incy
                    kk = kk + j
                 end do
              end if
           else
              ! form  y  when ap contains the lower triangle.
              if( ( incx==1_ilp ) .and. ( incy==1_ilp ) ) then
                 do j = 1, n
                    temp1 = alpha*x( j )
                    temp2 = czero
                    y( j ) = y( j ) + temp1*ap( kk )
                    k = kk + 1_ilp
                    do i = j + 1, n
                       y( i ) = y( i ) + temp1*ap( k )
                       temp2 = temp2 + ap( k )*x( i )
                       k = k + 1_ilp
                    end do
                    y( j ) = y( j ) + alpha*temp2
                    kk = kk + ( n-j+1 )
                 end do
              else
                 jx = kx
                 jy = ky
                 do j = 1, n
                    temp1 = alpha*x( jx )
                    temp2 = czero
                    y( jy ) = y( jy ) + temp1*ap( kk )
                    ix = jx
                    iy = jy
                    do k = kk + 1, kk + n - j
                       ix = ix + incx
                       iy = iy + incy
                       y( iy ) = y( iy ) + temp1*ap( k )
                       temp2 = temp2 + ap( k )*x( ix )
                    end do
                    y( jy ) = y( jy ) + alpha*temp2
                    jx = jx + incx
                    jy = jy + incy
                    kk = kk + ( n-j+1 )
                 end do
              end if
           end if
           return
     end subroutine stdlib_zspmv




     pure module subroutine stdlib_cspr( uplo, n, alpha, x, incx, ap )
     !! CSPR performs the symmetric rank 1 operation
     !! A := alpha*x*x**H + A,
     !! where alpha is a complex scalar, x is an n element vector and A is an
     !! n by n symmetric matrix, supplied in packed form.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: incx, n
           complex(sp), intent(in) :: alpha
           ! Array Arguments 
           complex(sp), intent(inout) :: ap(*)
           complex(sp), intent(in) :: x(*)
       ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, info, ix, j, jx, k, kk, kx
           complex(sp) :: temp
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = 1_ilp
           else if( n<0_ilp ) then
              info = 2_ilp
           else if( incx==0_ilp ) then
              info = 5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CSPR  ', info )
              return
           end if
           ! quick return if possible.
           if( ( n==0 ) .or. ( alpha==czero ) )return
           ! set the start point in x if the increment is not unity.
           if( incx<=0_ilp ) then
              kx = 1_ilp - ( n-1 )*incx
           else if( incx/=1_ilp ) then
              kx = 1_ilp
           end if
           ! start the operations. in this version the elements of the array ap
           ! are accessed sequentially with cone pass through ap.
           kk = 1_ilp
           if( stdlib_lsame( uplo, 'U' ) ) then
              ! form  a  when upper triangle is stored in ap.
              if( incx==1_ilp ) then
                 do j = 1, n
                    if( x( j )/=czero ) then
                       temp = alpha*x( j )
                       k = kk
                       do i = 1, j - 1
                          ap( k ) = ap( k ) + x( i )*temp
                          k = k + 1_ilp
                       end do
                       ap( kk+j-1 ) = ap( kk+j-1 ) + x( j )*temp
                    else
                       ap( kk+j-1 ) = ap( kk+j-1 )
                    end if
                    kk = kk + j
                 end do
              else
                 jx = kx
                 do j = 1, n
                    if( x( jx )/=czero ) then
                       temp = alpha*x( jx )
                       ix = kx
                       do k = kk, kk + j - 2
                          ap( k ) = ap( k ) + x( ix )*temp
                          ix = ix + incx
                       end do
                       ap( kk+j-1 ) = ap( kk+j-1 ) + x( jx )*temp
                    else
                       ap( kk+j-1 ) = ap( kk+j-1 )
                    end if
                    jx = jx + incx
                    kk = kk + j
                 end do
              end if
           else
              ! form  a  when lower triangle is stored in ap.
              if( incx==1_ilp ) then
                 do j = 1, n
                    if( x( j )/=czero ) then
                       temp = alpha*x( j )
                       ap( kk ) = ap( kk ) + temp*x( j )
                       k = kk + 1_ilp
                       do i = j + 1, n
                          ap( k ) = ap( k ) + x( i )*temp
                          k = k + 1_ilp
                       end do
                    else
                       ap( kk ) = ap( kk )
                    end if
                    kk = kk + n - j + 1_ilp
                 end do
              else
                 jx = kx
                 do j = 1, n
                    if( x( jx )/=czero ) then
                       temp = alpha*x( jx )
                       ap( kk ) = ap( kk ) + temp*x( jx )
                       ix = jx
                       do k = kk + 1, kk + n - j
                          ix = ix + incx
                          ap( k ) = ap( k ) + x( ix )*temp
                       end do
                    else
                       ap( kk ) = ap( kk )
                    end if
                    jx = jx + incx
                    kk = kk + n - j + 1_ilp
                 end do
              end if
           end if
           return
     end subroutine stdlib_cspr

     pure module subroutine stdlib_zspr( uplo, n, alpha, x, incx, ap )
     !! ZSPR performs the symmetric rank 1 operation
     !! A := alpha*x*x**H + A,
     !! where alpha is a complex scalar, x is an n element vector and A is an
     !! n by n symmetric matrix, supplied in packed form.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: incx, n
           complex(dp), intent(in) :: alpha
           ! Array Arguments 
           complex(dp), intent(inout) :: ap(*)
           complex(dp), intent(in) :: x(*)
       ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, info, ix, j, jx, k, kk, kx
           complex(dp) :: temp
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = 1_ilp
           else if( n<0_ilp ) then
              info = 2_ilp
           else if( incx==0_ilp ) then
              info = 5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZSPR  ', info )
              return
           end if
           ! quick return if possible.
           if( ( n==0 ) .or. ( alpha==czero ) )return
           ! set the start point in x if the increment is not unity.
           if( incx<=0_ilp ) then
              kx = 1_ilp - ( n-1 )*incx
           else if( incx/=1_ilp ) then
              kx = 1_ilp
           end if
           ! start the operations. in this version the elements of the array ap
           ! are accessed sequentially with cone pass through ap.
           kk = 1_ilp
           if( stdlib_lsame( uplo, 'U' ) ) then
              ! form  a  when upper triangle is stored in ap.
              if( incx==1_ilp ) then
                 do j = 1, n
                    if( x( j )/=czero ) then
                       temp = alpha*x( j )
                       k = kk
                       do i = 1, j - 1
                          ap( k ) = ap( k ) + x( i )*temp
                          k = k + 1_ilp
                       end do
                       ap( kk+j-1 ) = ap( kk+j-1 ) + x( j )*temp
                    else
                       ap( kk+j-1 ) = ap( kk+j-1 )
                    end if
                    kk = kk + j
                 end do
              else
                 jx = kx
                 do j = 1, n
                    if( x( jx )/=czero ) then
                       temp = alpha*x( jx )
                       ix = kx
                       do k = kk, kk + j - 2
                          ap( k ) = ap( k ) + x( ix )*temp
                          ix = ix + incx
                       end do
                       ap( kk+j-1 ) = ap( kk+j-1 ) + x( jx )*temp
                    else
                       ap( kk+j-1 ) = ap( kk+j-1 )
                    end if
                    jx = jx + incx
                    kk = kk + j
                 end do
              end if
           else
              ! form  a  when lower triangle is stored in ap.
              if( incx==1_ilp ) then
                 do j = 1, n
                    if( x( j )/=czero ) then
                       temp = alpha*x( j )
                       ap( kk ) = ap( kk ) + temp*x( j )
                       k = kk + 1_ilp
                       do i = j + 1, n
                          ap( k ) = ap( k ) + x( i )*temp
                          k = k + 1_ilp
                       end do
                    else
                       ap( kk ) = ap( kk )
                    end if
                    kk = kk + n - j + 1_ilp
                 end do
              else
                 jx = kx
                 do j = 1, n
                    if( x( jx )/=czero ) then
                       temp = alpha*x( jx )
                       ap( kk ) = ap( kk ) + temp*x( jx )
                       ix = jx
                       do k = kk + 1, kk + n - j
                          ix = ix + incx
                          ap( k ) = ap( k ) + x( ix )*temp
                       end do
                    else
                       ap( kk ) = ap( kk )
                    end if
                    jx = jx + incx
                    kk = kk + n - j + 1_ilp
                 end do
              end if
           end if
           return
     end subroutine stdlib_zspr




     pure module subroutine stdlib_csymv( uplo, n, alpha, a, lda, x, incx, beta, y, incy )
     !! CSYMV performs the matrix-vector  operation
     !! y := alpha*A*x + beta*y,
     !! where alpha and beta are scalars, x and y are n element vectors and
     !! A is an n by n symmetric matrix.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: incx, incy, lda, n
           complex(sp), intent(in) :: alpha, beta
           ! Array Arguments 
           complex(sp), intent(in) :: a(lda,*), x(*)
           complex(sp), intent(inout) :: y(*)
       ! =====================================================================
           
           
           ! Local Scalars 
           integer(ilp) :: i, info, ix, iy, j, jx, jy, kx, ky
           complex(sp) :: temp1, temp2
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = 1_ilp
           else if( n<0_ilp ) then
              info = 2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = 5_ilp
           else if( incx==0_ilp ) then
              info = 7_ilp
           else if( incy==0_ilp ) then
              info = 10_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CSYMV ', info )
              return
           end if
           ! quick return if possible.
           if( ( n==0 ) .or. ( ( alpha==czero ) .and. ( beta==cone ) ) )return
           ! set up the start points in  x  and  y.
           if( incx>0_ilp ) then
              kx = 1_ilp
           else
              kx = 1_ilp - ( n-1 )*incx
           end if
           if( incy>0_ilp ) then
              ky = 1_ilp
           else
              ky = 1_ilp - ( n-1 )*incy
           end if
           ! start the operations. in this version the elements of a are
           ! accessed sequentially with cone pass through the triangular part
           ! of a.
           ! first form  y := beta*y.
           if( beta/=cone ) then
              if( incy==1_ilp ) then
                 if( beta==czero ) then
                    do i = 1, n
                       y( i ) = czero
                    end do
                 else
                    do i = 1, n
                       y( i ) = beta*y( i )
                    end do
                 end if
              else
                 iy = ky
                 if( beta==czero ) then
                    do i = 1, n
                       y( iy ) = czero
                       iy = iy + incy
                    end do
                 else
                    do i = 1, n
                       y( iy ) = beta*y( iy )
                       iy = iy + incy
                    end do
                 end if
              end if
           end if
           if( alpha==czero )return
           if( stdlib_lsame( uplo, 'U' ) ) then
              ! form  y  when a is stored in upper triangle.
              if( ( incx==1_ilp ) .and. ( incy==1_ilp ) ) then
                 do j = 1, n
                    temp1 = alpha*x( j )
                    temp2 = czero
                    do i = 1, j - 1
                       y( i ) = y( i ) + temp1*a( i, j )
                       temp2 = temp2 + a( i, j )*x( i )
                    end do
                    y( j ) = y( j ) + temp1*a( j, j ) + alpha*temp2
                 end do
              else
                 jx = kx
                 jy = ky
                 do j = 1, n
                    temp1 = alpha*x( jx )
                    temp2 = czero
                    ix = kx
                    iy = ky
                    do i = 1, j - 1
                       y( iy ) = y( iy ) + temp1*a( i, j )
                       temp2 = temp2 + a( i, j )*x( ix )
                       ix = ix + incx
                       iy = iy + incy
                    end do
                    y( jy ) = y( jy ) + temp1*a( j, j ) + alpha*temp2
                    jx = jx + incx
                    jy = jy + incy
                 end do
              end if
           else
              ! form  y  when a is stored in lower triangle.
              if( ( incx==1_ilp ) .and. ( incy==1_ilp ) ) then
                 do j = 1, n
                    temp1 = alpha*x( j )
                    temp2 = czero
                    y( j ) = y( j ) + temp1*a( j, j )
                    do i = j + 1, n
                       y( i ) = y( i ) + temp1*a( i, j )
                       temp2 = temp2 + a( i, j )*x( i )
                    end do
                    y( j ) = y( j ) + alpha*temp2
                 end do
              else
                 jx = kx
                 jy = ky
                 do j = 1, n
                    temp1 = alpha*x( jx )
                    temp2 = czero
                    y( jy ) = y( jy ) + temp1*a( j, j )
                    ix = jx
                    iy = jy
                    do i = j + 1, n
                       ix = ix + incx
                       iy = iy + incy
                       y( iy ) = y( iy ) + temp1*a( i, j )
                       temp2 = temp2 + a( i, j )*x( ix )
                    end do
                    y( jy ) = y( jy ) + alpha*temp2
                    jx = jx + incx
                    jy = jy + incy
                 end do
              end if
           end if
           return
     end subroutine stdlib_csymv

     pure module subroutine stdlib_zsymv( uplo, n, alpha, a, lda, x, incx, beta, y, incy )
     !! ZSYMV performs the matrix-vector  operation
     !! y := alpha*A*x + beta*y,
     !! where alpha and beta are scalars, x and y are n element vectors and
     !! A is an n by n symmetric matrix.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: incx, incy, lda, n
           complex(dp), intent(in) :: alpha, beta
           ! Array Arguments 
           complex(dp), intent(in) :: a(lda,*), x(*)
           complex(dp), intent(inout) :: y(*)
       ! =====================================================================
           
           
           ! Local Scalars 
           integer(ilp) :: i, info, ix, iy, j, jx, jy, kx, ky
           complex(dp) :: temp1, temp2
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = 1_ilp
           else if( n<0_ilp ) then
              info = 2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = 5_ilp
           else if( incx==0_ilp ) then
              info = 7_ilp
           else if( incy==0_ilp ) then
              info = 10_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZSYMV ', info )
              return
           end if
           ! quick return if possible.
           if( ( n==0 ) .or. ( ( alpha==czero ) .and. ( beta==cone ) ) )return
           ! set up the start points in  x  and  y.
           if( incx>0_ilp ) then
              kx = 1_ilp
           else
              kx = 1_ilp - ( n-1 )*incx
           end if
           if( incy>0_ilp ) then
              ky = 1_ilp
           else
              ky = 1_ilp - ( n-1 )*incy
           end if
           ! start the operations. in this version the elements of a are
           ! accessed sequentially with cone pass through the triangular part
           ! of a.
           ! first form  y := beta*y.
           if( beta/=cone ) then
              if( incy==1_ilp ) then
                 if( beta==czero ) then
                    do i = 1, n
                       y( i ) = czero
                    end do
                 else
                    do i = 1, n
                       y( i ) = beta*y( i )
                    end do
                 end if
              else
                 iy = ky
                 if( beta==czero ) then
                    do i = 1, n
                       y( iy ) = czero
                       iy = iy + incy
                    end do
                 else
                    do i = 1, n
                       y( iy ) = beta*y( iy )
                       iy = iy + incy
                    end do
                 end if
              end if
           end if
           if( alpha==czero )return
           if( stdlib_lsame( uplo, 'U' ) ) then
              ! form  y  when a is stored in upper triangle.
              if( ( incx==1_ilp ) .and. ( incy==1_ilp ) ) then
                 do j = 1, n
                    temp1 = alpha*x( j )
                    temp2 = czero
                    do i = 1, j - 1
                       y( i ) = y( i ) + temp1*a( i, j )
                       temp2 = temp2 + a( i, j )*x( i )
                    end do
                    y( j ) = y( j ) + temp1*a( j, j ) + alpha*temp2
                 end do
              else
                 jx = kx
                 jy = ky
                 do j = 1, n
                    temp1 = alpha*x( jx )
                    temp2 = czero
                    ix = kx
                    iy = ky
                    do i = 1, j - 1
                       y( iy ) = y( iy ) + temp1*a( i, j )
                       temp2 = temp2 + a( i, j )*x( ix )
                       ix = ix + incx
                       iy = iy + incy
                    end do
                    y( jy ) = y( jy ) + temp1*a( j, j ) + alpha*temp2
                    jx = jx + incx
                    jy = jy + incy
                 end do
              end if
           else
              ! form  y  when a is stored in lower triangle.
              if( ( incx==1_ilp ) .and. ( incy==1_ilp ) ) then
                 do j = 1, n
                    temp1 = alpha*x( j )
                    temp2 = czero
                    y( j ) = y( j ) + temp1*a( j, j )
                    do i = j + 1, n
                       y( i ) = y( i ) + temp1*a( i, j )
                       temp2 = temp2 + a( i, j )*x( i )
                    end do
                    y( j ) = y( j ) + alpha*temp2
                 end do
              else
                 jx = kx
                 jy = ky
                 do j = 1, n
                    temp1 = alpha*x( jx )
                    temp2 = czero
                    y( jy ) = y( jy ) + temp1*a( j, j )
                    ix = jx
                    iy = jy
                    do i = j + 1, n
                       ix = ix + incx
                       iy = iy + incy
                       y( iy ) = y( iy ) + temp1*a( i, j )
                       temp2 = temp2 + a( i, j )*x( ix )
                    end do
                    y( jy ) = y( jy ) + alpha*temp2
                    jx = jx + incx
                    jy = jy + incy
                 end do
              end if
           end if
           return
     end subroutine stdlib_zsymv




     pure module subroutine stdlib_csyr( uplo, n, alpha, x, incx, a, lda )
     !! CSYR performs the symmetric rank 1 operation
     !! A := alpha*x*x**H + A,
     !! where alpha is a complex scalar, x is an n element vector and A is an
     !! n by n symmetric matrix.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: incx, lda, n
           complex(sp), intent(in) :: alpha
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(in) :: x(*)
       ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, info, ix, j, jx, kx
           complex(sp) :: temp
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = 1_ilp
           else if( n<0_ilp ) then
              info = 2_ilp
           else if( incx==0_ilp ) then
              info = 5_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = 7_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CSYR  ', info )
              return
           end if
           ! quick return if possible.
           if( ( n==0 ) .or. ( alpha==czero ) )return
           ! set the start point in x if the increment is not unity.
           if( incx<=0_ilp ) then
              kx = 1_ilp - ( n-1 )*incx
           else if( incx/=1_ilp ) then
              kx = 1_ilp
           end if
           ! start the operations. in this version the elements of a are
           ! accessed sequentially with cone pass through the triangular part
           ! of a.
           if( stdlib_lsame( uplo, 'U' ) ) then
              ! form  a  when a is stored in upper triangle.
              if( incx==1_ilp ) then
                 do j = 1, n
                    if( x( j )/=czero ) then
                       temp = alpha*x( j )
                       do i = 1, j
                          a( i, j ) = a( i, j ) + x( i )*temp
                       end do
                    end if
                 end do
              else
                 jx = kx
                 do j = 1, n
                    if( x( jx )/=czero ) then
                       temp = alpha*x( jx )
                       ix = kx
                       do i = 1, j
                          a( i, j ) = a( i, j ) + x( ix )*temp
                          ix = ix + incx
                       end do
                    end if
                    jx = jx + incx
                 end do
              end if
           else
              ! form  a  when a is stored in lower triangle.
              if( incx==1_ilp ) then
                 do j = 1, n
                    if( x( j )/=czero ) then
                       temp = alpha*x( j )
                       do i = j, n
                          a( i, j ) = a( i, j ) + x( i )*temp
                       end do
                    end if
                 end do
              else
                 jx = kx
                 do j = 1, n
                    if( x( jx )/=czero ) then
                       temp = alpha*x( jx )
                       ix = jx
                       do i = j, n
                          a( i, j ) = a( i, j ) + x( ix )*temp
                          ix = ix + incx
                       end do
                    end if
                    jx = jx + incx
                 end do
              end if
           end if
           return
     end subroutine stdlib_csyr

     pure module subroutine stdlib_zsyr( uplo, n, alpha, x, incx, a, lda )
     !! ZSYR performs the symmetric rank 1 operation
     !! A := alpha*x*x**H + A,
     !! where alpha is a complex scalar, x is an n element vector and A is an
     !! n by n symmetric matrix.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: incx, lda, n
           complex(dp), intent(in) :: alpha
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(in) :: x(*)
       ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, info, ix, j, jx, kx
           complex(dp) :: temp
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = 1_ilp
           else if( n<0_ilp ) then
              info = 2_ilp
           else if( incx==0_ilp ) then
              info = 5_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = 7_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZSYR  ', info )
              return
           end if
           ! quick return if possible.
           if( ( n==0 ) .or. ( alpha==czero ) )return
           ! set the start point in x if the increment is not unity.
           if( incx<=0_ilp ) then
              kx = 1_ilp - ( n-1 )*incx
           else if( incx/=1_ilp ) then
              kx = 1_ilp
           end if
           ! start the operations. in this version the elements of a are
           ! accessed sequentially with cone pass through the triangular part
           ! of a.
           if( stdlib_lsame( uplo, 'U' ) ) then
              ! form  a  when a is stored in upper triangle.
              if( incx==1_ilp ) then
                 do j = 1, n
                    if( x( j )/=czero ) then
                       temp = alpha*x( j )
                       do i = 1, j
                          a( i, j ) = a( i, j ) + x( i )*temp
                       end do
                    end if
                 end do
              else
                 jx = kx
                 do j = 1, n
                    if( x( jx )/=czero ) then
                       temp = alpha*x( jx )
                       ix = kx
                       do i = 1, j
                          a( i, j ) = a( i, j ) + x( ix )*temp
                          ix = ix + incx
                       end do
                    end if
                    jx = jx + incx
                 end do
              end if
           else
              ! form  a  when a is stored in lower triangle.
              if( incx==1_ilp ) then
                 do j = 1, n
                    if( x( j )/=czero ) then
                       temp = alpha*x( j )
                       do i = j, n
                          a( i, j ) = a( i, j ) + x( i )*temp
                       end do
                    end if
                 end do
              else
                 jx = kx
                 do j = 1, n
                    if( x( jx )/=czero ) then
                       temp = alpha*x( jx )
                       ix = jx
                       do i = j, n
                          a( i, j ) = a( i, j ) + x( ix )*temp
                          ix = ix + incx
                       end do
                    end if
                    jx = jx + incx
                 end do
              end if
           end if
           return
     end subroutine stdlib_zsyr




     pure module subroutine stdlib_I64_slascl( type, kl, ku, cfrom, cto, m, n, a, lda, info )
     !! SLASCL multiplies the M by N real matrix A by the real scalar
     !! CTO/CFROM.  This is done without over/underflow as long as the final
     !! result CTO*A(I,J)/CFROM does not over/underflow. TYPE specifies that
     !! A may be full, upper triangular, lower triangular, upper Hessenberg,
     !! or banded.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: zero, half, one
           ! Scalar Arguments 
           character, intent(in) :: type
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: kl, ku, lda, m, n
           real(sp), intent(in) :: cfrom, cto
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: done
           integer(ilp64) :: i, itype, j, k1, k2, k3, k4
           real(sp) :: bignum, cfrom1, cfromc, cto1, ctoc, mul, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp64
           if( stdlib_lsame( type, 'G' ) ) then
              itype = 0_ilp64
           else if( stdlib_lsame( type, 'L' ) ) then
              itype = 1_ilp64
           else if( stdlib_lsame( type, 'U' ) ) then
              itype = 2_ilp64
           else if( stdlib_lsame( type, 'H' ) ) then
              itype = 3_ilp64
           else if( stdlib_lsame( type, 'B' ) ) then
              itype = 4_ilp64
           else if( stdlib_lsame( type, 'Q' ) ) then
              itype = 5_ilp64
           else if( stdlib_lsame( type, 'Z' ) ) then
              itype = 6_ilp64
           else
              itype = -1_ilp64
           end if
           if( itype==-1_ilp64 ) then
              info = -1_ilp64
           else if( cfrom==zero .or. stdlib_I64_sisnan(cfrom) ) then
              info = -4_ilp64
           else if( stdlib_I64_sisnan(cto) ) then
              info = -5_ilp64
           else if( m<0_ilp64 ) then
              info = -6_ilp64
           else if( n<0_ilp64 .or. ( itype==4_ilp64 .and. n/=m ) .or.( itype==5_ilp64 .and. n/=m ) ) then
              info = -7_ilp64
           else if( itype<=3_ilp64 .and. lda<max( 1_ilp64, m ) ) then
              info = -9_ilp64
           else if( itype>=4_ilp64 ) then
              if( kl<0_ilp64 .or. kl>max( m-1, 0_ilp64 ) ) then
                 info = -2_ilp64
              else if( ku<0_ilp64 .or. ku>max( n-1, 0_ilp64 ) .or.( ( itype==4_ilp64 .or. itype==5_ilp64 ) .and. kl/=ku ) &
                        )then
                 info = -3_ilp64
              else if( ( itype==4_ilp64 .and. lda<kl+1 ) .or.( itype==5_ilp64 .and. lda<ku+1 ) .or.( itype==6_ilp64 &
                        .and. lda<2_ilp64*kl+ku+1 ) ) then
                 info = -9_ilp64
              end if
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'SLASCL', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. m==0 )return
           ! get machine parameters
           smlnum = stdlib_I64_slamch( 'S' )
           bignum = one / smlnum
           cfromc = cfrom
           ctoc = cto
           10 continue
           cfrom1 = cfromc*smlnum
           if( cfrom1==cfromc ) then
              ! cfromc is an inf.  multiply by a correctly signed zero for
              ! finite ctoc, or a nan if ctoc is infinite.
              mul = ctoc / cfromc
              done = .true.
              cto1 = ctoc
           else
              cto1 = ctoc / bignum
              if( cto1==ctoc ) then
                 ! ctoc is either 0 or an inf.  in both cases, ctoc itself
                 ! serves as the correct multiplication factor.
                 mul = ctoc
                 done = .true.
                 cfromc = one
              else if( abs( cfrom1 )>abs( ctoc ) .and. ctoc/=zero ) then
                 mul = smlnum
                 done = .false.
                 cfromc = cfrom1
              else if( abs( cto1 )>abs( cfromc ) ) then
                 mul = bignum
                 done = .false.
                 ctoc = cto1
              else
                 mul = ctoc / cfromc
                 done = .true.
              end if
           end if
           if( itype==0_ilp64 ) then
              ! full matrix
              do j = 1, n
                 do i = 1, m
                    a( i, j ) = a( i, j )*mul
                 end do
              end do
           else if( itype==1_ilp64 ) then
              ! lower triangular matrix
              do j = 1, n
                 do i = j, m
                    a( i, j ) = a( i, j )*mul
                 end do
              end do
           else if( itype==2_ilp64 ) then
              ! upper triangular matrix
              do j = 1, n
                 do i = 1, min( j, m )
                    a( i, j ) = a( i, j )*mul
                 end do
              end do
           else if( itype==3_ilp64 ) then
              ! upper hessenberg matrix
              do j = 1, n
                 do i = 1, min( j+1, m )
                    a( i, j ) = a( i, j )*mul
                 end do
              end do
           else if( itype==4_ilp64 ) then
              ! lower half of a symmetric band matrix
              k3 = kl + 1_ilp64
              k4 = n + 1_ilp64
              do j = 1, n
                 do i = 1, min( k3, k4-j )
                    a( i, j ) = a( i, j )*mul
                 end do
              end do
           else if( itype==5_ilp64 ) then
              ! upper half of a symmetric band matrix
              k1 = ku + 2_ilp64
              k3 = ku + 1_ilp64
              do j = 1, n
                 do i = max( k1-j, 1 ), k3
                    a( i, j ) = a( i, j )*mul
                 end do
              end do
           else if( itype==6_ilp64 ) then
              ! band matrix
              k1 = kl + ku + 2_ilp64
              k2 = kl + 1_ilp64
              k3 = 2_ilp64*kl + ku + 1_ilp64
              k4 = kl + ku + 1_ilp64 + m
              do j = 1, n
                 do i = max( k1-j, k2 ), min( k3, k4-j )
                    a( i, j ) = a( i, j )*mul
                 end do
              end do
           end if
           if( .not.done )go to 10
           return
     end subroutine stdlib_I64_slascl

     pure module subroutine stdlib_I64_dlascl( type, kl, ku, cfrom, cto, m, n, a, lda, info )
     !! DLASCL multiplies the M by N real matrix A by the real scalar
     !! CTO/CFROM.  This is done without over/underflow as long as the final
     !! result CTO*A(I,J)/CFROM does not over/underflow. TYPE specifies that
     !! A may be full, upper triangular, lower triangular, upper Hessenberg,
     !! or banded.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: zero, half, one
           ! Scalar Arguments 
           character, intent(in) :: type
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: kl, ku, lda, m, n
           real(dp), intent(in) :: cfrom, cto
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: done
           integer(ilp64) :: i, itype, j, k1, k2, k3, k4
           real(dp) :: bignum, cfrom1, cfromc, cto1, ctoc, mul, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp64
           if( stdlib_lsame( type, 'G' ) ) then
              itype = 0_ilp64
           else if( stdlib_lsame( type, 'L' ) ) then
              itype = 1_ilp64
           else if( stdlib_lsame( type, 'U' ) ) then
              itype = 2_ilp64
           else if( stdlib_lsame( type, 'H' ) ) then
              itype = 3_ilp64
           else if( stdlib_lsame( type, 'B' ) ) then
              itype = 4_ilp64
           else if( stdlib_lsame( type, 'Q' ) ) then
              itype = 5_ilp64
           else if( stdlib_lsame( type, 'Z' ) ) then
              itype = 6_ilp64
           else
              itype = -1_ilp64
           end if
           if( itype==-1_ilp64 ) then
              info = -1_ilp64
           else if( cfrom==zero .or. stdlib_I64_disnan(cfrom) ) then
              info = -4_ilp64
           else if( stdlib_I64_disnan(cto) ) then
              info = -5_ilp64
           else if( m<0_ilp64 ) then
              info = -6_ilp64
           else if( n<0_ilp64 .or. ( itype==4_ilp64 .and. n/=m ) .or.( itype==5_ilp64 .and. n/=m ) ) then
              info = -7_ilp64
           else if( itype<=3_ilp64 .and. lda<max( 1_ilp64, m ) ) then
              info = -9_ilp64
           else if( itype>=4_ilp64 ) then
              if( kl<0_ilp64 .or. kl>max( m-1, 0_ilp64 ) ) then
                 info = -2_ilp64
              else if( ku<0_ilp64 .or. ku>max( n-1, 0_ilp64 ) .or.( ( itype==4_ilp64 .or. itype==5_ilp64 ) .and. kl/=ku ) &
                        )then
                 info = -3_ilp64
              else if( ( itype==4_ilp64 .and. lda<kl+1 ) .or.( itype==5_ilp64 .and. lda<ku+1 ) .or.( itype==6_ilp64 &
                        .and. lda<2_ilp64*kl+ku+1 ) ) then
                 info = -9_ilp64
              end if
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'DLASCL', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. m==0 )return
           ! get machine parameters
           smlnum = stdlib_I64_dlamch( 'S' )
           bignum = one / smlnum
           cfromc = cfrom
           ctoc = cto
           10 continue
           cfrom1 = cfromc*smlnum
           if( cfrom1==cfromc ) then
              ! cfromc is an inf.  multiply by a correctly signed zero for
              ! finite ctoc, or a nan if ctoc is infinite.
              mul = ctoc / cfromc
              done = .true.
              cto1 = ctoc
           else
              cto1 = ctoc / bignum
              if( cto1==ctoc ) then
                 ! ctoc is either 0 or an inf.  in both cases, ctoc itself
                 ! serves as the correct multiplication factor.
                 mul = ctoc
                 done = .true.
                 cfromc = one
              else if( abs( cfrom1 )>abs( ctoc ) .and. ctoc/=zero ) then
                 mul = smlnum
                 done = .false.
                 cfromc = cfrom1
              else if( abs( cto1 )>abs( cfromc ) ) then
                 mul = bignum
                 done = .false.
                 ctoc = cto1
              else
                 mul = ctoc / cfromc
                 done = .true.
              end if
           end if
           if( itype==0_ilp64 ) then
              ! full matrix
              do j = 1, n
                 do i = 1, m
                    a( i, j ) = a( i, j )*mul
                 end do
              end do
           else if( itype==1_ilp64 ) then
              ! lower triangular matrix
              do j = 1, n
                 do i = j, m
                    a( i, j ) = a( i, j )*mul
                 end do
              end do
           else if( itype==2_ilp64 ) then
              ! upper triangular matrix
              do j = 1, n
                 do i = 1, min( j, m )
                    a( i, j ) = a( i, j )*mul
                 end do
              end do
           else if( itype==3_ilp64 ) then
              ! upper hessenberg matrix
              do j = 1, n
                 do i = 1, min( j+1, m )
                    a( i, j ) = a( i, j )*mul
                 end do
              end do
           else if( itype==4_ilp64 ) then
              ! lower half of a symmetric band matrix
              k3 = kl + 1_ilp64
              k4 = n + 1_ilp64
              do j = 1, n
                 do i = 1, min( k3, k4-j )
                    a( i, j ) = a( i, j )*mul
                 end do
              end do
           else if( itype==5_ilp64 ) then
              ! upper half of a symmetric band matrix
              k1 = ku + 2_ilp64
              k3 = ku + 1_ilp64
              do j = 1, n
                 do i = max( k1-j, 1 ), k3
                    a( i, j ) = a( i, j )*mul
                 end do
              end do
           else if( itype==6_ilp64 ) then
              ! band matrix
              k1 = kl + ku + 2_ilp64
              k2 = kl + 1_ilp64
              k3 = 2_ilp64*kl + ku + 1_ilp64
              k4 = kl + ku + 1_ilp64 + m
              do j = 1, n
                 do i = max( k1-j, k2 ), min( k3, k4-j )
                    a( i, j ) = a( i, j )*mul
                 end do
              end do
           end if
           if( .not.done )go to 10
           return
     end subroutine stdlib_I64_dlascl


     pure module subroutine stdlib_I64_clascl( type, kl, ku, cfrom, cto, m, n, a, lda, info )
     !! CLASCL multiplies the M by N complex matrix A by the real scalar
     !! CTO/CFROM.  This is done without over/underflow as long as the final
     !! result CTO*A(I,J)/CFROM does not over/underflow. TYPE specifies that
     !! A may be full, upper triangular, lower triangular, upper Hessenberg,
     !! or banded.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: zero, half, one
           ! Scalar Arguments 
           character, intent(in) :: type
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: kl, ku, lda, m, n
           real(sp), intent(in) :: cfrom, cto
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: done
           integer(ilp64) :: i, itype, j, k1, k2, k3, k4
           real(sp) :: bignum, cfrom1, cfromc, cto1, ctoc, mul, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp64
           if( stdlib_lsame( type, 'G' ) ) then
              itype = 0_ilp64
           else if( stdlib_lsame( type, 'L' ) ) then
              itype = 1_ilp64
           else if( stdlib_lsame( type, 'U' ) ) then
              itype = 2_ilp64
           else if( stdlib_lsame( type, 'H' ) ) then
              itype = 3_ilp64
           else if( stdlib_lsame( type, 'B' ) ) then
              itype = 4_ilp64
           else if( stdlib_lsame( type, 'Q' ) ) then
              itype = 5_ilp64
           else if( stdlib_lsame( type, 'Z' ) ) then
              itype = 6_ilp64
           else
              itype = -1_ilp64
           end if
           if( itype==-1_ilp64 ) then
              info = -1_ilp64
           else if( cfrom==zero .or. stdlib_I64_sisnan(cfrom) ) then
              info = -4_ilp64
           else if( stdlib_I64_sisnan(cto) ) then
              info = -5_ilp64
           else if( m<0_ilp64 ) then
              info = -6_ilp64
           else if( n<0_ilp64 .or. ( itype==4_ilp64 .and. n/=m ) .or.( itype==5_ilp64 .and. n/=m ) ) then
              info = -7_ilp64
           else if( itype<=3_ilp64 .and. lda<max( 1_ilp64, m ) ) then
              info = -9_ilp64
           else if( itype>=4_ilp64 ) then
              if( kl<0_ilp64 .or. kl>max( m-1, 0_ilp64 ) ) then
                 info = -2_ilp64
              else if( ku<0_ilp64 .or. ku>max( n-1, 0_ilp64 ) .or.( ( itype==4_ilp64 .or. itype==5_ilp64 ) .and. kl/=ku ) &
                        )then
                 info = -3_ilp64
              else if( ( itype==4_ilp64 .and. lda<kl+1 ) .or.( itype==5_ilp64 .and. lda<ku+1 ) .or.( itype==6_ilp64 &
                        .and. lda<2_ilp64*kl+ku+1 ) ) then
                 info = -9_ilp64
              end if
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'CLASCL', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. m==0 )return
           ! get machine parameters
           smlnum = stdlib_I64_slamch( 'S' )
           bignum = one / smlnum
           cfromc = cfrom
           ctoc = cto
           10 continue
           cfrom1 = cfromc*smlnum
           if( cfrom1==cfromc ) then
              ! cfromc is an inf.  multiply by a correctly signed zero for
              ! finite ctoc, or a nan if ctoc is infinite.
              mul = ctoc / cfromc
              done = .true.
              cto1 = ctoc
           else
              cto1 = ctoc / bignum
              if( cto1==ctoc ) then
                 ! ctoc is either 0 or an inf.  in both cases, ctoc itself
                 ! serves as the correct multiplication factor.
                 mul = ctoc
                 done = .true.
                 cfromc = one
              else if( abs( cfrom1 )>abs( ctoc ) .and. ctoc/=zero ) then
                 mul = smlnum
                 done = .false.
                 cfromc = cfrom1
              else if( abs( cto1 )>abs( cfromc ) ) then
                 mul = bignum
                 done = .false.
                 ctoc = cto1
              else
                 mul = ctoc / cfromc
                 done = .true.
              end if
           end if
           if( itype==0_ilp64 ) then
              ! full matrix
              do j = 1, n
                 do i = 1, m
                    a( i, j ) = a( i, j )*mul
                 end do
              end do
           else if( itype==1_ilp64 ) then
              ! lower triangular matrix
              do j = 1, n
                 do i = j, m
                    a( i, j ) = a( i, j )*mul
                 end do
              end do
           else if( itype==2_ilp64 ) then
              ! upper triangular matrix
              do j = 1, n
                 do i = 1, min( j, m )
                    a( i, j ) = a( i, j )*mul
                 end do
              end do
           else if( itype==3_ilp64 ) then
              ! upper hessenberg matrix
              do j = 1, n
                 do i = 1, min( j+1, m )
                    a( i, j ) = a( i, j )*mul
                 end do
              end do
           else if( itype==4_ilp64 ) then
              ! lower chalf of a symmetric band matrix
              k3 = kl + 1_ilp64
              k4 = n + 1_ilp64
              do j = 1, n
                 do i = 1, min( k3, k4-j )
                    a( i, j ) = a( i, j )*mul
                 end do
              end do
           else if( itype==5_ilp64 ) then
              ! upper chalf of a symmetric band matrix
              k1 = ku + 2_ilp64
              k3 = ku + 1_ilp64
              do j = 1, n
                 do i = max( k1-j, 1 ), k3
                    a( i, j ) = a( i, j )*mul
                 end do
              end do
           else if( itype==6_ilp64 ) then
              ! band matrix
              k1 = kl + ku + 2_ilp64
              k2 = kl + 1_ilp64
              k3 = 2_ilp64*kl + ku + 1_ilp64
              k4 = kl + ku + 1_ilp64 + m
              do j = 1, n
                 do i = max( k1-j, k2 ), min( k3, k4-j )
                    a( i, j ) = a( i, j )*mul
                 end do
              end do
           end if
           if( .not.done )go to 10
           return
     end subroutine stdlib_I64_clascl

     pure module subroutine stdlib_I64_zlascl( type, kl, ku, cfrom, cto, m, n, a, lda, info )
     !! ZLASCL multiplies the M by N complex matrix A by the real scalar
     !! CTO/CFROM.  This is done without over/underflow as long as the final
     !! result CTO*A(I,J)/CFROM does not over/underflow. TYPE specifies that
     !! A may be full, upper triangular, lower triangular, upper Hessenberg,
     !! or banded.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: zero, half, one
           ! Scalar Arguments 
           character, intent(in) :: type
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: kl, ku, lda, m, n
           real(dp), intent(in) :: cfrom, cto
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: done
           integer(ilp64) :: i, itype, j, k1, k2, k3, k4
           real(dp) :: bignum, cfrom1, cfromc, cto1, ctoc, mul, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp64
           if( stdlib_lsame( type, 'G' ) ) then
              itype = 0_ilp64
           else if( stdlib_lsame( type, 'L' ) ) then
              itype = 1_ilp64
           else if( stdlib_lsame( type, 'U' ) ) then
              itype = 2_ilp64
           else if( stdlib_lsame( type, 'H' ) ) then
              itype = 3_ilp64
           else if( stdlib_lsame( type, 'B' ) ) then
              itype = 4_ilp64
           else if( stdlib_lsame( type, 'Q' ) ) then
              itype = 5_ilp64
           else if( stdlib_lsame( type, 'Z' ) ) then
              itype = 6_ilp64
           else
              itype = -1_ilp64
           end if
           if( itype==-1_ilp64 ) then
              info = -1_ilp64
           else if( cfrom==zero .or. stdlib_I64_disnan(cfrom) ) then
              info = -4_ilp64
           else if( stdlib_I64_disnan(cto) ) then
              info = -5_ilp64
           else if( m<0_ilp64 ) then
              info = -6_ilp64
           else if( n<0_ilp64 .or. ( itype==4_ilp64 .and. n/=m ) .or.( itype==5_ilp64 .and. n/=m ) ) then
              info = -7_ilp64
           else if( itype<=3_ilp64 .and. lda<max( 1_ilp64, m ) ) then
              info = -9_ilp64
           else if( itype>=4_ilp64 ) then
              if( kl<0_ilp64 .or. kl>max( m-1, 0_ilp64 ) ) then
                 info = -2_ilp64
              else if( ku<0_ilp64 .or. ku>max( n-1, 0_ilp64 ) .or.( ( itype==4_ilp64 .or. itype==5_ilp64 ) .and. kl/=ku ) &
                        )then
                 info = -3_ilp64
              else if( ( itype==4_ilp64 .and. lda<kl+1 ) .or.( itype==5_ilp64 .and. lda<ku+1 ) .or.( itype==6_ilp64 &
                        .and. lda<2_ilp64*kl+ku+1 ) ) then
                 info = -9_ilp64
              end if
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'ZLASCL', -info )
              return
           end if
           ! quick return if possible
           if( n==0 .or. m==0 )return
           ! get machine parameters
           smlnum = stdlib_I64_dlamch( 'S' )
           bignum = one / smlnum
           cfromc = cfrom
           ctoc = cto
           10 continue
           cfrom1 = cfromc*smlnum
           if( cfrom1==cfromc ) then
              ! cfromc is an inf.  multiply by a correctly signed zero for
              ! finite ctoc, or a nan if ctoc is infinite.
              mul = ctoc / cfromc
              done = .true.
              cto1 = ctoc
           else
              cto1 = ctoc / bignum
              if( cto1==ctoc ) then
                 ! ctoc is either 0 or an inf.  in both cases, ctoc itself
                 ! serves as the correct multiplication factor.
                 mul = ctoc
                 done = .true.
                 cfromc = one
              else if( abs( cfrom1 )>abs( ctoc ) .and. ctoc/=zero ) then
                 mul = smlnum
                 done = .false.
                 cfromc = cfrom1
              else if( abs( cto1 )>abs( cfromc ) ) then
                 mul = bignum
                 done = .false.
                 ctoc = cto1
              else
                 mul = ctoc / cfromc
                 done = .true.
              end if
           end if
           if( itype==0_ilp64 ) then
              ! full matrix
              do j = 1, n
                 do i = 1, m
                    a( i, j ) = a( i, j )*mul
                 end do
              end do
           else if( itype==1_ilp64 ) then
              ! lower triangular matrix
              do j = 1, n
                 do i = j, m
                    a( i, j ) = a( i, j )*mul
                 end do
              end do
           else if( itype==2_ilp64 ) then
              ! upper triangular matrix
              do j = 1, n
                 do i = 1, min( j, m )
                    a( i, j ) = a( i, j )*mul
                 end do
              end do
           else if( itype==3_ilp64 ) then
              ! upper hessenberg matrix
              do j = 1, n
                 do i = 1, min( j+1, m )
                    a( i, j ) = a( i, j )*mul
                 end do
              end do
           else if( itype==4_ilp64 ) then
              ! lower chalf of a symmetric band matrix
              k3 = kl + 1_ilp64
              k4 = n + 1_ilp64
              do j = 1, n
                 do i = 1, min( k3, k4-j )
                    a( i, j ) = a( i, j )*mul
                 end do
              end do
           else if( itype==5_ilp64 ) then
              ! upper chalf of a symmetric band matrix
              k1 = ku + 2_ilp64
              k3 = ku + 1_ilp64
              do j = 1, n
                 do i = max( k1-j, 1 ), k3
                    a( i, j ) = a( i, j )*mul
                 end do
              end do
           else if( itype==6_ilp64 ) then
              ! band matrix
              k1 = kl + ku + 2_ilp64
              k2 = kl + 1_ilp64
              k3 = 2_ilp64*kl + ku + 1_ilp64
              k4 = kl + ku + 1_ilp64 + m
              do j = 1, n
                 do i = max( k1-j, k2 ), min( k3, k4-j )
                    a( i, j ) = a( i, j )*mul
                 end do
              end do
           end if
           if( .not.done )go to 10
           return
     end subroutine stdlib_I64_zlascl




     module subroutine stdlib_I64_sla_geamv( trans, m, n, alpha, a, lda, x, incx, beta,y, incy )
     !! SLA_GEAMV performs one of the matrix-vector operations
     !! y := alpha*abs(A)*abs(x) + beta*abs(y),
     !! or   y := alpha*abs(A)**T*abs(x) + beta*abs(y),
     !! where alpha and beta are scalars, x and y are vectors and A is an
     !! m by n matrix.
     !! This function is primarily used in calculating error bounds.
     !! To protect against underflow during evaluation, components in
     !! the resulting vector are perturbed away from zero by (N+1)
     !! times the underflow threshold.  To prevent unnecessarily large
     !! errors for block-structure embedded in general matrices,
     !! "symbolically" zero components are not perturbed.  A zero
     !! entry is considered "symbolic" if all multiplications involved
     !! in computing that entry have at least one zero multiplicand.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           real(sp), intent(in) :: alpha, beta
           integer(ilp64), intent(in) :: incx, incy, lda, m, n, trans
           ! Array Arguments 
           real(sp), intent(in) :: a(lda,*), x(*)
           real(sp), intent(inout) :: y(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: symb_zero
           real(sp) :: temp, safe1
           integer(ilp64) :: i, info, iy, j, jx, kx, ky, lenx, leny
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           if     ( .not.( ( trans==stdlib_I64_ilatrans( 'N' ) ).or. ( trans==stdlib_I64_ilatrans( 'T' ) )&
                     .or. ( trans==stdlib_I64_ilatrans( 'C' )) ) ) then
              info = 1_ilp64
           else if( m<0_ilp64 )then
              info = 2_ilp64
           else if( n<0_ilp64 )then
              info = 3_ilp64
           else if( lda<max( 1_ilp64, m ) )then
              info = 6_ilp64
           else if( incx==0_ilp64 )then
              info = 8_ilp64
           else if( incy==0_ilp64 )then
              info = 11_ilp64
           end if
           if( info/=0_ilp64 )then
              call stdlib_I64_xerbla( 'SLA_GEAMV ', info )
              return
           end if
           ! quick return if possible.
           if( ( m==0 ).or.( n==0 ).or.( ( alpha==zero ).and.( beta==one ) ) )return
           ! set  lenx  and  leny, the lengths of the vectors x and y, and set
           ! up the start points in  x  and  y.
           if( trans==stdlib_I64_ilatrans( 'N' ) )then
              lenx = n
              leny = m
           else
              lenx = m
              leny = n
           end if
           if( incx>0_ilp64 )then
              kx = 1_ilp64
           else
              kx = 1_ilp64 - ( lenx - 1_ilp64 )*incx
           end if
           if( incy>0_ilp64 )then
              ky = 1_ilp64
           else
              ky = 1_ilp64 - ( leny - 1_ilp64 )*incy
           end if
           ! set safe1 essentially to be the underflow threshold times the
           ! number of additions in each row.
           safe1 = stdlib_I64_slamch( 'SAFE MINIMUM' )
           safe1 = (n+1)*safe1
           ! form  y := alpha*abs(a)*abs(x) + beta*abs(y).
           ! the o(m*n) symb_zero tests could be replaced by o(n) queries to
           ! the inexact flag.  still doesn't help change the iteration order
           ! to per-column.
           iy = ky
           if ( incx==1_ilp64 ) then
              if( trans==stdlib_I64_ilatrans( 'N' ) )then
                 do i = 1, leny
                    if ( beta == zero ) then
                       symb_zero = .true.
                       y( iy ) = zero
                    else if ( y( iy ) == zero ) then
                       symb_zero = .true.
                    else
                       symb_zero = .false.
                       y( iy ) = beta * abs( y( iy ) )
                    end if
                    if ( alpha /= zero ) then
                       do j = 1, lenx
                          temp = abs( a( i, j ) )
                          symb_zero = symb_zero .and.( x( j ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*abs( x( j ) )*temp
                       end do
                    end if
                    if ( .not.symb_zero )y( iy ) = y( iy ) + sign( safe1, y( iy ) )
                    iy = iy + incy
                 end do
              else
                 do i = 1, leny
                    if ( beta == zero ) then
                       symb_zero = .true.
                       y( iy ) = zero
                    else if ( y( iy ) == zero ) then
                       symb_zero = .true.
                    else
                       symb_zero = .false.
                       y( iy ) = beta * abs( y( iy ) )
                    end if
                    if ( alpha /= zero ) then
                       do j = 1, lenx
                          temp = abs( a( j, i ) )
                          symb_zero = symb_zero .and.( x( j ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*abs( x( j ) )*temp
                       end do
                    end if
                    if ( .not.symb_zero )y( iy ) = y( iy ) + sign( safe1, y( iy ) )
                    iy = iy + incy
                 end do
              end if
           else
              if( trans==stdlib_I64_ilatrans( 'N' ) )then
                 do i = 1, leny
                    if ( beta == zero ) then
                       symb_zero = .true.
                       y( iy ) = zero
                    else if ( y( iy ) == zero ) then
                       symb_zero = .true.
                    else
                       symb_zero = .false.
                       y( iy ) = beta * abs( y( iy ) )
                    end if
                    if ( alpha /= zero ) then
                       jx = kx
                       do j = 1, lenx
                          temp = abs( a( i, j ) )
                          symb_zero = symb_zero .and.( x( jx ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*abs( x( jx ) )*temp
                          jx = jx + incx
                       end do
                    end if
                    if (.not.symb_zero)y( iy ) = y( iy ) + sign( safe1, y( iy ) )
                    iy = iy + incy
                 end do
              else
                 do i = 1, leny
                    if ( beta == zero ) then
                       symb_zero = .true.
                       y( iy ) = zero
                    else if ( y( iy ) == zero ) then
                       symb_zero = .true.
                    else
                       symb_zero = .false.
                       y( iy ) = beta * abs( y( iy ) )
                    end if
                    if ( alpha /= zero ) then
                       jx = kx
                       do j = 1, lenx
                          temp = abs( a( j, i ) )
                          symb_zero = symb_zero .and.( x( jx ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*abs( x( jx ) )*temp
                          jx = jx + incx
                       end do
                    end if
                    if (.not.symb_zero)y( iy ) = y( iy ) + sign( safe1, y( iy ) )
                    iy = iy + incy
                 end do
              end if
           end if
           return
     end subroutine stdlib_I64_sla_geamv

     module subroutine stdlib_I64_dla_geamv ( trans, m, n, alpha, a, lda, x, incx, beta,y, incy )
     !! DLA_GEAMV performs one of the matrix-vector operations
     !! y := alpha*abs(A)*abs(x) + beta*abs(y),
     !! or   y := alpha*abs(A)**T*abs(x) + beta*abs(y),
     !! where alpha and beta are scalars, x and y are vectors and A is an
     !! m by n matrix.
     !! This function is primarily used in calculating error bounds.
     !! To protect against underflow during evaluation, components in
     !! the resulting vector are perturbed away from zero by (N+1)
     !! times the underflow threshold.  To prevent unnecessarily large
     !! errors for block-structure embedded in general matrices,
     !! "symbolically" zero components are not perturbed.  A zero
     !! entry is considered "symbolic" if all multiplications involved
     !! in computing that entry have at least one zero multiplicand.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           real(dp), intent(in) :: alpha, beta
           integer(ilp64), intent(in) :: incx, incy, lda, m, n, trans
           ! Array Arguments 
           real(dp), intent(in) :: a(lda,*), x(*)
           real(dp), intent(inout) :: y(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: symb_zero
           real(dp) :: temp, safe1
           integer(ilp64) :: i, info, iy, j, jx, kx, ky, lenx, leny
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           if     ( .not.( ( trans==stdlib_I64_ilatrans( 'N' ) ).or. ( trans==stdlib_I64_ilatrans( 'T' ) )&
                     .or. ( trans==stdlib_I64_ilatrans( 'C' )) ) ) then
              info = 1_ilp64
           else if( m<0_ilp64 )then
              info = 2_ilp64
           else if( n<0_ilp64 )then
              info = 3_ilp64
           else if( lda<max( 1_ilp64, m ) )then
              info = 6_ilp64
           else if( incx==0_ilp64 )then
              info = 8_ilp64
           else if( incy==0_ilp64 )then
              info = 11_ilp64
           end if
           if( info/=0_ilp64 )then
              call stdlib_I64_xerbla( 'DLA_GEAMV ', info )
              return
           end if
           ! quick return if possible.
           if( ( m==0 ).or.( n==0 ).or.( ( alpha==zero ).and.( beta==one ) ) )return
           ! set  lenx  and  leny, the lengths of the vectors x and y, and set
           ! up the start points in  x  and  y.
           if( trans==stdlib_I64_ilatrans( 'N' ) )then
              lenx = n
              leny = m
           else
              lenx = m
              leny = n
           end if
           if( incx>0_ilp64 )then
              kx = 1_ilp64
           else
              kx = 1_ilp64 - ( lenx - 1_ilp64 )*incx
           end if
           if( incy>0_ilp64 )then
              ky = 1_ilp64
           else
              ky = 1_ilp64 - ( leny - 1_ilp64 )*incy
           end if
           ! set safe1 essentially to be the underflow threshold times the
           ! number of additions in each row.
           safe1 = stdlib_I64_dlamch( 'SAFE MINIMUM' )
           safe1 = (n+1)*safe1
           ! form  y := alpha*abs(a)*abs(x) + beta*abs(y).
           ! the o(m*n) symb_zero tests could be replaced by o(n) queries to
           ! the inexact flag.  still doesn't help change the iteration order
           ! to per-column.
           iy = ky
           if ( incx==1_ilp64 ) then
              if( trans==stdlib_I64_ilatrans( 'N' ) )then
                 do i = 1, leny
                    if ( beta == zero ) then
                       symb_zero = .true.
                       y( iy ) = zero
                    else if ( y( iy ) == zero ) then
                       symb_zero = .true.
                    else
                       symb_zero = .false.
                       y( iy ) = beta * abs( y( iy ) )
                    end if
                    if ( alpha /= zero ) then
                       do j = 1, lenx
                          temp = abs( a( i, j ) )
                          symb_zero = symb_zero .and.( x( j ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*abs( x( j ) )*temp
                       end do
                    end if
                    if ( .not.symb_zero )y( iy ) = y( iy ) + sign( safe1, y( iy ) )
                    iy = iy + incy
                 end do
              else
                 do i = 1, leny
                    if ( beta == zero ) then
                       symb_zero = .true.
                       y( iy ) = zero
                    else if ( y( iy ) == zero ) then
                       symb_zero = .true.
                    else
                       symb_zero = .false.
                       y( iy ) = beta * abs( y( iy ) )
                    end if
                    if ( alpha /= zero ) then
                       do j = 1, lenx
                          temp = abs( a( j, i ) )
                          symb_zero = symb_zero .and.( x( j ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*abs( x( j ) )*temp
                       end do
                    end if
                    if ( .not.symb_zero )y( iy ) = y( iy ) + sign( safe1, y( iy ) )
                    iy = iy + incy
                 end do
              end if
           else
              if( trans==stdlib_I64_ilatrans( 'N' ) )then
                 do i = 1, leny
                    if ( beta == zero ) then
                       symb_zero = .true.
                       y( iy ) = zero
                    else if ( y( iy ) == zero ) then
                       symb_zero = .true.
                    else
                       symb_zero = .false.
                       y( iy ) = beta * abs( y( iy ) )
                    end if
                    if ( alpha /= zero ) then
                       jx = kx
                       do j = 1, lenx
                          temp = abs( a( i, j ) )
                          symb_zero = symb_zero .and.( x( jx ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*abs( x( jx ) )*temp
                          jx = jx + incx
                       end do
                    end if
                    if (.not.symb_zero)y( iy ) = y( iy ) + sign( safe1, y( iy ) )
                    iy = iy + incy
                 end do
              else
                 do i = 1, leny
                    if ( beta == zero ) then
                       symb_zero = .true.
                       y( iy ) = zero
                    else if ( y( iy ) == zero ) then
                       symb_zero = .true.
                    else
                       symb_zero = .false.
                       y( iy ) = beta * abs( y( iy ) )
                    end if
                    if ( alpha /= zero ) then
                       jx = kx
                       do j = 1, lenx
                          temp = abs( a( j, i ) )
                          symb_zero = symb_zero .and.( x( jx ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*abs( x( jx ) )*temp
                          jx = jx + incx
                       end do
                    end if
                    if (.not.symb_zero)y( iy ) = y( iy ) + sign( safe1, y( iy ) )
                    iy = iy + incy
                 end do
              end if
           end if
           return
     end subroutine stdlib_I64_dla_geamv


     module subroutine stdlib_I64_cla_geamv( trans, m, n, alpha, a, lda, x, incx, beta,y, incy )
     !! CLA_GEAMV performs one of the matrix-vector operations
     !! y := alpha*abs(A)*abs(x) + beta*abs(y),
     !! or   y := alpha*abs(A)**T*abs(x) + beta*abs(y),
     !! where alpha and beta are scalars, x and y are vectors and A is an
     !! m by n matrix.
     !! This function is primarily used in calculating error bounds.
     !! To protect against underflow during evaluation, components in
     !! the resulting vector are perturbed away from zero by (N+1)
     !! times the underflow threshold.  To prevent unnecessarily large
     !! errors for block-structure embedded in general matrices,
     !! "symbolically" zero components are not perturbed.  A zero
     !! entry is considered "symbolic" if all multiplications involved
     !! in computing that entry have at least one zero multiplicand.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           real(sp), intent(in) :: alpha, beta
           integer(ilp64), intent(in) :: incx, incy, lda, m, n
           integer(ilp64), intent(in) :: trans
           ! Array Arguments 
           complex(sp), intent(in) :: a(lda,*), x(*)
           real(sp), intent(inout) :: y(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: symb_zero
           real(sp) :: temp, safe1
           integer(ilp64) :: i, info, iy, j, jx, kx, ky, lenx, leny
           complex(sp) :: cdum
           ! Intrinsic Functions 
           ! Statement Functions 
           real(sp) :: cabs1
           ! Statement Function Definitions 
           cabs1( cdum ) = abs( real( cdum,KIND=sp) ) + abs( aimag( cdum ) )
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           if     ( .not.( ( trans==stdlib_I64_ilatrans( 'N' ) ).or. ( trans==stdlib_I64_ilatrans( 'T' ) )&
                     .or. ( trans==stdlib_I64_ilatrans( 'C' ) ) ) ) then
              info = 1_ilp64
           else if( m<0_ilp64 )then
              info = 2_ilp64
           else if( n<0_ilp64 )then
              info = 3_ilp64
           else if( lda<max( 1_ilp64, m ) )then
              info = 6_ilp64
           else if( incx==0_ilp64 )then
              info = 8_ilp64
           else if( incy==0_ilp64 )then
              info = 11_ilp64
           end if
           if( info/=0_ilp64 )then
              call stdlib_I64_xerbla( 'CLA_GEAMV ', info )
              return
           end if
           ! quick return if possible.
           if( ( m==0 ).or.( n==0 ).or.( ( alpha==czero ).and.( beta==cone ) ) )return
           ! set  lenx  and  leny, the lengths of the vectors x and y, and set
           ! up the start points in  x  and  y.
           if( trans==stdlib_I64_ilatrans( 'N' ) )then
              lenx = n
              leny = m
           else
              lenx = m
              leny = n
           end if
           if( incx>0_ilp64 )then
              kx = 1_ilp64
           else
              kx = 1_ilp64 - ( lenx - 1_ilp64 )*incx
           end if
           if( incy>0_ilp64 )then
              ky = 1_ilp64
           else
              ky = 1_ilp64 - ( leny - 1_ilp64 )*incy
           end if
           ! set safe1 essentially to be the underflow threshold times the
           ! number of additions in each row.
           safe1 = stdlib_I64_slamch( 'SAFE MINIMUM' )
           safe1 = (n+1)*safe1
           ! form  y := alpha*abs(a)*abs(x) + beta*abs(y).
           ! the o(m*n) symb_zero tests could be replaced by o(n) queries to
           ! the inexact flag.  still doesn't help change the iteration order
           ! to per-column.
           iy = ky
           if ( incx==1_ilp64 ) then
              if( trans==stdlib_I64_ilatrans( 'N' ) )then
                 do i = 1, leny
                    if ( beta == 0.0_sp ) then
                       symb_zero = .true.
                       y( iy ) = czero
                    else if ( y( iy ) == 0.0_sp ) then
                       symb_zero = .true.
                    else
                       symb_zero = .false.
                       y( iy ) = beta * abs( y( iy ) )
                    end if
                    if ( alpha /= 0.0_sp ) then
                       do j = 1, lenx
                          temp = cabs1( a( i, j ) )
                          symb_zero = symb_zero .and.( x( j ) == czero .or. temp == czero )
                                    
                          y( iy ) = y( iy ) + alpha*cabs1( x( j ) )*temp
                       end do
                    end if
                    if ( .not.symb_zero ) y( iy ) =y( iy ) + sign( safe1, y( iy ) )
                    iy = iy + incy
                 end do
              else
                 do i = 1, leny
                    if ( beta == 0.0_sp ) then
                       symb_zero = .true.
                       y( iy ) = czero
                    else if ( y( iy ) == 0.0_sp ) then
                       symb_zero = .true.
                    else
                       symb_zero = .false.
                       y( iy ) = beta * abs( y( iy ) )
                    end if
                    if ( alpha /= 0.0_sp ) then
                       do j = 1, lenx
                          temp = cabs1( a( j, i ) )
                          symb_zero = symb_zero .and.( x( j ) == czero .or. temp == czero )
                                    
                          y( iy ) = y( iy ) + alpha*cabs1( x( j ) )*temp
                       end do
                    end if
                    if ( .not.symb_zero ) y( iy ) =y( iy ) + sign( safe1, y( iy ) )
                    iy = iy + incy
                 end do
              end if
           else
              if( trans==stdlib_I64_ilatrans( 'N' ) )then
                 do i = 1, leny
                    if ( beta == 0.0_sp ) then
                       symb_zero = .true.
                       y( iy ) = czero
                    else if ( y( iy ) == 0.0_sp ) then
                       symb_zero = .true.
                    else
                       symb_zero = .false.
                       y( iy ) = beta * abs( y( iy ) )
                    end if
                    if ( alpha /= 0.0_sp ) then
                       jx = kx
                       do j = 1, lenx
                          temp = cabs1( a( i, j ) )
                          symb_zero = symb_zero .and.( x( jx ) == czero .or. temp == czero )
                                    
                          y( iy ) = y( iy ) + alpha*cabs1( x( jx ) )*temp
                          jx = jx + incx
                       end do
                    end if
                    if ( .not.symb_zero ) y( iy ) =y( iy ) + sign( safe1, y( iy ) )
                    iy = iy + incy
                 end do
              else
                 do i = 1, leny
                    if ( beta == 0.0_sp ) then
                       symb_zero = .true.
                       y( iy ) = czero
                    else if ( y( iy ) == 0.0_sp ) then
                       symb_zero = .true.
                    else
                       symb_zero = .false.
                       y( iy ) = beta * abs( y( iy ) )
                    end if
                    if ( alpha /= 0.0_sp ) then
                       jx = kx
                       do j = 1, lenx
                          temp = cabs1( a( j, i ) )
                          symb_zero = symb_zero .and.( x( jx ) == czero .or. temp == czero )
                                    
                          y( iy ) = y( iy ) + alpha*cabs1( x( jx ) )*temp
                          jx = jx + incx
                       end do
                    end if
                    if ( .not.symb_zero ) y( iy ) =y( iy ) + sign( safe1, y( iy ) )
                    iy = iy + incy
                 end do
              end if
           end if
           return
     end subroutine stdlib_I64_cla_geamv

     module subroutine stdlib_I64_zla_geamv( trans, m, n, alpha, a, lda, x, incx, beta,y, incy )
     !! ZLA_GEAMV performs one of the matrix-vector operations
     !! y := alpha*abs(A)*abs(x) + beta*abs(y),
     !! or   y := alpha*abs(A)**T*abs(x) + beta*abs(y),
     !! where alpha and beta are scalars, x and y are vectors and A is an
     !! m by n matrix.
     !! This function is primarily used in calculating error bounds.
     !! To protect against underflow during evaluation, components in
     !! the resulting vector are perturbed away from zero by (N+1)
     !! times the underflow threshold.  To prevent unnecessarily large
     !! errors for block-structure embedded in general matrices,
     !! "symbolically" zero components are not perturbed.  A zero
     !! entry is considered "symbolic" if all multiplications involved
     !! in computing that entry have at least one zero multiplicand.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           real(dp), intent(in) :: alpha, beta
           integer(ilp64), intent(in) :: incx, incy, lda, m, n
           integer(ilp64), intent(in) :: trans
           ! Array Arguments 
           complex(dp), intent(in) :: a(lda,*), x(*)
           real(dp), intent(inout) :: y(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: symb_zero
           real(dp) :: temp, safe1
           integer(ilp64) :: i, info, iy, j, jx, kx, ky, lenx, leny
           complex(dp) :: cdum
           ! Intrinsic Functions 
           ! Statement Functions 
           real(dp) :: cabs1
           ! Statement Function Definitions 
           cabs1( cdum ) = abs( real( cdum,KIND=dp) ) + abs( aimag( cdum ) )
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           if     ( .not.( ( trans==stdlib_I64_ilatrans( 'N' ) ).or. ( trans==stdlib_I64_ilatrans( 'T' ) )&
                     .or. ( trans==stdlib_I64_ilatrans( 'C' ) ) ) ) then
              info = 1_ilp64
           else if( m<0_ilp64 )then
              info = 2_ilp64
           else if( n<0_ilp64 )then
              info = 3_ilp64
           else if( lda<max( 1_ilp64, m ) )then
              info = 6_ilp64
           else if( incx==0_ilp64 )then
              info = 8_ilp64
           else if( incy==0_ilp64 )then
              info = 11_ilp64
           end if
           if( info/=0_ilp64 )then
              call stdlib_I64_xerbla( 'ZLA_GEAMV ', info )
              return
           end if
           ! quick return if possible.
           if( ( m==0 ).or.( n==0 ).or.( ( alpha==czero ).and.( beta==cone ) ) )return
           ! set  lenx  and  leny, the lengths of the vectors x and y, and set
           ! up the start points in  x  and  y.
           if( trans==stdlib_I64_ilatrans( 'N' ) )then
              lenx = n
              leny = m
           else
              lenx = m
              leny = n
           end if
           if( incx>0_ilp64 )then
              kx = 1_ilp64
           else
              kx = 1_ilp64 - ( lenx - 1_ilp64 )*incx
           end if
           if( incy>0_ilp64 )then
              ky = 1_ilp64
           else
              ky = 1_ilp64 - ( leny - 1_ilp64 )*incy
           end if
           ! set safe1 essentially to be the underflow threshold times the
           ! number of additions in each row.
           safe1 = stdlib_I64_dlamch( 'SAFE MINIMUM' )
           safe1 = (n+1)*safe1
           ! form  y := alpha*abs(a)*abs(x) + beta*abs(y).
           ! the o(m*n) symb_zero tests could be replaced by o(n) queries to
           ! the inexact flag.  still doesn't help change the iteration order
           ! to per-column.
           iy = ky
           if ( incx==1_ilp64 ) then
              if( trans==stdlib_I64_ilatrans( 'N' ) )then
                 do i = 1, leny
                    if ( beta == czero ) then
                       symb_zero = .true.
                       y( iy ) = czero
                    else if ( y( iy ) == czero ) then
                       symb_zero = .true.
                    else
                       symb_zero = .false.
                       y( iy ) = beta * abs( y( iy ) )
                    end if
                    if ( alpha /= czero ) then
                       do j = 1, lenx
                          temp = cabs1( a( i, j ) )
                          symb_zero = symb_zero .and.( x( j ) == czero .or. temp == czero )
                                    
                          y( iy ) = y( iy ) + alpha*cabs1( x( j ) )*temp
                       end do
                    end if
                    if ( .not.symb_zero ) y( iy ) =y( iy ) + sign( safe1, y( iy ) )
                    iy = iy + incy
                 end do
              else
                 do i = 1, leny
                    if ( beta == czero ) then
                       symb_zero = .true.
                       y( iy ) = czero
                    else if ( y( iy ) == czero ) then
                       symb_zero = .true.
                    else
                       symb_zero = .false.
                       y( iy ) = beta * abs( y( iy ) )
                    end if
                    if ( alpha /= czero ) then
                       do j = 1, lenx
                          temp = cabs1( a( j, i ) )
                          symb_zero = symb_zero .and.( x( j ) == czero .or. temp == czero )
                                    
                          y( iy ) = y( iy ) + alpha*cabs1( x( j ) )*temp
                       end do
                    end if
                    if ( .not.symb_zero ) y( iy ) =y( iy ) + sign( safe1, y( iy ) )
                    iy = iy + incy
                 end do
              end if
           else
              if( trans==stdlib_I64_ilatrans( 'N' ) )then
                 do i = 1, leny
                    if ( beta == czero ) then
                       symb_zero = .true.
                       y( iy ) = czero
                    else if ( y( iy ) == czero ) then
                       symb_zero = .true.
                    else
                       symb_zero = .false.
                       y( iy ) = beta * abs( y( iy ) )
                    end if
                    if ( alpha /= czero ) then
                       jx = kx
                       do j = 1, lenx
                          temp = cabs1( a( i, j ) )
                          symb_zero = symb_zero .and.( x( jx ) == czero .or. temp == czero )
                                    
                          y( iy ) = y( iy ) + alpha*cabs1( x( jx ) )*temp
                          jx = jx + incx
                       end do
                    end if
                    if ( .not.symb_zero ) y( iy ) =y( iy ) + sign( safe1, y( iy ) )
                    iy = iy + incy
                 end do
              else
                 do i = 1, leny
                    if ( beta == czero ) then
                       symb_zero = .true.
                       y( iy ) = czero
                    else if ( y( iy ) == czero ) then
                       symb_zero = .true.
                    else
                       symb_zero = .false.
                       y( iy ) = beta * abs( y( iy ) )
                    end if
                    if ( alpha /= czero ) then
                       jx = kx
                       do j = 1, lenx
                          temp = cabs1( a( j, i ) )
                          symb_zero = symb_zero .and.( x( jx ) == czero .or. temp == czero )
                                    
                          y( iy ) = y( iy ) + alpha*cabs1( x( jx ) )*temp
                          jx = jx + incx
                       end do
                    end if
                    if ( .not.symb_zero ) y( iy ) =y( iy ) + sign( safe1, y( iy ) )
                    iy = iy + incy
                 end do
              end if
           end if
           return
     end subroutine stdlib_I64_zla_geamv




     module subroutine stdlib_I64_sla_gbamv( trans, m, n, kl, ku, alpha, ab, ldab, x,incx, beta, y, incy )
     !! SLA_GBAMV performs one of the matrix-vector operations
     !! y := alpha*abs(A)*abs(x) + beta*abs(y),
     !! or   y := alpha*abs(A)**T*abs(x) + beta*abs(y),
     !! where alpha and beta are scalars, x and y are vectors and A is an
     !! m by n matrix.
     !! This function is primarily used in calculating error bounds.
     !! To protect against underflow during evaluation, components in
     !! the resulting vector are perturbed away from zero by (N+1)
     !! times the underflow threshold.  To prevent unnecessarily large
     !! errors for block-structure embedded in general matrices,
     !! "symbolically" zero components are not perturbed.  A zero
     !! entry is considered "symbolic" if all multiplications involved
     !! in computing that entry have at least one zero multiplicand.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           real(sp), intent(in) :: alpha, beta
           integer(ilp64), intent(in) :: incx, incy, ldab, m, n, kl, ku, trans
           ! Array Arguments 
           real(sp), intent(in) :: ab(ldab,*), x(*)
           real(sp), intent(inout) :: y(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: symb_zero
           real(sp) :: temp, safe1
           integer(ilp64) :: i, info, iy, j, jx, kx, ky, lenx, leny, kd, ke
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           if     ( .not.( ( trans==stdlib_I64_ilatrans( 'N' ) ).or. ( trans==stdlib_I64_ilatrans( 'T' ) )&
                     .or. ( trans==stdlib_I64_ilatrans( 'C' ) ) ) ) then
              info = 1_ilp64
           else if( m<0_ilp64 )then
              info = 2_ilp64
           else if( n<0_ilp64 )then
              info = 3_ilp64
           else if( kl<0_ilp64 .or. kl>m-1 ) then
              info = 4_ilp64
           else if( ku<0_ilp64 .or. ku>n-1 ) then
              info = 5_ilp64
           else if( ldab<kl+ku+1 )then
              info = 6_ilp64
           else if( incx==0_ilp64 )then
              info = 8_ilp64
           else if( incy==0_ilp64 )then
              info = 11_ilp64
           end if
           if( info/=0_ilp64 )then
              call stdlib_I64_xerbla( 'SLA_GBAMV ', info )
              return
           end if
           ! quick return if possible.
           if( ( m==0 ).or.( n==0 ).or.( ( alpha==zero ).and.( beta==one ) ) )return
           ! set  lenx  and  leny, the lengths of the vectors x and y, and set
           ! up the start points in  x  and  y.
           if( trans==stdlib_I64_ilatrans( 'N' ) )then
              lenx = n
              leny = m
           else
              lenx = m
              leny = n
           end if
           if( incx>0_ilp64 )then
              kx = 1_ilp64
           else
              kx = 1_ilp64 - ( lenx - 1_ilp64 )*incx
           end if
           if( incy>0_ilp64 )then
              ky = 1_ilp64
           else
              ky = 1_ilp64 - ( leny - 1_ilp64 )*incy
           end if
           ! set safe1 essentially to be the underflow threshold times the
           ! number of additions in each row.
           safe1 = stdlib_I64_slamch( 'SAFE MINIMUM' )
           safe1 = (n+1)*safe1
           ! form  y := alpha*abs(a)*abs(x) + beta*abs(y).
           ! the o(m*n) symb_zero tests could be replaced by o(n) queries to
           ! the inexact flag.  still doesn't help change the iteration order
           ! to per-column.
           kd = ku + 1_ilp64
           ke = kl + 1_ilp64
           iy = ky
           if ( incx==1_ilp64 ) then
              if( trans==stdlib_I64_ilatrans( 'N' ) )then
                 do i = 1, leny
                    if ( beta == zero ) then
                       symb_zero = .true.
                       y( iy ) = zero
                    else if ( y( iy ) == zero ) then
                       symb_zero = .true.
                    else
                       symb_zero = .false.
                       y( iy ) = beta * abs( y( iy ) )
                    end if
                    if ( alpha /= zero ) then
                       do j = max( i-kl, 1 ), min( i+ku, lenx )
                          temp = abs( ab( kd+i-j, j ) )
                          symb_zero = symb_zero .and.( x( j ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*abs( x( j ) )*temp
                       end do
                    end if
                    if ( .not.symb_zero )y( iy ) = y( iy ) + sign( safe1, y( iy ) )
                    iy = iy + incy
                 end do
              else
                 do i = 1, leny
                    if ( beta == zero ) then
                       symb_zero = .true.
                       y( iy ) = zero
                    else if ( y( iy ) == zero ) then
                       symb_zero = .true.
                    else
                       symb_zero = .false.
                       y( iy ) = beta * abs( y( iy ) )
                    end if
                    if ( alpha /= zero ) then
                       do j = max( i-kl, 1 ), min( i+ku, lenx )
                          temp = abs( ab( ke-i+j, i ) )
                          symb_zero = symb_zero .and.( x( j ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*abs( x( j ) )*temp
                       end do
                    end if
                    if ( .not.symb_zero )y( iy ) = y( iy ) + sign( safe1, y( iy ) )
                    iy = iy + incy
                 end do
              end if
           else
              if( trans==stdlib_I64_ilatrans( 'N' ) )then
                 do i = 1, leny
                    if ( beta == zero ) then
                       symb_zero = .true.
                       y( iy ) = zero
                    else if ( y( iy ) == zero ) then
                       symb_zero = .true.
                    else
                       symb_zero = .false.
                       y( iy ) = beta * abs( y( iy ) )
                    end if
                    if ( alpha /= zero ) then
                       jx = kx
                       do j = max( i-kl, 1 ), min( i+ku, lenx )
                          temp = abs( ab( kd+i-j, j ) )
                          symb_zero = symb_zero .and.( x( jx ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*abs( x( jx ) )*temp
                          jx = jx + incx
                       end do
                    end if
                    if ( .not.symb_zero )y( iy ) = y( iy ) + sign( safe1, y( iy ) )
                    iy = iy + incy
                 end do
              else
                 do i = 1, leny
                    if ( beta == zero ) then
                       symb_zero = .true.
                       y( iy ) = zero
                    else if ( y( iy ) == zero ) then
                       symb_zero = .true.
                    else
                       symb_zero = .false.
                       y( iy ) = beta * abs( y( iy ) )
                    end if
                    if ( alpha /= zero ) then
                       jx = kx
                       do j = max( i-kl, 1 ), min( i+ku, lenx )
                          temp = abs( ab( ke-i+j, i ) )
                          symb_zero = symb_zero .and.( x( jx ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*abs( x( jx ) )*temp
                          jx = jx + incx
                       end do
                    end if
                    if ( .not.symb_zero )y( iy ) = y( iy ) + sign( safe1, y( iy ) )
                    iy = iy + incy
                 end do
              end if
           end if
           return
     end subroutine stdlib_I64_sla_gbamv

     module subroutine stdlib_I64_dla_gbamv( trans, m, n, kl, ku, alpha, ab, ldab, x,incx, beta, y, incy )
     !! DLA_GBAMV performs one of the matrix-vector operations
     !! y := alpha*abs(A)*abs(x) + beta*abs(y),
     !! or   y := alpha*abs(A)**T*abs(x) + beta*abs(y),
     !! where alpha and beta are scalars, x and y are vectors and A is an
     !! m by n matrix.
     !! This function is primarily used in calculating error bounds.
     !! To protect against underflow during evaluation, components in
     !! the resulting vector are perturbed away from zero by (N+1)
     !! times the underflow threshold.  To prevent unnecessarily large
     !! errors for block-structure embedded in general matrices,
     !! "symbolically" zero components are not perturbed.  A zero
     !! entry is considered "symbolic" if all multiplications involved
     !! in computing that entry have at least one zero multiplicand.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           real(dp), intent(in) :: alpha, beta
           integer(ilp64), intent(in) :: incx, incy, ldab, m, n, kl, ku, trans
           ! Array Arguments 
           real(dp), intent(in) :: ab(ldab,*), x(*)
           real(dp), intent(inout) :: y(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: symb_zero
           real(dp) :: temp, safe1
           integer(ilp64) :: i, info, iy, j, jx, kx, ky, lenx, leny, kd, ke
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           if     ( .not.( ( trans==stdlib_I64_ilatrans( 'N' ) ).or. ( trans==stdlib_I64_ilatrans( 'T' ) )&
                     .or. ( trans==stdlib_I64_ilatrans( 'C' ) ) ) ) then
              info = 1_ilp64
           else if( m<0_ilp64 )then
              info = 2_ilp64
           else if( n<0_ilp64 )then
              info = 3_ilp64
           else if( kl<0_ilp64 .or. kl>m-1 ) then
              info = 4_ilp64
           else if( ku<0_ilp64 .or. ku>n-1 ) then
              info = 5_ilp64
           else if( ldab<kl+ku+1 )then
              info = 6_ilp64
           else if( incx==0_ilp64 )then
              info = 8_ilp64
           else if( incy==0_ilp64 )then
              info = 11_ilp64
           end if
           if( info/=0_ilp64 )then
              call stdlib_I64_xerbla( 'DLA_GBAMV ', info )
              return
           end if
           ! quick return if possible.
           if( ( m==0 ).or.( n==0 ).or.( ( alpha==zero ).and.( beta==one ) ) )return
           ! set  lenx  and  leny, the lengths of the vectors x and y, and set
           ! up the start points in  x  and  y.
           if( trans==stdlib_I64_ilatrans( 'N' ) )then
              lenx = n
              leny = m
           else
              lenx = m
              leny = n
           end if
           if( incx>0_ilp64 )then
              kx = 1_ilp64
           else
              kx = 1_ilp64 - ( lenx - 1_ilp64 )*incx
           end if
           if( incy>0_ilp64 )then
              ky = 1_ilp64
           else
              ky = 1_ilp64 - ( leny - 1_ilp64 )*incy
           end if
           ! set safe1 essentially to be the underflow threshold times the
           ! number of additions in each row.
           safe1 = stdlib_I64_dlamch( 'SAFE MINIMUM' )
           safe1 = (n+1)*safe1
           ! form  y := alpha*abs(a)*abs(x) + beta*abs(y).
           ! the o(m*n) symb_zero tests could be replaced by o(n) queries to
           ! the inexact flag.  still doesn't help change the iteration order
           ! to per-column.
           kd = ku + 1_ilp64
           ke = kl + 1_ilp64
           iy = ky
           if ( incx==1_ilp64 ) then
              if( trans==stdlib_I64_ilatrans( 'N' ) )then
                 do i = 1, leny
                    if ( beta == zero ) then
                       symb_zero = .true.
                       y( iy ) = zero
                    else if ( y( iy ) == zero ) then
                       symb_zero = .true.
                    else
                       symb_zero = .false.
                       y( iy ) = beta * abs( y( iy ) )
                    end if
                    if ( alpha /= zero ) then
                       do j = max( i-kl, 1 ), min( i+ku, lenx )
                          temp = abs( ab( kd+i-j, j ) )
                          symb_zero = symb_zero .and.( x( j ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*abs( x( j ) )*temp
                       end do
                    end if
                    if ( .not.symb_zero )y( iy ) = y( iy ) + sign( safe1, y( iy ) )
                    iy = iy + incy
                 end do
              else
                 do i = 1, leny
                    if ( beta == zero ) then
                       symb_zero = .true.
                       y( iy ) = zero
                    else if ( y( iy ) == zero ) then
                       symb_zero = .true.
                    else
                       symb_zero = .false.
                       y( iy ) = beta * abs( y( iy ) )
                    end if
                    if ( alpha /= zero ) then
                       do j = max( i-kl, 1 ), min( i+ku, lenx )
                          temp = abs( ab( ke-i+j, i ) )
                          symb_zero = symb_zero .and.( x( j ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*abs( x( j ) )*temp
                       end do
                    end if
                    if ( .not.symb_zero )y( iy ) = y( iy ) + sign( safe1, y( iy ) )
                    iy = iy + incy
                 end do
              end if
           else
              if( trans==stdlib_I64_ilatrans( 'N' ) )then
                 do i = 1, leny
                    if ( beta == zero ) then
                       symb_zero = .true.
                       y( iy ) = zero
                    else if ( y( iy ) == zero ) then
                       symb_zero = .true.
                    else
                       symb_zero = .false.
                       y( iy ) = beta * abs( y( iy ) )
                    end if
                    if ( alpha /= zero ) then
                       jx = kx
                       do j = max( i-kl, 1 ), min( i+ku, lenx )
                          temp = abs( ab( kd+i-j, j ) )
                          symb_zero = symb_zero .and.( x( jx ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*abs( x( jx ) )*temp
                          jx = jx + incx
                       end do
                    end if
                    if ( .not.symb_zero )y( iy ) = y( iy ) + sign( safe1, y( iy ) )
                    iy = iy + incy
                 end do
              else
                 do i = 1, leny
                    if ( beta == zero ) then
                       symb_zero = .true.
                       y( iy ) = zero
                    else if ( y( iy ) == zero ) then
                       symb_zero = .true.
                    else
                       symb_zero = .false.
                       y( iy ) = beta * abs( y( iy ) )
                    end if
                    if ( alpha /= zero ) then
                       jx = kx
                       do j = max( i-kl, 1 ), min( i+ku, lenx )
                          temp = abs( ab( ke-i+j, i ) )
                          symb_zero = symb_zero .and.( x( jx ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*abs( x( jx ) )*temp
                          jx = jx + incx
                       end do
                    end if
                    if ( .not.symb_zero )y( iy ) = y( iy ) + sign( safe1, y( iy ) )
                    iy = iy + incy
                 end do
              end if
           end if
           return
     end subroutine stdlib_I64_dla_gbamv


     module subroutine stdlib_I64_cla_gbamv( trans, m, n, kl, ku, alpha, ab, ldab, x,incx, beta, y, incy )
     !! CLA_GBAMV performs one of the matrix-vector operations
     !! y := alpha*abs(A)*abs(x) + beta*abs(y),
     !! or   y := alpha*abs(A)**T*abs(x) + beta*abs(y),
     !! where alpha and beta are scalars, x and y are vectors and A is an
     !! m by n matrix.
     !! This function is primarily used in calculating error bounds.
     !! To protect against underflow during evaluation, components in
     !! the resulting vector are perturbed away from zero by (N+1)
     !! times the underflow threshold.  To prevent unnecessarily large
     !! errors for block-structure embedded in general matrices,
     !! "symbolically" zero components are not perturbed.  A zero
     !! entry is considered "symbolic" if all multiplications involved
     !! in computing that entry have at least one zero multiplicand.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           real(sp), intent(in) :: alpha, beta
           integer(ilp64), intent(in) :: incx, incy, ldab, m, n, kl, ku, trans
           ! Array Arguments 
           complex(sp), intent(in) :: ab(ldab,*), x(*)
           real(sp), intent(inout) :: y(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: symb_zero
           real(sp) :: temp, safe1
           integer(ilp64) :: i, info, iy, j, jx, kx, ky, lenx, leny, kd, ke
           complex(sp) :: cdum
           ! Intrinsic Functions 
           ! Statement Functions
           real(sp) :: cabs1
           ! Statement Function Definitions 
           cabs1( cdum ) = abs( real( cdum,KIND=sp) ) + abs( aimag( cdum ) )
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           if     ( .not.( ( trans==stdlib_I64_ilatrans( 'N' ) ).or. ( trans==stdlib_I64_ilatrans( 'T' ) )&
                     .or. ( trans==stdlib_I64_ilatrans( 'C' ) ) ) ) then
              info = 1_ilp64
           else if( m<0_ilp64 )then
              info = 2_ilp64
           else if( n<0_ilp64 )then
              info = 3_ilp64
           else if( kl<0_ilp64 .or. kl>m-1 ) then
              info = 4_ilp64
           else if( ku<0_ilp64 .or. ku>n-1 ) then
              info = 5_ilp64
           else if( ldab<kl+ku+1 )then
              info = 6_ilp64
           else if( incx==0_ilp64 )then
              info = 8_ilp64
           else if( incy==0_ilp64 )then
              info = 11_ilp64
           end if
           if( info/=0_ilp64 )then
              call stdlib_I64_xerbla( 'CLA_GBAMV ', info )
              return
           end if
           ! quick return if possible.
           if( ( m==0 ).or.( n==0 ).or.( ( alpha==czero ).and.( beta==cone ) ) )return
           ! set  lenx  and  leny, the lengths of the vectors x and y, and set
           ! up the start points in  x  and  y.
           if( trans==stdlib_I64_ilatrans( 'N' ) )then
              lenx = n
              leny = m
           else
              lenx = m
              leny = n
           end if
           if( incx>0_ilp64 )then
              kx = 1_ilp64
           else
              kx = 1_ilp64 - ( lenx - 1_ilp64 )*incx
           end if
           if( incy>0_ilp64 )then
              ky = 1_ilp64
           else
              ky = 1_ilp64 - ( leny - 1_ilp64 )*incy
           end if
           ! set safe1 essentially to be the underflow threshold times the
           ! number of additions in each row.
           safe1 = stdlib_I64_slamch( 'SAFE MINIMUM' )
           safe1 = (n+1)*safe1
           ! form  y := alpha*abs(a)*abs(x) + beta*abs(y).
           ! the o(m*n) symb_zero tests could be replaced by o(n) queries to
           ! the inexact flag.  still doesn't help change the iteration order
           ! to per-column.
           kd = ku + 1_ilp64
           ke = kl + 1_ilp64
           iy = ky
           if ( incx==1_ilp64 ) then
              if( trans==stdlib_I64_ilatrans( 'N' ) )then
                 do i = 1, leny
                    if ( beta == 0.0_sp ) then
                       symb_zero = .true.
                       y( iy ) = czero
                    else if ( y( iy ) == 0.0_sp ) then
                       symb_zero = .true.
                    else
                       symb_zero = .false.
                       y( iy ) = beta * abs( y( iy ) )
                    end if
                    if ( alpha /= 0.0_sp ) then
                       do j = max( i-kl, 1 ), min( i+ku, lenx )
                          temp = cabs1( ab( kd+i-j, j ) )
                          symb_zero = symb_zero .and.( x( j ) == czero .or. temp == czero )
                                    
                          y( iy ) = y( iy ) + alpha*cabs1( x( j ) )*temp
                       end do
                    end if
                    if ( .not.symb_zero)y( iy ) = y( iy ) + sign( safe1, y( iy ) )
                    iy = iy + incy
                 end do
              else
                 do i = 1, leny
                    if ( beta == 0.0_sp ) then
                       symb_zero = .true.
                       y( iy ) = czero
                    else if ( y( iy ) == 0.0_sp ) then
                       symb_zero = .true.
                    else
                       symb_zero = .false.
                       y( iy ) = beta * abs( y( iy ) )
                    end if
                    if ( alpha /= 0.0_sp ) then
                       do j = max( i-kl, 1 ), min( i+ku, lenx )
                          temp = cabs1( ab( ke-i+j, i ) )
                          symb_zero = symb_zero .and.( x( j ) == czero .or. temp == czero )
                                    
                          y( iy ) = y( iy ) + alpha*cabs1( x( j ) )*temp
                       end do
                    end if
                    if ( .not.symb_zero)y( iy ) = y( iy ) + sign( safe1, y( iy ) )
                    iy = iy + incy
                 end do
              end if
           else
              if( trans==stdlib_I64_ilatrans( 'N' ) )then
                 do i = 1, leny
                    if ( beta == 0.0_sp ) then
                       symb_zero = .true.
                       y( iy ) = czero
                    else if ( y( iy ) == 0.0_sp ) then
                       symb_zero = .true.
                    else
                       symb_zero = .false.
                       y( iy ) = beta * abs( y( iy ) )
                    end if
                    if ( alpha /= 0.0_sp ) then
                       jx = kx
                       do j = max( i-kl, 1 ), min( i+ku, lenx )
                          temp = cabs1( ab( kd+i-j, j ) )
                          symb_zero = symb_zero .and.( x( jx ) == czero .or. temp == czero )
                                    
                          y( iy ) = y( iy ) + alpha*cabs1( x( jx ) )*temp
                          jx = jx + incx
                       end do
                    end if
                    if ( .not.symb_zero )y( iy ) = y( iy ) + sign( safe1, y( iy ) )
                    iy = iy + incy
                 end do
              else
                 do i = 1, leny
                    if ( beta == 0.0_sp ) then
                       symb_zero = .true.
                       y( iy ) = czero
                    else if ( y( iy ) == 0.0_sp ) then
                       symb_zero = .true.
                    else
                       symb_zero = .false.
                       y( iy ) = beta * abs( y( iy ) )
                    end if
                    if ( alpha /= 0.0_sp ) then
                       jx = kx
                       do j = max( i-kl, 1 ), min( i+ku, lenx )
                          temp = cabs1( ab( ke-i+j, i ) )
                          symb_zero = symb_zero .and.( x( jx ) == czero .or. temp == czero )
                                    
                          y( iy ) = y( iy ) + alpha*cabs1( x( jx ) )*temp
                          jx = jx + incx
                       end do
                    end if
                    if ( .not.symb_zero )y( iy ) = y( iy ) + sign( safe1, y( iy ) )
                    iy = iy + incy
                 end do
              end if
           end if
           return
     end subroutine stdlib_I64_cla_gbamv

     module subroutine stdlib_I64_zla_gbamv( trans, m, n, kl, ku, alpha, ab, ldab, x,incx, beta, y, incy )
     !! ZLA_GBAMV performs one of the matrix-vector operations
     !! y := alpha*abs(A)*abs(x) + beta*abs(y),
     !! or   y := alpha*abs(A)**T*abs(x) + beta*abs(y),
     !! where alpha and beta are scalars, x and y are vectors and A is an
     !! m by n matrix.
     !! This function is primarily used in calculating error bounds.
     !! To protect against underflow during evaluation, components in
     !! the resulting vector are perturbed away from zero by (N+1)
     !! times the underflow threshold.  To prevent unnecessarily large
     !! errors for block-structure embedded in general matrices,
     !! "symbolically" zero components are not perturbed.  A zero
     !! entry is considered "symbolic" if all multiplications involved
     !! in computing that entry have at least one zero multiplicand.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           real(dp), intent(in) :: alpha, beta
           integer(ilp64), intent(in) :: incx, incy, ldab, m, n, kl, ku, trans
           ! Array Arguments 
           complex(dp), intent(in) :: ab(ldab,*), x(*)
           real(dp), intent(inout) :: y(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: symb_zero
           real(dp) :: temp, safe1
           integer(ilp64) :: i, info, iy, j, jx, kx, ky, lenx, leny, kd, ke
           complex(dp) :: cdum
           ! Intrinsic Functions 
           ! Statement Functions
           real(dp) :: cabs1
           ! Statement Function Definitions 
           cabs1( cdum ) = abs( real( cdum,KIND=dp) ) + abs( aimag( cdum ) )
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           if     ( .not.( ( trans==stdlib_I64_ilatrans( 'N' ) ).or. ( trans==stdlib_I64_ilatrans( 'T' ) )&
                     .or. ( trans==stdlib_I64_ilatrans( 'C' ) ) ) ) then
              info = 1_ilp64
           else if( m<0_ilp64 )then
              info = 2_ilp64
           else if( n<0_ilp64 )then
              info = 3_ilp64
           else if( kl<0_ilp64 .or. kl>m-1 ) then
              info = 4_ilp64
           else if( ku<0_ilp64 .or. ku>n-1 ) then
              info = 5_ilp64
           else if( ldab<kl+ku+1 )then
              info = 6_ilp64
           else if( incx==0_ilp64 )then
              info = 8_ilp64
           else if( incy==0_ilp64 )then
              info = 11_ilp64
           end if
           if( info/=0_ilp64 )then
              call stdlib_I64_xerbla( 'ZLA_GBAMV ', info )
              return
           end if
           ! quick return if possible.
           if( ( m==0 ).or.( n==0 ).or.( ( alpha==czero ).and.( beta==cone ) ) )return
           ! set  lenx  and  leny, the lengths of the vectors x and y, and set
           ! up the start points in  x  and  y.
           if( trans==stdlib_I64_ilatrans( 'N' ) )then
              lenx = n
              leny = m
           else
              lenx = m
              leny = n
           end if
           if( incx>0_ilp64 )then
              kx = 1_ilp64
           else
              kx = 1_ilp64 - ( lenx - 1_ilp64 )*incx
           end if
           if( incy>0_ilp64 )then
              ky = 1_ilp64
           else
              ky = 1_ilp64 - ( leny - 1_ilp64 )*incy
           end if
           ! set safe1 essentially to be the underflow threshold times the
           ! number of additions in each row.
           safe1 = stdlib_I64_dlamch( 'SAFE MINIMUM' )
           safe1 = (n+1)*safe1
           ! form  y := alpha*abs(a)*abs(x) + beta*abs(y).
           ! the o(m*n) symb_zero tests could be replaced by o(n) queries to
           ! the inexact flag.  still doesn't help change the iteration order
           ! to per-column.
           kd = ku + 1_ilp64
           ke = kl + 1_ilp64
           iy = ky
           if ( incx==1_ilp64 ) then
              if( trans==stdlib_I64_ilatrans( 'N' ) )then
                 do i = 1, leny
                    if ( beta == czero ) then
                       symb_zero = .true.
                       y( iy ) = czero
                    else if ( y( iy ) == czero ) then
                       symb_zero = .true.
                    else
                       symb_zero = .false.
                       y( iy ) = beta * abs( y( iy ) )
                    end if
                    if ( alpha /= czero ) then
                       do j = max( i-kl, 1 ), min( i+ku, lenx )
                          temp = cabs1( ab( kd+i-j, j ) )
                          symb_zero = symb_zero .and.( x( j ) == czero .or. temp == czero )
                                    
                          y( iy ) = y( iy ) + alpha*cabs1( x( j ) )*temp
                       end do
                    end if
                    if ( .not.symb_zero)y( iy ) = y( iy ) + sign( safe1, y( iy ) )
                    iy = iy + incy
                 end do
              else
                 do i = 1, leny
                    if ( beta == czero ) then
                       symb_zero = .true.
                       y( iy ) = czero
                    else if ( y( iy ) == czero ) then
                       symb_zero = .true.
                    else
                       symb_zero = .false.
                       y( iy ) = beta * abs( y( iy ) )
                    end if
                    if ( alpha /= czero ) then
                       do j = max( i-kl, 1 ), min( i+ku, lenx )
                          temp = cabs1( ab( ke-i+j, i ) )
                          symb_zero = symb_zero .and.( x( j ) == czero .or. temp == czero )
                                    
                          y( iy ) = y( iy ) + alpha*cabs1( x( j ) )*temp
                       end do
                    end if
                    if ( .not.symb_zero)y( iy ) = y( iy ) + sign( safe1, y( iy ) )
                    iy = iy + incy
                 end do
              end if
           else
              if( trans==stdlib_I64_ilatrans( 'N' ) )then
                 do i = 1, leny
                    if ( beta == czero ) then
                       symb_zero = .true.
                       y( iy ) = czero
                    else if ( y( iy ) == czero ) then
                       symb_zero = .true.
                    else
                       symb_zero = .false.
                       y( iy ) = beta * abs( y( iy ) )
                    end if
                    if ( alpha /= czero ) then
                       jx = kx
                       do j = max( i-kl, 1 ), min( i+ku, lenx )
                          temp = cabs1( ab( kd+i-j, j ) )
                          symb_zero = symb_zero .and.( x( jx ) == czero .or. temp == czero )
                                    
                          y( iy ) = y( iy ) + alpha*cabs1( x( jx ) )*temp
                          jx = jx + incx
                       end do
                    end if
                    if ( .not.symb_zero )y( iy ) = y( iy ) + sign( safe1, y( iy ) )
                    iy = iy + incy
                 end do
              else
                 do i = 1, leny
                    if ( beta == czero ) then
                       symb_zero = .true.
                       y( iy ) = czero
                    else if ( y( iy ) == czero ) then
                       symb_zero = .true.
                    else
                       symb_zero = .false.
                       y( iy ) = beta * abs( y( iy ) )
                    end if
                    if ( alpha /= czero ) then
                       jx = kx
                       do j = max( i-kl, 1 ), min( i+ku, lenx )
                          temp = cabs1( ab( ke-i+j, i ) )
                          symb_zero = symb_zero .and.( x( jx ) == czero .or. temp == czero )
                                    
                          y( iy ) = y( iy ) + alpha*cabs1( x( jx ) )*temp
                          jx = jx + incx
                       end do
                    end if
                    if ( .not.symb_zero )y( iy ) = y( iy ) + sign( safe1, y( iy ) )
                    iy = iy + incy
                 end do
              end if
           end if
           return
     end subroutine stdlib_I64_zla_gbamv




     module subroutine stdlib_I64_cla_heamv( uplo, n, alpha, a, lda, x, incx, beta, y,incy )
     !! CLA_SYAMV  performs the matrix-vector operation
     !! y := alpha*abs(A)*abs(x) + beta*abs(y),
     !! where alpha and beta are scalars, x and y are vectors and A is an
     !! n by n symmetric matrix.
     !! This function is primarily used in calculating error bounds.
     !! To protect against underflow during evaluation, components in
     !! the resulting vector are perturbed away from zero by (N+1)
     !! times the underflow threshold.  To prevent unnecessarily large
     !! errors for block-structure embedded in general matrices,
     !! "symbolically" zero components are not perturbed.  A zero
     !! entry is considered "symbolic" if all multiplications involved
     !! in computing that entry have at least one zero multiplicand.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           real(sp), intent(in) :: alpha, beta
           integer(ilp64), intent(in) :: incx, incy, lda, n, uplo
           ! Array Arguments 
           complex(sp), intent(in) :: a(lda,*), x(*)
           real(sp), intent(inout) :: y(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: symb_zero
           real(sp) :: temp, safe1
           integer(ilp64) :: i, info, iy, j, jx, kx, ky
           complex(sp) :: zdum
           ! Intrinsic Functions 
           ! Statement Functions 
           real(sp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=sp) ) + abs( aimag ( zdum ) )
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           if     ( uplo/=stdlib_I64_ilauplo( 'U' ) .and.uplo/=stdlib_I64_ilauplo( 'L' ) )then
              info = 1_ilp64
           else if( n<0_ilp64 )then
              info = 2_ilp64
           else if( lda<max( 1_ilp64, n ) )then
              info = 5_ilp64
           else if( incx==0_ilp64 )then
              info = 7_ilp64
           else if( incy==0_ilp64 )then
              info = 10_ilp64
           end if
           if( info/=0_ilp64 )then
              call stdlib_I64_xerbla( 'CHEMV ', info )
              return
           end if
           ! quick return if possible.
           if( ( n==0 ).or.( ( alpha==zero ).and.( beta==one ) ) )return
           ! set up the start points in  x  and  y.
           if( incx>0_ilp64 )then
              kx = 1_ilp64
           else
              kx = 1_ilp64 - ( n - 1_ilp64 )*incx
           end if
           if( incy>0_ilp64 )then
              ky = 1_ilp64
           else
              ky = 1_ilp64 - ( n - 1_ilp64 )*incy
           end if
           ! set safe1 essentially to be the underflow threshold times the
           ! number of additions in each row.
           safe1 = stdlib_I64_slamch( 'SAFE MINIMUM' )
           safe1 = (n+1)*safe1
           ! form  y := alpha*abs(a)*abs(x) + beta*abs(y).
           ! the o(n^2) symb_zero tests could be replaced by o(n) queries to
           ! the inexact flag.  still doesn't help change the iteration order
           ! to per-column.
           iy = ky
           if ( incx==1_ilp64 ) then
              if ( uplo == stdlib_I64_ilauplo( 'U' ) ) then
                 do i = 1, n
                    if ( beta == zero ) then
                       symb_zero = .true.
                       y( iy ) = zero
                    else if ( y( iy ) == zero ) then
                       symb_zero = .true.
                    else
                       symb_zero = .false.
                       y( iy ) = beta * abs( y( iy ) )
                    end if
                    if ( alpha /= zero ) then
                       do j = 1, i
                          temp = cabs1( a( j, i ) )
                          symb_zero = symb_zero .and.( x( j ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*cabs1( x( j ) )*temp
                       end do
                       do j = i+1, n
                          temp = cabs1( a( i, j ) )
                          symb_zero = symb_zero .and.( x( j ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*cabs1( x( j ) )*temp
                       end do
                    end if
                    if (.not.symb_zero)y( iy ) = y( iy ) + sign( safe1, y( iy ) )
                    iy = iy + incy
                 end do
              else
                 do i = 1, n
                    if ( beta == zero ) then
                       symb_zero = .true.
                       y( iy ) = zero
                    else if ( y( iy ) == zero ) then
                       symb_zero = .true.
                    else
                       symb_zero = .false.
                       y( iy ) = beta * abs( y( iy ) )
                    end if
                    if ( alpha /= zero ) then
                       do j = 1, i
                          temp = cabs1( a( i, j ) )
                          symb_zero = symb_zero .and.( x( j ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*cabs1( x( j ) )*temp
                       end do
                       do j = i+1, n
                          temp = cabs1( a( j, i ) )
                          symb_zero = symb_zero .and.( x( j ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*cabs1( x( j ) )*temp
                       end do
                    end if
                    if (.not.symb_zero)y( iy ) = y( iy ) + sign( safe1, y( iy ) )
                    iy = iy + incy
                 end do
              end if
           else
              if ( uplo == stdlib_I64_ilauplo( 'U' ) ) then
                 do i = 1, n
                    if ( beta == zero ) then
                       symb_zero = .true.
                       y( iy ) = zero
                    else if ( y( iy ) == zero ) then
                       symb_zero = .true.
                    else
                       symb_zero = .false.
                       y( iy ) = beta * abs( y( iy ) )
                    end if
                    jx = kx
                    if ( alpha /= zero ) then
                       do j = 1, i
                          temp = cabs1( a( j, i ) )
                          symb_zero = symb_zero .and.( x( j ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*cabs1( x( jx ) )*temp
                          jx = jx + incx
                       end do
                       do j = i+1, n
                          temp = cabs1( a( i, j ) )
                          symb_zero = symb_zero .and.( x( j ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*cabs1( x( jx ) )*temp
                          jx = jx + incx
                       end do
                    end if
                    if ( .not.symb_zero )y( iy ) = y( iy ) + sign( safe1, y( iy ) )
                    iy = iy + incy
                 end do
              else
                 do i = 1, n
                    if ( beta == zero ) then
                       symb_zero = .true.
                       y( iy ) = zero
                    else if ( y( iy ) == zero ) then
                       symb_zero = .true.
                    else
                       symb_zero = .false.
                       y( iy ) = beta * abs( y( iy ) )
                    end if
                    jx = kx
                    if ( alpha /= zero ) then
                       do j = 1, i
                          temp = cabs1( a( i, j ) )
                          symb_zero = symb_zero .and.( x( j ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*cabs1( x( jx ) )*temp
                          jx = jx + incx
                       end do
                       do j = i+1, n
                          temp = cabs1( a( j, i ) )
                          symb_zero = symb_zero .and.( x( j ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*cabs1( x( jx ) )*temp
                          jx = jx + incx
                       end do
                    end if
                    if ( .not.symb_zero )y( iy ) = y( iy ) + sign( safe1, y( iy ) )
                    iy = iy + incy
                 end do
              end if
           end if
           return
     end subroutine stdlib_I64_cla_heamv

     module subroutine stdlib_I64_zla_heamv( uplo, n, alpha, a, lda, x, incx, beta, y,incy )
     !! ZLA_SYAMV  performs the matrix-vector operation
     !! y := alpha*abs(A)*abs(x) + beta*abs(y),
     !! where alpha and beta are scalars, x and y are vectors and A is an
     !! n by n symmetric matrix.
     !! This function is primarily used in calculating error bounds.
     !! To protect against underflow during evaluation, components in
     !! the resulting vector are perturbed away from zero by (N+1)
     !! times the underflow threshold.  To prevent unnecessarily large
     !! errors for block-structure embedded in general matrices,
     !! "symbolically" zero components are not perturbed.  A zero
     !! entry is considered "symbolic" if all multiplications involved
     !! in computing that entry have at least one zero multiplicand.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           real(dp), intent(in) :: alpha, beta
           integer(ilp64), intent(in) :: incx, incy, lda, n, uplo
           ! Array Arguments 
           complex(dp), intent(in) :: a(lda,*), x(*)
           real(dp), intent(inout) :: y(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: symb_zero
           real(dp) :: temp, safe1
           integer(ilp64) :: i, info, iy, j, jx, kx, ky
           complex(dp) :: zdum
           ! Intrinsic Functions 
           ! Statement Functions 
           real(dp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=dp) ) + abs( aimag ( zdum ) )
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           if     ( uplo/=stdlib_I64_ilauplo( 'U' ) .and.uplo/=stdlib_I64_ilauplo( 'L' ) )then
              info = 1_ilp64
           else if( n<0_ilp64 )then
              info = 2_ilp64
           else if( lda<max( 1_ilp64, n ) )then
              info = 5_ilp64
           else if( incx==0_ilp64 )then
              info = 7_ilp64
           else if( incy==0_ilp64 )then
              info = 10_ilp64
           end if
           if( info/=0_ilp64 )then
              call stdlib_I64_xerbla( 'ZHEMV ', info )
              return
           end if
           ! quick return if possible.
           if( ( n==0 ).or.( ( alpha==zero ).and.( beta==one ) ) )return
           ! set up the start points in  x  and  y.
           if( incx>0_ilp64 )then
              kx = 1_ilp64
           else
              kx = 1_ilp64 - ( n - 1_ilp64 )*incx
           end if
           if( incy>0_ilp64 )then
              ky = 1_ilp64
           else
              ky = 1_ilp64 - ( n - 1_ilp64 )*incy
           end if
           ! set safe1 essentially to be the underflow threshold times the
           ! number of additions in each row.
           safe1 = stdlib_I64_dlamch( 'SAFE MINIMUM' )
           safe1 = (n+1)*safe1
           ! form  y := alpha*abs(a)*abs(x) + beta*abs(y).
           ! the o(n^2) symb_zero tests could be replaced by o(n) queries to
           ! the inexact flag.  still doesn't help change the iteration order
           ! to per-column.
           iy = ky
           if ( incx==1_ilp64 ) then
              if ( uplo == stdlib_I64_ilauplo( 'U' ) ) then
                 do i = 1, n
                    if ( beta == zero ) then
                       symb_zero = .true.
                       y( iy ) = zero
                    else if ( y( iy ) == zero ) then
                       symb_zero = .true.
                    else
                       symb_zero = .false.
                       y( iy ) = beta * abs( y( iy ) )
                    end if
                    if ( alpha /= zero ) then
                       do j = 1, i
                          temp = cabs1( a( j, i ) )
                          symb_zero = symb_zero .and.( x( j ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*cabs1( x( j ) )*temp
                       end do
                       do j = i+1, n
                          temp = cabs1( a( i, j ) )
                          symb_zero = symb_zero .and.( x( j ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*cabs1( x( j ) )*temp
                       end do
                    end if
                    if (.not.symb_zero)y( iy ) = y( iy ) + sign( safe1, y( iy ) )
                    iy = iy + incy
                 end do
              else
                 do i = 1, n
                    if ( beta == zero ) then
                       symb_zero = .true.
                       y( iy ) = zero
                    else if ( y( iy ) == zero ) then
                       symb_zero = .true.
                    else
                       symb_zero = .false.
                       y( iy ) = beta * abs( y( iy ) )
                    end if
                    if ( alpha /= zero ) then
                       do j = 1, i
                          temp = cabs1( a( i, j ) )
                          symb_zero = symb_zero .and.( x( j ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*cabs1( x( j ) )*temp
                       end do
                       do j = i+1, n
                          temp = cabs1( a( j, i ) )
                          symb_zero = symb_zero .and.( x( j ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*cabs1( x( j ) )*temp
                       end do
                    end if
                    if (.not.symb_zero)y( iy ) = y( iy ) + sign( safe1, y( iy ) )
                    iy = iy + incy
                 end do
              end if
           else
              if ( uplo == stdlib_I64_ilauplo( 'U' ) ) then
                 do i = 1, n
                    if ( beta == zero ) then
                       symb_zero = .true.
                       y( iy ) = zero
                    else if ( y( iy ) == zero ) then
                       symb_zero = .true.
                    else
                       symb_zero = .false.
                       y( iy ) = beta * abs( y( iy ) )
                    end if
                    jx = kx
                    if ( alpha /= zero ) then
                       do j = 1, i
                          temp = cabs1( a( j, i ) )
                          symb_zero = symb_zero .and.( x( j ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*cabs1( x( jx ) )*temp
                          jx = jx + incx
                       end do
                       do j = i+1, n
                          temp = cabs1( a( i, j ) )
                          symb_zero = symb_zero .and.( x( j ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*cabs1( x( jx ) )*temp
                          jx = jx + incx
                       end do
                    end if
                    if ( .not.symb_zero )y( iy ) = y( iy ) + sign( safe1, y( iy ) )
                    iy = iy + incy
                 end do
              else
                 do i = 1, n
                    if ( beta == zero ) then
                       symb_zero = .true.
                       y( iy ) = zero
                    else if ( y( iy ) == zero ) then
                       symb_zero = .true.
                    else
                       symb_zero = .false.
                       y( iy ) = beta * abs( y( iy ) )
                    end if
                    jx = kx
                    if ( alpha /= zero ) then
                       do j = 1, i
                          temp = cabs1( a( i, j ) )
                          symb_zero = symb_zero .and.( x( j ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*cabs1( x( jx ) )*temp
                          jx = jx + incx
                       end do
                       do j = i+1, n
                          temp = cabs1( a( j, i ) )
                          symb_zero = symb_zero .and.( x( j ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*cabs1( x( jx ) )*temp
                          jx = jx + incx
                       end do
                    end if
                    if ( .not.symb_zero )y( iy ) = y( iy ) + sign( safe1, y( iy ) )
                    iy = iy + incy
                 end do
              end if
           end if
           return
     end subroutine stdlib_I64_zla_heamv




     pure module subroutine stdlib_I64_sla_wwaddw( n, x, y, w )
     !! SLA_WWADDW adds a vector W into a doubled-single vector (X, Y).
     !! This works for all extant IBM's hex and binary floating point
     !! arithmetic, but not for decimal.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: n
           ! Array Arguments 
           real(sp), intent(inout) :: x(*), y(*)
           real(sp), intent(in) :: w(*)
        ! =====================================================================
           ! Local Scalars 
           real(sp) :: s
           integer(ilp64) :: i
           ! Executable Statements 
           do 10 i = 1, n
             s = x(i) + w(i)
             s = (s + s) - s
             y(i) = ((x(i) - s) + w(i)) + y(i)
             x(i) = s
             10 continue
           return
     end subroutine stdlib_I64_sla_wwaddw

     pure module subroutine stdlib_I64_dla_wwaddw( n, x, y, w )
     !! DLA_WWADDW adds a vector W into a doubled-single vector (X, Y).
     !! This works for all extant IBM's hex and binary floating point
     !! arithmetic, but not for decimal.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: n
           ! Array Arguments 
           real(dp), intent(inout) :: x(*), y(*)
           real(dp), intent(in) :: w(*)
        ! =====================================================================
           ! Local Scalars 
           real(dp) :: s
           integer(ilp64) :: i
           ! Executable Statements 
           do 10 i = 1, n
             s = x(i) + w(i)
             s = (s + s) - s
             y(i) = ((x(i) - s) + w(i)) + y(i)
             x(i) = s
             10 continue
           return
     end subroutine stdlib_I64_dla_wwaddw


     pure module subroutine stdlib_I64_cla_wwaddw( n, x, y, w )
     !! CLA_WWADDW adds a vector W into a doubled-single vector (X, Y).
     !! This works for all extant IBM's hex and binary floating point
     !! arithmetic, but not for decimal.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: n
           ! Array Arguments 
           complex(sp), intent(inout) :: x(*), y(*)
           complex(sp), intent(in) :: w(*)
        ! =====================================================================
           ! Local Scalars 
           complex(sp) :: s
           integer(ilp64) :: i
           ! Executable Statements 
           do 10 i = 1, n
             s = x(i) + w(i)
             s = (s + s) - s
             y(i) = ((x(i) - s) + w(i)) + y(i)
             x(i) = s
             10 continue
           return
     end subroutine stdlib_I64_cla_wwaddw

     pure module subroutine stdlib_I64_zla_wwaddw( n, x, y, w )
     !! ZLA_WWADDW adds a vector W into a doubled-single vector (X, Y).
     !! This works for all extant IBM's hex and binary floating point
     !! arithmetic, but not for decimal.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: n
           ! Array Arguments 
           complex(dp), intent(inout) :: x(*), y(*)
           complex(dp), intent(in) :: w(*)
        ! =====================================================================
           ! Local Scalars 
           complex(dp) :: s
           integer(ilp64) :: i
           ! Executable Statements 
           do 10 i = 1, n
             s = x(i) + w(i)
             s = (s + s) - s
             y(i) = ((x(i) - s) + w(i)) + y(i)
             x(i) = s
             10 continue
           return
     end subroutine stdlib_I64_zla_wwaddw




     pure module subroutine stdlib_I64_cspmv( uplo, n, alpha, ap, x, incx, beta, y, incy )
     !! CSPMV performs the matrix-vector operation
     !! y := alpha*A*x + beta*y,
     !! where alpha and beta are scalars, x and y are n element vectors and
     !! A is an n by n symmetric matrix, supplied in packed form.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(in) :: incx, incy, n
           complex(sp), intent(in) :: alpha, beta
           ! Array Arguments 
           complex(sp), intent(in) :: ap(*), x(*)
           complex(sp), intent(inout) :: y(*)
       ! =====================================================================
           
           
           ! Local Scalars 
           integer(ilp64) :: i, info, ix, iy, j, jx, jy, k, kk, kx, ky
           complex(sp) :: temp1, temp2
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = 1_ilp64
           else if( n<0_ilp64 ) then
              info = 2_ilp64
           else if( incx==0_ilp64 ) then
              info = 6_ilp64
           else if( incy==0_ilp64 ) then
              info = 9_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'CSPMV ', info )
              return
           end if
           ! quick return if possible.
           if( ( n==0 ) .or. ( ( alpha==czero ) .and. ( beta==cone ) ) )return
           ! set up the start points in  x  and  y.
           if( incx>0_ilp64 ) then
              kx = 1_ilp64
           else
              kx = 1_ilp64 - ( n-1 )*incx
           end if
           if( incy>0_ilp64 ) then
              ky = 1_ilp64
           else
              ky = 1_ilp64 - ( n-1 )*incy
           end if
           ! start the operations. in this version the elements of the array ap
           ! are accessed sequentially with cone pass through ap.
           ! first form  y := beta*y.
           if( beta/=cone ) then
              if( incy==1_ilp64 ) then
                 if( beta==czero ) then
                    do i = 1, n
                       y( i ) = czero
                    end do
                 else
                    do i = 1, n
                       y( i ) = beta*y( i )
                    end do
                 end if
              else
                 iy = ky
                 if( beta==czero ) then
                    do i = 1, n
                       y( iy ) = czero
                       iy = iy + incy
                    end do
                 else
                    do i = 1, n
                       y( iy ) = beta*y( iy )
                       iy = iy + incy
                    end do
                 end if
              end if
           end if
           if( alpha==czero )return
           kk = 1_ilp64
           if( stdlib_lsame( uplo, 'U' ) ) then
              ! form  y  when ap contains the upper triangle.
              if( ( incx==1_ilp64 ) .and. ( incy==1_ilp64 ) ) then
                 do j = 1, n
                    temp1 = alpha*x( j )
                    temp2 = czero
                    k = kk
                    do i = 1, j - 1
                       y( i ) = y( i ) + temp1*ap( k )
                       temp2 = temp2 + ap( k )*x( i )
                       k = k + 1_ilp64
                    end do
                    y( j ) = y( j ) + temp1*ap( kk+j-1 ) + alpha*temp2
                    kk = kk + j
                 end do
              else
                 jx = kx
                 jy = ky
                 do j = 1, n
                    temp1 = alpha*x( jx )
                    temp2 = czero
                    ix = kx
                    iy = ky
                    do k = kk, kk + j - 2
                       y( iy ) = y( iy ) + temp1*ap( k )
                       temp2 = temp2 + ap( k )*x( ix )
                       ix = ix + incx
                       iy = iy + incy
                    end do
                    y( jy ) = y( jy ) + temp1*ap( kk+j-1 ) + alpha*temp2
                    jx = jx + incx
                    jy = jy + incy
                    kk = kk + j
                 end do
              end if
           else
              ! form  y  when ap contains the lower triangle.
              if( ( incx==1_ilp64 ) .and. ( incy==1_ilp64 ) ) then
                 do j = 1, n
                    temp1 = alpha*x( j )
                    temp2 = czero
                    y( j ) = y( j ) + temp1*ap( kk )
                    k = kk + 1_ilp64
                    do i = j + 1, n
                       y( i ) = y( i ) + temp1*ap( k )
                       temp2 = temp2 + ap( k )*x( i )
                       k = k + 1_ilp64
                    end do
                    y( j ) = y( j ) + alpha*temp2
                    kk = kk + ( n-j+1 )
                 end do
              else
                 jx = kx
                 jy = ky
                 do j = 1, n
                    temp1 = alpha*x( jx )
                    temp2 = czero
                    y( jy ) = y( jy ) + temp1*ap( kk )
                    ix = jx
                    iy = jy
                    do k = kk + 1, kk + n - j
                       ix = ix + incx
                       iy = iy + incy
                       y( iy ) = y( iy ) + temp1*ap( k )
                       temp2 = temp2 + ap( k )*x( ix )
                    end do
                    y( jy ) = y( jy ) + alpha*temp2
                    jx = jx + incx
                    jy = jy + incy
                    kk = kk + ( n-j+1 )
                 end do
              end if
           end if
           return
     end subroutine stdlib_I64_cspmv

     pure module subroutine stdlib_I64_zspmv( uplo, n, alpha, ap, x, incx, beta, y, incy )
     !! ZSPMV performs the matrix-vector operation
     !! y := alpha*A*x + beta*y,
     !! where alpha and beta are scalars, x and y are n element vectors and
     !! A is an n by n symmetric matrix, supplied in packed form.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(in) :: incx, incy, n
           complex(dp), intent(in) :: alpha, beta
           ! Array Arguments 
           complex(dp), intent(in) :: ap(*), x(*)
           complex(dp), intent(inout) :: y(*)
       ! =====================================================================
           
           
           ! Local Scalars 
           integer(ilp64) :: i, info, ix, iy, j, jx, jy, k, kk, kx, ky
           complex(dp) :: temp1, temp2
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = 1_ilp64
           else if( n<0_ilp64 ) then
              info = 2_ilp64
           else if( incx==0_ilp64 ) then
              info = 6_ilp64
           else if( incy==0_ilp64 ) then
              info = 9_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'ZSPMV ', info )
              return
           end if
           ! quick return if possible.
           if( ( n==0 ) .or. ( ( alpha==czero ) .and. ( beta==cone ) ) )return
           ! set up the start points in  x  and  y.
           if( incx>0_ilp64 ) then
              kx = 1_ilp64
           else
              kx = 1_ilp64 - ( n-1 )*incx
           end if
           if( incy>0_ilp64 ) then
              ky = 1_ilp64
           else
              ky = 1_ilp64 - ( n-1 )*incy
           end if
           ! start the operations. in this version the elements of the array ap
           ! are accessed sequentially with cone pass through ap.
           ! first form  y := beta*y.
           if( beta/=cone ) then
              if( incy==1_ilp64 ) then
                 if( beta==czero ) then
                    do i = 1, n
                       y( i ) = czero
                    end do
                 else
                    do i = 1, n
                       y( i ) = beta*y( i )
                    end do
                 end if
              else
                 iy = ky
                 if( beta==czero ) then
                    do i = 1, n
                       y( iy ) = czero
                       iy = iy + incy
                    end do
                 else
                    do i = 1, n
                       y( iy ) = beta*y( iy )
                       iy = iy + incy
                    end do
                 end if
              end if
           end if
           if( alpha==czero )return
           kk = 1_ilp64
           if( stdlib_lsame( uplo, 'U' ) ) then
              ! form  y  when ap contains the upper triangle.
              if( ( incx==1_ilp64 ) .and. ( incy==1_ilp64 ) ) then
                 do j = 1, n
                    temp1 = alpha*x( j )
                    temp2 = czero
                    k = kk
                    do i = 1, j - 1
                       y( i ) = y( i ) + temp1*ap( k )
                       temp2 = temp2 + ap( k )*x( i )
                       k = k + 1_ilp64
                    end do
                    y( j ) = y( j ) + temp1*ap( kk+j-1 ) + alpha*temp2
                    kk = kk + j
                 end do
              else
                 jx = kx
                 jy = ky
                 do j = 1, n
                    temp1 = alpha*x( jx )
                    temp2 = czero
                    ix = kx
                    iy = ky
                    do k = kk, kk + j - 2
                       y( iy ) = y( iy ) + temp1*ap( k )
                       temp2 = temp2 + ap( k )*x( ix )
                       ix = ix + incx
                       iy = iy + incy
                    end do
                    y( jy ) = y( jy ) + temp1*ap( kk+j-1 ) + alpha*temp2
                    jx = jx + incx
                    jy = jy + incy
                    kk = kk + j
                 end do
              end if
           else
              ! form  y  when ap contains the lower triangle.
              if( ( incx==1_ilp64 ) .and. ( incy==1_ilp64 ) ) then
                 do j = 1, n
                    temp1 = alpha*x( j )
                    temp2 = czero
                    y( j ) = y( j ) + temp1*ap( kk )
                    k = kk + 1_ilp64
                    do i = j + 1, n
                       y( i ) = y( i ) + temp1*ap( k )
                       temp2 = temp2 + ap( k )*x( i )
                       k = k + 1_ilp64
                    end do
                    y( j ) = y( j ) + alpha*temp2
                    kk = kk + ( n-j+1 )
                 end do
              else
                 jx = kx
                 jy = ky
                 do j = 1, n
                    temp1 = alpha*x( jx )
                    temp2 = czero
                    y( jy ) = y( jy ) + temp1*ap( kk )
                    ix = jx
                    iy = jy
                    do k = kk + 1, kk + n - j
                       ix = ix + incx
                       iy = iy + incy
                       y( iy ) = y( iy ) + temp1*ap( k )
                       temp2 = temp2 + ap( k )*x( ix )
                    end do
                    y( jy ) = y( jy ) + alpha*temp2
                    jx = jx + incx
                    jy = jy + incy
                    kk = kk + ( n-j+1 )
                 end do
              end if
           end if
           return
     end subroutine stdlib_I64_zspmv




     pure module subroutine stdlib_I64_cspr( uplo, n, alpha, x, incx, ap )
     !! CSPR performs the symmetric rank 1 operation
     !! A := alpha*x*x**H + A,
     !! where alpha is a complex scalar, x is an n element vector and A is an
     !! n by n symmetric matrix, supplied in packed form.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(in) :: incx, n
           complex(sp), intent(in) :: alpha
           ! Array Arguments 
           complex(sp), intent(inout) :: ap(*)
           complex(sp), intent(in) :: x(*)
       ! =====================================================================
           
           ! Local Scalars 
           integer(ilp64) :: i, info, ix, j, jx, k, kk, kx
           complex(sp) :: temp
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = 1_ilp64
           else if( n<0_ilp64 ) then
              info = 2_ilp64
           else if( incx==0_ilp64 ) then
              info = 5_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'CSPR  ', info )
              return
           end if
           ! quick return if possible.
           if( ( n==0 ) .or. ( alpha==czero ) )return
           ! set the start point in x if the increment is not unity.
           if( incx<=0_ilp64 ) then
              kx = 1_ilp64 - ( n-1 )*incx
           else if( incx/=1_ilp64 ) then
              kx = 1_ilp64
           end if
           ! start the operations. in this version the elements of the array ap
           ! are accessed sequentially with cone pass through ap.
           kk = 1_ilp64
           if( stdlib_lsame( uplo, 'U' ) ) then
              ! form  a  when upper triangle is stored in ap.
              if( incx==1_ilp64 ) then
                 do j = 1, n
                    if( x( j )/=czero ) then
                       temp = alpha*x( j )
                       k = kk
                       do i = 1, j - 1
                          ap( k ) = ap( k ) + x( i )*temp
                          k = k + 1_ilp64
                       end do
                       ap( kk+j-1 ) = ap( kk+j-1 ) + x( j )*temp
                    else
                       ap( kk+j-1 ) = ap( kk+j-1 )
                    end if
                    kk = kk + j
                 end do
              else
                 jx = kx
                 do j = 1, n
                    if( x( jx )/=czero ) then
                       temp = alpha*x( jx )
                       ix = kx
                       do k = kk, kk + j - 2
                          ap( k ) = ap( k ) + x( ix )*temp
                          ix = ix + incx
                       end do
                       ap( kk+j-1 ) = ap( kk+j-1 ) + x( jx )*temp
                    else
                       ap( kk+j-1 ) = ap( kk+j-1 )
                    end if
                    jx = jx + incx
                    kk = kk + j
                 end do
              end if
           else
              ! form  a  when lower triangle is stored in ap.
              if( incx==1_ilp64 ) then
                 do j = 1, n
                    if( x( j )/=czero ) then
                       temp = alpha*x( j )
                       ap( kk ) = ap( kk ) + temp*x( j )
                       k = kk + 1_ilp64
                       do i = j + 1, n
                          ap( k ) = ap( k ) + x( i )*temp
                          k = k + 1_ilp64
                       end do
                    else
                       ap( kk ) = ap( kk )
                    end if
                    kk = kk + n - j + 1_ilp64
                 end do
              else
                 jx = kx
                 do j = 1, n
                    if( x( jx )/=czero ) then
                       temp = alpha*x( jx )
                       ap( kk ) = ap( kk ) + temp*x( jx )
                       ix = jx
                       do k = kk + 1, kk + n - j
                          ix = ix + incx
                          ap( k ) = ap( k ) + x( ix )*temp
                       end do
                    else
                       ap( kk ) = ap( kk )
                    end if
                    jx = jx + incx
                    kk = kk + n - j + 1_ilp64
                 end do
              end if
           end if
           return
     end subroutine stdlib_I64_cspr

     pure module subroutine stdlib_I64_zspr( uplo, n, alpha, x, incx, ap )
     !! ZSPR performs the symmetric rank 1 operation
     !! A := alpha*x*x**H + A,
     !! where alpha is a complex scalar, x is an n element vector and A is an
     !! n by n symmetric matrix, supplied in packed form.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(in) :: incx, n
           complex(dp), intent(in) :: alpha
           ! Array Arguments 
           complex(dp), intent(inout) :: ap(*)
           complex(dp), intent(in) :: x(*)
       ! =====================================================================
           
           ! Local Scalars 
           integer(ilp64) :: i, info, ix, j, jx, k, kk, kx
           complex(dp) :: temp
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = 1_ilp64
           else if( n<0_ilp64 ) then
              info = 2_ilp64
           else if( incx==0_ilp64 ) then
              info = 5_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'ZSPR  ', info )
              return
           end if
           ! quick return if possible.
           if( ( n==0 ) .or. ( alpha==czero ) )return
           ! set the start point in x if the increment is not unity.
           if( incx<=0_ilp64 ) then
              kx = 1_ilp64 - ( n-1 )*incx
           else if( incx/=1_ilp64 ) then
              kx = 1_ilp64
           end if
           ! start the operations. in this version the elements of the array ap
           ! are accessed sequentially with cone pass through ap.
           kk = 1_ilp64
           if( stdlib_lsame( uplo, 'U' ) ) then
              ! form  a  when upper triangle is stored in ap.
              if( incx==1_ilp64 ) then
                 do j = 1, n
                    if( x( j )/=czero ) then
                       temp = alpha*x( j )
                       k = kk
                       do i = 1, j - 1
                          ap( k ) = ap( k ) + x( i )*temp
                          k = k + 1_ilp64
                       end do
                       ap( kk+j-1 ) = ap( kk+j-1 ) + x( j )*temp
                    else
                       ap( kk+j-1 ) = ap( kk+j-1 )
                    end if
                    kk = kk + j
                 end do
              else
                 jx = kx
                 do j = 1, n
                    if( x( jx )/=czero ) then
                       temp = alpha*x( jx )
                       ix = kx
                       do k = kk, kk + j - 2
                          ap( k ) = ap( k ) + x( ix )*temp
                          ix = ix + incx
                       end do
                       ap( kk+j-1 ) = ap( kk+j-1 ) + x( jx )*temp
                    else
                       ap( kk+j-1 ) = ap( kk+j-1 )
                    end if
                    jx = jx + incx
                    kk = kk + j
                 end do
              end if
           else
              ! form  a  when lower triangle is stored in ap.
              if( incx==1_ilp64 ) then
                 do j = 1, n
                    if( x( j )/=czero ) then
                       temp = alpha*x( j )
                       ap( kk ) = ap( kk ) + temp*x( j )
                       k = kk + 1_ilp64
                       do i = j + 1, n
                          ap( k ) = ap( k ) + x( i )*temp
                          k = k + 1_ilp64
                       end do
                    else
                       ap( kk ) = ap( kk )
                    end if
                    kk = kk + n - j + 1_ilp64
                 end do
              else
                 jx = kx
                 do j = 1, n
                    if( x( jx )/=czero ) then
                       temp = alpha*x( jx )
                       ap( kk ) = ap( kk ) + temp*x( jx )
                       ix = jx
                       do k = kk + 1, kk + n - j
                          ix = ix + incx
                          ap( k ) = ap( k ) + x( ix )*temp
                       end do
                    else
                       ap( kk ) = ap( kk )
                    end if
                    jx = jx + incx
                    kk = kk + n - j + 1_ilp64
                 end do
              end if
           end if
           return
     end subroutine stdlib_I64_zspr




     pure module subroutine stdlib_I64_csymv( uplo, n, alpha, a, lda, x, incx, beta, y, incy )
     !! CSYMV performs the matrix-vector  operation
     !! y := alpha*A*x + beta*y,
     !! where alpha and beta are scalars, x and y are n element vectors and
     !! A is an n by n symmetric matrix.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(in) :: incx, incy, lda, n
           complex(sp), intent(in) :: alpha, beta
           ! Array Arguments 
           complex(sp), intent(in) :: a(lda,*), x(*)
           complex(sp), intent(inout) :: y(*)
       ! =====================================================================
           
           
           ! Local Scalars 
           integer(ilp64) :: i, info, ix, iy, j, jx, jy, kx, ky
           complex(sp) :: temp1, temp2
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = 1_ilp64
           else if( n<0_ilp64 ) then
              info = 2_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = 5_ilp64
           else if( incx==0_ilp64 ) then
              info = 7_ilp64
           else if( incy==0_ilp64 ) then
              info = 10_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'CSYMV ', info )
              return
           end if
           ! quick return if possible.
           if( ( n==0 ) .or. ( ( alpha==czero ) .and. ( beta==cone ) ) )return
           ! set up the start points in  x  and  y.
           if( incx>0_ilp64 ) then
              kx = 1_ilp64
           else
              kx = 1_ilp64 - ( n-1 )*incx
           end if
           if( incy>0_ilp64 ) then
              ky = 1_ilp64
           else
              ky = 1_ilp64 - ( n-1 )*incy
           end if
           ! start the operations. in this version the elements of a are
           ! accessed sequentially with cone pass through the triangular part
           ! of a.
           ! first form  y := beta*y.
           if( beta/=cone ) then
              if( incy==1_ilp64 ) then
                 if( beta==czero ) then
                    do i = 1, n
                       y( i ) = czero
                    end do
                 else
                    do i = 1, n
                       y( i ) = beta*y( i )
                    end do
                 end if
              else
                 iy = ky
                 if( beta==czero ) then
                    do i = 1, n
                       y( iy ) = czero
                       iy = iy + incy
                    end do
                 else
                    do i = 1, n
                       y( iy ) = beta*y( iy )
                       iy = iy + incy
                    end do
                 end if
              end if
           end if
           if( alpha==czero )return
           if( stdlib_lsame( uplo, 'U' ) ) then
              ! form  y  when a is stored in upper triangle.
              if( ( incx==1_ilp64 ) .and. ( incy==1_ilp64 ) ) then
                 do j = 1, n
                    temp1 = alpha*x( j )
                    temp2 = czero
                    do i = 1, j - 1
                       y( i ) = y( i ) + temp1*a( i, j )
                       temp2 = temp2 + a( i, j )*x( i )
                    end do
                    y( j ) = y( j ) + temp1*a( j, j ) + alpha*temp2
                 end do
              else
                 jx = kx
                 jy = ky
                 do j = 1, n
                    temp1 = alpha*x( jx )
                    temp2 = czero
                    ix = kx
                    iy = ky
                    do i = 1, j - 1
                       y( iy ) = y( iy ) + temp1*a( i, j )
                       temp2 = temp2 + a( i, j )*x( ix )
                       ix = ix + incx
                       iy = iy + incy
                    end do
                    y( jy ) = y( jy ) + temp1*a( j, j ) + alpha*temp2
                    jx = jx + incx
                    jy = jy + incy
                 end do
              end if
           else
              ! form  y  when a is stored in lower triangle.
              if( ( incx==1_ilp64 ) .and. ( incy==1_ilp64 ) ) then
                 do j = 1, n
                    temp1 = alpha*x( j )
                    temp2 = czero
                    y( j ) = y( j ) + temp1*a( j, j )
                    do i = j + 1, n
                       y( i ) = y( i ) + temp1*a( i, j )
                       temp2 = temp2 + a( i, j )*x( i )
                    end do
                    y( j ) = y( j ) + alpha*temp2
                 end do
              else
                 jx = kx
                 jy = ky
                 do j = 1, n
                    temp1 = alpha*x( jx )
                    temp2 = czero
                    y( jy ) = y( jy ) + temp1*a( j, j )
                    ix = jx
                    iy = jy
                    do i = j + 1, n
                       ix = ix + incx
                       iy = iy + incy
                       y( iy ) = y( iy ) + temp1*a( i, j )
                       temp2 = temp2 + a( i, j )*x( ix )
                    end do
                    y( jy ) = y( jy ) + alpha*temp2
                    jx = jx + incx
                    jy = jy + incy
                 end do
              end if
           end if
           return
     end subroutine stdlib_I64_csymv

     pure module subroutine stdlib_I64_zsymv( uplo, n, alpha, a, lda, x, incx, beta, y, incy )
     !! ZSYMV performs the matrix-vector  operation
     !! y := alpha*A*x + beta*y,
     !! where alpha and beta are scalars, x and y are n element vectors and
     !! A is an n by n symmetric matrix.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(in) :: incx, incy, lda, n
           complex(dp), intent(in) :: alpha, beta
           ! Array Arguments 
           complex(dp), intent(in) :: a(lda,*), x(*)
           complex(dp), intent(inout) :: y(*)
       ! =====================================================================
           
           
           ! Local Scalars 
           integer(ilp64) :: i, info, ix, iy, j, jx, jy, kx, ky
           complex(dp) :: temp1, temp2
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = 1_ilp64
           else if( n<0_ilp64 ) then
              info = 2_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = 5_ilp64
           else if( incx==0_ilp64 ) then
              info = 7_ilp64
           else if( incy==0_ilp64 ) then
              info = 10_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'ZSYMV ', info )
              return
           end if
           ! quick return if possible.
           if( ( n==0 ) .or. ( ( alpha==czero ) .and. ( beta==cone ) ) )return
           ! set up the start points in  x  and  y.
           if( incx>0_ilp64 ) then
              kx = 1_ilp64
           else
              kx = 1_ilp64 - ( n-1 )*incx
           end if
           if( incy>0_ilp64 ) then
              ky = 1_ilp64
           else
              ky = 1_ilp64 - ( n-1 )*incy
           end if
           ! start the operations. in this version the elements of a are
           ! accessed sequentially with cone pass through the triangular part
           ! of a.
           ! first form  y := beta*y.
           if( beta/=cone ) then
              if( incy==1_ilp64 ) then
                 if( beta==czero ) then
                    do i = 1, n
                       y( i ) = czero
                    end do
                 else
                    do i = 1, n
                       y( i ) = beta*y( i )
                    end do
                 end if
              else
                 iy = ky
                 if( beta==czero ) then
                    do i = 1, n
                       y( iy ) = czero
                       iy = iy + incy
                    end do
                 else
                    do i = 1, n
                       y( iy ) = beta*y( iy )
                       iy = iy + incy
                    end do
                 end if
              end if
           end if
           if( alpha==czero )return
           if( stdlib_lsame( uplo, 'U' ) ) then
              ! form  y  when a is stored in upper triangle.
              if( ( incx==1_ilp64 ) .and. ( incy==1_ilp64 ) ) then
                 do j = 1, n
                    temp1 = alpha*x( j )
                    temp2 = czero
                    do i = 1, j - 1
                       y( i ) = y( i ) + temp1*a( i, j )
                       temp2 = temp2 + a( i, j )*x( i )
                    end do
                    y( j ) = y( j ) + temp1*a( j, j ) + alpha*temp2
                 end do
              else
                 jx = kx
                 jy = ky
                 do j = 1, n
                    temp1 = alpha*x( jx )
                    temp2 = czero
                    ix = kx
                    iy = ky
                    do i = 1, j - 1
                       y( iy ) = y( iy ) + temp1*a( i, j )
                       temp2 = temp2 + a( i, j )*x( ix )
                       ix = ix + incx
                       iy = iy + incy
                    end do
                    y( jy ) = y( jy ) + temp1*a( j, j ) + alpha*temp2
                    jx = jx + incx
                    jy = jy + incy
                 end do
              end if
           else
              ! form  y  when a is stored in lower triangle.
              if( ( incx==1_ilp64 ) .and. ( incy==1_ilp64 ) ) then
                 do j = 1, n
                    temp1 = alpha*x( j )
                    temp2 = czero
                    y( j ) = y( j ) + temp1*a( j, j )
                    do i = j + 1, n
                       y( i ) = y( i ) + temp1*a( i, j )
                       temp2 = temp2 + a( i, j )*x( i )
                    end do
                    y( j ) = y( j ) + alpha*temp2
                 end do
              else
                 jx = kx
                 jy = ky
                 do j = 1, n
                    temp1 = alpha*x( jx )
                    temp2 = czero
                    y( jy ) = y( jy ) + temp1*a( j, j )
                    ix = jx
                    iy = jy
                    do i = j + 1, n
                       ix = ix + incx
                       iy = iy + incy
                       y( iy ) = y( iy ) + temp1*a( i, j )
                       temp2 = temp2 + a( i, j )*x( ix )
                    end do
                    y( jy ) = y( jy ) + alpha*temp2
                    jx = jx + incx
                    jy = jy + incy
                 end do
              end if
           end if
           return
     end subroutine stdlib_I64_zsymv




     pure module subroutine stdlib_I64_csyr( uplo, n, alpha, x, incx, a, lda )
     !! CSYR performs the symmetric rank 1 operation
     !! A := alpha*x*x**H + A,
     !! where alpha is a complex scalar, x is an n element vector and A is an
     !! n by n symmetric matrix.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(in) :: incx, lda, n
           complex(sp), intent(in) :: alpha
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(in) :: x(*)
       ! =====================================================================
           
           ! Local Scalars 
           integer(ilp64) :: i, info, ix, j, jx, kx
           complex(sp) :: temp
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = 1_ilp64
           else if( n<0_ilp64 ) then
              info = 2_ilp64
           else if( incx==0_ilp64 ) then
              info = 5_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = 7_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'CSYR  ', info )
              return
           end if
           ! quick return if possible.
           if( ( n==0 ) .or. ( alpha==czero ) )return
           ! set the start point in x if the increment is not unity.
           if( incx<=0_ilp64 ) then
              kx = 1_ilp64 - ( n-1 )*incx
           else if( incx/=1_ilp64 ) then
              kx = 1_ilp64
           end if
           ! start the operations. in this version the elements of a are
           ! accessed sequentially with cone pass through the triangular part
           ! of a.
           if( stdlib_lsame( uplo, 'U' ) ) then
              ! form  a  when a is stored in upper triangle.
              if( incx==1_ilp64 ) then
                 do j = 1, n
                    if( x( j )/=czero ) then
                       temp = alpha*x( j )
                       do i = 1, j
                          a( i, j ) = a( i, j ) + x( i )*temp
                       end do
                    end if
                 end do
              else
                 jx = kx
                 do j = 1, n
                    if( x( jx )/=czero ) then
                       temp = alpha*x( jx )
                       ix = kx
                       do i = 1, j
                          a( i, j ) = a( i, j ) + x( ix )*temp
                          ix = ix + incx
                       end do
                    end if
                    jx = jx + incx
                 end do
              end if
           else
              ! form  a  when a is stored in lower triangle.
              if( incx==1_ilp64 ) then
                 do j = 1, n
                    if( x( j )/=czero ) then
                       temp = alpha*x( j )
                       do i = j, n
                          a( i, j ) = a( i, j ) + x( i )*temp
                       end do
                    end if
                 end do
              else
                 jx = kx
                 do j = 1, n
                    if( x( jx )/=czero ) then
                       temp = alpha*x( jx )
                       ix = jx
                       do i = j, n
                          a( i, j ) = a( i, j ) + x( ix )*temp
                          ix = ix + incx
                       end do
                    end if
                    jx = jx + incx
                 end do
              end if
           end if
           return
     end subroutine stdlib_I64_csyr

     pure module subroutine stdlib_I64_zsyr( uplo, n, alpha, x, incx, a, lda )
     !! ZSYR performs the symmetric rank 1 operation
     !! A := alpha*x*x**H + A,
     !! where alpha is a complex scalar, x is an n element vector and A is an
     !! n by n symmetric matrix.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(in) :: incx, lda, n
           complex(dp), intent(in) :: alpha
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(in) :: x(*)
       ! =====================================================================
           
           ! Local Scalars 
           integer(ilp64) :: i, info, ix, j, jx, kx
           complex(dp) :: temp
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           if( .not.stdlib_lsame( uplo, 'U' ) .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = 1_ilp64
           else if( n<0_ilp64 ) then
              info = 2_ilp64
           else if( incx==0_ilp64 ) then
              info = 5_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = 7_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'ZSYR  ', info )
              return
           end if
           ! quick return if possible.
           if( ( n==0 ) .or. ( alpha==czero ) )return
           ! set the start point in x if the increment is not unity.
           if( incx<=0_ilp64 ) then
              kx = 1_ilp64 - ( n-1 )*incx
           else if( incx/=1_ilp64 ) then
              kx = 1_ilp64
           end if
           ! start the operations. in this version the elements of a are
           ! accessed sequentially with cone pass through the triangular part
           ! of a.
           if( stdlib_lsame( uplo, 'U' ) ) then
              ! form  a  when a is stored in upper triangle.
              if( incx==1_ilp64 ) then
                 do j = 1, n
                    if( x( j )/=czero ) then
                       temp = alpha*x( j )
                       do i = 1, j
                          a( i, j ) = a( i, j ) + x( i )*temp
                       end do
                    end if
                 end do
              else
                 jx = kx
                 do j = 1, n
                    if( x( jx )/=czero ) then
                       temp = alpha*x( jx )
                       ix = kx
                       do i = 1, j
                          a( i, j ) = a( i, j ) + x( ix )*temp
                          ix = ix + incx
                       end do
                    end if
                    jx = jx + incx
                 end do
              end if
           else
              ! form  a  when a is stored in lower triangle.
              if( incx==1_ilp64 ) then
                 do j = 1, n
                    if( x( j )/=czero ) then
                       temp = alpha*x( j )
                       do i = j, n
                          a( i, j ) = a( i, j ) + x( i )*temp
                       end do
                    end if
                 end do
              else
                 jx = kx
                 do j = 1, n
                    if( x( jx )/=czero ) then
                       temp = alpha*x( jx )
                       ix = jx
                       do i = j, n
                          a( i, j ) = a( i, j ) + x( ix )*temp
                          ix = ix + incx
                       end do
                    end if
                    jx = jx + incx
                 end do
              end if
           end if
           return
     end subroutine stdlib_I64_zsyr



end submodule stdlib_lapack_blas_like_l2
