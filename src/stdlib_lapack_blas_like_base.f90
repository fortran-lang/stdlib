submodule(stdlib_lapack_base) stdlib_lapack_blas_like_base
  implicit none


  contains

     pure module subroutine stdlib_slaset( uplo, m, n, alpha, beta, a, lda )
     !! SLASET initializes an m-by-n matrix A to BETA on the diagonal and
     !! ALPHA on the offdiagonals.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: lda, m, n
           real(sp), intent(in) :: alpha, beta
           ! Array Arguments 
           real(sp), intent(out) :: a(lda,*)
       ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, j
           ! Intrinsic Functions 
           ! Executable Statements 
           if( stdlib_lsame( uplo, 'U' ) ) then
              ! set the strictly upper triangular or trapezoidal part of the
              ! array to alpha.
              do j = 2, n
                 do i = 1, min( j-1, m )
                    a( i, j ) = alpha
                 end do
              end do
           else if( stdlib_lsame( uplo, 'L' ) ) then
              ! set the strictly lower triangular or trapezoidal part of the
              ! array to alpha.
              do j = 1, min( m, n )
                 do i = j + 1, m
                    a( i, j ) = alpha
                 end do
              end do
           else
              ! set the leading m-by-n submatrix to alpha.
              do j = 1, n
                 do i = 1, m
                    a( i, j ) = alpha
                 end do
              end do
           end if
           ! set the first min(m,n) diagonal elements to beta.
           do i = 1, min( m, n )
              a( i, i ) = beta
           end do
           return
     end subroutine stdlib_slaset

     pure module subroutine stdlib_dlaset( uplo, m, n, alpha, beta, a, lda )
     !! DLASET initializes an m-by-n matrix A to BETA on the diagonal and
     !! ALPHA on the offdiagonals.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: lda, m, n
           real(dp), intent(in) :: alpha, beta
           ! Array Arguments 
           real(dp), intent(out) :: a(lda,*)
       ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, j
           ! Intrinsic Functions 
           ! Executable Statements 
           if( stdlib_lsame( uplo, 'U' ) ) then
              ! set the strictly upper triangular or trapezoidal part of the
              ! array to alpha.
              do j = 2, n
                 do i = 1, min( j-1, m )
                    a( i, j ) = alpha
                 end do
              end do
           else if( stdlib_lsame( uplo, 'L' ) ) then
              ! set the strictly lower triangular or trapezoidal part of the
              ! array to alpha.
              do j = 1, min( m, n )
                 do i = j + 1, m
                    a( i, j ) = alpha
                 end do
              end do
           else
              ! set the leading m-by-n submatrix to alpha.
              do j = 1, n
                 do i = 1, m
                    a( i, j ) = alpha
                 end do
              end do
           end if
           ! set the first min(m,n) diagonal elements to beta.
           do i = 1, min( m, n )
              a( i, i ) = beta
           end do
           return
     end subroutine stdlib_dlaset


     pure module subroutine stdlib_claset( uplo, m, n, alpha, beta, a, lda )
     !! CLASET initializes a 2-D array A to BETA on the diagonal and
     !! ALPHA on the offdiagonals.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: lda, m, n
           complex(sp), intent(in) :: alpha, beta
           ! Array Arguments 
           complex(sp), intent(out) :: a(lda,*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, j
           ! Intrinsic Functions 
           ! Executable Statements 
           if( stdlib_lsame( uplo, 'U' ) ) then
              ! set the diagonal to beta and the strictly upper triangular
              ! part of the array to alpha.
              do j = 2, n
                 do i = 1, min( j-1, m )
                    a( i, j ) = alpha
                 end do
              end do
              do i = 1, min( n, m )
                 a( i, i ) = beta
              end do
           else if( stdlib_lsame( uplo, 'L' ) ) then
              ! set the diagonal to beta and the strictly lower triangular
              ! part of the array to alpha.
              do j = 1, min( m, n )
                 do i = j + 1, m
                    a( i, j ) = alpha
                 end do
              end do
              do i = 1, min( n, m )
                 a( i, i ) = beta
              end do
           else
              ! set the array to beta on the diagonal and alpha on the
              ! offdiagonal.
              do j = 1, n
                 do i = 1, m
                    a( i, j ) = alpha
                 end do
              end do
              do i = 1, min( m, n )
                 a( i, i ) = beta
              end do
           end if
           return
     end subroutine stdlib_claset

     pure module subroutine stdlib_zlaset( uplo, m, n, alpha, beta, a, lda )
     !! ZLASET initializes a 2-D array A to BETA on the diagonal and
     !! ALPHA on the offdiagonals.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: lda, m, n
           complex(dp), intent(in) :: alpha, beta
           ! Array Arguments 
           complex(dp), intent(out) :: a(lda,*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, j
           ! Intrinsic Functions 
           ! Executable Statements 
           if( stdlib_lsame( uplo, 'U' ) ) then
              ! set the diagonal to beta and the strictly upper triangular
              ! part of the array to alpha.
              do j = 2, n
                 do i = 1, min( j-1, m )
                    a( i, j ) = alpha
                 end do
              end do
              do i = 1, min( n, m )
                 a( i, i ) = beta
              end do
           else if( stdlib_lsame( uplo, 'L' ) ) then
              ! set the diagonal to beta and the strictly lower triangular
              ! part of the array to alpha.
              do j = 1, min( m, n )
                 do i = j + 1, m
                    a( i, j ) = alpha
                 end do
              end do
              do i = 1, min( n, m )
                 a( i, i ) = beta
              end do
           else
              ! set the array to beta on the diagonal and alpha on the
              ! offdiagonal.
              do j = 1, n
                 do i = 1, m
                    a( i, j ) = alpha
                 end do
              end do
              do i = 1, min( m, n )
                 a( i, i ) = beta
              end do
           end if
           return
     end subroutine stdlib_zlaset




     pure module subroutine stdlib_slarnv( idist, iseed, n, x )
     !! SLARNV returns a vector of n random real numbers from a uniform or
     !! normal distribution.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: one, two
           ! Scalar Arguments 
           integer(ilp), intent(in) :: idist, n
           ! Array Arguments 
           integer(ilp), intent(inout) :: iseed(4_ilp)
           real(sp), intent(out) :: x(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: lv = 128_ilp
           real(sp), parameter :: twopi = 6.28318530717958647692528676655900576839e+0_sp
           
           
           
           ! Local Scalars 
           integer(ilp) :: i, il, il2, iv
           ! Local Arrays 
           real(sp) :: u(lv)
           ! Intrinsic Functions 
           ! Executable Statements 
           do 40 iv = 1, n, lv / 2
              il = min( lv / 2_ilp, n-iv+1 )
              if( idist==3_ilp ) then
                 il2 = 2_ilp*il
              else
                 il2 = il
              end if
              ! call stdlib_slaruv to generate il2 numbers from a uniform (0,1)
              ! distribution (il2 <= lv)
              call stdlib_slaruv( iseed, il2, u )
              if( idist==1_ilp ) then
                 ! copy generated numbers
                 do i = 1, il
                    x( iv+i-1 ) = u( i )
                 end do
              else if( idist==2_ilp ) then
                 ! convert generated numbers to uniform (-1,1) distribution
                 do i = 1, il
                    x( iv+i-1 ) = two*u( i ) - one
                 end do
              else if( idist==3_ilp ) then
                 ! convert generated numbers to normal (0,1) distribution
                 do i = 1, il
                    x( iv+i-1 ) = sqrt( -two*log( u( 2_ilp*i-1 ) ) )*cos( twopi*u( 2_ilp*i ) )
                 end do
              end if
              40 continue
           return
     end subroutine stdlib_slarnv

     pure module subroutine stdlib_dlarnv( idist, iseed, n, x )
     !! DLARNV returns a vector of n random real numbers from a uniform or
     !! normal distribution.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: one, two
           ! Scalar Arguments 
           integer(ilp), intent(in) :: idist, n
           ! Array Arguments 
           integer(ilp), intent(inout) :: iseed(4_ilp)
           real(dp), intent(out) :: x(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: lv = 128_ilp
           real(dp), parameter :: twopi = 6.28318530717958647692528676655900576839e+0_dp
           
           
           
           ! Local Scalars 
           integer(ilp) :: i, il, il2, iv
           ! Local Arrays 
           real(dp) :: u(lv)
           ! Intrinsic Functions 
           ! Executable Statements 
           do 40 iv = 1, n, lv / 2
              il = min( lv / 2_ilp, n-iv+1 )
              if( idist==3_ilp ) then
                 il2 = 2_ilp*il
              else
                 il2 = il
              end if
              ! call stdlib_dlaruv to generate il2 numbers from a uniform (0,1)
              ! distribution (il2 <= lv)
              call stdlib_dlaruv( iseed, il2, u )
              if( idist==1_ilp ) then
                 ! copy generated numbers
                 do i = 1, il
                    x( iv+i-1 ) = u( i )
                 end do
              else if( idist==2_ilp ) then
                 ! convert generated numbers to uniform (-1,1) distribution
                 do i = 1, il
                    x( iv+i-1 ) = two*u( i ) - one
                 end do
              else if( idist==3_ilp ) then
                 ! convert generated numbers to normal (0,1) distribution
                 do i = 1, il
                    x( iv+i-1 ) = sqrt( -two*log( u( 2_ilp*i-1 ) ) )*cos( twopi*u( 2_ilp*i ) )
                 end do
              end if
              40 continue
           return
     end subroutine stdlib_dlarnv


     pure module subroutine stdlib_clarnv( idist, iseed, n, x )
     !! CLARNV returns a vector of n random complex numbers from a uniform or
     !! normal distribution.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: zero, one, two
           ! Scalar Arguments 
           integer(ilp), intent(in) :: idist, n
           ! Array Arguments 
           integer(ilp), intent(inout) :: iseed(4_ilp)
           complex(sp), intent(out) :: x(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: lv = 128_ilp
           real(sp), parameter :: twopi = 6.28318530717958647692528676655900576839e+0_sp
           
           
           
           ! Local Scalars 
           integer(ilp) :: i, il, iv
           ! Local Arrays 
           real(sp) :: u(lv)
           ! Intrinsic Functions 
           ! Executable Statements 
           do 60 iv = 1, n, lv / 2
              il = min( lv / 2_ilp, n-iv+1 )
              ! call stdlib_slaruv to generate 2*il realnumbers from a uniform (0,1,KIND=sp)
              ! distribution (2*il <= lv)
              call stdlib_slaruv( iseed, 2_ilp*il, u )
              if( idist==1_ilp ) then
                 ! copy generated numbers
                 do i = 1, il
                    x( iv+i-1 ) = cmplx( u( 2_ilp*i-1 ), u( 2_ilp*i ),KIND=sp)
                 end do
              else if( idist==2_ilp ) then
                 ! convert generated numbers to uniform (-1,1) distribution
                 do i = 1, il
                    x( iv+i-1 ) = cmplx( two*u( 2_ilp*i-1 )-one,two*u( 2_ilp*i )-one,KIND=sp)
                 end do
              else if( idist==3_ilp ) then
                 ! convert generated numbers to normal (0,1) distribution
                 do i = 1, il
                    x( iv+i-1 ) = sqrt( -two*log( u( 2_ilp*i-1 ) ) )*exp( cmplx( zero, twopi*u( 2_ilp*i ),&
                              KIND=sp) )
                 end do
              else if( idist==4_ilp ) then
                 ! convert generated numbers to complex numbers uniformly
                 ! distributed on the unit disk
                 do i = 1, il
                    x( iv+i-1 ) = sqrt( u( 2_ilp*i-1 ) )*exp( cmplx( zero, twopi*u( 2_ilp*i ),KIND=sp) )
                              
                 end do
              else if( idist==5_ilp ) then
                 ! convert generated numbers to complex numbers uniformly
                 ! distributed on the unit circle
                 do i = 1, il
                    x( iv+i-1 ) = exp( cmplx( zero, twopi*u( 2_ilp*i ),KIND=sp) )
                 end do
              end if
              60 continue
           return
     end subroutine stdlib_clarnv

     pure module subroutine stdlib_zlarnv( idist, iseed, n, x )
     !! ZLARNV returns a vector of n random complex numbers from a uniform or
     !! normal distribution.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: zero, one, two
           ! Scalar Arguments 
           integer(ilp), intent(in) :: idist, n
           ! Array Arguments 
           integer(ilp), intent(inout) :: iseed(4_ilp)
           complex(dp), intent(out) :: x(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: lv = 128_ilp
           real(dp), parameter :: twopi = 6.28318530717958647692528676655900576839e+0_dp
           
           
           
           ! Local Scalars 
           integer(ilp) :: i, il, iv
           ! Local Arrays 
           real(dp) :: u(lv)
           ! Intrinsic Functions 
           ! Executable Statements 
           do 60 iv = 1, n, lv / 2
              il = min( lv / 2_ilp, n-iv+1 )
              ! call stdlib_dlaruv to generate 2*il realnumbers from a uniform (0,1,KIND=dp)
              ! distribution (2*il <= lv)
              call stdlib_dlaruv( iseed, 2_ilp*il, u )
              if( idist==1_ilp ) then
                 ! copy generated numbers
                 do i = 1, il
                    x( iv+i-1 ) = cmplx( u( 2_ilp*i-1 ), u( 2_ilp*i ),KIND=dp)
                 end do
              else if( idist==2_ilp ) then
                 ! convert generated numbers to uniform (-1,1) distribution
                 do i = 1, il
                    x( iv+i-1 ) = cmplx( two*u( 2_ilp*i-1 )-one,two*u( 2_ilp*i )-one,KIND=dp)
                 end do
              else if( idist==3_ilp ) then
                 ! convert generated numbers to normal (0,1) distribution
                 do i = 1, il
                    x( iv+i-1 ) = sqrt( -two*log( u( 2_ilp*i-1 ) ) )*exp( cmplx( zero, twopi*u( 2_ilp*i ),&
                              KIND=dp) )
                 end do
              else if( idist==4_ilp ) then
                 ! convert generated numbers to complex numbers uniformly
                 ! distributed on the unit disk
                 do i = 1, il
                    x( iv+i-1 ) = sqrt( u( 2_ilp*i-1 ) )*exp( cmplx( zero, twopi*u( 2_ilp*i ),KIND=dp) )
                              
                 end do
              else if( idist==5_ilp ) then
                 ! convert generated numbers to complex numbers uniformly
                 ! distributed on the unit circle
                 do i = 1, il
                    x( iv+i-1 ) = exp( cmplx( zero, twopi*u( 2_ilp*i ),KIND=dp) )
                 end do
              end if
              60 continue
           return
     end subroutine stdlib_zlarnv




     pure module subroutine stdlib_slaruv( iseed, n, x )
     !! SLARUV returns a vector of n random real numbers from a uniform (0,1)
     !! distribution (n <= 128).
     !! This is an auxiliary routine called by SLARNV and CLARNV.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: one
           ! Scalar Arguments 
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           integer(ilp), intent(inout) :: iseed(4_ilp)
           real(sp), intent(out) :: x(n)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: lv = 128_ilp
           integer(ilp), parameter :: ipw2 = 4096_ilp
           real(sp), parameter :: r = one/ipw2
           
           
           
           ! Local Scalars 
           integer(ilp) :: i, i1, i2, i3, i4, it1, it2, it3, it4
           ! Local Arrays 
           integer(ilp) :: mm(lv,4_ilp)
           ! Intrinsic Functions 
           ! Data Statements 
           mm(1_ilp,1_ilp:4_ilp)=[494_ilp,322_ilp,2508_ilp,2549_ilp]
           mm(2_ilp,1_ilp:4_ilp)=[2637_ilp,789_ilp,3754_ilp,1145_ilp]
           mm(3_ilp,1_ilp:4_ilp)=[255_ilp,1440_ilp,1766_ilp,2253_ilp]
           mm(4_ilp,1_ilp:4_ilp)=[2008_ilp,752_ilp,3572_ilp,305_ilp]
           mm(5_ilp,1_ilp:4_ilp)=[1253_ilp,2859_ilp,2893_ilp,3301_ilp]
           mm(6_ilp,1_ilp:4_ilp)=[3344_ilp,123_ilp,307_ilp,1065_ilp]
           mm(7_ilp,1_ilp:4_ilp)=[4084_ilp,1848_ilp,1297_ilp,3133_ilp]
           mm(8_ilp,1_ilp:4_ilp)=[1739_ilp,643_ilp,3966_ilp,2913_ilp]
           mm(9_ilp,1_ilp:4_ilp)=[3143_ilp,2405_ilp,758_ilp,3285_ilp]
           mm(10_ilp,1_ilp:4_ilp)=[3468_ilp,2638_ilp,2598_ilp,1241_ilp]
           mm(11_ilp,1_ilp:4_ilp)=[688_ilp,2344_ilp,3406_ilp,1197_ilp]
           mm(12_ilp,1_ilp:4_ilp)=[1657_ilp,46_ilp,2922_ilp,3729_ilp]
           mm(13_ilp,1_ilp:4_ilp)=[1238_ilp,3814_ilp,1038_ilp,2501_ilp]
           mm(14_ilp,1_ilp:4_ilp)=[3166_ilp,913_ilp,2934_ilp,1673_ilp]
           mm(15_ilp,1_ilp:4_ilp)=[1292_ilp,3649_ilp,2091_ilp,541_ilp]
           mm(16_ilp,1_ilp:4_ilp)=[3422_ilp,339_ilp,2451_ilp,2753_ilp]
           mm(17_ilp,1_ilp:4_ilp)=[1270_ilp,3808_ilp,1580_ilp,949_ilp]
           mm(18_ilp,1_ilp:4_ilp)=[2016_ilp,822_ilp,1958_ilp,2361_ilp]
           mm(19_ilp,1_ilp:4_ilp)=[154_ilp,2832_ilp,2055_ilp,1165_ilp]
           mm(20_ilp,1_ilp:4_ilp)=[2862_ilp,3078_ilp,1507_ilp,4081_ilp]
           mm(21_ilp,1_ilp:4_ilp)=[697_ilp,3633_ilp,1078_ilp,2725_ilp]
           mm(22_ilp,1_ilp:4_ilp)=[1706_ilp,2970_ilp,3273_ilp,3305_ilp]
           mm(23_ilp,1_ilp:4_ilp)=[491_ilp,637_ilp,17_ilp,3069_ilp]
           mm(24_ilp,1_ilp:4_ilp)=[931_ilp,2249_ilp,854_ilp,3617_ilp]
           mm(25_ilp,1_ilp:4_ilp)=[1444_ilp,2081_ilp,2916_ilp,3733_ilp]
           mm(26_ilp,1_ilp:4_ilp)=[444_ilp,4019_ilp,3971_ilp,409_ilp]
           mm(27_ilp,1_ilp:4_ilp)=[3577_ilp,1478_ilp,2889_ilp,2157_ilp]
           mm(28_ilp,1_ilp:4_ilp)=[3944_ilp,242_ilp,3831_ilp,1361_ilp]
           mm(29_ilp,1_ilp:4_ilp)=[2184_ilp,481_ilp,2621_ilp,3973_ilp]
           mm(30_ilp,1_ilp:4_ilp)=[1661_ilp,2075_ilp,1541_ilp,1865_ilp]
           mm(31_ilp,1_ilp:4_ilp)=[3482_ilp,4058_ilp,893_ilp,2525_ilp]
           mm(32_ilp,1_ilp:4_ilp)=[657_ilp,622_ilp,736_ilp,1409_ilp]
           mm(33_ilp,1_ilp:4_ilp)=[3023_ilp,3376_ilp,3992_ilp,3445_ilp]
           mm(34_ilp,1_ilp:4_ilp)=[3618_ilp,812_ilp,787_ilp,3577_ilp]
           mm(35_ilp,1_ilp:4_ilp)=[1267_ilp,234_ilp,2125_ilp,77_ilp]
           mm(36_ilp,1_ilp:4_ilp)=[1828_ilp,641_ilp,2364_ilp,3761_ilp]
           mm(37_ilp,1_ilp:4_ilp)=[164_ilp,4005_ilp,2460_ilp,2149_ilp]
           mm(38_ilp,1_ilp:4_ilp)=[3798_ilp,1122_ilp,257_ilp,1449_ilp]
           mm(39_ilp,1_ilp:4_ilp)=[3087_ilp,3135_ilp,1574_ilp,3005_ilp]
           mm(40_ilp,1_ilp:4_ilp)=[2400_ilp,2640_ilp,3912_ilp,225_ilp]
           mm(41_ilp,1_ilp:4_ilp)=[2870_ilp,2302_ilp,1216_ilp,85_ilp]
           mm(42_ilp,1_ilp:4_ilp)=[3876_ilp,40_ilp,3248_ilp,3673_ilp]
           mm(43_ilp,1_ilp:4_ilp)=[1905_ilp,1832_ilp,3401_ilp,3117_ilp]
           mm(44_ilp,1_ilp:4_ilp)=[1593_ilp,2247_ilp,2124_ilp,3089_ilp]
           mm(45_ilp,1_ilp:4_ilp)=[1797_ilp,2034_ilp,2762_ilp,1349_ilp]
           mm(46_ilp,1_ilp:4_ilp)=[1234_ilp,2637_ilp,149_ilp,2057_ilp]
           mm(47_ilp,1_ilp:4_ilp)=[3460_ilp,1287_ilp,2245_ilp,413_ilp]
           mm(48_ilp,1_ilp:4_ilp)=[328_ilp,1691_ilp,166_ilp,65_ilp]
           mm(49_ilp,1_ilp:4_ilp)=[2861_ilp,496_ilp,466_ilp,1845_ilp]
           mm(50_ilp,1_ilp:4_ilp)=[1950_ilp,1597_ilp,4018_ilp,697_ilp]
           mm(51_ilp,1_ilp:4_ilp)=[617_ilp,2394_ilp,1399_ilp,3085_ilp]
           mm(52_ilp,1_ilp:4_ilp)=[2070_ilp,2584_ilp,190_ilp,3441_ilp]
           mm(53_ilp,1_ilp:4_ilp)=[3331_ilp,1843_ilp,2879_ilp,1573_ilp]
           mm(54_ilp,1_ilp:4_ilp)=[769_ilp,336_ilp,153_ilp,3689_ilp]
           mm(55_ilp,1_ilp:4_ilp)=[1558_ilp,1472_ilp,2320_ilp,2941_ilp]
           mm(56_ilp,1_ilp:4_ilp)=[2412_ilp,2407_ilp,18_ilp,929_ilp]
           mm(57_ilp,1_ilp:4_ilp)=[2800_ilp,433_ilp,712_ilp,533_ilp]
           mm(58_ilp,1_ilp:4_ilp)=[189_ilp,2096_ilp,2159_ilp,2841_ilp]
           mm(59_ilp,1_ilp:4_ilp)=[287_ilp,1761_ilp,2318_ilp,4077_ilp]
           mm(60_ilp,1_ilp:4_ilp)=[2045_ilp,2810_ilp,2091_ilp,721_ilp]
           mm(61_ilp,1_ilp:4_ilp)=[1227_ilp,566_ilp,3443_ilp,2821_ilp]
           mm(62_ilp,1_ilp:4_ilp)=[2838_ilp,442_ilp,1510_ilp,2249_ilp]
           mm(63_ilp,1_ilp:4_ilp)=[209_ilp,41_ilp,449_ilp,2397_ilp]
           mm(64_ilp,1_ilp:4_ilp)=[2770_ilp,1238_ilp,1956_ilp,2817_ilp]
           mm(65_ilp,1_ilp:4_ilp)=[3654_ilp,1086_ilp,2201_ilp,245_ilp]
           mm(66_ilp,1_ilp:4_ilp)=[3993_ilp,603_ilp,3137_ilp,1913_ilp]
           mm(67_ilp,1_ilp:4_ilp)=[192_ilp,840_ilp,3399_ilp,1997_ilp]
           mm(68_ilp,1_ilp:4_ilp)=[2253_ilp,3168_ilp,1321_ilp,3121_ilp]
           mm(69_ilp,1_ilp:4_ilp)=[3491_ilp,1499_ilp,2271_ilp,997_ilp]
           mm(70_ilp,1_ilp:4_ilp)=[2889_ilp,1084_ilp,3667_ilp,1833_ilp]
           mm(71_ilp,1_ilp:4_ilp)=[2857_ilp,3438_ilp,2703_ilp,2877_ilp]
           mm(72_ilp,1_ilp:4_ilp)=[2094_ilp,2408_ilp,629_ilp,1633_ilp]
           mm(73_ilp,1_ilp:4_ilp)=[1818_ilp,1589_ilp,2365_ilp,981_ilp]
           mm(74_ilp,1_ilp:4_ilp)=[688_ilp,2391_ilp,2431_ilp,2009_ilp]
           mm(75_ilp,1_ilp:4_ilp)=[1407_ilp,288_ilp,1113_ilp,941_ilp]
           mm(76_ilp,1_ilp:4_ilp)=[634_ilp,26_ilp,3922_ilp,2449_ilp]
           mm(77_ilp,1_ilp:4_ilp)=[3231_ilp,512_ilp,2554_ilp,197_ilp]
           mm(78_ilp,1_ilp:4_ilp)=[815_ilp,1456_ilp,184_ilp,2441_ilp]
           mm(79_ilp,1_ilp:4_ilp)=[3524_ilp,171_ilp,2099_ilp,285_ilp]
           mm(80_ilp,1_ilp:4_ilp)=[1914_ilp,1677_ilp,3228_ilp,1473_ilp]
           mm(81_ilp,1_ilp:4_ilp)=[516_ilp,2657_ilp,4012_ilp,2741_ilp]
           mm(82_ilp,1_ilp:4_ilp)=[164_ilp,2270_ilp,1921_ilp,3129_ilp]
           mm(83_ilp,1_ilp:4_ilp)=[303_ilp,2587_ilp,3452_ilp,909_ilp]
           mm(84_ilp,1_ilp:4_ilp)=[2144_ilp,2961_ilp,3901_ilp,2801_ilp]
           mm(85_ilp,1_ilp:4_ilp)=[3480_ilp,1970_ilp,572_ilp,421_ilp]
           mm(86_ilp,1_ilp:4_ilp)=[119_ilp,1817_ilp,3309_ilp,4073_ilp]
           mm(87_ilp,1_ilp:4_ilp)=[3357_ilp,676_ilp,3171_ilp,2813_ilp]
           mm(88_ilp,1_ilp:4_ilp)=[837_ilp,1410_ilp,817_ilp,2337_ilp]
           mm(89_ilp,1_ilp:4_ilp)=[2826_ilp,3723_ilp,3039_ilp,1429_ilp]
           mm(90_ilp,1_ilp:4_ilp)=[2332_ilp,2803_ilp,1696_ilp,1177_ilp]
           mm(91_ilp,1_ilp:4_ilp)=[2089_ilp,3185_ilp,1256_ilp,1901_ilp]
           mm(92_ilp,1_ilp:4_ilp)=[3780_ilp,184_ilp,3715_ilp,81_ilp]
           mm(93_ilp,1_ilp:4_ilp)=[1700_ilp,663_ilp,2077_ilp,1669_ilp]
           mm(94_ilp,1_ilp:4_ilp)=[3712_ilp,499_ilp,3019_ilp,2633_ilp]
           mm(95_ilp,1_ilp:4_ilp)=[150_ilp,3784_ilp,1497_ilp,2269_ilp]
           mm(96_ilp,1_ilp:4_ilp)=[2000_ilp,1631_ilp,1101_ilp,129_ilp]
           mm(97_ilp,1_ilp:4_ilp)=[3375_ilp,1925_ilp,717_ilp,1141_ilp]
           mm(98_ilp,1_ilp:4_ilp)=[1621_ilp,3912_ilp,51_ilp,249_ilp]
           mm(99_ilp,1_ilp:4_ilp)=[3090_ilp,1398_ilp,981_ilp,3917_ilp]
           mm(100_ilp,1_ilp:4_ilp)=[3765_ilp,1349_ilp,1978_ilp,2481_ilp]
           mm(101_ilp,1_ilp:4_ilp)=[1149_ilp,1441_ilp,1813_ilp,3941_ilp]
           mm(102_ilp,1_ilp:4_ilp)=[3146_ilp,2224_ilp,3881_ilp,2217_ilp]
           mm(103_ilp,1_ilp:4_ilp)=[33_ilp,2411_ilp,76_ilp,2749_ilp]
           mm(104_ilp,1_ilp:4_ilp)=[3082_ilp,1907_ilp,3846_ilp,3041_ilp]
           mm(105_ilp,1_ilp:4_ilp)=[2741_ilp,3192_ilp,3694_ilp,1877_ilp]
           mm(106_ilp,1_ilp:4_ilp)=[359_ilp,2786_ilp,1682_ilp,345_ilp]
           mm(107_ilp,1_ilp:4_ilp)=[3316_ilp,382_ilp,124_ilp,2861_ilp]
           mm(108_ilp,1_ilp:4_ilp)=[1749_ilp,37_ilp,1660_ilp,1809_ilp]
           mm(109_ilp,1_ilp:4_ilp)=[185_ilp,759_ilp,3997_ilp,3141_ilp]
           mm(110_ilp,1_ilp:4_ilp)=[2784_ilp,2948_ilp,479_ilp,2825_ilp]
           mm(111_ilp,1_ilp:4_ilp)=[2202_ilp,1862_ilp,1141_ilp,157_ilp]
           mm(112_ilp,1_ilp:4_ilp)=[2199_ilp,3802_ilp,886_ilp,2881_ilp]
           mm(113_ilp,1_ilp:4_ilp)=[1364_ilp,2423_ilp,3514_ilp,3637_ilp]
           mm(114_ilp,1_ilp:4_ilp)=[1244_ilp,2051_ilp,1301_ilp,1465_ilp]
           mm(115_ilp,1_ilp:4_ilp)=[2020_ilp,2295_ilp,3604_ilp,2829_ilp]
           mm(116_ilp,1_ilp:4_ilp)=[3160_ilp,1332_ilp,1888_ilp,2161_ilp]
           mm(117_ilp,1_ilp:4_ilp)=[2785_ilp,1832_ilp,1836_ilp,3365_ilp]
           mm(118_ilp,1_ilp:4_ilp)=[2772_ilp,2405_ilp,1990_ilp,361_ilp]
           mm(119_ilp,1_ilp:4_ilp)=[1217_ilp,3638_ilp,2058_ilp,2685_ilp]
           mm(120_ilp,1_ilp:4_ilp)=[1822_ilp,3661_ilp,692_ilp,3745_ilp]
           mm(121_ilp,1_ilp:4_ilp)=[1245_ilp,327_ilp,1194_ilp,2325_ilp]
           mm(122_ilp,1_ilp:4_ilp)=[2252_ilp,3660_ilp,20_ilp,3609_ilp]
           mm(123_ilp,1_ilp:4_ilp)=[3904_ilp,716_ilp,3285_ilp,3821_ilp]
           mm(124_ilp,1_ilp:4_ilp)=[2774_ilp,1842_ilp,2046_ilp,3537_ilp]
           mm(125_ilp,1_ilp:4_ilp)=[997_ilp,3987_ilp,2107_ilp,517_ilp]
           mm(126_ilp,1_ilp:4_ilp)=[2573_ilp,1368_ilp,3508_ilp,3017_ilp]
           mm(127_ilp,1_ilp:4_ilp)=[1148_ilp,1848_ilp,3525_ilp,2141_ilp]
           mm(128_ilp,1_ilp:4_ilp)=[545_ilp,2366_ilp,3801_ilp,1537_ilp]
           ! Executable Statements 
           i1 = iseed( 1_ilp )
           i2 = iseed( 2_ilp )
           i3 = iseed( 3_ilp )
           i4 = iseed( 4_ilp )
           loop_10: do i = 1, min( n, lv )
           20 continue
              ! multiply the seed by i-th power of the multiplier modulo 2**48
              it4 = i4*mm( i, 4_ilp )
              it3 = it4 / ipw2
              it4 = it4 - ipw2*it3
              it3 = it3 + i3*mm( i, 4_ilp ) + i4*mm( i, 3_ilp )
              it2 = it3 / ipw2
              it3 = it3 - ipw2*it2
              it2 = it2 + i2*mm( i, 4_ilp ) + i3*mm( i, 3_ilp ) + i4*mm( i, 2_ilp )
              it1 = it2 / ipw2
              it2 = it2 - ipw2*it1
              it1 = it1 + i1*mm( i, 4_ilp ) + i2*mm( i, 3_ilp ) + i3*mm( i, 2_ilp ) +i4*mm( i, 1_ilp )
              it1 = mod( it1, ipw2 )
              ! convert 48-bit integer to a realnumber in the interval (0,1,KIND=sp)
              x( i ) = r*( real( it1,KIND=sp)+r*( real( it2,KIND=sp)+r*( real( it3,KIND=sp)+&
                        r*real( it4,KIND=sp) ) ) )
              if (x( i )==one) then
                 ! if a real number has n bits of precision, and the first
                 ! n bits of the 48-bit integer above happen to be all 1 (which
                 ! will occur about once every 2**n calls), then x( i ) will
                 ! be rounded to exactly one. in ieee single precision arithmetic,
                 ! this will happen relatively often since n = 24.
                 ! since x( i ) is not supposed to return exactly 0.0_sp or one,
                 ! the statistically correct thing to do in this situation is
                 ! simply to iterate again.
                 ! n.b. the case x( i ) = 0.0_sp should not be possible.
                 i1 = i1 + 2_ilp
                 i2 = i2 + 2_ilp
                 i3 = i3 + 2_ilp
                 i4 = i4 + 2_ilp
                 goto 20
              end if
           end do loop_10
           ! return final value of seed
           iseed( 1_ilp ) = it1
           iseed( 2_ilp ) = it2
           iseed( 3_ilp ) = it3
           iseed( 4_ilp ) = it4
           return
     end subroutine stdlib_slaruv

     pure module subroutine stdlib_dlaruv( iseed, n, x )
     !! DLARUV returns a vector of n random real numbers from a uniform (0,1)
     !! distribution (n <= 128).
     !! This is an auxiliary routine called by DLARNV and ZLARNV.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: one
           ! Scalar Arguments 
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           integer(ilp), intent(inout) :: iseed(4_ilp)
           real(dp), intent(out) :: x(n)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: lv = 128_ilp
           integer(ilp), parameter :: ipw2 = 4096_ilp
           real(dp), parameter :: r = one/ipw2
           
           
           
           ! Local Scalars 
           integer(ilp) :: i, i1, i2, i3, i4, it1, it2, it3, it4
           ! Local Arrays 
           integer(ilp) :: mm(lv,4_ilp)
           ! Intrinsic Functions 
           ! Data Statements 
           mm(1_ilp,1_ilp:4_ilp)=[494_ilp,322_ilp,2508_ilp,2549_ilp]
           mm(2_ilp,1_ilp:4_ilp)=[2637_ilp,789_ilp,3754_ilp,1145_ilp]
           mm(3_ilp,1_ilp:4_ilp)=[255_ilp,1440_ilp,1766_ilp,2253_ilp]
           mm(4_ilp,1_ilp:4_ilp)=[2008_ilp,752_ilp,3572_ilp,305_ilp]
           mm(5_ilp,1_ilp:4_ilp)=[1253_ilp,2859_ilp,2893_ilp,3301_ilp]
           mm(6_ilp,1_ilp:4_ilp)=[3344_ilp,123_ilp,307_ilp,1065_ilp]
           mm(7_ilp,1_ilp:4_ilp)=[4084_ilp,1848_ilp,1297_ilp,3133_ilp]
           mm(8_ilp,1_ilp:4_ilp)=[1739_ilp,643_ilp,3966_ilp,2913_ilp]
           mm(9_ilp,1_ilp:4_ilp)=[3143_ilp,2405_ilp,758_ilp,3285_ilp]
           mm(10_ilp,1_ilp:4_ilp)=[3468_ilp,2638_ilp,2598_ilp,1241_ilp]
           mm(11_ilp,1_ilp:4_ilp)=[688_ilp,2344_ilp,3406_ilp,1197_ilp]
           mm(12_ilp,1_ilp:4_ilp)=[1657_ilp,46_ilp,2922_ilp,3729_ilp]
           mm(13_ilp,1_ilp:4_ilp)=[1238_ilp,3814_ilp,1038_ilp,2501_ilp]
           mm(14_ilp,1_ilp:4_ilp)=[3166_ilp,913_ilp,2934_ilp,1673_ilp]
           mm(15_ilp,1_ilp:4_ilp)=[1292_ilp,3649_ilp,2091_ilp,541_ilp]
           mm(16_ilp,1_ilp:4_ilp)=[3422_ilp,339_ilp,2451_ilp,2753_ilp]
           mm(17_ilp,1_ilp:4_ilp)=[1270_ilp,3808_ilp,1580_ilp,949_ilp]
           mm(18_ilp,1_ilp:4_ilp)=[2016_ilp,822_ilp,1958_ilp,2361_ilp]
           mm(19_ilp,1_ilp:4_ilp)=[154_ilp,2832_ilp,2055_ilp,1165_ilp]
           mm(20_ilp,1_ilp:4_ilp)=[2862_ilp,3078_ilp,1507_ilp,4081_ilp]
           mm(21_ilp,1_ilp:4_ilp)=[697_ilp,3633_ilp,1078_ilp,2725_ilp]
           mm(22_ilp,1_ilp:4_ilp)=[1706_ilp,2970_ilp,3273_ilp,3305_ilp]
           mm(23_ilp,1_ilp:4_ilp)=[491_ilp,637_ilp,17_ilp,3069_ilp]
           mm(24_ilp,1_ilp:4_ilp)=[931_ilp,2249_ilp,854_ilp,3617_ilp]
           mm(25_ilp,1_ilp:4_ilp)=[1444_ilp,2081_ilp,2916_ilp,3733_ilp]
           mm(26_ilp,1_ilp:4_ilp)=[444_ilp,4019_ilp,3971_ilp,409_ilp]
           mm(27_ilp,1_ilp:4_ilp)=[3577_ilp,1478_ilp,2889_ilp,2157_ilp]
           mm(28_ilp,1_ilp:4_ilp)=[3944_ilp,242_ilp,3831_ilp,1361_ilp]
           mm(29_ilp,1_ilp:4_ilp)=[2184_ilp,481_ilp,2621_ilp,3973_ilp]
           mm(30_ilp,1_ilp:4_ilp)=[1661_ilp,2075_ilp,1541_ilp,1865_ilp]
           mm(31_ilp,1_ilp:4_ilp)=[3482_ilp,4058_ilp,893_ilp,2525_ilp]
           mm(32_ilp,1_ilp:4_ilp)=[657_ilp,622_ilp,736_ilp,1409_ilp]
           mm(33_ilp,1_ilp:4_ilp)=[3023_ilp,3376_ilp,3992_ilp,3445_ilp]
           mm(34_ilp,1_ilp:4_ilp)=[3618_ilp,812_ilp,787_ilp,3577_ilp]
           mm(35_ilp,1_ilp:4_ilp)=[1267_ilp,234_ilp,2125_ilp,77_ilp]
           mm(36_ilp,1_ilp:4_ilp)=[1828_ilp,641_ilp,2364_ilp,3761_ilp]
           mm(37_ilp,1_ilp:4_ilp)=[164_ilp,4005_ilp,2460_ilp,2149_ilp]
           mm(38_ilp,1_ilp:4_ilp)=[3798_ilp,1122_ilp,257_ilp,1449_ilp]
           mm(39_ilp,1_ilp:4_ilp)=[3087_ilp,3135_ilp,1574_ilp,3005_ilp]
           mm(40_ilp,1_ilp:4_ilp)=[2400_ilp,2640_ilp,3912_ilp,225_ilp]
           mm(41_ilp,1_ilp:4_ilp)=[2870_ilp,2302_ilp,1216_ilp,85_ilp]
           mm(42_ilp,1_ilp:4_ilp)=[3876_ilp,40_ilp,3248_ilp,3673_ilp]
           mm(43_ilp,1_ilp:4_ilp)=[1905_ilp,1832_ilp,3401_ilp,3117_ilp]
           mm(44_ilp,1_ilp:4_ilp)=[1593_ilp,2247_ilp,2124_ilp,3089_ilp]
           mm(45_ilp,1_ilp:4_ilp)=[1797_ilp,2034_ilp,2762_ilp,1349_ilp]
           mm(46_ilp,1_ilp:4_ilp)=[1234_ilp,2637_ilp,149_ilp,2057_ilp]
           mm(47_ilp,1_ilp:4_ilp)=[3460_ilp,1287_ilp,2245_ilp,413_ilp]
           mm(48_ilp,1_ilp:4_ilp)=[328_ilp,1691_ilp,166_ilp,65_ilp]
           mm(49_ilp,1_ilp:4_ilp)=[2861_ilp,496_ilp,466_ilp,1845_ilp]
           mm(50_ilp,1_ilp:4_ilp)=[1950_ilp,1597_ilp,4018_ilp,697_ilp]
           mm(51_ilp,1_ilp:4_ilp)=[617_ilp,2394_ilp,1399_ilp,3085_ilp]
           mm(52_ilp,1_ilp:4_ilp)=[2070_ilp,2584_ilp,190_ilp,3441_ilp]
           mm(53_ilp,1_ilp:4_ilp)=[3331_ilp,1843_ilp,2879_ilp,1573_ilp]
           mm(54_ilp,1_ilp:4_ilp)=[769_ilp,336_ilp,153_ilp,3689_ilp]
           mm(55_ilp,1_ilp:4_ilp)=[1558_ilp,1472_ilp,2320_ilp,2941_ilp]
           mm(56_ilp,1_ilp:4_ilp)=[2412_ilp,2407_ilp,18_ilp,929_ilp]
           mm(57_ilp,1_ilp:4_ilp)=[2800_ilp,433_ilp,712_ilp,533_ilp]
           mm(58_ilp,1_ilp:4_ilp)=[189_ilp,2096_ilp,2159_ilp,2841_ilp]
           mm(59_ilp,1_ilp:4_ilp)=[287_ilp,1761_ilp,2318_ilp,4077_ilp]
           mm(60_ilp,1_ilp:4_ilp)=[2045_ilp,2810_ilp,2091_ilp,721_ilp]
           mm(61_ilp,1_ilp:4_ilp)=[1227_ilp,566_ilp,3443_ilp,2821_ilp]
           mm(62_ilp,1_ilp:4_ilp)=[2838_ilp,442_ilp,1510_ilp,2249_ilp]
           mm(63_ilp,1_ilp:4_ilp)=[209_ilp,41_ilp,449_ilp,2397_ilp]
           mm(64_ilp,1_ilp:4_ilp)=[2770_ilp,1238_ilp,1956_ilp,2817_ilp]
           mm(65_ilp,1_ilp:4_ilp)=[3654_ilp,1086_ilp,2201_ilp,245_ilp]
           mm(66_ilp,1_ilp:4_ilp)=[3993_ilp,603_ilp,3137_ilp,1913_ilp]
           mm(67_ilp,1_ilp:4_ilp)=[192_ilp,840_ilp,3399_ilp,1997_ilp]
           mm(68_ilp,1_ilp:4_ilp)=[2253_ilp,3168_ilp,1321_ilp,3121_ilp]
           mm(69_ilp,1_ilp:4_ilp)=[3491_ilp,1499_ilp,2271_ilp,997_ilp]
           mm(70_ilp,1_ilp:4_ilp)=[2889_ilp,1084_ilp,3667_ilp,1833_ilp]
           mm(71_ilp,1_ilp:4_ilp)=[2857_ilp,3438_ilp,2703_ilp,2877_ilp]
           mm(72_ilp,1_ilp:4_ilp)=[2094_ilp,2408_ilp,629_ilp,1633_ilp]
           mm(73_ilp,1_ilp:4_ilp)=[1818_ilp,1589_ilp,2365_ilp,981_ilp]
           mm(74_ilp,1_ilp:4_ilp)=[688_ilp,2391_ilp,2431_ilp,2009_ilp]
           mm(75_ilp,1_ilp:4_ilp)=[1407_ilp,288_ilp,1113_ilp,941_ilp]
           mm(76_ilp,1_ilp:4_ilp)=[634_ilp,26_ilp,3922_ilp,2449_ilp]
           mm(77_ilp,1_ilp:4_ilp)=[3231_ilp,512_ilp,2554_ilp,197_ilp]
           mm(78_ilp,1_ilp:4_ilp)=[815_ilp,1456_ilp,184_ilp,2441_ilp]
           mm(79_ilp,1_ilp:4_ilp)=[3524_ilp,171_ilp,2099_ilp,285_ilp]
           mm(80_ilp,1_ilp:4_ilp)=[1914_ilp,1677_ilp,3228_ilp,1473_ilp]
           mm(81_ilp,1_ilp:4_ilp)=[516_ilp,2657_ilp,4012_ilp,2741_ilp]
           mm(82_ilp,1_ilp:4_ilp)=[164_ilp,2270_ilp,1921_ilp,3129_ilp]
           mm(83_ilp,1_ilp:4_ilp)=[303_ilp,2587_ilp,3452_ilp,909_ilp]
           mm(84_ilp,1_ilp:4_ilp)=[2144_ilp,2961_ilp,3901_ilp,2801_ilp]
           mm(85_ilp,1_ilp:4_ilp)=[3480_ilp,1970_ilp,572_ilp,421_ilp]
           mm(86_ilp,1_ilp:4_ilp)=[119_ilp,1817_ilp,3309_ilp,4073_ilp]
           mm(87_ilp,1_ilp:4_ilp)=[3357_ilp,676_ilp,3171_ilp,2813_ilp]
           mm(88_ilp,1_ilp:4_ilp)=[837_ilp,1410_ilp,817_ilp,2337_ilp]
           mm(89_ilp,1_ilp:4_ilp)=[2826_ilp,3723_ilp,3039_ilp,1429_ilp]
           mm(90_ilp,1_ilp:4_ilp)=[2332_ilp,2803_ilp,1696_ilp,1177_ilp]
           mm(91_ilp,1_ilp:4_ilp)=[2089_ilp,3185_ilp,1256_ilp,1901_ilp]
           mm(92_ilp,1_ilp:4_ilp)=[3780_ilp,184_ilp,3715_ilp,81_ilp]
           mm(93_ilp,1_ilp:4_ilp)=[1700_ilp,663_ilp,2077_ilp,1669_ilp]
           mm(94_ilp,1_ilp:4_ilp)=[3712_ilp,499_ilp,3019_ilp,2633_ilp]
           mm(95_ilp,1_ilp:4_ilp)=[150_ilp,3784_ilp,1497_ilp,2269_ilp]
           mm(96_ilp,1_ilp:4_ilp)=[2000_ilp,1631_ilp,1101_ilp,129_ilp]
           mm(97_ilp,1_ilp:4_ilp)=[3375_ilp,1925_ilp,717_ilp,1141_ilp]
           mm(98_ilp,1_ilp:4_ilp)=[1621_ilp,3912_ilp,51_ilp,249_ilp]
           mm(99_ilp,1_ilp:4_ilp)=[3090_ilp,1398_ilp,981_ilp,3917_ilp]
           mm(100_ilp,1_ilp:4_ilp)=[3765_ilp,1349_ilp,1978_ilp,2481_ilp]
           mm(101_ilp,1_ilp:4_ilp)=[1149_ilp,1441_ilp,1813_ilp,3941_ilp]
           mm(102_ilp,1_ilp:4_ilp)=[3146_ilp,2224_ilp,3881_ilp,2217_ilp]
           mm(103_ilp,1_ilp:4_ilp)=[33_ilp,2411_ilp,76_ilp,2749_ilp]
           mm(104_ilp,1_ilp:4_ilp)=[3082_ilp,1907_ilp,3846_ilp,3041_ilp]
           mm(105_ilp,1_ilp:4_ilp)=[2741_ilp,3192_ilp,3694_ilp,1877_ilp]
           mm(106_ilp,1_ilp:4_ilp)=[359_ilp,2786_ilp,1682_ilp,345_ilp]
           mm(107_ilp,1_ilp:4_ilp)=[3316_ilp,382_ilp,124_ilp,2861_ilp]
           mm(108_ilp,1_ilp:4_ilp)=[1749_ilp,37_ilp,1660_ilp,1809_ilp]
           mm(109_ilp,1_ilp:4_ilp)=[185_ilp,759_ilp,3997_ilp,3141_ilp]
           mm(110_ilp,1_ilp:4_ilp)=[2784_ilp,2948_ilp,479_ilp,2825_ilp]
           mm(111_ilp,1_ilp:4_ilp)=[2202_ilp,1862_ilp,1141_ilp,157_ilp]
           mm(112_ilp,1_ilp:4_ilp)=[2199_ilp,3802_ilp,886_ilp,2881_ilp]
           mm(113_ilp,1_ilp:4_ilp)=[1364_ilp,2423_ilp,3514_ilp,3637_ilp]
           mm(114_ilp,1_ilp:4_ilp)=[1244_ilp,2051_ilp,1301_ilp,1465_ilp]
           mm(115_ilp,1_ilp:4_ilp)=[2020_ilp,2295_ilp,3604_ilp,2829_ilp]
           mm(116_ilp,1_ilp:4_ilp)=[3160_ilp,1332_ilp,1888_ilp,2161_ilp]
           mm(117_ilp,1_ilp:4_ilp)=[2785_ilp,1832_ilp,1836_ilp,3365_ilp]
           mm(118_ilp,1_ilp:4_ilp)=[2772_ilp,2405_ilp,1990_ilp,361_ilp]
           mm(119_ilp,1_ilp:4_ilp)=[1217_ilp,3638_ilp,2058_ilp,2685_ilp]
           mm(120_ilp,1_ilp:4_ilp)=[1822_ilp,3661_ilp,692_ilp,3745_ilp]
           mm(121_ilp,1_ilp:4_ilp)=[1245_ilp,327_ilp,1194_ilp,2325_ilp]
           mm(122_ilp,1_ilp:4_ilp)=[2252_ilp,3660_ilp,20_ilp,3609_ilp]
           mm(123_ilp,1_ilp:4_ilp)=[3904_ilp,716_ilp,3285_ilp,3821_ilp]
           mm(124_ilp,1_ilp:4_ilp)=[2774_ilp,1842_ilp,2046_ilp,3537_ilp]
           mm(125_ilp,1_ilp:4_ilp)=[997_ilp,3987_ilp,2107_ilp,517_ilp]
           mm(126_ilp,1_ilp:4_ilp)=[2573_ilp,1368_ilp,3508_ilp,3017_ilp]
           mm(127_ilp,1_ilp:4_ilp)=[1148_ilp,1848_ilp,3525_ilp,2141_ilp]
           mm(128_ilp,1_ilp:4_ilp)=[545_ilp,2366_ilp,3801_ilp,1537_ilp]
           ! Executable Statements 
           i1 = iseed( 1_ilp )
           i2 = iseed( 2_ilp )
           i3 = iseed( 3_ilp )
           i4 = iseed( 4_ilp )
           loop_10: do i = 1, min( n, lv )
           20 continue
              ! multiply the seed by i-th power of the multiplier modulo 2**48
              it4 = i4*mm( i, 4_ilp )
              it3 = it4 / ipw2
              it4 = it4 - ipw2*it3
              it3 = it3 + i3*mm( i, 4_ilp ) + i4*mm( i, 3_ilp )
              it2 = it3 / ipw2
              it3 = it3 - ipw2*it2
              it2 = it2 + i2*mm( i, 4_ilp ) + i3*mm( i, 3_ilp ) + i4*mm( i, 2_ilp )
              it1 = it2 / ipw2
              it2 = it2 - ipw2*it1
              it1 = it1 + i1*mm( i, 4_ilp ) + i2*mm( i, 3_ilp ) + i3*mm( i, 2_ilp ) +i4*mm( i, 1_ilp )
              it1 = mod( it1, ipw2 )
              ! convert 48-bit integer to a realnumber in the interval (0,1,KIND=dp)
              x( i ) = r*( real( it1,KIND=dp)+r*( real( it2,KIND=dp)+r*( real( it3,KIND=dp)+&
                        r*real( it4,KIND=dp) ) ) )
              if (x( i )==one) then
                 ! if a real number has n bits of precision, and the first
                 ! n bits of the 48-bit integer above happen to be all 1 (which
                 ! will occur about once every 2**n calls), then x( i ) will
                 ! be rounded to exactly one.
                 ! since x( i ) is not supposed to return exactly 0.0_dp or one,
                 ! the statistically correct thing to do in this situation is
                 ! simply to iterate again.
                 ! n.b. the case x( i ) = 0.0_dp should not be possible.
                 i1 = i1 + 2_ilp
                 i2 = i2 + 2_ilp
                 i3 = i3 + 2_ilp
                 i4 = i4 + 2_ilp
                 goto 20
              end if
           end do loop_10
           ! return final value of seed
           iseed( 1_ilp ) = it1
           iseed( 2_ilp ) = it2
           iseed( 3_ilp ) = it3
           iseed( 4_ilp ) = it4
           return
     end subroutine stdlib_dlaruv




     pure module subroutine stdlib_slacpy( uplo, m, n, a, lda, b, ldb )
     !! SLACPY copies all or part of a two-dimensional matrix A to another
     !! matrix B.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: lda, ldb, m, n
           ! Array Arguments 
           real(sp), intent(in) :: a(lda,*)
           real(sp), intent(out) :: b(ldb,*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, j
           ! Intrinsic Functions 
           ! Executable Statements 
           if( stdlib_lsame( uplo, 'U' ) ) then
              do j = 1, n
                 do i = 1, min( j, m )
                    b( i, j ) = a( i, j )
                 end do
              end do
           else if( stdlib_lsame( uplo, 'L' ) ) then
              do j = 1, n
                 do i = j, m
                    b( i, j ) = a( i, j )
                 end do
              end do
           else
              do j = 1, n
                 do i = 1, m
                    b( i, j ) = a( i, j )
                 end do
              end do
           end if
           return
     end subroutine stdlib_slacpy

     pure module subroutine stdlib_dlacpy( uplo, m, n, a, lda, b, ldb )
     !! DLACPY copies all or part of a two-dimensional matrix A to another
     !! matrix B.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: lda, ldb, m, n
           ! Array Arguments 
           real(dp), intent(in) :: a(lda,*)
           real(dp), intent(out) :: b(ldb,*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, j
           ! Intrinsic Functions 
           ! Executable Statements 
           if( stdlib_lsame( uplo, 'U' ) ) then
              do j = 1, n
                 do i = 1, min( j, m )
                    b( i, j ) = a( i, j )
                 end do
              end do
           else if( stdlib_lsame( uplo, 'L' ) ) then
              do j = 1, n
                 do i = j, m
                    b( i, j ) = a( i, j )
                 end do
              end do
           else
              do j = 1, n
                 do i = 1, m
                    b( i, j ) = a( i, j )
                 end do
              end do
           end if
           return
     end subroutine stdlib_dlacpy


     pure module subroutine stdlib_clacpy( uplo, m, n, a, lda, b, ldb )
     !! CLACPY copies all or part of a two-dimensional matrix A to another
     !! matrix B.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: lda, ldb, m, n
           ! Array Arguments 
           complex(sp), intent(in) :: a(lda,*)
           complex(sp), intent(out) :: b(ldb,*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, j
           ! Intrinsic Functions 
           ! Executable Statements 
           if( stdlib_lsame( uplo, 'U' ) ) then
              do j = 1, n
                 do i = 1, min( j, m )
                    b( i, j ) = a( i, j )
                 end do
              end do
           else if( stdlib_lsame( uplo, 'L' ) ) then
              do j = 1, n
                 do i = j, m
                    b( i, j ) = a( i, j )
                 end do
              end do
           else
              do j = 1, n
                 do i = 1, m
                    b( i, j ) = a( i, j )
                 end do
              end do
           end if
           return
     end subroutine stdlib_clacpy

     pure module subroutine stdlib_zlacpy( uplo, m, n, a, lda, b, ldb )
     !! ZLACPY copies all or part of a two-dimensional matrix A to another
     !! matrix B.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: lda, ldb, m, n
           ! Array Arguments 
           complex(dp), intent(in) :: a(lda,*)
           complex(dp), intent(out) :: b(ldb,*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, j
           ! Intrinsic Functions 
           ! Executable Statements 
           if( stdlib_lsame( uplo, 'U' ) ) then
              do j = 1, n
                 do i = 1, min( j, m )
                    b( i, j ) = a( i, j )
                 end do
              end do
           else if( stdlib_lsame( uplo, 'L' ) ) then
              do j = 1, n
                 do i = j, m
                    b( i, j ) = a( i, j )
                 end do
              end do
           else
              do j = 1, n
                 do i = 1, m
                    b( i, j ) = a( i, j )
                 end do
              end do
           end if
           return
     end subroutine stdlib_zlacpy




     pure module subroutine stdlib_clacp2( uplo, m, n, a, lda, b, ldb )
     !! CLACP2 copies all or part of a real two-dimensional matrix A to a
     !! complex matrix B.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: lda, ldb, m, n
           ! Array Arguments 
           real(sp), intent(in) :: a(lda,*)
           complex(sp), intent(out) :: b(ldb,*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, j
           ! Intrinsic Functions 
           ! Executable Statements 
           if( stdlib_lsame( uplo, 'U' ) ) then
              do j = 1, n
                 do i = 1, min( j, m )
                    b( i, j ) = a( i, j )
                 end do
              end do
           else if( stdlib_lsame( uplo, 'L' ) ) then
              do j = 1, n
                 do i = j, m
                    b( i, j ) = a( i, j )
                 end do
              end do
           else
              do j = 1, n
                 do i = 1, m
                    b( i, j ) = a( i, j )
                 end do
              end do
           end if
           return
     end subroutine stdlib_clacp2

     pure module subroutine stdlib_zlacp2( uplo, m, n, a, lda, b, ldb )
     !! ZLACP2 copies all or part of a real two-dimensional matrix A to a
     !! complex matrix B.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: lda, ldb, m, n
           ! Array Arguments 
           real(dp), intent(in) :: a(lda,*)
           complex(dp), intent(out) :: b(ldb,*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, j
           ! Intrinsic Functions 
           ! Executable Statements 
           if( stdlib_lsame( uplo, 'U' ) ) then
              do j = 1, n
                 do i = 1, min( j, m )
                    b( i, j ) = a( i, j )
                 end do
              end do
           else if( stdlib_lsame( uplo, 'L' ) ) then
              do j = 1, n
                 do i = j, m
                    b( i, j ) = a( i, j )
                 end do
              end do
           else
              do j = 1, n
                 do i = 1, m
                    b( i, j ) = a( i, j )
                 end do
              end do
           end if
           return
     end subroutine stdlib_zlacp2




     pure module subroutine stdlib_stfttp( transr, uplo, n, arf, ap, info )
     !! STFTTP copies a triangular matrix A from rectangular full packed
     !! format (TF) to standard packed format (TP).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character, intent(in) :: transr, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           real(sp), intent(out) :: ap(0_ilp:*)
           real(sp), intent(in) :: arf(0_ilp:*)
        ! =====================================================================
           ! Parameters 
           ! Local Scalars 
           logical(lk) :: lower, nisodd, normaltransr
           integer(ilp) :: n1, n2, k, nt
           integer(ilp) :: i, j, ij
           integer(ilp) :: ijp, jp, lda, js
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           normaltransr = stdlib_lsame( transr, 'N' )
           lower = stdlib_lsame( uplo, 'L' )
           if( .not.normaltransr .and. .not.stdlib_lsame( transr, 'T' ) ) then
              info = -1_ilp
           else if( .not.lower .and. .not.stdlib_lsame( uplo, 'U' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'STFTTP', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( n==1_ilp ) then
              if( normaltransr ) then
                 ap( 0_ilp ) = arf( 0_ilp )
              else
                 ap( 0_ilp ) = arf( 0_ilp )
              end if
              return
           end if
           ! size of array arf(0:nt-1)
           nt = n*( n+1 ) / 2_ilp
           ! set n1 and n2 depending on lower
           if( lower ) then
              n2 = n / 2_ilp
              n1 = n - n2
           else
              n1 = n / 2_ilp
              n2 = n - n1
           end if
           ! if n is odd, set nisodd = .true.
           ! if n is even, set k = n/2 and nisodd = .false.
           ! set lda of arf^c; arf^c is (0:(n+1)/2-1,0:n-noe)
           ! where noe = 0 if n is even, noe = 1 if n is odd
           if( mod( n, 2_ilp )==0_ilp ) then
              k = n / 2_ilp
              nisodd = .false.
              lda = n + 1_ilp
           else
              nisodd = .true.
              lda = n
           end if
           ! arf^c has lda rows and n+1-noe cols
           if( .not.normaltransr )lda = ( n+1 ) / 2_ilp
           ! start execution: there are eight cases
           if( nisodd ) then
              ! n is odd
              if( normaltransr ) then
                 ! n is odd and transr = 'n'
                 if( lower ) then
                   ! srpa for lower, normal and n is odd ( a(0:n-1,0:n1-1) )
                   ! t1 -> a(0,0), t2 -> a(0,1), s -> a(n1,0)
                   ! t1 -> a(0), t2 -> a(n), s -> a(n1); lda = n
                    ijp = 0_ilp
                    jp = 0_ilp
                    do j = 0, n2
                       do i = j, n - 1
                          ij = i + jp
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp
                       end do
                       jp = jp + lda
                    end do
                    do i = 0, n2 - 1
                       do j = 1 + i, n2
                          ij = i + j*lda
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp
                       end do
                    end do
                 else
                   ! srpa for upper, normal and n is odd ( a(0:n-1,0:n2-1)
                   ! t1 -> a(n1+1,0), t2 -> a(n1,0), s -> a(0,0)
                   ! t1 -> a(n2), t2 -> a(n1), s -> a(0)
                    ijp = 0_ilp
                    do j = 0, n1 - 1
                       ij = n2 + j
                       do i = 0, j
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp
                          ij = ij + lda
                       end do
                    end do
                    js = 0_ilp
                    do j = n1, n - 1
                       ij = js
                       do ij = js, js + j
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp
                       end do
                       js = js + lda
                    end do
                 end if
              else
                 ! n is odd and transr = 't'
                 if( lower ) then
                    ! srpa for lower, transpose and n is odd
                    ! t1 -> a(0,0) , t2 -> a(1,0) , s -> a(0,n1)
                    ! t1 -> a(0+0) , t2 -> a(1+0) , s -> a(0+n1*n1); lda=n1
                    ijp = 0_ilp
                    do i = 0, n2
                       do ij = i*( lda+1 ), n*lda - 1, lda
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp
                       end do
                    end do
                    js = 1_ilp
                    do j = 0, n2 - 1
                       do ij = js, js + n2 - j - 1
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp
                       end do
                       js = js + lda + 1_ilp
                    end do
                 else
                    ! srpa for upper, transpose and n is odd
                    ! t1 -> a(0,n1+1), t2 -> a(0,n1), s -> a(0,0)
                    ! t1 -> a(n2*n2), t2 -> a(n1*n2), s -> a(0); lda = n2
                    ijp = 0_ilp
                    js = n2*lda
                    do j = 0, n1 - 1
                       do ij = js, js + j
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp
                       end do
                       js = js + lda
                    end do
                    do i = 0, n1
                       do ij = i, i + ( n1+i )*lda, lda
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp
                       end do
                    end do
                 end if
              end if
           else
              ! n is even
              if( normaltransr ) then
                 ! n is even and transr = 'n'
                 if( lower ) then
                    ! srpa for lower, normal, and n is even ( a(0:n,0:k-1) )
                    ! t1 -> a(1,0), t2 -> a(0,0), s -> a(k+1,0)
                    ! t1 -> a(1), t2 -> a(0), s -> a(k+1)
                    ijp = 0_ilp
                    jp = 0_ilp
                    do j = 0, k - 1
                       do i = j, n - 1
                          ij = 1_ilp + i + jp
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp
                       end do
                       jp = jp + lda
                    end do
                    do i = 0, k - 1
                       do j = i, k - 1
                          ij = i + j*lda
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp
                       end do
                    end do
                 else
                    ! srpa for upper, normal, and n is even ( a(0:n,0:k-1) )
                    ! t1 -> a(k+1,0) ,  t2 -> a(k,0),   s -> a(0,0)
                    ! t1 -> a(k+1), t2 -> a(k), s -> a(0)
                    ijp = 0_ilp
                    do j = 0, k - 1
                       ij = k + 1_ilp + j
                       do i = 0, j
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp
                          ij = ij + lda
                       end do
                    end do
                    js = 0_ilp
                    do j = k, n - 1
                       ij = js
                       do ij = js, js + j
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp
                       end do
                       js = js + lda
                    end do
                 end if
              else
                 ! n is even and transr = 't'
                 if( lower ) then
                    ! srpa for lower, transpose and n is even (see paper)
                    ! t1 -> b(0,1), t2 -> b(0,0), s -> b(0,k+1)
                    ! t1 -> a(0+k), t2 -> a(0+0), s -> a(0+k*(k+1)); lda=k
                    ijp = 0_ilp
                    do i = 0, k - 1
                       do ij = i + ( i+1 )*lda, ( n+1 )*lda - 1, lda
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp
                       end do
                    end do
                    js = 0_ilp
                    do j = 0, k - 1
                       do ij = js, js + k - j - 1
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp
                       end do
                       js = js + lda + 1_ilp
                    end do
                 else
                    ! srpa for upper, transpose and n is even (see paper)
                    ! t1 -> b(0,k+1),     t2 -> b(0,k),   s -> b(0,0)
                    ! t1 -> a(0+k*(k+1)), t2 -> a(0+k*k), s -> a(0+0)); lda=k
                    ijp = 0_ilp
                    js = ( k+1 )*lda
                    do j = 0, k - 1
                       do ij = js, js + j
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp
                       end do
                       js = js + lda
                    end do
                    do i = 0, k - 1
                       do ij = i, i + ( k+i )*lda, lda
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp
                       end do
                    end do
                 end if
              end if
           end if
           return
     end subroutine stdlib_stfttp

     pure module subroutine stdlib_dtfttp( transr, uplo, n, arf, ap, info )
     !! DTFTTP copies a triangular matrix A from rectangular full packed
     !! format (TF) to standard packed format (TP).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character, intent(in) :: transr, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           real(dp), intent(out) :: ap(0_ilp:*)
           real(dp), intent(in) :: arf(0_ilp:*)
        ! =====================================================================
           ! Parameters 
           ! Local Scalars 
           logical(lk) :: lower, nisodd, normaltransr
           integer(ilp) :: n1, n2, k, nt
           integer(ilp) :: i, j, ij
           integer(ilp) :: ijp, jp, lda, js
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           normaltransr = stdlib_lsame( transr, 'N' )
           lower = stdlib_lsame( uplo, 'L' )
           if( .not.normaltransr .and. .not.stdlib_lsame( transr, 'T' ) ) then
              info = -1_ilp
           else if( .not.lower .and. .not.stdlib_lsame( uplo, 'U' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DTFTTP', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( n==1_ilp ) then
              if( normaltransr ) then
                 ap( 0_ilp ) = arf( 0_ilp )
              else
                 ap( 0_ilp ) = arf( 0_ilp )
              end if
              return
           end if
           ! size of array arf(0:nt-1)
           nt = n*( n+1 ) / 2_ilp
           ! set n1 and n2 depending on lower
           if( lower ) then
              n2 = n / 2_ilp
              n1 = n - n2
           else
              n1 = n / 2_ilp
              n2 = n - n1
           end if
           ! if n is odd, set nisodd = .true.
           ! if n is even, set k = n/2 and nisodd = .false.
           ! set lda of arf^c; arf^c is (0:(n+1)/2-1,0:n-noe)
           ! where noe = 0 if n is even, noe = 1 if n is odd
           if( mod( n, 2_ilp )==0_ilp ) then
              k = n / 2_ilp
              nisodd = .false.
              lda = n + 1_ilp
           else
              nisodd = .true.
              lda = n
           end if
           ! arf^c has lda rows and n+1-noe cols
           if( .not.normaltransr )lda = ( n+1 ) / 2_ilp
           ! start execution: there are eight cases
           if( nisodd ) then
              ! n is odd
              if( normaltransr ) then
                 ! n is odd and transr = 'n'
                 if( lower ) then
                   ! srpa for lower, normal and n is odd ( a(0:n-1,0:n1-1) )
                   ! t1 -> a(0,0), t2 -> a(0,1), s -> a(n1,0)
                   ! t1 -> a(0), t2 -> a(n), s -> a(n1); lda = n
                    ijp = 0_ilp
                    jp = 0_ilp
                    do j = 0, n2
                       do i = j, n - 1
                          ij = i + jp
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp
                       end do
                       jp = jp + lda
                    end do
                    do i = 0, n2 - 1
                       do j = 1 + i, n2
                          ij = i + j*lda
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp
                       end do
                    end do
                 else
                   ! srpa for upper, normal and n is odd ( a(0:n-1,0:n2-1)
                   ! t1 -> a(n1+1,0), t2 -> a(n1,0), s -> a(0,0)
                   ! t1 -> a(n2), t2 -> a(n1), s -> a(0)
                    ijp = 0_ilp
                    do j = 0, n1 - 1
                       ij = n2 + j
                       do i = 0, j
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp
                          ij = ij + lda
                       end do
                    end do
                    js = 0_ilp
                    do j = n1, n - 1
                       ij = js
                       do ij = js, js + j
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp
                       end do
                       js = js + lda
                    end do
                 end if
              else
                 ! n is odd and transr = 't'
                 if( lower ) then
                    ! srpa for lower, transpose and n is odd
                    ! t1 -> a(0,0) , t2 -> a(1,0) , s -> a(0,n1)
                    ! t1 -> a(0+0) , t2 -> a(1+0) , s -> a(0+n1*n1); lda=n1
                    ijp = 0_ilp
                    do i = 0, n2
                       do ij = i*( lda+1 ), n*lda - 1, lda
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp
                       end do
                    end do
                    js = 1_ilp
                    do j = 0, n2 - 1
                       do ij = js, js + n2 - j - 1
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp
                       end do
                       js = js + lda + 1_ilp
                    end do
                 else
                    ! srpa for upper, transpose and n is odd
                    ! t1 -> a(0,n1+1), t2 -> a(0,n1), s -> a(0,0)
                    ! t1 -> a(n2*n2), t2 -> a(n1*n2), s -> a(0); lda = n2
                    ijp = 0_ilp
                    js = n2*lda
                    do j = 0, n1 - 1
                       do ij = js, js + j
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp
                       end do
                       js = js + lda
                    end do
                    do i = 0, n1
                       do ij = i, i + ( n1+i )*lda, lda
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp
                       end do
                    end do
                 end if
              end if
           else
              ! n is even
              if( normaltransr ) then
                 ! n is even and transr = 'n'
                 if( lower ) then
                    ! srpa for lower, normal, and n is even ( a(0:n,0:k-1) )
                    ! t1 -> a(1,0), t2 -> a(0,0), s -> a(k+1,0)
                    ! t1 -> a(1), t2 -> a(0), s -> a(k+1)
                    ijp = 0_ilp
                    jp = 0_ilp
                    do j = 0, k - 1
                       do i = j, n - 1
                          ij = 1_ilp + i + jp
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp
                       end do
                       jp = jp + lda
                    end do
                    do i = 0, k - 1
                       do j = i, k - 1
                          ij = i + j*lda
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp
                       end do
                    end do
                 else
                    ! srpa for upper, normal, and n is even ( a(0:n,0:k-1) )
                    ! t1 -> a(k+1,0) ,  t2 -> a(k,0),   s -> a(0,0)
                    ! t1 -> a(k+1), t2 -> a(k), s -> a(0)
                    ijp = 0_ilp
                    do j = 0, k - 1
                       ij = k + 1_ilp + j
                       do i = 0, j
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp
                          ij = ij + lda
                       end do
                    end do
                    js = 0_ilp
                    do j = k, n - 1
                       ij = js
                       do ij = js, js + j
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp
                       end do
                       js = js + lda
                    end do
                 end if
              else
                 ! n is even and transr = 't'
                 if( lower ) then
                    ! srpa for lower, transpose and n is even (see paper)
                    ! t1 -> b(0,1), t2 -> b(0,0), s -> b(0,k+1)
                    ! t1 -> a(0+k), t2 -> a(0+0), s -> a(0+k*(k+1)); lda=k
                    ijp = 0_ilp
                    do i = 0, k - 1
                       do ij = i + ( i+1 )*lda, ( n+1 )*lda - 1, lda
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp
                       end do
                    end do
                    js = 0_ilp
                    do j = 0, k - 1
                       do ij = js, js + k - j - 1
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp
                       end do
                       js = js + lda + 1_ilp
                    end do
                 else
                    ! srpa for upper, transpose and n is even (see paper)
                    ! t1 -> b(0,k+1),     t2 -> b(0,k),   s -> b(0,0)
                    ! t1 -> a(0+k*(k+1)), t2 -> a(0+k*k), s -> a(0+0)); lda=k
                    ijp = 0_ilp
                    js = ( k+1 )*lda
                    do j = 0, k - 1
                       do ij = js, js + j
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp
                       end do
                       js = js + lda
                    end do
                    do i = 0, k - 1
                       do ij = i, i + ( k+i )*lda, lda
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp
                       end do
                    end do
                 end if
              end if
           end if
           return
     end subroutine stdlib_dtfttp


     pure module subroutine stdlib_ctfttp( transr, uplo, n, arf, ap, info )
     !! CTFTTP copies a triangular matrix A from rectangular full packed
     !! format (TF) to standard packed format (TP).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character, intent(in) :: transr, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           complex(sp), intent(out) :: ap(0_ilp:*)
           complex(sp), intent(in) :: arf(0_ilp:*)
        ! =====================================================================
           ! Parameters 
           ! Local Scalars 
           logical(lk) :: lower, nisodd, normaltransr
           integer(ilp) :: n1, n2, k, nt
           integer(ilp) :: i, j, ij
           integer(ilp) :: ijp, jp, lda, js
           ! Intrinsic Functions 
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           normaltransr = stdlib_lsame( transr, 'N' )
           lower = stdlib_lsame( uplo, 'L' )
           if( .not.normaltransr .and. .not.stdlib_lsame( transr, 'C' ) ) then
              info = -1_ilp
           else if( .not.lower .and. .not.stdlib_lsame( uplo, 'U' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CTFTTP', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( n==1_ilp ) then
              if( normaltransr ) then
                 ap( 0_ilp ) = arf( 0_ilp )
              else
                 ap( 0_ilp ) = conjg( arf( 0_ilp ) )
              end if
              return
           end if
           ! size of array arf(0:nt-1)
           nt = n*( n+1 ) / 2_ilp
           ! set n1 and n2 depending on lower
           if( lower ) then
              n2 = n / 2_ilp
              n1 = n - n2
           else
              n1 = n / 2_ilp
              n2 = n - n1
           end if
           ! if n is odd, set nisodd = .true.
           ! if n is even, set k = n/2 and nisodd = .false.
           ! set lda of arf^c; arf^c is (0:(n+1)/2-1,0:n-noe)
           ! where noe = 0 if n is even, noe = 1 if n is odd
           if( mod( n, 2_ilp )==0_ilp ) then
              k = n / 2_ilp
              nisodd = .false.
              lda = n + 1_ilp
           else
              nisodd = .true.
              lda = n
           end if
           ! arf^c has lda rows and n+1-noe cols
           if( .not.normaltransr )lda = ( n+1 ) / 2_ilp
           ! start execution: there are eight cases
           if( nisodd ) then
              ! n is odd
              if( normaltransr ) then
                 ! n is odd and transr = 'n'
                 if( lower ) then
                   ! srpa for lower, normal and n is odd ( a(0:n-1,0:n1-1) )
                   ! t1 -> a(0,0), t2 -> a(0,1), s -> a(n1,0)
                   ! t1 -> a(0), t2 -> a(n), s -> a(n1); lda = n
                    ijp = 0_ilp
                    jp = 0_ilp
                    do j = 0, n2
                       do i = j, n - 1
                          ij = i + jp
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp
                       end do
                       jp = jp + lda
                    end do
                    do i = 0, n2 - 1
                       do j = 1 + i, n2
                          ij = i + j*lda
                          ap( ijp ) = conjg( arf( ij ) )
                          ijp = ijp + 1_ilp
                       end do
                    end do
                 else
                   ! srpa for upper, normal and n is odd ( a(0:n-1,0:n2-1)
                   ! t1 -> a(n1+1,0), t2 -> a(n1,0), s -> a(0,0)
                   ! t1 -> a(n2), t2 -> a(n1), s -> a(0)
                    ijp = 0_ilp
                    do j = 0, n1 - 1
                       ij = n2 + j
                       do i = 0, j
                          ap( ijp ) = conjg( arf( ij ) )
                          ijp = ijp + 1_ilp
                          ij = ij + lda
                       end do
                    end do
                    js = 0_ilp
                    do j = n1, n - 1
                       ij = js
                       do ij = js, js + j
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp
                       end do
                       js = js + lda
                    end do
                 end if
              else
                 ! n is odd and transr = 'c'
                 if( lower ) then
                    ! srpa for lower, transpose and n is odd
                    ! t1 -> a(0,0) , t2 -> a(1,0) , s -> a(0,n1)
                    ! t1 -> a(0+0) , t2 -> a(1+0) , s -> a(0+n1*n1); lda=n1
                    ijp = 0_ilp
                    do i = 0, n2
                       do ij = i*( lda+1 ), n*lda - 1, lda
                          ap( ijp ) = conjg( arf( ij ) )
                          ijp = ijp + 1_ilp
                       end do
                    end do
                    js = 1_ilp
                    do j = 0, n2 - 1
                       do ij = js, js + n2 - j - 1
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp
                       end do
                       js = js + lda + 1_ilp
                    end do
                 else
                    ! srpa for upper, transpose and n is odd
                    ! t1 -> a(0,n1+1), t2 -> a(0,n1), s -> a(0,0)
                    ! t1 -> a(n2*n2), t2 -> a(n1*n2), s -> a(0); lda = n2
                    ijp = 0_ilp
                    js = n2*lda
                    do j = 0, n1 - 1
                       do ij = js, js + j
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp
                       end do
                       js = js + lda
                    end do
                    do i = 0, n1
                       do ij = i, i + ( n1+i )*lda, lda
                          ap( ijp ) = conjg( arf( ij ) )
                          ijp = ijp + 1_ilp
                       end do
                    end do
                 end if
              end if
           else
              ! n is even
              if( normaltransr ) then
                 ! n is even and transr = 'n'
                 if( lower ) then
                    ! srpa for lower, normal, and n is even ( a(0:n,0:k-1) )
                    ! t1 -> a(1,0), t2 -> a(0,0), s -> a(k+1,0)
                    ! t1 -> a(1), t2 -> a(0), s -> a(k+1)
                    ijp = 0_ilp
                    jp = 0_ilp
                    do j = 0, k - 1
                       do i = j, n - 1
                          ij = 1_ilp + i + jp
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp
                       end do
                       jp = jp + lda
                    end do
                    do i = 0, k - 1
                       do j = i, k - 1
                          ij = i + j*lda
                          ap( ijp ) = conjg( arf( ij ) )
                          ijp = ijp + 1_ilp
                       end do
                    end do
                 else
                    ! srpa for upper, normal, and n is even ( a(0:n,0:k-1) )
                    ! t1 -> a(k+1,0) ,  t2 -> a(k,0),   s -> a(0,0)
                    ! t1 -> a(k+1), t2 -> a(k), s -> a(0)
                    ijp = 0_ilp
                    do j = 0, k - 1
                       ij = k + 1_ilp + j
                       do i = 0, j
                          ap( ijp ) = conjg( arf( ij ) )
                          ijp = ijp + 1_ilp
                          ij = ij + lda
                       end do
                    end do
                    js = 0_ilp
                    do j = k, n - 1
                       ij = js
                       do ij = js, js + j
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp
                       end do
                       js = js + lda
                    end do
                 end if
              else
                 ! n is even and transr = 'c'
                 if( lower ) then
                    ! srpa for lower, transpose and n is even (see paper)
                    ! t1 -> b(0,1), t2 -> b(0,0), s -> b(0,k+1)
                    ! t1 -> a(0+k), t2 -> a(0+0), s -> a(0+k*(k+1)); lda=k
                    ijp = 0_ilp
                    do i = 0, k - 1
                       do ij = i + ( i+1 )*lda, ( n+1 )*lda - 1, lda
                          ap( ijp ) = conjg( arf( ij ) )
                          ijp = ijp + 1_ilp
                       end do
                    end do
                    js = 0_ilp
                    do j = 0, k - 1
                       do ij = js, js + k - j - 1
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp
                       end do
                       js = js + lda + 1_ilp
                    end do
                 else
                    ! srpa for upper, transpose and n is even (see paper)
                    ! t1 -> b(0,k+1),     t2 -> b(0,k),   s -> b(0,0)
                    ! t1 -> a(0+k*(k+1)), t2 -> a(0+k*k), s -> a(0+0)); lda=k
                    ijp = 0_ilp
                    js = ( k+1 )*lda
                    do j = 0, k - 1
                       do ij = js, js + j
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp
                       end do
                       js = js + lda
                    end do
                    do i = 0, k - 1
                       do ij = i, i + ( k+i )*lda, lda
                          ap( ijp ) = conjg( arf( ij ) )
                          ijp = ijp + 1_ilp
                       end do
                    end do
                 end if
              end if
           end if
           return
     end subroutine stdlib_ctfttp

     pure module subroutine stdlib_ztfttp( transr, uplo, n, arf, ap, info )
     !! ZTFTTP copies a triangular matrix A from rectangular full packed
     !! format (TF) to standard packed format (TP).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character, intent(in) :: transr, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           complex(dp), intent(out) :: ap(0_ilp:*)
           complex(dp), intent(in) :: arf(0_ilp:*)
        ! =====================================================================
           ! Parameters 
           ! Local Scalars 
           logical(lk) :: lower, nisodd, normaltransr
           integer(ilp) :: n1, n2, k, nt
           integer(ilp) :: i, j, ij
           integer(ilp) :: ijp, jp, lda, js
           ! Intrinsic Functions 
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           normaltransr = stdlib_lsame( transr, 'N' )
           lower = stdlib_lsame( uplo, 'L' )
           if( .not.normaltransr .and. .not.stdlib_lsame( transr, 'C' ) ) then
              info = -1_ilp
           else if( .not.lower .and. .not.stdlib_lsame( uplo, 'U' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZTFTTP', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( n==1_ilp ) then
              if( normaltransr ) then
                 ap( 0_ilp ) = arf( 0_ilp )
              else
                 ap( 0_ilp ) = conjg( arf( 0_ilp ) )
              end if
              return
           end if
           ! size of array arf(0:nt-1)
           nt = n*( n+1 ) / 2_ilp
           ! set n1 and n2 depending on lower
           if( lower ) then
              n2 = n / 2_ilp
              n1 = n - n2
           else
              n1 = n / 2_ilp
              n2 = n - n1
           end if
           ! if n is odd, set nisodd = .true.
           ! if n is even, set k = n/2 and nisodd = .false.
           ! set lda of arf^c; arf^c is (0:(n+1)/2-1,0:n-noe)
           ! where noe = 0 if n is even, noe = 1 if n is odd
           if( mod( n, 2_ilp )==0_ilp ) then
              k = n / 2_ilp
              nisodd = .false.
              lda = n + 1_ilp
           else
              nisodd = .true.
              lda = n
           end if
           ! arf^c has lda rows and n+1-noe cols
           if( .not.normaltransr )lda = ( n+1 ) / 2_ilp
           ! start execution: there are eight cases
           if( nisodd ) then
              ! n is odd
              if( normaltransr ) then
                 ! n is odd and transr = 'n'
                 if( lower ) then
                   ! srpa for lower, normal and n is odd ( a(0:n-1,0:n1-1) )
                   ! t1 -> a(0,0), t2 -> a(0,1), s -> a(n1,0)
                   ! t1 -> a(0), t2 -> a(n), s -> a(n1); lda = n
                    ijp = 0_ilp
                    jp = 0_ilp
                    do j = 0, n2
                       do i = j, n - 1
                          ij = i + jp
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp
                       end do
                       jp = jp + lda
                    end do
                    do i = 0, n2 - 1
                       do j = 1 + i, n2
                          ij = i + j*lda
                          ap( ijp ) = conjg( arf( ij ) )
                          ijp = ijp + 1_ilp
                       end do
                    end do
                 else
                   ! srpa for upper, normal and n is odd ( a(0:n-1,0:n2-1)
                   ! t1 -> a(n1+1,0), t2 -> a(n1,0), s -> a(0,0)
                   ! t1 -> a(n2), t2 -> a(n1), s -> a(0)
                    ijp = 0_ilp
                    do j = 0, n1 - 1
                       ij = n2 + j
                       do i = 0, j
                          ap( ijp ) = conjg( arf( ij ) )
                          ijp = ijp + 1_ilp
                          ij = ij + lda
                       end do
                    end do
                    js = 0_ilp
                    do j = n1, n - 1
                       ij = js
                       do ij = js, js + j
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp
                       end do
                       js = js + lda
                    end do
                 end if
              else
                 ! n is odd and transr = 'c'
                 if( lower ) then
                    ! srpa for lower, transpose and n is odd
                    ! t1 -> a(0,0) , t2 -> a(1,0) , s -> a(0,n1)
                    ! t1 -> a(0+0) , t2 -> a(1+0) , s -> a(0+n1*n1); lda=n1
                    ijp = 0_ilp
                    do i = 0, n2
                       do ij = i*( lda+1 ), n*lda - 1, lda
                          ap( ijp ) = conjg( arf( ij ) )
                          ijp = ijp + 1_ilp
                       end do
                    end do
                    js = 1_ilp
                    do j = 0, n2 - 1
                       do ij = js, js + n2 - j - 1
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp
                       end do
                       js = js + lda + 1_ilp
                    end do
                 else
                    ! srpa for upper, transpose and n is odd
                    ! t1 -> a(0,n1+1), t2 -> a(0,n1), s -> a(0,0)
                    ! t1 -> a(n2*n2), t2 -> a(n1*n2), s -> a(0); lda = n2
                    ijp = 0_ilp
                    js = n2*lda
                    do j = 0, n1 - 1
                       do ij = js, js + j
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp
                       end do
                       js = js + lda
                    end do
                    do i = 0, n1
                       do ij = i, i + ( n1+i )*lda, lda
                          ap( ijp ) = conjg( arf( ij ) )
                          ijp = ijp + 1_ilp
                       end do
                    end do
                 end if
              end if
           else
              ! n is even
              if( normaltransr ) then
                 ! n is even and transr = 'n'
                 if( lower ) then
                    ! srpa for lower, normal, and n is even ( a(0:n,0:k-1) )
                    ! t1 -> a(1,0), t2 -> a(0,0), s -> a(k+1,0)
                    ! t1 -> a(1), t2 -> a(0), s -> a(k+1)
                    ijp = 0_ilp
                    jp = 0_ilp
                    do j = 0, k - 1
                       do i = j, n - 1
                          ij = 1_ilp + i + jp
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp
                       end do
                       jp = jp + lda
                    end do
                    do i = 0, k - 1
                       do j = i, k - 1
                          ij = i + j*lda
                          ap( ijp ) = conjg( arf( ij ) )
                          ijp = ijp + 1_ilp
                       end do
                    end do
                 else
                    ! srpa for upper, normal, and n is even ( a(0:n,0:k-1) )
                    ! t1 -> a(k+1,0) ,  t2 -> a(k,0),   s -> a(0,0)
                    ! t1 -> a(k+1), t2 -> a(k), s -> a(0)
                    ijp = 0_ilp
                    do j = 0, k - 1
                       ij = k + 1_ilp + j
                       do i = 0, j
                          ap( ijp ) = conjg( arf( ij ) )
                          ijp = ijp + 1_ilp
                          ij = ij + lda
                       end do
                    end do
                    js = 0_ilp
                    do j = k, n - 1
                       ij = js
                       do ij = js, js + j
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp
                       end do
                       js = js + lda
                    end do
                 end if
              else
                 ! n is even and transr = 'c'
                 if( lower ) then
                    ! srpa for lower, transpose and n is even (see paper)
                    ! t1 -> b(0,1), t2 -> b(0,0), s -> b(0,k+1)
                    ! t1 -> a(0+k), t2 -> a(0+0), s -> a(0+k*(k+1)); lda=k
                    ijp = 0_ilp
                    do i = 0, k - 1
                       do ij = i + ( i+1 )*lda, ( n+1 )*lda - 1, lda
                          ap( ijp ) = conjg( arf( ij ) )
                          ijp = ijp + 1_ilp
                       end do
                    end do
                    js = 0_ilp
                    do j = 0, k - 1
                       do ij = js, js + k - j - 1
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp
                       end do
                       js = js + lda + 1_ilp
                    end do
                 else
                    ! srpa for upper, transpose and n is even (see paper)
                    ! t1 -> b(0,k+1),     t2 -> b(0,k),   s -> b(0,0)
                    ! t1 -> a(0+k*(k+1)), t2 -> a(0+k*k), s -> a(0+0)); lda=k
                    ijp = 0_ilp
                    js = ( k+1 )*lda
                    do j = 0, k - 1
                       do ij = js, js + j
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp
                       end do
                       js = js + lda
                    end do
                    do i = 0, k - 1
                       do ij = i, i + ( k+i )*lda, lda
                          ap( ijp ) = conjg( arf( ij ) )
                          ijp = ijp + 1_ilp
                       end do
                    end do
                 end if
              end if
           end if
           return
     end subroutine stdlib_ztfttp




     pure module subroutine stdlib_stfttr( transr, uplo, n, arf, a, lda, info )
     !! STFTTR copies a triangular matrix A from rectangular full packed
     !! format (TF) to standard full format (TR).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character, intent(in) :: transr, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n, lda
           ! Array Arguments 
           real(sp), intent(out) :: a(0_ilp:lda-1,0_ilp:*)
           real(sp), intent(in) :: arf(0_ilp:*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lower, nisodd, normaltransr
           integer(ilp) :: n1, n2, k, nt, nx2, np1x2
           integer(ilp) :: i, j, l, ij
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           normaltransr = stdlib_lsame( transr, 'N' )
           lower = stdlib_lsame( uplo, 'L' )
           if( .not.normaltransr .and. .not.stdlib_lsame( transr, 'T' ) ) then
              info = -1_ilp
           else if( .not.lower .and. .not.stdlib_lsame( uplo, 'U' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'STFTTR', -info )
              return
           end if
           ! quick return if possible
           if( n<=1_ilp ) then
              if( n==1_ilp ) then
                 a( 0_ilp, 0_ilp ) = arf( 0_ilp )
              end if
              return
           end if
           ! size of array arf(0:nt-1)
           nt = n*( n+1 ) / 2_ilp
           ! set n1 and n2 depending on lower: for n even n1=n2=k
           if( lower ) then
              n2 = n / 2_ilp
              n1 = n - n2
           else
              n1 = n / 2_ilp
              n2 = n - n1
           end if
           ! if n is odd, set nisodd = .true., lda=n+1 and a is (n+1)--by--k2.
           ! if n is even, set k = n/2 and nisodd = .false., lda=n and a is
           ! n--by--(n+1)/2.
           if( mod( n, 2_ilp )==0_ilp ) then
              k = n / 2_ilp
              nisodd = .false.
              if( .not.lower )np1x2 = n + n + 2_ilp
           else
              nisodd = .true.
              if( .not.lower )nx2 = n + n
           end if
           if( nisodd ) then
              ! n is odd
              if( normaltransr ) then
                 ! n is odd and transr = 'n'
                 if( lower ) then
                    ! n is odd, transr = 'n', and uplo = 'l'
                    ij = 0_ilp
                    do j = 0, n2
                       do i = n1, n2 + j
                          a( n2+j, i ) = arf( ij )
                          ij = ij + 1_ilp
                       end do
                       do i = j, n - 1
                          a( i, j ) = arf( ij )
                          ij = ij + 1_ilp
                       end do
                    end do
                 else
                    ! n is odd, transr = 'n', and uplo = 'u'
                    ij = nt - n
                    do j = n - 1, n1, -1
                       do i = 0, j
                          a( i, j ) = arf( ij )
                          ij = ij + 1_ilp
                       end do
                       do l = j - n1, n1 - 1
                          a( j-n1, l ) = arf( ij )
                          ij = ij + 1_ilp
                       end do
                       ij = ij - nx2
                    end do
                 end if
              else
                 ! n is odd and transr = 't'
                 if( lower ) then
                    ! n is odd, transr = 't', and uplo = 'l'
                    ij = 0_ilp
                    do j = 0, n2 - 1
                       do i = 0, j
                          a( j, i ) = arf( ij )
                          ij = ij + 1_ilp
                       end do
                       do i = n1 + j, n - 1
                          a( i, n1+j ) = arf( ij )
                          ij = ij + 1_ilp
                       end do
                    end do
                    do j = n2, n - 1
                       do i = 0, n1 - 1
                          a( j, i ) = arf( ij )
                          ij = ij + 1_ilp
                       end do
                    end do
                 else
                    ! n is odd, transr = 't', and uplo = 'u'
                    ij = 0_ilp
                    do j = 0, n1
                       do i = n1, n - 1
                          a( j, i ) = arf( ij )
                          ij = ij + 1_ilp
                       end do
                    end do
                    do j = 0, n1 - 1
                       do i = 0, j
                          a( i, j ) = arf( ij )
                          ij = ij + 1_ilp
                       end do
                       do l = n2 + j, n - 1
                          a( n2+j, l ) = arf( ij )
                          ij = ij + 1_ilp
                       end do
                    end do
                 end if
              end if
           else
              ! n is even
              if( normaltransr ) then
                 ! n is even and transr = 'n'
                 if( lower ) then
                    ! n is even, transr = 'n', and uplo = 'l'
                    ij = 0_ilp
                    do j = 0, k - 1
                       do i = k, k + j
                          a( k+j, i ) = arf( ij )
                          ij = ij + 1_ilp
                       end do
                       do i = j, n - 1
                          a( i, j ) = arf( ij )
                          ij = ij + 1_ilp
                       end do
                    end do
                 else
                    ! n is even, transr = 'n', and uplo = 'u'
                    ij = nt - n - 1_ilp
                    do j = n - 1, k, -1
                       do i = 0, j
                          a( i, j ) = arf( ij )
                          ij = ij + 1_ilp
                       end do
                       do l = j - k, k - 1
                          a( j-k, l ) = arf( ij )
                          ij = ij + 1_ilp
                       end do
                       ij = ij - np1x2
                    end do
                 end if
              else
                 ! n is even and transr = 't'
                 if( lower ) then
                    ! n is even, transr = 't', and uplo = 'l'
                    ij = 0_ilp
                    j = k
                    do i = k, n - 1
                       a( i, j ) = arf( ij )
                       ij = ij + 1_ilp
                    end do
                    do j = 0, k - 2
                       do i = 0, j
                          a( j, i ) = arf( ij )
                          ij = ij + 1_ilp
                       end do
                       do i = k + 1 + j, n - 1
                          a( i, k+1+j ) = arf( ij )
                          ij = ij + 1_ilp
                       end do
                    end do
                    do j = k - 1, n - 1
                       do i = 0, k - 1
                          a( j, i ) = arf( ij )
                          ij = ij + 1_ilp
                       end do
                    end do
                 else
                    ! n is even, transr = 't', and uplo = 'u'
                    ij = 0_ilp
                    do j = 0, k
                       do i = k, n - 1
                          a( j, i ) = arf( ij )
                          ij = ij + 1_ilp
                       end do
                    end do
                    do j = 0, k - 2
                       do i = 0, j
                          a( i, j ) = arf( ij )
                          ij = ij + 1_ilp
                       end do
                       do l = k + 1 + j, n - 1
                          a( k+1+j, l ) = arf( ij )
                          ij = ij + 1_ilp
                       end do
                    end do
                    ! note that here, on exit of the loop, j = k-1
                    do i = 0, j
                       a( i, j ) = arf( ij )
                       ij = ij + 1_ilp
                    end do
                 end if
              end if
           end if
           return
     end subroutine stdlib_stfttr

     pure module subroutine stdlib_dtfttr( transr, uplo, n, arf, a, lda, info )
     !! DTFTTR copies a triangular matrix A from rectangular full packed
     !! format (TF) to standard full format (TR).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character, intent(in) :: transr, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n, lda
           ! Array Arguments 
           real(dp), intent(out) :: a(0_ilp:lda-1,0_ilp:*)
           real(dp), intent(in) :: arf(0_ilp:*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lower, nisodd, normaltransr
           integer(ilp) :: n1, n2, k, nt, nx2, np1x2
           integer(ilp) :: i, j, l, ij
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           normaltransr = stdlib_lsame( transr, 'N' )
           lower = stdlib_lsame( uplo, 'L' )
           if( .not.normaltransr .and. .not.stdlib_lsame( transr, 'T' ) ) then
              info = -1_ilp
           else if( .not.lower .and. .not.stdlib_lsame( uplo, 'U' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DTFTTR', -info )
              return
           end if
           ! quick return if possible
           if( n<=1_ilp ) then
              if( n==1_ilp ) then
                 a( 0_ilp, 0_ilp ) = arf( 0_ilp )
              end if
              return
           end if
           ! size of array arf(0:nt-1)
           nt = n*( n+1 ) / 2_ilp
           ! set n1 and n2 depending on lower: for n even n1=n2=k
           if( lower ) then
              n2 = n / 2_ilp
              n1 = n - n2
           else
              n1 = n / 2_ilp
              n2 = n - n1
           end if
           ! if n is odd, set nisodd = .true., lda=n+1 and a is (n+1)--by--k2.
           ! if n is even, set k = n/2 and nisodd = .false., lda=n and a is
           ! n--by--(n+1)/2.
           if( mod( n, 2_ilp )==0_ilp ) then
              k = n / 2_ilp
              nisodd = .false.
              if( .not.lower )np1x2 = n + n + 2_ilp
           else
              nisodd = .true.
              if( .not.lower )nx2 = n + n
           end if
           if( nisodd ) then
              ! n is odd
              if( normaltransr ) then
                 ! n is odd and transr = 'n'
                 if( lower ) then
                    ! n is odd, transr = 'n', and uplo = 'l'
                    ij = 0_ilp
                    do j = 0, n2
                       do i = n1, n2 + j
                          a( n2+j, i ) = arf( ij )
                          ij = ij + 1_ilp
                       end do
                       do i = j, n - 1
                          a( i, j ) = arf( ij )
                          ij = ij + 1_ilp
                       end do
                    end do
                 else
                    ! n is odd, transr = 'n', and uplo = 'u'
                    ij = nt - n
                    do j = n - 1, n1, -1
                       do i = 0, j
                          a( i, j ) = arf( ij )
                          ij = ij + 1_ilp
                       end do
                       do l = j - n1, n1 - 1
                          a( j-n1, l ) = arf( ij )
                          ij = ij + 1_ilp
                       end do
                       ij = ij - nx2
                    end do
                 end if
              else
                 ! n is odd and transr = 't'
                 if( lower ) then
                    ! n is odd, transr = 't', and uplo = 'l'
                    ij = 0_ilp
                    do j = 0, n2 - 1
                       do i = 0, j
                          a( j, i ) = arf( ij )
                          ij = ij + 1_ilp
                       end do
                       do i = n1 + j, n - 1
                          a( i, n1+j ) = arf( ij )
                          ij = ij + 1_ilp
                       end do
                    end do
                    do j = n2, n - 1
                       do i = 0, n1 - 1
                          a( j, i ) = arf( ij )
                          ij = ij + 1_ilp
                       end do
                    end do
                 else
                    ! n is odd, transr = 't', and uplo = 'u'
                    ij = 0_ilp
                    do j = 0, n1
                       do i = n1, n - 1
                          a( j, i ) = arf( ij )
                          ij = ij + 1_ilp
                       end do
                    end do
                    do j = 0, n1 - 1
                       do i = 0, j
                          a( i, j ) = arf( ij )
                          ij = ij + 1_ilp
                       end do
                       do l = n2 + j, n - 1
                          a( n2+j, l ) = arf( ij )
                          ij = ij + 1_ilp
                       end do
                    end do
                 end if
              end if
           else
              ! n is even
              if( normaltransr ) then
                 ! n is even and transr = 'n'
                 if( lower ) then
                    ! n is even, transr = 'n', and uplo = 'l'
                    ij = 0_ilp
                    do j = 0, k - 1
                       do i = k, k + j
                          a( k+j, i ) = arf( ij )
                          ij = ij + 1_ilp
                       end do
                       do i = j, n - 1
                          a( i, j ) = arf( ij )
                          ij = ij + 1_ilp
                       end do
                    end do
                 else
                    ! n is even, transr = 'n', and uplo = 'u'
                    ij = nt - n - 1_ilp
                    do j = n - 1, k, -1
                       do i = 0, j
                          a( i, j ) = arf( ij )
                          ij = ij + 1_ilp
                       end do
                       do l = j - k, k - 1
                          a( j-k, l ) = arf( ij )
                          ij = ij + 1_ilp
                       end do
                       ij = ij - np1x2
                    end do
                 end if
              else
                 ! n is even and transr = 't'
                 if( lower ) then
                    ! n is even, transr = 't', and uplo = 'l'
                    ij = 0_ilp
                    j = k
                    do i = k, n - 1
                       a( i, j ) = arf( ij )
                       ij = ij + 1_ilp
                    end do
                    do j = 0, k - 2
                       do i = 0, j
                          a( j, i ) = arf( ij )
                          ij = ij + 1_ilp
                       end do
                       do i = k + 1 + j, n - 1
                          a( i, k+1+j ) = arf( ij )
                          ij = ij + 1_ilp
                       end do
                    end do
                    do j = k - 1, n - 1
                       do i = 0, k - 1
                          a( j, i ) = arf( ij )
                          ij = ij + 1_ilp
                       end do
                    end do
                 else
                    ! n is even, transr = 't', and uplo = 'u'
                    ij = 0_ilp
                    do j = 0, k
                       do i = k, n - 1
                          a( j, i ) = arf( ij )
                          ij = ij + 1_ilp
                       end do
                    end do
                    do j = 0, k - 2
                       do i = 0, j
                          a( i, j ) = arf( ij )
                          ij = ij + 1_ilp
                       end do
                       do l = k + 1 + j, n - 1
                          a( k+1+j, l ) = arf( ij )
                          ij = ij + 1_ilp
                       end do
                    end do
                    ! note that here, on exit of the loop, j = k-1
                    do i = 0, j
                       a( i, j ) = arf( ij )
                       ij = ij + 1_ilp
                    end do
                 end if
              end if
           end if
           return
     end subroutine stdlib_dtfttr


     pure module subroutine stdlib_ctfttr( transr, uplo, n, arf, a, lda, info )
     !! CTFTTR copies a triangular matrix A from rectangular full packed
     !! format (TF) to standard full format (TR).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character, intent(in) :: transr, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n, lda
           ! Array Arguments 
           complex(sp), intent(out) :: a(0_ilp:lda-1,0_ilp:*)
           complex(sp), intent(in) :: arf(0_ilp:*)
        ! =====================================================================
           ! Parameters 
           ! Local Scalars 
           logical(lk) :: lower, nisodd, normaltransr
           integer(ilp) :: n1, n2, k, nt, nx2, np1x2
           integer(ilp) :: i, j, l, ij
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           normaltransr = stdlib_lsame( transr, 'N' )
           lower = stdlib_lsame( uplo, 'L' )
           if( .not.normaltransr .and. .not.stdlib_lsame( transr, 'C' ) ) then
              info = -1_ilp
           else if( .not.lower .and. .not.stdlib_lsame( uplo, 'U' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CTFTTR', -info )
              return
           end if
           ! quick return if possible
           if( n<=1_ilp ) then
              if( n==1_ilp ) then
                 if( normaltransr ) then
                    a( 0_ilp, 0_ilp ) = arf( 0_ilp )
                 else
                    a( 0_ilp, 0_ilp ) = conjg( arf( 0_ilp ) )
                 end if
              end if
              return
           end if
           ! size of array arf(1:2,0:nt-1)
           nt = n*( n+1 ) / 2_ilp
           ! set n1 and n2 depending on lower: for n even n1=n2=k
           if( lower ) then
              n2 = n / 2_ilp
              n1 = n - n2
           else
              n1 = n / 2_ilp
              n2 = n - n1
           end if
           ! if n is odd, set nisodd = .true., lda=n+1 and a is (n+1)--by--k2.
           ! if n is even, set k = n/2 and nisodd = .false., lda=n and a is
           ! n--by--(n+1)/2.
           if( mod( n, 2_ilp )==0_ilp ) then
              k = n / 2_ilp
              nisodd = .false.
              if( .not.lower )np1x2 = n + n + 2_ilp
           else
              nisodd = .true.
              if( .not.lower )nx2 = n + n
           end if
           if( nisodd ) then
              ! n is odd
              if( normaltransr ) then
                 ! n is odd and transr = 'n'
                 if( lower ) then
                   ! srpa for lower, normal and n is odd ( a(0:n-1,0:n1-1) )
                   ! t1 -> a(0,0), t2 -> a(0,1), s -> a(n1,0)
                   ! t1 -> a(0), t2 -> a(n), s -> a(n1); lda=n
                    ij = 0_ilp
                    do j = 0, n2
                       do i = n1, n2 + j
                          a( n2+j, i ) = conjg( arf( ij ) )
                          ij = ij + 1_ilp
                       end do
                       do i = j, n - 1
                          a( i, j ) = arf( ij )
                          ij = ij + 1_ilp
                       end do
                    end do
                 else
                   ! srpa for upper, normal and n is odd ( a(0:n-1,0:n2-1)
                   ! t1 -> a(n1+1,0), t2 -> a(n1,0), s -> a(0,0)
                   ! t1 -> a(n2), t2 -> a(n1), s -> a(0); lda=n
                    ij = nt - n
                    do j = n - 1, n1, -1
                       do i = 0, j
                          a( i, j ) = arf( ij )
                          ij = ij + 1_ilp
                       end do
                       do l = j - n1, n1 - 1
                          a( j-n1, l ) = conjg( arf( ij ) )
                          ij = ij + 1_ilp
                       end do
                       ij = ij - nx2
                    end do
                 end if
              else
                 ! n is odd and transr = 'c'
                 if( lower ) then
                    ! srpa for lower, transpose and n is odd
                    ! t1 -> a(0,0) , t2 -> a(1,0) , s -> a(0,n1)
                    ! t1 -> a(0+0) , t2 -> a(1+0) , s -> a(0+n1*n1); lda=n1
                    ij = 0_ilp
                    do j = 0, n2 - 1
                       do i = 0, j
                          a( j, i ) = conjg( arf( ij ) )
                          ij = ij + 1_ilp
                       end do
                       do i = n1 + j, n - 1
                          a( i, n1+j ) = arf( ij )
                          ij = ij + 1_ilp
                       end do
                    end do
                    do j = n2, n - 1
                       do i = 0, n1 - 1
                          a( j, i ) = conjg( arf( ij ) )
                          ij = ij + 1_ilp
                       end do
                    end do
                 else
                    ! srpa for upper, transpose and n is odd
                    ! t1 -> a(0,n1+1), t2 -> a(0,n1), s -> a(0,0)
                    ! t1 -> a(n2*n2), t2 -> a(n1*n2), s -> a(0); lda = n2
                    ij = 0_ilp
                    do j = 0, n1
                       do i = n1, n - 1
                          a( j, i ) = conjg( arf( ij ) )
                          ij = ij + 1_ilp
                       end do
                    end do
                    do j = 0, n1 - 1
                       do i = 0, j
                          a( i, j ) = arf( ij )
                          ij = ij + 1_ilp
                       end do
                       do l = n2 + j, n - 1
                          a( n2+j, l ) = conjg( arf( ij ) )
                          ij = ij + 1_ilp
                       end do
                    end do
                 end if
              end if
           else
              ! n is even
              if( normaltransr ) then
                 ! n is even and transr = 'n'
                 if( lower ) then
                    ! srpa for lower, normal, and n is even ( a(0:n,0:k-1) )
                    ! t1 -> a(1,0), t2 -> a(0,0), s -> a(k+1,0)
                    ! t1 -> a(1), t2 -> a(0), s -> a(k+1); lda=n+1
                    ij = 0_ilp
                    do j = 0, k - 1
                       do i = k, k + j
                          a( k+j, i ) = conjg( arf( ij ) )
                          ij = ij + 1_ilp
                       end do
                       do i = j, n - 1
                          a( i, j ) = arf( ij )
                          ij = ij + 1_ilp
                       end do
                    end do
                 else
                    ! srpa for upper, normal, and n is even ( a(0:n,0:k-1) )
                    ! t1 -> a(k+1,0) ,  t2 -> a(k,0),   s -> a(0,0)
                    ! t1 -> a(k+1), t2 -> a(k), s -> a(0); lda=n+1
                    ij = nt - n - 1_ilp
                    do j = n - 1, k, -1
                       do i = 0, j
                          a( i, j ) = arf( ij )
                          ij = ij + 1_ilp
                       end do
                       do l = j - k, k - 1
                          a( j-k, l ) = conjg( arf( ij ) )
                          ij = ij + 1_ilp
                       end do
                       ij = ij - np1x2
                    end do
                 end if
              else
                 ! n is even and transr = 'c'
                 if( lower ) then
                    ! srpa for lower, transpose and n is even (see paper, a=b)
                    ! t1 -> a(0,1) , t2 -> a(0,0) , s -> a(0,k+1) :
                    ! t1 -> a(0+k) , t2 -> a(0+0) , s -> a(0+k*(k+1)); lda=k
                    ij = 0_ilp
                    j = k
                    do i = k, n - 1
                       a( i, j ) = arf( ij )
                       ij = ij + 1_ilp
                    end do
                    do j = 0, k - 2
                       do i = 0, j
                          a( j, i ) = conjg( arf( ij ) )
                          ij = ij + 1_ilp
                       end do
                       do i = k + 1 + j, n - 1
                          a( i, k+1+j ) = arf( ij )
                          ij = ij + 1_ilp
                       end do
                    end do
                    do j = k - 1, n - 1
                       do i = 0, k - 1
                          a( j, i ) = conjg( arf( ij ) )
                          ij = ij + 1_ilp
                       end do
                    end do
                 else
                    ! srpa for upper, transpose and n is even (see paper, a=b)
                    ! t1 -> a(0,k+1) , t2 -> a(0,k) , s -> a(0,0)
                    ! t1 -> a(0+k*(k+1)) , t2 -> a(0+k*k) , s -> a(0+0)); lda=k
                    ij = 0_ilp
                    do j = 0, k
                       do i = k, n - 1
                          a( j, i ) = conjg( arf( ij ) )
                          ij = ij + 1_ilp
                       end do
                    end do
                    do j = 0, k - 2
                       do i = 0, j
                          a( i, j ) = arf( ij )
                          ij = ij + 1_ilp
                       end do
                       do l = k + 1 + j, n - 1
                          a( k+1+j, l ) = conjg( arf( ij ) )
                          ij = ij + 1_ilp
                       end do
                    end do
                    ! note that here j = k-1
                    do i = 0, j
                       a( i, j ) = arf( ij )
                       ij = ij + 1_ilp
                    end do
                 end if
              end if
           end if
           return
     end subroutine stdlib_ctfttr

     pure module subroutine stdlib_ztfttr( transr, uplo, n, arf, a, lda, info )
     !! ZTFTTR copies a triangular matrix A from rectangular full packed
     !! format (TF) to standard full format (TR).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character, intent(in) :: transr, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n, lda
           ! Array Arguments 
           complex(dp), intent(out) :: a(0_ilp:lda-1,0_ilp:*)
           complex(dp), intent(in) :: arf(0_ilp:*)
        ! =====================================================================
           ! Parameters 
           ! Local Scalars 
           logical(lk) :: lower, nisodd, normaltransr
           integer(ilp) :: n1, n2, k, nt, nx2, np1x2
           integer(ilp) :: i, j, l, ij
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           normaltransr = stdlib_lsame( transr, 'N' )
           lower = stdlib_lsame( uplo, 'L' )
           if( .not.normaltransr .and. .not.stdlib_lsame( transr, 'C' ) ) then
              info = -1_ilp
           else if( .not.lower .and. .not.stdlib_lsame( uplo, 'U' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZTFTTR', -info )
              return
           end if
           ! quick return if possible
           if( n<=1_ilp ) then
              if( n==1_ilp ) then
                 if( normaltransr ) then
                    a( 0_ilp, 0_ilp ) = arf( 0_ilp )
                 else
                    a( 0_ilp, 0_ilp ) = conjg( arf( 0_ilp ) )
                 end if
              end if
              return
           end if
           ! size of array arf(1:2,0:nt-1)
           nt = n*( n+1 ) / 2_ilp
           ! set n1 and n2 depending on lower: for n even n1=n2=k
           if( lower ) then
              n2 = n / 2_ilp
              n1 = n - n2
           else
              n1 = n / 2_ilp
              n2 = n - n1
           end if
           ! if n is odd, set nisodd = .true., lda=n+1 and a is (n+1)--by--k2.
           ! if n is even, set k = n/2 and nisodd = .false., lda=n and a is
           ! n--by--(n+1)/2.
           if( mod( n, 2_ilp )==0_ilp ) then
              k = n / 2_ilp
              nisodd = .false.
              if( .not.lower )np1x2 = n + n + 2_ilp
           else
              nisodd = .true.
              if( .not.lower )nx2 = n + n
           end if
           if( nisodd ) then
              ! n is odd
              if( normaltransr ) then
                 ! n is odd and transr = 'n'
                 if( lower ) then
                   ! srpa for lower, normal and n is odd ( a(0:n-1,0:n1-1) )
                   ! t1 -> a(0,0), t2 -> a(0,1), s -> a(n1,0)
                   ! t1 -> a(0), t2 -> a(n), s -> a(n1); lda=n
                    ij = 0_ilp
                    do j = 0, n2
                       do i = n1, n2 + j
                          a( n2+j, i ) = conjg( arf( ij ) )
                          ij = ij + 1_ilp
                       end do
                       do i = j, n - 1
                          a( i, j ) = arf( ij )
                          ij = ij + 1_ilp
                       end do
                    end do
                 else
                   ! srpa for upper, normal and n is odd ( a(0:n-1,0:n2-1)
                   ! t1 -> a(n1+1,0), t2 -> a(n1,0), s -> a(0,0)
                   ! t1 -> a(n2), t2 -> a(n1), s -> a(0); lda=n
                    ij = nt - n
                    do j = n - 1, n1, -1
                       do i = 0, j
                          a( i, j ) = arf( ij )
                          ij = ij + 1_ilp
                       end do
                       do l = j - n1, n1 - 1
                          a( j-n1, l ) = conjg( arf( ij ) )
                          ij = ij + 1_ilp
                       end do
                       ij = ij - nx2
                    end do
                 end if
              else
                 ! n is odd and transr = 'c'
                 if( lower ) then
                    ! srpa for lower, transpose and n is odd
                    ! t1 -> a(0,0) , t2 -> a(1,0) , s -> a(0,n1)
                    ! t1 -> a(0+0) , t2 -> a(1+0) , s -> a(0+n1*n1); lda=n1
                    ij = 0_ilp
                    do j = 0, n2 - 1
                       do i = 0, j
                          a( j, i ) = conjg( arf( ij ) )
                          ij = ij + 1_ilp
                       end do
                       do i = n1 + j, n - 1
                          a( i, n1+j ) = arf( ij )
                          ij = ij + 1_ilp
                       end do
                    end do
                    do j = n2, n - 1
                       do i = 0, n1 - 1
                          a( j, i ) = conjg( arf( ij ) )
                          ij = ij + 1_ilp
                       end do
                    end do
                 else
                    ! srpa for upper, transpose and n is odd
                    ! t1 -> a(0,n1+1), t2 -> a(0,n1), s -> a(0,0)
                    ! t1 -> a(n2*n2), t2 -> a(n1*n2), s -> a(0); lda = n2
                    ij = 0_ilp
                    do j = 0, n1
                       do i = n1, n - 1
                          a( j, i ) = conjg( arf( ij ) )
                          ij = ij + 1_ilp
                       end do
                    end do
                    do j = 0, n1 - 1
                       do i = 0, j
                          a( i, j ) = arf( ij )
                          ij = ij + 1_ilp
                       end do
                       do l = n2 + j, n - 1
                          a( n2+j, l ) = conjg( arf( ij ) )
                          ij = ij + 1_ilp
                       end do
                    end do
                 end if
              end if
           else
              ! n is even
              if( normaltransr ) then
                 ! n is even and transr = 'n'
                 if( lower ) then
                    ! srpa for lower, normal, and n is even ( a(0:n,0:k-1) )
                    ! t1 -> a(1,0), t2 -> a(0,0), s -> a(k+1,0)
                    ! t1 -> a(1), t2 -> a(0), s -> a(k+1); lda=n+1
                    ij = 0_ilp
                    do j = 0, k - 1
                       do i = k, k + j
                          a( k+j, i ) = conjg( arf( ij ) )
                          ij = ij + 1_ilp
                       end do
                       do i = j, n - 1
                          a( i, j ) = arf( ij )
                          ij = ij + 1_ilp
                       end do
                    end do
                 else
                    ! srpa for upper, normal, and n is even ( a(0:n,0:k-1) )
                    ! t1 -> a(k+1,0) ,  t2 -> a(k,0),   s -> a(0,0)
                    ! t1 -> a(k+1), t2 -> a(k), s -> a(0); lda=n+1
                    ij = nt - n - 1_ilp
                    do j = n - 1, k, -1
                       do i = 0, j
                          a( i, j ) = arf( ij )
                          ij = ij + 1_ilp
                       end do
                       do l = j - k, k - 1
                          a( j-k, l ) = conjg( arf( ij ) )
                          ij = ij + 1_ilp
                       end do
                       ij = ij - np1x2
                    end do
                 end if
              else
                 ! n is even and transr = 'c'
                 if( lower ) then
                    ! srpa for lower, transpose and n is even (see paper, a=b)
                    ! t1 -> a(0,1) , t2 -> a(0,0) , s -> a(0,k+1) :
                    ! t1 -> a(0+k) , t2 -> a(0+0) , s -> a(0+k*(k+1)); lda=k
                    ij = 0_ilp
                    j = k
                    do i = k, n - 1
                       a( i, j ) = arf( ij )
                       ij = ij + 1_ilp
                    end do
                    do j = 0, k - 2
                       do i = 0, j
                          a( j, i ) = conjg( arf( ij ) )
                          ij = ij + 1_ilp
                       end do
                       do i = k + 1 + j, n - 1
                          a( i, k+1+j ) = arf( ij )
                          ij = ij + 1_ilp
                       end do
                    end do
                    do j = k - 1, n - 1
                       do i = 0, k - 1
                          a( j, i ) = conjg( arf( ij ) )
                          ij = ij + 1_ilp
                       end do
                    end do
                 else
                    ! srpa for upper, transpose and n is even (see paper, a=b)
                    ! t1 -> a(0,k+1) , t2 -> a(0,k) , s -> a(0,0)
                    ! t1 -> a(0+k*(k+1)) , t2 -> a(0+k*k) , s -> a(0+0)); lda=k
                    ij = 0_ilp
                    do j = 0, k
                       do i = k, n - 1
                          a( j, i ) = conjg( arf( ij ) )
                          ij = ij + 1_ilp
                       end do
                    end do
                    do j = 0, k - 2
                       do i = 0, j
                          a( i, j ) = arf( ij )
                          ij = ij + 1_ilp
                       end do
                       do l = k + 1 + j, n - 1
                          a( k+1+j, l ) = conjg( arf( ij ) )
                          ij = ij + 1_ilp
                       end do
                    end do
                    ! note that here j = k-1
                    do i = 0, j
                       a( i, j ) = arf( ij )
                       ij = ij + 1_ilp
                    end do
                 end if
              end if
           end if
           return
     end subroutine stdlib_ztfttr




     pure module subroutine stdlib_stpttf( transr, uplo, n, ap, arf, info )
     !! STPTTF copies a triangular matrix A from standard packed format (TP)
     !! to rectangular full packed format (TF).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character, intent(in) :: transr, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           real(sp), intent(in) :: ap(0_ilp:*)
           real(sp), intent(out) :: arf(0_ilp:*)
        ! =====================================================================
           ! Parameters 
           ! Local Scalars 
           logical(lk) :: lower, nisodd, normaltransr
           integer(ilp) :: n1, n2, k, nt
           integer(ilp) :: i, j, ij
           integer(ilp) :: ijp, jp, lda, js
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           normaltransr = stdlib_lsame( transr, 'N' )
           lower = stdlib_lsame( uplo, 'L' )
           if( .not.normaltransr .and. .not.stdlib_lsame( transr, 'T' ) ) then
              info = -1_ilp
           else if( .not.lower .and. .not.stdlib_lsame( uplo, 'U' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'STPTTF', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( n==1_ilp ) then
              if( normaltransr ) then
                 arf( 0_ilp ) = ap( 0_ilp )
              else
                 arf( 0_ilp ) = ap( 0_ilp )
              end if
              return
           end if
           ! size of array arf(0:nt-1)
           nt = n*( n+1 ) / 2_ilp
           ! set n1 and n2 depending on lower
           if( lower ) then
              n2 = n / 2_ilp
              n1 = n - n2
           else
              n1 = n / 2_ilp
              n2 = n - n1
           end if
           ! if n is odd, set nisodd = .true.
           ! if n is even, set k = n/2 and nisodd = .false.
           ! set lda of arf^c; arf^c is (0:(n+1)/2-1,0:n-noe)
           ! where noe = 0 if n is even, noe = 1 if n is odd
           if( mod( n, 2_ilp )==0_ilp ) then
              k = n / 2_ilp
              nisodd = .false.
              lda = n + 1_ilp
           else
              nisodd = .true.
              lda = n
           end if
           ! arf^c has lda rows and n+1-noe cols
           if( .not.normaltransr )lda = ( n+1 ) / 2_ilp
           ! start execution: there are eight cases
           if( nisodd ) then
              ! n is odd
              if( normaltransr ) then
                 ! n is odd and transr = 'n'
                 if( lower ) then
                    ! n is odd, transr = 'n', and uplo = 'l'
                    ijp = 0_ilp
                    jp = 0_ilp
                    do j = 0, n2
                       do i = j, n - 1
                          ij = i + jp
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp
                       end do
                       jp = jp + lda
                    end do
                    do i = 0, n2 - 1
                       do j = 1 + i, n2
                          ij = i + j*lda
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp
                       end do
                    end do
                 else
                    ! n is odd, transr = 'n', and uplo = 'u'
                    ijp = 0_ilp
                    do j = 0, n1 - 1
                       ij = n2 + j
                       do i = 0, j
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp
                          ij = ij + lda
                       end do
                    end do
                    js = 0_ilp
                    do j = n1, n - 1
                       ij = js
                       do ij = js, js + j
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp
                       end do
                       js = js + lda
                    end do
                 end if
              else
                 ! n is odd and transr = 't'
                 if( lower ) then
                    ! n is odd, transr = 't', and uplo = 'l'
                    ijp = 0_ilp
                    do i = 0, n2
                       do ij = i*( lda+1 ), n*lda - 1, lda
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp
                       end do
                    end do
                    js = 1_ilp
                    do j = 0, n2 - 1
                       do ij = js, js + n2 - j - 1
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp
                       end do
                       js = js + lda + 1_ilp
                    end do
                 else
                    ! n is odd, transr = 't', and uplo = 'u'
                    ijp = 0_ilp
                    js = n2*lda
                    do j = 0, n1 - 1
                       do ij = js, js + j
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp
                       end do
                       js = js + lda
                    end do
                    do i = 0, n1
                       do ij = i, i + ( n1+i )*lda, lda
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp
                       end do
                    end do
                 end if
              end if
           else
              ! n is even
              if( normaltransr ) then
                 ! n is even and transr = 'n'
                 if( lower ) then
                    ! n is even, transr = 'n', and uplo = 'l'
                    ijp = 0_ilp
                    jp = 0_ilp
                    do j = 0, k - 1
                       do i = j, n - 1
                          ij = 1_ilp + i + jp
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp
                       end do
                       jp = jp + lda
                    end do
                    do i = 0, k - 1
                       do j = i, k - 1
                          ij = i + j*lda
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp
                       end do
                    end do
                 else
                    ! n is even, transr = 'n', and uplo = 'u'
                    ijp = 0_ilp
                    do j = 0, k - 1
                       ij = k + 1_ilp + j
                       do i = 0, j
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp
                          ij = ij + lda
                       end do
                    end do
                    js = 0_ilp
                    do j = k, n - 1
                       ij = js
                       do ij = js, js + j
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp
                       end do
                       js = js + lda
                    end do
                 end if
              else
                 ! n is even and transr = 't'
                 if( lower ) then
                    ! n is even, transr = 't', and uplo = 'l'
                    ijp = 0_ilp
                    do i = 0, k - 1
                       do ij = i + ( i+1 )*lda, ( n+1 )*lda - 1, lda
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp
                       end do
                    end do
                    js = 0_ilp
                    do j = 0, k - 1
                       do ij = js, js + k - j - 1
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp
                       end do
                       js = js + lda + 1_ilp
                    end do
                 else
                    ! n is even, transr = 't', and uplo = 'u'
                    ijp = 0_ilp
                    js = ( k+1 )*lda
                    do j = 0, k - 1
                       do ij = js, js + j
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp
                       end do
                       js = js + lda
                    end do
                    do i = 0, k - 1
                       do ij = i, i + ( k+i )*lda, lda
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp
                       end do
                    end do
                 end if
              end if
           end if
           return
     end subroutine stdlib_stpttf

     pure module subroutine stdlib_dtpttf( transr, uplo, n, ap, arf, info )
     !! DTPTTF copies a triangular matrix A from standard packed format (TP)
     !! to rectangular full packed format (TF).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character, intent(in) :: transr, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           real(dp), intent(in) :: ap(0_ilp:*)
           real(dp), intent(out) :: arf(0_ilp:*)
        ! =====================================================================
           ! Parameters 
           ! Local Scalars 
           logical(lk) :: lower, nisodd, normaltransr
           integer(ilp) :: n1, n2, k, nt
           integer(ilp) :: i, j, ij
           integer(ilp) :: ijp, jp, lda, js
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           normaltransr = stdlib_lsame( transr, 'N' )
           lower = stdlib_lsame( uplo, 'L' )
           if( .not.normaltransr .and. .not.stdlib_lsame( transr, 'T' ) ) then
              info = -1_ilp
           else if( .not.lower .and. .not.stdlib_lsame( uplo, 'U' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DTPTTF', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( n==1_ilp ) then
              if( normaltransr ) then
                 arf( 0_ilp ) = ap( 0_ilp )
              else
                 arf( 0_ilp ) = ap( 0_ilp )
              end if
              return
           end if
           ! size of array arf(0:nt-1)
           nt = n*( n+1 ) / 2_ilp
           ! set n1 and n2 depending on lower
           if( lower ) then
              n2 = n / 2_ilp
              n1 = n - n2
           else
              n1 = n / 2_ilp
              n2 = n - n1
           end if
           ! if n is odd, set nisodd = .true.
           ! if n is even, set k = n/2 and nisodd = .false.
           ! set lda of arf^c; arf^c is (0:(n+1)/2-1,0:n-noe)
           ! where noe = 0 if n is even, noe = 1 if n is odd
           if( mod( n, 2_ilp )==0_ilp ) then
              k = n / 2_ilp
              nisodd = .false.
              lda = n + 1_ilp
           else
              nisodd = .true.
              lda = n
           end if
           ! arf^c has lda rows and n+1-noe cols
           if( .not.normaltransr )lda = ( n+1 ) / 2_ilp
           ! start execution: there are eight cases
           if( nisodd ) then
              ! n is odd
              if( normaltransr ) then
                 ! n is odd and transr = 'n'
                 if( lower ) then
                    ! n is odd, transr = 'n', and uplo = 'l'
                    ijp = 0_ilp
                    jp = 0_ilp
                    do j = 0, n2
                       do i = j, n - 1
                          ij = i + jp
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp
                       end do
                       jp = jp + lda
                    end do
                    do i = 0, n2 - 1
                       do j = 1 + i, n2
                          ij = i + j*lda
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp
                       end do
                    end do
                 else
                    ! n is odd, transr = 'n', and uplo = 'u'
                    ijp = 0_ilp
                    do j = 0, n1 - 1
                       ij = n2 + j
                       do i = 0, j
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp
                          ij = ij + lda
                       end do
                    end do
                    js = 0_ilp
                    do j = n1, n - 1
                       ij = js
                       do ij = js, js + j
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp
                       end do
                       js = js + lda
                    end do
                 end if
              else
                 ! n is odd and transr = 't'
                 if( lower ) then
                    ! n is odd, transr = 't', and uplo = 'l'
                    ijp = 0_ilp
                    do i = 0, n2
                       do ij = i*( lda+1 ), n*lda - 1, lda
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp
                       end do
                    end do
                    js = 1_ilp
                    do j = 0, n2 - 1
                       do ij = js, js + n2 - j - 1
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp
                       end do
                       js = js + lda + 1_ilp
                    end do
                 else
                    ! n is odd, transr = 't', and uplo = 'u'
                    ijp = 0_ilp
                    js = n2*lda
                    do j = 0, n1 - 1
                       do ij = js, js + j
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp
                       end do
                       js = js + lda
                    end do
                    do i = 0, n1
                       do ij = i, i + ( n1+i )*lda, lda
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp
                       end do
                    end do
                 end if
              end if
           else
              ! n is even
              if( normaltransr ) then
                 ! n is even and transr = 'n'
                 if( lower ) then
                    ! n is even, transr = 'n', and uplo = 'l'
                    ijp = 0_ilp
                    jp = 0_ilp
                    do j = 0, k - 1
                       do i = j, n - 1
                          ij = 1_ilp + i + jp
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp
                       end do
                       jp = jp + lda
                    end do
                    do i = 0, k - 1
                       do j = i, k - 1
                          ij = i + j*lda
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp
                       end do
                    end do
                 else
                    ! n is even, transr = 'n', and uplo = 'u'
                    ijp = 0_ilp
                    do j = 0, k - 1
                       ij = k + 1_ilp + j
                       do i = 0, j
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp
                          ij = ij + lda
                       end do
                    end do
                    js = 0_ilp
                    do j = k, n - 1
                       ij = js
                       do ij = js, js + j
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp
                       end do
                       js = js + lda
                    end do
                 end if
              else
                 ! n is even and transr = 't'
                 if( lower ) then
                    ! n is even, transr = 't', and uplo = 'l'
                    ijp = 0_ilp
                    do i = 0, k - 1
                       do ij = i + ( i+1 )*lda, ( n+1 )*lda - 1, lda
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp
                       end do
                    end do
                    js = 0_ilp
                    do j = 0, k - 1
                       do ij = js, js + k - j - 1
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp
                       end do
                       js = js + lda + 1_ilp
                    end do
                 else
                    ! n is even, transr = 't', and uplo = 'u'
                    ijp = 0_ilp
                    js = ( k+1 )*lda
                    do j = 0, k - 1
                       do ij = js, js + j
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp
                       end do
                       js = js + lda
                    end do
                    do i = 0, k - 1
                       do ij = i, i + ( k+i )*lda, lda
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp
                       end do
                    end do
                 end if
              end if
           end if
           return
     end subroutine stdlib_dtpttf


     pure module subroutine stdlib_ctpttf( transr, uplo, n, ap, arf, info )
     !! CTPTTF copies a triangular matrix A from standard packed format (TP)
     !! to rectangular full packed format (TF).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character, intent(in) :: transr, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           complex(sp), intent(in) :: ap(0_ilp:*)
           complex(sp), intent(out) :: arf(0_ilp:*)
        ! =====================================================================
           ! Parameters 
           ! Local Scalars 
           logical(lk) :: lower, nisodd, normaltransr
           integer(ilp) :: n1, n2, k, nt
           integer(ilp) :: i, j, ij
           integer(ilp) :: ijp, jp, lda, js
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           normaltransr = stdlib_lsame( transr, 'N' )
           lower = stdlib_lsame( uplo, 'L' )
           if( .not.normaltransr .and. .not.stdlib_lsame( transr, 'C' ) ) then
              info = -1_ilp
           else if( .not.lower .and. .not.stdlib_lsame( uplo, 'U' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CTPTTF', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( n==1_ilp ) then
              if( normaltransr ) then
                 arf( 0_ilp ) = ap( 0_ilp )
              else
                 arf( 0_ilp ) = conjg( ap( 0_ilp ) )
              end if
              return
           end if
           ! size of array arf(0:nt-1)
           nt = n*( n+1 ) / 2_ilp
           ! set n1 and n2 depending on lower
           if( lower ) then
              n2 = n / 2_ilp
              n1 = n - n2
           else
              n1 = n / 2_ilp
              n2 = n - n1
           end if
           ! if n is odd, set nisodd = .true.
           ! if n is even, set k = n/2 and nisodd = .false.
           ! set lda of arf^c; arf^c is (0:(n+1)/2-1,0:n-noe)
           ! where noe = 0 if n is even, noe = 1 if n is odd
           if( mod( n, 2_ilp )==0_ilp ) then
              k = n / 2_ilp
              nisodd = .false.
              lda = n + 1_ilp
           else
              nisodd = .true.
              lda = n
           end if
           ! arf^c has lda rows and n+1-noe cols
           if( .not.normaltransr )lda = ( n+1 ) / 2_ilp
           ! start execution: there are eight cases
           if( nisodd ) then
              ! n is odd
              if( normaltransr ) then
                 ! n is odd and transr = 'n'
                 if( lower ) then
                   ! srpa for lower, normal and n is odd ( a(0:n-1,0:n1-1) )
                   ! t1 -> a(0,0), t2 -> a(0,1), s -> a(n1,0)
                   ! t1 -> a(0), t2 -> a(n), s -> a(n1); lda = n
                    ijp = 0_ilp
                    jp = 0_ilp
                    do j = 0, n2
                       do i = j, n - 1
                          ij = i + jp
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp
                       end do
                       jp = jp + lda
                    end do
                    do i = 0, n2 - 1
                       do j = 1 + i, n2
                          ij = i + j*lda
                          arf( ij ) = conjg( ap( ijp ) )
                          ijp = ijp + 1_ilp
                       end do
                    end do
                 else
                   ! srpa for upper, normal and n is odd ( a(0:n-1,0:n2-1)
                   ! t1 -> a(n1+1,0), t2 -> a(n1,0), s -> a(0,0)
                   ! t1 -> a(n2), t2 -> a(n1), s -> a(0)
                    ijp = 0_ilp
                    do j = 0, n1 - 1
                       ij = n2 + j
                       do i = 0, j
                          arf( ij ) = conjg( ap( ijp ) )
                          ijp = ijp + 1_ilp
                          ij = ij + lda
                       end do
                    end do
                    js = 0_ilp
                    do j = n1, n - 1
                       ij = js
                       do ij = js, js + j
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp
                       end do
                       js = js + lda
                    end do
                 end if
              else
                 ! n is odd and transr = 'c'
                 if( lower ) then
                    ! srpa for lower, transpose and n is odd
                    ! t1 -> a(0,0) , t2 -> a(1,0) , s -> a(0,n1)
                    ! t1 -> a(0+0) , t2 -> a(1+0) , s -> a(0+n1*n1); lda=n1
                    ijp = 0_ilp
                    do i = 0, n2
                       do ij = i*( lda+1 ), n*lda - 1, lda
                          arf( ij ) = conjg( ap( ijp ) )
                          ijp = ijp + 1_ilp
                       end do
                    end do
                    js = 1_ilp
                    do j = 0, n2 - 1
                       do ij = js, js + n2 - j - 1
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp
                       end do
                       js = js + lda + 1_ilp
                    end do
                 else
                    ! srpa for upper, transpose and n is odd
                    ! t1 -> a(0,n1+1), t2 -> a(0,n1), s -> a(0,0)
                    ! t1 -> a(n2*n2), t2 -> a(n1*n2), s -> a(0); lda = n2
                    ijp = 0_ilp
                    js = n2*lda
                    do j = 0, n1 - 1
                       do ij = js, js + j
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp
                       end do
                       js = js + lda
                    end do
                    do i = 0, n1
                       do ij = i, i + ( n1+i )*lda, lda
                          arf( ij ) = conjg( ap( ijp ) )
                          ijp = ijp + 1_ilp
                       end do
                    end do
                 end if
              end if
           else
              ! n is even
              if( normaltransr ) then
                 ! n is even and transr = 'n'
                 if( lower ) then
                    ! srpa for lower, normal, and n is even ( a(0:n,0:k-1) )
                    ! t1 -> a(1,0), t2 -> a(0,0), s -> a(k+1,0)
                    ! t1 -> a(1), t2 -> a(0), s -> a(k+1)
                    ijp = 0_ilp
                    jp = 0_ilp
                    do j = 0, k - 1
                       do i = j, n - 1
                          ij = 1_ilp + i + jp
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp
                       end do
                       jp = jp + lda
                    end do
                    do i = 0, k - 1
                       do j = i, k - 1
                          ij = i + j*lda
                          arf( ij ) = conjg( ap( ijp ) )
                          ijp = ijp + 1_ilp
                       end do
                    end do
                 else
                    ! srpa for upper, normal, and n is even ( a(0:n,0:k-1) )
                    ! t1 -> a(k+1,0) ,  t2 -> a(k,0),   s -> a(0,0)
                    ! t1 -> a(k+1), t2 -> a(k), s -> a(0)
                    ijp = 0_ilp
                    do j = 0, k - 1
                       ij = k + 1_ilp + j
                       do i = 0, j
                          arf( ij ) = conjg( ap( ijp ) )
                          ijp = ijp + 1_ilp
                          ij = ij + lda
                       end do
                    end do
                    js = 0_ilp
                    do j = k, n - 1
                       ij = js
                       do ij = js, js + j
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp
                       end do
                       js = js + lda
                    end do
                 end if
              else
                 ! n is even and transr = 'c'
                 if( lower ) then
                    ! srpa for lower, transpose and n is even (see paper)
                    ! t1 -> b(0,1), t2 -> b(0,0), s -> b(0,k+1)
                    ! t1 -> a(0+k), t2 -> a(0+0), s -> a(0+k*(k+1)); lda=k
                    ijp = 0_ilp
                    do i = 0, k - 1
                       do ij = i + ( i+1 )*lda, ( n+1 )*lda - 1, lda
                          arf( ij ) = conjg( ap( ijp ) )
                          ijp = ijp + 1_ilp
                       end do
                    end do
                    js = 0_ilp
                    do j = 0, k - 1
                       do ij = js, js + k - j - 1
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp
                       end do
                       js = js + lda + 1_ilp
                    end do
                 else
                    ! srpa for upper, transpose and n is even (see paper)
                    ! t1 -> b(0,k+1),     t2 -> b(0,k),   s -> b(0,0)
                    ! t1 -> a(0+k*(k+1)), t2 -> a(0+k*k), s -> a(0+0)); lda=k
                    ijp = 0_ilp
                    js = ( k+1 )*lda
                    do j = 0, k - 1
                       do ij = js, js + j
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp
                       end do
                       js = js + lda
                    end do
                    do i = 0, k - 1
                       do ij = i, i + ( k+i )*lda, lda
                          arf( ij ) = conjg( ap( ijp ) )
                          ijp = ijp + 1_ilp
                       end do
                    end do
                 end if
              end if
           end if
           return
     end subroutine stdlib_ctpttf

     pure module subroutine stdlib_ztpttf( transr, uplo, n, ap, arf, info )
     !! ZTPTTF copies a triangular matrix A from standard packed format (TP)
     !! to rectangular full packed format (TF).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character, intent(in) :: transr, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           complex(dp), intent(in) :: ap(0_ilp:*)
           complex(dp), intent(out) :: arf(0_ilp:*)
        ! =====================================================================
           ! Parameters 
           ! Local Scalars 
           logical(lk) :: lower, nisodd, normaltransr
           integer(ilp) :: n1, n2, k, nt
           integer(ilp) :: i, j, ij
           integer(ilp) :: ijp, jp, lda, js
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           normaltransr = stdlib_lsame( transr, 'N' )
           lower = stdlib_lsame( uplo, 'L' )
           if( .not.normaltransr .and. .not.stdlib_lsame( transr, 'C' ) ) then
              info = -1_ilp
           else if( .not.lower .and. .not.stdlib_lsame( uplo, 'U' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZTPTTF', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( n==1_ilp ) then
              if( normaltransr ) then
                 arf( 0_ilp ) = ap( 0_ilp )
              else
                 arf( 0_ilp ) = conjg( ap( 0_ilp ) )
              end if
              return
           end if
           ! size of array arf(0:nt-1)
           nt = n*( n+1 ) / 2_ilp
           ! set n1 and n2 depending on lower
           if( lower ) then
              n2 = n / 2_ilp
              n1 = n - n2
           else
              n1 = n / 2_ilp
              n2 = n - n1
           end if
           ! if n is odd, set nisodd = .true.
           ! if n is even, set k = n/2 and nisodd = .false.
           ! set lda of arf^c; arf^c is (0:(n+1)/2-1,0:n-noe)
           ! where noe = 0 if n is even, noe = 1 if n is odd
           if( mod( n, 2_ilp )==0_ilp ) then
              k = n / 2_ilp
              nisodd = .false.
              lda = n + 1_ilp
           else
              nisodd = .true.
              lda = n
           end if
           ! arf^c has lda rows and n+1-noe cols
           if( .not.normaltransr )lda = ( n+1 ) / 2_ilp
           ! start execution: there are eight cases
           if( nisodd ) then
              ! n is odd
              if( normaltransr ) then
                 ! n is odd and transr = 'n'
                 if( lower ) then
                   ! srpa for lower, normal and n is odd ( a(0:n-1,0:n1-1) )
                   ! t1 -> a(0,0), t2 -> a(0,1), s -> a(n1,0)
                   ! t1 -> a(0), t2 -> a(n), s -> a(n1); lda = n
                    ijp = 0_ilp
                    jp = 0_ilp
                    do j = 0, n2
                       do i = j, n - 1
                          ij = i + jp
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp
                       end do
                       jp = jp + lda
                    end do
                    do i = 0, n2 - 1
                       do j = 1 + i, n2
                          ij = i + j*lda
                          arf( ij ) = conjg( ap( ijp ) )
                          ijp = ijp + 1_ilp
                       end do
                    end do
                 else
                   ! srpa for upper, normal and n is odd ( a(0:n-1,0:n2-1)
                   ! t1 -> a(n1+1,0), t2 -> a(n1,0), s -> a(0,0)
                   ! t1 -> a(n2), t2 -> a(n1), s -> a(0)
                    ijp = 0_ilp
                    do j = 0, n1 - 1
                       ij = n2 + j
                       do i = 0, j
                          arf( ij ) = conjg( ap( ijp ) )
                          ijp = ijp + 1_ilp
                          ij = ij + lda
                       end do
                    end do
                    js = 0_ilp
                    do j = n1, n - 1
                       ij = js
                       do ij = js, js + j
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp
                       end do
                       js = js + lda
                    end do
                 end if
              else
                 ! n is odd and transr = 'c'
                 if( lower ) then
                    ! srpa for lower, transpose and n is odd
                    ! t1 -> a(0,0) , t2 -> a(1,0) , s -> a(0,n1)
                    ! t1 -> a(0+0) , t2 -> a(1+0) , s -> a(0+n1*n1); lda=n1
                    ijp = 0_ilp
                    do i = 0, n2
                       do ij = i*( lda+1 ), n*lda - 1, lda
                          arf( ij ) = conjg( ap( ijp ) )
                          ijp = ijp + 1_ilp
                       end do
                    end do
                    js = 1_ilp
                    do j = 0, n2 - 1
                       do ij = js, js + n2 - j - 1
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp
                       end do
                       js = js + lda + 1_ilp
                    end do
                 else
                    ! srpa for upper, transpose and n is odd
                    ! t1 -> a(0,n1+1), t2 -> a(0,n1), s -> a(0,0)
                    ! t1 -> a(n2*n2), t2 -> a(n1*n2), s -> a(0); lda = n2
                    ijp = 0_ilp
                    js = n2*lda
                    do j = 0, n1 - 1
                       do ij = js, js + j
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp
                       end do
                       js = js + lda
                    end do
                    do i = 0, n1
                       do ij = i, i + ( n1+i )*lda, lda
                          arf( ij ) = conjg( ap( ijp ) )
                          ijp = ijp + 1_ilp
                       end do
                    end do
                 end if
              end if
           else
              ! n is even
              if( normaltransr ) then
                 ! n is even and transr = 'n'
                 if( lower ) then
                    ! srpa for lower, normal, and n is even ( a(0:n,0:k-1) )
                    ! t1 -> a(1,0), t2 -> a(0,0), s -> a(k+1,0)
                    ! t1 -> a(1), t2 -> a(0), s -> a(k+1)
                    ijp = 0_ilp
                    jp = 0_ilp
                    do j = 0, k - 1
                       do i = j, n - 1
                          ij = 1_ilp + i + jp
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp
                       end do
                       jp = jp + lda
                    end do
                    do i = 0, k - 1
                       do j = i, k - 1
                          ij = i + j*lda
                          arf( ij ) = conjg( ap( ijp ) )
                          ijp = ijp + 1_ilp
                       end do
                    end do
                 else
                    ! srpa for upper, normal, and n is even ( a(0:n,0:k-1) )
                    ! t1 -> a(k+1,0) ,  t2 -> a(k,0),   s -> a(0,0)
                    ! t1 -> a(k+1), t2 -> a(k), s -> a(0)
                    ijp = 0_ilp
                    do j = 0, k - 1
                       ij = k + 1_ilp + j
                       do i = 0, j
                          arf( ij ) = conjg( ap( ijp ) )
                          ijp = ijp + 1_ilp
                          ij = ij + lda
                       end do
                    end do
                    js = 0_ilp
                    do j = k, n - 1
                       ij = js
                       do ij = js, js + j
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp
                       end do
                       js = js + lda
                    end do
                 end if
              else
                 ! n is even and transr = 'c'
                 if( lower ) then
                    ! srpa for lower, transpose and n is even (see paper)
                    ! t1 -> b(0,1), t2 -> b(0,0), s -> b(0,k+1)
                    ! t1 -> a(0+k), t2 -> a(0+0), s -> a(0+k*(k+1)); lda=k
                    ijp = 0_ilp
                    do i = 0, k - 1
                       do ij = i + ( i+1 )*lda, ( n+1 )*lda - 1, lda
                          arf( ij ) = conjg( ap( ijp ) )
                          ijp = ijp + 1_ilp
                       end do
                    end do
                    js = 0_ilp
                    do j = 0, k - 1
                       do ij = js, js + k - j - 1
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp
                       end do
                       js = js + lda + 1_ilp
                    end do
                 else
                    ! srpa for upper, transpose and n is even (see paper)
                    ! t1 -> b(0,k+1),     t2 -> b(0,k),   s -> b(0,0)
                    ! t1 -> a(0+k*(k+1)), t2 -> a(0+k*k), s -> a(0+0)); lda=k
                    ijp = 0_ilp
                    js = ( k+1 )*lda
                    do j = 0, k - 1
                       do ij = js, js + j
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp
                       end do
                       js = js + lda
                    end do
                    do i = 0, k - 1
                       do ij = i, i + ( k+i )*lda, lda
                          arf( ij ) = conjg( ap( ijp ) )
                          ijp = ijp + 1_ilp
                       end do
                    end do
                 end if
              end if
           end if
           return
     end subroutine stdlib_ztpttf




     pure module subroutine stdlib_stpttr( uplo, n, ap, a, lda, info )
     !! STPTTR copies a triangular matrix A from standard packed format (TP)
     !! to standard full format (TR).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n, lda
           ! Array Arguments 
           real(sp), intent(out) :: a(lda,*)
           real(sp), intent(in) :: ap(*)
        ! =====================================================================
           ! Parameters 
           ! Local Scalars 
           logical(lk) :: lower
           integer(ilp) :: i, j, k
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           lower = stdlib_lsame( uplo, 'L' )
           if( .not.lower .and. .not.stdlib_lsame( uplo, 'U' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'STPTTR', -info )
              return
           end if
           if( lower ) then
              k = 0_ilp
              do j = 1, n
                 do i = j, n
                    k = k + 1_ilp
                    a( i, j ) = ap( k )
                 end do
              end do
           else
              k = 0_ilp
              do j = 1, n
                 do i = 1, j
                    k = k + 1_ilp
                    a( i, j ) = ap( k )
                 end do
              end do
           end if
           return
     end subroutine stdlib_stpttr

     pure module subroutine stdlib_dtpttr( uplo, n, ap, a, lda, info )
     !! DTPTTR copies a triangular matrix A from standard packed format (TP)
     !! to standard full format (TR).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n, lda
           ! Array Arguments 
           real(dp), intent(out) :: a(lda,*)
           real(dp), intent(in) :: ap(*)
        ! =====================================================================
           ! Parameters 
           ! Local Scalars 
           logical(lk) :: lower
           integer(ilp) :: i, j, k
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           lower = stdlib_lsame( uplo, 'L' )
           if( .not.lower .and. .not.stdlib_lsame( uplo, 'U' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DTPTTR', -info )
              return
           end if
           if( lower ) then
              k = 0_ilp
              do j = 1, n
                 do i = j, n
                    k = k + 1_ilp
                    a( i, j ) = ap( k )
                 end do
              end do
           else
              k = 0_ilp
              do j = 1, n
                 do i = 1, j
                    k = k + 1_ilp
                    a( i, j ) = ap( k )
                 end do
              end do
           end if
           return
     end subroutine stdlib_dtpttr


     pure module subroutine stdlib_ctpttr( uplo, n, ap, a, lda, info )
     !! CTPTTR copies a triangular matrix A from standard packed format (TP)
     !! to standard full format (TR).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n, lda
           ! Array Arguments 
           complex(sp), intent(out) :: a(lda,*)
           complex(sp), intent(in) :: ap(*)
        ! =====================================================================
           ! Parameters 
           ! Local Scalars 
           logical(lk) :: lower
           integer(ilp) :: i, j, k
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           lower = stdlib_lsame( uplo, 'L' )
           if( .not.lower .and. .not.stdlib_lsame( uplo, 'U' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CTPTTR', -info )
              return
           end if
           if( lower ) then
              k = 0_ilp
              do j = 1, n
                 do i = j, n
                    k = k + 1_ilp
                    a( i, j ) = ap( k )
                 end do
              end do
           else
              k = 0_ilp
              do j = 1, n
                 do i = 1, j
                    k = k + 1_ilp
                    a( i, j ) = ap( k )
                 end do
              end do
           end if
           return
     end subroutine stdlib_ctpttr

     pure module subroutine stdlib_ztpttr( uplo, n, ap, a, lda, info )
     !! ZTPTTR copies a triangular matrix A from standard packed format (TP)
     !! to standard full format (TR).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n, lda
           ! Array Arguments 
           complex(dp), intent(out) :: a(lda,*)
           complex(dp), intent(in) :: ap(*)
        ! =====================================================================
           ! Parameters 
           ! Local Scalars 
           logical(lk) :: lower
           integer(ilp) :: i, j, k
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           lower = stdlib_lsame( uplo, 'L' )
           if( .not.lower .and. .not.stdlib_lsame( uplo, 'U' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZTPTTR', -info )
              return
           end if
           if( lower ) then
              k = 0_ilp
              do j = 1, n
                 do i = j, n
                    k = k + 1_ilp
                    a( i, j ) = ap( k )
                 end do
              end do
           else
              k = 0_ilp
              do j = 1, n
                 do i = 1, j
                    k = k + 1_ilp
                    a( i, j ) = ap( k )
                 end do
              end do
           end if
           return
     end subroutine stdlib_ztpttr




     pure module subroutine stdlib_strttf( transr, uplo, n, a, lda, arf, info )
     !! STRTTF copies a triangular matrix A from standard full format (TR)
     !! to rectangular full packed format (TF) .
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character, intent(in) :: transr, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n, lda
           ! Array Arguments 
           real(sp), intent(in) :: a(0_ilp:lda-1,0_ilp:*)
           real(sp), intent(out) :: arf(0_ilp:*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lower, nisodd, normaltransr
           integer(ilp) :: i, ij, j, k, l, n1, n2, nt, nx2, np1x2
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           normaltransr = stdlib_lsame( transr, 'N' )
           lower = stdlib_lsame( uplo, 'L' )
           if( .not.normaltransr .and. .not.stdlib_lsame( transr, 'T' ) ) then
              info = -1_ilp
           else if( .not.lower .and. .not.stdlib_lsame( uplo, 'U' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'STRTTF', -info )
              return
           end if
           ! quick return if possible
           if( n<=1_ilp ) then
              if( n==1_ilp ) then
                 arf( 0_ilp ) = a( 0_ilp, 0_ilp )
              end if
              return
           end if
           ! size of array arf(0:nt-1)
           nt = n*( n+1 ) / 2_ilp
           ! set n1 and n2 depending on lower: for n even n1=n2=k
           if( lower ) then
              n2 = n / 2_ilp
              n1 = n - n2
           else
              n1 = n / 2_ilp
              n2 = n - n1
           end if
           ! if n is odd, set nisodd = .true., lda=n+1 and a is (n+1)--by--k2.
           ! if n is even, set k = n/2 and nisodd = .false., lda=n and a is
           ! n--by--(n+1)/2.
           if( mod( n, 2_ilp )==0_ilp ) then
              k = n / 2_ilp
              nisodd = .false.
              if( .not.lower )np1x2 = n + n + 2_ilp
           else
              nisodd = .true.
              if( .not.lower )nx2 = n + n
           end if
           if( nisodd ) then
              ! n is odd
              if( normaltransr ) then
                 ! n is odd and transr = 'n'
                 if( lower ) then
                    ! n is odd, transr = 'n', and uplo = 'l'
                    ij = 0_ilp
                    do j = 0, n2
                       do i = n1, n2 + j
                          arf( ij ) = a( n2+j, i )
                          ij = ij + 1_ilp
                       end do
                       do i = j, n - 1
                          arf( ij ) = a( i, j )
                          ij = ij + 1_ilp
                       end do
                    end do
                 else
                    ! n is odd, transr = 'n', and uplo = 'u'
                    ij = nt - n
                    do j = n - 1, n1, -1
                       do i = 0, j
                          arf( ij ) = a( i, j )
                          ij = ij + 1_ilp
                       end do
                       do l = j - n1, n1 - 1
                          arf( ij ) = a( j-n1, l )
                          ij = ij + 1_ilp
                       end do
                       ij = ij - nx2
                    end do
                 end if
              else
                 ! n is odd and transr = 't'
                 if( lower ) then
                    ! n is odd, transr = 't', and uplo = 'l'
                    ij = 0_ilp
                    do j = 0, n2 - 1
                       do i = 0, j
                          arf( ij ) = a( j, i )
                          ij = ij + 1_ilp
                       end do
                       do i = n1 + j, n - 1
                          arf( ij ) = a( i, n1+j )
                          ij = ij + 1_ilp
                       end do
                    end do
                    do j = n2, n - 1
                       do i = 0, n1 - 1
                          arf( ij ) = a( j, i )
                          ij = ij + 1_ilp
                       end do
                    end do
                 else
                    ! n is odd, transr = 't', and uplo = 'u'
                    ij = 0_ilp
                    do j = 0, n1
                       do i = n1, n - 1
                          arf( ij ) = a( j, i )
                          ij = ij + 1_ilp
                       end do
                    end do
                    do j = 0, n1 - 1
                       do i = 0, j
                          arf( ij ) = a( i, j )
                          ij = ij + 1_ilp
                       end do
                       do l = n2 + j, n - 1
                          arf( ij ) = a( n2+j, l )
                          ij = ij + 1_ilp
                       end do
                    end do
                 end if
              end if
           else
              ! n is even
              if( normaltransr ) then
                 ! n is even and transr = 'n'
                 if( lower ) then
                    ! n is even, transr = 'n', and uplo = 'l'
                    ij = 0_ilp
                    do j = 0, k - 1
                       do i = k, k + j
                          arf( ij ) = a( k+j, i )
                          ij = ij + 1_ilp
                       end do
                       do i = j, n - 1
                          arf( ij ) = a( i, j )
                          ij = ij + 1_ilp
                       end do
                    end do
                 else
                    ! n is even, transr = 'n', and uplo = 'u'
                    ij = nt - n - 1_ilp
                    do j = n - 1, k, -1
                       do i = 0, j
                          arf( ij ) = a( i, j )
                          ij = ij + 1_ilp
                       end do
                       do l = j - k, k - 1
                          arf( ij ) = a( j-k, l )
                          ij = ij + 1_ilp
                       end do
                       ij = ij - np1x2
                    end do
                 end if
              else
                 ! n is even and transr = 't'
                 if( lower ) then
                    ! n is even, transr = 't', and uplo = 'l'
                    ij = 0_ilp
                    j = k
                    do i = k, n - 1
                       arf( ij ) = a( i, j )
                       ij = ij + 1_ilp
                    end do
                    do j = 0, k - 2
                       do i = 0, j
                          arf( ij ) = a( j, i )
                          ij = ij + 1_ilp
                       end do
                       do i = k + 1 + j, n - 1
                          arf( ij ) = a( i, k+1+j )
                          ij = ij + 1_ilp
                       end do
                    end do
                    do j = k - 1, n - 1
                       do i = 0, k - 1
                          arf( ij ) = a( j, i )
                          ij = ij + 1_ilp
                       end do
                    end do
                 else
                    ! n is even, transr = 't', and uplo = 'u'
                    ij = 0_ilp
                    do j = 0, k
                       do i = k, n - 1
                          arf( ij ) = a( j, i )
                          ij = ij + 1_ilp
                       end do
                    end do
                    do j = 0, k - 2
                       do i = 0, j
                          arf( ij ) = a( i, j )
                          ij = ij + 1_ilp
                       end do
                       do l = k + 1 + j, n - 1
                          arf( ij ) = a( k+1+j, l )
                          ij = ij + 1_ilp
                       end do
                    end do
                    ! note that here, on exit of the loop, j = k-1
                    do i = 0, j
                       arf( ij ) = a( i, j )
                       ij = ij + 1_ilp
                    end do
                 end if
              end if
           end if
           return
     end subroutine stdlib_strttf

     pure module subroutine stdlib_dtrttf( transr, uplo, n, a, lda, arf, info )
     !! DTRTTF copies a triangular matrix A from standard full format (TR)
     !! to rectangular full packed format (TF) .
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character, intent(in) :: transr, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n, lda
           ! Array Arguments 
           real(dp), intent(in) :: a(0_ilp:lda-1,0_ilp:*)
           real(dp), intent(out) :: arf(0_ilp:*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lower, nisodd, normaltransr
           integer(ilp) :: i, ij, j, k, l, n1, n2, nt, nx2, np1x2
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           normaltransr = stdlib_lsame( transr, 'N' )
           lower = stdlib_lsame( uplo, 'L' )
           if( .not.normaltransr .and. .not.stdlib_lsame( transr, 'T' ) ) then
              info = -1_ilp
           else if( .not.lower .and. .not.stdlib_lsame( uplo, 'U' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DTRTTF', -info )
              return
           end if
           ! quick return if possible
           if( n<=1_ilp ) then
              if( n==1_ilp ) then
                 arf( 0_ilp ) = a( 0_ilp, 0_ilp )
              end if
              return
           end if
           ! size of array arf(0:nt-1)
           nt = n*( n+1 ) / 2_ilp
           ! set n1 and n2 depending on lower: for n even n1=n2=k
           if( lower ) then
              n2 = n / 2_ilp
              n1 = n - n2
           else
              n1 = n / 2_ilp
              n2 = n - n1
           end if
           ! if n is odd, set nisodd = .true., lda=n+1 and a is (n+1)--by--k2.
           ! if n is even, set k = n/2 and nisodd = .false., lda=n and a is
           ! n--by--(n+1)/2.
           if( mod( n, 2_ilp )==0_ilp ) then
              k = n / 2_ilp
              nisodd = .false.
              if( .not.lower )np1x2 = n + n + 2_ilp
           else
              nisodd = .true.
              if( .not.lower )nx2 = n + n
           end if
           if( nisodd ) then
              ! n is odd
              if( normaltransr ) then
                 ! n is odd and transr = 'n'
                 if( lower ) then
                    ! n is odd, transr = 'n', and uplo = 'l'
                    ij = 0_ilp
                    do j = 0, n2
                       do i = n1, n2 + j
                          arf( ij ) = a( n2+j, i )
                          ij = ij + 1_ilp
                       end do
                       do i = j, n - 1
                          arf( ij ) = a( i, j )
                          ij = ij + 1_ilp
                       end do
                    end do
                 else
                    ! n is odd, transr = 'n', and uplo = 'u'
                    ij = nt - n
                    do j = n - 1, n1, -1
                       do i = 0, j
                          arf( ij ) = a( i, j )
                          ij = ij + 1_ilp
                       end do
                       do l = j - n1, n1 - 1
                          arf( ij ) = a( j-n1, l )
                          ij = ij + 1_ilp
                       end do
                       ij = ij - nx2
                    end do
                 end if
              else
                 ! n is odd and transr = 't'
                 if( lower ) then
                    ! n is odd, transr = 't', and uplo = 'l'
                    ij = 0_ilp
                    do j = 0, n2 - 1
                       do i = 0, j
                          arf( ij ) = a( j, i )
                          ij = ij + 1_ilp
                       end do
                       do i = n1 + j, n - 1
                          arf( ij ) = a( i, n1+j )
                          ij = ij + 1_ilp
                       end do
                    end do
                    do j = n2, n - 1
                       do i = 0, n1 - 1
                          arf( ij ) = a( j, i )
                          ij = ij + 1_ilp
                       end do
                    end do
                 else
                    ! n is odd, transr = 't', and uplo = 'u'
                    ij = 0_ilp
                    do j = 0, n1
                       do i = n1, n - 1
                          arf( ij ) = a( j, i )
                          ij = ij + 1_ilp
                       end do
                    end do
                    do j = 0, n1 - 1
                       do i = 0, j
                          arf( ij ) = a( i, j )
                          ij = ij + 1_ilp
                       end do
                       do l = n2 + j, n - 1
                          arf( ij ) = a( n2+j, l )
                          ij = ij + 1_ilp
                       end do
                    end do
                 end if
              end if
           else
              ! n is even
              if( normaltransr ) then
                 ! n is even and transr = 'n'
                 if( lower ) then
                    ! n is even, transr = 'n', and uplo = 'l'
                    ij = 0_ilp
                    do j = 0, k - 1
                       do i = k, k + j
                          arf( ij ) = a( k+j, i )
                          ij = ij + 1_ilp
                       end do
                       do i = j, n - 1
                          arf( ij ) = a( i, j )
                          ij = ij + 1_ilp
                       end do
                    end do
                 else
                    ! n is even, transr = 'n', and uplo = 'u'
                    ij = nt - n - 1_ilp
                    do j = n - 1, k, -1
                       do i = 0, j
                          arf( ij ) = a( i, j )
                          ij = ij + 1_ilp
                       end do
                       do l = j - k, k - 1
                          arf( ij ) = a( j-k, l )
                          ij = ij + 1_ilp
                       end do
                       ij = ij - np1x2
                    end do
                 end if
              else
                 ! n is even and transr = 't'
                 if( lower ) then
                    ! n is even, transr = 't', and uplo = 'l'
                    ij = 0_ilp
                    j = k
                    do i = k, n - 1
                       arf( ij ) = a( i, j )
                       ij = ij + 1_ilp
                    end do
                    do j = 0, k - 2
                       do i = 0, j
                          arf( ij ) = a( j, i )
                          ij = ij + 1_ilp
                       end do
                       do i = k + 1 + j, n - 1
                          arf( ij ) = a( i, k+1+j )
                          ij = ij + 1_ilp
                       end do
                    end do
                    do j = k - 1, n - 1
                       do i = 0, k - 1
                          arf( ij ) = a( j, i )
                          ij = ij + 1_ilp
                       end do
                    end do
                 else
                    ! n is even, transr = 't', and uplo = 'u'
                    ij = 0_ilp
                    do j = 0, k
                       do i = k, n - 1
                          arf( ij ) = a( j, i )
                          ij = ij + 1_ilp
                       end do
                    end do
                    do j = 0, k - 2
                       do i = 0, j
                          arf( ij ) = a( i, j )
                          ij = ij + 1_ilp
                       end do
                       do l = k + 1 + j, n - 1
                          arf( ij ) = a( k+1+j, l )
                          ij = ij + 1_ilp
                       end do
                    end do
                    ! note that here, on exit of the loop, j = k-1
                    do i = 0, j
                       arf( ij ) = a( i, j )
                       ij = ij + 1_ilp
                    end do
                 end if
              end if
           end if
           return
     end subroutine stdlib_dtrttf


     pure module subroutine stdlib_ctrttf( transr, uplo, n, a, lda, arf, info )
     !! CTRTTF copies a triangular matrix A from standard full format (TR)
     !! to rectangular full packed format (TF) .
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character, intent(in) :: transr, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n, lda
           ! Array Arguments 
           complex(sp), intent(in) :: a(0_ilp:lda-1,0_ilp:*)
           complex(sp), intent(out) :: arf(0_ilp:*)
        ! =====================================================================
           ! Parameters 
           ! Local Scalars 
           logical(lk) :: lower, nisodd, normaltransr
           integer(ilp) :: i, ij, j, k, l, n1, n2, nt, nx2, np1x2
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           normaltransr = stdlib_lsame( transr, 'N' )
           lower = stdlib_lsame( uplo, 'L' )
           if( .not.normaltransr .and. .not.stdlib_lsame( transr, 'C' ) ) then
              info = -1_ilp
           else if( .not.lower .and. .not.stdlib_lsame( uplo, 'U' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CTRTTF', -info )
              return
           end if
           ! quick return if possible
           if( n<=1_ilp ) then
              if( n==1_ilp ) then
                 if( normaltransr ) then
                    arf( 0_ilp ) = a( 0_ilp, 0_ilp )
                 else
                    arf( 0_ilp ) = conjg( a( 0_ilp, 0_ilp ) )
                 end if
              end if
              return
           end if
           ! size of array arf(1:2,0:nt-1)
           nt = n*( n+1 ) / 2_ilp
           ! set n1 and n2 depending on lower: for n even n1=n2=k
           if( lower ) then
              n2 = n / 2_ilp
              n1 = n - n2
           else
              n1 = n / 2_ilp
              n2 = n - n1
           end if
           ! if n is odd, set nisodd = .true., lda=n+1 and a is (n+1)--by--k2.
           ! if n is even, set k = n/2 and nisodd = .false., lda=n and a is
           ! n--by--(n+1)/2.
           if( mod( n, 2_ilp )==0_ilp ) then
              k = n / 2_ilp
              nisodd = .false.
              if( .not.lower )np1x2 = n + n + 2_ilp
           else
              nisodd = .true.
              if( .not.lower )nx2 = n + n
           end if
           if( nisodd ) then
              ! n is odd
              if( normaltransr ) then
                 ! n is odd and transr = 'n'
                 if( lower ) then
                   ! srpa for lower, normal and n is odd ( a(0:n-1,0:n1-1) )
                   ! t1 -> a(0,0), t2 -> a(0,1), s -> a(n1,0)
                   ! t1 -> a(0), t2 -> a(n), s -> a(n1); lda=n
                    ij = 0_ilp
                    do j = 0, n2
                       do i = n1, n2 + j
                          arf( ij ) = conjg( a( n2+j, i ) )
                          ij = ij + 1_ilp
                       end do
                       do i = j, n - 1
                          arf( ij ) = a( i, j )
                          ij = ij + 1_ilp
                       end do
                    end do
                 else
                   ! srpa for upper, normal and n is odd ( a(0:n-1,0:n2-1)
                   ! t1 -> a(n1+1,0), t2 -> a(n1,0), s -> a(0,0)
                   ! t1 -> a(n2), t2 -> a(n1), s -> a(0); lda=n
                    ij = nt - n
                    do j = n - 1, n1, -1
                       do i = 0, j
                          arf( ij ) = a( i, j )
                          ij = ij + 1_ilp
                       end do
                       do l = j - n1, n1 - 1
                          arf( ij ) = conjg( a( j-n1, l ) )
                          ij = ij + 1_ilp
                       end do
                       ij = ij - nx2
                    end do
                 end if
              else
                 ! n is odd and transr = 'c'
                 if( lower ) then
                    ! srpa for lower, transpose and n is odd
                    ! t1 -> a(0,0) , t2 -> a(1,0) , s -> a(0,n1)
                    ! t1 -> a(0+0) , t2 -> a(1+0) , s -> a(0+n1*n1); lda=n1
                    ij = 0_ilp
                    do j = 0, n2 - 1
                       do i = 0, j
                          arf( ij ) = conjg( a( j, i ) )
                          ij = ij + 1_ilp
                       end do
                       do i = n1 + j, n - 1
                          arf( ij ) = a( i, n1+j )
                          ij = ij + 1_ilp
                       end do
                    end do
                    do j = n2, n - 1
                       do i = 0, n1 - 1
                          arf( ij ) = conjg( a( j, i ) )
                          ij = ij + 1_ilp
                       end do
                    end do
                 else
                    ! srpa for upper, transpose and n is odd
                    ! t1 -> a(0,n1+1), t2 -> a(0,n1), s -> a(0,0)
                    ! t1 -> a(n2*n2), t2 -> a(n1*n2), s -> a(0); lda=n2
                    ij = 0_ilp
                    do j = 0, n1
                       do i = n1, n - 1
                          arf( ij ) = conjg( a( j, i ) )
                          ij = ij + 1_ilp
                       end do
                    end do
                    do j = 0, n1 - 1
                       do i = 0, j
                          arf( ij ) = a( i, j )
                          ij = ij + 1_ilp
                       end do
                       do l = n2 + j, n - 1
                          arf( ij ) = conjg( a( n2+j, l ) )
                          ij = ij + 1_ilp
                       end do
                    end do
                 end if
              end if
           else
              ! n is even
              if( normaltransr ) then
                 ! n is even and transr = 'n'
                 if( lower ) then
                    ! srpa for lower, normal, and n is even ( a(0:n,0:k-1) )
                    ! t1 -> a(1,0), t2 -> a(0,0), s -> a(k+1,0)
                    ! t1 -> a(1), t2 -> a(0), s -> a(k+1); lda=n+1
                    ij = 0_ilp
                    do j = 0, k - 1
                       do i = k, k + j
                          arf( ij ) = conjg( a( k+j, i ) )
                          ij = ij + 1_ilp
                       end do
                       do i = j, n - 1
                          arf( ij ) = a( i, j )
                          ij = ij + 1_ilp
                       end do
                    end do
                 else
                    ! srpa for upper, normal, and n is even ( a(0:n,0:k-1) )
                    ! t1 -> a(k+1,0) ,  t2 -> a(k,0),   s -> a(0,0)
                    ! t1 -> a(k+1), t2 -> a(k), s -> a(0); lda=n+1
                    ij = nt - n - 1_ilp
                    do j = n - 1, k, -1
                       do i = 0, j
                          arf( ij ) = a( i, j )
                          ij = ij + 1_ilp
                       end do
                       do l = j - k, k - 1
                          arf( ij ) = conjg( a( j-k, l ) )
                          ij = ij + 1_ilp
                       end do
                       ij = ij - np1x2
                    end do
                 end if
              else
                 ! n is even and transr = 'c'
                 if( lower ) then
                    ! srpa for lower, transpose and n is even (see paper, a=b)
                    ! t1 -> a(0,1) , t2 -> a(0,0) , s -> a(0,k+1) :
                    ! t1 -> a(0+k) , t2 -> a(0+0) , s -> a(0+k*(k+1)); lda=k
                    ij = 0_ilp
                    j = k
                    do i = k, n - 1
                       arf( ij ) = a( i, j )
                       ij = ij + 1_ilp
                    end do
                    do j = 0, k - 2
                       do i = 0, j
                          arf( ij ) = conjg( a( j, i ) )
                          ij = ij + 1_ilp
                       end do
                       do i = k + 1 + j, n - 1
                          arf( ij ) = a( i, k+1+j )
                          ij = ij + 1_ilp
                       end do
                    end do
                    do j = k - 1, n - 1
                       do i = 0, k - 1
                          arf( ij ) = conjg( a( j, i ) )
                          ij = ij + 1_ilp
                       end do
                    end do
                 else
                    ! srpa for upper, transpose and n is even (see paper, a=b)
                    ! t1 -> a(0,k+1) , t2 -> a(0,k) , s -> a(0,0)
                    ! t1 -> a(0+k*(k+1)) , t2 -> a(0+k*k) , s -> a(0+0)); lda=k
                    ij = 0_ilp
                    do j = 0, k
                       do i = k, n - 1
                          arf( ij ) = conjg( a( j, i ) )
                          ij = ij + 1_ilp
                       end do
                    end do
                    do j = 0, k - 2
                       do i = 0, j
                          arf( ij ) = a( i, j )
                          ij = ij + 1_ilp
                       end do
                       do l = k + 1 + j, n - 1
                          arf( ij ) = conjg( a( k+1+j, l ) )
                          ij = ij + 1_ilp
                       end do
                    end do
                    ! note that here j = k-1
                    do i = 0, j
                       arf( ij ) = a( i, j )
                       ij = ij + 1_ilp
                    end do
                 end if
              end if
           end if
           return
     end subroutine stdlib_ctrttf

     pure module subroutine stdlib_ztrttf( transr, uplo, n, a, lda, arf, info )
     !! ZTRTTF copies a triangular matrix A from standard full format (TR)
     !! to rectangular full packed format (TF) .
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character, intent(in) :: transr, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n, lda
           ! Array Arguments 
           complex(dp), intent(in) :: a(0_ilp:lda-1,0_ilp:*)
           complex(dp), intent(out) :: arf(0_ilp:*)
        ! =====================================================================
           ! Parameters 
           ! Local Scalars 
           logical(lk) :: lower, nisodd, normaltransr
           integer(ilp) :: i, ij, j, k, l, n1, n2, nt, nx2, np1x2
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           normaltransr = stdlib_lsame( transr, 'N' )
           lower = stdlib_lsame( uplo, 'L' )
           if( .not.normaltransr .and. .not.stdlib_lsame( transr, 'C' ) ) then
              info = -1_ilp
           else if( .not.lower .and. .not.stdlib_lsame( uplo, 'U' ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZTRTTF', -info )
              return
           end if
           ! quick return if possible
           if( n<=1_ilp ) then
              if( n==1_ilp ) then
                 if( normaltransr ) then
                    arf( 0_ilp ) = a( 0_ilp, 0_ilp )
                 else
                    arf( 0_ilp ) = conjg( a( 0_ilp, 0_ilp ) )
                 end if
              end if
              return
           end if
           ! size of array arf(1:2,0:nt-1)
           nt = n*( n+1 ) / 2_ilp
           ! set n1 and n2 depending on lower: for n even n1=n2=k
           if( lower ) then
              n2 = n / 2_ilp
              n1 = n - n2
           else
              n1 = n / 2_ilp
              n2 = n - n1
           end if
           ! if n is odd, set nisodd = .true., lda=n+1 and a is (n+1)--by--k2.
           ! if n is even, set k = n/2 and nisodd = .false., lda=n and a is
           ! n--by--(n+1)/2.
           if( mod( n, 2_ilp )==0_ilp ) then
              k = n / 2_ilp
              nisodd = .false.
              if( .not.lower )np1x2 = n + n + 2_ilp
           else
              nisodd = .true.
              if( .not.lower )nx2 = n + n
           end if
           if( nisodd ) then
              ! n is odd
              if( normaltransr ) then
                 ! n is odd and transr = 'n'
                 if( lower ) then
                   ! srpa for lower, normal and n is odd ( a(0:n-1,0:n1-1) )
                   ! t1 -> a(0,0), t2 -> a(0,1), s -> a(n1,0)
                   ! t1 -> a(0), t2 -> a(n), s -> a(n1); lda=n
                    ij = 0_ilp
                    do j = 0, n2
                       do i = n1, n2 + j
                          arf( ij ) = conjg( a( n2+j, i ) )
                          ij = ij + 1_ilp
                       end do
                       do i = j, n - 1
                          arf( ij ) = a( i, j )
                          ij = ij + 1_ilp
                       end do
                    end do
                 else
                   ! srpa for upper, normal and n is odd ( a(0:n-1,0:n2-1)
                   ! t1 -> a(n1+1,0), t2 -> a(n1,0), s -> a(0,0)
                   ! t1 -> a(n2), t2 -> a(n1), s -> a(0); lda=n
                    ij = nt - n
                    do j = n - 1, n1, -1
                       do i = 0, j
                          arf( ij ) = a( i, j )
                          ij = ij + 1_ilp
                       end do
                       do l = j - n1, n1 - 1
                          arf( ij ) = conjg( a( j-n1, l ) )
                          ij = ij + 1_ilp
                       end do
                       ij = ij - nx2
                    end do
                 end if
              else
                 ! n is odd and transr = 'c'
                 if( lower ) then
                    ! srpa for lower, transpose and n is odd
                    ! t1 -> a(0,0) , t2 -> a(1,0) , s -> a(0,n1)
                    ! t1 -> a(0+0) , t2 -> a(1+0) , s -> a(0+n1*n1); lda=n1
                    ij = 0_ilp
                    do j = 0, n2 - 1
                       do i = 0, j
                          arf( ij ) = conjg( a( j, i ) )
                          ij = ij + 1_ilp
                       end do
                       do i = n1 + j, n - 1
                          arf( ij ) = a( i, n1+j )
                          ij = ij + 1_ilp
                       end do
                    end do
                    do j = n2, n - 1
                       do i = 0, n1 - 1
                          arf( ij ) = conjg( a( j, i ) )
                          ij = ij + 1_ilp
                       end do
                    end do
                 else
                    ! srpa for upper, transpose and n is odd
                    ! t1 -> a(0,n1+1), t2 -> a(0,n1), s -> a(0,0)
                    ! t1 -> a(n2*n2), t2 -> a(n1*n2), s -> a(0); lda=n2
                    ij = 0_ilp
                    do j = 0, n1
                       do i = n1, n - 1
                          arf( ij ) = conjg( a( j, i ) )
                          ij = ij + 1_ilp
                       end do
                    end do
                    do j = 0, n1 - 1
                       do i = 0, j
                          arf( ij ) = a( i, j )
                          ij = ij + 1_ilp
                       end do
                       do l = n2 + j, n - 1
                          arf( ij ) = conjg( a( n2+j, l ) )
                          ij = ij + 1_ilp
                       end do
                    end do
                 end if
              end if
           else
              ! n is even
              if( normaltransr ) then
                 ! n is even and transr = 'n'
                 if( lower ) then
                    ! srpa for lower, normal, and n is even ( a(0:n,0:k-1) )
                    ! t1 -> a(1,0), t2 -> a(0,0), s -> a(k+1,0)
                    ! t1 -> a(1), t2 -> a(0), s -> a(k+1); lda=n+1
                    ij = 0_ilp
                    do j = 0, k - 1
                       do i = k, k + j
                          arf( ij ) = conjg( a( k+j, i ) )
                          ij = ij + 1_ilp
                       end do
                       do i = j, n - 1
                          arf( ij ) = a( i, j )
                          ij = ij + 1_ilp
                       end do
                    end do
                 else
                    ! srpa for upper, normal, and n is even ( a(0:n,0:k-1) )
                    ! t1 -> a(k+1,0) ,  t2 -> a(k,0),   s -> a(0,0)
                    ! t1 -> a(k+1), t2 -> a(k), s -> a(0); lda=n+1
                    ij = nt - n - 1_ilp
                    do j = n - 1, k, -1
                       do i = 0, j
                          arf( ij ) = a( i, j )
                          ij = ij + 1_ilp
                       end do
                       do l = j - k, k - 1
                          arf( ij ) = conjg( a( j-k, l ) )
                          ij = ij + 1_ilp
                       end do
                       ij = ij - np1x2
                    end do
                 end if
              else
                 ! n is even and transr = 'c'
                 if( lower ) then
                    ! srpa for lower, transpose and n is even (see paper, a=b)
                    ! t1 -> a(0,1) , t2 -> a(0,0) , s -> a(0,k+1) :
                    ! t1 -> a(0+k) , t2 -> a(0+0) , s -> a(0+k*(k+1)); lda=k
                    ij = 0_ilp
                    j = k
                    do i = k, n - 1
                       arf( ij ) = a( i, j )
                       ij = ij + 1_ilp
                    end do
                    do j = 0, k - 2
                       do i = 0, j
                          arf( ij ) = conjg( a( j, i ) )
                          ij = ij + 1_ilp
                       end do
                       do i = k + 1 + j, n - 1
                          arf( ij ) = a( i, k+1+j )
                          ij = ij + 1_ilp
                       end do
                    end do
                    do j = k - 1, n - 1
                       do i = 0, k - 1
                          arf( ij ) = conjg( a( j, i ) )
                          ij = ij + 1_ilp
                       end do
                    end do
                 else
                    ! srpa for upper, transpose and n is even (see paper, a=b)
                    ! t1 -> a(0,k+1) , t2 -> a(0,k) , s -> a(0,0)
                    ! t1 -> a(0+k*(k+1)) , t2 -> a(0+k*k) , s -> a(0+0)); lda=k
                    ij = 0_ilp
                    do j = 0, k
                       do i = k, n - 1
                          arf( ij ) = conjg( a( j, i ) )
                          ij = ij + 1_ilp
                       end do
                    end do
                    do j = 0, k - 2
                       do i = 0, j
                          arf( ij ) = a( i, j )
                          ij = ij + 1_ilp
                       end do
                       do l = k + 1 + j, n - 1
                          arf( ij ) = conjg( a( k+1+j, l ) )
                          ij = ij + 1_ilp
                       end do
                    end do
                    ! note that here j = k-1
                    do i = 0, j
                       arf( ij ) = a( i, j )
                       ij = ij + 1_ilp
                    end do
                 end if
              end if
           end if
           return
     end subroutine stdlib_ztrttf




     pure module subroutine stdlib_strttp( uplo, n, a, lda, ap, info )
     !! STRTTP copies a triangular matrix A from full format (TR) to standard
     !! packed format (TP).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n, lda
           ! Array Arguments 
           real(sp), intent(in) :: a(lda,*)
           real(sp), intent(out) :: ap(*)
        ! =====================================================================
           ! Parameters 
           ! Local Scalars 
           logical(lk) :: lower
           integer(ilp) :: i, j, k
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           lower = stdlib_lsame( uplo, 'L' )
           if( .not.lower .and. .not.stdlib_lsame( uplo, 'U' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'STRTTP', -info )
              return
           end if
           if( lower ) then
              k = 0_ilp
              do j = 1, n
                 do i = j, n
                    k = k + 1_ilp
                    ap( k ) = a( i, j )
                 end do
              end do
           else
              k = 0_ilp
              do j = 1, n
                 do i = 1, j
                    k = k + 1_ilp
                    ap( k ) = a( i, j )
                 end do
              end do
           end if
           return
     end subroutine stdlib_strttp

     pure module subroutine stdlib_dtrttp( uplo, n, a, lda, ap, info )
     !! DTRTTP copies a triangular matrix A from full format (TR) to standard
     !! packed format (TP).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n, lda
           ! Array Arguments 
           real(dp), intent(in) :: a(lda,*)
           real(dp), intent(out) :: ap(*)
        ! =====================================================================
           ! Parameters 
           ! Local Scalars 
           logical(lk) :: lower
           integer(ilp) :: i, j, k
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           lower = stdlib_lsame( uplo, 'L' )
           if( .not.lower .and. .not.stdlib_lsame( uplo, 'U' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DTRTTP', -info )
              return
           end if
           if( lower ) then
              k = 0_ilp
              do j = 1, n
                 do i = j, n
                    k = k + 1_ilp
                    ap( k ) = a( i, j )
                 end do
              end do
           else
              k = 0_ilp
              do j = 1, n
                 do i = 1, j
                    k = k + 1_ilp
                    ap( k ) = a( i, j )
                 end do
              end do
           end if
           return
     end subroutine stdlib_dtrttp


     pure module subroutine stdlib_ctrttp( uplo, n, a, lda, ap, info )
     !! CTRTTP copies a triangular matrix A from full format (TR) to standard
     !! packed format (TP).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n, lda
           ! Array Arguments 
           complex(sp), intent(in) :: a(lda,*)
           complex(sp), intent(out) :: ap(*)
        ! =====================================================================
           ! Parameters 
           ! Local Scalars 
           logical(lk) :: lower
           integer(ilp) :: i, j, k
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           lower = stdlib_lsame( uplo, 'L' )
           if( .not.lower .and. .not.stdlib_lsame( uplo, 'U' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CTRTTP', -info )
              return
           end if
           if( lower ) then
              k = 0_ilp
              do j = 1, n
                 do i = j, n
                    k = k + 1_ilp
                    ap( k ) = a( i, j )
                 end do
              end do
           else
              k = 0_ilp
              do j = 1, n
                 do i = 1, j
                    k = k + 1_ilp
                    ap( k ) = a( i, j )
                 end do
              end do
           end if
           return
     end subroutine stdlib_ctrttp

     pure module subroutine stdlib_ztrttp( uplo, n, a, lda, ap, info )
     !! ZTRTTP copies a triangular matrix A from full format (TR) to standard
     !! packed format (TP).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n, lda
           ! Array Arguments 
           complex(dp), intent(in) :: a(lda,*)
           complex(dp), intent(out) :: ap(*)
        ! =====================================================================
           ! Parameters 
           ! Local Scalars 
           logical(lk) :: lower
           integer(ilp) :: i, j, k
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           lower = stdlib_lsame( uplo, 'L' )
           if( .not.lower .and. .not.stdlib_lsame( uplo, 'U' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZTRTTP', -info )
              return
           end if
           if( lower ) then
              k = 0_ilp
              do j = 1, n
                 do i = j, n
                    k = k + 1_ilp
                    ap( k ) = a( i, j )
                 end do
              end do
           else
              k = 0_ilp
              do j = 1, n
                 do i = 1, j
                    k = k + 1_ilp
                    ap( k ) = a( i, j )
                 end do
              end do
           end if
           return
     end subroutine stdlib_ztrttp




     pure module subroutine stdlib_dlag2s( m, n, a, lda, sa, ldsa, info )
     !! DLAG2S converts a DOUBLE PRECISION matrix, SA, to a SINGLE
     !! PRECISION matrix, A.
     !! RMAX is the overflow for the SINGLE PRECISION arithmetic
     !! DLAG2S checks that all the entries of A are between -RMAX and
     !! RMAX. If not the conversion is aborted and a flag is raised.
     !! This is an auxiliary routine so there is no argument checking.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldsa, m, n
           ! Array Arguments 
           real(sp), intent(out) :: sa(ldsa,*)
           real(dp), intent(in) :: a(lda,*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, j
           real(dp) :: rmax
           ! Executable Statements 
           rmax = stdlib_slamch( 'O' )
           do j = 1, n
              do i = 1, m
                 if( ( a( i, j )<-rmax ) .or. ( a( i, j )>rmax ) ) then
                    info = 1_ilp
                    go to 30
                 end if
                 sa( i, j ) = a( i, j )
              end do
           end do
           info = 0_ilp
           30 continue
           return
     end subroutine stdlib_dlag2s




     pure module subroutine stdlib_dlat2s( uplo, n, a, lda, sa, ldsa, info )
     !! DLAT2S converts a DOUBLE PRECISION triangular matrix, SA, to a SINGLE
     !! PRECISION triangular matrix, A.
     !! RMAX is the overflow for the SINGLE PRECISION arithmetic
     !! DLAS2S checks that all the entries of A are between -RMAX and
     !! RMAX. If not the conversion is aborted and a flag is raised.
     !! This is an auxiliary routine so there is no argument checking.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldsa, n
           ! Array Arguments 
           real(sp), intent(out) :: sa(ldsa,*)
           real(dp), intent(in) :: a(lda,*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, j
           real(dp) :: rmax
           logical(lk) :: upper
           ! Executable Statements 
           rmax = stdlib_slamch( 'O' )
           upper = stdlib_lsame( uplo, 'U' )
           if( upper ) then
              do j = 1, n
                 do i = 1, j
                    if( ( a( i, j )<-rmax ) .or. ( a( i, j )>rmax ) )then
                       info = 1_ilp
                       go to 50
                    end if
                    sa( i, j ) = a( i, j )
                 end do
              end do
           else
              do j = 1, n
                 do i = j, n
                    if( ( a( i, j )<-rmax ) .or. ( a( i, j )>rmax ) )then
                       info = 1_ilp
                       go to 50
                    end if
                    sa( i, j ) = a( i, j )
                 end do
              end do
           end if
           50 continue
           return
     end subroutine stdlib_dlat2s




     pure module subroutine stdlib_slag2d( m, n, sa, ldsa, a, lda, info )
     !! SLAG2D converts a SINGLE PRECISION matrix, SA, to a DOUBLE
     !! PRECISION matrix, A.
     !! Note that while it is possible to overflow while converting
     !! from double to single, it is not possible to overflow when
     !! converting from single to double.
     !! This is an auxiliary routine so there is no argument checking.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldsa, m, n
           ! Array Arguments 
           real(sp), intent(in) :: sa(ldsa,*)
           real(dp), intent(out) :: a(lda,*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, j
           ! Executable Statements 
           info = 0_ilp
           do j = 1, n
              do i = 1, m
                 a( i, j ) = sa( i, j )
              end do
           end do
           return
     end subroutine stdlib_slag2d



     pure module subroutine stdlib_I64_slaset( uplo, m, n, alpha, beta, a, lda )
     !! SLASET initializes an m-by-n matrix A to BETA on the diagonal and
     !! ALPHA on the offdiagonals.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(in) :: lda, m, n
           real(sp), intent(in) :: alpha, beta
           ! Array Arguments 
           real(sp), intent(out) :: a(lda,*)
       ! =====================================================================
           ! Local Scalars 
           integer(ilp64) :: i, j
           ! Intrinsic Functions 
           ! Executable Statements 
           if( stdlib_lsame( uplo, 'U' ) ) then
              ! set the strictly upper triangular or trapezoidal part of the
              ! array to alpha.
              do j = 2, n
                 do i = 1, min( j-1, m )
                    a( i, j ) = alpha
                 end do
              end do
           else if( stdlib_lsame( uplo, 'L' ) ) then
              ! set the strictly lower triangular or trapezoidal part of the
              ! array to alpha.
              do j = 1, min( m, n )
                 do i = j + 1, m
                    a( i, j ) = alpha
                 end do
              end do
           else
              ! set the leading m-by-n submatrix to alpha.
              do j = 1, n
                 do i = 1, m
                    a( i, j ) = alpha
                 end do
              end do
           end if
           ! set the first min(m,n) diagonal elements to beta.
           do i = 1, min( m, n )
              a( i, i ) = beta
           end do
           return
     end subroutine stdlib_I64_slaset

     pure module subroutine stdlib_I64_dlaset( uplo, m, n, alpha, beta, a, lda )
     !! DLASET initializes an m-by-n matrix A to BETA on the diagonal and
     !! ALPHA on the offdiagonals.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(in) :: lda, m, n
           real(dp), intent(in) :: alpha, beta
           ! Array Arguments 
           real(dp), intent(out) :: a(lda,*)
       ! =====================================================================
           ! Local Scalars 
           integer(ilp64) :: i, j
           ! Intrinsic Functions 
           ! Executable Statements 
           if( stdlib_lsame( uplo, 'U' ) ) then
              ! set the strictly upper triangular or trapezoidal part of the
              ! array to alpha.
              do j = 2, n
                 do i = 1, min( j-1, m )
                    a( i, j ) = alpha
                 end do
              end do
           else if( stdlib_lsame( uplo, 'L' ) ) then
              ! set the strictly lower triangular or trapezoidal part of the
              ! array to alpha.
              do j = 1, min( m, n )
                 do i = j + 1, m
                    a( i, j ) = alpha
                 end do
              end do
           else
              ! set the leading m-by-n submatrix to alpha.
              do j = 1, n
                 do i = 1, m
                    a( i, j ) = alpha
                 end do
              end do
           end if
           ! set the first min(m,n) diagonal elements to beta.
           do i = 1, min( m, n )
              a( i, i ) = beta
           end do
           return
     end subroutine stdlib_I64_dlaset


     pure module subroutine stdlib_I64_claset( uplo, m, n, alpha, beta, a, lda )
     !! CLASET initializes a 2-D array A to BETA on the diagonal and
     !! ALPHA on the offdiagonals.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(in) :: lda, m, n
           complex(sp), intent(in) :: alpha, beta
           ! Array Arguments 
           complex(sp), intent(out) :: a(lda,*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp64) :: i, j
           ! Intrinsic Functions 
           ! Executable Statements 
           if( stdlib_lsame( uplo, 'U' ) ) then
              ! set the diagonal to beta and the strictly upper triangular
              ! part of the array to alpha.
              do j = 2, n
                 do i = 1, min( j-1, m )
                    a( i, j ) = alpha
                 end do
              end do
              do i = 1, min( n, m )
                 a( i, i ) = beta
              end do
           else if( stdlib_lsame( uplo, 'L' ) ) then
              ! set the diagonal to beta and the strictly lower triangular
              ! part of the array to alpha.
              do j = 1, min( m, n )
                 do i = j + 1, m
                    a( i, j ) = alpha
                 end do
              end do
              do i = 1, min( n, m )
                 a( i, i ) = beta
              end do
           else
              ! set the array to beta on the diagonal and alpha on the
              ! offdiagonal.
              do j = 1, n
                 do i = 1, m
                    a( i, j ) = alpha
                 end do
              end do
              do i = 1, min( m, n )
                 a( i, i ) = beta
              end do
           end if
           return
     end subroutine stdlib_I64_claset

     pure module subroutine stdlib_I64_zlaset( uplo, m, n, alpha, beta, a, lda )
     !! ZLASET initializes a 2-D array A to BETA on the diagonal and
     !! ALPHA on the offdiagonals.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(in) :: lda, m, n
           complex(dp), intent(in) :: alpha, beta
           ! Array Arguments 
           complex(dp), intent(out) :: a(lda,*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp64) :: i, j
           ! Intrinsic Functions 
           ! Executable Statements 
           if( stdlib_lsame( uplo, 'U' ) ) then
              ! set the diagonal to beta and the strictly upper triangular
              ! part of the array to alpha.
              do j = 2, n
                 do i = 1, min( j-1, m )
                    a( i, j ) = alpha
                 end do
              end do
              do i = 1, min( n, m )
                 a( i, i ) = beta
              end do
           else if( stdlib_lsame( uplo, 'L' ) ) then
              ! set the diagonal to beta and the strictly lower triangular
              ! part of the array to alpha.
              do j = 1, min( m, n )
                 do i = j + 1, m
                    a( i, j ) = alpha
                 end do
              end do
              do i = 1, min( n, m )
                 a( i, i ) = beta
              end do
           else
              ! set the array to beta on the diagonal and alpha on the
              ! offdiagonal.
              do j = 1, n
                 do i = 1, m
                    a( i, j ) = alpha
                 end do
              end do
              do i = 1, min( m, n )
                 a( i, i ) = beta
              end do
           end if
           return
     end subroutine stdlib_I64_zlaset




     pure module subroutine stdlib_I64_slarnv( idist, iseed, n, x )
     !! SLARNV returns a vector of n random real numbers from a uniform or
     !! normal distribution.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: one, two
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: idist, n
           ! Array Arguments 
           integer(ilp64), intent(inout) :: iseed(4_ilp64)
           real(sp), intent(out) :: x(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp64), parameter :: lv = 128_ilp64
           real(sp), parameter :: twopi = 6.28318530717958647692528676655900576839e+0_sp
           
           
           
           ! Local Scalars 
           integer(ilp64) :: i, il, il2, iv
           ! Local Arrays 
           real(sp) :: u(lv)
           ! Intrinsic Functions 
           ! Executable Statements 
           do 40 iv = 1, n, lv / 2
              il = min( lv / 2_ilp64, n-iv+1 )
              if( idist==3_ilp64 ) then
                 il2 = 2_ilp64*il
              else
                 il2 = il
              end if
              ! call stdlib_I64_slaruv to generate il2 numbers from a uniform (0,1)
              ! distribution (il2 <= lv)
              call stdlib_I64_slaruv( iseed, il2, u )
              if( idist==1_ilp64 ) then
                 ! copy generated numbers
                 do i = 1, il
                    x( iv+i-1 ) = u( i )
                 end do
              else if( idist==2_ilp64 ) then
                 ! convert generated numbers to uniform (-1,1) distribution
                 do i = 1, il
                    x( iv+i-1 ) = two*u( i ) - one
                 end do
              else if( idist==3_ilp64 ) then
                 ! convert generated numbers to normal (0,1) distribution
                 do i = 1, il
                    x( iv+i-1 ) = sqrt( -two*log( u( 2_ilp64*i-1 ) ) )*cos( twopi*u( 2_ilp64*i ) )
                 end do
              end if
              40 continue
           return
     end subroutine stdlib_I64_slarnv

     pure module subroutine stdlib_I64_dlarnv( idist, iseed, n, x )
     !! DLARNV returns a vector of n random real numbers from a uniform or
     !! normal distribution.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: one, two
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: idist, n
           ! Array Arguments 
           integer(ilp64), intent(inout) :: iseed(4_ilp64)
           real(dp), intent(out) :: x(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp64), parameter :: lv = 128_ilp64
           real(dp), parameter :: twopi = 6.28318530717958647692528676655900576839e+0_dp
           
           
           
           ! Local Scalars 
           integer(ilp64) :: i, il, il2, iv
           ! Local Arrays 
           real(dp) :: u(lv)
           ! Intrinsic Functions 
           ! Executable Statements 
           do 40 iv = 1, n, lv / 2
              il = min( lv / 2_ilp64, n-iv+1 )
              if( idist==3_ilp64 ) then
                 il2 = 2_ilp64*il
              else
                 il2 = il
              end if
              ! call stdlib_I64_dlaruv to generate il2 numbers from a uniform (0,1)
              ! distribution (il2 <= lv)
              call stdlib_I64_dlaruv( iseed, il2, u )
              if( idist==1_ilp64 ) then
                 ! copy generated numbers
                 do i = 1, il
                    x( iv+i-1 ) = u( i )
                 end do
              else if( idist==2_ilp64 ) then
                 ! convert generated numbers to uniform (-1,1) distribution
                 do i = 1, il
                    x( iv+i-1 ) = two*u( i ) - one
                 end do
              else if( idist==3_ilp64 ) then
                 ! convert generated numbers to normal (0,1) distribution
                 do i = 1, il
                    x( iv+i-1 ) = sqrt( -two*log( u( 2_ilp64*i-1 ) ) )*cos( twopi*u( 2_ilp64*i ) )
                 end do
              end if
              40 continue
           return
     end subroutine stdlib_I64_dlarnv


     pure module subroutine stdlib_I64_clarnv( idist, iseed, n, x )
     !! CLARNV returns a vector of n random complex numbers from a uniform or
     !! normal distribution.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: zero, one, two
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: idist, n
           ! Array Arguments 
           integer(ilp64), intent(inout) :: iseed(4_ilp64)
           complex(sp), intent(out) :: x(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp64), parameter :: lv = 128_ilp64
           real(sp), parameter :: twopi = 6.28318530717958647692528676655900576839e+0_sp
           
           
           
           ! Local Scalars 
           integer(ilp64) :: i, il, iv
           ! Local Arrays 
           real(sp) :: u(lv)
           ! Intrinsic Functions 
           ! Executable Statements 
           do 60 iv = 1, n, lv / 2
              il = min( lv / 2_ilp64, n-iv+1 )
              ! call stdlib_I64_slaruv to generate 2*il realnumbers from a uniform (0,1,KIND=sp)
              ! distribution (2*il <= lv)
              call stdlib_I64_slaruv( iseed, 2_ilp64*il, u )
              if( idist==1_ilp64 ) then
                 ! copy generated numbers
                 do i = 1, il
                    x( iv+i-1 ) = cmplx( u( 2_ilp64*i-1 ), u( 2_ilp64*i ),KIND=sp)
                 end do
              else if( idist==2_ilp64 ) then
                 ! convert generated numbers to uniform (-1,1) distribution
                 do i = 1, il
                    x( iv+i-1 ) = cmplx( two*u( 2_ilp64*i-1 )-one,two*u( 2_ilp64*i )-one,KIND=sp)
                 end do
              else if( idist==3_ilp64 ) then
                 ! convert generated numbers to normal (0,1) distribution
                 do i = 1, il
                    x( iv+i-1 ) = sqrt( -two*log( u( 2_ilp64*i-1 ) ) )*exp( cmplx( zero, twopi*u( 2_ilp64*i ),&
                              KIND=sp) )
                 end do
              else if( idist==4_ilp64 ) then
                 ! convert generated numbers to complex numbers uniformly
                 ! distributed on the unit disk
                 do i = 1, il
                    x( iv+i-1 ) = sqrt( u( 2_ilp64*i-1 ) )*exp( cmplx( zero, twopi*u( 2_ilp64*i ),KIND=sp) )
                              
                 end do
              else if( idist==5_ilp64 ) then
                 ! convert generated numbers to complex numbers uniformly
                 ! distributed on the unit circle
                 do i = 1, il
                    x( iv+i-1 ) = exp( cmplx( zero, twopi*u( 2_ilp64*i ),KIND=sp) )
                 end do
              end if
              60 continue
           return
     end subroutine stdlib_I64_clarnv

     pure module subroutine stdlib_I64_zlarnv( idist, iseed, n, x )
     !! ZLARNV returns a vector of n random complex numbers from a uniform or
     !! normal distribution.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: zero, one, two
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: idist, n
           ! Array Arguments 
           integer(ilp64), intent(inout) :: iseed(4_ilp64)
           complex(dp), intent(out) :: x(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp64), parameter :: lv = 128_ilp64
           real(dp), parameter :: twopi = 6.28318530717958647692528676655900576839e+0_dp
           
           
           
           ! Local Scalars 
           integer(ilp64) :: i, il, iv
           ! Local Arrays 
           real(dp) :: u(lv)
           ! Intrinsic Functions 
           ! Executable Statements 
           do 60 iv = 1, n, lv / 2
              il = min( lv / 2_ilp64, n-iv+1 )
              ! call stdlib_I64_dlaruv to generate 2*il realnumbers from a uniform (0,1,KIND=dp)
              ! distribution (2*il <= lv)
              call stdlib_I64_dlaruv( iseed, 2_ilp64*il, u )
              if( idist==1_ilp64 ) then
                 ! copy generated numbers
                 do i = 1, il
                    x( iv+i-1 ) = cmplx( u( 2_ilp64*i-1 ), u( 2_ilp64*i ),KIND=dp)
                 end do
              else if( idist==2_ilp64 ) then
                 ! convert generated numbers to uniform (-1,1) distribution
                 do i = 1, il
                    x( iv+i-1 ) = cmplx( two*u( 2_ilp64*i-1 )-one,two*u( 2_ilp64*i )-one,KIND=dp)
                 end do
              else if( idist==3_ilp64 ) then
                 ! convert generated numbers to normal (0,1) distribution
                 do i = 1, il
                    x( iv+i-1 ) = sqrt( -two*log( u( 2_ilp64*i-1 ) ) )*exp( cmplx( zero, twopi*u( 2_ilp64*i ),&
                              KIND=dp) )
                 end do
              else if( idist==4_ilp64 ) then
                 ! convert generated numbers to complex numbers uniformly
                 ! distributed on the unit disk
                 do i = 1, il
                    x( iv+i-1 ) = sqrt( u( 2_ilp64*i-1 ) )*exp( cmplx( zero, twopi*u( 2_ilp64*i ),KIND=dp) )
                              
                 end do
              else if( idist==5_ilp64 ) then
                 ! convert generated numbers to complex numbers uniformly
                 ! distributed on the unit circle
                 do i = 1, il
                    x( iv+i-1 ) = exp( cmplx( zero, twopi*u( 2_ilp64*i ),KIND=dp) )
                 end do
              end if
              60 continue
           return
     end subroutine stdlib_I64_zlarnv




     pure module subroutine stdlib_I64_slaruv( iseed, n, x )
     !! SLARUV returns a vector of n random real numbers from a uniform (0,1)
     !! distribution (n <= 128).
     !! This is an auxiliary routine called by SLARNV and CLARNV.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: one
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: n
           ! Array Arguments 
           integer(ilp64), intent(inout) :: iseed(4_ilp64)
           real(sp), intent(out) :: x(n)
        ! =====================================================================
           ! Parameters 
           integer(ilp64), parameter :: lv = 128_ilp64
           integer(ilp64), parameter :: ipw2 = 4096_ilp64
           real(sp), parameter :: r = one/ipw2
           
           
           
           ! Local Scalars 
           integer(ilp64) :: i, i1, i2, i3, i4, it1, it2, it3, it4
           ! Local Arrays 
           integer(ilp64) :: mm(lv,4_ilp64)
           ! Intrinsic Functions 
           ! Data Statements 
           mm(1_ilp64,1_ilp64:4_ilp64)=[494_ilp64,322_ilp64,2508_ilp64,2549_ilp64]
           mm(2_ilp64,1_ilp64:4_ilp64)=[2637_ilp64,789_ilp64,3754_ilp64,1145_ilp64]
           mm(3_ilp64,1_ilp64:4_ilp64)=[255_ilp64,1440_ilp64,1766_ilp64,2253_ilp64]
           mm(4_ilp64,1_ilp64:4_ilp64)=[2008_ilp64,752_ilp64,3572_ilp64,305_ilp64]
           mm(5_ilp64,1_ilp64:4_ilp64)=[1253_ilp64,2859_ilp64,2893_ilp64,3301_ilp64]
           mm(6_ilp64,1_ilp64:4_ilp64)=[3344_ilp64,123_ilp64,307_ilp64,1065_ilp64]
           mm(7_ilp64,1_ilp64:4_ilp64)=[4084_ilp64,1848_ilp64,1297_ilp64,3133_ilp64]
           mm(8_ilp64,1_ilp64:4_ilp64)=[1739_ilp64,643_ilp64,3966_ilp64,2913_ilp64]
           mm(9_ilp64,1_ilp64:4_ilp64)=[3143_ilp64,2405_ilp64,758_ilp64,3285_ilp64]
           mm(10_ilp64,1_ilp64:4_ilp64)=[3468_ilp64,2638_ilp64,2598_ilp64,1241_ilp64]
           mm(11_ilp64,1_ilp64:4_ilp64)=[688_ilp64,2344_ilp64,3406_ilp64,1197_ilp64]
           mm(12_ilp64,1_ilp64:4_ilp64)=[1657_ilp64,46_ilp64,2922_ilp64,3729_ilp64]
           mm(13_ilp64,1_ilp64:4_ilp64)=[1238_ilp64,3814_ilp64,1038_ilp64,2501_ilp64]
           mm(14_ilp64,1_ilp64:4_ilp64)=[3166_ilp64,913_ilp64,2934_ilp64,1673_ilp64]
           mm(15_ilp64,1_ilp64:4_ilp64)=[1292_ilp64,3649_ilp64,2091_ilp64,541_ilp64]
           mm(16_ilp64,1_ilp64:4_ilp64)=[3422_ilp64,339_ilp64,2451_ilp64,2753_ilp64]
           mm(17_ilp64,1_ilp64:4_ilp64)=[1270_ilp64,3808_ilp64,1580_ilp64,949_ilp64]
           mm(18_ilp64,1_ilp64:4_ilp64)=[2016_ilp64,822_ilp64,1958_ilp64,2361_ilp64]
           mm(19_ilp64,1_ilp64:4_ilp64)=[154_ilp64,2832_ilp64,2055_ilp64,1165_ilp64]
           mm(20_ilp64,1_ilp64:4_ilp64)=[2862_ilp64,3078_ilp64,1507_ilp64,4081_ilp64]
           mm(21_ilp64,1_ilp64:4_ilp64)=[697_ilp64,3633_ilp64,1078_ilp64,2725_ilp64]
           mm(22_ilp64,1_ilp64:4_ilp64)=[1706_ilp64,2970_ilp64,3273_ilp64,3305_ilp64]
           mm(23_ilp64,1_ilp64:4_ilp64)=[491_ilp64,637_ilp64,17_ilp64,3069_ilp64]
           mm(24_ilp64,1_ilp64:4_ilp64)=[931_ilp64,2249_ilp64,854_ilp64,3617_ilp64]
           mm(25_ilp64,1_ilp64:4_ilp64)=[1444_ilp64,2081_ilp64,2916_ilp64,3733_ilp64]
           mm(26_ilp64,1_ilp64:4_ilp64)=[444_ilp64,4019_ilp64,3971_ilp64,409_ilp64]
           mm(27_ilp64,1_ilp64:4_ilp64)=[3577_ilp64,1478_ilp64,2889_ilp64,2157_ilp64]
           mm(28_ilp64,1_ilp64:4_ilp64)=[3944_ilp64,242_ilp64,3831_ilp64,1361_ilp64]
           mm(29_ilp64,1_ilp64:4_ilp64)=[2184_ilp64,481_ilp64,2621_ilp64,3973_ilp64]
           mm(30_ilp64,1_ilp64:4_ilp64)=[1661_ilp64,2075_ilp64,1541_ilp64,1865_ilp64]
           mm(31_ilp64,1_ilp64:4_ilp64)=[3482_ilp64,4058_ilp64,893_ilp64,2525_ilp64]
           mm(32_ilp64,1_ilp64:4_ilp64)=[657_ilp64,622_ilp64,736_ilp64,1409_ilp64]
           mm(33_ilp64,1_ilp64:4_ilp64)=[3023_ilp64,3376_ilp64,3992_ilp64,3445_ilp64]
           mm(34_ilp64,1_ilp64:4_ilp64)=[3618_ilp64,812_ilp64,787_ilp64,3577_ilp64]
           mm(35_ilp64,1_ilp64:4_ilp64)=[1267_ilp64,234_ilp64,2125_ilp64,77_ilp64]
           mm(36_ilp64,1_ilp64:4_ilp64)=[1828_ilp64,641_ilp64,2364_ilp64,3761_ilp64]
           mm(37_ilp64,1_ilp64:4_ilp64)=[164_ilp64,4005_ilp64,2460_ilp64,2149_ilp64]
           mm(38_ilp64,1_ilp64:4_ilp64)=[3798_ilp64,1122_ilp64,257_ilp64,1449_ilp64]
           mm(39_ilp64,1_ilp64:4_ilp64)=[3087_ilp64,3135_ilp64,1574_ilp64,3005_ilp64]
           mm(40_ilp64,1_ilp64:4_ilp64)=[2400_ilp64,2640_ilp64,3912_ilp64,225_ilp64]
           mm(41_ilp64,1_ilp64:4_ilp64)=[2870_ilp64,2302_ilp64,1216_ilp64,85_ilp64]
           mm(42_ilp64,1_ilp64:4_ilp64)=[3876_ilp64,40_ilp64,3248_ilp64,3673_ilp64]
           mm(43_ilp64,1_ilp64:4_ilp64)=[1905_ilp64,1832_ilp64,3401_ilp64,3117_ilp64]
           mm(44_ilp64,1_ilp64:4_ilp64)=[1593_ilp64,2247_ilp64,2124_ilp64,3089_ilp64]
           mm(45_ilp64,1_ilp64:4_ilp64)=[1797_ilp64,2034_ilp64,2762_ilp64,1349_ilp64]
           mm(46_ilp64,1_ilp64:4_ilp64)=[1234_ilp64,2637_ilp64,149_ilp64,2057_ilp64]
           mm(47_ilp64,1_ilp64:4_ilp64)=[3460_ilp64,1287_ilp64,2245_ilp64,413_ilp64]
           mm(48_ilp64,1_ilp64:4_ilp64)=[328_ilp64,1691_ilp64,166_ilp64,65_ilp64]
           mm(49_ilp64,1_ilp64:4_ilp64)=[2861_ilp64,496_ilp64,466_ilp64,1845_ilp64]
           mm(50_ilp64,1_ilp64:4_ilp64)=[1950_ilp64,1597_ilp64,4018_ilp64,697_ilp64]
           mm(51_ilp64,1_ilp64:4_ilp64)=[617_ilp64,2394_ilp64,1399_ilp64,3085_ilp64]
           mm(52_ilp64,1_ilp64:4_ilp64)=[2070_ilp64,2584_ilp64,190_ilp64,3441_ilp64]
           mm(53_ilp64,1_ilp64:4_ilp64)=[3331_ilp64,1843_ilp64,2879_ilp64,1573_ilp64]
           mm(54_ilp64,1_ilp64:4_ilp64)=[769_ilp64,336_ilp64,153_ilp64,3689_ilp64]
           mm(55_ilp64,1_ilp64:4_ilp64)=[1558_ilp64,1472_ilp64,2320_ilp64,2941_ilp64]
           mm(56_ilp64,1_ilp64:4_ilp64)=[2412_ilp64,2407_ilp64,18_ilp64,929_ilp64]
           mm(57_ilp64,1_ilp64:4_ilp64)=[2800_ilp64,433_ilp64,712_ilp64,533_ilp64]
           mm(58_ilp64,1_ilp64:4_ilp64)=[189_ilp64,2096_ilp64,2159_ilp64,2841_ilp64]
           mm(59_ilp64,1_ilp64:4_ilp64)=[287_ilp64,1761_ilp64,2318_ilp64,4077_ilp64]
           mm(60_ilp64,1_ilp64:4_ilp64)=[2045_ilp64,2810_ilp64,2091_ilp64,721_ilp64]
           mm(61_ilp64,1_ilp64:4_ilp64)=[1227_ilp64,566_ilp64,3443_ilp64,2821_ilp64]
           mm(62_ilp64,1_ilp64:4_ilp64)=[2838_ilp64,442_ilp64,1510_ilp64,2249_ilp64]
           mm(63_ilp64,1_ilp64:4_ilp64)=[209_ilp64,41_ilp64,449_ilp64,2397_ilp64]
           mm(64_ilp64,1_ilp64:4_ilp64)=[2770_ilp64,1238_ilp64,1956_ilp64,2817_ilp64]
           mm(65_ilp64,1_ilp64:4_ilp64)=[3654_ilp64,1086_ilp64,2201_ilp64,245_ilp64]
           mm(66_ilp64,1_ilp64:4_ilp64)=[3993_ilp64,603_ilp64,3137_ilp64,1913_ilp64]
           mm(67_ilp64,1_ilp64:4_ilp64)=[192_ilp64,840_ilp64,3399_ilp64,1997_ilp64]
           mm(68_ilp64,1_ilp64:4_ilp64)=[2253_ilp64,3168_ilp64,1321_ilp64,3121_ilp64]
           mm(69_ilp64,1_ilp64:4_ilp64)=[3491_ilp64,1499_ilp64,2271_ilp64,997_ilp64]
           mm(70_ilp64,1_ilp64:4_ilp64)=[2889_ilp64,1084_ilp64,3667_ilp64,1833_ilp64]
           mm(71_ilp64,1_ilp64:4_ilp64)=[2857_ilp64,3438_ilp64,2703_ilp64,2877_ilp64]
           mm(72_ilp64,1_ilp64:4_ilp64)=[2094_ilp64,2408_ilp64,629_ilp64,1633_ilp64]
           mm(73_ilp64,1_ilp64:4_ilp64)=[1818_ilp64,1589_ilp64,2365_ilp64,981_ilp64]
           mm(74_ilp64,1_ilp64:4_ilp64)=[688_ilp64,2391_ilp64,2431_ilp64,2009_ilp64]
           mm(75_ilp64,1_ilp64:4_ilp64)=[1407_ilp64,288_ilp64,1113_ilp64,941_ilp64]
           mm(76_ilp64,1_ilp64:4_ilp64)=[634_ilp64,26_ilp64,3922_ilp64,2449_ilp64]
           mm(77_ilp64,1_ilp64:4_ilp64)=[3231_ilp64,512_ilp64,2554_ilp64,197_ilp64]
           mm(78_ilp64,1_ilp64:4_ilp64)=[815_ilp64,1456_ilp64,184_ilp64,2441_ilp64]
           mm(79_ilp64,1_ilp64:4_ilp64)=[3524_ilp64,171_ilp64,2099_ilp64,285_ilp64]
           mm(80_ilp64,1_ilp64:4_ilp64)=[1914_ilp64,1677_ilp64,3228_ilp64,1473_ilp64]
           mm(81_ilp64,1_ilp64:4_ilp64)=[516_ilp64,2657_ilp64,4012_ilp64,2741_ilp64]
           mm(82_ilp64,1_ilp64:4_ilp64)=[164_ilp64,2270_ilp64,1921_ilp64,3129_ilp64]
           mm(83_ilp64,1_ilp64:4_ilp64)=[303_ilp64,2587_ilp64,3452_ilp64,909_ilp64]
           mm(84_ilp64,1_ilp64:4_ilp64)=[2144_ilp64,2961_ilp64,3901_ilp64,2801_ilp64]
           mm(85_ilp64,1_ilp64:4_ilp64)=[3480_ilp64,1970_ilp64,572_ilp64,421_ilp64]
           mm(86_ilp64,1_ilp64:4_ilp64)=[119_ilp64,1817_ilp64,3309_ilp64,4073_ilp64]
           mm(87_ilp64,1_ilp64:4_ilp64)=[3357_ilp64,676_ilp64,3171_ilp64,2813_ilp64]
           mm(88_ilp64,1_ilp64:4_ilp64)=[837_ilp64,1410_ilp64,817_ilp64,2337_ilp64]
           mm(89_ilp64,1_ilp64:4_ilp64)=[2826_ilp64,3723_ilp64,3039_ilp64,1429_ilp64]
           mm(90_ilp64,1_ilp64:4_ilp64)=[2332_ilp64,2803_ilp64,1696_ilp64,1177_ilp64]
           mm(91_ilp64,1_ilp64:4_ilp64)=[2089_ilp64,3185_ilp64,1256_ilp64,1901_ilp64]
           mm(92_ilp64,1_ilp64:4_ilp64)=[3780_ilp64,184_ilp64,3715_ilp64,81_ilp64]
           mm(93_ilp64,1_ilp64:4_ilp64)=[1700_ilp64,663_ilp64,2077_ilp64,1669_ilp64]
           mm(94_ilp64,1_ilp64:4_ilp64)=[3712_ilp64,499_ilp64,3019_ilp64,2633_ilp64]
           mm(95_ilp64,1_ilp64:4_ilp64)=[150_ilp64,3784_ilp64,1497_ilp64,2269_ilp64]
           mm(96_ilp64,1_ilp64:4_ilp64)=[2000_ilp64,1631_ilp64,1101_ilp64,129_ilp64]
           mm(97_ilp64,1_ilp64:4_ilp64)=[3375_ilp64,1925_ilp64,717_ilp64,1141_ilp64]
           mm(98_ilp64,1_ilp64:4_ilp64)=[1621_ilp64,3912_ilp64,51_ilp64,249_ilp64]
           mm(99_ilp64,1_ilp64:4_ilp64)=[3090_ilp64,1398_ilp64,981_ilp64,3917_ilp64]
           mm(100_ilp64,1_ilp64:4_ilp64)=[3765_ilp64,1349_ilp64,1978_ilp64,2481_ilp64]
           mm(101_ilp64,1_ilp64:4_ilp64)=[1149_ilp64,1441_ilp64,1813_ilp64,3941_ilp64]
           mm(102_ilp64,1_ilp64:4_ilp64)=[3146_ilp64,2224_ilp64,3881_ilp64,2217_ilp64]
           mm(103_ilp64,1_ilp64:4_ilp64)=[33_ilp64,2411_ilp64,76_ilp64,2749_ilp64]
           mm(104_ilp64,1_ilp64:4_ilp64)=[3082_ilp64,1907_ilp64,3846_ilp64,3041_ilp64]
           mm(105_ilp64,1_ilp64:4_ilp64)=[2741_ilp64,3192_ilp64,3694_ilp64,1877_ilp64]
           mm(106_ilp64,1_ilp64:4_ilp64)=[359_ilp64,2786_ilp64,1682_ilp64,345_ilp64]
           mm(107_ilp64,1_ilp64:4_ilp64)=[3316_ilp64,382_ilp64,124_ilp64,2861_ilp64]
           mm(108_ilp64,1_ilp64:4_ilp64)=[1749_ilp64,37_ilp64,1660_ilp64,1809_ilp64]
           mm(109_ilp64,1_ilp64:4_ilp64)=[185_ilp64,759_ilp64,3997_ilp64,3141_ilp64]
           mm(110_ilp64,1_ilp64:4_ilp64)=[2784_ilp64,2948_ilp64,479_ilp64,2825_ilp64]
           mm(111_ilp64,1_ilp64:4_ilp64)=[2202_ilp64,1862_ilp64,1141_ilp64,157_ilp64]
           mm(112_ilp64,1_ilp64:4_ilp64)=[2199_ilp64,3802_ilp64,886_ilp64,2881_ilp64]
           mm(113_ilp64,1_ilp64:4_ilp64)=[1364_ilp64,2423_ilp64,3514_ilp64,3637_ilp64]
           mm(114_ilp64,1_ilp64:4_ilp64)=[1244_ilp64,2051_ilp64,1301_ilp64,1465_ilp64]
           mm(115_ilp64,1_ilp64:4_ilp64)=[2020_ilp64,2295_ilp64,3604_ilp64,2829_ilp64]
           mm(116_ilp64,1_ilp64:4_ilp64)=[3160_ilp64,1332_ilp64,1888_ilp64,2161_ilp64]
           mm(117_ilp64,1_ilp64:4_ilp64)=[2785_ilp64,1832_ilp64,1836_ilp64,3365_ilp64]
           mm(118_ilp64,1_ilp64:4_ilp64)=[2772_ilp64,2405_ilp64,1990_ilp64,361_ilp64]
           mm(119_ilp64,1_ilp64:4_ilp64)=[1217_ilp64,3638_ilp64,2058_ilp64,2685_ilp64]
           mm(120_ilp64,1_ilp64:4_ilp64)=[1822_ilp64,3661_ilp64,692_ilp64,3745_ilp64]
           mm(121_ilp64,1_ilp64:4_ilp64)=[1245_ilp64,327_ilp64,1194_ilp64,2325_ilp64]
           mm(122_ilp64,1_ilp64:4_ilp64)=[2252_ilp64,3660_ilp64,20_ilp64,3609_ilp64]
           mm(123_ilp64,1_ilp64:4_ilp64)=[3904_ilp64,716_ilp64,3285_ilp64,3821_ilp64]
           mm(124_ilp64,1_ilp64:4_ilp64)=[2774_ilp64,1842_ilp64,2046_ilp64,3537_ilp64]
           mm(125_ilp64,1_ilp64:4_ilp64)=[997_ilp64,3987_ilp64,2107_ilp64,517_ilp64]
           mm(126_ilp64,1_ilp64:4_ilp64)=[2573_ilp64,1368_ilp64,3508_ilp64,3017_ilp64]
           mm(127_ilp64,1_ilp64:4_ilp64)=[1148_ilp64,1848_ilp64,3525_ilp64,2141_ilp64]
           mm(128_ilp64,1_ilp64:4_ilp64)=[545_ilp64,2366_ilp64,3801_ilp64,1537_ilp64]
           ! Executable Statements 
           i1 = iseed( 1_ilp64 )
           i2 = iseed( 2_ilp64 )
           i3 = iseed( 3_ilp64 )
           i4 = iseed( 4_ilp64 )
           loop_10: do i = 1, min( n, lv )
           20 continue
              ! multiply the seed by i-th power of the multiplier modulo 2**48
              it4 = i4*mm( i, 4_ilp64 )
              it3 = it4 / ipw2
              it4 = it4 - ipw2*it3
              it3 = it3 + i3*mm( i, 4_ilp64 ) + i4*mm( i, 3_ilp64 )
              it2 = it3 / ipw2
              it3 = it3 - ipw2*it2
              it2 = it2 + i2*mm( i, 4_ilp64 ) + i3*mm( i, 3_ilp64 ) + i4*mm( i, 2_ilp64 )
              it1 = it2 / ipw2
              it2 = it2 - ipw2*it1
              it1 = it1 + i1*mm( i, 4_ilp64 ) + i2*mm( i, 3_ilp64 ) + i3*mm( i, 2_ilp64 ) +i4*mm( i, 1_ilp64 )
              it1 = mod( it1, ipw2 )
              ! convert 48-bit integer to a realnumber in the interval (0,1,KIND=sp)
              x( i ) = r*( real( it1,KIND=sp)+r*( real( it2,KIND=sp)+r*( real( it3,KIND=sp)+&
                        r*real( it4,KIND=sp) ) ) )
              if (x( i )==one) then
                 ! if a real number has n bits of precision, and the first
                 ! n bits of the 48-bit integer above happen to be all 1 (which
                 ! will occur about once every 2**n calls), then x( i ) will
                 ! be rounded to exactly one. in ieee single precision arithmetic,
                 ! this will happen relatively often since n = 24.
                 ! since x( i ) is not supposed to return exactly 0.0_sp or one,
                 ! the statistically correct thing to do in this situation is
                 ! simply to iterate again.
                 ! n.b. the case x( i ) = 0.0_sp should not be possible.
                 i1 = i1 + 2_ilp64
                 i2 = i2 + 2_ilp64
                 i3 = i3 + 2_ilp64
                 i4 = i4 + 2_ilp64
                 goto 20
              end if
           end do loop_10
           ! return final value of seed
           iseed( 1_ilp64 ) = it1
           iseed( 2_ilp64 ) = it2
           iseed( 3_ilp64 ) = it3
           iseed( 4_ilp64 ) = it4
           return
     end subroutine stdlib_I64_slaruv

     pure module subroutine stdlib_I64_dlaruv( iseed, n, x )
     !! DLARUV returns a vector of n random real numbers from a uniform (0,1)
     !! distribution (n <= 128).
     !! This is an auxiliary routine called by DLARNV and ZLARNV.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: one
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: n
           ! Array Arguments 
           integer(ilp64), intent(inout) :: iseed(4_ilp64)
           real(dp), intent(out) :: x(n)
        ! =====================================================================
           ! Parameters 
           integer(ilp64), parameter :: lv = 128_ilp64
           integer(ilp64), parameter :: ipw2 = 4096_ilp64
           real(dp), parameter :: r = one/ipw2
           
           
           
           ! Local Scalars 
           integer(ilp64) :: i, i1, i2, i3, i4, it1, it2, it3, it4
           ! Local Arrays 
           integer(ilp64) :: mm(lv,4_ilp64)
           ! Intrinsic Functions 
           ! Data Statements 
           mm(1_ilp64,1_ilp64:4_ilp64)=[494_ilp64,322_ilp64,2508_ilp64,2549_ilp64]
           mm(2_ilp64,1_ilp64:4_ilp64)=[2637_ilp64,789_ilp64,3754_ilp64,1145_ilp64]
           mm(3_ilp64,1_ilp64:4_ilp64)=[255_ilp64,1440_ilp64,1766_ilp64,2253_ilp64]
           mm(4_ilp64,1_ilp64:4_ilp64)=[2008_ilp64,752_ilp64,3572_ilp64,305_ilp64]
           mm(5_ilp64,1_ilp64:4_ilp64)=[1253_ilp64,2859_ilp64,2893_ilp64,3301_ilp64]
           mm(6_ilp64,1_ilp64:4_ilp64)=[3344_ilp64,123_ilp64,307_ilp64,1065_ilp64]
           mm(7_ilp64,1_ilp64:4_ilp64)=[4084_ilp64,1848_ilp64,1297_ilp64,3133_ilp64]
           mm(8_ilp64,1_ilp64:4_ilp64)=[1739_ilp64,643_ilp64,3966_ilp64,2913_ilp64]
           mm(9_ilp64,1_ilp64:4_ilp64)=[3143_ilp64,2405_ilp64,758_ilp64,3285_ilp64]
           mm(10_ilp64,1_ilp64:4_ilp64)=[3468_ilp64,2638_ilp64,2598_ilp64,1241_ilp64]
           mm(11_ilp64,1_ilp64:4_ilp64)=[688_ilp64,2344_ilp64,3406_ilp64,1197_ilp64]
           mm(12_ilp64,1_ilp64:4_ilp64)=[1657_ilp64,46_ilp64,2922_ilp64,3729_ilp64]
           mm(13_ilp64,1_ilp64:4_ilp64)=[1238_ilp64,3814_ilp64,1038_ilp64,2501_ilp64]
           mm(14_ilp64,1_ilp64:4_ilp64)=[3166_ilp64,913_ilp64,2934_ilp64,1673_ilp64]
           mm(15_ilp64,1_ilp64:4_ilp64)=[1292_ilp64,3649_ilp64,2091_ilp64,541_ilp64]
           mm(16_ilp64,1_ilp64:4_ilp64)=[3422_ilp64,339_ilp64,2451_ilp64,2753_ilp64]
           mm(17_ilp64,1_ilp64:4_ilp64)=[1270_ilp64,3808_ilp64,1580_ilp64,949_ilp64]
           mm(18_ilp64,1_ilp64:4_ilp64)=[2016_ilp64,822_ilp64,1958_ilp64,2361_ilp64]
           mm(19_ilp64,1_ilp64:4_ilp64)=[154_ilp64,2832_ilp64,2055_ilp64,1165_ilp64]
           mm(20_ilp64,1_ilp64:4_ilp64)=[2862_ilp64,3078_ilp64,1507_ilp64,4081_ilp64]
           mm(21_ilp64,1_ilp64:4_ilp64)=[697_ilp64,3633_ilp64,1078_ilp64,2725_ilp64]
           mm(22_ilp64,1_ilp64:4_ilp64)=[1706_ilp64,2970_ilp64,3273_ilp64,3305_ilp64]
           mm(23_ilp64,1_ilp64:4_ilp64)=[491_ilp64,637_ilp64,17_ilp64,3069_ilp64]
           mm(24_ilp64,1_ilp64:4_ilp64)=[931_ilp64,2249_ilp64,854_ilp64,3617_ilp64]
           mm(25_ilp64,1_ilp64:4_ilp64)=[1444_ilp64,2081_ilp64,2916_ilp64,3733_ilp64]
           mm(26_ilp64,1_ilp64:4_ilp64)=[444_ilp64,4019_ilp64,3971_ilp64,409_ilp64]
           mm(27_ilp64,1_ilp64:4_ilp64)=[3577_ilp64,1478_ilp64,2889_ilp64,2157_ilp64]
           mm(28_ilp64,1_ilp64:4_ilp64)=[3944_ilp64,242_ilp64,3831_ilp64,1361_ilp64]
           mm(29_ilp64,1_ilp64:4_ilp64)=[2184_ilp64,481_ilp64,2621_ilp64,3973_ilp64]
           mm(30_ilp64,1_ilp64:4_ilp64)=[1661_ilp64,2075_ilp64,1541_ilp64,1865_ilp64]
           mm(31_ilp64,1_ilp64:4_ilp64)=[3482_ilp64,4058_ilp64,893_ilp64,2525_ilp64]
           mm(32_ilp64,1_ilp64:4_ilp64)=[657_ilp64,622_ilp64,736_ilp64,1409_ilp64]
           mm(33_ilp64,1_ilp64:4_ilp64)=[3023_ilp64,3376_ilp64,3992_ilp64,3445_ilp64]
           mm(34_ilp64,1_ilp64:4_ilp64)=[3618_ilp64,812_ilp64,787_ilp64,3577_ilp64]
           mm(35_ilp64,1_ilp64:4_ilp64)=[1267_ilp64,234_ilp64,2125_ilp64,77_ilp64]
           mm(36_ilp64,1_ilp64:4_ilp64)=[1828_ilp64,641_ilp64,2364_ilp64,3761_ilp64]
           mm(37_ilp64,1_ilp64:4_ilp64)=[164_ilp64,4005_ilp64,2460_ilp64,2149_ilp64]
           mm(38_ilp64,1_ilp64:4_ilp64)=[3798_ilp64,1122_ilp64,257_ilp64,1449_ilp64]
           mm(39_ilp64,1_ilp64:4_ilp64)=[3087_ilp64,3135_ilp64,1574_ilp64,3005_ilp64]
           mm(40_ilp64,1_ilp64:4_ilp64)=[2400_ilp64,2640_ilp64,3912_ilp64,225_ilp64]
           mm(41_ilp64,1_ilp64:4_ilp64)=[2870_ilp64,2302_ilp64,1216_ilp64,85_ilp64]
           mm(42_ilp64,1_ilp64:4_ilp64)=[3876_ilp64,40_ilp64,3248_ilp64,3673_ilp64]
           mm(43_ilp64,1_ilp64:4_ilp64)=[1905_ilp64,1832_ilp64,3401_ilp64,3117_ilp64]
           mm(44_ilp64,1_ilp64:4_ilp64)=[1593_ilp64,2247_ilp64,2124_ilp64,3089_ilp64]
           mm(45_ilp64,1_ilp64:4_ilp64)=[1797_ilp64,2034_ilp64,2762_ilp64,1349_ilp64]
           mm(46_ilp64,1_ilp64:4_ilp64)=[1234_ilp64,2637_ilp64,149_ilp64,2057_ilp64]
           mm(47_ilp64,1_ilp64:4_ilp64)=[3460_ilp64,1287_ilp64,2245_ilp64,413_ilp64]
           mm(48_ilp64,1_ilp64:4_ilp64)=[328_ilp64,1691_ilp64,166_ilp64,65_ilp64]
           mm(49_ilp64,1_ilp64:4_ilp64)=[2861_ilp64,496_ilp64,466_ilp64,1845_ilp64]
           mm(50_ilp64,1_ilp64:4_ilp64)=[1950_ilp64,1597_ilp64,4018_ilp64,697_ilp64]
           mm(51_ilp64,1_ilp64:4_ilp64)=[617_ilp64,2394_ilp64,1399_ilp64,3085_ilp64]
           mm(52_ilp64,1_ilp64:4_ilp64)=[2070_ilp64,2584_ilp64,190_ilp64,3441_ilp64]
           mm(53_ilp64,1_ilp64:4_ilp64)=[3331_ilp64,1843_ilp64,2879_ilp64,1573_ilp64]
           mm(54_ilp64,1_ilp64:4_ilp64)=[769_ilp64,336_ilp64,153_ilp64,3689_ilp64]
           mm(55_ilp64,1_ilp64:4_ilp64)=[1558_ilp64,1472_ilp64,2320_ilp64,2941_ilp64]
           mm(56_ilp64,1_ilp64:4_ilp64)=[2412_ilp64,2407_ilp64,18_ilp64,929_ilp64]
           mm(57_ilp64,1_ilp64:4_ilp64)=[2800_ilp64,433_ilp64,712_ilp64,533_ilp64]
           mm(58_ilp64,1_ilp64:4_ilp64)=[189_ilp64,2096_ilp64,2159_ilp64,2841_ilp64]
           mm(59_ilp64,1_ilp64:4_ilp64)=[287_ilp64,1761_ilp64,2318_ilp64,4077_ilp64]
           mm(60_ilp64,1_ilp64:4_ilp64)=[2045_ilp64,2810_ilp64,2091_ilp64,721_ilp64]
           mm(61_ilp64,1_ilp64:4_ilp64)=[1227_ilp64,566_ilp64,3443_ilp64,2821_ilp64]
           mm(62_ilp64,1_ilp64:4_ilp64)=[2838_ilp64,442_ilp64,1510_ilp64,2249_ilp64]
           mm(63_ilp64,1_ilp64:4_ilp64)=[209_ilp64,41_ilp64,449_ilp64,2397_ilp64]
           mm(64_ilp64,1_ilp64:4_ilp64)=[2770_ilp64,1238_ilp64,1956_ilp64,2817_ilp64]
           mm(65_ilp64,1_ilp64:4_ilp64)=[3654_ilp64,1086_ilp64,2201_ilp64,245_ilp64]
           mm(66_ilp64,1_ilp64:4_ilp64)=[3993_ilp64,603_ilp64,3137_ilp64,1913_ilp64]
           mm(67_ilp64,1_ilp64:4_ilp64)=[192_ilp64,840_ilp64,3399_ilp64,1997_ilp64]
           mm(68_ilp64,1_ilp64:4_ilp64)=[2253_ilp64,3168_ilp64,1321_ilp64,3121_ilp64]
           mm(69_ilp64,1_ilp64:4_ilp64)=[3491_ilp64,1499_ilp64,2271_ilp64,997_ilp64]
           mm(70_ilp64,1_ilp64:4_ilp64)=[2889_ilp64,1084_ilp64,3667_ilp64,1833_ilp64]
           mm(71_ilp64,1_ilp64:4_ilp64)=[2857_ilp64,3438_ilp64,2703_ilp64,2877_ilp64]
           mm(72_ilp64,1_ilp64:4_ilp64)=[2094_ilp64,2408_ilp64,629_ilp64,1633_ilp64]
           mm(73_ilp64,1_ilp64:4_ilp64)=[1818_ilp64,1589_ilp64,2365_ilp64,981_ilp64]
           mm(74_ilp64,1_ilp64:4_ilp64)=[688_ilp64,2391_ilp64,2431_ilp64,2009_ilp64]
           mm(75_ilp64,1_ilp64:4_ilp64)=[1407_ilp64,288_ilp64,1113_ilp64,941_ilp64]
           mm(76_ilp64,1_ilp64:4_ilp64)=[634_ilp64,26_ilp64,3922_ilp64,2449_ilp64]
           mm(77_ilp64,1_ilp64:4_ilp64)=[3231_ilp64,512_ilp64,2554_ilp64,197_ilp64]
           mm(78_ilp64,1_ilp64:4_ilp64)=[815_ilp64,1456_ilp64,184_ilp64,2441_ilp64]
           mm(79_ilp64,1_ilp64:4_ilp64)=[3524_ilp64,171_ilp64,2099_ilp64,285_ilp64]
           mm(80_ilp64,1_ilp64:4_ilp64)=[1914_ilp64,1677_ilp64,3228_ilp64,1473_ilp64]
           mm(81_ilp64,1_ilp64:4_ilp64)=[516_ilp64,2657_ilp64,4012_ilp64,2741_ilp64]
           mm(82_ilp64,1_ilp64:4_ilp64)=[164_ilp64,2270_ilp64,1921_ilp64,3129_ilp64]
           mm(83_ilp64,1_ilp64:4_ilp64)=[303_ilp64,2587_ilp64,3452_ilp64,909_ilp64]
           mm(84_ilp64,1_ilp64:4_ilp64)=[2144_ilp64,2961_ilp64,3901_ilp64,2801_ilp64]
           mm(85_ilp64,1_ilp64:4_ilp64)=[3480_ilp64,1970_ilp64,572_ilp64,421_ilp64]
           mm(86_ilp64,1_ilp64:4_ilp64)=[119_ilp64,1817_ilp64,3309_ilp64,4073_ilp64]
           mm(87_ilp64,1_ilp64:4_ilp64)=[3357_ilp64,676_ilp64,3171_ilp64,2813_ilp64]
           mm(88_ilp64,1_ilp64:4_ilp64)=[837_ilp64,1410_ilp64,817_ilp64,2337_ilp64]
           mm(89_ilp64,1_ilp64:4_ilp64)=[2826_ilp64,3723_ilp64,3039_ilp64,1429_ilp64]
           mm(90_ilp64,1_ilp64:4_ilp64)=[2332_ilp64,2803_ilp64,1696_ilp64,1177_ilp64]
           mm(91_ilp64,1_ilp64:4_ilp64)=[2089_ilp64,3185_ilp64,1256_ilp64,1901_ilp64]
           mm(92_ilp64,1_ilp64:4_ilp64)=[3780_ilp64,184_ilp64,3715_ilp64,81_ilp64]
           mm(93_ilp64,1_ilp64:4_ilp64)=[1700_ilp64,663_ilp64,2077_ilp64,1669_ilp64]
           mm(94_ilp64,1_ilp64:4_ilp64)=[3712_ilp64,499_ilp64,3019_ilp64,2633_ilp64]
           mm(95_ilp64,1_ilp64:4_ilp64)=[150_ilp64,3784_ilp64,1497_ilp64,2269_ilp64]
           mm(96_ilp64,1_ilp64:4_ilp64)=[2000_ilp64,1631_ilp64,1101_ilp64,129_ilp64]
           mm(97_ilp64,1_ilp64:4_ilp64)=[3375_ilp64,1925_ilp64,717_ilp64,1141_ilp64]
           mm(98_ilp64,1_ilp64:4_ilp64)=[1621_ilp64,3912_ilp64,51_ilp64,249_ilp64]
           mm(99_ilp64,1_ilp64:4_ilp64)=[3090_ilp64,1398_ilp64,981_ilp64,3917_ilp64]
           mm(100_ilp64,1_ilp64:4_ilp64)=[3765_ilp64,1349_ilp64,1978_ilp64,2481_ilp64]
           mm(101_ilp64,1_ilp64:4_ilp64)=[1149_ilp64,1441_ilp64,1813_ilp64,3941_ilp64]
           mm(102_ilp64,1_ilp64:4_ilp64)=[3146_ilp64,2224_ilp64,3881_ilp64,2217_ilp64]
           mm(103_ilp64,1_ilp64:4_ilp64)=[33_ilp64,2411_ilp64,76_ilp64,2749_ilp64]
           mm(104_ilp64,1_ilp64:4_ilp64)=[3082_ilp64,1907_ilp64,3846_ilp64,3041_ilp64]
           mm(105_ilp64,1_ilp64:4_ilp64)=[2741_ilp64,3192_ilp64,3694_ilp64,1877_ilp64]
           mm(106_ilp64,1_ilp64:4_ilp64)=[359_ilp64,2786_ilp64,1682_ilp64,345_ilp64]
           mm(107_ilp64,1_ilp64:4_ilp64)=[3316_ilp64,382_ilp64,124_ilp64,2861_ilp64]
           mm(108_ilp64,1_ilp64:4_ilp64)=[1749_ilp64,37_ilp64,1660_ilp64,1809_ilp64]
           mm(109_ilp64,1_ilp64:4_ilp64)=[185_ilp64,759_ilp64,3997_ilp64,3141_ilp64]
           mm(110_ilp64,1_ilp64:4_ilp64)=[2784_ilp64,2948_ilp64,479_ilp64,2825_ilp64]
           mm(111_ilp64,1_ilp64:4_ilp64)=[2202_ilp64,1862_ilp64,1141_ilp64,157_ilp64]
           mm(112_ilp64,1_ilp64:4_ilp64)=[2199_ilp64,3802_ilp64,886_ilp64,2881_ilp64]
           mm(113_ilp64,1_ilp64:4_ilp64)=[1364_ilp64,2423_ilp64,3514_ilp64,3637_ilp64]
           mm(114_ilp64,1_ilp64:4_ilp64)=[1244_ilp64,2051_ilp64,1301_ilp64,1465_ilp64]
           mm(115_ilp64,1_ilp64:4_ilp64)=[2020_ilp64,2295_ilp64,3604_ilp64,2829_ilp64]
           mm(116_ilp64,1_ilp64:4_ilp64)=[3160_ilp64,1332_ilp64,1888_ilp64,2161_ilp64]
           mm(117_ilp64,1_ilp64:4_ilp64)=[2785_ilp64,1832_ilp64,1836_ilp64,3365_ilp64]
           mm(118_ilp64,1_ilp64:4_ilp64)=[2772_ilp64,2405_ilp64,1990_ilp64,361_ilp64]
           mm(119_ilp64,1_ilp64:4_ilp64)=[1217_ilp64,3638_ilp64,2058_ilp64,2685_ilp64]
           mm(120_ilp64,1_ilp64:4_ilp64)=[1822_ilp64,3661_ilp64,692_ilp64,3745_ilp64]
           mm(121_ilp64,1_ilp64:4_ilp64)=[1245_ilp64,327_ilp64,1194_ilp64,2325_ilp64]
           mm(122_ilp64,1_ilp64:4_ilp64)=[2252_ilp64,3660_ilp64,20_ilp64,3609_ilp64]
           mm(123_ilp64,1_ilp64:4_ilp64)=[3904_ilp64,716_ilp64,3285_ilp64,3821_ilp64]
           mm(124_ilp64,1_ilp64:4_ilp64)=[2774_ilp64,1842_ilp64,2046_ilp64,3537_ilp64]
           mm(125_ilp64,1_ilp64:4_ilp64)=[997_ilp64,3987_ilp64,2107_ilp64,517_ilp64]
           mm(126_ilp64,1_ilp64:4_ilp64)=[2573_ilp64,1368_ilp64,3508_ilp64,3017_ilp64]
           mm(127_ilp64,1_ilp64:4_ilp64)=[1148_ilp64,1848_ilp64,3525_ilp64,2141_ilp64]
           mm(128_ilp64,1_ilp64:4_ilp64)=[545_ilp64,2366_ilp64,3801_ilp64,1537_ilp64]
           ! Executable Statements 
           i1 = iseed( 1_ilp64 )
           i2 = iseed( 2_ilp64 )
           i3 = iseed( 3_ilp64 )
           i4 = iseed( 4_ilp64 )
           loop_10: do i = 1, min( n, lv )
           20 continue
              ! multiply the seed by i-th power of the multiplier modulo 2**48
              it4 = i4*mm( i, 4_ilp64 )
              it3 = it4 / ipw2
              it4 = it4 - ipw2*it3
              it3 = it3 + i3*mm( i, 4_ilp64 ) + i4*mm( i, 3_ilp64 )
              it2 = it3 / ipw2
              it3 = it3 - ipw2*it2
              it2 = it2 + i2*mm( i, 4_ilp64 ) + i3*mm( i, 3_ilp64 ) + i4*mm( i, 2_ilp64 )
              it1 = it2 / ipw2
              it2 = it2 - ipw2*it1
              it1 = it1 + i1*mm( i, 4_ilp64 ) + i2*mm( i, 3_ilp64 ) + i3*mm( i, 2_ilp64 ) +i4*mm( i, 1_ilp64 )
              it1 = mod( it1, ipw2 )
              ! convert 48-bit integer to a realnumber in the interval (0,1,KIND=dp)
              x( i ) = r*( real( it1,KIND=dp)+r*( real( it2,KIND=dp)+r*( real( it3,KIND=dp)+&
                        r*real( it4,KIND=dp) ) ) )
              if (x( i )==one) then
                 ! if a real number has n bits of precision, and the first
                 ! n bits of the 48-bit integer above happen to be all 1 (which
                 ! will occur about once every 2**n calls), then x( i ) will
                 ! be rounded to exactly one.
                 ! since x( i ) is not supposed to return exactly 0.0_dp or one,
                 ! the statistically correct thing to do in this situation is
                 ! simply to iterate again.
                 ! n.b. the case x( i ) = 0.0_dp should not be possible.
                 i1 = i1 + 2_ilp64
                 i2 = i2 + 2_ilp64
                 i3 = i3 + 2_ilp64
                 i4 = i4 + 2_ilp64
                 goto 20
              end if
           end do loop_10
           ! return final value of seed
           iseed( 1_ilp64 ) = it1
           iseed( 2_ilp64 ) = it2
           iseed( 3_ilp64 ) = it3
           iseed( 4_ilp64 ) = it4
           return
     end subroutine stdlib_I64_dlaruv




     pure module subroutine stdlib_I64_slacpy( uplo, m, n, a, lda, b, ldb )
     !! SLACPY copies all or part of a two-dimensional matrix A to another
     !! matrix B.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(in) :: lda, ldb, m, n
           ! Array Arguments 
           real(sp), intent(in) :: a(lda,*)
           real(sp), intent(out) :: b(ldb,*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp64) :: i, j
           ! Intrinsic Functions 
           ! Executable Statements 
           if( stdlib_lsame( uplo, 'U' ) ) then
              do j = 1, n
                 do i = 1, min( j, m )
                    b( i, j ) = a( i, j )
                 end do
              end do
           else if( stdlib_lsame( uplo, 'L' ) ) then
              do j = 1, n
                 do i = j, m
                    b( i, j ) = a( i, j )
                 end do
              end do
           else
              do j = 1, n
                 do i = 1, m
                    b( i, j ) = a( i, j )
                 end do
              end do
           end if
           return
     end subroutine stdlib_I64_slacpy

     pure module subroutine stdlib_I64_dlacpy( uplo, m, n, a, lda, b, ldb )
     !! DLACPY copies all or part of a two-dimensional matrix A to another
     !! matrix B.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(in) :: lda, ldb, m, n
           ! Array Arguments 
           real(dp), intent(in) :: a(lda,*)
           real(dp), intent(out) :: b(ldb,*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp64) :: i, j
           ! Intrinsic Functions 
           ! Executable Statements 
           if( stdlib_lsame( uplo, 'U' ) ) then
              do j = 1, n
                 do i = 1, min( j, m )
                    b( i, j ) = a( i, j )
                 end do
              end do
           else if( stdlib_lsame( uplo, 'L' ) ) then
              do j = 1, n
                 do i = j, m
                    b( i, j ) = a( i, j )
                 end do
              end do
           else
              do j = 1, n
                 do i = 1, m
                    b( i, j ) = a( i, j )
                 end do
              end do
           end if
           return
     end subroutine stdlib_I64_dlacpy


     pure module subroutine stdlib_I64_clacpy( uplo, m, n, a, lda, b, ldb )
     !! CLACPY copies all or part of a two-dimensional matrix A to another
     !! matrix B.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(in) :: lda, ldb, m, n
           ! Array Arguments 
           complex(sp), intent(in) :: a(lda,*)
           complex(sp), intent(out) :: b(ldb,*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp64) :: i, j
           ! Intrinsic Functions 
           ! Executable Statements 
           if( stdlib_lsame( uplo, 'U' ) ) then
              do j = 1, n
                 do i = 1, min( j, m )
                    b( i, j ) = a( i, j )
                 end do
              end do
           else if( stdlib_lsame( uplo, 'L' ) ) then
              do j = 1, n
                 do i = j, m
                    b( i, j ) = a( i, j )
                 end do
              end do
           else
              do j = 1, n
                 do i = 1, m
                    b( i, j ) = a( i, j )
                 end do
              end do
           end if
           return
     end subroutine stdlib_I64_clacpy

     pure module subroutine stdlib_I64_zlacpy( uplo, m, n, a, lda, b, ldb )
     !! ZLACPY copies all or part of a two-dimensional matrix A to another
     !! matrix B.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(in) :: lda, ldb, m, n
           ! Array Arguments 
           complex(dp), intent(in) :: a(lda,*)
           complex(dp), intent(out) :: b(ldb,*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp64) :: i, j
           ! Intrinsic Functions 
           ! Executable Statements 
           if( stdlib_lsame( uplo, 'U' ) ) then
              do j = 1, n
                 do i = 1, min( j, m )
                    b( i, j ) = a( i, j )
                 end do
              end do
           else if( stdlib_lsame( uplo, 'L' ) ) then
              do j = 1, n
                 do i = j, m
                    b( i, j ) = a( i, j )
                 end do
              end do
           else
              do j = 1, n
                 do i = 1, m
                    b( i, j ) = a( i, j )
                 end do
              end do
           end if
           return
     end subroutine stdlib_I64_zlacpy




     pure module subroutine stdlib_I64_clacp2( uplo, m, n, a, lda, b, ldb )
     !! CLACP2 copies all or part of a real two-dimensional matrix A to a
     !! complex matrix B.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(in) :: lda, ldb, m, n
           ! Array Arguments 
           real(sp), intent(in) :: a(lda,*)
           complex(sp), intent(out) :: b(ldb,*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp64) :: i, j
           ! Intrinsic Functions 
           ! Executable Statements 
           if( stdlib_lsame( uplo, 'U' ) ) then
              do j = 1, n
                 do i = 1, min( j, m )
                    b( i, j ) = a( i, j )
                 end do
              end do
           else if( stdlib_lsame( uplo, 'L' ) ) then
              do j = 1, n
                 do i = j, m
                    b( i, j ) = a( i, j )
                 end do
              end do
           else
              do j = 1, n
                 do i = 1, m
                    b( i, j ) = a( i, j )
                 end do
              end do
           end if
           return
     end subroutine stdlib_I64_clacp2

     pure module subroutine stdlib_I64_zlacp2( uplo, m, n, a, lda, b, ldb )
     !! ZLACP2 copies all or part of a real two-dimensional matrix A to a
     !! complex matrix B.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(in) :: lda, ldb, m, n
           ! Array Arguments 
           real(dp), intent(in) :: a(lda,*)
           complex(dp), intent(out) :: b(ldb,*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp64) :: i, j
           ! Intrinsic Functions 
           ! Executable Statements 
           if( stdlib_lsame( uplo, 'U' ) ) then
              do j = 1, n
                 do i = 1, min( j, m )
                    b( i, j ) = a( i, j )
                 end do
              end do
           else if( stdlib_lsame( uplo, 'L' ) ) then
              do j = 1, n
                 do i = j, m
                    b( i, j ) = a( i, j )
                 end do
              end do
           else
              do j = 1, n
                 do i = 1, m
                    b( i, j ) = a( i, j )
                 end do
              end do
           end if
           return
     end subroutine stdlib_I64_zlacp2




     pure module subroutine stdlib_I64_stfttp( transr, uplo, n, arf, ap, info )
     !! STFTTP copies a triangular matrix A from rectangular full packed
     !! format (TF) to standard packed format (TP).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character, intent(in) :: transr, uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: n
           ! Array Arguments 
           real(sp), intent(out) :: ap(0_ilp64:*)
           real(sp), intent(in) :: arf(0_ilp64:*)
        ! =====================================================================
           ! Parameters 
           ! Local Scalars 
           logical(lk) :: lower, nisodd, normaltransr
           integer(ilp64) :: n1, n2, k, nt
           integer(ilp64) :: i, j, ij
           integer(ilp64) :: ijp, jp, lda, js
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           normaltransr = stdlib_lsame( transr, 'N' )
           lower = stdlib_lsame( uplo, 'L' )
           if( .not.normaltransr .and. .not.stdlib_lsame( transr, 'T' ) ) then
              info = -1_ilp64
           else if( .not.lower .and. .not.stdlib_lsame( uplo, 'U' ) ) then
              info = -2_ilp64
           else if( n<0_ilp64 ) then
              info = -3_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'STFTTP', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( n==1_ilp64 ) then
              if( normaltransr ) then
                 ap( 0_ilp64 ) = arf( 0_ilp64 )
              else
                 ap( 0_ilp64 ) = arf( 0_ilp64 )
              end if
              return
           end if
           ! size of array arf(0:nt-1)
           nt = n*( n+1 ) / 2_ilp64
           ! set n1 and n2 depending on lower
           if( lower ) then
              n2 = n / 2_ilp64
              n1 = n - n2
           else
              n1 = n / 2_ilp64
              n2 = n - n1
           end if
           ! if n is odd, set nisodd = .true.
           ! if n is even, set k = n/2 and nisodd = .false.
           ! set lda of arf^c; arf^c is (0:(n+1)/2-1,0:n-noe)
           ! where noe = 0 if n is even, noe = 1 if n is odd
           if( mod( n, 2_ilp64 )==0_ilp64 ) then
              k = n / 2_ilp64
              nisodd = .false.
              lda = n + 1_ilp64
           else
              nisodd = .true.
              lda = n
           end if
           ! arf^c has lda rows and n+1-noe cols
           if( .not.normaltransr )lda = ( n+1 ) / 2_ilp64
           ! start execution: there are eight cases
           if( nisodd ) then
              ! n is odd
              if( normaltransr ) then
                 ! n is odd and transr = 'n'
                 if( lower ) then
                   ! srpa for lower, normal and n is odd ( a(0:n-1,0:n1-1) )
                   ! t1 -> a(0,0), t2 -> a(0,1), s -> a(n1,0)
                   ! t1 -> a(0), t2 -> a(n), s -> a(n1); lda = n
                    ijp = 0_ilp64
                    jp = 0_ilp64
                    do j = 0, n2
                       do i = j, n - 1
                          ij = i + jp
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp64
                       end do
                       jp = jp + lda
                    end do
                    do i = 0, n2 - 1
                       do j = 1 + i, n2
                          ij = i + j*lda
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp64
                       end do
                    end do
                 else
                   ! srpa for upper, normal and n is odd ( a(0:n-1,0:n2-1)
                   ! t1 -> a(n1+1,0), t2 -> a(n1,0), s -> a(0,0)
                   ! t1 -> a(n2), t2 -> a(n1), s -> a(0)
                    ijp = 0_ilp64
                    do j = 0, n1 - 1
                       ij = n2 + j
                       do i = 0, j
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp64
                          ij = ij + lda
                       end do
                    end do
                    js = 0_ilp64
                    do j = n1, n - 1
                       ij = js
                       do ij = js, js + j
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp64
                       end do
                       js = js + lda
                    end do
                 end if
              else
                 ! n is odd and transr = 't'
                 if( lower ) then
                    ! srpa for lower, transpose and n is odd
                    ! t1 -> a(0,0) , t2 -> a(1,0) , s -> a(0,n1)
                    ! t1 -> a(0+0) , t2 -> a(1+0) , s -> a(0+n1*n1); lda=n1
                    ijp = 0_ilp64
                    do i = 0, n2
                       do ij = i*( lda+1 ), n*lda - 1, lda
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp64
                       end do
                    end do
                    js = 1_ilp64
                    do j = 0, n2 - 1
                       do ij = js, js + n2 - j - 1
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp64
                       end do
                       js = js + lda + 1_ilp64
                    end do
                 else
                    ! srpa for upper, transpose and n is odd
                    ! t1 -> a(0,n1+1), t2 -> a(0,n1), s -> a(0,0)
                    ! t1 -> a(n2*n2), t2 -> a(n1*n2), s -> a(0); lda = n2
                    ijp = 0_ilp64
                    js = n2*lda
                    do j = 0, n1 - 1
                       do ij = js, js + j
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp64
                       end do
                       js = js + lda
                    end do
                    do i = 0, n1
                       do ij = i, i + ( n1+i )*lda, lda
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp64
                       end do
                    end do
                 end if
              end if
           else
              ! n is even
              if( normaltransr ) then
                 ! n is even and transr = 'n'
                 if( lower ) then
                    ! srpa for lower, normal, and n is even ( a(0:n,0:k-1) )
                    ! t1 -> a(1,0), t2 -> a(0,0), s -> a(k+1,0)
                    ! t1 -> a(1), t2 -> a(0), s -> a(k+1)
                    ijp = 0_ilp64
                    jp = 0_ilp64
                    do j = 0, k - 1
                       do i = j, n - 1
                          ij = 1_ilp64 + i + jp
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp64
                       end do
                       jp = jp + lda
                    end do
                    do i = 0, k - 1
                       do j = i, k - 1
                          ij = i + j*lda
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp64
                       end do
                    end do
                 else
                    ! srpa for upper, normal, and n is even ( a(0:n,0:k-1) )
                    ! t1 -> a(k+1,0) ,  t2 -> a(k,0),   s -> a(0,0)
                    ! t1 -> a(k+1), t2 -> a(k), s -> a(0)
                    ijp = 0_ilp64
                    do j = 0, k - 1
                       ij = k + 1_ilp64 + j
                       do i = 0, j
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp64
                          ij = ij + lda
                       end do
                    end do
                    js = 0_ilp64
                    do j = k, n - 1
                       ij = js
                       do ij = js, js + j
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp64
                       end do
                       js = js + lda
                    end do
                 end if
              else
                 ! n is even and transr = 't'
                 if( lower ) then
                    ! srpa for lower, transpose and n is even (see paper)
                    ! t1 -> b(0,1), t2 -> b(0,0), s -> b(0,k+1)
                    ! t1 -> a(0+k), t2 -> a(0+0), s -> a(0+k*(k+1)); lda=k
                    ijp = 0_ilp64
                    do i = 0, k - 1
                       do ij = i + ( i+1 )*lda, ( n+1 )*lda - 1, lda
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp64
                       end do
                    end do
                    js = 0_ilp64
                    do j = 0, k - 1
                       do ij = js, js + k - j - 1
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp64
                       end do
                       js = js + lda + 1_ilp64
                    end do
                 else
                    ! srpa for upper, transpose and n is even (see paper)
                    ! t1 -> b(0,k+1),     t2 -> b(0,k),   s -> b(0,0)
                    ! t1 -> a(0+k*(k+1)), t2 -> a(0+k*k), s -> a(0+0)); lda=k
                    ijp = 0_ilp64
                    js = ( k+1 )*lda
                    do j = 0, k - 1
                       do ij = js, js + j
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp64
                       end do
                       js = js + lda
                    end do
                    do i = 0, k - 1
                       do ij = i, i + ( k+i )*lda, lda
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp64
                       end do
                    end do
                 end if
              end if
           end if
           return
     end subroutine stdlib_I64_stfttp

     pure module subroutine stdlib_I64_dtfttp( transr, uplo, n, arf, ap, info )
     !! DTFTTP copies a triangular matrix A from rectangular full packed
     !! format (TF) to standard packed format (TP).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character, intent(in) :: transr, uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: n
           ! Array Arguments 
           real(dp), intent(out) :: ap(0_ilp64:*)
           real(dp), intent(in) :: arf(0_ilp64:*)
        ! =====================================================================
           ! Parameters 
           ! Local Scalars 
           logical(lk) :: lower, nisodd, normaltransr
           integer(ilp64) :: n1, n2, k, nt
           integer(ilp64) :: i, j, ij
           integer(ilp64) :: ijp, jp, lda, js
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           normaltransr = stdlib_lsame( transr, 'N' )
           lower = stdlib_lsame( uplo, 'L' )
           if( .not.normaltransr .and. .not.stdlib_lsame( transr, 'T' ) ) then
              info = -1_ilp64
           else if( .not.lower .and. .not.stdlib_lsame( uplo, 'U' ) ) then
              info = -2_ilp64
           else if( n<0_ilp64 ) then
              info = -3_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'DTFTTP', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( n==1_ilp64 ) then
              if( normaltransr ) then
                 ap( 0_ilp64 ) = arf( 0_ilp64 )
              else
                 ap( 0_ilp64 ) = arf( 0_ilp64 )
              end if
              return
           end if
           ! size of array arf(0:nt-1)
           nt = n*( n+1 ) / 2_ilp64
           ! set n1 and n2 depending on lower
           if( lower ) then
              n2 = n / 2_ilp64
              n1 = n - n2
           else
              n1 = n / 2_ilp64
              n2 = n - n1
           end if
           ! if n is odd, set nisodd = .true.
           ! if n is even, set k = n/2 and nisodd = .false.
           ! set lda of arf^c; arf^c is (0:(n+1)/2-1,0:n-noe)
           ! where noe = 0 if n is even, noe = 1 if n is odd
           if( mod( n, 2_ilp64 )==0_ilp64 ) then
              k = n / 2_ilp64
              nisodd = .false.
              lda = n + 1_ilp64
           else
              nisodd = .true.
              lda = n
           end if
           ! arf^c has lda rows and n+1-noe cols
           if( .not.normaltransr )lda = ( n+1 ) / 2_ilp64
           ! start execution: there are eight cases
           if( nisodd ) then
              ! n is odd
              if( normaltransr ) then
                 ! n is odd and transr = 'n'
                 if( lower ) then
                   ! srpa for lower, normal and n is odd ( a(0:n-1,0:n1-1) )
                   ! t1 -> a(0,0), t2 -> a(0,1), s -> a(n1,0)
                   ! t1 -> a(0), t2 -> a(n), s -> a(n1); lda = n
                    ijp = 0_ilp64
                    jp = 0_ilp64
                    do j = 0, n2
                       do i = j, n - 1
                          ij = i + jp
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp64
                       end do
                       jp = jp + lda
                    end do
                    do i = 0, n2 - 1
                       do j = 1 + i, n2
                          ij = i + j*lda
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp64
                       end do
                    end do
                 else
                   ! srpa for upper, normal and n is odd ( a(0:n-1,0:n2-1)
                   ! t1 -> a(n1+1,0), t2 -> a(n1,0), s -> a(0,0)
                   ! t1 -> a(n2), t2 -> a(n1), s -> a(0)
                    ijp = 0_ilp64
                    do j = 0, n1 - 1
                       ij = n2 + j
                       do i = 0, j
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp64
                          ij = ij + lda
                       end do
                    end do
                    js = 0_ilp64
                    do j = n1, n - 1
                       ij = js
                       do ij = js, js + j
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp64
                       end do
                       js = js + lda
                    end do
                 end if
              else
                 ! n is odd and transr = 't'
                 if( lower ) then
                    ! srpa for lower, transpose and n is odd
                    ! t1 -> a(0,0) , t2 -> a(1,0) , s -> a(0,n1)
                    ! t1 -> a(0+0) , t2 -> a(1+0) , s -> a(0+n1*n1); lda=n1
                    ijp = 0_ilp64
                    do i = 0, n2
                       do ij = i*( lda+1 ), n*lda - 1, lda
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp64
                       end do
                    end do
                    js = 1_ilp64
                    do j = 0, n2 - 1
                       do ij = js, js + n2 - j - 1
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp64
                       end do
                       js = js + lda + 1_ilp64
                    end do
                 else
                    ! srpa for upper, transpose and n is odd
                    ! t1 -> a(0,n1+1), t2 -> a(0,n1), s -> a(0,0)
                    ! t1 -> a(n2*n2), t2 -> a(n1*n2), s -> a(0); lda = n2
                    ijp = 0_ilp64
                    js = n2*lda
                    do j = 0, n1 - 1
                       do ij = js, js + j
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp64
                       end do
                       js = js + lda
                    end do
                    do i = 0, n1
                       do ij = i, i + ( n1+i )*lda, lda
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp64
                       end do
                    end do
                 end if
              end if
           else
              ! n is even
              if( normaltransr ) then
                 ! n is even and transr = 'n'
                 if( lower ) then
                    ! srpa for lower, normal, and n is even ( a(0:n,0:k-1) )
                    ! t1 -> a(1,0), t2 -> a(0,0), s -> a(k+1,0)
                    ! t1 -> a(1), t2 -> a(0), s -> a(k+1)
                    ijp = 0_ilp64
                    jp = 0_ilp64
                    do j = 0, k - 1
                       do i = j, n - 1
                          ij = 1_ilp64 + i + jp
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp64
                       end do
                       jp = jp + lda
                    end do
                    do i = 0, k - 1
                       do j = i, k - 1
                          ij = i + j*lda
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp64
                       end do
                    end do
                 else
                    ! srpa for upper, normal, and n is even ( a(0:n,0:k-1) )
                    ! t1 -> a(k+1,0) ,  t2 -> a(k,0),   s -> a(0,0)
                    ! t1 -> a(k+1), t2 -> a(k), s -> a(0)
                    ijp = 0_ilp64
                    do j = 0, k - 1
                       ij = k + 1_ilp64 + j
                       do i = 0, j
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp64
                          ij = ij + lda
                       end do
                    end do
                    js = 0_ilp64
                    do j = k, n - 1
                       ij = js
                       do ij = js, js + j
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp64
                       end do
                       js = js + lda
                    end do
                 end if
              else
                 ! n is even and transr = 't'
                 if( lower ) then
                    ! srpa for lower, transpose and n is even (see paper)
                    ! t1 -> b(0,1), t2 -> b(0,0), s -> b(0,k+1)
                    ! t1 -> a(0+k), t2 -> a(0+0), s -> a(0+k*(k+1)); lda=k
                    ijp = 0_ilp64
                    do i = 0, k - 1
                       do ij = i + ( i+1 )*lda, ( n+1 )*lda - 1, lda
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp64
                       end do
                    end do
                    js = 0_ilp64
                    do j = 0, k - 1
                       do ij = js, js + k - j - 1
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp64
                       end do
                       js = js + lda + 1_ilp64
                    end do
                 else
                    ! srpa for upper, transpose and n is even (see paper)
                    ! t1 -> b(0,k+1),     t2 -> b(0,k),   s -> b(0,0)
                    ! t1 -> a(0+k*(k+1)), t2 -> a(0+k*k), s -> a(0+0)); lda=k
                    ijp = 0_ilp64
                    js = ( k+1 )*lda
                    do j = 0, k - 1
                       do ij = js, js + j
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp64
                       end do
                       js = js + lda
                    end do
                    do i = 0, k - 1
                       do ij = i, i + ( k+i )*lda, lda
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp64
                       end do
                    end do
                 end if
              end if
           end if
           return
     end subroutine stdlib_I64_dtfttp


     pure module subroutine stdlib_I64_ctfttp( transr, uplo, n, arf, ap, info )
     !! CTFTTP copies a triangular matrix A from rectangular full packed
     !! format (TF) to standard packed format (TP).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character, intent(in) :: transr, uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: n
           ! Array Arguments 
           complex(sp), intent(out) :: ap(0_ilp64:*)
           complex(sp), intent(in) :: arf(0_ilp64:*)
        ! =====================================================================
           ! Parameters 
           ! Local Scalars 
           logical(lk) :: lower, nisodd, normaltransr
           integer(ilp64) :: n1, n2, k, nt
           integer(ilp64) :: i, j, ij
           integer(ilp64) :: ijp, jp, lda, js
           ! Intrinsic Functions 
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           normaltransr = stdlib_lsame( transr, 'N' )
           lower = stdlib_lsame( uplo, 'L' )
           if( .not.normaltransr .and. .not.stdlib_lsame( transr, 'C' ) ) then
              info = -1_ilp64
           else if( .not.lower .and. .not.stdlib_lsame( uplo, 'U' ) ) then
              info = -2_ilp64
           else if( n<0_ilp64 ) then
              info = -3_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'CTFTTP', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( n==1_ilp64 ) then
              if( normaltransr ) then
                 ap( 0_ilp64 ) = arf( 0_ilp64 )
              else
                 ap( 0_ilp64 ) = conjg( arf( 0_ilp64 ) )
              end if
              return
           end if
           ! size of array arf(0:nt-1)
           nt = n*( n+1 ) / 2_ilp64
           ! set n1 and n2 depending on lower
           if( lower ) then
              n2 = n / 2_ilp64
              n1 = n - n2
           else
              n1 = n / 2_ilp64
              n2 = n - n1
           end if
           ! if n is odd, set nisodd = .true.
           ! if n is even, set k = n/2 and nisodd = .false.
           ! set lda of arf^c; arf^c is (0:(n+1)/2-1,0:n-noe)
           ! where noe = 0 if n is even, noe = 1 if n is odd
           if( mod( n, 2_ilp64 )==0_ilp64 ) then
              k = n / 2_ilp64
              nisodd = .false.
              lda = n + 1_ilp64
           else
              nisodd = .true.
              lda = n
           end if
           ! arf^c has lda rows and n+1-noe cols
           if( .not.normaltransr )lda = ( n+1 ) / 2_ilp64
           ! start execution: there are eight cases
           if( nisodd ) then
              ! n is odd
              if( normaltransr ) then
                 ! n is odd and transr = 'n'
                 if( lower ) then
                   ! srpa for lower, normal and n is odd ( a(0:n-1,0:n1-1) )
                   ! t1 -> a(0,0), t2 -> a(0,1), s -> a(n1,0)
                   ! t1 -> a(0), t2 -> a(n), s -> a(n1); lda = n
                    ijp = 0_ilp64
                    jp = 0_ilp64
                    do j = 0, n2
                       do i = j, n - 1
                          ij = i + jp
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp64
                       end do
                       jp = jp + lda
                    end do
                    do i = 0, n2 - 1
                       do j = 1 + i, n2
                          ij = i + j*lda
                          ap( ijp ) = conjg( arf( ij ) )
                          ijp = ijp + 1_ilp64
                       end do
                    end do
                 else
                   ! srpa for upper, normal and n is odd ( a(0:n-1,0:n2-1)
                   ! t1 -> a(n1+1,0), t2 -> a(n1,0), s -> a(0,0)
                   ! t1 -> a(n2), t2 -> a(n1), s -> a(0)
                    ijp = 0_ilp64
                    do j = 0, n1 - 1
                       ij = n2 + j
                       do i = 0, j
                          ap( ijp ) = conjg( arf( ij ) )
                          ijp = ijp + 1_ilp64
                          ij = ij + lda
                       end do
                    end do
                    js = 0_ilp64
                    do j = n1, n - 1
                       ij = js
                       do ij = js, js + j
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp64
                       end do
                       js = js + lda
                    end do
                 end if
              else
                 ! n is odd and transr = 'c'
                 if( lower ) then
                    ! srpa for lower, transpose and n is odd
                    ! t1 -> a(0,0) , t2 -> a(1,0) , s -> a(0,n1)
                    ! t1 -> a(0+0) , t2 -> a(1+0) , s -> a(0+n1*n1); lda=n1
                    ijp = 0_ilp64
                    do i = 0, n2
                       do ij = i*( lda+1 ), n*lda - 1, lda
                          ap( ijp ) = conjg( arf( ij ) )
                          ijp = ijp + 1_ilp64
                       end do
                    end do
                    js = 1_ilp64
                    do j = 0, n2 - 1
                       do ij = js, js + n2 - j - 1
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp64
                       end do
                       js = js + lda + 1_ilp64
                    end do
                 else
                    ! srpa for upper, transpose and n is odd
                    ! t1 -> a(0,n1+1), t2 -> a(0,n1), s -> a(0,0)
                    ! t1 -> a(n2*n2), t2 -> a(n1*n2), s -> a(0); lda = n2
                    ijp = 0_ilp64
                    js = n2*lda
                    do j = 0, n1 - 1
                       do ij = js, js + j
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp64
                       end do
                       js = js + lda
                    end do
                    do i = 0, n1
                       do ij = i, i + ( n1+i )*lda, lda
                          ap( ijp ) = conjg( arf( ij ) )
                          ijp = ijp + 1_ilp64
                       end do
                    end do
                 end if
              end if
           else
              ! n is even
              if( normaltransr ) then
                 ! n is even and transr = 'n'
                 if( lower ) then
                    ! srpa for lower, normal, and n is even ( a(0:n,0:k-1) )
                    ! t1 -> a(1,0), t2 -> a(0,0), s -> a(k+1,0)
                    ! t1 -> a(1), t2 -> a(0), s -> a(k+1)
                    ijp = 0_ilp64
                    jp = 0_ilp64
                    do j = 0, k - 1
                       do i = j, n - 1
                          ij = 1_ilp64 + i + jp
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp64
                       end do
                       jp = jp + lda
                    end do
                    do i = 0, k - 1
                       do j = i, k - 1
                          ij = i + j*lda
                          ap( ijp ) = conjg( arf( ij ) )
                          ijp = ijp + 1_ilp64
                       end do
                    end do
                 else
                    ! srpa for upper, normal, and n is even ( a(0:n,0:k-1) )
                    ! t1 -> a(k+1,0) ,  t2 -> a(k,0),   s -> a(0,0)
                    ! t1 -> a(k+1), t2 -> a(k), s -> a(0)
                    ijp = 0_ilp64
                    do j = 0, k - 1
                       ij = k + 1_ilp64 + j
                       do i = 0, j
                          ap( ijp ) = conjg( arf( ij ) )
                          ijp = ijp + 1_ilp64
                          ij = ij + lda
                       end do
                    end do
                    js = 0_ilp64
                    do j = k, n - 1
                       ij = js
                       do ij = js, js + j
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp64
                       end do
                       js = js + lda
                    end do
                 end if
              else
                 ! n is even and transr = 'c'
                 if( lower ) then
                    ! srpa for lower, transpose and n is even (see paper)
                    ! t1 -> b(0,1), t2 -> b(0,0), s -> b(0,k+1)
                    ! t1 -> a(0+k), t2 -> a(0+0), s -> a(0+k*(k+1)); lda=k
                    ijp = 0_ilp64
                    do i = 0, k - 1
                       do ij = i + ( i+1 )*lda, ( n+1 )*lda - 1, lda
                          ap( ijp ) = conjg( arf( ij ) )
                          ijp = ijp + 1_ilp64
                       end do
                    end do
                    js = 0_ilp64
                    do j = 0, k - 1
                       do ij = js, js + k - j - 1
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp64
                       end do
                       js = js + lda + 1_ilp64
                    end do
                 else
                    ! srpa for upper, transpose and n is even (see paper)
                    ! t1 -> b(0,k+1),     t2 -> b(0,k),   s -> b(0,0)
                    ! t1 -> a(0+k*(k+1)), t2 -> a(0+k*k), s -> a(0+0)); lda=k
                    ijp = 0_ilp64
                    js = ( k+1 )*lda
                    do j = 0, k - 1
                       do ij = js, js + j
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp64
                       end do
                       js = js + lda
                    end do
                    do i = 0, k - 1
                       do ij = i, i + ( k+i )*lda, lda
                          ap( ijp ) = conjg( arf( ij ) )
                          ijp = ijp + 1_ilp64
                       end do
                    end do
                 end if
              end if
           end if
           return
     end subroutine stdlib_I64_ctfttp

     pure module subroutine stdlib_I64_ztfttp( transr, uplo, n, arf, ap, info )
     !! ZTFTTP copies a triangular matrix A from rectangular full packed
     !! format (TF) to standard packed format (TP).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character, intent(in) :: transr, uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: n
           ! Array Arguments 
           complex(dp), intent(out) :: ap(0_ilp64:*)
           complex(dp), intent(in) :: arf(0_ilp64:*)
        ! =====================================================================
           ! Parameters 
           ! Local Scalars 
           logical(lk) :: lower, nisodd, normaltransr
           integer(ilp64) :: n1, n2, k, nt
           integer(ilp64) :: i, j, ij
           integer(ilp64) :: ijp, jp, lda, js
           ! Intrinsic Functions 
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           normaltransr = stdlib_lsame( transr, 'N' )
           lower = stdlib_lsame( uplo, 'L' )
           if( .not.normaltransr .and. .not.stdlib_lsame( transr, 'C' ) ) then
              info = -1_ilp64
           else if( .not.lower .and. .not.stdlib_lsame( uplo, 'U' ) ) then
              info = -2_ilp64
           else if( n<0_ilp64 ) then
              info = -3_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'ZTFTTP', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( n==1_ilp64 ) then
              if( normaltransr ) then
                 ap( 0_ilp64 ) = arf( 0_ilp64 )
              else
                 ap( 0_ilp64 ) = conjg( arf( 0_ilp64 ) )
              end if
              return
           end if
           ! size of array arf(0:nt-1)
           nt = n*( n+1 ) / 2_ilp64
           ! set n1 and n2 depending on lower
           if( lower ) then
              n2 = n / 2_ilp64
              n1 = n - n2
           else
              n1 = n / 2_ilp64
              n2 = n - n1
           end if
           ! if n is odd, set nisodd = .true.
           ! if n is even, set k = n/2 and nisodd = .false.
           ! set lda of arf^c; arf^c is (0:(n+1)/2-1,0:n-noe)
           ! where noe = 0 if n is even, noe = 1 if n is odd
           if( mod( n, 2_ilp64 )==0_ilp64 ) then
              k = n / 2_ilp64
              nisodd = .false.
              lda = n + 1_ilp64
           else
              nisodd = .true.
              lda = n
           end if
           ! arf^c has lda rows and n+1-noe cols
           if( .not.normaltransr )lda = ( n+1 ) / 2_ilp64
           ! start execution: there are eight cases
           if( nisodd ) then
              ! n is odd
              if( normaltransr ) then
                 ! n is odd and transr = 'n'
                 if( lower ) then
                   ! srpa for lower, normal and n is odd ( a(0:n-1,0:n1-1) )
                   ! t1 -> a(0,0), t2 -> a(0,1), s -> a(n1,0)
                   ! t1 -> a(0), t2 -> a(n), s -> a(n1); lda = n
                    ijp = 0_ilp64
                    jp = 0_ilp64
                    do j = 0, n2
                       do i = j, n - 1
                          ij = i + jp
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp64
                       end do
                       jp = jp + lda
                    end do
                    do i = 0, n2 - 1
                       do j = 1 + i, n2
                          ij = i + j*lda
                          ap( ijp ) = conjg( arf( ij ) )
                          ijp = ijp + 1_ilp64
                       end do
                    end do
                 else
                   ! srpa for upper, normal and n is odd ( a(0:n-1,0:n2-1)
                   ! t1 -> a(n1+1,0), t2 -> a(n1,0), s -> a(0,0)
                   ! t1 -> a(n2), t2 -> a(n1), s -> a(0)
                    ijp = 0_ilp64
                    do j = 0, n1 - 1
                       ij = n2 + j
                       do i = 0, j
                          ap( ijp ) = conjg( arf( ij ) )
                          ijp = ijp + 1_ilp64
                          ij = ij + lda
                       end do
                    end do
                    js = 0_ilp64
                    do j = n1, n - 1
                       ij = js
                       do ij = js, js + j
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp64
                       end do
                       js = js + lda
                    end do
                 end if
              else
                 ! n is odd and transr = 'c'
                 if( lower ) then
                    ! srpa for lower, transpose and n is odd
                    ! t1 -> a(0,0) , t2 -> a(1,0) , s -> a(0,n1)
                    ! t1 -> a(0+0) , t2 -> a(1+0) , s -> a(0+n1*n1); lda=n1
                    ijp = 0_ilp64
                    do i = 0, n2
                       do ij = i*( lda+1 ), n*lda - 1, lda
                          ap( ijp ) = conjg( arf( ij ) )
                          ijp = ijp + 1_ilp64
                       end do
                    end do
                    js = 1_ilp64
                    do j = 0, n2 - 1
                       do ij = js, js + n2 - j - 1
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp64
                       end do
                       js = js + lda + 1_ilp64
                    end do
                 else
                    ! srpa for upper, transpose and n is odd
                    ! t1 -> a(0,n1+1), t2 -> a(0,n1), s -> a(0,0)
                    ! t1 -> a(n2*n2), t2 -> a(n1*n2), s -> a(0); lda = n2
                    ijp = 0_ilp64
                    js = n2*lda
                    do j = 0, n1 - 1
                       do ij = js, js + j
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp64
                       end do
                       js = js + lda
                    end do
                    do i = 0, n1
                       do ij = i, i + ( n1+i )*lda, lda
                          ap( ijp ) = conjg( arf( ij ) )
                          ijp = ijp + 1_ilp64
                       end do
                    end do
                 end if
              end if
           else
              ! n is even
              if( normaltransr ) then
                 ! n is even and transr = 'n'
                 if( lower ) then
                    ! srpa for lower, normal, and n is even ( a(0:n,0:k-1) )
                    ! t1 -> a(1,0), t2 -> a(0,0), s -> a(k+1,0)
                    ! t1 -> a(1), t2 -> a(0), s -> a(k+1)
                    ijp = 0_ilp64
                    jp = 0_ilp64
                    do j = 0, k - 1
                       do i = j, n - 1
                          ij = 1_ilp64 + i + jp
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp64
                       end do
                       jp = jp + lda
                    end do
                    do i = 0, k - 1
                       do j = i, k - 1
                          ij = i + j*lda
                          ap( ijp ) = conjg( arf( ij ) )
                          ijp = ijp + 1_ilp64
                       end do
                    end do
                 else
                    ! srpa for upper, normal, and n is even ( a(0:n,0:k-1) )
                    ! t1 -> a(k+1,0) ,  t2 -> a(k,0),   s -> a(0,0)
                    ! t1 -> a(k+1), t2 -> a(k), s -> a(0)
                    ijp = 0_ilp64
                    do j = 0, k - 1
                       ij = k + 1_ilp64 + j
                       do i = 0, j
                          ap( ijp ) = conjg( arf( ij ) )
                          ijp = ijp + 1_ilp64
                          ij = ij + lda
                       end do
                    end do
                    js = 0_ilp64
                    do j = k, n - 1
                       ij = js
                       do ij = js, js + j
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp64
                       end do
                       js = js + lda
                    end do
                 end if
              else
                 ! n is even and transr = 'c'
                 if( lower ) then
                    ! srpa for lower, transpose and n is even (see paper)
                    ! t1 -> b(0,1), t2 -> b(0,0), s -> b(0,k+1)
                    ! t1 -> a(0+k), t2 -> a(0+0), s -> a(0+k*(k+1)); lda=k
                    ijp = 0_ilp64
                    do i = 0, k - 1
                       do ij = i + ( i+1 )*lda, ( n+1 )*lda - 1, lda
                          ap( ijp ) = conjg( arf( ij ) )
                          ijp = ijp + 1_ilp64
                       end do
                    end do
                    js = 0_ilp64
                    do j = 0, k - 1
                       do ij = js, js + k - j - 1
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp64
                       end do
                       js = js + lda + 1_ilp64
                    end do
                 else
                    ! srpa for upper, transpose and n is even (see paper)
                    ! t1 -> b(0,k+1),     t2 -> b(0,k),   s -> b(0,0)
                    ! t1 -> a(0+k*(k+1)), t2 -> a(0+k*k), s -> a(0+0)); lda=k
                    ijp = 0_ilp64
                    js = ( k+1 )*lda
                    do j = 0, k - 1
                       do ij = js, js + j
                          ap( ijp ) = arf( ij )
                          ijp = ijp + 1_ilp64
                       end do
                       js = js + lda
                    end do
                    do i = 0, k - 1
                       do ij = i, i + ( k+i )*lda, lda
                          ap( ijp ) = conjg( arf( ij ) )
                          ijp = ijp + 1_ilp64
                       end do
                    end do
                 end if
              end if
           end if
           return
     end subroutine stdlib_I64_ztfttp




     pure module subroutine stdlib_I64_stfttr( transr, uplo, n, arf, a, lda, info )
     !! STFTTR copies a triangular matrix A from rectangular full packed
     !! format (TF) to standard full format (TR).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character, intent(in) :: transr, uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: n, lda
           ! Array Arguments 
           real(sp), intent(out) :: a(0_ilp64:lda-1,0_ilp64:*)
           real(sp), intent(in) :: arf(0_ilp64:*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lower, nisodd, normaltransr
           integer(ilp64) :: n1, n2, k, nt, nx2, np1x2
           integer(ilp64) :: i, j, l, ij
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           normaltransr = stdlib_lsame( transr, 'N' )
           lower = stdlib_lsame( uplo, 'L' )
           if( .not.normaltransr .and. .not.stdlib_lsame( transr, 'T' ) ) then
              info = -1_ilp64
           else if( .not.lower .and. .not.stdlib_lsame( uplo, 'U' ) ) then
              info = -2_ilp64
           else if( n<0_ilp64 ) then
              info = -3_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -6_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'STFTTR', -info )
              return
           end if
           ! quick return if possible
           if( n<=1_ilp64 ) then
              if( n==1_ilp64 ) then
                 a( 0_ilp64, 0_ilp64 ) = arf( 0_ilp64 )
              end if
              return
           end if
           ! size of array arf(0:nt-1)
           nt = n*( n+1 ) / 2_ilp64
           ! set n1 and n2 depending on lower: for n even n1=n2=k
           if( lower ) then
              n2 = n / 2_ilp64
              n1 = n - n2
           else
              n1 = n / 2_ilp64
              n2 = n - n1
           end if
           ! if n is odd, set nisodd = .true., lda=n+1 and a is (n+1)--by--k2.
           ! if n is even, set k = n/2 and nisodd = .false., lda=n and a is
           ! n--by--(n+1)/2.
           if( mod( n, 2_ilp64 )==0_ilp64 ) then
              k = n / 2_ilp64
              nisodd = .false.
              if( .not.lower )np1x2 = n + n + 2_ilp64
           else
              nisodd = .true.
              if( .not.lower )nx2 = n + n
           end if
           if( nisodd ) then
              ! n is odd
              if( normaltransr ) then
                 ! n is odd and transr = 'n'
                 if( lower ) then
                    ! n is odd, transr = 'n', and uplo = 'l'
                    ij = 0_ilp64
                    do j = 0, n2
                       do i = n1, n2 + j
                          a( n2+j, i ) = arf( ij )
                          ij = ij + 1_ilp64
                       end do
                       do i = j, n - 1
                          a( i, j ) = arf( ij )
                          ij = ij + 1_ilp64
                       end do
                    end do
                 else
                    ! n is odd, transr = 'n', and uplo = 'u'
                    ij = nt - n
                    do j = n - 1, n1, -1
                       do i = 0, j
                          a( i, j ) = arf( ij )
                          ij = ij + 1_ilp64
                       end do
                       do l = j - n1, n1 - 1
                          a( j-n1, l ) = arf( ij )
                          ij = ij + 1_ilp64
                       end do
                       ij = ij - nx2
                    end do
                 end if
              else
                 ! n is odd and transr = 't'
                 if( lower ) then
                    ! n is odd, transr = 't', and uplo = 'l'
                    ij = 0_ilp64
                    do j = 0, n2 - 1
                       do i = 0, j
                          a( j, i ) = arf( ij )
                          ij = ij + 1_ilp64
                       end do
                       do i = n1 + j, n - 1
                          a( i, n1+j ) = arf( ij )
                          ij = ij + 1_ilp64
                       end do
                    end do
                    do j = n2, n - 1
                       do i = 0, n1 - 1
                          a( j, i ) = arf( ij )
                          ij = ij + 1_ilp64
                       end do
                    end do
                 else
                    ! n is odd, transr = 't', and uplo = 'u'
                    ij = 0_ilp64
                    do j = 0, n1
                       do i = n1, n - 1
                          a( j, i ) = arf( ij )
                          ij = ij + 1_ilp64
                       end do
                    end do
                    do j = 0, n1 - 1
                       do i = 0, j
                          a( i, j ) = arf( ij )
                          ij = ij + 1_ilp64
                       end do
                       do l = n2 + j, n - 1
                          a( n2+j, l ) = arf( ij )
                          ij = ij + 1_ilp64
                       end do
                    end do
                 end if
              end if
           else
              ! n is even
              if( normaltransr ) then
                 ! n is even and transr = 'n'
                 if( lower ) then
                    ! n is even, transr = 'n', and uplo = 'l'
                    ij = 0_ilp64
                    do j = 0, k - 1
                       do i = k, k + j
                          a( k+j, i ) = arf( ij )
                          ij = ij + 1_ilp64
                       end do
                       do i = j, n - 1
                          a( i, j ) = arf( ij )
                          ij = ij + 1_ilp64
                       end do
                    end do
                 else
                    ! n is even, transr = 'n', and uplo = 'u'
                    ij = nt - n - 1_ilp64
                    do j = n - 1, k, -1
                       do i = 0, j
                          a( i, j ) = arf( ij )
                          ij = ij + 1_ilp64
                       end do
                       do l = j - k, k - 1
                          a( j-k, l ) = arf( ij )
                          ij = ij + 1_ilp64
                       end do
                       ij = ij - np1x2
                    end do
                 end if
              else
                 ! n is even and transr = 't'
                 if( lower ) then
                    ! n is even, transr = 't', and uplo = 'l'
                    ij = 0_ilp64
                    j = k
                    do i = k, n - 1
                       a( i, j ) = arf( ij )
                       ij = ij + 1_ilp64
                    end do
                    do j = 0, k - 2
                       do i = 0, j
                          a( j, i ) = arf( ij )
                          ij = ij + 1_ilp64
                       end do
                       do i = k + 1 + j, n - 1
                          a( i, k+1+j ) = arf( ij )
                          ij = ij + 1_ilp64
                       end do
                    end do
                    do j = k - 1, n - 1
                       do i = 0, k - 1
                          a( j, i ) = arf( ij )
                          ij = ij + 1_ilp64
                       end do
                    end do
                 else
                    ! n is even, transr = 't', and uplo = 'u'
                    ij = 0_ilp64
                    do j = 0, k
                       do i = k, n - 1
                          a( j, i ) = arf( ij )
                          ij = ij + 1_ilp64
                       end do
                    end do
                    do j = 0, k - 2
                       do i = 0, j
                          a( i, j ) = arf( ij )
                          ij = ij + 1_ilp64
                       end do
                       do l = k + 1 + j, n - 1
                          a( k+1+j, l ) = arf( ij )
                          ij = ij + 1_ilp64
                       end do
                    end do
                    ! note that here, on exit of the loop, j = k-1
                    do i = 0, j
                       a( i, j ) = arf( ij )
                       ij = ij + 1_ilp64
                    end do
                 end if
              end if
           end if
           return
     end subroutine stdlib_I64_stfttr

     pure module subroutine stdlib_I64_dtfttr( transr, uplo, n, arf, a, lda, info )
     !! DTFTTR copies a triangular matrix A from rectangular full packed
     !! format (TF) to standard full format (TR).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character, intent(in) :: transr, uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: n, lda
           ! Array Arguments 
           real(dp), intent(out) :: a(0_ilp64:lda-1,0_ilp64:*)
           real(dp), intent(in) :: arf(0_ilp64:*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lower, nisodd, normaltransr
           integer(ilp64) :: n1, n2, k, nt, nx2, np1x2
           integer(ilp64) :: i, j, l, ij
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           normaltransr = stdlib_lsame( transr, 'N' )
           lower = stdlib_lsame( uplo, 'L' )
           if( .not.normaltransr .and. .not.stdlib_lsame( transr, 'T' ) ) then
              info = -1_ilp64
           else if( .not.lower .and. .not.stdlib_lsame( uplo, 'U' ) ) then
              info = -2_ilp64
           else if( n<0_ilp64 ) then
              info = -3_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -6_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'DTFTTR', -info )
              return
           end if
           ! quick return if possible
           if( n<=1_ilp64 ) then
              if( n==1_ilp64 ) then
                 a( 0_ilp64, 0_ilp64 ) = arf( 0_ilp64 )
              end if
              return
           end if
           ! size of array arf(0:nt-1)
           nt = n*( n+1 ) / 2_ilp64
           ! set n1 and n2 depending on lower: for n even n1=n2=k
           if( lower ) then
              n2 = n / 2_ilp64
              n1 = n - n2
           else
              n1 = n / 2_ilp64
              n2 = n - n1
           end if
           ! if n is odd, set nisodd = .true., lda=n+1 and a is (n+1)--by--k2.
           ! if n is even, set k = n/2 and nisodd = .false., lda=n and a is
           ! n--by--(n+1)/2.
           if( mod( n, 2_ilp64 )==0_ilp64 ) then
              k = n / 2_ilp64
              nisodd = .false.
              if( .not.lower )np1x2 = n + n + 2_ilp64
           else
              nisodd = .true.
              if( .not.lower )nx2 = n + n
           end if
           if( nisodd ) then
              ! n is odd
              if( normaltransr ) then
                 ! n is odd and transr = 'n'
                 if( lower ) then
                    ! n is odd, transr = 'n', and uplo = 'l'
                    ij = 0_ilp64
                    do j = 0, n2
                       do i = n1, n2 + j
                          a( n2+j, i ) = arf( ij )
                          ij = ij + 1_ilp64
                       end do
                       do i = j, n - 1
                          a( i, j ) = arf( ij )
                          ij = ij + 1_ilp64
                       end do
                    end do
                 else
                    ! n is odd, transr = 'n', and uplo = 'u'
                    ij = nt - n
                    do j = n - 1, n1, -1
                       do i = 0, j
                          a( i, j ) = arf( ij )
                          ij = ij + 1_ilp64
                       end do
                       do l = j - n1, n1 - 1
                          a( j-n1, l ) = arf( ij )
                          ij = ij + 1_ilp64
                       end do
                       ij = ij - nx2
                    end do
                 end if
              else
                 ! n is odd and transr = 't'
                 if( lower ) then
                    ! n is odd, transr = 't', and uplo = 'l'
                    ij = 0_ilp64
                    do j = 0, n2 - 1
                       do i = 0, j
                          a( j, i ) = arf( ij )
                          ij = ij + 1_ilp64
                       end do
                       do i = n1 + j, n - 1
                          a( i, n1+j ) = arf( ij )
                          ij = ij + 1_ilp64
                       end do
                    end do
                    do j = n2, n - 1
                       do i = 0, n1 - 1
                          a( j, i ) = arf( ij )
                          ij = ij + 1_ilp64
                       end do
                    end do
                 else
                    ! n is odd, transr = 't', and uplo = 'u'
                    ij = 0_ilp64
                    do j = 0, n1
                       do i = n1, n - 1
                          a( j, i ) = arf( ij )
                          ij = ij + 1_ilp64
                       end do
                    end do
                    do j = 0, n1 - 1
                       do i = 0, j
                          a( i, j ) = arf( ij )
                          ij = ij + 1_ilp64
                       end do
                       do l = n2 + j, n - 1
                          a( n2+j, l ) = arf( ij )
                          ij = ij + 1_ilp64
                       end do
                    end do
                 end if
              end if
           else
              ! n is even
              if( normaltransr ) then
                 ! n is even and transr = 'n'
                 if( lower ) then
                    ! n is even, transr = 'n', and uplo = 'l'
                    ij = 0_ilp64
                    do j = 0, k - 1
                       do i = k, k + j
                          a( k+j, i ) = arf( ij )
                          ij = ij + 1_ilp64
                       end do
                       do i = j, n - 1
                          a( i, j ) = arf( ij )
                          ij = ij + 1_ilp64
                       end do
                    end do
                 else
                    ! n is even, transr = 'n', and uplo = 'u'
                    ij = nt - n - 1_ilp64
                    do j = n - 1, k, -1
                       do i = 0, j
                          a( i, j ) = arf( ij )
                          ij = ij + 1_ilp64
                       end do
                       do l = j - k, k - 1
                          a( j-k, l ) = arf( ij )
                          ij = ij + 1_ilp64
                       end do
                       ij = ij - np1x2
                    end do
                 end if
              else
                 ! n is even and transr = 't'
                 if( lower ) then
                    ! n is even, transr = 't', and uplo = 'l'
                    ij = 0_ilp64
                    j = k
                    do i = k, n - 1
                       a( i, j ) = arf( ij )
                       ij = ij + 1_ilp64
                    end do
                    do j = 0, k - 2
                       do i = 0, j
                          a( j, i ) = arf( ij )
                          ij = ij + 1_ilp64
                       end do
                       do i = k + 1 + j, n - 1
                          a( i, k+1+j ) = arf( ij )
                          ij = ij + 1_ilp64
                       end do
                    end do
                    do j = k - 1, n - 1
                       do i = 0, k - 1
                          a( j, i ) = arf( ij )
                          ij = ij + 1_ilp64
                       end do
                    end do
                 else
                    ! n is even, transr = 't', and uplo = 'u'
                    ij = 0_ilp64
                    do j = 0, k
                       do i = k, n - 1
                          a( j, i ) = arf( ij )
                          ij = ij + 1_ilp64
                       end do
                    end do
                    do j = 0, k - 2
                       do i = 0, j
                          a( i, j ) = arf( ij )
                          ij = ij + 1_ilp64
                       end do
                       do l = k + 1 + j, n - 1
                          a( k+1+j, l ) = arf( ij )
                          ij = ij + 1_ilp64
                       end do
                    end do
                    ! note that here, on exit of the loop, j = k-1
                    do i = 0, j
                       a( i, j ) = arf( ij )
                       ij = ij + 1_ilp64
                    end do
                 end if
              end if
           end if
           return
     end subroutine stdlib_I64_dtfttr


     pure module subroutine stdlib_I64_ctfttr( transr, uplo, n, arf, a, lda, info )
     !! CTFTTR copies a triangular matrix A from rectangular full packed
     !! format (TF) to standard full format (TR).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character, intent(in) :: transr, uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: n, lda
           ! Array Arguments 
           complex(sp), intent(out) :: a(0_ilp64:lda-1,0_ilp64:*)
           complex(sp), intent(in) :: arf(0_ilp64:*)
        ! =====================================================================
           ! Parameters 
           ! Local Scalars 
           logical(lk) :: lower, nisodd, normaltransr
           integer(ilp64) :: n1, n2, k, nt, nx2, np1x2
           integer(ilp64) :: i, j, l, ij
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           normaltransr = stdlib_lsame( transr, 'N' )
           lower = stdlib_lsame( uplo, 'L' )
           if( .not.normaltransr .and. .not.stdlib_lsame( transr, 'C' ) ) then
              info = -1_ilp64
           else if( .not.lower .and. .not.stdlib_lsame( uplo, 'U' ) ) then
              info = -2_ilp64
           else if( n<0_ilp64 ) then
              info = -3_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -6_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'CTFTTR', -info )
              return
           end if
           ! quick return if possible
           if( n<=1_ilp64 ) then
              if( n==1_ilp64 ) then
                 if( normaltransr ) then
                    a( 0_ilp64, 0_ilp64 ) = arf( 0_ilp64 )
                 else
                    a( 0_ilp64, 0_ilp64 ) = conjg( arf( 0_ilp64 ) )
                 end if
              end if
              return
           end if
           ! size of array arf(1:2,0:nt-1)
           nt = n*( n+1 ) / 2_ilp64
           ! set n1 and n2 depending on lower: for n even n1=n2=k
           if( lower ) then
              n2 = n / 2_ilp64
              n1 = n - n2
           else
              n1 = n / 2_ilp64
              n2 = n - n1
           end if
           ! if n is odd, set nisodd = .true., lda=n+1 and a is (n+1)--by--k2.
           ! if n is even, set k = n/2 and nisodd = .false., lda=n and a is
           ! n--by--(n+1)/2.
           if( mod( n, 2_ilp64 )==0_ilp64 ) then
              k = n / 2_ilp64
              nisodd = .false.
              if( .not.lower )np1x2 = n + n + 2_ilp64
           else
              nisodd = .true.
              if( .not.lower )nx2 = n + n
           end if
           if( nisodd ) then
              ! n is odd
              if( normaltransr ) then
                 ! n is odd and transr = 'n'
                 if( lower ) then
                   ! srpa for lower, normal and n is odd ( a(0:n-1,0:n1-1) )
                   ! t1 -> a(0,0), t2 -> a(0,1), s -> a(n1,0)
                   ! t1 -> a(0), t2 -> a(n), s -> a(n1); lda=n
                    ij = 0_ilp64
                    do j = 0, n2
                       do i = n1, n2 + j
                          a( n2+j, i ) = conjg( arf( ij ) )
                          ij = ij + 1_ilp64
                       end do
                       do i = j, n - 1
                          a( i, j ) = arf( ij )
                          ij = ij + 1_ilp64
                       end do
                    end do
                 else
                   ! srpa for upper, normal and n is odd ( a(0:n-1,0:n2-1)
                   ! t1 -> a(n1+1,0), t2 -> a(n1,0), s -> a(0,0)
                   ! t1 -> a(n2), t2 -> a(n1), s -> a(0); lda=n
                    ij = nt - n
                    do j = n - 1, n1, -1
                       do i = 0, j
                          a( i, j ) = arf( ij )
                          ij = ij + 1_ilp64
                       end do
                       do l = j - n1, n1 - 1
                          a( j-n1, l ) = conjg( arf( ij ) )
                          ij = ij + 1_ilp64
                       end do
                       ij = ij - nx2
                    end do
                 end if
              else
                 ! n is odd and transr = 'c'
                 if( lower ) then
                    ! srpa for lower, transpose and n is odd
                    ! t1 -> a(0,0) , t2 -> a(1,0) , s -> a(0,n1)
                    ! t1 -> a(0+0) , t2 -> a(1+0) , s -> a(0+n1*n1); lda=n1
                    ij = 0_ilp64
                    do j = 0, n2 - 1
                       do i = 0, j
                          a( j, i ) = conjg( arf( ij ) )
                          ij = ij + 1_ilp64
                       end do
                       do i = n1 + j, n - 1
                          a( i, n1+j ) = arf( ij )
                          ij = ij + 1_ilp64
                       end do
                    end do
                    do j = n2, n - 1
                       do i = 0, n1 - 1
                          a( j, i ) = conjg( arf( ij ) )
                          ij = ij + 1_ilp64
                       end do
                    end do
                 else
                    ! srpa for upper, transpose and n is odd
                    ! t1 -> a(0,n1+1), t2 -> a(0,n1), s -> a(0,0)
                    ! t1 -> a(n2*n2), t2 -> a(n1*n2), s -> a(0); lda = n2
                    ij = 0_ilp64
                    do j = 0, n1
                       do i = n1, n - 1
                          a( j, i ) = conjg( arf( ij ) )
                          ij = ij + 1_ilp64
                       end do
                    end do
                    do j = 0, n1 - 1
                       do i = 0, j
                          a( i, j ) = arf( ij )
                          ij = ij + 1_ilp64
                       end do
                       do l = n2 + j, n - 1
                          a( n2+j, l ) = conjg( arf( ij ) )
                          ij = ij + 1_ilp64
                       end do
                    end do
                 end if
              end if
           else
              ! n is even
              if( normaltransr ) then
                 ! n is even and transr = 'n'
                 if( lower ) then
                    ! srpa for lower, normal, and n is even ( a(0:n,0:k-1) )
                    ! t1 -> a(1,0), t2 -> a(0,0), s -> a(k+1,0)
                    ! t1 -> a(1), t2 -> a(0), s -> a(k+1); lda=n+1
                    ij = 0_ilp64
                    do j = 0, k - 1
                       do i = k, k + j
                          a( k+j, i ) = conjg( arf( ij ) )
                          ij = ij + 1_ilp64
                       end do
                       do i = j, n - 1
                          a( i, j ) = arf( ij )
                          ij = ij + 1_ilp64
                       end do
                    end do
                 else
                    ! srpa for upper, normal, and n is even ( a(0:n,0:k-1) )
                    ! t1 -> a(k+1,0) ,  t2 -> a(k,0),   s -> a(0,0)
                    ! t1 -> a(k+1), t2 -> a(k), s -> a(0); lda=n+1
                    ij = nt - n - 1_ilp64
                    do j = n - 1, k, -1
                       do i = 0, j
                          a( i, j ) = arf( ij )
                          ij = ij + 1_ilp64
                       end do
                       do l = j - k, k - 1
                          a( j-k, l ) = conjg( arf( ij ) )
                          ij = ij + 1_ilp64
                       end do
                       ij = ij - np1x2
                    end do
                 end if
              else
                 ! n is even and transr = 'c'
                 if( lower ) then
                    ! srpa for lower, transpose and n is even (see paper, a=b)
                    ! t1 -> a(0,1) , t2 -> a(0,0) , s -> a(0,k+1) :
                    ! t1 -> a(0+k) , t2 -> a(0+0) , s -> a(0+k*(k+1)); lda=k
                    ij = 0_ilp64
                    j = k
                    do i = k, n - 1
                       a( i, j ) = arf( ij )
                       ij = ij + 1_ilp64
                    end do
                    do j = 0, k - 2
                       do i = 0, j
                          a( j, i ) = conjg( arf( ij ) )
                          ij = ij + 1_ilp64
                       end do
                       do i = k + 1 + j, n - 1
                          a( i, k+1+j ) = arf( ij )
                          ij = ij + 1_ilp64
                       end do
                    end do
                    do j = k - 1, n - 1
                       do i = 0, k - 1
                          a( j, i ) = conjg( arf( ij ) )
                          ij = ij + 1_ilp64
                       end do
                    end do
                 else
                    ! srpa for upper, transpose and n is even (see paper, a=b)
                    ! t1 -> a(0,k+1) , t2 -> a(0,k) , s -> a(0,0)
                    ! t1 -> a(0+k*(k+1)) , t2 -> a(0+k*k) , s -> a(0+0)); lda=k
                    ij = 0_ilp64
                    do j = 0, k
                       do i = k, n - 1
                          a( j, i ) = conjg( arf( ij ) )
                          ij = ij + 1_ilp64
                       end do
                    end do
                    do j = 0, k - 2
                       do i = 0, j
                          a( i, j ) = arf( ij )
                          ij = ij + 1_ilp64
                       end do
                       do l = k + 1 + j, n - 1
                          a( k+1+j, l ) = conjg( arf( ij ) )
                          ij = ij + 1_ilp64
                       end do
                    end do
                    ! note that here j = k-1
                    do i = 0, j
                       a( i, j ) = arf( ij )
                       ij = ij + 1_ilp64
                    end do
                 end if
              end if
           end if
           return
     end subroutine stdlib_I64_ctfttr

     pure module subroutine stdlib_I64_ztfttr( transr, uplo, n, arf, a, lda, info )
     !! ZTFTTR copies a triangular matrix A from rectangular full packed
     !! format (TF) to standard full format (TR).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character, intent(in) :: transr, uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: n, lda
           ! Array Arguments 
           complex(dp), intent(out) :: a(0_ilp64:lda-1,0_ilp64:*)
           complex(dp), intent(in) :: arf(0_ilp64:*)
        ! =====================================================================
           ! Parameters 
           ! Local Scalars 
           logical(lk) :: lower, nisodd, normaltransr
           integer(ilp64) :: n1, n2, k, nt, nx2, np1x2
           integer(ilp64) :: i, j, l, ij
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           normaltransr = stdlib_lsame( transr, 'N' )
           lower = stdlib_lsame( uplo, 'L' )
           if( .not.normaltransr .and. .not.stdlib_lsame( transr, 'C' ) ) then
              info = -1_ilp64
           else if( .not.lower .and. .not.stdlib_lsame( uplo, 'U' ) ) then
              info = -2_ilp64
           else if( n<0_ilp64 ) then
              info = -3_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -6_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'ZTFTTR', -info )
              return
           end if
           ! quick return if possible
           if( n<=1_ilp64 ) then
              if( n==1_ilp64 ) then
                 if( normaltransr ) then
                    a( 0_ilp64, 0_ilp64 ) = arf( 0_ilp64 )
                 else
                    a( 0_ilp64, 0_ilp64 ) = conjg( arf( 0_ilp64 ) )
                 end if
              end if
              return
           end if
           ! size of array arf(1:2,0:nt-1)
           nt = n*( n+1 ) / 2_ilp64
           ! set n1 and n2 depending on lower: for n even n1=n2=k
           if( lower ) then
              n2 = n / 2_ilp64
              n1 = n - n2
           else
              n1 = n / 2_ilp64
              n2 = n - n1
           end if
           ! if n is odd, set nisodd = .true., lda=n+1 and a is (n+1)--by--k2.
           ! if n is even, set k = n/2 and nisodd = .false., lda=n and a is
           ! n--by--(n+1)/2.
           if( mod( n, 2_ilp64 )==0_ilp64 ) then
              k = n / 2_ilp64
              nisodd = .false.
              if( .not.lower )np1x2 = n + n + 2_ilp64
           else
              nisodd = .true.
              if( .not.lower )nx2 = n + n
           end if
           if( nisodd ) then
              ! n is odd
              if( normaltransr ) then
                 ! n is odd and transr = 'n'
                 if( lower ) then
                   ! srpa for lower, normal and n is odd ( a(0:n-1,0:n1-1) )
                   ! t1 -> a(0,0), t2 -> a(0,1), s -> a(n1,0)
                   ! t1 -> a(0), t2 -> a(n), s -> a(n1); lda=n
                    ij = 0_ilp64
                    do j = 0, n2
                       do i = n1, n2 + j
                          a( n2+j, i ) = conjg( arf( ij ) )
                          ij = ij + 1_ilp64
                       end do
                       do i = j, n - 1
                          a( i, j ) = arf( ij )
                          ij = ij + 1_ilp64
                       end do
                    end do
                 else
                   ! srpa for upper, normal and n is odd ( a(0:n-1,0:n2-1)
                   ! t1 -> a(n1+1,0), t2 -> a(n1,0), s -> a(0,0)
                   ! t1 -> a(n2), t2 -> a(n1), s -> a(0); lda=n
                    ij = nt - n
                    do j = n - 1, n1, -1
                       do i = 0, j
                          a( i, j ) = arf( ij )
                          ij = ij + 1_ilp64
                       end do
                       do l = j - n1, n1 - 1
                          a( j-n1, l ) = conjg( arf( ij ) )
                          ij = ij + 1_ilp64
                       end do
                       ij = ij - nx2
                    end do
                 end if
              else
                 ! n is odd and transr = 'c'
                 if( lower ) then
                    ! srpa for lower, transpose and n is odd
                    ! t1 -> a(0,0) , t2 -> a(1,0) , s -> a(0,n1)
                    ! t1 -> a(0+0) , t2 -> a(1+0) , s -> a(0+n1*n1); lda=n1
                    ij = 0_ilp64
                    do j = 0, n2 - 1
                       do i = 0, j
                          a( j, i ) = conjg( arf( ij ) )
                          ij = ij + 1_ilp64
                       end do
                       do i = n1 + j, n - 1
                          a( i, n1+j ) = arf( ij )
                          ij = ij + 1_ilp64
                       end do
                    end do
                    do j = n2, n - 1
                       do i = 0, n1 - 1
                          a( j, i ) = conjg( arf( ij ) )
                          ij = ij + 1_ilp64
                       end do
                    end do
                 else
                    ! srpa for upper, transpose and n is odd
                    ! t1 -> a(0,n1+1), t2 -> a(0,n1), s -> a(0,0)
                    ! t1 -> a(n2*n2), t2 -> a(n1*n2), s -> a(0); lda = n2
                    ij = 0_ilp64
                    do j = 0, n1
                       do i = n1, n - 1
                          a( j, i ) = conjg( arf( ij ) )
                          ij = ij + 1_ilp64
                       end do
                    end do
                    do j = 0, n1 - 1
                       do i = 0, j
                          a( i, j ) = arf( ij )
                          ij = ij + 1_ilp64
                       end do
                       do l = n2 + j, n - 1
                          a( n2+j, l ) = conjg( arf( ij ) )
                          ij = ij + 1_ilp64
                       end do
                    end do
                 end if
              end if
           else
              ! n is even
              if( normaltransr ) then
                 ! n is even and transr = 'n'
                 if( lower ) then
                    ! srpa for lower, normal, and n is even ( a(0:n,0:k-1) )
                    ! t1 -> a(1,0), t2 -> a(0,0), s -> a(k+1,0)
                    ! t1 -> a(1), t2 -> a(0), s -> a(k+1); lda=n+1
                    ij = 0_ilp64
                    do j = 0, k - 1
                       do i = k, k + j
                          a( k+j, i ) = conjg( arf( ij ) )
                          ij = ij + 1_ilp64
                       end do
                       do i = j, n - 1
                          a( i, j ) = arf( ij )
                          ij = ij + 1_ilp64
                       end do
                    end do
                 else
                    ! srpa for upper, normal, and n is even ( a(0:n,0:k-1) )
                    ! t1 -> a(k+1,0) ,  t2 -> a(k,0),   s -> a(0,0)
                    ! t1 -> a(k+1), t2 -> a(k), s -> a(0); lda=n+1
                    ij = nt - n - 1_ilp64
                    do j = n - 1, k, -1
                       do i = 0, j
                          a( i, j ) = arf( ij )
                          ij = ij + 1_ilp64
                       end do
                       do l = j - k, k - 1
                          a( j-k, l ) = conjg( arf( ij ) )
                          ij = ij + 1_ilp64
                       end do
                       ij = ij - np1x2
                    end do
                 end if
              else
                 ! n is even and transr = 'c'
                 if( lower ) then
                    ! srpa for lower, transpose and n is even (see paper, a=b)
                    ! t1 -> a(0,1) , t2 -> a(0,0) , s -> a(0,k+1) :
                    ! t1 -> a(0+k) , t2 -> a(0+0) , s -> a(0+k*(k+1)); lda=k
                    ij = 0_ilp64
                    j = k
                    do i = k, n - 1
                       a( i, j ) = arf( ij )
                       ij = ij + 1_ilp64
                    end do
                    do j = 0, k - 2
                       do i = 0, j
                          a( j, i ) = conjg( arf( ij ) )
                          ij = ij + 1_ilp64
                       end do
                       do i = k + 1 + j, n - 1
                          a( i, k+1+j ) = arf( ij )
                          ij = ij + 1_ilp64
                       end do
                    end do
                    do j = k - 1, n - 1
                       do i = 0, k - 1
                          a( j, i ) = conjg( arf( ij ) )
                          ij = ij + 1_ilp64
                       end do
                    end do
                 else
                    ! srpa for upper, transpose and n is even (see paper, a=b)
                    ! t1 -> a(0,k+1) , t2 -> a(0,k) , s -> a(0,0)
                    ! t1 -> a(0+k*(k+1)) , t2 -> a(0+k*k) , s -> a(0+0)); lda=k
                    ij = 0_ilp64
                    do j = 0, k
                       do i = k, n - 1
                          a( j, i ) = conjg( arf( ij ) )
                          ij = ij + 1_ilp64
                       end do
                    end do
                    do j = 0, k - 2
                       do i = 0, j
                          a( i, j ) = arf( ij )
                          ij = ij + 1_ilp64
                       end do
                       do l = k + 1 + j, n - 1
                          a( k+1+j, l ) = conjg( arf( ij ) )
                          ij = ij + 1_ilp64
                       end do
                    end do
                    ! note that here j = k-1
                    do i = 0, j
                       a( i, j ) = arf( ij )
                       ij = ij + 1_ilp64
                    end do
                 end if
              end if
           end if
           return
     end subroutine stdlib_I64_ztfttr




     pure module subroutine stdlib_I64_stpttf( transr, uplo, n, ap, arf, info )
     !! STPTTF copies a triangular matrix A from standard packed format (TP)
     !! to rectangular full packed format (TF).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character, intent(in) :: transr, uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: n
           ! Array Arguments 
           real(sp), intent(in) :: ap(0_ilp64:*)
           real(sp), intent(out) :: arf(0_ilp64:*)
        ! =====================================================================
           ! Parameters 
           ! Local Scalars 
           logical(lk) :: lower, nisodd, normaltransr
           integer(ilp64) :: n1, n2, k, nt
           integer(ilp64) :: i, j, ij
           integer(ilp64) :: ijp, jp, lda, js
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           normaltransr = stdlib_lsame( transr, 'N' )
           lower = stdlib_lsame( uplo, 'L' )
           if( .not.normaltransr .and. .not.stdlib_lsame( transr, 'T' ) ) then
              info = -1_ilp64
           else if( .not.lower .and. .not.stdlib_lsame( uplo, 'U' ) ) then
              info = -2_ilp64
           else if( n<0_ilp64 ) then
              info = -3_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'STPTTF', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( n==1_ilp64 ) then
              if( normaltransr ) then
                 arf( 0_ilp64 ) = ap( 0_ilp64 )
              else
                 arf( 0_ilp64 ) = ap( 0_ilp64 )
              end if
              return
           end if
           ! size of array arf(0:nt-1)
           nt = n*( n+1 ) / 2_ilp64
           ! set n1 and n2 depending on lower
           if( lower ) then
              n2 = n / 2_ilp64
              n1 = n - n2
           else
              n1 = n / 2_ilp64
              n2 = n - n1
           end if
           ! if n is odd, set nisodd = .true.
           ! if n is even, set k = n/2 and nisodd = .false.
           ! set lda of arf^c; arf^c is (0:(n+1)/2-1,0:n-noe)
           ! where noe = 0 if n is even, noe = 1 if n is odd
           if( mod( n, 2_ilp64 )==0_ilp64 ) then
              k = n / 2_ilp64
              nisodd = .false.
              lda = n + 1_ilp64
           else
              nisodd = .true.
              lda = n
           end if
           ! arf^c has lda rows and n+1-noe cols
           if( .not.normaltransr )lda = ( n+1 ) / 2_ilp64
           ! start execution: there are eight cases
           if( nisodd ) then
              ! n is odd
              if( normaltransr ) then
                 ! n is odd and transr = 'n'
                 if( lower ) then
                    ! n is odd, transr = 'n', and uplo = 'l'
                    ijp = 0_ilp64
                    jp = 0_ilp64
                    do j = 0, n2
                       do i = j, n - 1
                          ij = i + jp
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp64
                       end do
                       jp = jp + lda
                    end do
                    do i = 0, n2 - 1
                       do j = 1 + i, n2
                          ij = i + j*lda
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp64
                       end do
                    end do
                 else
                    ! n is odd, transr = 'n', and uplo = 'u'
                    ijp = 0_ilp64
                    do j = 0, n1 - 1
                       ij = n2 + j
                       do i = 0, j
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp64
                          ij = ij + lda
                       end do
                    end do
                    js = 0_ilp64
                    do j = n1, n - 1
                       ij = js
                       do ij = js, js + j
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp64
                       end do
                       js = js + lda
                    end do
                 end if
              else
                 ! n is odd and transr = 't'
                 if( lower ) then
                    ! n is odd, transr = 't', and uplo = 'l'
                    ijp = 0_ilp64
                    do i = 0, n2
                       do ij = i*( lda+1 ), n*lda - 1, lda
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp64
                       end do
                    end do
                    js = 1_ilp64
                    do j = 0, n2 - 1
                       do ij = js, js + n2 - j - 1
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp64
                       end do
                       js = js + lda + 1_ilp64
                    end do
                 else
                    ! n is odd, transr = 't', and uplo = 'u'
                    ijp = 0_ilp64
                    js = n2*lda
                    do j = 0, n1 - 1
                       do ij = js, js + j
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp64
                       end do
                       js = js + lda
                    end do
                    do i = 0, n1
                       do ij = i, i + ( n1+i )*lda, lda
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp64
                       end do
                    end do
                 end if
              end if
           else
              ! n is even
              if( normaltransr ) then
                 ! n is even and transr = 'n'
                 if( lower ) then
                    ! n is even, transr = 'n', and uplo = 'l'
                    ijp = 0_ilp64
                    jp = 0_ilp64
                    do j = 0, k - 1
                       do i = j, n - 1
                          ij = 1_ilp64 + i + jp
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp64
                       end do
                       jp = jp + lda
                    end do
                    do i = 0, k - 1
                       do j = i, k - 1
                          ij = i + j*lda
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp64
                       end do
                    end do
                 else
                    ! n is even, transr = 'n', and uplo = 'u'
                    ijp = 0_ilp64
                    do j = 0, k - 1
                       ij = k + 1_ilp64 + j
                       do i = 0, j
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp64
                          ij = ij + lda
                       end do
                    end do
                    js = 0_ilp64
                    do j = k, n - 1
                       ij = js
                       do ij = js, js + j
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp64
                       end do
                       js = js + lda
                    end do
                 end if
              else
                 ! n is even and transr = 't'
                 if( lower ) then
                    ! n is even, transr = 't', and uplo = 'l'
                    ijp = 0_ilp64
                    do i = 0, k - 1
                       do ij = i + ( i+1 )*lda, ( n+1 )*lda - 1, lda
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp64
                       end do
                    end do
                    js = 0_ilp64
                    do j = 0, k - 1
                       do ij = js, js + k - j - 1
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp64
                       end do
                       js = js + lda + 1_ilp64
                    end do
                 else
                    ! n is even, transr = 't', and uplo = 'u'
                    ijp = 0_ilp64
                    js = ( k+1 )*lda
                    do j = 0, k - 1
                       do ij = js, js + j
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp64
                       end do
                       js = js + lda
                    end do
                    do i = 0, k - 1
                       do ij = i, i + ( k+i )*lda, lda
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp64
                       end do
                    end do
                 end if
              end if
           end if
           return
     end subroutine stdlib_I64_stpttf

     pure module subroutine stdlib_I64_dtpttf( transr, uplo, n, ap, arf, info )
     !! DTPTTF copies a triangular matrix A from standard packed format (TP)
     !! to rectangular full packed format (TF).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character, intent(in) :: transr, uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: n
           ! Array Arguments 
           real(dp), intent(in) :: ap(0_ilp64:*)
           real(dp), intent(out) :: arf(0_ilp64:*)
        ! =====================================================================
           ! Parameters 
           ! Local Scalars 
           logical(lk) :: lower, nisodd, normaltransr
           integer(ilp64) :: n1, n2, k, nt
           integer(ilp64) :: i, j, ij
           integer(ilp64) :: ijp, jp, lda, js
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           normaltransr = stdlib_lsame( transr, 'N' )
           lower = stdlib_lsame( uplo, 'L' )
           if( .not.normaltransr .and. .not.stdlib_lsame( transr, 'T' ) ) then
              info = -1_ilp64
           else if( .not.lower .and. .not.stdlib_lsame( uplo, 'U' ) ) then
              info = -2_ilp64
           else if( n<0_ilp64 ) then
              info = -3_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'DTPTTF', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( n==1_ilp64 ) then
              if( normaltransr ) then
                 arf( 0_ilp64 ) = ap( 0_ilp64 )
              else
                 arf( 0_ilp64 ) = ap( 0_ilp64 )
              end if
              return
           end if
           ! size of array arf(0:nt-1)
           nt = n*( n+1 ) / 2_ilp64
           ! set n1 and n2 depending on lower
           if( lower ) then
              n2 = n / 2_ilp64
              n1 = n - n2
           else
              n1 = n / 2_ilp64
              n2 = n - n1
           end if
           ! if n is odd, set nisodd = .true.
           ! if n is even, set k = n/2 and nisodd = .false.
           ! set lda of arf^c; arf^c is (0:(n+1)/2-1,0:n-noe)
           ! where noe = 0 if n is even, noe = 1 if n is odd
           if( mod( n, 2_ilp64 )==0_ilp64 ) then
              k = n / 2_ilp64
              nisodd = .false.
              lda = n + 1_ilp64
           else
              nisodd = .true.
              lda = n
           end if
           ! arf^c has lda rows and n+1-noe cols
           if( .not.normaltransr )lda = ( n+1 ) / 2_ilp64
           ! start execution: there are eight cases
           if( nisodd ) then
              ! n is odd
              if( normaltransr ) then
                 ! n is odd and transr = 'n'
                 if( lower ) then
                    ! n is odd, transr = 'n', and uplo = 'l'
                    ijp = 0_ilp64
                    jp = 0_ilp64
                    do j = 0, n2
                       do i = j, n - 1
                          ij = i + jp
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp64
                       end do
                       jp = jp + lda
                    end do
                    do i = 0, n2 - 1
                       do j = 1 + i, n2
                          ij = i + j*lda
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp64
                       end do
                    end do
                 else
                    ! n is odd, transr = 'n', and uplo = 'u'
                    ijp = 0_ilp64
                    do j = 0, n1 - 1
                       ij = n2 + j
                       do i = 0, j
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp64
                          ij = ij + lda
                       end do
                    end do
                    js = 0_ilp64
                    do j = n1, n - 1
                       ij = js
                       do ij = js, js + j
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp64
                       end do
                       js = js + lda
                    end do
                 end if
              else
                 ! n is odd and transr = 't'
                 if( lower ) then
                    ! n is odd, transr = 't', and uplo = 'l'
                    ijp = 0_ilp64
                    do i = 0, n2
                       do ij = i*( lda+1 ), n*lda - 1, lda
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp64
                       end do
                    end do
                    js = 1_ilp64
                    do j = 0, n2 - 1
                       do ij = js, js + n2 - j - 1
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp64
                       end do
                       js = js + lda + 1_ilp64
                    end do
                 else
                    ! n is odd, transr = 't', and uplo = 'u'
                    ijp = 0_ilp64
                    js = n2*lda
                    do j = 0, n1 - 1
                       do ij = js, js + j
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp64
                       end do
                       js = js + lda
                    end do
                    do i = 0, n1
                       do ij = i, i + ( n1+i )*lda, lda
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp64
                       end do
                    end do
                 end if
              end if
           else
              ! n is even
              if( normaltransr ) then
                 ! n is even and transr = 'n'
                 if( lower ) then
                    ! n is even, transr = 'n', and uplo = 'l'
                    ijp = 0_ilp64
                    jp = 0_ilp64
                    do j = 0, k - 1
                       do i = j, n - 1
                          ij = 1_ilp64 + i + jp
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp64
                       end do
                       jp = jp + lda
                    end do
                    do i = 0, k - 1
                       do j = i, k - 1
                          ij = i + j*lda
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp64
                       end do
                    end do
                 else
                    ! n is even, transr = 'n', and uplo = 'u'
                    ijp = 0_ilp64
                    do j = 0, k - 1
                       ij = k + 1_ilp64 + j
                       do i = 0, j
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp64
                          ij = ij + lda
                       end do
                    end do
                    js = 0_ilp64
                    do j = k, n - 1
                       ij = js
                       do ij = js, js + j
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp64
                       end do
                       js = js + lda
                    end do
                 end if
              else
                 ! n is even and transr = 't'
                 if( lower ) then
                    ! n is even, transr = 't', and uplo = 'l'
                    ijp = 0_ilp64
                    do i = 0, k - 1
                       do ij = i + ( i+1 )*lda, ( n+1 )*lda - 1, lda
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp64
                       end do
                    end do
                    js = 0_ilp64
                    do j = 0, k - 1
                       do ij = js, js + k - j - 1
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp64
                       end do
                       js = js + lda + 1_ilp64
                    end do
                 else
                    ! n is even, transr = 't', and uplo = 'u'
                    ijp = 0_ilp64
                    js = ( k+1 )*lda
                    do j = 0, k - 1
                       do ij = js, js + j
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp64
                       end do
                       js = js + lda
                    end do
                    do i = 0, k - 1
                       do ij = i, i + ( k+i )*lda, lda
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp64
                       end do
                    end do
                 end if
              end if
           end if
           return
     end subroutine stdlib_I64_dtpttf


     pure module subroutine stdlib_I64_ctpttf( transr, uplo, n, ap, arf, info )
     !! CTPTTF copies a triangular matrix A from standard packed format (TP)
     !! to rectangular full packed format (TF).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character, intent(in) :: transr, uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: n
           ! Array Arguments 
           complex(sp), intent(in) :: ap(0_ilp64:*)
           complex(sp), intent(out) :: arf(0_ilp64:*)
        ! =====================================================================
           ! Parameters 
           ! Local Scalars 
           logical(lk) :: lower, nisodd, normaltransr
           integer(ilp64) :: n1, n2, k, nt
           integer(ilp64) :: i, j, ij
           integer(ilp64) :: ijp, jp, lda, js
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           normaltransr = stdlib_lsame( transr, 'N' )
           lower = stdlib_lsame( uplo, 'L' )
           if( .not.normaltransr .and. .not.stdlib_lsame( transr, 'C' ) ) then
              info = -1_ilp64
           else if( .not.lower .and. .not.stdlib_lsame( uplo, 'U' ) ) then
              info = -2_ilp64
           else if( n<0_ilp64 ) then
              info = -3_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'CTPTTF', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( n==1_ilp64 ) then
              if( normaltransr ) then
                 arf( 0_ilp64 ) = ap( 0_ilp64 )
              else
                 arf( 0_ilp64 ) = conjg( ap( 0_ilp64 ) )
              end if
              return
           end if
           ! size of array arf(0:nt-1)
           nt = n*( n+1 ) / 2_ilp64
           ! set n1 and n2 depending on lower
           if( lower ) then
              n2 = n / 2_ilp64
              n1 = n - n2
           else
              n1 = n / 2_ilp64
              n2 = n - n1
           end if
           ! if n is odd, set nisodd = .true.
           ! if n is even, set k = n/2 and nisodd = .false.
           ! set lda of arf^c; arf^c is (0:(n+1)/2-1,0:n-noe)
           ! where noe = 0 if n is even, noe = 1 if n is odd
           if( mod( n, 2_ilp64 )==0_ilp64 ) then
              k = n / 2_ilp64
              nisodd = .false.
              lda = n + 1_ilp64
           else
              nisodd = .true.
              lda = n
           end if
           ! arf^c has lda rows and n+1-noe cols
           if( .not.normaltransr )lda = ( n+1 ) / 2_ilp64
           ! start execution: there are eight cases
           if( nisodd ) then
              ! n is odd
              if( normaltransr ) then
                 ! n is odd and transr = 'n'
                 if( lower ) then
                   ! srpa for lower, normal and n is odd ( a(0:n-1,0:n1-1) )
                   ! t1 -> a(0,0), t2 -> a(0,1), s -> a(n1,0)
                   ! t1 -> a(0), t2 -> a(n), s -> a(n1); lda = n
                    ijp = 0_ilp64
                    jp = 0_ilp64
                    do j = 0, n2
                       do i = j, n - 1
                          ij = i + jp
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp64
                       end do
                       jp = jp + lda
                    end do
                    do i = 0, n2 - 1
                       do j = 1 + i, n2
                          ij = i + j*lda
                          arf( ij ) = conjg( ap( ijp ) )
                          ijp = ijp + 1_ilp64
                       end do
                    end do
                 else
                   ! srpa for upper, normal and n is odd ( a(0:n-1,0:n2-1)
                   ! t1 -> a(n1+1,0), t2 -> a(n1,0), s -> a(0,0)
                   ! t1 -> a(n2), t2 -> a(n1), s -> a(0)
                    ijp = 0_ilp64
                    do j = 0, n1 - 1
                       ij = n2 + j
                       do i = 0, j
                          arf( ij ) = conjg( ap( ijp ) )
                          ijp = ijp + 1_ilp64
                          ij = ij + lda
                       end do
                    end do
                    js = 0_ilp64
                    do j = n1, n - 1
                       ij = js
                       do ij = js, js + j
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp64
                       end do
                       js = js + lda
                    end do
                 end if
              else
                 ! n is odd and transr = 'c'
                 if( lower ) then
                    ! srpa for lower, transpose and n is odd
                    ! t1 -> a(0,0) , t2 -> a(1,0) , s -> a(0,n1)
                    ! t1 -> a(0+0) , t2 -> a(1+0) , s -> a(0+n1*n1); lda=n1
                    ijp = 0_ilp64
                    do i = 0, n2
                       do ij = i*( lda+1 ), n*lda - 1, lda
                          arf( ij ) = conjg( ap( ijp ) )
                          ijp = ijp + 1_ilp64
                       end do
                    end do
                    js = 1_ilp64
                    do j = 0, n2 - 1
                       do ij = js, js + n2 - j - 1
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp64
                       end do
                       js = js + lda + 1_ilp64
                    end do
                 else
                    ! srpa for upper, transpose and n is odd
                    ! t1 -> a(0,n1+1), t2 -> a(0,n1), s -> a(0,0)
                    ! t1 -> a(n2*n2), t2 -> a(n1*n2), s -> a(0); lda = n2
                    ijp = 0_ilp64
                    js = n2*lda
                    do j = 0, n1 - 1
                       do ij = js, js + j
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp64
                       end do
                       js = js + lda
                    end do
                    do i = 0, n1
                       do ij = i, i + ( n1+i )*lda, lda
                          arf( ij ) = conjg( ap( ijp ) )
                          ijp = ijp + 1_ilp64
                       end do
                    end do
                 end if
              end if
           else
              ! n is even
              if( normaltransr ) then
                 ! n is even and transr = 'n'
                 if( lower ) then
                    ! srpa for lower, normal, and n is even ( a(0:n,0:k-1) )
                    ! t1 -> a(1,0), t2 -> a(0,0), s -> a(k+1,0)
                    ! t1 -> a(1), t2 -> a(0), s -> a(k+1)
                    ijp = 0_ilp64
                    jp = 0_ilp64
                    do j = 0, k - 1
                       do i = j, n - 1
                          ij = 1_ilp64 + i + jp
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp64
                       end do
                       jp = jp + lda
                    end do
                    do i = 0, k - 1
                       do j = i, k - 1
                          ij = i + j*lda
                          arf( ij ) = conjg( ap( ijp ) )
                          ijp = ijp + 1_ilp64
                       end do
                    end do
                 else
                    ! srpa for upper, normal, and n is even ( a(0:n,0:k-1) )
                    ! t1 -> a(k+1,0) ,  t2 -> a(k,0),   s -> a(0,0)
                    ! t1 -> a(k+1), t2 -> a(k), s -> a(0)
                    ijp = 0_ilp64
                    do j = 0, k - 1
                       ij = k + 1_ilp64 + j
                       do i = 0, j
                          arf( ij ) = conjg( ap( ijp ) )
                          ijp = ijp + 1_ilp64
                          ij = ij + lda
                       end do
                    end do
                    js = 0_ilp64
                    do j = k, n - 1
                       ij = js
                       do ij = js, js + j
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp64
                       end do
                       js = js + lda
                    end do
                 end if
              else
                 ! n is even and transr = 'c'
                 if( lower ) then
                    ! srpa for lower, transpose and n is even (see paper)
                    ! t1 -> b(0,1), t2 -> b(0,0), s -> b(0,k+1)
                    ! t1 -> a(0+k), t2 -> a(0+0), s -> a(0+k*(k+1)); lda=k
                    ijp = 0_ilp64
                    do i = 0, k - 1
                       do ij = i + ( i+1 )*lda, ( n+1 )*lda - 1, lda
                          arf( ij ) = conjg( ap( ijp ) )
                          ijp = ijp + 1_ilp64
                       end do
                    end do
                    js = 0_ilp64
                    do j = 0, k - 1
                       do ij = js, js + k - j - 1
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp64
                       end do
                       js = js + lda + 1_ilp64
                    end do
                 else
                    ! srpa for upper, transpose and n is even (see paper)
                    ! t1 -> b(0,k+1),     t2 -> b(0,k),   s -> b(0,0)
                    ! t1 -> a(0+k*(k+1)), t2 -> a(0+k*k), s -> a(0+0)); lda=k
                    ijp = 0_ilp64
                    js = ( k+1 )*lda
                    do j = 0, k - 1
                       do ij = js, js + j
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp64
                       end do
                       js = js + lda
                    end do
                    do i = 0, k - 1
                       do ij = i, i + ( k+i )*lda, lda
                          arf( ij ) = conjg( ap( ijp ) )
                          ijp = ijp + 1_ilp64
                       end do
                    end do
                 end if
              end if
           end if
           return
     end subroutine stdlib_I64_ctpttf

     pure module subroutine stdlib_I64_ztpttf( transr, uplo, n, ap, arf, info )
     !! ZTPTTF copies a triangular matrix A from standard packed format (TP)
     !! to rectangular full packed format (TF).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character, intent(in) :: transr, uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: n
           ! Array Arguments 
           complex(dp), intent(in) :: ap(0_ilp64:*)
           complex(dp), intent(out) :: arf(0_ilp64:*)
        ! =====================================================================
           ! Parameters 
           ! Local Scalars 
           logical(lk) :: lower, nisodd, normaltransr
           integer(ilp64) :: n1, n2, k, nt
           integer(ilp64) :: i, j, ij
           integer(ilp64) :: ijp, jp, lda, js
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           normaltransr = stdlib_lsame( transr, 'N' )
           lower = stdlib_lsame( uplo, 'L' )
           if( .not.normaltransr .and. .not.stdlib_lsame( transr, 'C' ) ) then
              info = -1_ilp64
           else if( .not.lower .and. .not.stdlib_lsame( uplo, 'U' ) ) then
              info = -2_ilp64
           else if( n<0_ilp64 ) then
              info = -3_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'ZTPTTF', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( n==1_ilp64 ) then
              if( normaltransr ) then
                 arf( 0_ilp64 ) = ap( 0_ilp64 )
              else
                 arf( 0_ilp64 ) = conjg( ap( 0_ilp64 ) )
              end if
              return
           end if
           ! size of array arf(0:nt-1)
           nt = n*( n+1 ) / 2_ilp64
           ! set n1 and n2 depending on lower
           if( lower ) then
              n2 = n / 2_ilp64
              n1 = n - n2
           else
              n1 = n / 2_ilp64
              n2 = n - n1
           end if
           ! if n is odd, set nisodd = .true.
           ! if n is even, set k = n/2 and nisodd = .false.
           ! set lda of arf^c; arf^c is (0:(n+1)/2-1,0:n-noe)
           ! where noe = 0 if n is even, noe = 1 if n is odd
           if( mod( n, 2_ilp64 )==0_ilp64 ) then
              k = n / 2_ilp64
              nisodd = .false.
              lda = n + 1_ilp64
           else
              nisodd = .true.
              lda = n
           end if
           ! arf^c has lda rows and n+1-noe cols
           if( .not.normaltransr )lda = ( n+1 ) / 2_ilp64
           ! start execution: there are eight cases
           if( nisodd ) then
              ! n is odd
              if( normaltransr ) then
                 ! n is odd and transr = 'n'
                 if( lower ) then
                   ! srpa for lower, normal and n is odd ( a(0:n-1,0:n1-1) )
                   ! t1 -> a(0,0), t2 -> a(0,1), s -> a(n1,0)
                   ! t1 -> a(0), t2 -> a(n), s -> a(n1); lda = n
                    ijp = 0_ilp64
                    jp = 0_ilp64
                    do j = 0, n2
                       do i = j, n - 1
                          ij = i + jp
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp64
                       end do
                       jp = jp + lda
                    end do
                    do i = 0, n2 - 1
                       do j = 1 + i, n2
                          ij = i + j*lda
                          arf( ij ) = conjg( ap( ijp ) )
                          ijp = ijp + 1_ilp64
                       end do
                    end do
                 else
                   ! srpa for upper, normal and n is odd ( a(0:n-1,0:n2-1)
                   ! t1 -> a(n1+1,0), t2 -> a(n1,0), s -> a(0,0)
                   ! t1 -> a(n2), t2 -> a(n1), s -> a(0)
                    ijp = 0_ilp64
                    do j = 0, n1 - 1
                       ij = n2 + j
                       do i = 0, j
                          arf( ij ) = conjg( ap( ijp ) )
                          ijp = ijp + 1_ilp64
                          ij = ij + lda
                       end do
                    end do
                    js = 0_ilp64
                    do j = n1, n - 1
                       ij = js
                       do ij = js, js + j
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp64
                       end do
                       js = js + lda
                    end do
                 end if
              else
                 ! n is odd and transr = 'c'
                 if( lower ) then
                    ! srpa for lower, transpose and n is odd
                    ! t1 -> a(0,0) , t2 -> a(1,0) , s -> a(0,n1)
                    ! t1 -> a(0+0) , t2 -> a(1+0) , s -> a(0+n1*n1); lda=n1
                    ijp = 0_ilp64
                    do i = 0, n2
                       do ij = i*( lda+1 ), n*lda - 1, lda
                          arf( ij ) = conjg( ap( ijp ) )
                          ijp = ijp + 1_ilp64
                       end do
                    end do
                    js = 1_ilp64
                    do j = 0, n2 - 1
                       do ij = js, js + n2 - j - 1
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp64
                       end do
                       js = js + lda + 1_ilp64
                    end do
                 else
                    ! srpa for upper, transpose and n is odd
                    ! t1 -> a(0,n1+1), t2 -> a(0,n1), s -> a(0,0)
                    ! t1 -> a(n2*n2), t2 -> a(n1*n2), s -> a(0); lda = n2
                    ijp = 0_ilp64
                    js = n2*lda
                    do j = 0, n1 - 1
                       do ij = js, js + j
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp64
                       end do
                       js = js + lda
                    end do
                    do i = 0, n1
                       do ij = i, i + ( n1+i )*lda, lda
                          arf( ij ) = conjg( ap( ijp ) )
                          ijp = ijp + 1_ilp64
                       end do
                    end do
                 end if
              end if
           else
              ! n is even
              if( normaltransr ) then
                 ! n is even and transr = 'n'
                 if( lower ) then
                    ! srpa for lower, normal, and n is even ( a(0:n,0:k-1) )
                    ! t1 -> a(1,0), t2 -> a(0,0), s -> a(k+1,0)
                    ! t1 -> a(1), t2 -> a(0), s -> a(k+1)
                    ijp = 0_ilp64
                    jp = 0_ilp64
                    do j = 0, k - 1
                       do i = j, n - 1
                          ij = 1_ilp64 + i + jp
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp64
                       end do
                       jp = jp + lda
                    end do
                    do i = 0, k - 1
                       do j = i, k - 1
                          ij = i + j*lda
                          arf( ij ) = conjg( ap( ijp ) )
                          ijp = ijp + 1_ilp64
                       end do
                    end do
                 else
                    ! srpa for upper, normal, and n is even ( a(0:n,0:k-1) )
                    ! t1 -> a(k+1,0) ,  t2 -> a(k,0),   s -> a(0,0)
                    ! t1 -> a(k+1), t2 -> a(k), s -> a(0)
                    ijp = 0_ilp64
                    do j = 0, k - 1
                       ij = k + 1_ilp64 + j
                       do i = 0, j
                          arf( ij ) = conjg( ap( ijp ) )
                          ijp = ijp + 1_ilp64
                          ij = ij + lda
                       end do
                    end do
                    js = 0_ilp64
                    do j = k, n - 1
                       ij = js
                       do ij = js, js + j
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp64
                       end do
                       js = js + lda
                    end do
                 end if
              else
                 ! n is even and transr = 'c'
                 if( lower ) then
                    ! srpa for lower, transpose and n is even (see paper)
                    ! t1 -> b(0,1), t2 -> b(0,0), s -> b(0,k+1)
                    ! t1 -> a(0+k), t2 -> a(0+0), s -> a(0+k*(k+1)); lda=k
                    ijp = 0_ilp64
                    do i = 0, k - 1
                       do ij = i + ( i+1 )*lda, ( n+1 )*lda - 1, lda
                          arf( ij ) = conjg( ap( ijp ) )
                          ijp = ijp + 1_ilp64
                       end do
                    end do
                    js = 0_ilp64
                    do j = 0, k - 1
                       do ij = js, js + k - j - 1
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp64
                       end do
                       js = js + lda + 1_ilp64
                    end do
                 else
                    ! srpa for upper, transpose and n is even (see paper)
                    ! t1 -> b(0,k+1),     t2 -> b(0,k),   s -> b(0,0)
                    ! t1 -> a(0+k*(k+1)), t2 -> a(0+k*k), s -> a(0+0)); lda=k
                    ijp = 0_ilp64
                    js = ( k+1 )*lda
                    do j = 0, k - 1
                       do ij = js, js + j
                          arf( ij ) = ap( ijp )
                          ijp = ijp + 1_ilp64
                       end do
                       js = js + lda
                    end do
                    do i = 0, k - 1
                       do ij = i, i + ( k+i )*lda, lda
                          arf( ij ) = conjg( ap( ijp ) )
                          ijp = ijp + 1_ilp64
                       end do
                    end do
                 end if
              end if
           end if
           return
     end subroutine stdlib_I64_ztpttf




     pure module subroutine stdlib_I64_stpttr( uplo, n, ap, a, lda, info )
     !! STPTTR copies a triangular matrix A from standard packed format (TP)
     !! to standard full format (TR).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: n, lda
           ! Array Arguments 
           real(sp), intent(out) :: a(lda,*)
           real(sp), intent(in) :: ap(*)
        ! =====================================================================
           ! Parameters 
           ! Local Scalars 
           logical(lk) :: lower
           integer(ilp64) :: i, j, k
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           lower = stdlib_lsame( uplo, 'L' )
           if( .not.lower .and. .not.stdlib_lsame( uplo, 'U' ) ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -2_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -5_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'STPTTR', -info )
              return
           end if
           if( lower ) then
              k = 0_ilp64
              do j = 1, n
                 do i = j, n
                    k = k + 1_ilp64
                    a( i, j ) = ap( k )
                 end do
              end do
           else
              k = 0_ilp64
              do j = 1, n
                 do i = 1, j
                    k = k + 1_ilp64
                    a( i, j ) = ap( k )
                 end do
              end do
           end if
           return
     end subroutine stdlib_I64_stpttr

     pure module subroutine stdlib_I64_dtpttr( uplo, n, ap, a, lda, info )
     !! DTPTTR copies a triangular matrix A from standard packed format (TP)
     !! to standard full format (TR).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: n, lda
           ! Array Arguments 
           real(dp), intent(out) :: a(lda,*)
           real(dp), intent(in) :: ap(*)
        ! =====================================================================
           ! Parameters 
           ! Local Scalars 
           logical(lk) :: lower
           integer(ilp64) :: i, j, k
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           lower = stdlib_lsame( uplo, 'L' )
           if( .not.lower .and. .not.stdlib_lsame( uplo, 'U' ) ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -2_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -5_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'DTPTTR', -info )
              return
           end if
           if( lower ) then
              k = 0_ilp64
              do j = 1, n
                 do i = j, n
                    k = k + 1_ilp64
                    a( i, j ) = ap( k )
                 end do
              end do
           else
              k = 0_ilp64
              do j = 1, n
                 do i = 1, j
                    k = k + 1_ilp64
                    a( i, j ) = ap( k )
                 end do
              end do
           end if
           return
     end subroutine stdlib_I64_dtpttr


     pure module subroutine stdlib_I64_ctpttr( uplo, n, ap, a, lda, info )
     !! CTPTTR copies a triangular matrix A from standard packed format (TP)
     !! to standard full format (TR).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: n, lda
           ! Array Arguments 
           complex(sp), intent(out) :: a(lda,*)
           complex(sp), intent(in) :: ap(*)
        ! =====================================================================
           ! Parameters 
           ! Local Scalars 
           logical(lk) :: lower
           integer(ilp64) :: i, j, k
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           lower = stdlib_lsame( uplo, 'L' )
           if( .not.lower .and. .not.stdlib_lsame( uplo, 'U' ) ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -2_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -5_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'CTPTTR', -info )
              return
           end if
           if( lower ) then
              k = 0_ilp64
              do j = 1, n
                 do i = j, n
                    k = k + 1_ilp64
                    a( i, j ) = ap( k )
                 end do
              end do
           else
              k = 0_ilp64
              do j = 1, n
                 do i = 1, j
                    k = k + 1_ilp64
                    a( i, j ) = ap( k )
                 end do
              end do
           end if
           return
     end subroutine stdlib_I64_ctpttr

     pure module subroutine stdlib_I64_ztpttr( uplo, n, ap, a, lda, info )
     !! ZTPTTR copies a triangular matrix A from standard packed format (TP)
     !! to standard full format (TR).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: n, lda
           ! Array Arguments 
           complex(dp), intent(out) :: a(lda,*)
           complex(dp), intent(in) :: ap(*)
        ! =====================================================================
           ! Parameters 
           ! Local Scalars 
           logical(lk) :: lower
           integer(ilp64) :: i, j, k
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           lower = stdlib_lsame( uplo, 'L' )
           if( .not.lower .and. .not.stdlib_lsame( uplo, 'U' ) ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -2_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -5_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'ZTPTTR', -info )
              return
           end if
           if( lower ) then
              k = 0_ilp64
              do j = 1, n
                 do i = j, n
                    k = k + 1_ilp64
                    a( i, j ) = ap( k )
                 end do
              end do
           else
              k = 0_ilp64
              do j = 1, n
                 do i = 1, j
                    k = k + 1_ilp64
                    a( i, j ) = ap( k )
                 end do
              end do
           end if
           return
     end subroutine stdlib_I64_ztpttr




     pure module subroutine stdlib_I64_strttf( transr, uplo, n, a, lda, arf, info )
     !! STRTTF copies a triangular matrix A from standard full format (TR)
     !! to rectangular full packed format (TF) .
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character, intent(in) :: transr, uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: n, lda
           ! Array Arguments 
           real(sp), intent(in) :: a(0_ilp64:lda-1,0_ilp64:*)
           real(sp), intent(out) :: arf(0_ilp64:*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lower, nisodd, normaltransr
           integer(ilp64) :: i, ij, j, k, l, n1, n2, nt, nx2, np1x2
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           normaltransr = stdlib_lsame( transr, 'N' )
           lower = stdlib_lsame( uplo, 'L' )
           if( .not.normaltransr .and. .not.stdlib_lsame( transr, 'T' ) ) then
              info = -1_ilp64
           else if( .not.lower .and. .not.stdlib_lsame( uplo, 'U' ) ) then
              info = -2_ilp64
           else if( n<0_ilp64 ) then
              info = -3_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -5_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'STRTTF', -info )
              return
           end if
           ! quick return if possible
           if( n<=1_ilp64 ) then
              if( n==1_ilp64 ) then
                 arf( 0_ilp64 ) = a( 0_ilp64, 0_ilp64 )
              end if
              return
           end if
           ! size of array arf(0:nt-1)
           nt = n*( n+1 ) / 2_ilp64
           ! set n1 and n2 depending on lower: for n even n1=n2=k
           if( lower ) then
              n2 = n / 2_ilp64
              n1 = n - n2
           else
              n1 = n / 2_ilp64
              n2 = n - n1
           end if
           ! if n is odd, set nisodd = .true., lda=n+1 and a is (n+1)--by--k2.
           ! if n is even, set k = n/2 and nisodd = .false., lda=n and a is
           ! n--by--(n+1)/2.
           if( mod( n, 2_ilp64 )==0_ilp64 ) then
              k = n / 2_ilp64
              nisodd = .false.
              if( .not.lower )np1x2 = n + n + 2_ilp64
           else
              nisodd = .true.
              if( .not.lower )nx2 = n + n
           end if
           if( nisodd ) then
              ! n is odd
              if( normaltransr ) then
                 ! n is odd and transr = 'n'
                 if( lower ) then
                    ! n is odd, transr = 'n', and uplo = 'l'
                    ij = 0_ilp64
                    do j = 0, n2
                       do i = n1, n2 + j
                          arf( ij ) = a( n2+j, i )
                          ij = ij + 1_ilp64
                       end do
                       do i = j, n - 1
                          arf( ij ) = a( i, j )
                          ij = ij + 1_ilp64
                       end do
                    end do
                 else
                    ! n is odd, transr = 'n', and uplo = 'u'
                    ij = nt - n
                    do j = n - 1, n1, -1
                       do i = 0, j
                          arf( ij ) = a( i, j )
                          ij = ij + 1_ilp64
                       end do
                       do l = j - n1, n1 - 1
                          arf( ij ) = a( j-n1, l )
                          ij = ij + 1_ilp64
                       end do
                       ij = ij - nx2
                    end do
                 end if
              else
                 ! n is odd and transr = 't'
                 if( lower ) then
                    ! n is odd, transr = 't', and uplo = 'l'
                    ij = 0_ilp64
                    do j = 0, n2 - 1
                       do i = 0, j
                          arf( ij ) = a( j, i )
                          ij = ij + 1_ilp64
                       end do
                       do i = n1 + j, n - 1
                          arf( ij ) = a( i, n1+j )
                          ij = ij + 1_ilp64
                       end do
                    end do
                    do j = n2, n - 1
                       do i = 0, n1 - 1
                          arf( ij ) = a( j, i )
                          ij = ij + 1_ilp64
                       end do
                    end do
                 else
                    ! n is odd, transr = 't', and uplo = 'u'
                    ij = 0_ilp64
                    do j = 0, n1
                       do i = n1, n - 1
                          arf( ij ) = a( j, i )
                          ij = ij + 1_ilp64
                       end do
                    end do
                    do j = 0, n1 - 1
                       do i = 0, j
                          arf( ij ) = a( i, j )
                          ij = ij + 1_ilp64
                       end do
                       do l = n2 + j, n - 1
                          arf( ij ) = a( n2+j, l )
                          ij = ij + 1_ilp64
                       end do
                    end do
                 end if
              end if
           else
              ! n is even
              if( normaltransr ) then
                 ! n is even and transr = 'n'
                 if( lower ) then
                    ! n is even, transr = 'n', and uplo = 'l'
                    ij = 0_ilp64
                    do j = 0, k - 1
                       do i = k, k + j
                          arf( ij ) = a( k+j, i )
                          ij = ij + 1_ilp64
                       end do
                       do i = j, n - 1
                          arf( ij ) = a( i, j )
                          ij = ij + 1_ilp64
                       end do
                    end do
                 else
                    ! n is even, transr = 'n', and uplo = 'u'
                    ij = nt - n - 1_ilp64
                    do j = n - 1, k, -1
                       do i = 0, j
                          arf( ij ) = a( i, j )
                          ij = ij + 1_ilp64
                       end do
                       do l = j - k, k - 1
                          arf( ij ) = a( j-k, l )
                          ij = ij + 1_ilp64
                       end do
                       ij = ij - np1x2
                    end do
                 end if
              else
                 ! n is even and transr = 't'
                 if( lower ) then
                    ! n is even, transr = 't', and uplo = 'l'
                    ij = 0_ilp64
                    j = k
                    do i = k, n - 1
                       arf( ij ) = a( i, j )
                       ij = ij + 1_ilp64
                    end do
                    do j = 0, k - 2
                       do i = 0, j
                          arf( ij ) = a( j, i )
                          ij = ij + 1_ilp64
                       end do
                       do i = k + 1 + j, n - 1
                          arf( ij ) = a( i, k+1+j )
                          ij = ij + 1_ilp64
                       end do
                    end do
                    do j = k - 1, n - 1
                       do i = 0, k - 1
                          arf( ij ) = a( j, i )
                          ij = ij + 1_ilp64
                       end do
                    end do
                 else
                    ! n is even, transr = 't', and uplo = 'u'
                    ij = 0_ilp64
                    do j = 0, k
                       do i = k, n - 1
                          arf( ij ) = a( j, i )
                          ij = ij + 1_ilp64
                       end do
                    end do
                    do j = 0, k - 2
                       do i = 0, j
                          arf( ij ) = a( i, j )
                          ij = ij + 1_ilp64
                       end do
                       do l = k + 1 + j, n - 1
                          arf( ij ) = a( k+1+j, l )
                          ij = ij + 1_ilp64
                       end do
                    end do
                    ! note that here, on exit of the loop, j = k-1
                    do i = 0, j
                       arf( ij ) = a( i, j )
                       ij = ij + 1_ilp64
                    end do
                 end if
              end if
           end if
           return
     end subroutine stdlib_I64_strttf

     pure module subroutine stdlib_I64_dtrttf( transr, uplo, n, a, lda, arf, info )
     !! DTRTTF copies a triangular matrix A from standard full format (TR)
     !! to rectangular full packed format (TF) .
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character, intent(in) :: transr, uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: n, lda
           ! Array Arguments 
           real(dp), intent(in) :: a(0_ilp64:lda-1,0_ilp64:*)
           real(dp), intent(out) :: arf(0_ilp64:*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lower, nisodd, normaltransr
           integer(ilp64) :: i, ij, j, k, l, n1, n2, nt, nx2, np1x2
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           normaltransr = stdlib_lsame( transr, 'N' )
           lower = stdlib_lsame( uplo, 'L' )
           if( .not.normaltransr .and. .not.stdlib_lsame( transr, 'T' ) ) then
              info = -1_ilp64
           else if( .not.lower .and. .not.stdlib_lsame( uplo, 'U' ) ) then
              info = -2_ilp64
           else if( n<0_ilp64 ) then
              info = -3_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -5_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'DTRTTF', -info )
              return
           end if
           ! quick return if possible
           if( n<=1_ilp64 ) then
              if( n==1_ilp64 ) then
                 arf( 0_ilp64 ) = a( 0_ilp64, 0_ilp64 )
              end if
              return
           end if
           ! size of array arf(0:nt-1)
           nt = n*( n+1 ) / 2_ilp64
           ! set n1 and n2 depending on lower: for n even n1=n2=k
           if( lower ) then
              n2 = n / 2_ilp64
              n1 = n - n2
           else
              n1 = n / 2_ilp64
              n2 = n - n1
           end if
           ! if n is odd, set nisodd = .true., lda=n+1 and a is (n+1)--by--k2.
           ! if n is even, set k = n/2 and nisodd = .false., lda=n and a is
           ! n--by--(n+1)/2.
           if( mod( n, 2_ilp64 )==0_ilp64 ) then
              k = n / 2_ilp64
              nisodd = .false.
              if( .not.lower )np1x2 = n + n + 2_ilp64
           else
              nisodd = .true.
              if( .not.lower )nx2 = n + n
           end if
           if( nisodd ) then
              ! n is odd
              if( normaltransr ) then
                 ! n is odd and transr = 'n'
                 if( lower ) then
                    ! n is odd, transr = 'n', and uplo = 'l'
                    ij = 0_ilp64
                    do j = 0, n2
                       do i = n1, n2 + j
                          arf( ij ) = a( n2+j, i )
                          ij = ij + 1_ilp64
                       end do
                       do i = j, n - 1
                          arf( ij ) = a( i, j )
                          ij = ij + 1_ilp64
                       end do
                    end do
                 else
                    ! n is odd, transr = 'n', and uplo = 'u'
                    ij = nt - n
                    do j = n - 1, n1, -1
                       do i = 0, j
                          arf( ij ) = a( i, j )
                          ij = ij + 1_ilp64
                       end do
                       do l = j - n1, n1 - 1
                          arf( ij ) = a( j-n1, l )
                          ij = ij + 1_ilp64
                       end do
                       ij = ij - nx2
                    end do
                 end if
              else
                 ! n is odd and transr = 't'
                 if( lower ) then
                    ! n is odd, transr = 't', and uplo = 'l'
                    ij = 0_ilp64
                    do j = 0, n2 - 1
                       do i = 0, j
                          arf( ij ) = a( j, i )
                          ij = ij + 1_ilp64
                       end do
                       do i = n1 + j, n - 1
                          arf( ij ) = a( i, n1+j )
                          ij = ij + 1_ilp64
                       end do
                    end do
                    do j = n2, n - 1
                       do i = 0, n1 - 1
                          arf( ij ) = a( j, i )
                          ij = ij + 1_ilp64
                       end do
                    end do
                 else
                    ! n is odd, transr = 't', and uplo = 'u'
                    ij = 0_ilp64
                    do j = 0, n1
                       do i = n1, n - 1
                          arf( ij ) = a( j, i )
                          ij = ij + 1_ilp64
                       end do
                    end do
                    do j = 0, n1 - 1
                       do i = 0, j
                          arf( ij ) = a( i, j )
                          ij = ij + 1_ilp64
                       end do
                       do l = n2 + j, n - 1
                          arf( ij ) = a( n2+j, l )
                          ij = ij + 1_ilp64
                       end do
                    end do
                 end if
              end if
           else
              ! n is even
              if( normaltransr ) then
                 ! n is even and transr = 'n'
                 if( lower ) then
                    ! n is even, transr = 'n', and uplo = 'l'
                    ij = 0_ilp64
                    do j = 0, k - 1
                       do i = k, k + j
                          arf( ij ) = a( k+j, i )
                          ij = ij + 1_ilp64
                       end do
                       do i = j, n - 1
                          arf( ij ) = a( i, j )
                          ij = ij + 1_ilp64
                       end do
                    end do
                 else
                    ! n is even, transr = 'n', and uplo = 'u'
                    ij = nt - n - 1_ilp64
                    do j = n - 1, k, -1
                       do i = 0, j
                          arf( ij ) = a( i, j )
                          ij = ij + 1_ilp64
                       end do
                       do l = j - k, k - 1
                          arf( ij ) = a( j-k, l )
                          ij = ij + 1_ilp64
                       end do
                       ij = ij - np1x2
                    end do
                 end if
              else
                 ! n is even and transr = 't'
                 if( lower ) then
                    ! n is even, transr = 't', and uplo = 'l'
                    ij = 0_ilp64
                    j = k
                    do i = k, n - 1
                       arf( ij ) = a( i, j )
                       ij = ij + 1_ilp64
                    end do
                    do j = 0, k - 2
                       do i = 0, j
                          arf( ij ) = a( j, i )
                          ij = ij + 1_ilp64
                       end do
                       do i = k + 1 + j, n - 1
                          arf( ij ) = a( i, k+1+j )
                          ij = ij + 1_ilp64
                       end do
                    end do
                    do j = k - 1, n - 1
                       do i = 0, k - 1
                          arf( ij ) = a( j, i )
                          ij = ij + 1_ilp64
                       end do
                    end do
                 else
                    ! n is even, transr = 't', and uplo = 'u'
                    ij = 0_ilp64
                    do j = 0, k
                       do i = k, n - 1
                          arf( ij ) = a( j, i )
                          ij = ij + 1_ilp64
                       end do
                    end do
                    do j = 0, k - 2
                       do i = 0, j
                          arf( ij ) = a( i, j )
                          ij = ij + 1_ilp64
                       end do
                       do l = k + 1 + j, n - 1
                          arf( ij ) = a( k+1+j, l )
                          ij = ij + 1_ilp64
                       end do
                    end do
                    ! note that here, on exit of the loop, j = k-1
                    do i = 0, j
                       arf( ij ) = a( i, j )
                       ij = ij + 1_ilp64
                    end do
                 end if
              end if
           end if
           return
     end subroutine stdlib_I64_dtrttf


     pure module subroutine stdlib_I64_ctrttf( transr, uplo, n, a, lda, arf, info )
     !! CTRTTF copies a triangular matrix A from standard full format (TR)
     !! to rectangular full packed format (TF) .
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character, intent(in) :: transr, uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: n, lda
           ! Array Arguments 
           complex(sp), intent(in) :: a(0_ilp64:lda-1,0_ilp64:*)
           complex(sp), intent(out) :: arf(0_ilp64:*)
        ! =====================================================================
           ! Parameters 
           ! Local Scalars 
           logical(lk) :: lower, nisodd, normaltransr
           integer(ilp64) :: i, ij, j, k, l, n1, n2, nt, nx2, np1x2
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           normaltransr = stdlib_lsame( transr, 'N' )
           lower = stdlib_lsame( uplo, 'L' )
           if( .not.normaltransr .and. .not.stdlib_lsame( transr, 'C' ) ) then
              info = -1_ilp64
           else if( .not.lower .and. .not.stdlib_lsame( uplo, 'U' ) ) then
              info = -2_ilp64
           else if( n<0_ilp64 ) then
              info = -3_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -5_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'CTRTTF', -info )
              return
           end if
           ! quick return if possible
           if( n<=1_ilp64 ) then
              if( n==1_ilp64 ) then
                 if( normaltransr ) then
                    arf( 0_ilp64 ) = a( 0_ilp64, 0_ilp64 )
                 else
                    arf( 0_ilp64 ) = conjg( a( 0_ilp64, 0_ilp64 ) )
                 end if
              end if
              return
           end if
           ! size of array arf(1:2,0:nt-1)
           nt = n*( n+1 ) / 2_ilp64
           ! set n1 and n2 depending on lower: for n even n1=n2=k
           if( lower ) then
              n2 = n / 2_ilp64
              n1 = n - n2
           else
              n1 = n / 2_ilp64
              n2 = n - n1
           end if
           ! if n is odd, set nisodd = .true., lda=n+1 and a is (n+1)--by--k2.
           ! if n is even, set k = n/2 and nisodd = .false., lda=n and a is
           ! n--by--(n+1)/2.
           if( mod( n, 2_ilp64 )==0_ilp64 ) then
              k = n / 2_ilp64
              nisodd = .false.
              if( .not.lower )np1x2 = n + n + 2_ilp64
           else
              nisodd = .true.
              if( .not.lower )nx2 = n + n
           end if
           if( nisodd ) then
              ! n is odd
              if( normaltransr ) then
                 ! n is odd and transr = 'n'
                 if( lower ) then
                   ! srpa for lower, normal and n is odd ( a(0:n-1,0:n1-1) )
                   ! t1 -> a(0,0), t2 -> a(0,1), s -> a(n1,0)
                   ! t1 -> a(0), t2 -> a(n), s -> a(n1); lda=n
                    ij = 0_ilp64
                    do j = 0, n2
                       do i = n1, n2 + j
                          arf( ij ) = conjg( a( n2+j, i ) )
                          ij = ij + 1_ilp64
                       end do
                       do i = j, n - 1
                          arf( ij ) = a( i, j )
                          ij = ij + 1_ilp64
                       end do
                    end do
                 else
                   ! srpa for upper, normal and n is odd ( a(0:n-1,0:n2-1)
                   ! t1 -> a(n1+1,0), t2 -> a(n1,0), s -> a(0,0)
                   ! t1 -> a(n2), t2 -> a(n1), s -> a(0); lda=n
                    ij = nt - n
                    do j = n - 1, n1, -1
                       do i = 0, j
                          arf( ij ) = a( i, j )
                          ij = ij + 1_ilp64
                       end do
                       do l = j - n1, n1 - 1
                          arf( ij ) = conjg( a( j-n1, l ) )
                          ij = ij + 1_ilp64
                       end do
                       ij = ij - nx2
                    end do
                 end if
              else
                 ! n is odd and transr = 'c'
                 if( lower ) then
                    ! srpa for lower, transpose and n is odd
                    ! t1 -> a(0,0) , t2 -> a(1,0) , s -> a(0,n1)
                    ! t1 -> a(0+0) , t2 -> a(1+0) , s -> a(0+n1*n1); lda=n1
                    ij = 0_ilp64
                    do j = 0, n2 - 1
                       do i = 0, j
                          arf( ij ) = conjg( a( j, i ) )
                          ij = ij + 1_ilp64
                       end do
                       do i = n1 + j, n - 1
                          arf( ij ) = a( i, n1+j )
                          ij = ij + 1_ilp64
                       end do
                    end do
                    do j = n2, n - 1
                       do i = 0, n1 - 1
                          arf( ij ) = conjg( a( j, i ) )
                          ij = ij + 1_ilp64
                       end do
                    end do
                 else
                    ! srpa for upper, transpose and n is odd
                    ! t1 -> a(0,n1+1), t2 -> a(0,n1), s -> a(0,0)
                    ! t1 -> a(n2*n2), t2 -> a(n1*n2), s -> a(0); lda=n2
                    ij = 0_ilp64
                    do j = 0, n1
                       do i = n1, n - 1
                          arf( ij ) = conjg( a( j, i ) )
                          ij = ij + 1_ilp64
                       end do
                    end do
                    do j = 0, n1 - 1
                       do i = 0, j
                          arf( ij ) = a( i, j )
                          ij = ij + 1_ilp64
                       end do
                       do l = n2 + j, n - 1
                          arf( ij ) = conjg( a( n2+j, l ) )
                          ij = ij + 1_ilp64
                       end do
                    end do
                 end if
              end if
           else
              ! n is even
              if( normaltransr ) then
                 ! n is even and transr = 'n'
                 if( lower ) then
                    ! srpa for lower, normal, and n is even ( a(0:n,0:k-1) )
                    ! t1 -> a(1,0), t2 -> a(0,0), s -> a(k+1,0)
                    ! t1 -> a(1), t2 -> a(0), s -> a(k+1); lda=n+1
                    ij = 0_ilp64
                    do j = 0, k - 1
                       do i = k, k + j
                          arf( ij ) = conjg( a( k+j, i ) )
                          ij = ij + 1_ilp64
                       end do
                       do i = j, n - 1
                          arf( ij ) = a( i, j )
                          ij = ij + 1_ilp64
                       end do
                    end do
                 else
                    ! srpa for upper, normal, and n is even ( a(0:n,0:k-1) )
                    ! t1 -> a(k+1,0) ,  t2 -> a(k,0),   s -> a(0,0)
                    ! t1 -> a(k+1), t2 -> a(k), s -> a(0); lda=n+1
                    ij = nt - n - 1_ilp64
                    do j = n - 1, k, -1
                       do i = 0, j
                          arf( ij ) = a( i, j )
                          ij = ij + 1_ilp64
                       end do
                       do l = j - k, k - 1
                          arf( ij ) = conjg( a( j-k, l ) )
                          ij = ij + 1_ilp64
                       end do
                       ij = ij - np1x2
                    end do
                 end if
              else
                 ! n is even and transr = 'c'
                 if( lower ) then
                    ! srpa for lower, transpose and n is even (see paper, a=b)
                    ! t1 -> a(0,1) , t2 -> a(0,0) , s -> a(0,k+1) :
                    ! t1 -> a(0+k) , t2 -> a(0+0) , s -> a(0+k*(k+1)); lda=k
                    ij = 0_ilp64
                    j = k
                    do i = k, n - 1
                       arf( ij ) = a( i, j )
                       ij = ij + 1_ilp64
                    end do
                    do j = 0, k - 2
                       do i = 0, j
                          arf( ij ) = conjg( a( j, i ) )
                          ij = ij + 1_ilp64
                       end do
                       do i = k + 1 + j, n - 1
                          arf( ij ) = a( i, k+1+j )
                          ij = ij + 1_ilp64
                       end do
                    end do
                    do j = k - 1, n - 1
                       do i = 0, k - 1
                          arf( ij ) = conjg( a( j, i ) )
                          ij = ij + 1_ilp64
                       end do
                    end do
                 else
                    ! srpa for upper, transpose and n is even (see paper, a=b)
                    ! t1 -> a(0,k+1) , t2 -> a(0,k) , s -> a(0,0)
                    ! t1 -> a(0+k*(k+1)) , t2 -> a(0+k*k) , s -> a(0+0)); lda=k
                    ij = 0_ilp64
                    do j = 0, k
                       do i = k, n - 1
                          arf( ij ) = conjg( a( j, i ) )
                          ij = ij + 1_ilp64
                       end do
                    end do
                    do j = 0, k - 2
                       do i = 0, j
                          arf( ij ) = a( i, j )
                          ij = ij + 1_ilp64
                       end do
                       do l = k + 1 + j, n - 1
                          arf( ij ) = conjg( a( k+1+j, l ) )
                          ij = ij + 1_ilp64
                       end do
                    end do
                    ! note that here j = k-1
                    do i = 0, j
                       arf( ij ) = a( i, j )
                       ij = ij + 1_ilp64
                    end do
                 end if
              end if
           end if
           return
     end subroutine stdlib_I64_ctrttf

     pure module subroutine stdlib_I64_ztrttf( transr, uplo, n, a, lda, arf, info )
     !! ZTRTTF copies a triangular matrix A from standard full format (TR)
     !! to rectangular full packed format (TF) .
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character, intent(in) :: transr, uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: n, lda
           ! Array Arguments 
           complex(dp), intent(in) :: a(0_ilp64:lda-1,0_ilp64:*)
           complex(dp), intent(out) :: arf(0_ilp64:*)
        ! =====================================================================
           ! Parameters 
           ! Local Scalars 
           logical(lk) :: lower, nisodd, normaltransr
           integer(ilp64) :: i, ij, j, k, l, n1, n2, nt, nx2, np1x2
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           normaltransr = stdlib_lsame( transr, 'N' )
           lower = stdlib_lsame( uplo, 'L' )
           if( .not.normaltransr .and. .not.stdlib_lsame( transr, 'C' ) ) then
              info = -1_ilp64
           else if( .not.lower .and. .not.stdlib_lsame( uplo, 'U' ) ) then
              info = -2_ilp64
           else if( n<0_ilp64 ) then
              info = -3_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -5_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'ZTRTTF', -info )
              return
           end if
           ! quick return if possible
           if( n<=1_ilp64 ) then
              if( n==1_ilp64 ) then
                 if( normaltransr ) then
                    arf( 0_ilp64 ) = a( 0_ilp64, 0_ilp64 )
                 else
                    arf( 0_ilp64 ) = conjg( a( 0_ilp64, 0_ilp64 ) )
                 end if
              end if
              return
           end if
           ! size of array arf(1:2,0:nt-1)
           nt = n*( n+1 ) / 2_ilp64
           ! set n1 and n2 depending on lower: for n even n1=n2=k
           if( lower ) then
              n2 = n / 2_ilp64
              n1 = n - n2
           else
              n1 = n / 2_ilp64
              n2 = n - n1
           end if
           ! if n is odd, set nisodd = .true., lda=n+1 and a is (n+1)--by--k2.
           ! if n is even, set k = n/2 and nisodd = .false., lda=n and a is
           ! n--by--(n+1)/2.
           if( mod( n, 2_ilp64 )==0_ilp64 ) then
              k = n / 2_ilp64
              nisodd = .false.
              if( .not.lower )np1x2 = n + n + 2_ilp64
           else
              nisodd = .true.
              if( .not.lower )nx2 = n + n
           end if
           if( nisodd ) then
              ! n is odd
              if( normaltransr ) then
                 ! n is odd and transr = 'n'
                 if( lower ) then
                   ! srpa for lower, normal and n is odd ( a(0:n-1,0:n1-1) )
                   ! t1 -> a(0,0), t2 -> a(0,1), s -> a(n1,0)
                   ! t1 -> a(0), t2 -> a(n), s -> a(n1); lda=n
                    ij = 0_ilp64
                    do j = 0, n2
                       do i = n1, n2 + j
                          arf( ij ) = conjg( a( n2+j, i ) )
                          ij = ij + 1_ilp64
                       end do
                       do i = j, n - 1
                          arf( ij ) = a( i, j )
                          ij = ij + 1_ilp64
                       end do
                    end do
                 else
                   ! srpa for upper, normal and n is odd ( a(0:n-1,0:n2-1)
                   ! t1 -> a(n1+1,0), t2 -> a(n1,0), s -> a(0,0)
                   ! t1 -> a(n2), t2 -> a(n1), s -> a(0); lda=n
                    ij = nt - n
                    do j = n - 1, n1, -1
                       do i = 0, j
                          arf( ij ) = a( i, j )
                          ij = ij + 1_ilp64
                       end do
                       do l = j - n1, n1 - 1
                          arf( ij ) = conjg( a( j-n1, l ) )
                          ij = ij + 1_ilp64
                       end do
                       ij = ij - nx2
                    end do
                 end if
              else
                 ! n is odd and transr = 'c'
                 if( lower ) then
                    ! srpa for lower, transpose and n is odd
                    ! t1 -> a(0,0) , t2 -> a(1,0) , s -> a(0,n1)
                    ! t1 -> a(0+0) , t2 -> a(1+0) , s -> a(0+n1*n1); lda=n1
                    ij = 0_ilp64
                    do j = 0, n2 - 1
                       do i = 0, j
                          arf( ij ) = conjg( a( j, i ) )
                          ij = ij + 1_ilp64
                       end do
                       do i = n1 + j, n - 1
                          arf( ij ) = a( i, n1+j )
                          ij = ij + 1_ilp64
                       end do
                    end do
                    do j = n2, n - 1
                       do i = 0, n1 - 1
                          arf( ij ) = conjg( a( j, i ) )
                          ij = ij + 1_ilp64
                       end do
                    end do
                 else
                    ! srpa for upper, transpose and n is odd
                    ! t1 -> a(0,n1+1), t2 -> a(0,n1), s -> a(0,0)
                    ! t1 -> a(n2*n2), t2 -> a(n1*n2), s -> a(0); lda=n2
                    ij = 0_ilp64
                    do j = 0, n1
                       do i = n1, n - 1
                          arf( ij ) = conjg( a( j, i ) )
                          ij = ij + 1_ilp64
                       end do
                    end do
                    do j = 0, n1 - 1
                       do i = 0, j
                          arf( ij ) = a( i, j )
                          ij = ij + 1_ilp64
                       end do
                       do l = n2 + j, n - 1
                          arf( ij ) = conjg( a( n2+j, l ) )
                          ij = ij + 1_ilp64
                       end do
                    end do
                 end if
              end if
           else
              ! n is even
              if( normaltransr ) then
                 ! n is even and transr = 'n'
                 if( lower ) then
                    ! srpa for lower, normal, and n is even ( a(0:n,0:k-1) )
                    ! t1 -> a(1,0), t2 -> a(0,0), s -> a(k+1,0)
                    ! t1 -> a(1), t2 -> a(0), s -> a(k+1); lda=n+1
                    ij = 0_ilp64
                    do j = 0, k - 1
                       do i = k, k + j
                          arf( ij ) = conjg( a( k+j, i ) )
                          ij = ij + 1_ilp64
                       end do
                       do i = j, n - 1
                          arf( ij ) = a( i, j )
                          ij = ij + 1_ilp64
                       end do
                    end do
                 else
                    ! srpa for upper, normal, and n is even ( a(0:n,0:k-1) )
                    ! t1 -> a(k+1,0) ,  t2 -> a(k,0),   s -> a(0,0)
                    ! t1 -> a(k+1), t2 -> a(k), s -> a(0); lda=n+1
                    ij = nt - n - 1_ilp64
                    do j = n - 1, k, -1
                       do i = 0, j
                          arf( ij ) = a( i, j )
                          ij = ij + 1_ilp64
                       end do
                       do l = j - k, k - 1
                          arf( ij ) = conjg( a( j-k, l ) )
                          ij = ij + 1_ilp64
                       end do
                       ij = ij - np1x2
                    end do
                 end if
              else
                 ! n is even and transr = 'c'
                 if( lower ) then
                    ! srpa for lower, transpose and n is even (see paper, a=b)
                    ! t1 -> a(0,1) , t2 -> a(0,0) , s -> a(0,k+1) :
                    ! t1 -> a(0+k) , t2 -> a(0+0) , s -> a(0+k*(k+1)); lda=k
                    ij = 0_ilp64
                    j = k
                    do i = k, n - 1
                       arf( ij ) = a( i, j )
                       ij = ij + 1_ilp64
                    end do
                    do j = 0, k - 2
                       do i = 0, j
                          arf( ij ) = conjg( a( j, i ) )
                          ij = ij + 1_ilp64
                       end do
                       do i = k + 1 + j, n - 1
                          arf( ij ) = a( i, k+1+j )
                          ij = ij + 1_ilp64
                       end do
                    end do
                    do j = k - 1, n - 1
                       do i = 0, k - 1
                          arf( ij ) = conjg( a( j, i ) )
                          ij = ij + 1_ilp64
                       end do
                    end do
                 else
                    ! srpa for upper, transpose and n is even (see paper, a=b)
                    ! t1 -> a(0,k+1) , t2 -> a(0,k) , s -> a(0,0)
                    ! t1 -> a(0+k*(k+1)) , t2 -> a(0+k*k) , s -> a(0+0)); lda=k
                    ij = 0_ilp64
                    do j = 0, k
                       do i = k, n - 1
                          arf( ij ) = conjg( a( j, i ) )
                          ij = ij + 1_ilp64
                       end do
                    end do
                    do j = 0, k - 2
                       do i = 0, j
                          arf( ij ) = a( i, j )
                          ij = ij + 1_ilp64
                       end do
                       do l = k + 1 + j, n - 1
                          arf( ij ) = conjg( a( k+1+j, l ) )
                          ij = ij + 1_ilp64
                       end do
                    end do
                    ! note that here j = k-1
                    do i = 0, j
                       arf( ij ) = a( i, j )
                       ij = ij + 1_ilp64
                    end do
                 end if
              end if
           end if
           return
     end subroutine stdlib_I64_ztrttf




     pure module subroutine stdlib_I64_strttp( uplo, n, a, lda, ap, info )
     !! STRTTP copies a triangular matrix A from full format (TR) to standard
     !! packed format (TP).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: n, lda
           ! Array Arguments 
           real(sp), intent(in) :: a(lda,*)
           real(sp), intent(out) :: ap(*)
        ! =====================================================================
           ! Parameters 
           ! Local Scalars 
           logical(lk) :: lower
           integer(ilp64) :: i, j, k
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           lower = stdlib_lsame( uplo, 'L' )
           if( .not.lower .and. .not.stdlib_lsame( uplo, 'U' ) ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -2_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -4_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'STRTTP', -info )
              return
           end if
           if( lower ) then
              k = 0_ilp64
              do j = 1, n
                 do i = j, n
                    k = k + 1_ilp64
                    ap( k ) = a( i, j )
                 end do
              end do
           else
              k = 0_ilp64
              do j = 1, n
                 do i = 1, j
                    k = k + 1_ilp64
                    ap( k ) = a( i, j )
                 end do
              end do
           end if
           return
     end subroutine stdlib_I64_strttp

     pure module subroutine stdlib_I64_dtrttp( uplo, n, a, lda, ap, info )
     !! DTRTTP copies a triangular matrix A from full format (TR) to standard
     !! packed format (TP).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: n, lda
           ! Array Arguments 
           real(dp), intent(in) :: a(lda,*)
           real(dp), intent(out) :: ap(*)
        ! =====================================================================
           ! Parameters 
           ! Local Scalars 
           logical(lk) :: lower
           integer(ilp64) :: i, j, k
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           lower = stdlib_lsame( uplo, 'L' )
           if( .not.lower .and. .not.stdlib_lsame( uplo, 'U' ) ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -2_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -4_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'DTRTTP', -info )
              return
           end if
           if( lower ) then
              k = 0_ilp64
              do j = 1, n
                 do i = j, n
                    k = k + 1_ilp64
                    ap( k ) = a( i, j )
                 end do
              end do
           else
              k = 0_ilp64
              do j = 1, n
                 do i = 1, j
                    k = k + 1_ilp64
                    ap( k ) = a( i, j )
                 end do
              end do
           end if
           return
     end subroutine stdlib_I64_dtrttp


     pure module subroutine stdlib_I64_ctrttp( uplo, n, a, lda, ap, info )
     !! CTRTTP copies a triangular matrix A from full format (TR) to standard
     !! packed format (TP).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: n, lda
           ! Array Arguments 
           complex(sp), intent(in) :: a(lda,*)
           complex(sp), intent(out) :: ap(*)
        ! =====================================================================
           ! Parameters 
           ! Local Scalars 
           logical(lk) :: lower
           integer(ilp64) :: i, j, k
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           lower = stdlib_lsame( uplo, 'L' )
           if( .not.lower .and. .not.stdlib_lsame( uplo, 'U' ) ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -2_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -4_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'CTRTTP', -info )
              return
           end if
           if( lower ) then
              k = 0_ilp64
              do j = 1, n
                 do i = j, n
                    k = k + 1_ilp64
                    ap( k ) = a( i, j )
                 end do
              end do
           else
              k = 0_ilp64
              do j = 1, n
                 do i = 1, j
                    k = k + 1_ilp64
                    ap( k ) = a( i, j )
                 end do
              end do
           end if
           return
     end subroutine stdlib_I64_ctrttp

     pure module subroutine stdlib_I64_ztrttp( uplo, n, a, lda, ap, info )
     !! ZTRTTP copies a triangular matrix A from full format (TR) to standard
     !! packed format (TP).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: n, lda
           ! Array Arguments 
           complex(dp), intent(in) :: a(lda,*)
           complex(dp), intent(out) :: ap(*)
        ! =====================================================================
           ! Parameters 
           ! Local Scalars 
           logical(lk) :: lower
           integer(ilp64) :: i, j, k
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           lower = stdlib_lsame( uplo, 'L' )
           if( .not.lower .and. .not.stdlib_lsame( uplo, 'U' ) ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -2_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -4_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'ZTRTTP', -info )
              return
           end if
           if( lower ) then
              k = 0_ilp64
              do j = 1, n
                 do i = j, n
                    k = k + 1_ilp64
                    ap( k ) = a( i, j )
                 end do
              end do
           else
              k = 0_ilp64
              do j = 1, n
                 do i = 1, j
                    k = k + 1_ilp64
                    ap( k ) = a( i, j )
                 end do
              end do
           end if
           return
     end subroutine stdlib_I64_ztrttp




     pure module subroutine stdlib_I64_dlag2s( m, n, a, lda, sa, ldsa, info )
     !! DLAG2S converts a DOUBLE PRECISION matrix, SA, to a SINGLE
     !! PRECISION matrix, A.
     !! RMAX is the overflow for the SINGLE PRECISION arithmetic
     !! DLAG2S checks that all the entries of A are between -RMAX and
     !! RMAX. If not the conversion is aborted and a flag is raised.
     !! This is an auxiliary routine so there is no argument checking.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldsa, m, n
           ! Array Arguments 
           real(sp), intent(out) :: sa(ldsa,*)
           real(dp), intent(in) :: a(lda,*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp64) :: i, j
           real(dp) :: rmax
           ! Executable Statements 
           rmax = stdlib_I64_slamch( 'O' )
           do j = 1, n
              do i = 1, m
                 if( ( a( i, j )<-rmax ) .or. ( a( i, j )>rmax ) ) then
                    info = 1_ilp64
                    go to 30
                 end if
                 sa( i, j ) = a( i, j )
              end do
           end do
           info = 0_ilp64
           30 continue
           return
     end subroutine stdlib_I64_dlag2s




     pure module subroutine stdlib_I64_dlat2s( uplo, n, a, lda, sa, ldsa, info )
     !! DLAT2S converts a DOUBLE PRECISION triangular matrix, SA, to a SINGLE
     !! PRECISION triangular matrix, A.
     !! RMAX is the overflow for the SINGLE PRECISION arithmetic
     !! DLAS2S checks that all the entries of A are between -RMAX and
     !! RMAX. If not the conversion is aborted and a flag is raised.
     !! This is an auxiliary routine so there is no argument checking.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldsa, n
           ! Array Arguments 
           real(sp), intent(out) :: sa(ldsa,*)
           real(dp), intent(in) :: a(lda,*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp64) :: i, j
           real(dp) :: rmax
           logical(lk) :: upper
           ! Executable Statements 
           rmax = stdlib_I64_slamch( 'O' )
           upper = stdlib_lsame( uplo, 'U' )
           if( upper ) then
              do j = 1, n
                 do i = 1, j
                    if( ( a( i, j )<-rmax ) .or. ( a( i, j )>rmax ) )then
                       info = 1_ilp64
                       go to 50
                    end if
                    sa( i, j ) = a( i, j )
                 end do
              end do
           else
              do j = 1, n
                 do i = j, n
                    if( ( a( i, j )<-rmax ) .or. ( a( i, j )>rmax ) )then
                       info = 1_ilp64
                       go to 50
                    end if
                    sa( i, j ) = a( i, j )
                 end do
              end do
           end if
           50 continue
           return
     end subroutine stdlib_I64_dlat2s




     pure module subroutine stdlib_I64_slag2d( m, n, sa, ldsa, a, lda, info )
     !! SLAG2D converts a SINGLE PRECISION matrix, SA, to a DOUBLE
     !! PRECISION matrix, A.
     !! Note that while it is possible to overflow while converting
     !! from double to single, it is not possible to overflow when
     !! converting from single to double.
     !! This is an auxiliary routine so there is no argument checking.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldsa, m, n
           ! Array Arguments 
           real(sp), intent(in) :: sa(ldsa,*)
           real(dp), intent(out) :: a(lda,*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp64) :: i, j
           ! Executable Statements 
           info = 0_ilp64
           do j = 1, n
              do i = 1, m
                 a( i, j ) = sa( i, j )
              end do
           end do
           return
     end subroutine stdlib_I64_slag2d


end submodule stdlib_lapack_blas_like_base
