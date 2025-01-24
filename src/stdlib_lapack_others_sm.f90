submodule(stdlib_lapack_others) stdlib_lapack_others_sm
  implicit none


  contains

     real(sp) module function stdlib_sla_syrpvgrw( uplo, n, info, a, lda, af, ldaf, ipiv,work )
     !! SLA_SYRPVGRW computes the reciprocal pivot growth factor
     !! norm(A)/norm(U). The "max absolute element" norm is used. If this is
     !! much less than 1, the stability of the LU factorization of the
     !! (equilibrated) matrix A could be poor. This also means that the
     !! solution X, estimated condition numbers, and error bounds could be
     !! unreliable.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: n, info, lda, ldaf
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           real(sp), intent(in) :: a(lda,*), af(ldaf,*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: ncols, i, j, k, kp
           real(sp) :: amax, umax, rpvgrw, tmp
           logical(lk) :: upper
           ! Intrinsic Functions 
           ! Executable Statements 
           upper = stdlib_lsame( 'UPPER', uplo )
           if ( info==0_ilp ) then
              if ( upper ) then
                 ncols = 1_ilp
              else
                 ncols = n
              end if
           else
              ncols = info
           end if
           rpvgrw = one
           do i = 1, 2*n
              work( i ) = zero
           end do
           ! find the max magnitude entry of each column of a.  compute the max
           ! for all n columns so we can apply the pivot permutation while
           ! looping below.  assume a full factorization is the common case.
           if ( upper ) then
              do j = 1, n
                 do i = 1, j
                    work( n+i ) = max( abs( a( i, j ) ), work( n+i ) )
                    work( n+j ) = max( abs( a( i, j ) ), work( n+j ) )
                 end do
              end do
           else
              do j = 1, n
                 do i = j, n
                    work( n+i ) = max( abs( a( i, j ) ), work( n+i ) )
                    work( n+j ) = max( abs( a( i, j ) ), work( n+j ) )
                 end do
              end do
           end if
           ! now find the max magnitude entry of each column of u or l.  also
           ! permute the magnitudes of a above so they're in the same order as
           ! the factor.
           ! the iteration orders and permutations were copied from stdlib_ssytrs.
           ! calls to stdlib_sswap would be severe overkill.
           if ( upper ) then
              k = n
              do while ( k < ncols .and. k>0 )
                 if ( ipiv( k )>0_ilp ) then
                    ! 1x1 pivot
                    kp = ipiv( k )
                    if ( kp /= k ) then
                       tmp = work( n+k )
                       work( n+k ) = work( n+kp )
                       work( n+kp ) = tmp
                    end if
                    do i = 1, k
                       work( k ) = max( abs( af( i, k ) ), work( k ) )
                    end do
                    k = k - 1_ilp
                 else
                    ! 2x2 pivot
                    kp = -ipiv( k )
                    tmp = work( n+k-1 )
                    work( n+k-1 ) = work( n+kp )
                    work( n+kp ) = tmp
                    do i = 1, k-1
                       work( k ) = max( abs( af( i, k ) ), work( k ) )
                       work( k-1 ) = max( abs( af( i, k-1 ) ), work( k-1 ) )
                    end do
                    work( k ) = max( abs( af( k, k ) ), work( k ) )
                    k = k - 2_ilp
                 end if
              end do
              k = ncols
              do while ( k <= n )
                 if ( ipiv( k )>0_ilp ) then
                    kp = ipiv( k )
                    if ( kp /= k ) then
                       tmp = work( n+k )
                       work( n+k ) = work( n+kp )
                       work( n+kp ) = tmp
                    end if
                    k = k + 1_ilp
                 else
                    kp = -ipiv( k )
                    tmp = work( n+k )
                    work( n+k ) = work( n+kp )
                    work( n+kp ) = tmp
                    k = k + 2_ilp
                 end if
              end do
           else
              k = 1_ilp
              do while ( k <= ncols )
                 if ( ipiv( k )>0_ilp ) then
                    ! 1x1 pivot
                    kp = ipiv( k )
                    if ( kp /= k ) then
                       tmp = work( n+k )
                       work( n+k ) = work( n+kp )
                       work( n+kp ) = tmp
                    end if
                    do i = k, n
                       work( k ) = max( abs( af( i, k ) ), work( k ) )
                    end do
                    k = k + 1_ilp
                 else
                    ! 2x2 pivot
                    kp = -ipiv( k )
                    tmp = work( n+k+1 )
                    work( n+k+1 ) = work( n+kp )
                    work( n+kp ) = tmp
                    do i = k+1, n
                       work( k ) = max( abs( af( i, k ) ), work( k ) )
                       work( k+1 ) = max( abs( af(i, k+1 ) ), work( k+1 ) )
                    end do
                    work( k ) = max( abs( af( k, k ) ), work( k ) )
                    k = k + 2_ilp
                 end if
              end do
              k = ncols
              do while ( k >= 1 )
                 if ( ipiv( k )>0_ilp ) then
                    kp = ipiv( k )
                    if ( kp /= k ) then
                       tmp = work( n+k )
                       work( n+k ) = work( n+kp )
                       work( n+kp ) = tmp
                    end if
                    k = k - 1_ilp
                 else
                    kp = -ipiv( k )
                    tmp = work( n+k )
                    work( n+k ) = work( n+kp )
                    work( n+kp ) = tmp
                    k = k - 2_ilp
                 endif
              end do
           end if
           ! compute the *inverse* of the max element growth factor.  dividing
           ! by zero would imply the largest entry of the factor's column is
           ! zero.  than can happen when either the column of a is zero or
           ! massive pivots made the factor underflow to zero.  neither counts
           ! as growth in itself, so simply ignore terms with zero
           ! denominators.
           if ( upper ) then
              do i = ncols, n
                 umax = work( i )
                 amax = work( n+i )
                 if ( umax /= 0.0_sp ) then
                    rpvgrw = min( amax / umax, rpvgrw )
                 end if
              end do
           else
              do i = 1, ncols
                 umax = work( i )
                 amax = work( n+i )
                 if ( umax /= 0.0_sp ) then
                    rpvgrw = min( amax / umax, rpvgrw )
                 end if
              end do
           end if
           stdlib_sla_syrpvgrw = rpvgrw
     end function stdlib_sla_syrpvgrw

     real(dp) module function stdlib_dla_syrpvgrw( uplo, n, info, a, lda, af,ldaf, ipiv, work )
     !! DLA_SYRPVGRW computes the reciprocal pivot growth factor
     !! norm(A)/norm(U). The "max absolute element" norm is used. If this is
     !! much less than 1, the stability of the LU factorization of the
     !! (equilibrated) matrix A could be poor. This also means that the
     !! solution X, estimated condition numbers, and error bounds could be
     !! unreliable.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: n, info, lda, ldaf
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           real(dp), intent(in) :: a(lda,*), af(ldaf,*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: ncols, i, j, k, kp
           real(dp) :: amax, umax, rpvgrw, tmp
           logical(lk) :: upper
           ! Intrinsic Functions 
           ! Executable Statements 
           upper = stdlib_lsame( 'UPPER', uplo )
           if ( info==0_ilp ) then
              if ( upper ) then
                 ncols = 1_ilp
              else
                 ncols = n
              end if
           else
              ncols = info
           end if
           rpvgrw = one
           do i = 1, 2*n
              work( i ) = zero
           end do
           ! find the max magnitude entry of each column of a.  compute the max
           ! for all n columns so we can apply the pivot permutation while
           ! looping below.  assume a full factorization is the common case.
           if ( upper ) then
              do j = 1, n
                 do i = 1, j
                    work( n+i ) = max( abs( a( i, j ) ), work( n+i ) )
                    work( n+j ) = max( abs( a( i, j ) ), work( n+j ) )
                 end do
              end do
           else
              do j = 1, n
                 do i = j, n
                    work( n+i ) = max( abs( a( i, j ) ), work( n+i ) )
                    work( n+j ) = max( abs( a( i, j ) ), work( n+j ) )
                 end do
              end do
           end if
           ! now find the max magnitude entry of each column of u or l.  also
           ! permute the magnitudes of a above so they're in the same order as
           ! the factor.
           ! the iteration orders and permutations were copied from stdlib_dsytrs.
           ! calls to stdlib_sswap would be severe overkill.
           if ( upper ) then
              k = n
              do while ( k < ncols .and. k>0 )
                 if ( ipiv( k )>0_ilp ) then
                    ! 1x1 pivot
                    kp = ipiv( k )
                    if ( kp /= k ) then
                       tmp = work( n+k )
                       work( n+k ) = work( n+kp )
                       work( n+kp ) = tmp
                    end if
                    do i = 1, k
                       work( k ) = max( abs( af( i, k ) ), work( k ) )
                    end do
                    k = k - 1_ilp
                 else
                    ! 2x2 pivot
                    kp = -ipiv( k )
                    tmp = work( n+k-1 )
                    work( n+k-1 ) = work( n+kp )
                    work( n+kp ) = tmp
                    do i = 1, k-1
                       work( k ) = max( abs( af( i, k ) ), work( k ) )
                       work( k-1 ) = max( abs( af( i, k-1 ) ), work( k-1 ) )
                    end do
                    work( k ) = max( abs( af( k, k ) ), work( k ) )
                    k = k - 2_ilp
                 end if
              end do
              k = ncols
              do while ( k <= n )
                 if ( ipiv( k )>0_ilp ) then
                    kp = ipiv( k )
                    if ( kp /= k ) then
                       tmp = work( n+k )
                       work( n+k ) = work( n+kp )
                       work( n+kp ) = tmp
                    end if
                    k = k + 1_ilp
                 else
                    kp = -ipiv( k )
                    tmp = work( n+k )
                    work( n+k ) = work( n+kp )
                    work( n+kp ) = tmp
                    k = k + 2_ilp
                 end if
              end do
           else
              k = 1_ilp
              do while ( k <= ncols )
                 if ( ipiv( k )>0_ilp ) then
                    ! 1x1 pivot
                    kp = ipiv( k )
                    if ( kp /= k ) then
                       tmp = work( n+k )
                       work( n+k ) = work( n+kp )
                       work( n+kp ) = tmp
                    end if
                    do i = k, n
                       work( k ) = max( abs( af( i, k ) ), work( k ) )
                    end do
                    k = k + 1_ilp
                 else
                    ! 2x2 pivot
                    kp = -ipiv( k )
                    tmp = work( n+k+1 )
                    work( n+k+1 ) = work( n+kp )
                    work( n+kp ) = tmp
                    do i = k+1, n
                       work( k ) = max( abs( af( i, k ) ), work( k ) )
                       work( k+1 ) = max( abs( af(i, k+1 ) ), work( k+1 ) )
                    end do
                    work( k ) = max( abs( af( k, k ) ), work( k ) )
                    k = k + 2_ilp
                 end if
              end do
              k = ncols
              do while ( k >= 1 )
                 if ( ipiv( k )>0_ilp ) then
                    kp = ipiv( k )
                    if ( kp /= k ) then
                       tmp = work( n+k )
                       work( n+k ) = work( n+kp )
                       work( n+kp ) = tmp
                    end if
                    k = k - 1_ilp
                 else
                    kp = -ipiv( k )
                    tmp = work( n+k )
                    work( n+k ) = work( n+kp )
                    work( n+kp ) = tmp
                    k = k - 2_ilp
                 endif
              end do
           end if
           ! compute the *inverse* of the max element growth factor.  dividing
           ! by zero would imply the largest entry of the factor's column is
           ! zero.  than can happen when either the column of a is zero or
           ! massive pivots made the factor underflow to zero.  neither counts
           ! as growth in itself, so simply ignore terms with zero
           ! denominators.
           if ( upper ) then
              do i = ncols, n
                 umax = work( i )
                 amax = work( n+i )
                 if ( umax /= zero ) then
                    rpvgrw = min( amax / umax, rpvgrw )
                 end if
              end do
           else
              do i = 1, ncols
                 umax = work( i )
                 amax = work( n+i )
                 if ( umax /= zero ) then
                    rpvgrw = min( amax / umax, rpvgrw )
                 end if
              end do
           end if
           stdlib_dla_syrpvgrw = rpvgrw
     end function stdlib_dla_syrpvgrw


     real(sp) module function stdlib_cla_syrpvgrw( uplo, n, info, a, lda, af, ldaf, ipiv,work )
     !! CLA_SYRPVGRW computes the reciprocal pivot growth factor
     !! norm(A)/norm(U). The "max absolute element" norm is used. If this is
     !! much less than 1, the stability of the LU factorization of the
     !! (equilibrated) matrix A could be poor. This also means that the
     !! solution X, estimated condition numbers, and error bounds could be
     !! unreliable.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: n, info, lda, ldaf
           ! Array Arguments 
           complex(sp), intent(in) :: a(lda,*), af(ldaf,*)
           real(sp), intent(out) :: work(*)
           integer(ilp), intent(in) :: ipiv(*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: ncols, i, j, k, kp
           real(sp) :: amax, umax, rpvgrw, tmp
           logical(lk) :: upper
           complex(sp) :: zdum
           ! Intrinsic Functions 
           ! Statement Functions 
           real(sp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=sp) ) + abs( aimag ( zdum ) )
           ! Executable Statements 
           upper = stdlib_lsame( 'UPPER', uplo )
           if ( info==0_ilp ) then
              if ( upper ) then
                 ncols = 1_ilp
              else
                 ncols = n
              end if
           else
              ncols = info
           end if
           rpvgrw = one
           do i = 1, 2*n
              work( i ) = zero
           end do
           ! find the max magnitude entry of each column of a.  compute the max
           ! for all n columns so we can apply the pivot permutation while
           ! looping below.  assume a full factorization is the common case.
           if ( upper ) then
              do j = 1, n
                 do i = 1, j
                    work( n+i ) = max( cabs1( a( i, j ) ), work( n+i ) )
                    work( n+j ) = max( cabs1( a( i, j ) ), work( n+j ) )
                 end do
              end do
           else
              do j = 1, n
                 do i = j, n
                    work( n+i ) = max( cabs1( a( i, j ) ), work( n+i ) )
                    work( n+j ) = max( cabs1( a( i, j ) ), work( n+j ) )
                 end do
              end do
           end if
           ! now find the max magnitude entry of each column of u or l.  also
           ! permute the magnitudes of a above so they're in the same order as
           ! the factor.
           ! the iteration orders and permutations were copied from stdlib_csytrs.
           ! calls to stdlib_sswap would be severe overkill.
           if ( upper ) then
              k = n
              do while ( k < ncols .and. k>0 )
                 if ( ipiv( k )>0_ilp ) then
                    ! 1x1 pivot
                    kp = ipiv( k )
                    if ( kp /= k ) then
                       tmp = work( n+k )
                       work( n+k ) = work( n+kp )
                       work( n+kp ) = tmp
                    end if
                    do i = 1, k
                       work( k ) = max( cabs1( af( i, k ) ), work( k ) )
                    end do
                    k = k - 1_ilp
                 else
                    ! 2x2 pivot
                    kp = -ipiv( k )
                    tmp = work( n+k-1 )
                    work( n+k-1 ) = work( n+kp )
                    work( n+kp ) = tmp
                    do i = 1, k-1
                       work( k ) = max( cabs1( af( i, k ) ), work( k ) )
                       work( k-1 ) =max( cabs1( af( i, k-1 ) ), work( k-1 ) )
                    end do
                    work( k ) = max( cabs1( af( k, k ) ), work( k ) )
                    k = k - 2_ilp
                 end if
              end do
              k = ncols
              do while ( k <= n )
                 if ( ipiv( k )>0_ilp ) then
                    kp = ipiv( k )
                    if ( kp /= k ) then
                       tmp = work( n+k )
                       work( n+k ) = work( n+kp )
                       work( n+kp ) = tmp
                    end if
                    k = k + 1_ilp
                 else
                    kp = -ipiv( k )
                    tmp = work( n+k )
                    work( n+k ) = work( n+kp )
                    work( n+kp ) = tmp
                    k = k + 2_ilp
                 end if
              end do
           else
              k = 1_ilp
              do while ( k <= ncols )
                 if ( ipiv( k )>0_ilp ) then
                    ! 1x1 pivot
                    kp = ipiv( k )
                    if ( kp /= k ) then
                       tmp = work( n+k )
                       work( n+k ) = work( n+kp )
                       work( n+kp ) = tmp
                    end if
                    do i = k, n
                       work( k ) = max( cabs1( af( i, k ) ), work( k ) )
                    end do
                    k = k + 1_ilp
                 else
                    ! 2x2 pivot
                    kp = -ipiv( k )
                    tmp = work( n+k+1 )
                    work( n+k+1 ) = work( n+kp )
                    work( n+kp ) = tmp
                    do i = k+1, n
                       work( k ) = max( cabs1( af( i, k ) ), work( k ) )
                       work( k+1 ) =max( cabs1( af( i, k+1 ) ), work( k+1 ) )
                    end do
                    work( k ) = max( cabs1( af( k, k ) ), work( k ) )
                    k = k + 2_ilp
                 end if
              end do
              k = ncols
              do while ( k >= 1 )
                 if ( ipiv( k )>0_ilp ) then
                    kp = ipiv( k )
                    if ( kp /= k ) then
                       tmp = work( n+k )
                       work( n+k ) = work( n+kp )
                       work( n+kp ) = tmp
                    end if
                    k = k - 1_ilp
                 else
                    kp = -ipiv( k )
                    tmp = work( n+k )
                    work( n+k ) = work( n+kp )
                    work( n+kp ) = tmp
                    k = k - 2_ilp
                 endif
              end do
           end if
           ! compute the *inverse* of the max element growth factor.  dividing
           ! by zero would imply the largest entry of the factor's column is
           ! zero.  than can happen when either the column of a is zero or
           ! massive pivots made the factor underflow to zero.  neither counts
           ! as growth in itself, so simply ignore terms with zero
           ! denominators.
           if ( upper ) then
              do i = ncols, n
                 umax = work( i )
                 amax = work( n+i )
                 if ( umax /= 0.0_sp ) then
                    rpvgrw = min( amax / umax, rpvgrw )
                 end if
              end do
           else
              do i = 1, ncols
                 umax = work( i )
                 amax = work( n+i )
                 if ( umax /= 0.0_sp ) then
                    rpvgrw = min( amax / umax, rpvgrw )
                 end if
              end do
           end if
           stdlib_cla_syrpvgrw = rpvgrw
     end function stdlib_cla_syrpvgrw

     real(dp) module function stdlib_zla_syrpvgrw( uplo, n, info, a, lda, af,ldaf, ipiv, work )
     !! ZLA_SYRPVGRW computes the reciprocal pivot growth factor
     !! norm(A)/norm(U). The "max absolute element" norm is used. If this is
     !! much less than 1, the stability of the LU factorization of the
     !! (equilibrated) matrix A could be poor. This also means that the
     !! solution X, estimated condition numbers, and error bounds could be
     !! unreliable.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: n, info, lda, ldaf
           ! Array Arguments 
           complex(dp), intent(in) :: a(lda,*), af(ldaf,*)
           real(dp), intent(out) :: work(*)
           integer(ilp), intent(in) :: ipiv(*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: ncols, i, j, k, kp
           real(dp) :: amax, umax, rpvgrw, tmp
           logical(lk) :: upper
           complex(dp) :: zdum
           ! Intrinsic Functions 
           ! Statement Functions 
           real(dp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=dp) ) + abs( aimag ( zdum ) )
           ! Executable Statements 
           upper = stdlib_lsame( 'UPPER', uplo )
           if ( info==0_ilp ) then
              if ( upper ) then
                 ncols = 1_ilp
              else
                 ncols = n
              end if
           else
              ncols = info
           end if
           rpvgrw = one
           do i = 1, 2*n
              work( i ) = zero
           end do
           ! find the max magnitude entry of each column of a.  compute the max
           ! for all n columns so we can apply the pivot permutation while
           ! looping below.  assume a full factorization is the common case.
           if ( upper ) then
              do j = 1, n
                 do i = 1, j
                    work( n+i ) = max( cabs1( a( i, j ) ), work( n+i ) )
                    work( n+j ) = max( cabs1( a( i, j ) ), work( n+j ) )
                 end do
              end do
           else
              do j = 1, n
                 do i = j, n
                    work( n+i ) = max( cabs1( a( i, j ) ), work( n+i ) )
                    work( n+j ) = max( cabs1( a( i, j ) ), work( n+j ) )
                 end do
              end do
           end if
           ! now find the max magnitude entry of each column of u or l.  also
           ! permute the magnitudes of a above so they're in the same order as
           ! the factor.
           ! the iteration orders and permutations were copied from stdlib_zsytrs.
           ! calls to stdlib_sswap would be severe overkill.
           if ( upper ) then
              k = n
              do while ( k < ncols .and. k>0 )
                 if ( ipiv( k )>0_ilp ) then
                    ! 1x1 pivot
                    kp = ipiv( k )
                    if ( kp /= k ) then
                       tmp = work( n+k )
                       work( n+k ) = work( n+kp )
                       work( n+kp ) = tmp
                    end if
                    do i = 1, k
                       work( k ) = max( cabs1( af( i, k ) ), work( k ) )
                    end do
                    k = k - 1_ilp
                 else
                    ! 2x2 pivot
                    kp = -ipiv( k )
                    tmp = work( n+k-1 )
                    work( n+k-1 ) = work( n+kp )
                    work( n+kp ) = tmp
                    do i = 1, k-1
                       work( k ) = max( cabs1( af( i, k ) ), work( k ) )
                       work( k-1 ) =max( cabs1( af( i, k-1 ) ), work( k-1 ) )
                    end do
                    work( k ) = max( cabs1( af( k, k ) ), work( k ) )
                    k = k - 2_ilp
                 end if
              end do
              k = ncols
              do while ( k <= n )
                 if ( ipiv( k )>0_ilp ) then
                    kp = ipiv( k )
                    if ( kp /= k ) then
                       tmp = work( n+k )
                       work( n+k ) = work( n+kp )
                       work( n+kp ) = tmp
                    end if
                    k = k + 1_ilp
                 else
                    kp = -ipiv( k )
                    tmp = work( n+k )
                    work( n+k ) = work( n+kp )
                    work( n+kp ) = tmp
                    k = k + 2_ilp
                 end if
              end do
           else
              k = 1_ilp
              do while ( k <= ncols )
                 if ( ipiv( k )>0_ilp ) then
                    ! 1x1 pivot
                    kp = ipiv( k )
                    if ( kp /= k ) then
                       tmp = work( n+k )
                       work( n+k ) = work( n+kp )
                       work( n+kp ) = tmp
                    end if
                    do i = k, n
                       work( k ) = max( cabs1( af( i, k ) ), work( k ) )
                    end do
                    k = k + 1_ilp
                 else
                    ! 2x2 pivot
                    kp = -ipiv( k )
                    tmp = work( n+k+1 )
                    work( n+k+1 ) = work( n+kp )
                    work( n+kp ) = tmp
                    do i = k+1, n
                       work( k ) = max( cabs1( af( i, k ) ), work( k ) )
                       work( k+1 ) =max( cabs1( af( i, k+1 ) ), work( k+1 ) )
                    end do
                    work( k ) = max( cabs1( af( k, k ) ), work( k ) )
                    k = k + 2_ilp
                 end if
              end do
              k = ncols
              do while ( k >= 1 )
                 if ( ipiv( k )>0_ilp ) then
                    kp = ipiv( k )
                    if ( kp /= k ) then
                       tmp = work( n+k )
                       work( n+k ) = work( n+kp )
                       work( n+kp ) = tmp
                    end if
                    k = k - 1_ilp
                 else
                    kp = -ipiv( k )
                    tmp = work( n+k )
                    work( n+k ) = work( n+kp )
                    work( n+kp ) = tmp
                    k = k - 2_ilp
                 endif
              end do
           end if
           ! compute the *inverse* of the max element growth factor.  dividing
           ! by zero would imply the largest entry of the factor's column is
           ! zero.  than can happen when either the column of a is zero or
           ! massive pivots made the factor underflow to zero.  neither counts
           ! as growth in itself, so simply ignore terms with zero
           ! denominators.
           if ( upper ) then
              do i = ncols, n
                 umax = work( i )
                 amax = work( n+i )
                 if ( umax /= zero ) then
                    rpvgrw = min( amax / umax, rpvgrw )
                 end if
              end do
           else
              do i = 1, ncols
                 umax = work( i )
                 amax = work( n+i )
                 if ( umax /= zero ) then
                    rpvgrw = min( amax / umax, rpvgrw )
                 end if
              end do
           end if
           stdlib_zla_syrpvgrw = rpvgrw
     end function stdlib_zla_syrpvgrw




     pure real(sp) module function stdlib_sla_gerpvgrw( n, ncols, a, lda, af, ldaf )
     !! SLA_GERPVGRW computes the reciprocal pivot growth factor
     !! norm(A)/norm(U). The "max absolute element" norm is used. If this is
     !! much less than 1, the stability of the LU factorization of the
     !! (equilibrated) matrix A could be poor. This also means that the
     !! solution X, estimated condition numbers, and error bounds could be
     !! unreliable.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: n, ncols, lda, ldaf
           ! Array Arguments 
           real(sp), intent(in) :: a(lda,*), af(ldaf,*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, j
           real(sp) :: amax, umax, rpvgrw
           ! Intrinsic Functions 
           ! Executable Statements 
           rpvgrw = one
           do j = 1, ncols
              amax = zero
              umax = zero
              do i = 1, n
                 amax = max( abs( a( i, j ) ), amax )
              end do
              do i = 1, j
                 umax = max( abs( af( i, j ) ), umax )
              end do
              if ( umax /= 0.0_sp ) then
                 rpvgrw = min( amax / umax, rpvgrw )
              end if
           end do
           stdlib_sla_gerpvgrw = rpvgrw
     end function stdlib_sla_gerpvgrw

     pure real(dp) module function stdlib_dla_gerpvgrw( n, ncols, a, lda, af,ldaf )
     !! DLA_GERPVGRW computes the reciprocal pivot growth factor
     !! norm(A)/norm(U). The "max absolute element" norm is used. If this is
     !! much less than 1, the stability of the LU factorization of the
     !! (equilibrated) matrix A could be poor. This also means that the
     !! solution X, estimated condition numbers, and error bounds could be
     !! unreliable.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: n, ncols, lda, ldaf
           ! Array Arguments 
           real(dp), intent(in) :: a(lda,*), af(ldaf,*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, j
           real(dp) :: amax, umax, rpvgrw
           ! Intrinsic Functions 
           ! Executable Statements 
           rpvgrw = one
           do j = 1, ncols
              amax = zero
              umax = zero
              do i = 1, n
                 amax = max( abs( a( i, j ) ), amax )
              end do
              do i = 1, j
                 umax = max( abs( af( i, j ) ), umax )
              end do
              if ( umax /= zero ) then
                 rpvgrw = min( amax / umax, rpvgrw )
              end if
           end do
           stdlib_dla_gerpvgrw = rpvgrw
     end function stdlib_dla_gerpvgrw


     pure real(sp) module function stdlib_cla_gerpvgrw( n, ncols, a, lda, af, ldaf )
     !! CLA_GERPVGRW computes the reciprocal pivot growth factor
     !! norm(A)/norm(U). The "max absolute element" norm is used. If this is
     !! much less than 1, the stability of the LU factorization of the
     !! (equilibrated) matrix A could be poor. This also means that the
     !! solution X, estimated condition numbers, and error bounds could be
     !! unreliable.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: n, ncols, lda, ldaf
           ! Array Arguments 
           complex(sp), intent(in) :: a(lda,*), af(ldaf,*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, j
           real(sp) :: amax, umax, rpvgrw
           complex(sp) :: zdum
           ! Intrinsic Functions 
           ! Statement Functions 
           real(sp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=sp) ) + abs( aimag( zdum ) )
           ! Executable Statements 
           rpvgrw = one
           do j = 1, ncols
              amax = zero
              umax = zero
              do i = 1, n
                 amax = max( cabs1( a( i, j ) ), amax )
              end do
              do i = 1, j
                 umax = max( cabs1( af( i, j ) ), umax )
              end do
              if ( umax /= 0.0_sp ) then
                 rpvgrw = min( amax / umax, rpvgrw )
              end if
           end do
           stdlib_cla_gerpvgrw = rpvgrw
     end function stdlib_cla_gerpvgrw

     pure real(dp) module function stdlib_zla_gerpvgrw( n, ncols, a, lda, af,ldaf )
     !! ZLA_GERPVGRW computes the reciprocal pivot growth factor
     !! norm(A)/norm(U). The "max absolute element" norm is used. If this is
     !! much less than 1, the stability of the LU factorization of the
     !! (equilibrated) matrix A could be poor. This also means that the
     !! solution X, estimated condition numbers, and error bounds could be
     !! unreliable.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: n, ncols, lda, ldaf
           ! Array Arguments 
           complex(dp), intent(in) :: a(lda,*), af(ldaf,*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, j
           real(dp) :: amax, umax, rpvgrw
           complex(dp) :: zdum
           ! Intrinsic Functions 
           ! Statement Functions 
           real(dp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=dp) ) + abs( aimag( zdum ) )
           ! Executable Statements 
           rpvgrw = one
           do j = 1, ncols
              amax = zero
              umax = zero
              do i = 1, n
                 amax = max( cabs1( a( i, j ) ), amax )
              end do
              do i = 1, j
                 umax = max( cabs1( af( i, j ) ), umax )
              end do
              if ( umax /= zero ) then
                 rpvgrw = min( amax / umax, rpvgrw )
              end if
           end do
           stdlib_zla_gerpvgrw = rpvgrw
     end function stdlib_zla_gerpvgrw




     real(sp) module function stdlib_cla_gbrcond_c( trans, n, kl, ku, ab, ldab, afb,ldafb, ipiv, c, &
     !! CLA_GBRCOND_C Computes the infinity norm condition number of
     !! op(A) * inv(diag(C)) where C is a REAL vector.
               capply, info, work,rwork )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: trans
           logical(lk), intent(in) :: capply
           integer(ilp), intent(in) :: n, kl, ku, ldab, ldafb
           integer(ilp) :: kd, ke
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           complex(sp), intent(in) :: ab(ldab,*), afb(ldafb,*)
           complex(sp), intent(out) :: work(*)
           real(sp), intent(in) :: c(*)
           real(sp), intent(out) :: rwork(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: notrans
           integer(ilp) :: kase, i, j
           real(sp) :: ainvnm, anorm, tmp
           complex(sp) :: zdum
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Statement Functions 
           real(sp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=sp) ) + abs( aimag( zdum ) )
           ! Executable Statements 
           stdlib_cla_gbrcond_c = zero
           info = 0_ilp
           notrans = stdlib_lsame( trans, 'N' )
           if ( .not. notrans .and. .not. stdlib_lsame( trans, 'T' ) .and. .not.stdlib_lsame( &
                     trans, 'C' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( kl<0_ilp .or. kl>n-1 ) then
              info = -3_ilp
           else if( ku<0_ilp .or. ku>n-1 ) then
              info = -4_ilp
           else if( ldab<kl+ku+1 ) then
              info = -6_ilp
           else if( ldafb<2_ilp*kl+ku+1 ) then
              info = -8_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CLA_GBRCOND_C', -info )
              return
           end if
           ! compute norm of op(a)*op2(c).
           anorm = zero
           kd = ku + 1_ilp
           ke = kl + 1_ilp
           if ( notrans ) then
              do i = 1, n
                 tmp = zero
                 if ( capply ) then
                    do j = max( i-kl, 1 ), min( i+ku, n )
                       tmp = tmp + cabs1( ab( kd+i-j, j ) ) / c( j )
                    end do
                 else
                    do j = max( i-kl, 1 ), min( i+ku, n )
                       tmp = tmp + cabs1( ab( kd+i-j, j ) )
                    end do
                 end if
                 rwork( i ) = tmp
                 anorm = max( anorm, tmp )
              end do
           else
              do i = 1, n
                 tmp = zero
                 if ( capply ) then
                    do j = max( i-kl, 1 ), min( i+ku, n )
                       tmp = tmp + cabs1( ab( ke-i+j, i ) ) / c( j )
                    end do
                 else
                    do j = max( i-kl, 1 ), min( i+ku, n )
                       tmp = tmp + cabs1( ab( ke-i+j, i ) )
                    end do
                 end if
                 rwork( i ) = tmp
                 anorm = max( anorm, tmp )
              end do
           end if
           ! quick return if possible.
           if( n==0_ilp ) then
              stdlib_cla_gbrcond_c = one
              return
           else if( anorm == zero ) then
              return
           end if
           ! estimate the norm of inv(op(a)).
           ainvnm = zero
           kase = 0_ilp
           10 continue
           call stdlib_clacn2( n, work( n+1 ), work, ainvnm, kase, isave )
           if( kase/=0_ilp ) then
              if( kase==2_ilp ) then
                 ! multiply by r.
                 do i = 1, n
                    work( i ) = work( i ) * rwork( i )
                 end do
                 if ( notrans ) then
                    call stdlib_cgbtrs( 'NO TRANSPOSE', n, kl, ku, 1_ilp, afb, ldafb,ipiv, work, n, &
                              info )
                 else
                    call stdlib_cgbtrs( 'CONJUGATE TRANSPOSE', n, kl, ku, 1_ilp, afb,ldafb, ipiv, &
                              work, n, info )
                 endif
                 ! multiply by inv(c).
                 if ( capply ) then
                    do i = 1, n
                       work( i ) = work( i ) * c( i )
                    end do
                 end if
              else
                 ! multiply by inv(c**h).
                 if ( capply ) then
                    do i = 1, n
                       work( i ) = work( i ) * c( i )
                    end do
                 end if
                 if ( notrans ) then
                    call stdlib_cgbtrs( 'CONJUGATE TRANSPOSE', n, kl, ku, 1_ilp, afb,ldafb, ipiv,  &
                              work, n, info )
                 else
                    call stdlib_cgbtrs( 'NO TRANSPOSE', n, kl, ku, 1_ilp, afb, ldafb,ipiv, work, n, &
                              info )
                 end if
                 ! multiply by r.
                 do i = 1, n
                    work( i ) = work( i ) * rwork( i )
                 end do
              end if
              go to 10
           end if
           ! compute the estimate of the reciprocal condition number.
           if( ainvnm /= zero )stdlib_cla_gbrcond_c = one / ainvnm
           return
     end function stdlib_cla_gbrcond_c

     real(dp) module function stdlib_zla_gbrcond_c( trans, n, kl, ku, ab,ldab, afb, ldafb, ipiv,c, &
     !! ZLA_GBRCOND_C Computes the infinity norm condition number of
     !! op(A) * inv(diag(C)) where C is a DOUBLE PRECISION vector.
               capply, info, work,rwork )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: trans
           logical(lk), intent(in) :: capply
           integer(ilp), intent(in) :: n, kl, ku, ldab, ldafb
           integer(ilp) :: kd, ke
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           complex(dp), intent(in) :: ab(ldab,*), afb(ldafb,*)
           complex(dp), intent(out) :: work(*)
           real(dp), intent(in) :: c(*)
           real(dp), intent(out) :: rwork(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: notrans
           integer(ilp) :: kase, i, j
           real(dp) :: ainvnm, anorm, tmp
           complex(dp) :: zdum
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Statement Functions 
           real(dp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=dp) ) + abs( aimag( zdum ) )
           ! Executable Statements 
           stdlib_zla_gbrcond_c = zero
           info = 0_ilp
           notrans = stdlib_lsame( trans, 'N' )
           if ( .not. notrans .and. .not. stdlib_lsame( trans, 'T' ) .and. .not.stdlib_lsame( &
                     trans, 'C' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( kl<0_ilp .or. kl>n-1 ) then
              info = -3_ilp
           else if( ku<0_ilp .or. ku>n-1 ) then
              info = -4_ilp
           else if( ldab<kl+ku+1 ) then
              info = -6_ilp
           else if( ldafb<2_ilp*kl+ku+1 ) then
              info = -8_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZLA_GBRCOND_C', -info )
              return
           end if
           ! compute norm of op(a)*op2(c).
           anorm = zero
           kd = ku + 1_ilp
           ke = kl + 1_ilp
           if ( notrans ) then
              do i = 1, n
                 tmp = zero
                 if ( capply ) then
                    do j = max( i-kl, 1 ), min( i+ku, n )
                       tmp = tmp + cabs1( ab( kd+i-j, j ) ) / c( j )
                    end do
                 else
                    do j = max( i-kl, 1 ), min( i+ku, n )
                       tmp = tmp + cabs1( ab( kd+i-j, j ) )
                    end do
                 end if
                 rwork( i ) = tmp
                 anorm = max( anorm, tmp )
              end do
           else
              do i = 1, n
                 tmp = zero
                 if ( capply ) then
                    do j = max( i-kl, 1 ), min( i+ku, n )
                       tmp = tmp + cabs1( ab( ke-i+j, i ) ) / c( j )
                    end do
                 else
                    do j = max( i-kl, 1 ), min( i+ku, n )
                       tmp = tmp + cabs1( ab( ke-i+j, i ) )
                    end do
                 end if
                 rwork( i ) = tmp
                 anorm = max( anorm, tmp )
              end do
           end if
           ! quick return if possible.
           if( n==0_ilp ) then
              stdlib_zla_gbrcond_c = one
              return
           else if( anorm == zero ) then
              return
           end if
           ! estimate the norm of inv(op(a)).
           ainvnm = zero
           kase = 0_ilp
           10 continue
           call stdlib_zlacn2( n, work( n+1 ), work, ainvnm, kase, isave )
           if( kase/=0_ilp ) then
              if( kase==2_ilp ) then
                 ! multiply by r.
                 do i = 1, n
                    work( i ) = work( i ) * rwork( i )
                 end do
                 if ( notrans ) then
                    call stdlib_zgbtrs( 'NO TRANSPOSE', n, kl, ku, 1_ilp, afb, ldafb,ipiv, work, n, &
                              info )
                 else
                    call stdlib_zgbtrs( 'CONJUGATE TRANSPOSE', n, kl, ku, 1_ilp, afb,ldafb, ipiv, &
                              work, n, info )
                 endif
                 ! multiply by inv(c).
                 if ( capply ) then
                    do i = 1, n
                       work( i ) = work( i ) * c( i )
                    end do
                 end if
              else
                 ! multiply by inv(c**h).
                 if ( capply ) then
                    do i = 1, n
                       work( i ) = work( i ) * c( i )
                    end do
                 end if
                 if ( notrans ) then
                    call stdlib_zgbtrs( 'CONJUGATE TRANSPOSE', n, kl, ku, 1_ilp, afb,ldafb, ipiv,  &
                              work, n, info )
                 else
                    call stdlib_zgbtrs( 'NO TRANSPOSE', n, kl, ku, 1_ilp, afb, ldafb,ipiv, work, n, &
                              info )
                 end if
                 ! multiply by r.
                 do i = 1, n
                    work( i ) = work( i ) * rwork( i )
                 end do
              end if
              go to 10
           end if
           ! compute the estimate of the reciprocal condition number.
           if( ainvnm /= zero )stdlib_zla_gbrcond_c = one / ainvnm
           return
     end function stdlib_zla_gbrcond_c




     real(sp) module function stdlib_cla_gercond_c( trans, n, a, lda, af, ldaf, ipiv, c,capply, info, &
     !! CLA_GERCOND_C computes the infinity norm condition number of
     !! op(A) * inv(diag(C)) where C is a REAL vector.
               work, rwork )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: trans
           logical(lk), intent(in) :: capply
           integer(ilp), intent(in) :: n, lda, ldaf
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           complex(sp), intent(in) :: a(lda,*), af(ldaf,*)
           complex(sp), intent(out) :: work(*)
           real(sp), intent(in) :: c(*)
           real(sp), intent(out) :: rwork(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: notrans
           integer(ilp) :: kase, i, j
           real(sp) :: ainvnm, anorm, tmp
           complex(sp) :: zdum
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Statement Functions 
           real(sp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=sp) ) + abs( aimag( zdum ) )
           ! Executable Statements 
           stdlib_cla_gercond_c = zero
           info = 0_ilp
           notrans = stdlib_lsame( trans, 'N' )
           if ( .not. notrans .and. .not. stdlib_lsame( trans, 'T' ) .and. .not.stdlib_lsame( &
                     trans, 'C' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           else if( ldaf<max( 1_ilp, n ) ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CLA_GERCOND_C', -info )
              return
           end if
           ! compute norm of op(a)*op2(c).
           anorm = zero
           if ( notrans ) then
              do i = 1, n
                 tmp = zero
                 if ( capply ) then
                    do j = 1, n
                       tmp = tmp + cabs1( a( i, j ) ) / c( j )
                    end do
                 else
                    do j = 1, n
                       tmp = tmp + cabs1( a( i, j ) )
                    end do
                 end if
                 rwork( i ) = tmp
                 anorm = max( anorm, tmp )
              end do
           else
              do i = 1, n
                 tmp = zero
                 if ( capply ) then
                    do j = 1, n
                       tmp = tmp + cabs1( a( j, i ) ) / c( j )
                    end do
                 else
                    do j = 1, n
                       tmp = tmp + cabs1( a( j, i ) )
                    end do
                 end if
                 rwork( i ) = tmp
                 anorm = max( anorm, tmp )
              end do
           end if
           ! quick return if possible.
           if( n==0_ilp ) then
              stdlib_cla_gercond_c = one
              return
           else if( anorm == zero ) then
              return
           end if
           ! estimate the norm of inv(op(a)).
           ainvnm = zero
           kase = 0_ilp
           10 continue
           call stdlib_clacn2( n, work( n+1 ), work, ainvnm, kase, isave )
           if( kase/=0_ilp ) then
              if( kase==2_ilp ) then
                 ! multiply by r.
                 do i = 1, n
                    work( i ) = work( i ) * rwork( i )
                 end do
                 if (notrans) then
                    call stdlib_cgetrs( 'NO TRANSPOSE', n, 1_ilp, af, ldaf, ipiv,work, n, info )
                              
                 else
                    call stdlib_cgetrs( 'CONJUGATE TRANSPOSE', n, 1_ilp, af, ldaf, ipiv,work, n, info &
                              )
                 endif
                 ! multiply by inv(c).
                 if ( capply ) then
                    do i = 1, n
                       work( i ) = work( i ) * c( i )
                    end do
                 end if
              else
                 ! multiply by inv(c**h).
                 if ( capply ) then
                    do i = 1, n
                       work( i ) = work( i ) * c( i )
                    end do
                 end if
                 if ( notrans ) then
                    call stdlib_cgetrs( 'CONJUGATE TRANSPOSE', n, 1_ilp, af, ldaf, ipiv,work, n, info &
                              )
                 else
                    call stdlib_cgetrs( 'NO TRANSPOSE', n, 1_ilp, af, ldaf, ipiv,work, n, info )
                              
                 end if
                 ! multiply by r.
                 do i = 1, n
                    work( i ) = work( i ) * rwork( i )
                 end do
              end if
              go to 10
           end if
           ! compute the estimate of the reciprocal condition number.
           if( ainvnm /= zero )stdlib_cla_gercond_c = one / ainvnm
           return
     end function stdlib_cla_gercond_c

     real(dp) module function stdlib_zla_gercond_c( trans, n, a, lda, af,ldaf, ipiv, c, capply,info, &
     !! ZLA_GERCOND_C computes the infinity norm condition number of
     !! op(A) * inv(diag(C)) where C is a DOUBLE PRECISION vector.
               work, rwork )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: trans
           logical(lk), intent(in) :: capply
           integer(ilp), intent(in) :: n, lda, ldaf
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           complex(dp), intent(in) :: a(lda,*), af(ldaf,*)
           complex(dp), intent(out) :: work(*)
           real(dp), intent(in) :: c(*)
           real(dp), intent(out) :: rwork(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: notrans
           integer(ilp) :: kase, i, j
           real(dp) :: ainvnm, anorm, tmp
           complex(dp) :: zdum
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Statement Functions 
           real(dp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=dp) ) + abs( aimag( zdum ) )
           ! Executable Statements 
           stdlib_zla_gercond_c = zero
           info = 0_ilp
           notrans = stdlib_lsame( trans, 'N' )
           if ( .not. notrans .and. .not. stdlib_lsame( trans, 'T' ) .and. .not.stdlib_lsame( &
                     trans, 'C' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           else if( ldaf<max( 1_ilp, n ) ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZLA_GERCOND_C', -info )
              return
           end if
           ! compute norm of op(a)*op2(c).
           anorm = zero
           if ( notrans ) then
              do i = 1, n
                 tmp = zero
                 if ( capply ) then
                    do j = 1, n
                       tmp = tmp + cabs1( a( i, j ) ) / c( j )
                    end do
                 else
                    do j = 1, n
                       tmp = tmp + cabs1( a( i, j ) )
                    end do
                 end if
                 rwork( i ) = tmp
                 anorm = max( anorm, tmp )
              end do
           else
              do i = 1, n
                 tmp = zero
                 if ( capply ) then
                    do j = 1, n
                       tmp = tmp + cabs1( a( j, i ) ) / c( j )
                    end do
                 else
                    do j = 1, n
                       tmp = tmp + cabs1( a( j, i ) )
                    end do
                 end if
                 rwork( i ) = tmp
                 anorm = max( anorm, tmp )
              end do
           end if
           ! quick return if possible.
           if( n==0_ilp ) then
              stdlib_zla_gercond_c = one
              return
           else if( anorm == zero ) then
              return
           end if
           ! estimate the norm of inv(op(a)).
           ainvnm = zero
           kase = 0_ilp
           10 continue
           call stdlib_zlacn2( n, work( n+1 ), work, ainvnm, kase, isave )
           if( kase/=0_ilp ) then
              if( kase==2_ilp ) then
                 ! multiply by r.
                 do i = 1, n
                    work( i ) = work( i ) * rwork( i )
                 end do
                 if (notrans) then
                    call stdlib_zgetrs( 'NO TRANSPOSE', n, 1_ilp, af, ldaf, ipiv,work, n, info )
                              
                 else
                    call stdlib_zgetrs( 'CONJUGATE TRANSPOSE', n, 1_ilp, af, ldaf, ipiv,work, n, info &
                              )
                 endif
                 ! multiply by inv(c).
                 if ( capply ) then
                    do i = 1, n
                       work( i ) = work( i ) * c( i )
                    end do
                 end if
              else
                 ! multiply by inv(c**h).
                 if ( capply ) then
                    do i = 1, n
                       work( i ) = work( i ) * c( i )
                    end do
                 end if
                 if ( notrans ) then
                    call stdlib_zgetrs( 'CONJUGATE TRANSPOSE', n, 1_ilp, af, ldaf, ipiv,work, n, info &
                              )
                 else
                    call stdlib_zgetrs( 'NO TRANSPOSE', n, 1_ilp, af, ldaf, ipiv,work, n, info )
                              
                 end if
                 ! multiply by r.
                 do i = 1, n
                    work( i ) = work( i ) * rwork( i )
                 end do
              end if
              go to 10
           end if
           ! compute the estimate of the reciprocal condition number.
           if( ainvnm /= zero )stdlib_zla_gercond_c = one / ainvnm
           return
     end function stdlib_zla_gercond_c




     real(sp) module function stdlib_cla_hercond_c( uplo, n, a, lda, af, ldaf, ipiv, c,capply, info, &
     !! CLA_HERCOND_C computes the infinity norm condition number of
     !! op(A) * inv(diag(C)) where C is a REAL vector.
               work, rwork )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           logical(lk), intent(in) :: capply
           integer(ilp), intent(in) :: n, lda, ldaf
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           complex(sp), intent(in) :: a(lda,*), af(ldaf,*)
           complex(sp), intent(out) :: work(*)
           real(sp), intent(in) :: c(*)
           real(sp), intent(out) :: rwork(*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: kase, i, j
           real(sp) :: ainvnm, anorm, tmp
           logical(lk) :: up, upper
           complex(sp) :: zdum
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Statement Functions 
           real(sp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=sp) ) + abs( aimag( zdum ) )
           ! Executable Statements 
           stdlib_cla_hercond_c = zero
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           else if( ldaf<max( 1_ilp, n ) ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CLA_HERCOND_C', -info )
              return
           end if
           up = .false.
           if ( stdlib_lsame( uplo, 'U' ) ) up = .true.
           ! compute norm of op(a)*op2(c).
           anorm = zero
           if ( up ) then
              do i = 1, n
                 tmp = zero
                 if ( capply ) then
                    do j = 1, i
                       tmp = tmp + cabs1( a( j, i ) ) / c( j )
                    end do
                    do j = i+1, n
                       tmp = tmp + cabs1( a( i, j ) ) / c( j )
                    end do
                 else
                    do j = 1, i
                       tmp = tmp + cabs1( a( j, i ) )
                    end do
                    do j = i+1, n
                       tmp = tmp + cabs1( a( i, j ) )
                    end do
                 end if
                 rwork( i ) = tmp
                 anorm = max( anorm, tmp )
              end do
           else
              do i = 1, n
                 tmp = zero
                 if ( capply ) then
                    do j = 1, i
                       tmp = tmp + cabs1( a( i, j ) ) / c( j )
                    end do
                    do j = i+1, n
                       tmp = tmp + cabs1( a( j, i ) ) / c( j )
                    end do
                 else
                    do j = 1, i
                       tmp = tmp + cabs1( a( i, j ) )
                    end do
                    do j = i+1, n
                       tmp = tmp + cabs1( a( j, i ) )
                    end do
                 end if
                 rwork( i ) = tmp
                 anorm = max( anorm, tmp )
              end do
           end if
           ! quick return if possible.
           if( n==0_ilp ) then
              stdlib_cla_hercond_c = one
              return
           else if( anorm == zero ) then
              return
           end if
           ! estimate the norm of inv(op(a)).
           ainvnm = zero
           kase = 0_ilp
           10 continue
           call stdlib_clacn2( n, work( n+1 ), work, ainvnm, kase, isave )
           if( kase/=0_ilp ) then
              if( kase==2_ilp ) then
                 ! multiply by r.
                 do i = 1, n
                    work( i ) = work( i ) * rwork( i )
                 end do
                 if ( up ) then
                    call stdlib_chetrs( 'U', n, 1_ilp, af, ldaf, ipiv,work, n, info )
                 else
                    call stdlib_chetrs( 'L', n, 1_ilp, af, ldaf, ipiv,work, n, info )
                 endif
                 ! multiply by inv(c).
                 if ( capply ) then
                    do i = 1, n
                       work( i ) = work( i ) * c( i )
                    end do
                 end if
              else
                 ! multiply by inv(c**h).
                 if ( capply ) then
                    do i = 1, n
                       work( i ) = work( i ) * c( i )
                    end do
                 end if
                 if ( up ) then
                    call stdlib_chetrs( 'U', n, 1_ilp, af, ldaf, ipiv,work, n, info )
                 else
                    call stdlib_chetrs( 'L', n, 1_ilp, af, ldaf, ipiv,work, n, info )
                 end if
                 ! multiply by r.
                 do i = 1, n
                    work( i ) = work( i ) * rwork( i )
                 end do
              end if
              go to 10
           end if
           ! compute the estimate of the reciprocal condition number.
           if( ainvnm /= zero )stdlib_cla_hercond_c = one / ainvnm
           return
     end function stdlib_cla_hercond_c

     real(dp) module function stdlib_zla_hercond_c( uplo, n, a, lda, af,ldaf, ipiv, c, capply,info, work,&
     !! ZLA_HERCOND_C computes the infinity norm condition number of
     !! op(A) * inv(diag(C)) where C is a DOUBLE PRECISION vector.
                rwork )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           logical(lk), intent(in) :: capply
           integer(ilp), intent(in) :: n, lda, ldaf
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           complex(dp), intent(in) :: a(lda,*), af(ldaf,*)
           complex(dp), intent(out) :: work(*)
           real(dp), intent(in) :: c(*)
           real(dp), intent(out) :: rwork(*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: kase, i, j
           real(dp) :: ainvnm, anorm, tmp
           logical(lk) :: up, upper
           complex(dp) :: zdum
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Statement Functions 
           real(dp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=dp) ) + abs( aimag( zdum ) )
           ! Executable Statements 
           stdlib_zla_hercond_c = zero
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           else if( ldaf<max( 1_ilp, n ) ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZLA_HERCOND_C', -info )
              return
           end if
           up = .false.
           if ( stdlib_lsame( uplo, 'U' ) ) up = .true.
           ! compute norm of op(a)*op2(c).
           anorm = zero
           if ( up ) then
              do i = 1, n
                 tmp = zero
                 if ( capply ) then
                    do j = 1, i
                       tmp = tmp + cabs1( a( j, i ) ) / c( j )
                    end do
                    do j = i+1, n
                       tmp = tmp + cabs1( a( i, j ) ) / c( j )
                    end do
                 else
                    do j = 1, i
                       tmp = tmp + cabs1( a( j, i ) )
                    end do
                    do j = i+1, n
                       tmp = tmp + cabs1( a( i, j ) )
                    end do
                 end if
                 rwork( i ) = tmp
                 anorm = max( anorm, tmp )
              end do
           else
              do i = 1, n
                 tmp = zero
                 if ( capply ) then
                    do j = 1, i
                       tmp = tmp + cabs1( a( i, j ) ) / c( j )
                    end do
                    do j = i+1, n
                       tmp = tmp + cabs1( a( j, i ) ) / c( j )
                    end do
                 else
                    do j = 1, i
                       tmp = tmp + cabs1( a( i, j ) )
                    end do
                    do j = i+1, n
                       tmp = tmp + cabs1( a( j, i ) )
                    end do
                 end if
                 rwork( i ) = tmp
                 anorm = max( anorm, tmp )
              end do
           end if
           ! quick return if possible.
           if( n==0_ilp ) then
              stdlib_zla_hercond_c = one
              return
           else if( anorm == zero ) then
              return
           end if
           ! estimate the norm of inv(op(a)).
           ainvnm = zero
           kase = 0_ilp
           10 continue
           call stdlib_zlacn2( n, work( n+1 ), work, ainvnm, kase, isave )
           if( kase/=0_ilp ) then
              if( kase==2_ilp ) then
                 ! multiply by r.
                 do i = 1, n
                    work( i ) = work( i ) * rwork( i )
                 end do
                 if ( up ) then
                    call stdlib_zhetrs( 'U', n, 1_ilp, af, ldaf, ipiv,work, n, info )
                 else
                    call stdlib_zhetrs( 'L', n, 1_ilp, af, ldaf, ipiv,work, n, info )
                 endif
                 ! multiply by inv(c).
                 if ( capply ) then
                    do i = 1, n
                       work( i ) = work( i ) * c( i )
                    end do
                 end if
              else
                 ! multiply by inv(c**h).
                 if ( capply ) then
                    do i = 1, n
                       work( i ) = work( i ) * c( i )
                    end do
                 end if
                 if ( up ) then
                    call stdlib_zhetrs( 'U', n, 1_ilp, af, ldaf, ipiv,work, n, info )
                 else
                    call stdlib_zhetrs( 'L', n, 1_ilp, af, ldaf, ipiv,work, n, info )
                 end if
                 ! multiply by r.
                 do i = 1, n
                    work( i ) = work( i ) * rwork( i )
                 end do
              end if
              go to 10
           end if
           ! compute the estimate of the reciprocal condition number.
           if( ainvnm /= zero )stdlib_zla_hercond_c = one / ainvnm
           return
     end function stdlib_zla_hercond_c




     module subroutine stdlib_sla_syamv( uplo, n, alpha, a, lda, x, incx, beta, y,incy )
     !! SLA_SYAMV performs the matrix-vector operation
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
           real(sp), intent(in) :: a(lda,*), x(*)
           real(sp), intent(inout) :: y(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: symb_zero
           real(sp) :: temp, safe1
           integer(ilp) :: i, info, iy, j, jx, kx, ky
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if     ( uplo/=stdlib_ilauplo( 'U' ) .and.uplo/=stdlib_ilauplo( 'L' ) ) then
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
              call stdlib_xerbla( 'SLA_SYAMV', info )
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
                          temp = abs( a( j, i ) )
                          symb_zero = symb_zero .and.( x( j ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*abs( x( j ) )*temp
                       end do
                       do j = i+1, n
                          temp = abs( a( i, j ) )
                          symb_zero = symb_zero .and.( x( j ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*abs( x( j ) )*temp
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
                    if ( alpha /= zero ) then
                       do j = 1, i
                          temp = abs( a( i, j ) )
                          symb_zero = symb_zero .and.( x( j ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*abs( x( j ) )*temp
                       end do
                       do j = i+1, n
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
                          temp = abs( a( j, i ) )
                          symb_zero = symb_zero .and.( x( j ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*abs( x( jx ) )*temp
                          jx = jx + incx
                       end do
                       do j = i+1, n
                          temp = abs( a( i, j ) )
                          symb_zero = symb_zero .and.( x( j ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*abs( x( jx ) )*temp
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
                          temp = abs( a( i, j ) )
                          symb_zero = symb_zero .and.( x( j ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*abs( x( jx ) )*temp
                          jx = jx + incx
                       end do
                       do j = i+1, n
                          temp = abs( a( j, i ) )
                          symb_zero = symb_zero .and.( x( j ) == zero .or. temp == zero )
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
     end subroutine stdlib_sla_syamv

     module subroutine stdlib_dla_syamv( uplo, n, alpha, a, lda, x, incx, beta, y,incy )
     !! DLA_SYAMV performs the matrix-vector operation
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
           real(dp), intent(in) :: a(lda,*), x(*)
           real(dp), intent(inout) :: y(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: symb_zero
           real(dp) :: temp, safe1
           integer(ilp) :: i, info, iy, j, jx, kx, ky
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if     ( uplo/=stdlib_ilauplo( 'U' ) .and.uplo/=stdlib_ilauplo( 'L' ) ) then
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
              call stdlib_xerbla( 'DLA_SYAMV', info )
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
                          temp = abs( a( j, i ) )
                          symb_zero = symb_zero .and.( x( j ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*abs( x( j ) )*temp
                       end do
                       do j = i+1, n
                          temp = abs( a( i, j ) )
                          symb_zero = symb_zero .and.( x( j ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*abs( x( j ) )*temp
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
                    if ( alpha /= zero ) then
                       do j = 1, i
                          temp = abs( a( i, j ) )
                          symb_zero = symb_zero .and.( x( j ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*abs( x( j ) )*temp
                       end do
                       do j = i+1, n
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
                          temp = abs( a( j, i ) )
                          symb_zero = symb_zero .and.( x( j ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*abs( x( jx ) )*temp
                          jx = jx + incx
                       end do
                       do j = i+1, n
                          temp = abs( a( i, j ) )
                          symb_zero = symb_zero .and.( x( j ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*abs( x( jx ) )*temp
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
                          temp = abs( a( i, j ) )
                          symb_zero = symb_zero .and.( x( j ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*abs( x( jx ) )*temp
                          jx = jx + incx
                       end do
                       do j = i+1, n
                          temp = abs( a( j, i ) )
                          symb_zero = symb_zero .and.( x( j ) == zero .or. temp == zero )
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
     end subroutine stdlib_dla_syamv


     module subroutine stdlib_cla_syamv( uplo, n, alpha, a, lda, x, incx, beta, y,incy )
     !! CLA_SYAMV performs the matrix-vector operation
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
           integer(ilp), intent(in) :: incx, incy, lda, n
           integer(ilp), intent(in) :: uplo
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
              call stdlib_xerbla( 'CLA_SYAMV', info )
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
                    if ( .not.symb_zero )y( iy ) = y( iy ) + sign( safe1, y( iy ) )
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
     end subroutine stdlib_cla_syamv

     module subroutine stdlib_zla_syamv( uplo, n, alpha, a, lda, x, incx, beta, y,incy )
     !! ZLA_SYAMV performs the matrix-vector operation
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
           integer(ilp), intent(in) :: incx, incy, lda, n
           integer(ilp), intent(in) :: uplo
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
              call stdlib_xerbla( 'ZLA_SYAMV', info )
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
                    if ( .not.symb_zero )y( iy ) = y( iy ) + sign( safe1, y( iy ) )
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
     end subroutine stdlib_zla_syamv




     real(sp) module function stdlib_sla_syrcond( uplo, n, a, lda, af, ldaf, ipiv, cmode,c, info, work, &
     !! SLA_SYRCOND estimates the Skeel condition number of  op(A) * op2(C)
     !! where op2 is determined by CMODE as follows
     !! CMODE =  1    op2(C) = C
     !! CMODE =  0    op2(C) = I
     !! CMODE = -1    op2(C) = inv(C)
     !! The Skeel condition number cond(A) = norminf( |inv(A)||A| )
     !! is computed by computing scaling factors R such that
     !! diag(R)*A*op2(C) is row equilibrated and computing the standard
     !! infinity-norm condition number.
               iwork )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: n, lda, ldaf, cmode
           integer(ilp), intent(out) :: info
           ! Array Arguments
           integer(ilp), intent(out) :: iwork(*)
           integer(ilp), intent(in) :: ipiv(*)
           real(sp), intent(in) :: a(lda,*), af(ldaf,*), c(*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           character :: normin
           integer(ilp) :: kase, i, j
           real(sp) :: ainvnm, smlnum, tmp
           logical(lk) :: up
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           stdlib_sla_syrcond = zero
           info = 0_ilp
           if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           else if( ldaf<max( 1_ilp, n ) ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SLA_SYRCOND', -info )
              return
           end if
           if( n==0_ilp ) then
              stdlib_sla_syrcond = one
              return
           end if
           up = .false.
           if ( stdlib_lsame( uplo, 'U' ) ) up = .true.
           ! compute the equilibration matrix r such that
           ! inv(r)*a*c has unit 1-norm.
           if ( up ) then
              do i = 1, n
                 tmp = zero
                 if ( cmode == 1_ilp ) then
                    do j = 1, i
                       tmp = tmp + abs( a( j, i ) * c( j ) )
                    end do
                    do j = i+1, n
                       tmp = tmp + abs( a( i, j ) * c( j ) )
                    end do
                 else if ( cmode == 0_ilp ) then
                    do j = 1, i
                       tmp = tmp + abs( a( j, i ) )
                    end do
                    do j = i+1, n
                       tmp = tmp + abs( a( i, j ) )
                    end do
                 else
                    do j = 1, i
                       tmp = tmp + abs( a( j, i ) / c( j ) )
                    end do
                    do j = i+1, n
                       tmp = tmp + abs( a( i, j ) / c( j ) )
                    end do
                 end if
                 work( 2_ilp*n+i ) = tmp
              end do
           else
              do i = 1, n
                 tmp = zero
                 if ( cmode == 1_ilp ) then
                    do j = 1, i
                       tmp = tmp + abs( a( i, j ) * c( j ) )
                    end do
                    do j = i+1, n
                       tmp = tmp + abs( a( j, i ) * c( j ) )
                    end do
                 else if ( cmode == 0_ilp ) then
                    do j = 1, i
                       tmp = tmp + abs( a( i, j ) )
                    end do
                    do j = i+1, n
                       tmp = tmp + abs( a( j, i ) )
                    end do
                 else
                    do j = 1, i
                       tmp = tmp + abs( a( i, j) / c( j ) )
                    end do
                    do j = i+1, n
                       tmp = tmp + abs( a( j, i) / c( j ) )
                    end do
                 end if
                 work( 2_ilp*n+i ) = tmp
              end do
           endif
           ! estimate the norm of inv(op(a)).
           smlnum = stdlib_slamch( 'SAFE MINIMUM' )
           ainvnm = zero
           normin = 'N'
           kase = 0_ilp
           10 continue
           call stdlib_slacn2( n, work( n+1 ), work, iwork, ainvnm, kase, isave )
           if( kase/=0_ilp ) then
              if( kase==2_ilp ) then
                 ! multiply by r.
                 do i = 1, n
                    work( i ) = work( i ) * work( 2_ilp*n+i )
                 end do
                 if ( up ) then
                    call stdlib_ssytrs( 'U', n, 1_ilp, af, ldaf, ipiv, work, n, info )
                 else
                    call stdlib_ssytrs( 'L', n, 1_ilp, af, ldaf, ipiv, work, n, info )
                 endif
                 ! multiply by inv(c).
                 if ( cmode == 1_ilp ) then
                    do i = 1, n
                       work( i ) = work( i ) / c( i )
                    end do
                 else if ( cmode == -1_ilp ) then
                    do i = 1, n
                       work( i ) = work( i ) * c( i )
                    end do
                 end if
              else
                 ! multiply by inv(c**t).
                 if ( cmode == 1_ilp ) then
                    do i = 1, n
                       work( i ) = work( i ) / c( i )
                    end do
                 else if ( cmode == -1_ilp ) then
                    do i = 1, n
                       work( i ) = work( i ) * c( i )
                    end do
                 end if
                 if ( up ) then
                    call stdlib_ssytrs( 'U', n, 1_ilp, af, ldaf, ipiv, work, n, info )
                 else
                    call stdlib_ssytrs( 'L', n, 1_ilp, af, ldaf, ipiv, work, n, info )
                 endif
                 ! multiply by r.
                 do i = 1, n
                    work( i ) = work( i ) * work( 2_ilp*n+i )
                 end do
              end if
              go to 10
           end if
           ! compute the estimate of the reciprocal condition number.
           if( ainvnm /= 0.0_sp )stdlib_sla_syrcond = ( one / ainvnm )
           return
     end function stdlib_sla_syrcond

     real(dp) module function stdlib_dla_syrcond( uplo, n, a, lda, af, ldaf,ipiv, cmode, c, info, work,&
     !! DLA_SYRCOND estimates the Skeel condition number of  op(A) * op2(C)
     !! where op2 is determined by CMODE as follows
     !! CMODE =  1    op2(C) = C
     !! CMODE =  0    op2(C) = I
     !! CMODE = -1    op2(C) = inv(C)
     !! The Skeel condition number cond(A) = norminf( |inv(A)||A| )
     !! is computed by computing scaling factors R such that
     !! diag(R)*A*op2(C) is row equilibrated and computing the standard
     !! infinity-norm condition number.
               iwork )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: n, lda, ldaf, cmode
           integer(ilp), intent(out) :: info
           ! Array Arguments
           integer(ilp), intent(out) :: iwork(*)
           integer(ilp), intent(in) :: ipiv(*)
           real(dp), intent(in) :: a(lda,*), af(ldaf,*), c(*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           character :: normin
           integer(ilp) :: kase, i, j
           real(dp) :: ainvnm, smlnum, tmp
           logical(lk) :: up
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           stdlib_dla_syrcond = zero
           info = 0_ilp
           if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           else if( ldaf<max( 1_ilp, n ) ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DLA_SYRCOND', -info )
              return
           end if
           if( n==0_ilp ) then
              stdlib_dla_syrcond = one
              return
           end if
           up = .false.
           if ( stdlib_lsame( uplo, 'U' ) ) up = .true.
           ! compute the equilibration matrix r such that
           ! inv(r)*a*c has unit 1-norm.
           if ( up ) then
              do i = 1, n
                 tmp = zero
                 if ( cmode == 1_ilp ) then
                    do j = 1, i
                       tmp = tmp + abs( a( j, i ) * c( j ) )
                    end do
                    do j = i+1, n
                       tmp = tmp + abs( a( i, j ) * c( j ) )
                    end do
                 else if ( cmode == 0_ilp ) then
                    do j = 1, i
                       tmp = tmp + abs( a( j, i ) )
                    end do
                    do j = i+1, n
                       tmp = tmp + abs( a( i, j ) )
                    end do
                 else
                    do j = 1, i
                       tmp = tmp + abs( a( j, i ) / c( j ) )
                    end do
                    do j = i+1, n
                       tmp = tmp + abs( a( i, j ) / c( j ) )
                    end do
                 end if
                 work( 2_ilp*n+i ) = tmp
              end do
           else
              do i = 1, n
                 tmp = zero
                 if ( cmode == 1_ilp ) then
                    do j = 1, i
                       tmp = tmp + abs( a( i, j ) * c( j ) )
                    end do
                    do j = i+1, n
                       tmp = tmp + abs( a( j, i ) * c( j ) )
                    end do
                 else if ( cmode == 0_ilp ) then
                    do j = 1, i
                       tmp = tmp + abs( a( i, j ) )
                    end do
                    do j = i+1, n
                       tmp = tmp + abs( a( j, i ) )
                    end do
                 else
                    do j = 1, i
                       tmp = tmp + abs( a( i, j) / c( j ) )
                    end do
                    do j = i+1, n
                       tmp = tmp + abs( a( j, i) / c( j ) )
                    end do
                 end if
                 work( 2_ilp*n+i ) = tmp
              end do
           endif
           ! estimate the norm of inv(op(a)).
           smlnum = stdlib_dlamch( 'SAFE MINIMUM' )
           ainvnm = zero
           normin = 'N'
           kase = 0_ilp
           10 continue
           call stdlib_dlacn2( n, work( n+1 ), work, iwork, ainvnm, kase, isave )
           if( kase/=0_ilp ) then
              if( kase==2_ilp ) then
                 ! multiply by r.
                 do i = 1, n
                    work( i ) = work( i ) * work( 2_ilp*n+i )
                 end do
                 if ( up ) then
                    call stdlib_dsytrs( 'U', n, 1_ilp, af, ldaf, ipiv, work, n, info )
                 else
                    call stdlib_dsytrs( 'L', n, 1_ilp, af, ldaf, ipiv, work, n, info )
                 endif
                 ! multiply by inv(c).
                 if ( cmode == 1_ilp ) then
                    do i = 1, n
                       work( i ) = work( i ) / c( i )
                    end do
                 else if ( cmode == -1_ilp ) then
                    do i = 1, n
                       work( i ) = work( i ) * c( i )
                    end do
                 end if
              else
                 ! multiply by inv(c**t).
                 if ( cmode == 1_ilp ) then
                    do i = 1, n
                       work( i ) = work( i ) / c( i )
                    end do
                 else if ( cmode == -1_ilp ) then
                    do i = 1, n
                       work( i ) = work( i ) * c( i )
                    end do
                 end if
                 if ( up ) then
                    call stdlib_dsytrs( 'U', n, 1_ilp, af, ldaf, ipiv, work, n, info )
                 else
                    call stdlib_dsytrs( 'L', n, 1_ilp, af, ldaf, ipiv, work, n, info )
                 endif
                 ! multiply by r.
                 do i = 1, n
                    work( i ) = work( i ) * work( 2_ilp*n+i )
                 end do
              end if
              go to 10
           end if
           ! compute the estimate of the reciprocal condition number.
           if( ainvnm /= zero )stdlib_dla_syrcond = ( one / ainvnm )
           return
     end function stdlib_dla_syrcond




     real(sp) module function stdlib_cla_syrcond_c( uplo, n, a, lda, af, ldaf, ipiv, c,capply, info, &
     !! CLA_SYRCOND_C Computes the infinity norm condition number of
     !! op(A) * inv(diag(C)) where C is a REAL vector.
               work, rwork )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           logical(lk), intent(in) :: capply
           integer(ilp), intent(in) :: n, lda, ldaf
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           complex(sp), intent(in) :: a(lda,*), af(ldaf,*)
           complex(sp), intent(out) :: work(*)
           real(sp), intent(in) :: c(*)
           real(sp), intent(out) :: rwork(*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: kase
           real(sp) :: ainvnm, anorm, tmp
           integer(ilp) :: i, j
           logical(lk) :: up, upper
           complex(sp) :: zdum
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Statement Functions 
           real(sp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=sp) ) + abs( aimag( zdum ) )
           ! Executable Statements 
           stdlib_cla_syrcond_c = zero
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           else if( ldaf<max( 1_ilp, n ) ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CLA_SYRCOND_C', -info )
              return
           end if
           up = .false.
           if ( stdlib_lsame( uplo, 'U' ) ) up = .true.
           ! compute norm of op(a)*op2(c).
           anorm = zero
           if ( up ) then
              do i = 1, n
                 tmp = zero
                 if ( capply ) then
                    do j = 1, i
                       tmp = tmp + cabs1( a( j, i ) ) / c( j )
                    end do
                    do j = i+1, n
                       tmp = tmp + cabs1( a( i, j ) ) / c( j )
                    end do
                 else
                    do j = 1, i
                       tmp = tmp + cabs1( a( j, i ) )
                    end do
                    do j = i+1, n
                       tmp = tmp + cabs1( a( i, j ) )
                    end do
                 end if
                 rwork( i ) = tmp
                 anorm = max( anorm, tmp )
              end do
           else
              do i = 1, n
                 tmp = zero
                 if ( capply ) then
                    do j = 1, i
                       tmp = tmp + cabs1( a( i, j ) ) / c( j )
                    end do
                    do j = i+1, n
                       tmp = tmp + cabs1( a( j, i ) ) / c( j )
                    end do
                 else
                    do j = 1, i
                       tmp = tmp + cabs1( a( i, j ) )
                    end do
                    do j = i+1, n
                       tmp = tmp + cabs1( a( j, i ) )
                    end do
                 end if
                 rwork( i ) = tmp
                 anorm = max( anorm, tmp )
              end do
           end if
           ! quick return if possible.
           if( n==0_ilp ) then
              stdlib_cla_syrcond_c = one
              return
           else if( anorm == zero ) then
              return
           end if
           ! estimate the norm of inv(op(a)).
           ainvnm = zero
           kase = 0_ilp
           10 continue
           call stdlib_clacn2( n, work( n+1 ), work, ainvnm, kase, isave )
           if( kase/=0_ilp ) then
              if( kase==2_ilp ) then
                 ! multiply by r.
                 do i = 1, n
                    work( i ) = work( i ) * rwork( i )
                 end do
                 if ( up ) then
                    call stdlib_csytrs( 'U', n, 1_ilp, af, ldaf, ipiv,work, n, info )
                 else
                    call stdlib_csytrs( 'L', n, 1_ilp, af, ldaf, ipiv,work, n, info )
                 endif
                 ! multiply by inv(c).
                 if ( capply ) then
                    do i = 1, n
                       work( i ) = work( i ) * c( i )
                    end do
                 end if
              else
                 ! multiply by inv(c**t).
                 if ( capply ) then
                    do i = 1, n
                       work( i ) = work( i ) * c( i )
                    end do
                 end if
                 if ( up ) then
                    call stdlib_csytrs( 'U', n, 1_ilp, af, ldaf, ipiv,work, n, info )
                 else
                    call stdlib_csytrs( 'L', n, 1_ilp, af, ldaf, ipiv,work, n, info )
                 end if
                 ! multiply by r.
                 do i = 1, n
                    work( i ) = work( i ) * rwork( i )
                 end do
              end if
              go to 10
           end if
           ! compute the estimate of the reciprocal condition number.
           if( ainvnm /= zero )stdlib_cla_syrcond_c = one / ainvnm
           return
     end function stdlib_cla_syrcond_c

     real(dp) module function stdlib_zla_syrcond_c( uplo, n, a, lda, af,ldaf, ipiv, c, capply,info, work,&
     !! ZLA_SYRCOND_C Computes the infinity norm condition number of
     !! op(A) * inv(diag(C)) where C is a DOUBLE PRECISION vector.
                rwork )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           logical(lk), intent(in) :: capply
           integer(ilp), intent(in) :: n, lda, ldaf
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           integer(ilp), intent(in) :: ipiv(*)
           complex(dp), intent(in) :: a(lda,*), af(ldaf,*)
           complex(dp), intent(out) :: work(*)
           real(dp), intent(in) :: c(*)
           real(dp), intent(out) :: rwork(*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: kase
           real(dp) :: ainvnm, anorm, tmp
           integer(ilp) :: i, j
           logical(lk) :: up, upper
           complex(dp) :: zdum
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Statement Functions 
           real(dp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=dp) ) + abs( aimag( zdum ) )
           ! Executable Statements 
           stdlib_zla_syrcond_c = zero
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           else if( ldaf<max( 1_ilp, n ) ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZLA_SYRCOND_C', -info )
              return
           end if
           up = .false.
           if ( stdlib_lsame( uplo, 'U' ) ) up = .true.
           ! compute norm of op(a)*op2(c).
           anorm = zero
           if ( up ) then
              do i = 1, n
                 tmp = zero
                 if ( capply ) then
                    do j = 1, i
                       tmp = tmp + cabs1( a( j, i ) ) / c( j )
                    end do
                    do j = i+1, n
                       tmp = tmp + cabs1( a( i, j ) ) / c( j )
                    end do
                 else
                    do j = 1, i
                       tmp = tmp + cabs1( a( j, i ) )
                    end do
                    do j = i+1, n
                       tmp = tmp + cabs1( a( i, j ) )
                    end do
                 end if
                 rwork( i ) = tmp
                 anorm = max( anorm, tmp )
              end do
           else
              do i = 1, n
                 tmp = zero
                 if ( capply ) then
                    do j = 1, i
                       tmp = tmp + cabs1( a( i, j ) ) / c( j )
                    end do
                    do j = i+1, n
                       tmp = tmp + cabs1( a( j, i ) ) / c( j )
                    end do
                 else
                    do j = 1, i
                       tmp = tmp + cabs1( a( i, j ) )
                    end do
                    do j = i+1, n
                       tmp = tmp + cabs1( a( j, i ) )
                    end do
                 end if
                 rwork( i ) = tmp
                 anorm = max( anorm, tmp )
              end do
           end if
           ! quick return if possible.
           if( n==0_ilp ) then
              stdlib_zla_syrcond_c = one
              return
           else if( anorm == zero ) then
              return
           end if
           ! estimate the norm of inv(op(a)).
           ainvnm = zero
           kase = 0_ilp
           10 continue
           call stdlib_zlacn2( n, work( n+1 ), work, ainvnm, kase, isave )
           if( kase/=0_ilp ) then
              if( kase==2_ilp ) then
                 ! multiply by r.
                 do i = 1, n
                    work( i ) = work( i ) * rwork( i )
                 end do
                 if ( up ) then
                    call stdlib_zsytrs( 'U', n, 1_ilp, af, ldaf, ipiv,work, n, info )
                 else
                    call stdlib_zsytrs( 'L', n, 1_ilp, af, ldaf, ipiv,work, n, info )
                 endif
                 ! multiply by inv(c).
                 if ( capply ) then
                    do i = 1, n
                       work( i ) = work( i ) * c( i )
                    end do
                 end if
              else
                 ! multiply by inv(c**t).
                 if ( capply ) then
                    do i = 1, n
                       work( i ) = work( i ) * c( i )
                    end do
                 end if
                 if ( up ) then
                    call stdlib_zsytrs( 'U', n, 1_ilp, af, ldaf, ipiv,work, n, info )
                 else
                    call stdlib_zsytrs( 'L', n, 1_ilp, af, ldaf, ipiv,work, n, info )
                 end if
                 ! multiply by r.
                 do i = 1, n
                    work( i ) = work( i ) * rwork( i )
                 end do
              end if
              go to 10
           end if
           ! compute the estimate of the reciprocal condition number.
           if( ainvnm /= zero )stdlib_zla_syrcond_c = one / ainvnm
           return
     end function stdlib_zla_syrcond_c




     real(sp) module function stdlib_cla_porcond_c( uplo, n, a, lda, af, ldaf, c, capply,info, work, &
     !! CLA_PORCOND_C Computes the infinity norm condition number of
     !! op(A) * inv(diag(C)) where C is a REAL vector
               rwork )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           logical(lk), intent(in) :: capply
           integer(ilp), intent(in) :: n, lda, ldaf
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           complex(sp), intent(in) :: a(lda,*), af(ldaf,*)
           complex(sp), intent(out) :: work(*)
           real(sp), intent(in) :: c(*)
           real(sp), intent(out) :: rwork(*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: kase
           real(sp) :: ainvnm, anorm, tmp
           integer(ilp) :: i, j
           logical(lk) :: up, upper
           complex(sp) :: zdum
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Statement Functions 
           real(sp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=sp) ) + abs( aimag( zdum ) )
           ! Executable Statements 
           stdlib_cla_porcond_c = zero
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           else if( ldaf<max( 1_ilp, n ) ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CLA_PORCOND_C', -info )
              return
           end if
           up = .false.
           if ( stdlib_lsame( uplo, 'U' ) ) up = .true.
           ! compute norm of op(a)*op2(c).
           anorm = zero
           if ( up ) then
              do i = 1, n
                 tmp = zero
                 if ( capply ) then
                    do j = 1, i
                       tmp = tmp + cabs1( a( j, i ) ) / c( j )
                    end do
                    do j = i+1, n
                       tmp = tmp + cabs1( a( i, j ) ) / c( j )
                    end do
                 else
                    do j = 1, i
                       tmp = tmp + cabs1( a( j, i ) )
                    end do
                    do j = i+1, n
                       tmp = tmp + cabs1( a( i, j ) )
                    end do
                 end if
                 rwork( i ) = tmp
                 anorm = max( anorm, tmp )
              end do
           else
              do i = 1, n
                 tmp = zero
                 if ( capply ) then
                    do j = 1, i
                       tmp = tmp + cabs1( a( i, j ) ) / c( j )
                    end do
                    do j = i+1, n
                       tmp = tmp + cabs1( a( j, i ) ) / c( j )
                    end do
                 else
                    do j = 1, i
                       tmp = tmp + cabs1( a( i, j ) )
                    end do
                    do j = i+1, n
                       tmp = tmp + cabs1( a( j, i ) )
                    end do
                 end if
                 rwork( i ) = tmp
                 anorm = max( anorm, tmp )
              end do
           end if
           ! quick return if possible.
           if( n==0_ilp ) then
              stdlib_cla_porcond_c = one
              return
           else if( anorm == zero ) then
              return
           end if
           ! estimate the norm of inv(op(a)).
           ainvnm = zero
           kase = 0_ilp
           10 continue
           call stdlib_clacn2( n, work( n+1 ), work, ainvnm, kase, isave )
           if( kase/=0_ilp ) then
              if( kase==2_ilp ) then
                 ! multiply by r.
                 do i = 1, n
                    work( i ) = work( i ) * rwork( i )
                 end do
                 if ( up ) then
                    call stdlib_cpotrs( 'U', n, 1_ilp, af, ldaf,work, n, info )
                 else
                    call stdlib_cpotrs( 'L', n, 1_ilp, af, ldaf,work, n, info )
                 endif
                 ! multiply by inv(c).
                 if ( capply ) then
                    do i = 1, n
                       work( i ) = work( i ) * c( i )
                    end do
                 end if
              else
                 ! multiply by inv(c**h).
                 if ( capply ) then
                    do i = 1, n
                       work( i ) = work( i ) * c( i )
                    end do
                 end if
                 if ( up ) then
                    call stdlib_cpotrs( 'U', n, 1_ilp, af, ldaf,work, n, info )
                 else
                    call stdlib_cpotrs( 'L', n, 1_ilp, af, ldaf,work, n, info )
                 end if
                 ! multiply by r.
                 do i = 1, n
                    work( i ) = work( i ) * rwork( i )
                 end do
              end if
              go to 10
           end if
           ! compute the estimate of the reciprocal condition number.
           if( ainvnm /= zero )stdlib_cla_porcond_c = one / ainvnm
           return
     end function stdlib_cla_porcond_c

     real(dp) module function stdlib_zla_porcond_c( uplo, n, a, lda, af,ldaf, c, capply, info,work, &
     !! ZLA_PORCOND_C Computes the infinity norm condition number of
     !! op(A) * inv(diag(C)) where C is a DOUBLE PRECISION vector
               rwork )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           logical(lk), intent(in) :: capply
           integer(ilp), intent(in) :: n, lda, ldaf
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           complex(dp), intent(in) :: a(lda,*), af(ldaf,*)
           complex(dp), intent(out) :: work(*)
           real(dp), intent(in) :: c(*)
           real(dp), intent(out) :: rwork(*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: kase
           real(dp) :: ainvnm, anorm, tmp
           integer(ilp) :: i, j
           logical(lk) :: up, upper
           complex(dp) :: zdum
           ! Local Arrays 
           integer(ilp) :: isave(3_ilp)
           ! Intrinsic Functions 
           ! Statement Functions 
           real(dp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=dp) ) + abs( aimag( zdum ) )
           ! Executable Statements 
           stdlib_zla_porcond_c = zero
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           else if( ldaf<max( 1_ilp, n ) ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZLA_PORCOND_C', -info )
              return
           end if
           up = .false.
           if ( stdlib_lsame( uplo, 'U' ) ) up = .true.
           ! compute norm of op(a)*op2(c).
           anorm = zero
           if ( up ) then
              do i = 1, n
                 tmp = zero
                 if ( capply ) then
                    do j = 1, i
                       tmp = tmp + cabs1( a( j, i ) ) / c( j )
                    end do
                    do j = i+1, n
                       tmp = tmp + cabs1( a( i, j ) ) / c( j )
                    end do
                 else
                    do j = 1, i
                       tmp = tmp + cabs1( a( j, i ) )
                    end do
                    do j = i+1, n
                       tmp = tmp + cabs1( a( i, j ) )
                    end do
                 end if
                 rwork( i ) = tmp
                 anorm = max( anorm, tmp )
              end do
           else
              do i = 1, n
                 tmp = zero
                 if ( capply ) then
                    do j = 1, i
                       tmp = tmp + cabs1( a( i, j ) ) / c( j )
                    end do
                    do j = i+1, n
                       tmp = tmp + cabs1( a( j, i ) ) / c( j )
                    end do
                 else
                    do j = 1, i
                       tmp = tmp + cabs1( a( i, j ) )
                    end do
                    do j = i+1, n
                       tmp = tmp + cabs1( a( j, i ) )
                    end do
                 end if
                 rwork( i ) = tmp
                 anorm = max( anorm, tmp )
              end do
           end if
           ! quick return if possible.
           if( n==0_ilp ) then
              stdlib_zla_porcond_c = one
              return
           else if( anorm == zero ) then
              return
           end if
           ! estimate the norm of inv(op(a)).
           ainvnm = zero
           kase = 0_ilp
           10 continue
           call stdlib_zlacn2( n, work( n+1 ), work, ainvnm, kase, isave )
           if( kase/=0_ilp ) then
              if( kase==2_ilp ) then
                 ! multiply by r.
                 do i = 1, n
                    work( i ) = work( i ) * rwork( i )
                 end do
                 if ( up ) then
                    call stdlib_zpotrs( 'U', n, 1_ilp, af, ldaf,work, n, info )
                 else
                    call stdlib_zpotrs( 'L', n, 1_ilp, af, ldaf,work, n, info )
                 endif
                 ! multiply by inv(c).
                 if ( capply ) then
                    do i = 1, n
                       work( i ) = work( i ) * c( i )
                    end do
                 end if
              else
                 ! multiply by inv(c**h).
                 if ( capply ) then
                    do i = 1, n
                       work( i ) = work( i ) * c( i )
                    end do
                 end if
                 if ( up ) then
                    call stdlib_zpotrs( 'U', n, 1_ilp, af, ldaf,work, n, info )
                 else
                    call stdlib_zpotrs( 'L', n, 1_ilp, af, ldaf,work, n, info )
                 end if
                 ! multiply by r.
                 do i = 1, n
                    work( i ) = work( i ) * rwork( i )
                 end do
              end if
              go to 10
           end if
           ! compute the estimate of the reciprocal condition number.
           if( ainvnm /= zero )stdlib_zla_porcond_c = one / ainvnm
           return
     end function stdlib_zla_porcond_c




     real(sp) module function stdlib_I64_sla_syrpvgrw( uplo, n, info, a, lda, af, ldaf, ipiv,work )
     !! SLA_SYRPVGRW computes the reciprocal pivot growth factor
     !! norm(A)/norm(U). The "max absolute element" norm is used. If this is
     !! much less than 1, the stability of the LU factorization of the
     !! (equilibrated) matrix A could be poor. This also means that the
     !! solution X, estimated condition numbers, and error bounds could be
     !! unreliable.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(in) :: n, info, lda, ldaf
           ! Array Arguments 
           integer(ilp64), intent(in) :: ipiv(*)
           real(sp), intent(in) :: a(lda,*), af(ldaf,*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp64) :: ncols, i, j, k, kp
           real(sp) :: amax, umax, rpvgrw, tmp
           logical(lk) :: upper
           ! Intrinsic Functions 
           ! Executable Statements 
           upper = stdlib_lsame( 'UPPER', uplo )
           if ( info==0_ilp64 ) then
              if ( upper ) then
                 ncols = 1_ilp64
              else
                 ncols = n
              end if
           else
              ncols = info
           end if
           rpvgrw = one
           do i = 1, 2*n
              work( i ) = zero
           end do
           ! find the max magnitude entry of each column of a.  compute the max
           ! for all n columns so we can apply the pivot permutation while
           ! looping below.  assume a full factorization is the common case.
           if ( upper ) then
              do j = 1, n
                 do i = 1, j
                    work( n+i ) = max( abs( a( i, j ) ), work( n+i ) )
                    work( n+j ) = max( abs( a( i, j ) ), work( n+j ) )
                 end do
              end do
           else
              do j = 1, n
                 do i = j, n
                    work( n+i ) = max( abs( a( i, j ) ), work( n+i ) )
                    work( n+j ) = max( abs( a( i, j ) ), work( n+j ) )
                 end do
              end do
           end if
           ! now find the max magnitude entry of each column of u or l.  also
           ! permute the magnitudes of a above so they're in the same order as
           ! the factor.
           ! the iteration orders and permutations were copied from stdlib_I64_ssytrs.
           ! calls to stdlib_I64_sswap would be severe overkill.
           if ( upper ) then
              k = n
              do while ( k < ncols .and. k>0 )
                 if ( ipiv( k )>0_ilp64 ) then
                    ! 1x1 pivot
                    kp = ipiv( k )
                    if ( kp /= k ) then
                       tmp = work( n+k )
                       work( n+k ) = work( n+kp )
                       work( n+kp ) = tmp
                    end if
                    do i = 1, k
                       work( k ) = max( abs( af( i, k ) ), work( k ) )
                    end do
                    k = k - 1_ilp64
                 else
                    ! 2x2 pivot
                    kp = -ipiv( k )
                    tmp = work( n+k-1 )
                    work( n+k-1 ) = work( n+kp )
                    work( n+kp ) = tmp
                    do i = 1, k-1
                       work( k ) = max( abs( af( i, k ) ), work( k ) )
                       work( k-1 ) = max( abs( af( i, k-1 ) ), work( k-1 ) )
                    end do
                    work( k ) = max( abs( af( k, k ) ), work( k ) )
                    k = k - 2_ilp64
                 end if
              end do
              k = ncols
              do while ( k <= n )
                 if ( ipiv( k )>0_ilp64 ) then
                    kp = ipiv( k )
                    if ( kp /= k ) then
                       tmp = work( n+k )
                       work( n+k ) = work( n+kp )
                       work( n+kp ) = tmp
                    end if
                    k = k + 1_ilp64
                 else
                    kp = -ipiv( k )
                    tmp = work( n+k )
                    work( n+k ) = work( n+kp )
                    work( n+kp ) = tmp
                    k = k + 2_ilp64
                 end if
              end do
           else
              k = 1_ilp64
              do while ( k <= ncols )
                 if ( ipiv( k )>0_ilp64 ) then
                    ! 1x1 pivot
                    kp = ipiv( k )
                    if ( kp /= k ) then
                       tmp = work( n+k )
                       work( n+k ) = work( n+kp )
                       work( n+kp ) = tmp
                    end if
                    do i = k, n
                       work( k ) = max( abs( af( i, k ) ), work( k ) )
                    end do
                    k = k + 1_ilp64
                 else
                    ! 2x2 pivot
                    kp = -ipiv( k )
                    tmp = work( n+k+1 )
                    work( n+k+1 ) = work( n+kp )
                    work( n+kp ) = tmp
                    do i = k+1, n
                       work( k ) = max( abs( af( i, k ) ), work( k ) )
                       work( k+1 ) = max( abs( af(i, k+1 ) ), work( k+1 ) )
                    end do
                    work( k ) = max( abs( af( k, k ) ), work( k ) )
                    k = k + 2_ilp64
                 end if
              end do
              k = ncols
              do while ( k >= 1 )
                 if ( ipiv( k )>0_ilp64 ) then
                    kp = ipiv( k )
                    if ( kp /= k ) then
                       tmp = work( n+k )
                       work( n+k ) = work( n+kp )
                       work( n+kp ) = tmp
                    end if
                    k = k - 1_ilp64
                 else
                    kp = -ipiv( k )
                    tmp = work( n+k )
                    work( n+k ) = work( n+kp )
                    work( n+kp ) = tmp
                    k = k - 2_ilp64
                 endif
              end do
           end if
           ! compute the *inverse* of the max element growth factor.  dividing
           ! by zero would imply the largest entry of the factor's column is
           ! zero.  than can happen when either the column of a is zero or
           ! massive pivots made the factor underflow to zero.  neither counts
           ! as growth in itself, so simply ignore terms with zero
           ! denominators.
           if ( upper ) then
              do i = ncols, n
                 umax = work( i )
                 amax = work( n+i )
                 if ( umax /= 0.0_sp ) then
                    rpvgrw = min( amax / umax, rpvgrw )
                 end if
              end do
           else
              do i = 1, ncols
                 umax = work( i )
                 amax = work( n+i )
                 if ( umax /= 0.0_sp ) then
                    rpvgrw = min( amax / umax, rpvgrw )
                 end if
              end do
           end if
           stdlib_I64_sla_syrpvgrw = rpvgrw
     end function stdlib_I64_sla_syrpvgrw

     real(dp) module function stdlib_I64_dla_syrpvgrw( uplo, n, info, a, lda, af,ldaf, ipiv, work )
     !! DLA_SYRPVGRW computes the reciprocal pivot growth factor
     !! norm(A)/norm(U). The "max absolute element" norm is used. If this is
     !! much less than 1, the stability of the LU factorization of the
     !! (equilibrated) matrix A could be poor. This also means that the
     !! solution X, estimated condition numbers, and error bounds could be
     !! unreliable.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(in) :: n, info, lda, ldaf
           ! Array Arguments 
           integer(ilp64), intent(in) :: ipiv(*)
           real(dp), intent(in) :: a(lda,*), af(ldaf,*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp64) :: ncols, i, j, k, kp
           real(dp) :: amax, umax, rpvgrw, tmp
           logical(lk) :: upper
           ! Intrinsic Functions 
           ! Executable Statements 
           upper = stdlib_lsame( 'UPPER', uplo )
           if ( info==0_ilp64 ) then
              if ( upper ) then
                 ncols = 1_ilp64
              else
                 ncols = n
              end if
           else
              ncols = info
           end if
           rpvgrw = one
           do i = 1, 2*n
              work( i ) = zero
           end do
           ! find the max magnitude entry of each column of a.  compute the max
           ! for all n columns so we can apply the pivot permutation while
           ! looping below.  assume a full factorization is the common case.
           if ( upper ) then
              do j = 1, n
                 do i = 1, j
                    work( n+i ) = max( abs( a( i, j ) ), work( n+i ) )
                    work( n+j ) = max( abs( a( i, j ) ), work( n+j ) )
                 end do
              end do
           else
              do j = 1, n
                 do i = j, n
                    work( n+i ) = max( abs( a( i, j ) ), work( n+i ) )
                    work( n+j ) = max( abs( a( i, j ) ), work( n+j ) )
                 end do
              end do
           end if
           ! now find the max magnitude entry of each column of u or l.  also
           ! permute the magnitudes of a above so they're in the same order as
           ! the factor.
           ! the iteration orders and permutations were copied from stdlib_I64_dsytrs.
           ! calls to stdlib_I64_sswap would be severe overkill.
           if ( upper ) then
              k = n
              do while ( k < ncols .and. k>0 )
                 if ( ipiv( k )>0_ilp64 ) then
                    ! 1x1 pivot
                    kp = ipiv( k )
                    if ( kp /= k ) then
                       tmp = work( n+k )
                       work( n+k ) = work( n+kp )
                       work( n+kp ) = tmp
                    end if
                    do i = 1, k
                       work( k ) = max( abs( af( i, k ) ), work( k ) )
                    end do
                    k = k - 1_ilp64
                 else
                    ! 2x2 pivot
                    kp = -ipiv( k )
                    tmp = work( n+k-1 )
                    work( n+k-1 ) = work( n+kp )
                    work( n+kp ) = tmp
                    do i = 1, k-1
                       work( k ) = max( abs( af( i, k ) ), work( k ) )
                       work( k-1 ) = max( abs( af( i, k-1 ) ), work( k-1 ) )
                    end do
                    work( k ) = max( abs( af( k, k ) ), work( k ) )
                    k = k - 2_ilp64
                 end if
              end do
              k = ncols
              do while ( k <= n )
                 if ( ipiv( k )>0_ilp64 ) then
                    kp = ipiv( k )
                    if ( kp /= k ) then
                       tmp = work( n+k )
                       work( n+k ) = work( n+kp )
                       work( n+kp ) = tmp
                    end if
                    k = k + 1_ilp64
                 else
                    kp = -ipiv( k )
                    tmp = work( n+k )
                    work( n+k ) = work( n+kp )
                    work( n+kp ) = tmp
                    k = k + 2_ilp64
                 end if
              end do
           else
              k = 1_ilp64
              do while ( k <= ncols )
                 if ( ipiv( k )>0_ilp64 ) then
                    ! 1x1 pivot
                    kp = ipiv( k )
                    if ( kp /= k ) then
                       tmp = work( n+k )
                       work( n+k ) = work( n+kp )
                       work( n+kp ) = tmp
                    end if
                    do i = k, n
                       work( k ) = max( abs( af( i, k ) ), work( k ) )
                    end do
                    k = k + 1_ilp64
                 else
                    ! 2x2 pivot
                    kp = -ipiv( k )
                    tmp = work( n+k+1 )
                    work( n+k+1 ) = work( n+kp )
                    work( n+kp ) = tmp
                    do i = k+1, n
                       work( k ) = max( abs( af( i, k ) ), work( k ) )
                       work( k+1 ) = max( abs( af(i, k+1 ) ), work( k+1 ) )
                    end do
                    work( k ) = max( abs( af( k, k ) ), work( k ) )
                    k = k + 2_ilp64
                 end if
              end do
              k = ncols
              do while ( k >= 1 )
                 if ( ipiv( k )>0_ilp64 ) then
                    kp = ipiv( k )
                    if ( kp /= k ) then
                       tmp = work( n+k )
                       work( n+k ) = work( n+kp )
                       work( n+kp ) = tmp
                    end if
                    k = k - 1_ilp64
                 else
                    kp = -ipiv( k )
                    tmp = work( n+k )
                    work( n+k ) = work( n+kp )
                    work( n+kp ) = tmp
                    k = k - 2_ilp64
                 endif
              end do
           end if
           ! compute the *inverse* of the max element growth factor.  dividing
           ! by zero would imply the largest entry of the factor's column is
           ! zero.  than can happen when either the column of a is zero or
           ! massive pivots made the factor underflow to zero.  neither counts
           ! as growth in itself, so simply ignore terms with zero
           ! denominators.
           if ( upper ) then
              do i = ncols, n
                 umax = work( i )
                 amax = work( n+i )
                 if ( umax /= zero ) then
                    rpvgrw = min( amax / umax, rpvgrw )
                 end if
              end do
           else
              do i = 1, ncols
                 umax = work( i )
                 amax = work( n+i )
                 if ( umax /= zero ) then
                    rpvgrw = min( amax / umax, rpvgrw )
                 end if
              end do
           end if
           stdlib_I64_dla_syrpvgrw = rpvgrw
     end function stdlib_I64_dla_syrpvgrw


     real(sp) module function stdlib_I64_cla_syrpvgrw( uplo, n, info, a, lda, af, ldaf, ipiv,work )
     !! CLA_SYRPVGRW computes the reciprocal pivot growth factor
     !! norm(A)/norm(U). The "max absolute element" norm is used. If this is
     !! much less than 1, the stability of the LU factorization of the
     !! (equilibrated) matrix A could be poor. This also means that the
     !! solution X, estimated condition numbers, and error bounds could be
     !! unreliable.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(in) :: n, info, lda, ldaf
           ! Array Arguments 
           complex(sp), intent(in) :: a(lda,*), af(ldaf,*)
           real(sp), intent(out) :: work(*)
           integer(ilp64), intent(in) :: ipiv(*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp64) :: ncols, i, j, k, kp
           real(sp) :: amax, umax, rpvgrw, tmp
           logical(lk) :: upper
           complex(sp) :: zdum
           ! Intrinsic Functions 
           ! Statement Functions 
           real(sp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=sp) ) + abs( aimag ( zdum ) )
           ! Executable Statements 
           upper = stdlib_lsame( 'UPPER', uplo )
           if ( info==0_ilp64 ) then
              if ( upper ) then
                 ncols = 1_ilp64
              else
                 ncols = n
              end if
           else
              ncols = info
           end if
           rpvgrw = one
           do i = 1, 2*n
              work( i ) = zero
           end do
           ! find the max magnitude entry of each column of a.  compute the max
           ! for all n columns so we can apply the pivot permutation while
           ! looping below.  assume a full factorization is the common case.
           if ( upper ) then
              do j = 1, n
                 do i = 1, j
                    work( n+i ) = max( cabs1( a( i, j ) ), work( n+i ) )
                    work( n+j ) = max( cabs1( a( i, j ) ), work( n+j ) )
                 end do
              end do
           else
              do j = 1, n
                 do i = j, n
                    work( n+i ) = max( cabs1( a( i, j ) ), work( n+i ) )
                    work( n+j ) = max( cabs1( a( i, j ) ), work( n+j ) )
                 end do
              end do
           end if
           ! now find the max magnitude entry of each column of u or l.  also
           ! permute the magnitudes of a above so they're in the same order as
           ! the factor.
           ! the iteration orders and permutations were copied from stdlib_I64_csytrs.
           ! calls to stdlib_I64_sswap would be severe overkill.
           if ( upper ) then
              k = n
              do while ( k < ncols .and. k>0 )
                 if ( ipiv( k )>0_ilp64 ) then
                    ! 1x1 pivot
                    kp = ipiv( k )
                    if ( kp /= k ) then
                       tmp = work( n+k )
                       work( n+k ) = work( n+kp )
                       work( n+kp ) = tmp
                    end if
                    do i = 1, k
                       work( k ) = max( cabs1( af( i, k ) ), work( k ) )
                    end do
                    k = k - 1_ilp64
                 else
                    ! 2x2 pivot
                    kp = -ipiv( k )
                    tmp = work( n+k-1 )
                    work( n+k-1 ) = work( n+kp )
                    work( n+kp ) = tmp
                    do i = 1, k-1
                       work( k ) = max( cabs1( af( i, k ) ), work( k ) )
                       work( k-1 ) =max( cabs1( af( i, k-1 ) ), work( k-1 ) )
                    end do
                    work( k ) = max( cabs1( af( k, k ) ), work( k ) )
                    k = k - 2_ilp64
                 end if
              end do
              k = ncols
              do while ( k <= n )
                 if ( ipiv( k )>0_ilp64 ) then
                    kp = ipiv( k )
                    if ( kp /= k ) then
                       tmp = work( n+k )
                       work( n+k ) = work( n+kp )
                       work( n+kp ) = tmp
                    end if
                    k = k + 1_ilp64
                 else
                    kp = -ipiv( k )
                    tmp = work( n+k )
                    work( n+k ) = work( n+kp )
                    work( n+kp ) = tmp
                    k = k + 2_ilp64
                 end if
              end do
           else
              k = 1_ilp64
              do while ( k <= ncols )
                 if ( ipiv( k )>0_ilp64 ) then
                    ! 1x1 pivot
                    kp = ipiv( k )
                    if ( kp /= k ) then
                       tmp = work( n+k )
                       work( n+k ) = work( n+kp )
                       work( n+kp ) = tmp
                    end if
                    do i = k, n
                       work( k ) = max( cabs1( af( i, k ) ), work( k ) )
                    end do
                    k = k + 1_ilp64
                 else
                    ! 2x2 pivot
                    kp = -ipiv( k )
                    tmp = work( n+k+1 )
                    work( n+k+1 ) = work( n+kp )
                    work( n+kp ) = tmp
                    do i = k+1, n
                       work( k ) = max( cabs1( af( i, k ) ), work( k ) )
                       work( k+1 ) =max( cabs1( af( i, k+1 ) ), work( k+1 ) )
                    end do
                    work( k ) = max( cabs1( af( k, k ) ), work( k ) )
                    k = k + 2_ilp64
                 end if
              end do
              k = ncols
              do while ( k >= 1 )
                 if ( ipiv( k )>0_ilp64 ) then
                    kp = ipiv( k )
                    if ( kp /= k ) then
                       tmp = work( n+k )
                       work( n+k ) = work( n+kp )
                       work( n+kp ) = tmp
                    end if
                    k = k - 1_ilp64
                 else
                    kp = -ipiv( k )
                    tmp = work( n+k )
                    work( n+k ) = work( n+kp )
                    work( n+kp ) = tmp
                    k = k - 2_ilp64
                 endif
              end do
           end if
           ! compute the *inverse* of the max element growth factor.  dividing
           ! by zero would imply the largest entry of the factor's column is
           ! zero.  than can happen when either the column of a is zero or
           ! massive pivots made the factor underflow to zero.  neither counts
           ! as growth in itself, so simply ignore terms with zero
           ! denominators.
           if ( upper ) then
              do i = ncols, n
                 umax = work( i )
                 amax = work( n+i )
                 if ( umax /= 0.0_sp ) then
                    rpvgrw = min( amax / umax, rpvgrw )
                 end if
              end do
           else
              do i = 1, ncols
                 umax = work( i )
                 amax = work( n+i )
                 if ( umax /= 0.0_sp ) then
                    rpvgrw = min( amax / umax, rpvgrw )
                 end if
              end do
           end if
           stdlib_I64_cla_syrpvgrw = rpvgrw
     end function stdlib_I64_cla_syrpvgrw

     real(dp) module function stdlib_I64_zla_syrpvgrw( uplo, n, info, a, lda, af,ldaf, ipiv, work )
     !! ZLA_SYRPVGRW computes the reciprocal pivot growth factor
     !! norm(A)/norm(U). The "max absolute element" norm is used. If this is
     !! much less than 1, the stability of the LU factorization of the
     !! (equilibrated) matrix A could be poor. This also means that the
     !! solution X, estimated condition numbers, and error bounds could be
     !! unreliable.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(in) :: n, info, lda, ldaf
           ! Array Arguments 
           complex(dp), intent(in) :: a(lda,*), af(ldaf,*)
           real(dp), intent(out) :: work(*)
           integer(ilp64), intent(in) :: ipiv(*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp64) :: ncols, i, j, k, kp
           real(dp) :: amax, umax, rpvgrw, tmp
           logical(lk) :: upper
           complex(dp) :: zdum
           ! Intrinsic Functions 
           ! Statement Functions 
           real(dp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=dp) ) + abs( aimag ( zdum ) )
           ! Executable Statements 
           upper = stdlib_lsame( 'UPPER', uplo )
           if ( info==0_ilp64 ) then
              if ( upper ) then
                 ncols = 1_ilp64
              else
                 ncols = n
              end if
           else
              ncols = info
           end if
           rpvgrw = one
           do i = 1, 2*n
              work( i ) = zero
           end do
           ! find the max magnitude entry of each column of a.  compute the max
           ! for all n columns so we can apply the pivot permutation while
           ! looping below.  assume a full factorization is the common case.
           if ( upper ) then
              do j = 1, n
                 do i = 1, j
                    work( n+i ) = max( cabs1( a( i, j ) ), work( n+i ) )
                    work( n+j ) = max( cabs1( a( i, j ) ), work( n+j ) )
                 end do
              end do
           else
              do j = 1, n
                 do i = j, n
                    work( n+i ) = max( cabs1( a( i, j ) ), work( n+i ) )
                    work( n+j ) = max( cabs1( a( i, j ) ), work( n+j ) )
                 end do
              end do
           end if
           ! now find the max magnitude entry of each column of u or l.  also
           ! permute the magnitudes of a above so they're in the same order as
           ! the factor.
           ! the iteration orders and permutations were copied from stdlib_I64_zsytrs.
           ! calls to stdlib_I64_sswap would be severe overkill.
           if ( upper ) then
              k = n
              do while ( k < ncols .and. k>0 )
                 if ( ipiv( k )>0_ilp64 ) then
                    ! 1x1 pivot
                    kp = ipiv( k )
                    if ( kp /= k ) then
                       tmp = work( n+k )
                       work( n+k ) = work( n+kp )
                       work( n+kp ) = tmp
                    end if
                    do i = 1, k
                       work( k ) = max( cabs1( af( i, k ) ), work( k ) )
                    end do
                    k = k - 1_ilp64
                 else
                    ! 2x2 pivot
                    kp = -ipiv( k )
                    tmp = work( n+k-1 )
                    work( n+k-1 ) = work( n+kp )
                    work( n+kp ) = tmp
                    do i = 1, k-1
                       work( k ) = max( cabs1( af( i, k ) ), work( k ) )
                       work( k-1 ) =max( cabs1( af( i, k-1 ) ), work( k-1 ) )
                    end do
                    work( k ) = max( cabs1( af( k, k ) ), work( k ) )
                    k = k - 2_ilp64
                 end if
              end do
              k = ncols
              do while ( k <= n )
                 if ( ipiv( k )>0_ilp64 ) then
                    kp = ipiv( k )
                    if ( kp /= k ) then
                       tmp = work( n+k )
                       work( n+k ) = work( n+kp )
                       work( n+kp ) = tmp
                    end if
                    k = k + 1_ilp64
                 else
                    kp = -ipiv( k )
                    tmp = work( n+k )
                    work( n+k ) = work( n+kp )
                    work( n+kp ) = tmp
                    k = k + 2_ilp64
                 end if
              end do
           else
              k = 1_ilp64
              do while ( k <= ncols )
                 if ( ipiv( k )>0_ilp64 ) then
                    ! 1x1 pivot
                    kp = ipiv( k )
                    if ( kp /= k ) then
                       tmp = work( n+k )
                       work( n+k ) = work( n+kp )
                       work( n+kp ) = tmp
                    end if
                    do i = k, n
                       work( k ) = max( cabs1( af( i, k ) ), work( k ) )
                    end do
                    k = k + 1_ilp64
                 else
                    ! 2x2 pivot
                    kp = -ipiv( k )
                    tmp = work( n+k+1 )
                    work( n+k+1 ) = work( n+kp )
                    work( n+kp ) = tmp
                    do i = k+1, n
                       work( k ) = max( cabs1( af( i, k ) ), work( k ) )
                       work( k+1 ) =max( cabs1( af( i, k+1 ) ), work( k+1 ) )
                    end do
                    work( k ) = max( cabs1( af( k, k ) ), work( k ) )
                    k = k + 2_ilp64
                 end if
              end do
              k = ncols
              do while ( k >= 1 )
                 if ( ipiv( k )>0_ilp64 ) then
                    kp = ipiv( k )
                    if ( kp /= k ) then
                       tmp = work( n+k )
                       work( n+k ) = work( n+kp )
                       work( n+kp ) = tmp
                    end if
                    k = k - 1_ilp64
                 else
                    kp = -ipiv( k )
                    tmp = work( n+k )
                    work( n+k ) = work( n+kp )
                    work( n+kp ) = tmp
                    k = k - 2_ilp64
                 endif
              end do
           end if
           ! compute the *inverse* of the max element growth factor.  dividing
           ! by zero would imply the largest entry of the factor's column is
           ! zero.  than can happen when either the column of a is zero or
           ! massive pivots made the factor underflow to zero.  neither counts
           ! as growth in itself, so simply ignore terms with zero
           ! denominators.
           if ( upper ) then
              do i = ncols, n
                 umax = work( i )
                 amax = work( n+i )
                 if ( umax /= zero ) then
                    rpvgrw = min( amax / umax, rpvgrw )
                 end if
              end do
           else
              do i = 1, ncols
                 umax = work( i )
                 amax = work( n+i )
                 if ( umax /= zero ) then
                    rpvgrw = min( amax / umax, rpvgrw )
                 end if
              end do
           end if
           stdlib_I64_zla_syrpvgrw = rpvgrw
     end function stdlib_I64_zla_syrpvgrw




     pure real(sp) module function stdlib_I64_sla_gerpvgrw( n, ncols, a, lda, af, ldaf )
     !! SLA_GERPVGRW computes the reciprocal pivot growth factor
     !! norm(A)/norm(U). The "max absolute element" norm is used. If this is
     !! much less than 1, the stability of the LU factorization of the
     !! (equilibrated) matrix A could be poor. This also means that the
     !! solution X, estimated condition numbers, and error bounds could be
     !! unreliable.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: n, ncols, lda, ldaf
           ! Array Arguments 
           real(sp), intent(in) :: a(lda,*), af(ldaf,*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp64) :: i, j
           real(sp) :: amax, umax, rpvgrw
           ! Intrinsic Functions 
           ! Executable Statements 
           rpvgrw = one
           do j = 1, ncols
              amax = zero
              umax = zero
              do i = 1, n
                 amax = max( abs( a( i, j ) ), amax )
              end do
              do i = 1, j
                 umax = max( abs( af( i, j ) ), umax )
              end do
              if ( umax /= 0.0_sp ) then
                 rpvgrw = min( amax / umax, rpvgrw )
              end if
           end do
           stdlib_I64_sla_gerpvgrw = rpvgrw
     end function stdlib_I64_sla_gerpvgrw

     pure real(dp) module function stdlib_I64_dla_gerpvgrw( n, ncols, a, lda, af,ldaf )
     !! DLA_GERPVGRW computes the reciprocal pivot growth factor
     !! norm(A)/norm(U). The "max absolute element" norm is used. If this is
     !! much less than 1, the stability of the LU factorization of the
     !! (equilibrated) matrix A could be poor. This also means that the
     !! solution X, estimated condition numbers, and error bounds could be
     !! unreliable.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: n, ncols, lda, ldaf
           ! Array Arguments 
           real(dp), intent(in) :: a(lda,*), af(ldaf,*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp64) :: i, j
           real(dp) :: amax, umax, rpvgrw
           ! Intrinsic Functions 
           ! Executable Statements 
           rpvgrw = one
           do j = 1, ncols
              amax = zero
              umax = zero
              do i = 1, n
                 amax = max( abs( a( i, j ) ), amax )
              end do
              do i = 1, j
                 umax = max( abs( af( i, j ) ), umax )
              end do
              if ( umax /= zero ) then
                 rpvgrw = min( amax / umax, rpvgrw )
              end if
           end do
           stdlib_I64_dla_gerpvgrw = rpvgrw
     end function stdlib_I64_dla_gerpvgrw


     pure real(sp) module function stdlib_I64_cla_gerpvgrw( n, ncols, a, lda, af, ldaf )
     !! CLA_GERPVGRW computes the reciprocal pivot growth factor
     !! norm(A)/norm(U). The "max absolute element" norm is used. If this is
     !! much less than 1, the stability of the LU factorization of the
     !! (equilibrated) matrix A could be poor. This also means that the
     !! solution X, estimated condition numbers, and error bounds could be
     !! unreliable.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: n, ncols, lda, ldaf
           ! Array Arguments 
           complex(sp), intent(in) :: a(lda,*), af(ldaf,*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp64) :: i, j
           real(sp) :: amax, umax, rpvgrw
           complex(sp) :: zdum
           ! Intrinsic Functions 
           ! Statement Functions 
           real(sp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=sp) ) + abs( aimag( zdum ) )
           ! Executable Statements 
           rpvgrw = one
           do j = 1, ncols
              amax = zero
              umax = zero
              do i = 1, n
                 amax = max( cabs1( a( i, j ) ), amax )
              end do
              do i = 1, j
                 umax = max( cabs1( af( i, j ) ), umax )
              end do
              if ( umax /= 0.0_sp ) then
                 rpvgrw = min( amax / umax, rpvgrw )
              end if
           end do
           stdlib_I64_cla_gerpvgrw = rpvgrw
     end function stdlib_I64_cla_gerpvgrw

     pure real(dp) module function stdlib_I64_zla_gerpvgrw( n, ncols, a, lda, af,ldaf )
     !! ZLA_GERPVGRW computes the reciprocal pivot growth factor
     !! norm(A)/norm(U). The "max absolute element" norm is used. If this is
     !! much less than 1, the stability of the LU factorization of the
     !! (equilibrated) matrix A could be poor. This also means that the
     !! solution X, estimated condition numbers, and error bounds could be
     !! unreliable.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: n, ncols, lda, ldaf
           ! Array Arguments 
           complex(dp), intent(in) :: a(lda,*), af(ldaf,*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp64) :: i, j
           real(dp) :: amax, umax, rpvgrw
           complex(dp) :: zdum
           ! Intrinsic Functions 
           ! Statement Functions 
           real(dp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=dp) ) + abs( aimag( zdum ) )
           ! Executable Statements 
           rpvgrw = one
           do j = 1, ncols
              amax = zero
              umax = zero
              do i = 1, n
                 amax = max( cabs1( a( i, j ) ), amax )
              end do
              do i = 1, j
                 umax = max( cabs1( af( i, j ) ), umax )
              end do
              if ( umax /= zero ) then
                 rpvgrw = min( amax / umax, rpvgrw )
              end if
           end do
           stdlib_I64_zla_gerpvgrw = rpvgrw
     end function stdlib_I64_zla_gerpvgrw




     real(sp) module function stdlib_I64_cla_gbrcond_c( trans, n, kl, ku, ab, ldab, afb,ldafb, ipiv, c, &
     !! CLA_GBRCOND_C Computes the infinity norm condition number of
     !! op(A) * inv(diag(C)) where C is a REAL vector.
               capply, info, work,rwork )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: trans
           logical(lk), intent(in) :: capply
           integer(ilp64), intent(in) :: n, kl, ku, ldab, ldafb
           integer(ilp64) :: kd, ke
           integer(ilp64), intent(out) :: info
           ! Array Arguments 
           integer(ilp64), intent(in) :: ipiv(*)
           complex(sp), intent(in) :: ab(ldab,*), afb(ldafb,*)
           complex(sp), intent(out) :: work(*)
           real(sp), intent(in) :: c(*)
           real(sp), intent(out) :: rwork(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: notrans
           integer(ilp64) :: kase, i, j
           real(sp) :: ainvnm, anorm, tmp
           complex(sp) :: zdum
           ! Local Arrays 
           integer(ilp64) :: isave(3_ilp64)
           ! Intrinsic Functions 
           ! Statement Functions 
           real(sp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=sp) ) + abs( aimag( zdum ) )
           ! Executable Statements 
           stdlib_I64_cla_gbrcond_c = zero
           info = 0_ilp64
           notrans = stdlib_lsame( trans, 'N' )
           if ( .not. notrans .and. .not. stdlib_lsame( trans, 'T' ) .and. .not.stdlib_lsame( &
                     trans, 'C' ) ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -2_ilp64
           else if( kl<0_ilp64 .or. kl>n-1 ) then
              info = -3_ilp64
           else if( ku<0_ilp64 .or. ku>n-1 ) then
              info = -4_ilp64
           else if( ldab<kl+ku+1 ) then
              info = -6_ilp64
           else if( ldafb<2_ilp64*kl+ku+1 ) then
              info = -8_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'CLA_GBRCOND_C', -info )
              return
           end if
           ! compute norm of op(a)*op2(c).
           anorm = zero
           kd = ku + 1_ilp64
           ke = kl + 1_ilp64
           if ( notrans ) then
              do i = 1, n
                 tmp = zero
                 if ( capply ) then
                    do j = max( i-kl, 1 ), min( i+ku, n )
                       tmp = tmp + cabs1( ab( kd+i-j, j ) ) / c( j )
                    end do
                 else
                    do j = max( i-kl, 1 ), min( i+ku, n )
                       tmp = tmp + cabs1( ab( kd+i-j, j ) )
                    end do
                 end if
                 rwork( i ) = tmp
                 anorm = max( anorm, tmp )
              end do
           else
              do i = 1, n
                 tmp = zero
                 if ( capply ) then
                    do j = max( i-kl, 1 ), min( i+ku, n )
                       tmp = tmp + cabs1( ab( ke-i+j, i ) ) / c( j )
                    end do
                 else
                    do j = max( i-kl, 1 ), min( i+ku, n )
                       tmp = tmp + cabs1( ab( ke-i+j, i ) )
                    end do
                 end if
                 rwork( i ) = tmp
                 anorm = max( anorm, tmp )
              end do
           end if
           ! quick return if possible.
           if( n==0_ilp64 ) then
              stdlib_I64_cla_gbrcond_c = one
              return
           else if( anorm == zero ) then
              return
           end if
           ! estimate the norm of inv(op(a)).
           ainvnm = zero
           kase = 0_ilp64
           10 continue
           call stdlib_I64_clacn2( n, work( n+1 ), work, ainvnm, kase, isave )
           if( kase/=0_ilp64 ) then
              if( kase==2_ilp64 ) then
                 ! multiply by r.
                 do i = 1, n
                    work( i ) = work( i ) * rwork( i )
                 end do
                 if ( notrans ) then
                    call stdlib_I64_cgbtrs( 'NO TRANSPOSE', n, kl, ku, 1_ilp64, afb, ldafb,ipiv, work, n, &
                              info )
                 else
                    call stdlib_I64_cgbtrs( 'CONJUGATE TRANSPOSE', n, kl, ku, 1_ilp64, afb,ldafb, ipiv, &
                              work, n, info )
                 endif
                 ! multiply by inv(c).
                 if ( capply ) then
                    do i = 1, n
                       work( i ) = work( i ) * c( i )
                    end do
                 end if
              else
                 ! multiply by inv(c**h).
                 if ( capply ) then
                    do i = 1, n
                       work( i ) = work( i ) * c( i )
                    end do
                 end if
                 if ( notrans ) then
                    call stdlib_I64_cgbtrs( 'CONJUGATE TRANSPOSE', n, kl, ku, 1_ilp64, afb,ldafb, ipiv,  &
                              work, n, info )
                 else
                    call stdlib_I64_cgbtrs( 'NO TRANSPOSE', n, kl, ku, 1_ilp64, afb, ldafb,ipiv, work, n, &
                              info )
                 end if
                 ! multiply by r.
                 do i = 1, n
                    work( i ) = work( i ) * rwork( i )
                 end do
              end if
              go to 10
           end if
           ! compute the estimate of the reciprocal condition number.
           if( ainvnm /= zero )stdlib_I64_cla_gbrcond_c = one / ainvnm
           return
     end function stdlib_I64_cla_gbrcond_c

     real(dp) module function stdlib_I64_zla_gbrcond_c( trans, n, kl, ku, ab,ldab, afb, ldafb, ipiv,c, &
     !! ZLA_GBRCOND_C Computes the infinity norm condition number of
     !! op(A) * inv(diag(C)) where C is a DOUBLE PRECISION vector.
               capply, info, work,rwork )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: trans
           logical(lk), intent(in) :: capply
           integer(ilp64), intent(in) :: n, kl, ku, ldab, ldafb
           integer(ilp64) :: kd, ke
           integer(ilp64), intent(out) :: info
           ! Array Arguments 
           integer(ilp64), intent(in) :: ipiv(*)
           complex(dp), intent(in) :: ab(ldab,*), afb(ldafb,*)
           complex(dp), intent(out) :: work(*)
           real(dp), intent(in) :: c(*)
           real(dp), intent(out) :: rwork(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: notrans
           integer(ilp64) :: kase, i, j
           real(dp) :: ainvnm, anorm, tmp
           complex(dp) :: zdum
           ! Local Arrays 
           integer(ilp64) :: isave(3_ilp64)
           ! Intrinsic Functions 
           ! Statement Functions 
           real(dp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=dp) ) + abs( aimag( zdum ) )
           ! Executable Statements 
           stdlib_I64_zla_gbrcond_c = zero
           info = 0_ilp64
           notrans = stdlib_lsame( trans, 'N' )
           if ( .not. notrans .and. .not. stdlib_lsame( trans, 'T' ) .and. .not.stdlib_lsame( &
                     trans, 'C' ) ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -2_ilp64
           else if( kl<0_ilp64 .or. kl>n-1 ) then
              info = -3_ilp64
           else if( ku<0_ilp64 .or. ku>n-1 ) then
              info = -4_ilp64
           else if( ldab<kl+ku+1 ) then
              info = -6_ilp64
           else if( ldafb<2_ilp64*kl+ku+1 ) then
              info = -8_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'ZLA_GBRCOND_C', -info )
              return
           end if
           ! compute norm of op(a)*op2(c).
           anorm = zero
           kd = ku + 1_ilp64
           ke = kl + 1_ilp64
           if ( notrans ) then
              do i = 1, n
                 tmp = zero
                 if ( capply ) then
                    do j = max( i-kl, 1 ), min( i+ku, n )
                       tmp = tmp + cabs1( ab( kd+i-j, j ) ) / c( j )
                    end do
                 else
                    do j = max( i-kl, 1 ), min( i+ku, n )
                       tmp = tmp + cabs1( ab( kd+i-j, j ) )
                    end do
                 end if
                 rwork( i ) = tmp
                 anorm = max( anorm, tmp )
              end do
           else
              do i = 1, n
                 tmp = zero
                 if ( capply ) then
                    do j = max( i-kl, 1 ), min( i+ku, n )
                       tmp = tmp + cabs1( ab( ke-i+j, i ) ) / c( j )
                    end do
                 else
                    do j = max( i-kl, 1 ), min( i+ku, n )
                       tmp = tmp + cabs1( ab( ke-i+j, i ) )
                    end do
                 end if
                 rwork( i ) = tmp
                 anorm = max( anorm, tmp )
              end do
           end if
           ! quick return if possible.
           if( n==0_ilp64 ) then
              stdlib_I64_zla_gbrcond_c = one
              return
           else if( anorm == zero ) then
              return
           end if
           ! estimate the norm of inv(op(a)).
           ainvnm = zero
           kase = 0_ilp64
           10 continue
           call stdlib_I64_zlacn2( n, work( n+1 ), work, ainvnm, kase, isave )
           if( kase/=0_ilp64 ) then
              if( kase==2_ilp64 ) then
                 ! multiply by r.
                 do i = 1, n
                    work( i ) = work( i ) * rwork( i )
                 end do
                 if ( notrans ) then
                    call stdlib_I64_zgbtrs( 'NO TRANSPOSE', n, kl, ku, 1_ilp64, afb, ldafb,ipiv, work, n, &
                              info )
                 else
                    call stdlib_I64_zgbtrs( 'CONJUGATE TRANSPOSE', n, kl, ku, 1_ilp64, afb,ldafb, ipiv, &
                              work, n, info )
                 endif
                 ! multiply by inv(c).
                 if ( capply ) then
                    do i = 1, n
                       work( i ) = work( i ) * c( i )
                    end do
                 end if
              else
                 ! multiply by inv(c**h).
                 if ( capply ) then
                    do i = 1, n
                       work( i ) = work( i ) * c( i )
                    end do
                 end if
                 if ( notrans ) then
                    call stdlib_I64_zgbtrs( 'CONJUGATE TRANSPOSE', n, kl, ku, 1_ilp64, afb,ldafb, ipiv,  &
                              work, n, info )
                 else
                    call stdlib_I64_zgbtrs( 'NO TRANSPOSE', n, kl, ku, 1_ilp64, afb, ldafb,ipiv, work, n, &
                              info )
                 end if
                 ! multiply by r.
                 do i = 1, n
                    work( i ) = work( i ) * rwork( i )
                 end do
              end if
              go to 10
           end if
           ! compute the estimate of the reciprocal condition number.
           if( ainvnm /= zero )stdlib_I64_zla_gbrcond_c = one / ainvnm
           return
     end function stdlib_I64_zla_gbrcond_c




     real(sp) module function stdlib_I64_cla_gercond_c( trans, n, a, lda, af, ldaf, ipiv, c,capply, info, &
     !! CLA_GERCOND_C computes the infinity norm condition number of
     !! op(A) * inv(diag(C)) where C is a REAL vector.
               work, rwork )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: trans
           logical(lk), intent(in) :: capply
           integer(ilp64), intent(in) :: n, lda, ldaf
           integer(ilp64), intent(out) :: info
           ! Array Arguments 
           integer(ilp64), intent(in) :: ipiv(*)
           complex(sp), intent(in) :: a(lda,*), af(ldaf,*)
           complex(sp), intent(out) :: work(*)
           real(sp), intent(in) :: c(*)
           real(sp), intent(out) :: rwork(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: notrans
           integer(ilp64) :: kase, i, j
           real(sp) :: ainvnm, anorm, tmp
           complex(sp) :: zdum
           ! Local Arrays 
           integer(ilp64) :: isave(3_ilp64)
           ! Intrinsic Functions 
           ! Statement Functions 
           real(sp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=sp) ) + abs( aimag( zdum ) )
           ! Executable Statements 
           stdlib_I64_cla_gercond_c = zero
           info = 0_ilp64
           notrans = stdlib_lsame( trans, 'N' )
           if ( .not. notrans .and. .not. stdlib_lsame( trans, 'T' ) .and. .not.stdlib_lsame( &
                     trans, 'C' ) ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -2_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -4_ilp64
           else if( ldaf<max( 1_ilp64, n ) ) then
              info = -6_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'CLA_GERCOND_C', -info )
              return
           end if
           ! compute norm of op(a)*op2(c).
           anorm = zero
           if ( notrans ) then
              do i = 1, n
                 tmp = zero
                 if ( capply ) then
                    do j = 1, n
                       tmp = tmp + cabs1( a( i, j ) ) / c( j )
                    end do
                 else
                    do j = 1, n
                       tmp = tmp + cabs1( a( i, j ) )
                    end do
                 end if
                 rwork( i ) = tmp
                 anorm = max( anorm, tmp )
              end do
           else
              do i = 1, n
                 tmp = zero
                 if ( capply ) then
                    do j = 1, n
                       tmp = tmp + cabs1( a( j, i ) ) / c( j )
                    end do
                 else
                    do j = 1, n
                       tmp = tmp + cabs1( a( j, i ) )
                    end do
                 end if
                 rwork( i ) = tmp
                 anorm = max( anorm, tmp )
              end do
           end if
           ! quick return if possible.
           if( n==0_ilp64 ) then
              stdlib_I64_cla_gercond_c = one
              return
           else if( anorm == zero ) then
              return
           end if
           ! estimate the norm of inv(op(a)).
           ainvnm = zero
           kase = 0_ilp64
           10 continue
           call stdlib_I64_clacn2( n, work( n+1 ), work, ainvnm, kase, isave )
           if( kase/=0_ilp64 ) then
              if( kase==2_ilp64 ) then
                 ! multiply by r.
                 do i = 1, n
                    work( i ) = work( i ) * rwork( i )
                 end do
                 if (notrans) then
                    call stdlib_I64_cgetrs( 'NO TRANSPOSE', n, 1_ilp64, af, ldaf, ipiv,work, n, info )
                              
                 else
                    call stdlib_I64_cgetrs( 'CONJUGATE TRANSPOSE', n, 1_ilp64, af, ldaf, ipiv,work, n, info &
                              )
                 endif
                 ! multiply by inv(c).
                 if ( capply ) then
                    do i = 1, n
                       work( i ) = work( i ) * c( i )
                    end do
                 end if
              else
                 ! multiply by inv(c**h).
                 if ( capply ) then
                    do i = 1, n
                       work( i ) = work( i ) * c( i )
                    end do
                 end if
                 if ( notrans ) then
                    call stdlib_I64_cgetrs( 'CONJUGATE TRANSPOSE', n, 1_ilp64, af, ldaf, ipiv,work, n, info &
                              )
                 else
                    call stdlib_I64_cgetrs( 'NO TRANSPOSE', n, 1_ilp64, af, ldaf, ipiv,work, n, info )
                              
                 end if
                 ! multiply by r.
                 do i = 1, n
                    work( i ) = work( i ) * rwork( i )
                 end do
              end if
              go to 10
           end if
           ! compute the estimate of the reciprocal condition number.
           if( ainvnm /= zero )stdlib_I64_cla_gercond_c = one / ainvnm
           return
     end function stdlib_I64_cla_gercond_c

     real(dp) module function stdlib_I64_zla_gercond_c( trans, n, a, lda, af,ldaf, ipiv, c, capply,info, &
     !! ZLA_GERCOND_C computes the infinity norm condition number of
     !! op(A) * inv(diag(C)) where C is a DOUBLE PRECISION vector.
               work, rwork )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: trans
           logical(lk), intent(in) :: capply
           integer(ilp64), intent(in) :: n, lda, ldaf
           integer(ilp64), intent(out) :: info
           ! Array Arguments 
           integer(ilp64), intent(in) :: ipiv(*)
           complex(dp), intent(in) :: a(lda,*), af(ldaf,*)
           complex(dp), intent(out) :: work(*)
           real(dp), intent(in) :: c(*)
           real(dp), intent(out) :: rwork(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: notrans
           integer(ilp64) :: kase, i, j
           real(dp) :: ainvnm, anorm, tmp
           complex(dp) :: zdum
           ! Local Arrays 
           integer(ilp64) :: isave(3_ilp64)
           ! Intrinsic Functions 
           ! Statement Functions 
           real(dp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=dp) ) + abs( aimag( zdum ) )
           ! Executable Statements 
           stdlib_I64_zla_gercond_c = zero
           info = 0_ilp64
           notrans = stdlib_lsame( trans, 'N' )
           if ( .not. notrans .and. .not. stdlib_lsame( trans, 'T' ) .and. .not.stdlib_lsame( &
                     trans, 'C' ) ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -2_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -4_ilp64
           else if( ldaf<max( 1_ilp64, n ) ) then
              info = -6_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'ZLA_GERCOND_C', -info )
              return
           end if
           ! compute norm of op(a)*op2(c).
           anorm = zero
           if ( notrans ) then
              do i = 1, n
                 tmp = zero
                 if ( capply ) then
                    do j = 1, n
                       tmp = tmp + cabs1( a( i, j ) ) / c( j )
                    end do
                 else
                    do j = 1, n
                       tmp = tmp + cabs1( a( i, j ) )
                    end do
                 end if
                 rwork( i ) = tmp
                 anorm = max( anorm, tmp )
              end do
           else
              do i = 1, n
                 tmp = zero
                 if ( capply ) then
                    do j = 1, n
                       tmp = tmp + cabs1( a( j, i ) ) / c( j )
                    end do
                 else
                    do j = 1, n
                       tmp = tmp + cabs1( a( j, i ) )
                    end do
                 end if
                 rwork( i ) = tmp
                 anorm = max( anorm, tmp )
              end do
           end if
           ! quick return if possible.
           if( n==0_ilp64 ) then
              stdlib_I64_zla_gercond_c = one
              return
           else if( anorm == zero ) then
              return
           end if
           ! estimate the norm of inv(op(a)).
           ainvnm = zero
           kase = 0_ilp64
           10 continue
           call stdlib_I64_zlacn2( n, work( n+1 ), work, ainvnm, kase, isave )
           if( kase/=0_ilp64 ) then
              if( kase==2_ilp64 ) then
                 ! multiply by r.
                 do i = 1, n
                    work( i ) = work( i ) * rwork( i )
                 end do
                 if (notrans) then
                    call stdlib_I64_zgetrs( 'NO TRANSPOSE', n, 1_ilp64, af, ldaf, ipiv,work, n, info )
                              
                 else
                    call stdlib_I64_zgetrs( 'CONJUGATE TRANSPOSE', n, 1_ilp64, af, ldaf, ipiv,work, n, info &
                              )
                 endif
                 ! multiply by inv(c).
                 if ( capply ) then
                    do i = 1, n
                       work( i ) = work( i ) * c( i )
                    end do
                 end if
              else
                 ! multiply by inv(c**h).
                 if ( capply ) then
                    do i = 1, n
                       work( i ) = work( i ) * c( i )
                    end do
                 end if
                 if ( notrans ) then
                    call stdlib_I64_zgetrs( 'CONJUGATE TRANSPOSE', n, 1_ilp64, af, ldaf, ipiv,work, n, info &
                              )
                 else
                    call stdlib_I64_zgetrs( 'NO TRANSPOSE', n, 1_ilp64, af, ldaf, ipiv,work, n, info )
                              
                 end if
                 ! multiply by r.
                 do i = 1, n
                    work( i ) = work( i ) * rwork( i )
                 end do
              end if
              go to 10
           end if
           ! compute the estimate of the reciprocal condition number.
           if( ainvnm /= zero )stdlib_I64_zla_gercond_c = one / ainvnm
           return
     end function stdlib_I64_zla_gercond_c




     real(sp) module function stdlib_I64_cla_hercond_c( uplo, n, a, lda, af, ldaf, ipiv, c,capply, info, &
     !! CLA_HERCOND_C computes the infinity norm condition number of
     !! op(A) * inv(diag(C)) where C is a REAL vector.
               work, rwork )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           logical(lk), intent(in) :: capply
           integer(ilp64), intent(in) :: n, lda, ldaf
           integer(ilp64), intent(out) :: info
           ! Array Arguments 
           integer(ilp64), intent(in) :: ipiv(*)
           complex(sp), intent(in) :: a(lda,*), af(ldaf,*)
           complex(sp), intent(out) :: work(*)
           real(sp), intent(in) :: c(*)
           real(sp), intent(out) :: rwork(*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp64) :: kase, i, j
           real(sp) :: ainvnm, anorm, tmp
           logical(lk) :: up, upper
           complex(sp) :: zdum
           ! Local Arrays 
           integer(ilp64) :: isave(3_ilp64)
           ! Intrinsic Functions 
           ! Statement Functions 
           real(sp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=sp) ) + abs( aimag( zdum ) )
           ! Executable Statements 
           stdlib_I64_cla_hercond_c = zero
           info = 0_ilp64
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -2_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -4_ilp64
           else if( ldaf<max( 1_ilp64, n ) ) then
              info = -6_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'CLA_HERCOND_C', -info )
              return
           end if
           up = .false.
           if ( stdlib_lsame( uplo, 'U' ) ) up = .true.
           ! compute norm of op(a)*op2(c).
           anorm = zero
           if ( up ) then
              do i = 1, n
                 tmp = zero
                 if ( capply ) then
                    do j = 1, i
                       tmp = tmp + cabs1( a( j, i ) ) / c( j )
                    end do
                    do j = i+1, n
                       tmp = tmp + cabs1( a( i, j ) ) / c( j )
                    end do
                 else
                    do j = 1, i
                       tmp = tmp + cabs1( a( j, i ) )
                    end do
                    do j = i+1, n
                       tmp = tmp + cabs1( a( i, j ) )
                    end do
                 end if
                 rwork( i ) = tmp
                 anorm = max( anorm, tmp )
              end do
           else
              do i = 1, n
                 tmp = zero
                 if ( capply ) then
                    do j = 1, i
                       tmp = tmp + cabs1( a( i, j ) ) / c( j )
                    end do
                    do j = i+1, n
                       tmp = tmp + cabs1( a( j, i ) ) / c( j )
                    end do
                 else
                    do j = 1, i
                       tmp = tmp + cabs1( a( i, j ) )
                    end do
                    do j = i+1, n
                       tmp = tmp + cabs1( a( j, i ) )
                    end do
                 end if
                 rwork( i ) = tmp
                 anorm = max( anorm, tmp )
              end do
           end if
           ! quick return if possible.
           if( n==0_ilp64 ) then
              stdlib_I64_cla_hercond_c = one
              return
           else if( anorm == zero ) then
              return
           end if
           ! estimate the norm of inv(op(a)).
           ainvnm = zero
           kase = 0_ilp64
           10 continue
           call stdlib_I64_clacn2( n, work( n+1 ), work, ainvnm, kase, isave )
           if( kase/=0_ilp64 ) then
              if( kase==2_ilp64 ) then
                 ! multiply by r.
                 do i = 1, n
                    work( i ) = work( i ) * rwork( i )
                 end do
                 if ( up ) then
                    call stdlib_I64_chetrs( 'U', n, 1_ilp64, af, ldaf, ipiv,work, n, info )
                 else
                    call stdlib_I64_chetrs( 'L', n, 1_ilp64, af, ldaf, ipiv,work, n, info )
                 endif
                 ! multiply by inv(c).
                 if ( capply ) then
                    do i = 1, n
                       work( i ) = work( i ) * c( i )
                    end do
                 end if
              else
                 ! multiply by inv(c**h).
                 if ( capply ) then
                    do i = 1, n
                       work( i ) = work( i ) * c( i )
                    end do
                 end if
                 if ( up ) then
                    call stdlib_I64_chetrs( 'U', n, 1_ilp64, af, ldaf, ipiv,work, n, info )
                 else
                    call stdlib_I64_chetrs( 'L', n, 1_ilp64, af, ldaf, ipiv,work, n, info )
                 end if
                 ! multiply by r.
                 do i = 1, n
                    work( i ) = work( i ) * rwork( i )
                 end do
              end if
              go to 10
           end if
           ! compute the estimate of the reciprocal condition number.
           if( ainvnm /= zero )stdlib_I64_cla_hercond_c = one / ainvnm
           return
     end function stdlib_I64_cla_hercond_c

     real(dp) module function stdlib_I64_zla_hercond_c( uplo, n, a, lda, af,ldaf, ipiv, c, capply,info, work,&
     !! ZLA_HERCOND_C computes the infinity norm condition number of
     !! op(A) * inv(diag(C)) where C is a DOUBLE PRECISION vector.
                rwork )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           logical(lk), intent(in) :: capply
           integer(ilp64), intent(in) :: n, lda, ldaf
           integer(ilp64), intent(out) :: info
           ! Array Arguments 
           integer(ilp64), intent(in) :: ipiv(*)
           complex(dp), intent(in) :: a(lda,*), af(ldaf,*)
           complex(dp), intent(out) :: work(*)
           real(dp), intent(in) :: c(*)
           real(dp), intent(out) :: rwork(*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp64) :: kase, i, j
           real(dp) :: ainvnm, anorm, tmp
           logical(lk) :: up, upper
           complex(dp) :: zdum
           ! Local Arrays 
           integer(ilp64) :: isave(3_ilp64)
           ! Intrinsic Functions 
           ! Statement Functions 
           real(dp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=dp) ) + abs( aimag( zdum ) )
           ! Executable Statements 
           stdlib_I64_zla_hercond_c = zero
           info = 0_ilp64
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -2_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -4_ilp64
           else if( ldaf<max( 1_ilp64, n ) ) then
              info = -6_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'ZLA_HERCOND_C', -info )
              return
           end if
           up = .false.
           if ( stdlib_lsame( uplo, 'U' ) ) up = .true.
           ! compute norm of op(a)*op2(c).
           anorm = zero
           if ( up ) then
              do i = 1, n
                 tmp = zero
                 if ( capply ) then
                    do j = 1, i
                       tmp = tmp + cabs1( a( j, i ) ) / c( j )
                    end do
                    do j = i+1, n
                       tmp = tmp + cabs1( a( i, j ) ) / c( j )
                    end do
                 else
                    do j = 1, i
                       tmp = tmp + cabs1( a( j, i ) )
                    end do
                    do j = i+1, n
                       tmp = tmp + cabs1( a( i, j ) )
                    end do
                 end if
                 rwork( i ) = tmp
                 anorm = max( anorm, tmp )
              end do
           else
              do i = 1, n
                 tmp = zero
                 if ( capply ) then
                    do j = 1, i
                       tmp = tmp + cabs1( a( i, j ) ) / c( j )
                    end do
                    do j = i+1, n
                       tmp = tmp + cabs1( a( j, i ) ) / c( j )
                    end do
                 else
                    do j = 1, i
                       tmp = tmp + cabs1( a( i, j ) )
                    end do
                    do j = i+1, n
                       tmp = tmp + cabs1( a( j, i ) )
                    end do
                 end if
                 rwork( i ) = tmp
                 anorm = max( anorm, tmp )
              end do
           end if
           ! quick return if possible.
           if( n==0_ilp64 ) then
              stdlib_I64_zla_hercond_c = one
              return
           else if( anorm == zero ) then
              return
           end if
           ! estimate the norm of inv(op(a)).
           ainvnm = zero
           kase = 0_ilp64
           10 continue
           call stdlib_I64_zlacn2( n, work( n+1 ), work, ainvnm, kase, isave )
           if( kase/=0_ilp64 ) then
              if( kase==2_ilp64 ) then
                 ! multiply by r.
                 do i = 1, n
                    work( i ) = work( i ) * rwork( i )
                 end do
                 if ( up ) then
                    call stdlib_I64_zhetrs( 'U', n, 1_ilp64, af, ldaf, ipiv,work, n, info )
                 else
                    call stdlib_I64_zhetrs( 'L', n, 1_ilp64, af, ldaf, ipiv,work, n, info )
                 endif
                 ! multiply by inv(c).
                 if ( capply ) then
                    do i = 1, n
                       work( i ) = work( i ) * c( i )
                    end do
                 end if
              else
                 ! multiply by inv(c**h).
                 if ( capply ) then
                    do i = 1, n
                       work( i ) = work( i ) * c( i )
                    end do
                 end if
                 if ( up ) then
                    call stdlib_I64_zhetrs( 'U', n, 1_ilp64, af, ldaf, ipiv,work, n, info )
                 else
                    call stdlib_I64_zhetrs( 'L', n, 1_ilp64, af, ldaf, ipiv,work, n, info )
                 end if
                 ! multiply by r.
                 do i = 1, n
                    work( i ) = work( i ) * rwork( i )
                 end do
              end if
              go to 10
           end if
           ! compute the estimate of the reciprocal condition number.
           if( ainvnm /= zero )stdlib_I64_zla_hercond_c = one / ainvnm
           return
     end function stdlib_I64_zla_hercond_c




     module subroutine stdlib_I64_sla_syamv( uplo, n, alpha, a, lda, x, incx, beta, y,incy )
     !! SLA_SYAMV performs the matrix-vector operation
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
           real(sp), intent(in) :: a(lda,*), x(*)
           real(sp), intent(inout) :: y(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: symb_zero
           real(sp) :: temp, safe1
           integer(ilp64) :: i, info, iy, j, jx, kx, ky
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           if     ( uplo/=stdlib_I64_ilauplo( 'U' ) .and.uplo/=stdlib_I64_ilauplo( 'L' ) ) then
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
              call stdlib_I64_xerbla( 'SLA_SYAMV', info )
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
                          temp = abs( a( j, i ) )
                          symb_zero = symb_zero .and.( x( j ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*abs( x( j ) )*temp
                       end do
                       do j = i+1, n
                          temp = abs( a( i, j ) )
                          symb_zero = symb_zero .and.( x( j ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*abs( x( j ) )*temp
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
                    if ( alpha /= zero ) then
                       do j = 1, i
                          temp = abs( a( i, j ) )
                          symb_zero = symb_zero .and.( x( j ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*abs( x( j ) )*temp
                       end do
                       do j = i+1, n
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
                          temp = abs( a( j, i ) )
                          symb_zero = symb_zero .and.( x( j ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*abs( x( jx ) )*temp
                          jx = jx + incx
                       end do
                       do j = i+1, n
                          temp = abs( a( i, j ) )
                          symb_zero = symb_zero .and.( x( j ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*abs( x( jx ) )*temp
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
                          temp = abs( a( i, j ) )
                          symb_zero = symb_zero .and.( x( j ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*abs( x( jx ) )*temp
                          jx = jx + incx
                       end do
                       do j = i+1, n
                          temp = abs( a( j, i ) )
                          symb_zero = symb_zero .and.( x( j ) == zero .or. temp == zero )
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
     end subroutine stdlib_I64_sla_syamv

     module subroutine stdlib_I64_dla_syamv( uplo, n, alpha, a, lda, x, incx, beta, y,incy )
     !! DLA_SYAMV performs the matrix-vector operation
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
           real(dp), intent(in) :: a(lda,*), x(*)
           real(dp), intent(inout) :: y(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: symb_zero
           real(dp) :: temp, safe1
           integer(ilp64) :: i, info, iy, j, jx, kx, ky
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           if     ( uplo/=stdlib_I64_ilauplo( 'U' ) .and.uplo/=stdlib_I64_ilauplo( 'L' ) ) then
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
              call stdlib_I64_xerbla( 'DLA_SYAMV', info )
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
                          temp = abs( a( j, i ) )
                          symb_zero = symb_zero .and.( x( j ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*abs( x( j ) )*temp
                       end do
                       do j = i+1, n
                          temp = abs( a( i, j ) )
                          symb_zero = symb_zero .and.( x( j ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*abs( x( j ) )*temp
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
                    if ( alpha /= zero ) then
                       do j = 1, i
                          temp = abs( a( i, j ) )
                          symb_zero = symb_zero .and.( x( j ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*abs( x( j ) )*temp
                       end do
                       do j = i+1, n
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
                          temp = abs( a( j, i ) )
                          symb_zero = symb_zero .and.( x( j ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*abs( x( jx ) )*temp
                          jx = jx + incx
                       end do
                       do j = i+1, n
                          temp = abs( a( i, j ) )
                          symb_zero = symb_zero .and.( x( j ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*abs( x( jx ) )*temp
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
                          temp = abs( a( i, j ) )
                          symb_zero = symb_zero .and.( x( j ) == zero .or. temp == zero )
                          y( iy ) = y( iy ) + alpha*abs( x( jx ) )*temp
                          jx = jx + incx
                       end do
                       do j = i+1, n
                          temp = abs( a( j, i ) )
                          symb_zero = symb_zero .and.( x( j ) == zero .or. temp == zero )
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
     end subroutine stdlib_I64_dla_syamv


     module subroutine stdlib_I64_cla_syamv( uplo, n, alpha, a, lda, x, incx, beta, y,incy )
     !! CLA_SYAMV performs the matrix-vector operation
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
           integer(ilp64), intent(in) :: incx, incy, lda, n
           integer(ilp64), intent(in) :: uplo
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
              call stdlib_I64_xerbla( 'CLA_SYAMV', info )
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
                    if ( .not.symb_zero )y( iy ) = y( iy ) + sign( safe1, y( iy ) )
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
     end subroutine stdlib_I64_cla_syamv

     module subroutine stdlib_I64_zla_syamv( uplo, n, alpha, a, lda, x, incx, beta, y,incy )
     !! ZLA_SYAMV performs the matrix-vector operation
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
           integer(ilp64), intent(in) :: incx, incy, lda, n
           integer(ilp64), intent(in) :: uplo
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
              call stdlib_I64_xerbla( 'ZLA_SYAMV', info )
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
                    if ( .not.symb_zero )y( iy ) = y( iy ) + sign( safe1, y( iy ) )
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
     end subroutine stdlib_I64_zla_syamv




     real(sp) module function stdlib_I64_sla_syrcond( uplo, n, a, lda, af, ldaf, ipiv, cmode,c, info, work, &
     !! SLA_SYRCOND estimates the Skeel condition number of  op(A) * op2(C)
     !! where op2 is determined by CMODE as follows
     !! CMODE =  1    op2(C) = C
     !! CMODE =  0    op2(C) = I
     !! CMODE = -1    op2(C) = inv(C)
     !! The Skeel condition number cond(A) = norminf( |inv(A)||A| )
     !! is computed by computing scaling factors R such that
     !! diag(R)*A*op2(C) is row equilibrated and computing the standard
     !! infinity-norm condition number.
               iwork )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(in) :: n, lda, ldaf, cmode
           integer(ilp64), intent(out) :: info
           ! Array Arguments
           integer(ilp64), intent(out) :: iwork(*)
           integer(ilp64), intent(in) :: ipiv(*)
           real(sp), intent(in) :: a(lda,*), af(ldaf,*), c(*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           character :: normin
           integer(ilp64) :: kase, i, j
           real(sp) :: ainvnm, smlnum, tmp
           logical(lk) :: up
           ! Local Arrays 
           integer(ilp64) :: isave(3_ilp64)
           ! Intrinsic Functions 
           ! Executable Statements 
           stdlib_I64_sla_syrcond = zero
           info = 0_ilp64
           if( n<0_ilp64 ) then
              info = -2_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -4_ilp64
           else if( ldaf<max( 1_ilp64, n ) ) then
              info = -6_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'SLA_SYRCOND', -info )
              return
           end if
           if( n==0_ilp64 ) then
              stdlib_I64_sla_syrcond = one
              return
           end if
           up = .false.
           if ( stdlib_lsame( uplo, 'U' ) ) up = .true.
           ! compute the equilibration matrix r such that
           ! inv(r)*a*c has unit 1-norm.
           if ( up ) then
              do i = 1, n
                 tmp = zero
                 if ( cmode == 1_ilp64 ) then
                    do j = 1, i
                       tmp = tmp + abs( a( j, i ) * c( j ) )
                    end do
                    do j = i+1, n
                       tmp = tmp + abs( a( i, j ) * c( j ) )
                    end do
                 else if ( cmode == 0_ilp64 ) then
                    do j = 1, i
                       tmp = tmp + abs( a( j, i ) )
                    end do
                    do j = i+1, n
                       tmp = tmp + abs( a( i, j ) )
                    end do
                 else
                    do j = 1, i
                       tmp = tmp + abs( a( j, i ) / c( j ) )
                    end do
                    do j = i+1, n
                       tmp = tmp + abs( a( i, j ) / c( j ) )
                    end do
                 end if
                 work( 2_ilp64*n+i ) = tmp
              end do
           else
              do i = 1, n
                 tmp = zero
                 if ( cmode == 1_ilp64 ) then
                    do j = 1, i
                       tmp = tmp + abs( a( i, j ) * c( j ) )
                    end do
                    do j = i+1, n
                       tmp = tmp + abs( a( j, i ) * c( j ) )
                    end do
                 else if ( cmode == 0_ilp64 ) then
                    do j = 1, i
                       tmp = tmp + abs( a( i, j ) )
                    end do
                    do j = i+1, n
                       tmp = tmp + abs( a( j, i ) )
                    end do
                 else
                    do j = 1, i
                       tmp = tmp + abs( a( i, j) / c( j ) )
                    end do
                    do j = i+1, n
                       tmp = tmp + abs( a( j, i) / c( j ) )
                    end do
                 end if
                 work( 2_ilp64*n+i ) = tmp
              end do
           endif
           ! estimate the norm of inv(op(a)).
           smlnum = stdlib_I64_slamch( 'SAFE MINIMUM' )
           ainvnm = zero
           normin = 'N'
           kase = 0_ilp64
           10 continue
           call stdlib_I64_slacn2( n, work( n+1 ), work, iwork, ainvnm, kase, isave )
           if( kase/=0_ilp64 ) then
              if( kase==2_ilp64 ) then
                 ! multiply by r.
                 do i = 1, n
                    work( i ) = work( i ) * work( 2_ilp64*n+i )
                 end do
                 if ( up ) then
                    call stdlib_I64_ssytrs( 'U', n, 1_ilp64, af, ldaf, ipiv, work, n, info )
                 else
                    call stdlib_I64_ssytrs( 'L', n, 1_ilp64, af, ldaf, ipiv, work, n, info )
                 endif
                 ! multiply by inv(c).
                 if ( cmode == 1_ilp64 ) then
                    do i = 1, n
                       work( i ) = work( i ) / c( i )
                    end do
                 else if ( cmode == -1_ilp64 ) then
                    do i = 1, n
                       work( i ) = work( i ) * c( i )
                    end do
                 end if
              else
                 ! multiply by inv(c**t).
                 if ( cmode == 1_ilp64 ) then
                    do i = 1, n
                       work( i ) = work( i ) / c( i )
                    end do
                 else if ( cmode == -1_ilp64 ) then
                    do i = 1, n
                       work( i ) = work( i ) * c( i )
                    end do
                 end if
                 if ( up ) then
                    call stdlib_I64_ssytrs( 'U', n, 1_ilp64, af, ldaf, ipiv, work, n, info )
                 else
                    call stdlib_I64_ssytrs( 'L', n, 1_ilp64, af, ldaf, ipiv, work, n, info )
                 endif
                 ! multiply by r.
                 do i = 1, n
                    work( i ) = work( i ) * work( 2_ilp64*n+i )
                 end do
              end if
              go to 10
           end if
           ! compute the estimate of the reciprocal condition number.
           if( ainvnm /= 0.0_sp )stdlib_I64_sla_syrcond = ( one / ainvnm )
           return
     end function stdlib_I64_sla_syrcond

     real(dp) module function stdlib_I64_dla_syrcond( uplo, n, a, lda, af, ldaf,ipiv, cmode, c, info, work,&
     !! DLA_SYRCOND estimates the Skeel condition number of  op(A) * op2(C)
     !! where op2 is determined by CMODE as follows
     !! CMODE =  1    op2(C) = C
     !! CMODE =  0    op2(C) = I
     !! CMODE = -1    op2(C) = inv(C)
     !! The Skeel condition number cond(A) = norminf( |inv(A)||A| )
     !! is computed by computing scaling factors R such that
     !! diag(R)*A*op2(C) is row equilibrated and computing the standard
     !! infinity-norm condition number.
               iwork )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(in) :: n, lda, ldaf, cmode
           integer(ilp64), intent(out) :: info
           ! Array Arguments
           integer(ilp64), intent(out) :: iwork(*)
           integer(ilp64), intent(in) :: ipiv(*)
           real(dp), intent(in) :: a(lda,*), af(ldaf,*), c(*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           character :: normin
           integer(ilp64) :: kase, i, j
           real(dp) :: ainvnm, smlnum, tmp
           logical(lk) :: up
           ! Local Arrays 
           integer(ilp64) :: isave(3_ilp64)
           ! Intrinsic Functions 
           ! Executable Statements 
           stdlib_I64_dla_syrcond = zero
           info = 0_ilp64
           if( n<0_ilp64 ) then
              info = -2_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -4_ilp64
           else if( ldaf<max( 1_ilp64, n ) ) then
              info = -6_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'DLA_SYRCOND', -info )
              return
           end if
           if( n==0_ilp64 ) then
              stdlib_I64_dla_syrcond = one
              return
           end if
           up = .false.
           if ( stdlib_lsame( uplo, 'U' ) ) up = .true.
           ! compute the equilibration matrix r such that
           ! inv(r)*a*c has unit 1-norm.
           if ( up ) then
              do i = 1, n
                 tmp = zero
                 if ( cmode == 1_ilp64 ) then
                    do j = 1, i
                       tmp = tmp + abs( a( j, i ) * c( j ) )
                    end do
                    do j = i+1, n
                       tmp = tmp + abs( a( i, j ) * c( j ) )
                    end do
                 else if ( cmode == 0_ilp64 ) then
                    do j = 1, i
                       tmp = tmp + abs( a( j, i ) )
                    end do
                    do j = i+1, n
                       tmp = tmp + abs( a( i, j ) )
                    end do
                 else
                    do j = 1, i
                       tmp = tmp + abs( a( j, i ) / c( j ) )
                    end do
                    do j = i+1, n
                       tmp = tmp + abs( a( i, j ) / c( j ) )
                    end do
                 end if
                 work( 2_ilp64*n+i ) = tmp
              end do
           else
              do i = 1, n
                 tmp = zero
                 if ( cmode == 1_ilp64 ) then
                    do j = 1, i
                       tmp = tmp + abs( a( i, j ) * c( j ) )
                    end do
                    do j = i+1, n
                       tmp = tmp + abs( a( j, i ) * c( j ) )
                    end do
                 else if ( cmode == 0_ilp64 ) then
                    do j = 1, i
                       tmp = tmp + abs( a( i, j ) )
                    end do
                    do j = i+1, n
                       tmp = tmp + abs( a( j, i ) )
                    end do
                 else
                    do j = 1, i
                       tmp = tmp + abs( a( i, j) / c( j ) )
                    end do
                    do j = i+1, n
                       tmp = tmp + abs( a( j, i) / c( j ) )
                    end do
                 end if
                 work( 2_ilp64*n+i ) = tmp
              end do
           endif
           ! estimate the norm of inv(op(a)).
           smlnum = stdlib_I64_dlamch( 'SAFE MINIMUM' )
           ainvnm = zero
           normin = 'N'
           kase = 0_ilp64
           10 continue
           call stdlib_I64_dlacn2( n, work( n+1 ), work, iwork, ainvnm, kase, isave )
           if( kase/=0_ilp64 ) then
              if( kase==2_ilp64 ) then
                 ! multiply by r.
                 do i = 1, n
                    work( i ) = work( i ) * work( 2_ilp64*n+i )
                 end do
                 if ( up ) then
                    call stdlib_I64_dsytrs( 'U', n, 1_ilp64, af, ldaf, ipiv, work, n, info )
                 else
                    call stdlib_I64_dsytrs( 'L', n, 1_ilp64, af, ldaf, ipiv, work, n, info )
                 endif
                 ! multiply by inv(c).
                 if ( cmode == 1_ilp64 ) then
                    do i = 1, n
                       work( i ) = work( i ) / c( i )
                    end do
                 else if ( cmode == -1_ilp64 ) then
                    do i = 1, n
                       work( i ) = work( i ) * c( i )
                    end do
                 end if
              else
                 ! multiply by inv(c**t).
                 if ( cmode == 1_ilp64 ) then
                    do i = 1, n
                       work( i ) = work( i ) / c( i )
                    end do
                 else if ( cmode == -1_ilp64 ) then
                    do i = 1, n
                       work( i ) = work( i ) * c( i )
                    end do
                 end if
                 if ( up ) then
                    call stdlib_I64_dsytrs( 'U', n, 1_ilp64, af, ldaf, ipiv, work, n, info )
                 else
                    call stdlib_I64_dsytrs( 'L', n, 1_ilp64, af, ldaf, ipiv, work, n, info )
                 endif
                 ! multiply by r.
                 do i = 1, n
                    work( i ) = work( i ) * work( 2_ilp64*n+i )
                 end do
              end if
              go to 10
           end if
           ! compute the estimate of the reciprocal condition number.
           if( ainvnm /= zero )stdlib_I64_dla_syrcond = ( one / ainvnm )
           return
     end function stdlib_I64_dla_syrcond




     real(sp) module function stdlib_I64_cla_syrcond_c( uplo, n, a, lda, af, ldaf, ipiv, c,capply, info, &
     !! CLA_SYRCOND_C Computes the infinity norm condition number of
     !! op(A) * inv(diag(C)) where C is a REAL vector.
               work, rwork )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           logical(lk), intent(in) :: capply
           integer(ilp64), intent(in) :: n, lda, ldaf
           integer(ilp64), intent(out) :: info
           ! Array Arguments 
           integer(ilp64), intent(in) :: ipiv(*)
           complex(sp), intent(in) :: a(lda,*), af(ldaf,*)
           complex(sp), intent(out) :: work(*)
           real(sp), intent(in) :: c(*)
           real(sp), intent(out) :: rwork(*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp64) :: kase
           real(sp) :: ainvnm, anorm, tmp
           integer(ilp64) :: i, j
           logical(lk) :: up, upper
           complex(sp) :: zdum
           ! Local Arrays 
           integer(ilp64) :: isave(3_ilp64)
           ! Intrinsic Functions 
           ! Statement Functions 
           real(sp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=sp) ) + abs( aimag( zdum ) )
           ! Executable Statements 
           stdlib_I64_cla_syrcond_c = zero
           info = 0_ilp64
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -2_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -4_ilp64
           else if( ldaf<max( 1_ilp64, n ) ) then
              info = -6_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'CLA_SYRCOND_C', -info )
              return
           end if
           up = .false.
           if ( stdlib_lsame( uplo, 'U' ) ) up = .true.
           ! compute norm of op(a)*op2(c).
           anorm = zero
           if ( up ) then
              do i = 1, n
                 tmp = zero
                 if ( capply ) then
                    do j = 1, i
                       tmp = tmp + cabs1( a( j, i ) ) / c( j )
                    end do
                    do j = i+1, n
                       tmp = tmp + cabs1( a( i, j ) ) / c( j )
                    end do
                 else
                    do j = 1, i
                       tmp = tmp + cabs1( a( j, i ) )
                    end do
                    do j = i+1, n
                       tmp = tmp + cabs1( a( i, j ) )
                    end do
                 end if
                 rwork( i ) = tmp
                 anorm = max( anorm, tmp )
              end do
           else
              do i = 1, n
                 tmp = zero
                 if ( capply ) then
                    do j = 1, i
                       tmp = tmp + cabs1( a( i, j ) ) / c( j )
                    end do
                    do j = i+1, n
                       tmp = tmp + cabs1( a( j, i ) ) / c( j )
                    end do
                 else
                    do j = 1, i
                       tmp = tmp + cabs1( a( i, j ) )
                    end do
                    do j = i+1, n
                       tmp = tmp + cabs1( a( j, i ) )
                    end do
                 end if
                 rwork( i ) = tmp
                 anorm = max( anorm, tmp )
              end do
           end if
           ! quick return if possible.
           if( n==0_ilp64 ) then
              stdlib_I64_cla_syrcond_c = one
              return
           else if( anorm == zero ) then
              return
           end if
           ! estimate the norm of inv(op(a)).
           ainvnm = zero
           kase = 0_ilp64
           10 continue
           call stdlib_I64_clacn2( n, work( n+1 ), work, ainvnm, kase, isave )
           if( kase/=0_ilp64 ) then
              if( kase==2_ilp64 ) then
                 ! multiply by r.
                 do i = 1, n
                    work( i ) = work( i ) * rwork( i )
                 end do
                 if ( up ) then
                    call stdlib_I64_csytrs( 'U', n, 1_ilp64, af, ldaf, ipiv,work, n, info )
                 else
                    call stdlib_I64_csytrs( 'L', n, 1_ilp64, af, ldaf, ipiv,work, n, info )
                 endif
                 ! multiply by inv(c).
                 if ( capply ) then
                    do i = 1, n
                       work( i ) = work( i ) * c( i )
                    end do
                 end if
              else
                 ! multiply by inv(c**t).
                 if ( capply ) then
                    do i = 1, n
                       work( i ) = work( i ) * c( i )
                    end do
                 end if
                 if ( up ) then
                    call stdlib_I64_csytrs( 'U', n, 1_ilp64, af, ldaf, ipiv,work, n, info )
                 else
                    call stdlib_I64_csytrs( 'L', n, 1_ilp64, af, ldaf, ipiv,work, n, info )
                 end if
                 ! multiply by r.
                 do i = 1, n
                    work( i ) = work( i ) * rwork( i )
                 end do
              end if
              go to 10
           end if
           ! compute the estimate of the reciprocal condition number.
           if( ainvnm /= zero )stdlib_I64_cla_syrcond_c = one / ainvnm
           return
     end function stdlib_I64_cla_syrcond_c

     real(dp) module function stdlib_I64_zla_syrcond_c( uplo, n, a, lda, af,ldaf, ipiv, c, capply,info, work,&
     !! ZLA_SYRCOND_C Computes the infinity norm condition number of
     !! op(A) * inv(diag(C)) where C is a DOUBLE PRECISION vector.
                rwork )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           logical(lk), intent(in) :: capply
           integer(ilp64), intent(in) :: n, lda, ldaf
           integer(ilp64), intent(out) :: info
           ! Array Arguments 
           integer(ilp64), intent(in) :: ipiv(*)
           complex(dp), intent(in) :: a(lda,*), af(ldaf,*)
           complex(dp), intent(out) :: work(*)
           real(dp), intent(in) :: c(*)
           real(dp), intent(out) :: rwork(*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp64) :: kase
           real(dp) :: ainvnm, anorm, tmp
           integer(ilp64) :: i, j
           logical(lk) :: up, upper
           complex(dp) :: zdum
           ! Local Arrays 
           integer(ilp64) :: isave(3_ilp64)
           ! Intrinsic Functions 
           ! Statement Functions 
           real(dp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=dp) ) + abs( aimag( zdum ) )
           ! Executable Statements 
           stdlib_I64_zla_syrcond_c = zero
           info = 0_ilp64
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -2_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -4_ilp64
           else if( ldaf<max( 1_ilp64, n ) ) then
              info = -6_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'ZLA_SYRCOND_C', -info )
              return
           end if
           up = .false.
           if ( stdlib_lsame( uplo, 'U' ) ) up = .true.
           ! compute norm of op(a)*op2(c).
           anorm = zero
           if ( up ) then
              do i = 1, n
                 tmp = zero
                 if ( capply ) then
                    do j = 1, i
                       tmp = tmp + cabs1( a( j, i ) ) / c( j )
                    end do
                    do j = i+1, n
                       tmp = tmp + cabs1( a( i, j ) ) / c( j )
                    end do
                 else
                    do j = 1, i
                       tmp = tmp + cabs1( a( j, i ) )
                    end do
                    do j = i+1, n
                       tmp = tmp + cabs1( a( i, j ) )
                    end do
                 end if
                 rwork( i ) = tmp
                 anorm = max( anorm, tmp )
              end do
           else
              do i = 1, n
                 tmp = zero
                 if ( capply ) then
                    do j = 1, i
                       tmp = tmp + cabs1( a( i, j ) ) / c( j )
                    end do
                    do j = i+1, n
                       tmp = tmp + cabs1( a( j, i ) ) / c( j )
                    end do
                 else
                    do j = 1, i
                       tmp = tmp + cabs1( a( i, j ) )
                    end do
                    do j = i+1, n
                       tmp = tmp + cabs1( a( j, i ) )
                    end do
                 end if
                 rwork( i ) = tmp
                 anorm = max( anorm, tmp )
              end do
           end if
           ! quick return if possible.
           if( n==0_ilp64 ) then
              stdlib_I64_zla_syrcond_c = one
              return
           else if( anorm == zero ) then
              return
           end if
           ! estimate the norm of inv(op(a)).
           ainvnm = zero
           kase = 0_ilp64
           10 continue
           call stdlib_I64_zlacn2( n, work( n+1 ), work, ainvnm, kase, isave )
           if( kase/=0_ilp64 ) then
              if( kase==2_ilp64 ) then
                 ! multiply by r.
                 do i = 1, n
                    work( i ) = work( i ) * rwork( i )
                 end do
                 if ( up ) then
                    call stdlib_I64_zsytrs( 'U', n, 1_ilp64, af, ldaf, ipiv,work, n, info )
                 else
                    call stdlib_I64_zsytrs( 'L', n, 1_ilp64, af, ldaf, ipiv,work, n, info )
                 endif
                 ! multiply by inv(c).
                 if ( capply ) then
                    do i = 1, n
                       work( i ) = work( i ) * c( i )
                    end do
                 end if
              else
                 ! multiply by inv(c**t).
                 if ( capply ) then
                    do i = 1, n
                       work( i ) = work( i ) * c( i )
                    end do
                 end if
                 if ( up ) then
                    call stdlib_I64_zsytrs( 'U', n, 1_ilp64, af, ldaf, ipiv,work, n, info )
                 else
                    call stdlib_I64_zsytrs( 'L', n, 1_ilp64, af, ldaf, ipiv,work, n, info )
                 end if
                 ! multiply by r.
                 do i = 1, n
                    work( i ) = work( i ) * rwork( i )
                 end do
              end if
              go to 10
           end if
           ! compute the estimate of the reciprocal condition number.
           if( ainvnm /= zero )stdlib_I64_zla_syrcond_c = one / ainvnm
           return
     end function stdlib_I64_zla_syrcond_c




     real(sp) module function stdlib_I64_cla_porcond_c( uplo, n, a, lda, af, ldaf, c, capply,info, work, &
     !! CLA_PORCOND_C Computes the infinity norm condition number of
     !! op(A) * inv(diag(C)) where C is a REAL vector
               rwork )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           logical(lk), intent(in) :: capply
           integer(ilp64), intent(in) :: n, lda, ldaf
           integer(ilp64), intent(out) :: info
           ! Array Arguments 
           complex(sp), intent(in) :: a(lda,*), af(ldaf,*)
           complex(sp), intent(out) :: work(*)
           real(sp), intent(in) :: c(*)
           real(sp), intent(out) :: rwork(*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp64) :: kase
           real(sp) :: ainvnm, anorm, tmp
           integer(ilp64) :: i, j
           logical(lk) :: up, upper
           complex(sp) :: zdum
           ! Local Arrays 
           integer(ilp64) :: isave(3_ilp64)
           ! Intrinsic Functions 
           ! Statement Functions 
           real(sp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=sp) ) + abs( aimag( zdum ) )
           ! Executable Statements 
           stdlib_I64_cla_porcond_c = zero
           info = 0_ilp64
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -2_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -4_ilp64
           else if( ldaf<max( 1_ilp64, n ) ) then
              info = -6_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'CLA_PORCOND_C', -info )
              return
           end if
           up = .false.
           if ( stdlib_lsame( uplo, 'U' ) ) up = .true.
           ! compute norm of op(a)*op2(c).
           anorm = zero
           if ( up ) then
              do i = 1, n
                 tmp = zero
                 if ( capply ) then
                    do j = 1, i
                       tmp = tmp + cabs1( a( j, i ) ) / c( j )
                    end do
                    do j = i+1, n
                       tmp = tmp + cabs1( a( i, j ) ) / c( j )
                    end do
                 else
                    do j = 1, i
                       tmp = tmp + cabs1( a( j, i ) )
                    end do
                    do j = i+1, n
                       tmp = tmp + cabs1( a( i, j ) )
                    end do
                 end if
                 rwork( i ) = tmp
                 anorm = max( anorm, tmp )
              end do
           else
              do i = 1, n
                 tmp = zero
                 if ( capply ) then
                    do j = 1, i
                       tmp = tmp + cabs1( a( i, j ) ) / c( j )
                    end do
                    do j = i+1, n
                       tmp = tmp + cabs1( a( j, i ) ) / c( j )
                    end do
                 else
                    do j = 1, i
                       tmp = tmp + cabs1( a( i, j ) )
                    end do
                    do j = i+1, n
                       tmp = tmp + cabs1( a( j, i ) )
                    end do
                 end if
                 rwork( i ) = tmp
                 anorm = max( anorm, tmp )
              end do
           end if
           ! quick return if possible.
           if( n==0_ilp64 ) then
              stdlib_I64_cla_porcond_c = one
              return
           else if( anorm == zero ) then
              return
           end if
           ! estimate the norm of inv(op(a)).
           ainvnm = zero
           kase = 0_ilp64
           10 continue
           call stdlib_I64_clacn2( n, work( n+1 ), work, ainvnm, kase, isave )
           if( kase/=0_ilp64 ) then
              if( kase==2_ilp64 ) then
                 ! multiply by r.
                 do i = 1, n
                    work( i ) = work( i ) * rwork( i )
                 end do
                 if ( up ) then
                    call stdlib_I64_cpotrs( 'U', n, 1_ilp64, af, ldaf,work, n, info )
                 else
                    call stdlib_I64_cpotrs( 'L', n, 1_ilp64, af, ldaf,work, n, info )
                 endif
                 ! multiply by inv(c).
                 if ( capply ) then
                    do i = 1, n
                       work( i ) = work( i ) * c( i )
                    end do
                 end if
              else
                 ! multiply by inv(c**h).
                 if ( capply ) then
                    do i = 1, n
                       work( i ) = work( i ) * c( i )
                    end do
                 end if
                 if ( up ) then
                    call stdlib_I64_cpotrs( 'U', n, 1_ilp64, af, ldaf,work, n, info )
                 else
                    call stdlib_I64_cpotrs( 'L', n, 1_ilp64, af, ldaf,work, n, info )
                 end if
                 ! multiply by r.
                 do i = 1, n
                    work( i ) = work( i ) * rwork( i )
                 end do
              end if
              go to 10
           end if
           ! compute the estimate of the reciprocal condition number.
           if( ainvnm /= zero )stdlib_I64_cla_porcond_c = one / ainvnm
           return
     end function stdlib_I64_cla_porcond_c

     real(dp) module function stdlib_I64_zla_porcond_c( uplo, n, a, lda, af,ldaf, c, capply, info,work, &
     !! ZLA_PORCOND_C Computes the infinity norm condition number of
     !! op(A) * inv(diag(C)) where C is a DOUBLE PRECISION vector
               rwork )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           logical(lk), intent(in) :: capply
           integer(ilp64), intent(in) :: n, lda, ldaf
           integer(ilp64), intent(out) :: info
           ! Array Arguments 
           complex(dp), intent(in) :: a(lda,*), af(ldaf,*)
           complex(dp), intent(out) :: work(*)
           real(dp), intent(in) :: c(*)
           real(dp), intent(out) :: rwork(*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp64) :: kase
           real(dp) :: ainvnm, anorm, tmp
           integer(ilp64) :: i, j
           logical(lk) :: up, upper
           complex(dp) :: zdum
           ! Local Arrays 
           integer(ilp64) :: isave(3_ilp64)
           ! Intrinsic Functions 
           ! Statement Functions 
           real(dp) :: cabs1
           ! Statement Function Definitions 
           cabs1( zdum ) = abs( real( zdum,KIND=dp) ) + abs( aimag( zdum ) )
           ! Executable Statements 
           stdlib_I64_zla_porcond_c = zero
           info = 0_ilp64
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -2_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -4_ilp64
           else if( ldaf<max( 1_ilp64, n ) ) then
              info = -6_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'ZLA_PORCOND_C', -info )
              return
           end if
           up = .false.
           if ( stdlib_lsame( uplo, 'U' ) ) up = .true.
           ! compute norm of op(a)*op2(c).
           anorm = zero
           if ( up ) then
              do i = 1, n
                 tmp = zero
                 if ( capply ) then
                    do j = 1, i
                       tmp = tmp + cabs1( a( j, i ) ) / c( j )
                    end do
                    do j = i+1, n
                       tmp = tmp + cabs1( a( i, j ) ) / c( j )
                    end do
                 else
                    do j = 1, i
                       tmp = tmp + cabs1( a( j, i ) )
                    end do
                    do j = i+1, n
                       tmp = tmp + cabs1( a( i, j ) )
                    end do
                 end if
                 rwork( i ) = tmp
                 anorm = max( anorm, tmp )
              end do
           else
              do i = 1, n
                 tmp = zero
                 if ( capply ) then
                    do j = 1, i
                       tmp = tmp + cabs1( a( i, j ) ) / c( j )
                    end do
                    do j = i+1, n
                       tmp = tmp + cabs1( a( j, i ) ) / c( j )
                    end do
                 else
                    do j = 1, i
                       tmp = tmp + cabs1( a( i, j ) )
                    end do
                    do j = i+1, n
                       tmp = tmp + cabs1( a( j, i ) )
                    end do
                 end if
                 rwork( i ) = tmp
                 anorm = max( anorm, tmp )
              end do
           end if
           ! quick return if possible.
           if( n==0_ilp64 ) then
              stdlib_I64_zla_porcond_c = one
              return
           else if( anorm == zero ) then
              return
           end if
           ! estimate the norm of inv(op(a)).
           ainvnm = zero
           kase = 0_ilp64
           10 continue
           call stdlib_I64_zlacn2( n, work( n+1 ), work, ainvnm, kase, isave )
           if( kase/=0_ilp64 ) then
              if( kase==2_ilp64 ) then
                 ! multiply by r.
                 do i = 1, n
                    work( i ) = work( i ) * rwork( i )
                 end do
                 if ( up ) then
                    call stdlib_I64_zpotrs( 'U', n, 1_ilp64, af, ldaf,work, n, info )
                 else
                    call stdlib_I64_zpotrs( 'L', n, 1_ilp64, af, ldaf,work, n, info )
                 endif
                 ! multiply by inv(c).
                 if ( capply ) then
                    do i = 1, n
                       work( i ) = work( i ) * c( i )
                    end do
                 end if
              else
                 ! multiply by inv(c**h).
                 if ( capply ) then
                    do i = 1, n
                       work( i ) = work( i ) * c( i )
                    end do
                 end if
                 if ( up ) then
                    call stdlib_I64_zpotrs( 'U', n, 1_ilp64, af, ldaf,work, n, info )
                 else
                    call stdlib_I64_zpotrs( 'L', n, 1_ilp64, af, ldaf,work, n, info )
                 end if
                 ! multiply by r.
                 do i = 1, n
                    work( i ) = work( i ) * rwork( i )
                 end do
              end if
              go to 10
           end if
           ! compute the estimate of the reciprocal condition number.
           if( ainvnm /= zero )stdlib_I64_zla_porcond_c = one / ainvnm
           return
     end function stdlib_I64_zla_porcond_c



end submodule stdlib_lapack_others_sm
