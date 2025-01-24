submodule(stdlib_lapack_base) stdlib_lapack_blas_like_mnorm
  implicit none


  contains

     real(sp) module function stdlib_slange( norm, m, n, a, lda, work )
     !! SLANGE returns the value of the one norm,  or the Frobenius norm, or
     !! the  infinity norm,  or the  element of  largest absolute value  of a
     !! real matrix A.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: norm
           integer(ilp), intent(in) :: lda, m, n
           ! Array Arguments 
           real(sp), intent(in) :: a(lda,*)
           real(sp), intent(out) :: work(*)
       ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j
           real(sp) :: scale, sum, value, temp
           ! Intrinsic Functions 
           ! Executable Statements 
           if( min( m, n )==0_ilp ) then
              value = zero
           else if( stdlib_lsame( norm, 'M' ) ) then
              ! find max(abs(a(i,j))).
              value = zero
              do j = 1, n
                 do i = 1, m
                    temp = abs( a( i, j ) )
                    if( value<temp .or. stdlib_sisnan( temp ) ) value = temp
                 end do
              end do
           else if( ( stdlib_lsame( norm, 'O' ) ) .or. ( norm=='1' ) ) then
              ! find norm1(a).
              value = zero
              do j = 1, n
                 sum = zero
                 do i = 1, m
                    sum = sum + abs( a( i, j ) )
                 end do
                 if( value<sum .or. stdlib_sisnan( sum ) ) value = sum
              end do
           else if( stdlib_lsame( norm, 'I' ) ) then
              ! find normi(a).
              do i = 1, m
                 work( i ) = zero
              end do
              do j = 1, n
                 do i = 1, m
                    work( i ) = work( i ) + abs( a( i, j ) )
                 end do
              end do
              value = zero
              do i = 1, m
                 temp = work( i )
                 if( value<temp .or. stdlib_sisnan( temp ) ) value = temp
              end do
           else if( ( stdlib_lsame( norm, 'F' ) ) .or. ( stdlib_lsame( norm, 'E' ) ) ) &
                     then
              ! find normf(a).
              scale = zero
              sum = one
              do j = 1, n
                 call stdlib_slassq( m, a( 1_ilp, j ), 1_ilp, scale, sum )
              end do
              value = scale*sqrt( sum )
           end if
           stdlib_slange = value
           return
     end function stdlib_slange

     real(dp) module function stdlib_dlange( norm, m, n, a, lda, work )
     !! DLANGE returns the value of the one norm,  or the Frobenius norm, or
     !! the  infinity norm,  or the  element of  largest absolute value  of a
     !! real matrix A.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: norm
           integer(ilp), intent(in) :: lda, m, n
           ! Array Arguments 
           real(dp), intent(in) :: a(lda,*)
           real(dp), intent(out) :: work(*)
       ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j
           real(dp) :: scale, sum, value, temp
           ! Intrinsic Functions 
           ! Executable Statements 
           if( min( m, n )==0_ilp ) then
              value = zero
           else if( stdlib_lsame( norm, 'M' ) ) then
              ! find max(abs(a(i,j))).
              value = zero
              do j = 1, n
                 do i = 1, m
                    temp = abs( a( i, j ) )
                    if( value<temp .or. stdlib_disnan( temp ) ) value = temp
                 end do
              end do
           else if( ( stdlib_lsame( norm, 'O' ) ) .or. ( norm=='1' ) ) then
              ! find norm1(a).
              value = zero
              do j = 1, n
                 sum = zero
                 do i = 1, m
                    sum = sum + abs( a( i, j ) )
                 end do
                 if( value<sum .or. stdlib_disnan( sum ) ) value = sum
              end do
           else if( stdlib_lsame( norm, 'I' ) ) then
              ! find normi(a).
              do i = 1, m
                 work( i ) = zero
              end do
              do j = 1, n
                 do i = 1, m
                    work( i ) = work( i ) + abs( a( i, j ) )
                 end do
              end do
              value = zero
              do i = 1, m
                 temp = work( i )
                 if( value<temp .or. stdlib_disnan( temp ) ) value = temp
              end do
           else if( ( stdlib_lsame( norm, 'F' ) ) .or. ( stdlib_lsame( norm, 'E' ) ) ) &
                     then
              ! find normf(a).
              scale = zero
              sum = one
              do j = 1, n
                 call stdlib_dlassq( m, a( 1_ilp, j ), 1_ilp, scale, sum )
              end do
              value = scale*sqrt( sum )
           end if
           stdlib_dlange = value
           return
     end function stdlib_dlange


     real(sp) module function stdlib_clange( norm, m, n, a, lda, work )
     !! CLANGE returns the value of the one norm,  or the Frobenius norm, or
     !! the  infinity norm,  or the  element of  largest absolute value  of a
     !! complex matrix A.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: norm
           integer(ilp), intent(in) :: lda, m, n
           ! Array Arguments 
           real(sp), intent(out) :: work(*)
           complex(sp), intent(in) :: a(lda,*)
       ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j
           real(sp) :: scale, sum, value, temp
           ! Intrinsic Functions 
           ! Executable Statements 
           if( min( m, n )==0_ilp ) then
              value = zero
           else if( stdlib_lsame( norm, 'M' ) ) then
              ! find max(abs(a(i,j))).
              value = zero
              do j = 1, n
                 do i = 1, m
                    temp = abs( a( i, j ) )
                    if( value<temp .or. stdlib_sisnan( temp ) ) value = temp
                 end do
              end do
           else if( ( stdlib_lsame( norm, 'O' ) ) .or. ( norm=='1' ) ) then
              ! find norm1(a).
              value = zero
              do j = 1, n
                 sum = zero
                 do i = 1, m
                    sum = sum + abs( a( i, j ) )
                 end do
                 if( value<sum .or. stdlib_sisnan( sum ) ) value = sum
              end do
           else if( stdlib_lsame( norm, 'I' ) ) then
              ! find normi(a).
              do i = 1, m
                 work( i ) = zero
              end do
              do j = 1, n
                 do i = 1, m
                    work( i ) = work( i ) + abs( a( i, j ) )
                 end do
              end do
              value = zero
              do i = 1, m
                 temp = work( i )
                 if( value<temp .or. stdlib_sisnan( temp ) ) value = temp
              end do
           else if( ( stdlib_lsame( norm, 'F' ) ) .or. ( stdlib_lsame( norm, 'E' ) ) ) &
                     then
              ! find normf(a).
              scale = zero
              sum = one
              do j = 1, n
                 call stdlib_classq( m, a( 1_ilp, j ), 1_ilp, scale, sum )
              end do
              value = scale*sqrt( sum )
           end if
           stdlib_clange = value
           return
     end function stdlib_clange

     real(dp) module function stdlib_zlange( norm, m, n, a, lda, work )
     !! ZLANGE returns the value of the one norm,  or the Frobenius norm, or
     !! the  infinity norm,  or the  element of  largest absolute value  of a
     !! complex matrix A.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: norm
           integer(ilp), intent(in) :: lda, m, n
           ! Array Arguments 
           real(dp), intent(out) :: work(*)
           complex(dp), intent(in) :: a(lda,*)
       ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j
           real(dp) :: scale, sum, value, temp
           ! Intrinsic Functions 
           ! Executable Statements 
           if( min( m, n )==0_ilp ) then
              value = zero
           else if( stdlib_lsame( norm, 'M' ) ) then
              ! find max(abs(a(i,j))).
              value = zero
              do j = 1, n
                 do i = 1, m
                    temp = abs( a( i, j ) )
                    if( value<temp .or. stdlib_disnan( temp ) ) value = temp
                 end do
              end do
           else if( ( stdlib_lsame( norm, 'O' ) ) .or. ( norm=='1' ) ) then
              ! find norm1(a).
              value = zero
              do j = 1, n
                 sum = zero
                 do i = 1, m
                    sum = sum + abs( a( i, j ) )
                 end do
                 if( value<sum .or. stdlib_disnan( sum ) ) value = sum
              end do
           else if( stdlib_lsame( norm, 'I' ) ) then
              ! find normi(a).
              do i = 1, m
                 work( i ) = zero
              end do
              do j = 1, n
                 do i = 1, m
                    work( i ) = work( i ) + abs( a( i, j ) )
                 end do
              end do
              value = zero
              do i = 1, m
                 temp = work( i )
                 if( value<temp .or. stdlib_disnan( temp ) ) value = temp
              end do
           else if( ( stdlib_lsame( norm, 'F' ) ) .or. ( stdlib_lsame( norm, 'E' ) ) ) &
                     then
              ! find normf(a).
              scale = zero
              sum = one
              do j = 1, n
                 call stdlib_zlassq( m, a( 1_ilp, j ), 1_ilp, scale, sum )
              end do
              value = scale*sqrt( sum )
           end if
           stdlib_zlange = value
           return
     end function stdlib_zlange




     real(sp) module function stdlib_slangb( norm, n, kl, ku, ab, ldab,work )
     !! SLANGB returns the value of the one norm,  or the Frobenius norm, or
     !! the  infinity norm,  or the element of  largest absolute value  of an
     !! n by n band matrix  A,  with kl sub-diagonals and ku super-diagonals.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: norm
           integer(ilp), intent(in) :: kl, ku, ldab, n
           ! Array Arguments 
           real(sp), intent(in) :: ab(ldab,*)
           real(sp), intent(out) :: work(*)
       ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j, k, l
           real(sp) :: scale, sum, value, temp
           ! Intrinsic Functions 
           ! Executable Statements 
           if( n==0_ilp ) then
              value = zero
           else if( stdlib_lsame( norm, 'M' ) ) then
              ! find max(abs(a(i,j))).
              value = zero
              do j = 1, n
                 do i = max( ku+2-j, 1 ), min( n+ku+1-j, kl+ku+1 )
                    temp = abs( ab( i, j ) )
                    if( value<temp .or. stdlib_sisnan( temp ) ) value = temp
                 end do
              end do
           else if( ( stdlib_lsame( norm, 'O' ) ) .or. ( norm=='1' ) ) then
              ! find norm1(a).
              value = zero
              do j = 1, n
                 sum = zero
                 do i = max( ku+2-j, 1 ), min( n+ku+1-j, kl+ku+1 )
                    sum = sum + abs( ab( i, j ) )
                 end do
                 if( value<sum .or. stdlib_sisnan( sum ) ) value = sum
              end do
           else if( stdlib_lsame( norm, 'I' ) ) then
              ! find normi(a).
              do i = 1, n
                 work( i ) = zero
              end do
              do j = 1, n
                 k = ku + 1_ilp - j
                 do i = max( 1, j-ku ), min( n, j+kl )
                    work( i ) = work( i ) + abs( ab( k+i, j ) )
                 end do
              end do
              value = zero
              do i = 1, n
                 temp = work( i )
                 if( value<temp .or. stdlib_sisnan( temp ) ) value = temp
              end do
           else if( ( stdlib_lsame( norm, 'F' ) ) .or. ( stdlib_lsame( norm, 'E' ) ) ) &
                     then
              ! find normf(a).
              scale = zero
              sum = one
              do j = 1, n
                 l = max( 1_ilp, j-ku )
                 k = ku + 1_ilp - j + l
                 call stdlib_slassq( min( n, j+kl )-l+1, ab( k, j ), 1_ilp, scale, sum )
              end do
              value = scale*sqrt( sum )
           end if
           stdlib_slangb = value
           return
     end function stdlib_slangb

     real(dp) module function stdlib_dlangb( norm, n, kl, ku, ab, ldab,work )
     !! DLANGB returns the value of the one norm,  or the Frobenius norm, or
     !! the  infinity norm,  or the element of  largest absolute value  of an
     !! n by n band matrix  A,  with kl sub-diagonals and ku super-diagonals.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: norm
           integer(ilp), intent(in) :: kl, ku, ldab, n
           ! Array Arguments 
           real(dp), intent(in) :: ab(ldab,*)
           real(dp), intent(out) :: work(*)
       ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j, k, l
           real(dp) :: scale, sum, value, temp
           ! Intrinsic Functions 
           ! Executable Statements 
           if( n==0_ilp ) then
              value = zero
           else if( stdlib_lsame( norm, 'M' ) ) then
              ! find max(abs(a(i,j))).
              value = zero
              do j = 1, n
                 do i = max( ku+2-j, 1 ), min( n+ku+1-j, kl+ku+1 )
                    temp = abs( ab( i, j ) )
                    if( value<temp .or. stdlib_disnan( temp ) ) value = temp
                 end do
              end do
           else if( ( stdlib_lsame( norm, 'O' ) ) .or. ( norm=='1' ) ) then
              ! find norm1(a).
              value = zero
              do j = 1, n
                 sum = zero
                 do i = max( ku+2-j, 1 ), min( n+ku+1-j, kl+ku+1 )
                    sum = sum + abs( ab( i, j ) )
                 end do
                 if( value<sum .or. stdlib_disnan( sum ) ) value = sum
              end do
           else if( stdlib_lsame( norm, 'I' ) ) then
              ! find normi(a).
              do i = 1, n
                 work( i ) = zero
              end do
              do j = 1, n
                 k = ku + 1_ilp - j
                 do i = max( 1, j-ku ), min( n, j+kl )
                    work( i ) = work( i ) + abs( ab( k+i, j ) )
                 end do
              end do
              value = zero
              do i = 1, n
                 temp = work( i )
                 if( value<temp .or. stdlib_disnan( temp ) ) value = temp
              end do
           else if( ( stdlib_lsame( norm, 'F' ) ) .or. ( stdlib_lsame( norm, 'E' ) ) ) &
                     then
              ! find normf(a).
              scale = zero
              sum = one
              do j = 1, n
                 l = max( 1_ilp, j-ku )
                 k = ku + 1_ilp - j + l
                 call stdlib_dlassq( min( n, j+kl )-l+1, ab( k, j ), 1_ilp, scale, sum )
              end do
              value = scale*sqrt( sum )
           end if
           stdlib_dlangb = value
           return
     end function stdlib_dlangb


     real(sp) module function stdlib_clangb( norm, n, kl, ku, ab, ldab,work )
     !! CLANGB returns the value of the one norm,  or the Frobenius norm, or
     !! the  infinity norm,  or the element of  largest absolute value  of an
     !! n by n band matrix  A,  with kl sub-diagonals and ku super-diagonals.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: norm
           integer(ilp), intent(in) :: kl, ku, ldab, n
           ! Array Arguments 
           real(sp), intent(out) :: work(*)
           complex(sp), intent(in) :: ab(ldab,*)
       ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j, k, l
           real(sp) :: scale, sum, value, temp
           ! Intrinsic Functions 
           ! Executable Statements 
           if( n==0_ilp ) then
              value = zero
           else if( stdlib_lsame( norm, 'M' ) ) then
              ! find max(abs(a(i,j))).
              value = zero
              do j = 1, n
                 do i = max( ku+2-j, 1 ), min( n+ku+1-j, kl+ku+1 )
                    temp = abs( ab( i, j ) )
                    if( value<temp .or. stdlib_sisnan( temp ) ) value = temp
                 end do
              end do
           else if( ( stdlib_lsame( norm, 'O' ) ) .or. ( norm=='1' ) ) then
              ! find norm1(a).
              value = zero
              do j = 1, n
                 sum = zero
                 do i = max( ku+2-j, 1 ), min( n+ku+1-j, kl+ku+1 )
                    sum = sum + abs( ab( i, j ) )
                 end do
                 if( value<sum .or. stdlib_sisnan( sum ) ) value = sum
              end do
           else if( stdlib_lsame( norm, 'I' ) ) then
              ! find normi(a).
              do i = 1, n
                 work( i ) = zero
              end do
              do j = 1, n
                 k = ku + 1_ilp - j
                 do i = max( 1, j-ku ), min( n, j+kl )
                    work( i ) = work( i ) + abs( ab( k+i, j ) )
                 end do
              end do
              value = zero
              do i = 1, n
                 temp = work( i )
                 if( value<temp .or. stdlib_sisnan( temp ) ) value = temp
              end do
           else if( ( stdlib_lsame( norm, 'F' ) ) .or. ( stdlib_lsame( norm, 'E' ) ) ) &
                     then
              ! find normf(a).
              scale = zero
              sum = one
              do j = 1, n
                 l = max( 1_ilp, j-ku )
                 k = ku + 1_ilp - j + l
                 call stdlib_classq( min( n, j+kl )-l+1, ab( k, j ), 1_ilp, scale, sum )
              end do
              value = scale*sqrt( sum )
           end if
           stdlib_clangb = value
           return
     end function stdlib_clangb

     real(dp) module function stdlib_zlangb( norm, n, kl, ku, ab, ldab,work )
     !! ZLANGB returns the value of the one norm,  or the Frobenius norm, or
     !! the  infinity norm,  or the element of  largest absolute value  of an
     !! n by n band matrix  A,  with kl sub-diagonals and ku super-diagonals.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: norm
           integer(ilp), intent(in) :: kl, ku, ldab, n
           ! Array Arguments 
           real(dp), intent(out) :: work(*)
           complex(dp), intent(in) :: ab(ldab,*)
       ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j, k, l
           real(dp) :: scale, sum, value, temp
           ! Intrinsic Functions 
           ! Executable Statements 
           if( n==0_ilp ) then
              value = zero
           else if( stdlib_lsame( norm, 'M' ) ) then
              ! find max(abs(a(i,j))).
              value = zero
              do j = 1, n
                 do i = max( ku+2-j, 1 ), min( n+ku+1-j, kl+ku+1 )
                    temp = abs( ab( i, j ) )
                    if( value<temp .or. stdlib_disnan( temp ) ) value = temp
                 end do
              end do
           else if( ( stdlib_lsame( norm, 'O' ) ) .or. ( norm=='1' ) ) then
              ! find norm1(a).
              value = zero
              do j = 1, n
                 sum = zero
                 do i = max( ku+2-j, 1 ), min( n+ku+1-j, kl+ku+1 )
                    sum = sum + abs( ab( i, j ) )
                 end do
                 if( value<sum .or. stdlib_disnan( sum ) ) value = sum
              end do
           else if( stdlib_lsame( norm, 'I' ) ) then
              ! find normi(a).
              do i = 1, n
                 work( i ) = zero
              end do
              do j = 1, n
                 k = ku + 1_ilp - j
                 do i = max( 1, j-ku ), min( n, j+kl )
                    work( i ) = work( i ) + abs( ab( k+i, j ) )
                 end do
              end do
              value = zero
              do i = 1, n
                 temp = work( i )
                 if( value<temp .or. stdlib_disnan( temp ) ) value = temp
              end do
           else if( ( stdlib_lsame( norm, 'F' ) ) .or. ( stdlib_lsame( norm, 'E' ) ) ) &
                     then
              ! find normf(a).
              scale = zero
              sum = one
              do j = 1, n
                 l = max( 1_ilp, j-ku )
                 k = ku + 1_ilp - j + l
                 call stdlib_zlassq( min( n, j+kl )-l+1, ab( k, j ), 1_ilp, scale, sum )
              end do
              value = scale*sqrt( sum )
           end if
           stdlib_zlangb = value
           return
     end function stdlib_zlangb




     pure real(sp) module function stdlib_slangt( norm, n, dl, d, du )
     !! SLANGT returns the value of the one norm,  or the Frobenius norm, or
     !! the  infinity norm,  or the  element of  largest absolute value  of a
     !! real tridiagonal matrix A.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: norm
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           real(sp), intent(in) :: d(*), dl(*), du(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i
           real(sp) :: anorm, scale, sum, temp
           ! Intrinsic Functions 
           ! Executable Statements 
           if( n<=0_ilp ) then
              anorm = zero
           else if( stdlib_lsame( norm, 'M' ) ) then
              ! find max(abs(a(i,j))).
              anorm = abs( d( n ) )
              do i = 1, n - 1
                 if( anorm<abs( dl( i ) ) .or. stdlib_sisnan( abs( dl( i ) ) ) )anorm = abs(dl(i))
                           
                 if( anorm<abs( d( i ) ) .or. stdlib_sisnan( abs( d( i ) ) ) )anorm = abs(d(i))
                           
                 if( anorm<abs( du( i ) ) .or. stdlib_sisnan (abs( du( i ) ) ) )anorm = abs(du(i))
                           
              end do
           else if( stdlib_lsame( norm, 'O' ) .or. norm=='1' ) then
              ! find norm1(a).
              if( n==1_ilp ) then
                 anorm = abs( d( 1_ilp ) )
              else
                 anorm = abs( d( 1_ilp ) )+abs( dl( 1_ilp ) )
                 temp = abs( d( n ) )+abs( du( n-1 ) )
                 if( anorm < temp .or. stdlib_sisnan( temp ) ) anorm = temp
                 do i = 2, n - 1
                    temp = abs( d( i ) )+abs( dl( i ) )+abs( du( i-1 ) )
                    if( anorm < temp .or. stdlib_sisnan( temp ) ) anorm = temp
                 end do
              end if
           else if( stdlib_lsame( norm, 'I' ) ) then
              ! find normi(a).
              if( n==1_ilp ) then
                 anorm = abs( d( 1_ilp ) )
              else
                 anorm = abs( d( 1_ilp ) )+abs( du( 1_ilp ) )
                 temp = abs( d( n ) )+abs( dl( n-1 ) )
                 if( anorm < temp .or. stdlib_sisnan( temp ) ) anorm = temp
                 do i = 2, n - 1
                    temp = abs( d( i ) )+abs( du( i ) )+abs( dl( i-1 ) )
                    if( anorm < temp .or. stdlib_sisnan( temp ) ) anorm = temp
                 end do
              end if
           else if( ( stdlib_lsame( norm, 'F' ) ) .or. ( stdlib_lsame( norm, 'E' ) ) ) &
                     then
              ! find normf(a).
              scale = zero
              sum = one
              call stdlib_slassq( n, d, 1_ilp, scale, sum )
              if( n>1_ilp ) then
                 call stdlib_slassq( n-1, dl, 1_ilp, scale, sum )
                 call stdlib_slassq( n-1, du, 1_ilp, scale, sum )
              end if
              anorm = scale*sqrt( sum )
           end if
           stdlib_slangt = anorm
           return
     end function stdlib_slangt

     pure real(dp) module function stdlib_dlangt( norm, n, dl, d, du )
     !! DLANGT returns the value of the one norm,  or the Frobenius norm, or
     !! the  infinity norm,  or the  element of  largest absolute value  of a
     !! real tridiagonal matrix A.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: norm
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           real(dp), intent(in) :: d(*), dl(*), du(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i
           real(dp) :: anorm, scale, sum, temp
           ! Intrinsic Functions 
           ! Executable Statements 
           if( n<=0_ilp ) then
              anorm = zero
           else if( stdlib_lsame( norm, 'M' ) ) then
              ! find max(abs(a(i,j))).
              anorm = abs( d( n ) )
              do i = 1, n - 1
                 if( anorm<abs( dl( i ) ) .or. stdlib_disnan( abs( dl( i ) ) ) )anorm = abs(dl(i))
                           
                 if( anorm<abs( d( i ) ) .or. stdlib_disnan( abs( d( i ) ) ) )anorm = abs(d(i))
                           
                 if( anorm<abs( du( i ) ) .or. stdlib_disnan (abs( du( i ) ) ) )anorm = abs(du(i))
                           
              end do
           else if( stdlib_lsame( norm, 'O' ) .or. norm=='1' ) then
              ! find norm1(a).
              if( n==1_ilp ) then
                 anorm = abs( d( 1_ilp ) )
              else
                 anorm = abs( d( 1_ilp ) )+abs( dl( 1_ilp ) )
                 temp = abs( d( n ) )+abs( du( n-1 ) )
                 if( anorm < temp .or. stdlib_disnan( temp ) ) anorm = temp
                 do i = 2, n - 1
                    temp = abs( d( i ) )+abs( dl( i ) )+abs( du( i-1 ) )
                    if( anorm < temp .or. stdlib_disnan( temp ) ) anorm = temp
                 end do
              end if
           else if( stdlib_lsame( norm, 'I' ) ) then
              ! find normi(a).
              if( n==1_ilp ) then
                 anorm = abs( d( 1_ilp ) )
              else
                 anorm = abs( d( 1_ilp ) )+abs( du( 1_ilp ) )
                 temp = abs( d( n ) )+abs( dl( n-1 ) )
                 if( anorm < temp .or. stdlib_disnan( temp ) ) anorm = temp
                 do i = 2, n - 1
                    temp = abs( d( i ) )+abs( du( i ) )+abs( dl( i-1 ) )
                    if( anorm < temp .or. stdlib_disnan( temp ) ) anorm = temp
                 end do
              end if
           else if( ( stdlib_lsame( norm, 'F' ) ) .or. ( stdlib_lsame( norm, 'E' ) ) ) &
                     then
              ! find normf(a).
              scale = zero
              sum = one
              call stdlib_dlassq( n, d, 1_ilp, scale, sum )
              if( n>1_ilp ) then
                 call stdlib_dlassq( n-1, dl, 1_ilp, scale, sum )
                 call stdlib_dlassq( n-1, du, 1_ilp, scale, sum )
              end if
              anorm = scale*sqrt( sum )
           end if
           stdlib_dlangt = anorm
           return
     end function stdlib_dlangt


     pure real(sp) module function stdlib_clangt( norm, n, dl, d, du )
     !! CLANGT returns the value of the one norm,  or the Frobenius norm, or
     !! the  infinity norm,  or the  element of  largest absolute value  of a
     !! complex tridiagonal matrix A.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: norm
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           complex(sp), intent(in) :: d(*), dl(*), du(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i
           real(sp) :: anorm, scale, sum, temp
           ! Intrinsic Functions 
           ! Executable Statements 
           if( n<=0_ilp ) then
              anorm = zero
           else if( stdlib_lsame( norm, 'M' ) ) then
              ! find max(abs(a(i,j))).
              anorm = abs( d( n ) )
              do i = 1, n - 1
                 if( anorm<abs( dl( i ) ) .or. stdlib_sisnan( abs( dl( i ) ) ) )anorm = abs(dl(i))
                           
                 if( anorm<abs( d( i ) ) .or. stdlib_sisnan( abs( d( i ) ) ) )anorm = abs(d(i))
                           
                 if( anorm<abs( du( i ) ) .or. stdlib_sisnan (abs( du( i ) ) ) )anorm = abs(du(i))
                           
              end do
           else if( stdlib_lsame( norm, 'O' ) .or. norm=='1' ) then
              ! find norm1(a).
              if( n==1_ilp ) then
                 anorm = abs( d( 1_ilp ) )
              else
                 anorm = abs( d( 1_ilp ) )+abs( dl( 1_ilp ) )
                 temp = abs( d( n ) )+abs( du( n-1 ) )
                 if( anorm < temp .or. stdlib_sisnan( temp ) ) anorm = temp
                 do i = 2, n - 1
                    temp = abs( d( i ) )+abs( dl( i ) )+abs( du( i-1 ) )
                    if( anorm < temp .or. stdlib_sisnan( temp ) ) anorm = temp
                 end do
              end if
           else if( stdlib_lsame( norm, 'I' ) ) then
              ! find normi(a).
              if( n==1_ilp ) then
                 anorm = abs( d( 1_ilp ) )
              else
                 anorm = abs( d( 1_ilp ) )+abs( du( 1_ilp ) )
                 temp = abs( d( n ) )+abs( dl( n-1 ) )
                 if( anorm < temp .or. stdlib_sisnan( temp ) ) anorm = temp
                 do i = 2, n - 1
                    temp = abs( d( i ) )+abs( du( i ) )+abs( dl( i-1 ) )
                    if( anorm < temp .or. stdlib_sisnan( temp ) ) anorm = temp
                 end do
              end if
           else if( ( stdlib_lsame( norm, 'F' ) ) .or. ( stdlib_lsame( norm, 'E' ) ) ) &
                     then
              ! find normf(a).
              scale = zero
              sum = one
              call stdlib_classq( n, d, 1_ilp, scale, sum )
              if( n>1_ilp ) then
                 call stdlib_classq( n-1, dl, 1_ilp, scale, sum )
                 call stdlib_classq( n-1, du, 1_ilp, scale, sum )
              end if
              anorm = scale*sqrt( sum )
           end if
           stdlib_clangt = anorm
           return
     end function stdlib_clangt

     pure real(dp) module function stdlib_zlangt( norm, n, dl, d, du )
     !! ZLANGT returns the value of the one norm,  or the Frobenius norm, or
     !! the  infinity norm,  or the  element of  largest absolute value  of a
     !! complex tridiagonal matrix A.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: norm
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           complex(dp), intent(in) :: d(*), dl(*), du(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i
           real(dp) :: anorm, scale, sum, temp
           ! Intrinsic Functions 
           ! Executable Statements 
           if( n<=0_ilp ) then
              anorm = zero
           else if( stdlib_lsame( norm, 'M' ) ) then
              ! find max(abs(a(i,j))).
              anorm = abs( d( n ) )
              do i = 1, n - 1
                 if( anorm<abs( dl( i ) ) .or. stdlib_disnan( abs( dl( i ) ) ) )anorm = abs(dl(i))
                           
                 if( anorm<abs( d( i ) ) .or. stdlib_disnan( abs( d( i ) ) ) )anorm = abs(d(i))
                           
                 if( anorm<abs( du( i ) ) .or. stdlib_disnan (abs( du( i ) ) ) )anorm = abs(du(i))
                           
              end do
           else if( stdlib_lsame( norm, 'O' ) .or. norm=='1' ) then
              ! find norm1(a).
              if( n==1_ilp ) then
                 anorm = abs( d( 1_ilp ) )
              else
                 anorm = abs( d( 1_ilp ) )+abs( dl( 1_ilp ) )
                 temp = abs( d( n ) )+abs( du( n-1 ) )
                 if( anorm < temp .or. stdlib_disnan( temp ) ) anorm = temp
                 do i = 2, n - 1
                    temp = abs( d( i ) )+abs( dl( i ) )+abs( du( i-1 ) )
                    if( anorm < temp .or. stdlib_disnan( temp ) ) anorm = temp
                 end do
              end if
           else if( stdlib_lsame( norm, 'I' ) ) then
              ! find normi(a).
              if( n==1_ilp ) then
                 anorm = abs( d( 1_ilp ) )
              else
                 anorm = abs( d( 1_ilp ) )+abs( du( 1_ilp ) )
                 temp = abs( d( n ) )+abs( dl( n-1 ) )
                 if( anorm < temp .or. stdlib_disnan( temp ) ) anorm = temp
                 do i = 2, n - 1
                    temp = abs( d( i ) )+abs( du( i ) )+abs( dl( i-1 ) )
                    if( anorm < temp .or. stdlib_disnan( temp ) ) anorm = temp
                 end do
              end if
           else if( ( stdlib_lsame( norm, 'F' ) ) .or. ( stdlib_lsame( norm, 'E' ) ) ) &
                     then
              ! find normf(a).
              scale = zero
              sum = one
              call stdlib_zlassq( n, d, 1_ilp, scale, sum )
              if( n>1_ilp ) then
                 call stdlib_zlassq( n-1, dl, 1_ilp, scale, sum )
                 call stdlib_zlassq( n-1, du, 1_ilp, scale, sum )
              end if
              anorm = scale*sqrt( sum )
           end if
           stdlib_zlangt = anorm
           return
     end function stdlib_zlangt




     real(sp) module function stdlib_slanhs( norm, n, a, lda, work )
     !! SLANHS returns the value of the one norm,  or the Frobenius norm, or
     !! the  infinity norm,  or the  element of  largest absolute value  of a
     !! Hessenberg matrix A.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: norm
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           real(sp), intent(in) :: a(lda,*)
           real(sp), intent(out) :: work(*)
       ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j
           real(sp) :: scale, sum, value
           ! Intrinsic Functions 
           ! Executable Statements 
           if( n==0_ilp ) then
              value = zero
           else if( stdlib_lsame( norm, 'M' ) ) then
              ! find max(abs(a(i,j))).
              value = zero
              do j = 1, n
                 do i = 1, min( n, j+1 )
                    sum = abs( a( i, j ) )
                    if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
                 end do
              end do
           else if( ( stdlib_lsame( norm, 'O' ) ) .or. ( norm=='1' ) ) then
              ! find norm1(a).
              value = zero
              do j = 1, n
                 sum = zero
                 do i = 1, min( n, j+1 )
                    sum = sum + abs( a( i, j ) )
                 end do
                 if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
              end do
           else if( stdlib_lsame( norm, 'I' ) ) then
              ! find normi(a).
              do i = 1, n
                 work( i ) = zero
              end do
              do j = 1, n
                 do i = 1, min( n, j+1 )
                    work( i ) = work( i ) + abs( a( i, j ) )
                 end do
              end do
              value = zero
              do i = 1, n
                 sum = work( i )
                 if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
              end do
           else if( ( stdlib_lsame( norm, 'F' ) ) .or. ( stdlib_lsame( norm, 'E' ) ) ) &
                     then
              ! find normf(a).
              scale = zero
              sum = one
              do j = 1, n
                 call stdlib_slassq( min( n, j+1 ), a( 1_ilp, j ), 1_ilp, scale, sum )
              end do
              value = scale*sqrt( sum )
           end if
           stdlib_slanhs = value
           return
     end function stdlib_slanhs

     real(dp) module function stdlib_dlanhs( norm, n, a, lda, work )
     !! DLANHS returns the value of the one norm,  or the Frobenius norm, or
     !! the  infinity norm,  or the  element of  largest absolute value  of a
     !! Hessenberg matrix A.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: norm
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           real(dp), intent(in) :: a(lda,*)
           real(dp), intent(out) :: work(*)
       ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j
           real(dp) :: scale, sum, value
           ! Intrinsic Functions 
           ! Executable Statements 
           if( n==0_ilp ) then
              value = zero
           else if( stdlib_lsame( norm, 'M' ) ) then
              ! find max(abs(a(i,j))).
              value = zero
              do j = 1, n
                 do i = 1, min( n, j+1 )
                    sum = abs( a( i, j ) )
                    if( value < sum .or. stdlib_disnan( sum ) ) value = sum
                 end do
              end do
           else if( ( stdlib_lsame( norm, 'O' ) ) .or. ( norm=='1' ) ) then
              ! find norm1(a).
              value = zero
              do j = 1, n
                 sum = zero
                 do i = 1, min( n, j+1 )
                    sum = sum + abs( a( i, j ) )
                 end do
                 if( value < sum .or. stdlib_disnan( sum ) ) value = sum
              end do
           else if( stdlib_lsame( norm, 'I' ) ) then
              ! find normi(a).
              do i = 1, n
                 work( i ) = zero
              end do
              do j = 1, n
                 do i = 1, min( n, j+1 )
                    work( i ) = work( i ) + abs( a( i, j ) )
                 end do
              end do
              value = zero
              do i = 1, n
                 sum = work( i )
                 if( value < sum .or. stdlib_disnan( sum ) ) value = sum
              end do
           else if( ( stdlib_lsame( norm, 'F' ) ) .or. ( stdlib_lsame( norm, 'E' ) ) ) &
                     then
              ! find normf(a).
              scale = zero
              sum = one
              do j = 1, n
                 call stdlib_dlassq( min( n, j+1 ), a( 1_ilp, j ), 1_ilp, scale, sum )
              end do
              value = scale*sqrt( sum )
           end if
           stdlib_dlanhs = value
           return
     end function stdlib_dlanhs


     real(sp) module function stdlib_clanhs( norm, n, a, lda, work )
     !! CLANHS returns the value of the one norm,  or the Frobenius norm, or
     !! the  infinity norm,  or the  element of  largest absolute value  of a
     !! Hessenberg matrix A.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: norm
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           real(sp), intent(out) :: work(*)
           complex(sp), intent(in) :: a(lda,*)
       ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j
           real(sp) :: scale, sum, value
           ! Intrinsic Functions 
           ! Executable Statements 
           if( n==0_ilp ) then
              value = zero
           else if( stdlib_lsame( norm, 'M' ) ) then
              ! find max(abs(a(i,j))).
              value = zero
              do j = 1, n
                 do i = 1, min( n, j+1 )
                    sum = abs( a( i, j ) )
                    if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
                 end do
              end do
           else if( ( stdlib_lsame( norm, 'O' ) ) .or. ( norm=='1' ) ) then
              ! find norm1(a).
              value = zero
              do j = 1, n
                 sum = zero
                 do i = 1, min( n, j+1 )
                    sum = sum + abs( a( i, j ) )
                 end do
                 if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
              end do
           else if( stdlib_lsame( norm, 'I' ) ) then
              ! find normi(a).
              do i = 1, n
                 work( i ) = zero
              end do
              do j = 1, n
                 do i = 1, min( n, j+1 )
                    work( i ) = work( i ) + abs( a( i, j ) )
                 end do
              end do
              value = zero
              do i = 1, n
                 sum = work( i )
                 if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
              end do
           else if( ( stdlib_lsame( norm, 'F' ) ) .or. ( stdlib_lsame( norm, 'E' ) ) ) &
                     then
              ! find normf(a).
              scale = zero
              sum = one
              do j = 1, n
                 call stdlib_classq( min( n, j+1 ), a( 1_ilp, j ), 1_ilp, scale, sum )
              end do
              value = scale*sqrt( sum )
           end if
           stdlib_clanhs = value
           return
     end function stdlib_clanhs

     real(dp) module function stdlib_zlanhs( norm, n, a, lda, work )
     !! ZLANHS returns the value of the one norm,  or the Frobenius norm, or
     !! the  infinity norm,  or the  element of  largest absolute value  of a
     !! Hessenberg matrix A.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: norm
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           real(dp), intent(out) :: work(*)
           complex(dp), intent(in) :: a(lda,*)
       ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j
           real(dp) :: scale, sum, value
           ! Intrinsic Functions 
           ! Executable Statements 
           if( n==0_ilp ) then
              value = zero
           else if( stdlib_lsame( norm, 'M' ) ) then
              ! find max(abs(a(i,j))).
              value = zero
              do j = 1, n
                 do i = 1, min( n, j+1 )
                    sum = abs( a( i, j ) )
                    if( value < sum .or. stdlib_disnan( sum ) ) value = sum
                 end do
              end do
           else if( ( stdlib_lsame( norm, 'O' ) ) .or. ( norm=='1' ) ) then
              ! find norm1(a).
              value = zero
              do j = 1, n
                 sum = zero
                 do i = 1, min( n, j+1 )
                    sum = sum + abs( a( i, j ) )
                 end do
                 if( value < sum .or. stdlib_disnan( sum ) ) value = sum
              end do
           else if( stdlib_lsame( norm, 'I' ) ) then
              ! find normi(a).
              do i = 1, n
                 work( i ) = zero
              end do
              do j = 1, n
                 do i = 1, min( n, j+1 )
                    work( i ) = work( i ) + abs( a( i, j ) )
                 end do
              end do
              value = zero
              do i = 1, n
                 sum = work( i )
                 if( value < sum .or. stdlib_disnan( sum ) ) value = sum
              end do
           else if( ( stdlib_lsame( norm, 'F' ) ) .or. ( stdlib_lsame( norm, 'E' ) ) ) &
                     then
              ! find normf(a).
              scale = zero
              sum = one
              do j = 1, n
                 call stdlib_zlassq( min( n, j+1 ), a( 1_ilp, j ), 1_ilp, scale, sum )
              end do
              value = scale*sqrt( sum )
           end if
           stdlib_zlanhs = value
           return
     end function stdlib_zlanhs




     real(sp) module function stdlib_clanhf( norm, transr, uplo, n, a, work )
     !! CLANHF returns the value of the one norm,  or the Frobenius norm, or
     !! the  infinity norm,  or the  element of  largest absolute value  of a
     !! complex Hermitian matrix A in RFP format.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: norm, transr, uplo
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           real(sp), intent(out) :: work(0_ilp:*)
           complex(sp), intent(in) :: a(0_ilp:*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j, ifm, ilu, noe, n1, k, l, lda
           real(sp) :: scale, s, value, aa, temp
           ! Intrinsic Functions 
           ! Executable Statements 
           if( n==0_ilp ) then
              stdlib_clanhf = zero
              return
           else if( n==1_ilp ) then
              stdlib_clanhf = abs(real(a(0_ilp),KIND=sp))
              return
           end if
           ! set noe = 1 if n is odd. if n is even set noe=0
           noe = 1_ilp
           if( mod( n, 2_ilp )==0_ilp )noe = 0_ilp
           ! set ifm = 0 when form='c' or 'c' and 1 otherwise
           ifm = 1_ilp
           if( stdlib_lsame( transr, 'C' ) )ifm = 0_ilp
           ! set ilu = 0 when uplo='u or 'u' and 1 otherwise
           ilu = 1_ilp
           if( stdlib_lsame( uplo, 'U' ) )ilu = 0_ilp
           ! set lda = (n+1)/2 when ifm = 0
           ! set lda = n when ifm = 1 and noe = 1
           ! set lda = n+1 when ifm = 1 and noe = 0
           if( ifm==1_ilp ) then
              if( noe==1_ilp ) then
                 lda = n
              else
                 ! noe=0
                 lda = n + 1_ilp
              end if
           else
              ! ifm=0
              lda = ( n+1 ) / 2_ilp
           end if
           if( stdlib_lsame( norm, 'M' ) ) then
             ! find max(abs(a(i,j))).
              k = ( n+1 ) / 2_ilp
              value = zero
              if( noe==1_ilp ) then
                 ! n is odd
                 if( ifm==1_ilp ) then
                    ! a is n by k
                    if( ilu==1_ilp ) then
                       ! uplo ='l'
                       j = 0_ilp
                       ! -> l(0,0)
                       temp = abs( real( a( j+j*lda ),KIND=sp) )
                       if( value < temp .or. stdlib_sisnan( temp ) )value = temp
                       do i = 1, n - 1
                          temp = abs( a( i+j*lda ) )
                          if( value < temp .or. stdlib_sisnan( temp ) )value = temp
                       end do
                       do j = 1, k - 1
                          do i = 0, j - 2
                             temp = abs( a( i+j*lda ) )
                             if( value < temp .or. stdlib_sisnan( temp ) )value = temp
                          end do
                          i = j - 1_ilp
                          ! l(k+j,k+j)
                          temp = abs( real( a( i+j*lda ),KIND=sp) )
                          if( value < temp .or. stdlib_sisnan( temp ) )value = temp
                          i = j
                          ! -> l(j,j)
                          temp = abs( real( a( i+j*lda ),KIND=sp) )
                          if( value < temp .or. stdlib_sisnan( temp ) )value = temp
                          do i = j + 1, n - 1
                             temp = abs( a( i+j*lda ) )
                             if( value < temp .or. stdlib_sisnan( temp ) )value = temp
                          end do
                       end do
                    else
                       ! uplo = 'u'
                       do j = 0, k - 2
                          do i = 0, k + j - 2
                             temp = abs( a( i+j*lda ) )
                             if( value < temp .or. stdlib_sisnan( temp ) )value = temp
                          end do
                          i = k + j - 1_ilp
                          ! -> u(i,i)
                          temp = abs( real( a( i+j*lda ),KIND=sp) )
                          if( value < temp .or. stdlib_sisnan( temp ) )value = temp
                          i = i + 1_ilp
                          ! =k+j; i -> u(j,j)
                          temp = abs( real( a( i+j*lda ),KIND=sp) )
                          if( value < temp .or. stdlib_sisnan( temp ) )value = temp
                          do i = k + j + 1, n - 1
                             temp = abs( a( i+j*lda ) )
                             if( value < temp .or. stdlib_sisnan( temp ) )value = temp
                          end do
                       end do
                       do i = 0, n - 2
                          temp = abs( a( i+j*lda ) )
                          if( value < temp .or. stdlib_sisnan( temp ) )value = temp
                          ! j=k-1
                       end do
                       ! i=n-1 -> u(n-1,n-1)
                       temp = abs( real( a( i+j*lda ),KIND=sp) )
                       if( value < temp .or. stdlib_sisnan( temp ) )value = temp
                    end if
                 else
                    ! xpose case; a is k by n
                    if( ilu==1_ilp ) then
                       ! uplo ='l'
                       do j = 0, k - 2
                          do i = 0, j - 1
                             temp = abs( a( i+j*lda ) )
                             if( value < temp .or. stdlib_sisnan( temp ) )value = temp
                          end do
                          i = j
                          ! l(i,i)
                          temp = abs( real( a( i+j*lda ),KIND=sp) )
                          if( value < temp .or. stdlib_sisnan( temp ) )value = temp
                          i = j + 1_ilp
                          ! l(j+k,j+k)
                          temp = abs( real( a( i+j*lda ),KIND=sp) )
                          if( value < temp .or. stdlib_sisnan( temp ) )value = temp
                          do i = j + 2, k - 1
                             temp = abs( a( i+j*lda ) )
                             if( value < temp .or. stdlib_sisnan( temp ) )value = temp
                          end do
                       end do
                       j = k - 1_ilp
                       do i = 0, k - 2
                          temp = abs( a( i+j*lda ) )
                          if( value < temp .or. stdlib_sisnan( temp ) )value = temp
                       end do
                       i = k - 1_ilp
                       ! -> l(i,i) is at a(i,j)
                       temp = abs( real( a( i+j*lda ),KIND=sp) )
                          if( value < temp .or. stdlib_sisnan( temp ) )value = temp
                       do j = k, n - 1
                          do i = 0, k - 1
                             temp = abs( a( i+j*lda ) )
                             if( value < temp .or. stdlib_sisnan( temp ) )value = temp
                          end do
                       end do
                    else
                       ! uplo = 'u'
                       do j = 0, k - 2
                          do i = 0, k - 1
                             temp = abs( a( i+j*lda ) )
                             if( value < temp .or. stdlib_sisnan( temp ) )value = temp
                          end do
                       end do
                       j = k - 1_ilp
                       ! -> u(j,j) is at a(0,j)
                       temp = abs( real( a( 0_ilp+j*lda ),KIND=sp) )
                       if( value < temp .or. stdlib_sisnan( temp ) )value = temp
                       do i = 1, k - 1
                          temp = abs( a( i+j*lda ) )
                          if( value < temp .or. stdlib_sisnan( temp ) )value = temp
                       end do
                       do j = k, n - 1
                          do i = 0, j - k - 1
                             temp = abs( a( i+j*lda ) )
                             if( value < temp .or. stdlib_sisnan( temp ) )value = temp
                          end do
                          i = j - k
                          ! -> u(i,i) at a(i,j)
                          temp = abs( real( a( i+j*lda ),KIND=sp) )
                          if( value < temp .or. stdlib_sisnan( temp ) )value = temp
                          i = j - k + 1_ilp
                          ! u(j,j)
                          temp = abs( real( a( i+j*lda ),KIND=sp) )
                          if( value < temp .or. stdlib_sisnan( temp ) )value = temp
                          do i = j - k + 2, k - 1
                             temp = abs( a( i+j*lda ) )
                             if( value < temp .or. stdlib_sisnan( temp ) )value = temp
                          end do
                       end do
                    end if
                 end if
              else
                 ! n is even
                 if( ifm==1_ilp ) then
                    ! a is n+1 by k
                    if( ilu==1_ilp ) then
                       ! uplo ='l'
                       j = 0_ilp
                       ! -> l(k,k)
                       temp = abs( real( a( j+j*lda ),KIND=sp) )
                       if( value < temp .or. stdlib_sisnan( temp ) )value = temp
                       temp = abs( real( a( j+1+j*lda ),KIND=sp) )
                       if( value < temp .or. stdlib_sisnan( temp ) )value = temp
                       do i = 2, n
                          temp = abs( a( i+j*lda ) )
                          if( value < temp .or. stdlib_sisnan( temp ) )value = temp
                       end do
                       do j = 1, k - 1
                          do i = 0, j - 1
                             temp = abs( a( i+j*lda ) )
                             if( value < temp .or. stdlib_sisnan( temp ) )value = temp
                          end do
                          i = j
                          ! l(k+j,k+j)
                          temp = abs( real( a( i+j*lda ),KIND=sp) )
                          if( value < temp .or. stdlib_sisnan( temp ) )value = temp
                          i = j + 1_ilp
                          ! -> l(j,j)
                          temp = abs( real( a( i+j*lda ),KIND=sp) )
                          if( value < temp .or. stdlib_sisnan( temp ) )value = temp
                          do i = j + 2, n
                             temp = abs( a( i+j*lda ) )
                             if( value < temp .or. stdlib_sisnan( temp ) )value = temp
                          end do
                       end do
                    else
                       ! uplo = 'u'
                       do j = 0, k - 2
                          do i = 0, k + j - 1
                             temp = abs( a( i+j*lda ) )
                             if( value < temp .or. stdlib_sisnan( temp ) )value = temp
                          end do
                          i = k + j
                          ! -> u(i,i)
                          temp = abs( real( a( i+j*lda ),KIND=sp) )
                          if( value < temp .or. stdlib_sisnan( temp ) )value = temp
                          i = i + 1_ilp
                          ! =k+j+1; i -> u(j,j)
                          temp = abs( real( a( i+j*lda ),KIND=sp) )
                          if( value < temp .or. stdlib_sisnan( temp ) )value = temp
                          do i = k + j + 2, n
                             temp = abs( a( i+j*lda ) )
                             if( value < temp .or. stdlib_sisnan( temp ) )value = temp
                          end do
                       end do
                       do i = 0, n - 2
                          temp = abs( a( i+j*lda ) )
                          if( value < temp .or. stdlib_sisnan( temp ) )value = temp
                       ! j=k-1
                       end do
                       ! i=n-1 -> u(n-1,n-1)
                       temp = abs( real( a( i+j*lda ),KIND=sp) )
                          if( value < temp .or. stdlib_sisnan( temp ) )value = temp
                       i = n
                       ! -> u(k-1,k-1)
                       temp = abs( real( a( i+j*lda ),KIND=sp) )
                          if( value < temp .or. stdlib_sisnan( temp ) )value = temp
                    end if
                 else
                    ! xpose case; a is k by n+1
                    if( ilu==1_ilp ) then
                       ! uplo ='l'
                       j = 0_ilp
                       ! -> l(k,k) at a(0,0)
                       temp = abs( real( a( j+j*lda ),KIND=sp) )
                       if( value < temp .or. stdlib_sisnan( temp ) )value = temp
                       do i = 1, k - 1
                          temp = abs( a( i+j*lda ) )
                          if( value < temp .or. stdlib_sisnan( temp ) )value = temp
                       end do
                       do j = 1, k - 1
                          do i = 0, j - 2
                             temp = abs( a( i+j*lda ) )
                             if( value < temp .or. stdlib_sisnan( temp ) )value = temp
                          end do
                          i = j - 1_ilp
                          ! l(i,i)
                          temp = abs( real( a( i+j*lda ),KIND=sp) )
                          if( value < temp .or. stdlib_sisnan( temp ) )value = temp
                          i = j
                          ! l(j+k,j+k)
                          temp = abs( real( a( i+j*lda ),KIND=sp) )
                          if( value < temp .or. stdlib_sisnan( temp ) )value = temp
                          do i = j + 1, k - 1
                             temp = abs( a( i+j*lda ) )
                             if( value < temp .or. stdlib_sisnan( temp ) )value = temp
                          end do
                       end do
                       j = k
                       do i = 0, k - 2
                          temp = abs( a( i+j*lda ) )
                          if( value < temp .or. stdlib_sisnan( temp ) )value = temp
                       end do
                       i = k - 1_ilp
                       ! -> l(i,i) is at a(i,j)
                       temp = abs( real( a( i+j*lda ),KIND=sp) )
                       if( value < temp .or. stdlib_sisnan( temp ) )value = temp
                       do j = k + 1, n
                          do i = 0, k - 1
                             temp = abs( a( i+j*lda ) )
                             if( value < temp .or. stdlib_sisnan( temp ) )value = temp
                          end do
                       end do
                    else
                       ! uplo = 'u'
                       do j = 0, k - 1
                          do i = 0, k - 1
                             temp = abs( a( i+j*lda ) )
                             if( value < temp .or. stdlib_sisnan( temp ) )value = temp
                          end do
                       end do
                       j = k
                       ! -> u(j,j) is at a(0,j)
                       temp = abs( real( a( 0_ilp+j*lda ),KIND=sp) )
                       if( value < temp .or. stdlib_sisnan( temp ) )value = temp
                       do i = 1, k - 1
                          temp = abs( a( i+j*lda ) )
                          if( value < temp .or. stdlib_sisnan( temp ) )value = temp
                       end do
                       do j = k + 1, n - 1
                          do i = 0, j - k - 2
                             temp = abs( a( i+j*lda ) )
                             if( value < temp .or. stdlib_sisnan( temp ) )value = temp
                          end do
                          i = j - k - 1_ilp
                          ! -> u(i,i) at a(i,j)
                          temp = abs( real( a( i+j*lda ),KIND=sp) )
                          if( value < temp .or. stdlib_sisnan( temp ) )value = temp
                          i = j - k
                          ! u(j,j)
                          temp = abs( real( a( i+j*lda ),KIND=sp) )
                          if( value < temp .or. stdlib_sisnan( temp ) )value = temp
                          do i = j - k + 1, k - 1
                             temp = abs( a( i+j*lda ) )
                             if( value < temp .or. stdlib_sisnan( temp ) )value = temp
                          end do
                       end do
                       j = n
                       do i = 0, k - 2
                          temp = abs( a( i+j*lda ) )
                          if( value < temp .or. stdlib_sisnan( temp ) )value = temp
                       end do
                       i = k - 1_ilp
                       ! u(k,k) at a(i,j)
                       temp = abs( real( a( i+j*lda ),KIND=sp) )
                       if( value < temp .or. stdlib_sisnan( temp ) )value = temp
                    end if
                 end if
              end if
           else if( ( stdlib_lsame( norm, 'I' ) ) .or. ( stdlib_lsame( norm, 'O' ) ) .or.( &
                     norm=='1' ) ) then
             ! find normi(a) ( = norm1(a), since a is hermitian).
              if( ifm==1_ilp ) then
                 ! a is 'n'
                 k = n / 2_ilp
                 if( noe==1_ilp ) then
                    ! n is odd
                    if( ilu==0_ilp ) then
                       ! uplo = 'u'
                       do i = 0, k - 1
                          work( i ) = zero
                       end do
                       do j = 0, k
                          s = zero
                          do i = 0, k + j - 1
                             aa = abs( a( i+j*lda ) )
                             ! -> a(i,j+k)
                             s = s + aa
                             work( i ) = work( i ) + aa
                          end do
                          aa = abs( real( a( i+j*lda ),KIND=sp) )
                          ! -> a(j+k,j+k)
                          work( j+k ) = s + aa
                          if( i==k+k )go to 10
                          i = i + 1_ilp
                          aa = abs( real( a( i+j*lda ),KIND=sp) )
                          ! -> a(j,j)
                          work( j ) = work( j ) + aa
                          s = zero
                          do l = j + 1, k - 1
                             i = i + 1_ilp
                             aa = abs( a( i+j*lda ) )
                             ! -> a(l,j)
                             s = s + aa
                             work( l ) = work( l ) + aa
                          end do
                          work( j ) = work( j ) + s
                       end do
                       10 continue
                       value = work( 0_ilp )
                       do i = 1, n-1
                          temp = work( i )
                          if( value < temp .or. stdlib_sisnan( temp ) )value = temp
                       end do
                    else
                       ! ilu = 1
                       k = k + 1_ilp
                       ! k=(n+1)/2 for n odd and ilu=1
                       do i = k, n - 1
                          work( i ) = zero
                       end do
                       do j = k - 1, 0, -1
                          s = zero
                          do i = 0, j - 2
                             aa = abs( a( i+j*lda ) )
                             ! -> a(j+k,i+k)
                             s = s + aa
                             work( i+k ) = work( i+k ) + aa
                          end do
                          if( j>0_ilp ) then
                             aa = abs( real( a( i+j*lda ),KIND=sp) )
                             ! -> a(j+k,j+k)
                             s = s + aa
                             work( i+k ) = work( i+k ) + s
                             ! i=j
                             i = i + 1_ilp
                          end if
                          aa = abs( real( a( i+j*lda ),KIND=sp) )
                          ! -> a(j,j)
                          work( j ) = aa
                          s = zero
                          do l = j + 1, n - 1
                             i = i + 1_ilp
                             aa = abs( a( i+j*lda ) )
                             ! -> a(l,j)
                             s = s + aa
                             work( l ) = work( l ) + aa
                          end do
                          work( j ) = work( j ) + s
                       end do
                       value = work( 0_ilp )
                       do i = 1, n-1
                          temp = work( i )
                          if( value < temp .or. stdlib_sisnan( temp ) )value = temp
                       end do
                    end if
                 else
                    ! n is even
                    if( ilu==0_ilp ) then
                       ! uplo = 'u'
                       do i = 0, k - 1
                          work( i ) = zero
                       end do
                       do j = 0, k - 1
                          s = zero
                          do i = 0, k + j - 1
                             aa = abs( a( i+j*lda ) )
                             ! -> a(i,j+k)
                             s = s + aa
                             work( i ) = work( i ) + aa
                          end do
                          aa = abs( real( a( i+j*lda ),KIND=sp) )
                          ! -> a(j+k,j+k)
                          work( j+k ) = s + aa
                          i = i + 1_ilp
                          aa = abs( real( a( i+j*lda ),KIND=sp) )
                          ! -> a(j,j)
                          work( j ) = work( j ) + aa
                          s = zero
                          do l = j + 1, k - 1
                             i = i + 1_ilp
                             aa = abs( a( i+j*lda ) )
                             ! -> a(l,j)
                             s = s + aa
                             work( l ) = work( l ) + aa
                          end do
                          work( j ) = work( j ) + s
                       end do
                       value = work( 0_ilp )
                       do i = 1, n-1
                          temp = work( i )
                          if( value < temp .or. stdlib_sisnan( temp ) )value = temp
                       end do
                    else
                       ! ilu = 1
                       do i = k, n - 1
                          work( i ) = zero
                       end do
                       do j = k - 1, 0, -1
                          s = zero
                          do i = 0, j - 1
                             aa = abs( a( i+j*lda ) )
                             ! -> a(j+k,i+k)
                             s = s + aa
                             work( i+k ) = work( i+k ) + aa
                          end do
                          aa = abs( real( a( i+j*lda ),KIND=sp) )
                          ! -> a(j+k,j+k)
                          s = s + aa
                          work( i+k ) = work( i+k ) + s
                          ! i=j
                          i = i + 1_ilp
                          aa = abs( real( a( i+j*lda ),KIND=sp) )
                          ! -> a(j,j)
                          work( j ) = aa
                          s = zero
                          do l = j + 1, n - 1
                             i = i + 1_ilp
                             aa = abs( a( i+j*lda ) )
                             ! -> a(l,j)
                             s = s + aa
                             work( l ) = work( l ) + aa
                          end do
                          work( j ) = work( j ) + s
                       end do
                       value = work( 0_ilp )
                       do i = 1, n-1
                          temp = work( i )
                          if( value < temp .or. stdlib_sisnan( temp ) )value = temp
                       end do
                    end if
                 end if
              else
                 ! ifm=0
                 k = n / 2_ilp
                 if( noe==1_ilp ) then
                    ! n is odd
                    if( ilu==0_ilp ) then
                       ! uplo = 'u'
                       n1 = k
                       ! n/2
                       k = k + 1_ilp
                       ! k is the row size and lda
                       do i = n1, n - 1
                          work( i ) = zero
                       end do
                       do j = 0, n1 - 1
                          s = zero
                          do i = 0, k - 1
                             aa = abs( a( i+j*lda ) )
                             ! a(j,n1+i)
                             work( i+n1 ) = work( i+n1 ) + aa
                             s = s + aa
                          end do
                          work( j ) = s
                       end do
                       ! j=n1=k-1 is special
                       s = abs( real( a( 0_ilp+j*lda ),KIND=sp) )
                       ! a(k-1,k-1)
                       do i = 1, k - 1
                          aa = abs( a( i+j*lda ) )
                          ! a(k-1,i+n1)
                          work( i+n1 ) = work( i+n1 ) + aa
                          s = s + aa
                       end do
                       work( j ) = work( j ) + s
                       do j = k, n - 1
                          s = zero
                          do i = 0, j - k - 1
                             aa = abs( a( i+j*lda ) )
                             ! a(i,j-k)
                             work( i ) = work( i ) + aa
                             s = s + aa
                          end do
                          ! i=j-k
                          aa = abs( real( a( i+j*lda ),KIND=sp) )
                          ! a(j-k,j-k)
                          s = s + aa
                          work( j-k ) = work( j-k ) + s
                          i = i + 1_ilp
                          s = abs( real( a( i+j*lda ),KIND=sp) )
                          ! a(j,j)
                          do l = j + 1, n - 1
                             i = i + 1_ilp
                             aa = abs( a( i+j*lda ) )
                             ! a(j,l)
                             work( l ) = work( l ) + aa
                             s = s + aa
                          end do
                          work( j ) = work( j ) + s
                       end do
                       value = work( 0_ilp )
                       do i = 1, n-1
                          temp = work( i )
                          if( value < temp .or. stdlib_sisnan( temp ) )value = temp
                       end do
                    else
                       ! ilu=1
                       k = k + 1_ilp
                       ! k=(n+1)/2 for n odd and ilu=1
                       do i = k, n - 1
                          work( i ) = zero
                       end do
                       do j = 0, k - 2
                          ! process
                          s = zero
                          do i = 0, j - 1
                             aa = abs( a( i+j*lda ) )
                             ! a(j,i)
                             work( i ) = work( i ) + aa
                             s = s + aa
                          end do
                          aa = abs( real( a( i+j*lda ),KIND=sp) )
                          ! i=j so process of a(j,j)
                          s = s + aa
                          work( j ) = s
                          ! is initialised here
                          i = i + 1_ilp
                          ! i=j process a(j+k,j+k)
                          aa = abs( real( a( i+j*lda ),KIND=sp) )
                          s = aa
                          do l = k + j + 1, n - 1
                             i = i + 1_ilp
                             aa = abs( a( i+j*lda ) )
                             ! a(l,k+j)
                             s = s + aa
                             work( l ) = work( l ) + aa
                          end do
                          work( k+j ) = work( k+j ) + s
                       end do
                       ! j=k-1 is special :process col a(k-1,0:k-1)
                       s = zero
                       do i = 0, k - 2
                          aa = abs( a( i+j*lda ) )
                          ! a(k,i)
                          work( i ) = work( i ) + aa
                          s = s + aa
                       end do
                       ! i=k-1
                       aa = abs( real( a( i+j*lda ),KIND=sp) )
                       ! a(k-1,k-1)
                       s = s + aa
                       work( i ) = s
                       ! done with col j=k+1
                       do j = k, n - 1
                          ! process col j of a = a(j,0:k-1)
                          s = zero
                          do i = 0, k - 1
                             aa = abs( a( i+j*lda ) )
                             ! a(j,i)
                             work( i ) = work( i ) + aa
                             s = s + aa
                          end do
                          work( j ) = work( j ) + s
                       end do
                       value = work( 0_ilp )
                       do i = 1, n-1
                          temp = work( i )
                          if( value < temp .or. stdlib_sisnan( temp ) )value = temp
                       end do
                    end if
                 else
                    ! n is even
                    if( ilu==0_ilp ) then
                       ! uplo = 'u'
                       do i = k, n - 1
                          work( i ) = zero
                       end do
                       do j = 0, k - 1
                          s = zero
                          do i = 0, k - 1
                             aa = abs( a( i+j*lda ) )
                             ! a(j,i+k)
                             work( i+k ) = work( i+k ) + aa
                             s = s + aa
                          end do
                          work( j ) = s
                       end do
                       ! j=k
                       aa = abs( real( a( 0_ilp+j*lda ),KIND=sp) )
                       ! a(k,k)
                       s = aa
                       do i = 1, k - 1
                          aa = abs( a( i+j*lda ) )
                          ! a(k,k+i)
                          work( i+k ) = work( i+k ) + aa
                          s = s + aa
                       end do
                       work( j ) = work( j ) + s
                       do j = k + 1, n - 1
                          s = zero
                          do i = 0, j - 2 - k
                             aa = abs( a( i+j*lda ) )
                             ! a(i,j-k-1)
                             work( i ) = work( i ) + aa
                             s = s + aa
                          end do
                          ! i=j-1-k
                          aa = abs( real( a( i+j*lda ),KIND=sp) )
                          ! a(j-k-1,j-k-1)
                          s = s + aa
                          work( j-k-1 ) = work( j-k-1 ) + s
                          i = i + 1_ilp
                          aa = abs( real( a( i+j*lda ),KIND=sp) )
                          ! a(j,j)
                          s = aa
                          do l = j + 1, n - 1
                             i = i + 1_ilp
                             aa = abs( a( i+j*lda ) )
                             ! a(j,l)
                             work( l ) = work( l ) + aa
                             s = s + aa
                          end do
                          work( j ) = work( j ) + s
                       end do
                       ! j=n
                       s = zero
                       do i = 0, k - 2
                          aa = abs( a( i+j*lda ) )
                          ! a(i,k-1)
                          work( i ) = work( i ) + aa
                          s = s + aa
                       end do
                       ! i=k-1
                       aa = abs( real( a( i+j*lda ),KIND=sp) )
                       ! a(k-1,k-1)
                       s = s + aa
                       work( i ) = work( i ) + s
                       value = work( 0_ilp )
                       do i = 1, n-1
                          temp = work( i )
                          if( value < temp .or. stdlib_sisnan( temp ) )value = temp
                       end do
                    else
                       ! ilu=1
                       do i = k, n - 1
                          work( i ) = zero
                       end do
                       ! j=0 is special :process col a(k:n-1,k)
                       s = abs( real( a( 0_ilp ),KIND=sp) )
                       ! a(k,k)
                       do i = 1, k - 1
                          aa = abs( a( i ) )
                          ! a(k+i,k)
                          work( i+k ) = work( i+k ) + aa
                          s = s + aa
                       end do
                       work( k ) = work( k ) + s
                       do j = 1, k - 1
                          ! process
                          s = zero
                          do i = 0, j - 2
                             aa = abs( a( i+j*lda ) )
                             ! a(j-1,i)
                             work( i ) = work( i ) + aa
                             s = s + aa
                          end do
                          aa = abs( real( a( i+j*lda ),KIND=sp) )
                          ! i=j-1 so process of a(j-1,j-1)
                          s = s + aa
                          work( j-1 ) = s
                          ! is initialised here
                          i = i + 1_ilp
                          ! i=j process a(j+k,j+k)
                          aa = abs( real( a( i+j*lda ),KIND=sp) )
                          s = aa
                          do l = k + j + 1, n - 1
                             i = i + 1_ilp
                             aa = abs( a( i+j*lda ) )
                             ! a(l,k+j)
                             s = s + aa
                             work( l ) = work( l ) + aa
                          end do
                          work( k+j ) = work( k+j ) + s
                       end do
                       ! j=k is special :process col a(k,0:k-1)
                       s = zero
                       do i = 0, k - 2
                          aa = abs( a( i+j*lda ) )
                          ! a(k,i)
                          work( i ) = work( i ) + aa
                          s = s + aa
                       end do
                       ! i=k-1
                       aa = abs( real( a( i+j*lda ),KIND=sp) )
                       ! a(k-1,k-1)
                       s = s + aa
                       work( i ) = s
                       ! done with col j=k+1
                       do j = k + 1, n
                          ! process col j-1 of a = a(j-1,0:k-1)
                          s = zero
                          do i = 0, k - 1
                             aa = abs( a( i+j*lda ) )
                             ! a(j-1,i)
                             work( i ) = work( i ) + aa
                             s = s + aa
                          end do
                          work( j-1 ) = work( j-1 ) + s
                       end do
                       value = work( 0_ilp )
                       do i = 1, n-1
                          temp = work( i )
                          if( value < temp .or. stdlib_sisnan( temp ) )value = temp
                       end do
                    end if
                 end if
              end if
           else if( ( stdlib_lsame( norm, 'F' ) ) .or. ( stdlib_lsame( norm, 'E' ) ) ) &
                     then
             ! find normf(a).
              k = ( n+1 ) / 2_ilp
              scale = zero
              s = one
              if( noe==1_ilp ) then
                 ! n is odd
                 if( ifm==1_ilp ) then
                    ! a is normal
                    if( ilu==0_ilp ) then
                       ! a is upper
                       do j = 0, k - 3
                          call stdlib_classq( k-j-2, a( k+j+1+j*lda ), 1_ilp, scale, s )
                          ! l at a(k,0)
                       end do
                       do j = 0, k - 1
                          call stdlib_classq( k+j-1, a( 0_ilp+j*lda ), 1_ilp, scale, s )
                          ! trap u at a(0,0)
                       end do
                       s = s + s
                       ! double s for the off diagonal elements
                       l = k - 1_ilp
                       ! -> u(k,k) at a(k-1,0)
                       do i = 0, k - 2
                          aa = real( a( l ),KIND=sp)
                          ! u(k+i,k+i)
                          if( aa/=zero ) then
                             if( scale<aa ) then
                                s = one + s*( scale / aa )**2_ilp
                                scale = aa
                             else
                                s = s + ( aa / scale )**2_ilp
                             end if
                          end if
                          aa = real( a( l+1 ),KIND=sp)
                          ! u(i,i)
                          if( aa/=zero ) then
                             if( scale<aa ) then
                                s = one + s*( scale / aa )**2_ilp
                                scale = aa
                             else
                                s = s + ( aa / scale )**2_ilp
                             end if
                          end if
                          l = l + lda + 1_ilp
                       end do
                       aa = real( a( l ),KIND=sp)
                       ! u(n-1,n-1)
                       if( aa/=zero ) then
                          if( scale<aa ) then
                             s = one + s*( scale / aa )**2_ilp
                             scale = aa
                          else
                             s = s + ( aa / scale )**2_ilp
                          end if
                       end if
                    else
                       ! ilu=1
                       do j = 0, k - 1
                          call stdlib_classq( n-j-1, a( j+1+j*lda ), 1_ilp, scale, s )
                          ! trap l at a(0,0)
                       end do
                       do j = 1, k - 2
                          call stdlib_classq( j, a( 0_ilp+( 1_ilp+j )*lda ), 1_ilp, scale, s )
                          ! u at a(0,1)
                       end do
                       s = s + s
                       ! double s for the off diagonal elements
                       aa = real( a( 0_ilp ),KIND=sp)
                       ! l(0,0) at a(0,0)
                       if( aa/=zero ) then
                          if( scale<aa ) then
                             s = one + s*( scale / aa )**2_ilp
                             scale = aa
                          else
                             s = s + ( aa / scale )**2_ilp
                          end if
                       end if
                       l = lda
                       ! -> l(k,k) at a(0,1)
                       do i = 1, k - 1
                          aa = real( a( l ),KIND=sp)
                          ! l(k-1+i,k-1+i)
                          if( aa/=zero ) then
                             if( scale<aa ) then
                                s = one + s*( scale / aa )**2_ilp
                                scale = aa
                             else
                                s = s + ( aa / scale )**2_ilp
                             end if
                          end if
                          aa = real( a( l+1 ),KIND=sp)
                          ! l(i,i)
                          if( aa/=zero ) then
                             if( scale<aa ) then
                                s = one + s*( scale / aa )**2_ilp
                                scale = aa
                             else
                                s = s + ( aa / scale )**2_ilp
                             end if
                          end if
                          l = l + lda + 1_ilp
                       end do
                    end if
                 else
                    ! a is xpose
                    if( ilu==0_ilp ) then
                       ! a**h is upper
                       do j = 1, k - 2
                          call stdlib_classq( j, a( 0_ilp+( k+j )*lda ), 1_ilp, scale, s )
                          ! u at a(0,k)
                       end do
                       do j = 0, k - 2
                          call stdlib_classq( k, a( 0_ilp+j*lda ), 1_ilp, scale, s )
                          ! k by k-1 rect. at a(0,0)
                       end do
                       do j = 0, k - 2
                          call stdlib_classq( k-j-1, a( j+1+( j+k-1 )*lda ), 1_ilp,scale, s )
                          ! l at a(0,k-1)
                       end do
                       s = s + s
                       ! double s for the off diagonal elements
                       l = 0_ilp + k*lda - lda
                       ! -> u(k-1,k-1) at a(0,k-1)
                       aa = real( a( l ),KIND=sp)
                       ! u(k-1,k-1)
                       if( aa/=zero ) then
                          if( scale<aa ) then
                             s = one + s*( scale / aa )**2_ilp
                             scale = aa
                          else
                             s = s + ( aa / scale )**2_ilp
                          end if
                       end if
                       l = l + lda
                       ! -> u(0,0) at a(0,k)
                       do j = k, n - 1
                          aa = real( a( l ),KIND=sp)
                          ! -> u(j-k,j-k)
                          if( aa/=zero ) then
                             if( scale<aa ) then
                                s = one + s*( scale / aa )**2_ilp
                                scale = aa
                             else
                                s = s + ( aa / scale )**2_ilp
                             end if
                          end if
                          aa = real( a( l+1 ),KIND=sp)
                          ! -> u(j,j)
                          if( aa/=zero ) then
                             if( scale<aa ) then
                                s = one + s*( scale / aa )**2_ilp
                                scale = aa
                             else
                                s = s + ( aa / scale )**2_ilp
                             end if
                          end if
                          l = l + lda + 1_ilp
                       end do
                    else
                       ! a**h is lower
                       do j = 1, k - 1
                          call stdlib_classq( j, a( 0_ilp+j*lda ), 1_ilp, scale, s )
                          ! u at a(0,0)
                       end do
                       do j = k, n - 1
                          call stdlib_classq( k, a( 0_ilp+j*lda ), 1_ilp, scale, s )
                          ! k by k-1 rect. at a(0,k)
                       end do
                       do j = 0, k - 3
                          call stdlib_classq( k-j-2, a( j+2+j*lda ), 1_ilp, scale, s )
                          ! l at a(1,0)
                       end do
                       s = s + s
                       ! double s for the off diagonal elements
                       l = 0_ilp
                       ! -> l(0,0) at a(0,0)
                       do i = 0, k - 2
                          aa = real( a( l ),KIND=sp)
                          ! l(i,i)
                          if( aa/=zero ) then
                             if( scale<aa ) then
                                s = one + s*( scale / aa )**2_ilp
                                scale = aa
                             else
                                s = s + ( aa / scale )**2_ilp
                             end if
                          end if
                          aa = real( a( l+1 ),KIND=sp)
                          ! l(k+i,k+i)
                          if( aa/=zero ) then
                             if( scale<aa ) then
                                s = one + s*( scale / aa )**2_ilp
                                scale = aa
                             else
                                s = s + ( aa / scale )**2_ilp
                             end if
                          end if
                          l = l + lda + 1_ilp
                       end do
                       ! l-> k-1 + (k-1)*lda or l(k-1,k-1) at a(k-1,k-1)
                       aa = real( a( l ),KIND=sp)
                       ! l(k-1,k-1) at a(k-1,k-1)
                       if( aa/=zero ) then
                          if( scale<aa ) then
                             s = one + s*( scale / aa )**2_ilp
                             scale = aa
                          else
                             s = s + ( aa / scale )**2_ilp
                          end if
                       end if
                    end if
                 end if
              else
                 ! n is even
                 if( ifm==1_ilp ) then
                    ! a is normal
                    if( ilu==0_ilp ) then
                       ! a is upper
                       do j = 0, k - 2
                          call stdlib_classq( k-j-1, a( k+j+2+j*lda ), 1_ilp, scale, s )
                       ! l at a(k+1,0)
                       end do
                       do j = 0, k - 1
                          call stdlib_classq( k+j, a( 0_ilp+j*lda ), 1_ilp, scale, s )
                       ! trap u at a(0,0)
                       end do
                       s = s + s
                       ! double s for the off diagonal elements
                       l = k
                       ! -> u(k,k) at a(k,0)
                       do i = 0, k - 1
                          aa = real( a( l ),KIND=sp)
                          ! u(k+i,k+i)
                          if( aa/=zero ) then
                             if( scale<aa ) then
                                s = one + s*( scale / aa )**2_ilp
                                scale = aa
                             else
                                s = s + ( aa / scale )**2_ilp
                             end if
                          end if
                          aa = real( a( l+1 ),KIND=sp)
                          ! u(i,i)
                          if( aa/=zero ) then
                             if( scale<aa ) then
                                s = one + s*( scale / aa )**2_ilp
                                scale = aa
                             else
                                s = s + ( aa / scale )**2_ilp
                             end if
                          end if
                          l = l + lda + 1_ilp
                       end do
                    else
                       ! ilu=1
                       do j = 0, k - 1
                          call stdlib_classq( n-j-1, a( j+2+j*lda ), 1_ilp, scale, s )
                          ! trap l at a(1,0)
                       end do
                       do j = 1, k - 1
                          call stdlib_classq( j, a( 0_ilp+j*lda ), 1_ilp, scale, s )
                          ! u at a(0,0)
                       end do
                       s = s + s
                       ! double s for the off diagonal elements
                       l = 0_ilp
                       ! -> l(k,k) at a(0,0)
                       do i = 0, k - 1
                          aa = real( a( l ),KIND=sp)
                          ! l(k-1+i,k-1+i)
                          if( aa/=zero ) then
                             if( scale<aa ) then
                                s = one + s*( scale / aa )**2_ilp
                                scale = aa
                             else
                                s = s + ( aa / scale )**2_ilp
                             end if
                          end if
                          aa = real( a( l+1 ),KIND=sp)
                          ! l(i,i)
                          if( aa/=zero ) then
                             if( scale<aa ) then
                                s = one + s*( scale / aa )**2_ilp
                                scale = aa
                             else
                                s = s + ( aa / scale )**2_ilp
                             end if
                          end if
                          l = l + lda + 1_ilp
                       end do
                    end if
                 else
                    ! a is xpose
                    if( ilu==0_ilp ) then
                       ! a**h is upper
                       do j = 1, k - 1
                          call stdlib_classq( j, a( 0_ilp+( k+1+j )*lda ), 1_ilp, scale, s )
                       ! u at a(0,k+1)
                       end do
                       do j = 0, k - 1
                          call stdlib_classq( k, a( 0_ilp+j*lda ), 1_ilp, scale, s )
                       ! k by k rect. at a(0,0)
                       end do
                       do j = 0, k - 2
                          call stdlib_classq( k-j-1, a( j+1+( j+k )*lda ), 1_ilp, scale,s )
                       ! l at a(0,k)
                       end do
                       s = s + s
                       ! double s for the off diagonal elements
                       l = 0_ilp + k*lda
                       ! -> u(k,k) at a(0,k)
                       aa = real( a( l ),KIND=sp)
                       ! u(k,k)
                       if( aa/=zero ) then
                          if( scale<aa ) then
                             s = one + s*( scale / aa )**2_ilp
                             scale = aa
                          else
                             s = s + ( aa / scale )**2_ilp
                          end if
                       end if
                       l = l + lda
                       ! -> u(0,0) at a(0,k+1)
                       do j = k + 1, n - 1
                          aa = real( a( l ),KIND=sp)
                          ! -> u(j-k-1,j-k-1)
                          if( aa/=zero ) then
                             if( scale<aa ) then
                                s = one + s*( scale / aa )**2_ilp
                                scale = aa
                             else
                                s = s + ( aa / scale )**2_ilp
                             end if
                          end if
                          aa = real( a( l+1 ),KIND=sp)
                          ! -> u(j,j)
                          if( aa/=zero ) then
                             if( scale<aa ) then
                                s = one + s*( scale / aa )**2_ilp
                                scale = aa
                             else
                                s = s + ( aa / scale )**2_ilp
                             end if
                          end if
                          l = l + lda + 1_ilp
                       end do
                       ! l=k-1+n*lda
                       ! -> u(k-1,k-1) at a(k-1,n)
                       aa = real( a( l ),KIND=sp)
                       ! u(k,k)
                       if( aa/=zero ) then
                          if( scale<aa ) then
                             s = one + s*( scale / aa )**2_ilp
                             scale = aa
                          else
                             s = s + ( aa / scale )**2_ilp
                          end if
                       end if
                    else
                       ! a**h is lower
                       do j = 1, k - 1
                          call stdlib_classq( j, a( 0_ilp+( j+1 )*lda ), 1_ilp, scale, s )
                       ! u at a(0,1)
                       end do
                       do j = k + 1, n
                          call stdlib_classq( k, a( 0_ilp+j*lda ), 1_ilp, scale, s )
                       ! k by k rect. at a(0,k+1)
                       end do
                       do j = 0, k - 2
                          call stdlib_classq( k-j-1, a( j+1+j*lda ), 1_ilp, scale, s )
                       ! l at a(0,0)
                       end do
                       s = s + s
                       ! double s for the off diagonal elements
                       l = 0_ilp
                       ! -> l(k,k) at a(0,0)
                       aa = real( a( l ),KIND=sp)
                       ! l(k,k) at a(0,0)
                       if( aa/=zero ) then
                          if( scale<aa ) then
                             s = one + s*( scale / aa )**2_ilp
                             scale = aa
                          else
                             s = s + ( aa / scale )**2_ilp
                          end if
                       end if
                       l = lda
                       ! -> l(0,0) at a(0,1)
                       do i = 0, k - 2
                          aa = real( a( l ),KIND=sp)
                          ! l(i,i)
                          if( aa/=zero ) then
                             if( scale<aa ) then
                                s = one + s*( scale / aa )**2_ilp
                                scale = aa
                             else
                                s = s + ( aa / scale )**2_ilp
                             end if
                          end if
                          aa = real( a( l+1 ),KIND=sp)
                          ! l(k+i+1,k+i+1)
                          if( aa/=zero ) then
                             if( scale<aa ) then
                                s = one + s*( scale / aa )**2_ilp
                                scale = aa
                             else
                                s = s + ( aa / scale )**2_ilp
                             end if
                          end if
                          l = l + lda + 1_ilp
                       end do
                       ! l-> k - 1 + k*lda or l(k-1,k-1) at a(k-1,k)
                       aa = real( a( l ),KIND=sp)
                       ! l(k-1,k-1) at a(k-1,k)
                       if( aa/=zero ) then
                          if( scale<aa ) then
                             s = one + s*( scale / aa )**2_ilp
                             scale = aa
                          else
                             s = s + ( aa / scale )**2_ilp
                          end if
                       end if
                    end if
                 end if
              end if
              value = scale*sqrt( s )
           end if
           stdlib_clanhf = value
           return
     end function stdlib_clanhf

     real(dp) module function stdlib_zlanhf( norm, transr, uplo, n, a, work )
     !! ZLANHF returns the value of the one norm,  or the Frobenius norm, or
     !! the  infinity norm,  or the  element of  largest absolute value  of a
     !! complex Hermitian matrix A in RFP format.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: norm, transr, uplo
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           real(dp), intent(out) :: work(0_ilp:*)
           complex(dp), intent(in) :: a(0_ilp:*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j, ifm, ilu, noe, n1, k, l, lda
           real(dp) :: scale, s, value, aa, temp
           ! Intrinsic Functions 
           ! Executable Statements 
           if( n==0_ilp ) then
              stdlib_zlanhf = zero
              return
           else if( n==1_ilp ) then
              stdlib_zlanhf = abs(real(a(0_ilp),KIND=dp))
              return
           end if
           ! set noe = 1 if n is odd. if n is even set noe=0
           noe = 1_ilp
           if( mod( n, 2_ilp )==0_ilp )noe = 0_ilp
           ! set ifm = 0 when form='c' or 'c' and 1 otherwise
           ifm = 1_ilp
           if( stdlib_lsame( transr, 'C' ) )ifm = 0_ilp
           ! set ilu = 0 when uplo='u or 'u' and 1 otherwise
           ilu = 1_ilp
           if( stdlib_lsame( uplo, 'U' ) )ilu = 0_ilp
           ! set lda = (n+1)/2 when ifm = 0
           ! set lda = n when ifm = 1 and noe = 1
           ! set lda = n+1 when ifm = 1 and noe = 0
           if( ifm==1_ilp ) then
              if( noe==1_ilp ) then
                 lda = n
              else
                 ! noe=0
                 lda = n + 1_ilp
              end if
           else
              ! ifm=0
              lda = ( n+1 ) / 2_ilp
           end if
           if( stdlib_lsame( norm, 'M' ) ) then
             ! find max(abs(a(i,j))).
              k = ( n+1 ) / 2_ilp
              value = zero
              if( noe==1_ilp ) then
                 ! n is odd
                 if( ifm==1_ilp ) then
                    ! a is n by k
                    if( ilu==1_ilp ) then
                       ! uplo ='l'
                       j = 0_ilp
                       ! -> l(0,0)
                       temp = abs( real( a( j+j*lda ),KIND=dp) )
                       if( value < temp .or. stdlib_disnan( temp ) )value = temp
                       do i = 1, n - 1
                          temp = abs( a( i+j*lda ) )
                          if( value < temp .or. stdlib_disnan( temp ) )value = temp
                       end do
                       do j = 1, k - 1
                          do i = 0, j - 2
                             temp = abs( a( i+j*lda ) )
                             if( value < temp .or. stdlib_disnan( temp ) )value = temp
                          end do
                          i = j - 1_ilp
                          ! l(k+j,k+j)
                          temp = abs( real( a( i+j*lda ),KIND=dp) )
                          if( value < temp .or. stdlib_disnan( temp ) )value = temp
                          i = j
                          ! -> l(j,j)
                          temp = abs( real( a( i+j*lda ),KIND=dp) )
                          if( value < temp .or. stdlib_disnan( temp ) )value = temp
                          do i = j + 1, n - 1
                             temp = abs( a( i+j*lda ) )
                             if( value < temp .or. stdlib_disnan( temp ) )value = temp
                          end do
                       end do
                    else
                       ! uplo = 'u'
                       do j = 0, k - 2
                          do i = 0, k + j - 2
                             temp = abs( a( i+j*lda ) )
                             if( value < temp .or. stdlib_disnan( temp ) )value = temp
                          end do
                          i = k + j - 1_ilp
                          ! -> u(i,i)
                          temp = abs( real( a( i+j*lda ),KIND=dp) )
                          if( value < temp .or. stdlib_disnan( temp ) )value = temp
                          i = i + 1_ilp
                          ! =k+j; i -> u(j,j)
                          temp = abs( real( a( i+j*lda ),KIND=dp) )
                          if( value < temp .or. stdlib_disnan( temp ) )value = temp
                          do i = k + j + 1, n - 1
                             temp = abs( a( i+j*lda ) )
                             if( value < temp .or. stdlib_disnan( temp ) )value = temp
                          end do
                       end do
                       do i = 0, n - 2
                          temp = abs( a( i+j*lda ) )
                          if( value < temp .or. stdlib_disnan( temp ) )value = temp
                          ! j=k-1
                       end do
                       ! i=n-1 -> u(n-1,n-1)
                       temp = abs( real( a( i+j*lda ),KIND=dp) )
                       if( value < temp .or. stdlib_disnan( temp ) )value = temp
                    end if
                 else
                    ! xpose case; a is k by n
                    if( ilu==1_ilp ) then
                       ! uplo ='l'
                       do j = 0, k - 2
                          do i = 0, j - 1
                             temp = abs( a( i+j*lda ) )
                             if( value < temp .or. stdlib_disnan( temp ) )value = temp
                          end do
                          i = j
                          ! l(i,i)
                          temp = abs( real( a( i+j*lda ),KIND=dp) )
                          if( value < temp .or. stdlib_disnan( temp ) )value = temp
                          i = j + 1_ilp
                          ! l(j+k,j+k)
                          temp = abs( real( a( i+j*lda ),KIND=dp) )
                          if( value < temp .or. stdlib_disnan( temp ) )value = temp
                          do i = j + 2, k - 1
                             temp = abs( a( i+j*lda ) )
                             if( value < temp .or. stdlib_disnan( temp ) )value = temp
                          end do
                       end do
                       j = k - 1_ilp
                       do i = 0, k - 2
                          temp = abs( a( i+j*lda ) )
                          if( value < temp .or. stdlib_disnan( temp ) )value = temp
                       end do
                       i = k - 1_ilp
                       ! -> l(i,i) is at a(i,j)
                       temp = abs( real( a( i+j*lda ),KIND=dp) )
                          if( value < temp .or. stdlib_disnan( temp ) )value = temp
                       do j = k, n - 1
                          do i = 0, k - 1
                             temp = abs( a( i+j*lda ) )
                             if( value < temp .or. stdlib_disnan( temp ) )value = temp
                          end do
                       end do
                    else
                       ! uplo = 'u'
                       do j = 0, k - 2
                          do i = 0, k - 1
                             temp = abs( a( i+j*lda ) )
                             if( value < temp .or. stdlib_disnan( temp ) )value = temp
                          end do
                       end do
                       j = k - 1_ilp
                       ! -> u(j,j) is at a(0,j)
                       temp = abs( real( a( 0_ilp+j*lda ),KIND=dp) )
                       if( value < temp .or. stdlib_disnan( temp ) )value = temp
                       do i = 1, k - 1
                          temp = abs( a( i+j*lda ) )
                          if( value < temp .or. stdlib_disnan( temp ) )value = temp
                       end do
                       do j = k, n - 1
                          do i = 0, j - k - 1
                             temp = abs( a( i+j*lda ) )
                             if( value < temp .or. stdlib_disnan( temp ) )value = temp
                          end do
                          i = j - k
                          ! -> u(i,i) at a(i,j)
                          temp = abs( real( a( i+j*lda ),KIND=dp) )
                          if( value < temp .or. stdlib_disnan( temp ) )value = temp
                          i = j - k + 1_ilp
                          ! u(j,j)
                          temp = abs( real( a( i+j*lda ),KIND=dp) )
                          if( value < temp .or. stdlib_disnan( temp ) )value = temp
                          do i = j - k + 2, k - 1
                             temp = abs( a( i+j*lda ) )
                             if( value < temp .or. stdlib_disnan( temp ) )value = temp
                          end do
                       end do
                    end if
                 end if
              else
                 ! n is even
                 if( ifm==1_ilp ) then
                    ! a is n+1 by k
                    if( ilu==1_ilp ) then
                       ! uplo ='l'
                       j = 0_ilp
                       ! -> l(k,k)
                       temp = abs( real( a( j+j*lda ),KIND=dp) )
                       if( value < temp .or. stdlib_disnan( temp ) )value = temp
                       temp = abs( real( a( j+1+j*lda ),KIND=dp) )
                       if( value < temp .or. stdlib_disnan( temp ) )value = temp
                       do i = 2, n
                          temp = abs( a( i+j*lda ) )
                          if( value < temp .or. stdlib_disnan( temp ) )value = temp
                       end do
                       do j = 1, k - 1
                          do i = 0, j - 1
                             temp = abs( a( i+j*lda ) )
                             if( value < temp .or. stdlib_disnan( temp ) )value = temp
                          end do
                          i = j
                          ! l(k+j,k+j)
                          temp = abs( real( a( i+j*lda ),KIND=dp) )
                          if( value < temp .or. stdlib_disnan( temp ) )value = temp
                          i = j + 1_ilp
                          ! -> l(j,j)
                          temp = abs( real( a( i+j*lda ),KIND=dp) )
                          if( value < temp .or. stdlib_disnan( temp ) )value = temp
                          do i = j + 2, n
                             temp = abs( a( i+j*lda ) )
                             if( value < temp .or. stdlib_disnan( temp ) )value = temp
                          end do
                       end do
                    else
                       ! uplo = 'u'
                       do j = 0, k - 2
                          do i = 0, k + j - 1
                             temp = abs( a( i+j*lda ) )
                             if( value < temp .or. stdlib_disnan( temp ) )value = temp
                          end do
                          i = k + j
                          ! -> u(i,i)
                          temp = abs( real( a( i+j*lda ),KIND=dp) )
                          if( value < temp .or. stdlib_disnan( temp ) )value = temp
                          i = i + 1_ilp
                          ! =k+j+1; i -> u(j,j)
                          temp = abs( real( a( i+j*lda ),KIND=dp) )
                          if( value < temp .or. stdlib_disnan( temp ) )value = temp
                          do i = k + j + 2, n
                             temp = abs( a( i+j*lda ) )
                             if( value < temp .or. stdlib_disnan( temp ) )value = temp
                          end do
                       end do
                       do i = 0, n - 2
                          temp = abs( a( i+j*lda ) )
                          if( value < temp .or. stdlib_disnan( temp ) )value = temp
                          ! j=k-1
                       end do
                       ! i=n-1 -> u(n-1,n-1)
                       temp = abs( real( a( i+j*lda ),KIND=dp) )
                          if( value < temp .or. stdlib_disnan( temp ) )value = temp
                       i = n
                       ! -> u(k-1,k-1)
                       temp = abs( real( a( i+j*lda ),KIND=dp) )
                          if( value < temp .or. stdlib_disnan( temp ) )value = temp
                    end if
                 else
                    ! xpose case; a is k by n+1
                    if( ilu==1_ilp ) then
                       ! uplo ='l'
                       j = 0_ilp
                       ! -> l(k,k) at a(0,0)
                       temp = abs( real( a( j+j*lda ),KIND=dp) )
                       if( value < temp .or. stdlib_disnan( temp ) )value = temp
                       do i = 1, k - 1
                          temp = abs( a( i+j*lda ) )
                          if( value < temp .or. stdlib_disnan( temp ) )value = temp
                       end do
                       do j = 1, k - 1
                          do i = 0, j - 2
                             temp = abs( a( i+j*lda ) )
                             if( value < temp .or. stdlib_disnan( temp ) )value = temp
                          end do
                          i = j - 1_ilp
                          ! l(i,i)
                          temp = abs( real( a( i+j*lda ),KIND=dp) )
                          if( value < temp .or. stdlib_disnan( temp ) )value = temp
                          i = j
                          ! l(j+k,j+k)
                          temp = abs( real( a( i+j*lda ),KIND=dp) )
                          if( value < temp .or. stdlib_disnan( temp ) )value = temp
                          do i = j + 1, k - 1
                             temp = abs( a( i+j*lda ) )
                             if( value < temp .or. stdlib_disnan( temp ) )value = temp
                          end do
                       end do
                       j = k
                       do i = 0, k - 2
                          temp = abs( a( i+j*lda ) )
                          if( value < temp .or. stdlib_disnan( temp ) )value = temp
                       end do
                       i = k - 1_ilp
                       ! -> l(i,i) is at a(i,j)
                       temp = abs( real( a( i+j*lda ),KIND=dp) )
                       if( value < temp .or. stdlib_disnan( temp ) )value = temp
                       do j = k + 1, n
                          do i = 0, k - 1
                             temp = abs( a( i+j*lda ) )
                             if( value < temp .or. stdlib_disnan( temp ) )value = temp
                          end do
                       end do
                    else
                       ! uplo = 'u'
                       do j = 0, k - 1
                          do i = 0, k - 1
                             temp = abs( a( i+j*lda ) )
                             if( value < temp .or. stdlib_disnan( temp ) )value = temp
                          end do
                       end do
                       j = k
                       ! -> u(j,j) is at a(0,j)
                       temp = abs( real( a( 0_ilp+j*lda ),KIND=dp) )
                       if( value < temp .or. stdlib_disnan( temp ) )value = temp
                       do i = 1, k - 1
                          temp = abs( a( i+j*lda ) )
                          if( value < temp .or. stdlib_disnan( temp ) )value = temp
                       end do
                       do j = k + 1, n - 1
                          do i = 0, j - k - 2
                             temp = abs( a( i+j*lda ) )
                             if( value < temp .or. stdlib_disnan( temp ) )value = temp
                          end do
                          i = j - k - 1_ilp
                          ! -> u(i,i) at a(i,j)
                          temp = abs( real( a( i+j*lda ),KIND=dp) )
                          if( value < temp .or. stdlib_disnan( temp ) )value = temp
                          i = j - k
                          ! u(j,j)
                          temp = abs( real( a( i+j*lda ),KIND=dp) )
                          if( value < temp .or. stdlib_disnan( temp ) )value = temp
                          do i = j - k + 1, k - 1
                             temp = abs( a( i+j*lda ) )
                             if( value < temp .or. stdlib_disnan( temp ) )value = temp
                          end do
                       end do
                       j = n
                       do i = 0, k - 2
                          temp = abs( a( i+j*lda ) )
                          if( value < temp .or. stdlib_disnan( temp ) )value = temp
                       end do
                       i = k - 1_ilp
                       ! u(k,k) at a(i,j)
                       temp = abs( real( a( i+j*lda ),KIND=dp) )
                       if( value < temp .or. stdlib_disnan( temp ) )value = temp
                    end if
                 end if
              end if
           else if( ( stdlib_lsame( norm, 'I' ) ) .or. ( stdlib_lsame( norm, 'O' ) ) .or.( &
                     norm=='1' ) ) then
             ! find normi(a) ( = norm1(a), since a is hermitian).
              if( ifm==1_ilp ) then
                 ! a is 'n'
                 k = n / 2_ilp
                 if( noe==1_ilp ) then
                    ! n is odd
                    if( ilu==0_ilp ) then
                       ! uplo = 'u'
                       do i = 0, k - 1
                          work( i ) = zero
                       end do
                       do j = 0, k
                          s = zero
                          do i = 0, k + j - 1
                             aa = abs( a( i+j*lda ) )
                             ! -> a(i,j+k)
                             s = s + aa
                             work( i ) = work( i ) + aa
                          end do
                          aa = abs( real( a( i+j*lda ),KIND=dp) )
                          ! -> a(j+k,j+k)
                          work( j+k ) = s + aa
                          if( i==k+k )go to 10
                          i = i + 1_ilp
                          aa = abs( real( a( i+j*lda ),KIND=dp) )
                          ! -> a(j,j)
                          work( j ) = work( j ) + aa
                          s = zero
                          do l = j + 1, k - 1
                             i = i + 1_ilp
                             aa = abs( a( i+j*lda ) )
                             ! -> a(l,j)
                             s = s + aa
                             work( l ) = work( l ) + aa
                          end do
                          work( j ) = work( j ) + s
                       end do
                       10 continue
                       value = work( 0_ilp )
                       do i = 1, n-1
                          temp = work( i )
                          if( value < temp .or. stdlib_disnan( temp ) )value = temp
                       end do
                    else
                       ! ilu = 1
                       k = k + 1_ilp
                       ! k=(n+1)/2 for n odd and ilu=1
                       do i = k, n - 1
                          work( i ) = zero
                       end do
                       do j = k - 1, 0, -1
                          s = zero
                          do i = 0, j - 2
                             aa = abs( a( i+j*lda ) )
                             ! -> a(j+k,i+k)
                             s = s + aa
                             work( i+k ) = work( i+k ) + aa
                          end do
                          if( j>0_ilp ) then
                             aa = abs( real( a( i+j*lda ),KIND=dp) )
                             ! -> a(j+k,j+k)
                             s = s + aa
                             work( i+k ) = work( i+k ) + s
                             ! i=j
                             i = i + 1_ilp
                          end if
                          aa = abs( real( a( i+j*lda ),KIND=dp) )
                          ! -> a(j,j)
                          work( j ) = aa
                          s = zero
                          do l = j + 1, n - 1
                             i = i + 1_ilp
                             aa = abs( a( i+j*lda ) )
                             ! -> a(l,j)
                             s = s + aa
                             work( l ) = work( l ) + aa
                          end do
                          work( j ) = work( j ) + s
                       end do
                       value = work( 0_ilp )
                       do i = 1, n-1
                          temp = work( i )
                          if( value < temp .or. stdlib_disnan( temp ) )value = temp
                       end do
                    end if
                 else
                    ! n is even
                    if( ilu==0_ilp ) then
                       ! uplo = 'u'
                       do i = 0, k - 1
                          work( i ) = zero
                       end do
                       do j = 0, k - 1
                          s = zero
                          do i = 0, k + j - 1
                             aa = abs( a( i+j*lda ) )
                             ! -> a(i,j+k)
                             s = s + aa
                             work( i ) = work( i ) + aa
                          end do
                          aa = abs( real( a( i+j*lda ),KIND=dp) )
                          ! -> a(j+k,j+k)
                          work( j+k ) = s + aa
                          i = i + 1_ilp
                          aa = abs( real( a( i+j*lda ),KIND=dp) )
                          ! -> a(j,j)
                          work( j ) = work( j ) + aa
                          s = zero
                          do l = j + 1, k - 1
                             i = i + 1_ilp
                             aa = abs( a( i+j*lda ) )
                             ! -> a(l,j)
                             s = s + aa
                             work( l ) = work( l ) + aa
                          end do
                          work( j ) = work( j ) + s
                       end do
                       value = work( 0_ilp )
                       do i = 1, n-1
                          temp = work( i )
                          if( value < temp .or. stdlib_disnan( temp ) )value = temp
                       end do
                    else
                       ! ilu = 1
                       do i = k, n - 1
                          work( i ) = zero
                       end do
                       do j = k - 1, 0, -1
                          s = zero
                          do i = 0, j - 1
                             aa = abs( a( i+j*lda ) )
                             ! -> a(j+k,i+k)
                             s = s + aa
                             work( i+k ) = work( i+k ) + aa
                          end do
                          aa = abs( real( a( i+j*lda ),KIND=dp) )
                          ! -> a(j+k,j+k)
                          s = s + aa
                          work( i+k ) = work( i+k ) + s
                          ! i=j
                          i = i + 1_ilp
                          aa = abs( real( a( i+j*lda ),KIND=dp) )
                          ! -> a(j,j)
                          work( j ) = aa
                          s = zero
                          do l = j + 1, n - 1
                             i = i + 1_ilp
                             aa = abs( a( i+j*lda ) )
                             ! -> a(l,j)
                             s = s + aa
                             work( l ) = work( l ) + aa
                          end do
                          work( j ) = work( j ) + s
                       end do
                       value = work( 0_ilp )
                       do i = 1, n-1
                          temp = work( i )
                          if( value < temp .or. stdlib_disnan( temp ) )value = temp
                       end do
                    end if
                 end if
              else
                 ! ifm=0
                 k = n / 2_ilp
                 if( noe==1_ilp ) then
                    ! n is odd
                    if( ilu==0_ilp ) then
                       ! uplo = 'u'
                       n1 = k
                       ! n/2
                       k = k + 1_ilp
                       ! k is the row size and lda
                       do i = n1, n - 1
                          work( i ) = zero
                       end do
                       do j = 0, n1 - 1
                          s = zero
                          do i = 0, k - 1
                             aa = abs( a( i+j*lda ) )
                             ! a(j,n1+i)
                             work( i+n1 ) = work( i+n1 ) + aa
                             s = s + aa
                          end do
                          work( j ) = s
                       end do
                       ! j=n1=k-1 is special
                       s = abs( real( a( 0_ilp+j*lda ),KIND=dp) )
                       ! a(k-1,k-1)
                       do i = 1, k - 1
                          aa = abs( a( i+j*lda ) )
                          ! a(k-1,i+n1)
                          work( i+n1 ) = work( i+n1 ) + aa
                          s = s + aa
                       end do
                       work( j ) = work( j ) + s
                       do j = k, n - 1
                          s = zero
                          do i = 0, j - k - 1
                             aa = abs( a( i+j*lda ) )
                             ! a(i,j-k)
                             work( i ) = work( i ) + aa
                             s = s + aa
                          end do
                          ! i=j-k
                          aa = abs( real( a( i+j*lda ),KIND=dp) )
                          ! a(j-k,j-k)
                          s = s + aa
                          work( j-k ) = work( j-k ) + s
                          i = i + 1_ilp
                          s = abs( real( a( i+j*lda ),KIND=dp) )
                          ! a(j,j)
                          do l = j + 1, n - 1
                             i = i + 1_ilp
                             aa = abs( a( i+j*lda ) )
                             ! a(j,l)
                             work( l ) = work( l ) + aa
                             s = s + aa
                          end do
                          work( j ) = work( j ) + s
                       end do
                       value = work( 0_ilp )
                       do i = 1, n-1
                          temp = work( i )
                          if( value < temp .or. stdlib_disnan( temp ) )value = temp
                       end do
                    else
                       ! ilu=1
                       k = k + 1_ilp
                       ! k=(n+1)/2 for n odd and ilu=1
                       do i = k, n - 1
                          work( i ) = zero
                       end do
                       do j = 0, k - 2
                          ! process
                          s = zero
                          do i = 0, j - 1
                             aa = abs( a( i+j*lda ) )
                             ! a(j,i)
                             work( i ) = work( i ) + aa
                             s = s + aa
                          end do
                          aa = abs( real( a( i+j*lda ),KIND=dp) )
                          ! i=j so process of a(j,j)
                          s = s + aa
                          work( j ) = s
                          ! is initialised here
                          i = i + 1_ilp
                          ! i=j process a(j+k,j+k)
                          aa = abs( real( a( i+j*lda ),KIND=dp) )
                          s = aa
                          do l = k + j + 1, n - 1
                             i = i + 1_ilp
                             aa = abs( a( i+j*lda ) )
                             ! a(l,k+j)
                             s = s + aa
                             work( l ) = work( l ) + aa
                          end do
                          work( k+j ) = work( k+j ) + s
                       end do
                       ! j=k-1 is special :process col a(k-1,0:k-1)
                       s = zero
                       do i = 0, k - 2
                          aa = abs( a( i+j*lda ) )
                          ! a(k,i)
                          work( i ) = work( i ) + aa
                          s = s + aa
                       end do
                       ! i=k-1
                       aa = abs( real( a( i+j*lda ),KIND=dp) )
                       ! a(k-1,k-1)
                       s = s + aa
                       work( i ) = s
                       ! done with col j=k+1
                       do j = k, n - 1
                          ! process col j of a = a(j,0:k-1)
                          s = zero
                          do i = 0, k - 1
                             aa = abs( a( i+j*lda ) )
                             ! a(j,i)
                             work( i ) = work( i ) + aa
                             s = s + aa
                          end do
                          work( j ) = work( j ) + s
                       end do
                       value = work( 0_ilp )
                       do i = 1, n-1
                          temp = work( i )
                          if( value < temp .or. stdlib_disnan( temp ) )value = temp
                       end do
                    end if
                 else
                    ! n is even
                    if( ilu==0_ilp ) then
                       ! uplo = 'u'
                       do i = k, n - 1
                          work( i ) = zero
                       end do
                       do j = 0, k - 1
                          s = zero
                          do i = 0, k - 1
                             aa = abs( a( i+j*lda ) )
                             ! a(j,i+k)
                             work( i+k ) = work( i+k ) + aa
                             s = s + aa
                          end do
                          work( j ) = s
                       end do
                       ! j=k
                       aa = abs( real( a( 0_ilp+j*lda ),KIND=dp) )
                       ! a(k,k)
                       s = aa
                       do i = 1, k - 1
                          aa = abs( a( i+j*lda ) )
                          ! a(k,k+i)
                          work( i+k ) = work( i+k ) + aa
                          s = s + aa
                       end do
                       work( j ) = work( j ) + s
                       do j = k + 1, n - 1
                          s = zero
                          do i = 0, j - 2 - k
                             aa = abs( a( i+j*lda ) )
                             ! a(i,j-k-1)
                             work( i ) = work( i ) + aa
                             s = s + aa
                          end do
                          ! i=j-1-k
                          aa = abs( real( a( i+j*lda ),KIND=dp) )
                          ! a(j-k-1,j-k-1)
                          s = s + aa
                          work( j-k-1 ) = work( j-k-1 ) + s
                          i = i + 1_ilp
                          aa = abs( real( a( i+j*lda ),KIND=dp) )
                          ! a(j,j)
                          s = aa
                          do l = j + 1, n - 1
                             i = i + 1_ilp
                             aa = abs( a( i+j*lda ) )
                             ! a(j,l)
                             work( l ) = work( l ) + aa
                             s = s + aa
                          end do
                          work( j ) = work( j ) + s
                       end do
                       ! j=n
                       s = zero
                       do i = 0, k - 2
                          aa = abs( a( i+j*lda ) )
                          ! a(i,k-1)
                          work( i ) = work( i ) + aa
                          s = s + aa
                       end do
                       ! i=k-1
                       aa = abs( real( a( i+j*lda ),KIND=dp) )
                       ! a(k-1,k-1)
                       s = s + aa
                       work( i ) = work( i ) + s
                       value = work( 0_ilp )
                       do i = 1, n-1
                          temp = work( i )
                          if( value < temp .or. stdlib_disnan( temp ) )value = temp
                       end do
                    else
                       ! ilu=1
                       do i = k, n - 1
                          work( i ) = zero
                       end do
                       ! j=0 is special :process col a(k:n-1,k)
                       s = abs( real( a( 0_ilp ),KIND=dp) )
                       ! a(k,k)
                       do i = 1, k - 1
                          aa = abs( a( i ) )
                          ! a(k+i,k)
                          work( i+k ) = work( i+k ) + aa
                          s = s + aa
                       end do
                       work( k ) = work( k ) + s
                       do j = 1, k - 1
                          ! process
                          s = zero
                          do i = 0, j - 2
                             aa = abs( a( i+j*lda ) )
                             ! a(j-1,i)
                             work( i ) = work( i ) + aa
                             s = s + aa
                          end do
                          aa = abs( real( a( i+j*lda ),KIND=dp) )
                          ! i=j-1 so process of a(j-1,j-1)
                          s = s + aa
                          work( j-1 ) = s
                          ! is initialised here
                          i = i + 1_ilp
                          ! i=j process a(j+k,j+k)
                          aa = abs( real( a( i+j*lda ),KIND=dp) )
                          s = aa
                          do l = k + j + 1, n - 1
                             i = i + 1_ilp
                             aa = abs( a( i+j*lda ) )
                             ! a(l,k+j)
                             s = s + aa
                             work( l ) = work( l ) + aa
                          end do
                          work( k+j ) = work( k+j ) + s
                       end do
                       ! j=k is special :process col a(k,0:k-1)
                       s = zero
                       do i = 0, k - 2
                          aa = abs( a( i+j*lda ) )
                          ! a(k,i)
                          work( i ) = work( i ) + aa
                          s = s + aa
                       end do
                       ! i=k-1
                       aa = abs( real( a( i+j*lda ),KIND=dp) )
                       ! a(k-1,k-1)
                       s = s + aa
                       work( i ) = s
                       ! done with col j=k+1
                       do j = k + 1, n
                          ! process col j-1 of a = a(j-1,0:k-1)
                          s = zero
                          do i = 0, k - 1
                             aa = abs( a( i+j*lda ) )
                             ! a(j-1,i)
                             work( i ) = work( i ) + aa
                             s = s + aa
                          end do
                          work( j-1 ) = work( j-1 ) + s
                       end do
                       value = work( 0_ilp )
                       do i = 1, n-1
                          temp = work( i )
                          if( value < temp .or. stdlib_disnan( temp ) )value = temp
                       end do
                    end if
                 end if
              end if
           else if( ( stdlib_lsame( norm, 'F' ) ) .or. ( stdlib_lsame( norm, 'E' ) ) ) &
                     then
             ! find normf(a).
              k = ( n+1 ) / 2_ilp
              scale = zero
              s = one
              if( noe==1_ilp ) then
                 ! n is odd
                 if( ifm==1_ilp ) then
                    ! a is normal
                    if( ilu==0_ilp ) then
                       ! a is upper
                       do j = 0, k - 3
                          call stdlib_zlassq( k-j-2, a( k+j+1+j*lda ), 1_ilp, scale, s )
                          ! l at a(k,0)
                       end do
                       do j = 0, k - 1
                          call stdlib_zlassq( k+j-1, a( 0_ilp+j*lda ), 1_ilp, scale, s )
                          ! trap u at a(0,0)
                       end do
                       s = s + s
                       ! double s for the off diagonal elements
                       l = k - 1_ilp
                       ! -> u(k,k) at a(k-1,0)
                       do i = 0, k - 2
                          aa = real( a( l ),KIND=dp)
                          ! u(k+i,k+i)
                          if( aa/=zero ) then
                             if( scale<aa ) then
                                s = one + s*( scale / aa )**2_ilp
                                scale = aa
                             else
                                s = s + ( aa / scale )**2_ilp
                             end if
                          end if
                          aa = real( a( l+1 ),KIND=dp)
                          ! u(i,i)
                          if( aa/=zero ) then
                             if( scale<aa ) then
                                s = one + s*( scale / aa )**2_ilp
                                scale = aa
                             else
                                s = s + ( aa / scale )**2_ilp
                             end if
                          end if
                          l = l + lda + 1_ilp
                       end do
                       aa = real( a( l ),KIND=dp)
                       ! u(n-1,n-1)
                       if( aa/=zero ) then
                          if( scale<aa ) then
                             s = one + s*( scale / aa )**2_ilp
                             scale = aa
                          else
                             s = s + ( aa / scale )**2_ilp
                          end if
                       end if
                    else
                       ! ilu=1
                       do j = 0, k - 1
                          call stdlib_zlassq( n-j-1, a( j+1+j*lda ), 1_ilp, scale, s )
                          ! trap l at a(0,0)
                       end do
                       do j = 1, k - 2
                          call stdlib_zlassq( j, a( 0_ilp+( 1_ilp+j )*lda ), 1_ilp, scale, s )
                          ! u at a(0,1)
                       end do
                       s = s + s
                       ! double s for the off diagonal elements
                       aa = real( a( 0_ilp ),KIND=dp)
                       ! l(0,0) at a(0,0)
                       if( aa/=zero ) then
                          if( scale<aa ) then
                             s = one + s*( scale / aa )**2_ilp
                             scale = aa
                          else
                             s = s + ( aa / scale )**2_ilp
                          end if
                       end if
                       l = lda
                       ! -> l(k,k) at a(0,1)
                       do i = 1, k - 1
                          aa = real( a( l ),KIND=dp)
                          ! l(k-1+i,k-1+i)
                          if( aa/=zero ) then
                             if( scale<aa ) then
                                s = one + s*( scale / aa )**2_ilp
                                scale = aa
                             else
                                s = s + ( aa / scale )**2_ilp
                             end if
                          end if
                          aa = real( a( l+1 ),KIND=dp)
                          ! l(i,i)
                          if( aa/=zero ) then
                             if( scale<aa ) then
                                s = one + s*( scale / aa )**2_ilp
                                scale = aa
                             else
                                s = s + ( aa / scale )**2_ilp
                             end if
                          end if
                          l = l + lda + 1_ilp
                       end do
                    end if
                 else
                    ! a is xpose
                    if( ilu==0_ilp ) then
                       ! a**h is upper
                       do j = 1, k - 2
                          call stdlib_zlassq( j, a( 0_ilp+( k+j )*lda ), 1_ilp, scale, s )
                          ! u at a(0,k)
                       end do
                       do j = 0, k - 2
                          call stdlib_zlassq( k, a( 0_ilp+j*lda ), 1_ilp, scale, s )
                          ! k by k-1 rect. at a(0,0)
                       end do
                       do j = 0, k - 2
                          call stdlib_zlassq( k-j-1, a( j+1+( j+k-1 )*lda ), 1_ilp,scale, s )
                          ! l at a(0,k-1)
                       end do
                       s = s + s
                       ! double s for the off diagonal elements
                       l = 0_ilp + k*lda - lda
                       ! -> u(k-1,k-1) at a(0,k-1)
                       aa = real( a( l ),KIND=dp)
                       ! u(k-1,k-1)
                       if( aa/=zero ) then
                          if( scale<aa ) then
                             s = one + s*( scale / aa )**2_ilp
                             scale = aa
                          else
                             s = s + ( aa / scale )**2_ilp
                          end if
                       end if
                       l = l + lda
                       ! -> u(0,0) at a(0,k)
                       do j = k, n - 1
                          aa = real( a( l ),KIND=dp)
                          ! -> u(j-k,j-k)
                          if( aa/=zero ) then
                             if( scale<aa ) then
                                s = one + s*( scale / aa )**2_ilp
                                scale = aa
                             else
                                s = s + ( aa / scale )**2_ilp
                             end if
                          end if
                          aa = real( a( l+1 ),KIND=dp)
                          ! -> u(j,j)
                          if( aa/=zero ) then
                             if( scale<aa ) then
                                s = one + s*( scale / aa )**2_ilp
                                scale = aa
                             else
                                s = s + ( aa / scale )**2_ilp
                             end if
                          end if
                          l = l + lda + 1_ilp
                       end do
                    else
                       ! a**h is lower
                       do j = 1, k - 1
                          call stdlib_zlassq( j, a( 0_ilp+j*lda ), 1_ilp, scale, s )
                          ! u at a(0,0)
                       end do
                       do j = k, n - 1
                          call stdlib_zlassq( k, a( 0_ilp+j*lda ), 1_ilp, scale, s )
                          ! k by k-1 rect. at a(0,k)
                       end do
                       do j = 0, k - 3
                          call stdlib_zlassq( k-j-2, a( j+2+j*lda ), 1_ilp, scale, s )
                          ! l at a(1,0)
                       end do
                       s = s + s
                       ! double s for the off diagonal elements
                       l = 0_ilp
                       ! -> l(0,0) at a(0,0)
                       do i = 0, k - 2
                          aa = real( a( l ),KIND=dp)
                          ! l(i,i)
                          if( aa/=zero ) then
                             if( scale<aa ) then
                                s = one + s*( scale / aa )**2_ilp
                                scale = aa
                             else
                                s = s + ( aa / scale )**2_ilp
                             end if
                          end if
                          aa = real( a( l+1 ),KIND=dp)
                          ! l(k+i,k+i)
                          if( aa/=zero ) then
                             if( scale<aa ) then
                                s = one + s*( scale / aa )**2_ilp
                                scale = aa
                             else
                                s = s + ( aa / scale )**2_ilp
                             end if
                          end if
                          l = l + lda + 1_ilp
                       end do
                       ! l-> k-1 + (k-1)*lda or l(k-1,k-1) at a(k-1,k-1)
                       aa = real( a( l ),KIND=dp)
                       ! l(k-1,k-1) at a(k-1,k-1)
                       if( aa/=zero ) then
                          if( scale<aa ) then
                             s = one + s*( scale / aa )**2_ilp
                             scale = aa
                          else
                             s = s + ( aa / scale )**2_ilp
                          end if
                       end if
                    end if
                 end if
              else
                 ! n is even
                 if( ifm==1_ilp ) then
                    ! a is normal
                    if( ilu==0_ilp ) then
                       ! a is upper
                       do j = 0, k - 2
                          call stdlib_zlassq( k-j-1, a( k+j+2+j*lda ), 1_ilp, scale, s )
                       ! l at a(k+1,0)
                       end do
                       do j = 0, k - 1
                          call stdlib_zlassq( k+j, a( 0_ilp+j*lda ), 1_ilp, scale, s )
                       ! trap u at a(0,0)
                       end do
                       s = s + s
                       ! double s for the off diagonal elements
                       l = k
                       ! -> u(k,k) at a(k,0)
                       do i = 0, k - 1
                          aa = real( a( l ),KIND=dp)
                          ! u(k+i,k+i)
                          if( aa/=zero ) then
                             if( scale<aa ) then
                                s = one + s*( scale / aa )**2_ilp
                                scale = aa
                             else
                                s = s + ( aa / scale )**2_ilp
                             end if
                          end if
                          aa = real( a( l+1 ),KIND=dp)
                          ! u(i,i)
                          if( aa/=zero ) then
                             if( scale<aa ) then
                                s = one + s*( scale / aa )**2_ilp
                                scale = aa
                             else
                                s = s + ( aa / scale )**2_ilp
                             end if
                          end if
                          l = l + lda + 1_ilp
                       end do
                    else
                       ! ilu=1
                       do j = 0, k - 1
                          call stdlib_zlassq( n-j-1, a( j+2+j*lda ), 1_ilp, scale, s )
                          ! trap l at a(1,0)
                       end do
                       do j = 1, k - 1
                          call stdlib_zlassq( j, a( 0_ilp+j*lda ), 1_ilp, scale, s )
                          ! u at a(0,0)
                       end do
                       s = s + s
                       ! double s for the off diagonal elements
                       l = 0_ilp
                       ! -> l(k,k) at a(0,0)
                       do i = 0, k - 1
                          aa = real( a( l ),KIND=dp)
                          ! l(k-1+i,k-1+i)
                          if( aa/=zero ) then
                             if( scale<aa ) then
                                s = one + s*( scale / aa )**2_ilp
                                scale = aa
                             else
                                s = s + ( aa / scale )**2_ilp
                             end if
                          end if
                          aa = real( a( l+1 ),KIND=dp)
                          ! l(i,i)
                          if( aa/=zero ) then
                             if( scale<aa ) then
                                s = one + s*( scale / aa )**2_ilp
                                scale = aa
                             else
                                s = s + ( aa / scale )**2_ilp
                             end if
                          end if
                          l = l + lda + 1_ilp
                       end do
                    end if
                 else
                    ! a is xpose
                    if( ilu==0_ilp ) then
                       ! a**h is upper
                       do j = 1, k - 1
                          call stdlib_zlassq( j, a( 0_ilp+( k+1+j )*lda ), 1_ilp, scale, s )
                       ! u at a(0,k+1)
                       end do
                       do j = 0, k - 1
                          call stdlib_zlassq( k, a( 0_ilp+j*lda ), 1_ilp, scale, s )
                       ! k by k rect. at a(0,0)
                       end do
                       do j = 0, k - 2
                          call stdlib_zlassq( k-j-1, a( j+1+( j+k )*lda ), 1_ilp, scale,s )
                       ! l at a(0,k)
                       end do
                       s = s + s
                       ! double s for the off diagonal elements
                       l = 0_ilp + k*lda
                       ! -> u(k,k) at a(0,k)
                       aa = real( a( l ),KIND=dp)
                       ! u(k,k)
                       if( aa/=zero ) then
                          if( scale<aa ) then
                             s = one + s*( scale / aa )**2_ilp
                             scale = aa
                          else
                             s = s + ( aa / scale )**2_ilp
                          end if
                       end if
                       l = l + lda
                       ! -> u(0,0) at a(0,k+1)
                       do j = k + 1, n - 1
                          aa = real( a( l ),KIND=dp)
                          ! -> u(j-k-1,j-k-1)
                          if( aa/=zero ) then
                             if( scale<aa ) then
                                s = one + s*( scale / aa )**2_ilp
                                scale = aa
                             else
                                s = s + ( aa / scale )**2_ilp
                             end if
                          end if
                          aa = real( a( l+1 ),KIND=dp)
                          ! -> u(j,j)
                          if( aa/=zero ) then
                             if( scale<aa ) then
                                s = one + s*( scale / aa )**2_ilp
                                scale = aa
                             else
                                s = s + ( aa / scale )**2_ilp
                             end if
                          end if
                          l = l + lda + 1_ilp
                       end do
                       ! l=k-1+n*lda
                       ! -> u(k-1,k-1) at a(k-1,n)
                       aa = real( a( l ),KIND=dp)
                       ! u(k,k)
                       if( aa/=zero ) then
                          if( scale<aa ) then
                             s = one + s*( scale / aa )**2_ilp
                             scale = aa
                          else
                             s = s + ( aa / scale )**2_ilp
                          end if
                       end if
                    else
                       ! a**h is lower
                       do j = 1, k - 1
                          call stdlib_zlassq( j, a( 0_ilp+( j+1 )*lda ), 1_ilp, scale, s )
                       ! u at a(0,1)
                       end do
                       do j = k + 1, n
                          call stdlib_zlassq( k, a( 0_ilp+j*lda ), 1_ilp, scale, s )
                       ! k by k rect. at a(0,k+1)
                       end do
                       do j = 0, k - 2
                          call stdlib_zlassq( k-j-1, a( j+1+j*lda ), 1_ilp, scale, s )
                       ! l at a(0,0)
                       end do
                       s = s + s
                       ! double s for the off diagonal elements
                       l = 0_ilp
                       ! -> l(k,k) at a(0,0)
                       aa = real( a( l ),KIND=dp)
                       ! l(k,k) at a(0,0)
                       if( aa/=zero ) then
                          if( scale<aa ) then
                             s = one + s*( scale / aa )**2_ilp
                             scale = aa
                          else
                             s = s + ( aa / scale )**2_ilp
                          end if
                       end if
                       l = lda
                       ! -> l(0,0) at a(0,1)
                       do i = 0, k - 2
                          aa = real( a( l ),KIND=dp)
                          ! l(i,i)
                          if( aa/=zero ) then
                             if( scale<aa ) then
                                s = one + s*( scale / aa )**2_ilp
                                scale = aa
                             else
                                s = s + ( aa / scale )**2_ilp
                             end if
                          end if
                          aa = real( a( l+1 ),KIND=dp)
                          ! l(k+i+1,k+i+1)
                          if( aa/=zero ) then
                             if( scale<aa ) then
                                s = one + s*( scale / aa )**2_ilp
                                scale = aa
                             else
                                s = s + ( aa / scale )**2_ilp
                             end if
                          end if
                          l = l + lda + 1_ilp
                       end do
                       ! l-> k - 1 + k*lda or l(k-1,k-1) at a(k-1,k)
                       aa = real( a( l ),KIND=dp)
                       ! l(k-1,k-1) at a(k-1,k)
                       if( aa/=zero ) then
                          if( scale<aa ) then
                             s = one + s*( scale / aa )**2_ilp
                             scale = aa
                          else
                             s = s + ( aa / scale )**2_ilp
                          end if
                       end if
                    end if
                 end if
              end if
              value = scale*sqrt( s )
           end if
           stdlib_zlanhf = value
           return
     end function stdlib_zlanhf




     real(sp) module function stdlib_slansf( norm, transr, uplo, n, a, work )
     !! SLANSF returns the value of the one norm, or the Frobenius norm, or
     !! the infinity norm, or the element of largest absolute value of a
     !! real symmetric matrix A in RFP format.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: norm, transr, uplo
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           real(sp), intent(in) :: a(0_ilp:*)
           real(sp), intent(out) :: work(0_ilp:*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j, ifm, ilu, noe, n1, k, l, lda
           real(sp) :: scale, s, value, aa, temp
           ! Intrinsic Functions 
           ! Executable Statements 
           if( n==0_ilp ) then
              stdlib_slansf = zero
              return
           else if( n==1_ilp ) then
              stdlib_slansf = abs( a(0_ilp) )
              return
           end if
           ! set noe = 1 if n is odd. if n is even set noe=0
           noe = 1_ilp
           if( mod( n, 2_ilp )==0_ilp )noe = 0_ilp
           ! set ifm = 0 when form='t or 't' and 1 otherwise
           ifm = 1_ilp
           if( stdlib_lsame( transr, 'T' ) )ifm = 0_ilp
           ! set ilu = 0 when uplo='u or 'u' and 1 otherwise
           ilu = 1_ilp
           if( stdlib_lsame( uplo, 'U' ) )ilu = 0_ilp
           ! set lda = (n+1)/2 when ifm = 0
           ! set lda = n when ifm = 1 and noe = 1
           ! set lda = n+1 when ifm = 1 and noe = 0
           if( ifm==1_ilp ) then
              if( noe==1_ilp ) then
                 lda = n
              else
                 ! noe=0
                 lda = n + 1_ilp
              end if
           else
              ! ifm=0
              lda = ( n+1 ) / 2_ilp
           end if
           if( stdlib_lsame( norm, 'M' ) ) then
             ! find max(abs(a(i,j))).
              k = ( n+1 ) / 2_ilp
              value = zero
              if( noe==1_ilp ) then
                 ! n is odd
                 if( ifm==1_ilp ) then
                 ! a is n by k
                    do j = 0, k - 1
                       do i = 0, n - 1
                          temp = abs( a( i+j*lda ) )
                          if( value < temp .or. stdlib_sisnan( temp ) )value = temp
                       end do
                    end do
                 else
                    ! xpose case; a is k by n
                    do j = 0, n - 1
                       do i = 0, k - 1
                          temp = abs( a( i+j*lda ) )
                          if( value < temp .or. stdlib_sisnan( temp ) )value = temp
                       end do
                    end do
                 end if
              else
                 ! n is even
                 if( ifm==1_ilp ) then
                    ! a is n+1 by k
                    do j = 0, k - 1
                       do i = 0, n
                          temp = abs( a( i+j*lda ) )
                          if( value < temp .or. stdlib_sisnan( temp ) )value = temp
                       end do
                    end do
                 else
                    ! xpose case; a is k by n+1
                    do j = 0, n
                       do i = 0, k - 1
                          temp = abs( a( i+j*lda ) )
                          if( value < temp .or. stdlib_sisnan( temp ) )value = temp
                       end do
                    end do
                 end if
              end if
           else if( ( stdlib_lsame( norm, 'I' ) ) .or. ( stdlib_lsame( norm, 'O' ) ) .or.( &
                     norm=='1' ) ) then
              ! find normi(a) ( = norm1(a), since a is symmetric).
              if( ifm==1_ilp ) then
                 k = n / 2_ilp
                 if( noe==1_ilp ) then
                    ! n is odd
                    if( ilu==0_ilp ) then
                       do i = 0, k - 1
                          work( i ) = zero
                       end do
                       do j = 0, k
                          s = zero
                          do i = 0, k + j - 1
                             aa = abs( a( i+j*lda ) )
                             ! -> a(i,j+k)
                             s = s + aa
                             work( i ) = work( i ) + aa
                          end do
                          aa = abs( a( i+j*lda ) )
                          ! -> a(j+k,j+k)
                          work( j+k ) = s + aa
                          if( i==k+k )go to 10
                          i = i + 1_ilp
                          aa = abs( a( i+j*lda ) )
                          ! -> a(j,j)
                          work( j ) = work( j ) + aa
                          s = zero
                          do l = j + 1, k - 1
                             i = i + 1_ilp
                             aa = abs( a( i+j*lda ) )
                             ! -> a(l,j)
                             s = s + aa
                             work( l ) = work( l ) + aa
                          end do
                          work( j ) = work( j ) + s
                       end do
                       10 continue
                       value = work( 0_ilp )
                       do i = 1, n-1
                          temp = work( i )
                          if( value < temp .or. stdlib_sisnan( temp ) )value = temp
                       end do
                    else
                       ! ilu = 1
                       k = k + 1_ilp
                       ! k=(n+1)/2 for n odd and ilu=1
                       do i = k, n - 1
                          work( i ) = zero
                       end do
                       do j = k - 1, 0, -1
                          s = zero
                          do i = 0, j - 2
                             aa = abs( a( i+j*lda ) )
                             ! -> a(j+k,i+k)
                             s = s + aa
                             work( i+k ) = work( i+k ) + aa
                          end do
                          if( j>0_ilp ) then
                             aa = abs( a( i+j*lda ) )
                             ! -> a(j+k,j+k)
                             s = s + aa
                             work( i+k ) = work( i+k ) + s
                             ! i=j
                             i = i + 1_ilp
                          end if
                          aa = abs( a( i+j*lda ) )
                          ! -> a(j,j)
                          work( j ) = aa
                          s = zero
                          do l = j + 1, n - 1
                             i = i + 1_ilp
                             aa = abs( a( i+j*lda ) )
                             ! -> a(l,j)
                             s = s + aa
                             work( l ) = work( l ) + aa
                          end do
                          work( j ) = work( j ) + s
                       end do
                       value = work( 0_ilp )
                       do i = 1, n-1
                          temp = work( i )
                          if( value < temp .or. stdlib_sisnan( temp ) )value = temp
                       end do
                    end if
                 else
                    ! n is even
                    if( ilu==0_ilp ) then
                       do i = 0, k - 1
                          work( i ) = zero
                       end do
                       do j = 0, k - 1
                          s = zero
                          do i = 0, k + j - 1
                             aa = abs( a( i+j*lda ) )
                             ! -> a(i,j+k)
                             s = s + aa
                             work( i ) = work( i ) + aa
                          end do
                          aa = abs( a( i+j*lda ) )
                          ! -> a(j+k,j+k)
                          work( j+k ) = s + aa
                          i = i + 1_ilp
                          aa = abs( a( i+j*lda ) )
                          ! -> a(j,j)
                          work( j ) = work( j ) + aa
                          s = zero
                          do l = j + 1, k - 1
                             i = i + 1_ilp
                             aa = abs( a( i+j*lda ) )
                             ! -> a(l,j)
                             s = s + aa
                             work( l ) = work( l ) + aa
                          end do
                          work( j ) = work( j ) + s
                       end do
                       value = work( 0_ilp )
                       do i = 1, n-1
                          temp = work( i )
                          if( value < temp .or. stdlib_sisnan( temp ) )value = temp
                       end do
                    else
                       ! ilu = 1
                       do i = k, n - 1
                          work( i ) = zero
                       end do
                       do j = k - 1, 0, -1
                          s = zero
                          do i = 0, j - 1
                             aa = abs( a( i+j*lda ) )
                             ! -> a(j+k,i+k)
                             s = s + aa
                             work( i+k ) = work( i+k ) + aa
                          end do
                          aa = abs( a( i+j*lda ) )
                          ! -> a(j+k,j+k)
                          s = s + aa
                          work( i+k ) = work( i+k ) + s
                          ! i=j
                          i = i + 1_ilp
                          aa = abs( a( i+j*lda ) )
                          ! -> a(j,j)
                          work( j ) = aa
                          s = zero
                          do l = j + 1, n - 1
                             i = i + 1_ilp
                             aa = abs( a( i+j*lda ) )
                             ! -> a(l,j)
                             s = s + aa
                             work( l ) = work( l ) + aa
                          end do
                          work( j ) = work( j ) + s
                       end do
                       value = work( 0_ilp )
                       do i = 1, n-1
                          temp = work( i )
                          if( value < temp .or. stdlib_sisnan( temp ) )value = temp
                       end do
                    end if
                 end if
              else
                 ! ifm=0
                 k = n / 2_ilp
                 if( noe==1_ilp ) then
                    ! n is odd
                    if( ilu==0_ilp ) then
                       n1 = k
                       ! n/2
                       k = k + 1_ilp
                       ! k is the row size and lda
                       do i = n1, n - 1
                          work( i ) = zero
                       end do
                       do j = 0, n1 - 1
                          s = zero
                          do i = 0, k - 1
                             aa = abs( a( i+j*lda ) )
                             ! a(j,n1+i)
                             work( i+n1 ) = work( i+n1 ) + aa
                             s = s + aa
                          end do
                          work( j ) = s
                       end do
                       ! j=n1=k-1 is special
                       s = abs( a( 0_ilp+j*lda ) )
                       ! a(k-1,k-1)
                       do i = 1, k - 1
                          aa = abs( a( i+j*lda ) )
                          ! a(k-1,i+n1)
                          work( i+n1 ) = work( i+n1 ) + aa
                          s = s + aa
                       end do
                       work( j ) = work( j ) + s
                       do j = k, n - 1
                          s = zero
                          do i = 0, j - k - 1
                             aa = abs( a( i+j*lda ) )
                             ! a(i,j-k)
                             work( i ) = work( i ) + aa
                             s = s + aa
                          end do
                          ! i=j-k
                          aa = abs( a( i+j*lda ) )
                          ! a(j-k,j-k)
                          s = s + aa
                          work( j-k ) = work( j-k ) + s
                          i = i + 1_ilp
                          s = abs( a( i+j*lda ) )
                          ! a(j,j)
                          do l = j + 1, n - 1
                             i = i + 1_ilp
                             aa = abs( a( i+j*lda ) )
                             ! a(j,l)
                             work( l ) = work( l ) + aa
                             s = s + aa
                          end do
                          work( j ) = work( j ) + s
                       end do
                       value = work( 0_ilp )
                       do i = 1, n-1
                          temp = work( i )
                          if( value < temp .or. stdlib_sisnan( temp ) )value = temp
                       end do
                    else
                       ! ilu=1
                       k = k + 1_ilp
                       ! k=(n+1)/2 for n odd and ilu=1
                       do i = k, n - 1
                          work( i ) = zero
                       end do
                       do j = 0, k - 2
                          ! process
                          s = zero
                          do i = 0, j - 1
                             aa = abs( a( i+j*lda ) )
                             ! a(j,i)
                             work( i ) = work( i ) + aa
                             s = s + aa
                          end do
                          aa = abs( a( i+j*lda ) )
                          ! i=j so process of a(j,j)
                          s = s + aa
                          work( j ) = s
                          ! is initialised here
                          i = i + 1_ilp
                          ! i=j process a(j+k,j+k)
                          aa = abs( a( i+j*lda ) )
                          s = aa
                          do l = k + j + 1, n - 1
                             i = i + 1_ilp
                             aa = abs( a( i+j*lda ) )
                             ! a(l,k+j)
                             s = s + aa
                             work( l ) = work( l ) + aa
                          end do
                          work( k+j ) = work( k+j ) + s
                       end do
                       ! j=k-1 is special :process col a(k-1,0:k-1)
                       s = zero
                       do i = 0, k - 2
                          aa = abs( a( i+j*lda ) )
                          ! a(k,i)
                          work( i ) = work( i ) + aa
                          s = s + aa
                       end do
                       ! i=k-1
                       aa = abs( a( i+j*lda ) )
                       ! a(k-1,k-1)
                       s = s + aa
                       work( i ) = s
                       ! done with col j=k+1
                       do j = k, n - 1
                          ! process col j of a = a(j,0:k-1)
                          s = zero
                          do i = 0, k - 1
                             aa = abs( a( i+j*lda ) )
                             ! a(j,i)
                             work( i ) = work( i ) + aa
                             s = s + aa
                          end do
                          work( j ) = work( j ) + s
                       end do
                       value = work( 0_ilp )
                       do i = 1, n-1
                          temp = work( i )
                          if( value < temp .or. stdlib_sisnan( temp ) )value = temp
                       end do
                    end if
                 else
                    ! n is even
                    if( ilu==0_ilp ) then
                       do i = k, n - 1
                          work( i ) = zero
                       end do
                       do j = 0, k - 1
                          s = zero
                          do i = 0, k - 1
                             aa = abs( a( i+j*lda ) )
                             ! a(j,i+k)
                             work( i+k ) = work( i+k ) + aa
                             s = s + aa
                          end do
                          work( j ) = s
                       end do
                       ! j=k
                       aa = abs( a( 0_ilp+j*lda ) )
                       ! a(k,k)
                       s = aa
                       do i = 1, k - 1
                          aa = abs( a( i+j*lda ) )
                          ! a(k,k+i)
                          work( i+k ) = work( i+k ) + aa
                          s = s + aa
                       end do
                       work( j ) = work( j ) + s
                       do j = k + 1, n - 1
                          s = zero
                          do i = 0, j - 2 - k
                             aa = abs( a( i+j*lda ) )
                             ! a(i,j-k-1)
                             work( i ) = work( i ) + aa
                             s = s + aa
                          end do
                           ! i=j-1-k
                          aa = abs( a( i+j*lda ) )
                          ! a(j-k-1,j-k-1)
                          s = s + aa
                          work( j-k-1 ) = work( j-k-1 ) + s
                          i = i + 1_ilp
                          aa = abs( a( i+j*lda ) )
                          ! a(j,j)
                          s = aa
                          do l = j + 1, n - 1
                             i = i + 1_ilp
                             aa = abs( a( i+j*lda ) )
                             ! a(j,l)
                             work( l ) = work( l ) + aa
                             s = s + aa
                          end do
                          work( j ) = work( j ) + s
                       end do
                       ! j=n
                       s = zero
                       do i = 0, k - 2
                          aa = abs( a( i+j*lda ) )
                          ! a(i,k-1)
                          work( i ) = work( i ) + aa
                          s = s + aa
                       end do
                       ! i=k-1
                       aa = abs( a( i+j*lda ) )
                       ! a(k-1,k-1)
                       s = s + aa
                       work( i ) = work( i ) + s
                       value = work ( 0_ilp )
                       do i = 1, n-1
                          temp = work( i )
                          if( value < temp .or. stdlib_sisnan( temp ) )value = temp
                       end do
                    else
                       ! ilu=1
                       do i = k, n - 1
                          work( i ) = zero
                       end do
                       ! j=0 is special :process col a(k:n-1,k)
                       s = abs( a( 0_ilp ) )
                       ! a(k,k)
                       do i = 1, k - 1
                          aa = abs( a( i ) )
                          ! a(k+i,k)
                          work( i+k ) = work( i+k ) + aa
                          s = s + aa
                       end do
                       work( k ) = work( k ) + s
                       do j = 1, k - 1
                          ! process
                          s = zero
                          do i = 0, j - 2
                             aa = abs( a( i+j*lda ) )
                             ! a(j-1,i)
                             work( i ) = work( i ) + aa
                             s = s + aa
                          end do
                          aa = abs( a( i+j*lda ) )
                          ! i=j-1 so process of a(j-1,j-1)
                          s = s + aa
                          work( j-1 ) = s
                          ! is initialised here
                          i = i + 1_ilp
                          ! i=j process a(j+k,j+k)
                          aa = abs( a( i+j*lda ) )
                          s = aa
                          do l = k + j + 1, n - 1
                             i = i + 1_ilp
                             aa = abs( a( i+j*lda ) )
                             ! a(l,k+j)
                             s = s + aa
                             work( l ) = work( l ) + aa
                          end do
                          work( k+j ) = work( k+j ) + s
                       end do
                       ! j=k is special :process col a(k,0:k-1)
                       s = zero
                       do i = 0, k - 2
                          aa = abs( a( i+j*lda ) )
                          ! a(k,i)
                          work( i ) = work( i ) + aa
                          s = s + aa
                       end do
                       ! i=k-1
                       aa = abs( a( i+j*lda ) )
                       ! a(k-1,k-1)
                       s = s + aa
                       work( i ) = s
                       ! done with col j=k+1
                       do j = k + 1, n
                          ! process col j-1 of a = a(j-1,0:k-1)
                          s = zero
                          do i = 0, k - 1
                             aa = abs( a( i+j*lda ) )
                             ! a(j-1,i)
                             work( i ) = work( i ) + aa
                             s = s + aa
                          end do
                          work( j-1 ) = work( j-1 ) + s
                       end do
                       value = work( 0_ilp )
                       do i = 1, n-1
                          temp = work( i )
                          if( value < temp .or. stdlib_sisnan( temp ) )value = temp
                       end do
                    end if
                 end if
              end if
           else if( ( stdlib_lsame( norm, 'F' ) ) .or. ( stdlib_lsame( norm, 'E' ) ) ) &
                     then
             ! find normf(a).
              k = ( n+1 ) / 2_ilp
              scale = zero
              s = one
              if( noe==1_ilp ) then
                 ! n is odd
                 if( ifm==1_ilp ) then
                    ! a is normal
                    if( ilu==0_ilp ) then
                       ! a is upper
                       do j = 0, k - 3
                          call stdlib_slassq( k-j-2, a( k+j+1+j*lda ), 1_ilp, scale, s )
                          ! l at a(k,0)
                       end do
                       do j = 0, k - 1
                          call stdlib_slassq( k+j-1, a( 0_ilp+j*lda ), 1_ilp, scale, s )
                          ! trap u at a(0,0)
                       end do
                       s = s + s
                       ! double s for the off diagonal elements
                       call stdlib_slassq( k-1, a( k ), lda+1, scale, s )
                       ! tri l at a(k,0)
                       call stdlib_slassq( k, a( k-1 ), lda+1, scale, s )
                       ! tri u at a(k-1,0)
                    else
                       ! ilu=1
                       do j = 0, k - 1
                          call stdlib_slassq( n-j-1, a( j+1+j*lda ), 1_ilp, scale, s )
                          ! trap l at a(0,0)
                       end do
                       do j = 0, k - 2
                          call stdlib_slassq( j, a( 0_ilp+( 1_ilp+j )*lda ), 1_ilp, scale, s )
                          ! u at a(0,1)
                       end do
                       s = s + s
                       ! double s for the off diagonal elements
                       call stdlib_slassq( k, a( 0_ilp ), lda+1, scale, s )
                       ! tri l at a(0,0)
                       call stdlib_slassq( k-1, a( 0_ilp+lda ), lda+1, scale, s )
                       ! tri u at a(0,1)
                    end if
                 else
                    ! a is xpose
                    if( ilu==0_ilp ) then
                       ! a**t is upper
                       do j = 1, k - 2
                          call stdlib_slassq( j, a( 0_ilp+( k+j )*lda ), 1_ilp, scale, s )
                          ! u at a(0,k)
                       end do
                       do j = 0, k - 2
                          call stdlib_slassq( k, a( 0_ilp+j*lda ), 1_ilp, scale, s )
                          ! k by k-1 rect. at a(0,0)
                       end do
                       do j = 0, k - 2
                          call stdlib_slassq( k-j-1, a( j+1+( j+k-1 )*lda ), 1_ilp,scale, s )
                          ! l at a(0,k-1)
                       end do
                       s = s + s
                       ! double s for the off diagonal elements
                       call stdlib_slassq( k-1, a( 0_ilp+k*lda ), lda+1, scale, s )
                       ! tri u at a(0,k)
                       call stdlib_slassq( k, a( 0_ilp+( k-1 )*lda ), lda+1, scale, s )
                       ! tri l at a(0,k-1)
                    else
                       ! a**t is lower
                       do j = 1, k - 1
                          call stdlib_slassq( j, a( 0_ilp+j*lda ), 1_ilp, scale, s )
                          ! u at a(0,0)
                       end do
                       do j = k, n - 1
                          call stdlib_slassq( k, a( 0_ilp+j*lda ), 1_ilp, scale, s )
                          ! k by k-1 rect. at a(0,k)
                       end do
                       do j = 0, k - 3
                          call stdlib_slassq( k-j-2, a( j+2+j*lda ), 1_ilp, scale, s )
                          ! l at a(1,0)
                       end do
                       s = s + s
                       ! double s for the off diagonal elements
                       call stdlib_slassq( k, a( 0_ilp ), lda+1, scale, s )
                       ! tri u at a(0,0)
                       call stdlib_slassq( k-1, a( 1_ilp ), lda+1, scale, s )
                       ! tri l at a(1,0)
                    end if
                 end if
              else
                 ! n is even
                 if( ifm==1_ilp ) then
                    ! a is normal
                    if( ilu==0_ilp ) then
                       ! a is upper
                       do j = 0, k - 2
                          call stdlib_slassq( k-j-1, a( k+j+2+j*lda ), 1_ilp, scale, s )
                          ! l at a(k+1,0)
                       end do
                       do j = 0, k - 1
                          call stdlib_slassq( k+j, a( 0_ilp+j*lda ), 1_ilp, scale, s )
                          ! trap u at a(0,0)
                       end do
                       s = s + s
                       ! double s for the off diagonal elements
                       call stdlib_slassq( k, a( k+1 ), lda+1, scale, s )
                       ! tri l at a(k+1,0)
                       call stdlib_slassq( k, a( k ), lda+1, scale, s )
                       ! tri u at a(k,0)
                    else
                       ! ilu=1
                       do j = 0, k - 1
                          call stdlib_slassq( n-j-1, a( j+2+j*lda ), 1_ilp, scale, s )
                          ! trap l at a(1,0)
                       end do
                       do j = 1, k - 1
                          call stdlib_slassq( j, a( 0_ilp+j*lda ), 1_ilp, scale, s )
                          ! u at a(0,0)
                       end do
                       s = s + s
                       ! double s for the off diagonal elements
                       call stdlib_slassq( k, a( 1_ilp ), lda+1, scale, s )
                       ! tri l at a(1,0)
                       call stdlib_slassq( k, a( 0_ilp ), lda+1, scale, s )
                       ! tri u at a(0,0)
                    end if
                 else
                    ! a is xpose
                    if( ilu==0_ilp ) then
                       ! a**t is upper
                       do j = 1, k - 1
                          call stdlib_slassq( j, a( 0_ilp+( k+1+j )*lda ), 1_ilp, scale, s )
                          ! u at a(0,k+1)
                       end do
                       do j = 0, k - 1
                          call stdlib_slassq( k, a( 0_ilp+j*lda ), 1_ilp, scale, s )
                          ! k by k rect. at a(0,0)
                       end do
                       do j = 0, k - 2
                          call stdlib_slassq( k-j-1, a( j+1+( j+k )*lda ), 1_ilp, scale,s )
                          ! l at a(0,k)
                       end do
                       s = s + s
                       ! double s for the off diagonal elements
                       call stdlib_slassq( k, a( 0_ilp+( k+1 )*lda ), lda+1, scale, s )
                       ! tri u at a(0,k+1)
                       call stdlib_slassq( k, a( 0_ilp+k*lda ), lda+1, scale, s )
                       ! tri l at a(0,k)
                    else
                       ! a**t is lower
                       do j = 1, k - 1
                          call stdlib_slassq( j, a( 0_ilp+( j+1 )*lda ), 1_ilp, scale, s )
                          ! u at a(0,1)
                       end do
                       do j = k + 1, n
                          call stdlib_slassq( k, a( 0_ilp+j*lda ), 1_ilp, scale, s )
                          ! k by k rect. at a(0,k+1)
                       end do
                       do j = 0, k - 2
                          call stdlib_slassq( k-j-1, a( j+1+j*lda ), 1_ilp, scale, s )
                          ! l at a(0,0)
                       end do
                       s = s + s
                       ! double s for the off diagonal elements
                       call stdlib_slassq( k, a( lda ), lda+1, scale, s )
                       ! tri l at a(0,1)
                       call stdlib_slassq( k, a( 0_ilp ), lda+1, scale, s )
                       ! tri u at a(0,0)
                    end if
                 end if
              end if
              value = scale*sqrt( s )
           end if
           stdlib_slansf = value
           return
     end function stdlib_slansf

     real(dp) module function stdlib_dlansf( norm, transr, uplo, n, a, work )
     !! DLANSF returns the value of the one norm, or the Frobenius norm, or
     !! the infinity norm, or the element of largest absolute value of a
     !! real symmetric matrix A in RFP format.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: norm, transr, uplo
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           real(dp), intent(in) :: a(0_ilp:*)
           real(dp), intent(out) :: work(0_ilp:*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j, ifm, ilu, noe, n1, k, l, lda
           real(dp) :: scale, s, value, aa, temp
           ! Intrinsic Functions 
           ! Executable Statements 
           if( n==0_ilp ) then
              stdlib_dlansf = zero
              return
           else if( n==1_ilp ) then
              stdlib_dlansf = abs( a(0_ilp) )
              return
           end if
           ! set noe = 1 if n is odd. if n is even set noe=0
           noe = 1_ilp
           if( mod( n, 2_ilp )==0_ilp )noe = 0_ilp
           ! set ifm = 0 when form='t or 't' and 1 otherwise
           ifm = 1_ilp
           if( stdlib_lsame( transr, 'T' ) )ifm = 0_ilp
           ! set ilu = 0 when uplo='u or 'u' and 1 otherwise
           ilu = 1_ilp
           if( stdlib_lsame( uplo, 'U' ) )ilu = 0_ilp
           ! set lda = (n+1)/2 when ifm = 0
           ! set lda = n when ifm = 1 and noe = 1
           ! set lda = n+1 when ifm = 1 and noe = 0
           if( ifm==1_ilp ) then
              if( noe==1_ilp ) then
                 lda = n
              else
                 ! noe=0
                 lda = n + 1_ilp
              end if
           else
              ! ifm=0
              lda = ( n+1 ) / 2_ilp
           end if
           if( stdlib_lsame( norm, 'M' ) ) then
             ! find max(abs(a(i,j))).
              k = ( n+1 ) / 2_ilp
              value = zero
              if( noe==1_ilp ) then
                 ! n is odd
                 if( ifm==1_ilp ) then
                 ! a is n by k
                    do j = 0, k - 1
                       do i = 0, n - 1
                          temp = abs( a( i+j*lda ) )
                          if( value < temp .or. stdlib_disnan( temp ) )value = temp
                       end do
                    end do
                 else
                    ! xpose case; a is k by n
                    do j = 0, n - 1
                       do i = 0, k - 1
                          temp = abs( a( i+j*lda ) )
                          if( value < temp .or. stdlib_disnan( temp ) )value = temp
                       end do
                    end do
                 end if
              else
                 ! n is even
                 if( ifm==1_ilp ) then
                    ! a is n+1 by k
                    do j = 0, k - 1
                       do i = 0, n
                          temp = abs( a( i+j*lda ) )
                          if( value < temp .or. stdlib_disnan( temp ) )value = temp
                       end do
                    end do
                 else
                    ! xpose case; a is k by n+1
                    do j = 0, n
                       do i = 0, k - 1
                          temp = abs( a( i+j*lda ) )
                          if( value < temp .or. stdlib_disnan( temp ) )value = temp
                       end do
                    end do
                 end if
              end if
           else if( ( stdlib_lsame( norm, 'I' ) ) .or. ( stdlib_lsame( norm, 'O' ) ) .or.( &
                     norm=='1' ) ) then
              ! find normi(a) ( = norm1(a), since a is symmetric).
              if( ifm==1_ilp ) then
                 k = n / 2_ilp
                 if( noe==1_ilp ) then
                    ! n is odd
                    if( ilu==0_ilp ) then
                       do i = 0, k - 1
                          work( i ) = zero
                       end do
                       do j = 0, k
                          s = zero
                          do i = 0, k + j - 1
                             aa = abs( a( i+j*lda ) )
                             ! -> a(i,j+k)
                             s = s + aa
                             work( i ) = work( i ) + aa
                          end do
                          aa = abs( a( i+j*lda ) )
                          ! -> a(j+k,j+k)
                          work( j+k ) = s + aa
                          if( i==k+k )go to 10
                          i = i + 1_ilp
                          aa = abs( a( i+j*lda ) )
                          ! -> a(j,j)
                          work( j ) = work( j ) + aa
                          s = zero
                          do l = j + 1, k - 1
                             i = i + 1_ilp
                             aa = abs( a( i+j*lda ) )
                             ! -> a(l,j)
                             s = s + aa
                             work( l ) = work( l ) + aa
                          end do
                          work( j ) = work( j ) + s
                       end do
                       10 continue
                       value = work( 0_ilp )
                       do i = 1, n-1
                          temp = work( i )
                          if( value < temp .or. stdlib_disnan( temp ) )value = temp
                       end do
                    else
                       ! ilu = 1
                       k = k + 1_ilp
                       ! k=(n+1)/2 for n odd and ilu=1
                       do i = k, n - 1
                          work( i ) = zero
                       end do
                       do j = k - 1, 0, -1
                          s = zero
                          do i = 0, j - 2
                             aa = abs( a( i+j*lda ) )
                             ! -> a(j+k,i+k)
                             s = s + aa
                             work( i+k ) = work( i+k ) + aa
                          end do
                          if( j>0_ilp ) then
                             aa = abs( a( i+j*lda ) )
                             ! -> a(j+k,j+k)
                             s = s + aa
                             work( i+k ) = work( i+k ) + s
                             ! i=j
                             i = i + 1_ilp
                          end if
                          aa = abs( a( i+j*lda ) )
                          ! -> a(j,j)
                          work( j ) = aa
                          s = zero
                          do l = j + 1, n - 1
                             i = i + 1_ilp
                             aa = abs( a( i+j*lda ) )
                             ! -> a(l,j)
                             s = s + aa
                             work( l ) = work( l ) + aa
                          end do
                          work( j ) = work( j ) + s
                       end do
                       value = work( 0_ilp )
                       do i = 1, n-1
                          temp = work( i )
                          if( value < temp .or. stdlib_disnan( temp ) )value = temp
                       end do
                    end if
                 else
                    ! n is even
                    if( ilu==0_ilp ) then
                       do i = 0, k - 1
                          work( i ) = zero
                       end do
                       do j = 0, k - 1
                          s = zero
                          do i = 0, k + j - 1
                             aa = abs( a( i+j*lda ) )
                             ! -> a(i,j+k)
                             s = s + aa
                             work( i ) = work( i ) + aa
                          end do
                          aa = abs( a( i+j*lda ) )
                          ! -> a(j+k,j+k)
                          work( j+k ) = s + aa
                          i = i + 1_ilp
                          aa = abs( a( i+j*lda ) )
                          ! -> a(j,j)
                          work( j ) = work( j ) + aa
                          s = zero
                          do l = j + 1, k - 1
                             i = i + 1_ilp
                             aa = abs( a( i+j*lda ) )
                             ! -> a(l,j)
                             s = s + aa
                             work( l ) = work( l ) + aa
                          end do
                          work( j ) = work( j ) + s
                       end do
                       value = work( 0_ilp )
                       do i = 1, n-1
                          temp = work( i )
                          if( value < temp .or. stdlib_disnan( temp ) )value = temp
                       end do
                    else
                       ! ilu = 1
                       do i = k, n - 1
                          work( i ) = zero
                       end do
                       do j = k - 1, 0, -1
                          s = zero
                          do i = 0, j - 1
                             aa = abs( a( i+j*lda ) )
                             ! -> a(j+k,i+k)
                             s = s + aa
                             work( i+k ) = work( i+k ) + aa
                          end do
                          aa = abs( a( i+j*lda ) )
                          ! -> a(j+k,j+k)
                          s = s + aa
                          work( i+k ) = work( i+k ) + s
                          ! i=j
                          i = i + 1_ilp
                          aa = abs( a( i+j*lda ) )
                          ! -> a(j,j)
                          work( j ) = aa
                          s = zero
                          do l = j + 1, n - 1
                             i = i + 1_ilp
                             aa = abs( a( i+j*lda ) )
                             ! -> a(l,j)
                             s = s + aa
                             work( l ) = work( l ) + aa
                          end do
                          work( j ) = work( j ) + s
                       end do
                       value = work( 0_ilp )
                       do i = 1, n-1
                          temp = work( i )
                          if( value < temp .or. stdlib_disnan( temp ) )value = temp
                       end do
                    end if
                 end if
              else
                 ! ifm=0
                 k = n / 2_ilp
                 if( noe==1_ilp ) then
                    ! n is odd
                    if( ilu==0_ilp ) then
                       n1 = k
                       ! n/2
                       k = k + 1_ilp
                       ! k is the row size and lda
                       do i = n1, n - 1
                          work( i ) = zero
                       end do
                       do j = 0, n1 - 1
                          s = zero
                          do i = 0, k - 1
                             aa = abs( a( i+j*lda ) )
                             ! a(j,n1+i)
                             work( i+n1 ) = work( i+n1 ) + aa
                             s = s + aa
                          end do
                          work( j ) = s
                       end do
                       ! j=n1=k-1 is special
                       s = abs( a( 0_ilp+j*lda ) )
                       ! a(k-1,k-1)
                       do i = 1, k - 1
                          aa = abs( a( i+j*lda ) )
                          ! a(k-1,i+n1)
                          work( i+n1 ) = work( i+n1 ) + aa
                          s = s + aa
                       end do
                       work( j ) = work( j ) + s
                       do j = k, n - 1
                          s = zero
                          do i = 0, j - k - 1
                             aa = abs( a( i+j*lda ) )
                             ! a(i,j-k)
                             work( i ) = work( i ) + aa
                             s = s + aa
                          end do
                          ! i=j-k
                          aa = abs( a( i+j*lda ) )
                          ! a(j-k,j-k)
                          s = s + aa
                          work( j-k ) = work( j-k ) + s
                          i = i + 1_ilp
                          s = abs( a( i+j*lda ) )
                          ! a(j,j)
                          do l = j + 1, n - 1
                             i = i + 1_ilp
                             aa = abs( a( i+j*lda ) )
                             ! a(j,l)
                             work( l ) = work( l ) + aa
                             s = s + aa
                          end do
                          work( j ) = work( j ) + s
                       end do
                       value = work( 0_ilp )
                       do i = 1, n-1
                          temp = work( i )
                          if( value < temp .or. stdlib_disnan( temp ) )value = temp
                       end do
                    else
                       ! ilu=1
                       k = k + 1_ilp
                       ! k=(n+1)/2 for n odd and ilu=1
                       do i = k, n - 1
                          work( i ) = zero
                       end do
                       do j = 0, k - 2
                          ! process
                          s = zero
                          do i = 0, j - 1
                             aa = abs( a( i+j*lda ) )
                             ! a(j,i)
                             work( i ) = work( i ) + aa
                             s = s + aa
                          end do
                          aa = abs( a( i+j*lda ) )
                          ! i=j so process of a(j,j)
                          s = s + aa
                          work( j ) = s
                          ! is initialised here
                          i = i + 1_ilp
                          ! i=j process a(j+k,j+k)
                          aa = abs( a( i+j*lda ) )
                          s = aa
                          do l = k + j + 1, n - 1
                             i = i + 1_ilp
                             aa = abs( a( i+j*lda ) )
                             ! a(l,k+j)
                             s = s + aa
                             work( l ) = work( l ) + aa
                          end do
                          work( k+j ) = work( k+j ) + s
                       end do
                       ! j=k-1 is special :process col a(k-1,0:k-1)
                       s = zero
                       do i = 0, k - 2
                          aa = abs( a( i+j*lda ) )
                          ! a(k,i)
                          work( i ) = work( i ) + aa
                          s = s + aa
                       end do
                       ! i=k-1
                       aa = abs( a( i+j*lda ) )
                       ! a(k-1,k-1)
                       s = s + aa
                       work( i ) = s
                       ! done with col j=k+1
                       do j = k, n - 1
                          ! process col j of a = a(j,0:k-1)
                          s = zero
                          do i = 0, k - 1
                             aa = abs( a( i+j*lda ) )
                             ! a(j,i)
                             work( i ) = work( i ) + aa
                             s = s + aa
                          end do
                          work( j ) = work( j ) + s
                       end do
                       value = work( 0_ilp )
                       do i = 1, n-1
                          temp = work( i )
                          if( value < temp .or. stdlib_disnan( temp ) )value = temp
                       end do
                    end if
                 else
                    ! n is even
                    if( ilu==0_ilp ) then
                       do i = k, n - 1
                          work( i ) = zero
                       end do
                       do j = 0, k - 1
                          s = zero
                          do i = 0, k - 1
                             aa = abs( a( i+j*lda ) )
                             ! a(j,i+k)
                             work( i+k ) = work( i+k ) + aa
                             s = s + aa
                          end do
                          work( j ) = s
                       end do
                       ! j=k
                       aa = abs( a( 0_ilp+j*lda ) )
                       ! a(k,k)
                       s = aa
                       do i = 1, k - 1
                          aa = abs( a( i+j*lda ) )
                          ! a(k,k+i)
                          work( i+k ) = work( i+k ) + aa
                          s = s + aa
                       end do
                       work( j ) = work( j ) + s
                       do j = k + 1, n - 1
                          s = zero
                          do i = 0, j - 2 - k
                             aa = abs( a( i+j*lda ) )
                             ! a(i,j-k-1)
                             work( i ) = work( i ) + aa
                             s = s + aa
                          end do
                           ! i=j-1-k
                          aa = abs( a( i+j*lda ) )
                          ! a(j-k-1,j-k-1)
                          s = s + aa
                          work( j-k-1 ) = work( j-k-1 ) + s
                          i = i + 1_ilp
                          aa = abs( a( i+j*lda ) )
                          ! a(j,j)
                          s = aa
                          do l = j + 1, n - 1
                             i = i + 1_ilp
                             aa = abs( a( i+j*lda ) )
                             ! a(j,l)
                             work( l ) = work( l ) + aa
                             s = s + aa
                          end do
                          work( j ) = work( j ) + s
                       end do
                       ! j=n
                       s = zero
                       do i = 0, k - 2
                          aa = abs( a( i+j*lda ) )
                          ! a(i,k-1)
                          work( i ) = work( i ) + aa
                          s = s + aa
                       end do
                       ! i=k-1
                       aa = abs( a( i+j*lda ) )
                       ! a(k-1,k-1)
                       s = s + aa
                       work( i ) = work( i ) + s
                       value = work( 0_ilp )
                       do i = 1, n-1
                          temp = work( i )
                          if( value < temp .or. stdlib_disnan( temp ) )value = temp
                       end do
                    else
                       ! ilu=1
                       do i = k, n - 1
                          work( i ) = zero
                       end do
                       ! j=0 is special :process col a(k:n-1,k)
                       s = abs( a( 0_ilp ) )
                       ! a(k,k)
                       do i = 1, k - 1
                          aa = abs( a( i ) )
                          ! a(k+i,k)
                          work( i+k ) = work( i+k ) + aa
                          s = s + aa
                       end do
                       work( k ) = work( k ) + s
                       do j = 1, k - 1
                          ! process
                          s = zero
                          do i = 0, j - 2
                             aa = abs( a( i+j*lda ) )
                             ! a(j-1,i)
                             work( i ) = work( i ) + aa
                             s = s + aa
                          end do
                          aa = abs( a( i+j*lda ) )
                          ! i=j-1 so process of a(j-1,j-1)
                          s = s + aa
                          work( j-1 ) = s
                          ! is initialised here
                          i = i + 1_ilp
                          ! i=j process a(j+k,j+k)
                          aa = abs( a( i+j*lda ) )
                          s = aa
                          do l = k + j + 1, n - 1
                             i = i + 1_ilp
                             aa = abs( a( i+j*lda ) )
                             ! a(l,k+j)
                             s = s + aa
                             work( l ) = work( l ) + aa
                          end do
                          work( k+j ) = work( k+j ) + s
                       end do
                       ! j=k is special :process col a(k,0:k-1)
                       s = zero
                       do i = 0, k - 2
                          aa = abs( a( i+j*lda ) )
                          ! a(k,i)
                          work( i ) = work( i ) + aa
                          s = s + aa
                       end do
                       ! i=k-1
                       aa = abs( a( i+j*lda ) )
                       ! a(k-1,k-1)
                       s = s + aa
                       work( i ) = s
                       ! done with col j=k+1
                       do j = k + 1, n
                          ! process col j-1 of a = a(j-1,0:k-1)
                          s = zero
                          do i = 0, k - 1
                             aa = abs( a( i+j*lda ) )
                             ! a(j-1,i)
                             work( i ) = work( i ) + aa
                             s = s + aa
                          end do
                          work( j-1 ) = work( j-1 ) + s
                       end do
                       value = work( 0_ilp )
                       do i = 1, n-1
                          temp = work( i )
                          if( value < temp .or. stdlib_disnan( temp ) )value = temp
                       end do
                    end if
                 end if
              end if
           else if( ( stdlib_lsame( norm, 'F' ) ) .or. ( stdlib_lsame( norm, 'E' ) ) ) &
                     then
             ! find normf(a).
              k = ( n+1 ) / 2_ilp
              scale = zero
              s = one
              if( noe==1_ilp ) then
                 ! n is odd
                 if( ifm==1_ilp ) then
                    ! a is normal
                    if( ilu==0_ilp ) then
                       ! a is upper
                       do j = 0, k - 3
                          call stdlib_dlassq( k-j-2, a( k+j+1+j*lda ), 1_ilp, scale, s )
                          ! l at a(k,0)
                       end do
                       do j = 0, k - 1
                          call stdlib_dlassq( k+j-1, a( 0_ilp+j*lda ), 1_ilp, scale, s )
                          ! trap u at a(0,0)
                       end do
                       s = s + s
                       ! double s for the off diagonal elements
                       call stdlib_dlassq( k-1, a( k ), lda+1, scale, s )
                       ! tri l at a(k,0)
                       call stdlib_dlassq( k, a( k-1 ), lda+1, scale, s )
                       ! tri u at a(k-1,0)
                    else
                       ! ilu=1
                       do j = 0, k - 1
                          call stdlib_dlassq( n-j-1, a( j+1+j*lda ), 1_ilp, scale, s )
                          ! trap l at a(0,0)
                       end do
                       do j = 0, k - 2
                          call stdlib_dlassq( j, a( 0_ilp+( 1_ilp+j )*lda ), 1_ilp, scale, s )
                          ! u at a(0,1)
                       end do
                       s = s + s
                       ! double s for the off diagonal elements
                       call stdlib_dlassq( k, a( 0_ilp ), lda+1, scale, s )
                       ! tri l at a(0,0)
                       call stdlib_dlassq( k-1, a( 0_ilp+lda ), lda+1, scale, s )
                       ! tri u at a(0,1)
                    end if
                 else
                    ! a is xpose
                    if( ilu==0_ilp ) then
                       ! a**t is upper
                       do j = 1, k - 2
                          call stdlib_dlassq( j, a( 0_ilp+( k+j )*lda ), 1_ilp, scale, s )
                          ! u at a(0,k)
                       end do
                       do j = 0, k - 2
                          call stdlib_dlassq( k, a( 0_ilp+j*lda ), 1_ilp, scale, s )
                          ! k by k-1 rect. at a(0,0)
                       end do
                       do j = 0, k - 2
                          call stdlib_dlassq( k-j-1, a( j+1+( j+k-1 )*lda ), 1_ilp,scale, s )
                          ! l at a(0,k-1)
                       end do
                       s = s + s
                       ! double s for the off diagonal elements
                       call stdlib_dlassq( k-1, a( 0_ilp+k*lda ), lda+1, scale, s )
                       ! tri u at a(0,k)
                       call stdlib_dlassq( k, a( 0_ilp+( k-1 )*lda ), lda+1, scale, s )
                       ! tri l at a(0,k-1)
                    else
                       ! a**t is lower
                       do j = 1, k - 1
                          call stdlib_dlassq( j, a( 0_ilp+j*lda ), 1_ilp, scale, s )
                          ! u at a(0,0)
                       end do
                       do j = k, n - 1
                          call stdlib_dlassq( k, a( 0_ilp+j*lda ), 1_ilp, scale, s )
                          ! k by k-1 rect. at a(0,k)
                       end do
                       do j = 0, k - 3
                          call stdlib_dlassq( k-j-2, a( j+2+j*lda ), 1_ilp, scale, s )
                          ! l at a(1,0)
                       end do
                       s = s + s
                       ! double s for the off diagonal elements
                       call stdlib_dlassq( k, a( 0_ilp ), lda+1, scale, s )
                       ! tri u at a(0,0)
                       call stdlib_dlassq( k-1, a( 1_ilp ), lda+1, scale, s )
                       ! tri l at a(1,0)
                    end if
                 end if
              else
                 ! n is even
                 if( ifm==1_ilp ) then
                    ! a is normal
                    if( ilu==0_ilp ) then
                       ! a is upper
                       do j = 0, k - 2
                          call stdlib_dlassq( k-j-1, a( k+j+2+j*lda ), 1_ilp, scale, s )
                          ! l at a(k+1,0)
                       end do
                       do j = 0, k - 1
                          call stdlib_dlassq( k+j, a( 0_ilp+j*lda ), 1_ilp, scale, s )
                          ! trap u at a(0,0)
                       end do
                       s = s + s
                       ! double s for the off diagonal elements
                       call stdlib_dlassq( k, a( k+1 ), lda+1, scale, s )
                       ! tri l at a(k+1,0)
                       call stdlib_dlassq( k, a( k ), lda+1, scale, s )
                       ! tri u at a(k,0)
                    else
                       ! ilu=1
                       do j = 0, k - 1
                          call stdlib_dlassq( n-j-1, a( j+2+j*lda ), 1_ilp, scale, s )
                          ! trap l at a(1,0)
                       end do
                       do j = 1, k - 1
                          call stdlib_dlassq( j, a( 0_ilp+j*lda ), 1_ilp, scale, s )
                          ! u at a(0,0)
                       end do
                       s = s + s
                       ! double s for the off diagonal elements
                       call stdlib_dlassq( k, a( 1_ilp ), lda+1, scale, s )
                       ! tri l at a(1,0)
                       call stdlib_dlassq( k, a( 0_ilp ), lda+1, scale, s )
                       ! tri u at a(0,0)
                    end if
                 else
                    ! a is xpose
                    if( ilu==0_ilp ) then
                       ! a**t is upper
                       do j = 1, k - 1
                          call stdlib_dlassq( j, a( 0_ilp+( k+1+j )*lda ), 1_ilp, scale, s )
                          ! u at a(0,k+1)
                       end do
                       do j = 0, k - 1
                          call stdlib_dlassq( k, a( 0_ilp+j*lda ), 1_ilp, scale, s )
                          ! k by k rect. at a(0,0)
                       end do
                       do j = 0, k - 2
                          call stdlib_dlassq( k-j-1, a( j+1+( j+k )*lda ), 1_ilp, scale,s )
                          ! l at a(0,k)
                       end do
                       s = s + s
                       ! double s for the off diagonal elements
                       call stdlib_dlassq( k, a( 0_ilp+( k+1 )*lda ), lda+1, scale, s )
                       ! tri u at a(0,k+1)
                       call stdlib_dlassq( k, a( 0_ilp+k*lda ), lda+1, scale, s )
                       ! tri l at a(0,k)
                    else
                       ! a**t is lower
                       do j = 1, k - 1
                          call stdlib_dlassq( j, a( 0_ilp+( j+1 )*lda ), 1_ilp, scale, s )
                          ! u at a(0,1)
                       end do
                       do j = k + 1, n
                          call stdlib_dlassq( k, a( 0_ilp+j*lda ), 1_ilp, scale, s )
                          ! k by k rect. at a(0,k+1)
                       end do
                       do j = 0, k - 2
                          call stdlib_dlassq( k-j-1, a( j+1+j*lda ), 1_ilp, scale, s )
                          ! l at a(0,0)
                       end do
                       s = s + s
                       ! double s for the off diagonal elements
                       call stdlib_dlassq( k, a( lda ), lda+1, scale, s )
                       ! tri l at a(0,1)
                       call stdlib_dlassq( k, a( 0_ilp ), lda+1, scale, s )
                       ! tri u at a(0,0)
                    end if
                 end if
              end if
              value = scale*sqrt( s )
           end if
           stdlib_dlansf = value
           return
     end function stdlib_dlansf




     real(sp) module function stdlib_clanhp( norm, uplo, n, ap, work )
     !! CLANHP returns the value of the one norm,  or the Frobenius norm, or
     !! the  infinity norm,  or the  element of  largest absolute value  of a
     !! complex hermitian matrix A,  supplied in packed form.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: norm, uplo
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           real(sp), intent(out) :: work(*)
           complex(sp), intent(in) :: ap(*)
       ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j, k
           real(sp) :: absa, scale, sum, value
           ! Intrinsic Functions 
           ! Executable Statements 
           if( n==0_ilp ) then
              value = zero
           else if( stdlib_lsame( norm, 'M' ) ) then
              ! find max(abs(a(i,j))).
              value = zero
              if( stdlib_lsame( uplo, 'U' ) ) then
                 k = 0_ilp
                 do j = 1, n
                    do i = k + 1, k + j - 1
                       sum = abs( ap( i ) )
                       if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
                    end do
                    k = k + j
                    sum = abs( real( ap( k ),KIND=sp) )
                    if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
                 end do
              else
                 k = 1_ilp
                 do j = 1, n
                    sum = abs( real( ap( k ),KIND=sp) )
                    if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
                    do i = k + 1, k + n - j
                       sum = abs( ap( i ) )
                       if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
                    end do
                    k = k + n - j + 1_ilp
                 end do
              end if
           else if( ( stdlib_lsame( norm, 'I' ) ) .or. ( stdlib_lsame( norm, 'O' ) ) .or.( &
                     norm=='1' ) ) then
              ! find normi(a) ( = norm1(a), since a is hermitian).
              value = zero
              k = 1_ilp
              if( stdlib_lsame( uplo, 'U' ) ) then
                 do j = 1, n
                    sum = zero
                    do i = 1, j - 1
                       absa = abs( ap( k ) )
                       sum = sum + absa
                       work( i ) = work( i ) + absa
                       k = k + 1_ilp
                    end do
                    work( j ) = sum + abs( real( ap( k ),KIND=sp) )
                    k = k + 1_ilp
                 end do
                 do i = 1, n
                    sum = work( i )
                    if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
                 end do
              else
                 do i = 1, n
                    work( i ) = zero
                 end do
                 do j = 1, n
                    sum = work( j ) + abs( real( ap( k ),KIND=sp) )
                    k = k + 1_ilp
                    do i = j + 1, n
                       absa = abs( ap( k ) )
                       sum = sum + absa
                       work( i ) = work( i ) + absa
                       k = k + 1_ilp
                    end do
                    if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
                 end do
              end if
           else if( ( stdlib_lsame( norm, 'F' ) ) .or. ( stdlib_lsame( norm, 'E' ) ) ) &
                     then
              ! find normf(a).
              scale = zero
              sum = one
              k = 2_ilp
              if( stdlib_lsame( uplo, 'U' ) ) then
                 do j = 2, n
                    call stdlib_classq( j-1, ap( k ), 1_ilp, scale, sum )
                    k = k + j
                 end do
              else
                 do j = 1, n - 1
                    call stdlib_classq( n-j, ap( k ), 1_ilp, scale, sum )
                    k = k + n - j + 1_ilp
                 end do
              end if
              sum = 2_ilp*sum
              k = 1_ilp
              do i = 1, n
                 if( real( ap( k ),KIND=sp)/=zero ) then
                    absa = abs( real( ap( k ),KIND=sp) )
                    if( scale<absa ) then
                       sum = one + sum*( scale / absa )**2_ilp
                       scale = absa
                    else
                       sum = sum + ( absa / scale )**2_ilp
                    end if
                 end if
                 if( stdlib_lsame( uplo, 'U' ) ) then
                    k = k + i + 1_ilp
                 else
                    k = k + n - i + 1_ilp
                 end if
              end do
              value = scale*sqrt( sum )
           end if
           stdlib_clanhp = value
           return
     end function stdlib_clanhp

     real(dp) module function stdlib_zlanhp( norm, uplo, n, ap, work )
     !! ZLANHP returns the value of the one norm,  or the Frobenius norm, or
     !! the  infinity norm,  or the  element of  largest absolute value  of a
     !! complex hermitian matrix A,  supplied in packed form.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: norm, uplo
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           real(dp), intent(out) :: work(*)
           complex(dp), intent(in) :: ap(*)
       ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j, k
           real(dp) :: absa, scale, sum, value
           ! Intrinsic Functions 
           ! Executable Statements 
           if( n==0_ilp ) then
              value = zero
           else if( stdlib_lsame( norm, 'M' ) ) then
              ! find max(abs(a(i,j))).
              value = zero
              if( stdlib_lsame( uplo, 'U' ) ) then
                 k = 0_ilp
                 do j = 1, n
                    do i = k + 1, k + j - 1
                       sum = abs( ap( i ) )
                       if( value < sum .or. stdlib_disnan( sum ) ) value = sum
                    end do
                    k = k + j
                    sum = abs( real( ap( k ),KIND=dp) )
                    if( value < sum .or. stdlib_disnan( sum ) ) value = sum
                 end do
              else
                 k = 1_ilp
                 do j = 1, n
                    sum = abs( real( ap( k ),KIND=dp) )
                    if( value < sum .or. stdlib_disnan( sum ) ) value = sum
                    do i = k + 1, k + n - j
                       sum = abs( ap( i ) )
                       if( value < sum .or. stdlib_disnan( sum ) ) value = sum
                    end do
                    k = k + n - j + 1_ilp
                 end do
              end if
           else if( ( stdlib_lsame( norm, 'I' ) ) .or. ( stdlib_lsame( norm, 'O' ) ) .or.( &
                     norm=='1' ) ) then
              ! find normi(a) ( = norm1(a), since a is hermitian).
              value = zero
              k = 1_ilp
              if( stdlib_lsame( uplo, 'U' ) ) then
                 do j = 1, n
                    sum = zero
                    do i = 1, j - 1
                       absa = abs( ap( k ) )
                       sum = sum + absa
                       work( i ) = work( i ) + absa
                       k = k + 1_ilp
                    end do
                    work( j ) = sum + abs( real( ap( k ),KIND=dp) )
                    k = k + 1_ilp
                 end do
                 do i = 1, n
                    sum = work( i )
                    if( value < sum .or. stdlib_disnan( sum ) ) value = sum
                 end do
              else
                 do i = 1, n
                    work( i ) = zero
                 end do
                 do j = 1, n
                    sum = work( j ) + abs( real( ap( k ),KIND=dp) )
                    k = k + 1_ilp
                    do i = j + 1, n
                       absa = abs( ap( k ) )
                       sum = sum + absa
                       work( i ) = work( i ) + absa
                       k = k + 1_ilp
                    end do
                    if( value < sum .or. stdlib_disnan( sum ) ) value = sum
                 end do
              end if
           else if( ( stdlib_lsame( norm, 'F' ) ) .or. ( stdlib_lsame( norm, 'E' ) ) ) &
                     then
              ! find normf(a).
              scale = zero
              sum = one
              k = 2_ilp
              if( stdlib_lsame( uplo, 'U' ) ) then
                 do j = 2, n
                    call stdlib_zlassq( j-1, ap( k ), 1_ilp, scale, sum )
                    k = k + j
                 end do
              else
                 do j = 1, n - 1
                    call stdlib_zlassq( n-j, ap( k ), 1_ilp, scale, sum )
                    k = k + n - j + 1_ilp
                 end do
              end if
              sum = 2_ilp*sum
              k = 1_ilp
              do i = 1, n
                 if( real( ap( k ),KIND=dp)/=zero ) then
                    absa = abs( real( ap( k ),KIND=dp) )
                    if( scale<absa ) then
                       sum = one + sum*( scale / absa )**2_ilp
                       scale = absa
                    else
                       sum = sum + ( absa / scale )**2_ilp
                    end if
                 end if
                 if( stdlib_lsame( uplo, 'U' ) ) then
                    k = k + i + 1_ilp
                 else
                    k = k + n - i + 1_ilp
                 end if
              end do
              value = scale*sqrt( sum )
           end if
           stdlib_zlanhp = value
           return
     end function stdlib_zlanhp




     real(sp) module function stdlib_slansp( norm, uplo, n, ap, work )
     !! SLANSP returns the value of the one norm,  or the Frobenius norm, or
     !! the  infinity norm,  or the  element of  largest absolute value  of a
     !! real symmetric matrix A,  supplied in packed form.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: norm, uplo
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           real(sp), intent(in) :: ap(*)
           real(sp), intent(out) :: work(*)
       ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j, k
           real(sp) :: absa, scale, sum, value
           ! Intrinsic Functions 
           ! Executable Statements 
           if( n==0_ilp ) then
              value = zero
           else if( stdlib_lsame( norm, 'M' ) ) then
              ! find max(abs(a(i,j))).
              value = zero
              if( stdlib_lsame( uplo, 'U' ) ) then
                 k = 1_ilp
                 do j = 1, n
                    do i = k, k + j - 1
                       sum = abs( ap( i ) )
                       if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
                    end do
                    k = k + j
                 end do
              else
                 k = 1_ilp
                 do j = 1, n
                    do i = k, k + n - j
                       sum = abs( ap( i ) )
                       if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
                    end do
                    k = k + n - j + 1_ilp
                 end do
              end if
           else if( ( stdlib_lsame( norm, 'I' ) ) .or. ( stdlib_lsame( norm, 'O' ) ) .or.( &
                     norm=='1' ) ) then
              ! find normi(a) ( = norm1(a), since a is symmetric).
              value = zero
              k = 1_ilp
              if( stdlib_lsame( uplo, 'U' ) ) then
                 do j = 1, n
                    sum = zero
                    do i = 1, j - 1
                       absa = abs( ap( k ) )
                       sum = sum + absa
                       work( i ) = work( i ) + absa
                       k = k + 1_ilp
                    end do
                    work( j ) = sum + abs( ap( k ) )
                    k = k + 1_ilp
                 end do
                 do i = 1, n
                    sum = work( i )
                    if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
                 end do
              else
                 do i = 1, n
                    work( i ) = zero
                 end do
                 do j = 1, n
                    sum = work( j ) + abs( ap( k ) )
                    k = k + 1_ilp
                    do i = j + 1, n
                       absa = abs( ap( k ) )
                       sum = sum + absa
                       work( i ) = work( i ) + absa
                       k = k + 1_ilp
                    end do
                    if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
                 end do
              end if
           else if( ( stdlib_lsame( norm, 'F' ) ) .or. ( stdlib_lsame( norm, 'E' ) ) ) &
                     then
              ! find normf(a).
              scale = zero
              sum = one
              k = 2_ilp
              if( stdlib_lsame( uplo, 'U' ) ) then
                 do j = 2, n
                    call stdlib_slassq( j-1, ap( k ), 1_ilp, scale, sum )
                    k = k + j
                 end do
              else
                 do j = 1, n - 1
                    call stdlib_slassq( n-j, ap( k ), 1_ilp, scale, sum )
                    k = k + n - j + 1_ilp
                 end do
              end if
              sum = 2_ilp*sum
              k = 1_ilp
              do i = 1, n
                 if( ap( k )/=zero ) then
                    absa = abs( ap( k ) )
                    if( scale<absa ) then
                       sum = one + sum*( scale / absa )**2_ilp
                       scale = absa
                    else
                       sum = sum + ( absa / scale )**2_ilp
                    end if
                 end if
                 if( stdlib_lsame( uplo, 'U' ) ) then
                    k = k + i + 1_ilp
                 else
                    k = k + n - i + 1_ilp
                 end if
              end do
              value = scale*sqrt( sum )
           end if
           stdlib_slansp = value
           return
     end function stdlib_slansp

     real(dp) module function stdlib_dlansp( norm, uplo, n, ap, work )
     !! DLANSP returns the value of the one norm,  or the Frobenius norm, or
     !! the  infinity norm,  or the  element of  largest absolute value  of a
     !! real symmetric matrix A,  supplied in packed form.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: norm, uplo
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           real(dp), intent(in) :: ap(*)
           real(dp), intent(out) :: work(*)
       ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j, k
           real(dp) :: absa, scale, sum, value
           ! Intrinsic Functions 
           ! Executable Statements 
           if( n==0_ilp ) then
              value = zero
           else if( stdlib_lsame( norm, 'M' ) ) then
              ! find max(abs(a(i,j))).
              value = zero
              if( stdlib_lsame( uplo, 'U' ) ) then
                 k = 1_ilp
                 do j = 1, n
                    do i = k, k + j - 1
                       sum = abs( ap( i ) )
                       if( value < sum .or. stdlib_disnan( sum ) ) value = sum
                    end do
                    k = k + j
                 end do
              else
                 k = 1_ilp
                 do j = 1, n
                    do i = k, k + n - j
                       sum = abs( ap( i ) )
                       if( value < sum .or. stdlib_disnan( sum ) ) value = sum
                    end do
                    k = k + n - j + 1_ilp
                 end do
              end if
           else if( ( stdlib_lsame( norm, 'I' ) ) .or. ( stdlib_lsame( norm, 'O' ) ) .or.( &
                     norm=='1' ) ) then
              ! find normi(a) ( = norm1(a), since a is symmetric).
              value = zero
              k = 1_ilp
              if( stdlib_lsame( uplo, 'U' ) ) then
                 do j = 1, n
                    sum = zero
                    do i = 1, j - 1
                       absa = abs( ap( k ) )
                       sum = sum + absa
                       work( i ) = work( i ) + absa
                       k = k + 1_ilp
                    end do
                    work( j ) = sum + abs( ap( k ) )
                    k = k + 1_ilp
                 end do
                 do i = 1, n
                    sum = work( i )
                    if( value < sum .or. stdlib_disnan( sum ) ) value = sum
                 end do
              else
                 do i = 1, n
                    work( i ) = zero
                 end do
                 do j = 1, n
                    sum = work( j ) + abs( ap( k ) )
                    k = k + 1_ilp
                    do i = j + 1, n
                       absa = abs( ap( k ) )
                       sum = sum + absa
                       work( i ) = work( i ) + absa
                       k = k + 1_ilp
                    end do
                    if( value < sum .or. stdlib_disnan( sum ) ) value = sum
                 end do
              end if
           else if( ( stdlib_lsame( norm, 'F' ) ) .or. ( stdlib_lsame( norm, 'E' ) ) ) &
                     then
              ! find normf(a).
              scale = zero
              sum = one
              k = 2_ilp
              if( stdlib_lsame( uplo, 'U' ) ) then
                 do j = 2, n
                    call stdlib_dlassq( j-1, ap( k ), 1_ilp, scale, sum )
                    k = k + j
                 end do
              else
                 do j = 1, n - 1
                    call stdlib_dlassq( n-j, ap( k ), 1_ilp, scale, sum )
                    k = k + n - j + 1_ilp
                 end do
              end if
              sum = 2_ilp*sum
              k = 1_ilp
              do i = 1, n
                 if( ap( k )/=zero ) then
                    absa = abs( ap( k ) )
                    if( scale<absa ) then
                       sum = one + sum*( scale / absa )**2_ilp
                       scale = absa
                    else
                       sum = sum + ( absa / scale )**2_ilp
                    end if
                 end if
                 if( stdlib_lsame( uplo, 'U' ) ) then
                    k = k + i + 1_ilp
                 else
                    k = k + n - i + 1_ilp
                 end if
              end do
              value = scale*sqrt( sum )
           end if
           stdlib_dlansp = value
           return
     end function stdlib_dlansp


     real(sp) module function stdlib_clansp( norm, uplo, n, ap, work )
     !! CLANSP returns the value of the one norm,  or the Frobenius norm, or
     !! the  infinity norm,  or the  element of  largest absolute value  of a
     !! complex symmetric matrix A,  supplied in packed form.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: norm, uplo
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           real(sp), intent(out) :: work(*)
           complex(sp), intent(in) :: ap(*)
       ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j, k
           real(sp) :: absa, scale, sum, value
           ! Intrinsic Functions 
           ! Executable Statements 
           if( n==0_ilp ) then
              value = zero
           else if( stdlib_lsame( norm, 'M' ) ) then
              ! find max(abs(a(i,j))).
              value = zero
              if( stdlib_lsame( uplo, 'U' ) ) then
                 k = 1_ilp
                 do j = 1, n
                    do i = k, k + j - 1
                       sum = abs( ap( i ) )
                       if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
                    end do
                    k = k + j
                 end do
              else
                 k = 1_ilp
                 do j = 1, n
                    do i = k, k + n - j
                       sum = abs( ap( i ) )
                       if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
                    end do
                    k = k + n - j + 1_ilp
                 end do
              end if
           else if( ( stdlib_lsame( norm, 'I' ) ) .or. ( stdlib_lsame( norm, 'O' ) ) .or.( &
                     norm=='1' ) ) then
              ! find normi(a) ( = norm1(a), since a is symmetric).
              value = zero
              k = 1_ilp
              if( stdlib_lsame( uplo, 'U' ) ) then
                 do j = 1, n
                    sum = zero
                    do i = 1, j - 1
                       absa = abs( ap( k ) )
                       sum = sum + absa
                       work( i ) = work( i ) + absa
                       k = k + 1_ilp
                    end do
                    work( j ) = sum + abs( ap( k ) )
                    k = k + 1_ilp
                 end do
                 do i = 1, n
                    sum = work( i )
                    if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
                 end do
              else
                 do i = 1, n
                    work( i ) = zero
                 end do
                 do j = 1, n
                    sum = work( j ) + abs( ap( k ) )
                    k = k + 1_ilp
                    do i = j + 1, n
                       absa = abs( ap( k ) )
                       sum = sum + absa
                       work( i ) = work( i ) + absa
                       k = k + 1_ilp
                    end do
                    if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
                 end do
              end if
           else if( ( stdlib_lsame( norm, 'F' ) ) .or. ( stdlib_lsame( norm, 'E' ) ) ) &
                     then
              ! find normf(a).
              scale = zero
              sum = one
              k = 2_ilp
              if( stdlib_lsame( uplo, 'U' ) ) then
                 do j = 2, n
                    call stdlib_classq( j-1, ap( k ), 1_ilp, scale, sum )
                    k = k + j
                 end do
              else
                 do j = 1, n - 1
                    call stdlib_classq( n-j, ap( k ), 1_ilp, scale, sum )
                    k = k + n - j + 1_ilp
                 end do
              end if
              sum = 2_ilp*sum
              k = 1_ilp
              do i = 1, n
                 if( real( ap( k ),KIND=sp)/=zero ) then
                    absa = abs( real( ap( k ),KIND=sp) )
                    if( scale<absa ) then
                       sum = one + sum*( scale / absa )**2_ilp
                       scale = absa
                    else
                       sum = sum + ( absa / scale )**2_ilp
                    end if
                 end if
                 if( aimag( ap( k ) )/=zero ) then
                    absa = abs( aimag( ap( k ) ) )
                    if( scale<absa ) then
                       sum = one + sum*( scale / absa )**2_ilp
                       scale = absa
                    else
                       sum = sum + ( absa / scale )**2_ilp
                    end if
                 end if
                 if( stdlib_lsame( uplo, 'U' ) ) then
                    k = k + i + 1_ilp
                 else
                    k = k + n - i + 1_ilp
                 end if
              end do
              value = scale*sqrt( sum )
           end if
           stdlib_clansp = value
           return
     end function stdlib_clansp

     real(dp) module function stdlib_zlansp( norm, uplo, n, ap, work )
     !! ZLANSP returns the value of the one norm,  or the Frobenius norm, or
     !! the  infinity norm,  or the  element of  largest absolute value  of a
     !! complex symmetric matrix A,  supplied in packed form.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: norm, uplo
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           real(dp), intent(out) :: work(*)
           complex(dp), intent(in) :: ap(*)
       ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j, k
           real(dp) :: absa, scale, sum, value
           ! Intrinsic Functions 
           ! Executable Statements 
           if( n==0_ilp ) then
              value = zero
           else if( stdlib_lsame( norm, 'M' ) ) then
              ! find max(abs(a(i,j))).
              value = zero
              if( stdlib_lsame( uplo, 'U' ) ) then
                 k = 1_ilp
                 do j = 1, n
                    do i = k, k + j - 1
                       sum = abs( ap( i ) )
                       if( value < sum .or. stdlib_disnan( sum ) ) value = sum
                    end do
                    k = k + j
                 end do
              else
                 k = 1_ilp
                 do j = 1, n
                    do i = k, k + n - j
                       sum = abs( ap( i ) )
                       if( value < sum .or. stdlib_disnan( sum ) ) value = sum
                    end do
                    k = k + n - j + 1_ilp
                 end do
              end if
           else if( ( stdlib_lsame( norm, 'I' ) ) .or. ( stdlib_lsame( norm, 'O' ) ) .or.( &
                     norm=='1' ) ) then
              ! find normi(a) ( = norm1(a), since a is symmetric).
              value = zero
              k = 1_ilp
              if( stdlib_lsame( uplo, 'U' ) ) then
                 do j = 1, n
                    sum = zero
                    do i = 1, j - 1
                       absa = abs( ap( k ) )
                       sum = sum + absa
                       work( i ) = work( i ) + absa
                       k = k + 1_ilp
                    end do
                    work( j ) = sum + abs( ap( k ) )
                    k = k + 1_ilp
                 end do
                 do i = 1, n
                    sum = work( i )
                    if( value < sum .or. stdlib_disnan( sum ) ) value = sum
                 end do
              else
                 do i = 1, n
                    work( i ) = zero
                 end do
                 do j = 1, n
                    sum = work( j ) + abs( ap( k ) )
                    k = k + 1_ilp
                    do i = j + 1, n
                       absa = abs( ap( k ) )
                       sum = sum + absa
                       work( i ) = work( i ) + absa
                       k = k + 1_ilp
                    end do
                    if( value < sum .or. stdlib_disnan( sum ) ) value = sum
                 end do
              end if
           else if( ( stdlib_lsame( norm, 'F' ) ) .or. ( stdlib_lsame( norm, 'E' ) ) ) &
                     then
              ! find normf(a).
              scale = zero
              sum = one
              k = 2_ilp
              if( stdlib_lsame( uplo, 'U' ) ) then
                 do j = 2, n
                    call stdlib_zlassq( j-1, ap( k ), 1_ilp, scale, sum )
                    k = k + j
                 end do
              else
                 do j = 1, n - 1
                    call stdlib_zlassq( n-j, ap( k ), 1_ilp, scale, sum )
                    k = k + n - j + 1_ilp
                 end do
              end if
              sum = 2_ilp*sum
              k = 1_ilp
              do i = 1, n
                 if( real( ap( k ),KIND=dp)/=zero ) then
                    absa = abs( real( ap( k ),KIND=dp) )
                    if( scale<absa ) then
                       sum = one + sum*( scale / absa )**2_ilp
                       scale = absa
                    else
                       sum = sum + ( absa / scale )**2_ilp
                    end if
                 end if
                 if( aimag( ap( k ) )/=zero ) then
                    absa = abs( aimag( ap( k ) ) )
                    if( scale<absa ) then
                       sum = one + sum*( scale / absa )**2_ilp
                       scale = absa
                    else
                       sum = sum + ( absa / scale )**2_ilp
                    end if
                 end if
                 if( stdlib_lsame( uplo, 'U' ) ) then
                    k = k + i + 1_ilp
                 else
                    k = k + n - i + 1_ilp
                 end if
              end do
              value = scale*sqrt( sum )
           end if
           stdlib_zlansp = value
           return
     end function stdlib_zlansp




     real(sp) module function stdlib_clanhb( norm, uplo, n, k, ab, ldab,work )
     !! CLANHB returns the value of the one norm,  or the Frobenius norm, or
     !! the  infinity norm,  or the element of  largest absolute value  of an
     !! n by n hermitian band matrix A,  with k super-diagonals.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: norm, uplo
           integer(ilp), intent(in) :: k, ldab, n
           ! Array Arguments 
           real(sp), intent(out) :: work(*)
           complex(sp), intent(in) :: ab(ldab,*)
       ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j, l
           real(sp) :: absa, scale, sum, value
           ! Intrinsic Functions 
           ! Executable Statements 
           if( n==0_ilp ) then
              value = zero
           else if( stdlib_lsame( norm, 'M' ) ) then
              ! find max(abs(a(i,j))).
              value = zero
              if( stdlib_lsame( uplo, 'U' ) ) then
                 do j = 1, n
                    do i = max( k+2-j, 1 ), k
                       sum = abs( ab( i, j ) )
                       if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
                    end do
                    sum = abs( real( ab( k+1, j ),KIND=sp) )
                    if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
                 end do
              else
                 do j = 1, n
                    sum = abs( real( ab( 1_ilp, j ),KIND=sp) )
                    if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
                    do i = 2, min( n+1-j, k+1 )
                       sum = abs( ab( i, j ) )
                       if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
                    end do
                 end do
              end if
           else if( ( stdlib_lsame( norm, 'I' ) ) .or. ( stdlib_lsame( norm, 'O' ) ) .or.( &
                     norm=='1' ) ) then
              ! find normi(a) ( = norm1(a), since a is hermitian).
              value = zero
              if( stdlib_lsame( uplo, 'U' ) ) then
                 do j = 1, n
                    sum = zero
                    l = k + 1_ilp - j
                    do i = max( 1, j-k ), j - 1
                       absa = abs( ab( l+i, j ) )
                       sum = sum + absa
                       work( i ) = work( i ) + absa
                    end do
                    work( j ) = sum + abs( real( ab( k+1, j ),KIND=sp) )
                 end do
                 do i = 1, n
                    sum = work( i )
                    if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
                 end do
              else
                 do i = 1, n
                    work( i ) = zero
                 end do
                 do j = 1, n
                    sum = work( j ) + abs( real( ab( 1_ilp, j ),KIND=sp) )
                    l = 1_ilp - j
                    do i = j + 1, min( n, j+k )
                       absa = abs( ab( l+i, j ) )
                       sum = sum + absa
                       work( i ) = work( i ) + absa
                    end do
                    if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
                 end do
              end if
           else if( ( stdlib_lsame( norm, 'F' ) ) .or. ( stdlib_lsame( norm, 'E' ) ) ) &
                     then
              ! find normf(a).
              scale = zero
              sum = one
              if( k>0_ilp ) then
                 if( stdlib_lsame( uplo, 'U' ) ) then
                    do j = 2, n
                       call stdlib_classq( min( j-1, k ), ab( max( k+2-j, 1_ilp ), j ),1_ilp, scale, sum )
                                 
                    end do
                    l = k + 1_ilp
                 else
                    do j = 1, n - 1
                       call stdlib_classq( min( n-j, k ), ab( 2_ilp, j ), 1_ilp, scale,sum )
                    end do
                    l = 1_ilp
                 end if
                 sum = 2_ilp*sum
              else
                 l = 1_ilp
              end if
              do j = 1, n
                 if( real( ab( l, j ),KIND=sp)/=zero ) then
                    absa = abs( real( ab( l, j ),KIND=sp) )
                    if( scale<absa ) then
                       sum = one + sum*( scale / absa )**2_ilp
                       scale = absa
                    else
                       sum = sum + ( absa / scale )**2_ilp
                    end if
                 end if
              end do
              value = scale*sqrt( sum )
           end if
           stdlib_clanhb = value
           return
     end function stdlib_clanhb

     real(dp) module function stdlib_zlanhb( norm, uplo, n, k, ab, ldab,work )
     !! ZLANHB returns the value of the one norm,  or the Frobenius norm, or
     !! the  infinity norm,  or the element of  largest absolute value  of an
     !! n by n hermitian band matrix A,  with k super-diagonals.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: norm, uplo
           integer(ilp), intent(in) :: k, ldab, n
           ! Array Arguments 
           real(dp), intent(out) :: work(*)
           complex(dp), intent(in) :: ab(ldab,*)
       ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j, l
           real(dp) :: absa, scale, sum, value
           ! Intrinsic Functions 
           ! Executable Statements 
           if( n==0_ilp ) then
              value = zero
           else if( stdlib_lsame( norm, 'M' ) ) then
              ! find max(abs(a(i,j))).
              value = zero
              if( stdlib_lsame( uplo, 'U' ) ) then
                 do j = 1, n
                    do i = max( k+2-j, 1 ), k
                       sum = abs( ab( i, j ) )
                       if( value < sum .or. stdlib_disnan( sum ) ) value = sum
                    end do
                    sum = abs( real( ab( k+1, j ),KIND=dp) )
                    if( value < sum .or. stdlib_disnan( sum ) ) value = sum
                 end do
              else
                 do j = 1, n
                    sum = abs( real( ab( 1_ilp, j ),KIND=dp) )
                    if( value < sum .or. stdlib_disnan( sum ) ) value = sum
                    do i = 2, min( n+1-j, k+1 )
                       sum = abs( ab( i, j ) )
                       if( value < sum .or. stdlib_disnan( sum ) ) value = sum
                    end do
                 end do
              end if
           else if( ( stdlib_lsame( norm, 'I' ) ) .or. ( stdlib_lsame( norm, 'O' ) ) .or.( &
                     norm=='1' ) ) then
              ! find normi(a) ( = norm1(a), since a is hermitian).
              value = zero
              if( stdlib_lsame( uplo, 'U' ) ) then
                 do j = 1, n
                    sum = zero
                    l = k + 1_ilp - j
                    do i = max( 1, j-k ), j - 1
                       absa = abs( ab( l+i, j ) )
                       sum = sum + absa
                       work( i ) = work( i ) + absa
                    end do
                    work( j ) = sum + abs( real( ab( k+1, j ),KIND=dp) )
                 end do
                 do i = 1, n
                    sum = work( i )
                    if( value < sum .or. stdlib_disnan( sum ) ) value = sum
                 end do
              else
                 do i = 1, n
                    work( i ) = zero
                 end do
                 do j = 1, n
                    sum = work( j ) + abs( real( ab( 1_ilp, j ),KIND=dp) )
                    l = 1_ilp - j
                    do i = j + 1, min( n, j+k )
                       absa = abs( ab( l+i, j ) )
                       sum = sum + absa
                       work( i ) = work( i ) + absa
                    end do
                    if( value < sum .or. stdlib_disnan( sum ) ) value = sum
                 end do
              end if
           else if( ( stdlib_lsame( norm, 'F' ) ) .or. ( stdlib_lsame( norm, 'E' ) ) ) &
                     then
              ! find normf(a).
              scale = zero
              sum = one
              if( k>0_ilp ) then
                 if( stdlib_lsame( uplo, 'U' ) ) then
                    do j = 2, n
                       call stdlib_zlassq( min( j-1, k ), ab( max( k+2-j, 1_ilp ), j ),1_ilp, scale, sum )
                                 
                    end do
                    l = k + 1_ilp
                 else
                    do j = 1, n - 1
                       call stdlib_zlassq( min( n-j, k ), ab( 2_ilp, j ), 1_ilp, scale,sum )
                    end do
                    l = 1_ilp
                 end if
                 sum = 2_ilp*sum
              else
                 l = 1_ilp
              end if
              do j = 1, n
                 if( real( ab( l, j ),KIND=dp)/=zero ) then
                    absa = abs( real( ab( l, j ),KIND=dp) )
                    if( scale<absa ) then
                       sum = one + sum*( scale / absa )**2_ilp
                       scale = absa
                    else
                       sum = sum + ( absa / scale )**2_ilp
                    end if
                 end if
              end do
              value = scale*sqrt( sum )
           end if
           stdlib_zlanhb = value
           return
     end function stdlib_zlanhb




     real(sp) module function stdlib_slansb( norm, uplo, n, k, ab, ldab,work )
     !! SLANSB returns the value of the one norm,  or the Frobenius norm, or
     !! the  infinity norm,  or the element of  largest absolute value  of an
     !! n by n symmetric band matrix A,  with k super-diagonals.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: norm, uplo
           integer(ilp), intent(in) :: k, ldab, n
           ! Array Arguments 
           real(sp), intent(in) :: ab(ldab,*)
           real(sp), intent(out) :: work(*)
       ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j, l
           real(sp) :: absa, scale, sum, value
           ! Intrinsic Functions 
           ! Executable Statements 
           if( n==0_ilp ) then
              value = zero
           else if( stdlib_lsame( norm, 'M' ) ) then
              ! find max(abs(a(i,j))).
              value = zero
              if( stdlib_lsame( uplo, 'U' ) ) then
                 do j = 1, n
                    do i = max( k+2-j, 1 ), k + 1
                       sum = abs( ab( i, j ) )
                       if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
                    end do
                 end do
              else
                 do j = 1, n
                    do i = 1, min( n+1-j, k+1 )
                       sum = abs( ab( i, j ) )
                       if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
                    end do
                 end do
              end if
           else if( ( stdlib_lsame( norm, 'I' ) ) .or. ( stdlib_lsame( norm, 'O' ) ) .or.( &
                     norm=='1' ) ) then
              ! find normi(a) ( = norm1(a), since a is symmetric).
              value = zero
              if( stdlib_lsame( uplo, 'U' ) ) then
                 do j = 1, n
                    sum = zero
                    l = k + 1_ilp - j
                    do i = max( 1, j-k ), j - 1
                       absa = abs( ab( l+i, j ) )
                       sum = sum + absa
                       work( i ) = work( i ) + absa
                    end do
                    work( j ) = sum + abs( ab( k+1, j ) )
                 end do
                 do i = 1, n
                    sum = work( i )
                    if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
                 end do
              else
                 do i = 1, n
                    work( i ) = zero
                 end do
                 do j = 1, n
                    sum = work( j ) + abs( ab( 1_ilp, j ) )
                    l = 1_ilp - j
                    do i = j + 1, min( n, j+k )
                       absa = abs( ab( l+i, j ) )
                       sum = sum + absa
                       work( i ) = work( i ) + absa
                    end do
                    if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
                 end do
              end if
           else if( ( stdlib_lsame( norm, 'F' ) ) .or. ( stdlib_lsame( norm, 'E' ) ) ) &
                     then
              ! find normf(a).
              scale = zero
              sum = one
              if( k>0_ilp ) then
                 if( stdlib_lsame( uplo, 'U' ) ) then
                    do j = 2, n
                       call stdlib_slassq( min( j-1, k ), ab( max( k+2-j, 1_ilp ), j ),1_ilp, scale, sum )
                                 
                    end do
                    l = k + 1_ilp
                 else
                    do j = 1, n - 1
                       call stdlib_slassq( min( n-j, k ), ab( 2_ilp, j ), 1_ilp, scale,sum )
                    end do
                    l = 1_ilp
                 end if
                 sum = 2_ilp*sum
              else
                 l = 1_ilp
              end if
              call stdlib_slassq( n, ab( l, 1_ilp ), ldab, scale, sum )
              value = scale*sqrt( sum )
           end if
           stdlib_slansb = value
           return
     end function stdlib_slansb

     real(dp) module function stdlib_dlansb( norm, uplo, n, k, ab, ldab,work )
     !! DLANSB returns the value of the one norm,  or the Frobenius norm, or
     !! the  infinity norm,  or the element of  largest absolute value  of an
     !! n by n symmetric band matrix A,  with k super-diagonals.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: norm, uplo
           integer(ilp), intent(in) :: k, ldab, n
           ! Array Arguments 
           real(dp), intent(in) :: ab(ldab,*)
           real(dp), intent(out) :: work(*)
       ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j, l
           real(dp) :: absa, scale, sum, value
           ! Intrinsic Functions 
           ! Executable Statements 
           if( n==0_ilp ) then
              value = zero
           else if( stdlib_lsame( norm, 'M' ) ) then
              ! find max(abs(a(i,j))).
              value = zero
              if( stdlib_lsame( uplo, 'U' ) ) then
                 do j = 1, n
                    do i = max( k+2-j, 1 ), k + 1
                       sum = abs( ab( i, j ) )
                       if( value < sum .or. stdlib_disnan( sum ) ) value = sum
                    end do
                 end do
              else
                 do j = 1, n
                    do i = 1, min( n+1-j, k+1 )
                       sum = abs( ab( i, j ) )
                       if( value < sum .or. stdlib_disnan( sum ) ) value = sum
                    end do
                 end do
              end if
           else if( ( stdlib_lsame( norm, 'I' ) ) .or. ( stdlib_lsame( norm, 'O' ) ) .or.( &
                     norm=='1' ) ) then
              ! find normi(a) ( = norm1(a), since a is symmetric).
              value = zero
              if( stdlib_lsame( uplo, 'U' ) ) then
                 do j = 1, n
                    sum = zero
                    l = k + 1_ilp - j
                    do i = max( 1, j-k ), j - 1
                       absa = abs( ab( l+i, j ) )
                       sum = sum + absa
                       work( i ) = work( i ) + absa
                    end do
                    work( j ) = sum + abs( ab( k+1, j ) )
                 end do
                 do i = 1, n
                    sum = work( i )
                    if( value < sum .or. stdlib_disnan( sum ) ) value = sum
                 end do
              else
                 do i = 1, n
                    work( i ) = zero
                 end do
                 do j = 1, n
                    sum = work( j ) + abs( ab( 1_ilp, j ) )
                    l = 1_ilp - j
                    do i = j + 1, min( n, j+k )
                       absa = abs( ab( l+i, j ) )
                       sum = sum + absa
                       work( i ) = work( i ) + absa
                    end do
                    if( value < sum .or. stdlib_disnan( sum ) ) value = sum
                 end do
              end if
           else if( ( stdlib_lsame( norm, 'F' ) ) .or. ( stdlib_lsame( norm, 'E' ) ) ) &
                     then
              ! find normf(a).
              scale = zero
              sum = one
              if( k>0_ilp ) then
                 if( stdlib_lsame( uplo, 'U' ) ) then
                    do j = 2, n
                       call stdlib_dlassq( min( j-1, k ), ab( max( k+2-j, 1_ilp ), j ),1_ilp, scale, sum )
                                 
                    end do
                    l = k + 1_ilp
                 else
                    do j = 1, n - 1
                       call stdlib_dlassq( min( n-j, k ), ab( 2_ilp, j ), 1_ilp, scale,sum )
                    end do
                    l = 1_ilp
                 end if
                 sum = 2_ilp*sum
              else
                 l = 1_ilp
              end if
              call stdlib_dlassq( n, ab( l, 1_ilp ), ldab, scale, sum )
              value = scale*sqrt( sum )
           end if
           stdlib_dlansb = value
           return
     end function stdlib_dlansb


     real(sp) module function stdlib_clansb( norm, uplo, n, k, ab, ldab,work )
     !! CLANSB returns the value of the one norm,  or the Frobenius norm, or
     !! the  infinity norm,  or the element of  largest absolute value  of an
     !! n by n symmetric band matrix A,  with k super-diagonals.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: norm, uplo
           integer(ilp), intent(in) :: k, ldab, n
           ! Array Arguments 
           real(sp), intent(out) :: work(*)
           complex(sp), intent(in) :: ab(ldab,*)
       ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j, l
           real(sp) :: absa, scale, sum, value
           ! Intrinsic Functions 
           ! Executable Statements 
           if( n==0_ilp ) then
              value = zero
           else if( stdlib_lsame( norm, 'M' ) ) then
              ! find max(abs(a(i,j))).
              value = zero
              if( stdlib_lsame( uplo, 'U' ) ) then
                 do j = 1, n
                    do i = max( k+2-j, 1 ), k + 1
                       sum = abs( ab( i, j ) )
                       if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
                    end do
                 end do
              else
                 do j = 1, n
                    do i = 1, min( n+1-j, k+1 )
                       sum = abs( ab( i, j ) )
                       if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
                    end do
                 end do
              end if
           else if( ( stdlib_lsame( norm, 'I' ) ) .or. ( stdlib_lsame( norm, 'O' ) ) .or.( &
                     norm=='1' ) ) then
              ! find normi(a) ( = norm1(a), since a is symmetric).
              value = zero
              if( stdlib_lsame( uplo, 'U' ) ) then
                 do j = 1, n
                    sum = zero
                    l = k + 1_ilp - j
                    do i = max( 1, j-k ), j - 1
                       absa = abs( ab( l+i, j ) )
                       sum = sum + absa
                       work( i ) = work( i ) + absa
                    end do
                    work( j ) = sum + abs( ab( k+1, j ) )
                 end do
                 do i = 1, n
                    sum = work( i )
                    if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
                 end do
              else
                 do i = 1, n
                    work( i ) = zero
                 end do
                 do j = 1, n
                    sum = work( j ) + abs( ab( 1_ilp, j ) )
                    l = 1_ilp - j
                    do i = j + 1, min( n, j+k )
                       absa = abs( ab( l+i, j ) )
                       sum = sum + absa
                       work( i ) = work( i ) + absa
                    end do
                    if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
                 end do
              end if
           else if( ( stdlib_lsame( norm, 'F' ) ) .or. ( stdlib_lsame( norm, 'E' ) ) ) &
                     then
              ! find normf(a).
              scale = zero
              sum = one
              if( k>0_ilp ) then
                 if( stdlib_lsame( uplo, 'U' ) ) then
                    do j = 2, n
                       call stdlib_classq( min( j-1, k ), ab( max( k+2-j, 1_ilp ), j ),1_ilp, scale, sum )
                                 
                    end do
                    l = k + 1_ilp
                 else
                    do j = 1, n - 1
                       call stdlib_classq( min( n-j, k ), ab( 2_ilp, j ), 1_ilp, scale,sum )
                    end do
                    l = 1_ilp
                 end if
                 sum = 2_ilp*sum
              else
                 l = 1_ilp
              end if
              call stdlib_classq( n, ab( l, 1_ilp ), ldab, scale, sum )
              value = scale*sqrt( sum )
           end if
           stdlib_clansb = value
           return
     end function stdlib_clansb

     real(dp) module function stdlib_zlansb( norm, uplo, n, k, ab, ldab,work )
     !! ZLANSB returns the value of the one norm,  or the Frobenius norm, or
     !! the  infinity norm,  or the element of  largest absolute value  of an
     !! n by n symmetric band matrix A,  with k super-diagonals.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: norm, uplo
           integer(ilp), intent(in) :: k, ldab, n
           ! Array Arguments 
           real(dp), intent(out) :: work(*)
           complex(dp), intent(in) :: ab(ldab,*)
       ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j, l
           real(dp) :: absa, scale, sum, value
           ! Intrinsic Functions 
           ! Executable Statements 
           if( n==0_ilp ) then
              value = zero
           else if( stdlib_lsame( norm, 'M' ) ) then
              ! find max(abs(a(i,j))).
              value = zero
              if( stdlib_lsame( uplo, 'U' ) ) then
                 do j = 1, n
                    do i = max( k+2-j, 1 ), k + 1
                       sum = abs( ab( i, j ) )
                       if( value < sum .or. stdlib_disnan( sum ) ) value = sum
                    end do
                 end do
              else
                 do j = 1, n
                    do i = 1, min( n+1-j, k+1 )
                       sum = abs( ab( i, j ) )
                       if( value < sum .or. stdlib_disnan( sum ) ) value = sum
                    end do
                 end do
              end if
           else if( ( stdlib_lsame( norm, 'I' ) ) .or. ( stdlib_lsame( norm, 'O' ) ) .or.( &
                     norm=='1' ) ) then
              ! find normi(a) ( = norm1(a), since a is symmetric).
              value = zero
              if( stdlib_lsame( uplo, 'U' ) ) then
                 do j = 1, n
                    sum = zero
                    l = k + 1_ilp - j
                    do i = max( 1, j-k ), j - 1
                       absa = abs( ab( l+i, j ) )
                       sum = sum + absa
                       work( i ) = work( i ) + absa
                    end do
                    work( j ) = sum + abs( ab( k+1, j ) )
                 end do
                 do i = 1, n
                    sum = work( i )
                    if( value < sum .or. stdlib_disnan( sum ) ) value = sum
                 end do
              else
                 do i = 1, n
                    work( i ) = zero
                 end do
                 do j = 1, n
                    sum = work( j ) + abs( ab( 1_ilp, j ) )
                    l = 1_ilp - j
                    do i = j + 1, min( n, j+k )
                       absa = abs( ab( l+i, j ) )
                       sum = sum + absa
                       work( i ) = work( i ) + absa
                    end do
                    if( value < sum .or. stdlib_disnan( sum ) ) value = sum
                 end do
              end if
           else if( ( stdlib_lsame( norm, 'F' ) ) .or. ( stdlib_lsame( norm, 'E' ) ) ) &
                     then
              ! find normf(a).
              scale = zero
              sum = one
              if( k>0_ilp ) then
                 if( stdlib_lsame( uplo, 'U' ) ) then
                    do j = 2, n
                       call stdlib_zlassq( min( j-1, k ), ab( max( k+2-j, 1_ilp ), j ),1_ilp, scale, sum )
                                 
                    end do
                    l = k + 1_ilp
                 else
                    do j = 1, n - 1
                       call stdlib_zlassq( min( n-j, k ), ab( 2_ilp, j ), 1_ilp, scale,sum )
                    end do
                    l = 1_ilp
                 end if
                 sum = 2_ilp*sum
              else
                 l = 1_ilp
              end if
              call stdlib_zlassq( n, ab( l, 1_ilp ), ldab, scale, sum )
              value = scale*sqrt( sum )
           end if
           stdlib_zlansb = value
           return
     end function stdlib_zlansb




     pure real(sp) module function stdlib_clanht( norm, n, d, e )
     !! CLANHT returns the value of the one norm,  or the Frobenius norm, or
     !! the  infinity norm,  or the  element of  largest absolute value  of a
     !! complex Hermitian tridiagonal matrix A.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: norm
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           real(sp), intent(in) :: d(*)
           complex(sp), intent(in) :: e(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i
           real(sp) :: anorm, scale, sum
           ! Intrinsic Functions 
           ! Executable Statements 
           if( n<=0_ilp ) then
              anorm = zero
           else if( stdlib_lsame( norm, 'M' ) ) then
              ! find max(abs(a(i,j))).
              anorm = abs( d( n ) )
              do i = 1, n - 1
                 sum = abs( d( i ) )
                 if( anorm < sum .or. stdlib_sisnan( sum ) ) anorm = sum
                 sum = abs( e( i ) )
                 if( anorm < sum .or. stdlib_sisnan( sum ) ) anorm = sum
              end do
           else if( stdlib_lsame( norm, 'O' ) .or. norm=='1' .or.stdlib_lsame( norm, 'I' ) ) &
                     then
              ! find norm1(a).
              if( n==1_ilp ) then
                 anorm = abs( d( 1_ilp ) )
              else
                 anorm = abs( d( 1_ilp ) )+abs( e( 1_ilp ) )
                 sum = abs( e( n-1 ) )+abs( d( n ) )
                 if( anorm < sum .or. stdlib_sisnan( sum ) ) anorm = sum
                 do i = 2, n - 1
                    sum = abs( d( i ) )+abs( e( i ) )+abs( e( i-1 ) )
                    if( anorm < sum .or. stdlib_sisnan( sum ) ) anorm = sum
                 end do
              end if
           else if( ( stdlib_lsame( norm, 'F' ) ) .or. ( stdlib_lsame( norm, 'E' ) ) ) &
                     then
              ! find normf(a).
              scale = zero
              sum = one
              if( n>1_ilp ) then
                 call stdlib_classq( n-1, e, 1_ilp, scale, sum )
                 sum = 2_ilp*sum
              end if
              call stdlib_slassq( n, d, 1_ilp, scale, sum )
              anorm = scale*sqrt( sum )
           end if
           stdlib_clanht = anorm
           return
     end function stdlib_clanht

     pure real(dp) module function stdlib_zlanht( norm, n, d, e )
     !! ZLANHT returns the value of the one norm,  or the Frobenius norm, or
     !! the  infinity norm,  or the  element of  largest absolute value  of a
     !! complex Hermitian tridiagonal matrix A.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: norm
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           real(dp), intent(in) :: d(*)
           complex(dp), intent(in) :: e(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i
           real(dp) :: anorm, scale, sum
           ! Intrinsic Functions 
           ! Executable Statements 
           if( n<=0_ilp ) then
              anorm = zero
           else if( stdlib_lsame( norm, 'M' ) ) then
              ! find max(abs(a(i,j))).
              anorm = abs( d( n ) )
              do i = 1, n - 1
                 sum =  abs( d( i ) )
                 if( anorm < sum .or. stdlib_disnan( sum ) ) anorm = sum
                 sum = abs( e( i ) )
                 if( anorm < sum .or. stdlib_disnan( sum ) ) anorm = sum
              end do
           else if( stdlib_lsame( norm, 'O' ) .or. norm=='1' .or.stdlib_lsame( norm, 'I' ) ) &
                     then
              ! find norm1(a).
              if( n==1_ilp ) then
                 anorm = abs( d( 1_ilp ) )
              else
                 anorm = abs( d( 1_ilp ) )+abs( e( 1_ilp ) )
                 sum = abs( e( n-1 ) )+abs( d( n ) )
                 if( anorm < sum .or. stdlib_disnan( sum ) ) anorm = sum
                 do i = 2, n - 1
                    sum = abs( d( i ) )+abs( e( i ) )+abs( e( i-1 ) )
                    if( anorm < sum .or. stdlib_disnan( sum ) ) anorm = sum
                 end do
              end if
           else if( ( stdlib_lsame( norm, 'F' ) ) .or. ( stdlib_lsame( norm, 'E' ) ) ) &
                     then
              ! find normf(a).
              scale = zero
              sum = one
              if( n>1_ilp ) then
                 call stdlib_zlassq( n-1, e, 1_ilp, scale, sum )
                 sum = 2_ilp*sum
              end if
              call stdlib_dlassq( n, d, 1_ilp, scale, sum )
              anorm = scale*sqrt( sum )
           end if
           stdlib_zlanht = anorm
           return
     end function stdlib_zlanht




     pure real(sp) module function stdlib_slanst( norm, n, d, e )
     !! SLANST returns the value of the one norm,  or the Frobenius norm, or
     !! the  infinity norm,  or the  element of  largest absolute value  of a
     !! real symmetric tridiagonal matrix A.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: norm
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           real(sp), intent(in) :: d(*), e(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i
           real(sp) :: anorm, scale, sum
           ! Intrinsic Functions 
           ! Executable Statements 
           if( n<=0_ilp ) then
              anorm = zero
           else if( stdlib_lsame( norm, 'M' ) ) then
              ! find max(abs(a(i,j))).
              anorm = abs( d( n ) )
              do i = 1, n - 1
                 sum = abs( d( i ) )
                 if( anorm < sum .or. stdlib_sisnan( sum ) ) anorm = sum
                 sum = abs( e( i ) )
                 if( anorm < sum .or. stdlib_sisnan( sum ) ) anorm = sum
              end do
           else if( stdlib_lsame( norm, 'O' ) .or. norm=='1' .or.stdlib_lsame( norm, 'I' ) ) &
                     then
              ! find norm1(a).
              if( n==1_ilp ) then
                 anorm = abs( d( 1_ilp ) )
              else
                 anorm = abs( d( 1_ilp ) )+abs( e( 1_ilp ) )
                 sum = abs( e( n-1 ) )+abs( d( n ) )
                 if( anorm < sum .or. stdlib_sisnan( sum ) ) anorm = sum
                 do i = 2, n - 1
                    sum = abs( d( i ) )+abs( e( i ) )+abs( e( i-1 ) )
                    if( anorm < sum .or. stdlib_sisnan( sum ) ) anorm = sum
                 end do
              end if
           else if( ( stdlib_lsame( norm, 'F' ) ) .or. ( stdlib_lsame( norm, 'E' ) ) ) &
                     then
              ! find normf(a).
              scale = zero
              sum = one
              if( n>1_ilp ) then
                 call stdlib_slassq( n-1, e, 1_ilp, scale, sum )
                 sum = 2_ilp*sum
              end if
              call stdlib_slassq( n, d, 1_ilp, scale, sum )
              anorm = scale*sqrt( sum )
           end if
           stdlib_slanst = anorm
           return
     end function stdlib_slanst

     pure real(dp) module function stdlib_dlanst( norm, n, d, e )
     !! DLANST returns the value of the one norm,  or the Frobenius norm, or
     !! the  infinity norm,  or the  element of  largest absolute value  of a
     !! real symmetric tridiagonal matrix A.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: norm
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           real(dp), intent(in) :: d(*), e(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i
           real(dp) :: anorm, scale, sum
           ! Intrinsic Functions 
           ! Executable Statements 
           if( n<=0_ilp ) then
              anorm = zero
           else if( stdlib_lsame( norm, 'M' ) ) then
              ! find max(abs(a(i,j))).
              anorm = abs( d( n ) )
              do i = 1, n - 1
                 sum = abs( d( i ) )
                 if( anorm < sum .or. stdlib_disnan( sum ) ) anorm = sum
                 sum = abs( e( i ) )
                 if( anorm < sum .or. stdlib_disnan( sum ) ) anorm = sum
              end do
           else if( stdlib_lsame( norm, 'O' ) .or. norm=='1' .or.stdlib_lsame( norm, 'I' ) ) &
                     then
              ! find norm1(a).
              if( n==1_ilp ) then
                 anorm = abs( d( 1_ilp ) )
              else
                 anorm = abs( d( 1_ilp ) )+abs( e( 1_ilp ) )
                 sum = abs( e( n-1 ) )+abs( d( n ) )
                 if( anorm < sum .or. stdlib_disnan( sum ) ) anorm = sum
                 do i = 2, n - 1
                    sum = abs( d( i ) )+abs( e( i ) )+abs( e( i-1 ) )
                    if( anorm < sum .or. stdlib_disnan( sum ) ) anorm = sum
                 end do
              end if
           else if( ( stdlib_lsame( norm, 'F' ) ) .or. ( stdlib_lsame( norm, 'E' ) ) ) &
                     then
              ! find normf(a).
              scale = zero
              sum = one
              if( n>1_ilp ) then
                 call stdlib_dlassq( n-1, e, 1_ilp, scale, sum )
                 sum = 2_ilp*sum
              end if
              call stdlib_dlassq( n, d, 1_ilp, scale, sum )
              anorm = scale*sqrt( sum )
           end if
           stdlib_dlanst = anorm
           return
     end function stdlib_dlanst




     real(sp) module function stdlib_slantr( norm, uplo, diag, m, n, a, lda,work )
     !! SLANTR returns the value of the one norm,  or the Frobenius norm, or
     !! the  infinity norm,  or the  element of  largest absolute value  of a
     !! trapezoidal or triangular matrix A.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: diag, norm, uplo
           integer(ilp), intent(in) :: lda, m, n
           ! Array Arguments 
           real(sp), intent(in) :: a(lda,*)
           real(sp), intent(out) :: work(*)
       ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: udiag
           integer(ilp) :: i, j
           real(sp) :: scale, sum, value
           ! Intrinsic Functions 
           ! Executable Statements 
           if( min( m, n )==0_ilp ) then
              value = zero
           else if( stdlib_lsame( norm, 'M' ) ) then
              ! find max(abs(a(i,j))).
              if( stdlib_lsame( diag, 'U' ) ) then
                 value = one
                 if( stdlib_lsame( uplo, 'U' ) ) then
                    do j = 1, n
                       do i = 1, min( m, j-1 )
                          sum = abs( a( i, j ) )
                          if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
                       end do
                    end do
                 else
                    do j = 1, n
                       do i = j + 1, m
                          sum = abs( a( i, j ) )
                          if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
                       end do
                    end do
                 end if
              else
                 value = zero
                 if( stdlib_lsame( uplo, 'U' ) ) then
                    do j = 1, n
                       do i = 1, min( m, j )
                          sum = abs( a( i, j ) )
                          if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
                       end do
                    end do
                 else
                    do j = 1, n
                       do i = j, m
                          sum = abs( a( i, j ) )
                          if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
                       end do
                    end do
                 end if
              end if
           else if( ( stdlib_lsame( norm, 'O' ) ) .or. ( norm=='1' ) ) then
              ! find norm1(a).
              value = zero
              udiag = stdlib_lsame( diag, 'U' )
              if( stdlib_lsame( uplo, 'U' ) ) then
                 do j = 1, n
                    if( ( udiag ) .and. ( j<=m ) ) then
                       sum = one
                       do i = 1, j - 1
                          sum = sum + abs( a( i, j ) )
                       end do
                    else
                       sum = zero
                       do i = 1, min( m, j )
                          sum = sum + abs( a( i, j ) )
                       end do
                    end if
                    if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
                 end do
              else
                 do j = 1, n
                    if( udiag ) then
                       sum = one
                       do i = j + 1, m
                          sum = sum + abs( a( i, j ) )
                       end do
                    else
                       sum = zero
                       do i = j, m
                          sum = sum + abs( a( i, j ) )
                       end do
                    end if
                    if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
                 end do
              end if
           else if( stdlib_lsame( norm, 'I' ) ) then
              ! find normi(a).
              if( stdlib_lsame( uplo, 'U' ) ) then
                 if( stdlib_lsame( diag, 'U' ) ) then
                    do i = 1, m
                       work( i ) = one
                    end do
                    do j = 1, n
                       do i = 1, min( m, j-1 )
                          work( i ) = work( i ) + abs( a( i, j ) )
                       end do
                    end do
                 else
                    do i = 1, m
                       work( i ) = zero
                    end do
                    do j = 1, n
                       do i = 1, min( m, j )
                          work( i ) = work( i ) + abs( a( i, j ) )
                       end do
                    end do
                 end if
              else
                 if( stdlib_lsame( diag, 'U' ) ) then
                    do i = 1, min( m, n )
                       work( i ) = one
                    end do
                    do i = n + 1, m
                       work( i ) = zero
                    end do
                    do j = 1, n
                       do i = j + 1, m
                          work( i ) = work( i ) + abs( a( i, j ) )
                       end do
                    end do
                 else
                    do i = 1, m
                       work( i ) = zero
                    end do
                    do j = 1, n
                       do i = j, m
                          work( i ) = work( i ) + abs( a( i, j ) )
                       end do
                    end do
                 end if
              end if
              value = zero
              do i = 1, m
                 sum = work( i )
                 if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
              end do
           else if( ( stdlib_lsame( norm, 'F' ) ) .or. ( stdlib_lsame( norm, 'E' ) ) ) &
                     then
              ! find normf(a).
              if( stdlib_lsame( uplo, 'U' ) ) then
                 if( stdlib_lsame( diag, 'U' ) ) then
                    scale = one
                    sum = min( m, n )
                    do j = 2, n
                       call stdlib_slassq( min( m, j-1 ), a( 1_ilp, j ), 1_ilp, scale, sum )
                    end do
                 else
                    scale = zero
                    sum = one
                    do j = 1, n
                       call stdlib_slassq( min( m, j ), a( 1_ilp, j ), 1_ilp, scale, sum )
                    end do
                 end if
              else
                 if( stdlib_lsame( diag, 'U' ) ) then
                    scale = one
                    sum = min( m, n )
                    do j = 1, n
                       call stdlib_slassq( m-j, a( min( m, j+1 ), j ), 1_ilp, scale,sum )
                    end do
                 else
                    scale = zero
                    sum = one
                    do j = 1, n
                       call stdlib_slassq( m-j+1, a( j, j ), 1_ilp, scale, sum )
                    end do
                 end if
              end if
              value = scale*sqrt( sum )
           end if
           stdlib_slantr = value
           return
     end function stdlib_slantr

     real(dp) module function stdlib_dlantr( norm, uplo, diag, m, n, a, lda,work )
     !! DLANTR returns the value of the one norm,  or the Frobenius norm, or
     !! the  infinity norm,  or the  element of  largest absolute value  of a
     !! trapezoidal or triangular matrix A.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: diag, norm, uplo
           integer(ilp), intent(in) :: lda, m, n
           ! Array Arguments 
           real(dp), intent(in) :: a(lda,*)
           real(dp), intent(out) :: work(*)
       ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: udiag
           integer(ilp) :: i, j
           real(dp) :: scale, sum, value
           ! Intrinsic Functions 
           ! Executable Statements 
           if( min( m, n )==0_ilp ) then
              value = zero
           else if( stdlib_lsame( norm, 'M' ) ) then
              ! find max(abs(a(i,j))).
              if( stdlib_lsame( diag, 'U' ) ) then
                 value = one
                 if( stdlib_lsame( uplo, 'U' ) ) then
                    do j = 1, n
                       do i = 1, min( m, j-1 )
                          sum = abs( a( i, j ) )
                          if( value < sum .or. stdlib_disnan( sum ) ) value = sum
                       end do
                    end do
                 else
                    do j = 1, n
                       do i = j + 1, m
                          sum = abs( a( i, j ) )
                          if( value < sum .or. stdlib_disnan( sum ) ) value = sum
                       end do
                    end do
                 end if
              else
                 value = zero
                 if( stdlib_lsame( uplo, 'U' ) ) then
                    do j = 1, n
                       do i = 1, min( m, j )
                          sum = abs( a( i, j ) )
                          if( value < sum .or. stdlib_disnan( sum ) ) value = sum
                       end do
                    end do
                 else
                    do j = 1, n
                       do i = j, m
                          sum = abs( a( i, j ) )
                          if( value < sum .or. stdlib_disnan( sum ) ) value = sum
                       end do
                    end do
                 end if
              end if
           else if( ( stdlib_lsame( norm, 'O' ) ) .or. ( norm=='1' ) ) then
              ! find norm1(a).
              value = zero
              udiag = stdlib_lsame( diag, 'U' )
              if( stdlib_lsame( uplo, 'U' ) ) then
                 do j = 1, n
                    if( ( udiag ) .and. ( j<=m ) ) then
                       sum = one
                       do i = 1, j - 1
                          sum = sum + abs( a( i, j ) )
                       end do
                    else
                       sum = zero
                       do i = 1, min( m, j )
                          sum = sum + abs( a( i, j ) )
                       end do
                    end if
                    if( value < sum .or. stdlib_disnan( sum ) ) value = sum
                 end do
              else
                 do j = 1, n
                    if( udiag ) then
                       sum = one
                       do i = j + 1, m
                          sum = sum + abs( a( i, j ) )
                       end do
                    else
                       sum = zero
                       do i = j, m
                          sum = sum + abs( a( i, j ) )
                       end do
                    end if
                    if( value < sum .or. stdlib_disnan( sum ) ) value = sum
                 end do
              end if
           else if( stdlib_lsame( norm, 'I' ) ) then
              ! find normi(a).
              if( stdlib_lsame( uplo, 'U' ) ) then
                 if( stdlib_lsame( diag, 'U' ) ) then
                    do i = 1, m
                       work( i ) = one
                    end do
                    do j = 1, n
                       do i = 1, min( m, j-1 )
                          work( i ) = work( i ) + abs( a( i, j ) )
                       end do
                    end do
                 else
                    do i = 1, m
                       work( i ) = zero
                    end do
                    do j = 1, n
                       do i = 1, min( m, j )
                          work( i ) = work( i ) + abs( a( i, j ) )
                       end do
                    end do
                 end if
              else
                 if( stdlib_lsame( diag, 'U' ) ) then
                    do i = 1, min( m, n )
                       work( i ) = one
                    end do
                    do i = n + 1, m
                       work( i ) = zero
                    end do
                    do j = 1, n
                       do i = j + 1, m
                          work( i ) = work( i ) + abs( a( i, j ) )
                       end do
                    end do
                 else
                    do i = 1, m
                       work( i ) = zero
                    end do
                    do j = 1, n
                       do i = j, m
                          work( i ) = work( i ) + abs( a( i, j ) )
                       end do
                    end do
                 end if
              end if
              value = zero
              do i = 1, m
                 sum = work( i )
                 if( value < sum .or. stdlib_disnan( sum ) ) value = sum
              end do
           else if( ( stdlib_lsame( norm, 'F' ) ) .or. ( stdlib_lsame( norm, 'E' ) ) ) &
                     then
              ! find normf(a).
              if( stdlib_lsame( uplo, 'U' ) ) then
                 if( stdlib_lsame( diag, 'U' ) ) then
                    scale = one
                    sum = min( m, n )
                    do j = 2, n
                       call stdlib_dlassq( min( m, j-1 ), a( 1_ilp, j ), 1_ilp, scale, sum )
                    end do
                 else
                    scale = zero
                    sum = one
                    do j = 1, n
                       call stdlib_dlassq( min( m, j ), a( 1_ilp, j ), 1_ilp, scale, sum )
                    end do
                 end if
              else
                 if( stdlib_lsame( diag, 'U' ) ) then
                    scale = one
                    sum = min( m, n )
                    do j = 1, n
                       call stdlib_dlassq( m-j, a( min( m, j+1 ), j ), 1_ilp, scale,sum )
                    end do
                 else
                    scale = zero
                    sum = one
                    do j = 1, n
                       call stdlib_dlassq( m-j+1, a( j, j ), 1_ilp, scale, sum )
                    end do
                 end if
              end if
              value = scale*sqrt( sum )
           end if
           stdlib_dlantr = value
           return
     end function stdlib_dlantr


     real(sp) module function stdlib_clantr( norm, uplo, diag, m, n, a, lda,work )
     !! CLANTR returns the value of the one norm,  or the Frobenius norm, or
     !! the  infinity norm,  or the  element of  largest absolute value  of a
     !! trapezoidal or triangular matrix A.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: diag, norm, uplo
           integer(ilp), intent(in) :: lda, m, n
           ! Array Arguments 
           real(sp), intent(out) :: work(*)
           complex(sp), intent(in) :: a(lda,*)
       ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: udiag
           integer(ilp) :: i, j
           real(sp) :: scale, sum, value
           ! Intrinsic Functions 
           ! Executable Statements 
           if( min( m, n )==0_ilp ) then
              value = zero
           else if( stdlib_lsame( norm, 'M' ) ) then
              ! find max(abs(a(i,j))).
              if( stdlib_lsame( diag, 'U' ) ) then
                 value = one
                 if( stdlib_lsame( uplo, 'U' ) ) then
                    do j = 1, n
                       do i = 1, min( m, j-1 )
                          sum = abs( a( i, j ) )
                          if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
                       end do
                    end do
                 else
                    do j = 1, n
                       do i = j + 1, m
                          sum = abs( a( i, j ) )
                          if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
                       end do
                    end do
                 end if
              else
                 value = zero
                 if( stdlib_lsame( uplo, 'U' ) ) then
                    do j = 1, n
                       do i = 1, min( m, j )
                          sum = abs( a( i, j ) )
                          if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
                       end do
                    end do
                 else
                    do j = 1, n
                       do i = j, m
                          sum = abs( a( i, j ) )
                          if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
                       end do
                    end do
                 end if
              end if
           else if( ( stdlib_lsame( norm, 'O' ) ) .or. ( norm=='1' ) ) then
              ! find norm1(a).
              value = zero
              udiag = stdlib_lsame( diag, 'U' )
              if( stdlib_lsame( uplo, 'U' ) ) then
                 do j = 1, n
                    if( ( udiag ) .and. ( j<=m ) ) then
                       sum = one
                       do i = 1, j - 1
                          sum = sum + abs( a( i, j ) )
                       end do
                    else
                       sum = zero
                       do i = 1, min( m, j )
                          sum = sum + abs( a( i, j ) )
                       end do
                    end if
                    if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
                 end do
              else
                 do j = 1, n
                    if( udiag ) then
                       sum = one
                       do i = j + 1, m
                          sum = sum + abs( a( i, j ) )
                       end do
                    else
                       sum = zero
                       do i = j, m
                          sum = sum + abs( a( i, j ) )
                       end do
                    end if
                    if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
                 end do
              end if
           else if( stdlib_lsame( norm, 'I' ) ) then
              ! find normi(a).
              if( stdlib_lsame( uplo, 'U' ) ) then
                 if( stdlib_lsame( diag, 'U' ) ) then
                    do i = 1, m
                       work( i ) = one
                    end do
                    do j = 1, n
                       do i = 1, min( m, j-1 )
                          work( i ) = work( i ) + abs( a( i, j ) )
                       end do
                    end do
                 else
                    do i = 1, m
                       work( i ) = zero
                    end do
                    do j = 1, n
                       do i = 1, min( m, j )
                          work( i ) = work( i ) + abs( a( i, j ) )
                       end do
                    end do
                 end if
              else
                 if( stdlib_lsame( diag, 'U' ) ) then
                    do i = 1, min( m, n )
                       work( i ) = one
                    end do
                    do i = n + 1, m
                       work( i ) = zero
                    end do
                    do j = 1, n
                       do i = j + 1, m
                          work( i ) = work( i ) + abs( a( i, j ) )
                       end do
                    end do
                 else
                    do i = 1, m
                       work( i ) = zero
                    end do
                    do j = 1, n
                       do i = j, m
                          work( i ) = work( i ) + abs( a( i, j ) )
                       end do
                    end do
                 end if
              end if
              value = zero
              do i = 1, m
                 sum = work( i )
                 if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
              end do
           else if( ( stdlib_lsame( norm, 'F' ) ) .or. ( stdlib_lsame( norm, 'E' ) ) ) &
                     then
              ! find normf(a).
              if( stdlib_lsame( uplo, 'U' ) ) then
                 if( stdlib_lsame( diag, 'U' ) ) then
                    scale = one
                    sum = min( m, n )
                    do j = 2, n
                       call stdlib_classq( min( m, j-1 ), a( 1_ilp, j ), 1_ilp, scale, sum )
                    end do
                 else
                    scale = zero
                    sum = one
                    do j = 1, n
                       call stdlib_classq( min( m, j ), a( 1_ilp, j ), 1_ilp, scale, sum )
                    end do
                 end if
              else
                 if( stdlib_lsame( diag, 'U' ) ) then
                    scale = one
                    sum = min( m, n )
                    do j = 1, n
                       call stdlib_classq( m-j, a( min( m, j+1 ), j ), 1_ilp, scale,sum )
                    end do
                 else
                    scale = zero
                    sum = one
                    do j = 1, n
                       call stdlib_classq( m-j+1, a( j, j ), 1_ilp, scale, sum )
                    end do
                 end if
              end if
              value = scale*sqrt( sum )
           end if
           stdlib_clantr = value
           return
     end function stdlib_clantr

     real(dp) module function stdlib_zlantr( norm, uplo, diag, m, n, a, lda,work )
     !! ZLANTR returns the value of the one norm,  or the Frobenius norm, or
     !! the  infinity norm,  or the  element of  largest absolute value  of a
     !! trapezoidal or triangular matrix A.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: diag, norm, uplo
           integer(ilp), intent(in) :: lda, m, n
           ! Array Arguments 
           real(dp), intent(out) :: work(*)
           complex(dp), intent(in) :: a(lda,*)
       ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: udiag
           integer(ilp) :: i, j
           real(dp) :: scale, sum, value
           ! Intrinsic Functions 
           ! Executable Statements 
           if( min( m, n )==0_ilp ) then
              value = zero
           else if( stdlib_lsame( norm, 'M' ) ) then
              ! find max(abs(a(i,j))).
              if( stdlib_lsame( diag, 'U' ) ) then
                 value = one
                 if( stdlib_lsame( uplo, 'U' ) ) then
                    do j = 1, n
                       do i = 1, min( m, j-1 )
                          sum = abs( a( i, j ) )
                          if( value < sum .or. stdlib_disnan( sum ) ) value = sum
                       end do
                    end do
                 else
                    do j = 1, n
                       do i = j + 1, m
                          sum = abs( a( i, j ) )
                          if( value < sum .or. stdlib_disnan( sum ) ) value = sum
                       end do
                    end do
                 end if
              else
                 value = zero
                 if( stdlib_lsame( uplo, 'U' ) ) then
                    do j = 1, n
                       do i = 1, min( m, j )
                          sum = abs( a( i, j ) )
                          if( value < sum .or. stdlib_disnan( sum ) ) value = sum
                       end do
                    end do
                 else
                    do j = 1, n
                       do i = j, m
                          sum = abs( a( i, j ) )
                          if( value < sum .or. stdlib_disnan( sum ) ) value = sum
                       end do
                    end do
                 end if
              end if
           else if( ( stdlib_lsame( norm, 'O' ) ) .or. ( norm=='1' ) ) then
              ! find norm1(a).
              value = zero
              udiag = stdlib_lsame( diag, 'U' )
              if( stdlib_lsame( uplo, 'U' ) ) then
                 do j = 1, n
                    if( ( udiag ) .and. ( j<=m ) ) then
                       sum = one
                       do i = 1, j - 1
                          sum = sum + abs( a( i, j ) )
                       end do
                    else
                       sum = zero
                       do i = 1, min( m, j )
                          sum = sum + abs( a( i, j ) )
                       end do
                    end if
                    if( value < sum .or. stdlib_disnan( sum ) ) value = sum
                 end do
              else
                 do j = 1, n
                    if( udiag ) then
                       sum = one
                       do i = j + 1, m
                          sum = sum + abs( a( i, j ) )
                       end do
                    else
                       sum = zero
                       do i = j, m
                          sum = sum + abs( a( i, j ) )
                       end do
                    end if
                    if( value < sum .or. stdlib_disnan( sum ) ) value = sum
                 end do
              end if
           else if( stdlib_lsame( norm, 'I' ) ) then
              ! find normi(a).
              if( stdlib_lsame( uplo, 'U' ) ) then
                 if( stdlib_lsame( diag, 'U' ) ) then
                    do i = 1, m
                       work( i ) = one
                    end do
                    do j = 1, n
                       do i = 1, min( m, j-1 )
                          work( i ) = work( i ) + abs( a( i, j ) )
                       end do
                    end do
                 else
                    do i = 1, m
                       work( i ) = zero
                    end do
                    do j = 1, n
                       do i = 1, min( m, j )
                          work( i ) = work( i ) + abs( a( i, j ) )
                       end do
                    end do
                 end if
              else
                 if( stdlib_lsame( diag, 'U' ) ) then
                    do i = 1, min( m, n )
                       work( i ) = one
                    end do
                    do i = n + 1, m
                       work( i ) = zero
                    end do
                    do j = 1, n
                       do i = j + 1, m
                          work( i ) = work( i ) + abs( a( i, j ) )
                       end do
                    end do
                 else
                    do i = 1, m
                       work( i ) = zero
                    end do
                    do j = 1, n
                       do i = j, m
                          work( i ) = work( i ) + abs( a( i, j ) )
                       end do
                    end do
                 end if
              end if
              value = zero
              do i = 1, m
                 sum = work( i )
                 if( value < sum .or. stdlib_disnan( sum ) ) value = sum
              end do
           else if( ( stdlib_lsame( norm, 'F' ) ) .or. ( stdlib_lsame( norm, 'E' ) ) ) &
                     then
              ! find normf(a).
              if( stdlib_lsame( uplo, 'U' ) ) then
                 if( stdlib_lsame( diag, 'U' ) ) then
                    scale = one
                    sum = min( m, n )
                    do j = 2, n
                       call stdlib_zlassq( min( m, j-1 ), a( 1_ilp, j ), 1_ilp, scale, sum )
                    end do
                 else
                    scale = zero
                    sum = one
                    do j = 1, n
                       call stdlib_zlassq( min( m, j ), a( 1_ilp, j ), 1_ilp, scale, sum )
                    end do
                 end if
              else
                 if( stdlib_lsame( diag, 'U' ) ) then
                    scale = one
                    sum = min( m, n )
                    do j = 1, n
                       call stdlib_zlassq( m-j, a( min( m, j+1 ), j ), 1_ilp, scale,sum )
                    end do
                 else
                    scale = zero
                    sum = one
                    do j = 1, n
                       call stdlib_zlassq( m-j+1, a( j, j ), 1_ilp, scale, sum )
                    end do
                 end if
              end if
              value = scale*sqrt( sum )
           end if
           stdlib_zlantr = value
           return
     end function stdlib_zlantr




     real(sp) module function stdlib_slantp( norm, uplo, diag, n, ap, work )
     !! SLANTP returns the value of the one norm,  or the Frobenius norm, or
     !! the  infinity norm,  or the  element of  largest absolute value  of a
     !! triangular matrix A, supplied in packed form.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: diag, norm, uplo
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           real(sp), intent(in) :: ap(*)
           real(sp), intent(out) :: work(*)
       ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: udiag
           integer(ilp) :: i, j, k
           real(sp) :: scale, sum, value
           ! Intrinsic Functions 
           ! Executable Statements 
           if( n==0_ilp ) then
              value = zero
           else if( stdlib_lsame( norm, 'M' ) ) then
              ! find max(abs(a(i,j))).
              k = 1_ilp
              if( stdlib_lsame( diag, 'U' ) ) then
                 value = one
                 if( stdlib_lsame( uplo, 'U' ) ) then
                    do j = 1, n
                       do i = k, k + j - 2
                          sum = abs( ap( i ) )
                          if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
                       end do
                       k = k + j
                    end do
                 else
                    do j = 1, n
                       do i = k + 1, k + n - j
                          sum = abs( ap( i ) )
                          if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
                       end do
                       k = k + n - j + 1_ilp
                    end do
                 end if
              else
                 value = zero
                 if( stdlib_lsame( uplo, 'U' ) ) then
                    do j = 1, n
                       do i = k, k + j - 1
                          sum = abs( ap( i ) )
                          if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
                       end do
                       k = k + j
                    end do
                 else
                    do j = 1, n
                       do i = k, k + n - j
                          sum = abs( ap( i ) )
                          if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
                       end do
                       k = k + n - j + 1_ilp
                    end do
                 end if
              end if
           else if( ( stdlib_lsame( norm, 'O' ) ) .or. ( norm=='1' ) ) then
              ! find norm1(a).
              value = zero
              k = 1_ilp
              udiag = stdlib_lsame( diag, 'U' )
              if( stdlib_lsame( uplo, 'U' ) ) then
                 do j = 1, n
                    if( udiag ) then
                       sum = one
                       do i = k, k + j - 2
                          sum = sum + abs( ap( i ) )
                       end do
                    else
                       sum = zero
                       do i = k, k + j - 1
                          sum = sum + abs( ap( i ) )
                       end do
                    end if
                    k = k + j
                    if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
                 end do
              else
                 do j = 1, n
                    if( udiag ) then
                       sum = one
                       do i = k + 1, k + n - j
                          sum = sum + abs( ap( i ) )
                       end do
                    else
                       sum = zero
                       do i = k, k + n - j
                          sum = sum + abs( ap( i ) )
                       end do
                    end if
                    k = k + n - j + 1_ilp
                    if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
                 end do
              end if
           else if( stdlib_lsame( norm, 'I' ) ) then
              ! find normi(a).
              k = 1_ilp
              if( stdlib_lsame( uplo, 'U' ) ) then
                 if( stdlib_lsame( diag, 'U' ) ) then
                    do i = 1, n
                       work( i ) = one
                    end do
                    do j = 1, n
                       do i = 1, j - 1
                          work( i ) = work( i ) + abs( ap( k ) )
                          k = k + 1_ilp
                       end do
                       k = k + 1_ilp
                    end do
                 else
                    do i = 1, n
                       work( i ) = zero
                    end do
                    do j = 1, n
                       do i = 1, j
                          work( i ) = work( i ) + abs( ap( k ) )
                          k = k + 1_ilp
                       end do
                    end do
                 end if
              else
                 if( stdlib_lsame( diag, 'U' ) ) then
                    do i = 1, n
                       work( i ) = one
                    end do
                    do j = 1, n
                       k = k + 1_ilp
                       do i = j + 1, n
                          work( i ) = work( i ) + abs( ap( k ) )
                          k = k + 1_ilp
                       end do
                    end do
                 else
                    do i = 1, n
                       work( i ) = zero
                    end do
                    do j = 1, n
                       do i = j, n
                          work( i ) = work( i ) + abs( ap( k ) )
                          k = k + 1_ilp
                       end do
                    end do
                 end if
              end if
              value = zero
              do i = 1, n
                 sum = work( i )
                 if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
              end do
           else if( ( stdlib_lsame( norm, 'F' ) ) .or. ( stdlib_lsame( norm, 'E' ) ) ) &
                     then
              ! find normf(a).
              if( stdlib_lsame( uplo, 'U' ) ) then
                 if( stdlib_lsame( diag, 'U' ) ) then
                    scale = one
                    sum = n
                    k = 2_ilp
                    do j = 2, n
                       call stdlib_slassq( j-1, ap( k ), 1_ilp, scale, sum )
                       k = k + j
                    end do
                 else
                    scale = zero
                    sum = one
                    k = 1_ilp
                    do j = 1, n
                       call stdlib_slassq( j, ap( k ), 1_ilp, scale, sum )
                       k = k + j
                    end do
                 end if
              else
                 if( stdlib_lsame( diag, 'U' ) ) then
                    scale = one
                    sum = n
                    k = 2_ilp
                    do j = 1, n - 1
                       call stdlib_slassq( n-j, ap( k ), 1_ilp, scale, sum )
                       k = k + n - j + 1_ilp
                    end do
                 else
                    scale = zero
                    sum = one
                    k = 1_ilp
                    do j = 1, n
                       call stdlib_slassq( n-j+1, ap( k ), 1_ilp, scale, sum )
                       k = k + n - j + 1_ilp
                    end do
                 end if
              end if
              value = scale*sqrt( sum )
           end if
           stdlib_slantp = value
           return
     end function stdlib_slantp

     real(dp) module function stdlib_dlantp( norm, uplo, diag, n, ap, work )
     !! DLANTP returns the value of the one norm,  or the Frobenius norm, or
     !! the  infinity norm,  or the  element of  largest absolute value  of a
     !! triangular matrix A, supplied in packed form.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: diag, norm, uplo
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           real(dp), intent(in) :: ap(*)
           real(dp), intent(out) :: work(*)
       ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: udiag
           integer(ilp) :: i, j, k
           real(dp) :: scale, sum, value
           ! Intrinsic Functions 
           ! Executable Statements 
           if( n==0_ilp ) then
              value = zero
           else if( stdlib_lsame( norm, 'M' ) ) then
              ! find max(abs(a(i,j))).
              k = 1_ilp
              if( stdlib_lsame( diag, 'U' ) ) then
                 value = one
                 if( stdlib_lsame( uplo, 'U' ) ) then
                    do j = 1, n
                       do i = k, k + j - 2
                          sum = abs( ap( i ) )
                          if( value < sum .or. stdlib_disnan( sum ) ) value = sum
                       end do
                       k = k + j
                    end do
                 else
                    do j = 1, n
                       do i = k + 1, k + n - j
                          sum = abs( ap( i ) )
                          if( value < sum .or. stdlib_disnan( sum ) ) value = sum
                       end do
                       k = k + n - j + 1_ilp
                    end do
                 end if
              else
                 value = zero
                 if( stdlib_lsame( uplo, 'U' ) ) then
                    do j = 1, n
                       do i = k, k + j - 1
                          sum = abs( ap( i ) )
                          if( value < sum .or. stdlib_disnan( sum ) ) value = sum
                       end do
                       k = k + j
                    end do
                 else
                    do j = 1, n
                       do i = k, k + n - j
                          sum = abs( ap( i ) )
                          if( value < sum .or. stdlib_disnan( sum ) ) value = sum
                       end do
                       k = k + n - j + 1_ilp
                    end do
                 end if
              end if
           else if( ( stdlib_lsame( norm, 'O' ) ) .or. ( norm=='1' ) ) then
              ! find norm1(a).
              value = zero
              k = 1_ilp
              udiag = stdlib_lsame( diag, 'U' )
              if( stdlib_lsame( uplo, 'U' ) ) then
                 do j = 1, n
                    if( udiag ) then
                       sum = one
                       do i = k, k + j - 2
                          sum = sum + abs( ap( i ) )
                       end do
                    else
                       sum = zero
                       do i = k, k + j - 1
                          sum = sum + abs( ap( i ) )
                       end do
                    end if
                    k = k + j
                    if( value < sum .or. stdlib_disnan( sum ) ) value = sum
                 end do
              else
                 do j = 1, n
                    if( udiag ) then
                       sum = one
                       do i = k + 1, k + n - j
                          sum = sum + abs( ap( i ) )
                       end do
                    else
                       sum = zero
                       do i = k, k + n - j
                          sum = sum + abs( ap( i ) )
                       end do
                    end if
                    k = k + n - j + 1_ilp
                    if( value < sum .or. stdlib_disnan( sum ) ) value = sum
                 end do
              end if
           else if( stdlib_lsame( norm, 'I' ) ) then
              ! find normi(a).
              k = 1_ilp
              if( stdlib_lsame( uplo, 'U' ) ) then
                 if( stdlib_lsame( diag, 'U' ) ) then
                    do i = 1, n
                       work( i ) = one
                    end do
                    do j = 1, n
                       do i = 1, j - 1
                          work( i ) = work( i ) + abs( ap( k ) )
                          k = k + 1_ilp
                       end do
                       k = k + 1_ilp
                    end do
                 else
                    do i = 1, n
                       work( i ) = zero
                    end do
                    do j = 1, n
                       do i = 1, j
                          work( i ) = work( i ) + abs( ap( k ) )
                          k = k + 1_ilp
                       end do
                    end do
                 end if
              else
                 if( stdlib_lsame( diag, 'U' ) ) then
                    do i = 1, n
                       work( i ) = one
                    end do
                    do j = 1, n
                       k = k + 1_ilp
                       do i = j + 1, n
                          work( i ) = work( i ) + abs( ap( k ) )
                          k = k + 1_ilp
                       end do
                    end do
                 else
                    do i = 1, n
                       work( i ) = zero
                    end do
                    do j = 1, n
                       do i = j, n
                          work( i ) = work( i ) + abs( ap( k ) )
                          k = k + 1_ilp
                       end do
                    end do
                 end if
              end if
              value = zero
              do i = 1, n
                 sum = work( i )
                 if( value < sum .or. stdlib_disnan( sum ) ) value = sum
              end do
           else if( ( stdlib_lsame( norm, 'F' ) ) .or. ( stdlib_lsame( norm, 'E' ) ) ) &
                     then
              ! find normf(a).
              if( stdlib_lsame( uplo, 'U' ) ) then
                 if( stdlib_lsame( diag, 'U' ) ) then
                    scale = one
                    sum = n
                    k = 2_ilp
                    do j = 2, n
                       call stdlib_dlassq( j-1, ap( k ), 1_ilp, scale, sum )
                       k = k + j
                    end do
                 else
                    scale = zero
                    sum = one
                    k = 1_ilp
                    do j = 1, n
                       call stdlib_dlassq( j, ap( k ), 1_ilp, scale, sum )
                       k = k + j
                    end do
                 end if
              else
                 if( stdlib_lsame( diag, 'U' ) ) then
                    scale = one
                    sum = n
                    k = 2_ilp
                    do j = 1, n - 1
                       call stdlib_dlassq( n-j, ap( k ), 1_ilp, scale, sum )
                       k = k + n - j + 1_ilp
                    end do
                 else
                    scale = zero
                    sum = one
                    k = 1_ilp
                    do j = 1, n
                       call stdlib_dlassq( n-j+1, ap( k ), 1_ilp, scale, sum )
                       k = k + n - j + 1_ilp
                    end do
                 end if
              end if
              value = scale*sqrt( sum )
           end if
           stdlib_dlantp = value
           return
     end function stdlib_dlantp


     real(sp) module function stdlib_clantp( norm, uplo, diag, n, ap, work )
     !! CLANTP returns the value of the one norm,  or the Frobenius norm, or
     !! the  infinity norm,  or the  element of  largest absolute value  of a
     !! triangular matrix A, supplied in packed form.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: diag, norm, uplo
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           real(sp), intent(out) :: work(*)
           complex(sp), intent(in) :: ap(*)
       ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: udiag
           integer(ilp) :: i, j, k
           real(sp) :: scale, sum, value
           ! Intrinsic Functions 
           ! Executable Statements 
           if( n==0_ilp ) then
              value = zero
           else if( stdlib_lsame( norm, 'M' ) ) then
              ! find max(abs(a(i,j))).
              k = 1_ilp
              if( stdlib_lsame( diag, 'U' ) ) then
                 value = one
                 if( stdlib_lsame( uplo, 'U' ) ) then
                    do j = 1, n
                       do i = k, k + j - 2
                          sum = abs( ap( i ) )
                          if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
                       end do
                       k = k + j
                    end do
                 else
                    do j = 1, n
                       do i = k + 1, k + n - j
                          sum = abs( ap( i ) )
                          if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
                       end do
                       k = k + n - j + 1_ilp
                    end do
                 end if
              else
                 value = zero
                 if( stdlib_lsame( uplo, 'U' ) ) then
                    do j = 1, n
                       do i = k, k + j - 1
                          sum = abs( ap( i ) )
                          if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
                       end do
                       k = k + j
                    end do
                 else
                    do j = 1, n
                       do i = k, k + n - j
                          sum = abs( ap( i ) )
                          if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
                       end do
                       k = k + n - j + 1_ilp
                    end do
                 end if
              end if
           else if( ( stdlib_lsame( norm, 'O' ) ) .or. ( norm=='1' ) ) then
              ! find norm1(a).
              value = zero
              k = 1_ilp
              udiag = stdlib_lsame( diag, 'U' )
              if( stdlib_lsame( uplo, 'U' ) ) then
                 do j = 1, n
                    if( udiag ) then
                       sum = one
                       do i = k, k + j - 2
                          sum = sum + abs( ap( i ) )
                       end do
                    else
                       sum = zero
                       do i = k, k + j - 1
                          sum = sum + abs( ap( i ) )
                       end do
                    end if
                    k = k + j
                    if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
                 end do
              else
                 do j = 1, n
                    if( udiag ) then
                       sum = one
                       do i = k + 1, k + n - j
                          sum = sum + abs( ap( i ) )
                       end do
                    else
                       sum = zero
                       do i = k, k + n - j
                          sum = sum + abs( ap( i ) )
                       end do
                    end if
                    k = k + n - j + 1_ilp
                    if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
                 end do
              end if
           else if( stdlib_lsame( norm, 'I' ) ) then
              ! find normi(a).
              k = 1_ilp
              if( stdlib_lsame( uplo, 'U' ) ) then
                 if( stdlib_lsame( diag, 'U' ) ) then
                    do i = 1, n
                       work( i ) = one
                    end do
                    do j = 1, n
                       do i = 1, j - 1
                          work( i ) = work( i ) + abs( ap( k ) )
                          k = k + 1_ilp
                       end do
                       k = k + 1_ilp
                    end do
                 else
                    do i = 1, n
                       work( i ) = zero
                    end do
                    do j = 1, n
                       do i = 1, j
                          work( i ) = work( i ) + abs( ap( k ) )
                          k = k + 1_ilp
                       end do
                    end do
                 end if
              else
                 if( stdlib_lsame( diag, 'U' ) ) then
                    do i = 1, n
                       work( i ) = one
                    end do
                    do j = 1, n
                       k = k + 1_ilp
                       do i = j + 1, n
                          work( i ) = work( i ) + abs( ap( k ) )
                          k = k + 1_ilp
                       end do
                    end do
                 else
                    do i = 1, n
                       work( i ) = zero
                    end do
                    do j = 1, n
                       do i = j, n
                          work( i ) = work( i ) + abs( ap( k ) )
                          k = k + 1_ilp
                       end do
                    end do
                 end if
              end if
              value = zero
              do i = 1, n
                 sum = work( i )
                 if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
              end do
           else if( ( stdlib_lsame( norm, 'F' ) ) .or. ( stdlib_lsame( norm, 'E' ) ) ) &
                     then
              ! find normf(a).
              if( stdlib_lsame( uplo, 'U' ) ) then
                 if( stdlib_lsame( diag, 'U' ) ) then
                    scale = one
                    sum = n
                    k = 2_ilp
                    do j = 2, n
                       call stdlib_classq( j-1, ap( k ), 1_ilp, scale, sum )
                       k = k + j
                    end do
                 else
                    scale = zero
                    sum = one
                    k = 1_ilp
                    do j = 1, n
                       call stdlib_classq( j, ap( k ), 1_ilp, scale, sum )
                       k = k + j
                    end do
                 end if
              else
                 if( stdlib_lsame( diag, 'U' ) ) then
                    scale = one
                    sum = n
                    k = 2_ilp
                    do j = 1, n - 1
                       call stdlib_classq( n-j, ap( k ), 1_ilp, scale, sum )
                       k = k + n - j + 1_ilp
                    end do
                 else
                    scale = zero
                    sum = one
                    k = 1_ilp
                    do j = 1, n
                       call stdlib_classq( n-j+1, ap( k ), 1_ilp, scale, sum )
                       k = k + n - j + 1_ilp
                    end do
                 end if
              end if
              value = scale*sqrt( sum )
           end if
           stdlib_clantp = value
           return
     end function stdlib_clantp

     real(dp) module function stdlib_zlantp( norm, uplo, diag, n, ap, work )
     !! ZLANTP returns the value of the one norm,  or the Frobenius norm, or
     !! the  infinity norm,  or the  element of  largest absolute value  of a
     !! triangular matrix A, supplied in packed form.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: diag, norm, uplo
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           real(dp), intent(out) :: work(*)
           complex(dp), intent(in) :: ap(*)
       ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: udiag
           integer(ilp) :: i, j, k
           real(dp) :: scale, sum, value
           ! Intrinsic Functions 
           ! Executable Statements 
           if( n==0_ilp ) then
              value = zero
           else if( stdlib_lsame( norm, 'M' ) ) then
              ! find max(abs(a(i,j))).
              k = 1_ilp
              if( stdlib_lsame( diag, 'U' ) ) then
                 value = one
                 if( stdlib_lsame( uplo, 'U' ) ) then
                    do j = 1, n
                       do i = k, k + j - 2
                          sum = abs( ap( i ) )
                          if( value < sum .or. stdlib_disnan( sum ) ) value = sum
                       end do
                       k = k + j
                    end do
                 else
                    do j = 1, n
                       do i = k + 1, k + n - j
                          sum = abs( ap( i ) )
                          if( value < sum .or. stdlib_disnan( sum ) ) value = sum
                       end do
                       k = k + n - j + 1_ilp
                    end do
                 end if
              else
                 value = zero
                 if( stdlib_lsame( uplo, 'U' ) ) then
                    do j = 1, n
                       do i = k, k + j - 1
                          sum = abs( ap( i ) )
                          if( value < sum .or. stdlib_disnan( sum ) ) value = sum
                       end do
                       k = k + j
                    end do
                 else
                    do j = 1, n
                       do i = k, k + n - j
                          sum = abs( ap( i ) )
                          if( value < sum .or. stdlib_disnan( sum ) ) value = sum
                       end do
                       k = k + n - j + 1_ilp
                    end do
                 end if
              end if
           else if( ( stdlib_lsame( norm, 'O' ) ) .or. ( norm=='1' ) ) then
              ! find norm1(a).
              value = zero
              k = 1_ilp
              udiag = stdlib_lsame( diag, 'U' )
              if( stdlib_lsame( uplo, 'U' ) ) then
                 do j = 1, n
                    if( udiag ) then
                       sum = one
                       do i = k, k + j - 2
                          sum = sum + abs( ap( i ) )
                       end do
                    else
                       sum = zero
                       do i = k, k + j - 1
                          sum = sum + abs( ap( i ) )
                       end do
                    end if
                    k = k + j
                    if( value < sum .or. stdlib_disnan( sum ) ) value = sum
                 end do
              else
                 do j = 1, n
                    if( udiag ) then
                       sum = one
                       do i = k + 1, k + n - j
                          sum = sum + abs( ap( i ) )
                       end do
                    else
                       sum = zero
                       do i = k, k + n - j
                          sum = sum + abs( ap( i ) )
                       end do
                    end if
                    k = k + n - j + 1_ilp
                    if( value < sum .or. stdlib_disnan( sum ) ) value = sum
                 end do
              end if
           else if( stdlib_lsame( norm, 'I' ) ) then
              ! find normi(a).
              k = 1_ilp
              if( stdlib_lsame( uplo, 'U' ) ) then
                 if( stdlib_lsame( diag, 'U' ) ) then
                    do i = 1, n
                       work( i ) = one
                    end do
                    do j = 1, n
                       do i = 1, j - 1
                          work( i ) = work( i ) + abs( ap( k ) )
                          k = k + 1_ilp
                       end do
                       k = k + 1_ilp
                    end do
                 else
                    do i = 1, n
                       work( i ) = zero
                    end do
                    do j = 1, n
                       do i = 1, j
                          work( i ) = work( i ) + abs( ap( k ) )
                          k = k + 1_ilp
                       end do
                    end do
                 end if
              else
                 if( stdlib_lsame( diag, 'U' ) ) then
                    do i = 1, n
                       work( i ) = one
                    end do
                    do j = 1, n
                       k = k + 1_ilp
                       do i = j + 1, n
                          work( i ) = work( i ) + abs( ap( k ) )
                          k = k + 1_ilp
                       end do
                    end do
                 else
                    do i = 1, n
                       work( i ) = zero
                    end do
                    do j = 1, n
                       do i = j, n
                          work( i ) = work( i ) + abs( ap( k ) )
                          k = k + 1_ilp
                       end do
                    end do
                 end if
              end if
              value = zero
              do i = 1, n
                 sum = work( i )
                 if( value < sum .or. stdlib_disnan( sum ) ) value = sum
              end do
           else if( ( stdlib_lsame( norm, 'F' ) ) .or. ( stdlib_lsame( norm, 'E' ) ) ) &
                     then
              ! find normf(a).
              if( stdlib_lsame( uplo, 'U' ) ) then
                 if( stdlib_lsame( diag, 'U' ) ) then
                    scale = one
                    sum = n
                    k = 2_ilp
                    do j = 2, n
                       call stdlib_zlassq( j-1, ap( k ), 1_ilp, scale, sum )
                       k = k + j
                    end do
                 else
                    scale = zero
                    sum = one
                    k = 1_ilp
                    do j = 1, n
                       call stdlib_zlassq( j, ap( k ), 1_ilp, scale, sum )
                       k = k + j
                    end do
                 end if
              else
                 if( stdlib_lsame( diag, 'U' ) ) then
                    scale = one
                    sum = n
                    k = 2_ilp
                    do j = 1, n - 1
                       call stdlib_zlassq( n-j, ap( k ), 1_ilp, scale, sum )
                       k = k + n - j + 1_ilp
                    end do
                 else
                    scale = zero
                    sum = one
                    k = 1_ilp
                    do j = 1, n
                       call stdlib_zlassq( n-j+1, ap( k ), 1_ilp, scale, sum )
                       k = k + n - j + 1_ilp
                    end do
                 end if
              end if
              value = scale*sqrt( sum )
           end if
           stdlib_zlantp = value
           return
     end function stdlib_zlantp




     real(sp) module function stdlib_slantb( norm, uplo, diag, n, k, ab,ldab, work )
     !! SLANTB returns the value of the one norm,  or the Frobenius norm, or
     !! the  infinity norm,  or the element of  largest absolute value  of an
     !! n by n triangular band matrix A,  with ( k + 1 ) diagonals.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: diag, norm, uplo
           integer(ilp), intent(in) :: k, ldab, n
           ! Array Arguments 
           real(sp), intent(in) :: ab(ldab,*)
           real(sp), intent(out) :: work(*)
       ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: udiag
           integer(ilp) :: i, j, l
           real(sp) :: scale, sum, value
           ! Intrinsic Functions 
           ! Executable Statements 
           if( n==0_ilp ) then
              value = zero
           else if( stdlib_lsame( norm, 'M' ) ) then
              ! find max(abs(a(i,j))).
              if( stdlib_lsame( diag, 'U' ) ) then
                 value = one
                 if( stdlib_lsame( uplo, 'U' ) ) then
                    do j = 1, n
                       do i = max( k+2-j, 1 ), k
                          sum = abs( ab( i, j ) )
                          if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
                       end do
                    end do
                 else
                    do j = 1, n
                       do i = 2, min( n+1-j, k+1 )
                          sum = abs( ab( i, j ) )
                          if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
                       end do
                    end do
                 end if
              else
                 value = zero
                 if( stdlib_lsame( uplo, 'U' ) ) then
                    do j = 1, n
                       do i = max( k+2-j, 1 ), k + 1
                          sum = abs( ab( i, j ) )
                          if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
                       end do
                    end do
                 else
                    do j = 1, n
                       do i = 1, min( n+1-j, k+1 )
                          sum = abs( ab( i, j ) )
                          if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
                       end do
                    end do
                 end if
              end if
           else if( ( stdlib_lsame( norm, 'O' ) ) .or. ( norm=='1' ) ) then
              ! find norm1(a).
              value = zero
              udiag = stdlib_lsame( diag, 'U' )
              if( stdlib_lsame( uplo, 'U' ) ) then
                 do j = 1, n
                    if( udiag ) then
                       sum = one
                       do i = max( k+2-j, 1 ), k
                          sum = sum + abs( ab( i, j ) )
                       end do
                    else
                       sum = zero
                       do i = max( k+2-j, 1 ), k + 1
                          sum = sum + abs( ab( i, j ) )
                       end do
                    end if
                    if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
                 end do
              else
                 do j = 1, n
                    if( udiag ) then
                       sum = one
                       do i = 2, min( n+1-j, k+1 )
                          sum = sum + abs( ab( i, j ) )
                       end do
                    else
                       sum = zero
                       do i = 1, min( n+1-j, k+1 )
                          sum = sum + abs( ab( i, j ) )
                       end do
                    end if
                    if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
                 end do
              end if
           else if( stdlib_lsame( norm, 'I' ) ) then
              ! find normi(a).
              value = zero
              if( stdlib_lsame( uplo, 'U' ) ) then
                 if( stdlib_lsame( diag, 'U' ) ) then
                    do i = 1, n
                       work( i ) = one
                    end do
                    do j = 1, n
                       l = k + 1_ilp - j
                       do i = max( 1, j-k ), j - 1
                          work( i ) = work( i ) + abs( ab( l+i, j ) )
                       end do
                    end do
                 else
                    do i = 1, n
                       work( i ) = zero
                    end do
                    do j = 1, n
                       l = k + 1_ilp - j
                       do i = max( 1, j-k ), j
                          work( i ) = work( i ) + abs( ab( l+i, j ) )
                       end do
                    end do
                 end if
              else
                 if( stdlib_lsame( diag, 'U' ) ) then
                    do i = 1, n
                       work( i ) = one
                    end do
                    do j = 1, n
                       l = 1_ilp - j
                       do i = j + 1, min( n, j+k )
                          work( i ) = work( i ) + abs( ab( l+i, j ) )
                       end do
                    end do
                 else
                    do i = 1, n
                       work( i ) = zero
                    end do
                    do j = 1, n
                       l = 1_ilp - j
                       do i = j, min( n, j+k )
                          work( i ) = work( i ) + abs( ab( l+i, j ) )
                       end do
                    end do
                 end if
              end if
              do i = 1, n
                 sum = work( i )
                 if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
              end do
           else if( ( stdlib_lsame( norm, 'F' ) ) .or. ( stdlib_lsame( norm, 'E' ) ) ) &
                     then
              ! find normf(a).
              if( stdlib_lsame( uplo, 'U' ) ) then
                 if( stdlib_lsame( diag, 'U' ) ) then
                    scale = one
                    sum = n
                    if( k>0_ilp ) then
                       do j = 2, n
                          call stdlib_slassq( min( j-1, k ),ab( max( k+2-j, 1_ilp ), j ), 1_ilp, scale,&
                                    sum )
                       end do
                    end if
                 else
                    scale = zero
                    sum = one
                    do j = 1, n
                       call stdlib_slassq( min( j, k+1 ), ab( max( k+2-j, 1_ilp ), j ),1_ilp, scale, sum )
                                 
                    end do
                 end if
              else
                 if( stdlib_lsame( diag, 'U' ) ) then
                    scale = one
                    sum = n
                    if( k>0_ilp ) then
                       do j = 1, n - 1
                          call stdlib_slassq( min( n-j, k ), ab( 2_ilp, j ), 1_ilp, scale,sum )
                       end do
                    end if
                 else
                    scale = zero
                    sum = one
                    do j = 1, n
                       call stdlib_slassq( min( n-j+1, k+1 ), ab( 1_ilp, j ), 1_ilp, scale,sum )
                    end do
                 end if
              end if
              value = scale*sqrt( sum )
           end if
           stdlib_slantb = value
           return
     end function stdlib_slantb

     real(dp) module function stdlib_dlantb( norm, uplo, diag, n, k, ab,ldab, work )
     !! DLANTB returns the value of the one norm,  or the Frobenius norm, or
     !! the  infinity norm,  or the element of  largest absolute value  of an
     !! n by n triangular band matrix A,  with ( k + 1 ) diagonals.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: diag, norm, uplo
           integer(ilp), intent(in) :: k, ldab, n
           ! Array Arguments 
           real(dp), intent(in) :: ab(ldab,*)
           real(dp), intent(out) :: work(*)
       ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: udiag
           integer(ilp) :: i, j, l
           real(dp) :: scale, sum, value
           ! Intrinsic Functions 
           ! Executable Statements 
           if( n==0_ilp ) then
              value = zero
           else if( stdlib_lsame( norm, 'M' ) ) then
              ! find max(abs(a(i,j))).
              if( stdlib_lsame( diag, 'U' ) ) then
                 value = one
                 if( stdlib_lsame( uplo, 'U' ) ) then
                    do j = 1, n
                       do i = max( k+2-j, 1 ), k
                          sum = abs( ab( i, j ) )
                          if( value < sum .or. stdlib_disnan( sum ) ) value = sum
                       end do
                    end do
                 else
                    do j = 1, n
                       do i = 2, min( n+1-j, k+1 )
                          sum = abs( ab( i, j ) )
                          if( value < sum .or. stdlib_disnan( sum ) ) value = sum
                       end do
                    end do
                 end if
              else
                 value = zero
                 if( stdlib_lsame( uplo, 'U' ) ) then
                    do j = 1, n
                       do i = max( k+2-j, 1 ), k + 1
                          sum = abs( ab( i, j ) )
                          if( value < sum .or. stdlib_disnan( sum ) ) value = sum
                       end do
                    end do
                 else
                    do j = 1, n
                       do i = 1, min( n+1-j, k+1 )
                          sum = abs( ab( i, j ) )
                          if( value < sum .or. stdlib_disnan( sum ) ) value = sum
                       end do
                    end do
                 end if
              end if
           else if( ( stdlib_lsame( norm, 'O' ) ) .or. ( norm=='1' ) ) then
              ! find norm1(a).
              value = zero
              udiag = stdlib_lsame( diag, 'U' )
              if( stdlib_lsame( uplo, 'U' ) ) then
                 do j = 1, n
                    if( udiag ) then
                       sum = one
                       do i = max( k+2-j, 1 ), k
                          sum = sum + abs( ab( i, j ) )
                       end do
                    else
                       sum = zero
                       do i = max( k+2-j, 1 ), k + 1
                          sum = sum + abs( ab( i, j ) )
                       end do
                    end if
                    if( value < sum .or. stdlib_disnan( sum ) ) value = sum
                 end do
              else
                 do j = 1, n
                    if( udiag ) then
                       sum = one
                       do i = 2, min( n+1-j, k+1 )
                          sum = sum + abs( ab( i, j ) )
                       end do
                    else
                       sum = zero
                       do i = 1, min( n+1-j, k+1 )
                          sum = sum + abs( ab( i, j ) )
                       end do
                    end if
                    if( value < sum .or. stdlib_disnan( sum ) ) value = sum
                 end do
              end if
           else if( stdlib_lsame( norm, 'I' ) ) then
              ! find normi(a).
              value = zero
              if( stdlib_lsame( uplo, 'U' ) ) then
                 if( stdlib_lsame( diag, 'U' ) ) then
                    do i = 1, n
                       work( i ) = one
                    end do
                    do j = 1, n
                       l = k + 1_ilp - j
                       do i = max( 1, j-k ), j - 1
                          work( i ) = work( i ) + abs( ab( l+i, j ) )
                       end do
                    end do
                 else
                    do i = 1, n
                       work( i ) = zero
                    end do
                    do j = 1, n
                       l = k + 1_ilp - j
                       do i = max( 1, j-k ), j
                          work( i ) = work( i ) + abs( ab( l+i, j ) )
                       end do
                    end do
                 end if
              else
                 if( stdlib_lsame( diag, 'U' ) ) then
                    do i = 1, n
                       work( i ) = one
                    end do
                    do j = 1, n
                       l = 1_ilp - j
                       do i = j + 1, min( n, j+k )
                          work( i ) = work( i ) + abs( ab( l+i, j ) )
                       end do
                    end do
                 else
                    do i = 1, n
                       work( i ) = zero
                    end do
                    do j = 1, n
                       l = 1_ilp - j
                       do i = j, min( n, j+k )
                          work( i ) = work( i ) + abs( ab( l+i, j ) )
                       end do
                    end do
                 end if
              end if
              do i = 1, n
                 sum = work( i )
                 if( value < sum .or. stdlib_disnan( sum ) ) value = sum
              end do
           else if( ( stdlib_lsame( norm, 'F' ) ) .or. ( stdlib_lsame( norm, 'E' ) ) ) &
                     then
              ! find normf(a).
              if( stdlib_lsame( uplo, 'U' ) ) then
                 if( stdlib_lsame( diag, 'U' ) ) then
                    scale = one
                    sum = n
                    if( k>0_ilp ) then
                       do j = 2, n
                          call stdlib_dlassq( min( j-1, k ),ab( max( k+2-j, 1_ilp ), j ), 1_ilp, scale,&
                                    sum )
                       end do
                    end if
                 else
                    scale = zero
                    sum = one
                    do j = 1, n
                       call stdlib_dlassq( min( j, k+1 ), ab( max( k+2-j, 1_ilp ), j ),1_ilp, scale, sum )
                                 
                    end do
                 end if
              else
                 if( stdlib_lsame( diag, 'U' ) ) then
                    scale = one
                    sum = n
                    if( k>0_ilp ) then
                       do j = 1, n - 1
                          call stdlib_dlassq( min( n-j, k ), ab( 2_ilp, j ), 1_ilp, scale,sum )
                       end do
                    end if
                 else
                    scale = zero
                    sum = one
                    do j = 1, n
                       call stdlib_dlassq( min( n-j+1, k+1 ), ab( 1_ilp, j ), 1_ilp, scale,sum )
                    end do
                 end if
              end if
              value = scale*sqrt( sum )
           end if
           stdlib_dlantb = value
           return
     end function stdlib_dlantb


     real(sp) module function stdlib_clantb( norm, uplo, diag, n, k, ab,ldab, work )
     !! CLANTB returns the value of the one norm,  or the Frobenius norm, or
     !! the  infinity norm,  or the element of  largest absolute value  of an
     !! n by n triangular band matrix A,  with ( k + 1 ) diagonals.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: diag, norm, uplo
           integer(ilp), intent(in) :: k, ldab, n
           ! Array Arguments 
           real(sp), intent(out) :: work(*)
           complex(sp), intent(in) :: ab(ldab,*)
       ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: udiag
           integer(ilp) :: i, j, l
           real(sp) :: scale, sum, value
           ! Intrinsic Functions 
           ! Executable Statements 
           if( n==0_ilp ) then
              value = zero
           else if( stdlib_lsame( norm, 'M' ) ) then
              ! find max(abs(a(i,j))).
              if( stdlib_lsame( diag, 'U' ) ) then
                 value = one
                 if( stdlib_lsame( uplo, 'U' ) ) then
                    do j = 1, n
                       do i = max( k+2-j, 1 ), k
                          sum = abs( ab( i, j ) )
                          if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
                       end do
                    end do
                 else
                    do j = 1, n
                       do i = 2, min( n+1-j, k+1 )
                          sum = abs( ab( i, j ) )
                          if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
                       end do
                    end do
                 end if
              else
                 value = zero
                 if( stdlib_lsame( uplo, 'U' ) ) then
                    do j = 1, n
                       do i = max( k+2-j, 1 ), k + 1
                          sum = abs( ab( i, j ) )
                          if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
                       end do
                    end do
                 else
                    do j = 1, n
                       do i = 1, min( n+1-j, k+1 )
                          sum = abs( ab( i, j ) )
                          if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
                       end do
                    end do
                 end if
              end if
           else if( ( stdlib_lsame( norm, 'O' ) ) .or. ( norm=='1' ) ) then
              ! find norm1(a).
              value = zero
              udiag = stdlib_lsame( diag, 'U' )
              if( stdlib_lsame( uplo, 'U' ) ) then
                 do j = 1, n
                    if( udiag ) then
                       sum = one
                       do i = max( k+2-j, 1 ), k
                          sum = sum + abs( ab( i, j ) )
                       end do
                    else
                       sum = zero
                       do i = max( k+2-j, 1 ), k + 1
                          sum = sum + abs( ab( i, j ) )
                       end do
                    end if
                    if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
                 end do
              else
                 do j = 1, n
                    if( udiag ) then
                       sum = one
                       do i = 2, min( n+1-j, k+1 )
                          sum = sum + abs( ab( i, j ) )
                       end do
                    else
                       sum = zero
                       do i = 1, min( n+1-j, k+1 )
                          sum = sum + abs( ab( i, j ) )
                       end do
                    end if
                    if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
                 end do
              end if
           else if( stdlib_lsame( norm, 'I' ) ) then
              ! find normi(a).
              value = zero
              if( stdlib_lsame( uplo, 'U' ) ) then
                 if( stdlib_lsame( diag, 'U' ) ) then
                    do i = 1, n
                       work( i ) = one
                    end do
                    do j = 1, n
                       l = k + 1_ilp - j
                       do i = max( 1, j-k ), j - 1
                          work( i ) = work( i ) + abs( ab( l+i, j ) )
                       end do
                    end do
                 else
                    do i = 1, n
                       work( i ) = zero
                    end do
                    do j = 1, n
                       l = k + 1_ilp - j
                       do i = max( 1, j-k ), j
                          work( i ) = work( i ) + abs( ab( l+i, j ) )
                       end do
                    end do
                 end if
              else
                 if( stdlib_lsame( diag, 'U' ) ) then
                    do i = 1, n
                       work( i ) = one
                    end do
                    do j = 1, n
                       l = 1_ilp - j
                       do i = j + 1, min( n, j+k )
                          work( i ) = work( i ) + abs( ab( l+i, j ) )
                       end do
                    end do
                 else
                    do i = 1, n
                       work( i ) = zero
                    end do
                    do j = 1, n
                       l = 1_ilp - j
                       do i = j, min( n, j+k )
                          work( i ) = work( i ) + abs( ab( l+i, j ) )
                       end do
                    end do
                 end if
              end if
              do i = 1, n
                 sum = work( i )
                 if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
              end do
           else if( ( stdlib_lsame( norm, 'F' ) ) .or. ( stdlib_lsame( norm, 'E' ) ) ) &
                     then
              ! find normf(a).
              if( stdlib_lsame( uplo, 'U' ) ) then
                 if( stdlib_lsame( diag, 'U' ) ) then
                    scale = one
                    sum = n
                    if( k>0_ilp ) then
                       do j = 2, n
                          call stdlib_classq( min( j-1, k ),ab( max( k+2-j, 1_ilp ), j ), 1_ilp, scale,&
                                    sum )
                       end do
                    end if
                 else
                    scale = zero
                    sum = one
                    do j = 1, n
                       call stdlib_classq( min( j, k+1 ), ab( max( k+2-j, 1_ilp ), j ),1_ilp, scale, sum )
                                 
                    end do
                 end if
              else
                 if( stdlib_lsame( diag, 'U' ) ) then
                    scale = one
                    sum = n
                    if( k>0_ilp ) then
                       do j = 1, n - 1
                          call stdlib_classq( min( n-j, k ), ab( 2_ilp, j ), 1_ilp, scale,sum )
                       end do
                    end if
                 else
                    scale = zero
                    sum = one
                    do j = 1, n
                       call stdlib_classq( min( n-j+1, k+1 ), ab( 1_ilp, j ), 1_ilp, scale,sum )
                    end do
                 end if
              end if
              value = scale*sqrt( sum )
           end if
           stdlib_clantb = value
           return
     end function stdlib_clantb

     real(dp) module function stdlib_zlantb( norm, uplo, diag, n, k, ab,ldab, work )
     !! ZLANTB returns the value of the one norm,  or the Frobenius norm, or
     !! the  infinity norm,  or the element of  largest absolute value  of an
     !! n by n triangular band matrix A,  with ( k + 1 ) diagonals.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: diag, norm, uplo
           integer(ilp), intent(in) :: k, ldab, n
           ! Array Arguments 
           real(dp), intent(out) :: work(*)
           complex(dp), intent(in) :: ab(ldab,*)
       ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: udiag
           integer(ilp) :: i, j, l
           real(dp) :: scale, sum, value
           ! Intrinsic Functions 
           ! Executable Statements 
           if( n==0_ilp ) then
              value = zero
           else if( stdlib_lsame( norm, 'M' ) ) then
              ! find max(abs(a(i,j))).
              if( stdlib_lsame( diag, 'U' ) ) then
                 value = one
                 if( stdlib_lsame( uplo, 'U' ) ) then
                    do j = 1, n
                       do i = max( k+2-j, 1 ), k
                          sum = abs( ab( i, j ) )
                          if( value < sum .or. stdlib_disnan( sum ) ) value = sum
                       end do
                    end do
                 else
                    do j = 1, n
                       do i = 2, min( n+1-j, k+1 )
                          sum = abs( ab( i, j ) )
                          if( value < sum .or. stdlib_disnan( sum ) ) value = sum
                       end do
                    end do
                 end if
              else
                 value = zero
                 if( stdlib_lsame( uplo, 'U' ) ) then
                    do j = 1, n
                       do i = max( k+2-j, 1 ), k + 1
                          sum = abs( ab( i, j ) )
                          if( value < sum .or. stdlib_disnan( sum ) ) value = sum
                       end do
                    end do
                 else
                    do j = 1, n
                       do i = 1, min( n+1-j, k+1 )
                          sum = abs( ab( i, j ) )
                          if( value < sum .or. stdlib_disnan( sum ) ) value = sum
                       end do
                    end do
                 end if
              end if
           else if( ( stdlib_lsame( norm, 'O' ) ) .or. ( norm=='1' ) ) then
              ! find norm1(a).
              value = zero
              udiag = stdlib_lsame( diag, 'U' )
              if( stdlib_lsame( uplo, 'U' ) ) then
                 do j = 1, n
                    if( udiag ) then
                       sum = one
                       do i = max( k+2-j, 1 ), k
                          sum = sum + abs( ab( i, j ) )
                       end do
                    else
                       sum = zero
                       do i = max( k+2-j, 1 ), k + 1
                          sum = sum + abs( ab( i, j ) )
                       end do
                    end if
                    if( value < sum .or. stdlib_disnan( sum ) ) value = sum
                 end do
              else
                 do j = 1, n
                    if( udiag ) then
                       sum = one
                       do i = 2, min( n+1-j, k+1 )
                          sum = sum + abs( ab( i, j ) )
                       end do
                    else
                       sum = zero
                       do i = 1, min( n+1-j, k+1 )
                          sum = sum + abs( ab( i, j ) )
                       end do
                    end if
                    if( value < sum .or. stdlib_disnan( sum ) ) value = sum
                 end do
              end if
           else if( stdlib_lsame( norm, 'I' ) ) then
              ! find normi(a).
              value = zero
              if( stdlib_lsame( uplo, 'U' ) ) then
                 if( stdlib_lsame( diag, 'U' ) ) then
                    do i = 1, n
                       work( i ) = one
                    end do
                    do j = 1, n
                       l = k + 1_ilp - j
                       do i = max( 1, j-k ), j - 1
                          work( i ) = work( i ) + abs( ab( l+i, j ) )
                       end do
                    end do
                 else
                    do i = 1, n
                       work( i ) = zero
                    end do
                    do j = 1, n
                       l = k + 1_ilp - j
                       do i = max( 1, j-k ), j
                          work( i ) = work( i ) + abs( ab( l+i, j ) )
                       end do
                    end do
                 end if
              else
                 if( stdlib_lsame( diag, 'U' ) ) then
                    do i = 1, n
                       work( i ) = one
                    end do
                    do j = 1, n
                       l = 1_ilp - j
                       do i = j + 1, min( n, j+k )
                          work( i ) = work( i ) + abs( ab( l+i, j ) )
                       end do
                    end do
                 else
                    do i = 1, n
                       work( i ) = zero
                    end do
                    do j = 1, n
                       l = 1_ilp - j
                       do i = j, min( n, j+k )
                          work( i ) = work( i ) + abs( ab( l+i, j ) )
                       end do
                    end do
                 end if
              end if
              do i = 1, n
                 sum = work( i )
                 if( value < sum .or. stdlib_disnan( sum ) ) value = sum
              end do
           else if( ( stdlib_lsame( norm, 'F' ) ) .or. ( stdlib_lsame( norm, 'E' ) ) ) &
                     then
              ! find normf(a).
              if( stdlib_lsame( uplo, 'U' ) ) then
                 if( stdlib_lsame( diag, 'U' ) ) then
                    scale = one
                    sum = n
                    if( k>0_ilp ) then
                       do j = 2, n
                          call stdlib_zlassq( min( j-1, k ),ab( max( k+2-j, 1_ilp ), j ), 1_ilp, scale,&
                                    sum )
                       end do
                    end if
                 else
                    scale = zero
                    sum = one
                    do j = 1, n
                       call stdlib_zlassq( min( j, k+1 ), ab( max( k+2-j, 1_ilp ), j ),1_ilp, scale, sum )
                                 
                    end do
                 end if
              else
                 if( stdlib_lsame( diag, 'U' ) ) then
                    scale = one
                    sum = n
                    if( k>0_ilp ) then
                       do j = 1, n - 1
                          call stdlib_zlassq( min( n-j, k ), ab( 2_ilp, j ), 1_ilp, scale,sum )
                       end do
                    end if
                 else
                    scale = zero
                    sum = one
                    do j = 1, n
                       call stdlib_zlassq( min( n-j+1, k+1 ), ab( 1_ilp, j ), 1_ilp, scale,sum )
                    end do
                 end if
              end if
              value = scale*sqrt( sum )
           end if
           stdlib_zlantb = value
           return
     end function stdlib_zlantb




     real(sp) module function stdlib_slansy( norm, uplo, n, a, lda, work )
     !! SLANSY returns the value of the one norm,  or the Frobenius norm, or
     !! the  infinity norm,  or the  element of  largest absolute value  of a
     !! real symmetric matrix A.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: norm, uplo
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           real(sp), intent(in) :: a(lda,*)
           real(sp), intent(out) :: work(*)
       ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j
           real(sp) :: absa, scale, sum, value
           ! Intrinsic Functions 
           ! Executable Statements 
           if( n==0_ilp ) then
              value = zero
           else if( stdlib_lsame( norm, 'M' ) ) then
              ! find max(abs(a(i,j))).
              value = zero
              if( stdlib_lsame( uplo, 'U' ) ) then
                 do j = 1, n
                    do i = 1, j
                       sum = abs( a( i, j ) )
                       if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
                    end do
                 end do
              else
                 do j = 1, n
                    do i = j, n
                       sum = abs( a( i, j ) )
                       if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
                    end do
                 end do
              end if
           else if( ( stdlib_lsame( norm, 'I' ) ) .or. ( stdlib_lsame( norm, 'O' ) ) .or.( &
                     norm=='1' ) ) then
              ! find normi(a) ( = norm1(a), since a is symmetric).
              value = zero
              if( stdlib_lsame( uplo, 'U' ) ) then
                 do j = 1, n
                    sum = zero
                    do i = 1, j - 1
                       absa = abs( a( i, j ) )
                       sum = sum + absa
                       work( i ) = work( i ) + absa
                    end do
                    work( j ) = sum + abs( a( j, j ) )
                 end do
                 do i = 1, n
                    sum = work( i )
                    if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
                 end do
              else
                 do i = 1, n
                    work( i ) = zero
                 end do
                 do j = 1, n
                    sum = work( j ) + abs( a( j, j ) )
                    do i = j + 1, n
                       absa = abs( a( i, j ) )
                       sum = sum + absa
                       work( i ) = work( i ) + absa
                    end do
                    if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
                 end do
              end if
           else if( ( stdlib_lsame( norm, 'F' ) ) .or. ( stdlib_lsame( norm, 'E' ) ) ) &
                     then
              ! find normf(a).
              scale = zero
              sum = one
              if( stdlib_lsame( uplo, 'U' ) ) then
                 do j = 2, n
                    call stdlib_slassq( j-1, a( 1_ilp, j ), 1_ilp, scale, sum )
                 end do
              else
                 do j = 1, n - 1
                    call stdlib_slassq( n-j, a( j+1, j ), 1_ilp, scale, sum )
                 end do
              end if
              sum = 2_ilp*sum
              call stdlib_slassq( n, a, lda+1, scale, sum )
              value = scale*sqrt( sum )
           end if
           stdlib_slansy = value
           return
     end function stdlib_slansy

     real(dp) module function stdlib_dlansy( norm, uplo, n, a, lda, work )
     !! DLANSY returns the value of the one norm,  or the Frobenius norm, or
     !! the  infinity norm,  or the  element of  largest absolute value  of a
     !! real symmetric matrix A.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: norm, uplo
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           real(dp), intent(in) :: a(lda,*)
           real(dp), intent(out) :: work(*)
       ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j
           real(dp) :: absa, scale, sum, value
           ! Intrinsic Functions 
           ! Executable Statements 
           if( n==0_ilp ) then
              value = zero
           else if( stdlib_lsame( norm, 'M' ) ) then
              ! find max(abs(a(i,j))).
              value = zero
              if( stdlib_lsame( uplo, 'U' ) ) then
                 do j = 1, n
                    do i = 1, j
                       sum = abs( a( i, j ) )
                       if( value < sum .or. stdlib_disnan( sum ) ) value = sum
                    end do
                 end do
              else
                 do j = 1, n
                    do i = j, n
                       sum = abs( a( i, j ) )
                       if( value < sum .or. stdlib_disnan( sum ) ) value = sum
                    end do
                 end do
              end if
           else if( ( stdlib_lsame( norm, 'I' ) ) .or. ( stdlib_lsame( norm, 'O' ) ) .or.( &
                     norm=='1' ) ) then
              ! find normi(a) ( = norm1(a), since a is symmetric).
              value = zero
              if( stdlib_lsame( uplo, 'U' ) ) then
                 do j = 1, n
                    sum = zero
                    do i = 1, j - 1
                       absa = abs( a( i, j ) )
                       sum = sum + absa
                       work( i ) = work( i ) + absa
                    end do
                    work( j ) = sum + abs( a( j, j ) )
                 end do
                 do i = 1, n
                    sum = work( i )
                    if( value < sum .or. stdlib_disnan( sum ) ) value = sum
                 end do
              else
                 do i = 1, n
                    work( i ) = zero
                 end do
                 do j = 1, n
                    sum = work( j ) + abs( a( j, j ) )
                    do i = j + 1, n
                       absa = abs( a( i, j ) )
                       sum = sum + absa
                       work( i ) = work( i ) + absa
                    end do
                    if( value < sum .or. stdlib_disnan( sum ) ) value = sum
                 end do
              end if
           else if( ( stdlib_lsame( norm, 'F' ) ) .or. ( stdlib_lsame( norm, 'E' ) ) ) &
                     then
              ! find normf(a).
              scale = zero
              sum = one
              if( stdlib_lsame( uplo, 'U' ) ) then
                 do j = 2, n
                    call stdlib_dlassq( j-1, a( 1_ilp, j ), 1_ilp, scale, sum )
                 end do
              else
                 do j = 1, n - 1
                    call stdlib_dlassq( n-j, a( j+1, j ), 1_ilp, scale, sum )
                 end do
              end if
              sum = 2_ilp*sum
              call stdlib_dlassq( n, a, lda+1, scale, sum )
              value = scale*sqrt( sum )
           end if
           stdlib_dlansy = value
           return
     end function stdlib_dlansy


     real(sp) module function stdlib_clansy( norm, uplo, n, a, lda, work )
     !! CLANSY returns the value of the one norm,  or the Frobenius norm, or
     !! the  infinity norm,  or the  element of  largest absolute value  of a
     !! complex symmetric matrix A.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: norm, uplo
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           real(sp), intent(out) :: work(*)
           complex(sp), intent(in) :: a(lda,*)
       ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j
           real(sp) :: absa, scale, sum, value
           ! Intrinsic Functions 
           ! Executable Statements 
           if( n==0_ilp ) then
              value = zero
           else if( stdlib_lsame( norm, 'M' ) ) then
              ! find max(abs(a(i,j))).
              value = zero
              if( stdlib_lsame( uplo, 'U' ) ) then
                 do j = 1, n
                    do i = 1, j
                       sum = abs( a( i, j ) )
                       if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
                    end do
                 end do
              else
                 do j = 1, n
                    do i = j, n
                       sum = abs( a( i, j ) )
                       if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
                    end do
                 end do
              end if
           else if( ( stdlib_lsame( norm, 'I' ) ) .or. ( stdlib_lsame( norm, 'O' ) ) .or.( &
                     norm=='1' ) ) then
              ! find normi(a) ( = norm1(a), since a is symmetric).
              value = zero
              if( stdlib_lsame( uplo, 'U' ) ) then
                 do j = 1, n
                    sum = zero
                    do i = 1, j - 1
                       absa = abs( a( i, j ) )
                       sum = sum + absa
                       work( i ) = work( i ) + absa
                    end do
                    work( j ) = sum + abs( a( j, j ) )
                 end do
                 do i = 1, n
                    sum = work( i )
                    if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
                 end do
              else
                 do i = 1, n
                    work( i ) = zero
                 end do
                 do j = 1, n
                    sum = work( j ) + abs( a( j, j ) )
                    do i = j + 1, n
                       absa = abs( a( i, j ) )
                       sum = sum + absa
                       work( i ) = work( i ) + absa
                    end do
                    if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
                 end do
              end if
           else if( ( stdlib_lsame( norm, 'F' ) ) .or. ( stdlib_lsame( norm, 'E' ) ) ) &
                     then
              ! find normf(a).
              scale = zero
              sum = one
              if( stdlib_lsame( uplo, 'U' ) ) then
                 do j = 2, n
                    call stdlib_classq( j-1, a( 1_ilp, j ), 1_ilp, scale, sum )
                 end do
              else
                 do j = 1, n - 1
                    call stdlib_classq( n-j, a( j+1, j ), 1_ilp, scale, sum )
                 end do
              end if
              sum = 2_ilp*sum
              call stdlib_classq( n, a, lda+1, scale, sum )
              value = scale*sqrt( sum )
           end if
           stdlib_clansy = value
           return
     end function stdlib_clansy

     real(dp) module function stdlib_zlansy( norm, uplo, n, a, lda, work )
     !! ZLANSY returns the value of the one norm,  or the Frobenius norm, or
     !! the  infinity norm,  or the  element of  largest absolute value  of a
     !! complex symmetric matrix A.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: norm, uplo
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           real(dp), intent(out) :: work(*)
           complex(dp), intent(in) :: a(lda,*)
       ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j
           real(dp) :: absa, scale, sum, value
           ! Intrinsic Functions 
           ! Executable Statements 
           if( n==0_ilp ) then
              value = zero
           else if( stdlib_lsame( norm, 'M' ) ) then
              ! find max(abs(a(i,j))).
              value = zero
              if( stdlib_lsame( uplo, 'U' ) ) then
                 do j = 1, n
                    do i = 1, j
                       sum = abs( a( i, j ) )
                       if( value < sum .or. stdlib_disnan( sum ) ) value = sum
                    end do
                 end do
              else
                 do j = 1, n
                    do i = j, n
                       sum = abs( a( i, j ) )
                       if( value < sum .or. stdlib_disnan( sum ) ) value = sum
                    end do
                 end do
              end if
           else if( ( stdlib_lsame( norm, 'I' ) ) .or. ( stdlib_lsame( norm, 'O' ) ) .or.( &
                     norm=='1' ) ) then
              ! find normi(a) ( = norm1(a), since a is symmetric).
              value = zero
              if( stdlib_lsame( uplo, 'U' ) ) then
                 do j = 1, n
                    sum = zero
                    do i = 1, j - 1
                       absa = abs( a( i, j ) )
                       sum = sum + absa
                       work( i ) = work( i ) + absa
                    end do
                    work( j ) = sum + abs( a( j, j ) )
                 end do
                 do i = 1, n
                    sum = work( i )
                    if( value < sum .or. stdlib_disnan( sum ) ) value = sum
                 end do
              else
                 do i = 1, n
                    work( i ) = zero
                 end do
                 do j = 1, n
                    sum = work( j ) + abs( a( j, j ) )
                    do i = j + 1, n
                       absa = abs( a( i, j ) )
                       sum = sum + absa
                       work( i ) = work( i ) + absa
                    end do
                    if( value < sum .or. stdlib_disnan( sum ) ) value = sum
                 end do
              end if
           else if( ( stdlib_lsame( norm, 'F' ) ) .or. ( stdlib_lsame( norm, 'E' ) ) ) &
                     then
              ! find normf(a).
              scale = zero
              sum = one
              if( stdlib_lsame( uplo, 'U' ) ) then
                 do j = 2, n
                    call stdlib_zlassq( j-1, a( 1_ilp, j ), 1_ilp, scale, sum )
                 end do
              else
                 do j = 1, n - 1
                    call stdlib_zlassq( n-j, a( j+1, j ), 1_ilp, scale, sum )
                 end do
              end if
              sum = 2_ilp*sum
              call stdlib_zlassq( n, a, lda+1, scale, sum )
              value = scale*sqrt( sum )
           end if
           stdlib_zlansy = value
           return
     end function stdlib_zlansy




     real(sp) module function stdlib_clanhe( norm, uplo, n, a, lda, work )
     !! CLANHE returns the value of the one norm,  or the Frobenius norm, or
     !! the  infinity norm,  or the  element of  largest absolute value  of a
     !! complex hermitian matrix A.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: norm, uplo
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           real(sp), intent(out) :: work(*)
           complex(sp), intent(in) :: a(lda,*)
       ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j
           real(sp) :: absa, scale, sum, value
           ! Intrinsic Functions 
           ! Executable Statements 
           if( n==0_ilp ) then
              value = zero
           else if( stdlib_lsame( norm, 'M' ) ) then
              ! find max(abs(a(i,j))).
              value = zero
              if( stdlib_lsame( uplo, 'U' ) ) then
                 do j = 1, n
                    do i = 1, j - 1
                       sum = abs( a( i, j ) )
                       if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
                    end do
                    sum = abs( real( a( j, j ),KIND=sp) )
                    if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
                 end do
              else
                 do j = 1, n
                    sum = abs( real( a( j, j ),KIND=sp) )
                    if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
                    do i = j + 1, n
                       sum = abs( a( i, j ) )
                       if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
                    end do
                 end do
              end if
           else if( ( stdlib_lsame( norm, 'I' ) ) .or. ( stdlib_lsame( norm, 'O' ) ) .or.( &
                     norm=='1' ) ) then
              ! find normi(a) ( = norm1(a), since a is hermitian).
              value = zero
              if( stdlib_lsame( uplo, 'U' ) ) then
                 do j = 1, n
                    sum = zero
                    do i = 1, j - 1
                       absa = abs( a( i, j ) )
                       sum = sum + absa
                       work( i ) = work( i ) + absa
                    end do
                    work( j ) = sum + abs( real( a( j, j ),KIND=sp) )
                 end do
                 do i = 1, n
                    sum = work( i )
                    if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
                 end do
              else
                 do i = 1, n
                    work( i ) = zero
                 end do
                 do j = 1, n
                    sum = work( j ) + abs( real( a( j, j ),KIND=sp) )
                    do i = j + 1, n
                       absa = abs( a( i, j ) )
                       sum = sum + absa
                       work( i ) = work( i ) + absa
                    end do
                    if( value < sum .or. stdlib_sisnan( sum ) ) value = sum
                 end do
              end if
           else if( ( stdlib_lsame( norm, 'F' ) ) .or. ( stdlib_lsame( norm, 'E' ) ) ) &
                     then
              ! find normf(a).
              scale = zero
              sum = one
              if( stdlib_lsame( uplo, 'U' ) ) then
                 do j = 2, n
                    call stdlib_classq( j-1, a( 1_ilp, j ), 1_ilp, scale, sum )
                 end do
              else
                 do j = 1, n - 1
                    call stdlib_classq( n-j, a( j+1, j ), 1_ilp, scale, sum )
                 end do
              end if
              sum = 2_ilp*sum
              do i = 1, n
                 if( real( a( i, i ),KIND=sp)/=zero ) then
                    absa = abs( real( a( i, i ),KIND=sp) )
                    if( scale<absa ) then
                       sum = one + sum*( scale / absa )**2_ilp
                       scale = absa
                    else
                       sum = sum + ( absa / scale )**2_ilp
                    end if
                 end if
              end do
              value = scale*sqrt( sum )
           end if
           stdlib_clanhe = value
           return
     end function stdlib_clanhe

     real(dp) module function stdlib_zlanhe( norm, uplo, n, a, lda, work )
     !! ZLANHE returns the value of the one norm,  or the Frobenius norm, or
     !! the  infinity norm,  or the  element of  largest absolute value  of a
     !! complex hermitian matrix A.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: norm, uplo
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           real(dp), intent(out) :: work(*)
           complex(dp), intent(in) :: a(lda,*)
       ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, j
           real(dp) :: absa, scale, sum, value
           ! Intrinsic Functions 
           ! Executable Statements 
           if( n==0_ilp ) then
              value = zero
           else if( stdlib_lsame( norm, 'M' ) ) then
              ! find max(abs(a(i,j))).
              value = zero
              if( stdlib_lsame( uplo, 'U' ) ) then
                 do j = 1, n
                    do i = 1, j - 1
                       sum = abs( a( i, j ) )
                       if( value < sum .or. stdlib_disnan( sum ) ) value = sum
                    end do
                    sum = abs( real( a( j, j ),KIND=dp) )
                    if( value < sum .or. stdlib_disnan( sum ) ) value = sum
                 end do
              else
                 do j = 1, n
                    sum = abs( real( a( j, j ),KIND=dp) )
                    if( value < sum .or. stdlib_disnan( sum ) ) value = sum
                    do i = j + 1, n
                       sum = abs( a( i, j ) )
                       if( value < sum .or. stdlib_disnan( sum ) ) value = sum
                    end do
                 end do
              end if
           else if( ( stdlib_lsame( norm, 'I' ) ) .or. ( stdlib_lsame( norm, 'O' ) ) .or.( &
                     norm=='1' ) ) then
              ! find normi(a) ( = norm1(a), since a is hermitian).
              value = zero
              if( stdlib_lsame( uplo, 'U' ) ) then
                 do j = 1, n
                    sum = zero
                    do i = 1, j - 1
                       absa = abs( a( i, j ) )
                       sum = sum + absa
                       work( i ) = work( i ) + absa
                    end do
                    work( j ) = sum + abs( real( a( j, j ),KIND=dp) )
                 end do
                 do i = 1, n
                    sum = work( i )
                    if( value < sum .or. stdlib_disnan( sum ) ) value = sum
                 end do
              else
                 do i = 1, n
                    work( i ) = zero
                 end do
                 do j = 1, n
                    sum = work( j ) + abs( real( a( j, j ),KIND=dp) )
                    do i = j + 1, n
                       absa = abs( a( i, j ) )
                       sum = sum + absa
                       work( i ) = work( i ) + absa
                    end do
                    if( value < sum .or. stdlib_disnan( sum ) ) value = sum
                 end do
              end if
           else if( ( stdlib_lsame( norm, 'F' ) ) .or. ( stdlib_lsame( norm, 'E' ) ) ) &
                     then
              ! find normf(a).
              scale = zero
              sum = one
              if( stdlib_lsame( uplo, 'U' ) ) then
                 do j = 2, n
                    call stdlib_zlassq( j-1, a( 1_ilp, j ), 1_ilp, scale, sum )
                 end do
              else
                 do j = 1, n - 1
                    call stdlib_zlassq( n-j, a( j+1, j ), 1_ilp, scale, sum )
                 end do
              end if
              sum = 2_ilp*sum
              do i = 1, n
                 if( real( a( i, i ),KIND=dp)/=zero ) then
                    absa = abs( real( a( i, i ),KIND=dp) )
                    if( scale<absa ) then
                       sum = one + sum*( scale / absa )**2_ilp
                       scale = absa
                    else
                       sum = sum + ( absa / scale )**2_ilp
                    end if
                 end if
              end do
              value = scale*sqrt( sum )
           end if
           stdlib_zlanhe = value
           return
     end function stdlib_zlanhe



end submodule stdlib_lapack_blas_like_mnorm
