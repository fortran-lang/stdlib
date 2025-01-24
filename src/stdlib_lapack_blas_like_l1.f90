submodule(stdlib_lapack_base) stdlib_lapack_blas_like_l1
  implicit none


  contains

     pure module subroutine stdlib_clacgv( n, x, incx )
     !! CLACGV conjugates a complex vector of length N.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx, n
           ! Array Arguments 
           complex(sp), intent(inout) :: x(*)
       ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, ioff
           ! Intrinsic Functions 
           ! Executable Statements 
           if( incx==1_ilp ) then
              do i = 1, n
                 x( i ) = conjg( x( i ) )
              end do
           else
              ioff = 1_ilp
              if( incx<0_ilp )ioff = 1_ilp - ( n-1 )*incx
              do i = 1, n
                 x( ioff ) = conjg( x( ioff ) )
                 ioff = ioff + incx
              end do
           end if
           return
     end subroutine stdlib_clacgv

     pure module subroutine stdlib_zlacgv( n, x, incx )
     !! ZLACGV conjugates a complex vector of length N.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx, n
           ! Array Arguments 
           complex(dp), intent(inout) :: x(*)
       ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, ioff
           ! Intrinsic Functions 
           ! Executable Statements 
           if( incx==1_ilp ) then
              do i = 1, n
                 x( i ) = conjg( x( i ) )
              end do
           else
              ioff = 1_ilp
              if( incx<0_ilp )ioff = 1_ilp - ( n-1 )*incx
              do i = 1, n
                 x( ioff ) = conjg( x( ioff ) )
                 ioff = ioff + incx
              end do
           end if
           return
     end subroutine stdlib_zlacgv




     pure module subroutine stdlib_slasrt( id, n, d, info )
     !! Sort the numbers in D in increasing order (if ID = 'I') or
     !! in decreasing order (if ID = 'D' ).
     !! Use Quick Sort, reverting to Insertion sort on arrays of
     !! size <= 20. Dimension of STACK limits N to about 2**32.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: id
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           real(sp), intent(inout) :: d(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: select = 20_ilp
           
           ! Local Scalars 
           integer(ilp) :: dir, endd, i, j, start, stkpnt
           real(sp) :: d1, d2, d3, dmnmx, tmp
           ! Local Arrays 
           integer(ilp) :: stack(2_ilp,32_ilp)
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           dir = -1_ilp
           if( stdlib_lsame( id, 'D' ) ) then
              dir = 0_ilp
           else if( stdlib_lsame( id, 'I' ) ) then
              dir = 1_ilp
           end if
           if( dir==-1_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SLASRT', -info )
              return
           end if
           ! quick return if possible
           if( n<=1 )return
           stkpnt = 1_ilp
           stack( 1_ilp, 1_ilp ) = 1_ilp
           stack( 2_ilp, 1_ilp ) = n
           10 continue
           start = stack( 1_ilp, stkpnt )
           endd = stack( 2_ilp, stkpnt )
           stkpnt = stkpnt - 1_ilp
           if( endd-start<=select .and. endd-start>0_ilp ) then
              ! do insertion sort on d( start:endd )
              if( dir==0_ilp ) then
                 ! sort into decreasing order
                 loop_30: do i = start + 1, endd
                    do j = i, start + 1, -1
                       if( d( j )>d( j-1 ) ) then
                          dmnmx = d( j )
                          d( j ) = d( j-1 )
                          d( j-1 ) = dmnmx
                       else
                          cycle loop_30
                       end if
                    end do
                 end do loop_30
              else
                 ! sort into increasing order
                 loop_50: do i = start + 1, endd
                    do j = i, start + 1, -1
                       if( d( j )<d( j-1 ) ) then
                          dmnmx = d( j )
                          d( j ) = d( j-1 )
                          d( j-1 ) = dmnmx
                       else
                          cycle loop_50
                       end if
                    end do
                 end do loop_50
              end if
           else if( endd-start>select ) then
              ! partition d( start:endd ) and stack parts, largest one first
              ! choose partition entry as median of 3
              d1 = d( start )
              d2 = d( endd )
              i = ( start+endd ) / 2_ilp
              d3 = d( i )
              if( d1<d2 ) then
                 if( d3<d1 ) then
                    dmnmx = d1
                 else if( d3<d2 ) then
                    dmnmx = d3
                 else
                    dmnmx = d2
                 end if
              else
                 if( d3<d2 ) then
                    dmnmx = d2
                 else if( d3<d1 ) then
                    dmnmx = d3
                 else
                    dmnmx = d1
                 end if
              end if
              if( dir==0_ilp ) then
                 ! sort into decreasing order
                 i = start - 1_ilp
                 j = endd + 1_ilp
                 60 continue
                 70 continue
                 j = j - 1_ilp
                 if( d( j )<dmnmx )go to 70
                 80 continue
                 i = i + 1_ilp
                 if( d( i )>dmnmx )go to 80
                 if( i<j ) then
                    tmp = d( i )
                    d( i ) = d( j )
                    d( j ) = tmp
                    go to 60
                 end if
                 if( j-start>endd-j-1 ) then
                    stkpnt = stkpnt + 1_ilp
                    stack( 1_ilp, stkpnt ) = start
                    stack( 2_ilp, stkpnt ) = j
                    stkpnt = stkpnt + 1_ilp
                    stack( 1_ilp, stkpnt ) = j + 1_ilp
                    stack( 2_ilp, stkpnt ) = endd
                 else
                    stkpnt = stkpnt + 1_ilp
                    stack( 1_ilp, stkpnt ) = j + 1_ilp
                    stack( 2_ilp, stkpnt ) = endd
                    stkpnt = stkpnt + 1_ilp
                    stack( 1_ilp, stkpnt ) = start
                    stack( 2_ilp, stkpnt ) = j
                 end if
              else
                 ! sort into increasing order
                 i = start - 1_ilp
                 j = endd + 1_ilp
                 90 continue
                 100 continue
                 j = j - 1_ilp
                 if( d( j )>dmnmx )go to 100
                 110 continue
                 i = i + 1_ilp
                 if( d( i )<dmnmx )go to 110
                 if( i<j ) then
                    tmp = d( i )
                    d( i ) = d( j )
                    d( j ) = tmp
                    go to 90
                 end if
                 if( j-start>endd-j-1 ) then
                    stkpnt = stkpnt + 1_ilp
                    stack( 1_ilp, stkpnt ) = start
                    stack( 2_ilp, stkpnt ) = j
                    stkpnt = stkpnt + 1_ilp
                    stack( 1_ilp, stkpnt ) = j + 1_ilp
                    stack( 2_ilp, stkpnt ) = endd
                 else
                    stkpnt = stkpnt + 1_ilp
                    stack( 1_ilp, stkpnt ) = j + 1_ilp
                    stack( 2_ilp, stkpnt ) = endd
                    stkpnt = stkpnt + 1_ilp
                    stack( 1_ilp, stkpnt ) = start
                    stack( 2_ilp, stkpnt ) = j
                 end if
              end if
           end if
           if( stkpnt>0 )go to 10
           return
     end subroutine stdlib_slasrt

     pure module subroutine stdlib_dlasrt( id, n, d, info )
     !! Sort the numbers in D in increasing order (if ID = 'I') or
     !! in decreasing order (if ID = 'D' ).
     !! Use Quick Sort, reverting to Insertion sort on arrays of
     !! size <= 20. Dimension of STACK limits N to about 2**32.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: id
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           ! Array Arguments 
           real(dp), intent(inout) :: d(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: select = 20_ilp
           
           ! Local Scalars 
           integer(ilp) :: dir, endd, i, j, start, stkpnt
           real(dp) :: d1, d2, d3, dmnmx, tmp
           ! Local Arrays 
           integer(ilp) :: stack(2_ilp,32_ilp)
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           dir = -1_ilp
           if( stdlib_lsame( id, 'D' ) ) then
              dir = 0_ilp
           else if( stdlib_lsame( id, 'I' ) ) then
              dir = 1_ilp
           end if
           if( dir==-1_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DLASRT', -info )
              return
           end if
           ! quick return if possible
           if( n<=1 )return
           stkpnt = 1_ilp
           stack( 1_ilp, 1_ilp ) = 1_ilp
           stack( 2_ilp, 1_ilp ) = n
           10 continue
           start = stack( 1_ilp, stkpnt )
           endd = stack( 2_ilp, stkpnt )
           stkpnt = stkpnt - 1_ilp
           if( endd-start<=select .and. endd-start>0_ilp ) then
              ! do insertion sort on d( start:endd )
              if( dir==0_ilp ) then
                 ! sort into decreasing order
                 loop_30: do i = start + 1, endd
                    do j = i, start + 1, -1
                       if( d( j )>d( j-1 ) ) then
                          dmnmx = d( j )
                          d( j ) = d( j-1 )
                          d( j-1 ) = dmnmx
                       else
                          cycle loop_30
                       end if
                    end do
                 end do loop_30
              else
                 ! sort into increasing order
                 loop_50: do i = start + 1, endd
                    do j = i, start + 1, -1
                       if( d( j )<d( j-1 ) ) then
                          dmnmx = d( j )
                          d( j ) = d( j-1 )
                          d( j-1 ) = dmnmx
                       else
                          cycle loop_50
                       end if
                    end do
                 end do loop_50
              end if
           else if( endd-start>select ) then
              ! partition d( start:endd ) and stack parts, largest one first
              ! choose partition entry as median of 3
              d1 = d( start )
              d2 = d( endd )
              i = ( start+endd ) / 2_ilp
              d3 = d( i )
              if( d1<d2 ) then
                 if( d3<d1 ) then
                    dmnmx = d1
                 else if( d3<d2 ) then
                    dmnmx = d3
                 else
                    dmnmx = d2
                 end if
              else
                 if( d3<d2 ) then
                    dmnmx = d2
                 else if( d3<d1 ) then
                    dmnmx = d3
                 else
                    dmnmx = d1
                 end if
              end if
              if( dir==0_ilp ) then
                 ! sort into decreasing order
                 i = start - 1_ilp
                 j = endd + 1_ilp
                 60 continue
                 70 continue
                 j = j - 1_ilp
                 if( d( j )<dmnmx )go to 70
                 80 continue
                 i = i + 1_ilp
                 if( d( i )>dmnmx )go to 80
                 if( i<j ) then
                    tmp = d( i )
                    d( i ) = d( j )
                    d( j ) = tmp
                    go to 60
                 end if
                 if( j-start>endd-j-1 ) then
                    stkpnt = stkpnt + 1_ilp
                    stack( 1_ilp, stkpnt ) = start
                    stack( 2_ilp, stkpnt ) = j
                    stkpnt = stkpnt + 1_ilp
                    stack( 1_ilp, stkpnt ) = j + 1_ilp
                    stack( 2_ilp, stkpnt ) = endd
                 else
                    stkpnt = stkpnt + 1_ilp
                    stack( 1_ilp, stkpnt ) = j + 1_ilp
                    stack( 2_ilp, stkpnt ) = endd
                    stkpnt = stkpnt + 1_ilp
                    stack( 1_ilp, stkpnt ) = start
                    stack( 2_ilp, stkpnt ) = j
                 end if
              else
                 ! sort into increasing order
                 i = start - 1_ilp
                 j = endd + 1_ilp
                 90 continue
                 100 continue
                 j = j - 1_ilp
                 if( d( j )>dmnmx )go to 100
                 110 continue
                 i = i + 1_ilp
                 if( d( i )<dmnmx )go to 110
                 if( i<j ) then
                    tmp = d( i )
                    d( i ) = d( j )
                    d( j ) = tmp
                    go to 90
                 end if
                 if( j-start>endd-j-1 ) then
                    stkpnt = stkpnt + 1_ilp
                    stack( 1_ilp, stkpnt ) = start
                    stack( 2_ilp, stkpnt ) = j
                    stkpnt = stkpnt + 1_ilp
                    stack( 1_ilp, stkpnt ) = j + 1_ilp
                    stack( 2_ilp, stkpnt ) = endd
                 else
                    stkpnt = stkpnt + 1_ilp
                    stack( 1_ilp, stkpnt ) = j + 1_ilp
                    stack( 2_ilp, stkpnt ) = endd
                    stkpnt = stkpnt + 1_ilp
                    stack( 1_ilp, stkpnt ) = start
                    stack( 2_ilp, stkpnt ) = j
                 end if
              end if
           end if
           if( stkpnt>0 )go to 10
           return
     end subroutine stdlib_dlasrt




     pure module subroutine stdlib_slassq( n, x, incx, scl, sumsq )
     !! SLASSQ returns the values  scl  and  smsq  such that
     !! ( scl**2 )*smsq = x( 1 )**2 +...+ x( n )**2 + ( scale**2 )*sumsq,
     !! where  x( i ) = X( 1 + ( i - 1 )*INCX ). The value of  sumsq  is
     !! assumed to be non-negative.
     !! scale and sumsq must be supplied in SCALE and SUMSQ and
     !! scl and smsq are overwritten on SCALE and SUMSQ respectively.
     !! If scale * sqrt( sumsq ) > tbig then
     !! we require:   scale >= sqrt( TINY*EPS ) / sbig   on entry,
     !! and if 0 < scale * sqrt( sumsq ) < tsml then
     !! we require:   scale <= sqrt( HUGE ) / ssml       on entry,
     !! where
     !! tbig -- upper threshold for values whose square is representable;
     !! sbig -- scaling constant for big numbers; \see la_constants.f90
     !! tsml -- lower threshold for values whose square is representable;
     !! ssml -- scaling constant for small numbers; \see la_constants.f90
     !! and
     !! TINY*EPS -- tiniest representable number;
     !! HUGE     -- biggest representable number.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: zero, half, one, two, tbig, tsml, ssml, sbig
        ! Scalar Arguments 
     integer(ilp), intent(in) :: incx, n
        real(sp), intent(inout) :: scl, sumsq
        ! Array Arguments 
        real(sp), intent(in) :: x(*)
        ! =====================================================================
        ! Local Scalars
     integer(ilp) :: i, ix
     logical(lk) :: notbig
        real(sp) :: abig, amed, asml, ax, ymax, ymin
        ! quick return if possible
        if( ieee_is_nan(scl) .or. ieee_is_nan(sumsq) ) return
        if( sumsq == zero ) scl = one
        if( scl == zero ) then
           scl = one
           sumsq = zero
        end if
        if (n <= 0_ilp) then
           return
        end if
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
        ix = 1_ilp
        if( incx < 0_ilp ) ix = 1_ilp - (n-1)*incx
        do i = 1, n
           ax = abs(x(ix))
           if (ax > tbig) then
              abig = abig + (ax*sbig)**2_ilp
              notbig = .false.
           else if (ax < tsml) then
              if (notbig) asml = asml + (ax*ssml)**2_ilp
           else
              amed = amed + ax**2_ilp
           end if
           ix = ix + incx
        end do
        ! put the existing sum of squares into one of the accumulators
        if( sumsq > zero ) then
           ax = scl*sqrt( sumsq )
           if (ax > tbig) then
              ! we assume scl >= sqrt( tiny*eps ) / sbig
              abig = abig + (scl*sbig)**2_ilp * sumsq
           else if (ax < tsml) then
              ! we assume scl <= sqrt( huge ) / ssml
              if (notbig) asml = asml + (scl*ssml)**2_ilp * sumsq
           else
              amed = amed + scl**2_ilp * sumsq
           end if
        end if
        ! combine abig and amed or amed and asml if more than one
        ! accumulator was used.
        if (abig > zero) then
           ! combine abig and amed if abig > 0.
           if (amed > zero .or. ieee_is_nan(amed)) then
              abig = abig + (amed*sbig)*sbig
           end if
           scl = one / sbig
           sumsq = abig
        else if (asml > zero) then
           ! combine amed and asml if asml > 0.
           if (amed > zero .or. ieee_is_nan(amed)) then
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
              sumsq = ymax**2_ilp*( one + (ymin/ymax)**2_ilp )
           else
              scl = one / ssml
              sumsq = asml
           end if
        else
           ! otherwise all values are mid-range or zero
           scl = one
           sumsq = amed
        end if
        return
     end subroutine stdlib_slassq

     pure module subroutine stdlib_dlassq( n, x, incx, scl, sumsq )
     !! DLASSQ returns the values  scl  and  smsq  such that
     !! ( scl**2 )*smsq = x( 1 )**2 +...+ x( n )**2 + ( scale**2 )*sumsq,
     !! where  x( i ) = X( 1 + ( i - 1 )*INCX ). The value of  sumsq  is
     !! assumed to be non-negative.
     !! scale and sumsq must be supplied in SCALE and SUMSQ and
     !! scl and smsq are overwritten on SCALE and SUMSQ respectively.
     !! If scale * sqrt( sumsq ) > tbig then
     !! we require:   scale >= sqrt( TINY*EPS ) / sbig   on entry,
     !! and if 0 < scale * sqrt( sumsq ) < tsml then
     !! we require:   scale <= sqrt( HUGE ) / ssml       on entry,
     !! where
     !! tbig -- upper threshold for values whose square is representable;
     !! sbig -- scaling constant for big numbers; \see la_constants.f90
     !! tsml -- lower threshold for values whose square is representable;
     !! ssml -- scaling constant for small numbers; \see la_constants.f90
     !! and
     !! TINY*EPS -- tiniest representable number;
     !! HUGE     -- biggest representable number.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: zero, half, one, two, tbig, tsml, ssml, sbig
        ! Scalar Arguments 
     integer(ilp), intent(in) :: incx, n
        real(dp), intent(inout) :: scl, sumsq
        ! Array Arguments 
        real(dp), intent(in) :: x(*)
        ! =====================================================================
        ! Local Scalars 
     integer(ilp) :: i, ix
     logical(lk) :: notbig
        real(dp) :: abig, amed, asml, ax, ymax, ymin
        ! quick return if possible
        if( ieee_is_nan(scl) .or. ieee_is_nan(sumsq) ) return
        if( sumsq == zero ) scl = one
        if( scl == zero ) then
           scl = one
           sumsq = zero
        end if
        if (n <= 0_ilp) then
           return
        end if
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
        ix = 1_ilp
        if( incx < 0_ilp ) ix = 1_ilp - (n-1)*incx
        do i = 1, n
           ax = abs(x(ix))
           if (ax > tbig) then
              abig = abig + (ax*sbig)**2_ilp
              notbig = .false.
           else if (ax < tsml) then
              if (notbig) asml = asml + (ax*ssml)**2_ilp
           else
              amed = amed + ax**2_ilp
           end if
           ix = ix + incx
        end do
        ! put the existing sum of squares into one of the accumulators
        if( sumsq > zero ) then
           ax = scl*sqrt( sumsq )
           if (ax > tbig) then
              ! we assume scl >= sqrt( tiny*eps ) / sbig
              abig = abig + (scl*sbig)**2_ilp * sumsq
           else if (ax < tsml) then
              ! we assume scl <= sqrt( huge ) / ssml
              if (notbig) asml = asml + (scl*ssml)**2_ilp * sumsq
           else
              amed = amed + scl**2_ilp * sumsq
           end if
        end if
        ! combine abig and amed or amed and asml if more than one
        ! accumulator was used.
        if (abig > zero) then
           ! combine abig and amed if abig > 0.
           if (amed > zero .or. ieee_is_nan(amed)) then
              abig = abig + (amed*sbig)*sbig
           end if
           scl = one / sbig
           sumsq = abig
        else if (asml > zero) then
           ! combine amed and asml if asml > 0.
           if (amed > zero .or. ieee_is_nan(amed)) then
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
              sumsq = ymax**2_ilp*( one + (ymin/ymax)**2_ilp )
           else
              scl = one / ssml
              sumsq = asml
           end if
        else
           ! otherwise all values are mid-range or zero
           scl = one
           sumsq = amed
        end if
        return
     end subroutine stdlib_dlassq


     pure module subroutine stdlib_classq( n, x, incx, scl, sumsq )
     !! CLASSQ returns the values  scl  and  smsq  such that
     !! ( scl**2 )*smsq = x( 1 )**2 +...+ x( n )**2 + ( scale**2 )*sumsq,
     !! where  x( i ) = X( 1 + ( i - 1 )*INCX ). The value of  sumsq  is
     !! assumed to be non-negative.
     !! scale and sumsq must be supplied in SCALE and SUMSQ and
     !! scl and smsq are overwritten on SCALE and SUMSQ respectively.
     !! If scale * sqrt( sumsq ) > tbig then
     !! we require:   scale >= sqrt( TINY*EPS ) / sbig   on entry,
     !! and if 0 < scale * sqrt( sumsq ) < tsml then
     !! we require:   scale <= sqrt( HUGE ) / ssml       on entry,
     !! where
     !! tbig -- upper threshold for values whose square is representable;
     !! sbig -- scaling constant for big numbers; \see la_constants.f90
     !! tsml -- lower threshold for values whose square is representable;
     !! ssml -- scaling constant for small numbers; \see la_constants.f90
     !! and
     !! TINY*EPS -- tiniest representable number;
     !! HUGE     -- biggest representable number.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: zero, half, one, two, tbig, tsml, ssml, sbig
        ! Scalar Arguments 
     integer(ilp), intent(in) :: incx, n
        real(sp), intent(inout) :: scl, sumsq
        ! Array Arguments 
        complex(sp), intent(in) :: x(*)
        ! =====================================================================
        ! Local Scalars 
     integer(ilp) :: i, ix
     logical(lk) :: notbig
        real(sp) :: abig, amed, asml, ax, ymax, ymin
        ! quick return if possible
        if( ieee_is_nan(scl) .or. ieee_is_nan(sumsq) ) return
        if( sumsq == zero ) scl = one
        if( scl == zero ) then
           scl = one
           sumsq = zero
        end if
        if (n <= 0_ilp) then
           return
        end if
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
        ix = 1_ilp
        if( incx < 0_ilp ) ix = 1_ilp - (n-1)*incx
        do i = 1, n
           ax = abs(real(x(ix),KIND=sp))
           if (ax > tbig) then
              abig = abig + (ax*sbig)**2_ilp
              notbig = .false.
           else if (ax < tsml) then
              if (notbig) asml = asml + (ax*ssml)**2_ilp
           else
              amed = amed + ax**2_ilp
           end if
           ax = abs(aimag(x(ix)))
           if (ax > tbig) then
              abig = abig + (ax*sbig)**2_ilp
              notbig = .false.
           else if (ax < tsml) then
              if (notbig) asml = asml + (ax*ssml)**2_ilp
           else
              amed = amed + ax**2_ilp
           end if
           ix = ix + incx
        end do
        ! put the existing sum of squares into one of the accumulators
        if( sumsq > zero ) then
           ax = scl*sqrt( sumsq )
           if (ax > tbig) then
              ! we assume scl >= sqrt( tiny*eps ) / sbig
              abig = abig + (scl*sbig)**2_ilp * sumsq
           else if (ax < tsml) then
              ! we assume scl <= sqrt( huge ) / ssml
              if (notbig) asml = asml + (scl*ssml)**2_ilp * sumsq
           else
              amed = amed + scl**2_ilp * sumsq
           end if
        end if
        ! combine abig and amed or amed and asml if more than one
        ! accumulator was used.
        if (abig > zero) then
           ! combine abig and amed if abig > 0.
           if (amed > zero .or. ieee_is_nan(amed)) then
              abig = abig + (amed*sbig)*sbig
           end if
           scl = one / sbig
           sumsq = abig
        else if (asml > zero) then
           ! combine amed and asml if asml > 0.
           if (amed > zero .or. ieee_is_nan(amed)) then
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
              sumsq = ymax**2_ilp*( one + (ymin/ymax)**2_ilp )
           else
              scl = one / ssml
              sumsq = asml
           end if
        else
           ! otherwise all values are mid-range or zero
           scl = one
           sumsq = amed
        end if
        return
     end subroutine stdlib_classq

     pure module subroutine stdlib_zlassq( n, x, incx, scl, sumsq )
     !! ZLASSQ returns the values  scl  and  smsq  such that
     !! ( scl**2 )*smsq = x( 1 )**2 +...+ x( n )**2 + ( scale**2 )*sumsq,
     !! where  x( i ) = X( 1 + ( i - 1 )*INCX ). The value of  sumsq  is
     !! assumed to be non-negative.
     !! scale and sumsq must be supplied in SCALE and SUMSQ and
     !! scl and smsq are overwritten on SCALE and SUMSQ respectively.
     !! If scale * sqrt( sumsq ) > tbig then
     !! we require:   scale >= sqrt( TINY*EPS ) / sbig   on entry,
     !! and if 0 < scale * sqrt( sumsq ) < tsml then
     !! we require:   scale <= sqrt( HUGE ) / ssml       on entry,
     !! where
     !! tbig -- upper threshold for values whose square is representable;
     !! sbig -- scaling constant for big numbers; \see la_constants.f90
     !! tsml -- lower threshold for values whose square is representable;
     !! ssml -- scaling constant for small numbers; \see la_constants.f90
     !! and
     !! TINY*EPS -- tiniest representable number;
     !! HUGE     -- biggest representable number.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: zero, half, one, two, tbig, tsml, ssml, sbig
        ! Scalar Arguments 
     integer(ilp), intent(in) :: incx, n
        real(dp), intent(inout) :: scl, sumsq
        ! Array Arguments 
        complex(dp), intent(in) :: x(*)
        ! =====================================================================
        ! Local Scalars 
     integer(ilp) :: i, ix
     logical(lk) :: notbig
        real(dp) :: abig, amed, asml, ax, ymax, ymin
        ! quick return if possible
        if( ieee_is_nan(scl) .or. ieee_is_nan(sumsq) ) return
        if( sumsq == zero ) scl = one
        if( scl == zero ) then
           scl = one
           sumsq = zero
        end if
        if (n <= 0_ilp) then
           return
        end if
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
        ix = 1_ilp
        if( incx < 0_ilp ) ix = 1_ilp - (n-1)*incx
        do i = 1, n
           ax = abs(real(x(ix),KIND=dp))
           if (ax > tbig) then
              abig = abig + (ax*sbig)**2_ilp
              notbig = .false.
           else if (ax < tsml) then
              if (notbig) asml = asml + (ax*ssml)**2_ilp
           else
              amed = amed + ax**2_ilp
           end if
           ax = abs(aimag(x(ix)))
           if (ax > tbig) then
              abig = abig + (ax*sbig)**2_ilp
              notbig = .false.
           else if (ax < tsml) then
              if (notbig) asml = asml + (ax*ssml)**2_ilp
           else
              amed = amed + ax**2_ilp
           end if
           ix = ix + incx
        end do
        ! put the existing sum of squares into one of the accumulators
        if( sumsq > zero ) then
           ax = scl*sqrt( sumsq )
           if (ax > tbig) then
              ! we assume scl >= sqrt( tiny*eps ) / sbig
              abig = abig + (scl*sbig)**2_ilp * sumsq
           else if (ax < tsml) then
              ! we assume scl <= sqrt( huge ) / ssml
              if (notbig) asml = asml + (scl*ssml)**2_ilp * sumsq
           else
              amed = amed + scl**2_ilp * sumsq
           end if
        end if
        ! combine abig and amed or amed and asml if more than one
        ! accumulator was used.
        if (abig > zero) then
           ! combine abig and amed if abig > 0.
           if (amed > zero .or. ieee_is_nan(amed)) then
              abig = abig + (amed*sbig)*sbig
           end if
           scl = one / sbig
           sumsq = abig
        else if (asml > zero) then
           ! combine amed and asml if asml > 0.
           if (amed > zero .or. ieee_is_nan(amed)) then
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
              sumsq = ymax**2_ilp*( one + (ymin/ymax)**2_ilp )
           else
              scl = one / ssml
              sumsq = asml
           end if
        else
           ! otherwise all values are mid-range or zero
           scl = one
           sumsq = amed
        end if
        return
     end subroutine stdlib_zlassq




     pure module subroutine stdlib_srscl( n, sa, sx, incx )
     !! SRSCL multiplies an n-element real vector x by the real scalar 1/a.
     !! This is done without overflow or underflow as long as
     !! the final result x/a does not overflow or underflow.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx, n
           real(sp), intent(in) :: sa
           ! Array Arguments 
           real(sp), intent(inout) :: sx(*)
       ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: done
           real(sp) :: bignum, cden, cden1, cnum, cnum1, mul, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           ! quick return if possible
           if( n<=0 )return
           ! get machine parameters
           smlnum = stdlib_slamch( 'S' )
           bignum = one / smlnum
           call stdlib_slabad( smlnum, bignum )
           ! initialize the denominator to sa and the numerator to 1.
           cden = sa
           cnum = one
           10 continue
           cden1 = cden*smlnum
           cnum1 = cnum / bignum
           if( abs( cden1 )>abs( cnum ) .and. cnum/=zero ) then
              ! pre-multiply x by smlnum if cden is large compared to cnum.
              mul = smlnum
              done = .false.
              cden = cden1
           else if( abs( cnum1 )>abs( cden ) ) then
              ! pre-multiply x by bignum if cden is small compared to cnum.
              mul = bignum
              done = .false.
              cnum = cnum1
           else
              ! multiply x by cnum / cden and return.
              mul = cnum / cden
              done = .true.
           end if
           ! scale the vector x by mul
           call stdlib_sscal( n, mul, sx, incx )
           if( .not.done )go to 10
           return
     end subroutine stdlib_srscl

     pure module subroutine stdlib_drscl( n, sa, sx, incx )
     !! DRSCL multiplies an n-element real vector x by the real scalar 1/a.
     !! This is done without overflow or underflow as long as
     !! the final result x/a does not overflow or underflow.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx, n
           real(dp), intent(in) :: sa
           ! Array Arguments 
           real(dp), intent(inout) :: sx(*)
       ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: done
           real(dp) :: bignum, cden, cden1, cnum, cnum1, mul, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           ! quick return if possible
           if( n<=0 )return
           ! get machine parameters
           smlnum = stdlib_dlamch( 'S' )
           bignum = one / smlnum
           call stdlib_dlabad( smlnum, bignum )
           ! initialize the denominator to sa and the numerator to 1.
           cden = sa
           cnum = one
           10 continue
           cden1 = cden*smlnum
           cnum1 = cnum / bignum
           if( abs( cden1 )>abs( cnum ) .and. cnum/=zero ) then
              ! pre-multiply x by smlnum if cden is large compared to cnum.
              mul = smlnum
              done = .false.
              cden = cden1
           else if( abs( cnum1 )>abs( cden ) ) then
              ! pre-multiply x by bignum if cden is small compared to cnum.
              mul = bignum
              done = .false.
              cnum = cnum1
           else
              ! multiply x by cnum / cden and return.
              mul = cnum / cden
              done = .true.
           end if
           ! scale the vector x by mul
           call stdlib_dscal( n, mul, sx, incx )
           if( .not.done )go to 10
           return
     end subroutine stdlib_drscl




     pure module subroutine stdlib_csrscl( n, sa, sx, incx )
     !! CSRSCL multiplies an n-element complex vector x by the real scalar
     !! 1/a.  This is done without overflow or underflow as long as
     !! the final result x/a does not overflow or underflow.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx, n
           real(sp), intent(in) :: sa
           ! Array Arguments 
           complex(sp), intent(inout) :: sx(*)
       ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: done
           real(sp) :: bignum, cden, cden1, cnum, cnum1, mul, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           ! quick return if possible
           if( n<=0 )return
           ! get machine parameters
           smlnum = stdlib_slamch( 'S' )
           bignum = one / smlnum
           call stdlib_slabad( smlnum, bignum )
           ! initialize the denominator to sa and the numerator to 1.
           cden = sa
           cnum = one
           10 continue
           cden1 = cden*smlnum
           cnum1 = cnum / bignum
           if( abs( cden1 )>abs( cnum ) .and. cnum/=zero ) then
              ! pre-multiply x by smlnum if cden is large compared to cnum.
              mul = smlnum
              done = .false.
              cden = cden1
           else if( abs( cnum1 )>abs( cden ) ) then
              ! pre-multiply x by bignum if cden is small compared to cnum.
              mul = bignum
              done = .false.
              cnum = cnum1
           else
              ! multiply x by cnum / cden and return.
              mul = cnum / cden
              done = .true.
           end if
           ! scale the vector x by mul
           call stdlib_csscal( n, mul, sx, incx )
           if( .not.done )go to 10
           return
     end subroutine stdlib_csrscl



     pure module subroutine stdlib_zdrscl( n, sa, sx, incx )
     !! ZDRSCL multiplies an n-element complex vector x by the real scalar
     !! 1/a.  This is done without overflow or underflow as long as
     !! the final result x/a does not overflow or underflow.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx, n
           real(dp), intent(in) :: sa
           ! Array Arguments 
           complex(dp), intent(inout) :: sx(*)
       ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: done
           real(dp) :: bignum, cden, cden1, cnum, cnum1, mul, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           ! quick return if possible
           if( n<=0 )return
           ! get machine parameters
           smlnum = stdlib_dlamch( 'S' )
           bignum = one / smlnum
           call stdlib_dlabad( smlnum, bignum )
           ! initialize the denominator to sa and the numerator to 1.
           cden = sa
           cnum = one
           10 continue
           cden1 = cden*smlnum
           cnum1 = cnum / bignum
           if( abs( cden1 )>abs( cnum ) .and. cnum/=zero ) then
              ! pre-multiply x by smlnum if cden is large compared to cnum.
              mul = smlnum
              done = .false.
              cden = cden1
           else if( abs( cnum1 )>abs( cden ) ) then
              ! pre-multiply x by bignum if cden is small compared to cnum.
              mul = bignum
              done = .false.
              cnum = cnum1
           else
              ! multiply x by cnum / cden and return.
              mul = cnum / cden
              done = .true.
           end if
           ! scale the vector x by mul
           call stdlib_zdscal( n, mul, sx, incx )
           if( .not.done )go to 10
           return
     end subroutine stdlib_zdrscl




     pure module subroutine stdlib_I64_clacgv( n, x, incx )
     !! CLACGV conjugates a complex vector of length N.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: incx, n
           ! Array Arguments 
           complex(sp), intent(inout) :: x(*)
       ! =====================================================================
           ! Local Scalars 
           integer(ilp64) :: i, ioff
           ! Intrinsic Functions 
           ! Executable Statements 
           if( incx==1_ilp64 ) then
              do i = 1, n
                 x( i ) = conjg( x( i ) )
              end do
           else
              ioff = 1_ilp64
              if( incx<0_ilp64 )ioff = 1_ilp64 - ( n-1 )*incx
              do i = 1, n
                 x( ioff ) = conjg( x( ioff ) )
                 ioff = ioff + incx
              end do
           end if
           return
     end subroutine stdlib_I64_clacgv

     pure module subroutine stdlib_I64_zlacgv( n, x, incx )
     !! ZLACGV conjugates a complex vector of length N.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: incx, n
           ! Array Arguments 
           complex(dp), intent(inout) :: x(*)
       ! =====================================================================
           ! Local Scalars 
           integer(ilp64) :: i, ioff
           ! Intrinsic Functions 
           ! Executable Statements 
           if( incx==1_ilp64 ) then
              do i = 1, n
                 x( i ) = conjg( x( i ) )
              end do
           else
              ioff = 1_ilp64
              if( incx<0_ilp64 )ioff = 1_ilp64 - ( n-1 )*incx
              do i = 1, n
                 x( ioff ) = conjg( x( ioff ) )
                 ioff = ioff + incx
              end do
           end if
           return
     end subroutine stdlib_I64_zlacgv




     pure module subroutine stdlib_I64_slasrt( id, n, d, info )
     !! Sort the numbers in D in increasing order (if ID = 'I') or
     !! in decreasing order (if ID = 'D' ).
     !! Use Quick Sort, reverting to Insertion sort on arrays of
     !! size <= 20. Dimension of STACK limits N to about 2**32.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: id
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: n
           ! Array Arguments 
           real(sp), intent(inout) :: d(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp64), parameter :: select = 20_ilp64
           
           ! Local Scalars 
           integer(ilp64) :: dir, endd, i, j, start, stkpnt
           real(sp) :: d1, d2, d3, dmnmx, tmp
           ! Local Arrays 
           integer(ilp64) :: stack(2_ilp64,32_ilp64)
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           dir = -1_ilp64
           if( stdlib_lsame( id, 'D' ) ) then
              dir = 0_ilp64
           else if( stdlib_lsame( id, 'I' ) ) then
              dir = 1_ilp64
           end if
           if( dir==-1_ilp64 ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -2_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'SLASRT', -info )
              return
           end if
           ! quick return if possible
           if( n<=1 )return
           stkpnt = 1_ilp64
           stack( 1_ilp64, 1_ilp64 ) = 1_ilp64
           stack( 2_ilp64, 1_ilp64 ) = n
           10 continue
           start = stack( 1_ilp64, stkpnt )
           endd = stack( 2_ilp64, stkpnt )
           stkpnt = stkpnt - 1_ilp64
           if( endd-start<=select .and. endd-start>0_ilp64 ) then
              ! do insertion sort on d( start:endd )
              if( dir==0_ilp64 ) then
                 ! sort into decreasing order
                 loop_30: do i = start + 1, endd
                    do j = i, start + 1, -1
                       if( d( j )>d( j-1 ) ) then
                          dmnmx = d( j )
                          d( j ) = d( j-1 )
                          d( j-1 ) = dmnmx
                       else
                          cycle loop_30
                       end if
                    end do
                 end do loop_30
              else
                 ! sort into increasing order
                 loop_50: do i = start + 1, endd
                    do j = i, start + 1, -1
                       if( d( j )<d( j-1 ) ) then
                          dmnmx = d( j )
                          d( j ) = d( j-1 )
                          d( j-1 ) = dmnmx
                       else
                          cycle loop_50
                       end if
                    end do
                 end do loop_50
              end if
           else if( endd-start>select ) then
              ! partition d( start:endd ) and stack parts, largest one first
              ! choose partition entry as median of 3
              d1 = d( start )
              d2 = d( endd )
              i = ( start+endd ) / 2_ilp64
              d3 = d( i )
              if( d1<d2 ) then
                 if( d3<d1 ) then
                    dmnmx = d1
                 else if( d3<d2 ) then
                    dmnmx = d3
                 else
                    dmnmx = d2
                 end if
              else
                 if( d3<d2 ) then
                    dmnmx = d2
                 else if( d3<d1 ) then
                    dmnmx = d3
                 else
                    dmnmx = d1
                 end if
              end if
              if( dir==0_ilp64 ) then
                 ! sort into decreasing order
                 i = start - 1_ilp64
                 j = endd + 1_ilp64
                 60 continue
                 70 continue
                 j = j - 1_ilp64
                 if( d( j )<dmnmx )go to 70
                 80 continue
                 i = i + 1_ilp64
                 if( d( i )>dmnmx )go to 80
                 if( i<j ) then
                    tmp = d( i )
                    d( i ) = d( j )
                    d( j ) = tmp
                    go to 60
                 end if
                 if( j-start>endd-j-1 ) then
                    stkpnt = stkpnt + 1_ilp64
                    stack( 1_ilp64, stkpnt ) = start
                    stack( 2_ilp64, stkpnt ) = j
                    stkpnt = stkpnt + 1_ilp64
                    stack( 1_ilp64, stkpnt ) = j + 1_ilp64
                    stack( 2_ilp64, stkpnt ) = endd
                 else
                    stkpnt = stkpnt + 1_ilp64
                    stack( 1_ilp64, stkpnt ) = j + 1_ilp64
                    stack( 2_ilp64, stkpnt ) = endd
                    stkpnt = stkpnt + 1_ilp64
                    stack( 1_ilp64, stkpnt ) = start
                    stack( 2_ilp64, stkpnt ) = j
                 end if
              else
                 ! sort into increasing order
                 i = start - 1_ilp64
                 j = endd + 1_ilp64
                 90 continue
                 100 continue
                 j = j - 1_ilp64
                 if( d( j )>dmnmx )go to 100
                 110 continue
                 i = i + 1_ilp64
                 if( d( i )<dmnmx )go to 110
                 if( i<j ) then
                    tmp = d( i )
                    d( i ) = d( j )
                    d( j ) = tmp
                    go to 90
                 end if
                 if( j-start>endd-j-1 ) then
                    stkpnt = stkpnt + 1_ilp64
                    stack( 1_ilp64, stkpnt ) = start
                    stack( 2_ilp64, stkpnt ) = j
                    stkpnt = stkpnt + 1_ilp64
                    stack( 1_ilp64, stkpnt ) = j + 1_ilp64
                    stack( 2_ilp64, stkpnt ) = endd
                 else
                    stkpnt = stkpnt + 1_ilp64
                    stack( 1_ilp64, stkpnt ) = j + 1_ilp64
                    stack( 2_ilp64, stkpnt ) = endd
                    stkpnt = stkpnt + 1_ilp64
                    stack( 1_ilp64, stkpnt ) = start
                    stack( 2_ilp64, stkpnt ) = j
                 end if
              end if
           end if
           if( stkpnt>0 )go to 10
           return
     end subroutine stdlib_I64_slasrt

     pure module subroutine stdlib_I64_dlasrt( id, n, d, info )
     !! Sort the numbers in D in increasing order (if ID = 'I') or
     !! in decreasing order (if ID = 'D' ).
     !! Use Quick Sort, reverting to Insertion sort on arrays of
     !! size <= 20. Dimension of STACK limits N to about 2**32.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: id
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: n
           ! Array Arguments 
           real(dp), intent(inout) :: d(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp64), parameter :: select = 20_ilp64
           
           ! Local Scalars 
           integer(ilp64) :: dir, endd, i, j, start, stkpnt
           real(dp) :: d1, d2, d3, dmnmx, tmp
           ! Local Arrays 
           integer(ilp64) :: stack(2_ilp64,32_ilp64)
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp64
           dir = -1_ilp64
           if( stdlib_lsame( id, 'D' ) ) then
              dir = 0_ilp64
           else if( stdlib_lsame( id, 'I' ) ) then
              dir = 1_ilp64
           end if
           if( dir==-1_ilp64 ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -2_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'DLASRT', -info )
              return
           end if
           ! quick return if possible
           if( n<=1 )return
           stkpnt = 1_ilp64
           stack( 1_ilp64, 1_ilp64 ) = 1_ilp64
           stack( 2_ilp64, 1_ilp64 ) = n
           10 continue
           start = stack( 1_ilp64, stkpnt )
           endd = stack( 2_ilp64, stkpnt )
           stkpnt = stkpnt - 1_ilp64
           if( endd-start<=select .and. endd-start>0_ilp64 ) then
              ! do insertion sort on d( start:endd )
              if( dir==0_ilp64 ) then
                 ! sort into decreasing order
                 loop_30: do i = start + 1, endd
                    do j = i, start + 1, -1
                       if( d( j )>d( j-1 ) ) then
                          dmnmx = d( j )
                          d( j ) = d( j-1 )
                          d( j-1 ) = dmnmx
                       else
                          cycle loop_30
                       end if
                    end do
                 end do loop_30
              else
                 ! sort into increasing order
                 loop_50: do i = start + 1, endd
                    do j = i, start + 1, -1
                       if( d( j )<d( j-1 ) ) then
                          dmnmx = d( j )
                          d( j ) = d( j-1 )
                          d( j-1 ) = dmnmx
                       else
                          cycle loop_50
                       end if
                    end do
                 end do loop_50
              end if
           else if( endd-start>select ) then
              ! partition d( start:endd ) and stack parts, largest one first
              ! choose partition entry as median of 3
              d1 = d( start )
              d2 = d( endd )
              i = ( start+endd ) / 2_ilp64
              d3 = d( i )
              if( d1<d2 ) then
                 if( d3<d1 ) then
                    dmnmx = d1
                 else if( d3<d2 ) then
                    dmnmx = d3
                 else
                    dmnmx = d2
                 end if
              else
                 if( d3<d2 ) then
                    dmnmx = d2
                 else if( d3<d1 ) then
                    dmnmx = d3
                 else
                    dmnmx = d1
                 end if
              end if
              if( dir==0_ilp64 ) then
                 ! sort into decreasing order
                 i = start - 1_ilp64
                 j = endd + 1_ilp64
                 60 continue
                 70 continue
                 j = j - 1_ilp64
                 if( d( j )<dmnmx )go to 70
                 80 continue
                 i = i + 1_ilp64
                 if( d( i )>dmnmx )go to 80
                 if( i<j ) then
                    tmp = d( i )
                    d( i ) = d( j )
                    d( j ) = tmp
                    go to 60
                 end if
                 if( j-start>endd-j-1 ) then
                    stkpnt = stkpnt + 1_ilp64
                    stack( 1_ilp64, stkpnt ) = start
                    stack( 2_ilp64, stkpnt ) = j
                    stkpnt = stkpnt + 1_ilp64
                    stack( 1_ilp64, stkpnt ) = j + 1_ilp64
                    stack( 2_ilp64, stkpnt ) = endd
                 else
                    stkpnt = stkpnt + 1_ilp64
                    stack( 1_ilp64, stkpnt ) = j + 1_ilp64
                    stack( 2_ilp64, stkpnt ) = endd
                    stkpnt = stkpnt + 1_ilp64
                    stack( 1_ilp64, stkpnt ) = start
                    stack( 2_ilp64, stkpnt ) = j
                 end if
              else
                 ! sort into increasing order
                 i = start - 1_ilp64
                 j = endd + 1_ilp64
                 90 continue
                 100 continue
                 j = j - 1_ilp64
                 if( d( j )>dmnmx )go to 100
                 110 continue
                 i = i + 1_ilp64
                 if( d( i )<dmnmx )go to 110
                 if( i<j ) then
                    tmp = d( i )
                    d( i ) = d( j )
                    d( j ) = tmp
                    go to 90
                 end if
                 if( j-start>endd-j-1 ) then
                    stkpnt = stkpnt + 1_ilp64
                    stack( 1_ilp64, stkpnt ) = start
                    stack( 2_ilp64, stkpnt ) = j
                    stkpnt = stkpnt + 1_ilp64
                    stack( 1_ilp64, stkpnt ) = j + 1_ilp64
                    stack( 2_ilp64, stkpnt ) = endd
                 else
                    stkpnt = stkpnt + 1_ilp64
                    stack( 1_ilp64, stkpnt ) = j + 1_ilp64
                    stack( 2_ilp64, stkpnt ) = endd
                    stkpnt = stkpnt + 1_ilp64
                    stack( 1_ilp64, stkpnt ) = start
                    stack( 2_ilp64, stkpnt ) = j
                 end if
              end if
           end if
           if( stkpnt>0 )go to 10
           return
     end subroutine stdlib_I64_dlasrt




     pure module subroutine stdlib_I64_slassq( n, x, incx, scl, sumsq )
     !! SLASSQ returns the values  scl  and  smsq  such that
     !! ( scl**2 )*smsq = x( 1 )**2 +...+ x( n )**2 + ( scale**2 )*sumsq,
     !! where  x( i ) = X( 1 + ( i - 1 )*INCX ). The value of  sumsq  is
     !! assumed to be non-negative.
     !! scale and sumsq must be supplied in SCALE and SUMSQ and
     !! scl and smsq are overwritten on SCALE and SUMSQ respectively.
     !! If scale * sqrt( sumsq ) > tbig then
     !! we require:   scale >= sqrt( TINY*EPS ) / sbig   on entry,
     !! and if 0 < scale * sqrt( sumsq ) < tsml then
     !! we require:   scale <= sqrt( HUGE ) / ssml       on entry,
     !! where
     !! tbig -- upper threshold for values whose square is representable;
     !! sbig -- scaling constant for big numbers; \see la_constants.f90
     !! tsml -- lower threshold for values whose square is representable;
     !! ssml -- scaling constant for small numbers; \see la_constants.f90
     !! and
     !! TINY*EPS -- tiniest representable number;
     !! HUGE     -- biggest representable number.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: zero, half, one, two, tbig, tsml, ssml, sbig
        ! Scalar Arguments 
     integer(ilp64), intent(in) :: incx, n
        real(sp), intent(inout) :: scl, sumsq
        ! Array Arguments 
        real(sp), intent(in) :: x(*)
        ! =====================================================================
        ! Local Scalars
     integer(ilp64) :: i, ix
     logical(lk) :: notbig
        real(sp) :: abig, amed, asml, ax, ymax, ymin
        ! quick return if possible
        if( ieee_is_nan(scl) .or. ieee_is_nan(sumsq) ) return
        if( sumsq == zero ) scl = one
        if( scl == zero ) then
           scl = one
           sumsq = zero
        end if
        if (n <= 0_ilp64) then
           return
        end if
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
        ix = 1_ilp64
        if( incx < 0_ilp64 ) ix = 1_ilp64 - (n-1)*incx
        do i = 1, n
           ax = abs(x(ix))
           if (ax > tbig) then
              abig = abig + (ax*sbig)**2_ilp64
              notbig = .false.
           else if (ax < tsml) then
              if (notbig) asml = asml + (ax*ssml)**2_ilp64
           else
              amed = amed + ax**2_ilp64
           end if
           ix = ix + incx
        end do
        ! put the existing sum of squares into one of the accumulators
        if( sumsq > zero ) then
           ax = scl*sqrt( sumsq )
           if (ax > tbig) then
              ! we assume scl >= sqrt( tiny*eps ) / sbig
              abig = abig + (scl*sbig)**2_ilp64 * sumsq
           else if (ax < tsml) then
              ! we assume scl <= sqrt( huge ) / ssml
              if (notbig) asml = asml + (scl*ssml)**2_ilp64 * sumsq
           else
              amed = amed + scl**2_ilp64 * sumsq
           end if
        end if
        ! combine abig and amed or amed and asml if more than one
        ! accumulator was used.
        if (abig > zero) then
           ! combine abig and amed if abig > 0.
           if (amed > zero .or. ieee_is_nan(amed)) then
              abig = abig + (amed*sbig)*sbig
           end if
           scl = one / sbig
           sumsq = abig
        else if (asml > zero) then
           ! combine amed and asml if asml > 0.
           if (amed > zero .or. ieee_is_nan(amed)) then
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
              sumsq = ymax**2_ilp64*( one + (ymin/ymax)**2_ilp64 )
           else
              scl = one / ssml
              sumsq = asml
           end if
        else
           ! otherwise all values are mid-range or zero
           scl = one
           sumsq = amed
        end if
        return
     end subroutine stdlib_I64_slassq

     pure module subroutine stdlib_I64_dlassq( n, x, incx, scl, sumsq )
     !! DLASSQ returns the values  scl  and  smsq  such that
     !! ( scl**2 )*smsq = x( 1 )**2 +...+ x( n )**2 + ( scale**2 )*sumsq,
     !! where  x( i ) = X( 1 + ( i - 1 )*INCX ). The value of  sumsq  is
     !! assumed to be non-negative.
     !! scale and sumsq must be supplied in SCALE and SUMSQ and
     !! scl and smsq are overwritten on SCALE and SUMSQ respectively.
     !! If scale * sqrt( sumsq ) > tbig then
     !! we require:   scale >= sqrt( TINY*EPS ) / sbig   on entry,
     !! and if 0 < scale * sqrt( sumsq ) < tsml then
     !! we require:   scale <= sqrt( HUGE ) / ssml       on entry,
     !! where
     !! tbig -- upper threshold for values whose square is representable;
     !! sbig -- scaling constant for big numbers; \see la_constants.f90
     !! tsml -- lower threshold for values whose square is representable;
     !! ssml -- scaling constant for small numbers; \see la_constants.f90
     !! and
     !! TINY*EPS -- tiniest representable number;
     !! HUGE     -- biggest representable number.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: zero, half, one, two, tbig, tsml, ssml, sbig
        ! Scalar Arguments 
     integer(ilp64), intent(in) :: incx, n
        real(dp), intent(inout) :: scl, sumsq
        ! Array Arguments 
        real(dp), intent(in) :: x(*)
        ! =====================================================================
        ! Local Scalars 
     integer(ilp64) :: i, ix
     logical(lk) :: notbig
        real(dp) :: abig, amed, asml, ax, ymax, ymin
        ! quick return if possible
        if( ieee_is_nan(scl) .or. ieee_is_nan(sumsq) ) return
        if( sumsq == zero ) scl = one
        if( scl == zero ) then
           scl = one
           sumsq = zero
        end if
        if (n <= 0_ilp64) then
           return
        end if
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
        ix = 1_ilp64
        if( incx < 0_ilp64 ) ix = 1_ilp64 - (n-1)*incx
        do i = 1, n
           ax = abs(x(ix))
           if (ax > tbig) then
              abig = abig + (ax*sbig)**2_ilp64
              notbig = .false.
           else if (ax < tsml) then
              if (notbig) asml = asml + (ax*ssml)**2_ilp64
           else
              amed = amed + ax**2_ilp64
           end if
           ix = ix + incx
        end do
        ! put the existing sum of squares into one of the accumulators
        if( sumsq > zero ) then
           ax = scl*sqrt( sumsq )
           if (ax > tbig) then
              ! we assume scl >= sqrt( tiny*eps ) / sbig
              abig = abig + (scl*sbig)**2_ilp64 * sumsq
           else if (ax < tsml) then
              ! we assume scl <= sqrt( huge ) / ssml
              if (notbig) asml = asml + (scl*ssml)**2_ilp64 * sumsq
           else
              amed = amed + scl**2_ilp64 * sumsq
           end if
        end if
        ! combine abig and amed or amed and asml if more than one
        ! accumulator was used.
        if (abig > zero) then
           ! combine abig and amed if abig > 0.
           if (amed > zero .or. ieee_is_nan(amed)) then
              abig = abig + (amed*sbig)*sbig
           end if
           scl = one / sbig
           sumsq = abig
        else if (asml > zero) then
           ! combine amed and asml if asml > 0.
           if (amed > zero .or. ieee_is_nan(amed)) then
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
              sumsq = ymax**2_ilp64*( one + (ymin/ymax)**2_ilp64 )
           else
              scl = one / ssml
              sumsq = asml
           end if
        else
           ! otherwise all values are mid-range or zero
           scl = one
           sumsq = amed
        end if
        return
     end subroutine stdlib_I64_dlassq


     pure module subroutine stdlib_I64_classq( n, x, incx, scl, sumsq )
     !! CLASSQ returns the values  scl  and  smsq  such that
     !! ( scl**2 )*smsq = x( 1 )**2 +...+ x( n )**2 + ( scale**2 )*sumsq,
     !! where  x( i ) = X( 1 + ( i - 1 )*INCX ). The value of  sumsq  is
     !! assumed to be non-negative.
     !! scale and sumsq must be supplied in SCALE and SUMSQ and
     !! scl and smsq are overwritten on SCALE and SUMSQ respectively.
     !! If scale * sqrt( sumsq ) > tbig then
     !! we require:   scale >= sqrt( TINY*EPS ) / sbig   on entry,
     !! and if 0 < scale * sqrt( sumsq ) < tsml then
     !! we require:   scale <= sqrt( HUGE ) / ssml       on entry,
     !! where
     !! tbig -- upper threshold for values whose square is representable;
     !! sbig -- scaling constant for big numbers; \see la_constants.f90
     !! tsml -- lower threshold for values whose square is representable;
     !! ssml -- scaling constant for small numbers; \see la_constants.f90
     !! and
     !! TINY*EPS -- tiniest representable number;
     !! HUGE     -- biggest representable number.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: zero, half, one, two, tbig, tsml, ssml, sbig
        ! Scalar Arguments 
     integer(ilp64), intent(in) :: incx, n
        real(sp), intent(inout) :: scl, sumsq
        ! Array Arguments 
        complex(sp), intent(in) :: x(*)
        ! =====================================================================
        ! Local Scalars 
     integer(ilp64) :: i, ix
     logical(lk) :: notbig
        real(sp) :: abig, amed, asml, ax, ymax, ymin
        ! quick return if possible
        if( ieee_is_nan(scl) .or. ieee_is_nan(sumsq) ) return
        if( sumsq == zero ) scl = one
        if( scl == zero ) then
           scl = one
           sumsq = zero
        end if
        if (n <= 0_ilp64) then
           return
        end if
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
        ix = 1_ilp64
        if( incx < 0_ilp64 ) ix = 1_ilp64 - (n-1)*incx
        do i = 1, n
           ax = abs(real(x(ix),KIND=sp))
           if (ax > tbig) then
              abig = abig + (ax*sbig)**2_ilp64
              notbig = .false.
           else if (ax < tsml) then
              if (notbig) asml = asml + (ax*ssml)**2_ilp64
           else
              amed = amed + ax**2_ilp64
           end if
           ax = abs(aimag(x(ix)))
           if (ax > tbig) then
              abig = abig + (ax*sbig)**2_ilp64
              notbig = .false.
           else if (ax < tsml) then
              if (notbig) asml = asml + (ax*ssml)**2_ilp64
           else
              amed = amed + ax**2_ilp64
           end if
           ix = ix + incx
        end do
        ! put the existing sum of squares into one of the accumulators
        if( sumsq > zero ) then
           ax = scl*sqrt( sumsq )
           if (ax > tbig) then
              ! we assume scl >= sqrt( tiny*eps ) / sbig
              abig = abig + (scl*sbig)**2_ilp64 * sumsq
           else if (ax < tsml) then
              ! we assume scl <= sqrt( huge ) / ssml
              if (notbig) asml = asml + (scl*ssml)**2_ilp64 * sumsq
           else
              amed = amed + scl**2_ilp64 * sumsq
           end if
        end if
        ! combine abig and amed or amed and asml if more than one
        ! accumulator was used.
        if (abig > zero) then
           ! combine abig and amed if abig > 0.
           if (amed > zero .or. ieee_is_nan(amed)) then
              abig = abig + (amed*sbig)*sbig
           end if
           scl = one / sbig
           sumsq = abig
        else if (asml > zero) then
           ! combine amed and asml if asml > 0.
           if (amed > zero .or. ieee_is_nan(amed)) then
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
              sumsq = ymax**2_ilp64*( one + (ymin/ymax)**2_ilp64 )
           else
              scl = one / ssml
              sumsq = asml
           end if
        else
           ! otherwise all values are mid-range or zero
           scl = one
           sumsq = amed
        end if
        return
     end subroutine stdlib_I64_classq

     pure module subroutine stdlib_I64_zlassq( n, x, incx, scl, sumsq )
     !! ZLASSQ returns the values  scl  and  smsq  such that
     !! ( scl**2 )*smsq = x( 1 )**2 +...+ x( n )**2 + ( scale**2 )*sumsq,
     !! where  x( i ) = X( 1 + ( i - 1 )*INCX ). The value of  sumsq  is
     !! assumed to be non-negative.
     !! scale and sumsq must be supplied in SCALE and SUMSQ and
     !! scl and smsq are overwritten on SCALE and SUMSQ respectively.
     !! If scale * sqrt( sumsq ) > tbig then
     !! we require:   scale >= sqrt( TINY*EPS ) / sbig   on entry,
     !! and if 0 < scale * sqrt( sumsq ) < tsml then
     !! we require:   scale <= sqrt( HUGE ) / ssml       on entry,
     !! where
     !! tbig -- upper threshold for values whose square is representable;
     !! sbig -- scaling constant for big numbers; \see la_constants.f90
     !! tsml -- lower threshold for values whose square is representable;
     !! ssml -- scaling constant for small numbers; \see la_constants.f90
     !! and
     !! TINY*EPS -- tiniest representable number;
     !! HUGE     -- biggest representable number.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: zero, half, one, two, tbig, tsml, ssml, sbig
        ! Scalar Arguments 
     integer(ilp64), intent(in) :: incx, n
        real(dp), intent(inout) :: scl, sumsq
        ! Array Arguments 
        complex(dp), intent(in) :: x(*)
        ! =====================================================================
        ! Local Scalars 
     integer(ilp64) :: i, ix
     logical(lk) :: notbig
        real(dp) :: abig, amed, asml, ax, ymax, ymin
        ! quick return if possible
        if( ieee_is_nan(scl) .or. ieee_is_nan(sumsq) ) return
        if( sumsq == zero ) scl = one
        if( scl == zero ) then
           scl = one
           sumsq = zero
        end if
        if (n <= 0_ilp64) then
           return
        end if
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
        ix = 1_ilp64
        if( incx < 0_ilp64 ) ix = 1_ilp64 - (n-1)*incx
        do i = 1, n
           ax = abs(real(x(ix),KIND=dp))
           if (ax > tbig) then
              abig = abig + (ax*sbig)**2_ilp64
              notbig = .false.
           else if (ax < tsml) then
              if (notbig) asml = asml + (ax*ssml)**2_ilp64
           else
              amed = amed + ax**2_ilp64
           end if
           ax = abs(aimag(x(ix)))
           if (ax > tbig) then
              abig = abig + (ax*sbig)**2_ilp64
              notbig = .false.
           else if (ax < tsml) then
              if (notbig) asml = asml + (ax*ssml)**2_ilp64
           else
              amed = amed + ax**2_ilp64
           end if
           ix = ix + incx
        end do
        ! put the existing sum of squares into one of the accumulators
        if( sumsq > zero ) then
           ax = scl*sqrt( sumsq )
           if (ax > tbig) then
              ! we assume scl >= sqrt( tiny*eps ) / sbig
              abig = abig + (scl*sbig)**2_ilp64 * sumsq
           else if (ax < tsml) then
              ! we assume scl <= sqrt( huge ) / ssml
              if (notbig) asml = asml + (scl*ssml)**2_ilp64 * sumsq
           else
              amed = amed + scl**2_ilp64 * sumsq
           end if
        end if
        ! combine abig and amed or amed and asml if more than one
        ! accumulator was used.
        if (abig > zero) then
           ! combine abig and amed if abig > 0.
           if (amed > zero .or. ieee_is_nan(amed)) then
              abig = abig + (amed*sbig)*sbig
           end if
           scl = one / sbig
           sumsq = abig
        else if (asml > zero) then
           ! combine amed and asml if asml > 0.
           if (amed > zero .or. ieee_is_nan(amed)) then
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
              sumsq = ymax**2_ilp64*( one + (ymin/ymax)**2_ilp64 )
           else
              scl = one / ssml
              sumsq = asml
           end if
        else
           ! otherwise all values are mid-range or zero
           scl = one
           sumsq = amed
        end if
        return
     end subroutine stdlib_I64_zlassq




     pure module subroutine stdlib_I64_srscl( n, sa, sx, incx )
     !! SRSCL multiplies an n-element real vector x by the real scalar 1/a.
     !! This is done without overflow or underflow as long as
     !! the final result x/a does not overflow or underflow.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: incx, n
           real(sp), intent(in) :: sa
           ! Array Arguments 
           real(sp), intent(inout) :: sx(*)
       ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: done
           real(sp) :: bignum, cden, cden1, cnum, cnum1, mul, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           ! quick return if possible
           if( n<=0 )return
           ! get machine parameters
           smlnum = stdlib_I64_slamch( 'S' )
           bignum = one / smlnum
           call stdlib_I64_slabad( smlnum, bignum )
           ! initialize the denominator to sa and the numerator to 1.
           cden = sa
           cnum = one
           10 continue
           cden1 = cden*smlnum
           cnum1 = cnum / bignum
           if( abs( cden1 )>abs( cnum ) .and. cnum/=zero ) then
              ! pre-multiply x by smlnum if cden is large compared to cnum.
              mul = smlnum
              done = .false.
              cden = cden1
           else if( abs( cnum1 )>abs( cden ) ) then
              ! pre-multiply x by bignum if cden is small compared to cnum.
              mul = bignum
              done = .false.
              cnum = cnum1
           else
              ! multiply x by cnum / cden and return.
              mul = cnum / cden
              done = .true.
           end if
           ! scale the vector x by mul
           call stdlib_I64_sscal( n, mul, sx, incx )
           if( .not.done )go to 10
           return
     end subroutine stdlib_I64_srscl

     pure module subroutine stdlib_I64_drscl( n, sa, sx, incx )
     !! DRSCL multiplies an n-element real vector x by the real scalar 1/a.
     !! This is done without overflow or underflow as long as
     !! the final result x/a does not overflow or underflow.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: incx, n
           real(dp), intent(in) :: sa
           ! Array Arguments 
           real(dp), intent(inout) :: sx(*)
       ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: done
           real(dp) :: bignum, cden, cden1, cnum, cnum1, mul, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           ! quick return if possible
           if( n<=0 )return
           ! get machine parameters
           smlnum = stdlib_I64_dlamch( 'S' )
           bignum = one / smlnum
           call stdlib_I64_dlabad( smlnum, bignum )
           ! initialize the denominator to sa and the numerator to 1.
           cden = sa
           cnum = one
           10 continue
           cden1 = cden*smlnum
           cnum1 = cnum / bignum
           if( abs( cden1 )>abs( cnum ) .and. cnum/=zero ) then
              ! pre-multiply x by smlnum if cden is large compared to cnum.
              mul = smlnum
              done = .false.
              cden = cden1
           else if( abs( cnum1 )>abs( cden ) ) then
              ! pre-multiply x by bignum if cden is small compared to cnum.
              mul = bignum
              done = .false.
              cnum = cnum1
           else
              ! multiply x by cnum / cden and return.
              mul = cnum / cden
              done = .true.
           end if
           ! scale the vector x by mul
           call stdlib_I64_dscal( n, mul, sx, incx )
           if( .not.done )go to 10
           return
     end subroutine stdlib_I64_drscl




     pure module subroutine stdlib_I64_csrscl( n, sa, sx, incx )
     !! CSRSCL multiplies an n-element complex vector x by the real scalar
     !! 1/a.  This is done without overflow or underflow as long as
     !! the final result x/a does not overflow or underflow.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: incx, n
           real(sp), intent(in) :: sa
           ! Array Arguments 
           complex(sp), intent(inout) :: sx(*)
       ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: done
           real(sp) :: bignum, cden, cden1, cnum, cnum1, mul, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           ! quick return if possible
           if( n<=0 )return
           ! get machine parameters
           smlnum = stdlib_I64_slamch( 'S' )
           bignum = one / smlnum
           call stdlib_I64_slabad( smlnum, bignum )
           ! initialize the denominator to sa and the numerator to 1.
           cden = sa
           cnum = one
           10 continue
           cden1 = cden*smlnum
           cnum1 = cnum / bignum
           if( abs( cden1 )>abs( cnum ) .and. cnum/=zero ) then
              ! pre-multiply x by smlnum if cden is large compared to cnum.
              mul = smlnum
              done = .false.
              cden = cden1
           else if( abs( cnum1 )>abs( cden ) ) then
              ! pre-multiply x by bignum if cden is small compared to cnum.
              mul = bignum
              done = .false.
              cnum = cnum1
           else
              ! multiply x by cnum / cden and return.
              mul = cnum / cden
              done = .true.
           end if
           ! scale the vector x by mul
           call stdlib_I64_csscal( n, mul, sx, incx )
           if( .not.done )go to 10
           return
     end subroutine stdlib_I64_csrscl



     pure module subroutine stdlib_I64_zdrscl( n, sa, sx, incx )
     !! ZDRSCL multiplies an n-element complex vector x by the real scalar
     !! 1/a.  This is done without overflow or underflow as long as
     !! the final result x/a does not overflow or underflow.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp64), intent(in) :: incx, n
           real(dp), intent(in) :: sa
           ! Array Arguments 
           complex(dp), intent(inout) :: sx(*)
       ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: done
           real(dp) :: bignum, cden, cden1, cnum, cnum1, mul, smlnum
           ! Intrinsic Functions 
           ! Executable Statements 
           ! quick return if possible
           if( n<=0 )return
           ! get machine parameters
           smlnum = stdlib_I64_dlamch( 'S' )
           bignum = one / smlnum
           call stdlib_I64_dlabad( smlnum, bignum )
           ! initialize the denominator to sa and the numerator to 1.
           cden = sa
           cnum = one
           10 continue
           cden1 = cden*smlnum
           cnum1 = cnum / bignum
           if( abs( cden1 )>abs( cnum ) .and. cnum/=zero ) then
              ! pre-multiply x by smlnum if cden is large compared to cnum.
              mul = smlnum
              done = .false.
              cden = cden1
           else if( abs( cnum1 )>abs( cden ) ) then
              ! pre-multiply x by bignum if cden is small compared to cnum.
              mul = bignum
              done = .false.
              cnum = cnum1
           else
              ! multiply x by cnum / cden and return.
              mul = cnum / cden
              done = .true.
           end if
           ! scale the vector x by mul
           call stdlib_I64_zdscal( n, mul, sx, incx )
           if( .not.done )go to 10
           return
     end subroutine stdlib_I64_zdrscl



end submodule stdlib_lapack_blas_like_l1
