submodule(stdlib_lapack_eig_svd_lsq) stdlib_lapack_eigv_comp
  implicit none


  contains

     pure module subroutine stdlib_sggbal( job, n, a, lda, b, ldb, ilo, ihi, lscale,rscale, work, info )
     !! SGGBAL balances a pair of general real matrices (A,B).  This
     !! involves, first, permuting A and B by similarity transformations to
     !! isolate eigenvalues in the first 1 to ILO$-$1 and last IHI+1 to N
     !! elements on the diagonal; and second, applying a diagonal similarity
     !! transformation to rows and columns ILO to IHI to make the rows
     !! and columns as close in norm as possible. Both steps are optional.
     !! Balancing may reduce the 1-norm of the matrices, and improve the
     !! accuracy of the computed eigenvalues and/or eigenvectors in the
     !! generalized eigenvalue problem A*x = lambda*B*x.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: job
           integer(ilp), intent(out) :: ihi, ilo, info
           integer(ilp), intent(in) :: lda, ldb, n
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*), b(ldb,*)
           real(sp), intent(out) :: lscale(*), rscale(*), work(*)
        ! =====================================================================
           ! Parameters 
           real(sp), parameter :: sclfac = ten
           
           
           ! Local Scalars 
           integer(ilp) :: i, icab, iflow, ip1, ir, irab, it, j, jc, jp1, k, kount, l, lcab, lm1, &
                     lrab, lsfmax, lsfmin, m, nr, nrp2
           real(sp) :: alpha, basl, beta, cab, cmax, coef, coef2, coef5, cor, ew, ewc, gamma, &
                     pgamma, rab, sfmax, sfmin, sum, t, ta, tb, tc
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp
           if( .not.stdlib_lsame( job, 'N' ) .and. .not.stdlib_lsame( job, 'P' ) &
                     .and..not.stdlib_lsame( job, 'S' ) .and. .not.stdlib_lsame( job, 'B' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SGGBAL', -info )
              return
           end if
           ! quick return if possible
           if( n==0_ilp ) then
              ilo = 1_ilp
              ihi = n
              return
           end if
           if( n==1_ilp ) then
              ilo = 1_ilp
              ihi = n
              lscale( 1_ilp ) = one
              rscale( 1_ilp ) = one
              return
           end if
           if( stdlib_lsame( job, 'N' ) ) then
              ilo = 1_ilp
              ihi = n
              do i = 1, n
                 lscale( i ) = one
                 rscale( i ) = one
              end do
              return
           end if
           k = 1_ilp
           l = n
           if( stdlib_lsame( job, 'S' ) )go to 190
           go to 30
           ! permute the matrices a and b to isolate the eigenvalues.
           ! find row with one nonzero in columns 1 through l
           20 continue
           l = lm1
           if( l/=1 )go to 30
           rscale( 1_ilp ) = one
           lscale( 1_ilp ) = one
           go to 190
           30 continue
           lm1 = l - 1_ilp
           loop_80: do i = l, 1, -1
              do j = 1, lm1
                 jp1 = j + 1_ilp
                 if( a( i, j )/=zero .or. b( i, j )/=zero )go to 50
              end do
              j = l
              go to 70
              50 continue
              do j = jp1, l
                 if( a( i, j )/=zero .or. b( i, j )/=zero )cycle loop_80
              end do
              j = jp1 - 1_ilp
              70 continue
              m = l
              iflow = 1_ilp
              go to 160
           end do loop_80
           go to 100
           ! find column with one nonzero in rows k through n
           90 continue
           k = k + 1_ilp
           100 continue
           loop_150: do j = k, l
              do i = k, lm1
                 ip1 = i + 1_ilp
                 if( a( i, j )/=zero .or. b( i, j )/=zero )go to 120
              end do
              i = l
              go to 140
              120 continue
              do i = ip1, l
                 if( a( i, j )/=zero .or. b( i, j )/=zero )cycle loop_150
              end do
              i = ip1 - 1_ilp
              140 continue
              m = k
              iflow = 2_ilp
              go to 160
           end do loop_150
           go to 190
           ! permute rows m and i
           160 continue
           lscale( m ) = i
           if( i==m )go to 170
           call stdlib_sswap( n-k+1, a( i, k ), lda, a( m, k ), lda )
           call stdlib_sswap( n-k+1, b( i, k ), ldb, b( m, k ), ldb )
           ! permute columns m and j
           170 continue
           rscale( m ) = j
           if( j==m )go to 180
           call stdlib_sswap( l, a( 1_ilp, j ), 1_ilp, a( 1_ilp, m ), 1_ilp )
           call stdlib_sswap( l, b( 1_ilp, j ), 1_ilp, b( 1_ilp, m ), 1_ilp )
           180 continue
           go to ( 20, 90 )iflow
           190 continue
           ilo = k
           ihi = l
           if( stdlib_lsame( job, 'P' ) ) then
              do i = ilo, ihi
                 lscale( i ) = one
                 rscale( i ) = one
              end do
              return
           end if
           if( ilo==ihi )return
           ! balance the submatrix in rows ilo to ihi.
           nr = ihi - ilo + 1_ilp
           do i = ilo, ihi
              rscale( i ) = zero
              lscale( i ) = zero
              work( i ) = zero
              work( i+n ) = zero
              work( i+2*n ) = zero
              work( i+3*n ) = zero
              work( i+4*n ) = zero
              work( i+5*n ) = zero
           end do
           ! compute right side vector in resulting linear equations
           basl = log10( sclfac )
           do i = ilo, ihi
              do j = ilo, ihi
                 tb = b( i, j )
                 ta = a( i, j )
                 if( ta==zero )go to 210
                 ta = log10( abs( ta ) ) / basl
                 210 continue
                 if( tb==zero )go to 220
                 tb = log10( abs( tb ) ) / basl
                 220 continue
                 work( i+4*n ) = work( i+4*n ) - ta - tb
                 work( j+5*n ) = work( j+5*n ) - ta - tb
              end do
           end do
           coef = one / real( 2_ilp*nr,KIND=sp)
           coef2 = coef*coef
           coef5 = half*coef2
           nrp2 = nr + 2_ilp
           beta = zero
           it = 1_ilp
           ! start generalized conjugate gradient iteration
           250 continue
           gamma = stdlib_sdot( nr, work( ilo+4*n ), 1_ilp, work( ilo+4*n ), 1_ilp ) +stdlib_sdot( nr, &
                     work( ilo+5*n ), 1_ilp, work( ilo+5*n ), 1_ilp )
           ew = zero
           ewc = zero
           do i = ilo, ihi
              ew = ew + work( i+4*n )
              ewc = ewc + work( i+5*n )
           end do
           gamma = coef*gamma - coef2*( ew**2_ilp+ewc**2_ilp ) - coef5*( ew-ewc )**2_ilp
           if( gamma==zero )go to 350
           if( it/=1_ilp )beta = gamma / pgamma
           t = coef5*( ewc-three*ew )
           tc = coef5*( ew-three*ewc )
           call stdlib_sscal( nr, beta, work( ilo ), 1_ilp )
           call stdlib_sscal( nr, beta, work( ilo+n ), 1_ilp )
           call stdlib_saxpy( nr, coef, work( ilo+4*n ), 1_ilp, work( ilo+n ), 1_ilp )
           call stdlib_saxpy( nr, coef, work( ilo+5*n ), 1_ilp, work( ilo ), 1_ilp )
           do i = ilo, ihi
              work( i ) = work( i ) + tc
              work( i+n ) = work( i+n ) + t
           end do
           ! apply matrix to vector
           do i = ilo, ihi
              kount = 0_ilp
              sum = zero
              loop_290: do j = ilo, ihi
                 if( a( i, j )==zero )go to 280
                 kount = kount + 1_ilp
                 sum = sum + work( j )
                 280 continue
                 if( b( i, j )==zero )cycle loop_290
                 kount = kount + 1_ilp
                 sum = sum + work( j )
              end do loop_290
              work( i+2*n ) = real( kount,KIND=sp)*work( i+n ) + sum
           end do
           do j = ilo, ihi
              kount = 0_ilp
              sum = zero
              loop_320: do i = ilo, ihi
                 if( a( i, j )==zero )go to 310
                 kount = kount + 1_ilp
                 sum = sum + work( i+n )
                 310 continue
                 if( b( i, j )==zero )cycle loop_320
                 kount = kount + 1_ilp
                 sum = sum + work( i+n )
              end do loop_320
              work( j+3*n ) = real( kount,KIND=sp)*work( j ) + sum
           end do
           sum = stdlib_sdot( nr, work( ilo+n ), 1_ilp, work( ilo+2*n ), 1_ilp ) +stdlib_sdot( nr, work( &
                     ilo ), 1_ilp, work( ilo+3*n ), 1_ilp )
           alpha = gamma / sum
           ! determine correction to current iteration
           cmax = zero
           do i = ilo, ihi
              cor = alpha*work( i+n )
              if( abs( cor )>cmax )cmax = abs( cor )
              lscale( i ) = lscale( i ) + cor
              cor = alpha*work( i )
              if( abs( cor )>cmax )cmax = abs( cor )
              rscale( i ) = rscale( i ) + cor
           end do
           if( cmax<half )go to 350
           call stdlib_saxpy( nr, -alpha, work( ilo+2*n ), 1_ilp, work( ilo+4*n ), 1_ilp )
           call stdlib_saxpy( nr, -alpha, work( ilo+3*n ), 1_ilp, work( ilo+5*n ), 1_ilp )
           pgamma = gamma
           it = it + 1_ilp
           if( it<=nrp2 )go to 250
           ! end generalized conjugate gradient iteration
           350 continue
           sfmin = stdlib_slamch( 'S' )
           sfmax = one / sfmin
           lsfmin = int( log10( sfmin ) / basl+one,KIND=ilp)
           lsfmax = int( log10( sfmax ) / basl,KIND=ilp)
           do i = ilo, ihi
              irab = stdlib_isamax( n-ilo+1, a( i, ilo ), lda )
              rab = abs( a( i, irab+ilo-1 ) )
              irab = stdlib_isamax( n-ilo+1, b( i, ilo ), ldb )
              rab = max( rab, abs( b( i, irab+ilo-1 ) ) )
              lrab = int( log10( rab+sfmin ) / basl+one,KIND=ilp)
              ir = lscale( i ) + sign( half, lscale( i ) )
              ir = min( max( ir, lsfmin ), lsfmax, lsfmax-lrab )
              lscale( i ) = sclfac**ir
              icab = stdlib_isamax( ihi, a( 1_ilp, i ), 1_ilp )
              cab = abs( a( icab, i ) )
              icab = stdlib_isamax( ihi, b( 1_ilp, i ), 1_ilp )
              cab = max( cab, abs( b( icab, i ) ) )
              lcab = int( log10( cab+sfmin ) / basl+one,KIND=ilp)
              jc = rscale( i ) + sign( half, rscale( i ) )
              jc = min( max( jc, lsfmin ), lsfmax, lsfmax-lcab )
              rscale( i ) = sclfac**jc
           end do
           ! row scaling of matrices a and b
           do i = ilo, ihi
              call stdlib_sscal( n-ilo+1, lscale( i ), a( i, ilo ), lda )
              call stdlib_sscal( n-ilo+1, lscale( i ), b( i, ilo ), ldb )
           end do
           ! column scaling of matrices a and b
           do j = ilo, ihi
              call stdlib_sscal( ihi, rscale( j ), a( 1_ilp, j ), 1_ilp )
              call stdlib_sscal( ihi, rscale( j ), b( 1_ilp, j ), 1_ilp )
           end do
           return
     end subroutine stdlib_sggbal

     pure module subroutine stdlib_dggbal( job, n, a, lda, b, ldb, ilo, ihi, lscale,rscale, work, info )
     !! DGGBAL balances a pair of general real matrices (A,B).  This
     !! involves, first, permuting A and B by similarity transformations to
     !! isolate eigenvalues in the first 1 to ILO$-$1 and last IHI+1 to N
     !! elements on the diagonal; and second, applying a diagonal similarity
     !! transformation to rows and columns ILO to IHI to make the rows
     !! and columns as close in norm as possible. Both steps are optional.
     !! Balancing may reduce the 1-norm of the matrices, and improve the
     !! accuracy of the computed eigenvalues and/or eigenvectors in the
     !! generalized eigenvalue problem A*x = lambda*B*x.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: job
           integer(ilp), intent(out) :: ihi, ilo, info
           integer(ilp), intent(in) :: lda, ldb, n
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*), b(ldb,*)
           real(dp), intent(out) :: lscale(*), rscale(*), work(*)
        ! =====================================================================
           ! Parameters 
           real(dp), parameter :: sclfac = ten
           
           
           ! Local Scalars 
           integer(ilp) :: i, icab, iflow, ip1, ir, irab, it, j, jc, jp1, k, kount, l, lcab, lm1, &
                     lrab, lsfmax, lsfmin, m, nr, nrp2
           real(dp) :: alpha, basl, beta, cab, cmax, coef, coef2, coef5, cor, ew, ewc, gamma, &
                     pgamma, rab, sfmax, sfmin, sum, t, ta, tb, tc
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp
           if( .not.stdlib_lsame( job, 'N' ) .and. .not.stdlib_lsame( job, 'P' ) &
                     .and..not.stdlib_lsame( job, 'S' ) .and. .not.stdlib_lsame( job, 'B' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DGGBAL', -info )
              return
           end if
           ! quick return if possible
           if( n==0_ilp ) then
              ilo = 1_ilp
              ihi = n
              return
           end if
           if( n==1_ilp ) then
              ilo = 1_ilp
              ihi = n
              lscale( 1_ilp ) = one
              rscale( 1_ilp ) = one
              return
           end if
           if( stdlib_lsame( job, 'N' ) ) then
              ilo = 1_ilp
              ihi = n
              do i = 1, n
                 lscale( i ) = one
                 rscale( i ) = one
              end do
              return
           end if
           k = 1_ilp
           l = n
           if( stdlib_lsame( job, 'S' ) )go to 190
           go to 30
           ! permute the matrices a and b to isolate the eigenvalues.
           ! find row with one nonzero in columns 1 through l
           20 continue
           l = lm1
           if( l/=1 )go to 30
           rscale( 1_ilp ) = one
           lscale( 1_ilp ) = one
           go to 190
           30 continue
           lm1 = l - 1_ilp
           loop_80: do i = l, 1, -1
              do j = 1, lm1
                 jp1 = j + 1_ilp
                 if( a( i, j )/=zero .or. b( i, j )/=zero )go to 50
              end do
              j = l
              go to 70
              50 continue
              do j = jp1, l
                 if( a( i, j )/=zero .or. b( i, j )/=zero )cycle loop_80
              end do
              j = jp1 - 1_ilp
              70 continue
              m = l
              iflow = 1_ilp
              go to 160
           end do loop_80
           go to 100
           ! find column with one nonzero in rows k through n
           90 continue
           k = k + 1_ilp
           100 continue
           loop_150: do j = k, l
              do i = k, lm1
                 ip1 = i + 1_ilp
                 if( a( i, j )/=zero .or. b( i, j )/=zero )go to 120
              end do
              i = l
              go to 140
              120 continue
              do i = ip1, l
                 if( a( i, j )/=zero .or. b( i, j )/=zero )cycle loop_150
              end do
              i = ip1 - 1_ilp
              140 continue
              m = k
              iflow = 2_ilp
              go to 160
           end do loop_150
           go to 190
           ! permute rows m and i
           160 continue
           lscale( m ) = i
           if( i==m )go to 170
           call stdlib_dswap( n-k+1, a( i, k ), lda, a( m, k ), lda )
           call stdlib_dswap( n-k+1, b( i, k ), ldb, b( m, k ), ldb )
           ! permute columns m and j
           170 continue
           rscale( m ) = j
           if( j==m )go to 180
           call stdlib_dswap( l, a( 1_ilp, j ), 1_ilp, a( 1_ilp, m ), 1_ilp )
           call stdlib_dswap( l, b( 1_ilp, j ), 1_ilp, b( 1_ilp, m ), 1_ilp )
           180 continue
           go to ( 20, 90 )iflow
           190 continue
           ilo = k
           ihi = l
           if( stdlib_lsame( job, 'P' ) ) then
              do i = ilo, ihi
                 lscale( i ) = one
                 rscale( i ) = one
              end do
              return
           end if
           if( ilo==ihi )return
           ! balance the submatrix in rows ilo to ihi.
           nr = ihi - ilo + 1_ilp
           do i = ilo, ihi
              rscale( i ) = zero
              lscale( i ) = zero
              work( i ) = zero
              work( i+n ) = zero
              work( i+2*n ) = zero
              work( i+3*n ) = zero
              work( i+4*n ) = zero
              work( i+5*n ) = zero
           end do
           ! compute right side vector in resulting linear equations
           basl = log10( sclfac )
           do i = ilo, ihi
              do j = ilo, ihi
                 tb = b( i, j )
                 ta = a( i, j )
                 if( ta==zero )go to 210
                 ta = log10( abs( ta ) ) / basl
                 210 continue
                 if( tb==zero )go to 220
                 tb = log10( abs( tb ) ) / basl
                 220 continue
                 work( i+4*n ) = work( i+4*n ) - ta - tb
                 work( j+5*n ) = work( j+5*n ) - ta - tb
              end do
           end do
           coef = one / real( 2_ilp*nr,KIND=dp)
           coef2 = coef*coef
           coef5 = half*coef2
           nrp2 = nr + 2_ilp
           beta = zero
           it = 1_ilp
           ! start generalized conjugate gradient iteration
           250 continue
           gamma = stdlib_ddot( nr, work( ilo+4*n ), 1_ilp, work( ilo+4*n ), 1_ilp ) +stdlib_ddot( nr, &
                     work( ilo+5*n ), 1_ilp, work( ilo+5*n ), 1_ilp )
           ew = zero
           ewc = zero
           do i = ilo, ihi
              ew = ew + work( i+4*n )
              ewc = ewc + work( i+5*n )
           end do
           gamma = coef*gamma - coef2*( ew**2_ilp+ewc**2_ilp ) - coef5*( ew-ewc )**2_ilp
           if( gamma==zero )go to 350
           if( it/=1_ilp )beta = gamma / pgamma
           t = coef5*( ewc-three*ew )
           tc = coef5*( ew-three*ewc )
           call stdlib_dscal( nr, beta, work( ilo ), 1_ilp )
           call stdlib_dscal( nr, beta, work( ilo+n ), 1_ilp )
           call stdlib_daxpy( nr, coef, work( ilo+4*n ), 1_ilp, work( ilo+n ), 1_ilp )
           call stdlib_daxpy( nr, coef, work( ilo+5*n ), 1_ilp, work( ilo ), 1_ilp )
           do i = ilo, ihi
              work( i ) = work( i ) + tc
              work( i+n ) = work( i+n ) + t
           end do
           ! apply matrix to vector
           do i = ilo, ihi
              kount = 0_ilp
              sum = zero
              loop_290: do j = ilo, ihi
                 if( a( i, j )==zero )go to 280
                 kount = kount + 1_ilp
                 sum = sum + work( j )
                 280 continue
                 if( b( i, j )==zero )cycle loop_290
                 kount = kount + 1_ilp
                 sum = sum + work( j )
              end do loop_290
              work( i+2*n ) = real( kount,KIND=dp)*work( i+n ) + sum
           end do
           do j = ilo, ihi
              kount = 0_ilp
              sum = zero
              loop_320: do i = ilo, ihi
                 if( a( i, j )==zero )go to 310
                 kount = kount + 1_ilp
                 sum = sum + work( i+n )
                 310 continue
                 if( b( i, j )==zero )cycle loop_320
                 kount = kount + 1_ilp
                 sum = sum + work( i+n )
              end do loop_320
              work( j+3*n ) = real( kount,KIND=dp)*work( j ) + sum
           end do
           sum = stdlib_ddot( nr, work( ilo+n ), 1_ilp, work( ilo+2*n ), 1_ilp ) +stdlib_ddot( nr, work( &
                     ilo ), 1_ilp, work( ilo+3*n ), 1_ilp )
           alpha = gamma / sum
           ! determine correction to current iteration
           cmax = zero
           do i = ilo, ihi
              cor = alpha*work( i+n )
              if( abs( cor )>cmax )cmax = abs( cor )
              lscale( i ) = lscale( i ) + cor
              cor = alpha*work( i )
              if( abs( cor )>cmax )cmax = abs( cor )
              rscale( i ) = rscale( i ) + cor
           end do
           if( cmax<half )go to 350
           call stdlib_daxpy( nr, -alpha, work( ilo+2*n ), 1_ilp, work( ilo+4*n ), 1_ilp )
           call stdlib_daxpy( nr, -alpha, work( ilo+3*n ), 1_ilp, work( ilo+5*n ), 1_ilp )
           pgamma = gamma
           it = it + 1_ilp
           if( it<=nrp2 )go to 250
           ! end generalized conjugate gradient iteration
           350 continue
           sfmin = stdlib_dlamch( 'S' )
           sfmax = one / sfmin
           lsfmin = int( log10( sfmin ) / basl+one,KIND=ilp)
           lsfmax = int( log10( sfmax ) / basl,KIND=ilp)
           do i = ilo, ihi
              irab = stdlib_idamax( n-ilo+1, a( i, ilo ), lda )
              rab = abs( a( i, irab+ilo-1 ) )
              irab = stdlib_idamax( n-ilo+1, b( i, ilo ), ldb )
              rab = max( rab, abs( b( i, irab+ilo-1 ) ) )
              lrab = int( log10( rab+sfmin ) / basl+one,KIND=ilp)
              ir = int(lscale( i ) + sign( half, lscale( i ) ),KIND=ilp)
              ir = min( max( ir, lsfmin ), lsfmax, lsfmax-lrab )
              lscale( i ) = sclfac**ir
              icab = stdlib_idamax( ihi, a( 1_ilp, i ), 1_ilp )
              cab = abs( a( icab, i ) )
              icab = stdlib_idamax( ihi, b( 1_ilp, i ), 1_ilp )
              cab = max( cab, abs( b( icab, i ) ) )
              lcab = int( log10( cab+sfmin ) / basl+one,KIND=ilp)
              jc = int(rscale( i ) + sign( half, rscale( i ) ),KIND=ilp)
              jc = min( max( jc, lsfmin ), lsfmax, lsfmax-lcab )
              rscale( i ) = sclfac**jc
           end do
           ! row scaling of matrices a and b
           do i = ilo, ihi
              call stdlib_dscal( n-ilo+1, lscale( i ), a( i, ilo ), lda )
              call stdlib_dscal( n-ilo+1, lscale( i ), b( i, ilo ), ldb )
           end do
           ! column scaling of matrices a and b
           do j = ilo, ihi
              call stdlib_dscal( ihi, rscale( j ), a( 1_ilp, j ), 1_ilp )
              call stdlib_dscal( ihi, rscale( j ), b( 1_ilp, j ), 1_ilp )
           end do
           return
     end subroutine stdlib_dggbal


     pure module subroutine stdlib_cggbal( job, n, a, lda, b, ldb, ilo, ihi, lscale,rscale, work, info )
     !! CGGBAL balances a pair of general complex matrices (A,B).  This
     !! involves, first, permuting A and B by similarity transformations to
     !! isolate eigenvalues in the first 1 to ILO$-$1 and last IHI+1 to N
     !! elements on the diagonal; and second, applying a diagonal similarity
     !! transformation to rows and columns ILO to IHI to make the rows
     !! and columns as close in norm as possible. Both steps are optional.
     !! Balancing may reduce the 1-norm of the matrices, and improve the
     !! accuracy of the computed eigenvalues and/or eigenvectors in the
     !! generalized eigenvalue problem A*x = lambda*B*x.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: job
           integer(ilp), intent(out) :: ihi, ilo, info
           integer(ilp), intent(in) :: lda, ldb, n
           ! Array Arguments 
           real(sp), intent(out) :: lscale(*), rscale(*), work(*)
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*)
        ! =====================================================================
           ! Parameters 
           real(sp), parameter :: sclfac = ten
           
           
           
           ! Local Scalars 
           integer(ilp) :: i, icab, iflow, ip1, ir, irab, it, j, jc, jp1, k, kount, l, lcab, lm1, &
                     lrab, lsfmax, lsfmin, m, nr, nrp2
           real(sp) :: alpha, basl, beta, cab, cmax, coef, coef2, coef5, cor, ew, ewc, gamma, &
                     pgamma, rab, sfmax, sfmin, sum, t, ta, tb, tc
           complex(sp) :: cdum
           ! Intrinsic Functions 
           ! Statement Functions 
           real(sp) :: cabs1
           ! Statement Function Definitions 
           cabs1( cdum ) = abs( real( cdum,KIND=sp) ) + abs( aimag( cdum ) )
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp
           if( .not.stdlib_lsame( job, 'N' ) .and. .not.stdlib_lsame( job, 'P' ) &
                     .and..not.stdlib_lsame( job, 'S' ) .and. .not.stdlib_lsame( job, 'B' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CGGBAL', -info )
              return
           end if
           ! quick return if possible
           if( n==0_ilp ) then
              ilo = 1_ilp
              ihi = n
              return
           end if
           if( n==1_ilp ) then
              ilo = 1_ilp
              ihi = n
              lscale( 1_ilp ) = one
              rscale( 1_ilp ) = one
              return
           end if
           if( stdlib_lsame( job, 'N' ) ) then
              ilo = 1_ilp
              ihi = n
              do i = 1, n
                 lscale( i ) = one
                 rscale( i ) = one
              end do
              return
           end if
           k = 1_ilp
           l = n
           if( stdlib_lsame( job, 'S' ) )go to 190
           go to 30
           ! permute the matrices a and b to isolate the eigenvalues.
           ! find row with one nonzero in columns 1 through l
           20 continue
           l = lm1
           if( l/=1 )go to 30
           rscale( 1_ilp ) = one
           lscale( 1_ilp ) = one
           go to 190
           30 continue
           lm1 = l - 1_ilp
           loop_80: do i = l, 1, -1
              do j = 1, lm1
                 jp1 = j + 1_ilp
                 if( a( i, j )/=czero .or. b( i, j )/=czero )go to 50
              end do
              j = l
              go to 70
              50 continue
              do j = jp1, l
                 if( a( i, j )/=czero .or. b( i, j )/=czero )cycle loop_80
              end do
              j = jp1 - 1_ilp
              70 continue
              m = l
              iflow = 1_ilp
              go to 160
           end do loop_80
           go to 100
           ! find column with one nonzero in rows k through n
           90 continue
           k = k + 1_ilp
           100 continue
           loop_150: do j = k, l
              do i = k, lm1
                 ip1 = i + 1_ilp
                 if( a( i, j )/=czero .or. b( i, j )/=czero )go to 120
              end do
              i = l
              go to 140
              120 continue
              do i = ip1, l
                 if( a( i, j )/=czero .or. b( i, j )/=czero )cycle loop_150
              end do
              i = ip1 - 1_ilp
              140 continue
              m = k
              iflow = 2_ilp
              go to 160
           end do loop_150
           go to 190
           ! permute rows m and i
           160 continue
           lscale( m ) = i
           if( i==m )go to 170
           call stdlib_cswap( n-k+1, a( i, k ), lda, a( m, k ), lda )
           call stdlib_cswap( n-k+1, b( i, k ), ldb, b( m, k ), ldb )
           ! permute columns m and j
           170 continue
           rscale( m ) = j
           if( j==m )go to 180
           call stdlib_cswap( l, a( 1_ilp, j ), 1_ilp, a( 1_ilp, m ), 1_ilp )
           call stdlib_cswap( l, b( 1_ilp, j ), 1_ilp, b( 1_ilp, m ), 1_ilp )
           180 continue
           go to ( 20, 90 )iflow
           190 continue
           ilo = k
           ihi = l
           if( stdlib_lsame( job, 'P' ) ) then
              do i = ilo, ihi
                 lscale( i ) = one
                 rscale( i ) = one
              end do
              return
           end if
           if( ilo==ihi )return
           ! balance the submatrix in rows ilo to ihi.
           nr = ihi - ilo + 1_ilp
           do i = ilo, ihi
              rscale( i ) = zero
              lscale( i ) = zero
              work( i ) = zero
              work( i+n ) = zero
              work( i+2*n ) = zero
              work( i+3*n ) = zero
              work( i+4*n ) = zero
              work( i+5*n ) = zero
           end do
           ! compute right side vector in resulting linear equations
           basl = log10( sclfac )
           do i = ilo, ihi
              do j = ilo, ihi
                 if( a( i, j )==czero ) then
                    ta = zero
                    go to 210
                 end if
                 ta = log10( cabs1( a( i, j ) ) ) / basl
                 210 continue
                 if( b( i, j )==czero ) then
                    tb = zero
                    go to 220
                 end if
                 tb = log10( cabs1( b( i, j ) ) ) / basl
                 220 continue
                 work( i+4*n ) = work( i+4*n ) - ta - tb
                 work( j+5*n ) = work( j+5*n ) - ta - tb
              end do
           end do
           coef = one / real( 2_ilp*nr,KIND=sp)
           coef2 = coef*coef
           coef5 = half*coef2
           nrp2 = nr + 2_ilp
           beta = zero
           it = 1_ilp
           ! start generalized conjugate gradient iteration
           250 continue
           gamma = stdlib_sdot( nr, work( ilo+4*n ), 1_ilp, work( ilo+4*n ), 1_ilp ) +stdlib_sdot( nr, &
                     work( ilo+5*n ), 1_ilp, work( ilo+5*n ), 1_ilp )
           ew = zero
           ewc = zero
           do i = ilo, ihi
              ew = ew + work( i+4*n )
              ewc = ewc + work( i+5*n )
           end do
           gamma = coef*gamma - coef2*( ew**2_ilp+ewc**2_ilp ) - coef5*( ew-ewc )**2_ilp
           if( gamma==zero )go to 350
           if( it/=1_ilp )beta = gamma / pgamma
           t = coef5*( ewc-three*ew )
           tc = coef5*( ew-three*ewc )
           call stdlib_sscal( nr, beta, work( ilo ), 1_ilp )
           call stdlib_sscal( nr, beta, work( ilo+n ), 1_ilp )
           call stdlib_saxpy( nr, coef, work( ilo+4*n ), 1_ilp, work( ilo+n ), 1_ilp )
           call stdlib_saxpy( nr, coef, work( ilo+5*n ), 1_ilp, work( ilo ), 1_ilp )
           do i = ilo, ihi
              work( i ) = work( i ) + tc
              work( i+n ) = work( i+n ) + t
           end do
           ! apply matrix to vector
           do i = ilo, ihi
              kount = 0_ilp
              sum = zero
              loop_290: do j = ilo, ihi
                 if( a( i, j )==czero )go to 280
                 kount = kount + 1_ilp
                 sum = sum + work( j )
                 280 continue
                 if( b( i, j )==czero )cycle loop_290
                 kount = kount + 1_ilp
                 sum = sum + work( j )
              end do loop_290
              work( i+2*n ) = real( kount,KIND=sp)*work( i+n ) + sum
           end do
           do j = ilo, ihi
              kount = 0_ilp
              sum = zero
              loop_320: do i = ilo, ihi
                 if( a( i, j )==czero )go to 310
                 kount = kount + 1_ilp
                 sum = sum + work( i+n )
                 310 continue
                 if( b( i, j )==czero )cycle loop_320
                 kount = kount + 1_ilp
                 sum = sum + work( i+n )
              end do loop_320
              work( j+3*n ) = real( kount,KIND=sp)*work( j ) + sum
           end do
           sum = stdlib_sdot( nr, work( ilo+n ), 1_ilp, work( ilo+2*n ), 1_ilp ) +stdlib_sdot( nr, work( &
                     ilo ), 1_ilp, work( ilo+3*n ), 1_ilp )
           alpha = gamma / sum
           ! determine correction to current iteration
           cmax = zero
           do i = ilo, ihi
              cor = alpha*work( i+n )
              if( abs( cor )>cmax )cmax = abs( cor )
              lscale( i ) = lscale( i ) + cor
              cor = alpha*work( i )
              if( abs( cor )>cmax )cmax = abs( cor )
              rscale( i ) = rscale( i ) + cor
           end do
           if( cmax<half )go to 350
           call stdlib_saxpy( nr, -alpha, work( ilo+2*n ), 1_ilp, work( ilo+4*n ), 1_ilp )
           call stdlib_saxpy( nr, -alpha, work( ilo+3*n ), 1_ilp, work( ilo+5*n ), 1_ilp )
           pgamma = gamma
           it = it + 1_ilp
           if( it<=nrp2 )go to 250
           ! end generalized conjugate gradient iteration
           350 continue
           sfmin = stdlib_slamch( 'S' )
           sfmax = one / sfmin
           lsfmin = int( log10( sfmin ) / basl+one,KIND=ilp)
           lsfmax = int( log10( sfmax ) / basl,KIND=ilp)
           do i = ilo, ihi
              irab = stdlib_icamax( n-ilo+1, a( i, ilo ), lda )
              rab = abs( a( i, irab+ilo-1 ) )
              irab = stdlib_icamax( n-ilo+1, b( i, ilo ), ldb )
              rab = max( rab, abs( b( i, irab+ilo-1 ) ) )
              lrab = int( log10( rab+sfmin ) / basl+one,KIND=ilp)
              ir = lscale( i ) + sign( half, lscale( i ) )
              ir = min( max( ir, lsfmin ), lsfmax, lsfmax-lrab )
              lscale( i ) = sclfac**ir
              icab = stdlib_icamax( ihi, a( 1_ilp, i ), 1_ilp )
              cab = abs( a( icab, i ) )
              icab = stdlib_icamax( ihi, b( 1_ilp, i ), 1_ilp )
              cab = max( cab, abs( b( icab, i ) ) )
              lcab = int( log10( cab+sfmin ) / basl+one,KIND=ilp)
              jc = rscale( i ) + sign( half, rscale( i ) )
              jc = min( max( jc, lsfmin ), lsfmax, lsfmax-lcab )
              rscale( i ) = sclfac**jc
           end do
           ! row scaling of matrices a and b
           do i = ilo, ihi
              call stdlib_csscal( n-ilo+1, lscale( i ), a( i, ilo ), lda )
              call stdlib_csscal( n-ilo+1, lscale( i ), b( i, ilo ), ldb )
           end do
           ! column scaling of matrices a and b
           do j = ilo, ihi
              call stdlib_csscal( ihi, rscale( j ), a( 1_ilp, j ), 1_ilp )
              call stdlib_csscal( ihi, rscale( j ), b( 1_ilp, j ), 1_ilp )
           end do
           return
     end subroutine stdlib_cggbal

     pure module subroutine stdlib_zggbal( job, n, a, lda, b, ldb, ilo, ihi, lscale,rscale, work, info )
     !! ZGGBAL balances a pair of general complex matrices (A,B).  This
     !! involves, first, permuting A and B by similarity transformations to
     !! isolate eigenvalues in the first 1 to ILO$-$1 and last IHI+1 to N
     !! elements on the diagonal; and second, applying a diagonal similarity
     !! transformation to rows and columns ILO to IHI to make the rows
     !! and columns as close in norm as possible. Both steps are optional.
     !! Balancing may reduce the 1-norm of the matrices, and improve the
     !! accuracy of the computed eigenvalues and/or eigenvectors in the
     !! generalized eigenvalue problem A*x = lambda*B*x.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: job
           integer(ilp), intent(out) :: ihi, ilo, info
           integer(ilp), intent(in) :: lda, ldb, n
           ! Array Arguments 
           real(dp), intent(out) :: lscale(*), rscale(*), work(*)
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*)
        ! =====================================================================
           ! Parameters 
           real(dp), parameter :: sclfac = ten
           
           
           
           ! Local Scalars 
           integer(ilp) :: i, icab, iflow, ip1, ir, irab, it, j, jc, jp1, k, kount, l, lcab, lm1, &
                     lrab, lsfmax, lsfmin, m, nr, nrp2
           real(dp) :: alpha, basl, beta, cab, cmax, coef, coef2, coef5, cor, ew, ewc, gamma, &
                     pgamma, rab, sfmax, sfmin, sum, t, ta, tb, tc
           complex(dp) :: cdum
           ! Intrinsic Functions 
           ! Statement Functions 
           real(dp) :: cabs1
           ! Statement Function Definitions 
           cabs1( cdum ) = abs( real( cdum,KIND=dp) ) + abs( aimag( cdum ) )
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp
           if( .not.stdlib_lsame( job, 'N' ) .and. .not.stdlib_lsame( job, 'P' ) &
                     .and..not.stdlib_lsame( job, 'S' ) .and. .not.stdlib_lsame( job, 'B' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -6_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZGGBAL', -info )
              return
           end if
           ! quick return if possible
           if( n==0_ilp ) then
              ilo = 1_ilp
              ihi = n
              return
           end if
           if( n==1_ilp ) then
              ilo = 1_ilp
              ihi = n
              lscale( 1_ilp ) = one
              rscale( 1_ilp ) = one
              return
           end if
           if( stdlib_lsame( job, 'N' ) ) then
              ilo = 1_ilp
              ihi = n
              do i = 1, n
                 lscale( i ) = one
                 rscale( i ) = one
              end do
              return
           end if
           k = 1_ilp
           l = n
           if( stdlib_lsame( job, 'S' ) )go to 190
           go to 30
           ! permute the matrices a and b to isolate the eigenvalues.
           ! find row with one nonzero in columns 1 through l
           20 continue
           l = lm1
           if( l/=1 )go to 30
           rscale( 1_ilp ) = 1_ilp
           lscale( 1_ilp ) = 1_ilp
           go to 190
           30 continue
           lm1 = l - 1_ilp
           loop_80: do i = l, 1, -1
              do j = 1, lm1
                 jp1 = j + 1_ilp
                 if( a( i, j )/=czero .or. b( i, j )/=czero )go to 50
              end do
              j = l
              go to 70
              50 continue
              do j = jp1, l
                 if( a( i, j )/=czero .or. b( i, j )/=czero )cycle loop_80
              end do
              j = jp1 - 1_ilp
              70 continue
              m = l
              iflow = 1_ilp
              go to 160
           end do loop_80
           go to 100
           ! find column with one nonzero in rows k through n
           90 continue
           k = k + 1_ilp
           100 continue
           loop_150: do j = k, l
              do i = k, lm1
                 ip1 = i + 1_ilp
                 if( a( i, j )/=czero .or. b( i, j )/=czero )go to 120
              end do
              i = l
              go to 140
              120 continue
              do i = ip1, l
                 if( a( i, j )/=czero .or. b( i, j )/=czero )cycle loop_150
              end do
              i = ip1 - 1_ilp
              140 continue
              m = k
              iflow = 2_ilp
              go to 160
           end do loop_150
           go to 190
           ! permute rows m and i
           160 continue
           lscale( m ) = i
           if( i==m )go to 170
           call stdlib_zswap( n-k+1, a( i, k ), lda, a( m, k ), lda )
           call stdlib_zswap( n-k+1, b( i, k ), ldb, b( m, k ), ldb )
           ! permute columns m and j
           170 continue
           rscale( m ) = j
           if( j==m )go to 180
           call stdlib_zswap( l, a( 1_ilp, j ), 1_ilp, a( 1_ilp, m ), 1_ilp )
           call stdlib_zswap( l, b( 1_ilp, j ), 1_ilp, b( 1_ilp, m ), 1_ilp )
           180 continue
           go to ( 20, 90 )iflow
           190 continue
           ilo = k
           ihi = l
           if( stdlib_lsame( job, 'P' ) ) then
              do i = ilo, ihi
                 lscale( i ) = one
                 rscale( i ) = one
              end do
              return
           end if
           if( ilo==ihi )return
           ! balance the submatrix in rows ilo to ihi.
           nr = ihi - ilo + 1_ilp
           do i = ilo, ihi
              rscale( i ) = zero
              lscale( i ) = zero
              work( i ) = zero
              work( i+n ) = zero
              work( i+2*n ) = zero
              work( i+3*n ) = zero
              work( i+4*n ) = zero
              work( i+5*n ) = zero
           end do
           ! compute right side vector in resulting linear equations
           basl = log10( sclfac )
           do i = ilo, ihi
              do j = ilo, ihi
                 if( a( i, j )==czero ) then
                    ta = zero
                    go to 210
                 end if
                 ta = log10( cabs1( a( i, j ) ) ) / basl
                 210 continue
                 if( b( i, j )==czero ) then
                    tb = zero
                    go to 220
                 end if
                 tb = log10( cabs1( b( i, j ) ) ) / basl
                 220 continue
                 work( i+4*n ) = work( i+4*n ) - ta - tb
                 work( j+5*n ) = work( j+5*n ) - ta - tb
              end do
           end do
           coef = one / real( 2_ilp*nr,KIND=dp)
           coef2 = coef*coef
           coef5 = half*coef2
           nrp2 = nr + 2_ilp
           beta = zero
           it = 1_ilp
           ! start generalized conjugate gradient iteration
           250 continue
           gamma = stdlib_ddot( nr, work( ilo+4*n ), 1_ilp, work( ilo+4*n ), 1_ilp ) +stdlib_ddot( nr, &
                     work( ilo+5*n ), 1_ilp, work( ilo+5*n ), 1_ilp )
           ew = zero
           ewc = zero
           do i = ilo, ihi
              ew = ew + work( i+4*n )
              ewc = ewc + work( i+5*n )
           end do
           gamma = coef*gamma - coef2*( ew**2_ilp+ewc**2_ilp ) - coef5*( ew-ewc )**2_ilp
           if( gamma==zero )go to 350
           if( it/=1_ilp )beta = gamma / pgamma
           t = coef5*( ewc-three*ew )
           tc = coef5*( ew-three*ewc )
           call stdlib_dscal( nr, beta, work( ilo ), 1_ilp )
           call stdlib_dscal( nr, beta, work( ilo+n ), 1_ilp )
           call stdlib_daxpy( nr, coef, work( ilo+4*n ), 1_ilp, work( ilo+n ), 1_ilp )
           call stdlib_daxpy( nr, coef, work( ilo+5*n ), 1_ilp, work( ilo ), 1_ilp )
           do i = ilo, ihi
              work( i ) = work( i ) + tc
              work( i+n ) = work( i+n ) + t
           end do
           ! apply matrix to vector
           do i = ilo, ihi
              kount = 0_ilp
              sum = zero
              loop_290: do j = ilo, ihi
                 if( a( i, j )==czero )go to 280
                 kount = kount + 1_ilp
                 sum = sum + work( j )
                 280 continue
                 if( b( i, j )==czero )cycle loop_290
                 kount = kount + 1_ilp
                 sum = sum + work( j )
              end do loop_290
              work( i+2*n ) = real( kount,KIND=dp)*work( i+n ) + sum
           end do
           do j = ilo, ihi
              kount = 0_ilp
              sum = zero
              loop_320: do i = ilo, ihi
                 if( a( i, j )==czero )go to 310
                 kount = kount + 1_ilp
                 sum = sum + work( i+n )
                 310 continue
                 if( b( i, j )==czero )cycle loop_320
                 kount = kount + 1_ilp
                 sum = sum + work( i+n )
              end do loop_320
              work( j+3*n ) = real( kount,KIND=dp)*work( j ) + sum
           end do
           sum = stdlib_ddot( nr, work( ilo+n ), 1_ilp, work( ilo+2*n ), 1_ilp ) +stdlib_ddot( nr, work( &
                     ilo ), 1_ilp, work( ilo+3*n ), 1_ilp )
           alpha = gamma / sum
           ! determine correction to current iteration
           cmax = zero
           do i = ilo, ihi
              cor = alpha*work( i+n )
              if( abs( cor )>cmax )cmax = abs( cor )
              lscale( i ) = lscale( i ) + cor
              cor = alpha*work( i )
              if( abs( cor )>cmax )cmax = abs( cor )
              rscale( i ) = rscale( i ) + cor
           end do
           if( cmax<half )go to 350
           call stdlib_daxpy( nr, -alpha, work( ilo+2*n ), 1_ilp, work( ilo+4*n ), 1_ilp )
           call stdlib_daxpy( nr, -alpha, work( ilo+3*n ), 1_ilp, work( ilo+5*n ), 1_ilp )
           pgamma = gamma
           it = it + 1_ilp
           if( it<=nrp2 )go to 250
           ! end generalized conjugate gradient iteration
           350 continue
           sfmin = stdlib_dlamch( 'S' )
           sfmax = one / sfmin
           lsfmin = int( log10( sfmin ) / basl+one,KIND=ilp)
           lsfmax = int( log10( sfmax ) / basl,KIND=ilp)
           do i = ilo, ihi
              irab = stdlib_izamax( n-ilo+1, a( i, ilo ), lda )
              rab = abs( a( i, irab+ilo-1 ) )
              irab = stdlib_izamax( n-ilo+1, b( i, ilo ), ldb )
              rab = max( rab, abs( b( i, irab+ilo-1 ) ) )
              lrab = int( log10( rab+sfmin ) / basl+one,KIND=ilp)
              ir = int(lscale( i ) + sign( half, lscale( i ) ),KIND=ilp)
              ir = min( max( ir, lsfmin ), lsfmax, lsfmax-lrab )
              lscale( i ) = sclfac**ir
              icab = stdlib_izamax( ihi, a( 1_ilp, i ), 1_ilp )
              cab = abs( a( icab, i ) )
              icab = stdlib_izamax( ihi, b( 1_ilp, i ), 1_ilp )
              cab = max( cab, abs( b( icab, i ) ) )
              lcab = int( log10( cab+sfmin ) / basl+one,KIND=ilp)
              jc = int(rscale( i ) + sign( half, rscale( i ) ),KIND=ilp)
              jc = min( max( jc, lsfmin ), lsfmax, lsfmax-lcab )
              rscale( i ) = sclfac**jc
           end do
           ! row scaling of matrices a and b
           do i = ilo, ihi
              call stdlib_zdscal( n-ilo+1, lscale( i ), a( i, ilo ), lda )
              call stdlib_zdscal( n-ilo+1, lscale( i ), b( i, ilo ), ldb )
           end do
           ! column scaling of matrices a and b
           do j = ilo, ihi
              call stdlib_zdscal( ihi, rscale( j ), a( 1_ilp, j ), 1_ilp )
              call stdlib_zdscal( ihi, rscale( j ), b( 1_ilp, j ), 1_ilp )
           end do
           return
     end subroutine stdlib_zggbal




     pure module subroutine stdlib_sgghrd( compq, compz, n, ilo, ihi, a, lda, b, ldb, q,ldq, z, ldz, &
     !! SGGHRD reduces a pair of real matrices (A,B) to generalized upper
     !! Hessenberg form using orthogonal transformations, where A is a
     !! general matrix and B is upper triangular.  The form of the
     !! generalized eigenvalue problem is
     !! A*x = lambda*B*x,
     !! and B is typically made upper triangular by computing its QR
     !! factorization and moving the orthogonal matrix Q to the left side
     !! of the equation.
     !! This subroutine simultaneously reduces A to a Hessenberg matrix H:
     !! Q**T*A*Z = H
     !! and transforms B to another upper triangular matrix T:
     !! Q**T*B*Z = T
     !! in order to reduce the problem to its standard form
     !! H*y = lambda*T*y
     !! where y = Z**T*x.
     !! The orthogonal matrices Q and Z are determined as products of Givens
     !! rotations.  They may either be formed explicitly, or they may be
     !! postmultiplied into input matrices Q1 and Z1, so that
     !! Q1 * A * Z1**T = (Q1*Q) * H * (Z1*Z)**T
     !! Q1 * B * Z1**T = (Q1*Q) * T * (Z1*Z)**T
     !! If Q1 is the orthogonal matrix from the QR factorization of B in the
     !! original equation A*x = lambda*B*x, then SGGHRD reduces the original
     !! problem to generalized Hessenberg form.
               info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: compq, compz
           integer(ilp), intent(in) :: ihi, ilo, lda, ldb, ldq, ldz, n
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*), b(ldb,*), q(ldq,*), z(ldz,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: ilq, ilz
           integer(ilp) :: icompq, icompz, jcol, jrow
           real(sp) :: c, s, temp
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode compq
           if( stdlib_lsame( compq, 'N' ) ) then
              ilq = .false.
              icompq = 1_ilp
           else if( stdlib_lsame( compq, 'V' ) ) then
              ilq = .true.
              icompq = 2_ilp
           else if( stdlib_lsame( compq, 'I' ) ) then
              ilq = .true.
              icompq = 3_ilp
           else
              icompq = 0_ilp
           end if
           ! decode compz
           if( stdlib_lsame( compz, 'N' ) ) then
              ilz = .false.
              icompz = 1_ilp
           else if( stdlib_lsame( compz, 'V' ) ) then
              ilz = .true.
              icompz = 2_ilp
           else if( stdlib_lsame( compz, 'I' ) ) then
              ilz = .true.
              icompz = 3_ilp
           else
              icompz = 0_ilp
           end if
           ! test the input parameters.
           info = 0_ilp
           if( icompq<=0_ilp ) then
              info = -1_ilp
           else if( icompz<=0_ilp ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( ilo<1_ilp ) then
              info = -4_ilp
           else if( ihi>n .or. ihi<ilo-1 ) then
              info = -5_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -7_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -9_ilp
           else if( ( ilq .and. ldq<n ) .or. ldq<1_ilp ) then
              info = -11_ilp
           else if( ( ilz .and. ldz<n ) .or. ldz<1_ilp ) then
              info = -13_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SGGHRD', -info )
              return
           end if
           ! initialize q and z if desired.
           if( icompq==3_ilp )call stdlib_slaset( 'FULL', n, n, zero, one, q, ldq )
           if( icompz==3_ilp )call stdlib_slaset( 'FULL', n, n, zero, one, z, ldz )
           ! quick return if possible
           if( n<=1 )return
           ! zero out lower triangle of b
           do jcol = 1, n - 1
              do jrow = jcol + 1, n
                 b( jrow, jcol ) = zero
              end do
           end do
           ! reduce a and b
           do jcol = ilo, ihi - 2
              do jrow = ihi, jcol + 2, -1
                 ! step 1: rotate rows jrow-1, jrow to kill a(jrow,jcol)
                 temp = a( jrow-1, jcol )
                 call stdlib_slartg( temp, a( jrow, jcol ), c, s,a( jrow-1, jcol ) )
                 a( jrow, jcol ) = zero
                 call stdlib_srot( n-jcol, a( jrow-1, jcol+1 ), lda,a( jrow, jcol+1 ), lda, c, s )
                           
                 call stdlib_srot( n+2-jrow, b( jrow-1, jrow-1 ), ldb,b( jrow, jrow-1 ), ldb, c, &
                           s )
                 if( ilq )call stdlib_srot( n, q( 1_ilp, jrow-1 ), 1_ilp, q( 1_ilp, jrow ), 1_ilp, c, s )
                 ! step 2: rotate columns jrow, jrow-1 to kill b(jrow,jrow-1)
                 temp = b( jrow, jrow )
                 call stdlib_slartg( temp, b( jrow, jrow-1 ), c, s,b( jrow, jrow ) )
                 b( jrow, jrow-1 ) = zero
                 call stdlib_srot( ihi, a( 1_ilp, jrow ), 1_ilp, a( 1_ilp, jrow-1 ), 1_ilp, c, s )
                 call stdlib_srot( jrow-1, b( 1_ilp, jrow ), 1_ilp, b( 1_ilp, jrow-1 ), 1_ilp, c,s )
                 if( ilz )call stdlib_srot( n, z( 1_ilp, jrow ), 1_ilp, z( 1_ilp, jrow-1 ), 1_ilp, c, s )
              end do
           end do
           return
     end subroutine stdlib_sgghrd

     pure module subroutine stdlib_dgghrd( compq, compz, n, ilo, ihi, a, lda, b, ldb, q,ldq, z, ldz, &
     !! DGGHRD reduces a pair of real matrices (A,B) to generalized upper
     !! Hessenberg form using orthogonal transformations, where A is a
     !! general matrix and B is upper triangular.  The form of the
     !! generalized eigenvalue problem is
     !! A*x = lambda*B*x,
     !! and B is typically made upper triangular by computing its QR
     !! factorization and moving the orthogonal matrix Q to the left side
     !! of the equation.
     !! This subroutine simultaneously reduces A to a Hessenberg matrix H:
     !! Q**T*A*Z = H
     !! and transforms B to another upper triangular matrix T:
     !! Q**T*B*Z = T
     !! in order to reduce the problem to its standard form
     !! H*y = lambda*T*y
     !! where y = Z**T*x.
     !! The orthogonal matrices Q and Z are determined as products of Givens
     !! rotations.  They may either be formed explicitly, or they may be
     !! postmultiplied into input matrices Q1 and Z1, so that
     !! Q1 * A * Z1**T = (Q1*Q) * H * (Z1*Z)**T
     !! Q1 * B * Z1**T = (Q1*Q) * T * (Z1*Z)**T
     !! If Q1 is the orthogonal matrix from the QR factorization of B in the
     !! original equation A*x = lambda*B*x, then DGGHRD reduces the original
     !! problem to generalized Hessenberg form.
               info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: compq, compz
           integer(ilp), intent(in) :: ihi, ilo, lda, ldb, ldq, ldz, n
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*), b(ldb,*), q(ldq,*), z(ldz,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: ilq, ilz
           integer(ilp) :: icompq, icompz, jcol, jrow
           real(dp) :: c, s, temp
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode compq
           if( stdlib_lsame( compq, 'N' ) ) then
              ilq = .false.
              icompq = 1_ilp
           else if( stdlib_lsame( compq, 'V' ) ) then
              ilq = .true.
              icompq = 2_ilp
           else if( stdlib_lsame( compq, 'I' ) ) then
              ilq = .true.
              icompq = 3_ilp
           else
              icompq = 0_ilp
           end if
           ! decode compz
           if( stdlib_lsame( compz, 'N' ) ) then
              ilz = .false.
              icompz = 1_ilp
           else if( stdlib_lsame( compz, 'V' ) ) then
              ilz = .true.
              icompz = 2_ilp
           else if( stdlib_lsame( compz, 'I' ) ) then
              ilz = .true.
              icompz = 3_ilp
           else
              icompz = 0_ilp
           end if
           ! test the input parameters.
           info = 0_ilp
           if( icompq<=0_ilp ) then
              info = -1_ilp
           else if( icompz<=0_ilp ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( ilo<1_ilp ) then
              info = -4_ilp
           else if( ihi>n .or. ihi<ilo-1 ) then
              info = -5_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -7_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -9_ilp
           else if( ( ilq .and. ldq<n ) .or. ldq<1_ilp ) then
              info = -11_ilp
           else if( ( ilz .and. ldz<n ) .or. ldz<1_ilp ) then
              info = -13_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DGGHRD', -info )
              return
           end if
           ! initialize q and z if desired.
           if( icompq==3_ilp )call stdlib_dlaset( 'FULL', n, n, zero, one, q, ldq )
           if( icompz==3_ilp )call stdlib_dlaset( 'FULL', n, n, zero, one, z, ldz )
           ! quick return if possible
           if( n<=1 )return
           ! zero out lower triangle of b
           do jcol = 1, n - 1
              do jrow = jcol + 1, n
                 b( jrow, jcol ) = zero
              end do
           end do
           ! reduce a and b
           do jcol = ilo, ihi - 2
              do jrow = ihi, jcol + 2, -1
                 ! step 1: rotate rows jrow-1, jrow to kill a(jrow,jcol)
                 temp = a( jrow-1, jcol )
                 call stdlib_dlartg( temp, a( jrow, jcol ), c, s,a( jrow-1, jcol ) )
                 a( jrow, jcol ) = zero
                 call stdlib_drot( n-jcol, a( jrow-1, jcol+1 ), lda,a( jrow, jcol+1 ), lda, c, s )
                           
                 call stdlib_drot( n+2-jrow, b( jrow-1, jrow-1 ), ldb,b( jrow, jrow-1 ), ldb, c, &
                           s )
                 if( ilq )call stdlib_drot( n, q( 1_ilp, jrow-1 ), 1_ilp, q( 1_ilp, jrow ), 1_ilp, c, s )
                 ! step 2: rotate columns jrow, jrow-1 to kill b(jrow,jrow-1)
                 temp = b( jrow, jrow )
                 call stdlib_dlartg( temp, b( jrow, jrow-1 ), c, s,b( jrow, jrow ) )
                 b( jrow, jrow-1 ) = zero
                 call stdlib_drot( ihi, a( 1_ilp, jrow ), 1_ilp, a( 1_ilp, jrow-1 ), 1_ilp, c, s )
                 call stdlib_drot( jrow-1, b( 1_ilp, jrow ), 1_ilp, b( 1_ilp, jrow-1 ), 1_ilp, c,s )
                 if( ilz )call stdlib_drot( n, z( 1_ilp, jrow ), 1_ilp, z( 1_ilp, jrow-1 ), 1_ilp, c, s )
              end do
           end do
           return
     end subroutine stdlib_dgghrd


     pure module subroutine stdlib_cgghrd( compq, compz, n, ilo, ihi, a, lda, b, ldb, q,ldq, z, ldz, &
     !! CGGHRD reduces a pair of complex matrices (A,B) to generalized upper
     !! Hessenberg form using unitary transformations, where A is a
     !! general matrix and B is upper triangular.  The form of the generalized
     !! eigenvalue problem is
     !! A*x = lambda*B*x,
     !! and B is typically made upper triangular by computing its QR
     !! factorization and moving the unitary matrix Q to the left side
     !! of the equation.
     !! This subroutine simultaneously reduces A to a Hessenberg matrix H:
     !! Q**H*A*Z = H
     !! and transforms B to another upper triangular matrix T:
     !! Q**H*B*Z = T
     !! in order to reduce the problem to its standard form
     !! H*y = lambda*T*y
     !! where y = Z**H*x.
     !! The unitary matrices Q and Z are determined as products of Givens
     !! rotations.  They may either be formed explicitly, or they may be
     !! postmultiplied into input matrices Q1 and Z1, so that
     !! Q1 * A * Z1**H = (Q1*Q) * H * (Z1*Z)**H
     !! Q1 * B * Z1**H = (Q1*Q) * T * (Z1*Z)**H
     !! If Q1 is the unitary matrix from the QR factorization of B in the
     !! original equation A*x = lambda*B*x, then CGGHRD reduces the original
     !! problem to generalized Hessenberg form.
               info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: compq, compz
           integer(ilp), intent(in) :: ihi, ilo, lda, ldb, ldq, ldz, n
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*), q(ldq,*), z(ldz,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: ilq, ilz
           integer(ilp) :: icompq, icompz, jcol, jrow
           real(sp) :: c
           complex(sp) :: ctemp, s
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode compq
           if( stdlib_lsame( compq, 'N' ) ) then
              ilq = .false.
              icompq = 1_ilp
           else if( stdlib_lsame( compq, 'V' ) ) then
              ilq = .true.
              icompq = 2_ilp
           else if( stdlib_lsame( compq, 'I' ) ) then
              ilq = .true.
              icompq = 3_ilp
           else
              icompq = 0_ilp
           end if
           ! decode compz
           if( stdlib_lsame( compz, 'N' ) ) then
              ilz = .false.
              icompz = 1_ilp
           else if( stdlib_lsame( compz, 'V' ) ) then
              ilz = .true.
              icompz = 2_ilp
           else if( stdlib_lsame( compz, 'I' ) ) then
              ilz = .true.
              icompz = 3_ilp
           else
              icompz = 0_ilp
           end if
           ! test the input parameters.
           info = 0_ilp
           if( icompq<=0_ilp ) then
              info = -1_ilp
           else if( icompz<=0_ilp ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( ilo<1_ilp ) then
              info = -4_ilp
           else if( ihi>n .or. ihi<ilo-1 ) then
              info = -5_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -7_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -9_ilp
           else if( ( ilq .and. ldq<n ) .or. ldq<1_ilp ) then
              info = -11_ilp
           else if( ( ilz .and. ldz<n ) .or. ldz<1_ilp ) then
              info = -13_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CGGHRD', -info )
              return
           end if
           ! initialize q and z if desired.
           if( icompq==3_ilp )call stdlib_claset( 'FULL', n, n, czero, cone, q, ldq )
           if( icompz==3_ilp )call stdlib_claset( 'FULL', n, n, czero, cone, z, ldz )
           ! quick return if possible
           if( n<=1 )return
           ! zero out lower triangle of b
           do jcol = 1, n - 1
              do jrow = jcol + 1, n
                 b( jrow, jcol ) = czero
              end do
           end do
           ! reduce a and b
           do jcol = ilo, ihi - 2
              do jrow = ihi, jcol + 2, -1
                 ! step 1: rotate rows jrow-1, jrow to kill a(jrow,jcol)
                 ctemp = a( jrow-1, jcol )
                 call stdlib_clartg( ctemp, a( jrow, jcol ), c, s,a( jrow-1, jcol ) )
                 a( jrow, jcol ) = czero
                 call stdlib_crot( n-jcol, a( jrow-1, jcol+1 ), lda,a( jrow, jcol+1 ), lda, c, s )
                           
                 call stdlib_crot( n+2-jrow, b( jrow-1, jrow-1 ), ldb,b( jrow, jrow-1 ), ldb, c, &
                           s )
                 if( ilq )call stdlib_crot( n, q( 1_ilp, jrow-1 ), 1_ilp, q( 1_ilp, jrow ), 1_ilp, c,conjg( s ) )
                           
                 ! step 2: rotate columns jrow, jrow-1 to kill b(jrow,jrow-1)
                 ctemp = b( jrow, jrow )
                 call stdlib_clartg( ctemp, b( jrow, jrow-1 ), c, s,b( jrow, jrow ) )
                 b( jrow, jrow-1 ) = czero
                 call stdlib_crot( ihi, a( 1_ilp, jrow ), 1_ilp, a( 1_ilp, jrow-1 ), 1_ilp, c, s )
                 call stdlib_crot( jrow-1, b( 1_ilp, jrow ), 1_ilp, b( 1_ilp, jrow-1 ), 1_ilp, c,s )
                 if( ilz )call stdlib_crot( n, z( 1_ilp, jrow ), 1_ilp, z( 1_ilp, jrow-1 ), 1_ilp, c, s )
              end do
           end do
           return
     end subroutine stdlib_cgghrd

     pure module subroutine stdlib_zgghrd( compq, compz, n, ilo, ihi, a, lda, b, ldb, q,ldq, z, ldz, &
     !! ZGGHRD reduces a pair of complex matrices (A,B) to generalized upper
     !! Hessenberg form using unitary transformations, where A is a
     !! general matrix and B is upper triangular.  The form of the
     !! generalized eigenvalue problem is
     !! A*x = lambda*B*x,
     !! and B is typically made upper triangular by computing its QR
     !! factorization and moving the unitary matrix Q to the left side
     !! of the equation.
     !! This subroutine simultaneously reduces A to a Hessenberg matrix H:
     !! Q**H*A*Z = H
     !! and transforms B to another upper triangular matrix T:
     !! Q**H*B*Z = T
     !! in order to reduce the problem to its standard form
     !! H*y = lambda*T*y
     !! where y = Z**H*x.
     !! The unitary matrices Q and Z are determined as products of Givens
     !! rotations.  They may either be formed explicitly, or they may be
     !! postmultiplied into input matrices Q1 and Z1, so that
     !! Q1 * A * Z1**H = (Q1*Q) * H * (Z1*Z)**H
     !! Q1 * B * Z1**H = (Q1*Q) * T * (Z1*Z)**H
     !! If Q1 is the unitary matrix from the QR factorization of B in the
     !! original equation A*x = lambda*B*x, then ZGGHRD reduces the original
     !! problem to generalized Hessenberg form.
               info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: compq, compz
           integer(ilp), intent(in) :: ihi, ilo, lda, ldb, ldq, ldz, n
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*), q(ldq,*), z(ldz,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: ilq, ilz
           integer(ilp) :: icompq, icompz, jcol, jrow
           real(dp) :: c
           complex(dp) :: ctemp, s
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode compq
           if( stdlib_lsame( compq, 'N' ) ) then
              ilq = .false.
              icompq = 1_ilp
           else if( stdlib_lsame( compq, 'V' ) ) then
              ilq = .true.
              icompq = 2_ilp
           else if( stdlib_lsame( compq, 'I' ) ) then
              ilq = .true.
              icompq = 3_ilp
           else
              icompq = 0_ilp
           end if
           ! decode compz
           if( stdlib_lsame( compz, 'N' ) ) then
              ilz = .false.
              icompz = 1_ilp
           else if( stdlib_lsame( compz, 'V' ) ) then
              ilz = .true.
              icompz = 2_ilp
           else if( stdlib_lsame( compz, 'I' ) ) then
              ilz = .true.
              icompz = 3_ilp
           else
              icompz = 0_ilp
           end if
           ! test the input parameters.
           info = 0_ilp
           if( icompq<=0_ilp ) then
              info = -1_ilp
           else if( icompz<=0_ilp ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( ilo<1_ilp ) then
              info = -4_ilp
           else if( ihi>n .or. ihi<ilo-1 ) then
              info = -5_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -7_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -9_ilp
           else if( ( ilq .and. ldq<n ) .or. ldq<1_ilp ) then
              info = -11_ilp
           else if( ( ilz .and. ldz<n ) .or. ldz<1_ilp ) then
              info = -13_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZGGHRD', -info )
              return
           end if
           ! initialize q and z if desired.
           if( icompq==3_ilp )call stdlib_zlaset( 'FULL', n, n, czero, cone, q, ldq )
           if( icompz==3_ilp )call stdlib_zlaset( 'FULL', n, n, czero, cone, z, ldz )
           ! quick return if possible
           if( n<=1 )return
           ! zero out lower triangle of b
           do jcol = 1, n - 1
              do jrow = jcol + 1, n
                 b( jrow, jcol ) = czero
              end do
           end do
           ! reduce a and b
           do jcol = ilo, ihi - 2
              do jrow = ihi, jcol + 2, -1
                 ! step 1: rotate rows jrow-1, jrow to kill a(jrow,jcol)
                 ctemp = a( jrow-1, jcol )
                 call stdlib_zlartg( ctemp, a( jrow, jcol ), c, s,a( jrow-1, jcol ) )
                 a( jrow, jcol ) = czero
                 call stdlib_zrot( n-jcol, a( jrow-1, jcol+1 ), lda,a( jrow, jcol+1 ), lda, c, s )
                           
                 call stdlib_zrot( n+2-jrow, b( jrow-1, jrow-1 ), ldb,b( jrow, jrow-1 ), ldb, c, &
                           s )
                 if( ilq )call stdlib_zrot( n, q( 1_ilp, jrow-1 ), 1_ilp, q( 1_ilp, jrow ), 1_ilp, c,conjg( s ) )
                           
                 ! step 2: rotate columns jrow, jrow-1 to kill b(jrow,jrow-1)
                 ctemp = b( jrow, jrow )
                 call stdlib_zlartg( ctemp, b( jrow, jrow-1 ), c, s,b( jrow, jrow ) )
                 b( jrow, jrow-1 ) = czero
                 call stdlib_zrot( ihi, a( 1_ilp, jrow ), 1_ilp, a( 1_ilp, jrow-1 ), 1_ilp, c, s )
                 call stdlib_zrot( jrow-1, b( 1_ilp, jrow ), 1_ilp, b( 1_ilp, jrow-1 ), 1_ilp, c,s )
                 if( ilz )call stdlib_zrot( n, z( 1_ilp, jrow ), 1_ilp, z( 1_ilp, jrow-1 ), 1_ilp, c, s )
              end do
           end do
           return
     end subroutine stdlib_zgghrd




     pure module subroutine stdlib_sgghd3( compq, compz, n, ilo, ihi, a, lda, b, ldb, q,ldq, z, ldz, &
     !! SGGHD3 reduces a pair of real matrices (A,B) to generalized upper
     !! Hessenberg form using orthogonal transformations, where A is a
     !! general matrix and B is upper triangular.  The form of the
     !! generalized eigenvalue problem is
     !! A*x = lambda*B*x,
     !! and B is typically made upper triangular by computing its QR
     !! factorization and moving the orthogonal matrix Q to the left side
     !! of the equation.
     !! This subroutine simultaneously reduces A to a Hessenberg matrix H:
     !! Q**T*A*Z = H
     !! and transforms B to another upper triangular matrix T:
     !! Q**T*B*Z = T
     !! in order to reduce the problem to its standard form
     !! H*y = lambda*T*y
     !! where y = Z**T*x.
     !! The orthogonal matrices Q and Z are determined as products of Givens
     !! rotations.  They may either be formed explicitly, or they may be
     !! postmultiplied into input matrices Q1 and Z1, so that
     !! Q1 * A * Z1**T = (Q1*Q) * H * (Z1*Z)**T
     !! Q1 * B * Z1**T = (Q1*Q) * T * (Z1*Z)**T
     !! If Q1 is the orthogonal matrix from the QR factorization of B in the
     !! original equation A*x = lambda*B*x, then SGGHD3 reduces the original
     !! problem to generalized Hessenberg form.
     !! This is a blocked variant of SGGHRD, using matrix-matrix
     !! multiplications for parts of the computation to enhance performance.
               work, lwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: compq, compz
           integer(ilp), intent(in) :: ihi, ilo, lda, ldb, ldq, ldz, n, lwork
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*), b(ldb,*), q(ldq,*), z(ldz,*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: blk22, initq, initz, lquery, wantq, wantz
           character :: compq2, compz2
           integer(ilp) :: cola, i, ierr, j, j0, jcol, jj, jrow, k, kacc22, len, lwkopt, n2nb, nb,&
                      nblst, nbmin, nh, nnb, nx, ppw, ppwo, pw, top, topq
           real(sp) :: c, c1, c2, s, s1, s2, temp, temp1, temp2, temp3
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode and test the input parameters.
           info = 0_ilp
           nb = stdlib_ilaenv( 1_ilp, 'SGGHD3', ' ', n, ilo, ihi, -1_ilp )
           lwkopt = max( 6_ilp*n*nb, 1_ilp )
           work( 1_ilp ) = real( lwkopt,KIND=sp)
           initq = stdlib_lsame( compq, 'I' )
           wantq = initq .or. stdlib_lsame( compq, 'V' )
           initz = stdlib_lsame( compz, 'I' )
           wantz = initz .or. stdlib_lsame( compz, 'V' )
           lquery = ( lwork==-1_ilp )
           if( .not.stdlib_lsame( compq, 'N' ) .and. .not.wantq ) then
              info = -1_ilp
           else if( .not.stdlib_lsame( compz, 'N' ) .and. .not.wantz ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( ilo<1_ilp ) then
              info = -4_ilp
           else if( ihi>n .or. ihi<ilo-1 ) then
              info = -5_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -7_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -9_ilp
           else if( ( wantq .and. ldq<n ) .or. ldq<1_ilp ) then
              info = -11_ilp
           else if( ( wantz .and. ldz<n ) .or. ldz<1_ilp ) then
              info = -13_ilp
           else if( lwork<1_ilp .and. .not.lquery ) then
              info = -15_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SGGHD3', -info )
              return
           else if( lquery ) then
              return
           end if
           ! initialize q and z if desired.
           if( initq )call stdlib_slaset( 'ALL', n, n, zero, one, q, ldq )
           if( initz )call stdlib_slaset( 'ALL', n, n, zero, one, z, ldz )
           ! zero out lower triangle of b.
           if( n>1_ilp )call stdlib_slaset( 'LOWER', n-1, n-1, zero, zero, b(2_ilp, 1_ilp), ldb )
           ! quick return if possible
           nh = ihi - ilo + 1_ilp
           if( nh<=1_ilp ) then
              work( 1_ilp ) = one
              return
           end if
           ! determine the blocksize.
           nbmin = stdlib_ilaenv( 2_ilp, 'SGGHD3', ' ', n, ilo, ihi, -1_ilp )
           if( nb>1_ilp .and. nb<nh ) then
              ! determine when to use unblocked instead of blocked code.
              nx = max( nb, stdlib_ilaenv( 3_ilp, 'SGGHD3', ' ', n, ilo, ihi, -1_ilp ) )
              if( nx<nh ) then
                 ! determine if workspace is large enough for blocked code.
                 if( lwork<lwkopt ) then
                    ! not enough workspace to use optimal nb:  determine the
                    ! minimum value of nb, and reduce nb or force use of
                    ! unblocked code.
                    nbmin = max( 2_ilp, stdlib_ilaenv( 2_ilp, 'SGGHD3', ' ', n, ilo, ihi,-1_ilp ) )
                    if( lwork>=6_ilp*n*nbmin ) then
                       nb = lwork / ( 6_ilp*n )
                    else
                       nb = 1_ilp
                    end if
                 end if
              end if
           end if
           if( nb<nbmin .or. nb>=nh ) then
              ! use unblocked code below
              jcol = ilo
           else
              ! use blocked code
              kacc22 = stdlib_ilaenv( 16_ilp, 'SGGHD3', ' ', n, ilo, ihi, -1_ilp )
              blk22 = kacc22==2_ilp
              do jcol = ilo, ihi-2, nb
                 nnb = min( nb, ihi-jcol-1 )
                 ! initialize small orthogonal factors that will hold the
                 ! accumulated givens rotations in workspace.
                 ! n2nb   denotes the number of 2*nnb-by-2*nnb factors
                 ! nblst  denotes the (possibly smaller) order of the last
                        ! factor.
                 n2nb = ( ihi-jcol-1 ) / nnb - 1_ilp
                 nblst = ihi - jcol - n2nb*nnb
                 call stdlib_slaset( 'ALL', nblst, nblst, zero, one, work, nblst )
                 pw = nblst * nblst + 1_ilp
                 do i = 1, n2nb
                    call stdlib_slaset( 'ALL', 2_ilp*nnb, 2_ilp*nnb, zero, one,work( pw ), 2_ilp*nnb )
                    pw = pw + 4_ilp*nnb*nnb
                 end do
                 ! reduce columns jcol:jcol+nnb-1 of a to hessenberg form.
                 do j = jcol, jcol+nnb-1
                    ! reduce jth column of a. store cosines and sines in jth
                    ! column of a and b, respectively.
                    do i = ihi, j+2, -1
                       temp = a( i-1, j )
                       call stdlib_slartg( temp, a( i, j ), c, s, a( i-1, j ) )
                       a( i, j ) = c
                       b( i, j ) = s
                    end do
                    ! accumulate givens rotations into workspace array.
                    ppw  = ( nblst + 1_ilp )*( nblst - 2_ilp ) - j + jcol + 1_ilp
                    len  = 2_ilp + j - jcol
                    jrow = j + n2nb*nnb + 2_ilp
                    do i = ihi, jrow, -1
                       c = a( i, j )
                       s = b( i, j )
                       do jj = ppw, ppw+len-1
                          temp = work( jj + nblst )
                          work( jj + nblst ) = c*temp - s*work( jj )
                          work( jj ) = s*temp + c*work( jj )
                       end do
                       len = len + 1_ilp
                       ppw = ppw - nblst - 1_ilp
                    end do
                    ppwo = nblst*nblst + ( nnb+j-jcol-1 )*2_ilp*nnb + nnb
                    j0 = jrow - nnb
                    do jrow = j0, j+2, -nnb
                       ppw = ppwo
                       len  = 2_ilp + j - jcol
                       do i = jrow+nnb-1, jrow, -1
                          c = a( i, j )
                          s = b( i, j )
                          do jj = ppw, ppw+len-1
                             temp = work( jj + 2_ilp*nnb )
                             work( jj + 2_ilp*nnb ) = c*temp - s*work( jj )
                             work( jj ) = s*temp + c*work( jj )
                          end do
                          len = len + 1_ilp
                          ppw = ppw - 2_ilp*nnb - 1_ilp
                       end do
                       ppwo = ppwo + 4_ilp*nnb*nnb
                    end do
                    ! top denotes the number of top rows in a and b that will
                    ! not be updated during the next steps.
                    if( jcol<=2_ilp ) then
                       top = 0_ilp
                    else
                       top = jcol
                    end if
                    ! propagate transformations through b and replace stored
                    ! left sines/cosines by right sines/cosines.
                    do jj = n, j+1, -1
                       ! update jjth column of b.
                       do i = min( jj+1, ihi ), j+2, -1
                          c = a( i, j )
                          s = b( i, j )
                          temp = b( i, jj )
                          b( i, jj ) = c*temp - s*b( i-1, jj )
                          b( i-1, jj ) = s*temp + c*b( i-1, jj )
                       end do
                       ! annihilate b( jj+1, jj ).
                       if( jj<ihi ) then
                          temp = b( jj+1, jj+1 )
                          call stdlib_slartg( temp, b( jj+1, jj ), c, s,b( jj+1, jj+1 ) )
                          b( jj+1, jj ) = zero
                          call stdlib_srot( jj-top, b( top+1, jj+1 ), 1_ilp,b( top+1, jj ), 1_ilp, c, s )
                                    
                          a( jj+1, j ) = c
                          b( jj+1, j ) = -s
                       end if
                    end do
                    ! update a by transformations from right.
                    ! explicit loop unrolling provides better performance
                    ! compared to stdlib_slasr.
                     ! call stdlib_slasr( 'right', 'variable', 'backward', ihi-top,
           ! $                     ihi-j, a( j+2, j ), b( j+2, j ),
           ! $                     a( top+1, j+1 ), lda )
                    jj = mod( ihi-j-1, 3_ilp )
                    do i = ihi-j-3, jj+1, -3
                       c = a( j+1+i, j )
                       s = -b( j+1+i, j )
                       c1 = a( j+2+i, j )
                       s1 = -b( j+2+i, j )
                       c2 = a( j+3+i, j )
                       s2 = -b( j+3+i, j )
                       do k = top+1, ihi
                          temp = a( k, j+i  )
                          temp1 = a( k, j+i+1 )
                          temp2 = a( k, j+i+2 )
                          temp3 = a( k, j+i+3 )
                          a( k, j+i+3 ) = c2*temp3 + s2*temp2
                          temp2 = -s2*temp3 + c2*temp2
                          a( k, j+i+2 ) = c1*temp2 + s1*temp1
                          temp1 = -s1*temp2 + c1*temp1
                          a( k, j+i+1 ) = c*temp1 + s*temp
                          a( k, j+i ) = -s*temp1 + c*temp
                       end do
                    end do
                    if( jj>0_ilp ) then
                       do i = jj, 1, -1
                          call stdlib_srot( ihi-top, a( top+1, j+i+1 ), 1_ilp,a( top+1, j+i ), 1_ilp, a( &
                                    j+1+i, j ),-b( j+1+i, j ) )
                       end do
                    end if
                    ! update (j+1)th column of a by transformations from left.
                    if ( j < jcol + nnb - 1_ilp ) then
                       len  = 1_ilp + j - jcol
                       ! multiply with the trailing accumulated orthogonal
                       ! matrix, which takes the form
                              ! [  u11  u12  ]
                          ! u = [            ],
                              ! [  u21  u22  ]
                       ! where u21 is a len-by-len matrix and u12 is lower
                       ! triangular.
                       jrow = ihi - nblst + 1_ilp
                       call stdlib_sgemv( 'TRANSPOSE', nblst, len, one, work,nblst, a( jrow, j+1 )&
                                 , 1_ilp, zero,work( pw ), 1_ilp )
                       ppw = pw + len
                       do i = jrow, jrow+nblst-len-1
                          work( ppw ) = a( i, j+1 )
                          ppw = ppw + 1_ilp
                       end do
                       call stdlib_strmv( 'LOWER', 'TRANSPOSE', 'NON-UNIT',nblst-len, work( &
                                 len*nblst + 1_ilp ), nblst,work( pw+len ), 1_ilp )
                       call stdlib_sgemv( 'TRANSPOSE', len, nblst-len, one,work( (len+1)*nblst - &
                       len + 1_ilp ), nblst,a( jrow+nblst-len, j+1 ), 1_ilp, one,work( pw+len ), 1_ilp )
                                 
                       ppw = pw
                       do i = jrow, jrow+nblst-1
                          a( i, j+1 ) = work( ppw )
                          ppw = ppw + 1_ilp
                       end do
                       ! multiply with the other accumulated orthogonal
                       ! matrices, which take the form
                              ! [  u11  u12   0  ]
                              ! [                ]
                          ! u = [  u21  u22   0  ],
                              ! [                ]
                              ! [   0    0    i  ]
                       ! where i denotes the (nnb-len)-by-(nnb-len) identity
                       ! matrix, u21 is a len-by-len upper triangular matrix
                       ! and u12 is an nnb-by-nnb lower triangular matrix.
                       ppwo = 1_ilp + nblst*nblst
                       j0 = jrow - nnb
                       do jrow = j0, jcol+1, -nnb
                          ppw = pw + len
                          do i = jrow, jrow+nnb-1
                             work( ppw ) = a( i, j+1 )
                             ppw = ppw + 1_ilp
                          end do
                          ppw = pw
                          do i = jrow+nnb, jrow+nnb+len-1
                             work( ppw ) = a( i, j+1 )
                             ppw = ppw + 1_ilp
                          end do
                          call stdlib_strmv( 'UPPER', 'TRANSPOSE', 'NON-UNIT', len,work( ppwo + &
                                    nnb ), 2_ilp*nnb, work( pw ),1_ilp )
                          call stdlib_strmv( 'LOWER', 'TRANSPOSE', 'NON-UNIT', nnb,work( ppwo + &
                                    2_ilp*len*nnb ),2_ilp*nnb, work( pw + len ), 1_ilp )
                          call stdlib_sgemv( 'TRANSPOSE', nnb, len, one,work( ppwo ), 2_ilp*nnb, a( &
                                    jrow, j+1 ), 1_ilp,one, work( pw ), 1_ilp )
                          call stdlib_sgemv( 'TRANSPOSE', len, nnb, one,work( ppwo + 2_ilp*len*nnb + &
                                    nnb ), 2_ilp*nnb,a( jrow+nnb, j+1 ), 1_ilp, one,work( pw+len ), 1_ilp )
                          ppw = pw
                          do i = jrow, jrow+len+nnb-1
                             a( i, j+1 ) = work( ppw )
                             ppw = ppw + 1_ilp
                          end do
                          ppwo = ppwo + 4_ilp*nnb*nnb
                       end do
                    end if
                 end do
                 ! apply accumulated orthogonal matrices to a.
                 cola = n - jcol - nnb + 1_ilp
                 j = ihi - nblst + 1_ilp
                 call stdlib_sgemm( 'TRANSPOSE', 'NO TRANSPOSE', nblst,cola, nblst, one, work, &
                           nblst,a( j, jcol+nnb ), lda, zero, work( pw ),nblst )
                 call stdlib_slacpy( 'ALL', nblst, cola, work( pw ), nblst,a( j, jcol+nnb ), lda )
                           
                 ppwo = nblst*nblst + 1_ilp
                 j0 = j - nnb
                 do j = j0, jcol+1, -nnb
                    if ( blk22 ) then
                       ! exploit the structure of
                              ! [  u11  u12  ]
                          ! u = [            ]
                              ! [  u21  u22  ],
                       ! where all blocks are nnb-by-nnb, u21 is upper
                       ! triangular and u12 is lower triangular.
                       call stdlib_sorm22( 'LEFT', 'TRANSPOSE', 2_ilp*nnb, cola, nnb,nnb, work( ppwo )&
                                 , 2_ilp*nnb,a( j, jcol+nnb ), lda, work( pw ),lwork-pw+1, ierr )
                    else
                       ! ignore the structure of u.
                       call stdlib_sgemm( 'TRANSPOSE', 'NO TRANSPOSE', 2_ilp*nnb,cola, 2_ilp*nnb, one, &
                                 work( ppwo ), 2_ilp*nnb,a( j, jcol+nnb ), lda, zero, work( pw ),2_ilp*nnb )
                       call stdlib_slacpy( 'ALL', 2_ilp*nnb, cola, work( pw ), 2_ilp*nnb,a( j, jcol+nnb ),&
                                  lda )
                    end if
                    ppwo = ppwo + 4_ilp*nnb*nnb
                 end do
                 ! apply accumulated orthogonal matrices to q.
                 if( wantq ) then
                    j = ihi - nblst + 1_ilp
                    if ( initq ) then
                       topq = max( 2_ilp, j - jcol + 1_ilp )
                       nh  = ihi - topq + 1_ilp
                    else
                       topq = 1_ilp
                       nh = n
                    end if
                    call stdlib_sgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', nh,nblst, nblst, one, q( &
                              topq, j ), ldq,work, nblst, zero, work( pw ), nh )
                    call stdlib_slacpy( 'ALL', nh, nblst, work( pw ), nh,q( topq, j ), ldq )
                              
                    ppwo = nblst*nblst + 1_ilp
                    j0 = j - nnb
                    do j = j0, jcol+1, -nnb
                       if ( initq ) then
                          topq = max( 2_ilp, j - jcol + 1_ilp )
                          nh  = ihi - topq + 1_ilp
                       end if
                       if ( blk22 ) then
                          ! exploit the structure of u.
                          call stdlib_sorm22( 'RIGHT', 'NO TRANSPOSE', nh, 2_ilp*nnb,nnb, nnb, work( &
                                    ppwo ), 2_ilp*nnb,q( topq, j ), ldq, work( pw ),lwork-pw+1, ierr )
                       else
                          ! ignore the structure of u.
                          call stdlib_sgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', nh,2_ilp*nnb, 2_ilp*nnb, one,&
                                     q( topq, j ), ldq,work( ppwo ), 2_ilp*nnb, zero, work( pw ),nh )
                          call stdlib_slacpy( 'ALL', nh, 2_ilp*nnb, work( pw ), nh,q( topq, j ), ldq )
                                    
                       end if
                       ppwo = ppwo + 4_ilp*nnb*nnb
                    end do
                 end if
                 ! accumulate right givens rotations if required.
                 if ( wantz .or. top>0_ilp ) then
                    ! initialize small orthogonal factors that will hold the
                    ! accumulated givens rotations in workspace.
                    call stdlib_slaset( 'ALL', nblst, nblst, zero, one, work,nblst )
                    pw = nblst * nblst + 1_ilp
                    do i = 1, n2nb
                       call stdlib_slaset( 'ALL', 2_ilp*nnb, 2_ilp*nnb, zero, one,work( pw ), 2_ilp*nnb )
                                 
                       pw = pw + 4_ilp*nnb*nnb
                    end do
                    ! accumulate givens rotations into workspace array.
                    do j = jcol, jcol+nnb-1
                       ppw  = ( nblst + 1_ilp )*( nblst - 2_ilp ) - j + jcol + 1_ilp
                       len  = 2_ilp + j - jcol
                       jrow = j + n2nb*nnb + 2_ilp
                       do i = ihi, jrow, -1
                          c = a( i, j )
                          a( i, j ) = zero
                          s = b( i, j )
                          b( i, j ) = zero
                          do jj = ppw, ppw+len-1
                             temp = work( jj + nblst )
                             work( jj + nblst ) = c*temp - s*work( jj )
                             work( jj ) = s*temp + c*work( jj )
                          end do
                          len = len + 1_ilp
                          ppw = ppw - nblst - 1_ilp
                       end do
                       ppwo = nblst*nblst + ( nnb+j-jcol-1 )*2_ilp*nnb + nnb
                       j0 = jrow - nnb
                       do jrow = j0, j+2, -nnb
                          ppw = ppwo
                          len  = 2_ilp + j - jcol
                          do i = jrow+nnb-1, jrow, -1
                             c = a( i, j )
                             a( i, j ) = zero
                             s = b( i, j )
                             b( i, j ) = zero
                             do jj = ppw, ppw+len-1
                                temp = work( jj + 2_ilp*nnb )
                                work( jj + 2_ilp*nnb ) = c*temp - s*work( jj )
                                work( jj ) = s*temp + c*work( jj )
                             end do
                             len = len + 1_ilp
                             ppw = ppw - 2_ilp*nnb - 1_ilp
                          end do
                          ppwo = ppwo + 4_ilp*nnb*nnb
                       end do
                    end do
                 else
                    call stdlib_slaset( 'LOWER', ihi - jcol - 1_ilp, nnb, zero, zero,a( jcol + 2_ilp, &
                              jcol ), lda )
                    call stdlib_slaset( 'LOWER', ihi - jcol - 1_ilp, nnb, zero, zero,b( jcol + 2_ilp, &
                              jcol ), ldb )
                 end if
                 ! apply accumulated orthogonal matrices to a and b.
                 if ( top>0_ilp ) then
                    j = ihi - nblst + 1_ilp
                    call stdlib_sgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', top,nblst, nblst, one, a( &
                              1_ilp, j ), lda,work, nblst, zero, work( pw ), top )
                    call stdlib_slacpy( 'ALL', top, nblst, work( pw ), top,a( 1_ilp, j ), lda )
                              
                    ppwo = nblst*nblst + 1_ilp
                    j0 = j - nnb
                    do j = j0, jcol+1, -nnb
                       if ( blk22 ) then
                          ! exploit the structure of u.
                          call stdlib_sorm22( 'RIGHT', 'NO TRANSPOSE', top, 2_ilp*nnb,nnb, nnb, work( &
                                    ppwo ), 2_ilp*nnb,a( 1_ilp, j ), lda, work( pw ),lwork-pw+1, ierr )
                       else
                          ! ignore the structure of u.
                          call stdlib_sgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', top,2_ilp*nnb, 2_ilp*nnb, &
                                    one, a( 1_ilp, j ), lda,work( ppwo ), 2_ilp*nnb, zero,work( pw ), top )
                          call stdlib_slacpy( 'ALL', top, 2_ilp*nnb, work( pw ), top,a( 1_ilp, j ), lda )
                                    
                       end if
                       ppwo = ppwo + 4_ilp*nnb*nnb
                    end do
                    j = ihi - nblst + 1_ilp
                    call stdlib_sgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', top,nblst, nblst, one, b( &
                              1_ilp, j ), ldb,work, nblst, zero, work( pw ), top )
                    call stdlib_slacpy( 'ALL', top, nblst, work( pw ), top,b( 1_ilp, j ), ldb )
                              
                    ppwo = nblst*nblst + 1_ilp
                    j0 = j - nnb
                    do j = j0, jcol+1, -nnb
                       if ( blk22 ) then
                          ! exploit the structure of u.
                          call stdlib_sorm22( 'RIGHT', 'NO TRANSPOSE', top, 2_ilp*nnb,nnb, nnb, work( &
                                    ppwo ), 2_ilp*nnb,b( 1_ilp, j ), ldb, work( pw ),lwork-pw+1, ierr )
                       else
                          ! ignore the structure of u.
                          call stdlib_sgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', top,2_ilp*nnb, 2_ilp*nnb, &
                                    one, b( 1_ilp, j ), ldb,work( ppwo ), 2_ilp*nnb, zero,work( pw ), top )
                          call stdlib_slacpy( 'ALL', top, 2_ilp*nnb, work( pw ), top,b( 1_ilp, j ), ldb )
                                    
                       end if
                       ppwo = ppwo + 4_ilp*nnb*nnb
                    end do
                 end if
                 ! apply accumulated orthogonal matrices to z.
                 if( wantz ) then
                    j = ihi - nblst + 1_ilp
                    if ( initq ) then
                       topq = max( 2_ilp, j - jcol + 1_ilp )
                       nh  = ihi - topq + 1_ilp
                    else
                       topq = 1_ilp
                       nh = n
                    end if
                    call stdlib_sgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', nh,nblst, nblst, one, z( &
                              topq, j ), ldz,work, nblst, zero, work( pw ), nh )
                    call stdlib_slacpy( 'ALL', nh, nblst, work( pw ), nh,z( topq, j ), ldz )
                              
                    ppwo = nblst*nblst + 1_ilp
                    j0 = j - nnb
                    do j = j0, jcol+1, -nnb
                          if ( initq ) then
                          topq = max( 2_ilp, j - jcol + 1_ilp )
                          nh  = ihi - topq + 1_ilp
                       end if
                       if ( blk22 ) then
                          ! exploit the structure of u.
                          call stdlib_sorm22( 'RIGHT', 'NO TRANSPOSE', nh, 2_ilp*nnb,nnb, nnb, work( &
                                    ppwo ), 2_ilp*nnb,z( topq, j ), ldz, work( pw ),lwork-pw+1, ierr )
                       else
                          ! ignore the structure of u.
                          call stdlib_sgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', nh,2_ilp*nnb, 2_ilp*nnb, one,&
                                     z( topq, j ), ldz,work( ppwo ), 2_ilp*nnb, zero, work( pw ),nh )
                          call stdlib_slacpy( 'ALL', nh, 2_ilp*nnb, work( pw ), nh,z( topq, j ), ldz )
                                    
                       end if
                       ppwo = ppwo + 4_ilp*nnb*nnb
                    end do
                 end if
              end do
           end if
           ! use unblocked code to reduce the rest of the matrix
           ! avoid re-initialization of modified q and z.
           compq2 = compq
           compz2 = compz
           if ( jcol/=ilo ) then
              if ( wantq )compq2 = 'V'
              if ( wantz )compz2 = 'V'
           end if
           if ( jcol<ihi )call stdlib_sgghrd( compq2, compz2, n, jcol, ihi, a, lda, b, ldb, q,ldq,&
                      z, ldz, ierr )
           work( 1_ilp ) = real( lwkopt,KIND=sp)
           return
     end subroutine stdlib_sgghd3

     pure module subroutine stdlib_dgghd3( compq, compz, n, ilo, ihi, a, lda, b, ldb, q,ldq, z, ldz, &
     !! DGGHD3 reduces a pair of real matrices (A,B) to generalized upper
     !! Hessenberg form using orthogonal transformations, where A is a
     !! general matrix and B is upper triangular.  The form of the
     !! generalized eigenvalue problem is
     !! A*x = lambda*B*x,
     !! and B is typically made upper triangular by computing its QR
     !! factorization and moving the orthogonal matrix Q to the left side
     !! of the equation.
     !! This subroutine simultaneously reduces A to a Hessenberg matrix H:
     !! Q**T*A*Z = H
     !! and transforms B to another upper triangular matrix T:
     !! Q**T*B*Z = T
     !! in order to reduce the problem to its standard form
     !! H*y = lambda*T*y
     !! where y = Z**T*x.
     !! The orthogonal matrices Q and Z are determined as products of Givens
     !! rotations.  They may either be formed explicitly, or they may be
     !! postmultiplied into input matrices Q1 and Z1, so that
     !! Q1 * A * Z1**T = (Q1*Q) * H * (Z1*Z)**T
     !! Q1 * B * Z1**T = (Q1*Q) * T * (Z1*Z)**T
     !! If Q1 is the orthogonal matrix from the QR factorization of B in the
     !! original equation A*x = lambda*B*x, then DGGHD3 reduces the original
     !! problem to generalized Hessenberg form.
     !! This is a blocked variant of DGGHRD, using matrix-matrix
     !! multiplications for parts of the computation to enhance performance.
               work, lwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: compq, compz
           integer(ilp), intent(in) :: ihi, ilo, lda, ldb, ldq, ldz, n, lwork
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*), b(ldb,*), q(ldq,*), z(ldz,*)
           real(dp), intent(out) :: work(*)
       ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: blk22, initq, initz, lquery, wantq, wantz
           character :: compq2, compz2
           integer(ilp) :: cola, i, ierr, j, j0, jcol, jj, jrow, k, kacc22, len, lwkopt, n2nb, nb,&
                      nblst, nbmin, nh, nnb, nx, ppw, ppwo, pw, top, topq
           real(dp) :: c, c1, c2, s, s1, s2, temp, temp1, temp2, temp3
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode and test the input parameters.
           info = 0_ilp
           nb = stdlib_ilaenv( 1_ilp, 'DGGHD3', ' ', n, ilo, ihi, -1_ilp )
           lwkopt = max( 6_ilp*n*nb, 1_ilp )
           work( 1_ilp ) = real( lwkopt,KIND=dp)
           initq = stdlib_lsame( compq, 'I' )
           wantq = initq .or. stdlib_lsame( compq, 'V' )
           initz = stdlib_lsame( compz, 'I' )
           wantz = initz .or. stdlib_lsame( compz, 'V' )
           lquery = ( lwork==-1_ilp )
           if( .not.stdlib_lsame( compq, 'N' ) .and. .not.wantq ) then
              info = -1_ilp
           else if( .not.stdlib_lsame( compz, 'N' ) .and. .not.wantz ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( ilo<1_ilp ) then
              info = -4_ilp
           else if( ihi>n .or. ihi<ilo-1 ) then
              info = -5_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -7_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -9_ilp
           else if( ( wantq .and. ldq<n ) .or. ldq<1_ilp ) then
              info = -11_ilp
           else if( ( wantz .and. ldz<n ) .or. ldz<1_ilp ) then
              info = -13_ilp
           else if( lwork<1_ilp .and. .not.lquery ) then
              info = -15_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DGGHD3', -info )
              return
           else if( lquery ) then
              return
           end if
           ! initialize q and z if desired.
           if( initq )call stdlib_dlaset( 'ALL', n, n, zero, one, q, ldq )
           if( initz )call stdlib_dlaset( 'ALL', n, n, zero, one, z, ldz )
           ! zero out lower triangle of b.
           if( n>1_ilp )call stdlib_dlaset( 'LOWER', n-1, n-1, zero, zero, b(2_ilp, 1_ilp), ldb )
           ! quick return if possible
           nh = ihi - ilo + 1_ilp
           if( nh<=1_ilp ) then
              work( 1_ilp ) = one
              return
           end if
           ! determine the blocksize.
           nbmin = stdlib_ilaenv( 2_ilp, 'DGGHD3', ' ', n, ilo, ihi, -1_ilp )
           if( nb>1_ilp .and. nb<nh ) then
              ! determine when to use unblocked instead of blocked code.
              nx = max( nb, stdlib_ilaenv( 3_ilp, 'DGGHD3', ' ', n, ilo, ihi, -1_ilp ) )
              if( nx<nh ) then
                 ! determine if workspace is large enough for blocked code.
                 if( lwork<lwkopt ) then
                    ! not enough workspace to use optimal nb:  determine the
                    ! minimum value of nb, and reduce nb or force use of
                    ! unblocked code.
                    nbmin = max( 2_ilp, stdlib_ilaenv( 2_ilp, 'DGGHD3', ' ', n, ilo, ihi,-1_ilp ) )
                    if( lwork>=6_ilp*n*nbmin ) then
                       nb = lwork / ( 6_ilp*n )
                    else
                       nb = 1_ilp
                    end if
                 end if
              end if
           end if
           if( nb<nbmin .or. nb>=nh ) then
              ! use unblocked code below
              jcol = ilo
           else
              ! use blocked code
              kacc22 = stdlib_ilaenv( 16_ilp, 'DGGHD3', ' ', n, ilo, ihi, -1_ilp )
              blk22 = kacc22==2_ilp
              do jcol = ilo, ihi-2, nb
                 nnb = min( nb, ihi-jcol-1 )
                 ! initialize small orthogonal factors that will hold the
                 ! accumulated givens rotations in workspace.
                 ! n2nb   denotes the number of 2*nnb-by-2*nnb factors
                 ! nblst  denotes the (possibly smaller) order of the last
                        ! factor.
                 n2nb = ( ihi-jcol-1 ) / nnb - 1_ilp
                 nblst = ihi - jcol - n2nb*nnb
                 call stdlib_dlaset( 'ALL', nblst, nblst, zero, one, work, nblst )
                 pw = nblst * nblst + 1_ilp
                 do i = 1, n2nb
                    call stdlib_dlaset( 'ALL', 2_ilp*nnb, 2_ilp*nnb, zero, one,work( pw ), 2_ilp*nnb )
                    pw = pw + 4_ilp*nnb*nnb
                 end do
                 ! reduce columns jcol:jcol+nnb-1 of a to hessenberg form.
                 do j = jcol, jcol+nnb-1
                    ! reduce jth column of a. store cosines and sines in jth
                    ! column of a and b, respectively.
                    do i = ihi, j+2, -1
                       temp = a( i-1, j )
                       call stdlib_dlartg( temp, a( i, j ), c, s, a( i-1, j ) )
                       a( i, j ) = c
                       b( i, j ) = s
                    end do
                    ! accumulate givens rotations into workspace array.
                    ppw  = ( nblst + 1_ilp )*( nblst - 2_ilp ) - j + jcol + 1_ilp
                    len  = 2_ilp + j - jcol
                    jrow = j + n2nb*nnb + 2_ilp
                    do i = ihi, jrow, -1
                       c = a( i, j )
                       s = b( i, j )
                       do jj = ppw, ppw+len-1
                          temp = work( jj + nblst )
                          work( jj + nblst ) = c*temp - s*work( jj )
                          work( jj ) = s*temp + c*work( jj )
                       end do
                       len = len + 1_ilp
                       ppw = ppw - nblst - 1_ilp
                    end do
                    ppwo = nblst*nblst + ( nnb+j-jcol-1 )*2_ilp*nnb + nnb
                    j0 = jrow - nnb
                    do jrow = j0, j+2, -nnb
                       ppw = ppwo
                       len  = 2_ilp + j - jcol
                       do i = jrow+nnb-1, jrow, -1
                          c = a( i, j )
                          s = b( i, j )
                          do jj = ppw, ppw+len-1
                             temp = work( jj + 2_ilp*nnb )
                             work( jj + 2_ilp*nnb ) = c*temp - s*work( jj )
                             work( jj ) = s*temp + c*work( jj )
                          end do
                          len = len + 1_ilp
                          ppw = ppw - 2_ilp*nnb - 1_ilp
                       end do
                       ppwo = ppwo + 4_ilp*nnb*nnb
                    end do
                    ! top denotes the number of top rows in a and b that will
                    ! not be updated during the next steps.
                    if( jcol<=2_ilp ) then
                       top = 0_ilp
                    else
                       top = jcol
                    end if
                    ! propagate transformations through b and replace stored
                    ! left sines/cosines by right sines/cosines.
                    do jj = n, j+1, -1
                       ! update jjth column of b.
                       do i = min( jj+1, ihi ), j+2, -1
                          c = a( i, j )
                          s = b( i, j )
                          temp = b( i, jj )
                          b( i, jj ) = c*temp - s*b( i-1, jj )
                          b( i-1, jj ) = s*temp + c*b( i-1, jj )
                       end do
                       ! annihilate b( jj+1, jj ).
                       if( jj<ihi ) then
                          temp = b( jj+1, jj+1 )
                          call stdlib_dlartg( temp, b( jj+1, jj ), c, s,b( jj+1, jj+1 ) )
                          b( jj+1, jj ) = zero
                          call stdlib_drot( jj-top, b( top+1, jj+1 ), 1_ilp,b( top+1, jj ), 1_ilp, c, s )
                                    
                          a( jj+1, j ) = c
                          b( jj+1, j ) = -s
                       end if
                    end do
                    ! update a by transformations from right.
                    ! explicit loop unrolling provides better performance
                    ! compared to stdlib_dlasr.
                     ! call stdlib_dlasr( 'right', 'variable', 'backward', ihi-top,
           ! $                     ihi-j, a( j+2, j ), b( j+2, j ),
           ! $                     a( top+1, j+1 ), lda )
                    jj = mod( ihi-j-1, 3_ilp )
                    do i = ihi-j-3, jj+1, -3
                       c = a( j+1+i, j )
                       s = -b( j+1+i, j )
                       c1 = a( j+2+i, j )
                       s1 = -b( j+2+i, j )
                       c2 = a( j+3+i, j )
                       s2 = -b( j+3+i, j )
                       do k = top+1, ihi
                          temp = a( k, j+i  )
                          temp1 = a( k, j+i+1 )
                          temp2 = a( k, j+i+2 )
                          temp3 = a( k, j+i+3 )
                          a( k, j+i+3 ) = c2*temp3 + s2*temp2
                          temp2 = -s2*temp3 + c2*temp2
                          a( k, j+i+2 ) = c1*temp2 + s1*temp1
                          temp1 = -s1*temp2 + c1*temp1
                          a( k, j+i+1 ) = c*temp1 + s*temp
                          a( k, j+i ) = -s*temp1 + c*temp
                       end do
                    end do
                    if( jj>0_ilp ) then
                       do i = jj, 1, -1
                          call stdlib_drot( ihi-top, a( top+1, j+i+1 ), 1_ilp,a( top+1, j+i ), 1_ilp, a( &
                                    j+1+i, j ),-b( j+1+i, j ) )
                       end do
                    end if
                    ! update (j+1)th column of a by transformations from left.
                    if ( j < jcol + nnb - 1_ilp ) then
                       len  = 1_ilp + j - jcol
                       ! multiply with the trailing accumulated orthogonal
                       ! matrix, which takes the form
                              ! [  u11  u12  ]
                          ! u = [            ],
                              ! [  u21  u22  ]
                       ! where u21 is a len-by-len matrix and u12 is lower
                       ! triangular.
                       jrow = ihi - nblst + 1_ilp
                       call stdlib_dgemv( 'TRANSPOSE', nblst, len, one, work,nblst, a( jrow, j+1 )&
                                 , 1_ilp, zero,work( pw ), 1_ilp )
                       ppw = pw + len
                       do i = jrow, jrow+nblst-len-1
                          work( ppw ) = a( i, j+1 )
                          ppw = ppw + 1_ilp
                       end do
                       call stdlib_dtrmv( 'LOWER', 'TRANSPOSE', 'NON-UNIT',nblst-len, work( &
                                 len*nblst + 1_ilp ), nblst,work( pw+len ), 1_ilp )
                       call stdlib_dgemv( 'TRANSPOSE', len, nblst-len, one,work( (len+1)*nblst - &
                       len + 1_ilp ), nblst,a( jrow+nblst-len, j+1 ), 1_ilp, one,work( pw+len ), 1_ilp )
                                 
                       ppw = pw
                       do i = jrow, jrow+nblst-1
                          a( i, j+1 ) = work( ppw )
                          ppw = ppw + 1_ilp
                       end do
                       ! multiply with the other accumulated orthogonal
                       ! matrices, which take the form
                              ! [  u11  u12   0  ]
                              ! [                ]
                          ! u = [  u21  u22   0  ],
                              ! [                ]
                              ! [   0    0    i  ]
                       ! where i denotes the (nnb-len)-by-(nnb-len) identity
                       ! matrix, u21 is a len-by-len upper triangular matrix
                       ! and u12 is an nnb-by-nnb lower triangular matrix.
                       ppwo = 1_ilp + nblst*nblst
                       j0 = jrow - nnb
                       do jrow = j0, jcol+1, -nnb
                          ppw = pw + len
                          do i = jrow, jrow+nnb-1
                             work( ppw ) = a( i, j+1 )
                             ppw = ppw + 1_ilp
                          end do
                          ppw = pw
                          do i = jrow+nnb, jrow+nnb+len-1
                             work( ppw ) = a( i, j+1 )
                             ppw = ppw + 1_ilp
                          end do
                          call stdlib_dtrmv( 'UPPER', 'TRANSPOSE', 'NON-UNIT', len,work( ppwo + &
                                    nnb ), 2_ilp*nnb, work( pw ),1_ilp )
                          call stdlib_dtrmv( 'LOWER', 'TRANSPOSE', 'NON-UNIT', nnb,work( ppwo + &
                                    2_ilp*len*nnb ),2_ilp*nnb, work( pw + len ), 1_ilp )
                          call stdlib_dgemv( 'TRANSPOSE', nnb, len, one,work( ppwo ), 2_ilp*nnb, a( &
                                    jrow, j+1 ), 1_ilp,one, work( pw ), 1_ilp )
                          call stdlib_dgemv( 'TRANSPOSE', len, nnb, one,work( ppwo + 2_ilp*len*nnb + &
                                    nnb ), 2_ilp*nnb,a( jrow+nnb, j+1 ), 1_ilp, one,work( pw+len ), 1_ilp )
                          ppw = pw
                          do i = jrow, jrow+len+nnb-1
                             a( i, j+1 ) = work( ppw )
                             ppw = ppw + 1_ilp
                          end do
                          ppwo = ppwo + 4_ilp*nnb*nnb
                       end do
                    end if
                 end do
                 ! apply accumulated orthogonal matrices to a.
                 cola = n - jcol - nnb + 1_ilp
                 j = ihi - nblst + 1_ilp
                 call stdlib_dgemm( 'TRANSPOSE', 'NO TRANSPOSE', nblst,cola, nblst, one, work, &
                           nblst,a( j, jcol+nnb ), lda, zero, work( pw ),nblst )
                 call stdlib_dlacpy( 'ALL', nblst, cola, work( pw ), nblst,a( j, jcol+nnb ), lda )
                           
                 ppwo = nblst*nblst + 1_ilp
                 j0 = j - nnb
                 do j = j0, jcol+1, -nnb
                    if ( blk22 ) then
                       ! exploit the structure of
                              ! [  u11  u12  ]
                          ! u = [            ]
                              ! [  u21  u22  ],
                       ! where all blocks are nnb-by-nnb, u21 is upper
                       ! triangular and u12 is lower triangular.
                       call stdlib_dorm22( 'LEFT', 'TRANSPOSE', 2_ilp*nnb, cola, nnb,nnb, work( ppwo )&
                                 , 2_ilp*nnb,a( j, jcol+nnb ), lda, work( pw ),lwork-pw+1, ierr )
                    else
                       ! ignore the structure of u.
                       call stdlib_dgemm( 'TRANSPOSE', 'NO TRANSPOSE', 2_ilp*nnb,cola, 2_ilp*nnb, one, &
                                 work( ppwo ), 2_ilp*nnb,a( j, jcol+nnb ), lda, zero, work( pw ),2_ilp*nnb )
                       call stdlib_dlacpy( 'ALL', 2_ilp*nnb, cola, work( pw ), 2_ilp*nnb,a( j, jcol+nnb ),&
                                  lda )
                    end if
                    ppwo = ppwo + 4_ilp*nnb*nnb
                 end do
                 ! apply accumulated orthogonal matrices to q.
                 if( wantq ) then
                    j = ihi - nblst + 1_ilp
                    if ( initq ) then
                       topq = max( 2_ilp, j - jcol + 1_ilp )
                       nh  = ihi - topq + 1_ilp
                    else
                       topq = 1_ilp
                       nh = n
                    end if
                    call stdlib_dgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', nh,nblst, nblst, one, q( &
                              topq, j ), ldq,work, nblst, zero, work( pw ), nh )
                    call stdlib_dlacpy( 'ALL', nh, nblst, work( pw ), nh,q( topq, j ), ldq )
                              
                    ppwo = nblst*nblst + 1_ilp
                    j0 = j - nnb
                    do j = j0, jcol+1, -nnb
                       if ( initq ) then
                          topq = max( 2_ilp, j - jcol + 1_ilp )
                          nh  = ihi - topq + 1_ilp
                       end if
                       if ( blk22 ) then
                          ! exploit the structure of u.
                          call stdlib_dorm22( 'RIGHT', 'NO TRANSPOSE', nh, 2_ilp*nnb,nnb, nnb, work( &
                                    ppwo ), 2_ilp*nnb,q( topq, j ), ldq, work( pw ),lwork-pw+1, ierr )
                       else
                          ! ignore the structure of u.
                          call stdlib_dgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', nh,2_ilp*nnb, 2_ilp*nnb, one,&
                                     q( topq, j ), ldq,work( ppwo ), 2_ilp*nnb, zero, work( pw ),nh )
                          call stdlib_dlacpy( 'ALL', nh, 2_ilp*nnb, work( pw ), nh,q( topq, j ), ldq )
                                    
                       end if
                       ppwo = ppwo + 4_ilp*nnb*nnb
                    end do
                 end if
                 ! accumulate right givens rotations if required.
                 if ( wantz .or. top>0_ilp ) then
                    ! initialize small orthogonal factors that will hold the
                    ! accumulated givens rotations in workspace.
                    call stdlib_dlaset( 'ALL', nblst, nblst, zero, one, work,nblst )
                    pw = nblst * nblst + 1_ilp
                    do i = 1, n2nb
                       call stdlib_dlaset( 'ALL', 2_ilp*nnb, 2_ilp*nnb, zero, one,work( pw ), 2_ilp*nnb )
                                 
                       pw = pw + 4_ilp*nnb*nnb
                    end do
                    ! accumulate givens rotations into workspace array.
                    do j = jcol, jcol+nnb-1
                       ppw  = ( nblst + 1_ilp )*( nblst - 2_ilp ) - j + jcol + 1_ilp
                       len  = 2_ilp + j - jcol
                       jrow = j + n2nb*nnb + 2_ilp
                       do i = ihi, jrow, -1
                          c = a( i, j )
                          a( i, j ) = zero
                          s = b( i, j )
                          b( i, j ) = zero
                          do jj = ppw, ppw+len-1
                             temp = work( jj + nblst )
                             work( jj + nblst ) = c*temp - s*work( jj )
                             work( jj ) = s*temp + c*work( jj )
                          end do
                          len = len + 1_ilp
                          ppw = ppw - nblst - 1_ilp
                       end do
                       ppwo = nblst*nblst + ( nnb+j-jcol-1 )*2_ilp*nnb + nnb
                       j0 = jrow - nnb
                       do jrow = j0, j+2, -nnb
                          ppw = ppwo
                          len  = 2_ilp + j - jcol
                          do i = jrow+nnb-1, jrow, -1
                             c = a( i, j )
                             a( i, j ) = zero
                             s = b( i, j )
                             b( i, j ) = zero
                             do jj = ppw, ppw+len-1
                                temp = work( jj + 2_ilp*nnb )
                                work( jj + 2_ilp*nnb ) = c*temp - s*work( jj )
                                work( jj ) = s*temp + c*work( jj )
                             end do
                             len = len + 1_ilp
                             ppw = ppw - 2_ilp*nnb - 1_ilp
                          end do
                          ppwo = ppwo + 4_ilp*nnb*nnb
                       end do
                    end do
                 else
                    call stdlib_dlaset( 'LOWER', ihi - jcol - 1_ilp, nnb, zero, zero,a( jcol + 2_ilp, &
                              jcol ), lda )
                    call stdlib_dlaset( 'LOWER', ihi - jcol - 1_ilp, nnb, zero, zero,b( jcol + 2_ilp, &
                              jcol ), ldb )
                 end if
                 ! apply accumulated orthogonal matrices to a and b.
                 if ( top>0_ilp ) then
                    j = ihi - nblst + 1_ilp
                    call stdlib_dgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', top,nblst, nblst, one, a( &
                              1_ilp, j ), lda,work, nblst, zero, work( pw ), top )
                    call stdlib_dlacpy( 'ALL', top, nblst, work( pw ), top,a( 1_ilp, j ), lda )
                              
                    ppwo = nblst*nblst + 1_ilp
                    j0 = j - nnb
                    do j = j0, jcol+1, -nnb
                       if ( blk22 ) then
                          ! exploit the structure of u.
                          call stdlib_dorm22( 'RIGHT', 'NO TRANSPOSE', top, 2_ilp*nnb,nnb, nnb, work( &
                                    ppwo ), 2_ilp*nnb,a( 1_ilp, j ), lda, work( pw ),lwork-pw+1, ierr )
                       else
                          ! ignore the structure of u.
                          call stdlib_dgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', top,2_ilp*nnb, 2_ilp*nnb, &
                                    one, a( 1_ilp, j ), lda,work( ppwo ), 2_ilp*nnb, zero,work( pw ), top )
                          call stdlib_dlacpy( 'ALL', top, 2_ilp*nnb, work( pw ), top,a( 1_ilp, j ), lda )
                                    
                       end if
                       ppwo = ppwo + 4_ilp*nnb*nnb
                    end do
                    j = ihi - nblst + 1_ilp
                    call stdlib_dgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', top,nblst, nblst, one, b( &
                              1_ilp, j ), ldb,work, nblst, zero, work( pw ), top )
                    call stdlib_dlacpy( 'ALL', top, nblst, work( pw ), top,b( 1_ilp, j ), ldb )
                              
                    ppwo = nblst*nblst + 1_ilp
                    j0 = j - nnb
                    do j = j0, jcol+1, -nnb
                       if ( blk22 ) then
                          ! exploit the structure of u.
                          call stdlib_dorm22( 'RIGHT', 'NO TRANSPOSE', top, 2_ilp*nnb,nnb, nnb, work( &
                                    ppwo ), 2_ilp*nnb,b( 1_ilp, j ), ldb, work( pw ),lwork-pw+1, ierr )
                       else
                          ! ignore the structure of u.
                          call stdlib_dgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', top,2_ilp*nnb, 2_ilp*nnb, &
                                    one, b( 1_ilp, j ), ldb,work( ppwo ), 2_ilp*nnb, zero,work( pw ), top )
                          call stdlib_dlacpy( 'ALL', top, 2_ilp*nnb, work( pw ), top,b( 1_ilp, j ), ldb )
                                    
                       end if
                       ppwo = ppwo + 4_ilp*nnb*nnb
                    end do
                 end if
                 ! apply accumulated orthogonal matrices to z.
                 if( wantz ) then
                    j = ihi - nblst + 1_ilp
                    if ( initq ) then
                       topq = max( 2_ilp, j - jcol + 1_ilp )
                       nh  = ihi - topq + 1_ilp
                    else
                       topq = 1_ilp
                       nh = n
                    end if
                    call stdlib_dgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', nh,nblst, nblst, one, z( &
                              topq, j ), ldz,work, nblst, zero, work( pw ), nh )
                    call stdlib_dlacpy( 'ALL', nh, nblst, work( pw ), nh,z( topq, j ), ldz )
                              
                    ppwo = nblst*nblst + 1_ilp
                    j0 = j - nnb
                    do j = j0, jcol+1, -nnb
                          if ( initq ) then
                          topq = max( 2_ilp, j - jcol + 1_ilp )
                          nh  = ihi - topq + 1_ilp
                       end if
                       if ( blk22 ) then
                          ! exploit the structure of u.
                          call stdlib_dorm22( 'RIGHT', 'NO TRANSPOSE', nh, 2_ilp*nnb,nnb, nnb, work( &
                                    ppwo ), 2_ilp*nnb,z( topq, j ), ldz, work( pw ),lwork-pw+1, ierr )
                       else
                          ! ignore the structure of u.
                          call stdlib_dgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', nh,2_ilp*nnb, 2_ilp*nnb, one,&
                                     z( topq, j ), ldz,work( ppwo ), 2_ilp*nnb, zero, work( pw ),nh )
                          call stdlib_dlacpy( 'ALL', nh, 2_ilp*nnb, work( pw ), nh,z( topq, j ), ldz )
                                    
                       end if
                       ppwo = ppwo + 4_ilp*nnb*nnb
                    end do
                 end if
              end do
           end if
           ! use unblocked code to reduce the rest of the matrix
           ! avoid re-initialization of modified q and z.
           compq2 = compq
           compz2 = compz
           if ( jcol/=ilo ) then
              if ( wantq )compq2 = 'V'
              if ( wantz )compz2 = 'V'
           end if
           if ( jcol<ihi )call stdlib_dgghrd( compq2, compz2, n, jcol, ihi, a, lda, b, ldb, q,ldq,&
                      z, ldz, ierr )
           work( 1_ilp ) = real( lwkopt,KIND=dp)
           return
     end subroutine stdlib_dgghd3


     pure module subroutine stdlib_cgghd3( compq, compz, n, ilo, ihi, a, lda, b, ldb, q,ldq, z, ldz, &
     !! CGGHD3 reduces a pair of complex matrices (A,B) to generalized upper
     !! Hessenberg form using unitary transformations, where A is a
     !! general matrix and B is upper triangular.  The form of the
     !! generalized eigenvalue problem is
     !! A*x = lambda*B*x,
     !! and B is typically made upper triangular by computing its QR
     !! factorization and moving the unitary matrix Q to the left side
     !! of the equation.
     !! This subroutine simultaneously reduces A to a Hessenberg matrix H:
     !! Q**H*A*Z = H
     !! and transforms B to another upper triangular matrix T:
     !! Q**H*B*Z = T
     !! in order to reduce the problem to its standard form
     !! H*y = lambda*T*y
     !! where y = Z**H*x.
     !! The unitary matrices Q and Z are determined as products of Givens
     !! rotations.  They may either be formed explicitly, or they may be
     !! postmultiplied into input matrices Q1 and Z1, so that
     !! Q1 * A * Z1**H = (Q1*Q) * H * (Z1*Z)**H
     !! Q1 * B * Z1**H = (Q1*Q) * T * (Z1*Z)**H
     !! If Q1 is the unitary matrix from the QR factorization of B in the
     !! original equation A*x = lambda*B*x, then CGGHD3 reduces the original
     !! problem to generalized Hessenberg form.
     !! This is a blocked variant of CGGHRD, using matrix-matrix
     !! multiplications for parts of the computation to enhance performance.
               work, lwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: compq, compz
           integer(ilp), intent(in) :: ihi, ilo, lda, ldb, ldq, ldz, n, lwork
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*), q(ldq,*), z(ldz,*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: blk22, initq, initz, lquery, wantq, wantz
           character :: compq2, compz2
           integer(ilp) :: cola, i, ierr, j, j0, jcol, jj, jrow, k, kacc22, len, lwkopt, n2nb, nb,&
                      nblst, nbmin, nh, nnb, nx, ppw, ppwo, pw, top, topq
           real(sp) :: c
           complex(sp) :: c1, c2, ctemp, s, s1, s2, temp, temp1, temp2, temp3
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode and test the input parameters.
           info = 0_ilp
           nb = stdlib_ilaenv( 1_ilp, 'CGGHD3', ' ', n, ilo, ihi, -1_ilp )
           lwkopt = max( 6_ilp*n*nb, 1_ilp )
           work( 1_ilp ) = cmplx( lwkopt,KIND=sp)
           initq = stdlib_lsame( compq, 'I' )
           wantq = initq .or. stdlib_lsame( compq, 'V' )
           initz = stdlib_lsame( compz, 'I' )
           wantz = initz .or. stdlib_lsame( compz, 'V' )
           lquery = ( lwork==-1_ilp )
           if( .not.stdlib_lsame( compq, 'N' ) .and. .not.wantq ) then
              info = -1_ilp
           else if( .not.stdlib_lsame( compz, 'N' ) .and. .not.wantz ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( ilo<1_ilp ) then
              info = -4_ilp
           else if( ihi>n .or. ihi<ilo-1 ) then
              info = -5_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -7_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -9_ilp
           else if( ( wantq .and. ldq<n ) .or. ldq<1_ilp ) then
              info = -11_ilp
           else if( ( wantz .and. ldz<n ) .or. ldz<1_ilp ) then
              info = -13_ilp
           else if( lwork<1_ilp .and. .not.lquery ) then
              info = -15_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CGGHD3', -info )
              return
           else if( lquery ) then
              return
           end if
           ! initialize q and z if desired.
           if( initq )call stdlib_claset( 'ALL', n, n, czero, cone, q, ldq )
           if( initz )call stdlib_claset( 'ALL', n, n, czero, cone, z, ldz )
           ! zero out lower triangle of b.
           if( n>1_ilp )call stdlib_claset( 'LOWER', n-1, n-1, czero, czero, b(2_ilp, 1_ilp), ldb )
           ! quick return if possible
           nh = ihi - ilo + 1_ilp
           if( nh<=1_ilp ) then
              work( 1_ilp ) = cone
              return
           end if
           ! determine the blocksize.
           nbmin = stdlib_ilaenv( 2_ilp, 'CGGHD3', ' ', n, ilo, ihi, -1_ilp )
           if( nb>1_ilp .and. nb<nh ) then
              ! determine when to use unblocked instead of blocked code.
              nx = max( nb, stdlib_ilaenv( 3_ilp, 'CGGHD3', ' ', n, ilo, ihi, -1_ilp ) )
              if( nx<nh ) then
                 ! determine if workspace is large enough for blocked code.
                 if( lwork<lwkopt ) then
                    ! not enough workspace to use optimal nb:  determine the
                    ! minimum value of nb, and reduce nb or force use of
                    ! unblocked code.
                    nbmin = max( 2_ilp, stdlib_ilaenv( 2_ilp, 'CGGHD3', ' ', n, ilo, ihi,-1_ilp ) )
                    if( lwork>=6_ilp*n*nbmin ) then
                       nb = lwork / ( 6_ilp*n )
                    else
                       nb = 1_ilp
                    end if
                 end if
              end if
           end if
           if( nb<nbmin .or. nb>=nh ) then
              ! use unblocked code below
              jcol = ilo
           else
              ! use blocked code
              kacc22 = stdlib_ilaenv( 16_ilp, 'CGGHD3', ' ', n, ilo, ihi, -1_ilp )
              blk22 = kacc22==2_ilp
              do jcol = ilo, ihi-2, nb
                 nnb = min( nb, ihi-jcol-1 )
                 ! initialize small unitary factors that will hold the
                 ! accumulated givens rotations in workspace.
                 ! n2nb   denotes the number of 2*nnb-by-2*nnb factors
                 ! nblst  denotes the (possibly smaller) order of the last
                        ! factor.
                 n2nb = ( ihi-jcol-1 ) / nnb - 1_ilp
                 nblst = ihi - jcol - n2nb*nnb
                 call stdlib_claset( 'ALL', nblst, nblst, czero, cone, work, nblst )
                 pw = nblst * nblst + 1_ilp
                 do i = 1, n2nb
                    call stdlib_claset( 'ALL', 2_ilp*nnb, 2_ilp*nnb, czero, cone,work( pw ), 2_ilp*nnb )
                              
                    pw = pw + 4_ilp*nnb*nnb
                 end do
                 ! reduce columns jcol:jcol+nnb-1 of a to hessenberg form.
                 do j = jcol, jcol+nnb-1
                    ! reduce jth column of a. store cosines and sines in jth
                    ! column of a and b, respectively.
                    do i = ihi, j+2, -1
                       temp = a( i-1, j )
                       call stdlib_clartg( temp, a( i, j ), c, s, a( i-1, j ) )
                       a( i, j ) = cmplx( c,KIND=sp)
                       b( i, j ) = s
                    end do
                    ! accumulate givens rotations into workspace array.
                    ppw  = ( nblst + 1_ilp )*( nblst - 2_ilp ) - j + jcol + 1_ilp
                    len  = 2_ilp + j - jcol
                    jrow = j + n2nb*nnb + 2_ilp
                    do i = ihi, jrow, -1
                       ctemp = a( i, j )
                       s = b( i, j )
                       do jj = ppw, ppw+len-1
                          temp = work( jj + nblst )
                          work( jj + nblst ) = ctemp*temp - s*work( jj )
                          work( jj ) = conjg( s )*temp + ctemp*work( jj )
                       end do
                       len = len + 1_ilp
                       ppw = ppw - nblst - 1_ilp
                    end do
                    ppwo = nblst*nblst + ( nnb+j-jcol-1 )*2_ilp*nnb + nnb
                    j0 = jrow - nnb
                    do jrow = j0, j+2, -nnb
                       ppw = ppwo
                       len  = 2_ilp + j - jcol
                       do i = jrow+nnb-1, jrow, -1
                          ctemp = a( i, j )
                          s = b( i, j )
                          do jj = ppw, ppw+len-1
                             temp = work( jj + 2_ilp*nnb )
                             work( jj + 2_ilp*nnb ) = ctemp*temp - s*work( jj )
                             work( jj ) = conjg( s )*temp + ctemp*work( jj )
                          end do
                          len = len + 1_ilp
                          ppw = ppw - 2_ilp*nnb - 1_ilp
                       end do
                       ppwo = ppwo + 4_ilp*nnb*nnb
                    end do
                    ! top denotes the number of top rows in a and b that will
                    ! not be updated during the next steps.
                    if( jcol<=2_ilp ) then
                       top = 0_ilp
                    else
                       top = jcol
                    end if
                    ! propagate transformations through b and replace stored
                    ! left sines/cosines by right sines/cosines.
                    do jj = n, j+1, -1
                       ! update jjth column of b.
                       do i = min( jj+1, ihi ), j+2, -1
                          ctemp = a( i, j )
                          s = b( i, j )
                          temp = b( i, jj )
                          b( i, jj ) = ctemp*temp - conjg( s )*b( i-1, jj )
                          b( i-1, jj ) = s*temp + ctemp*b( i-1, jj )
                       end do
                       ! annihilate b( jj+1, jj ).
                       if( jj<ihi ) then
                          temp = b( jj+1, jj+1 )
                          call stdlib_clartg( temp, b( jj+1, jj ), c, s,b( jj+1, jj+1 ) )
                          b( jj+1, jj ) = czero
                          call stdlib_crot( jj-top, b( top+1, jj+1 ), 1_ilp,b( top+1, jj ), 1_ilp, c, s )
                                    
                          a( jj+1, j ) = cmplx( c,KIND=sp)
                          b( jj+1, j ) = -conjg( s )
                       end if
                    end do
                    ! update a by transformations from right.
                    jj = mod( ihi-j-1, 3_ilp )
                    do i = ihi-j-3, jj+1, -3
                       ctemp = a( j+1+i, j )
                       s = -b( j+1+i, j )
                       c1 = a( j+2+i, j )
                       s1 = -b( j+2+i, j )
                       c2 = a( j+3+i, j )
                       s2 = -b( j+3+i, j )
                       do k = top+1, ihi
                          temp = a( k, j+i  )
                          temp1 = a( k, j+i+1 )
                          temp2 = a( k, j+i+2 )
                          temp3 = a( k, j+i+3 )
                          a( k, j+i+3 ) = c2*temp3 + conjg( s2 )*temp2
                          temp2 = -s2*temp3 + c2*temp2
                          a( k, j+i+2 ) = c1*temp2 + conjg( s1 )*temp1
                          temp1 = -s1*temp2 + c1*temp1
                          a( k, j+i+1 ) = ctemp*temp1 + conjg( s )*temp
                          a( k, j+i ) = -s*temp1 + ctemp*temp
                       end do
                    end do
                    if( jj>0_ilp ) then
                       do i = jj, 1, -1
                          c = real( a( j+1+i, j ),KIND=sp)
                          call stdlib_crot( ihi-top, a( top+1, j+i+1 ), 1_ilp,a( top+1, j+i ), 1_ilp, c,-&
                                    conjg( b( j+1+i, j ) ) )
                       end do
                    end if
                    ! update (j+1)th column of a by transformations from left.
                    if ( j < jcol + nnb - 1_ilp ) then
                       len  = 1_ilp + j - jcol
                       ! multiply with the trailing accumulated unitary
                       ! matrix, which takes the form
                              ! [  u11  u12  ]
                          ! u = [            ],
                              ! [  u21  u22  ]
                       ! where u21 is a len-by-len matrix and u12 is lower
                       ! triangular.
                       jrow = ihi - nblst + 1_ilp
                       call stdlib_cgemv( 'CONJUGATE', nblst, len, cone, work,nblst, a( jrow, j+1 &
                                 ), 1_ilp, czero,work( pw ), 1_ilp )
                       ppw = pw + len
                       do i = jrow, jrow+nblst-len-1
                          work( ppw ) = a( i, j+1 )
                          ppw = ppw + 1_ilp
                       end do
                       call stdlib_ctrmv( 'LOWER', 'CONJUGATE', 'NON-UNIT',nblst-len, work( &
                                 len*nblst + 1_ilp ), nblst,work( pw+len ), 1_ilp )
                       call stdlib_cgemv( 'CONJUGATE', len, nblst-len, cone,work( (len+1)*nblst - &
                       len + 1_ilp ), nblst,a( jrow+nblst-len, j+1 ), 1_ilp, cone,work( pw+len ), 1_ilp )
                                 
                       ppw = pw
                       do i = jrow, jrow+nblst-1
                          a( i, j+1 ) = work( ppw )
                          ppw = ppw + 1_ilp
                       end do
                       ! multiply with the other accumulated unitary
                       ! matrices, which take the form
                              ! [  u11  u12   0  ]
                              ! [                ]
                          ! u = [  u21  u22   0  ],
                              ! [                ]
                              ! [   0    0    i  ]
                       ! where i denotes the (nnb-len)-by-(nnb-len) identity
                       ! matrix, u21 is a len-by-len upper triangular matrix
                       ! and u12 is an nnb-by-nnb lower triangular matrix.
                       ppwo = 1_ilp + nblst*nblst
                       j0 = jrow - nnb
                       do jrow = j0, jcol+1, -nnb
                          ppw = pw + len
                          do i = jrow, jrow+nnb-1
                             work( ppw ) = a( i, j+1 )
                             ppw = ppw + 1_ilp
                          end do
                          ppw = pw
                          do i = jrow+nnb, jrow+nnb+len-1
                             work( ppw ) = a( i, j+1 )
                             ppw = ppw + 1_ilp
                          end do
                          call stdlib_ctrmv( 'UPPER', 'CONJUGATE', 'NON-UNIT', len,work( ppwo + &
                                    nnb ), 2_ilp*nnb, work( pw ),1_ilp )
                          call stdlib_ctrmv( 'LOWER', 'CONJUGATE', 'NON-UNIT', nnb,work( ppwo + &
                                    2_ilp*len*nnb ),2_ilp*nnb, work( pw + len ), 1_ilp )
                          call stdlib_cgemv( 'CONJUGATE', nnb, len, cone,work( ppwo ), 2_ilp*nnb, a( &
                                    jrow, j+1 ), 1_ilp,cone, work( pw ), 1_ilp )
                          call stdlib_cgemv( 'CONJUGATE', len, nnb, cone,work( ppwo + 2_ilp*len*nnb + &
                                    nnb ), 2_ilp*nnb,a( jrow+nnb, j+1 ), 1_ilp, cone,work( pw+len ), 1_ilp )
                          ppw = pw
                          do i = jrow, jrow+len+nnb-1
                             a( i, j+1 ) = work( ppw )
                             ppw = ppw + 1_ilp
                          end do
                          ppwo = ppwo + 4_ilp*nnb*nnb
                       end do
                    end if
                 end do
                 ! apply accumulated unitary matrices to a.
                 cola = n - jcol - nnb + 1_ilp
                 j = ihi - nblst + 1_ilp
                 call stdlib_cgemm( 'CONJUGATE', 'NO TRANSPOSE', nblst,cola, nblst, cone, work, &
                           nblst,a( j, jcol+nnb ), lda, czero, work( pw ),nblst )
                 call stdlib_clacpy( 'ALL', nblst, cola, work( pw ), nblst,a( j, jcol+nnb ), lda )
                           
                 ppwo = nblst*nblst + 1_ilp
                 j0 = j - nnb
                 do j = j0, jcol+1, -nnb
                    if ( blk22 ) then
                       ! exploit the structure of
                              ! [  u11  u12  ]
                          ! u = [            ]
                              ! [  u21  u22  ],
                       ! where all blocks are nnb-by-nnb, u21 is upper
                       ! triangular and u12 is lower triangular.
                       call stdlib_cunm22( 'LEFT', 'CONJUGATE', 2_ilp*nnb, cola, nnb,nnb, work( ppwo )&
                                 , 2_ilp*nnb,a( j, jcol+nnb ), lda, work( pw ),lwork-pw+1, ierr )
                    else
                       ! ignore the structure of u.
                       call stdlib_cgemm( 'CONJUGATE', 'NO TRANSPOSE', 2_ilp*nnb,cola, 2_ilp*nnb, cone, &
                       work( ppwo ), 2_ilp*nnb,a( j, jcol+nnb ), lda, czero, work( pw ),2_ilp*nnb )
                                 
                       call stdlib_clacpy( 'ALL', 2_ilp*nnb, cola, work( pw ), 2_ilp*nnb,a( j, jcol+nnb ),&
                                  lda )
                    end if
                    ppwo = ppwo + 4_ilp*nnb*nnb
                 end do
                 ! apply accumulated unitary matrices to q.
                 if( wantq ) then
                    j = ihi - nblst + 1_ilp
                    if ( initq ) then
                       topq = max( 2_ilp, j - jcol + 1_ilp )
                       nh  = ihi - topq + 1_ilp
                    else
                       topq = 1_ilp
                       nh = n
                    end if
                    call stdlib_cgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', nh,nblst, nblst, cone, q( &
                              topq, j ), ldq,work, nblst, czero, work( pw ), nh )
                    call stdlib_clacpy( 'ALL', nh, nblst, work( pw ), nh,q( topq, j ), ldq )
                              
                    ppwo = nblst*nblst + 1_ilp
                    j0 = j - nnb
                    do j = j0, jcol+1, -nnb
                       if ( initq ) then
                          topq = max( 2_ilp, j - jcol + 1_ilp )
                          nh  = ihi - topq + 1_ilp
                       end if
                       if ( blk22 ) then
                          ! exploit the structure of u.
                          call stdlib_cunm22( 'RIGHT', 'NO TRANSPOSE', nh, 2_ilp*nnb,nnb, nnb, work( &
                                    ppwo ), 2_ilp*nnb,q( topq, j ), ldq, work( pw ),lwork-pw+1, ierr )
                       else
                          ! ignore the structure of u.
                          call stdlib_cgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', nh,2_ilp*nnb, 2_ilp*nnb, &
                          cone, q( topq, j ), ldq,work( ppwo ), 2_ilp*nnb, czero, work( pw ),nh )
                                    
                          call stdlib_clacpy( 'ALL', nh, 2_ilp*nnb, work( pw ), nh,q( topq, j ), ldq )
                                    
                       end if
                       ppwo = ppwo + 4_ilp*nnb*nnb
                    end do
                 end if
                 ! accumulate right givens rotations if required.
                 if ( wantz .or. top>0_ilp ) then
                    ! initialize small unitary factors that will hold the
                    ! accumulated givens rotations in workspace.
                    call stdlib_claset( 'ALL', nblst, nblst, czero, cone, work,nblst )
                    pw = nblst * nblst + 1_ilp
                    do i = 1, n2nb
                       call stdlib_claset( 'ALL', 2_ilp*nnb, 2_ilp*nnb, czero, cone,work( pw ), 2_ilp*nnb )
                                 
                       pw = pw + 4_ilp*nnb*nnb
                    end do
                    ! accumulate givens rotations into workspace array.
                    do j = jcol, jcol+nnb-1
                       ppw  = ( nblst + 1_ilp )*( nblst - 2_ilp ) - j + jcol + 1_ilp
                       len  = 2_ilp + j - jcol
                       jrow = j + n2nb*nnb + 2_ilp
                       do i = ihi, jrow, -1
                          ctemp = a( i, j )
                          a( i, j ) = czero
                          s = b( i, j )
                          b( i, j ) = czero
                          do jj = ppw, ppw+len-1
                             temp = work( jj + nblst )
                             work( jj + nblst ) = ctemp*temp -conjg( s )*work( jj )
                             work( jj ) = s*temp + ctemp*work( jj )
                          end do
                          len = len + 1_ilp
                          ppw = ppw - nblst - 1_ilp
                       end do
                       ppwo = nblst*nblst + ( nnb+j-jcol-1 )*2_ilp*nnb + nnb
                       j0 = jrow - nnb
                       do jrow = j0, j+2, -nnb
                          ppw = ppwo
                          len  = 2_ilp + j - jcol
                          do i = jrow+nnb-1, jrow, -1
                             ctemp = a( i, j )
                             a( i, j ) = czero
                             s = b( i, j )
                             b( i, j ) = czero
                             do jj = ppw, ppw+len-1
                                temp = work( jj + 2_ilp*nnb )
                                work( jj + 2_ilp*nnb ) = ctemp*temp -conjg( s )*work( jj )
                                work( jj ) = s*temp + ctemp*work( jj )
                             end do
                             len = len + 1_ilp
                             ppw = ppw - 2_ilp*nnb - 1_ilp
                          end do
                          ppwo = ppwo + 4_ilp*nnb*nnb
                       end do
                    end do
                 else
                    call stdlib_claset( 'LOWER', ihi - jcol - 1_ilp, nnb, czero, czero,a( jcol + 2_ilp, &
                              jcol ), lda )
                    call stdlib_claset( 'LOWER', ihi - jcol - 1_ilp, nnb, czero, czero,b( jcol + 2_ilp, &
                              jcol ), ldb )
                 end if
                 ! apply accumulated unitary matrices to a and b.
                 if ( top>0_ilp ) then
                    j = ihi - nblst + 1_ilp
                    call stdlib_cgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', top,nblst, nblst, cone, a( &
                              1_ilp, j ), lda,work, nblst, czero, work( pw ), top )
                    call stdlib_clacpy( 'ALL', top, nblst, work( pw ), top,a( 1_ilp, j ), lda )
                              
                    ppwo = nblst*nblst + 1_ilp
                    j0 = j - nnb
                    do j = j0, jcol+1, -nnb
                       if ( blk22 ) then
                          ! exploit the structure of u.
                          call stdlib_cunm22( 'RIGHT', 'NO TRANSPOSE', top, 2_ilp*nnb,nnb, nnb, work( &
                                    ppwo ), 2_ilp*nnb,a( 1_ilp, j ), lda, work( pw ),lwork-pw+1, ierr )
                       else
                          ! ignore the structure of u.
                          call stdlib_cgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', top,2_ilp*nnb, 2_ilp*nnb, &
                          cone, a( 1_ilp, j ), lda,work( ppwo ), 2_ilp*nnb, czero,work( pw ), top )
                                    
                          call stdlib_clacpy( 'ALL', top, 2_ilp*nnb, work( pw ), top,a( 1_ilp, j ), lda )
                                    
                       end if
                       ppwo = ppwo + 4_ilp*nnb*nnb
                    end do
                    j = ihi - nblst + 1_ilp
                    call stdlib_cgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', top,nblst, nblst, cone, b( &
                              1_ilp, j ), ldb,work, nblst, czero, work( pw ), top )
                    call stdlib_clacpy( 'ALL', top, nblst, work( pw ), top,b( 1_ilp, j ), ldb )
                              
                    ppwo = nblst*nblst + 1_ilp
                    j0 = j - nnb
                    do j = j0, jcol+1, -nnb
                       if ( blk22 ) then
                          ! exploit the structure of u.
                          call stdlib_cunm22( 'RIGHT', 'NO TRANSPOSE', top, 2_ilp*nnb,nnb, nnb, work( &
                                    ppwo ), 2_ilp*nnb,b( 1_ilp, j ), ldb, work( pw ),lwork-pw+1, ierr )
                       else
                          ! ignore the structure of u.
                          call stdlib_cgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', top,2_ilp*nnb, 2_ilp*nnb, &
                          cone, b( 1_ilp, j ), ldb,work( ppwo ), 2_ilp*nnb, czero,work( pw ), top )
                                    
                          call stdlib_clacpy( 'ALL', top, 2_ilp*nnb, work( pw ), top,b( 1_ilp, j ), ldb )
                                    
                       end if
                       ppwo = ppwo + 4_ilp*nnb*nnb
                    end do
                 end if
                 ! apply accumulated unitary matrices to z.
                 if( wantz ) then
                    j = ihi - nblst + 1_ilp
                    if ( initq ) then
                       topq = max( 2_ilp, j - jcol + 1_ilp )
                       nh  = ihi - topq + 1_ilp
                    else
                       topq = 1_ilp
                       nh = n
                    end if
                    call stdlib_cgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', nh,nblst, nblst, cone, z( &
                              topq, j ), ldz,work, nblst, czero, work( pw ), nh )
                    call stdlib_clacpy( 'ALL', nh, nblst, work( pw ), nh,z( topq, j ), ldz )
                              
                    ppwo = nblst*nblst + 1_ilp
                    j0 = j - nnb
                    do j = j0, jcol+1, -nnb
                          if ( initq ) then
                          topq = max( 2_ilp, j - jcol + 1_ilp )
                          nh  = ihi - topq + 1_ilp
                       end if
                       if ( blk22 ) then
                          ! exploit the structure of u.
                          call stdlib_cunm22( 'RIGHT', 'NO TRANSPOSE', nh, 2_ilp*nnb,nnb, nnb, work( &
                                    ppwo ), 2_ilp*nnb,z( topq, j ), ldz, work( pw ),lwork-pw+1, ierr )
                       else
                          ! ignore the structure of u.
                          call stdlib_cgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', nh,2_ilp*nnb, 2_ilp*nnb, &
                          cone, z( topq, j ), ldz,work( ppwo ), 2_ilp*nnb, czero, work( pw ),nh )
                                    
                          call stdlib_clacpy( 'ALL', nh, 2_ilp*nnb, work( pw ), nh,z( topq, j ), ldz )
                                    
                       end if
                       ppwo = ppwo + 4_ilp*nnb*nnb
                    end do
                 end if
              end do
           end if
           ! use unblocked code to reduce the rest of the matrix
           ! avoid re-initialization of modified q and z.
           compq2 = compq
           compz2 = compz
           if ( jcol/=ilo ) then
              if ( wantq )compq2 = 'V'
              if ( wantz )compz2 = 'V'
           end if
           if ( jcol<ihi )call stdlib_cgghrd( compq2, compz2, n, jcol, ihi, a, lda, b, ldb, q,ldq,&
                      z, ldz, ierr )
           work( 1_ilp ) = cmplx( lwkopt,KIND=sp)
           return
     end subroutine stdlib_cgghd3

     pure module subroutine stdlib_zgghd3( compq, compz, n, ilo, ihi, a, lda, b, ldb, q,ldq, z, ldz, &
     !! ZGGHD3 reduces a pair of complex matrices (A,B) to generalized upper
     !! Hessenberg form using unitary transformations, where A is a
     !! general matrix and B is upper triangular.  The form of the
     !! generalized eigenvalue problem is
     !! A*x = lambda*B*x,
     !! and B is typically made upper triangular by computing its QR
     !! factorization and moving the unitary matrix Q to the left side
     !! of the equation.
     !! This subroutine simultaneously reduces A to a Hessenberg matrix H:
     !! Q**H*A*Z = H
     !! and transforms B to another upper triangular matrix T:
     !! Q**H*B*Z = T
     !! in order to reduce the problem to its standard form
     !! H*y = lambda*T*y
     !! where y = Z**H*x.
     !! The unitary matrices Q and Z are determined as products of Givens
     !! rotations.  They may either be formed explicitly, or they may be
     !! postmultiplied into input matrices Q1 and Z1, so that
     !! Q1 * A * Z1**H = (Q1*Q) * H * (Z1*Z)**H
     !! Q1 * B * Z1**H = (Q1*Q) * T * (Z1*Z)**H
     !! If Q1 is the unitary matrix from the QR factorization of B in the
     !! original equation A*x = lambda*B*x, then ZGGHD3 reduces the original
     !! problem to generalized Hessenberg form.
     !! This is a blocked variant of CGGHRD, using matrix-matrix
     !! multiplications for parts of the computation to enhance performance.
               work, lwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: compq, compz
           integer(ilp), intent(in) :: ihi, ilo, lda, ldb, ldq, ldz, n, lwork
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*), q(ldq,*), z(ldz,*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: blk22, initq, initz, lquery, wantq, wantz
           character :: compq2, compz2
           integer(ilp) :: cola, i, ierr, j, j0, jcol, jj, jrow, k, kacc22, len, lwkopt, n2nb, nb,&
                      nblst, nbmin, nh, nnb, nx, ppw, ppwo, pw, top, topq
           real(dp) :: c
           complex(dp) :: c1, c2, ctemp, s, s1, s2, temp, temp1, temp2, temp3
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode and test the input parameters.
           info = 0_ilp
           nb = stdlib_ilaenv( 1_ilp, 'ZGGHD3', ' ', n, ilo, ihi, -1_ilp )
           lwkopt = max( 6_ilp*n*nb, 1_ilp )
           work( 1_ilp ) = cmplx( lwkopt,KIND=dp)
           initq = stdlib_lsame( compq, 'I' )
           wantq = initq .or. stdlib_lsame( compq, 'V' )
           initz = stdlib_lsame( compz, 'I' )
           wantz = initz .or. stdlib_lsame( compz, 'V' )
           lquery = ( lwork==-1_ilp )
           if( .not.stdlib_lsame( compq, 'N' ) .and. .not.wantq ) then
              info = -1_ilp
           else if( .not.stdlib_lsame( compz, 'N' ) .and. .not.wantz ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( ilo<1_ilp ) then
              info = -4_ilp
           else if( ihi>n .or. ihi<ilo-1 ) then
              info = -5_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -7_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -9_ilp
           else if( ( wantq .and. ldq<n ) .or. ldq<1_ilp ) then
              info = -11_ilp
           else if( ( wantz .and. ldz<n ) .or. ldz<1_ilp ) then
              info = -13_ilp
           else if( lwork<1_ilp .and. .not.lquery ) then
              info = -15_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZGGHD3', -info )
              return
           else if( lquery ) then
              return
           end if
           ! initialize q and z if desired.
           if( initq )call stdlib_zlaset( 'ALL', n, n, czero, cone, q, ldq )
           if( initz )call stdlib_zlaset( 'ALL', n, n, czero, cone, z, ldz )
           ! zero out lower triangle of b.
           if( n>1_ilp )call stdlib_zlaset( 'LOWER', n-1, n-1, czero, czero, b(2_ilp, 1_ilp), ldb )
           ! quick return if possible
           nh = ihi - ilo + 1_ilp
           if( nh<=1_ilp ) then
              work( 1_ilp ) = cone
              return
           end if
           ! determine the blocksize.
           nbmin = stdlib_ilaenv( 2_ilp, 'ZGGHD3', ' ', n, ilo, ihi, -1_ilp )
           if( nb>1_ilp .and. nb<nh ) then
              ! determine when to use unblocked instead of blocked code.
              nx = max( nb, stdlib_ilaenv( 3_ilp, 'ZGGHD3', ' ', n, ilo, ihi, -1_ilp ) )
              if( nx<nh ) then
                 ! determine if workspace is large enough for blocked code.
                 if( lwork<lwkopt ) then
                    ! not enough workspace to use optimal nb:  determine the
                    ! minimum value of nb, and reduce nb or force use of
                    ! unblocked code.
                    nbmin = max( 2_ilp, stdlib_ilaenv( 2_ilp, 'ZGGHD3', ' ', n, ilo, ihi,-1_ilp ) )
                    if( lwork>=6_ilp*n*nbmin ) then
                       nb = lwork / ( 6_ilp*n )
                    else
                       nb = 1_ilp
                    end if
                 end if
              end if
           end if
           if( nb<nbmin .or. nb>=nh ) then
              ! use unblocked code below
              jcol = ilo
           else
              ! use blocked code
              kacc22 = stdlib_ilaenv( 16_ilp, 'ZGGHD3', ' ', n, ilo, ihi, -1_ilp )
              blk22 = kacc22==2_ilp
              do jcol = ilo, ihi-2, nb
                 nnb = min( nb, ihi-jcol-1 )
                 ! initialize small unitary factors that will hold the
                 ! accumulated givens rotations in workspace.
                 ! n2nb   denotes the number of 2*nnb-by-2*nnb factors
                 ! nblst  denotes the (possibly smaller) order of the last
                        ! factor.
                 n2nb = ( ihi-jcol-1 ) / nnb - 1_ilp
                 nblst = ihi - jcol - n2nb*nnb
                 call stdlib_zlaset( 'ALL', nblst, nblst, czero, cone, work, nblst )
                 pw = nblst * nblst + 1_ilp
                 do i = 1, n2nb
                    call stdlib_zlaset( 'ALL', 2_ilp*nnb, 2_ilp*nnb, czero, cone,work( pw ), 2_ilp*nnb )
                              
                    pw = pw + 4_ilp*nnb*nnb
                 end do
                 ! reduce columns jcol:jcol+nnb-1 of a to hessenberg form.
                 do j = jcol, jcol+nnb-1
                    ! reduce jth column of a. store cosines and sines in jth
                    ! column of a and b, respectively.
                    do i = ihi, j+2, -1
                       temp = a( i-1, j )
                       call stdlib_zlartg( temp, a( i, j ), c, s, a( i-1, j ) )
                       a( i, j ) = cmplx( c,KIND=dp)
                       b( i, j ) = s
                    end do
                    ! accumulate givens rotations into workspace array.
                    ppw  = ( nblst + 1_ilp )*( nblst - 2_ilp ) - j + jcol + 1_ilp
                    len  = 2_ilp + j - jcol
                    jrow = j + n2nb*nnb + 2_ilp
                    do i = ihi, jrow, -1
                       ctemp = a( i, j )
                       s = b( i, j )
                       do jj = ppw, ppw+len-1
                          temp = work( jj + nblst )
                          work( jj + nblst ) = ctemp*temp - s*work( jj )
                          work( jj ) = conjg( s )*temp + ctemp*work( jj )
                       end do
                       len = len + 1_ilp
                       ppw = ppw - nblst - 1_ilp
                    end do
                    ppwo = nblst*nblst + ( nnb+j-jcol-1 )*2_ilp*nnb + nnb
                    j0 = jrow - nnb
                    do jrow = j0, j+2, -nnb
                       ppw = ppwo
                       len  = 2_ilp + j - jcol
                       do i = jrow+nnb-1, jrow, -1
                          ctemp = a( i, j )
                          s = b( i, j )
                          do jj = ppw, ppw+len-1
                             temp = work( jj + 2_ilp*nnb )
                             work( jj + 2_ilp*nnb ) = ctemp*temp - s*work( jj )
                             work( jj ) = conjg( s )*temp + ctemp*work( jj )
                          end do
                          len = len + 1_ilp
                          ppw = ppw - 2_ilp*nnb - 1_ilp
                       end do
                       ppwo = ppwo + 4_ilp*nnb*nnb
                    end do
                    ! top denotes the number of top rows in a and b that will
                    ! not be updated during the next steps.
                    if( jcol<=2_ilp ) then
                       top = 0_ilp
                    else
                       top = jcol
                    end if
                    ! propagate transformations through b and replace stored
                    ! left sines/cosines by right sines/cosines.
                    do jj = n, j+1, -1
                       ! update jjth column of b.
                       do i = min( jj+1, ihi ), j+2, -1
                          ctemp = a( i, j )
                          s = b( i, j )
                          temp = b( i, jj )
                          b( i, jj ) = ctemp*temp - conjg( s )*b( i-1, jj )
                          b( i-1, jj ) = s*temp + ctemp*b( i-1, jj )
                       end do
                       ! annihilate b( jj+1, jj ).
                       if( jj<ihi ) then
                          temp = b( jj+1, jj+1 )
                          call stdlib_zlartg( temp, b( jj+1, jj ), c, s,b( jj+1, jj+1 ) )
                          b( jj+1, jj ) = czero
                          call stdlib_zrot( jj-top, b( top+1, jj+1 ), 1_ilp,b( top+1, jj ), 1_ilp, c, s )
                                    
                          a( jj+1, j ) = cmplx( c,KIND=dp)
                          b( jj+1, j ) = -conjg( s )
                       end if
                    end do
                    ! update a by transformations from right.
                    jj = mod( ihi-j-1, 3_ilp )
                    do i = ihi-j-3, jj+1, -3
                       ctemp = a( j+1+i, j )
                       s = -b( j+1+i, j )
                       c1 = a( j+2+i, j )
                       s1 = -b( j+2+i, j )
                       c2 = a( j+3+i, j )
                       s2 = -b( j+3+i, j )
                       do k = top+1, ihi
                          temp = a( k, j+i  )
                          temp1 = a( k, j+i+1 )
                          temp2 = a( k, j+i+2 )
                          temp3 = a( k, j+i+3 )
                          a( k, j+i+3 ) = c2*temp3 + conjg( s2 )*temp2
                          temp2 = -s2*temp3 + c2*temp2
                          a( k, j+i+2 ) = c1*temp2 + conjg( s1 )*temp1
                          temp1 = -s1*temp2 + c1*temp1
                          a( k, j+i+1 ) = ctemp*temp1 + conjg( s )*temp
                          a( k, j+i ) = -s*temp1 + ctemp*temp
                       end do
                    end do
                    if( jj>0_ilp ) then
                       do i = jj, 1, -1
                          c = real( a( j+1+i, j ),KIND=dp)
                          call stdlib_zrot( ihi-top, a( top+1, j+i+1 ), 1_ilp,a( top+1, j+i ), 1_ilp, c,-&
                                    conjg( b( j+1+i, j ) ) )
                       end do
                    end if
                    ! update (j+1)th column of a by transformations from left.
                    if ( j < jcol + nnb - 1_ilp ) then
                       len  = 1_ilp + j - jcol
                       ! multiply with the trailing accumulated unitary
                       ! matrix, which takes the form
                              ! [  u11  u12  ]
                          ! u = [            ],
                              ! [  u21  u22  ]
                       ! where u21 is a len-by-len matrix and u12 is lower
                       ! triangular.
                       jrow = ihi - nblst + 1_ilp
                       call stdlib_zgemv( 'CONJUGATE', nblst, len, cone, work,nblst, a( jrow, j+1 &
                                 ), 1_ilp, czero,work( pw ), 1_ilp )
                       ppw = pw + len
                       do i = jrow, jrow+nblst-len-1
                          work( ppw ) = a( i, j+1 )
                          ppw = ppw + 1_ilp
                       end do
                       call stdlib_ztrmv( 'LOWER', 'CONJUGATE', 'NON-UNIT',nblst-len, work( &
                                 len*nblst + 1_ilp ), nblst,work( pw+len ), 1_ilp )
                       call stdlib_zgemv( 'CONJUGATE', len, nblst-len, cone,work( (len+1)*nblst - &
                       len + 1_ilp ), nblst,a( jrow+nblst-len, j+1 ), 1_ilp, cone,work( pw+len ), 1_ilp )
                                 
                       ppw = pw
                       do i = jrow, jrow+nblst-1
                          a( i, j+1 ) = work( ppw )
                          ppw = ppw + 1_ilp
                       end do
                       ! multiply with the other accumulated unitary
                       ! matrices, which take the form
                              ! [  u11  u12   0  ]
                              ! [                ]
                          ! u = [  u21  u22   0  ],
                              ! [                ]
                              ! [   0    0    i  ]
                       ! where i denotes the (nnb-len)-by-(nnb-len) identity
                       ! matrix, u21 is a len-by-len upper triangular matrix
                       ! and u12 is an nnb-by-nnb lower triangular matrix.
                       ppwo = 1_ilp + nblst*nblst
                       j0 = jrow - nnb
                       do jrow = j0, jcol+1, -nnb
                          ppw = pw + len
                          do i = jrow, jrow+nnb-1
                             work( ppw ) = a( i, j+1 )
                             ppw = ppw + 1_ilp
                          end do
                          ppw = pw
                          do i = jrow+nnb, jrow+nnb+len-1
                             work( ppw ) = a( i, j+1 )
                             ppw = ppw + 1_ilp
                          end do
                          call stdlib_ztrmv( 'UPPER', 'CONJUGATE', 'NON-UNIT', len,work( ppwo + &
                                    nnb ), 2_ilp*nnb, work( pw ),1_ilp )
                          call stdlib_ztrmv( 'LOWER', 'CONJUGATE', 'NON-UNIT', nnb,work( ppwo + &
                                    2_ilp*len*nnb ),2_ilp*nnb, work( pw + len ), 1_ilp )
                          call stdlib_zgemv( 'CONJUGATE', nnb, len, cone,work( ppwo ), 2_ilp*nnb, a( &
                                    jrow, j+1 ), 1_ilp,cone, work( pw ), 1_ilp )
                          call stdlib_zgemv( 'CONJUGATE', len, nnb, cone,work( ppwo + 2_ilp*len*nnb + &
                                    nnb ), 2_ilp*nnb,a( jrow+nnb, j+1 ), 1_ilp, cone,work( pw+len ), 1_ilp )
                          ppw = pw
                          do i = jrow, jrow+len+nnb-1
                             a( i, j+1 ) = work( ppw )
                             ppw = ppw + 1_ilp
                          end do
                          ppwo = ppwo + 4_ilp*nnb*nnb
                       end do
                    end if
                 end do
                 ! apply accumulated unitary matrices to a.
                 cola = n - jcol - nnb + 1_ilp
                 j = ihi - nblst + 1_ilp
                 call stdlib_zgemm( 'CONJUGATE', 'NO TRANSPOSE', nblst,cola, nblst, cone, work, &
                           nblst,a( j, jcol+nnb ), lda, czero, work( pw ),nblst )
                 call stdlib_zlacpy( 'ALL', nblst, cola, work( pw ), nblst,a( j, jcol+nnb ), lda )
                           
                 ppwo = nblst*nblst + 1_ilp
                 j0 = j - nnb
                 do j = j0, jcol+1, -nnb
                    if ( blk22 ) then
                       ! exploit the structure of
                              ! [  u11  u12  ]
                          ! u = [            ]
                              ! [  u21  u22  ],
                       ! where all blocks are nnb-by-nnb, u21 is upper
                       ! triangular and u12 is lower triangular.
                       call stdlib_zunm22( 'LEFT', 'CONJUGATE', 2_ilp*nnb, cola, nnb,nnb, work( ppwo )&
                                 , 2_ilp*nnb,a( j, jcol+nnb ), lda, work( pw ),lwork-pw+1, ierr )
                    else
                       ! ignore the structure of u.
                       call stdlib_zgemm( 'CONJUGATE', 'NO TRANSPOSE', 2_ilp*nnb,cola, 2_ilp*nnb, cone, &
                       work( ppwo ), 2_ilp*nnb,a( j, jcol+nnb ), lda, czero, work( pw ),2_ilp*nnb )
                                 
                       call stdlib_zlacpy( 'ALL', 2_ilp*nnb, cola, work( pw ), 2_ilp*nnb,a( j, jcol+nnb ),&
                                  lda )
                    end if
                    ppwo = ppwo + 4_ilp*nnb*nnb
                 end do
                 ! apply accumulated unitary matrices to q.
                 if( wantq ) then
                    j = ihi - nblst + 1_ilp
                    if ( initq ) then
                       topq = max( 2_ilp, j - jcol + 1_ilp )
                       nh  = ihi - topq + 1_ilp
                    else
                       topq = 1_ilp
                       nh = n
                    end if
                    call stdlib_zgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', nh,nblst, nblst, cone, q( &
                              topq, j ), ldq,work, nblst, czero, work( pw ), nh )
                    call stdlib_zlacpy( 'ALL', nh, nblst, work( pw ), nh,q( topq, j ), ldq )
                              
                    ppwo = nblst*nblst + 1_ilp
                    j0 = j - nnb
                    do j = j0, jcol+1, -nnb
                       if ( initq ) then
                          topq = max( 2_ilp, j - jcol + 1_ilp )
                          nh  = ihi - topq + 1_ilp
                       end if
                       if ( blk22 ) then
                          ! exploit the structure of u.
                          call stdlib_zunm22( 'RIGHT', 'NO TRANSPOSE', nh, 2_ilp*nnb,nnb, nnb, work( &
                                    ppwo ), 2_ilp*nnb,q( topq, j ), ldq, work( pw ),lwork-pw+1, ierr )
                       else
                          ! ignore the structure of u.
                          call stdlib_zgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', nh,2_ilp*nnb, 2_ilp*nnb, &
                          cone, q( topq, j ), ldq,work( ppwo ), 2_ilp*nnb, czero, work( pw ),nh )
                                    
                          call stdlib_zlacpy( 'ALL', nh, 2_ilp*nnb, work( pw ), nh,q( topq, j ), ldq )
                                    
                       end if
                       ppwo = ppwo + 4_ilp*nnb*nnb
                    end do
                 end if
                 ! accumulate right givens rotations if required.
                 if ( wantz .or. top>0_ilp ) then
                    ! initialize small unitary factors that will hold the
                    ! accumulated givens rotations in workspace.
                    call stdlib_zlaset( 'ALL', nblst, nblst, czero, cone, work,nblst )
                    pw = nblst * nblst + 1_ilp
                    do i = 1, n2nb
                       call stdlib_zlaset( 'ALL', 2_ilp*nnb, 2_ilp*nnb, czero, cone,work( pw ), 2_ilp*nnb )
                                 
                       pw = pw + 4_ilp*nnb*nnb
                    end do
                    ! accumulate givens rotations into workspace array.
                    do j = jcol, jcol+nnb-1
                       ppw  = ( nblst + 1_ilp )*( nblst - 2_ilp ) - j + jcol + 1_ilp
                       len  = 2_ilp + j - jcol
                       jrow = j + n2nb*nnb + 2_ilp
                       do i = ihi, jrow, -1
                          ctemp = a( i, j )
                          a( i, j ) = czero
                          s = b( i, j )
                          b( i, j ) = czero
                          do jj = ppw, ppw+len-1
                             temp = work( jj + nblst )
                             work( jj + nblst ) = ctemp*temp -conjg( s )*work( jj )
                             work( jj ) = s*temp + ctemp*work( jj )
                          end do
                          len = len + 1_ilp
                          ppw = ppw - nblst - 1_ilp
                       end do
                       ppwo = nblst*nblst + ( nnb+j-jcol-1 )*2_ilp*nnb + nnb
                       j0 = jrow - nnb
                       do jrow = j0, j+2, -nnb
                          ppw = ppwo
                          len  = 2_ilp + j - jcol
                          do i = jrow+nnb-1, jrow, -1
                             ctemp = a( i, j )
                             a( i, j ) = czero
                             s = b( i, j )
                             b( i, j ) = czero
                             do jj = ppw, ppw+len-1
                                temp = work( jj + 2_ilp*nnb )
                                work( jj + 2_ilp*nnb ) = ctemp*temp -conjg( s )*work( jj )
                                work( jj ) = s*temp + ctemp*work( jj )
                             end do
                             len = len + 1_ilp
                             ppw = ppw - 2_ilp*nnb - 1_ilp
                          end do
                          ppwo = ppwo + 4_ilp*nnb*nnb
                       end do
                    end do
                 else
                    call stdlib_zlaset( 'LOWER', ihi - jcol - 1_ilp, nnb, czero, czero,a( jcol + 2_ilp, &
                              jcol ), lda )
                    call stdlib_zlaset( 'LOWER', ihi - jcol - 1_ilp, nnb, czero, czero,b( jcol + 2_ilp, &
                              jcol ), ldb )
                 end if
                 ! apply accumulated unitary matrices to a and b.
                 if ( top>0_ilp ) then
                    j = ihi - nblst + 1_ilp
                    call stdlib_zgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', top,nblst, nblst, cone, a( &
                              1_ilp, j ), lda,work, nblst, czero, work( pw ), top )
                    call stdlib_zlacpy( 'ALL', top, nblst, work( pw ), top,a( 1_ilp, j ), lda )
                              
                    ppwo = nblst*nblst + 1_ilp
                    j0 = j - nnb
                    do j = j0, jcol+1, -nnb
                       if ( blk22 ) then
                          ! exploit the structure of u.
                          call stdlib_zunm22( 'RIGHT', 'NO TRANSPOSE', top, 2_ilp*nnb,nnb, nnb, work( &
                                    ppwo ), 2_ilp*nnb,a( 1_ilp, j ), lda, work( pw ),lwork-pw+1, ierr )
                       else
                          ! ignore the structure of u.
                          call stdlib_zgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', top,2_ilp*nnb, 2_ilp*nnb, &
                          cone, a( 1_ilp, j ), lda,work( ppwo ), 2_ilp*nnb, czero,work( pw ), top )
                                    
                          call stdlib_zlacpy( 'ALL', top, 2_ilp*nnb, work( pw ), top,a( 1_ilp, j ), lda )
                                    
                       end if
                       ppwo = ppwo + 4_ilp*nnb*nnb
                    end do
                    j = ihi - nblst + 1_ilp
                    call stdlib_zgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', top,nblst, nblst, cone, b( &
                              1_ilp, j ), ldb,work, nblst, czero, work( pw ), top )
                    call stdlib_zlacpy( 'ALL', top, nblst, work( pw ), top,b( 1_ilp, j ), ldb )
                              
                    ppwo = nblst*nblst + 1_ilp
                    j0 = j - nnb
                    do j = j0, jcol+1, -nnb
                       if ( blk22 ) then
                          ! exploit the structure of u.
                          call stdlib_zunm22( 'RIGHT', 'NO TRANSPOSE', top, 2_ilp*nnb,nnb, nnb, work( &
                                    ppwo ), 2_ilp*nnb,b( 1_ilp, j ), ldb, work( pw ),lwork-pw+1, ierr )
                       else
                          ! ignore the structure of u.
                          call stdlib_zgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', top,2_ilp*nnb, 2_ilp*nnb, &
                          cone, b( 1_ilp, j ), ldb,work( ppwo ), 2_ilp*nnb, czero,work( pw ), top )
                                    
                          call stdlib_zlacpy( 'ALL', top, 2_ilp*nnb, work( pw ), top,b( 1_ilp, j ), ldb )
                                    
                       end if
                       ppwo = ppwo + 4_ilp*nnb*nnb
                    end do
                 end if
                 ! apply accumulated unitary matrices to z.
                 if( wantz ) then
                    j = ihi - nblst + 1_ilp
                    if ( initq ) then
                       topq = max( 2_ilp, j - jcol + 1_ilp )
                       nh  = ihi - topq + 1_ilp
                    else
                       topq = 1_ilp
                       nh = n
                    end if
                    call stdlib_zgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', nh,nblst, nblst, cone, z( &
                              topq, j ), ldz,work, nblst, czero, work( pw ), nh )
                    call stdlib_zlacpy( 'ALL', nh, nblst, work( pw ), nh,z( topq, j ), ldz )
                              
                    ppwo = nblst*nblst + 1_ilp
                    j0 = j - nnb
                    do j = j0, jcol+1, -nnb
                          if ( initq ) then
                          topq = max( 2_ilp, j - jcol + 1_ilp )
                          nh  = ihi - topq + 1_ilp
                       end if
                       if ( blk22 ) then
                          ! exploit the structure of u.
                          call stdlib_zunm22( 'RIGHT', 'NO TRANSPOSE', nh, 2_ilp*nnb,nnb, nnb, work( &
                                    ppwo ), 2_ilp*nnb,z( topq, j ), ldz, work( pw ),lwork-pw+1, ierr )
                       else
                          ! ignore the structure of u.
                          call stdlib_zgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', nh,2_ilp*nnb, 2_ilp*nnb, &
                          cone, z( topq, j ), ldz,work( ppwo ), 2_ilp*nnb, czero, work( pw ),nh )
                                    
                          call stdlib_zlacpy( 'ALL', nh, 2_ilp*nnb, work( pw ), nh,z( topq, j ), ldz )
                                    
                       end if
                       ppwo = ppwo + 4_ilp*nnb*nnb
                    end do
                 end if
              end do
           end if
           ! use unblocked code to reduce the rest of the matrix
           ! avoid re-initialization of modified q and z.
           compq2 = compq
           compz2 = compz
           if ( jcol/=ilo ) then
              if ( wantq )compq2 = 'V'
              if ( wantz )compz2 = 'V'
           end if
           if ( jcol<ihi )call stdlib_zgghrd( compq2, compz2, n, jcol, ihi, a, lda, b, ldb, q,ldq,&
                      z, ldz, ierr )
           work( 1_ilp ) = cmplx( lwkopt,KIND=dp)
           return
     end subroutine stdlib_zgghd3




     module subroutine stdlib_shgeqz( job, compq, compz, n, ilo, ihi, h, ldh, t, ldt,alphar, alphai, &
     !! SHGEQZ computes the eigenvalues of a real matrix pair (H,T),
     !! where H is an upper Hessenberg matrix and T is upper triangular,
     !! using the double-shift QZ method.
     !! Matrix pairs of this type are produced by the reduction to
     !! generalized upper Hessenberg form of a real matrix pair (A,B):
     !! A = Q1*H*Z1**T,  B = Q1*T*Z1**T,
     !! as computed by SGGHRD.
     !! If JOB='S', then the Hessenberg-triangular pair (H,T) is
     !! also reduced to generalized Schur form,
     !! H = Q*S*Z**T,  T = Q*P*Z**T,
     !! where Q and Z are orthogonal matrices, P is an upper triangular
     !! matrix, and S is a quasi-triangular matrix with 1-by-1 and 2-by-2
     !! diagonal blocks.
     !! The 1-by-1 blocks correspond to real eigenvalues of the matrix pair
     !! (H,T) and the 2-by-2 blocks correspond to complex conjugate pairs of
     !! eigenvalues.
     !! Additionally, the 2-by-2 upper triangular diagonal blocks of P
     !! corresponding to 2-by-2 blocks of S are reduced to positive diagonal
     !! form, i.e., if S(j+1,j) is non-zero, then P(j+1,j) = P(j,j+1) = 0,
     !! P(j,j) > 0, and P(j+1,j+1) > 0.
     !! Optionally, the orthogonal matrix Q from the generalized Schur
     !! factorization may be postmultiplied into an input matrix Q1, and the
     !! orthogonal matrix Z may be postmultiplied into an input matrix Z1.
     !! If Q1 and Z1 are the orthogonal matrices from SGGHRD that reduced
     !! the matrix pair (A,B) to generalized upper Hessenberg form, then the
     !! output matrices Q1*Q and Z1*Z are the orthogonal factors from the
     !! generalized Schur factorization of (A,B):
     !! A = (Q1*Q)*S*(Z1*Z)**T,  B = (Q1*Q)*P*(Z1*Z)**T.
     !! To avoid overflow, eigenvalues of the matrix pair (H,T) (equivalently,
     !! of (A,B)) are computed as a pair of values (alpha,beta), where alpha is
     !! complex and beta real.
     !! If beta is nonzero, lambda = alpha / beta is an eigenvalue of the
     !! generalized nonsymmetric eigenvalue problem (GNEP)
     !! A*x = lambda*B*x
     !! and if alpha is nonzero, mu = beta / alpha is an eigenvalue of the
     !! alternate form of the GNEP
     !! mu*A*y = B*y.
     !! Real eigenvalues can be read directly from the generalized Schur
     !! form:
     !! alpha = S(i,i), beta = P(i,i).
     !! Ref: C.B. Moler
     !! Eigenvalue Problems", SIAM J. Numer. Anal., 10(1973),
     !! pp. 241--256.
               beta, q, ldq, z, ldz, work,lwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: compq, compz, job
           integer(ilp), intent(in) :: ihi, ilo, ldh, ldq, ldt, ldz, lwork, n
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           real(sp), intent(out) :: alphai(*), alphar(*), beta(*), work(*)
           real(sp), intent(inout) :: h(ldh,*), q(ldq,*), t(ldt,*), z(ldz,*)
        ! =====================================================================
           ! Parameters 
           real(sp), parameter :: safety = 1.0e+2_sp
          ! $                     safety = one )
           
           ! Local Scalars 
           logical(lk) :: ilazr2, ilazro, ilpivt, ilq, ilschr, ilz, lquery
           integer(ilp) :: icompq, icompz, ifirst, ifrstm, iiter, ilast, ilastm, in, ischur, &
                     istart, j, jc, jch, jiter, jr, maxit
           real(sp) :: a11, a12, a1i, a1r, a21, a22, a2i, a2r, ad11, ad11l, ad12, ad12l, ad21, &
           ad21l, ad22, ad22l, ad32l, an, anorm, ascale, atol, b11, b1a, b1i, b1r, b22, b2a, b2i, &
           b2r, bn, bnorm, bscale, btol, c, c11i, c11r, c12, c21, c22i, c22r, cl, cq, cr, cz, &
           eshift, s, s1, s1inv, s2, safmax, safmin, scale, sl, sqi, sqr, sr, szi, szr, t1, tau, &
           temp, temp2, tempi, tempr, u1, u12, u12l, u2, ulp, vs, w11, w12, w21, w22, wabs, wi, &
                     wr, wr2
           ! Local Arrays 
           real(sp) :: v(3_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode job, compq, compz
           if( stdlib_lsame( job, 'E' ) ) then
              ilschr = .false.
              ischur = 1_ilp
           else if( stdlib_lsame( job, 'S' ) ) then
              ilschr = .true.
              ischur = 2_ilp
           else
              ischur = 0_ilp
           end if
           if( stdlib_lsame( compq, 'N' ) ) then
              ilq = .false.
              icompq = 1_ilp
           else if( stdlib_lsame( compq, 'V' ) ) then
              ilq = .true.
              icompq = 2_ilp
           else if( stdlib_lsame( compq, 'I' ) ) then
              ilq = .true.
              icompq = 3_ilp
           else
              icompq = 0_ilp
           end if
           if( stdlib_lsame( compz, 'N' ) ) then
              ilz = .false.
              icompz = 1_ilp
           else if( stdlib_lsame( compz, 'V' ) ) then
              ilz = .true.
              icompz = 2_ilp
           else if( stdlib_lsame( compz, 'I' ) ) then
              ilz = .true.
              icompz = 3_ilp
           else
              icompz = 0_ilp
           end if
           ! check argument values
           info = 0_ilp
           work( 1_ilp ) = max( 1_ilp, n )
           lquery = ( lwork==-1_ilp )
           if( ischur==0_ilp ) then
              info = -1_ilp
           else if( icompq==0_ilp ) then
              info = -2_ilp
           else if( icompz==0_ilp ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( ilo<1_ilp ) then
              info = -5_ilp
           else if( ihi>n .or. ihi<ilo-1 ) then
              info = -6_ilp
           else if( ldh<n ) then
              info = -8_ilp
           else if( ldt<n ) then
              info = -10_ilp
           else if( ldq<1_ilp .or. ( ilq .and. ldq<n ) ) then
              info = -15_ilp
           else if( ldz<1_ilp .or. ( ilz .and. ldz<n ) ) then
              info = -17_ilp
           else if( lwork<max( 1_ilp, n ) .and. .not.lquery ) then
              info = -19_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SHGEQZ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n<=0_ilp ) then
              work( 1_ilp ) = real( 1_ilp,KIND=sp)
              return
           end if
           ! initialize q and z
           if( icompq==3_ilp )call stdlib_slaset( 'FULL', n, n, zero, one, q, ldq )
           if( icompz==3_ilp )call stdlib_slaset( 'FULL', n, n, zero, one, z, ldz )
           ! machine constants
           in = ihi + 1_ilp - ilo
           safmin = stdlib_slamch( 'S' )
           safmax = one / safmin
           ulp = stdlib_slamch( 'E' )*stdlib_slamch( 'B' )
           anorm = stdlib_slanhs( 'F', in, h( ilo, ilo ), ldh, work )
           bnorm = stdlib_slanhs( 'F', in, t( ilo, ilo ), ldt, work )
           atol = max( safmin, ulp*anorm )
           btol = max( safmin, ulp*bnorm )
           ascale = one / max( safmin, anorm )
           bscale = one / max( safmin, bnorm )
           ! set eigenvalues ihi+1:n
           do j = ihi + 1, n
              if( t( j, j )<zero ) then
                 if( ilschr ) then
                    do jr = 1, j
                       h( jr, j ) = -h( jr, j )
                       t( jr, j ) = -t( jr, j )
                    end do
                 else
                    h( j, j ) = -h( j, j )
                    t( j, j ) = -t( j, j )
                 end if
                 if( ilz ) then
                    do jr = 1, n
                       z( jr, j ) = -z( jr, j )
                    end do
                 end if
              end if
              alphar( j ) = h( j, j )
              alphai( j ) = zero
              beta( j ) = t( j, j )
           end do
           ! if ihi < ilo, skip qz steps
           if( ihi<ilo )go to 380
           ! main qz iteration loop
           ! initialize dynamic indices
           ! eigenvalues ilast+1:n have been found.
              ! column operations modify rows ifrstm:whatever.
              ! row operations modify columns whatever:ilastm.
           ! if only eigenvalues are being computed, then
              ! ifrstm is the row of the last splitting row above row ilast;
              ! this is always at least ilo.
           ! iiter counts iterations since the last eigenvalue was found,
              ! to tell when to use an extraordinary shift.
           ! maxit is the maximum number of qz sweeps allowed.
           ilast = ihi
           if( ilschr ) then
              ifrstm = 1_ilp
              ilastm = n
           else
              ifrstm = ilo
              ilastm = ihi
           end if
           iiter = 0_ilp
           eshift = zero
           maxit = 30_ilp*( ihi-ilo+1 )
           loop_360: do jiter = 1, maxit
              ! split the matrix if possible.
              ! two tests:
                 ! 1: h(j,j-1)=0  or  j=ilo
                 ! 2: t(j,j)=0
              if( ilast==ilo ) then
                 ! special case: j=ilast
                 go to 80
              else
                 if( abs( h( ilast, ilast-1 ) )<=max( safmin, ulp*(abs( h( ilast, ilast ) ) + abs(&
                            h( ilast-1, ilast-1 ) )) ) ) then
                    h( ilast, ilast-1 ) = zero
                    go to 80
                 end if
              end if
              if( abs( t( ilast, ilast ) )<=max( safmin, ulp*(abs( t( ilast - 1_ilp, ilast ) ) + abs( &
                        t( ilast-1, ilast-1 )) ) ) ) then
                 t( ilast, ilast ) = zero
                 go to 70
              end if
              ! general case: j<ilast
              loop_60: do j = ilast - 1, ilo, -1
                 ! test 1: for h(j,j-1)=0 or j=ilo
                 if( j==ilo ) then
                    ilazro = .true.
                 else
                    if( abs( h( j, j-1 ) )<=max( safmin, ulp*(abs( h( j, j ) ) + abs( h( j-1, j-1 &
                              ) )) ) ) then
                       h( j, j-1 ) = zero
                       ilazro = .true.
                    else
                       ilazro = .false.
                    end if
                 end if
                 ! test 2: for t(j,j)=0
                 temp = abs ( t( j, j + 1_ilp ) )
                 if ( j > ilo )temp = temp + abs ( t( j - 1_ilp, j ) )
                 if( abs( t( j, j ) )<max( safmin,ulp*temp ) ) then
                    t( j, j ) = zero
                    ! test 1a: check for 2 consecutive small subdiagonals in a
                    ilazr2 = .false.
                    if( .not.ilazro ) then
                       temp = abs( h( j, j-1 ) )
                       temp2 = abs( h( j, j ) )
                       tempr = max( temp, temp2 )
                       if( tempr<one .and. tempr/=zero ) then
                          temp = temp / tempr
                          temp2 = temp2 / tempr
                       end if
                       if( temp*( ascale*abs( h( j+1, j ) ) )<=temp2*( ascale*atol ) )ilazr2 = &
                                 .true.
                    end if
                    ! if both tests pass (1
                    ! element of b in the block is zero, split a 1x1 block off
                    ! at the top. (i.e., at the j-th row/column) the leading
                    ! diagonal element of the remainder can also be zero, so
                    ! this may have to be done repeatedly.
                    if( ilazro .or. ilazr2 ) then
                       do jch = j, ilast - 1
                          temp = h( jch, jch )
                          call stdlib_slartg( temp, h( jch+1, jch ), c, s,h( jch, jch ) )
                          h( jch+1, jch ) = zero
                          call stdlib_srot( ilastm-jch, h( jch, jch+1 ), ldh,h( jch+1, jch+1 ), &
                                    ldh, c, s )
                          call stdlib_srot( ilastm-jch, t( jch, jch+1 ), ldt,t( jch+1, jch+1 ), &
                                    ldt, c, s )
                          if( ilq )call stdlib_srot( n, q( 1_ilp, jch ), 1_ilp, q( 1_ilp, jch+1 ), 1_ilp,c, s )
                                    
                          if( ilazr2 )h( jch, jch-1 ) = h( jch, jch-1 )*c
                          ilazr2 = .false.
                          if( abs( t( jch+1, jch+1 ) )>=btol ) then
                             if( jch+1>=ilast ) then
                                go to 80
                             else
                                ifirst = jch + 1_ilp
                                go to 110
                             end if
                          end if
                          t( jch+1, jch+1 ) = zero
                       end do
                       go to 70
                    else
                       ! only test 2 passed -- chase the zero to t(ilast,ilast)
                       ! then process as in the case t(ilast,ilast)=0
                       do jch = j, ilast - 1
                          temp = t( jch, jch+1 )
                          call stdlib_slartg( temp, t( jch+1, jch+1 ), c, s,t( jch, jch+1 ) )
                                    
                          t( jch+1, jch+1 ) = zero
                          if( jch<ilastm-1 )call stdlib_srot( ilastm-jch-1, t( jch, jch+2 ), ldt,&
                                    t( jch+1, jch+2 ), ldt, c, s )
                          call stdlib_srot( ilastm-jch+2, h( jch, jch-1 ), ldh,h( jch+1, jch-1 ), &
                                    ldh, c, s )
                          if( ilq )call stdlib_srot( n, q( 1_ilp, jch ), 1_ilp, q( 1_ilp, jch+1 ), 1_ilp,c, s )
                                    
                          temp = h( jch+1, jch )
                          call stdlib_slartg( temp, h( jch+1, jch-1 ), c, s,h( jch+1, jch ) )
                                    
                          h( jch+1, jch-1 ) = zero
                          call stdlib_srot( jch+1-ifrstm, h( ifrstm, jch ), 1_ilp,h( ifrstm, jch-1 ), &
                                    1_ilp, c, s )
                          call stdlib_srot( jch-ifrstm, t( ifrstm, jch ), 1_ilp,t( ifrstm, jch-1 ), 1_ilp,&
                                     c, s )
                          if( ilz )call stdlib_srot( n, z( 1_ilp, jch ), 1_ilp, z( 1_ilp, jch-1 ), 1_ilp,c, s )
                                    
                       end do
                       go to 70
                    end if
                 else if( ilazro ) then
                    ! only test 1 passed -- work on j:ilast
                    ifirst = j
                    go to 110
                 end if
                 ! neither test passed -- try next j
              end do loop_60
              ! (drop-through is "impossible")
              info = n + 1_ilp
              go to 420
              ! t(ilast,ilast)=0 -- clear h(ilast,ilast-1) to split off a
              ! 1x1 block.
              70 continue
              temp = h( ilast, ilast )
              call stdlib_slartg( temp, h( ilast, ilast-1 ), c, s,h( ilast, ilast ) )
              h( ilast, ilast-1 ) = zero
              call stdlib_srot( ilast-ifrstm, h( ifrstm, ilast ), 1_ilp,h( ifrstm, ilast-1 ), 1_ilp, c, s &
                        )
              call stdlib_srot( ilast-ifrstm, t( ifrstm, ilast ), 1_ilp,t( ifrstm, ilast-1 ), 1_ilp, c, s &
                        )
              if( ilz )call stdlib_srot( n, z( 1_ilp, ilast ), 1_ilp, z( 1_ilp, ilast-1 ), 1_ilp, c, s )
              ! h(ilast,ilast-1)=0 -- standardize b, set alphar, alphai,
                                    ! and beta
                                    80 continue
              if( t( ilast, ilast )<zero ) then
                 if( ilschr ) then
                    do j = ifrstm, ilast
                       h( j, ilast ) = -h( j, ilast )
                       t( j, ilast ) = -t( j, ilast )
                    end do
                 else
                    h( ilast, ilast ) = -h( ilast, ilast )
                    t( ilast, ilast ) = -t( ilast, ilast )
                 end if
                 if( ilz ) then
                    do j = 1, n
                       z( j, ilast ) = -z( j, ilast )
                    end do
                 end if
              end if
              alphar( ilast ) = h( ilast, ilast )
              alphai( ilast ) = zero
              beta( ilast ) = t( ilast, ilast )
              ! go to next block -- exit if finished.
              ilast = ilast - 1_ilp
              if( ilast<ilo )go to 380
              ! reset counters
              iiter = 0_ilp
              eshift = zero
              if( .not.ilschr ) then
                 ilastm = ilast
                 if( ifrstm>ilast )ifrstm = ilo
              end if
              go to 350
              ! qz step
              ! this iteration only involves rows/columns ifirst:ilast. we
              ! assume ifirst < ilast, and that the diagonal of b is non-zero.
              110 continue
              iiter = iiter + 1_ilp
              if( .not.ilschr ) then
                 ifrstm = ifirst
              end if
              ! compute single shifts.
              ! at this point, ifirst < ilast, and the diagonal elements of
              ! t(ifirst:ilast,ifirst,ilast) are larger than btol (in
              ! magnitude)
              if( ( iiter / 10_ilp )*10_ilp==iiter ) then
                 ! exceptional shift.  chosen for no particularly good reason.
                 ! (single shift only.)
                 if( ( real( maxit,KIND=sp)*safmin )*abs( h( ilast, ilast-1 ) )<abs( t( ilast-1, &
                           ilast-1 ) ) ) then
                    eshift = h( ilast, ilast-1 ) /t( ilast-1, ilast-1 )
                 else
                    eshift = eshift + one / ( safmin*real( maxit,KIND=sp) )
                 end if
                 s1 = one
                 wr = eshift
              else
                 ! shifts based on the generalized eigenvalues of the
                 ! bottom-right 2x2 block of a and b. the first eigenvalue
                 ! returned by stdlib_slag2 is the wilkinson shift (aep p.512_sp),
                 call stdlib_slag2( h( ilast-1, ilast-1 ), ldh,t( ilast-1, ilast-1 ), ldt, &
                           safmin*safety, s1,s2, wr, wr2, wi )
                 if ( abs( (wr/s1)*t( ilast, ilast ) - h( ilast, ilast ) )> abs( (wr2/s2)*t( &
                           ilast, ilast )- h( ilast, ilast ) ) ) then
                    temp = wr
                    wr = wr2
                    wr2 = temp
                    temp = s1
                    s1 = s2
                    s2 = temp
                 end if
                 temp = max( s1, safmin*max( one, abs( wr ), abs( wi ) ) )
                 if( wi/=zero )go to 200
              end if
              ! fiddle with shift to avoid overflow
              temp = min( ascale, one )*( half*safmax )
              if( s1>temp ) then
                 scale = temp / s1
              else
                 scale = one
              end if
              temp = min( bscale, one )*( half*safmax )
              if( abs( wr )>temp )scale = min( scale, temp / abs( wr ) )
              s1 = scale*s1
              wr = scale*wr
              ! now check for two consecutive small subdiagonals.
              do j = ilast - 1, ifirst + 1, -1
                 istart = j
                 temp = abs( s1*h( j, j-1 ) )
                 temp2 = abs( s1*h( j, j )-wr*t( j, j ) )
                 tempr = max( temp, temp2 )
                 if( tempr<one .and. tempr/=zero ) then
                    temp = temp / tempr
                    temp2 = temp2 / tempr
                 end if
                 if( abs( ( ascale*h( j+1, j ) )*temp )<=( ascale*atol )*temp2 )go to 130
              end do
              istart = ifirst
              130 continue
              ! do an implicit single-shift qz sweep.
              ! initial q
              temp = s1*h( istart, istart ) - wr*t( istart, istart )
              temp2 = s1*h( istart+1, istart )
              call stdlib_slartg( temp, temp2, c, s, tempr )
              ! sweep
              loop_190: do j = istart, ilast - 1
                 if( j>istart ) then
                    temp = h( j, j-1 )
                    call stdlib_slartg( temp, h( j+1, j-1 ), c, s, h( j, j-1 ) )
                    h( j+1, j-1 ) = zero
                 end if
                 do jc = j, ilastm
                    temp = c*h( j, jc ) + s*h( j+1, jc )
                    h( j+1, jc ) = -s*h( j, jc ) + c*h( j+1, jc )
                    h( j, jc ) = temp
                    temp2 = c*t( j, jc ) + s*t( j+1, jc )
                    t( j+1, jc ) = -s*t( j, jc ) + c*t( j+1, jc )
                    t( j, jc ) = temp2
                 end do
                 if( ilq ) then
                    do jr = 1, n
                       temp = c*q( jr, j ) + s*q( jr, j+1 )
                       q( jr, j+1 ) = -s*q( jr, j ) + c*q( jr, j+1 )
                       q( jr, j ) = temp
                    end do
                 end if
                 temp = t( j+1, j+1 )
                 call stdlib_slartg( temp, t( j+1, j ), c, s, t( j+1, j+1 ) )
                 t( j+1, j ) = zero
                 do jr = ifrstm, min( j+2, ilast )
                    temp = c*h( jr, j+1 ) + s*h( jr, j )
                    h( jr, j ) = -s*h( jr, j+1 ) + c*h( jr, j )
                    h( jr, j+1 ) = temp
                 end do
                 do jr = ifrstm, j
                    temp = c*t( jr, j+1 ) + s*t( jr, j )
                    t( jr, j ) = -s*t( jr, j+1 ) + c*t( jr, j )
                    t( jr, j+1 ) = temp
                 end do
                 if( ilz ) then
                    do jr = 1, n
                       temp = c*z( jr, j+1 ) + s*z( jr, j )
                       z( jr, j ) = -s*z( jr, j+1 ) + c*z( jr, j )
                       z( jr, j+1 ) = temp
                    end do
                 end if
              end do loop_190
              go to 350
              ! use francis double-shift
              ! note: the francis double-shift should work with real shifts,
                    ! but only if the block is at least 3x3.
                    ! this code may break if this point is reached with
                    ! a 2x2 block with real eigenvalues.
                    200 continue
              if( ifirst+1==ilast ) then
                 ! special case -- 2x2 block with complex eigenvectors
                 ! step 1: standardize, that is, rotate so that
                             ! ( b11  0  )
                         ! b = (         )  with b11 non-negative.
                             ! (  0  b22 )
                 call stdlib_slasv2( t( ilast-1, ilast-1 ), t( ilast-1, ilast ),t( ilast, ilast ),&
                            b22, b11, sr, cr, sl, cl )
                 if( b11<zero ) then
                    cr = -cr
                    sr = -sr
                    b11 = -b11
                    b22 = -b22
                 end if
                 call stdlib_srot( ilastm+1-ifirst, h( ilast-1, ilast-1 ), ldh,h( ilast, ilast-1 )&
                           , ldh, cl, sl )
                 call stdlib_srot( ilast+1-ifrstm, h( ifrstm, ilast-1 ), 1_ilp,h( ifrstm, ilast ), 1_ilp, &
                           cr, sr )
                 if( ilast<ilastm )call stdlib_srot( ilastm-ilast, t( ilast-1, ilast+1 ), ldt,t( &
                           ilast, ilast+1 ), ldt, cl, sl )
                 if( ifrstm<ilast-1 )call stdlib_srot( ifirst-ifrstm, t( ifrstm, ilast-1 ), 1_ilp,t( &
                           ifrstm, ilast ), 1_ilp, cr, sr )
                 if( ilq )call stdlib_srot( n, q( 1_ilp, ilast-1 ), 1_ilp, q( 1_ilp, ilast ), 1_ilp, cl,sl )
                           
                 if( ilz )call stdlib_srot( n, z( 1_ilp, ilast-1 ), 1_ilp, z( 1_ilp, ilast ), 1_ilp, cr,sr )
                           
                 t( ilast-1, ilast-1 ) = b11
                 t( ilast-1, ilast ) = zero
                 t( ilast, ilast-1 ) = zero
                 t( ilast, ilast ) = b22
                 ! if b22 is negative, negate column ilast
                 if( b22<zero ) then
                    do j = ifrstm, ilast
                       h( j, ilast ) = -h( j, ilast )
                       t( j, ilast ) = -t( j, ilast )
                    end do
                    if( ilz ) then
                       do j = 1, n
                          z( j, ilast ) = -z( j, ilast )
                       end do
                    end if
                    b22 = -b22
                 end if
                 ! step 2: compute alphar, alphai, and beta (see refs.)
                 ! recompute shift
                 call stdlib_slag2( h( ilast-1, ilast-1 ), ldh,t( ilast-1, ilast-1 ), ldt, &
                           safmin*safety, s1,temp, wr, temp2, wi )
                 ! if standardization has perturbed the shift onto real line,
                 ! do another (real single-shift) qr step.
                 if( wi==zero )go to 350
                 s1inv = one / s1
                 ! do eispack (qzval) computation of alpha and beta
                 a11 = h( ilast-1, ilast-1 )
                 a21 = h( ilast, ilast-1 )
                 a12 = h( ilast-1, ilast )
                 a22 = h( ilast, ilast )
                 ! compute complex givens rotation on right
                 ! (assume some element of c = (sa - wb) > unfl )
                                  ! __
                 ! (sa - wb) ( cz   -sz )
                           ! ( sz    cz )
                 c11r = s1*a11 - wr*b11
                 c11i = -wi*b11
                 c12 = s1*a12
                 c21 = s1*a21
                 c22r = s1*a22 - wr*b22
                 c22i = -wi*b22
                 if( abs( c11r )+abs( c11i )+abs( c12 )>abs( c21 )+abs( c22r )+abs( c22i ) ) &
                           then
                    t1 = stdlib_slapy3( c12, c11r, c11i )
                    cz = c12 / t1
                    szr = -c11r / t1
                    szi = -c11i / t1
                 else
                    cz = stdlib_slapy2( c22r, c22i )
                    if( cz<=safmin ) then
                       cz = zero
                       szr = one
                       szi = zero
                    else
                       tempr = c22r / cz
                       tempi = c22i / cz
                       t1 = stdlib_slapy2( cz, c21 )
                       cz = cz / t1
                       szr = -c21*tempr / t1
                       szi = c21*tempi / t1
                    end if
                 end if
                 ! compute givens rotation on left
                 ! (  cq   sq )
                 ! (  __      )  a or b
                 ! ( -sq   cq )
                 an = abs( a11 ) + abs( a12 ) + abs( a21 ) + abs( a22 )
                 bn = abs( b11 ) + abs( b22 )
                 wabs = abs( wr ) + abs( wi )
                 if( s1*an>wabs*bn ) then
                    cq = cz*b11
                    sqr = szr*b22
                    sqi = -szi*b22
                 else
                    a1r = cz*a11 + szr*a12
                    a1i = szi*a12
                    a2r = cz*a21 + szr*a22
                    a2i = szi*a22
                    cq = stdlib_slapy2( a1r, a1i )
                    if( cq<=safmin ) then
                       cq = zero
                       sqr = one
                       sqi = zero
                    else
                       tempr = a1r / cq
                       tempi = a1i / cq
                       sqr = tempr*a2r + tempi*a2i
                       sqi = tempi*a2r - tempr*a2i
                    end if
                 end if
                 t1 = stdlib_slapy3( cq, sqr, sqi )
                 cq = cq / t1
                 sqr = sqr / t1
                 sqi = sqi / t1
                 ! compute diagonal elements of qbz
                 tempr = sqr*szr - sqi*szi
                 tempi = sqr*szi + sqi*szr
                 b1r = cq*cz*b11 + tempr*b22
                 b1i = tempi*b22
                 b1a = stdlib_slapy2( b1r, b1i )
                 b2r = cq*cz*b22 + tempr*b11
                 b2i = -tempi*b11
                 b2a = stdlib_slapy2( b2r, b2i )
                 ! normalize so beta > 0, and im( alpha1 ) > 0
                 beta( ilast-1 ) = b1a
                 beta( ilast ) = b2a
                 alphar( ilast-1 ) = ( wr*b1a )*s1inv
                 alphai( ilast-1 ) = ( wi*b1a )*s1inv
                 alphar( ilast ) = ( wr*b2a )*s1inv
                 alphai( ilast ) = -( wi*b2a )*s1inv
                 ! step 3: go to next block -- exit if finished.
                 ilast = ifirst - 1_ilp
                 if( ilast<ilo )go to 380
                 ! reset counters
                 iiter = 0_ilp
                 eshift = zero
                 if( .not.ilschr ) then
                    ilastm = ilast
                    if( ifrstm>ilast )ifrstm = ilo
                 end if
                 go to 350
              else
                 ! usual case: 3x3 or larger block, using francis implicit
                             ! double-shift
                                          ! 2
                 ! eigenvalue equation is  w  - c w + d = 0,
                                               ! -1 2        -1
                 ! so compute 1st column of  (a b  )  - c a b   + d
                 ! using the formula in qzit (from eispack)
                 ! we assume that the block is at least 3x3
                 ad11 = ( ascale*h( ilast-1, ilast-1 ) ) /( bscale*t( ilast-1, ilast-1 ) )
                 ad21 = ( ascale*h( ilast, ilast-1 ) ) /( bscale*t( ilast-1, ilast-1 ) )
                 ad12 = ( ascale*h( ilast-1, ilast ) ) /( bscale*t( ilast, ilast ) )
                 ad22 = ( ascale*h( ilast, ilast ) ) /( bscale*t( ilast, ilast ) )
                 u12 = t( ilast-1, ilast ) / t( ilast, ilast )
                 ad11l = ( ascale*h( ifirst, ifirst ) ) /( bscale*t( ifirst, ifirst ) )
                 ad21l = ( ascale*h( ifirst+1, ifirst ) ) /( bscale*t( ifirst, ifirst ) )
                 ad12l = ( ascale*h( ifirst, ifirst+1 ) ) /( bscale*t( ifirst+1, ifirst+1 ) )
                           
                 ad22l = ( ascale*h( ifirst+1, ifirst+1 ) ) /( bscale*t( ifirst+1, ifirst+1 ) )
                           
                 ad32l = ( ascale*h( ifirst+2, ifirst+1 ) ) /( bscale*t( ifirst+1, ifirst+1 ) )
                           
                 u12l = t( ifirst, ifirst+1 ) / t( ifirst+1, ifirst+1 )
                 v( 1_ilp ) = ( ad11-ad11l )*( ad22-ad11l ) - ad12*ad21 +ad21*u12*ad11l + ( ad12l-&
                           ad11l*u12l )*ad21l
                 v( 2_ilp ) = ( ( ad22l-ad11l )-ad21l*u12l-( ad11-ad11l )-( ad22-ad11l )+ad21*u12 )&
                           *ad21l
                 v( 3_ilp ) = ad32l*ad21l
                 istart = ifirst
                 call stdlib_slarfg( 3_ilp, v( 1_ilp ), v( 2_ilp ), 1_ilp, tau )
                 v( 1_ilp ) = one
                 ! sweep
                 loop_290: do j = istart, ilast - 2
                    ! all but last elements: use 3x3 householder transforms.
                    ! zero (j-1)st column of a
                    if( j>istart ) then
                       v( 1_ilp ) = h( j, j-1 )
                       v( 2_ilp ) = h( j+1, j-1 )
                       v( 3_ilp ) = h( j+2, j-1 )
                       call stdlib_slarfg( 3_ilp, h( j, j-1 ), v( 2_ilp ), 1_ilp, tau )
                       v( 1_ilp ) = one
                       h( j+1, j-1 ) = zero
                       h( j+2, j-1 ) = zero
                    end if
                    do jc = j, ilastm
                       temp = tau*( h( j, jc )+v( 2_ilp )*h( j+1, jc )+v( 3_ilp )*h( j+2, jc ) )
                       h( j, jc ) = h( j, jc ) - temp
                       h( j+1, jc ) = h( j+1, jc ) - temp*v( 2_ilp )
                       h( j+2, jc ) = h( j+2, jc ) - temp*v( 3_ilp )
                       temp2 = tau*( t( j, jc )+v( 2_ilp )*t( j+1, jc )+v( 3_ilp )*t( j+2, jc ) )
                       t( j, jc ) = t( j, jc ) - temp2
                       t( j+1, jc ) = t( j+1, jc ) - temp2*v( 2_ilp )
                       t( j+2, jc ) = t( j+2, jc ) - temp2*v( 3_ilp )
                    end do
                    if( ilq ) then
                       do jr = 1, n
                          temp = tau*( q( jr, j )+v( 2_ilp )*q( jr, j+1 )+v( 3_ilp )*q( jr, j+2 ) )
                                    
                          q( jr, j ) = q( jr, j ) - temp
                          q( jr, j+1 ) = q( jr, j+1 ) - temp*v( 2_ilp )
                          q( jr, j+2 ) = q( jr, j+2 ) - temp*v( 3_ilp )
                       end do
                    end if
                    ! zero j-th column of b (see slagbc for details)
                    ! swap rows to pivot
                    ilpivt = .false.
                    temp = max( abs( t( j+1, j+1 ) ), abs( t( j+1, j+2 ) ) )
                    temp2 = max( abs( t( j+2, j+1 ) ), abs( t( j+2, j+2 ) ) )
                    if( max( temp, temp2 )<safmin ) then
                       scale = zero
                       u1 = one
                       u2 = zero
                       go to 250
                    else if( temp>=temp2 ) then
                       w11 = t( j+1, j+1 )
                       w21 = t( j+2, j+1 )
                       w12 = t( j+1, j+2 )
                       w22 = t( j+2, j+2 )
                       u1 = t( j+1, j )
                       u2 = t( j+2, j )
                    else
                       w21 = t( j+1, j+1 )
                       w11 = t( j+2, j+1 )
                       w22 = t( j+1, j+2 )
                       w12 = t( j+2, j+2 )
                       u2 = t( j+1, j )
                       u1 = t( j+2, j )
                    end if
                    ! swap columns if nec.
                    if( abs( w12 )>abs( w11 ) ) then
                       ilpivt = .true.
                       temp = w12
                       temp2 = w22
                       w12 = w11
                       w22 = w21
                       w11 = temp
                       w21 = temp2
                    end if
                    ! lu-factor
                    temp = w21 / w11
                    u2 = u2 - temp*u1
                    w22 = w22 - temp*w12
                    w21 = zero
                    ! compute scale
                    scale = one
                    if( abs( w22 )<safmin ) then
                       scale = zero
                       u2 = one
                       u1 = -w12 / w11
                       go to 250
                    end if
                    if( abs( w22 )<abs( u2 ) )scale = abs( w22 / u2 )
                    if( abs( w11 )<abs( u1 ) )scale = min( scale, abs( w11 / u1 ) )
                    ! solve
                    u2 = ( scale*u2 ) / w22
                    u1 = ( scale*u1-w12*u2 ) / w11
                    250 continue
                    if( ilpivt ) then
                       temp = u2
                       u2 = u1
                       u1 = temp
                    end if
                    ! compute householder vector
                    t1 = sqrt( scale**2_ilp+u1**2_ilp+u2**2_ilp )
                    tau = one + scale / t1
                    vs = -one / ( scale+t1 )
                    v( 1_ilp ) = one
                    v( 2_ilp ) = vs*u1
                    v( 3_ilp ) = vs*u2
                    ! apply transformations from the right.
                    do jr = ifrstm, min( j+3, ilast )
                       temp = tau*( h( jr, j )+v( 2_ilp )*h( jr, j+1 )+v( 3_ilp )*h( jr, j+2 ) )
                       h( jr, j ) = h( jr, j ) - temp
                       h( jr, j+1 ) = h( jr, j+1 ) - temp*v( 2_ilp )
                       h( jr, j+2 ) = h( jr, j+2 ) - temp*v( 3_ilp )
                    end do
                    do jr = ifrstm, j + 2
                       temp = tau*( t( jr, j )+v( 2_ilp )*t( jr, j+1 )+v( 3_ilp )*t( jr, j+2 ) )
                       t( jr, j ) = t( jr, j ) - temp
                       t( jr, j+1 ) = t( jr, j+1 ) - temp*v( 2_ilp )
                       t( jr, j+2 ) = t( jr, j+2 ) - temp*v( 3_ilp )
                    end do
                    if( ilz ) then
                       do jr = 1, n
                          temp = tau*( z( jr, j )+v( 2_ilp )*z( jr, j+1 )+v( 3_ilp )*z( jr, j+2 ) )
                                    
                          z( jr, j ) = z( jr, j ) - temp
                          z( jr, j+1 ) = z( jr, j+1 ) - temp*v( 2_ilp )
                          z( jr, j+2 ) = z( jr, j+2 ) - temp*v( 3_ilp )
                       end do
                    end if
                    t( j+1, j ) = zero
                    t( j+2, j ) = zero
                 end do loop_290
                 ! last elements: use givens rotations
                 ! rotations from the left
                 j = ilast - 1_ilp
                 temp = h( j, j-1 )
                 call stdlib_slartg( temp, h( j+1, j-1 ), c, s, h( j, j-1 ) )
                 h( j+1, j-1 ) = zero
                 do jc = j, ilastm
                    temp = c*h( j, jc ) + s*h( j+1, jc )
                    h( j+1, jc ) = -s*h( j, jc ) + c*h( j+1, jc )
                    h( j, jc ) = temp
                    temp2 = c*t( j, jc ) + s*t( j+1, jc )
                    t( j+1, jc ) = -s*t( j, jc ) + c*t( j+1, jc )
                    t( j, jc ) = temp2
                 end do
                 if( ilq ) then
                    do jr = 1, n
                       temp = c*q( jr, j ) + s*q( jr, j+1 )
                       q( jr, j+1 ) = -s*q( jr, j ) + c*q( jr, j+1 )
                       q( jr, j ) = temp
                    end do
                 end if
                 ! rotations from the right.
                 temp = t( j+1, j+1 )
                 call stdlib_slartg( temp, t( j+1, j ), c, s, t( j+1, j+1 ) )
                 t( j+1, j ) = zero
                 do jr = ifrstm, ilast
                    temp = c*h( jr, j+1 ) + s*h( jr, j )
                    h( jr, j ) = -s*h( jr, j+1 ) + c*h( jr, j )
                    h( jr, j+1 ) = temp
                 end do
                 do jr = ifrstm, ilast - 1
                    temp = c*t( jr, j+1 ) + s*t( jr, j )
                    t( jr, j ) = -s*t( jr, j+1 ) + c*t( jr, j )
                    t( jr, j+1 ) = temp
                 end do
                 if( ilz ) then
                    do jr = 1, n
                       temp = c*z( jr, j+1 ) + s*z( jr, j )
                       z( jr, j ) = -s*z( jr, j+1 ) + c*z( jr, j )
                       z( jr, j+1 ) = temp
                    end do
                 end if
                 ! end of double-shift code
              end if
              go to 350
              ! end of iteration loop
              350 continue
           end do loop_360
           ! drop-through = non-convergence
           info = ilast
           go to 420
           ! successful completion of all qz steps
           380 continue
           ! set eigenvalues 1:ilo-1
           do j = 1, ilo - 1
              if( t( j, j )<zero ) then
                 if( ilschr ) then
                    do jr = 1, j
                       h( jr, j ) = -h( jr, j )
                       t( jr, j ) = -t( jr, j )
                    end do
                 else
                    h( j, j ) = -h( j, j )
                    t( j, j ) = -t( j, j )
                 end if
                 if( ilz ) then
                    do jr = 1, n
                       z( jr, j ) = -z( jr, j )
                    end do
                 end if
              end if
              alphar( j ) = h( j, j )
              alphai( j ) = zero
              beta( j ) = t( j, j )
           end do
           ! normal termination
           info = 0_ilp
           ! exit (other than argument error) -- return optimal workspace size
           420 continue
           work( 1_ilp ) = real( n,KIND=sp)
           return
     end subroutine stdlib_shgeqz

     module subroutine stdlib_dhgeqz( job, compq, compz, n, ilo, ihi, h, ldh, t, ldt,alphar, alphai, &
     !! DHGEQZ computes the eigenvalues of a real matrix pair (H,T),
     !! where H is an upper Hessenberg matrix and T is upper triangular,
     !! using the double-shift QZ method.
     !! Matrix pairs of this type are produced by the reduction to
     !! generalized upper Hessenberg form of a real matrix pair (A,B):
     !! A = Q1*H*Z1**T,  B = Q1*T*Z1**T,
     !! as computed by DGGHRD.
     !! If JOB='S', then the Hessenberg-triangular pair (H,T) is
     !! also reduced to generalized Schur form,
     !! H = Q*S*Z**T,  T = Q*P*Z**T,
     !! where Q and Z are orthogonal matrices, P is an upper triangular
     !! matrix, and S is a quasi-triangular matrix with 1-by-1 and 2-by-2
     !! diagonal blocks.
     !! The 1-by-1 blocks correspond to real eigenvalues of the matrix pair
     !! (H,T) and the 2-by-2 blocks correspond to complex conjugate pairs of
     !! eigenvalues.
     !! Additionally, the 2-by-2 upper triangular diagonal blocks of P
     !! corresponding to 2-by-2 blocks of S are reduced to positive diagonal
     !! form, i.e., if S(j+1,j) is non-zero, then P(j+1,j) = P(j,j+1) = 0,
     !! P(j,j) > 0, and P(j+1,j+1) > 0.
     !! Optionally, the orthogonal matrix Q from the generalized Schur
     !! factorization may be postmultiplied into an input matrix Q1, and the
     !! orthogonal matrix Z may be postmultiplied into an input matrix Z1.
     !! If Q1 and Z1 are the orthogonal matrices from DGGHRD that reduced
     !! the matrix pair (A,B) to generalized upper Hessenberg form, then the
     !! output matrices Q1*Q and Z1*Z are the orthogonal factors from the
     !! generalized Schur factorization of (A,B):
     !! A = (Q1*Q)*S*(Z1*Z)**T,  B = (Q1*Q)*P*(Z1*Z)**T.
     !! To avoid overflow, eigenvalues of the matrix pair (H,T) (equivalently,
     !! of (A,B)) are computed as a pair of values (alpha,beta), where alpha is
     !! complex and beta real.
     !! If beta is nonzero, lambda = alpha / beta is an eigenvalue of the
     !! generalized nonsymmetric eigenvalue problem (GNEP)
     !! A*x = lambda*B*x
     !! and if alpha is nonzero, mu = beta / alpha is an eigenvalue of the
     !! alternate form of the GNEP
     !! mu*A*y = B*y.
     !! Real eigenvalues can be read directly from the generalized Schur
     !! form:
     !! alpha = S(i,i), beta = P(i,i).
     !! Ref: C.B. Moler
     !! Eigenvalue Problems", SIAM J. Numer. Anal., 10(1973),
     !! pp. 241--256.
               beta, q, ldq, z, ldz, work,lwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: compq, compz, job
           integer(ilp), intent(in) :: ihi, ilo, ldh, ldq, ldt, ldz, lwork, n
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           real(dp), intent(out) :: alphai(*), alphar(*), beta(*), work(*)
           real(dp), intent(inout) :: h(ldh,*), q(ldq,*), t(ldt,*), z(ldz,*)
        ! =====================================================================
           ! Parameters 
           real(dp), parameter :: safety = 1.0e+2_dp
          ! $                     safety = one )
           
           ! Local Scalars 
           logical(lk) :: ilazr2, ilazro, ilpivt, ilq, ilschr, ilz, lquery
           integer(ilp) :: icompq, icompz, ifirst, ifrstm, iiter, ilast, ilastm, in, ischur, &
                     istart, j, jc, jch, jiter, jr, maxit
           real(dp) :: a11, a12, a1i, a1r, a21, a22, a2i, a2r, ad11, ad11l, ad12, ad12l, ad21, &
           ad21l, ad22, ad22l, ad32l, an, anorm, ascale, atol, b11, b1a, b1i, b1r, b22, b2a, b2i, &
           b2r, bn, bnorm, bscale, btol, c, c11i, c11r, c12, c21, c22i, c22r, cl, cq, cr, cz, &
           eshift, s, s1, s1inv, s2, safmax, safmin, scale, sl, sqi, sqr, sr, szi, szr, t1, tau, &
           temp, temp2, tempi, tempr, u1, u12, u12l, u2, ulp, vs, w11, w12, w21, w22, wabs, wi, &
                     wr, wr2
           ! Local Arrays 
           real(dp) :: v(3_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! decode job, compq, compz
           if( stdlib_lsame( job, 'E' ) ) then
              ilschr = .false.
              ischur = 1_ilp
           else if( stdlib_lsame( job, 'S' ) ) then
              ilschr = .true.
              ischur = 2_ilp
           else
              ischur = 0_ilp
           end if
           if( stdlib_lsame( compq, 'N' ) ) then
              ilq = .false.
              icompq = 1_ilp
           else if( stdlib_lsame( compq, 'V' ) ) then
              ilq = .true.
              icompq = 2_ilp
           else if( stdlib_lsame( compq, 'I' ) ) then
              ilq = .true.
              icompq = 3_ilp
           else
              icompq = 0_ilp
           end if
           if( stdlib_lsame( compz, 'N' ) ) then
              ilz = .false.
              icompz = 1_ilp
           else if( stdlib_lsame( compz, 'V' ) ) then
              ilz = .true.
              icompz = 2_ilp
           else if( stdlib_lsame( compz, 'I' ) ) then
              ilz = .true.
              icompz = 3_ilp
           else
              icompz = 0_ilp
           end if
           ! check argument values
           info = 0_ilp
           work( 1_ilp ) = max( 1_ilp, n )
           lquery = ( lwork==-1_ilp )
           if( ischur==0_ilp ) then
              info = -1_ilp
           else if( icompq==0_ilp ) then
              info = -2_ilp
           else if( icompz==0_ilp ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( ilo<1_ilp ) then
              info = -5_ilp
           else if( ihi>n .or. ihi<ilo-1 ) then
              info = -6_ilp
           else if( ldh<n ) then
              info = -8_ilp
           else if( ldt<n ) then
              info = -10_ilp
           else if( ldq<1_ilp .or. ( ilq .and. ldq<n ) ) then
              info = -15_ilp
           else if( ldz<1_ilp .or. ( ilz .and. ldz<n ) ) then
              info = -17_ilp
           else if( lwork<max( 1_ilp, n ) .and. .not.lquery ) then
              info = -19_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DHGEQZ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n<=0_ilp ) then
              work( 1_ilp ) = real( 1_ilp,KIND=dp)
              return
           end if
           ! initialize q and z
           if( icompq==3_ilp )call stdlib_dlaset( 'FULL', n, n, zero, one, q, ldq )
           if( icompz==3_ilp )call stdlib_dlaset( 'FULL', n, n, zero, one, z, ldz )
           ! machine constants
           in = ihi + 1_ilp - ilo
           safmin = stdlib_dlamch( 'S' )
           safmax = one / safmin
           ulp = stdlib_dlamch( 'E' )*stdlib_dlamch( 'B' )
           anorm = stdlib_dlanhs( 'F', in, h( ilo, ilo ), ldh, work )
           bnorm = stdlib_dlanhs( 'F', in, t( ilo, ilo ), ldt, work )
           atol = max( safmin, ulp*anorm )
           btol = max( safmin, ulp*bnorm )
           ascale = one / max( safmin, anorm )
           bscale = one / max( safmin, bnorm )
           ! set eigenvalues ihi+1:n
           do j = ihi + 1, n
              if( t( j, j )<zero ) then
                 if( ilschr ) then
                    do jr = 1, j
                       h( jr, j ) = -h( jr, j )
                       t( jr, j ) = -t( jr, j )
                    end do
                 else
                    h( j, j ) = -h( j, j )
                    t( j, j ) = -t( j, j )
                 end if
                 if( ilz ) then
                    do jr = 1, n
                       z( jr, j ) = -z( jr, j )
                    end do
                 end if
              end if
              alphar( j ) = h( j, j )
              alphai( j ) = zero
              beta( j ) = t( j, j )
           end do
           ! if ihi < ilo, skip qz steps
           if( ihi<ilo )go to 380
           ! main qz iteration loop
           ! initialize dynamic indices
           ! eigenvalues ilast+1:n have been found.
              ! column operations modify rows ifrstm:whatever.
              ! row operations modify columns whatever:ilastm.
           ! if only eigenvalues are being computed, then
              ! ifrstm is the row of the last splitting row above row ilast;
              ! this is always at least ilo.
           ! iiter counts iterations since the last eigenvalue was found,
              ! to tell when to use an extraordinary shift.
           ! maxit is the maximum number of qz sweeps allowed.
           ilast = ihi
           if( ilschr ) then
              ifrstm = 1_ilp
              ilastm = n
           else
              ifrstm = ilo
              ilastm = ihi
           end if
           iiter = 0_ilp
           eshift = zero
           maxit = 30_ilp*( ihi-ilo+1 )
           loop_360: do jiter = 1, maxit
              ! split the matrix if possible.
              ! two tests:
                 ! 1: h(j,j-1)=0  or  j=ilo
                 ! 2: t(j,j)=0
              if( ilast==ilo ) then
                 ! special case: j=ilast
                 go to 80
              else
                 if( abs( h( ilast, ilast-1 ) )<=max( safmin, ulp*(abs( h( ilast, ilast ) ) + abs(&
                            h( ilast-1, ilast-1 ) )) ) ) then
                    h( ilast, ilast-1 ) = zero
                    go to 80
                 end if
              end if
              if( abs( t( ilast, ilast ) )<=max( safmin, ulp*(abs( t( ilast - 1_ilp, ilast ) ) + abs( &
                        t( ilast-1, ilast-1 )) ) ) ) then
                 t( ilast, ilast ) = zero
                 go to 70
              end if
              ! general case: j<ilast
              loop_60: do j = ilast - 1, ilo, -1
                 ! test 1: for h(j,j-1)=0 or j=ilo
                 if( j==ilo ) then
                    ilazro = .true.
                 else
                    if( abs( h( j, j-1 ) )<=max( safmin, ulp*(abs( h( j, j ) ) + abs( h( j-1, j-1 &
                              ) )) ) ) then
                       h( j, j-1 ) = zero
                       ilazro = .true.
                    else
                       ilazro = .false.
                    end if
                 end if
                 ! test 2: for t(j,j)=0
                 temp = abs ( t( j, j + 1_ilp ) )
                 if ( j > ilo )temp = temp + abs ( t( j - 1_ilp, j ) )
                 if( abs( t( j, j ) )<max( safmin,ulp*temp ) ) then
                    t( j, j ) = zero
                    ! test 1a: check for 2 consecutive small subdiagonals in a
                    ilazr2 = .false.
                    if( .not.ilazro ) then
                       temp = abs( h( j, j-1 ) )
                       temp2 = abs( h( j, j ) )
                       tempr = max( temp, temp2 )
                       if( tempr<one .and. tempr/=zero ) then
                          temp = temp / tempr
                          temp2 = temp2 / tempr
                       end if
                       if( temp*( ascale*abs( h( j+1, j ) ) )<=temp2*( ascale*atol ) )ilazr2 = &
                                 .true.
                    end if
                    ! if both tests pass (1
                    ! element of b in the block is zero, split a 1x1 block off
                    ! at the top. (i.e., at the j-th row/column) the leading
                    ! diagonal element of the remainder can also be zero, so
                    ! this may have to be done repeatedly.
                    if( ilazro .or. ilazr2 ) then
                       do jch = j, ilast - 1
                          temp = h( jch, jch )
                          call stdlib_dlartg( temp, h( jch+1, jch ), c, s,h( jch, jch ) )
                          h( jch+1, jch ) = zero
                          call stdlib_drot( ilastm-jch, h( jch, jch+1 ), ldh,h( jch+1, jch+1 ), &
                                    ldh, c, s )
                          call stdlib_drot( ilastm-jch, t( jch, jch+1 ), ldt,t( jch+1, jch+1 ), &
                                    ldt, c, s )
                          if( ilq )call stdlib_drot( n, q( 1_ilp, jch ), 1_ilp, q( 1_ilp, jch+1 ), 1_ilp,c, s )
                                    
                          if( ilazr2 )h( jch, jch-1 ) = h( jch, jch-1 )*c
                          ilazr2 = .false.
                          if( abs( t( jch+1, jch+1 ) )>=btol ) then
                             if( jch+1>=ilast ) then
                                go to 80
                             else
                                ifirst = jch + 1_ilp
                                go to 110
                             end if
                          end if
                          t( jch+1, jch+1 ) = zero
                       end do
                       go to 70
                    else
                       ! only test 2 passed -- chase the zero to t(ilast,ilast)
                       ! then process as in the case t(ilast,ilast)=0
                       do jch = j, ilast - 1
                          temp = t( jch, jch+1 )
                          call stdlib_dlartg( temp, t( jch+1, jch+1 ), c, s,t( jch, jch+1 ) )
                                    
                          t( jch+1, jch+1 ) = zero
                          if( jch<ilastm-1 )call stdlib_drot( ilastm-jch-1, t( jch, jch+2 ), ldt,&
                                    t( jch+1, jch+2 ), ldt, c, s )
                          call stdlib_drot( ilastm-jch+2, h( jch, jch-1 ), ldh,h( jch+1, jch-1 ), &
                                    ldh, c, s )
                          if( ilq )call stdlib_drot( n, q( 1_ilp, jch ), 1_ilp, q( 1_ilp, jch+1 ), 1_ilp,c, s )
                                    
                          temp = h( jch+1, jch )
                          call stdlib_dlartg( temp, h( jch+1, jch-1 ), c, s,h( jch+1, jch ) )
                                    
                          h( jch+1, jch-1 ) = zero
                          call stdlib_drot( jch+1-ifrstm, h( ifrstm, jch ), 1_ilp,h( ifrstm, jch-1 ), &
                                    1_ilp, c, s )
                          call stdlib_drot( jch-ifrstm, t( ifrstm, jch ), 1_ilp,t( ifrstm, jch-1 ), 1_ilp,&
                                     c, s )
                          if( ilz )call stdlib_drot( n, z( 1_ilp, jch ), 1_ilp, z( 1_ilp, jch-1 ), 1_ilp,c, s )
                                    
                       end do
                       go to 70
                    end if
                 else if( ilazro ) then
                    ! only test 1 passed -- work on j:ilast
                    ifirst = j
                    go to 110
                 end if
                 ! neither test passed -- try next j
              end do loop_60
              ! (drop-through is "impossible")
              info = n + 1_ilp
              go to 420
              ! t(ilast,ilast)=0 -- clear h(ilast,ilast-1) to split off a
              ! 1x1 block.
              70 continue
              temp = h( ilast, ilast )
              call stdlib_dlartg( temp, h( ilast, ilast-1 ), c, s,h( ilast, ilast ) )
              h( ilast, ilast-1 ) = zero
              call stdlib_drot( ilast-ifrstm, h( ifrstm, ilast ), 1_ilp,h( ifrstm, ilast-1 ), 1_ilp, c, s &
                        )
              call stdlib_drot( ilast-ifrstm, t( ifrstm, ilast ), 1_ilp,t( ifrstm, ilast-1 ), 1_ilp, c, s &
                        )
              if( ilz )call stdlib_drot( n, z( 1_ilp, ilast ), 1_ilp, z( 1_ilp, ilast-1 ), 1_ilp, c, s )
              ! h(ilast,ilast-1)=0 -- standardize b, set alphar, alphai,
                                    ! and beta
                                    80 continue
              if( t( ilast, ilast )<zero ) then
                 if( ilschr ) then
                    do j = ifrstm, ilast
                       h( j, ilast ) = -h( j, ilast )
                       t( j, ilast ) = -t( j, ilast )
                    end do
                 else
                    h( ilast, ilast ) = -h( ilast, ilast )
                    t( ilast, ilast ) = -t( ilast, ilast )
                 end if
                 if( ilz ) then
                    do j = 1, n
                       z( j, ilast ) = -z( j, ilast )
                    end do
                 end if
              end if
              alphar( ilast ) = h( ilast, ilast )
              alphai( ilast ) = zero
              beta( ilast ) = t( ilast, ilast )
              ! go to next block -- exit if finished.
              ilast = ilast - 1_ilp
              if( ilast<ilo )go to 380
              ! reset counters
              iiter = 0_ilp
              eshift = zero
              if( .not.ilschr ) then
                 ilastm = ilast
                 if( ifrstm>ilast )ifrstm = ilo
              end if
              go to 350
              ! qz step
              ! this iteration only involves rows/columns ifirst:ilast. we
              ! assume ifirst < ilast, and that the diagonal of b is non-zero.
              110 continue
              iiter = iiter + 1_ilp
              if( .not.ilschr ) then
                 ifrstm = ifirst
              end if
              ! compute single shifts.
              ! at this point, ifirst < ilast, and the diagonal elements of
              ! t(ifirst:ilast,ifirst,ilast) are larger than btol (in
              ! magnitude)
              if( ( iiter / 10_ilp )*10_ilp==iiter ) then
                 ! exceptional shift.  chosen for no particularly good reason.
                 ! (single shift only.)
                 if( ( real( maxit,KIND=dp)*safmin )*abs( h( ilast, ilast-1 ) )<abs( t( ilast-1, &
                           ilast-1 ) ) ) then
                    eshift = h( ilast, ilast-1 ) /t( ilast-1, ilast-1 )
                 else
                    eshift = eshift + one / ( safmin*real( maxit,KIND=dp) )
                 end if
                 s1 = one
                 wr = eshift
              else
                 ! shifts based on the generalized eigenvalues of the
                 ! bottom-right 2x2 block of a and b. the first eigenvalue
                 ! returned by stdlib_dlag2 is the wilkinson shift (aep p.512_dp),
                 call stdlib_dlag2( h( ilast-1, ilast-1 ), ldh,t( ilast-1, ilast-1 ), ldt, &
                           safmin*safety, s1,s2, wr, wr2, wi )
                 if ( abs( (wr/s1)*t( ilast, ilast ) - h( ilast, ilast ) )> abs( (wr2/s2)*t( &
                           ilast, ilast )- h( ilast, ilast ) ) ) then
                    temp = wr
                    wr = wr2
                    wr2 = temp
                    temp = s1
                    s1 = s2
                    s2 = temp
                 end if
                 temp = max( s1, safmin*max( one, abs( wr ), abs( wi ) ) )
                 if( wi/=zero )go to 200
              end if
              ! fiddle with shift to avoid overflow
              temp = min( ascale, one )*( half*safmax )
              if( s1>temp ) then
                 scale = temp / s1
              else
                 scale = one
              end if
              temp = min( bscale, one )*( half*safmax )
              if( abs( wr )>temp )scale = min( scale, temp / abs( wr ) )
              s1 = scale*s1
              wr = scale*wr
              ! now check for two consecutive small subdiagonals.
              do j = ilast - 1, ifirst + 1, -1
                 istart = j
                 temp = abs( s1*h( j, j-1 ) )
                 temp2 = abs( s1*h( j, j )-wr*t( j, j ) )
                 tempr = max( temp, temp2 )
                 if( tempr<one .and. tempr/=zero ) then
                    temp = temp / tempr
                    temp2 = temp2 / tempr
                 end if
                 if( abs( ( ascale*h( j+1, j ) )*temp )<=( ascale*atol )*temp2 )go to 130
              end do
              istart = ifirst
              130 continue
              ! do an implicit single-shift qz sweep.
              ! initial q
              temp = s1*h( istart, istart ) - wr*t( istart, istart )
              temp2 = s1*h( istart+1, istart )
              call stdlib_dlartg( temp, temp2, c, s, tempr )
              ! sweep
              loop_190: do j = istart, ilast - 1
                 if( j>istart ) then
                    temp = h( j, j-1 )
                    call stdlib_dlartg( temp, h( j+1, j-1 ), c, s, h( j, j-1 ) )
                    h( j+1, j-1 ) = zero
                 end if
                 do jc = j, ilastm
                    temp = c*h( j, jc ) + s*h( j+1, jc )
                    h( j+1, jc ) = -s*h( j, jc ) + c*h( j+1, jc )
                    h( j, jc ) = temp
                    temp2 = c*t( j, jc ) + s*t( j+1, jc )
                    t( j+1, jc ) = -s*t( j, jc ) + c*t( j+1, jc )
                    t( j, jc ) = temp2
                 end do
                 if( ilq ) then
                    do jr = 1, n
                       temp = c*q( jr, j ) + s*q( jr, j+1 )
                       q( jr, j+1 ) = -s*q( jr, j ) + c*q( jr, j+1 )
                       q( jr, j ) = temp
                    end do
                 end if
                 temp = t( j+1, j+1 )
                 call stdlib_dlartg( temp, t( j+1, j ), c, s, t( j+1, j+1 ) )
                 t( j+1, j ) = zero
                 do jr = ifrstm, min( j+2, ilast )
                    temp = c*h( jr, j+1 ) + s*h( jr, j )
                    h( jr, j ) = -s*h( jr, j+1 ) + c*h( jr, j )
                    h( jr, j+1 ) = temp
                 end do
                 do jr = ifrstm, j
                    temp = c*t( jr, j+1 ) + s*t( jr, j )
                    t( jr, j ) = -s*t( jr, j+1 ) + c*t( jr, j )
                    t( jr, j+1 ) = temp
                 end do
                 if( ilz ) then
                    do jr = 1, n
                       temp = c*z( jr, j+1 ) + s*z( jr, j )
                       z( jr, j ) = -s*z( jr, j+1 ) + c*z( jr, j )
                       z( jr, j+1 ) = temp
                    end do
                 end if
              end do loop_190
              go to 350
              ! use francis double-shift
              ! note: the francis double-shift should work with real shifts,
                    ! but only if the block is at least 3x3.
                    ! this code may break if this point is reached with
                    ! a 2x2 block with real eigenvalues.
                    200 continue
              if( ifirst+1==ilast ) then
                 ! special case -- 2x2 block with complex eigenvectors
                 ! step 1: standardize, that is, rotate so that
                             ! ( b11  0  )
                         ! b = (         )  with b11 non-negative.
                             ! (  0  b22 )
                 call stdlib_dlasv2( t( ilast-1, ilast-1 ), t( ilast-1, ilast ),t( ilast, ilast ),&
                            b22, b11, sr, cr, sl, cl )
                 if( b11<zero ) then
                    cr = -cr
                    sr = -sr
                    b11 = -b11
                    b22 = -b22
                 end if
                 call stdlib_drot( ilastm+1-ifirst, h( ilast-1, ilast-1 ), ldh,h( ilast, ilast-1 )&
                           , ldh, cl, sl )
                 call stdlib_drot( ilast+1-ifrstm, h( ifrstm, ilast-1 ), 1_ilp,h( ifrstm, ilast ), 1_ilp, &
                           cr, sr )
                 if( ilast<ilastm )call stdlib_drot( ilastm-ilast, t( ilast-1, ilast+1 ), ldt,t( &
                           ilast, ilast+1 ), ldt, cl, sl )
                 if( ifrstm<ilast-1 )call stdlib_drot( ifirst-ifrstm, t( ifrstm, ilast-1 ), 1_ilp,t( &
                           ifrstm, ilast ), 1_ilp, cr, sr )
                 if( ilq )call stdlib_drot( n, q( 1_ilp, ilast-1 ), 1_ilp, q( 1_ilp, ilast ), 1_ilp, cl,sl )
                           
                 if( ilz )call stdlib_drot( n, z( 1_ilp, ilast-1 ), 1_ilp, z( 1_ilp, ilast ), 1_ilp, cr,sr )
                           
                 t( ilast-1, ilast-1 ) = b11
                 t( ilast-1, ilast ) = zero
                 t( ilast, ilast-1 ) = zero
                 t( ilast, ilast ) = b22
                 ! if b22 is negative, negate column ilast
                 if( b22<zero ) then
                    do j = ifrstm, ilast
                       h( j, ilast ) = -h( j, ilast )
                       t( j, ilast ) = -t( j, ilast )
                    end do
                    if( ilz ) then
                       do j = 1, n
                          z( j, ilast ) = -z( j, ilast )
                       end do
                    end if
                    b22 = -b22
                 end if
                 ! step 2: compute alphar, alphai, and beta (see refs.)
                 ! recompute shift
                 call stdlib_dlag2( h( ilast-1, ilast-1 ), ldh,t( ilast-1, ilast-1 ), ldt, &
                           safmin*safety, s1,temp, wr, temp2, wi )
                 ! if standardization has perturbed the shift onto real line,
                 ! do another (real single-shift) qr step.
                 if( wi==zero )go to 350
                 s1inv = one / s1
                 ! do eispack (qzval) computation of alpha and beta
                 a11 = h( ilast-1, ilast-1 )
                 a21 = h( ilast, ilast-1 )
                 a12 = h( ilast-1, ilast )
                 a22 = h( ilast, ilast )
                 ! compute complex givens rotation on right
                 ! (assume some element of c = (sa - wb) > unfl )
                                  ! __
                 ! (sa - wb) ( cz   -sz )
                           ! ( sz    cz )
                 c11r = s1*a11 - wr*b11
                 c11i = -wi*b11
                 c12 = s1*a12
                 c21 = s1*a21
                 c22r = s1*a22 - wr*b22
                 c22i = -wi*b22
                 if( abs( c11r )+abs( c11i )+abs( c12 )>abs( c21 )+abs( c22r )+abs( c22i ) ) &
                           then
                    t1 = stdlib_dlapy3( c12, c11r, c11i )
                    cz = c12 / t1
                    szr = -c11r / t1
                    szi = -c11i / t1
                 else
                    cz = stdlib_dlapy2( c22r, c22i )
                    if( cz<=safmin ) then
                       cz = zero
                       szr = one
                       szi = zero
                    else
                       tempr = c22r / cz
                       tempi = c22i / cz
                       t1 = stdlib_dlapy2( cz, c21 )
                       cz = cz / t1
                       szr = -c21*tempr / t1
                       szi = c21*tempi / t1
                    end if
                 end if
                 ! compute givens rotation on left
                 ! (  cq   sq )
                 ! (  __      )  a or b
                 ! ( -sq   cq )
                 an = abs( a11 ) + abs( a12 ) + abs( a21 ) + abs( a22 )
                 bn = abs( b11 ) + abs( b22 )
                 wabs = abs( wr ) + abs( wi )
                 if( s1*an>wabs*bn ) then
                    cq = cz*b11
                    sqr = szr*b22
                    sqi = -szi*b22
                 else
                    a1r = cz*a11 + szr*a12
                    a1i = szi*a12
                    a2r = cz*a21 + szr*a22
                    a2i = szi*a22
                    cq = stdlib_dlapy2( a1r, a1i )
                    if( cq<=safmin ) then
                       cq = zero
                       sqr = one
                       sqi = zero
                    else
                       tempr = a1r / cq
                       tempi = a1i / cq
                       sqr = tempr*a2r + tempi*a2i
                       sqi = tempi*a2r - tempr*a2i
                    end if
                 end if
                 t1 = stdlib_dlapy3( cq, sqr, sqi )
                 cq = cq / t1
                 sqr = sqr / t1
                 sqi = sqi / t1
                 ! compute diagonal elements of qbz
                 tempr = sqr*szr - sqi*szi
                 tempi = sqr*szi + sqi*szr
                 b1r = cq*cz*b11 + tempr*b22
                 b1i = tempi*b22
                 b1a = stdlib_dlapy2( b1r, b1i )
                 b2r = cq*cz*b22 + tempr*b11
                 b2i = -tempi*b11
                 b2a = stdlib_dlapy2( b2r, b2i )
                 ! normalize so beta > 0, and im( alpha1 ) > 0
                 beta( ilast-1 ) = b1a
                 beta( ilast ) = b2a
                 alphar( ilast-1 ) = ( wr*b1a )*s1inv
                 alphai( ilast-1 ) = ( wi*b1a )*s1inv
                 alphar( ilast ) = ( wr*b2a )*s1inv
                 alphai( ilast ) = -( wi*b2a )*s1inv
                 ! step 3: go to next block -- exit if finished.
                 ilast = ifirst - 1_ilp
                 if( ilast<ilo )go to 380
                 ! reset counters
                 iiter = 0_ilp
                 eshift = zero
                 if( .not.ilschr ) then
                    ilastm = ilast
                    if( ifrstm>ilast )ifrstm = ilo
                 end if
                 go to 350
              else
                 ! usual case: 3x3 or larger block, using francis implicit
                             ! double-shift
                                          ! 2
                 ! eigenvalue equation is  w  - c w + d = 0,
                                               ! -1 2        -1
                 ! so compute 1st column of  (a b  )  - c a b   + d
                 ! using the formula in qzit (from eispack)
                 ! we assume that the block is at least 3x3
                 ad11 = ( ascale*h( ilast-1, ilast-1 ) ) /( bscale*t( ilast-1, ilast-1 ) )
                 ad21 = ( ascale*h( ilast, ilast-1 ) ) /( bscale*t( ilast-1, ilast-1 ) )
                 ad12 = ( ascale*h( ilast-1, ilast ) ) /( bscale*t( ilast, ilast ) )
                 ad22 = ( ascale*h( ilast, ilast ) ) /( bscale*t( ilast, ilast ) )
                 u12 = t( ilast-1, ilast ) / t( ilast, ilast )
                 ad11l = ( ascale*h( ifirst, ifirst ) ) /( bscale*t( ifirst, ifirst ) )
                 ad21l = ( ascale*h( ifirst+1, ifirst ) ) /( bscale*t( ifirst, ifirst ) )
                 ad12l = ( ascale*h( ifirst, ifirst+1 ) ) /( bscale*t( ifirst+1, ifirst+1 ) )
                           
                 ad22l = ( ascale*h( ifirst+1, ifirst+1 ) ) /( bscale*t( ifirst+1, ifirst+1 ) )
                           
                 ad32l = ( ascale*h( ifirst+2, ifirst+1 ) ) /( bscale*t( ifirst+1, ifirst+1 ) )
                           
                 u12l = t( ifirst, ifirst+1 ) / t( ifirst+1, ifirst+1 )
                 v( 1_ilp ) = ( ad11-ad11l )*( ad22-ad11l ) - ad12*ad21 +ad21*u12*ad11l + ( ad12l-&
                           ad11l*u12l )*ad21l
                 v( 2_ilp ) = ( ( ad22l-ad11l )-ad21l*u12l-( ad11-ad11l )-( ad22-ad11l )+ad21*u12 )&
                           *ad21l
                 v( 3_ilp ) = ad32l*ad21l
                 istart = ifirst
                 call stdlib_dlarfg( 3_ilp, v( 1_ilp ), v( 2_ilp ), 1_ilp, tau )
                 v( 1_ilp ) = one
                 ! sweep
                 loop_290: do j = istart, ilast - 2
                    ! all but last elements: use 3x3 householder transforms.
                    ! zero (j-1)st column of a
                    if( j>istart ) then
                       v( 1_ilp ) = h( j, j-1 )
                       v( 2_ilp ) = h( j+1, j-1 )
                       v( 3_ilp ) = h( j+2, j-1 )
                       call stdlib_dlarfg( 3_ilp, h( j, j-1 ), v( 2_ilp ), 1_ilp, tau )
                       v( 1_ilp ) = one
                       h( j+1, j-1 ) = zero
                       h( j+2, j-1 ) = zero
                    end if
                    do jc = j, ilastm
                       temp = tau*( h( j, jc )+v( 2_ilp )*h( j+1, jc )+v( 3_ilp )*h( j+2, jc ) )
                       h( j, jc ) = h( j, jc ) - temp
                       h( j+1, jc ) = h( j+1, jc ) - temp*v( 2_ilp )
                       h( j+2, jc ) = h( j+2, jc ) - temp*v( 3_ilp )
                       temp2 = tau*( t( j, jc )+v( 2_ilp )*t( j+1, jc )+v( 3_ilp )*t( j+2, jc ) )
                       t( j, jc ) = t( j, jc ) - temp2
                       t( j+1, jc ) = t( j+1, jc ) - temp2*v( 2_ilp )
                       t( j+2, jc ) = t( j+2, jc ) - temp2*v( 3_ilp )
                    end do
                    if( ilq ) then
                       do jr = 1, n
                          temp = tau*( q( jr, j )+v( 2_ilp )*q( jr, j+1 )+v( 3_ilp )*q( jr, j+2 ) )
                                    
                          q( jr, j ) = q( jr, j ) - temp
                          q( jr, j+1 ) = q( jr, j+1 ) - temp*v( 2_ilp )
                          q( jr, j+2 ) = q( jr, j+2 ) - temp*v( 3_ilp )
                       end do
                    end if
                    ! zero j-th column of b (see dlagbc for details)
                    ! swap rows to pivot
                    ilpivt = .false.
                    temp = max( abs( t( j+1, j+1 ) ), abs( t( j+1, j+2 ) ) )
                    temp2 = max( abs( t( j+2, j+1 ) ), abs( t( j+2, j+2 ) ) )
                    if( max( temp, temp2 )<safmin ) then
                       scale = zero
                       u1 = one
                       u2 = zero
                       go to 250
                    else if( temp>=temp2 ) then
                       w11 = t( j+1, j+1 )
                       w21 = t( j+2, j+1 )
                       w12 = t( j+1, j+2 )
                       w22 = t( j+2, j+2 )
                       u1 = t( j+1, j )
                       u2 = t( j+2, j )
                    else
                       w21 = t( j+1, j+1 )
                       w11 = t( j+2, j+1 )
                       w22 = t( j+1, j+2 )
                       w12 = t( j+2, j+2 )
                       u2 = t( j+1, j )
                       u1 = t( j+2, j )
                    end if
                    ! swap columns if nec.
                    if( abs( w12 )>abs( w11 ) ) then
                       ilpivt = .true.
                       temp = w12
                       temp2 = w22
                       w12 = w11
                       w22 = w21
                       w11 = temp
                       w21 = temp2
                    end if
                    ! lu-factor
                    temp = w21 / w11
                    u2 = u2 - temp*u1
                    w22 = w22 - temp*w12
                    w21 = zero
                    ! compute scale
                    scale = one
                    if( abs( w22 )<safmin ) then
                       scale = zero
                       u2 = one
                       u1 = -w12 / w11
                       go to 250
                    end if
                    if( abs( w22 )<abs( u2 ) )scale = abs( w22 / u2 )
                    if( abs( w11 )<abs( u1 ) )scale = min( scale, abs( w11 / u1 ) )
                    ! solve
                    u2 = ( scale*u2 ) / w22
                    u1 = ( scale*u1-w12*u2 ) / w11
                    250 continue
                    if( ilpivt ) then
                       temp = u2
                       u2 = u1
                       u1 = temp
                    end if
                    ! compute householder vector
                    t1 = sqrt( scale**2_ilp+u1**2_ilp+u2**2_ilp )
                    tau = one + scale / t1
                    vs = -one / ( scale+t1 )
                    v( 1_ilp ) = one
                    v( 2_ilp ) = vs*u1
                    v( 3_ilp ) = vs*u2
                    ! apply transformations from the right.
                    do jr = ifrstm, min( j+3, ilast )
                       temp = tau*( h( jr, j )+v( 2_ilp )*h( jr, j+1 )+v( 3_ilp )*h( jr, j+2 ) )
                       h( jr, j ) = h( jr, j ) - temp
                       h( jr, j+1 ) = h( jr, j+1 ) - temp*v( 2_ilp )
                       h( jr, j+2 ) = h( jr, j+2 ) - temp*v( 3_ilp )
                    end do
                    do jr = ifrstm, j + 2
                       temp = tau*( t( jr, j )+v( 2_ilp )*t( jr, j+1 )+v( 3_ilp )*t( jr, j+2 ) )
                       t( jr, j ) = t( jr, j ) - temp
                       t( jr, j+1 ) = t( jr, j+1 ) - temp*v( 2_ilp )
                       t( jr, j+2 ) = t( jr, j+2 ) - temp*v( 3_ilp )
                    end do
                    if( ilz ) then
                       do jr = 1, n
                          temp = tau*( z( jr, j )+v( 2_ilp )*z( jr, j+1 )+v( 3_ilp )*z( jr, j+2 ) )
                                    
                          z( jr, j ) = z( jr, j ) - temp
                          z( jr, j+1 ) = z( jr, j+1 ) - temp*v( 2_ilp )
                          z( jr, j+2 ) = z( jr, j+2 ) - temp*v( 3_ilp )
                       end do
                    end if
                    t( j+1, j ) = zero
                    t( j+2, j ) = zero
                 end do loop_290
                 ! last elements: use givens rotations
                 ! rotations from the left
                 j = ilast - 1_ilp
                 temp = h( j, j-1 )
                 call stdlib_dlartg( temp, h( j+1, j-1 ), c, s, h( j, j-1 ) )
                 h( j+1, j-1 ) = zero
                 do jc = j, ilastm
                    temp = c*h( j, jc ) + s*h( j+1, jc )
                    h( j+1, jc ) = -s*h( j, jc ) + c*h( j+1, jc )
                    h( j, jc ) = temp
                    temp2 = c*t( j, jc ) + s*t( j+1, jc )
                    t( j+1, jc ) = -s*t( j, jc ) + c*t( j+1, jc )
                    t( j, jc ) = temp2
                 end do
                 if( ilq ) then
                    do jr = 1, n
                       temp = c*q( jr, j ) + s*q( jr, j+1 )
                       q( jr, j+1 ) = -s*q( jr, j ) + c*q( jr, j+1 )
                       q( jr, j ) = temp
                    end do
                 end if
                 ! rotations from the right.
                 temp = t( j+1, j+1 )
                 call stdlib_dlartg( temp, t( j+1, j ), c, s, t( j+1, j+1 ) )
                 t( j+1, j ) = zero
                 do jr = ifrstm, ilast
                    temp = c*h( jr, j+1 ) + s*h( jr, j )
                    h( jr, j ) = -s*h( jr, j+1 ) + c*h( jr, j )
                    h( jr, j+1 ) = temp
                 end do
                 do jr = ifrstm, ilast - 1
                    temp = c*t( jr, j+1 ) + s*t( jr, j )
                    t( jr, j ) = -s*t( jr, j+1 ) + c*t( jr, j )
                    t( jr, j+1 ) = temp
                 end do
                 if( ilz ) then
                    do jr = 1, n
                       temp = c*z( jr, j+1 ) + s*z( jr, j )
                       z( jr, j ) = -s*z( jr, j+1 ) + c*z( jr, j )
                       z( jr, j+1 ) = temp
                    end do
                 end if
                 ! end of double-shift code
              end if
              go to 350
              ! end of iteration loop
              350 continue
           end do loop_360
           ! drop-through = non-convergence
           info = ilast
           go to 420
           ! successful completion of all qz steps
           380 continue
           ! set eigenvalues 1:ilo-1
           do j = 1, ilo - 1
              if( t( j, j )<zero ) then
                 if( ilschr ) then
                    do jr = 1, j
                       h( jr, j ) = -h( jr, j )
                       t( jr, j ) = -t( jr, j )
                    end do
                 else
                    h( j, j ) = -h( j, j )
                    t( j, j ) = -t( j, j )
                 end if
                 if( ilz ) then
                    do jr = 1, n
                       z( jr, j ) = -z( jr, j )
                    end do
                 end if
              end if
              alphar( j ) = h( j, j )
              alphai( j ) = zero
              beta( j ) = t( j, j )
           end do
           ! normal termination
           info = 0_ilp
           ! exit (other than argument error) -- return optimal workspace size
           420 continue
           work( 1_ilp ) = real( n,KIND=dp)
           return
     end subroutine stdlib_dhgeqz


     module subroutine stdlib_chgeqz( job, compq, compz, n, ilo, ihi, h, ldh, t, ldt,alpha, beta, q, ldq,&
     !! CHGEQZ computes the eigenvalues of a complex matrix pair (H,T),
     !! where H is an upper Hessenberg matrix and T is upper triangular,
     !! using the single-shift QZ method.
     !! Matrix pairs of this type are produced by the reduction to
     !! generalized upper Hessenberg form of a complex matrix pair (A,B):
     !! A = Q1*H*Z1**H,  B = Q1*T*Z1**H,
     !! as computed by CGGHRD.
     !! If JOB='S', then the Hessenberg-triangular pair (H,T) is
     !! also reduced to generalized Schur form,
     !! H = Q*S*Z**H,  T = Q*P*Z**H,
     !! where Q and Z are unitary matrices and S and P are upper triangular.
     !! Optionally, the unitary matrix Q from the generalized Schur
     !! factorization may be postmultiplied into an input matrix Q1, and the
     !! unitary matrix Z may be postmultiplied into an input matrix Z1.
     !! If Q1 and Z1 are the unitary matrices from CGGHRD that reduced
     !! the matrix pair (A,B) to generalized Hessenberg form, then the output
     !! matrices Q1*Q and Z1*Z are the unitary factors from the generalized
     !! Schur factorization of (A,B):
     !! A = (Q1*Q)*S*(Z1*Z)**H,  B = (Q1*Q)*P*(Z1*Z)**H.
     !! To avoid overflow, eigenvalues of the matrix pair (H,T)
     !! (equivalently, of (A,B)) are computed as a pair of complex values
     !! (alpha,beta).  If beta is nonzero, lambda = alpha / beta is an
     !! eigenvalue of the generalized nonsymmetric eigenvalue problem (GNEP)
     !! A*x = lambda*B*x
     !! and if alpha is nonzero, mu = beta / alpha is an eigenvalue of the
     !! alternate form of the GNEP
     !! mu*A*y = B*y.
     !! The values of alpha and beta for the i-th eigenvalue can be read
     !! directly from the generalized Schur form:  alpha = S(i,i),
     !! beta = P(i,i).
     !! Ref: C.B. Moler
     !! Eigenvalue Problems", SIAM J. Numer. Anal., 10(1973),
     !! pp. 241--256.
                z, ldz, work, lwork,rwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: compq, compz, job
           integer(ilp), intent(in) :: ihi, ilo, ldh, ldq, ldt, ldz, lwork, n
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           real(sp), intent(out) :: rwork(*)
           complex(sp), intent(out) :: alpha(*), beta(*), work(*)
           complex(sp), intent(inout) :: h(ldh,*), q(ldq,*), t(ldt,*), z(ldz,*)
        ! =====================================================================
           
           
           
           ! Local Scalars 
           logical(lk) :: ilazr2, ilazro, ilq, ilschr, ilz, lquery
           integer(ilp) :: icompq, icompz, ifirst, ifrstm, iiter, ilast, ilastm, in, ischur, &
                     istart, j, jc, jch, jiter, jr, maxit
           real(sp) :: absb, anorm, ascale, atol, bnorm, bscale, btol, c, safmin, temp, temp2, &
                     tempr, ulp
           complex(sp) :: abi22, ad11, ad12, ad21, ad22, ctemp, ctemp2, ctemp3, eshift, s, shift, &
                     signbc, u12, x, abi12, y
           ! Intrinsic Functions 
           ! Statement Functions 
           real(sp) :: abs1
           ! Statement Function Definitions 
           abs1( x ) = abs( real( x,KIND=sp) ) + abs( aimag( x ) )
           ! Executable Statements 
           ! decode job, compq, compz
           if( stdlib_lsame( job, 'E' ) ) then
              ilschr = .false.
              ischur = 1_ilp
           else if( stdlib_lsame( job, 'S' ) ) then
              ilschr = .true.
              ischur = 2_ilp
           else
              ilschr = .true.
              ischur = 0_ilp
           end if
           if( stdlib_lsame( compq, 'N' ) ) then
              ilq = .false.
              icompq = 1_ilp
           else if( stdlib_lsame( compq, 'V' ) ) then
              ilq = .true.
              icompq = 2_ilp
           else if( stdlib_lsame( compq, 'I' ) ) then
              ilq = .true.
              icompq = 3_ilp
           else
              ilq = .true.
              icompq = 0_ilp
           end if
           if( stdlib_lsame( compz, 'N' ) ) then
              ilz = .false.
              icompz = 1_ilp
           else if( stdlib_lsame( compz, 'V' ) ) then
              ilz = .true.
              icompz = 2_ilp
           else if( stdlib_lsame( compz, 'I' ) ) then
              ilz = .true.
              icompz = 3_ilp
           else
              ilz = .true.
              icompz = 0_ilp
           end if
           ! check argument values
           info = 0_ilp
           work( 1_ilp ) = max( 1_ilp, n )
           lquery = ( lwork==-1_ilp )
           if( ischur==0_ilp ) then
              info = -1_ilp
           else if( icompq==0_ilp ) then
              info = -2_ilp
           else if( icompz==0_ilp ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( ilo<1_ilp ) then
              info = -5_ilp
           else if( ihi>n .or. ihi<ilo-1 ) then
              info = -6_ilp
           else if( ldh<n ) then
              info = -8_ilp
           else if( ldt<n ) then
              info = -10_ilp
           else if( ldq<1_ilp .or. ( ilq .and. ldq<n ) ) then
              info = -14_ilp
           else if( ldz<1_ilp .or. ( ilz .and. ldz<n ) ) then
              info = -16_ilp
           else if( lwork<max( 1_ilp, n ) .and. .not.lquery ) then
              info = -18_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CHGEQZ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           ! work( 1 ) = cmplx( 1,KIND=sp)
           if( n<=0_ilp ) then
              work( 1_ilp ) = cmplx( 1_ilp,KIND=sp)
              return
           end if
           ! initialize q and z
           if( icompq==3_ilp )call stdlib_claset( 'FULL', n, n, czero, cone, q, ldq )
           if( icompz==3_ilp )call stdlib_claset( 'FULL', n, n, czero, cone, z, ldz )
           ! machine constants
           in = ihi + 1_ilp - ilo
           safmin = stdlib_slamch( 'S' )
           ulp = stdlib_slamch( 'E' )*stdlib_slamch( 'B' )
           anorm = stdlib_clanhs( 'F', in, h( ilo, ilo ), ldh, rwork )
           bnorm = stdlib_clanhs( 'F', in, t( ilo, ilo ), ldt, rwork )
           atol = max( safmin, ulp*anorm )
           btol = max( safmin, ulp*bnorm )
           ascale = one / max( safmin, anorm )
           bscale = one / max( safmin, bnorm )
           ! set eigenvalues ihi+1:n
           do j = ihi + 1, n
              absb = abs( t( j, j ) )
              if( absb>safmin ) then
                 signbc = conjg( t( j, j ) / absb )
                 t( j, j ) = absb
                 if( ilschr ) then
                    call stdlib_cscal( j-1, signbc, t( 1_ilp, j ), 1_ilp )
                    call stdlib_cscal( j, signbc, h( 1_ilp, j ), 1_ilp )
                 else
                    call stdlib_cscal( 1_ilp, signbc, h( j, j ), 1_ilp )
                 end if
                 if( ilz )call stdlib_cscal( n, signbc, z( 1_ilp, j ), 1_ilp )
              else
                 t( j, j ) = czero
              end if
              alpha( j ) = h( j, j )
              beta( j ) = t( j, j )
           end do
           ! if ihi < ilo, skip qz steps
           if( ihi<ilo )go to 190
           ! main qz iteration loop
           ! initialize dynamic indices
           ! eigenvalues ilast+1:n have been found.
              ! column operations modify rows ifrstm:whatever
              ! row operations modify columns whatever:ilastm
           ! if only eigenvalues are being computed, then
              ! ifrstm is the row of the last splitting row above row ilast;
              ! this is always at least ilo.
           ! iiter counts iterations since the last eigenvalue was found,
              ! to tell when to use an extraordinary shift.
           ! maxit is the maximum number of qz sweeps allowed.
           ilast = ihi
           if( ilschr ) then
              ifrstm = 1_ilp
              ilastm = n
           else
              ifrstm = ilo
              ilastm = ihi
           end if
           iiter = 0_ilp
           eshift = czero
           maxit = 30_ilp*( ihi-ilo+1 )
           loop_170: do jiter = 1, maxit
              ! check for too many iterations.
              if( jiter>maxit )go to 180
              ! split the matrix if possible.
              ! two tests:
                 ! 1: h(j,j-1)=0  or  j=ilo
                 ! 2: t(j,j)=0
              ! special case: j=ilast
              if( ilast==ilo ) then
                 go to 60
              else
                 if( abs1( h( ilast, ilast-1 ) )<=max( safmin, ulp*(abs1( h( ilast, ilast ) ) + &
                           abs1( h( ilast-1, ilast-1 )) ) ) ) then
                    h( ilast, ilast-1 ) = czero
                    go to 60
                 end if
              end if
              if( abs( t( ilast, ilast ) )<=max( safmin, ulp*(abs( t( ilast - 1_ilp, ilast ) ) + abs( &
                        t( ilast-1, ilast-1 )) ) ) ) then
                 t( ilast, ilast ) = czero
                 go to 50
              end if
              ! general case: j<ilast
              loop_40: do j = ilast - 1, ilo, -1
                 ! test 1: for h(j,j-1)=0 or j=ilo
                 if( j==ilo ) then
                    ilazro = .true.
                 else
                    if( abs1( h( j, j-1 ) )<=max( safmin, ulp*(abs1( h( j, j ) ) + abs1( h( j-1, &
                              j-1 ) )) ) ) then
                       h( j, j-1 ) = czero
                       ilazro = .true.
                    else
                       ilazro = .false.
                    end if
                 end if
                 ! test 2: for t(j,j)=0
                 temp = abs ( t( j, j + 1_ilp ) )
                 if ( j > ilo )temp = temp + abs ( t( j - 1_ilp, j ) )
                 if( abs( t( j, j ) )<max( safmin,ulp*temp ) ) then
                    t( j, j ) = czero
                    ! test 1a: check for 2 consecutive small subdiagonals in a
                    ilazr2 = .false.
                    if( .not.ilazro ) then
                       if( abs1( h( j, j-1 ) )*( ascale*abs1( h( j+1,j ) ) )<=abs1( h( j, j ) )*( &
                                 ascale*atol ) )ilazr2 = .true.
                    end if
                    ! if both tests pass (1
                    ! element of b in the block is zero, split a 1x1 block off
                    ! at the top. (i.e., at the j-th row/column) the leading
                    ! diagonal element of the remainder can also be zero, so
                    ! this may have to be done repeatedly.
                    if( ilazro .or. ilazr2 ) then
                       do jch = j, ilast - 1
                          ctemp = h( jch, jch )
                          call stdlib_clartg( ctemp, h( jch+1, jch ), c, s,h( jch, jch ) )
                          h( jch+1, jch ) = czero
                          call stdlib_crot( ilastm-jch, h( jch, jch+1 ), ldh,h( jch+1, jch+1 ), &
                                    ldh, c, s )
                          call stdlib_crot( ilastm-jch, t( jch, jch+1 ), ldt,t( jch+1, jch+1 ), &
                                    ldt, c, s )
                          if( ilq )call stdlib_crot( n, q( 1_ilp, jch ), 1_ilp, q( 1_ilp, jch+1 ), 1_ilp,c, conjg(&
                                     s ) )
                          if( ilazr2 )h( jch, jch-1 ) = h( jch, jch-1 )*c
                          ilazr2 = .false.
                          if( abs1( t( jch+1, jch+1 ) )>=btol ) then
                             if( jch+1>=ilast ) then
                                go to 60
                             else
                                ifirst = jch + 1_ilp
                                go to 70
                             end if
                          end if
                          t( jch+1, jch+1 ) = czero
                       end do
                       go to 50
                    else
                       ! only test 2 passed -- chase the zero to t(ilast,ilast)
                       ! then process as in the case t(ilast,ilast)=0
                       do jch = j, ilast - 1
                          ctemp = t( jch, jch+1 )
                          call stdlib_clartg( ctemp, t( jch+1, jch+1 ), c, s,t( jch, jch+1 ) )
                                    
                          t( jch+1, jch+1 ) = czero
                          if( jch<ilastm-1 )call stdlib_crot( ilastm-jch-1, t( jch, jch+2 ), ldt,&
                                    t( jch+1, jch+2 ), ldt, c, s )
                          call stdlib_crot( ilastm-jch+2, h( jch, jch-1 ), ldh,h( jch+1, jch-1 ), &
                                    ldh, c, s )
                          if( ilq )call stdlib_crot( n, q( 1_ilp, jch ), 1_ilp, q( 1_ilp, jch+1 ), 1_ilp,c, conjg(&
                                     s ) )
                          ctemp = h( jch+1, jch )
                          call stdlib_clartg( ctemp, h( jch+1, jch-1 ), c, s,h( jch+1, jch ) )
                                    
                          h( jch+1, jch-1 ) = czero
                          call stdlib_crot( jch+1-ifrstm, h( ifrstm, jch ), 1_ilp,h( ifrstm, jch-1 ), &
                                    1_ilp, c, s )
                          call stdlib_crot( jch-ifrstm, t( ifrstm, jch ), 1_ilp,t( ifrstm, jch-1 ), 1_ilp,&
                                     c, s )
                          if( ilz )call stdlib_crot( n, z( 1_ilp, jch ), 1_ilp, z( 1_ilp, jch-1 ), 1_ilp,c, s )
                                    
                       end do
                       go to 50
                    end if
                 else if( ilazro ) then
                    ! only test 1 passed -- work on j:ilast
                    ifirst = j
                    go to 70
                 end if
                 ! neither test passed -- try next j
              end do loop_40
              ! (drop-through is "impossible")
              info = 2_ilp*n + 1_ilp
              go to 210
              ! t(ilast,ilast)=0 -- clear h(ilast,ilast-1) to split off a
              ! 1x1 block.
              50 continue
              ctemp = h( ilast, ilast )
              call stdlib_clartg( ctemp, h( ilast, ilast-1 ), c, s,h( ilast, ilast ) )
              h( ilast, ilast-1 ) = czero
              call stdlib_crot( ilast-ifrstm, h( ifrstm, ilast ), 1_ilp,h( ifrstm, ilast-1 ), 1_ilp, c, s &
                        )
              call stdlib_crot( ilast-ifrstm, t( ifrstm, ilast ), 1_ilp,t( ifrstm, ilast-1 ), 1_ilp, c, s &
                        )
              if( ilz )call stdlib_crot( n, z( 1_ilp, ilast ), 1_ilp, z( 1_ilp, ilast-1 ), 1_ilp, c, s )
              ! h(ilast,ilast-1)=0 -- standardize b, set alpha and beta
              60 continue
              absb = abs( t( ilast, ilast ) )
              if( absb>safmin ) then
                 signbc = conjg( t( ilast, ilast ) / absb )
                 t( ilast, ilast ) = absb
                 if( ilschr ) then
                    call stdlib_cscal( ilast-ifrstm, signbc, t( ifrstm, ilast ), 1_ilp )
                    call stdlib_cscal( ilast+1-ifrstm, signbc, h( ifrstm, ilast ),1_ilp )
                 else
                    call stdlib_cscal( 1_ilp, signbc, h( ilast, ilast ), 1_ilp )
                 end if
                 if( ilz )call stdlib_cscal( n, signbc, z( 1_ilp, ilast ), 1_ilp )
              else
                 t( ilast, ilast ) = czero
              end if
              alpha( ilast ) = h( ilast, ilast )
              beta( ilast ) = t( ilast, ilast )
              ! go to next block -- exit if finished.
              ilast = ilast - 1_ilp
              if( ilast<ilo )go to 190
              ! reset counters
              iiter = 0_ilp
              eshift = czero
              if( .not.ilschr ) then
                 ilastm = ilast
                 if( ifrstm>ilast )ifrstm = ilo
              end if
              go to 160
              ! qz step
              ! this iteration only involves rows/columns ifirst:ilast.  we
              ! assume ifirst < ilast, and that the diagonal of b is non-zero.
              70 continue
              iiter = iiter + 1_ilp
              if( .not.ilschr ) then
                 ifrstm = ifirst
              end if
              ! compute the shift.
              ! at this point, ifirst < ilast, and the diagonal elements of
              ! t(ifirst:ilast,ifirst,ilast) are larger than btol (in
              ! magnitude)
              if( ( iiter / 10_ilp )*10_ilp/=iiter ) then
                 ! the wilkinson shift (aep p.512_sp), i.e., the eigenvalue of
                 ! the bottom-right 2x2 block of a inv(b) which is nearest to
                 ! the bottom-right element.
                 ! we factor b as u*d, where u has unit diagonals, and
                 ! compute (a*inv(d))*inv(u).
                 u12 = ( bscale*t( ilast-1, ilast ) ) /( bscale*t( ilast, ilast ) )
                 ad11 = ( ascale*h( ilast-1, ilast-1 ) ) /( bscale*t( ilast-1, ilast-1 ) )
                 ad21 = ( ascale*h( ilast, ilast-1 ) ) /( bscale*t( ilast-1, ilast-1 ) )
                 ad12 = ( ascale*h( ilast-1, ilast ) ) /( bscale*t( ilast, ilast ) )
                 ad22 = ( ascale*h( ilast, ilast ) ) /( bscale*t( ilast, ilast ) )
                 abi22 = ad22 - u12*ad21
                 abi12 = ad12 - u12*ad11
                 shift = abi22
                 ctemp = sqrt( abi12 )*sqrt( ad21 )
                 temp = abs1( ctemp )
                 if( ctemp/=zero ) then
                    x = half*( ad11-shift )
                    temp2 = abs1( x )
                    temp = max( temp, abs1( x ) )
                    y = temp*sqrt( ( x / temp )**2_ilp+( ctemp / temp )**2_ilp )
                    if( temp2>zero ) then
                       if( real( x / temp2,KIND=sp)*real( y,KIND=sp)+aimag( x / temp2 )*aimag( y )&
                                 <zero )y = -y
                    end if
                    shift = shift - ctemp*stdlib_cladiv( ctemp, ( x+y ) )
                 end if
              else
                 ! exceptional shift.  chosen for no particularly good reason.
                 if( ( iiter / 20_ilp )*20_ilp==iiter .and.bscale*abs1(t( ilast, ilast ))>safmin ) &
                           then
                    eshift = eshift + ( ascale*h( ilast,ilast ) )/( bscale*t( ilast, ilast ) )
                              
                 else
                    eshift = eshift + ( ascale*h( ilast,ilast-1 ) )/( bscale*t( ilast-1, ilast-1 )&
                               )
                 end if
                 shift = eshift
              end if
              ! now check for two consecutive small subdiagonals.
              do j = ilast - 1, ifirst + 1, -1
                 istart = j
                 ctemp = ascale*h( j, j ) - shift*( bscale*t( j, j ) )
                 temp = abs1( ctemp )
                 temp2 = ascale*abs1( h( j+1, j ) )
                 tempr = max( temp, temp2 )
                 if( tempr<one .and. tempr/=zero ) then
                    temp = temp / tempr
                    temp2 = temp2 / tempr
                 end if
                 if( abs1( h( j, j-1 ) )*temp2<=temp*atol )go to 90
              end do
              istart = ifirst
              ctemp = ascale*h( ifirst, ifirst ) -shift*( bscale*t( ifirst, ifirst ) )
              90 continue
              ! do an implicit-shift qz sweep.
              ! initial q
              ctemp2 = ascale*h( istart+1, istart )
              call stdlib_clartg( ctemp, ctemp2, c, s, ctemp3 )
              ! sweep
              loop_150: do j = istart, ilast - 1
                 if( j>istart ) then
                    ctemp = h( j, j-1 )
                    call stdlib_clartg( ctemp, h( j+1, j-1 ), c, s, h( j, j-1 ) )
                    h( j+1, j-1 ) = czero
                 end if
                 do jc = j, ilastm
                    ctemp = c*h( j, jc ) + s*h( j+1, jc )
                    h( j+1, jc ) = -conjg( s )*h( j, jc ) + c*h( j+1, jc )
                    h( j, jc ) = ctemp
                    ctemp2 = c*t( j, jc ) + s*t( j+1, jc )
                    t( j+1, jc ) = -conjg( s )*t( j, jc ) + c*t( j+1, jc )
                    t( j, jc ) = ctemp2
                 end do
                 if( ilq ) then
                    do jr = 1, n
                       ctemp = c*q( jr, j ) + conjg( s )*q( jr, j+1 )
                       q( jr, j+1 ) = -s*q( jr, j ) + c*q( jr, j+1 )
                       q( jr, j ) = ctemp
                    end do
                 end if
                 ctemp = t( j+1, j+1 )
                 call stdlib_clartg( ctemp, t( j+1, j ), c, s, t( j+1, j+1 ) )
                 t( j+1, j ) = czero
                 do jr = ifrstm, min( j+2, ilast )
                    ctemp = c*h( jr, j+1 ) + s*h( jr, j )
                    h( jr, j ) = -conjg( s )*h( jr, j+1 ) + c*h( jr, j )
                    h( jr, j+1 ) = ctemp
                 end do
                 do jr = ifrstm, j
                    ctemp = c*t( jr, j+1 ) + s*t( jr, j )
                    t( jr, j ) = -conjg( s )*t( jr, j+1 ) + c*t( jr, j )
                    t( jr, j+1 ) = ctemp
                 end do
                 if( ilz ) then
                    do jr = 1, n
                       ctemp = c*z( jr, j+1 ) + s*z( jr, j )
                       z( jr, j ) = -conjg( s )*z( jr, j+1 ) + c*z( jr, j )
                       z( jr, j+1 ) = ctemp
                    end do
                 end if
              end do loop_150
              160 continue
           end do loop_170
           ! drop-through = non-convergence
           180 continue
           info = ilast
           go to 210
           ! successful completion of all qz steps
           190 continue
           ! set eigenvalues 1:ilo-1
           do j = 1, ilo - 1
              absb = abs( t( j, j ) )
              if( absb>safmin ) then
                 signbc = conjg( t( j, j ) / absb )
                 t( j, j ) = absb
                 if( ilschr ) then
                    call stdlib_cscal( j-1, signbc, t( 1_ilp, j ), 1_ilp )
                    call stdlib_cscal( j, signbc, h( 1_ilp, j ), 1_ilp )
                 else
                    call stdlib_cscal( 1_ilp, signbc, h( j, j ), 1_ilp )
                 end if
                 if( ilz )call stdlib_cscal( n, signbc, z( 1_ilp, j ), 1_ilp )
              else
                 t( j, j ) = czero
              end if
              alpha( j ) = h( j, j )
              beta( j ) = t( j, j )
           end do
           ! normal termination
           info = 0_ilp
           ! exit (other than argument error) -- return optimal workspace size
           210 continue
           work( 1_ilp ) = cmplx( n,KIND=sp)
           return
     end subroutine stdlib_chgeqz

     module subroutine stdlib_zhgeqz( job, compq, compz, n, ilo, ihi, h, ldh, t, ldt,alpha, beta, q, ldq,&
     !! ZHGEQZ computes the eigenvalues of a complex matrix pair (H,T),
     !! where H is an upper Hessenberg matrix and T is upper triangular,
     !! using the single-shift QZ method.
     !! Matrix pairs of this type are produced by the reduction to
     !! generalized upper Hessenberg form of a complex matrix pair (A,B):
     !! A = Q1*H*Z1**H,  B = Q1*T*Z1**H,
     !! as computed by ZGGHRD.
     !! If JOB='S', then the Hessenberg-triangular pair (H,T) is
     !! also reduced to generalized Schur form,
     !! H = Q*S*Z**H,  T = Q*P*Z**H,
     !! where Q and Z are unitary matrices and S and P are upper triangular.
     !! Optionally, the unitary matrix Q from the generalized Schur
     !! factorization may be postmultiplied into an input matrix Q1, and the
     !! unitary matrix Z may be postmultiplied into an input matrix Z1.
     !! If Q1 and Z1 are the unitary matrices from ZGGHRD that reduced
     !! the matrix pair (A,B) to generalized Hessenberg form, then the output
     !! matrices Q1*Q and Z1*Z are the unitary factors from the generalized
     !! Schur factorization of (A,B):
     !! A = (Q1*Q)*S*(Z1*Z)**H,  B = (Q1*Q)*P*(Z1*Z)**H.
     !! To avoid overflow, eigenvalues of the matrix pair (H,T)
     !! (equivalently, of (A,B)) are computed as a pair of complex values
     !! (alpha,beta).  If beta is nonzero, lambda = alpha / beta is an
     !! eigenvalue of the generalized nonsymmetric eigenvalue problem (GNEP)
     !! A*x = lambda*B*x
     !! and if alpha is nonzero, mu = beta / alpha is an eigenvalue of the
     !! alternate form of the GNEP
     !! mu*A*y = B*y.
     !! The values of alpha and beta for the i-th eigenvalue can be read
     !! directly from the generalized Schur form:  alpha = S(i,i),
     !! beta = P(i,i).
     !! Ref: C.B. Moler
     !! Eigenvalue Problems", SIAM J. Numer. Anal., 10(1973),
     !! pp. 241--256.
                z, ldz, work, lwork,rwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: compq, compz, job
           integer(ilp), intent(in) :: ihi, ilo, ldh, ldq, ldt, ldz, lwork, n
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           real(dp), intent(out) :: rwork(*)
           complex(dp), intent(out) :: alpha(*), beta(*), work(*)
           complex(dp), intent(inout) :: h(ldh,*), q(ldq,*), t(ldt,*), z(ldz,*)
        ! =====================================================================
           
           
           
           ! Local Scalars 
           logical(lk) :: ilazr2, ilazro, ilq, ilschr, ilz, lquery
           integer(ilp) :: icompq, icompz, ifirst, ifrstm, iiter, ilast, ilastm, in, ischur, &
                     istart, j, jc, jch, jiter, jr, maxit
           real(dp) :: absb, anorm, ascale, atol, bnorm, bscale, btol, c, safmin, temp, temp2, &
                     tempr, ulp
           complex(dp) :: abi22, ad11, ad12, ad21, ad22, ctemp, ctemp2, ctemp3, eshift, s, shift, &
                     signbc, u12, x, abi12, y
           ! Intrinsic Functions 
           ! Statement Functions 
           real(dp) :: abs1
           ! Statement Function Definitions 
           abs1( x ) = abs( real( x,KIND=dp) ) + abs( aimag( x ) )
           ! Executable Statements 
           ! decode job, compq, compz
           if( stdlib_lsame( job, 'E' ) ) then
              ilschr = .false.
              ischur = 1_ilp
           else if( stdlib_lsame( job, 'S' ) ) then
              ilschr = .true.
              ischur = 2_ilp
           else
              ilschr = .true.
              ischur = 0_ilp
           end if
           if( stdlib_lsame( compq, 'N' ) ) then
              ilq = .false.
              icompq = 1_ilp
           else if( stdlib_lsame( compq, 'V' ) ) then
              ilq = .true.
              icompq = 2_ilp
           else if( stdlib_lsame( compq, 'I' ) ) then
              ilq = .true.
              icompq = 3_ilp
           else
              ilq = .true.
              icompq = 0_ilp
           end if
           if( stdlib_lsame( compz, 'N' ) ) then
              ilz = .false.
              icompz = 1_ilp
           else if( stdlib_lsame( compz, 'V' ) ) then
              ilz = .true.
              icompz = 2_ilp
           else if( stdlib_lsame( compz, 'I' ) ) then
              ilz = .true.
              icompz = 3_ilp
           else
              ilz = .true.
              icompz = 0_ilp
           end if
           ! check argument values
           info = 0_ilp
           work( 1_ilp ) = max( 1_ilp, n )
           lquery = ( lwork==-1_ilp )
           if( ischur==0_ilp ) then
              info = -1_ilp
           else if( icompq==0_ilp ) then
              info = -2_ilp
           else if( icompz==0_ilp ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( ilo<1_ilp ) then
              info = -5_ilp
           else if( ihi>n .or. ihi<ilo-1 ) then
              info = -6_ilp
           else if( ldh<n ) then
              info = -8_ilp
           else if( ldt<n ) then
              info = -10_ilp
           else if( ldq<1_ilp .or. ( ilq .and. ldq<n ) ) then
              info = -14_ilp
           else if( ldz<1_ilp .or. ( ilz .and. ldz<n ) ) then
              info = -16_ilp
           else if( lwork<max( 1_ilp, n ) .and. .not.lquery ) then
              info = -18_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZHGEQZ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           ! work( 1 ) = cmplx( 1,KIND=dp)
           if( n<=0_ilp ) then
              work( 1_ilp ) = cmplx( 1_ilp,KIND=dp)
              return
           end if
           ! initialize q and z
           if( icompq==3_ilp )call stdlib_zlaset( 'FULL', n, n, czero, cone, q, ldq )
           if( icompz==3_ilp )call stdlib_zlaset( 'FULL', n, n, czero, cone, z, ldz )
           ! machine constants
           in = ihi + 1_ilp - ilo
           safmin = stdlib_dlamch( 'S' )
           ulp = stdlib_dlamch( 'E' )*stdlib_dlamch( 'B' )
           anorm = stdlib_zlanhs( 'F', in, h( ilo, ilo ), ldh, rwork )
           bnorm = stdlib_zlanhs( 'F', in, t( ilo, ilo ), ldt, rwork )
           atol = max( safmin, ulp*anorm )
           btol = max( safmin, ulp*bnorm )
           ascale = one / max( safmin, anorm )
           bscale = one / max( safmin, bnorm )
           ! set eigenvalues ihi+1:n
           do j = ihi + 1, n
              absb = abs( t( j, j ) )
              if( absb>safmin ) then
                 signbc = conjg( t( j, j ) / absb )
                 t( j, j ) = absb
                 if( ilschr ) then
                    call stdlib_zscal( j-1, signbc, t( 1_ilp, j ), 1_ilp )
                    call stdlib_zscal( j, signbc, h( 1_ilp, j ), 1_ilp )
                 else
                    call stdlib_zscal( 1_ilp, signbc, h( j, j ), 1_ilp )
                 end if
                 if( ilz )call stdlib_zscal( n, signbc, z( 1_ilp, j ), 1_ilp )
              else
                 t( j, j ) = czero
              end if
              alpha( j ) = h( j, j )
              beta( j ) = t( j, j )
           end do
           ! if ihi < ilo, skip qz steps
           if( ihi<ilo )go to 190
           ! main qz iteration loop
           ! initialize dynamic indices
           ! eigenvalues ilast+1:n have been found.
              ! column operations modify rows ifrstm:whatever
              ! row operations modify columns whatever:ilastm
           ! if only eigenvalues are being computed, then
              ! ifrstm is the row of the last splitting row above row ilast;
              ! this is always at least ilo.
           ! iiter counts iterations since the last eigenvalue was found,
              ! to tell when to use an extraordinary shift.
           ! maxit is the maximum number of qz sweeps allowed.
           ilast = ihi
           if( ilschr ) then
              ifrstm = 1_ilp
              ilastm = n
           else
              ifrstm = ilo
              ilastm = ihi
           end if
           iiter = 0_ilp
           eshift = czero
           maxit = 30_ilp*( ihi-ilo+1 )
           loop_170: do jiter = 1, maxit
              ! check for too many iterations.
              if( jiter>maxit )go to 180
              ! split the matrix if possible.
              ! two tests:
                 ! 1: h(j,j-1)=0  or  j=ilo
                 ! 2: t(j,j)=0
              ! special case: j=ilast
              if( ilast==ilo ) then
                 go to 60
              else
                 if( abs1( h( ilast, ilast-1 ) )<=max( safmin, ulp*(abs1( h( ilast, ilast ) ) + &
                           abs1( h( ilast-1, ilast-1 )) ) ) ) then
                    h( ilast, ilast-1 ) = czero
                    go to 60
                 end if
              end if
              if( abs( t( ilast, ilast ) )<=max( safmin, ulp*(abs( t( ilast - 1_ilp, ilast ) ) + abs( &
                        t( ilast-1, ilast-1 )) ) ) ) then
                 t( ilast, ilast ) = czero
                 go to 50
              end if
              ! general case: j<ilast
              loop_40: do j = ilast - 1, ilo, -1
                 ! test 1: for h(j,j-1)=0 or j=ilo
                 if( j==ilo ) then
                    ilazro = .true.
                 else
                    if( abs1( h( j, j-1 ) )<=max( safmin, ulp*(abs1( h( j, j ) ) + abs1( h( j-1, &
                              j-1 ) )) ) ) then
                       h( j, j-1 ) = czero
                       ilazro = .true.
                    else
                       ilazro = .false.
                    end if
                 end if
                 ! test 2: for t(j,j)=0
                 temp = abs ( t( j, j + 1_ilp ) )
                 if ( j > ilo )temp = temp + abs ( t( j - 1_ilp, j ) )
                 if( abs( t( j, j ) )<max( safmin,ulp*temp ) ) then
                    t( j, j ) = czero
                    ! test 1a: check for 2 consecutive small subdiagonals in a
                    ilazr2 = .false.
                    if( .not.ilazro ) then
                       if( abs1( h( j, j-1 ) )*( ascale*abs1( h( j+1,j ) ) )<=abs1( h( j, j ) )*( &
                                 ascale*atol ) )ilazr2 = .true.
                    end if
                    ! if both tests pass (1
                    ! element of b in the block is zero, split a 1x1 block off
                    ! at the top. (i.e., at the j-th row/column) the leading
                    ! diagonal element of the remainder can also be zero, so
                    ! this may have to be done repeatedly.
                    if( ilazro .or. ilazr2 ) then
                       do jch = j, ilast - 1
                          ctemp = h( jch, jch )
                          call stdlib_zlartg( ctemp, h( jch+1, jch ), c, s,h( jch, jch ) )
                          h( jch+1, jch ) = czero
                          call stdlib_zrot( ilastm-jch, h( jch, jch+1 ), ldh,h( jch+1, jch+1 ), &
                                    ldh, c, s )
                          call stdlib_zrot( ilastm-jch, t( jch, jch+1 ), ldt,t( jch+1, jch+1 ), &
                                    ldt, c, s )
                          if( ilq )call stdlib_zrot( n, q( 1_ilp, jch ), 1_ilp, q( 1_ilp, jch+1 ), 1_ilp,c, conjg(&
                                     s ) )
                          if( ilazr2 )h( jch, jch-1 ) = h( jch, jch-1 )*c
                          ilazr2 = .false.
                          if( abs1( t( jch+1, jch+1 ) )>=btol ) then
                             if( jch+1>=ilast ) then
                                go to 60
                             else
                                ifirst = jch + 1_ilp
                                go to 70
                             end if
                          end if
                          t( jch+1, jch+1 ) = czero
                       end do
                       go to 50
                    else
                       ! only test 2 passed -- chase the zero to t(ilast,ilast)
                       ! then process as in the case t(ilast,ilast)=0
                       do jch = j, ilast - 1
                          ctemp = t( jch, jch+1 )
                          call stdlib_zlartg( ctemp, t( jch+1, jch+1 ), c, s,t( jch, jch+1 ) )
                                    
                          t( jch+1, jch+1 ) = czero
                          if( jch<ilastm-1 )call stdlib_zrot( ilastm-jch-1, t( jch, jch+2 ), ldt,&
                                    t( jch+1, jch+2 ), ldt, c, s )
                          call stdlib_zrot( ilastm-jch+2, h( jch, jch-1 ), ldh,h( jch+1, jch-1 ), &
                                    ldh, c, s )
                          if( ilq )call stdlib_zrot( n, q( 1_ilp, jch ), 1_ilp, q( 1_ilp, jch+1 ), 1_ilp,c, conjg(&
                                     s ) )
                          ctemp = h( jch+1, jch )
                          call stdlib_zlartg( ctemp, h( jch+1, jch-1 ), c, s,h( jch+1, jch ) )
                                    
                          h( jch+1, jch-1 ) = czero
                          call stdlib_zrot( jch+1-ifrstm, h( ifrstm, jch ), 1_ilp,h( ifrstm, jch-1 ), &
                                    1_ilp, c, s )
                          call stdlib_zrot( jch-ifrstm, t( ifrstm, jch ), 1_ilp,t( ifrstm, jch-1 ), 1_ilp,&
                                     c, s )
                          if( ilz )call stdlib_zrot( n, z( 1_ilp, jch ), 1_ilp, z( 1_ilp, jch-1 ), 1_ilp,c, s )
                                    
                       end do
                       go to 50
                    end if
                 else if( ilazro ) then
                    ! only test 1 passed -- work on j:ilast
                    ifirst = j
                    go to 70
                 end if
                 ! neither test passed -- try next j
              end do loop_40
              ! (drop-through is "impossible")
              info = 2_ilp*n + 1_ilp
              go to 210
              ! t(ilast,ilast)=0 -- clear h(ilast,ilast-1) to split off a
              ! 1x1 block.
              50 continue
              ctemp = h( ilast, ilast )
              call stdlib_zlartg( ctemp, h( ilast, ilast-1 ), c, s,h( ilast, ilast ) )
              h( ilast, ilast-1 ) = czero
              call stdlib_zrot( ilast-ifrstm, h( ifrstm, ilast ), 1_ilp,h( ifrstm, ilast-1 ), 1_ilp, c, s &
                        )
              call stdlib_zrot( ilast-ifrstm, t( ifrstm, ilast ), 1_ilp,t( ifrstm, ilast-1 ), 1_ilp, c, s &
                        )
              if( ilz )call stdlib_zrot( n, z( 1_ilp, ilast ), 1_ilp, z( 1_ilp, ilast-1 ), 1_ilp, c, s )
              ! h(ilast,ilast-1)=0 -- standardize b, set alpha and beta
              60 continue
              absb = abs( t( ilast, ilast ) )
              if( absb>safmin ) then
                 signbc = conjg( t( ilast, ilast ) / absb )
                 t( ilast, ilast ) = absb
                 if( ilschr ) then
                    call stdlib_zscal( ilast-ifrstm, signbc, t( ifrstm, ilast ), 1_ilp )
                    call stdlib_zscal( ilast+1-ifrstm, signbc, h( ifrstm, ilast ),1_ilp )
                 else
                    call stdlib_zscal( 1_ilp, signbc, h( ilast, ilast ), 1_ilp )
                 end if
                 if( ilz )call stdlib_zscal( n, signbc, z( 1_ilp, ilast ), 1_ilp )
              else
                 t( ilast, ilast ) = czero
              end if
              alpha( ilast ) = h( ilast, ilast )
              beta( ilast ) = t( ilast, ilast )
              ! go to next block -- exit if finished.
              ilast = ilast - 1_ilp
              if( ilast<ilo )go to 190
              ! reset counters
              iiter = 0_ilp
              eshift = czero
              if( .not.ilschr ) then
                 ilastm = ilast
                 if( ifrstm>ilast )ifrstm = ilo
              end if
              go to 160
              ! qz step
              ! this iteration only involves rows/columns ifirst:ilast.  we
              ! assume ifirst < ilast, and that the diagonal of b is non-zero.
              70 continue
              iiter = iiter + 1_ilp
              if( .not.ilschr ) then
                 ifrstm = ifirst
              end if
              ! compute the shift.
              ! at this point, ifirst < ilast, and the diagonal elements of
              ! t(ifirst:ilast,ifirst,ilast) are larger than btol (in
              ! magnitude)
              if( ( iiter / 10_ilp )*10_ilp/=iiter ) then
                 ! the wilkinson shift (aep p.512_dp), i.e., the eigenvalue of
                 ! the bottom-right 2x2 block of a inv(b) which is nearest to
                 ! the bottom-right element.
                 ! we factor b as u*d, where u has unit diagonals, and
                 ! compute (a*inv(d))*inv(u).
                 u12 = ( bscale*t( ilast-1, ilast ) ) /( bscale*t( ilast, ilast ) )
                 ad11 = ( ascale*h( ilast-1, ilast-1 ) ) /( bscale*t( ilast-1, ilast-1 ) )
                 ad21 = ( ascale*h( ilast, ilast-1 ) ) /( bscale*t( ilast-1, ilast-1 ) )
                 ad12 = ( ascale*h( ilast-1, ilast ) ) /( bscale*t( ilast, ilast ) )
                 ad22 = ( ascale*h( ilast, ilast ) ) /( bscale*t( ilast, ilast ) )
                 abi22 = ad22 - u12*ad21
                 abi12 = ad12 - u12*ad11
                 shift = abi22
                 ctemp = sqrt( abi12 )*sqrt( ad21 )
                 temp = abs1( ctemp )
                 if( ctemp/=zero ) then
                    x = half*( ad11-shift )
                    temp2 = abs1( x )
                    temp = max( temp, abs1( x ) )
                    y = temp*sqrt( ( x / temp )**2_ilp+( ctemp / temp )**2_ilp )
                    if( temp2>zero ) then
                       if( real( x / temp2,KIND=dp)*real( y,KIND=dp)+aimag( x / temp2 )*aimag( y )&
                                 <zero )y = -y
                    end if
                    shift = shift - ctemp*stdlib_zladiv( ctemp, ( x+y ) )
                 end if
              else
                 ! exceptional shift.  chosen for no particularly good reason.
                 if( ( iiter / 20_ilp )*20_ilp==iiter .and.bscale*abs1(t( ilast, ilast ))>safmin ) &
                           then
                    eshift = eshift + ( ascale*h( ilast,ilast ) )/( bscale*t( ilast, ilast ) )
                              
                 else
                    eshift = eshift + ( ascale*h( ilast,ilast-1 ) )/( bscale*t( ilast-1, ilast-1 )&
                               )
                 end if
                 shift = eshift
              end if
              ! now check for two consecutive small subdiagonals.
              do j = ilast - 1, ifirst + 1, -1
                 istart = j
                 ctemp = ascale*h( j, j ) - shift*( bscale*t( j, j ) )
                 temp = abs1( ctemp )
                 temp2 = ascale*abs1( h( j+1, j ) )
                 tempr = max( temp, temp2 )
                 if( tempr<one .and. tempr/=zero ) then
                    temp = temp / tempr
                    temp2 = temp2 / tempr
                 end if
                 if( abs1( h( j, j-1 ) )*temp2<=temp*atol )go to 90
              end do
              istart = ifirst
              ctemp = ascale*h( ifirst, ifirst ) -shift*( bscale*t( ifirst, ifirst ) )
              90 continue
              ! do an implicit-shift qz sweep.
              ! initial q
              ctemp2 = ascale*h( istart+1, istart )
              call stdlib_zlartg( ctemp, ctemp2, c, s, ctemp3 )
              ! sweep
              loop_150: do j = istart, ilast - 1
                 if( j>istart ) then
                    ctemp = h( j, j-1 )
                    call stdlib_zlartg( ctemp, h( j+1, j-1 ), c, s, h( j, j-1 ) )
                    h( j+1, j-1 ) = czero
                 end if
                 do jc = j, ilastm
                    ctemp = c*h( j, jc ) + s*h( j+1, jc )
                    h( j+1, jc ) = -conjg( s )*h( j, jc ) + c*h( j+1, jc )
                    h( j, jc ) = ctemp
                    ctemp2 = c*t( j, jc ) + s*t( j+1, jc )
                    t( j+1, jc ) = -conjg( s )*t( j, jc ) + c*t( j+1, jc )
                    t( j, jc ) = ctemp2
                 end do
                 if( ilq ) then
                    do jr = 1, n
                       ctemp = c*q( jr, j ) + conjg( s )*q( jr, j+1 )
                       q( jr, j+1 ) = -s*q( jr, j ) + c*q( jr, j+1 )
                       q( jr, j ) = ctemp
                    end do
                 end if
                 ctemp = t( j+1, j+1 )
                 call stdlib_zlartg( ctemp, t( j+1, j ), c, s, t( j+1, j+1 ) )
                 t( j+1, j ) = czero
                 do jr = ifrstm, min( j+2, ilast )
                    ctemp = c*h( jr, j+1 ) + s*h( jr, j )
                    h( jr, j ) = -conjg( s )*h( jr, j+1 ) + c*h( jr, j )
                    h( jr, j+1 ) = ctemp
                 end do
                 do jr = ifrstm, j
                    ctemp = c*t( jr, j+1 ) + s*t( jr, j )
                    t( jr, j ) = -conjg( s )*t( jr, j+1 ) + c*t( jr, j )
                    t( jr, j+1 ) = ctemp
                 end do
                 if( ilz ) then
                    do jr = 1, n
                       ctemp = c*z( jr, j+1 ) + s*z( jr, j )
                       z( jr, j ) = -conjg( s )*z( jr, j+1 ) + c*z( jr, j )
                       z( jr, j+1 ) = ctemp
                    end do
                 end if
              end do loop_150
              160 continue
           end do loop_170
           ! drop-through = non-convergence
           180 continue
           info = ilast
           go to 210
           ! successful completion of all qz steps
           190 continue
           ! set eigenvalues 1:ilo-1
           do j = 1, ilo - 1
              absb = abs( t( j, j ) )
              if( absb>safmin ) then
                 signbc = conjg( t( j, j ) / absb )
                 t( j, j ) = absb
                 if( ilschr ) then
                    call stdlib_zscal( j-1, signbc, t( 1_ilp, j ), 1_ilp )
                    call stdlib_zscal( j, signbc, h( 1_ilp, j ), 1_ilp )
                 else
                    call stdlib_zscal( 1_ilp, signbc, h( j, j ), 1_ilp )
                 end if
                 if( ilz )call stdlib_zscal( n, signbc, z( 1_ilp, j ), 1_ilp )
              else
                 t( j, j ) = czero
              end if
              alpha( j ) = h( j, j )
              beta( j ) = t( j, j )
           end do
           ! normal termination
           info = 0_ilp
           ! exit (other than argument error) -- return optimal workspace size
           210 continue
           work( 1_ilp ) = cmplx( n,KIND=dp)
           return
     end subroutine stdlib_zhgeqz




     pure module subroutine stdlib_sggbak( job, side, n, ilo, ihi, lscale, rscale, m, v,ldv, info )
     !! SGGBAK forms the right or left eigenvectors of a real generalized
     !! eigenvalue problem A*x = lambda*B*x, by backward transformation on
     !! the computed eigenvectors of the balanced pair of matrices output by
     !! SGGBAL.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: job, side
           integer(ilp), intent(in) :: ihi, ilo, ldv, m, n
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           real(sp), intent(in) :: lscale(*), rscale(*)
           real(sp), intent(inout) :: v(ldv,*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: leftv, rightv
           integer(ilp) :: i, k
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           rightv = stdlib_lsame( side, 'R' )
           leftv = stdlib_lsame( side, 'L' )
           info = 0_ilp
           if( .not.stdlib_lsame( job, 'N' ) .and. .not.stdlib_lsame( job, 'P' ) &
                     .and..not.stdlib_lsame( job, 'S' ) .and. .not.stdlib_lsame( job, 'B' ) ) then
              info = -1_ilp
           else if( .not.rightv .and. .not.leftv ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( ilo<1_ilp ) then
              info = -4_ilp
           else if( n==0_ilp .and. ihi==0_ilp .and. ilo/=1_ilp ) then
              info = -4_ilp
           else if( n>0_ilp .and. ( ihi<ilo .or. ihi>max( 1_ilp, n ) ) )then
              info = -5_ilp
           else if( n==0_ilp .and. ilo==1_ilp .and. ihi/=0_ilp ) then
              info = -5_ilp
           else if( m<0_ilp ) then
              info = -8_ilp
           else if( ldv<max( 1_ilp, n ) ) then
              info = -10_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SGGBAK', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( m==0 )return
           if( stdlib_lsame( job, 'N' ) )return
           if( ilo==ihi )go to 30
           ! backward balance
           if( stdlib_lsame( job, 'S' ) .or. stdlib_lsame( job, 'B' ) ) then
              ! backward transformation on right eigenvectors
              if( rightv ) then
                 do i = ilo, ihi
                    call stdlib_sscal( m, rscale( i ), v( i, 1_ilp ), ldv )
                 end do
              end if
              ! backward transformation on left eigenvectors
              if( leftv ) then
                 do i = ilo, ihi
                    call stdlib_sscal( m, lscale( i ), v( i, 1_ilp ), ldv )
                 end do
              end if
           end if
           ! backward permutation
           30 continue
           if( stdlib_lsame( job, 'P' ) .or. stdlib_lsame( job, 'B' ) ) then
              ! backward permutation on right eigenvectors
              if( rightv ) then
                 if( ilo==1 )go to 50
                 loop_40: do i = ilo - 1, 1, -1
                    k = rscale( i )
                    if( k==i )cycle loop_40
                    call stdlib_sswap( m, v( i, 1_ilp ), ldv, v( k, 1_ilp ), ldv )
                 end do loop_40
                 50 continue
                 if( ihi==n )go to 70
                 loop_60: do i = ihi + 1, n
                    k = rscale( i )
                    if( k==i )cycle loop_60
                    call stdlib_sswap( m, v( i, 1_ilp ), ldv, v( k, 1_ilp ), ldv )
                 end do loop_60
              end if
              ! backward permutation on left eigenvectors
              70 continue
              if( leftv ) then
                 if( ilo==1 )go to 90
                 loop_80: do i = ilo - 1, 1, -1
                    k = lscale( i )
                    if( k==i )cycle loop_80
                    call stdlib_sswap( m, v( i, 1_ilp ), ldv, v( k, 1_ilp ), ldv )
                 end do loop_80
                 90 continue
                 if( ihi==n )go to 110
                 loop_100: do i = ihi + 1, n
                    k = lscale( i )
                    if( k==i )cycle loop_100
                    call stdlib_sswap( m, v( i, 1_ilp ), ldv, v( k, 1_ilp ), ldv )
                 end do loop_100
              end if
           end if
           110 continue
           return
     end subroutine stdlib_sggbak

     pure module subroutine stdlib_dggbak( job, side, n, ilo, ihi, lscale, rscale, m, v,ldv, info )
     !! DGGBAK forms the right or left eigenvectors of a real generalized
     !! eigenvalue problem A*x = lambda*B*x, by backward transformation on
     !! the computed eigenvectors of the balanced pair of matrices output by
     !! DGGBAL.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: job, side
           integer(ilp), intent(in) :: ihi, ilo, ldv, m, n
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           real(dp), intent(in) :: lscale(*), rscale(*)
           real(dp), intent(inout) :: v(ldv,*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: leftv, rightv
           integer(ilp) :: i, k
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           rightv = stdlib_lsame( side, 'R' )
           leftv = stdlib_lsame( side, 'L' )
           info = 0_ilp
           if( .not.stdlib_lsame( job, 'N' ) .and. .not.stdlib_lsame( job, 'P' ) &
                     .and..not.stdlib_lsame( job, 'S' ) .and. .not.stdlib_lsame( job, 'B' ) ) then
              info = -1_ilp
           else if( .not.rightv .and. .not.leftv ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( ilo<1_ilp ) then
              info = -4_ilp
           else if( n==0_ilp .and. ihi==0_ilp .and. ilo/=1_ilp ) then
              info = -4_ilp
           else if( n>0_ilp .and. ( ihi<ilo .or. ihi>max( 1_ilp, n ) ) )then
              info = -5_ilp
           else if( n==0_ilp .and. ilo==1_ilp .and. ihi/=0_ilp ) then
              info = -5_ilp
           else if( m<0_ilp ) then
              info = -8_ilp
           else if( ldv<max( 1_ilp, n ) ) then
              info = -10_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DGGBAK', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( m==0 )return
           if( stdlib_lsame( job, 'N' ) )return
           if( ilo==ihi )go to 30
           ! backward balance
           if( stdlib_lsame( job, 'S' ) .or. stdlib_lsame( job, 'B' ) ) then
              ! backward transformation on right eigenvectors
              if( rightv ) then
                 do i = ilo, ihi
                    call stdlib_dscal( m, rscale( i ), v( i, 1_ilp ), ldv )
                 end do
              end if
              ! backward transformation on left eigenvectors
              if( leftv ) then
                 do i = ilo, ihi
                    call stdlib_dscal( m, lscale( i ), v( i, 1_ilp ), ldv )
                 end do
              end if
           end if
           ! backward permutation
           30 continue
           if( stdlib_lsame( job, 'P' ) .or. stdlib_lsame( job, 'B' ) ) then
              ! backward permutation on right eigenvectors
              if( rightv ) then
                 if( ilo==1 )go to 50
                 loop_40: do i = ilo - 1, 1, -1
                    k = int(rscale( i ),KIND=ilp)
                    if( k==i )cycle loop_40
                    call stdlib_dswap( m, v( i, 1_ilp ), ldv, v( k, 1_ilp ), ldv )
                 end do loop_40
                 50 continue
                 if( ihi==n )go to 70
                 loop_60: do i = ihi + 1, n
                    k = int(rscale( i ),KIND=ilp)
                    if( k==i )cycle loop_60
                    call stdlib_dswap( m, v( i, 1_ilp ), ldv, v( k, 1_ilp ), ldv )
                 end do loop_60
              end if
              ! backward permutation on left eigenvectors
              70 continue
              if( leftv ) then
                 if( ilo==1 )go to 90
                 loop_80: do i = ilo - 1, 1, -1
                    k = int(lscale( i ),KIND=ilp)
                    if( k==i )cycle loop_80
                    call stdlib_dswap( m, v( i, 1_ilp ), ldv, v( k, 1_ilp ), ldv )
                 end do loop_80
                 90 continue
                 if( ihi==n )go to 110
                 loop_100: do i = ihi + 1, n
                    k = int(lscale( i ),KIND=ilp)
                    if( k==i )cycle loop_100
                    call stdlib_dswap( m, v( i, 1_ilp ), ldv, v( k, 1_ilp ), ldv )
                 end do loop_100
              end if
           end if
           110 continue
           return
     end subroutine stdlib_dggbak


     pure module subroutine stdlib_cggbak( job, side, n, ilo, ihi, lscale, rscale, m, v,ldv, info )
     !! CGGBAK forms the right or left eigenvectors of a complex generalized
     !! eigenvalue problem A*x = lambda*B*x, by backward transformation on
     !! the computed eigenvectors of the balanced pair of matrices output by
     !! CGGBAL.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: job, side
           integer(ilp), intent(in) :: ihi, ilo, ldv, m, n
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           real(sp), intent(in) :: lscale(*), rscale(*)
           complex(sp), intent(inout) :: v(ldv,*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: leftv, rightv
           integer(ilp) :: i, k
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           rightv = stdlib_lsame( side, 'R' )
           leftv = stdlib_lsame( side, 'L' )
           info = 0_ilp
           if( .not.stdlib_lsame( job, 'N' ) .and. .not.stdlib_lsame( job, 'P' ) &
                     .and..not.stdlib_lsame( job, 'S' ) .and. .not.stdlib_lsame( job, 'B' ) ) then
              info = -1_ilp
           else if( .not.rightv .and. .not.leftv ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( ilo<1_ilp ) then
              info = -4_ilp
           else if( n==0_ilp .and. ihi==0_ilp .and. ilo/=1_ilp ) then
              info = -4_ilp
           else if( n>0_ilp .and. ( ihi<ilo .or. ihi>max( 1_ilp, n ) ) )then
              info = -5_ilp
           else if( n==0_ilp .and. ilo==1_ilp .and. ihi/=0_ilp ) then
              info = -5_ilp
           else if( m<0_ilp ) then
              info = -8_ilp
           else if( ldv<max( 1_ilp, n ) ) then
              info = -10_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CGGBAK', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( m==0 )return
           if( stdlib_lsame( job, 'N' ) )return
           if( ilo==ihi )go to 30
           ! backward balance
           if( stdlib_lsame( job, 'S' ) .or. stdlib_lsame( job, 'B' ) ) then
              ! backward transformation on right eigenvectors
              if( rightv ) then
                 do i = ilo, ihi
                    call stdlib_csscal( m, rscale( i ), v( i, 1_ilp ), ldv )
                 end do
              end if
              ! backward transformation on left eigenvectors
              if( leftv ) then
                 do i = ilo, ihi
                    call stdlib_csscal( m, lscale( i ), v( i, 1_ilp ), ldv )
                 end do
              end if
           end if
           ! backward permutation
           30 continue
           if( stdlib_lsame( job, 'P' ) .or. stdlib_lsame( job, 'B' ) ) then
              ! backward permutation on right eigenvectors
              if( rightv ) then
                 if( ilo==1 )go to 50
                 loop_40: do i = ilo - 1, 1, -1
                    k = rscale( i )
                    if( k==i )cycle loop_40
                    call stdlib_cswap( m, v( i, 1_ilp ), ldv, v( k, 1_ilp ), ldv )
                 end do loop_40
                 50 continue
                 if( ihi==n )go to 70
                 loop_60: do i = ihi + 1, n
                    k = rscale( i )
                    if( k==i )cycle loop_60
                    call stdlib_cswap( m, v( i, 1_ilp ), ldv, v( k, 1_ilp ), ldv )
                 end do loop_60
              end if
              ! backward permutation on left eigenvectors
              70 continue
              if( leftv ) then
                 if( ilo==1 )go to 90
                 loop_80: do i = ilo - 1, 1, -1
                    k = lscale( i )
                    if( k==i )cycle loop_80
                    call stdlib_cswap( m, v( i, 1_ilp ), ldv, v( k, 1_ilp ), ldv )
                 end do loop_80
                 90 continue
                 if( ihi==n )go to 110
                 loop_100: do i = ihi + 1, n
                    k = lscale( i )
                    if( k==i )cycle loop_100
                    call stdlib_cswap( m, v( i, 1_ilp ), ldv, v( k, 1_ilp ), ldv )
                 end do loop_100
              end if
           end if
           110 continue
           return
     end subroutine stdlib_cggbak

     pure module subroutine stdlib_zggbak( job, side, n, ilo, ihi, lscale, rscale, m, v,ldv, info )
     !! ZGGBAK forms the right or left eigenvectors of a complex generalized
     !! eigenvalue problem A*x = lambda*B*x, by backward transformation on
     !! the computed eigenvectors of the balanced pair of matrices output by
     !! ZGGBAL.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: job, side
           integer(ilp), intent(in) :: ihi, ilo, ldv, m, n
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           real(dp), intent(in) :: lscale(*), rscale(*)
           complex(dp), intent(inout) :: v(ldv,*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: leftv, rightv
           integer(ilp) :: i, k
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           rightv = stdlib_lsame( side, 'R' )
           leftv = stdlib_lsame( side, 'L' )
           info = 0_ilp
           if( .not.stdlib_lsame( job, 'N' ) .and. .not.stdlib_lsame( job, 'P' ) &
                     .and..not.stdlib_lsame( job, 'S' ) .and. .not.stdlib_lsame( job, 'B' ) ) then
              info = -1_ilp
           else if( .not.rightv .and. .not.leftv ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( ilo<1_ilp ) then
              info = -4_ilp
           else if( n==0_ilp .and. ihi==0_ilp .and. ilo/=1_ilp ) then
              info = -4_ilp
           else if( n>0_ilp .and. ( ihi<ilo .or. ihi>max( 1_ilp, n ) ) )then
              info = -5_ilp
           else if( n==0_ilp .and. ilo==1_ilp .and. ihi/=0_ilp ) then
              info = -5_ilp
           else if( m<0_ilp ) then
              info = -8_ilp
           else if( ldv<max( 1_ilp, n ) ) then
              info = -10_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZGGBAK', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           if( m==0 )return
           if( stdlib_lsame( job, 'N' ) )return
           if( ilo==ihi )go to 30
           ! backward balance
           if( stdlib_lsame( job, 'S' ) .or. stdlib_lsame( job, 'B' ) ) then
              ! backward transformation on right eigenvectors
              if( rightv ) then
                 do i = ilo, ihi
                    call stdlib_zdscal( m, rscale( i ), v( i, 1_ilp ), ldv )
                 end do
              end if
              ! backward transformation on left eigenvectors
              if( leftv ) then
                 do i = ilo, ihi
                    call stdlib_zdscal( m, lscale( i ), v( i, 1_ilp ), ldv )
                 end do
              end if
           end if
           ! backward permutation
           30 continue
           if( stdlib_lsame( job, 'P' ) .or. stdlib_lsame( job, 'B' ) ) then
              ! backward permutation on right eigenvectors
              if( rightv ) then
                 if( ilo==1 )go to 50
                 loop_40: do i = ilo - 1, 1, -1
                    k = int(rscale( i ),KIND=ilp)
                    if( k==i )cycle loop_40
                    call stdlib_zswap( m, v( i, 1_ilp ), ldv, v( k, 1_ilp ), ldv )
                 end do loop_40
                 50 continue
                 if( ihi==n )go to 70
                 loop_60: do i = ihi + 1, n
                    k = int(rscale( i ),KIND=ilp)
                    if( k==i )cycle loop_60
                    call stdlib_zswap( m, v( i, 1_ilp ), ldv, v( k, 1_ilp ), ldv )
                 end do loop_60
              end if
              ! backward permutation on left eigenvectors
              70 continue
              if( leftv ) then
                 if( ilo==1 )go to 90
                 loop_80: do i = ilo - 1, 1, -1
                    k = int(lscale( i ),KIND=ilp)
                    if( k==i )cycle loop_80
                    call stdlib_zswap( m, v( i, 1_ilp ), ldv, v( k, 1_ilp ), ldv )
                 end do loop_80
                 90 continue
                 if( ihi==n )go to 110
                 loop_100: do i = ihi + 1, n
                    k = int(lscale( i ),KIND=ilp)
                    if( k==i )cycle loop_100
                    call stdlib_zswap( m, v( i, 1_ilp ), ldv, v( k, 1_ilp ), ldv )
                 end do loop_100
              end if
           end if
           110 continue
           return
     end subroutine stdlib_zggbak



end submodule stdlib_lapack_eigv_comp
