submodule(stdlib_lapack_eig_svd_lsq) stdlib_lapack_lsq_constrained
  implicit none


  contains

     pure module subroutine stdlib_sgglse( m, n, p, a, lda, b, ldb, c, d, x, work, lwork,info )
     !! SGGLSE solves the linear equality-constrained least squares (LSE)
     !! problem:
     !! minimize || c - A*x ||_2   subject to   B*x = d
     !! where A is an M-by-N matrix, B is a P-by-N matrix, c is a given
     !! M-vector, and d is a given P-vector. It is assumed that
     !! P <= N <= M+P, and
     !! rank(B) = P and  rank( (A) ) = N.
     !! ( (B) )
     !! These conditions ensure that the LSE problem has a unique solution,
     !! which is obtained using a generalized RQ factorization of the
     !! matrices (B, A) given by
     !! B = (0 R)*Q,   A = Z*T*Q.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, lwork, m, n, p
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*), b(ldb,*), c(*), d(*)
           real(sp), intent(out) :: work(*), x(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: lopt, lwkmin, lwkopt, mn, nb, nb1, nb2, nb3, nb4, nr
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp
           mn = min( m, n )
           lquery = ( lwork==-1_ilp )
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( p<0_ilp .or. p>n .or. p<n-m ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, p ) ) then
              info = -7_ilp
           end if
           ! calculate workspace
           if( info==0_ilp) then
              if( n==0_ilp ) then
                 lwkmin = 1_ilp
                 lwkopt = 1_ilp
              else
                 nb1 = stdlib_ilaenv( 1_ilp, 'SGEQRF', ' ', m, n, -1_ilp, -1_ilp )
                 nb2 = stdlib_ilaenv( 1_ilp, 'SGERQF', ' ', m, n, -1_ilp, -1_ilp )
                 nb3 = stdlib_ilaenv( 1_ilp, 'SORMQR', ' ', m, n, p, -1_ilp )
                 nb4 = stdlib_ilaenv( 1_ilp, 'SORMRQ', ' ', m, n, p, -1_ilp )
                 nb = max( nb1, nb2, nb3, nb4 )
                 lwkmin = m + n + p
                 lwkopt = p + mn + max( m, n )*nb
              end if
              work( 1_ilp ) = lwkopt
              if( lwork<lwkmin .and. .not.lquery ) then
                 info = -12_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SGGLSE', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! compute the grq factorization of matrices b and a:
                  ! b*q**t = (  0  t12 ) p   z**t*a*q**t = ( r11 r12 ) n-p
                              ! n-p  p                     (  0  r22 ) m+p-n
                                                            ! n-p  p
           ! where t12 and r11 are upper triangular, and q and z are
           ! orthogonal.
           call stdlib_sggrqf( p, m, n, b, ldb, work, a, lda, work( p+1 ),work( p+mn+1 ), lwork-p-&
                     mn, info )
           lopt = work( p+mn+1 )
           ! update c = z**t *c = ( c1 ) n-p
                                ! ( c2 ) m+p-n
           call stdlib_sormqr( 'LEFT', 'TRANSPOSE', m, 1_ilp, mn, a, lda, work( p+1 ),c, max( 1_ilp, m ), &
                     work( p+mn+1 ), lwork-p-mn, info )
           lopt = max( lopt, int( work( p+mn+1 ),KIND=ilp) )
           ! solve t12*x2 = d for x2
           if( p>0_ilp ) then
              call stdlib_strtrs( 'UPPER', 'NO TRANSPOSE', 'NON-UNIT', p, 1_ilp,b( 1_ilp, n-p+1 ), ldb, d,&
                         p, info )
              if( info>0_ilp ) then
                 info = 1_ilp
                 return
              end if
              ! put the solution in x
              call stdlib_scopy( p, d, 1_ilp, x( n-p+1 ), 1_ilp )
              ! update c1
              call stdlib_sgemv( 'NO TRANSPOSE', n-p, p, -one, a( 1_ilp, n-p+1 ), lda,d, 1_ilp, one, c, 1_ilp &
                        )
           end if
           ! solve r11*x1 = c1 for x1
           if( n>p ) then
              call stdlib_strtrs( 'UPPER', 'NO TRANSPOSE', 'NON-UNIT', n-p, 1_ilp,a, lda, c, n-p, &
                        info )
              if( info>0_ilp ) then
                 info = 2_ilp
                 return
              end if
              ! put the solutions in x
              call stdlib_scopy( n-p, c, 1_ilp, x, 1_ilp )
           end if
           ! compute the residual vector:
           if( m<n ) then
              nr = m + p - n
              if( nr>0_ilp )call stdlib_sgemv( 'NO TRANSPOSE', nr, n-m, -one, a( n-p+1, m+1 ),lda, d( &
                        nr+1 ), 1_ilp, one, c( n-p+1 ), 1_ilp )
           else
              nr = p
           end if
           if( nr>0_ilp ) then
              call stdlib_strmv( 'UPPER', 'NO TRANSPOSE', 'NON UNIT', nr,a( n-p+1, n-p+1 ), lda, &
                        d, 1_ilp )
              call stdlib_saxpy( nr, -one, d, 1_ilp, c( n-p+1 ), 1_ilp )
           end if
           ! backward transformation x = q**t*x
           call stdlib_sormrq( 'LEFT', 'TRANSPOSE', n, 1_ilp, p, b, ldb, work( 1_ilp ), x,n, work( p+mn+1 &
                     ), lwork-p-mn, info )
           work( 1_ilp ) = p + mn + max( lopt, int( work( p+mn+1 ),KIND=ilp) )
           return
     end subroutine stdlib_sgglse

     pure module subroutine stdlib_dgglse( m, n, p, a, lda, b, ldb, c, d, x, work, lwork,info )
     !! DGGLSE solves the linear equality-constrained least squares (LSE)
     !! problem:
     !! minimize || c - A*x ||_2   subject to   B*x = d
     !! where A is an M-by-N matrix, B is a P-by-N matrix, c is a given
     !! M-vector, and d is a given P-vector. It is assumed that
     !! P <= N <= M+P, and
     !! rank(B) = P and  rank( (A) ) = N.
     !! ( (B) )
     !! These conditions ensure that the LSE problem has a unique solution,
     !! which is obtained using a generalized RQ factorization of the
     !! matrices (B, A) given by
     !! B = (0 R)*Q,   A = Z*T*Q.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, lwork, m, n, p
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*), b(ldb,*), c(*), d(*)
           real(dp), intent(out) :: work(*), x(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: lopt, lwkmin, lwkopt, mn, nb, nb1, nb2, nb3, nb4, nr
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp
           mn = min( m, n )
           lquery = ( lwork==-1_ilp )
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( p<0_ilp .or. p>n .or. p<n-m ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, p ) ) then
              info = -7_ilp
           end if
           ! calculate workspace
           if( info==0_ilp) then
              if( n==0_ilp ) then
                 lwkmin = 1_ilp
                 lwkopt = 1_ilp
              else
                 nb1 = stdlib_ilaenv( 1_ilp, 'DGEQRF', ' ', m, n, -1_ilp, -1_ilp )
                 nb2 = stdlib_ilaenv( 1_ilp, 'DGERQF', ' ', m, n, -1_ilp, -1_ilp )
                 nb3 = stdlib_ilaenv( 1_ilp, 'DORMQR', ' ', m, n, p, -1_ilp )
                 nb4 = stdlib_ilaenv( 1_ilp, 'DORMRQ', ' ', m, n, p, -1_ilp )
                 nb = max( nb1, nb2, nb3, nb4 )
                 lwkmin = m + n + p
                 lwkopt = p + mn + max( m, n )*nb
              end if
              work( 1_ilp ) = lwkopt
              if( lwork<lwkmin .and. .not.lquery ) then
                 info = -12_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DGGLSE', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! compute the grq factorization of matrices b and a:
                  ! b*q**t = (  0  t12 ) p   z**t*a*q**t = ( r11 r12 ) n-p
                              ! n-p  p                     (  0  r22 ) m+p-n
                                                            ! n-p  p
           ! where t12 and r11 are upper triangular, and q and z are
           ! orthogonal.
           call stdlib_dggrqf( p, m, n, b, ldb, work, a, lda, work( p+1 ),work( p+mn+1 ), lwork-p-&
                     mn, info )
           lopt = work( p+mn+1 )
           ! update c = z**t *c = ( c1 ) n-p
                                ! ( c2 ) m+p-n
           call stdlib_dormqr( 'LEFT', 'TRANSPOSE', m, 1_ilp, mn, a, lda, work( p+1 ),c, max( 1_ilp, m ), &
                     work( p+mn+1 ), lwork-p-mn, info )
           lopt = max( lopt, int( work( p+mn+1 ),KIND=ilp) )
           ! solve t12*x2 = d for x2
           if( p>0_ilp ) then
              call stdlib_dtrtrs( 'UPPER', 'NO TRANSPOSE', 'NON-UNIT', p, 1_ilp,b( 1_ilp, n-p+1 ), ldb, d,&
                         p, info )
              if( info>0_ilp ) then
                 info = 1_ilp
                 return
              end if
              ! put the solution in x
              call stdlib_dcopy( p, d, 1_ilp, x( n-p+1 ), 1_ilp )
              ! update c1
              call stdlib_dgemv( 'NO TRANSPOSE', n-p, p, -one, a( 1_ilp, n-p+1 ), lda,d, 1_ilp, one, c, 1_ilp &
                        )
           end if
           ! solve r11*x1 = c1 for x1
           if( n>p ) then
              call stdlib_dtrtrs( 'UPPER', 'NO TRANSPOSE', 'NON-UNIT', n-p, 1_ilp,a, lda, c, n-p, &
                        info )
              if( info>0_ilp ) then
                 info = 2_ilp
                 return
              end if
              ! put the solutions in x
              call stdlib_dcopy( n-p, c, 1_ilp, x, 1_ilp )
           end if
           ! compute the residual vector:
           if( m<n ) then
              nr = m + p - n
              if( nr>0_ilp )call stdlib_dgemv( 'NO TRANSPOSE', nr, n-m, -one, a( n-p+1, m+1 ),lda, d( &
                        nr+1 ), 1_ilp, one, c( n-p+1 ), 1_ilp )
           else
              nr = p
           end if
           if( nr>0_ilp ) then
              call stdlib_dtrmv( 'UPPER', 'NO TRANSPOSE', 'NON UNIT', nr,a( n-p+1, n-p+1 ), lda, &
                        d, 1_ilp )
              call stdlib_daxpy( nr, -one, d, 1_ilp, c( n-p+1 ), 1_ilp )
           end if
           ! backward transformation x = q**t*x
           call stdlib_dormrq( 'LEFT', 'TRANSPOSE', n, 1_ilp, p, b, ldb, work( 1_ilp ), x,n, work( p+mn+1 &
                     ), lwork-p-mn, info )
           work( 1_ilp ) = p + mn + max( lopt, int( work( p+mn+1 ),KIND=ilp) )
           return
     end subroutine stdlib_dgglse


     pure module subroutine stdlib_cgglse( m, n, p, a, lda, b, ldb, c, d, x, work, lwork,info )
     !! CGGLSE solves the linear equality-constrained least squares (LSE)
     !! problem:
     !! minimize || c - A*x ||_2   subject to   B*x = d
     !! where A is an M-by-N matrix, B is a P-by-N matrix, c is a given
     !! M-vector, and d is a given P-vector. It is assumed that
     !! P <= N <= M+P, and
     !! rank(B) = P and  rank( (A) ) = N.
     !! ( (B) )
     !! These conditions ensure that the LSE problem has a unique solution,
     !! which is obtained using a generalized RQ factorization of the
     !! matrices (B, A) given by
     !! B = (0 R)*Q,   A = Z*T*Q.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, lwork, m, n, p
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*), c(*), d(*)
           complex(sp), intent(out) :: work(*), x(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: lopt, lwkmin, lwkopt, mn, nb, nb1, nb2, nb3, nb4, nr
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp
           mn = min( m, n )
           lquery = ( lwork==-1_ilp )
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( p<0_ilp .or. p>n .or. p<n-m ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, p ) ) then
              info = -7_ilp
           end if
           ! calculate workspace
           if( info==0_ilp) then
              if( n==0_ilp ) then
                 lwkmin = 1_ilp
                 lwkopt = 1_ilp
              else
                 nb1 = stdlib_ilaenv( 1_ilp, 'CGEQRF', ' ', m, n, -1_ilp, -1_ilp )
                 nb2 = stdlib_ilaenv( 1_ilp, 'CGERQF', ' ', m, n, -1_ilp, -1_ilp )
                 nb3 = stdlib_ilaenv( 1_ilp, 'CUNMQR', ' ', m, n, p, -1_ilp )
                 nb4 = stdlib_ilaenv( 1_ilp, 'CUNMRQ', ' ', m, n, p, -1_ilp )
                 nb = max( nb1, nb2, nb3, nb4 )
                 lwkmin = m + n + p
                 lwkopt = p + mn + max( m, n )*nb
              end if
              work( 1_ilp ) = lwkopt
              if( lwork<lwkmin .and. .not.lquery ) then
                 info = -12_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CGGLSE', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! compute the grq factorization of matrices b and a:
                  ! b*q**h = (  0  t12 ) p   z**h*a*q**h = ( r11 r12 ) n-p
                              ! n-p  p                     (  0  r22 ) m+p-n
                                                            ! n-p  p
           ! where t12 and r11 are upper triangular, and q and z are
           ! unitary.
           call stdlib_cggrqf( p, m, n, b, ldb, work, a, lda, work( p+1 ),work( p+mn+1 ), lwork-p-&
                     mn, info )
           lopt = real( work( p+mn+1 ),KIND=sp)
           ! update c = z**h *c = ( c1 ) n-p
                             ! ( c2 ) m+p-n
           call stdlib_cunmqr( 'LEFT', 'CONJUGATE TRANSPOSE', m, 1_ilp, mn, a, lda,work( p+1 ), c, &
                     max( 1_ilp, m ), work( p+mn+1 ),lwork-p-mn, info )
           lopt = max( lopt, int( work( p+mn+1 ),KIND=ilp) )
           ! solve t12*x2 = d for x2
           if( p>0_ilp ) then
              call stdlib_ctrtrs( 'UPPER', 'NO TRANSPOSE', 'NON-UNIT', p, 1_ilp,b( 1_ilp, n-p+1 ), ldb, d,&
                         p, info )
              if( info>0_ilp ) then
                 info = 1_ilp
                 return
              end if
              ! put the solution in x
           call stdlib_ccopy( p, d, 1_ilp, x( n-p+1 ), 1_ilp )
              ! update c1
              call stdlib_cgemv( 'NO TRANSPOSE', n-p, p, -cone, a( 1_ilp, n-p+1 ), lda,d, 1_ilp, cone, c, &
                        1_ilp )
           end if
           ! solve r11*x1 = c1 for x1
           if( n>p ) then
              call stdlib_ctrtrs( 'UPPER', 'NO TRANSPOSE', 'NON-UNIT', n-p, 1_ilp,a, lda, c, n-p, &
                        info )
              if( info>0_ilp ) then
                 info = 2_ilp
                 return
              end if
              ! put the solutions in x
              call stdlib_ccopy( n-p, c, 1_ilp, x, 1_ilp )
           end if
           ! compute the residual vector:
           if( m<n ) then
              nr = m + p - n
              if( nr>0_ilp )call stdlib_cgemv( 'NO TRANSPOSE', nr, n-m, -cone, a( n-p+1, m+1 ),lda, d(&
                         nr+1 ), 1_ilp, cone, c( n-p+1 ), 1_ilp )
           else
              nr = p
           end if
           if( nr>0_ilp ) then
              call stdlib_ctrmv( 'UPPER', 'NO TRANSPOSE', 'NON UNIT', nr,a( n-p+1, n-p+1 ), lda, &
                        d, 1_ilp )
              call stdlib_caxpy( nr, -cone, d, 1_ilp, c( n-p+1 ), 1_ilp )
           end if
           ! backward transformation x = q**h*x
           call stdlib_cunmrq( 'LEFT', 'CONJUGATE TRANSPOSE', n, 1_ilp, p, b, ldb,work( 1_ilp ), x, n, &
                     work( p+mn+1 ), lwork-p-mn, info )
           work( 1_ilp ) = p + mn + max( lopt, int( work( p+mn+1 ),KIND=ilp) )
           return
     end subroutine stdlib_cgglse

     pure module subroutine stdlib_zgglse( m, n, p, a, lda, b, ldb, c, d, x, work, lwork,info )
     !! ZGGLSE solves the linear equality-constrained least squares (LSE)
     !! problem:
     !! minimize || c - A*x ||_2   subject to   B*x = d
     !! where A is an M-by-N matrix, B is a P-by-N matrix, c is a given
     !! M-vector, and d is a given P-vector. It is assumed that
     !! P <= N <= M+P, and
     !! rank(B) = P and  rank( (A) ) = N.
     !! ( (B) )
     !! These conditions ensure that the LSE problem has a unique solution,
     !! which is obtained using a generalized RQ factorization of the
     !! matrices (B, A) given by
     !! B = (0 R)*Q,   A = Z*T*Q.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, lwork, m, n, p
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*), c(*), d(*)
           complex(dp), intent(out) :: work(*), x(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: lopt, lwkmin, lwkopt, mn, nb, nb1, nb2, nb3, nb4, nr
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp
           mn = min( m, n )
           lquery = ( lwork==-1_ilp )
           if( m<0_ilp ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( p<0_ilp .or. p>n .or. p<n-m ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, m ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, p ) ) then
              info = -7_ilp
           end if
           ! calculate workspace
           if( info==0_ilp) then
              if( n==0_ilp ) then
                 lwkmin = 1_ilp
                 lwkopt = 1_ilp
              else
                 nb1 = stdlib_ilaenv( 1_ilp, 'ZGEQRF', ' ', m, n, -1_ilp, -1_ilp )
                 nb2 = stdlib_ilaenv( 1_ilp, 'ZGERQF', ' ', m, n, -1_ilp, -1_ilp )
                 nb3 = stdlib_ilaenv( 1_ilp, 'ZUNMQR', ' ', m, n, p, -1_ilp )
                 nb4 = stdlib_ilaenv( 1_ilp, 'ZUNMRQ', ' ', m, n, p, -1_ilp )
                 nb = max( nb1, nb2, nb3, nb4 )
                 lwkmin = m + n + p
                 lwkopt = p + mn + max( m, n )*nb
              end if
              work( 1_ilp ) = lwkopt
              if( lwork<lwkmin .and. .not.lquery ) then
                 info = -12_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZGGLSE', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! compute the grq factorization of matrices b and a:
                  ! b*q**h = (  0  t12 ) p   z**h*a*q**h = ( r11 r12 ) n-p
                              ! n-p  p                     (  0  r22 ) m+p-n
                                                            ! n-p  p
           ! where t12 and r11 are upper triangular, and q and z are
           ! unitary.
           call stdlib_zggrqf( p, m, n, b, ldb, work, a, lda, work( p+1 ),work( p+mn+1 ), lwork-p-&
                     mn, info )
           lopt = real( work( p+mn+1 ),KIND=dp)
           ! update c = z**h *c = ( c1 ) n-p
                             ! ( c2 ) m+p-n
           call stdlib_zunmqr( 'LEFT', 'CONJUGATE TRANSPOSE', m, 1_ilp, mn, a, lda,work( p+1 ), c, &
                     max( 1_ilp, m ), work( p+mn+1 ),lwork-p-mn, info )
           lopt = max( lopt, int( work( p+mn+1 ),KIND=ilp) )
           ! solve t12*x2 = d for x2
           if( p>0_ilp ) then
              call stdlib_ztrtrs( 'UPPER', 'NO TRANSPOSE', 'NON-UNIT', p, 1_ilp,b( 1_ilp, n-p+1 ), ldb, d,&
                         p, info )
              if( info>0_ilp ) then
                 info = 1_ilp
                 return
              end if
              ! put the solution in x
              call stdlib_zcopy( p, d, 1_ilp, x( n-p+1 ), 1_ilp )
              ! update c1
              call stdlib_zgemv( 'NO TRANSPOSE', n-p, p, -cone, a( 1_ilp, n-p+1 ), lda,d, 1_ilp, cone, c, &
                        1_ilp )
           end if
           ! solve r11*x1 = c1 for x1
           if( n>p ) then
              call stdlib_ztrtrs( 'UPPER', 'NO TRANSPOSE', 'NON-UNIT', n-p, 1_ilp,a, lda, c, n-p, &
                        info )
              if( info>0_ilp ) then
                 info = 2_ilp
                 return
              end if
              ! put the solutions in x
              call stdlib_zcopy( n-p, c, 1_ilp, x, 1_ilp )
           end if
           ! compute the residual vector:
           if( m<n ) then
              nr = m + p - n
              if( nr>0_ilp )call stdlib_zgemv( 'NO TRANSPOSE', nr, n-m, -cone, a( n-p+1, m+1 ),lda, d(&
                         nr+1 ), 1_ilp, cone, c( n-p+1 ), 1_ilp )
           else
              nr = p
           end if
           if( nr>0_ilp ) then
              call stdlib_ztrmv( 'UPPER', 'NO TRANSPOSE', 'NON UNIT', nr,a( n-p+1, n-p+1 ), lda, &
                        d, 1_ilp )
              call stdlib_zaxpy( nr, -cone, d, 1_ilp, c( n-p+1 ), 1_ilp )
           end if
           ! backward transformation x = q**h*x
           call stdlib_zunmrq( 'LEFT', 'CONJUGATE TRANSPOSE', n, 1_ilp, p, b, ldb,work( 1_ilp ), x, n, &
                     work( p+mn+1 ), lwork-p-mn, info )
           work( 1_ilp ) = p + mn + max( lopt, int( work( p+mn+1 ),KIND=ilp) )
           return
     end subroutine stdlib_zgglse




     pure module subroutine stdlib_sggglm( n, m, p, a, lda, b, ldb, d, x, y, work, lwork,info )
     !! SGGGLM solves a general Gauss-Markov linear model (GLM) problem:
     !! minimize || y ||_2   subject to   d = A*x + B*y
     !! x
     !! where A is an N-by-M matrix, B is an N-by-P matrix, and d is a
     !! given N-vector. It is assumed that M <= N <= M+P, and
     !! rank(A) = M    and    rank( A B ) = N.
     !! Under these assumptions, the constrained equation is always
     !! consistent, and there is a unique solution x and a minimal 2-norm
     !! solution y, which is obtained using a generalized QR factorization
     !! of the matrices (A, B) given by
     !! A = Q*(R),   B = Q*T*Z.
     !! (0)
     !! In particular, if matrix B is square nonsingular, then the problem
     !! GLM is equivalent to the following weighted linear least squares
     !! problem
     !! minimize || inv(B)*(d-A*x) ||_2
     !! x
     !! where inv(B) denotes the inverse of B.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, lwork, m, n, p
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*), b(ldb,*), d(*)
           real(sp), intent(out) :: work(*), x(*), y(*)
        ! ===================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: i, lopt, lwkmin, lwkopt, nb, nb1, nb2, nb3, nb4, np
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp
           np = min( n, p )
           lquery = ( lwork==-1_ilp )
           if( n<0_ilp ) then
              info = -1_ilp
           else if( m<0_ilp .or. m>n ) then
              info = -2_ilp
           else if( p<0_ilp .or. p<n-m ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -7_ilp
           end if
           ! calculate workspace
           if( info==0_ilp) then
              if( n==0_ilp ) then
                 lwkmin = 1_ilp
                 lwkopt = 1_ilp
              else
                 nb1 = stdlib_ilaenv( 1_ilp, 'SGEQRF', ' ', n, m, -1_ilp, -1_ilp )
                 nb2 = stdlib_ilaenv( 1_ilp, 'SGERQF', ' ', n, m, -1_ilp, -1_ilp )
                 nb3 = stdlib_ilaenv( 1_ilp, 'SORMQR', ' ', n, m, p, -1_ilp )
                 nb4 = stdlib_ilaenv( 1_ilp, 'SORMRQ', ' ', n, m, p, -1_ilp )
                 nb = max( nb1, nb2, nb3, nb4 )
                 lwkmin = m + n + p
                 lwkopt = m + np + max( n, p )*nb
              end if
              work( 1_ilp ) = lwkopt
              if( lwork<lwkmin .and. .not.lquery ) then
                 info = -12_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SGGGLM', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0_ilp ) then
              do i = 1, m
                 x(i) = zero
              end do
              do i = 1, p
                 y(i) = zero
              end do
              return
           end if
           ! compute the gqr factorization of matrices a and b:
                ! q**t*a = ( r11 ) m,    q**t*b*z**t = ( t11   t12 ) m
                         ! (  0  ) n-m                 (  0    t22 ) n-m
                            ! m                         m+p-n  n-m
           ! where r11 and t22 are upper triangular, and q and z are
           ! orthogonal.
           call stdlib_sggqrf( n, m, p, a, lda, work, b, ldb, work( m+1 ),work( m+np+1 ), lwork-m-&
                     np, info )
           lopt = work( m+np+1 )
           ! update left-hand-side vector d = q**t*d = ( d1 ) m
                                                     ! ( d2 ) n-m
           call stdlib_sormqr( 'LEFT', 'TRANSPOSE', n, 1_ilp, m, a, lda, work, d,max( 1_ilp, n ), work( m+&
                     np+1 ), lwork-m-np, info )
           lopt = max( lopt, int( work( m+np+1 ),KIND=ilp) )
           ! solve t22*y2 = d2 for y2
           if( n>m ) then
              call stdlib_strtrs( 'UPPER', 'NO TRANSPOSE', 'NON UNIT', n-m, 1_ilp,b( m+1, m+p-n+1 ), &
                        ldb, d( m+1 ), n-m, info )
              if( info>0_ilp ) then
                 info = 1_ilp
                 return
              end if
              call stdlib_scopy( n-m, d( m+1 ), 1_ilp, y( m+p-n+1 ), 1_ilp )
           end if
           ! set y1 = 0
           do i = 1, m + p - n
              y( i ) = zero
           end do
           ! update d1 = d1 - t12*y2
           call stdlib_sgemv( 'NO TRANSPOSE', m, n-m, -one, b( 1_ilp, m+p-n+1 ), ldb,y( m+p-n+1 ), 1_ilp, &
                     one, d, 1_ilp )
           ! solve triangular system: r11*x = d1
           if( m>0_ilp ) then
              call stdlib_strtrs( 'UPPER', 'NO TRANSPOSE', 'NON UNIT', m, 1_ilp, a, lda,d, m, info )
                        
              if( info>0_ilp ) then
                 info = 2_ilp
                 return
              end if
              ! copy d to x
              call stdlib_scopy( m, d, 1_ilp, x, 1_ilp )
           end if
           ! backward transformation y = z**t *y
           call stdlib_sormrq( 'LEFT', 'TRANSPOSE', p, 1_ilp, np,b( max( 1_ilp, n-p+1 ), 1_ilp ), ldb, work( &
                     m+1 ), y,max( 1_ilp, p ), work( m+np+1 ), lwork-m-np, info )
           work( 1_ilp ) = m + np + max( lopt, int( work( m+np+1 ),KIND=ilp) )
           return
     end subroutine stdlib_sggglm

     pure module subroutine stdlib_dggglm( n, m, p, a, lda, b, ldb, d, x, y, work, lwork,info )
     !! DGGGLM solves a general Gauss-Markov linear model (GLM) problem:
     !! minimize || y ||_2   subject to   d = A*x + B*y
     !! x
     !! where A is an N-by-M matrix, B is an N-by-P matrix, and d is a
     !! given N-vector. It is assumed that M <= N <= M+P, and
     !! rank(A) = M    and    rank( A B ) = N.
     !! Under these assumptions, the constrained equation is always
     !! consistent, and there is a unique solution x and a minimal 2-norm
     !! solution y, which is obtained using a generalized QR factorization
     !! of the matrices (A, B) given by
     !! A = Q*(R),   B = Q*T*Z.
     !! (0)
     !! In particular, if matrix B is square nonsingular, then the problem
     !! GLM is equivalent to the following weighted linear least squares
     !! problem
     !! minimize || inv(B)*(d-A*x) ||_2
     !! x
     !! where inv(B) denotes the inverse of B.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, lwork, m, n, p
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*), b(ldb,*), d(*)
           real(dp), intent(out) :: work(*), x(*), y(*)
        ! ===================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: i, lopt, lwkmin, lwkopt, nb, nb1, nb2, nb3, nb4, np
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp
           np = min( n, p )
           lquery = ( lwork==-1_ilp )
           if( n<0_ilp ) then
              info = -1_ilp
           else if( m<0_ilp .or. m>n ) then
              info = -2_ilp
           else if( p<0_ilp .or. p<n-m ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -7_ilp
           end if
           ! calculate workspace
           if( info==0_ilp) then
              if( n==0_ilp ) then
                 lwkmin = 1_ilp
                 lwkopt = 1_ilp
              else
                 nb1 = stdlib_ilaenv( 1_ilp, 'DGEQRF', ' ', n, m, -1_ilp, -1_ilp )
                 nb2 = stdlib_ilaenv( 1_ilp, 'DGERQF', ' ', n, m, -1_ilp, -1_ilp )
                 nb3 = stdlib_ilaenv( 1_ilp, 'DORMQR', ' ', n, m, p, -1_ilp )
                 nb4 = stdlib_ilaenv( 1_ilp, 'DORMRQ', ' ', n, m, p, -1_ilp )
                 nb = max( nb1, nb2, nb3, nb4 )
                 lwkmin = m + n + p
                 lwkopt = m + np + max( n, p )*nb
              end if
              work( 1_ilp ) = lwkopt
              if( lwork<lwkmin .and. .not.lquery ) then
                 info = -12_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DGGGLM', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0_ilp ) then
              do i = 1, m
                 x(i) = zero
              end do
              do i = 1, p
                 y(i) = zero
              end do
              return
           end if
           ! compute the gqr factorization of matrices a and b:
                ! q**t*a = ( r11 ) m,    q**t*b*z**t = ( t11   t12 ) m
                         ! (  0  ) n-m                 (  0    t22 ) n-m
                            ! m                         m+p-n  n-m
           ! where r11 and t22 are upper triangular, and q and z are
           ! orthogonal.
           call stdlib_dggqrf( n, m, p, a, lda, work, b, ldb, work( m+1 ),work( m+np+1 ), lwork-m-&
                     np, info )
           lopt = work( m+np+1 )
           ! update left-hand-side vector d = q**t*d = ( d1 ) m
                                                     ! ( d2 ) n-m
           call stdlib_dormqr( 'LEFT', 'TRANSPOSE', n, 1_ilp, m, a, lda, work, d,max( 1_ilp, n ), work( m+&
                     np+1 ), lwork-m-np, info )
           lopt = max( lopt, int( work( m+np+1 ),KIND=ilp) )
           ! solve t22*y2 = d2 for y2
           if( n>m ) then
              call stdlib_dtrtrs( 'UPPER', 'NO TRANSPOSE', 'NON UNIT', n-m, 1_ilp,b( m+1, m+p-n+1 ), &
                        ldb, d( m+1 ), n-m, info )
              if( info>0_ilp ) then
                 info = 1_ilp
                 return
              end if
              call stdlib_dcopy( n-m, d( m+1 ), 1_ilp, y( m+p-n+1 ), 1_ilp )
           end if
           ! set y1 = 0
           do i = 1, m + p - n
              y( i ) = zero
           end do
           ! update d1 = d1 - t12*y2
           call stdlib_dgemv( 'NO TRANSPOSE', m, n-m, -one, b( 1_ilp, m+p-n+1 ), ldb,y( m+p-n+1 ), 1_ilp, &
                     one, d, 1_ilp )
           ! solve triangular system: r11*x = d1
           if( m>0_ilp ) then
              call stdlib_dtrtrs( 'UPPER', 'NO TRANSPOSE', 'NON UNIT', m, 1_ilp, a, lda,d, m, info )
                        
              if( info>0_ilp ) then
                 info = 2_ilp
                 return
              end if
              ! copy d to x
              call stdlib_dcopy( m, d, 1_ilp, x, 1_ilp )
           end if
           ! backward transformation y = z**t *y
           call stdlib_dormrq( 'LEFT', 'TRANSPOSE', p, 1_ilp, np,b( max( 1_ilp, n-p+1 ), 1_ilp ), ldb, work( &
                     m+1 ), y,max( 1_ilp, p ), work( m+np+1 ), lwork-m-np, info )
           work( 1_ilp ) = m + np + max( lopt, int( work( m+np+1 ),KIND=ilp) )
           return
     end subroutine stdlib_dggglm


     pure module subroutine stdlib_cggglm( n, m, p, a, lda, b, ldb, d, x, y, work, lwork,info )
     !! CGGGLM solves a general Gauss-Markov linear model (GLM) problem:
     !! minimize || y ||_2   subject to   d = A*x + B*y
     !! x
     !! where A is an N-by-M matrix, B is an N-by-P matrix, and d is a
     !! given N-vector. It is assumed that M <= N <= M+P, and
     !! rank(A) = M    and    rank( A B ) = N.
     !! Under these assumptions, the constrained equation is always
     !! consistent, and there is a unique solution x and a minimal 2-norm
     !! solution y, which is obtained using a generalized QR factorization
     !! of the matrices (A, B) given by
     !! A = Q*(R),   B = Q*T*Z.
     !! (0)
     !! In particular, if matrix B is square nonsingular, then the problem
     !! GLM is equivalent to the following weighted linear least squares
     !! problem
     !! minimize || inv(B)*(d-A*x) ||_2
     !! x
     !! where inv(B) denotes the inverse of B.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, lwork, m, n, p
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*), d(*)
           complex(sp), intent(out) :: work(*), x(*), y(*)
        ! ===================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: i, lopt, lwkmin, lwkopt, nb, nb1, nb2, nb3, nb4, np
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp
           np = min( n, p )
           lquery = ( lwork==-1_ilp )
           if( n<0_ilp ) then
              info = -1_ilp
           else if( m<0_ilp .or. m>n ) then
              info = -2_ilp
           else if( p<0_ilp .or. p<n-m ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -7_ilp
           end if
           ! calculate workspace
           if( info==0_ilp) then
              if( n==0_ilp ) then
                 lwkmin = 1_ilp
                 lwkopt = 1_ilp
              else
                 nb1 = stdlib_ilaenv( 1_ilp, 'CGEQRF', ' ', n, m, -1_ilp, -1_ilp )
                 nb2 = stdlib_ilaenv( 1_ilp, 'CGERQF', ' ', n, m, -1_ilp, -1_ilp )
                 nb3 = stdlib_ilaenv( 1_ilp, 'CUNMQR', ' ', n, m, p, -1_ilp )
                 nb4 = stdlib_ilaenv( 1_ilp, 'CUNMRQ', ' ', n, m, p, -1_ilp )
                 nb = max( nb1, nb2, nb3, nb4 )
                 lwkmin = m + n + p
                 lwkopt = m + np + max( n, p )*nb
              end if
              work( 1_ilp ) = lwkopt
              if( lwork<lwkmin .and. .not.lquery ) then
                 info = -12_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CGGGLM', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0_ilp ) then
              do i = 1, m
                 x(i) = czero
              end do
              do i = 1, p
                 y(i) = czero
              end do
              return
           end if
           ! compute the gqr factorization of matrices a and b:
                ! q**h*a = ( r11 ) m,    q**h*b*z**h = ( t11   t12 ) m
                         ! (  0  ) n-m                 (  0    t22 ) n-m
                            ! m                         m+p-n  n-m
           ! where r11 and t22 are upper triangular, and q and z are
           ! unitary.
           call stdlib_cggqrf( n, m, p, a, lda, work, b, ldb, work( m+1 ),work( m+np+1 ), lwork-m-&
                     np, info )
           lopt = real( work( m+np+1 ),KIND=sp)
           ! update left-hand-side vector d = q**h*d = ( d1 ) m
                                                     ! ( d2 ) n-m
           call stdlib_cunmqr( 'LEFT', 'CONJUGATE TRANSPOSE', n, 1_ilp, m, a, lda, work,d, max( 1_ilp, n )&
                     , work( m+np+1 ), lwork-m-np, info )
           lopt = max( lopt, int( work( m+np+1 ),KIND=ilp) )
           ! solve t22*y2 = d2 for y2
           if( n>m ) then
              call stdlib_ctrtrs( 'UPPER', 'NO TRANSPOSE', 'NON UNIT', n-m, 1_ilp,b( m+1, m+p-n+1 ), &
                        ldb, d( m+1 ), n-m, info )
              if( info>0_ilp ) then
                 info = 1_ilp
                 return
              end if
              call stdlib_ccopy( n-m, d( m+1 ), 1_ilp, y( m+p-n+1 ), 1_ilp )
           end if
           ! set y1 = 0
           do i = 1, m + p - n
              y( i ) = czero
           end do
           ! update d1 = d1 - t12*y2
           call stdlib_cgemv( 'NO TRANSPOSE', m, n-m, -cone, b( 1_ilp, m+p-n+1 ), ldb,y( m+p-n+1 ), 1_ilp,&
                      cone, d, 1_ilp )
           ! solve triangular system: r11*x = d1
           if( m>0_ilp ) then
              call stdlib_ctrtrs( 'UPPER', 'NO TRANSPOSE', 'NON UNIT', m, 1_ilp, a, lda,d, m, info )
                        
              if( info>0_ilp ) then
                 info = 2_ilp
                 return
              end if
              ! copy d to x
              call stdlib_ccopy( m, d, 1_ilp, x, 1_ilp )
           end if
           ! backward transformation y = z**h *y
           call stdlib_cunmrq( 'LEFT', 'CONJUGATE TRANSPOSE', p, 1_ilp, np,b( max( 1_ilp, n-p+1 ), 1_ilp ), &
                     ldb, work( m+1 ), y,max( 1_ilp, p ), work( m+np+1 ), lwork-m-np, info )
           work( 1_ilp ) = m + np + max( lopt, int( work( m+np+1 ),KIND=ilp) )
           return
     end subroutine stdlib_cggglm

     pure module subroutine stdlib_zggglm( n, m, p, a, lda, b, ldb, d, x, y, work, lwork,info )
     !! ZGGGLM solves a general Gauss-Markov linear model (GLM) problem:
     !! minimize || y ||_2   subject to   d = A*x + B*y
     !! x
     !! where A is an N-by-M matrix, B is an N-by-P matrix, and d is a
     !! given N-vector. It is assumed that M <= N <= M+P, and
     !! rank(A) = M    and    rank( A B ) = N.
     !! Under these assumptions, the constrained equation is always
     !! consistent, and there is a unique solution x and a minimal 2-norm
     !! solution y, which is obtained using a generalized QR factorization
     !! of the matrices (A, B) given by
     !! A = Q*(R),   B = Q*T*Z.
     !! (0)
     !! In particular, if matrix B is square nonsingular, then the problem
     !! GLM is equivalent to the following weighted linear least squares
     !! problem
     !! minimize || inv(B)*(d-A*x) ||_2
     !! x
     !! where inv(B) denotes the inverse of B.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, lwork, m, n, p
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*), d(*)
           complex(dp), intent(out) :: work(*), x(*), y(*)
        ! ===================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery
           integer(ilp) :: i, lopt, lwkmin, lwkopt, nb, nb1, nb2, nb3, nb4, np
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp
           np = min( n, p )
           lquery = ( lwork==-1_ilp )
           if( n<0_ilp ) then
              info = -1_ilp
           else if( m<0_ilp .or. m>n ) then
              info = -2_ilp
           else if( p<0_ilp .or. p<n-m ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -7_ilp
           end if
           ! calculate workspace
           if( info==0_ilp) then
              if( n==0_ilp ) then
                 lwkmin = 1_ilp
                 lwkopt = 1_ilp
              else
                 nb1 = stdlib_ilaenv( 1_ilp, 'ZGEQRF', ' ', n, m, -1_ilp, -1_ilp )
                 nb2 = stdlib_ilaenv( 1_ilp, 'ZGERQF', ' ', n, m, -1_ilp, -1_ilp )
                 nb3 = stdlib_ilaenv( 1_ilp, 'ZUNMQR', ' ', n, m, p, -1_ilp )
                 nb4 = stdlib_ilaenv( 1_ilp, 'ZUNMRQ', ' ', n, m, p, -1_ilp )
                 nb = max( nb1, nb2, nb3, nb4 )
                 lwkmin = m + n + p
                 lwkopt = m + np + max( n, p )*nb
              end if
              work( 1_ilp ) = lwkopt
              if( lwork<lwkmin .and. .not.lquery ) then
                 info = -12_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZGGGLM', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0_ilp ) then
              do i = 1, m
                 x(i) = czero
              end do
              do i = 1, p
                 y(i) = czero
              end do
              return
           end if
           ! compute the gqr factorization of matrices a and b:
                ! q**h*a = ( r11 ) m,    q**h*b*z**h = ( t11   t12 ) m
                         ! (  0  ) n-m                 (  0    t22 ) n-m
                            ! m                         m+p-n  n-m
           ! where r11 and t22 are upper triangular, and q and z are
           ! unitary.
           call stdlib_zggqrf( n, m, p, a, lda, work, b, ldb, work( m+1 ),work( m+np+1 ), lwork-m-&
                     np, info )
           lopt = real( work( m+np+1 ),KIND=dp)
           ! update left-hand-side vector d = q**h*d = ( d1 ) m
                                                     ! ( d2 ) n-m
           call stdlib_zunmqr( 'LEFT', 'CONJUGATE TRANSPOSE', n, 1_ilp, m, a, lda, work,d, max( 1_ilp, n )&
                     , work( m+np+1 ), lwork-m-np, info )
           lopt = max( lopt, int( work( m+np+1 ),KIND=ilp) )
           ! solve t22*y2 = d2 for y2
           if( n>m ) then
              call stdlib_ztrtrs( 'UPPER', 'NO TRANSPOSE', 'NON UNIT', n-m, 1_ilp,b( m+1, m+p-n+1 ), &
                        ldb, d( m+1 ), n-m, info )
              if( info>0_ilp ) then
                 info = 1_ilp
                 return
              end if
              call stdlib_zcopy( n-m, d( m+1 ), 1_ilp, y( m+p-n+1 ), 1_ilp )
           end if
           ! set y1 = 0
           do i = 1, m + p - n
              y( i ) = czero
           end do
           ! update d1 = d1 - t12*y2
           call stdlib_zgemv( 'NO TRANSPOSE', m, n-m, -cone, b( 1_ilp, m+p-n+1 ), ldb,y( m+p-n+1 ), 1_ilp,&
                      cone, d, 1_ilp )
           ! solve triangular system: r11*x = d1
           if( m>0_ilp ) then
              call stdlib_ztrtrs( 'UPPER', 'NO TRANSPOSE', 'NON UNIT', m, 1_ilp, a, lda,d, m, info )
                        
              if( info>0_ilp ) then
                 info = 2_ilp
                 return
              end if
              ! copy d to x
              call stdlib_zcopy( m, d, 1_ilp, x, 1_ilp )
           end if
           ! backward transformation y = z**h *y
           call stdlib_zunmrq( 'LEFT', 'CONJUGATE TRANSPOSE', p, 1_ilp, np,b( max( 1_ilp, n-p+1 ), 1_ilp ), &
                     ldb, work( m+1 ), y,max( 1_ilp, p ), work( m+np+1 ), lwork-m-np, info )
           work( 1_ilp ) = m + np + max( lopt, int( work( m+np+1 ),KIND=ilp) )
           return
     end subroutine stdlib_zggglm



end submodule stdlib_lapack_lsq_constrained
