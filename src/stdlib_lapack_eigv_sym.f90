submodule(stdlib_lapack_eig_svd_lsq) stdlib_lapack_eigv_sym
  implicit none


  contains

     module subroutine stdlib_ssygv( itype, jobz, uplo, n, a, lda, b, ldb, w, work,lwork, info )
     !! SSYGV computes all the eigenvalues, and optionally, the eigenvectors
     !! of a real generalized symmetric-definite eigenproblem, of the form
     !! A*x=(lambda)*B*x,  A*Bx=(lambda)*x,  or B*A*x=(lambda)*x.
     !! Here A and B are assumed to be symmetric and B is also
     !! positive definite.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: itype, lda, ldb, lwork, n
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*), b(ldb,*)
           real(sp), intent(out) :: w(*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery, upper, wantz
           character :: trans
           integer(ilp) :: lwkmin, lwkopt, nb, neig
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           upper = stdlib_lsame( uplo, 'U' )
           lquery = ( lwork==-1_ilp )
           info = 0_ilp
           if( itype<1_ilp .or. itype>3_ilp ) then
              info = -1_ilp
           else if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -2_ilp
           else if( .not.( upper .or. stdlib_lsame( uplo, 'L' ) ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -6_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -8_ilp
           end if
           if( info==0_ilp ) then
              lwkmin = max( 1_ilp, 3_ilp*n - 1_ilp )
              nb = stdlib_ilaenv( 1_ilp, 'SSYTRD', uplo, n, -1_ilp, -1_ilp, -1_ilp )
              lwkopt = max( lwkmin, ( nb + 2_ilp )*n )
              work( 1_ilp ) = lwkopt
              if( lwork<lwkmin .and. .not.lquery ) then
                 info = -11_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SSYGV ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! form a cholesky factorization of b.
           call stdlib_spotrf( uplo, n, b, ldb, info )
           if( info/=0_ilp ) then
              info = n + info
              return
           end if
           ! transform problem to standard eigenvalue problem and solve.
           call stdlib_ssygst( itype, uplo, n, a, lda, b, ldb, info )
           call stdlib_ssyev( jobz, uplo, n, a, lda, w, work, lwork, info )
           if( wantz ) then
              ! backtransform eigenvectors to the original problem.
              neig = n
              if( info>0_ilp )neig = info - 1_ilp
              if( itype==1_ilp .or. itype==2_ilp ) then
                 ! for a*x=(lambda)*b*x and a*b*x=(lambda)*x;
                 ! backtransform eigenvectors: x = inv(l)**t*y or inv(u)*y
                 if( upper ) then
                    trans = 'N'
                 else
                    trans = 'T'
                 end if
                 call stdlib_strsm( 'LEFT', uplo, trans, 'NON-UNIT', n, neig, one,b, ldb, a, lda )
                           
              else if( itype==3_ilp ) then
                 ! for b*a*x=(lambda)*x;
                 ! backtransform eigenvectors: x = l*y or u**t*y
                 if( upper ) then
                    trans = 'T'
                 else
                    trans = 'N'
                 end if
                 call stdlib_strmm( 'LEFT', uplo, trans, 'NON-UNIT', n, neig, one,b, ldb, a, lda )
                           
              end if
           end if
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_ssygv

     module subroutine stdlib_dsygv( itype, jobz, uplo, n, a, lda, b, ldb, w, work,lwork, info )
     !! DSYGV computes all the eigenvalues, and optionally, the eigenvectors
     !! of a real generalized symmetric-definite eigenproblem, of the form
     !! A*x=(lambda)*B*x,  A*Bx=(lambda)*x,  or B*A*x=(lambda)*x.
     !! Here A and B are assumed to be symmetric and B is also
     !! positive definite.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: itype, lda, ldb, lwork, n
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*), b(ldb,*)
           real(dp), intent(out) :: w(*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery, upper, wantz
           character :: trans
           integer(ilp) :: lwkmin, lwkopt, nb, neig
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           upper = stdlib_lsame( uplo, 'U' )
           lquery = ( lwork==-1_ilp )
           info = 0_ilp
           if( itype<1_ilp .or. itype>3_ilp ) then
              info = -1_ilp
           else if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -2_ilp
           else if( .not.( upper .or. stdlib_lsame( uplo, 'L' ) ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -6_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -8_ilp
           end if
           if( info==0_ilp ) then
              lwkmin = max( 1_ilp, 3_ilp*n - 1_ilp )
              nb = stdlib_ilaenv( 1_ilp, 'DSYTRD', uplo, n, -1_ilp, -1_ilp, -1_ilp )
              lwkopt = max( lwkmin, ( nb + 2_ilp )*n )
              work( 1_ilp ) = lwkopt
              if( lwork<lwkmin .and. .not.lquery ) then
                 info = -11_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DSYGV ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! form a cholesky factorization of b.
           call stdlib_dpotrf( uplo, n, b, ldb, info )
           if( info/=0_ilp ) then
              info = n + info
              return
           end if
           ! transform problem to standard eigenvalue problem and solve.
           call stdlib_dsygst( itype, uplo, n, a, lda, b, ldb, info )
           call stdlib_dsyev( jobz, uplo, n, a, lda, w, work, lwork, info )
           if( wantz ) then
              ! backtransform eigenvectors to the original problem.
              neig = n
              if( info>0_ilp )neig = info - 1_ilp
              if( itype==1_ilp .or. itype==2_ilp ) then
                 ! for a*x=(lambda)*b*x and a*b*x=(lambda)*x;
                 ! backtransform eigenvectors: x = inv(l)**t*y or inv(u)*y
                 if( upper ) then
                    trans = 'N'
                 else
                    trans = 'T'
                 end if
                 call stdlib_dtrsm( 'LEFT', uplo, trans, 'NON-UNIT', n, neig, one,b, ldb, a, lda )
                           
              else if( itype==3_ilp ) then
                 ! for b*a*x=(lambda)*x;
                 ! backtransform eigenvectors: x = l*y or u**t*y
                 if( upper ) then
                    trans = 'T'
                 else
                    trans = 'N'
                 end if
                 call stdlib_dtrmm( 'LEFT', uplo, trans, 'NON-UNIT', n, neig, one,b, ldb, a, lda )
                           
              end if
           end if
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_dsygv




     module subroutine stdlib_ssygvd( itype, jobz, uplo, n, a, lda, b, ldb, w, work,lwork, iwork, liwork,&
     !! SSYGVD computes all the eigenvalues, and optionally, the eigenvectors
     !! of a real generalized symmetric-definite eigenproblem, of the form
     !! A*x=(lambda)*B*x,  A*Bx=(lambda)*x,  or B*A*x=(lambda)*x.  Here A and
     !! B are assumed to be symmetric and B is also positive definite.
     !! If eigenvectors are desired, it uses a divide and conquer algorithm.
     !! The divide and conquer algorithm makes very mild assumptions about
     !! floating point arithmetic. It will work on machines with a guard
     !! digit in add/subtract, or on those binary machines without guard
     !! digits which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or
     !! Cray-2. It could conceivably fail on hexadecimal or decimal machines
     !! without guard digits, but we know of none.
                info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: itype, lda, ldb, liwork, lwork, n
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(inout) :: a(lda,*), b(ldb,*)
           real(sp), intent(out) :: w(*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery, upper, wantz
           character :: trans
           integer(ilp) :: liopt, liwmin, lopt, lwmin
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           upper = stdlib_lsame( uplo, 'U' )
           lquery = ( lwork==-1_ilp .or. liwork==-1_ilp )
           info = 0_ilp
           if( n<=1_ilp ) then
              liwmin = 1_ilp
              lwmin = 1_ilp
           else if( wantz ) then
              liwmin = 3_ilp + 5_ilp*n
              lwmin = 1_ilp + 6_ilp*n + 2_ilp*n**2_ilp
           else
              liwmin = 1_ilp
              lwmin = 2_ilp*n + 1_ilp
           end if
           lopt = lwmin
           liopt = liwmin
           if( itype<1_ilp .or. itype>3_ilp ) then
              info = -1_ilp
           else if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -2_ilp
           else if( .not.( upper .or. stdlib_lsame( uplo, 'L' ) ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -6_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -8_ilp
           end if
           if( info==0_ilp ) then
              work( 1_ilp ) = lopt
              iwork( 1_ilp ) = liopt
              if( lwork<lwmin .and. .not.lquery ) then
                 info = -11_ilp
              else if( liwork<liwmin .and. .not.lquery ) then
                 info = -13_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SSYGVD', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! form a cholesky factorization of b.
           call stdlib_spotrf( uplo, n, b, ldb, info )
           if( info/=0_ilp ) then
              info = n + info
              return
           end if
           ! transform problem to standard eigenvalue problem and solve.
           call stdlib_ssygst( itype, uplo, n, a, lda, b, ldb, info )
           call stdlib_ssyevd( jobz, uplo, n, a, lda, w, work, lwork, iwork, liwork,info )
           lopt = max( real( lopt,KIND=sp), real( work( 1_ilp ),KIND=sp) )
           liopt = max( real( liopt,KIND=sp), real( iwork( 1_ilp ),KIND=sp) )
           if( wantz .and. info==0_ilp ) then
              ! backtransform eigenvectors to the original problem.
              if( itype==1_ilp .or. itype==2_ilp ) then
                 ! for a*x=(lambda)*b*x and a*b*x=(lambda)*x;
                 ! backtransform eigenvectors: x = inv(l)**t*y or inv(u)*y
                 if( upper ) then
                    trans = 'N'
                 else
                    trans = 'T'
                 end if
                 call stdlib_strsm( 'LEFT', uplo, trans, 'NON-UNIT', n, n, one,b, ldb, a, lda )
                           
              else if( itype==3_ilp ) then
                 ! for b*a*x=(lambda)*x;
                 ! backtransform eigenvectors: x = l*y or u**t*y
                 if( upper ) then
                    trans = 'T'
                 else
                    trans = 'N'
                 end if
                 call stdlib_strmm( 'LEFT', uplo, trans, 'NON-UNIT', n, n, one,b, ldb, a, lda )
                           
              end if
           end if
           work( 1_ilp ) = lopt
           iwork( 1_ilp ) = liopt
           return
     end subroutine stdlib_ssygvd

     module subroutine stdlib_dsygvd( itype, jobz, uplo, n, a, lda, b, ldb, w, work,lwork, iwork, liwork,&
     !! DSYGVD computes all the eigenvalues, and optionally, the eigenvectors
     !! of a real generalized symmetric-definite eigenproblem, of the form
     !! A*x=(lambda)*B*x,  A*Bx=(lambda)*x,  or B*A*x=(lambda)*x.  Here A and
     !! B are assumed to be symmetric and B is also positive definite.
     !! If eigenvectors are desired, it uses a divide and conquer algorithm.
     !! The divide and conquer algorithm makes very mild assumptions about
     !! floating point arithmetic. It will work on machines with a guard
     !! digit in add/subtract, or on those binary machines without guard
     !! digits which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or
     !! Cray-2. It could conceivably fail on hexadecimal or decimal machines
     !! without guard digits, but we know of none.
                info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: itype, lda, ldb, liwork, lwork, n
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(inout) :: a(lda,*), b(ldb,*)
           real(dp), intent(out) :: w(*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery, upper, wantz
           character :: trans
           integer(ilp) :: liopt, liwmin, lopt, lwmin
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           upper = stdlib_lsame( uplo, 'U' )
           lquery = ( lwork==-1_ilp .or. liwork==-1_ilp )
           info = 0_ilp
           if( n<=1_ilp ) then
              liwmin = 1_ilp
              lwmin = 1_ilp
           else if( wantz ) then
              liwmin = 3_ilp + 5_ilp*n
              lwmin = 1_ilp + 6_ilp*n + 2_ilp*n**2_ilp
           else
              liwmin = 1_ilp
              lwmin = 2_ilp*n + 1_ilp
           end if
           lopt = lwmin
           liopt = liwmin
           if( itype<1_ilp .or. itype>3_ilp ) then
              info = -1_ilp
           else if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -2_ilp
           else if( .not.( upper .or. stdlib_lsame( uplo, 'L' ) ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -6_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -8_ilp
           end if
           if( info==0_ilp ) then
              work( 1_ilp ) = lopt
              iwork( 1_ilp ) = liopt
              if( lwork<lwmin .and. .not.lquery ) then
                 info = -11_ilp
              else if( liwork<liwmin .and. .not.lquery ) then
                 info = -13_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DSYGVD', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! form a cholesky factorization of b.
           call stdlib_dpotrf( uplo, n, b, ldb, info )
           if( info/=0_ilp ) then
              info = n + info
              return
           end if
           ! transform problem to standard eigenvalue problem and solve.
           call stdlib_dsygst( itype, uplo, n, a, lda, b, ldb, info )
           call stdlib_dsyevd( jobz, uplo, n, a, lda, w, work, lwork, iwork, liwork,info )
           lopt = max( real( lopt,KIND=dp), real( work( 1_ilp ),KIND=dp) )
           liopt = max( real( liopt,KIND=dp), real( iwork( 1_ilp ),KIND=dp) )
           if( wantz .and. info==0_ilp ) then
              ! backtransform eigenvectors to the original problem.
              if( itype==1_ilp .or. itype==2_ilp ) then
                 ! for a*x=(lambda)*b*x and a*b*x=(lambda)*x;
                 ! backtransform eigenvectors: x = inv(l)**t*y or inv(u)*y
                 if( upper ) then
                    trans = 'N'
                 else
                    trans = 'T'
                 end if
                 call stdlib_dtrsm( 'LEFT', uplo, trans, 'NON-UNIT', n, n, one,b, ldb, a, lda )
                           
              else if( itype==3_ilp ) then
                 ! for b*a*x=(lambda)*x;
                 ! backtransform eigenvectors: x = l*y or u**t*y
                 if( upper ) then
                    trans = 'T'
                 else
                    trans = 'N'
                 end if
                 call stdlib_dtrmm( 'LEFT', uplo, trans, 'NON-UNIT', n, n, one,b, ldb, a, lda )
                           
              end if
           end if
           work( 1_ilp ) = lopt
           iwork( 1_ilp ) = liopt
           return
     end subroutine stdlib_dsygvd




     module subroutine stdlib_ssygvx( itype, jobz, range, uplo, n, a, lda, b, ldb,vl, vu, il, iu, abstol,&
     !! SSYGVX computes selected eigenvalues, and optionally, eigenvectors
     !! of a real generalized symmetric-definite eigenproblem, of the form
     !! A*x=(lambda)*B*x,  A*Bx=(lambda)*x,  or B*A*x=(lambda)*x.  Here A
     !! and B are assumed to be symmetric and B is also positive definite.
     !! Eigenvalues and eigenvectors can be selected by specifying either a
     !! range of values or a range of indices for the desired eigenvalues.
                m, w, z, ldz, work,lwork, iwork, ifail, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, range, uplo
           integer(ilp), intent(in) :: il, itype, iu, lda, ldb, ldz, lwork, n
           integer(ilp), intent(out) :: info, m
           real(sp), intent(in) :: abstol, vl, vu
           ! Array Arguments 
           integer(ilp), intent(out) :: ifail(*), iwork(*)
           real(sp), intent(inout) :: a(lda,*), b(ldb,*)
           real(sp), intent(out) :: w(*), work(*), z(ldz,*)
       ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: alleig, indeig, lquery, upper, valeig, wantz
           character :: trans
           integer(ilp) :: lwkmin, lwkopt, nb
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           upper = stdlib_lsame( uplo, 'U' )
           wantz = stdlib_lsame( jobz, 'V' )
           alleig = stdlib_lsame( range, 'A' )
           valeig = stdlib_lsame( range, 'V' )
           indeig = stdlib_lsame( range, 'I' )
           lquery = ( lwork==-1_ilp )
           info = 0_ilp
           if( itype<1_ilp .or. itype>3_ilp ) then
              info = -1_ilp
           else if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -2_ilp
           else if( .not.( alleig .or. valeig .or. indeig ) ) then
              info = -3_ilp
           else if( .not.( upper .or. stdlib_lsame( uplo, 'L' ) ) ) then
              info = -4_ilp
           else if( n<0_ilp ) then
              info = -5_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -7_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -9_ilp
           else
              if( valeig ) then
                 if( n>0_ilp .and. vu<=vl )info = -11_ilp
              else if( indeig ) then
                 if( il<1_ilp .or. il>max( 1_ilp, n ) ) then
                    info = -12_ilp
                 else if( iu<min( n, il ) .or. iu>n ) then
                    info = -13_ilp
                 end if
              end if
           end if
           if (info==0_ilp) then
              if (ldz<1_ilp .or. (wantz .and. ldz<n)) then
                 info = -18_ilp
              end if
           end if
           if( info==0_ilp ) then
              lwkmin = max( 1_ilp, 8_ilp*n )
              nb = stdlib_ilaenv( 1_ilp, 'SSYTRD', uplo, n, -1_ilp, -1_ilp, -1_ilp )
              lwkopt = max( lwkmin, ( nb + 3_ilp )*n )
              work( 1_ilp ) = lwkopt
              if( lwork<lwkmin .and. .not.lquery ) then
                 info = -20_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SSYGVX', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           m = 0_ilp
           if( n==0_ilp ) then
              return
           end if
           ! form a cholesky factorization of b.
           call stdlib_spotrf( uplo, n, b, ldb, info )
           if( info/=0_ilp ) then
              info = n + info
              return
           end if
           ! transform problem to standard eigenvalue problem and solve.
           call stdlib_ssygst( itype, uplo, n, a, lda, b, ldb, info )
           call stdlib_ssyevx( jobz, range, uplo, n, a, lda, vl, vu, il, iu, abstol,m, w, z, ldz, &
                     work, lwork, iwork, ifail, info )
           if( wantz ) then
              ! backtransform eigenvectors to the original problem.
              if( info>0_ilp )m = info - 1_ilp
              if( itype==1_ilp .or. itype==2_ilp ) then
                 ! for a*x=(lambda)*b*x and a*b*x=(lambda)*x;
                 ! backtransform eigenvectors: x = inv(l)**t*y or inv(u)*y
                 if( upper ) then
                    trans = 'N'
                 else
                    trans = 'T'
                 end if
                 call stdlib_strsm( 'LEFT', uplo, trans, 'NON-UNIT', n, m, one, b,ldb, z, ldz )
                           
              else if( itype==3_ilp ) then
                 ! for b*a*x=(lambda)*x;
                 ! backtransform eigenvectors: x = l*y or u**t*y
                 if( upper ) then
                    trans = 'T'
                 else
                    trans = 'N'
                 end if
                 call stdlib_strmm( 'LEFT', uplo, trans, 'NON-UNIT', n, m, one, b,ldb, z, ldz )
                           
              end if
           end if
           ! set work(1) to optimal workspace size.
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_ssygvx

     module subroutine stdlib_dsygvx( itype, jobz, range, uplo, n, a, lda, b, ldb,vl, vu, il, iu, abstol,&
     !! DSYGVX computes selected eigenvalues, and optionally, eigenvectors
     !! of a real generalized symmetric-definite eigenproblem, of the form
     !! A*x=(lambda)*B*x,  A*Bx=(lambda)*x,  or B*A*x=(lambda)*x.  Here A
     !! and B are assumed to be symmetric and B is also positive definite.
     !! Eigenvalues and eigenvectors can be selected by specifying either a
     !! range of values or a range of indices for the desired eigenvalues.
                m, w, z, ldz, work,lwork, iwork, ifail, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, range, uplo
           integer(ilp), intent(in) :: il, itype, iu, lda, ldb, ldz, lwork, n
           integer(ilp), intent(out) :: info, m
           real(dp), intent(in) :: abstol, vl, vu
           ! Array Arguments 
           integer(ilp), intent(out) :: ifail(*), iwork(*)
           real(dp), intent(inout) :: a(lda,*), b(ldb,*)
           real(dp), intent(out) :: w(*), work(*), z(ldz,*)
       ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: alleig, indeig, lquery, upper, valeig, wantz
           character :: trans
           integer(ilp) :: lwkmin, lwkopt, nb
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           upper = stdlib_lsame( uplo, 'U' )
           wantz = stdlib_lsame( jobz, 'V' )
           alleig = stdlib_lsame( range, 'A' )
           valeig = stdlib_lsame( range, 'V' )
           indeig = stdlib_lsame( range, 'I' )
           lquery = ( lwork==-1_ilp )
           info = 0_ilp
           if( itype<1_ilp .or. itype>3_ilp ) then
              info = -1_ilp
           else if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -2_ilp
           else if( .not.( alleig .or. valeig .or. indeig ) ) then
              info = -3_ilp
           else if( .not.( upper .or. stdlib_lsame( uplo, 'L' ) ) ) then
              info = -4_ilp
           else if( n<0_ilp ) then
              info = -5_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -7_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -9_ilp
           else
              if( valeig ) then
                 if( n>0_ilp .and. vu<=vl )info = -11_ilp
              else if( indeig ) then
                 if( il<1_ilp .or. il>max( 1_ilp, n ) ) then
                    info = -12_ilp
                 else if( iu<min( n, il ) .or. iu>n ) then
                    info = -13_ilp
                 end if
              end if
           end if
           if (info==0_ilp) then
              if (ldz<1_ilp .or. (wantz .and. ldz<n)) then
                 info = -18_ilp
              end if
           end if
           if( info==0_ilp ) then
              lwkmin = max( 1_ilp, 8_ilp*n )
              nb = stdlib_ilaenv( 1_ilp, 'DSYTRD', uplo, n, -1_ilp, -1_ilp, -1_ilp )
              lwkopt = max( lwkmin, ( nb + 3_ilp )*n )
              work( 1_ilp ) = lwkopt
              if( lwork<lwkmin .and. .not.lquery ) then
                 info = -20_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DSYGVX', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           m = 0_ilp
           if( n==0_ilp ) then
              return
           end if
           ! form a cholesky factorization of b.
           call stdlib_dpotrf( uplo, n, b, ldb, info )
           if( info/=0_ilp ) then
              info = n + info
              return
           end if
           ! transform problem to standard eigenvalue problem and solve.
           call stdlib_dsygst( itype, uplo, n, a, lda, b, ldb, info )
           call stdlib_dsyevx( jobz, range, uplo, n, a, lda, vl, vu, il, iu, abstol,m, w, z, ldz, &
                     work, lwork, iwork, ifail, info )
           if( wantz ) then
              ! backtransform eigenvectors to the original problem.
              if( info>0_ilp )m = info - 1_ilp
              if( itype==1_ilp .or. itype==2_ilp ) then
                 ! for a*x=(lambda)*b*x and a*b*x=(lambda)*x;
                 ! backtransform eigenvectors: x = inv(l)**t*y or inv(u)*y
                 if( upper ) then
                    trans = 'N'
                 else
                    trans = 'T'
                 end if
                 call stdlib_dtrsm( 'LEFT', uplo, trans, 'NON-UNIT', n, m, one, b,ldb, z, ldz )
                           
              else if( itype==3_ilp ) then
                 ! for b*a*x=(lambda)*x;
                 ! backtransform eigenvectors: x = l*y or u**t*y
                 if( upper ) then
                    trans = 'T'
                 else
                    trans = 'N'
                 end if
                 call stdlib_dtrmm( 'LEFT', uplo, trans, 'NON-UNIT', n, m, one, b,ldb, z, ldz )
                           
              end if
           end if
           ! set work(1) to optimal workspace size.
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_dsygvx




     module subroutine stdlib_sspgv( itype, jobz, uplo, n, ap, bp, w, z, ldz, work,info )
     !! SSPGV computes all the eigenvalues and, optionally, the eigenvectors
     !! of a real generalized symmetric-definite eigenproblem, of the form
     !! A*x=(lambda)*B*x,  A*Bx=(lambda)*x,  or B*A*x=(lambda)*x.
     !! Here A and B are assumed to be symmetric, stored in packed format,
     !! and B is also positive definite.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: itype, ldz, n
           ! Array Arguments 
           real(sp), intent(inout) :: ap(*), bp(*)
           real(sp), intent(out) :: w(*), work(*), z(ldz,*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: upper, wantz
           character :: trans
           integer(ilp) :: j, neig
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           upper = stdlib_lsame( uplo, 'U' )
           info = 0_ilp
           if( itype<1_ilp .or. itype>3_ilp ) then
              info = -1_ilp
           else if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -2_ilp
           else if( .not.( upper .or. stdlib_lsame( uplo, 'L' ) ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( ldz<1_ilp .or. ( wantz .and. ldz<n ) ) then
              info = -9_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SSPGV ', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! form a cholesky factorization of b.
           call stdlib_spptrf( uplo, n, bp, info )
           if( info/=0_ilp ) then
              info = n + info
              return
           end if
           ! transform problem to standard eigenvalue problem and solve.
           call stdlib_sspgst( itype, uplo, n, ap, bp, info )
           call stdlib_sspev( jobz, uplo, n, ap, w, z, ldz, work, info )
           if( wantz ) then
              ! backtransform eigenvectors to the original problem.
              neig = n
              if( info>0_ilp )neig = info - 1_ilp
              if( itype==1_ilp .or. itype==2_ilp ) then
                 ! for a*x=(lambda)*b*x and a*b*x=(lambda)*x;
                 ! backtransform eigenvectors: x = inv(l)**t*y or inv(u)*y
                 if( upper ) then
                    trans = 'N'
                 else
                    trans = 'T'
                 end if
                 do j = 1, neig
                    call stdlib_stpsv( uplo, trans, 'NON-UNIT', n, bp, z( 1_ilp, j ),1_ilp )
                 end do
              else if( itype==3_ilp ) then
                 ! for b*a*x=(lambda)*x;
                 ! backtransform eigenvectors: x = l*y or u**t*y
                 if( upper ) then
                    trans = 'T'
                 else
                    trans = 'N'
                 end if
                 do j = 1, neig
                    call stdlib_stpmv( uplo, trans, 'NON-UNIT', n, bp, z( 1_ilp, j ),1_ilp )
                 end do
              end if
           end if
           return
     end subroutine stdlib_sspgv

     module subroutine stdlib_dspgv( itype, jobz, uplo, n, ap, bp, w, z, ldz, work,info )
     !! DSPGV computes all the eigenvalues and, optionally, the eigenvectors
     !! of a real generalized symmetric-definite eigenproblem, of the form
     !! A*x=(lambda)*B*x,  A*Bx=(lambda)*x,  or B*A*x=(lambda)*x.
     !! Here A and B are assumed to be symmetric, stored in packed format,
     !! and B is also positive definite.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: itype, ldz, n
           ! Array Arguments 
           real(dp), intent(inout) :: ap(*), bp(*)
           real(dp), intent(out) :: w(*), work(*), z(ldz,*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: upper, wantz
           character :: trans
           integer(ilp) :: j, neig
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           upper = stdlib_lsame( uplo, 'U' )
           info = 0_ilp
           if( itype<1_ilp .or. itype>3_ilp ) then
              info = -1_ilp
           else if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -2_ilp
           else if( .not.( upper .or. stdlib_lsame( uplo, 'L' ) ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( ldz<1_ilp .or. ( wantz .and. ldz<n ) ) then
              info = -9_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DSPGV ', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! form a cholesky factorization of b.
           call stdlib_dpptrf( uplo, n, bp, info )
           if( info/=0_ilp ) then
              info = n + info
              return
           end if
           ! transform problem to standard eigenvalue problem and solve.
           call stdlib_dspgst( itype, uplo, n, ap, bp, info )
           call stdlib_dspev( jobz, uplo, n, ap, w, z, ldz, work, info )
           if( wantz ) then
              ! backtransform eigenvectors to the original problem.
              neig = n
              if( info>0_ilp )neig = info - 1_ilp
              if( itype==1_ilp .or. itype==2_ilp ) then
                 ! for a*x=(lambda)*b*x and a*b*x=(lambda)*x;
                 ! backtransform eigenvectors: x = inv(l)**t*y or inv(u)*y
                 if( upper ) then
                    trans = 'N'
                 else
                    trans = 'T'
                 end if
                 do j = 1, neig
                    call stdlib_dtpsv( uplo, trans, 'NON-UNIT', n, bp, z( 1_ilp, j ),1_ilp )
                 end do
              else if( itype==3_ilp ) then
                 ! for b*a*x=(lambda)*x;
                 ! backtransform eigenvectors: x = l*y or u**t*y
                 if( upper ) then
                    trans = 'T'
                 else
                    trans = 'N'
                 end if
                 do j = 1, neig
                    call stdlib_dtpmv( uplo, trans, 'NON-UNIT', n, bp, z( 1_ilp, j ),1_ilp )
                 end do
              end if
           end if
           return
     end subroutine stdlib_dspgv




     module subroutine stdlib_sspgvd( itype, jobz, uplo, n, ap, bp, w, z, ldz, work,lwork, iwork, liwork,&
     !! SSPGVD computes all the eigenvalues, and optionally, the eigenvectors
     !! of a real generalized symmetric-definite eigenproblem, of the form
     !! A*x=(lambda)*B*x,  A*Bx=(lambda)*x,  or B*A*x=(lambda)*x.  Here A and
     !! B are assumed to be symmetric, stored in packed format, and B is also
     !! positive definite.
     !! If eigenvectors are desired, it uses a divide and conquer algorithm.
     !! The divide and conquer algorithm makes very mild assumptions about
     !! floating point arithmetic. It will work on machines with a guard
     !! digit in add/subtract, or on those binary machines without guard
     !! digits which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or
     !! Cray-2. It could conceivably fail on hexadecimal or decimal machines
     !! without guard digits, but we know of none.
                info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: itype, ldz, liwork, lwork, n
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(inout) :: ap(*), bp(*)
           real(sp), intent(out) :: w(*), work(*), z(ldz,*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lquery, upper, wantz
           character :: trans
           integer(ilp) :: j, liwmin, lwmin, neig
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           upper = stdlib_lsame( uplo, 'U' )
           lquery = ( lwork==-1_ilp .or. liwork==-1_ilp )
           info = 0_ilp
           if( itype<1_ilp .or. itype>3_ilp ) then
              info = -1_ilp
           else if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -2_ilp
           else if( .not.( upper .or. stdlib_lsame( uplo, 'L' ) ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( ldz<1_ilp .or. ( wantz .and. ldz<n ) ) then
              info = -9_ilp
           end if
           if( info==0_ilp ) then
              if( n<=1_ilp ) then
                 liwmin = 1_ilp
                 lwmin = 1_ilp
              else
                 if( wantz ) then
                    liwmin = 3_ilp + 5_ilp*n
                    lwmin = 1_ilp + 6_ilp*n + 2_ilp*n**2_ilp
                 else
                    liwmin = 1_ilp
                    lwmin = 2_ilp*n
                 end if
              end if
              work( 1_ilp ) = lwmin
              iwork( 1_ilp ) = liwmin
              if( lwork<lwmin .and. .not.lquery ) then
                 info = -11_ilp
              else if( liwork<liwmin .and. .not.lquery ) then
                 info = -13_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SSPGVD', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! form a cholesky factorization of bp.
           call stdlib_spptrf( uplo, n, bp, info )
           if( info/=0_ilp ) then
              info = n + info
              return
           end if
           ! transform problem to standard eigenvalue problem and solve.
           call stdlib_sspgst( itype, uplo, n, ap, bp, info )
           call stdlib_sspevd( jobz, uplo, n, ap, w, z, ldz, work, lwork, iwork,liwork, info )
                     
           lwmin = max( real( lwmin,KIND=sp), real( work( 1_ilp ),KIND=sp) )
           liwmin = max( real( liwmin,KIND=sp), real( iwork( 1_ilp ),KIND=sp) )
           if( wantz ) then
              ! backtransform eigenvectors to the original problem.
              neig = n
              if( info>0_ilp )neig = info - 1_ilp
              if( itype==1_ilp .or. itype==2_ilp ) then
                 ! for a*x=(lambda)*b*x and a*b*x=(lambda)*x;
                 ! backtransform eigenvectors: x = inv(l)**t *y or inv(u)*y
                 if( upper ) then
                    trans = 'N'
                 else
                    trans = 'T'
                 end if
                 do j = 1, neig
                    call stdlib_stpsv( uplo, trans, 'NON-UNIT', n, bp, z( 1_ilp, j ),1_ilp )
                 end do
              else if( itype==3_ilp ) then
                 ! for b*a*x=(lambda)*x;
                 ! backtransform eigenvectors: x = l*y or u**t *y
                 if( upper ) then
                    trans = 'T'
                 else
                    trans = 'N'
                 end if
                 do j = 1, neig
                    call stdlib_stpmv( uplo, trans, 'NON-UNIT', n, bp, z( 1_ilp, j ),1_ilp )
                 end do
              end if
           end if
           work( 1_ilp ) = lwmin
           iwork( 1_ilp ) = liwmin
           return
     end subroutine stdlib_sspgvd

     module subroutine stdlib_dspgvd( itype, jobz, uplo, n, ap, bp, w, z, ldz, work,lwork, iwork, liwork,&
     !! DSPGVD computes all the eigenvalues, and optionally, the eigenvectors
     !! of a real generalized symmetric-definite eigenproblem, of the form
     !! A*x=(lambda)*B*x,  A*Bx=(lambda)*x,  or B*A*x=(lambda)*x.  Here A and
     !! B are assumed to be symmetric, stored in packed format, and B is also
     !! positive definite.
     !! If eigenvectors are desired, it uses a divide and conquer algorithm.
     !! The divide and conquer algorithm makes very mild assumptions about
     !! floating point arithmetic. It will work on machines with a guard
     !! digit in add/subtract, or on those binary machines without guard
     !! digits which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or
     !! Cray-2. It could conceivably fail on hexadecimal or decimal machines
     !! without guard digits, but we know of none.
                info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: itype, ldz, liwork, lwork, n
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(inout) :: ap(*), bp(*)
           real(dp), intent(out) :: w(*), work(*), z(ldz,*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lquery, upper, wantz
           character :: trans
           integer(ilp) :: j, liwmin, lwmin, neig
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           upper = stdlib_lsame( uplo, 'U' )
           lquery = ( lwork==-1_ilp .or. liwork==-1_ilp )
           info = 0_ilp
           if( itype<1_ilp .or. itype>3_ilp ) then
              info = -1_ilp
           else if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -2_ilp
           else if( .not.( upper .or. stdlib_lsame( uplo, 'L' ) ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( ldz<1_ilp .or. ( wantz .and. ldz<n ) ) then
              info = -9_ilp
           end if
           if( info==0_ilp ) then
              if( n<=1_ilp ) then
                 liwmin = 1_ilp
                 lwmin = 1_ilp
              else
                 if( wantz ) then
                    liwmin = 3_ilp + 5_ilp*n
                    lwmin = 1_ilp + 6_ilp*n + 2_ilp*n**2_ilp
                 else
                    liwmin = 1_ilp
                    lwmin = 2_ilp*n
                 end if
              end if
              work( 1_ilp ) = lwmin
              iwork( 1_ilp ) = liwmin
              if( lwork<lwmin .and. .not.lquery ) then
                 info = -11_ilp
              else if( liwork<liwmin .and. .not.lquery ) then
                 info = -13_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DSPGVD', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! form a cholesky factorization of bp.
           call stdlib_dpptrf( uplo, n, bp, info )
           if( info/=0_ilp ) then
              info = n + info
              return
           end if
           ! transform problem to standard eigenvalue problem and solve.
           call stdlib_dspgst( itype, uplo, n, ap, bp, info )
           call stdlib_dspevd( jobz, uplo, n, ap, w, z, ldz, work, lwork, iwork,liwork, info )
                     
           lwmin = max( real( lwmin,KIND=dp), real( work( 1_ilp ),KIND=dp) )
           liwmin = max( real( liwmin,KIND=dp), real( iwork( 1_ilp ),KIND=dp) )
           if( wantz ) then
              ! backtransform eigenvectors to the original problem.
              neig = n
              if( info>0_ilp )neig = info - 1_ilp
              if( itype==1_ilp .or. itype==2_ilp ) then
                 ! for a*x=(lambda)*b*x and a*b*x=(lambda)*x;
                 ! backtransform eigenvectors: x = inv(l)**t *y or inv(u)*y
                 if( upper ) then
                    trans = 'N'
                 else
                    trans = 'T'
                 end if
                 do j = 1, neig
                    call stdlib_dtpsv( uplo, trans, 'NON-UNIT', n, bp, z( 1_ilp, j ),1_ilp )
                 end do
              else if( itype==3_ilp ) then
                 ! for b*a*x=(lambda)*x;
                 ! backtransform eigenvectors: x = l*y or u**t *y
                 if( upper ) then
                    trans = 'T'
                 else
                    trans = 'N'
                 end if
                 do j = 1, neig
                    call stdlib_dtpmv( uplo, trans, 'NON-UNIT', n, bp, z( 1_ilp, j ),1_ilp )
                 end do
              end if
           end if
           work( 1_ilp ) = lwmin
           iwork( 1_ilp ) = liwmin
           return
     end subroutine stdlib_dspgvd




     module subroutine stdlib_sspgvx( itype, jobz, range, uplo, n, ap, bp, vl, vu,il, iu, abstol, m, w, &
     !! SSPGVX computes selected eigenvalues, and optionally, eigenvectors
     !! of a real generalized symmetric-definite eigenproblem, of the form
     !! A*x=(lambda)*B*x,  A*Bx=(lambda)*x,  or B*A*x=(lambda)*x.  Here A
     !! and B are assumed to be symmetric, stored in packed storage, and B
     !! is also positive definite.  Eigenvalues and eigenvectors can be
     !! selected by specifying either a range of values or a range of indices
     !! for the desired eigenvalues.
               z, ldz, work, iwork,ifail, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, range, uplo
           integer(ilp), intent(in) :: il, itype, iu, ldz, n
           integer(ilp), intent(out) :: info, m
           real(sp), intent(in) :: abstol, vl, vu
           ! Array Arguments 
           integer(ilp), intent(out) :: ifail(*), iwork(*)
           real(sp), intent(inout) :: ap(*), bp(*)
           real(sp), intent(out) :: w(*), work(*), z(ldz,*)
       ! =====================================================================
           ! Local Scalars 
           logical(lk) :: alleig, indeig, upper, valeig, wantz
           character :: trans
           integer(ilp) :: j
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           upper = stdlib_lsame( uplo, 'U' )
           wantz = stdlib_lsame( jobz, 'V' )
           alleig = stdlib_lsame( range, 'A' )
           valeig = stdlib_lsame( range, 'V' )
           indeig = stdlib_lsame( range, 'I' )
           info = 0_ilp
           if( itype<1_ilp .or. itype>3_ilp ) then
              info = -1_ilp
           else if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -2_ilp
           else if( .not.( alleig .or. valeig .or. indeig ) ) then
              info = -3_ilp
           else if( .not.( upper .or. stdlib_lsame( uplo, 'L' ) ) ) then
              info = -4_ilp
           else if( n<0_ilp ) then
              info = -5_ilp
           else
              if( valeig ) then
                 if( n>0_ilp .and. vu<=vl ) then
                    info = -9_ilp
                 end if
              else if( indeig ) then
                 if( il<1_ilp ) then
                    info = -10_ilp
                 else if( iu<min( n, il ) .or. iu>n ) then
                    info = -11_ilp
                 end if
              end if
           end if
           if( info==0_ilp ) then
              if( ldz<1_ilp .or. ( wantz .and. ldz<n ) ) then
                 info = -16_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SSPGVX', -info )
              return
           end if
           ! quick return if possible
           m = 0_ilp
           if( n==0 )return
           ! form a cholesky factorization of b.
           call stdlib_spptrf( uplo, n, bp, info )
           if( info/=0_ilp ) then
              info = n + info
              return
           end if
           ! transform problem to standard eigenvalue problem and solve.
           call stdlib_sspgst( itype, uplo, n, ap, bp, info )
           call stdlib_sspevx( jobz, range, uplo, n, ap, vl, vu, il, iu, abstol, m,w, z, ldz, &
                     work, iwork, ifail, info )
           if( wantz ) then
              ! backtransform eigenvectors to the original problem.
              if( info>0_ilp )m = info - 1_ilp
              if( itype==1_ilp .or. itype==2_ilp ) then
                 ! for a*x=(lambda)*b*x and a*b*x=(lambda)*x;
                 ! backtransform eigenvectors: x = inv(l)**t*y or inv(u)*y
                 if( upper ) then
                    trans = 'N'
                 else
                    trans = 'T'
                 end if
                 do j = 1, m
                    call stdlib_stpsv( uplo, trans, 'NON-UNIT', n, bp, z( 1_ilp, j ),1_ilp )
                 end do
              else if( itype==3_ilp ) then
                 ! for b*a*x=(lambda)*x;
                 ! backtransform eigenvectors: x = l*y or u**t*y
                 if( upper ) then
                    trans = 'T'
                 else
                    trans = 'N'
                 end if
                 do j = 1, m
                    call stdlib_stpmv( uplo, trans, 'NON-UNIT', n, bp, z( 1_ilp, j ),1_ilp )
                 end do
              end if
           end if
           return
     end subroutine stdlib_sspgvx

     module subroutine stdlib_dspgvx( itype, jobz, range, uplo, n, ap, bp, vl, vu,il, iu, abstol, m, w, &
     !! DSPGVX computes selected eigenvalues, and optionally, eigenvectors
     !! of a real generalized symmetric-definite eigenproblem, of the form
     !! A*x=(lambda)*B*x,  A*Bx=(lambda)*x,  or B*A*x=(lambda)*x.  Here A
     !! and B are assumed to be symmetric, stored in packed storage, and B
     !! is also positive definite.  Eigenvalues and eigenvectors can be
     !! selected by specifying either a range of values or a range of indices
     !! for the desired eigenvalues.
               z, ldz, work, iwork,ifail, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, range, uplo
           integer(ilp), intent(in) :: il, itype, iu, ldz, n
           integer(ilp), intent(out) :: info, m
           real(dp), intent(in) :: abstol, vl, vu
           ! Array Arguments 
           integer(ilp), intent(out) :: ifail(*), iwork(*)
           real(dp), intent(inout) :: ap(*), bp(*)
           real(dp), intent(out) :: w(*), work(*), z(ldz,*)
       ! =====================================================================
           ! Local Scalars 
           logical(lk) :: alleig, indeig, upper, valeig, wantz
           character :: trans
           integer(ilp) :: j
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           upper = stdlib_lsame( uplo, 'U' )
           wantz = stdlib_lsame( jobz, 'V' )
           alleig = stdlib_lsame( range, 'A' )
           valeig = stdlib_lsame( range, 'V' )
           indeig = stdlib_lsame( range, 'I' )
           info = 0_ilp
           if( itype<1_ilp .or. itype>3_ilp ) then
              info = -1_ilp
           else if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -2_ilp
           else if( .not.( alleig .or. valeig .or. indeig ) ) then
              info = -3_ilp
           else if( .not.( upper .or. stdlib_lsame( uplo, 'L' ) ) ) then
              info = -4_ilp
           else if( n<0_ilp ) then
              info = -5_ilp
           else
              if( valeig ) then
                 if( n>0_ilp .and. vu<=vl ) then
                    info = -9_ilp
                 end if
              else if( indeig ) then
                 if( il<1_ilp ) then
                    info = -10_ilp
                 else if( iu<min( n, il ) .or. iu>n ) then
                    info = -11_ilp
                 end if
              end if
           end if
           if( info==0_ilp ) then
              if( ldz<1_ilp .or. ( wantz .and. ldz<n ) ) then
                 info = -16_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DSPGVX', -info )
              return
           end if
           ! quick return if possible
           m = 0_ilp
           if( n==0 )return
           ! form a cholesky factorization of b.
           call stdlib_dpptrf( uplo, n, bp, info )
           if( info/=0_ilp ) then
              info = n + info
              return
           end if
           ! transform problem to standard eigenvalue problem and solve.
           call stdlib_dspgst( itype, uplo, n, ap, bp, info )
           call stdlib_dspevx( jobz, range, uplo, n, ap, vl, vu, il, iu, abstol, m,w, z, ldz, &
                     work, iwork, ifail, info )
           if( wantz ) then
              ! backtransform eigenvectors to the original problem.
              if( info>0_ilp )m = info - 1_ilp
              if( itype==1_ilp .or. itype==2_ilp ) then
                 ! for a*x=(lambda)*b*x and a*b*x=(lambda)*x;
                 ! backtransform eigenvectors: x = inv(l)**t*y or inv(u)*y
                 if( upper ) then
                    trans = 'N'
                 else
                    trans = 'T'
                 end if
                 do j = 1, m
                    call stdlib_dtpsv( uplo, trans, 'NON-UNIT', n, bp, z( 1_ilp, j ),1_ilp )
                 end do
              else if( itype==3_ilp ) then
                 ! for b*a*x=(lambda)*x;
                 ! backtransform eigenvectors: x = l*y or u**t*y
                 if( upper ) then
                    trans = 'T'
                 else
                    trans = 'N'
                 end if
                 do j = 1, m
                    call stdlib_dtpmv( uplo, trans, 'NON-UNIT', n, bp, z( 1_ilp, j ),1_ilp )
                 end do
              end if
           end if
           return
     end subroutine stdlib_dspgvx




     pure module subroutine stdlib_ssbgv( jobz, uplo, n, ka, kb, ab, ldab, bb, ldbb, w, z,ldz, work, &
     !! SSBGV computes all the eigenvalues, and optionally, the eigenvectors
     !! of a real generalized symmetric-definite banded eigenproblem, of
     !! the form A*x=(lambda)*B*x. Here A and B are assumed to be symmetric
     !! and banded, and B is also positive definite.
               info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ka, kb, ldab, ldbb, ldz, n
           ! Array Arguments 
           real(sp), intent(inout) :: ab(ldab,*), bb(ldbb,*)
           real(sp), intent(out) :: w(*), work(*), z(ldz,*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: upper, wantz
           character :: vect
           integer(ilp) :: iinfo, inde, indwrk
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           upper = stdlib_lsame( uplo, 'U' )
           info = 0_ilp
           if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -1_ilp
           else if( .not.( upper .or. stdlib_lsame( uplo, 'L' ) ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( ka<0_ilp ) then
              info = -4_ilp
           else if( kb<0_ilp .or. kb>ka ) then
              info = -5_ilp
           else if( ldab<ka+1 ) then
              info = -7_ilp
           else if( ldbb<kb+1 ) then
              info = -9_ilp
           else if( ldz<1_ilp .or. ( wantz .and. ldz<n ) ) then
              info = -12_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SSBGV ', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! form a split cholesky factorization of b.
           call stdlib_spbstf( uplo, n, kb, bb, ldbb, info )
           if( info/=0_ilp ) then
              info = n + info
              return
           end if
           ! transform problem to standard eigenvalue problem.
           inde = 1_ilp
           indwrk = inde + n
           call stdlib_ssbgst( jobz, uplo, n, ka, kb, ab, ldab, bb, ldbb, z, ldz,work( indwrk ), &
                     iinfo )
           ! reduce to tridiagonal form.
           if( wantz ) then
              vect = 'U'
           else
              vect = 'N'
           end if
           call stdlib_ssbtrd( vect, uplo, n, ka, ab, ldab, w, work( inde ), z, ldz,work( indwrk )&
                     , iinfo )
           ! for eigenvalues only, call stdlib_ssterf.  for eigenvectors, call stdlib_ssteqr.
           if( .not.wantz ) then
              call stdlib_ssterf( n, w, work( inde ), info )
           else
              call stdlib_ssteqr( jobz, n, w, work( inde ), z, ldz, work( indwrk ),info )
           end if
           return
     end subroutine stdlib_ssbgv

     pure module subroutine stdlib_dsbgv( jobz, uplo, n, ka, kb, ab, ldab, bb, ldbb, w, z,ldz, work, &
     !! DSBGV computes all the eigenvalues, and optionally, the eigenvectors
     !! of a real generalized symmetric-definite banded eigenproblem, of
     !! the form A*x=(lambda)*B*x. Here A and B are assumed to be symmetric
     !! and banded, and B is also positive definite.
               info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ka, kb, ldab, ldbb, ldz, n
           ! Array Arguments 
           real(dp), intent(inout) :: ab(ldab,*), bb(ldbb,*)
           real(dp), intent(out) :: w(*), work(*), z(ldz,*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: upper, wantz
           character :: vect
           integer(ilp) :: iinfo, inde, indwrk
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           upper = stdlib_lsame( uplo, 'U' )
           info = 0_ilp
           if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -1_ilp
           else if( .not.( upper .or. stdlib_lsame( uplo, 'L' ) ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( ka<0_ilp ) then
              info = -4_ilp
           else if( kb<0_ilp .or. kb>ka ) then
              info = -5_ilp
           else if( ldab<ka+1 ) then
              info = -7_ilp
           else if( ldbb<kb+1 ) then
              info = -9_ilp
           else if( ldz<1_ilp .or. ( wantz .and. ldz<n ) ) then
              info = -12_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DSBGV ', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! form a split cholesky factorization of b.
           call stdlib_dpbstf( uplo, n, kb, bb, ldbb, info )
           if( info/=0_ilp ) then
              info = n + info
              return
           end if
           ! transform problem to standard eigenvalue problem.
           inde = 1_ilp
           indwrk = inde + n
           call stdlib_dsbgst( jobz, uplo, n, ka, kb, ab, ldab, bb, ldbb, z, ldz,work( indwrk ), &
                     iinfo )
           ! reduce to tridiagonal form.
           if( wantz ) then
              vect = 'U'
           else
              vect = 'N'
           end if
           call stdlib_dsbtrd( vect, uplo, n, ka, ab, ldab, w, work( inde ), z, ldz,work( indwrk )&
                     , iinfo )
           ! for eigenvalues only, call stdlib_dsterf.  for eigenvectors, call stdlib_ssteqr.
           if( .not.wantz ) then
              call stdlib_dsterf( n, w, work( inde ), info )
           else
              call stdlib_dsteqr( jobz, n, w, work( inde ), z, ldz, work( indwrk ),info )
           end if
           return
     end subroutine stdlib_dsbgv




     pure module subroutine stdlib_ssbgvd( jobz, uplo, n, ka, kb, ab, ldab, bb, ldbb, w,z, ldz, work, &
     !! SSBGVD computes all the eigenvalues, and optionally, the eigenvectors
     !! of a real generalized symmetric-definite banded eigenproblem, of the
     !! form A*x=(lambda)*B*x.  Here A and B are assumed to be symmetric and
     !! banded, and B is also positive definite.  If eigenvectors are
     !! desired, it uses a divide and conquer algorithm.
     !! The divide and conquer algorithm makes very mild assumptions about
     !! floating point arithmetic. It will work on machines with a guard
     !! digit in add/subtract, or on those binary machines without guard
     !! digits which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or
     !! Cray-2. It could conceivably fail on hexadecimal or decimal machines
     !! without guard digits, but we know of none.
               lwork, iwork, liwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ka, kb, ldab, ldbb, ldz, liwork, lwork, n
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(inout) :: ab(ldab,*), bb(ldbb,*)
           real(sp), intent(out) :: w(*), work(*), z(ldz,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery, upper, wantz
           character :: vect
           integer(ilp) :: iinfo, inde, indwk2, indwrk, liwmin, llwrk2, lwmin
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           upper = stdlib_lsame( uplo, 'U' )
           lquery = ( lwork==-1_ilp .or. liwork==-1_ilp )
           info = 0_ilp
           if( n<=1_ilp ) then
              liwmin = 1_ilp
              lwmin = 1_ilp
           else if( wantz ) then
              liwmin = 3_ilp + 5_ilp*n
              lwmin = 1_ilp + 5_ilp*n + 2_ilp*n**2_ilp
           else
              liwmin = 1_ilp
              lwmin = 2_ilp*n
           end if
           if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -1_ilp
           else if( .not.( upper .or. stdlib_lsame( uplo, 'L' ) ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( ka<0_ilp ) then
              info = -4_ilp
           else if( kb<0_ilp .or. kb>ka ) then
              info = -5_ilp
           else if( ldab<ka+1 ) then
              info = -7_ilp
           else if( ldbb<kb+1 ) then
              info = -9_ilp
           else if( ldz<1_ilp .or. ( wantz .and. ldz<n ) ) then
              info = -12_ilp
           end if
           if( info==0_ilp ) then
              work( 1_ilp ) = lwmin
              iwork( 1_ilp ) = liwmin
              if( lwork<lwmin .and. .not.lquery ) then
                 info = -14_ilp
              else if( liwork<liwmin .and. .not.lquery ) then
                 info = -16_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SSBGVD', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! form a split cholesky factorization of b.
           call stdlib_spbstf( uplo, n, kb, bb, ldbb, info )
           if( info/=0_ilp ) then
              info = n + info
              return
           end if
           ! transform problem to standard eigenvalue problem.
           inde = 1_ilp
           indwrk = inde + n
           indwk2 = indwrk + n*n
           llwrk2 = lwork - indwk2 + 1_ilp
           call stdlib_ssbgst( jobz, uplo, n, ka, kb, ab, ldab, bb, ldbb, z, ldz,work, iinfo )
                     
           ! reduce to tridiagonal form.
           if( wantz ) then
              vect = 'U'
           else
              vect = 'N'
           end if
           call stdlib_ssbtrd( vect, uplo, n, ka, ab, ldab, w, work( inde ), z, ldz,work( indwrk )&
                     , iinfo )
           ! for eigenvalues only, call stdlib_ssterf. for eigenvectors, call stdlib_sstedc.
           if( .not.wantz ) then
              call stdlib_ssterf( n, w, work( inde ), info )
           else
              call stdlib_sstedc( 'I', n, w, work( inde ), work( indwrk ), n,work( indwk2 ), &
                        llwrk2, iwork, liwork, info )
              call stdlib_sgemm( 'N', 'N', n, n, n, one, z, ldz, work( indwrk ), n,zero, work( &
                        indwk2 ), n )
              call stdlib_slacpy( 'A', n, n, work( indwk2 ), n, z, ldz )
           end if
           work( 1_ilp ) = lwmin
           iwork( 1_ilp ) = liwmin
           return
     end subroutine stdlib_ssbgvd

     pure module subroutine stdlib_dsbgvd( jobz, uplo, n, ka, kb, ab, ldab, bb, ldbb, w,z, ldz, work, &
     !! DSBGVD computes all the eigenvalues, and optionally, the eigenvectors
     !! of a real generalized symmetric-definite banded eigenproblem, of the
     !! form A*x=(lambda)*B*x.  Here A and B are assumed to be symmetric and
     !! banded, and B is also positive definite.  If eigenvectors are
     !! desired, it uses a divide and conquer algorithm.
     !! The divide and conquer algorithm makes very mild assumptions about
     !! floating point arithmetic. It will work on machines with a guard
     !! digit in add/subtract, or on those binary machines without guard
     !! digits which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or
     !! Cray-2. It could conceivably fail on hexadecimal or decimal machines
     !! without guard digits, but we know of none.
               lwork, iwork, liwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ka, kb, ldab, ldbb, ldz, liwork, lwork, n
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(inout) :: ab(ldab,*), bb(ldbb,*)
           real(dp), intent(out) :: w(*), work(*), z(ldz,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery, upper, wantz
           character :: vect
           integer(ilp) :: iinfo, inde, indwk2, indwrk, liwmin, llwrk2, lwmin
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           upper = stdlib_lsame( uplo, 'U' )
           lquery = ( lwork==-1_ilp .or. liwork==-1_ilp )
           info = 0_ilp
           if( n<=1_ilp ) then
              liwmin = 1_ilp
              lwmin = 1_ilp
           else if( wantz ) then
              liwmin = 3_ilp + 5_ilp*n
              lwmin = 1_ilp + 5_ilp*n + 2_ilp*n**2_ilp
           else
              liwmin = 1_ilp
              lwmin = 2_ilp*n
           end if
           if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -1_ilp
           else if( .not.( upper .or. stdlib_lsame( uplo, 'L' ) ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( ka<0_ilp ) then
              info = -4_ilp
           else if( kb<0_ilp .or. kb>ka ) then
              info = -5_ilp
           else if( ldab<ka+1 ) then
              info = -7_ilp
           else if( ldbb<kb+1 ) then
              info = -9_ilp
           else if( ldz<1_ilp .or. ( wantz .and. ldz<n ) ) then
              info = -12_ilp
           end if
           if( info==0_ilp ) then
              work( 1_ilp ) = lwmin
              iwork( 1_ilp ) = liwmin
              if( lwork<lwmin .and. .not.lquery ) then
                 info = -14_ilp
              else if( liwork<liwmin .and. .not.lquery ) then
                 info = -16_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DSBGVD', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! form a split cholesky factorization of b.
           call stdlib_dpbstf( uplo, n, kb, bb, ldbb, info )
           if( info/=0_ilp ) then
              info = n + info
              return
           end if
           ! transform problem to standard eigenvalue problem.
           inde = 1_ilp
           indwrk = inde + n
           indwk2 = indwrk + n*n
           llwrk2 = lwork - indwk2 + 1_ilp
           call stdlib_dsbgst( jobz, uplo, n, ka, kb, ab, ldab, bb, ldbb, z, ldz,work, iinfo )
                     
           ! reduce to tridiagonal form.
           if( wantz ) then
              vect = 'U'
           else
              vect = 'N'
           end if
           call stdlib_dsbtrd( vect, uplo, n, ka, ab, ldab, w, work( inde ), z, ldz,work( indwrk )&
                     , iinfo )
           ! for eigenvalues only, call stdlib_dsterf. for eigenvectors, call stdlib_sstedc.
           if( .not.wantz ) then
              call stdlib_dsterf( n, w, work( inde ), info )
           else
              call stdlib_dstedc( 'I', n, w, work( inde ), work( indwrk ), n,work( indwk2 ), &
                        llwrk2, iwork, liwork, info )
              call stdlib_dgemm( 'N', 'N', n, n, n, one, z, ldz, work( indwrk ), n,zero, work( &
                        indwk2 ), n )
              call stdlib_dlacpy( 'A', n, n, work( indwk2 ), n, z, ldz )
           end if
           work( 1_ilp ) = lwmin
           iwork( 1_ilp ) = liwmin
           return
     end subroutine stdlib_dsbgvd




     pure module subroutine stdlib_ssbgvx( jobz, range, uplo, n, ka, kb, ab, ldab, bb,ldbb, q, ldq, vl, &
     !! SSBGVX computes selected eigenvalues, and optionally, eigenvectors
     !! of a real generalized symmetric-definite banded eigenproblem, of
     !! the form A*x=(lambda)*B*x.  Here A and B are assumed to be symmetric
     !! and banded, and B is also positive definite.  Eigenvalues and
     !! eigenvectors can be selected by specifying either all eigenvalues,
     !! a range of values or a range of indices for the desired eigenvalues.
               vu, il, iu, abstol, m, w, z,ldz, work, iwork, ifail, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, range, uplo
           integer(ilp), intent(in) :: il, iu, ka, kb, ldab, ldbb, ldq, ldz, n
           integer(ilp), intent(out) :: info, m
           real(sp), intent(in) :: abstol, vl, vu
           ! Array Arguments 
           integer(ilp), intent(out) :: ifail(*), iwork(*)
           real(sp), intent(inout) :: ab(ldab,*), bb(ldbb,*)
           real(sp), intent(out) :: q(ldq,*), w(*), work(*), z(ldz,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: alleig, indeig, test, upper, valeig, wantz
           character :: order, vect
           integer(ilp) :: i, iinfo, indd, inde, indee, indibl, indisp, indiwo, indwrk, itmp1, j, &
                     jj, nsplit
           real(sp) :: tmp1
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           upper = stdlib_lsame( uplo, 'U' )
           alleig = stdlib_lsame( range, 'A' )
           valeig = stdlib_lsame( range, 'V' )
           indeig = stdlib_lsame( range, 'I' )
           info = 0_ilp
           if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -1_ilp
           else if( .not.( alleig .or. valeig .or. indeig ) ) then
              info = -2_ilp
           else if( .not.( upper .or. stdlib_lsame( uplo, 'L' ) ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( ka<0_ilp ) then
              info = -5_ilp
           else if( kb<0_ilp .or. kb>ka ) then
              info = -6_ilp
           else if( ldab<ka+1 ) then
              info = -8_ilp
           else if( ldbb<kb+1 ) then
              info = -10_ilp
           else if( ldq<1_ilp .or. ( wantz .and. ldq<n ) ) then
              info = -12_ilp
           else
              if( valeig ) then
                 if( n>0_ilp .and. vu<=vl )info = -14_ilp
              else if( indeig ) then
                 if( il<1_ilp .or. il>max( 1_ilp, n ) ) then
                    info = -15_ilp
                 else if ( iu<min( n, il ) .or. iu>n ) then
                    info = -16_ilp
                 end if
              end if
           end if
           if( info==0_ilp) then
              if( ldz<1_ilp .or. ( wantz .and. ldz<n ) ) then
                 info = -21_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SSBGVX', -info )
              return
           end if
           ! quick return if possible
           m = 0_ilp
           if( n==0 )return
           ! form a split cholesky factorization of b.
           call stdlib_spbstf( uplo, n, kb, bb, ldbb, info )
           if( info/=0_ilp ) then
              info = n + info
              return
           end if
           ! transform problem to standard eigenvalue problem.
           call stdlib_ssbgst( jobz, uplo, n, ka, kb, ab, ldab, bb, ldbb, q, ldq,work, iinfo )
                     
           ! reduce symmetric band matrix to tridiagonal form.
           indd = 1_ilp
           inde = indd + n
           indwrk = inde + n
           if( wantz ) then
              vect = 'U'
           else
              vect = 'N'
           end if
           call stdlib_ssbtrd( vect, uplo, n, ka, ab, ldab, work( indd ),work( inde ), q, ldq, &
                     work( indwrk ), iinfo )
           ! if all eigenvalues are desired and abstol is less than or equal
           ! to zero, then call stdlib_ssterf or stdlib_ssteqr.  if this fails for some
           ! eigenvalue, then try stdlib_sstebz.
           test = .false.
           if( indeig ) then
              if( il==1_ilp .and. iu==n ) then
                 test = .true.
              end if
           end if
           if( ( alleig .or. test ) .and. ( abstol<=zero ) ) then
              call stdlib_scopy( n, work( indd ), 1_ilp, w, 1_ilp )
              indee = indwrk + 2_ilp*n
              call stdlib_scopy( n-1, work( inde ), 1_ilp, work( indee ), 1_ilp )
              if( .not.wantz ) then
                 call stdlib_ssterf( n, w, work( indee ), info )
              else
                 call stdlib_slacpy( 'A', n, n, q, ldq, z, ldz )
                 call stdlib_ssteqr( jobz, n, w, work( indee ), z, ldz,work( indwrk ), info )
                           
                 if( info==0_ilp ) then
                    do i = 1, n
                       ifail( i ) = 0_ilp
                    end do
                 end if
              end if
              if( info==0_ilp ) then
                 m = n
                 go to 30
              end if
              info = 0_ilp
           end if
           ! otherwise, call stdlib_sstebz and, if eigenvectors are desired,
           ! call stdlib_sstein.
           if( wantz ) then
              order = 'B'
           else
              order = 'E'
           end if
           indibl = 1_ilp
           indisp = indibl + n
           indiwo = indisp + n
           call stdlib_sstebz( range, order, n, vl, vu, il, iu, abstol,work( indd ), work( inde ),&
            m, nsplit, w,iwork( indibl ), iwork( indisp ), work( indwrk ),iwork( indiwo ), info )
                      
           if( wantz ) then
              call stdlib_sstein( n, work( indd ), work( inde ), m, w,iwork( indibl ), iwork( &
                        indisp ), z, ldz,work( indwrk ), iwork( indiwo ), ifail, info )
              ! apply transformation matrix used in reduction to tridiagonal
              ! form to eigenvectors returned by stdlib_sstein.
              do j = 1, m
                 call stdlib_scopy( n, z( 1_ilp, j ), 1_ilp, work( 1_ilp ), 1_ilp )
                 call stdlib_sgemv( 'N', n, n, one, q, ldq, work, 1_ilp, zero,z( 1_ilp, j ), 1_ilp )
              end do
           end if
           30 continue
           ! if eigenvalues are not in order, then sort them, along with
           ! eigenvectors.
           if( wantz ) then
              do j = 1, m - 1
                 i = 0_ilp
                 tmp1 = w( j )
                 do jj = j + 1, m
                    if( w( jj )<tmp1 ) then
                       i = jj
                       tmp1 = w( jj )
                    end if
                 end do
                 if( i/=0_ilp ) then
                    itmp1 = iwork( indibl+i-1 )
                    w( i ) = w( j )
                    iwork( indibl+i-1 ) = iwork( indibl+j-1 )
                    w( j ) = tmp1
                    iwork( indibl+j-1 ) = itmp1
                    call stdlib_sswap( n, z( 1_ilp, i ), 1_ilp, z( 1_ilp, j ), 1_ilp )
                    if( info/=0_ilp ) then
                       itmp1 = ifail( i )
                       ifail( i ) = ifail( j )
                       ifail( j ) = itmp1
                    end if
                 end if
              end do
           end if
           return
     end subroutine stdlib_ssbgvx

     pure module subroutine stdlib_dsbgvx( jobz, range, uplo, n, ka, kb, ab, ldab, bb,ldbb, q, ldq, vl, &
     !! DSBGVX computes selected eigenvalues, and optionally, eigenvectors
     !! of a real generalized symmetric-definite banded eigenproblem, of
     !! the form A*x=(lambda)*B*x.  Here A and B are assumed to be symmetric
     !! and banded, and B is also positive definite.  Eigenvalues and
     !! eigenvectors can be selected by specifying either all eigenvalues,
     !! a range of values or a range of indices for the desired eigenvalues.
               vu, il, iu, abstol, m, w, z,ldz, work, iwork, ifail, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, range, uplo
           integer(ilp), intent(in) :: il, iu, ka, kb, ldab, ldbb, ldq, ldz, n
           integer(ilp), intent(out) :: info, m
           real(dp), intent(in) :: abstol, vl, vu
           ! Array Arguments 
           integer(ilp), intent(out) :: ifail(*), iwork(*)
           real(dp), intent(inout) :: ab(ldab,*), bb(ldbb,*)
           real(dp), intent(out) :: q(ldq,*), w(*), work(*), z(ldz,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: alleig, indeig, test, upper, valeig, wantz
           character :: order, vect
           integer(ilp) :: i, iinfo, indd, inde, indee, indibl, indisp, indiwo, indwrk, itmp1, j, &
                     jj, nsplit
           real(dp) :: tmp1
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           upper = stdlib_lsame( uplo, 'U' )
           alleig = stdlib_lsame( range, 'A' )
           valeig = stdlib_lsame( range, 'V' )
           indeig = stdlib_lsame( range, 'I' )
           info = 0_ilp
           if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -1_ilp
           else if( .not.( alleig .or. valeig .or. indeig ) ) then
              info = -2_ilp
           else if( .not.( upper .or. stdlib_lsame( uplo, 'L' ) ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( ka<0_ilp ) then
              info = -5_ilp
           else if( kb<0_ilp .or. kb>ka ) then
              info = -6_ilp
           else if( ldab<ka+1 ) then
              info = -8_ilp
           else if( ldbb<kb+1 ) then
              info = -10_ilp
           else if( ldq<1_ilp .or. ( wantz .and. ldq<n ) ) then
              info = -12_ilp
           else
              if( valeig ) then
                 if( n>0_ilp .and. vu<=vl )info = -14_ilp
              else if( indeig ) then
                 if( il<1_ilp .or. il>max( 1_ilp, n ) ) then
                    info = -15_ilp
                 else if ( iu<min( n, il ) .or. iu>n ) then
                    info = -16_ilp
                 end if
              end if
           end if
           if( info==0_ilp) then
              if( ldz<1_ilp .or. ( wantz .and. ldz<n ) ) then
                 info = -21_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DSBGVX', -info )
              return
           end if
           ! quick return if possible
           m = 0_ilp
           if( n==0 )return
           ! form a split cholesky factorization of b.
           call stdlib_dpbstf( uplo, n, kb, bb, ldbb, info )
           if( info/=0_ilp ) then
              info = n + info
              return
           end if
           ! transform problem to standard eigenvalue problem.
           call stdlib_dsbgst( jobz, uplo, n, ka, kb, ab, ldab, bb, ldbb, q, ldq,work, iinfo )
                     
           ! reduce symmetric band matrix to tridiagonal form.
           indd = 1_ilp
           inde = indd + n
           indwrk = inde + n
           if( wantz ) then
              vect = 'U'
           else
              vect = 'N'
           end if
           call stdlib_dsbtrd( vect, uplo, n, ka, ab, ldab, work( indd ),work( inde ), q, ldq, &
                     work( indwrk ), iinfo )
           ! if all eigenvalues are desired and abstol is less than or equal
           ! to zero, then call stdlib_dsterf or stdlib_ssteqr.  if this fails for some
           ! eigenvalue, then try stdlib_dstebz.
           test = .false.
           if( indeig ) then
              if( il==1_ilp .and. iu==n ) then
                 test = .true.
              end if
           end if
           if( ( alleig .or. test ) .and. ( abstol<=zero ) ) then
              call stdlib_dcopy( n, work( indd ), 1_ilp, w, 1_ilp )
              indee = indwrk + 2_ilp*n
              call stdlib_dcopy( n-1, work( inde ), 1_ilp, work( indee ), 1_ilp )
              if( .not.wantz ) then
                 call stdlib_dsterf( n, w, work( indee ), info )
              else
                 call stdlib_dlacpy( 'A', n, n, q, ldq, z, ldz )
                 call stdlib_dsteqr( jobz, n, w, work( indee ), z, ldz,work( indwrk ), info )
                           
                 if( info==0_ilp ) then
                    do i = 1, n
                       ifail( i ) = 0_ilp
                    end do
                 end if
              end if
              if( info==0_ilp ) then
                 m = n
                 go to 30
              end if
              info = 0_ilp
           end if
           ! otherwise, call stdlib_dstebz and, if eigenvectors are desired,
           ! call stdlib_dstein.
           if( wantz ) then
              order = 'B'
           else
              order = 'E'
           end if
           indibl = 1_ilp
           indisp = indibl + n
           indiwo = indisp + n
           call stdlib_dstebz( range, order, n, vl, vu, il, iu, abstol,work( indd ), work( inde ),&
            m, nsplit, w,iwork( indibl ), iwork( indisp ), work( indwrk ),iwork( indiwo ), info )
                      
           if( wantz ) then
              call stdlib_dstein( n, work( indd ), work( inde ), m, w,iwork( indibl ), iwork( &
                        indisp ), z, ldz,work( indwrk ), iwork( indiwo ), ifail, info )
              ! apply transformation matrix used in reduction to tridiagonal
              ! form to eigenvectors returned by stdlib_dstein.
              do j = 1, m
                 call stdlib_dcopy( n, z( 1_ilp, j ), 1_ilp, work( 1_ilp ), 1_ilp )
                 call stdlib_dgemv( 'N', n, n, one, q, ldq, work, 1_ilp, zero,z( 1_ilp, j ), 1_ilp )
              end do
           end if
           30 continue
           ! if eigenvalues are not in order, then sort them, along with
           ! eigenvectors.
           if( wantz ) then
              do j = 1, m - 1
                 i = 0_ilp
                 tmp1 = w( j )
                 do jj = j + 1, m
                    if( w( jj )<tmp1 ) then
                       i = jj
                       tmp1 = w( jj )
                    end if
                 end do
                 if( i/=0_ilp ) then
                    itmp1 = iwork( indibl+i-1 )
                    w( i ) = w( j )
                    iwork( indibl+i-1 ) = iwork( indibl+j-1 )
                    w( j ) = tmp1
                    iwork( indibl+j-1 ) = itmp1
                    call stdlib_dswap( n, z( 1_ilp, i ), 1_ilp, z( 1_ilp, j ), 1_ilp )
                    if( info/=0_ilp ) then
                       itmp1 = ifail( i )
                       ifail( i ) = ifail( j )
                       ifail( j ) = itmp1
                    end if
                 end if
              end do
           end if
           return
     end subroutine stdlib_dsbgvx




     pure module subroutine stdlib_ssytrd( uplo, n, a, lda, d, e, tau, work, lwork, info )
     !! SSYTRD reduces a real symmetric matrix A to real symmetric
     !! tridiagonal form T by an orthogonal similarity transformation:
     !! Q**T * A * Q = T.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, n
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: d(*), e(*), tau(*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery, upper
           integer(ilp) :: i, iinfo, iws, j, kk, ldwork, lwkopt, nb, nbmin, nx
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           lquery = ( lwork==-1_ilp )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           else if( lwork<1_ilp .and. .not.lquery ) then
              info = -9_ilp
           end if
           if( info==0_ilp ) then
              ! determine the block size.
              nb = stdlib_ilaenv( 1_ilp, 'SSYTRD', uplo, n, -1_ilp, -1_ilp, -1_ilp )
              lwkopt = n*nb
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SSYTRD', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0_ilp ) then
              work( 1_ilp ) = 1_ilp
              return
           end if
           nx = n
           iws = 1_ilp
           if( nb>1_ilp .and. nb<n ) then
              ! determine when to cross over from blocked to unblocked code
              ! (last block is always handled by unblocked code).
              nx = max( nb, stdlib_ilaenv( 3_ilp, 'SSYTRD', uplo, n, -1_ilp, -1_ilp, -1_ilp ) )
              if( nx<n ) then
                 ! determine if workspace is large enough for blocked code.
                 ldwork = n
                 iws = ldwork*nb
                 if( lwork<iws ) then
                    ! not enough workspace to use optimal nb:  determine the
                    ! minimum value of nb, and reduce nb or force use of
                    ! unblocked code by setting nx = n.
                    nb = max( lwork / ldwork, 1_ilp )
                    nbmin = stdlib_ilaenv( 2_ilp, 'SSYTRD', uplo, n, -1_ilp, -1_ilp, -1_ilp )
                    if( nb<nbmin )nx = n
                 end if
              else
                 nx = n
              end if
           else
              nb = 1_ilp
           end if
           if( upper ) then
              ! reduce the upper triangle of a.
              ! columns 1:kk are handled by the unblocked method.
              kk = n - ( ( n-nx+nb-1 ) / nb )*nb
              do i = n - nb + 1, kk + 1, -nb
                 ! reduce columns i:i+nb-1 to tridiagonal form and form the
                 ! matrix w which is needed to update the unreduced part of
                 ! the matrix
                 call stdlib_slatrd( uplo, i+nb-1, nb, a, lda, e, tau, work,ldwork )
                 ! update the unreduced submatrix a(1:i-1,1:i-1), using an
                 ! update of the form:  a := a - v*w**t - w*v**t
                 call stdlib_ssyr2k( uplo, 'NO TRANSPOSE', i-1, nb, -one, a( 1_ilp, i ),lda, work, &
                           ldwork, one, a, lda )
                 ! copy superdiagonal elements back into a, and diagonal
                 ! elements into d
                 do j = i, i + nb - 1
                    a( j-1, j ) = e( j-1 )
                    d( j ) = a( j, j )
                 end do
              end do
              ! use unblocked code to reduce the last or only block
              call stdlib_ssytd2( uplo, kk, a, lda, d, e, tau, iinfo )
           else
              ! reduce the lower triangle of a
              do i = 1, n - nx, nb
                 ! reduce columns i:i+nb-1 to tridiagonal form and form the
                 ! matrix w which is needed to update the unreduced part of
                 ! the matrix
                 call stdlib_slatrd( uplo, n-i+1, nb, a( i, i ), lda, e( i ),tau( i ), work, &
                           ldwork )
                 ! update the unreduced submatrix a(i+ib:n,i+ib:n), using
                 ! an update of the form:  a := a - v*w**t - w*v**t
                 call stdlib_ssyr2k( uplo, 'NO TRANSPOSE', n-i-nb+1, nb, -one,a( i+nb, i ), lda, &
                           work( nb+1 ), ldwork, one,a( i+nb, i+nb ), lda )
                 ! copy subdiagonal elements back into a, and diagonal
                 ! elements into d
                 do j = i, i + nb - 1
                    a( j+1, j ) = e( j )
                    d( j ) = a( j, j )
                 end do
              end do
              ! use unblocked code to reduce the last or only block
              call stdlib_ssytd2( uplo, n-i+1, a( i, i ), lda, d( i ), e( i ),tau( i ), iinfo )
                        
           end if
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_ssytrd

     pure module subroutine stdlib_dsytrd( uplo, n, a, lda, d, e, tau, work, lwork, info )
     !! DSYTRD reduces a real symmetric matrix A to real symmetric
     !! tridiagonal form T by an orthogonal similarity transformation:
     !! Q**T * A * Q = T.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, n
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: d(*), e(*), tau(*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery, upper
           integer(ilp) :: i, iinfo, iws, j, kk, ldwork, lwkopt, nb, nbmin, nx
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           lquery = ( lwork==-1_ilp )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           else if( lwork<1_ilp .and. .not.lquery ) then
              info = -9_ilp
           end if
           if( info==0_ilp ) then
              ! determine the block size.
              nb = stdlib_ilaenv( 1_ilp, 'DSYTRD', uplo, n, -1_ilp, -1_ilp, -1_ilp )
              lwkopt = n*nb
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DSYTRD', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0_ilp ) then
              work( 1_ilp ) = 1_ilp
              return
           end if
           nx = n
           iws = 1_ilp
           if( nb>1_ilp .and. nb<n ) then
              ! determine when to cross over from blocked to unblocked code
              ! (last block is always handled by unblocked code).
              nx = max( nb, stdlib_ilaenv( 3_ilp, 'DSYTRD', uplo, n, -1_ilp, -1_ilp, -1_ilp ) )
              if( nx<n ) then
                 ! determine if workspace is large enough for blocked code.
                 ldwork = n
                 iws = ldwork*nb
                 if( lwork<iws ) then
                    ! not enough workspace to use optimal nb:  determine the
                    ! minimum value of nb, and reduce nb or force use of
                    ! unblocked code by setting nx = n.
                    nb = max( lwork / ldwork, 1_ilp )
                    nbmin = stdlib_ilaenv( 2_ilp, 'DSYTRD', uplo, n, -1_ilp, -1_ilp, -1_ilp )
                    if( nb<nbmin )nx = n
                 end if
              else
                 nx = n
              end if
           else
              nb = 1_ilp
           end if
           if( upper ) then
              ! reduce the upper triangle of a.
              ! columns 1:kk are handled by the unblocked method.
              kk = n - ( ( n-nx+nb-1 ) / nb )*nb
              do i = n - nb + 1, kk + 1, -nb
                 ! reduce columns i:i+nb-1 to tridiagonal form and form the
                 ! matrix w which is needed to update the unreduced part of
                 ! the matrix
                 call stdlib_dlatrd( uplo, i+nb-1, nb, a, lda, e, tau, work,ldwork )
                 ! update the unreduced submatrix a(1:i-1,1:i-1), using an
                 ! update of the form:  a := a - v*w**t - w*v**t
                 call stdlib_dsyr2k( uplo, 'NO TRANSPOSE', i-1, nb, -one, a( 1_ilp, i ),lda, work, &
                           ldwork, one, a, lda )
                 ! copy superdiagonal elements back into a, and diagonal
                 ! elements into d
                 do j = i, i + nb - 1
                    a( j-1, j ) = e( j-1 )
                    d( j ) = a( j, j )
                 end do
              end do
              ! use unblocked code to reduce the last or only block
              call stdlib_dsytd2( uplo, kk, a, lda, d, e, tau, iinfo )
           else
              ! reduce the lower triangle of a
              do i = 1, n - nx, nb
                 ! reduce columns i:i+nb-1 to tridiagonal form and form the
                 ! matrix w which is needed to update the unreduced part of
                 ! the matrix
                 call stdlib_dlatrd( uplo, n-i+1, nb, a( i, i ), lda, e( i ),tau( i ), work, &
                           ldwork )
                 ! update the unreduced submatrix a(i+ib:n,i+ib:n), using
                 ! an update of the form:  a := a - v*w**t - w*v**t
                 call stdlib_dsyr2k( uplo, 'NO TRANSPOSE', n-i-nb+1, nb, -one,a( i+nb, i ), lda, &
                           work( nb+1 ), ldwork, one,a( i+nb, i+nb ), lda )
                 ! copy subdiagonal elements back into a, and diagonal
                 ! elements into d
                 do j = i, i + nb - 1
                    a( j+1, j ) = e( j )
                    d( j ) = a( j, j )
                 end do
              end do
              ! use unblocked code to reduce the last or only block
              call stdlib_dsytd2( uplo, n-i+1, a( i, i ), lda, d( i ), e( i ),tau( i ), iinfo )
                        
           end if
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_dsytrd




     pure module subroutine stdlib_ssytd2( uplo, n, a, lda, d, e, tau, info )
     !! SSYTD2 reduces a real symmetric matrix A to symmetric tridiagonal
     !! form T by an orthogonal similarity transformation: Q**T * A * Q = T.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: d(*), e(*), tau(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: i
           real(sp) :: alpha, taui
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SSYTD2', -info )
              return
           end if
           ! quick return if possible
           if( n<=0 )return
           if( upper ) then
              ! reduce the upper triangle of a
              do i = n - 1, 1, -1
                 ! generate elementary reflector h(i) = i - tau * v * v**t
                 ! to annihilate a(1:i-1,i+1)
                 call stdlib_slarfg( i, a( i, i+1 ), a( 1_ilp, i+1 ), 1_ilp, taui )
                 e( i ) = a( i, i+1 )
                 if( taui/=zero ) then
                    ! apply h(i) from both sides to a(1:i,1:i)
                    a( i, i+1 ) = one
                    ! compute  x := tau * a * v  storing x in tau(1:i)
                    call stdlib_ssymv( uplo, i, taui, a, lda, a( 1_ilp, i+1 ), 1_ilp, zero,tau, 1_ilp )
                              
                    ! compute  w := x - 1/2 * tau * (x**t * v) * v
                    alpha = -half*taui*stdlib_sdot( i, tau, 1_ilp, a( 1_ilp, i+1 ), 1_ilp )
                    call stdlib_saxpy( i, alpha, a( 1_ilp, i+1 ), 1_ilp, tau, 1_ilp )
                    ! apply the transformation as a rank-2 update:
                       ! a := a - v * w**t - w * v**t
                    call stdlib_ssyr2( uplo, i, -one, a( 1_ilp, i+1 ), 1_ilp, tau, 1_ilp, a,lda )
                    a( i, i+1 ) = e( i )
                 end if
                 d( i+1 ) = a( i+1, i+1 )
                 tau( i ) = taui
              end do
              d( 1_ilp ) = a( 1_ilp, 1_ilp )
           else
              ! reduce the lower triangle of a
              do i = 1, n - 1
                 ! generate elementary reflector h(i) = i - tau * v * v**t
                 ! to annihilate a(i+2:n,i)
                 call stdlib_slarfg( n-i, a( i+1, i ), a( min( i+2, n ), i ), 1_ilp,taui )
                 e( i ) = a( i+1, i )
                 if( taui/=zero ) then
                    ! apply h(i) from both sides to a(i+1:n,i+1:n)
                    a( i+1, i ) = one
                    ! compute  x := tau * a * v  storing y in tau(i:n-1)
                    call stdlib_ssymv( uplo, n-i, taui, a( i+1, i+1 ), lda,a( i+1, i ), 1_ilp, zero, &
                              tau( i ), 1_ilp )
                    ! compute  w := x - 1/2 * tau * (x**t * v) * v
                    alpha = -half*taui*stdlib_sdot( n-i, tau( i ), 1_ilp, a( i+1, i ),1_ilp )
                    call stdlib_saxpy( n-i, alpha, a( i+1, i ), 1_ilp, tau( i ), 1_ilp )
                    ! apply the transformation as a rank-2 update:
                       ! a := a - v * w**t - w * v**t
                    call stdlib_ssyr2( uplo, n-i, -one, a( i+1, i ), 1_ilp, tau( i ), 1_ilp,a( i+1, i+1 ),&
                               lda )
                    a( i+1, i ) = e( i )
                 end if
                 d( i ) = a( i, i )
                 tau( i ) = taui
              end do
              d( n ) = a( n, n )
           end if
           return
     end subroutine stdlib_ssytd2

     pure module subroutine stdlib_dsytd2( uplo, n, a, lda, d, e, tau, info )
     !! DSYTD2 reduces a real symmetric matrix A to symmetric tridiagonal
     !! form T by an orthogonal similarity transformation: Q**T * A * Q = T.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: d(*), e(*), tau(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: i
           real(dp) :: alpha, taui
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DSYTD2', -info )
              return
           end if
           ! quick return if possible
           if( n<=0 )return
           if( upper ) then
              ! reduce the upper triangle of a
              do i = n - 1, 1, -1
                 ! generate elementary reflector h(i) = i - tau * v * v**t
                 ! to annihilate a(1:i-1,i+1)
                 call stdlib_dlarfg( i, a( i, i+1 ), a( 1_ilp, i+1 ), 1_ilp, taui )
                 e( i ) = a( i, i+1 )
                 if( taui/=zero ) then
                    ! apply h(i) from both sides to a(1:i,1:i)
                    a( i, i+1 ) = one
                    ! compute  x := tau * a * v  storing x in tau(1:i)
                    call stdlib_dsymv( uplo, i, taui, a, lda, a( 1_ilp, i+1 ), 1_ilp, zero,tau, 1_ilp )
                              
                    ! compute  w := x - 1/2 * tau * (x**t * v) * v
                    alpha = -half*taui*stdlib_ddot( i, tau, 1_ilp, a( 1_ilp, i+1 ), 1_ilp )
                    call stdlib_daxpy( i, alpha, a( 1_ilp, i+1 ), 1_ilp, tau, 1_ilp )
                    ! apply the transformation as a rank-2 update:
                       ! a := a - v * w**t - w * v**t
                    call stdlib_dsyr2( uplo, i, -one, a( 1_ilp, i+1 ), 1_ilp, tau, 1_ilp, a,lda )
                    a( i, i+1 ) = e( i )
                 end if
                 d( i+1 ) = a( i+1, i+1 )
                 tau( i ) = taui
              end do
              d( 1_ilp ) = a( 1_ilp, 1_ilp )
           else
              ! reduce the lower triangle of a
              do i = 1, n - 1
                 ! generate elementary reflector h(i) = i - tau * v * v**t
                 ! to annihilate a(i+2:n,i)
                 call stdlib_dlarfg( n-i, a( i+1, i ), a( min( i+2, n ), i ), 1_ilp,taui )
                 e( i ) = a( i+1, i )
                 if( taui/=zero ) then
                    ! apply h(i) from both sides to a(i+1:n,i+1:n)
                    a( i+1, i ) = one
                    ! compute  x := tau * a * v  storing y in tau(i:n-1)
                    call stdlib_dsymv( uplo, n-i, taui, a( i+1, i+1 ), lda,a( i+1, i ), 1_ilp, zero, &
                              tau( i ), 1_ilp )
                    ! compute  w := x - 1/2 * tau * (x**t * v) * v
                    alpha = -half*taui*stdlib_ddot( n-i, tau( i ), 1_ilp, a( i+1, i ),1_ilp )
                    call stdlib_daxpy( n-i, alpha, a( i+1, i ), 1_ilp, tau( i ), 1_ilp )
                    ! apply the transformation as a rank-2 update:
                       ! a := a - v * w**t - w * v**t
                    call stdlib_dsyr2( uplo, n-i, -one, a( i+1, i ), 1_ilp, tau( i ), 1_ilp,a( i+1, i+1 ),&
                               lda )
                    a( i+1, i ) = e( i )
                 end if
                 d( i ) = a( i, i )
                 tau( i ) = taui
              end do
              d( n ) = a( n, n )
           end if
           return
     end subroutine stdlib_dsytd2




     pure module subroutine stdlib_sorgtr( uplo, n, a, lda, tau, work, lwork, info )
     !! SORGTR generates a real orthogonal matrix Q which is defined as the
     !! product of n-1 elementary reflectors of order N, as returned by
     !! SSYTRD:
     !! if UPLO = 'U', Q = H(n-1) . . . H(2) H(1),
     !! if UPLO = 'L', Q = H(1) H(2) . . . H(n-1).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, n
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(in) :: tau(*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery, upper
           integer(ilp) :: i, iinfo, j, lwkopt, nb
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           else if( lwork<max( 1_ilp, n-1 ) .and. .not.lquery ) then
              info = -7_ilp
           end if
           if( info==0_ilp ) then
              if ( upper ) then
                nb = stdlib_ilaenv( 1_ilp, 'SORGQL', ' ', n-1, n-1, n-1, -1_ilp )
              else
                nb = stdlib_ilaenv( 1_ilp, 'SORGQR', ' ', n-1, n-1, n-1, -1_ilp )
              end if
              lwkopt = max( 1_ilp, n-1 )*nb
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SORGTR', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0_ilp ) then
              work( 1_ilp ) = 1_ilp
              return
           end if
           if( upper ) then
              ! q was determined by a call to stdlib_ssytrd with uplo = 'u'
              ! shift the vectors which define the elementary reflectors one
              ! column to the left, and set the last row and column of q to
              ! those of the unit matrix
              do j = 1, n - 1
                 do i = 1, j - 1
                    a( i, j ) = a( i, j+1 )
                 end do
                 a( n, j ) = zero
              end do
              do i = 1, n - 1
                 a( i, n ) = zero
              end do
              a( n, n ) = one
              ! generate q(1:n-1,1:n-1)
              call stdlib_sorgql( n-1, n-1, n-1, a, lda, tau, work, lwork, iinfo )
           else
              ! q was determined by a call to stdlib_ssytrd with uplo = 'l'.
              ! shift the vectors which define the elementary reflectors one
              ! column to the right, and set the first row and column of q to
              ! those of the unit matrix
              do j = n, 2, -1
                 a( 1_ilp, j ) = zero
                 do i = j + 1, n
                    a( i, j ) = a( i, j-1 )
                 end do
              end do
              a( 1_ilp, 1_ilp ) = one
              do i = 2, n
                 a( i, 1_ilp ) = zero
              end do
              if( n>1_ilp ) then
                 ! generate q(2:n,2:n)
                 call stdlib_sorgqr( n-1, n-1, n-1, a( 2_ilp, 2_ilp ), lda, tau, work,lwork, iinfo )
                           
              end if
           end if
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_sorgtr

     pure module subroutine stdlib_dorgtr( uplo, n, a, lda, tau, work, lwork, info )
     !! DORGTR generates a real orthogonal matrix Q which is defined as the
     !! product of n-1 elementary reflectors of order N, as returned by
     !! DSYTRD:
     !! if UPLO = 'U', Q = H(n-1) . . . H(2) H(1),
     !! if UPLO = 'L', Q = H(1) H(2) . . . H(n-1).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, n
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(in) :: tau(*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery, upper
           integer(ilp) :: i, iinfo, j, lwkopt, nb
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           else if( lwork<max( 1_ilp, n-1 ) .and. .not.lquery ) then
              info = -7_ilp
           end if
           if( info==0_ilp ) then
              if( upper ) then
                 nb = stdlib_ilaenv( 1_ilp, 'DORGQL', ' ', n-1, n-1, n-1, -1_ilp )
              else
                 nb = stdlib_ilaenv( 1_ilp, 'DORGQR', ' ', n-1, n-1, n-1, -1_ilp )
              end if
              lwkopt = max( 1_ilp, n-1 )*nb
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DORGTR', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0_ilp ) then
              work( 1_ilp ) = 1_ilp
              return
           end if
           if( upper ) then
              ! q was determined by a call to stdlib_dsytrd with uplo = 'u'
              ! shift the vectors which define the elementary reflectors one
              ! column to the left, and set the last row and column of q to
              ! those of the unit matrix
              do j = 1, n - 1
                 do i = 1, j - 1
                    a( i, j ) = a( i, j+1 )
                 end do
                 a( n, j ) = zero
              end do
              do i = 1, n - 1
                 a( i, n ) = zero
              end do
              a( n, n ) = one
              ! generate q(1:n-1,1:n-1)
              call stdlib_dorgql( n-1, n-1, n-1, a, lda, tau, work, lwork, iinfo )
           else
              ! q was determined by a call to stdlib_dsytrd with uplo = 'l'.
              ! shift the vectors which define the elementary reflectors one
              ! column to the right, and set the first row and column of q to
              ! those of the unit matrix
              do j = n, 2, -1
                 a( 1_ilp, j ) = zero
                 do i = j + 1, n
                    a( i, j ) = a( i, j-1 )
                 end do
              end do
              a( 1_ilp, 1_ilp ) = one
              do i = 2, n
                 a( i, 1_ilp ) = zero
              end do
              if( n>1_ilp ) then
                 ! generate q(2:n,2:n)
                 call stdlib_dorgqr( n-1, n-1, n-1, a( 2_ilp, 2_ilp ), lda, tau, work,lwork, iinfo )
                           
              end if
           end if
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_dorgtr




     pure module subroutine stdlib_sormtr( side, uplo, trans, m, n, a, lda, tau, c, ldc,work, lwork, &
     !! SORMTR overwrites the general real M-by-N matrix C with
     !! SIDE = 'L'     SIDE = 'R'
     !! TRANS = 'N':      Q * C          C * Q
     !! TRANS = 'T':      Q**T * C       C * Q**T
     !! where Q is a real orthogonal matrix of order nq, with nq = m if
     !! SIDE = 'L' and nq = n if SIDE = 'R'. Q is defined as the product of
     !! nq-1 elementary reflectors, as returned by SSYTRD:
     !! if UPLO = 'U', Q = H(nq-1) . . . H(2) H(1);
     !! if UPLO = 'L', Q = H(1) H(2) . . . H(nq-1).
               info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side, trans, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldc, lwork, m, n
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*), c(ldc,*)
           real(sp), intent(in) :: tau(*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: left, lquery, upper
           integer(ilp) :: i1, i2, iinfo, lwkopt, mi, ni, nb, nq, nw
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           left = stdlib_lsame( side, 'L' )
           upper = stdlib_lsame( uplo, 'U' )
           lquery = ( lwork==-1_ilp )
           ! nq is the order of q and nw is the minimum dimension of work
           if( left ) then
              nq = m
              nw = max( 1_ilp, n )
           else
              nq = n
              nw = max( 1_ilp, m )
           end if
           if( .not.left .and. .not.stdlib_lsame( side, 'R' ) ) then
              info = -1_ilp
           else if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -2_ilp
           else if( .not.stdlib_lsame( trans, 'N' ) .and. .not.stdlib_lsame( trans, 'T' ) )&
                     then
              info = -3_ilp
           else if( m<0_ilp ) then
              info = -4_ilp
           else if( n<0_ilp ) then
              info = -5_ilp
           else if( lda<max( 1_ilp, nq ) ) then
              info = -7_ilp
           else if( ldc<max( 1_ilp, m ) ) then
              info = -10_ilp
           else if( lwork<nw .and. .not.lquery ) then
              info = -12_ilp
           end if
           if( info==0_ilp ) then
              if( upper ) then
                 if( left ) then
                    nb = stdlib_ilaenv( 1_ilp, 'SORMQL', side // trans, m-1, n, m-1,-1_ilp )
                 else
                    nb = stdlib_ilaenv( 1_ilp, 'SORMQL', side // trans, m, n-1, n-1,-1_ilp )
                 end if
              else
                 if( left ) then
                    nb = stdlib_ilaenv( 1_ilp, 'SORMQR', side // trans, m-1, n, m-1,-1_ilp )
                 else
                    nb = stdlib_ilaenv( 1_ilp, 'SORMQR', side // trans, m, n-1, n-1,-1_ilp )
                 end if
              end if
              lwkopt = nw*nb
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SORMTR', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( m==0_ilp .or. n==0_ilp .or. nq==1_ilp ) then
              work( 1_ilp ) = 1_ilp
              return
           end if
           if( left ) then
              mi = m - 1_ilp
              ni = n
           else
              mi = m
              ni = n - 1_ilp
           end if
           if( upper ) then
              ! q was determined by a call to stdlib_ssytrd with uplo = 'u'
              call stdlib_sormql( side, trans, mi, ni, nq-1, a( 1_ilp, 2_ilp ), lda, tau, c,ldc, work, &
                        lwork, iinfo )
           else
              ! q was determined by a call to stdlib_ssytrd with uplo = 'l'
              if( left ) then
                 i1 = 2_ilp
                 i2 = 1_ilp
              else
                 i1 = 1_ilp
                 i2 = 2_ilp
              end if
              call stdlib_sormqr( side, trans, mi, ni, nq-1, a( 2_ilp, 1_ilp ), lda, tau,c( i1, i2 ), ldc,&
                         work, lwork, iinfo )
           end if
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_sormtr

     pure module subroutine stdlib_dormtr( side, uplo, trans, m, n, a, lda, tau, c, ldc,work, lwork, &
     !! DORMTR overwrites the general real M-by-N matrix C with
     !! SIDE = 'L'     SIDE = 'R'
     !! TRANS = 'N':      Q * C          C * Q
     !! TRANS = 'T':      Q**T * C       C * Q**T
     !! where Q is a real orthogonal matrix of order nq, with nq = m if
     !! SIDE = 'L' and nq = n if SIDE = 'R'. Q is defined as the product of
     !! nq-1 elementary reflectors, as returned by DSYTRD:
     !! if UPLO = 'U', Q = H(nq-1) . . . H(2) H(1);
     !! if UPLO = 'L', Q = H(1) H(2) . . . H(nq-1).
               info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side, trans, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldc, lwork, m, n
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*), c(ldc,*)
           real(dp), intent(in) :: tau(*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: left, lquery, upper
           integer(ilp) :: i1, i2, iinfo, lwkopt, mi, nb, ni, nq, nw
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           left = stdlib_lsame( side, 'L' )
           upper = stdlib_lsame( uplo, 'U' )
           lquery = ( lwork==-1_ilp )
           ! nq is the order of q and nw is the minimum dimension of work
           if( left ) then
              nq = m
              nw = max( 1_ilp, n )
           else
              nq = n
              nw = max( 1_ilp, m )
           end if
           if( .not.left .and. .not.stdlib_lsame( side, 'R' ) ) then
              info = -1_ilp
           else if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -2_ilp
           else if( .not.stdlib_lsame( trans, 'N' ) .and. .not.stdlib_lsame( trans, 'T' ) )&
                     then
              info = -3_ilp
           else if( m<0_ilp ) then
              info = -4_ilp
           else if( n<0_ilp ) then
              info = -5_ilp
           else if( lda<max( 1_ilp, nq ) ) then
              info = -7_ilp
           else if( ldc<max( 1_ilp, m ) ) then
              info = -10_ilp
           else if( lwork<nw .and. .not.lquery ) then
              info = -12_ilp
           end if
           if( info==0_ilp ) then
              if( upper ) then
                 if( left ) then
                    nb = stdlib_ilaenv( 1_ilp, 'DORMQL', side // trans, m-1, n, m-1,-1_ilp )
                 else
                    nb = stdlib_ilaenv( 1_ilp, 'DORMQL', side // trans, m, n-1, n-1,-1_ilp )
                 end if
              else
                 if( left ) then
                    nb = stdlib_ilaenv( 1_ilp, 'DORMQR', side // trans, m-1, n, m-1,-1_ilp )
                 else
                    nb = stdlib_ilaenv( 1_ilp, 'DORMQR', side // trans, m, n-1, n-1,-1_ilp )
                 end if
              end if
              lwkopt = nw*nb
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DORMTR', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( m==0_ilp .or. n==0_ilp .or. nq==1_ilp ) then
              work( 1_ilp ) = 1_ilp
              return
           end if
           if( left ) then
              mi = m - 1_ilp
              ni = n
           else
              mi = m
              ni = n - 1_ilp
           end if
           if( upper ) then
              ! q was determined by a call to stdlib_dsytrd with uplo = 'u'
              call stdlib_dormql( side, trans, mi, ni, nq-1, a( 1_ilp, 2_ilp ), lda, tau, c,ldc, work, &
                        lwork, iinfo )
           else
              ! q was determined by a call to stdlib_dsytrd with uplo = 'l'
              if( left ) then
                 i1 = 2_ilp
                 i2 = 1_ilp
              else
                 i1 = 1_ilp
                 i2 = 2_ilp
              end if
              call stdlib_dormqr( side, trans, mi, ni, nq-1, a( 2_ilp, 1_ilp ), lda, tau,c( i1, i2 ), ldc,&
                         work, lwork, iinfo )
           end if
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_dormtr




     pure module subroutine stdlib_ssb2st_kernels( uplo, wantz, ttype,st, ed, sweep, n, nb, ib,a, lda, &
     !! SSB2ST_KERNELS is an internal routine used by the SSYTRD_SB2ST
     !! subroutine.
               v, tau, ldvt, work)
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           logical(lk), intent(in) :: wantz
           integer(ilp), intent(in) :: ttype, st, ed, sweep, n, nb, ib, lda, ldvt
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: v(*), tau(*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: i, j1, j2, lm, ln, vpos, taupos, dpos, ofdpos, ajeter
           real(sp) :: ctmp
           ! Intrinsic Functions 
           ! Executable Statements 
           ajeter = ib + ldvt
           upper = stdlib_lsame( uplo, 'U' )
           if( upper ) then
               dpos    = 2_ilp * nb + 1_ilp
               ofdpos  = 2_ilp * nb
           else
               dpos    = 1_ilp
               ofdpos  = 2_ilp
           endif
           ! upper case
           if( upper ) then
               if( wantz ) then
                   vpos   = mod( sweep-1, 2_ilp ) * n + st
                   taupos = mod( sweep-1, 2_ilp ) * n + st
               else
                   vpos   = mod( sweep-1, 2_ilp ) * n + st
                   taupos = mod( sweep-1, 2_ilp ) * n + st
               endif
               if( ttype==1_ilp ) then
                   lm = ed - st + 1_ilp
                   v( vpos ) = one
                   do i = 1, lm-1
                       v( vpos+i )         = ( a( ofdpos-i, st+i ) )
                       a( ofdpos-i, st+i ) = zero
                   end do
                   ctmp = ( a( ofdpos, st ) )
                   call stdlib_slarfg( lm, ctmp, v( vpos+1 ), 1_ilp,tau( taupos ) )
                   a( ofdpos, st ) = ctmp
                   lm = ed - st + 1_ilp
                   call stdlib_slarfy( uplo, lm, v( vpos ), 1_ilp,( tau( taupos ) ),a( dpos, st ), &
                             lda-1, work)
               endif
               if( ttype==3_ilp ) then
                   lm = ed - st + 1_ilp
                   call stdlib_slarfy( uplo, lm, v( vpos ), 1_ilp,( tau( taupos ) ),a( dpos, st ), &
                             lda-1, work)
               endif
               if( ttype==2_ilp ) then
                   j1 = ed+1
                   j2 = min( ed+nb, n )
                   ln = ed-st+1
                   lm = j2-j1+1
                   if( lm>0_ilp) then
                       call stdlib_slarfx( 'LEFT', ln, lm, v( vpos ),( tau( taupos ) ),a( dpos-nb,&
                                  j1 ), lda-1, work)
                       if( wantz ) then
                           vpos   = mod( sweep-1, 2_ilp ) * n + j1
                           taupos = mod( sweep-1, 2_ilp ) * n + j1
                       else
                           vpos   = mod( sweep-1, 2_ilp ) * n + j1
                           taupos = mod( sweep-1, 2_ilp ) * n + j1
                       endif
                       v( vpos ) = one
                       do i = 1, lm-1
                           v( vpos+i )          =( a( dpos-nb-i, j1+i ) )
                           a( dpos-nb-i, j1+i ) = zero
                       end do
                       ctmp = ( a( dpos-nb, j1 ) )
                       call stdlib_slarfg( lm, ctmp, v( vpos+1 ), 1_ilp, tau( taupos ) )
                       a( dpos-nb, j1 ) = ctmp
                       call stdlib_slarfx( 'RIGHT', ln-1, lm, v( vpos ),tau( taupos ),a( dpos-nb+&
                                 1_ilp, j1 ), lda-1, work)
                   endif
               endif
           ! lower case
           else
               if( wantz ) then
                   vpos   = mod( sweep-1, 2_ilp ) * n + st
                   taupos = mod( sweep-1, 2_ilp ) * n + st
               else
                   vpos   = mod( sweep-1, 2_ilp ) * n + st
                   taupos = mod( sweep-1, 2_ilp ) * n + st
               endif
               if( ttype==1_ilp ) then
                   lm = ed - st + 1_ilp
                   v( vpos ) = one
                   do i = 1, lm-1
                       v( vpos+i )         = a( ofdpos+i, st-1 )
                       a( ofdpos+i, st-1 ) = zero
                   end do
                   call stdlib_slarfg( lm, a( ofdpos, st-1 ), v( vpos+1 ), 1_ilp,tau( taupos ) )
                             
                   lm = ed - st + 1_ilp
                   call stdlib_slarfy( uplo, lm, v( vpos ), 1_ilp,( tau( taupos ) ),a( dpos, st ), &
                             lda-1, work)
               endif
               if( ttype==3_ilp ) then
                   lm = ed - st + 1_ilp
                   call stdlib_slarfy( uplo, lm, v( vpos ), 1_ilp,( tau( taupos ) ),a( dpos, st ), &
                             lda-1, work)
               endif
               if( ttype==2_ilp ) then
                   j1 = ed+1
                   j2 = min( ed+nb, n )
                   ln = ed-st+1
                   lm = j2-j1+1
                   if( lm>0_ilp) then
                       call stdlib_slarfx( 'RIGHT', lm, ln, v( vpos ),tau( taupos ), a( dpos+nb, &
                                 st ),lda-1, work)
                       if( wantz ) then
                           vpos   = mod( sweep-1, 2_ilp ) * n + j1
                           taupos = mod( sweep-1, 2_ilp ) * n + j1
                       else
                           vpos   = mod( sweep-1, 2_ilp ) * n + j1
                           taupos = mod( sweep-1, 2_ilp ) * n + j1
                       endif
                       v( vpos ) = one
                       do i = 1, lm-1
                           v( vpos+i )        = a( dpos+nb+i, st )
                           a( dpos+nb+i, st ) = zero
                       end do
                       call stdlib_slarfg( lm, a( dpos+nb, st ), v( vpos+1 ), 1_ilp,tau( taupos ) )
                                 
                       call stdlib_slarfx( 'LEFT', lm, ln-1, v( vpos ),( tau( taupos ) ),a( dpos+&
                                 nb-1, st+1 ), lda-1, work)
                   endif
               endif
           endif
           return
     end subroutine stdlib_ssb2st_kernels

     pure module subroutine stdlib_dsb2st_kernels( uplo, wantz, ttype,st, ed, sweep, n, nb, ib,a, lda, &
     !! DSB2ST_KERNELS is an internal routine used by the DSYTRD_SB2ST
     !! subroutine.
               v, tau, ldvt, work)
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           logical(lk), intent(in) :: wantz
           integer(ilp), intent(in) :: ttype, st, ed, sweep, n, nb, ib, lda, ldvt
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: v(*), tau(*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: i, j1, j2, lm, ln, vpos, taupos, dpos, ofdpos, ajeter
           real(dp) :: ctmp
           ! Intrinsic Functions 
           ! Executable Statements 
           ajeter = ib + ldvt
           upper = stdlib_lsame( uplo, 'U' )
           if( upper ) then
               dpos    = 2_ilp * nb + 1_ilp
               ofdpos  = 2_ilp * nb
           else
               dpos    = 1_ilp
               ofdpos  = 2_ilp
           endif
           ! upper case
           if( upper ) then
               if( wantz ) then
                   vpos   = mod( sweep-1, 2_ilp ) * n + st
                   taupos = mod( sweep-1, 2_ilp ) * n + st
               else
                   vpos   = mod( sweep-1, 2_ilp ) * n + st
                   taupos = mod( sweep-1, 2_ilp ) * n + st
               endif
               if( ttype==1_ilp ) then
                   lm = ed - st + 1_ilp
                   v( vpos ) = one
                   do i = 1, lm-1
                       v( vpos+i )         = ( a( ofdpos-i, st+i ) )
                       a( ofdpos-i, st+i ) = zero
                   end do
                   ctmp = ( a( ofdpos, st ) )
                   call stdlib_dlarfg( lm, ctmp, v( vpos+1 ), 1_ilp,tau( taupos ) )
                   a( ofdpos, st ) = ctmp
                   lm = ed - st + 1_ilp
                   call stdlib_dlarfy( uplo, lm, v( vpos ), 1_ilp,( tau( taupos ) ),a( dpos, st ), &
                             lda-1, work)
               endif
               if( ttype==3_ilp ) then
                   lm = ed - st + 1_ilp
                   call stdlib_dlarfy( uplo, lm, v( vpos ), 1_ilp,( tau( taupos ) ),a( dpos, st ), &
                             lda-1, work)
               endif
               if( ttype==2_ilp ) then
                   j1 = ed+1
                   j2 = min( ed+nb, n )
                   ln = ed-st+1
                   lm = j2-j1+1
                   if( lm>0_ilp) then
                       call stdlib_dlarfx( 'LEFT', ln, lm, v( vpos ),( tau( taupos ) ),a( dpos-nb,&
                                  j1 ), lda-1, work)
                       if( wantz ) then
                           vpos   = mod( sweep-1, 2_ilp ) * n + j1
                           taupos = mod( sweep-1, 2_ilp ) * n + j1
                       else
                           vpos   = mod( sweep-1, 2_ilp ) * n + j1
                           taupos = mod( sweep-1, 2_ilp ) * n + j1
                       endif
                       v( vpos ) = one
                       do i = 1, lm-1
                           v( vpos+i )          =( a( dpos-nb-i, j1+i ) )
                           a( dpos-nb-i, j1+i ) = zero
                       end do
                       ctmp = ( a( dpos-nb, j1 ) )
                       call stdlib_dlarfg( lm, ctmp, v( vpos+1 ), 1_ilp, tau( taupos ) )
                       a( dpos-nb, j1 ) = ctmp
                       call stdlib_dlarfx( 'RIGHT', ln-1, lm, v( vpos ),tau( taupos ),a( dpos-nb+&
                                 1_ilp, j1 ), lda-1, work)
                   endif
               endif
           ! lower case
           else
               if( wantz ) then
                   vpos   = mod( sweep-1, 2_ilp ) * n + st
                   taupos = mod( sweep-1, 2_ilp ) * n + st
               else
                   vpos   = mod( sweep-1, 2_ilp ) * n + st
                   taupos = mod( sweep-1, 2_ilp ) * n + st
               endif
               if( ttype==1_ilp ) then
                   lm = ed - st + 1_ilp
                   v( vpos ) = one
                   do i = 1, lm-1
                       v( vpos+i )         = a( ofdpos+i, st-1 )
                       a( ofdpos+i, st-1 ) = zero
                   end do
                   call stdlib_dlarfg( lm, a( ofdpos, st-1 ), v( vpos+1 ), 1_ilp,tau( taupos ) )
                             
                   lm = ed - st + 1_ilp
                   call stdlib_dlarfy( uplo, lm, v( vpos ), 1_ilp,( tau( taupos ) ),a( dpos, st ), &
                             lda-1, work)
               endif
               if( ttype==3_ilp ) then
                   lm = ed - st + 1_ilp
                   call stdlib_dlarfy( uplo, lm, v( vpos ), 1_ilp,( tau( taupos ) ),a( dpos, st ), &
                             lda-1, work)
               endif
               if( ttype==2_ilp ) then
                   j1 = ed+1
                   j2 = min( ed+nb, n )
                   ln = ed-st+1
                   lm = j2-j1+1
                   if( lm>0_ilp) then
                       call stdlib_dlarfx( 'RIGHT', lm, ln, v( vpos ),tau( taupos ), a( dpos+nb, &
                                 st ),lda-1, work)
                       if( wantz ) then
                           vpos   = mod( sweep-1, 2_ilp ) * n + j1
                           taupos = mod( sweep-1, 2_ilp ) * n + j1
                       else
                           vpos   = mod( sweep-1, 2_ilp ) * n + j1
                           taupos = mod( sweep-1, 2_ilp ) * n + j1
                       endif
                       v( vpos ) = one
                       do i = 1, lm-1
                           v( vpos+i )        = a( dpos+nb+i, st )
                           a( dpos+nb+i, st ) = zero
                       end do
                       call stdlib_dlarfg( lm, a( dpos+nb, st ), v( vpos+1 ), 1_ilp,tau( taupos ) )
                                 
                       call stdlib_dlarfx( 'LEFT', lm, ln-1, v( vpos ),( tau( taupos ) ),a( dpos+&
                                 nb-1, st+1 ), lda-1, work)
                   endif
               endif
           endif
           return
     end subroutine stdlib_dsb2st_kernels




     module subroutine stdlib_chegv( itype, jobz, uplo, n, a, lda, b, ldb, w, work,lwork, rwork, info )
     !! CHEGV computes all the eigenvalues, and optionally, the eigenvectors
     !! of a complex generalized Hermitian-definite eigenproblem, of the form
     !! A*x=(lambda)*B*x,  A*Bx=(lambda)*x,  or B*A*x=(lambda)*x.
     !! Here A and B are assumed to be Hermitian and B is also
     !! positive definite.
               
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: itype, lda, ldb, lwork, n
           ! Array Arguments 
           real(sp), intent(out) :: rwork(*), w(*)
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery, upper, wantz
           character :: trans
           integer(ilp) :: lwkopt, nb, neig
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           upper = stdlib_lsame( uplo, 'U' )
           lquery = ( lwork== -1_ilp )
           info = 0_ilp
           if( itype<1_ilp .or. itype>3_ilp ) then
              info = -1_ilp
           else if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -2_ilp
           else if( .not.( upper .or. stdlib_lsame( uplo, 'L' ) ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -6_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -8_ilp
           end if
           if( info==0_ilp ) then
              nb = stdlib_ilaenv( 1_ilp, 'CHETRD', uplo, n, -1_ilp, -1_ilp, -1_ilp )
              lwkopt = max( 1_ilp, ( nb + 1_ilp )*n )
              work( 1_ilp ) = lwkopt
              if( lwork<max( 1_ilp, 2_ilp*n-1 ) .and. .not.lquery ) then
                 info = -11_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CHEGV ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! form a cholesky factorization of b.
           call stdlib_cpotrf( uplo, n, b, ldb, info )
           if( info/=0_ilp ) then
              info = n + info
              return
           end if
           ! transform problem to standard eigenvalue problem and solve.
           call stdlib_chegst( itype, uplo, n, a, lda, b, ldb, info )
           call stdlib_cheev( jobz, uplo, n, a, lda, w, work, lwork, rwork, info )
           if( wantz ) then
              ! backtransform eigenvectors to the original problem.
              neig = n
              if( info>0_ilp )neig = info - 1_ilp
              if( itype==1_ilp .or. itype==2_ilp ) then
                 ! for a*x=(lambda)*b*x and a*b*x=(lambda)*x;
                 ! backtransform eigenvectors: x = inv(l)**h*y or inv(u)*y
                 if( upper ) then
                    trans = 'N'
                 else
                    trans = 'C'
                 end if
                 call stdlib_ctrsm( 'LEFT', uplo, trans, 'NON-UNIT', n, neig, cone,b, ldb, a, lda &
                           )
              else if( itype==3_ilp ) then
                 ! for b*a*x=(lambda)*x;
                 ! backtransform eigenvectors: x = l*y or u**h*y
                 if( upper ) then
                    trans = 'C'
                 else
                    trans = 'N'
                 end if
                 call stdlib_ctrmm( 'LEFT', uplo, trans, 'NON-UNIT', n, neig, cone,b, ldb, a, lda &
                           )
              end if
           end if
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_chegv

     module subroutine stdlib_zhegv( itype, jobz, uplo, n, a, lda, b, ldb, w, work,lwork, rwork, info )
     !! ZHEGV computes all the eigenvalues, and optionally, the eigenvectors
     !! of a complex generalized Hermitian-definite eigenproblem, of the form
     !! A*x=(lambda)*B*x,  A*Bx=(lambda)*x,  or B*A*x=(lambda)*x.
     !! Here A and B are assumed to be Hermitian and B is also
     !! positive definite.
               
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: itype, lda, ldb, lwork, n
           ! Array Arguments 
           real(dp), intent(out) :: rwork(*), w(*)
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery, upper, wantz
           character :: trans
           integer(ilp) :: lwkopt, nb, neig
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           upper = stdlib_lsame( uplo, 'U' )
           lquery = ( lwork==-1_ilp )
           info = 0_ilp
           if( itype<1_ilp .or. itype>3_ilp ) then
              info = -1_ilp
           else if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -2_ilp
           else if( .not.( upper .or. stdlib_lsame( uplo, 'L' ) ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -6_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -8_ilp
           end if
           if( info==0_ilp ) then
              nb = stdlib_ilaenv( 1_ilp, 'ZHETRD', uplo, n, -1_ilp, -1_ilp, -1_ilp )
              lwkopt = max( 1_ilp, ( nb + 1_ilp )*n )
              work( 1_ilp ) = lwkopt
              if( lwork<max( 1_ilp, 2_ilp*n - 1_ilp ) .and. .not.lquery ) then
                 info = -11_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZHEGV ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! form a cholesky factorization of b.
           call stdlib_zpotrf( uplo, n, b, ldb, info )
           if( info/=0_ilp ) then
              info = n + info
              return
           end if
           ! transform problem to standard eigenvalue problem and solve.
           call stdlib_zhegst( itype, uplo, n, a, lda, b, ldb, info )
           call stdlib_zheev( jobz, uplo, n, a, lda, w, work, lwork, rwork, info )
           if( wantz ) then
              ! backtransform eigenvectors to the original problem.
              neig = n
              if( info>0_ilp )neig = info - 1_ilp
              if( itype==1_ilp .or. itype==2_ilp ) then
                 ! for a*x=(lambda)*b*x and a*b*x=(lambda)*x;
                 ! backtransform eigenvectors: x = inv(l)**h *y or inv(u)*y
                 if( upper ) then
                    trans = 'N'
                 else
                    trans = 'C'
                 end if
                 call stdlib_ztrsm( 'LEFT', uplo, trans, 'NON-UNIT', n, neig, cone,b, ldb, a, lda &
                           )
              else if( itype==3_ilp ) then
                 ! for b*a*x=(lambda)*x;
                 ! backtransform eigenvectors: x = l*y or u**h *y
                 if( upper ) then
                    trans = 'C'
                 else
                    trans = 'N'
                 end if
                 call stdlib_ztrmm( 'LEFT', uplo, trans, 'NON-UNIT', n, neig, cone,b, ldb, a, lda &
                           )
              end if
           end if
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_zhegv




     module subroutine stdlib_chegvd( itype, jobz, uplo, n, a, lda, b, ldb, w, work,lwork, rwork, lrwork,&
     !! CHEGVD computes all the eigenvalues, and optionally, the eigenvectors
     !! of a complex generalized Hermitian-definite eigenproblem, of the form
     !! A*x=(lambda)*B*x,  A*Bx=(lambda)*x,  or B*A*x=(lambda)*x.  Here A and
     !! B are assumed to be Hermitian and B is also positive definite.
     !! If eigenvectors are desired, it uses a divide and conquer algorithm.
     !! The divide and conquer algorithm makes very mild assumptions about
     !! floating point arithmetic. It will work on machines with a guard
     !! digit in add/subtract, or on those binary machines without guard
     !! digits which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or
     !! Cray-2. It could conceivably fail on hexadecimal or decimal machines
     !! without guard digits, but we know of none.
                iwork, liwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: itype, lda, ldb, liwork, lrwork, lwork, n
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(out) :: rwork(*), w(*)
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery, upper, wantz
           character :: trans
           integer(ilp) :: liopt, liwmin, lopt, lropt, lrwmin, lwmin
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           upper = stdlib_lsame( uplo, 'U' )
           lquery = ( lwork==-1_ilp .or. lrwork==-1_ilp .or. liwork==-1_ilp )
           info = 0_ilp
           if( n<=1_ilp ) then
              lwmin = 1_ilp
              lrwmin = 1_ilp
              liwmin = 1_ilp
           else if( wantz ) then
              lwmin = 2_ilp*n + n*n
              lrwmin = 1_ilp + 5_ilp*n + 2_ilp*n*n
              liwmin = 3_ilp + 5_ilp*n
           else
              lwmin = n + 1_ilp
              lrwmin = n
              liwmin = 1_ilp
           end if
           lopt = lwmin
           lropt = lrwmin
           liopt = liwmin
           if( itype<1_ilp .or. itype>3_ilp ) then
              info = -1_ilp
           else if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -2_ilp
           else if( .not.( upper .or. stdlib_lsame( uplo, 'L' ) ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -6_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -8_ilp
           end if
           if( info==0_ilp ) then
              work( 1_ilp ) = lopt
              rwork( 1_ilp ) = lropt
              iwork( 1_ilp ) = liopt
              if( lwork<lwmin .and. .not.lquery ) then
                 info = -11_ilp
              else if( lrwork<lrwmin .and. .not.lquery ) then
                 info = -13_ilp
              else if( liwork<liwmin .and. .not.lquery ) then
                 info = -15_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CHEGVD', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! form a cholesky factorization of b.
           call stdlib_cpotrf( uplo, n, b, ldb, info )
           if( info/=0_ilp ) then
              info = n + info
              return
           end if
           ! transform problem to standard eigenvalue problem and solve.
           call stdlib_chegst( itype, uplo, n, a, lda, b, ldb, info )
           call stdlib_cheevd( jobz, uplo, n, a, lda, w, work, lwork, rwork, lrwork,iwork, liwork,&
                      info )
           lopt = max( real( lopt,KIND=sp), real( work( 1_ilp ),KIND=sp) )
           lropt = max( real( lropt,KIND=sp), real( rwork( 1_ilp ),KIND=sp) )
           liopt = max( real( liopt,KIND=sp), real( iwork( 1_ilp ),KIND=sp) )
           if( wantz .and. info==0_ilp ) then
              ! backtransform eigenvectors to the original problem.
              if( itype==1_ilp .or. itype==2_ilp ) then
                 ! for a*x=(lambda)*b*x and a*b*x=(lambda)*x;
                 ! backtransform eigenvectors: x = inv(l)**h *y or inv(u)*y
                 if( upper ) then
                    trans = 'N'
                 else
                    trans = 'C'
                 end if
                 call stdlib_ctrsm( 'LEFT', uplo, trans, 'NON-UNIT', n, n, cone,b, ldb, a, lda )
                           
              else if( itype==3_ilp ) then
                 ! for b*a*x=(lambda)*x;
                 ! backtransform eigenvectors: x = l*y or u**h *y
                 if( upper ) then
                    trans = 'C'
                 else
                    trans = 'N'
                 end if
                 call stdlib_ctrmm( 'LEFT', uplo, trans, 'NON-UNIT', n, n, cone,b, ldb, a, lda )
                           
              end if
           end if
           work( 1_ilp ) = lopt
           rwork( 1_ilp ) = lropt
           iwork( 1_ilp ) = liopt
           return
     end subroutine stdlib_chegvd

     module subroutine stdlib_zhegvd( itype, jobz, uplo, n, a, lda, b, ldb, w, work,lwork, rwork, lrwork,&
     !! ZHEGVD computes all the eigenvalues, and optionally, the eigenvectors
     !! of a complex generalized Hermitian-definite eigenproblem, of the form
     !! A*x=(lambda)*B*x,  A*Bx=(lambda)*x,  or B*A*x=(lambda)*x.  Here A and
     !! B are assumed to be Hermitian and B is also positive definite.
     !! If eigenvectors are desired, it uses a divide and conquer algorithm.
     !! The divide and conquer algorithm makes very mild assumptions about
     !! floating point arithmetic. It will work on machines with a guard
     !! digit in add/subtract, or on those binary machines without guard
     !! digits which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or
     !! Cray-2. It could conceivably fail on hexadecimal or decimal machines
     !! without guard digits, but we know of none.
                iwork, liwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: itype, lda, ldb, liwork, lrwork, lwork, n
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(out) :: rwork(*), w(*)
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery, upper, wantz
           character :: trans
           integer(ilp) :: liopt, liwmin, lopt, lropt, lrwmin, lwmin
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           upper = stdlib_lsame( uplo, 'U' )
           lquery = ( lwork==-1_ilp .or. lrwork==-1_ilp .or. liwork==-1_ilp )
           info = 0_ilp
           if( n<=1_ilp ) then
              lwmin = 1_ilp
              lrwmin = 1_ilp
              liwmin = 1_ilp
           else if( wantz ) then
              lwmin = 2_ilp*n + n*n
              lrwmin = 1_ilp + 5_ilp*n + 2_ilp*n*n
              liwmin = 3_ilp + 5_ilp*n
           else
              lwmin = n + 1_ilp
              lrwmin = n
              liwmin = 1_ilp
           end if
           lopt = lwmin
           lropt = lrwmin
           liopt = liwmin
           if( itype<1_ilp .or. itype>3_ilp ) then
              info = -1_ilp
           else if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -2_ilp
           else if( .not.( upper .or. stdlib_lsame( uplo, 'L' ) ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -6_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -8_ilp
           end if
           if( info==0_ilp ) then
              work( 1_ilp ) = lopt
              rwork( 1_ilp ) = lropt
              iwork( 1_ilp ) = liopt
              if( lwork<lwmin .and. .not.lquery ) then
                 info = -11_ilp
              else if( lrwork<lrwmin .and. .not.lquery ) then
                 info = -13_ilp
              else if( liwork<liwmin .and. .not.lquery ) then
                 info = -15_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZHEGVD', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! form a cholesky factorization of b.
           call stdlib_zpotrf( uplo, n, b, ldb, info )
           if( info/=0_ilp ) then
              info = n + info
              return
           end if
           ! transform problem to standard eigenvalue problem and solve.
           call stdlib_zhegst( itype, uplo, n, a, lda, b, ldb, info )
           call stdlib_zheevd( jobz, uplo, n, a, lda, w, work, lwork, rwork, lrwork,iwork, liwork,&
                      info )
           lopt = max( real( lopt,KIND=dp), real( work( 1_ilp ),KIND=dp) )
           lropt = max( real( lropt,KIND=dp), real( rwork( 1_ilp ),KIND=dp) )
           liopt = max( real( liopt,KIND=dp), real( iwork( 1_ilp ),KIND=dp) )
           if( wantz .and. info==0_ilp ) then
              ! backtransform eigenvectors to the original problem.
              if( itype==1_ilp .or. itype==2_ilp ) then
                 ! for a*x=(lambda)*b*x and a*b*x=(lambda)*x;
                 ! backtransform eigenvectors: x = inv(l)**h *y or inv(u)*y
                 if( upper ) then
                    trans = 'N'
                 else
                    trans = 'C'
                 end if
                 call stdlib_ztrsm( 'LEFT', uplo, trans, 'NON-UNIT', n, n, cone,b, ldb, a, lda )
                           
              else if( itype==3_ilp ) then
                 ! for b*a*x=(lambda)*x;
                 ! backtransform eigenvectors: x = l*y or u**h *y
                 if( upper ) then
                    trans = 'C'
                 else
                    trans = 'N'
                 end if
                 call stdlib_ztrmm( 'LEFT', uplo, trans, 'NON-UNIT', n, n, cone,b, ldb, a, lda )
                           
              end if
           end if
           work( 1_ilp ) = lopt
           rwork( 1_ilp ) = lropt
           iwork( 1_ilp ) = liopt
           return
     end subroutine stdlib_zhegvd




     module subroutine stdlib_chegvx( itype, jobz, range, uplo, n, a, lda, b, ldb,vl, vu, il, iu, abstol,&
     !! CHEGVX computes selected eigenvalues, and optionally, eigenvectors
     !! of a complex generalized Hermitian-definite eigenproblem, of the form
     !! A*x=(lambda)*B*x,  A*Bx=(lambda)*x,  or B*A*x=(lambda)*x.  Here A and
     !! B are assumed to be Hermitian and B is also positive definite.
     !! Eigenvalues and eigenvectors can be selected by specifying either a
     !! range of values or a range of indices for the desired eigenvalues.
                m, w, z, ldz, work,lwork, rwork, iwork, ifail, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, range, uplo
           integer(ilp), intent(in) :: il, itype, iu, lda, ldb, ldz, lwork, n
           integer(ilp), intent(out) :: info, m
           real(sp), intent(in) :: abstol, vl, vu
           ! Array Arguments 
           integer(ilp), intent(out) :: ifail(*), iwork(*)
           real(sp), intent(out) :: rwork(*), w(*)
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(sp), intent(out) :: work(*), z(ldz,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: alleig, indeig, lquery, upper, valeig, wantz
           character :: trans
           integer(ilp) :: lwkopt, nb
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           upper = stdlib_lsame( uplo, 'U' )
           alleig = stdlib_lsame( range, 'A' )
           valeig = stdlib_lsame( range, 'V' )
           indeig = stdlib_lsame( range, 'I' )
           lquery = ( lwork==-1_ilp )
           info = 0_ilp
           if( itype<1_ilp .or. itype>3_ilp ) then
              info = -1_ilp
           else if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -2_ilp
           else if( .not.( alleig .or. valeig .or. indeig ) ) then
              info = -3_ilp
           else if( .not.( upper .or. stdlib_lsame( uplo, 'L' ) ) ) then
              info = -4_ilp
           else if( n<0_ilp ) then
              info = -5_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -7_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -9_ilp
           else
              if( valeig ) then
                 if( n>0_ilp .and. vu<=vl )info = -11_ilp
              else if( indeig ) then
                 if( il<1_ilp .or. il>max( 1_ilp, n ) ) then
                    info = -12_ilp
                 else if( iu<min( n, il ) .or. iu>n ) then
                    info = -13_ilp
                 end if
              end if
           end if
           if (info==0_ilp) then
              if (ldz<1_ilp .or. (wantz .and. ldz<n)) then
                 info = -18_ilp
              end if
           end if
           if( info==0_ilp ) then
              nb = stdlib_ilaenv( 1_ilp, 'CHETRD', uplo, n, -1_ilp, -1_ilp, -1_ilp )
              lwkopt = max( 1_ilp, ( nb + 1_ilp )*n )
              work( 1_ilp ) = lwkopt
              if( lwork<max( 1_ilp, 2_ilp*n ) .and. .not.lquery ) then
                 info = -20_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CHEGVX', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           m = 0_ilp
           if( n==0_ilp ) then
              return
           end if
           ! form a cholesky factorization of b.
           call stdlib_cpotrf( uplo, n, b, ldb, info )
           if( info/=0_ilp ) then
              info = n + info
              return
           end if
           ! transform problem to standard eigenvalue problem and solve.
           call stdlib_chegst( itype, uplo, n, a, lda, b, ldb, info )
           call stdlib_cheevx( jobz, range, uplo, n, a, lda, vl, vu, il, iu, abstol,m, w, z, ldz, &
                     work, lwork, rwork, iwork, ifail,info )
           if( wantz ) then
              ! backtransform eigenvectors to the original problem.
              if( info>0_ilp )m = info - 1_ilp
              if( itype==1_ilp .or. itype==2_ilp ) then
                 ! for a*x=(lambda)*b*x and a*b*x=(lambda)*x;
                 ! backtransform eigenvectors: x = inv(l)**h*y or inv(u)*y
                 if( upper ) then
                    trans = 'N'
                 else
                    trans = 'C'
                 end if
                 call stdlib_ctrsm( 'LEFT', uplo, trans, 'NON-UNIT', n, m, cone, b,ldb, z, ldz )
                           
              else if( itype==3_ilp ) then
                 ! for b*a*x=(lambda)*x;
                 ! backtransform eigenvectors: x = l*y or u**h*y
                 if( upper ) then
                    trans = 'C'
                 else
                    trans = 'N'
                 end if
                 call stdlib_ctrmm( 'LEFT', uplo, trans, 'NON-UNIT', n, m, cone, b,ldb, z, ldz )
                           
              end if
           end if
           ! set work(1) to optimal complex workspace size.
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_chegvx

     module subroutine stdlib_zhegvx( itype, jobz, range, uplo, n, a, lda, b, ldb,vl, vu, il, iu, abstol,&
     !! ZHEGVX computes selected eigenvalues, and optionally, eigenvectors
     !! of a complex generalized Hermitian-definite eigenproblem, of the form
     !! A*x=(lambda)*B*x,  A*Bx=(lambda)*x,  or B*A*x=(lambda)*x.  Here A and
     !! B are assumed to be Hermitian and B is also positive definite.
     !! Eigenvalues and eigenvectors can be selected by specifying either a
     !! range of values or a range of indices for the desired eigenvalues.
                m, w, z, ldz, work,lwork, rwork, iwork, ifail, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, range, uplo
           integer(ilp), intent(in) :: il, itype, iu, lda, ldb, ldz, lwork, n
           integer(ilp), intent(out) :: info, m
           real(dp), intent(in) :: abstol, vl, vu
           ! Array Arguments 
           integer(ilp), intent(out) :: ifail(*), iwork(*)
           real(dp), intent(out) :: rwork(*), w(*)
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(dp), intent(out) :: work(*), z(ldz,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: alleig, indeig, lquery, upper, valeig, wantz
           character :: trans
           integer(ilp) :: lwkopt, nb
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           upper = stdlib_lsame( uplo, 'U' )
           alleig = stdlib_lsame( range, 'A' )
           valeig = stdlib_lsame( range, 'V' )
           indeig = stdlib_lsame( range, 'I' )
           lquery = ( lwork==-1_ilp )
           info = 0_ilp
           if( itype<1_ilp .or. itype>3_ilp ) then
              info = -1_ilp
           else if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -2_ilp
           else if( .not.( alleig .or. valeig .or. indeig ) ) then
              info = -3_ilp
           else if( .not.( upper .or. stdlib_lsame( uplo, 'L' ) ) ) then
              info = -4_ilp
           else if( n<0_ilp ) then
              info = -5_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -7_ilp
           else if( ldb<max( 1_ilp, n ) ) then
              info = -9_ilp
           else
              if( valeig ) then
                 if( n>0_ilp .and. vu<=vl )info = -11_ilp
              else if( indeig ) then
                 if( il<1_ilp .or. il>max( 1_ilp, n ) ) then
                    info = -12_ilp
                 else if( iu<min( n, il ) .or. iu>n ) then
                    info = -13_ilp
                 end if
              end if
           end if
           if (info==0_ilp) then
              if (ldz<1_ilp .or. (wantz .and. ldz<n)) then
                 info = -18_ilp
              end if
           end if
           if( info==0_ilp ) then
              nb = stdlib_ilaenv( 1_ilp, 'ZHETRD', uplo, n, -1_ilp, -1_ilp, -1_ilp )
              lwkopt = max( 1_ilp, ( nb + 1_ilp )*n )
              work( 1_ilp ) = lwkopt
              if( lwork<max( 1_ilp, 2_ilp*n ) .and. .not.lquery ) then
                 info = -20_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZHEGVX', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           m = 0_ilp
           if( n==0_ilp ) then
              return
           end if
           ! form a cholesky factorization of b.
           call stdlib_zpotrf( uplo, n, b, ldb, info )
           if( info/=0_ilp ) then
              info = n + info
              return
           end if
           ! transform problem to standard eigenvalue problem and solve.
           call stdlib_zhegst( itype, uplo, n, a, lda, b, ldb, info )
           call stdlib_zheevx( jobz, range, uplo, n, a, lda, vl, vu, il, iu, abstol,m, w, z, ldz, &
                     work, lwork, rwork, iwork, ifail,info )
           if( wantz ) then
              ! backtransform eigenvectors to the original problem.
              if( info>0_ilp )m = info - 1_ilp
              if( itype==1_ilp .or. itype==2_ilp ) then
                 ! for a*x=(lambda)*b*x and a*b*x=(lambda)*x;
                 ! backtransform eigenvectors: x = inv(l)**h *y or inv(u)*y
                 if( upper ) then
                    trans = 'N'
                 else
                    trans = 'C'
                 end if
                 call stdlib_ztrsm( 'LEFT', uplo, trans, 'NON-UNIT', n, m, cone, b,ldb, z, ldz )
                           
              else if( itype==3_ilp ) then
                 ! for b*a*x=(lambda)*x;
                 ! backtransform eigenvectors: x = l*y or u**h *y
                 if( upper ) then
                    trans = 'C'
                 else
                    trans = 'N'
                 end if
                 call stdlib_ztrmm( 'LEFT', uplo, trans, 'NON-UNIT', n, m, cone, b,ldb, z, ldz )
                           
              end if
           end if
           ! set work(1) to optimal complex workspace size.
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_zhegvx




     module subroutine stdlib_chpgv( itype, jobz, uplo, n, ap, bp, w, z, ldz, work,rwork, info )
     !! CHPGV computes all the eigenvalues and, optionally, the eigenvectors
     !! of a complex generalized Hermitian-definite eigenproblem, of the form
     !! A*x=(lambda)*B*x,  A*Bx=(lambda)*x,  or B*A*x=(lambda)*x.
     !! Here A and B are assumed to be Hermitian, stored in packed format,
     !! and B is also positive definite.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: itype, ldz, n
           ! Array Arguments 
           real(sp), intent(out) :: rwork(*), w(*)
           complex(sp), intent(inout) :: ap(*), bp(*)
           complex(sp), intent(out) :: work(*), z(ldz,*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: upper, wantz
           character :: trans
           integer(ilp) :: j, neig
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           upper = stdlib_lsame( uplo, 'U' )
           info = 0_ilp
           if( itype<1_ilp .or. itype>3_ilp ) then
              info = -1_ilp
           else if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -2_ilp
           else if( .not.( upper .or. stdlib_lsame( uplo, 'L' ) ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( ldz<1_ilp .or. ( wantz .and. ldz<n ) ) then
              info = -9_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CHPGV ', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! form a cholesky factorization of b.
           call stdlib_cpptrf( uplo, n, bp, info )
           if( info/=0_ilp ) then
              info = n + info
              return
           end if
           ! transform problem to standard eigenvalue problem and solve.
           call stdlib_chpgst( itype, uplo, n, ap, bp, info )
           call stdlib_chpev( jobz, uplo, n, ap, w, z, ldz, work, rwork, info )
           if( wantz ) then
              ! backtransform eigenvectors to the original problem.
              neig = n
              if( info>0_ilp )neig = info - 1_ilp
              if( itype==1_ilp .or. itype==2_ilp ) then
                 ! for a*x=(lambda)*b*x and a*b*x=(lambda)*x;
                 ! backtransform eigenvectors: x = inv(l)**h*y or inv(u)*y
                 if( upper ) then
                    trans = 'N'
                 else
                    trans = 'C'
                 end if
                 do j = 1, neig
                    call stdlib_ctpsv( uplo, trans, 'NON-UNIT', n, bp, z( 1_ilp, j ),1_ilp )
                 end do
              else if( itype==3_ilp ) then
                 ! for b*a*x=(lambda)*x;
                 ! backtransform eigenvectors: x = l*y or u**h*y
                 if( upper ) then
                    trans = 'C'
                 else
                    trans = 'N'
                 end if
                 do j = 1, neig
                    call stdlib_ctpmv( uplo, trans, 'NON-UNIT', n, bp, z( 1_ilp, j ),1_ilp )
                 end do
              end if
           end if
           return
     end subroutine stdlib_chpgv

     module subroutine stdlib_zhpgv( itype, jobz, uplo, n, ap, bp, w, z, ldz, work,rwork, info )
     !! ZHPGV computes all the eigenvalues and, optionally, the eigenvectors
     !! of a complex generalized Hermitian-definite eigenproblem, of the form
     !! A*x=(lambda)*B*x,  A*Bx=(lambda)*x,  or B*A*x=(lambda)*x.
     !! Here A and B are assumed to be Hermitian, stored in packed format,
     !! and B is also positive definite.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: itype, ldz, n
           ! Array Arguments 
           real(dp), intent(out) :: rwork(*), w(*)
           complex(dp), intent(inout) :: ap(*), bp(*)
           complex(dp), intent(out) :: work(*), z(ldz,*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: upper, wantz
           character :: trans
           integer(ilp) :: j, neig
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           upper = stdlib_lsame( uplo, 'U' )
           info = 0_ilp
           if( itype<1_ilp .or. itype>3_ilp ) then
              info = -1_ilp
           else if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -2_ilp
           else if( .not.( upper .or. stdlib_lsame( uplo, 'L' ) ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( ldz<1_ilp .or. ( wantz .and. ldz<n ) ) then
              info = -9_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZHPGV ', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! form a cholesky factorization of b.
           call stdlib_zpptrf( uplo, n, bp, info )
           if( info/=0_ilp ) then
              info = n + info
              return
           end if
           ! transform problem to standard eigenvalue problem and solve.
           call stdlib_zhpgst( itype, uplo, n, ap, bp, info )
           call stdlib_zhpev( jobz, uplo, n, ap, w, z, ldz, work, rwork, info )
           if( wantz ) then
              ! backtransform eigenvectors to the original problem.
              neig = n
              if( info>0_ilp )neig = info - 1_ilp
              if( itype==1_ilp .or. itype==2_ilp ) then
                 ! for a*x=(lambda)*b*x and a*b*x=(lambda)*x;
                 ! backtransform eigenvectors: x = inv(l)**h *y or inv(u)*y
                 if( upper ) then
                    trans = 'N'
                 else
                    trans = 'C'
                 end if
                 do j = 1, neig
                    call stdlib_ztpsv( uplo, trans, 'NON-UNIT', n, bp, z( 1_ilp, j ),1_ilp )
                 end do
              else if( itype==3_ilp ) then
                 ! for b*a*x=(lambda)*x;
                 ! backtransform eigenvectors: x = l*y or u**h *y
                 if( upper ) then
                    trans = 'C'
                 else
                    trans = 'N'
                 end if
                 do j = 1, neig
                    call stdlib_ztpmv( uplo, trans, 'NON-UNIT', n, bp, z( 1_ilp, j ),1_ilp )
                 end do
              end if
           end if
           return
     end subroutine stdlib_zhpgv




     module subroutine stdlib_chpgvd( itype, jobz, uplo, n, ap, bp, w, z, ldz, work,lwork, rwork, lrwork,&
     !! CHPGVD computes all the eigenvalues and, optionally, the eigenvectors
     !! of a complex generalized Hermitian-definite eigenproblem, of the form
     !! A*x=(lambda)*B*x,  A*Bx=(lambda)*x,  or B*A*x=(lambda)*x.  Here A and
     !! B are assumed to be Hermitian, stored in packed format, and B is also
     !! positive definite.
     !! If eigenvectors are desired, it uses a divide and conquer algorithm.
     !! The divide and conquer algorithm makes very mild assumptions about
     !! floating point arithmetic. It will work on machines with a guard
     !! digit in add/subtract, or on those binary machines without guard
     !! digits which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or
     !! Cray-2. It could conceivably fail on hexadecimal or decimal machines
     !! without guard digits, but we know of none.
                iwork, liwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: itype, ldz, liwork, lrwork, lwork, n
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(out) :: rwork(*), w(*)
           complex(sp), intent(inout) :: ap(*), bp(*)
           complex(sp), intent(out) :: work(*), z(ldz,*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lquery, upper, wantz
           character :: trans
           integer(ilp) :: j, liwmin, lrwmin, lwmin, neig
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           upper = stdlib_lsame( uplo, 'U' )
           lquery = ( lwork==-1_ilp .or. lrwork==-1_ilp .or. liwork==-1_ilp )
           info = 0_ilp
           if( itype<1_ilp .or. itype>3_ilp ) then
              info = -1_ilp
           else if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -2_ilp
           else if( .not.( upper .or. stdlib_lsame( uplo, 'L' ) ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( ldz<1_ilp .or. ( wantz .and. ldz<n ) ) then
              info = -9_ilp
           end if
           if( info==0_ilp ) then
              if( n<=1_ilp ) then
                 lwmin = 1_ilp
                 liwmin = 1_ilp
                 lrwmin = 1_ilp
              else
                 if( wantz ) then
                    lwmin = 2_ilp*n
                    lrwmin = 1_ilp + 5_ilp*n + 2_ilp*n**2_ilp
                    liwmin = 3_ilp + 5_ilp*n
                 else
                    lwmin = n
                    lrwmin = n
                    liwmin = 1_ilp
                 end if
              end if
              work( 1_ilp ) = lwmin
              rwork( 1_ilp ) = lrwmin
              iwork( 1_ilp ) = liwmin
              if( lwork<lwmin .and. .not.lquery ) then
                 info = -11_ilp
              else if( lrwork<lrwmin .and. .not.lquery ) then
                 info = -13_ilp
              else if( liwork<liwmin .and. .not.lquery ) then
                 info = -15_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CHPGVD', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! form a cholesky factorization of b.
           call stdlib_cpptrf( uplo, n, bp, info )
           if( info/=0_ilp ) then
              info = n + info
              return
           end if
           ! transform problem to standard eigenvalue problem and solve.
           call stdlib_chpgst( itype, uplo, n, ap, bp, info )
           call stdlib_chpevd( jobz, uplo, n, ap, w, z, ldz, work, lwork, rwork,lrwork, iwork, &
                     liwork, info )
           lwmin = max( real( lwmin,KIND=sp), real( work( 1_ilp ),KIND=sp) )
           lrwmin = max( real( lrwmin,KIND=sp), real( rwork( 1_ilp ),KIND=sp) )
           liwmin = max( real( liwmin,KIND=sp), real( iwork( 1_ilp ),KIND=sp) )
           if( wantz ) then
              ! backtransform eigenvectors to the original problem.
              neig = n
              if( info>0_ilp )neig = info - 1_ilp
              if( itype==1_ilp .or. itype==2_ilp ) then
                 ! for a*x=(lambda)*b*x and a*b*x=(lambda)*x;
                 ! backtransform eigenvectors: x = inv(l)**h *y or inv(u)*y
                 if( upper ) then
                    trans = 'N'
                 else
                    trans = 'C'
                 end if
                 do j = 1, neig
                    call stdlib_ctpsv( uplo, trans, 'NON-UNIT', n, bp, z( 1_ilp, j ),1_ilp )
                 end do
              else if( itype==3_ilp ) then
                 ! for b*a*x=(lambda)*x;
                 ! backtransform eigenvectors: x = l*y or u**h *y
                 if( upper ) then
                    trans = 'C'
                 else
                    trans = 'N'
                 end if
                 do j = 1, neig
                    call stdlib_ctpmv( uplo, trans, 'NON-UNIT', n, bp, z( 1_ilp, j ),1_ilp )
                 end do
              end if
           end if
           work( 1_ilp ) = lwmin
           rwork( 1_ilp ) = lrwmin
           iwork( 1_ilp ) = liwmin
           return
     end subroutine stdlib_chpgvd

     module subroutine stdlib_zhpgvd( itype, jobz, uplo, n, ap, bp, w, z, ldz, work,lwork, rwork, lrwork,&
     !! ZHPGVD computes all the eigenvalues and, optionally, the eigenvectors
     !! of a complex generalized Hermitian-definite eigenproblem, of the form
     !! A*x=(lambda)*B*x,  A*Bx=(lambda)*x,  or B*A*x=(lambda)*x.  Here A and
     !! B are assumed to be Hermitian, stored in packed format, and B is also
     !! positive definite.
     !! If eigenvectors are desired, it uses a divide and conquer algorithm.
     !! The divide and conquer algorithm makes very mild assumptions about
     !! floating point arithmetic. It will work on machines with a guard
     !! digit in add/subtract, or on those binary machines without guard
     !! digits which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or
     !! Cray-2. It could conceivably fail on hexadecimal or decimal machines
     !! without guard digits, but we know of none.
                iwork, liwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: itype, ldz, liwork, lrwork, lwork, n
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(out) :: rwork(*), w(*)
           complex(dp), intent(inout) :: ap(*), bp(*)
           complex(dp), intent(out) :: work(*), z(ldz,*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lquery, upper, wantz
           character :: trans
           integer(ilp) :: j, liwmin, lrwmin, lwmin, neig
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           upper = stdlib_lsame( uplo, 'U' )
           lquery = ( lwork==-1_ilp .or. lrwork==-1_ilp .or. liwork==-1_ilp )
           info = 0_ilp
           if( itype<1_ilp .or. itype>3_ilp ) then
              info = -1_ilp
           else if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -2_ilp
           else if( .not.( upper .or. stdlib_lsame( uplo, 'L' ) ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( ldz<1_ilp .or. ( wantz .and. ldz<n ) ) then
              info = -9_ilp
           end if
           if( info==0_ilp ) then
              if( n<=1_ilp ) then
                 lwmin = 1_ilp
                 liwmin = 1_ilp
                 lrwmin = 1_ilp
              else
                 if( wantz ) then
                    lwmin = 2_ilp*n
                    lrwmin = 1_ilp + 5_ilp*n + 2_ilp*n**2_ilp
                    liwmin = 3_ilp + 5_ilp*n
                 else
                    lwmin = n
                    lrwmin = n
                    liwmin = 1_ilp
                 end if
              end if
              work( 1_ilp ) = lwmin
              rwork( 1_ilp ) = lrwmin
              iwork( 1_ilp ) = liwmin
              if( lwork<lwmin .and. .not.lquery ) then
                 info = -11_ilp
              else if( lrwork<lrwmin .and. .not.lquery ) then
                 info = -13_ilp
              else if( liwork<liwmin .and. .not.lquery ) then
                 info = -15_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZHPGVD', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! form a cholesky factorization of b.
           call stdlib_zpptrf( uplo, n, bp, info )
           if( info/=0_ilp ) then
              info = n + info
              return
           end if
           ! transform problem to standard eigenvalue problem and solve.
           call stdlib_zhpgst( itype, uplo, n, ap, bp, info )
           call stdlib_zhpevd( jobz, uplo, n, ap, w, z, ldz, work, lwork, rwork,lrwork, iwork, &
                     liwork, info )
           lwmin = max( real( lwmin,KIND=dp), real( work( 1_ilp ),KIND=dp) )
           lrwmin = max( real( lrwmin,KIND=dp), real( rwork( 1_ilp ),KIND=dp) )
           liwmin = max( real( liwmin,KIND=dp), real( iwork( 1_ilp ),KIND=dp) )
           if( wantz ) then
              ! backtransform eigenvectors to the original problem.
              neig = n
              if( info>0_ilp )neig = info - 1_ilp
              if( itype==1_ilp .or. itype==2_ilp ) then
                 ! for a*x=(lambda)*b*x and a*b*x=(lambda)*x;
                 ! backtransform eigenvectors: x = inv(l)**h *y or inv(u)*y
                 if( upper ) then
                    trans = 'N'
                 else
                    trans = 'C'
                 end if
                 do j = 1, neig
                    call stdlib_ztpsv( uplo, trans, 'NON-UNIT', n, bp, z( 1_ilp, j ),1_ilp )
                 end do
              else if( itype==3_ilp ) then
                 ! for b*a*x=(lambda)*x;
                 ! backtransform eigenvectors: x = l*y or u**h *y
                 if( upper ) then
                    trans = 'C'
                 else
                    trans = 'N'
                 end if
                 do j = 1, neig
                    call stdlib_ztpmv( uplo, trans, 'NON-UNIT', n, bp, z( 1_ilp, j ),1_ilp )
                 end do
              end if
           end if
           work( 1_ilp ) = lwmin
           rwork( 1_ilp ) = lrwmin
           iwork( 1_ilp ) = liwmin
           return
     end subroutine stdlib_zhpgvd




     module subroutine stdlib_chpgvx( itype, jobz, range, uplo, n, ap, bp, vl, vu,il, iu, abstol, m, w, &
     !! CHPGVX computes selected eigenvalues and, optionally, eigenvectors
     !! of a complex generalized Hermitian-definite eigenproblem, of the form
     !! A*x=(lambda)*B*x,  A*Bx=(lambda)*x,  or B*A*x=(lambda)*x.  Here A and
     !! B are assumed to be Hermitian, stored in packed format, and B is also
     !! positive definite.  Eigenvalues and eigenvectors can be selected by
     !! specifying either a range of values or a range of indices for the
     !! desired eigenvalues.
               z, ldz, work, rwork,iwork, ifail, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, range, uplo
           integer(ilp), intent(in) :: il, itype, iu, ldz, n
           integer(ilp), intent(out) :: info, m
           real(sp), intent(in) :: abstol, vl, vu
           ! Array Arguments 
           integer(ilp), intent(out) :: ifail(*), iwork(*)
           real(sp), intent(out) :: rwork(*), w(*)
           complex(sp), intent(inout) :: ap(*), bp(*)
           complex(sp), intent(out) :: work(*), z(ldz,*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: alleig, indeig, upper, valeig, wantz
           character :: trans
           integer(ilp) :: j
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           upper = stdlib_lsame( uplo, 'U' )
           alleig = stdlib_lsame( range, 'A' )
           valeig = stdlib_lsame( range, 'V' )
           indeig = stdlib_lsame( range, 'I' )
           info = 0_ilp
           if( itype<1_ilp .or. itype>3_ilp ) then
              info = -1_ilp
           else if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -2_ilp
           else if( .not.( alleig .or. valeig .or. indeig ) ) then
              info = -3_ilp
           else if( .not.( upper .or. stdlib_lsame( uplo, 'L' ) ) ) then
              info = -4_ilp
           else if( n<0_ilp ) then
              info = -5_ilp
           else
              if( valeig ) then
                 if( n>0_ilp .and. vu<=vl ) then
                    info = -9_ilp
                 end if
              else if( indeig ) then
                 if( il<1_ilp ) then
                    info = -10_ilp
                 else if( iu<min( n, il ) .or. iu>n ) then
                    info = -11_ilp
                 end if
              end if
           end if
           if( info==0_ilp ) then
              if( ldz<1_ilp .or. ( wantz .and. ldz<n ) ) then
                 info = -16_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CHPGVX', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! form a cholesky factorization of b.
           call stdlib_cpptrf( uplo, n, bp, info )
           if( info/=0_ilp ) then
              info = n + info
              return
           end if
           ! transform problem to standard eigenvalue problem and solve.
           call stdlib_chpgst( itype, uplo, n, ap, bp, info )
           call stdlib_chpevx( jobz, range, uplo, n, ap, vl, vu, il, iu, abstol, m,w, z, ldz, &
                     work, rwork, iwork, ifail, info )
           if( wantz ) then
              ! backtransform eigenvectors to the original problem.
              if( info>0_ilp )m = info - 1_ilp
              if( itype==1_ilp .or. itype==2_ilp ) then
                 ! for a*x=(lambda)*b*x and a*b*x=(lambda)*x;
                 ! backtransform eigenvectors: x = inv(l)**h*y or inv(u)*y
                 if( upper ) then
                    trans = 'N'
                 else
                    trans = 'C'
                 end if
                 do j = 1, m
                    call stdlib_ctpsv( uplo, trans, 'NON-UNIT', n, bp, z( 1_ilp, j ),1_ilp )
                 end do
              else if( itype==3_ilp ) then
                 ! for b*a*x=(lambda)*x;
                 ! backtransform eigenvectors: x = l*y or u**h*y
                 if( upper ) then
                    trans = 'C'
                 else
                    trans = 'N'
                 end if
                 do j = 1, m
                    call stdlib_ctpmv( uplo, trans, 'NON-UNIT', n, bp, z( 1_ilp, j ),1_ilp )
                 end do
              end if
           end if
           return
     end subroutine stdlib_chpgvx

     module subroutine stdlib_zhpgvx( itype, jobz, range, uplo, n, ap, bp, vl, vu,il, iu, abstol, m, w, &
     !! ZHPGVX computes selected eigenvalues and, optionally, eigenvectors
     !! of a complex generalized Hermitian-definite eigenproblem, of the form
     !! A*x=(lambda)*B*x,  A*Bx=(lambda)*x,  or B*A*x=(lambda)*x.  Here A and
     !! B are assumed to be Hermitian, stored in packed format, and B is also
     !! positive definite.  Eigenvalues and eigenvectors can be selected by
     !! specifying either a range of values or a range of indices for the
     !! desired eigenvalues.
               z, ldz, work, rwork,iwork, ifail, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, range, uplo
           integer(ilp), intent(in) :: il, itype, iu, ldz, n
           integer(ilp), intent(out) :: info, m
           real(dp), intent(in) :: abstol, vl, vu
           ! Array Arguments 
           integer(ilp), intent(out) :: ifail(*), iwork(*)
           real(dp), intent(out) :: rwork(*), w(*)
           complex(dp), intent(inout) :: ap(*), bp(*)
           complex(dp), intent(out) :: work(*), z(ldz,*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: alleig, indeig, upper, valeig, wantz
           character :: trans
           integer(ilp) :: j
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           upper = stdlib_lsame( uplo, 'U' )
           alleig = stdlib_lsame( range, 'A' )
           valeig = stdlib_lsame( range, 'V' )
           indeig = stdlib_lsame( range, 'I' )
           info = 0_ilp
           if( itype<1_ilp .or. itype>3_ilp ) then
              info = -1_ilp
           else if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -2_ilp
           else if( .not.( alleig .or. valeig .or. indeig ) ) then
              info = -3_ilp
           else if( .not.( upper .or. stdlib_lsame( uplo, 'L' ) ) ) then
              info = -4_ilp
           else if( n<0_ilp ) then
              info = -5_ilp
           else
              if( valeig ) then
                 if( n>0_ilp .and. vu<=vl ) then
                    info = -9_ilp
                 end if
              else if( indeig ) then
                 if( il<1_ilp ) then
                    info = -10_ilp
                 else if( iu<min( n, il ) .or. iu>n ) then
                    info = -11_ilp
                 end if
              end if
           end if
           if( info==0_ilp ) then
              if( ldz<1_ilp .or. ( wantz .and. ldz<n ) ) then
                 info = -16_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZHPGVX', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! form a cholesky factorization of b.
           call stdlib_zpptrf( uplo, n, bp, info )
           if( info/=0_ilp ) then
              info = n + info
              return
           end if
           ! transform problem to standard eigenvalue problem and solve.
           call stdlib_zhpgst( itype, uplo, n, ap, bp, info )
           call stdlib_zhpevx( jobz, range, uplo, n, ap, vl, vu, il, iu, abstol, m,w, z, ldz, &
                     work, rwork, iwork, ifail, info )
           if( wantz ) then
              ! backtransform eigenvectors to the original problem.
              if( info>0_ilp )m = info - 1_ilp
              if( itype==1_ilp .or. itype==2_ilp ) then
                 ! for a*x=(lambda)*b*x and a*b*x=(lambda)*x;
                 ! backtransform eigenvectors: x = inv(l)**h *y or inv(u)*y
                 if( upper ) then
                    trans = 'N'
                 else
                    trans = 'C'
                 end if
                 do j = 1, m
                    call stdlib_ztpsv( uplo, trans, 'NON-UNIT', n, bp, z( 1_ilp, j ),1_ilp )
                 end do
              else if( itype==3_ilp ) then
                 ! for b*a*x=(lambda)*x;
                 ! backtransform eigenvectors: x = l*y or u**h *y
                 if( upper ) then
                    trans = 'C'
                 else
                    trans = 'N'
                 end if
                 do j = 1, m
                    call stdlib_ztpmv( uplo, trans, 'NON-UNIT', n, bp, z( 1_ilp, j ),1_ilp )
                 end do
              end if
           end if
           return
     end subroutine stdlib_zhpgvx




     pure module subroutine stdlib_chbgv( jobz, uplo, n, ka, kb, ab, ldab, bb, ldbb, w, z,ldz, work, &
     !! CHBGV computes all the eigenvalues, and optionally, the eigenvectors
     !! of a complex generalized Hermitian-definite banded eigenproblem, of
     !! the form A*x=(lambda)*B*x. Here A and B are assumed to be Hermitian
     !! and banded, and B is also positive definite.
               rwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ka, kb, ldab, ldbb, ldz, n
           ! Array Arguments 
           real(sp), intent(out) :: rwork(*), w(*)
           complex(sp), intent(inout) :: ab(ldab,*), bb(ldbb,*)
           complex(sp), intent(out) :: work(*), z(ldz,*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: upper, wantz
           character :: vect
           integer(ilp) :: iinfo, inde, indwrk
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           upper = stdlib_lsame( uplo, 'U' )
           info = 0_ilp
           if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -1_ilp
           else if( .not.( upper .or. stdlib_lsame( uplo, 'L' ) ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( ka<0_ilp ) then
              info = -4_ilp
           else if( kb<0_ilp .or. kb>ka ) then
              info = -5_ilp
           else if( ldab<ka+1 ) then
              info = -7_ilp
           else if( ldbb<kb+1 ) then
              info = -9_ilp
           else if( ldz<1_ilp .or. ( wantz .and. ldz<n ) ) then
              info = -12_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CHBGV ', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! form a split cholesky factorization of b.
           call stdlib_cpbstf( uplo, n, kb, bb, ldbb, info )
           if( info/=0_ilp ) then
              info = n + info
              return
           end if
           ! transform problem to standard eigenvalue problem.
           inde = 1_ilp
           indwrk = inde + n
           call stdlib_chbgst( jobz, uplo, n, ka, kb, ab, ldab, bb, ldbb, z, ldz,work, rwork( &
                     indwrk ), iinfo )
           ! reduce to tridiagonal form.
           if( wantz ) then
              vect = 'U'
           else
              vect = 'N'
           end if
           call stdlib_chbtrd( vect, uplo, n, ka, ab, ldab, w, rwork( inde ), z,ldz, work, iinfo )
                     
           ! for eigenvalues only, call stdlib_ssterf.  for eigenvectors, call stdlib_csteqr.
           if( .not.wantz ) then
              call stdlib_ssterf( n, w, rwork( inde ), info )
           else
              call stdlib_csteqr( jobz, n, w, rwork( inde ), z, ldz,rwork( indwrk ), info )
                        
           end if
           return
     end subroutine stdlib_chbgv

     pure module subroutine stdlib_zhbgv( jobz, uplo, n, ka, kb, ab, ldab, bb, ldbb, w, z,ldz, work, &
     !! ZHBGV computes all the eigenvalues, and optionally, the eigenvectors
     !! of a complex generalized Hermitian-definite banded eigenproblem, of
     !! the form A*x=(lambda)*B*x. Here A and B are assumed to be Hermitian
     !! and banded, and B is also positive definite.
               rwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ka, kb, ldab, ldbb, ldz, n
           ! Array Arguments 
           real(dp), intent(out) :: rwork(*), w(*)
           complex(dp), intent(inout) :: ab(ldab,*), bb(ldbb,*)
           complex(dp), intent(out) :: work(*), z(ldz,*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: upper, wantz
           character :: vect
           integer(ilp) :: iinfo, inde, indwrk
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           upper = stdlib_lsame( uplo, 'U' )
           info = 0_ilp
           if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -1_ilp
           else if( .not.( upper .or. stdlib_lsame( uplo, 'L' ) ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( ka<0_ilp ) then
              info = -4_ilp
           else if( kb<0_ilp .or. kb>ka ) then
              info = -5_ilp
           else if( ldab<ka+1 ) then
              info = -7_ilp
           else if( ldbb<kb+1 ) then
              info = -9_ilp
           else if( ldz<1_ilp .or. ( wantz .and. ldz<n ) ) then
              info = -12_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZHBGV ', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! form a split cholesky factorization of b.
           call stdlib_zpbstf( uplo, n, kb, bb, ldbb, info )
           if( info/=0_ilp ) then
              info = n + info
              return
           end if
           ! transform problem to standard eigenvalue problem.
           inde = 1_ilp
           indwrk = inde + n
           call stdlib_zhbgst( jobz, uplo, n, ka, kb, ab, ldab, bb, ldbb, z, ldz,work, rwork( &
                     indwrk ), iinfo )
           ! reduce to tridiagonal form.
           if( wantz ) then
              vect = 'U'
           else
              vect = 'N'
           end if
           call stdlib_zhbtrd( vect, uplo, n, ka, ab, ldab, w, rwork( inde ), z,ldz, work, iinfo )
                     
           ! for eigenvalues only, call stdlib_dsterf.  for eigenvectors, call stdlib_zsteqr.
           if( .not.wantz ) then
              call stdlib_dsterf( n, w, rwork( inde ), info )
           else
              call stdlib_zsteqr( jobz, n, w, rwork( inde ), z, ldz,rwork( indwrk ), info )
                        
           end if
           return
     end subroutine stdlib_zhbgv




     pure module subroutine stdlib_chbgvd( jobz, uplo, n, ka, kb, ab, ldab, bb, ldbb, w,z, ldz, work, &
     !! CHBGVD computes all the eigenvalues, and optionally, the eigenvectors
     !! of a complex generalized Hermitian-definite banded eigenproblem, of
     !! the form A*x=(lambda)*B*x. Here A and B are assumed to be Hermitian
     !! and banded, and B is also positive definite.  If eigenvectors are
     !! desired, it uses a divide and conquer algorithm.
     !! The divide and conquer algorithm makes very mild assumptions about
     !! floating point arithmetic. It will work on machines with a guard
     !! digit in add/subtract, or on those binary machines without guard
     !! digits which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or
     !! Cray-2. It could conceivably fail on hexadecimal or decimal machines
     !! without guard digits, but we know of none.
               lwork, rwork, lrwork, iwork,liwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ka, kb, ldab, ldbb, ldz, liwork, lrwork, lwork, n
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(out) :: rwork(*), w(*)
           complex(sp), intent(inout) :: ab(ldab,*), bb(ldbb,*)
           complex(sp), intent(out) :: work(*), z(ldz,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery, upper, wantz
           character :: vect
           integer(ilp) :: iinfo, inde, indwk2, indwrk, liwmin, llrwk, llwk2, lrwmin, &
                     lwmin
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           upper = stdlib_lsame( uplo, 'U' )
           lquery = ( lwork==-1_ilp .or. lrwork==-1_ilp .or. liwork==-1_ilp )
           info = 0_ilp
           if( n<=1_ilp ) then
              lwmin = 1_ilp+n
              lrwmin = 1_ilp+n
              liwmin = 1_ilp
           else if( wantz ) then
              lwmin = 2_ilp*n**2_ilp
              lrwmin = 1_ilp + 5_ilp*n + 2_ilp*n**2_ilp
              liwmin = 3_ilp + 5_ilp*n
           else
              lwmin = n
              lrwmin = n
              liwmin = 1_ilp
           end if
           if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -1_ilp
           else if( .not.( upper .or. stdlib_lsame( uplo, 'L' ) ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( ka<0_ilp ) then
              info = -4_ilp
           else if( kb<0_ilp .or. kb>ka ) then
              info = -5_ilp
           else if( ldab<ka+1 ) then
              info = -7_ilp
           else if( ldbb<kb+1 ) then
              info = -9_ilp
           else if( ldz<1_ilp .or. ( wantz .and. ldz<n ) ) then
              info = -12_ilp
           end if
           if( info==0_ilp ) then
              work( 1_ilp ) = lwmin
              rwork( 1_ilp ) = lrwmin
              iwork( 1_ilp ) = liwmin
              if( lwork<lwmin .and. .not.lquery ) then
                 info = -14_ilp
              else if( lrwork<lrwmin .and. .not.lquery ) then
                 info = -16_ilp
              else if( liwork<liwmin .and. .not.lquery ) then
                 info = -18_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CHBGVD', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! form a split cholesky factorization of b.
           call stdlib_cpbstf( uplo, n, kb, bb, ldbb, info )
           if( info/=0_ilp ) then
              info = n + info
              return
           end if
           ! transform problem to standard eigenvalue problem.
           inde = 1_ilp
           indwrk = inde + n
           indwk2 = 1_ilp + n*n
           llwk2 = lwork - indwk2 + 2_ilp
           llrwk = lrwork - indwrk + 2_ilp
           call stdlib_chbgst( jobz, uplo, n, ka, kb, ab, ldab, bb, ldbb, z, ldz,work, rwork, &
                     iinfo )
           ! reduce hermitian band matrix to tridiagonal form.
           if( wantz ) then
              vect = 'U'
           else
              vect = 'N'
           end if
           call stdlib_chbtrd( vect, uplo, n, ka, ab, ldab, w, rwork( inde ), z,ldz, work, iinfo )
                     
           ! for eigenvalues only, call stdlib_ssterf.  for eigenvectors, call stdlib_cstedc.
           if( .not.wantz ) then
              call stdlib_ssterf( n, w, rwork( inde ), info )
           else
              call stdlib_cstedc( 'I', n, w, rwork( inde ), work, n, work( indwk2 ),llwk2, rwork( &
                        indwrk ), llrwk, iwork, liwork,info )
              call stdlib_cgemm( 'N', 'N', n, n, n, cone, z, ldz, work, n, czero,work( indwk2 ), &
                        n )
              call stdlib_clacpy( 'A', n, n, work( indwk2 ), n, z, ldz )
           end if
           work( 1_ilp ) = lwmin
           rwork( 1_ilp ) = lrwmin
           iwork( 1_ilp ) = liwmin
           return
     end subroutine stdlib_chbgvd

     pure module subroutine stdlib_zhbgvd( jobz, uplo, n, ka, kb, ab, ldab, bb, ldbb, w,z, ldz, work, &
     !! ZHBGVD computes all the eigenvalues, and optionally, the eigenvectors
     !! of a complex generalized Hermitian-definite banded eigenproblem, of
     !! the form A*x=(lambda)*B*x. Here A and B are assumed to be Hermitian
     !! and banded, and B is also positive definite.  If eigenvectors are
     !! desired, it uses a divide and conquer algorithm.
     !! The divide and conquer algorithm makes very mild assumptions about
     !! floating point arithmetic. It will work on machines with a guard
     !! digit in add/subtract, or on those binary machines without guard
     !! digits which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or
     !! Cray-2. It could conceivably fail on hexadecimal or decimal machines
     !! without guard digits, but we know of none.
               lwork, rwork, lrwork, iwork,liwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ka, kb, ldab, ldbb, ldz, liwork, lrwork, lwork, n
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(out) :: rwork(*), w(*)
           complex(dp), intent(inout) :: ab(ldab,*), bb(ldbb,*)
           complex(dp), intent(out) :: work(*), z(ldz,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery, upper, wantz
           character :: vect
           integer(ilp) :: iinfo, inde, indwk2, indwrk, liwmin, llrwk, llwk2, lrwmin, &
                     lwmin
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           upper = stdlib_lsame( uplo, 'U' )
           lquery = ( lwork==-1_ilp .or. lrwork==-1_ilp .or. liwork==-1_ilp )
           info = 0_ilp
           if( n<=1_ilp ) then
              lwmin = 1_ilp+n
              lrwmin = 1_ilp+n
              liwmin = 1_ilp
           else if( wantz ) then
              lwmin = 2_ilp*n**2_ilp
              lrwmin = 1_ilp + 5_ilp*n + 2_ilp*n**2_ilp
              liwmin = 3_ilp + 5_ilp*n
           else
              lwmin = n
              lrwmin = n
              liwmin = 1_ilp
           end if
           if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -1_ilp
           else if( .not.( upper .or. stdlib_lsame( uplo, 'L' ) ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( ka<0_ilp ) then
              info = -4_ilp
           else if( kb<0_ilp .or. kb>ka ) then
              info = -5_ilp
           else if( ldab<ka+1 ) then
              info = -7_ilp
           else if( ldbb<kb+1 ) then
              info = -9_ilp
           else if( ldz<1_ilp .or. ( wantz .and. ldz<n ) ) then
              info = -12_ilp
           end if
           if( info==0_ilp ) then
              work( 1_ilp ) = lwmin
              rwork( 1_ilp ) = lrwmin
              iwork( 1_ilp ) = liwmin
              if( lwork<lwmin .and. .not.lquery ) then
                 info = -14_ilp
              else if( lrwork<lrwmin .and. .not.lquery ) then
                 info = -16_ilp
              else if( liwork<liwmin .and. .not.lquery ) then
                 info = -18_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZHBGVD', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! form a split cholesky factorization of b.
           call stdlib_zpbstf( uplo, n, kb, bb, ldbb, info )
           if( info/=0_ilp ) then
              info = n + info
              return
           end if
           ! transform problem to standard eigenvalue problem.
           inde = 1_ilp
           indwrk = inde + n
           indwk2 = 1_ilp + n*n
           llwk2 = lwork - indwk2 + 2_ilp
           llrwk = lrwork - indwrk + 2_ilp
           call stdlib_zhbgst( jobz, uplo, n, ka, kb, ab, ldab, bb, ldbb, z, ldz,work, rwork, &
                     iinfo )
           ! reduce hermitian band matrix to tridiagonal form.
           if( wantz ) then
              vect = 'U'
           else
              vect = 'N'
           end if
           call stdlib_zhbtrd( vect, uplo, n, ka, ab, ldab, w, rwork( inde ), z,ldz, work, iinfo )
                     
           ! for eigenvalues only, call stdlib_dsterf.  for eigenvectors, call stdlib_zstedc.
           if( .not.wantz ) then
              call stdlib_dsterf( n, w, rwork( inde ), info )
           else
              call stdlib_zstedc( 'I', n, w, rwork( inde ), work, n, work( indwk2 ),llwk2, rwork( &
                        indwrk ), llrwk, iwork, liwork,info )
              call stdlib_zgemm( 'N', 'N', n, n, n, cone, z, ldz, work, n, czero,work( indwk2 ), &
                        n )
              call stdlib_zlacpy( 'A', n, n, work( indwk2 ), n, z, ldz )
           end if
           work( 1_ilp ) = lwmin
           rwork( 1_ilp ) = lrwmin
           iwork( 1_ilp ) = liwmin
           return
     end subroutine stdlib_zhbgvd




     pure module subroutine stdlib_chbgvx( jobz, range, uplo, n, ka, kb, ab, ldab, bb,ldbb, q, ldq, vl, &
     !! CHBGVX computes all the eigenvalues, and optionally, the eigenvectors
     !! of a complex generalized Hermitian-definite banded eigenproblem, of
     !! the form A*x=(lambda)*B*x. Here A and B are assumed to be Hermitian
     !! and banded, and B is also positive definite.  Eigenvalues and
     !! eigenvectors can be selected by specifying either all eigenvalues,
     !! a range of values or a range of indices for the desired eigenvalues.
               vu, il, iu, abstol, m, w, z,ldz, work, rwork, iwork, ifail, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, range, uplo
           integer(ilp), intent(in) :: il, iu, ka, kb, ldab, ldbb, ldq, ldz, n
           integer(ilp), intent(out) :: info, m
           real(sp), intent(in) :: abstol, vl, vu
           ! Array Arguments 
           integer(ilp), intent(out) :: ifail(*), iwork(*)
           real(sp), intent(out) :: rwork(*), w(*)
           complex(sp), intent(inout) :: ab(ldab,*), bb(ldbb,*)
           complex(sp), intent(out) :: q(ldq,*), work(*), z(ldz,*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: alleig, indeig, test, upper, valeig, wantz
           character :: order, vect
           integer(ilp) :: i, iinfo, indd, inde, indee, indibl, indisp, indiwk, indrwk, indwrk, &
                     itmp1, j, jj, nsplit
           real(sp) :: tmp1
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           upper = stdlib_lsame( uplo, 'U' )
           alleig = stdlib_lsame( range, 'A' )
           valeig = stdlib_lsame( range, 'V' )
           indeig = stdlib_lsame( range, 'I' )
           info = 0_ilp
           if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -1_ilp
           else if( .not.( alleig .or. valeig .or. indeig ) ) then
              info = -2_ilp
           else if( .not.( upper .or. stdlib_lsame( uplo, 'L' ) ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( ka<0_ilp ) then
              info = -5_ilp
           else if( kb<0_ilp .or. kb>ka ) then
              info = -6_ilp
           else if( ldab<ka+1 ) then
              info = -8_ilp
           else if( ldbb<kb+1 ) then
              info = -10_ilp
           else if( ldq<1_ilp .or. ( wantz .and. ldq<n ) ) then
              info = -12_ilp
           else
              if( valeig ) then
                 if( n>0_ilp .and. vu<=vl )info = -14_ilp
              else if( indeig ) then
                 if( il<1_ilp .or. il>max( 1_ilp, n ) ) then
                    info = -15_ilp
                 else if ( iu<min( n, il ) .or. iu>n ) then
                    info = -16_ilp
                 end if
              end if
           end if
           if( info==0_ilp) then
              if( ldz<1_ilp .or. ( wantz .and. ldz<n ) ) then
                 info = -21_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CHBGVX', -info )
              return
           end if
           ! quick return if possible
           m = 0_ilp
           if( n==0 )return
           ! form a split cholesky factorization of b.
           call stdlib_cpbstf( uplo, n, kb, bb, ldbb, info )
           if( info/=0_ilp ) then
              info = n + info
              return
           end if
           ! transform problem to standard eigenvalue problem.
           call stdlib_chbgst( jobz, uplo, n, ka, kb, ab, ldab, bb, ldbb, q, ldq,work, rwork, &
                     iinfo )
           ! solve the standard eigenvalue problem.
           ! reduce hermitian band matrix to tridiagonal form.
           indd = 1_ilp
           inde = indd + n
           indrwk = inde + n
           indwrk = 1_ilp
           if( wantz ) then
              vect = 'U'
           else
              vect = 'N'
           end if
           call stdlib_chbtrd( vect, uplo, n, ka, ab, ldab, rwork( indd ),rwork( inde ), q, ldq, &
                     work( indwrk ), iinfo )
           ! if all eigenvalues are desired and abstol is less than or equal
           ! to zero, then call stdlib_ssterf or stdlib_csteqr.  if this fails for some
           ! eigenvalue, then try stdlib_sstebz.
           test = .false.
           if( indeig ) then
              if( il==1_ilp .and. iu==n ) then
                 test = .true.
              end if
           end if
           if( ( alleig .or. test ) .and. ( abstol<=zero ) ) then
              call stdlib_scopy( n, rwork( indd ), 1_ilp, w, 1_ilp )
              indee = indrwk + 2_ilp*n
              call stdlib_scopy( n-1, rwork( inde ), 1_ilp, rwork( indee ), 1_ilp )
              if( .not.wantz ) then
                 call stdlib_ssterf( n, w, rwork( indee ), info )
              else
                 call stdlib_clacpy( 'A', n, n, q, ldq, z, ldz )
                 call stdlib_csteqr( jobz, n, w, rwork( indee ), z, ldz,rwork( indrwk ), info )
                           
                 if( info==0_ilp ) then
                    do i = 1, n
                       ifail( i ) = 0_ilp
                    end do
                 end if
              end if
              if( info==0_ilp ) then
                 m = n
                 go to 30
              end if
              info = 0_ilp
           end if
           ! otherwise, call stdlib_sstebz and, if eigenvectors are desired,
           ! call stdlib_cstein.
           if( wantz ) then
              order = 'B'
           else
              order = 'E'
           end if
           indibl = 1_ilp
           indisp = indibl + n
           indiwk = indisp + n
           call stdlib_sstebz( range, order, n, vl, vu, il, iu, abstol,rwork( indd ), rwork( inde &
           ), m, nsplit, w,iwork( indibl ), iwork( indisp ), rwork( indrwk ),iwork( indiwk ), &
                     info )
           if( wantz ) then
              call stdlib_cstein( n, rwork( indd ), rwork( inde ), m, w,iwork( indibl ), iwork( &
                        indisp ), z, ldz,rwork( indrwk ), iwork( indiwk ), ifail, info )
              ! apply unitary matrix used in reduction to tridiagonal
              ! form to eigenvectors returned by stdlib_cstein.
              do j = 1, m
                 call stdlib_ccopy( n, z( 1_ilp, j ), 1_ilp, work( 1_ilp ), 1_ilp )
                 call stdlib_cgemv( 'N', n, n, cone, q, ldq, work, 1_ilp, czero,z( 1_ilp, j ), 1_ilp )
              end do
           end if
           30 continue
           ! if eigenvalues are not in order, then sort them, along with
           ! eigenvectors.
           if( wantz ) then
              do j = 1, m - 1
                 i = 0_ilp
                 tmp1 = w( j )
                 do jj = j + 1, m
                    if( w( jj )<tmp1 ) then
                       i = jj
                       tmp1 = w( jj )
                    end if
                 end do
                 if( i/=0_ilp ) then
                    itmp1 = iwork( indibl+i-1 )
                    w( i ) = w( j )
                    iwork( indibl+i-1 ) = iwork( indibl+j-1 )
                    w( j ) = tmp1
                    iwork( indibl+j-1 ) = itmp1
                    call stdlib_cswap( n, z( 1_ilp, i ), 1_ilp, z( 1_ilp, j ), 1_ilp )
                    if( info/=0_ilp ) then
                       itmp1 = ifail( i )
                       ifail( i ) = ifail( j )
                       ifail( j ) = itmp1
                    end if
                 end if
              end do
           end if
           return
     end subroutine stdlib_chbgvx

     pure module subroutine stdlib_zhbgvx( jobz, range, uplo, n, ka, kb, ab, ldab, bb,ldbb, q, ldq, vl, &
     !! ZHBGVX computes all the eigenvalues, and optionally, the eigenvectors
     !! of a complex generalized Hermitian-definite banded eigenproblem, of
     !! the form A*x=(lambda)*B*x. Here A and B are assumed to be Hermitian
     !! and banded, and B is also positive definite.  Eigenvalues and
     !! eigenvectors can be selected by specifying either all eigenvalues,
     !! a range of values or a range of indices for the desired eigenvalues.
               vu, il, iu, abstol, m, w, z,ldz, work, rwork, iwork, ifail, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, range, uplo
           integer(ilp), intent(in) :: il, iu, ka, kb, ldab, ldbb, ldq, ldz, n
           integer(ilp), intent(out) :: info, m
           real(dp), intent(in) :: abstol, vl, vu
           ! Array Arguments 
           integer(ilp), intent(out) :: ifail(*), iwork(*)
           real(dp), intent(out) :: rwork(*), w(*)
           complex(dp), intent(inout) :: ab(ldab,*), bb(ldbb,*)
           complex(dp), intent(out) :: q(ldq,*), work(*), z(ldz,*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: alleig, indeig, test, upper, valeig, wantz
           character :: order, vect
           integer(ilp) :: i, iinfo, indd, inde, indee, indibl, indisp, indiwk, indrwk, indwrk, &
                     itmp1, j, jj, nsplit
           real(dp) :: tmp1
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           upper = stdlib_lsame( uplo, 'U' )
           alleig = stdlib_lsame( range, 'A' )
           valeig = stdlib_lsame( range, 'V' )
           indeig = stdlib_lsame( range, 'I' )
           info = 0_ilp
           if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -1_ilp
           else if( .not.( alleig .or. valeig .or. indeig ) ) then
              info = -2_ilp
           else if( .not.( upper .or. stdlib_lsame( uplo, 'L' ) ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( ka<0_ilp ) then
              info = -5_ilp
           else if( kb<0_ilp .or. kb>ka ) then
              info = -6_ilp
           else if( ldab<ka+1 ) then
              info = -8_ilp
           else if( ldbb<kb+1 ) then
              info = -10_ilp
           else if( ldq<1_ilp .or. ( wantz .and. ldq<n ) ) then
              info = -12_ilp
           else
              if( valeig ) then
                 if( n>0_ilp .and. vu<=vl )info = -14_ilp
              else if( indeig ) then
                 if( il<1_ilp .or. il>max( 1_ilp, n ) ) then
                    info = -15_ilp
                 else if ( iu<min( n, il ) .or. iu>n ) then
                    info = -16_ilp
                 end if
              end if
           end if
           if( info==0_ilp) then
              if( ldz<1_ilp .or. ( wantz .and. ldz<n ) ) then
                 info = -21_ilp
              end if
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZHBGVX', -info )
              return
           end if
           ! quick return if possible
           m = 0_ilp
           if( n==0 )return
           ! form a split cholesky factorization of b.
           call stdlib_zpbstf( uplo, n, kb, bb, ldbb, info )
           if( info/=0_ilp ) then
              info = n + info
              return
           end if
           ! transform problem to standard eigenvalue problem.
           call stdlib_zhbgst( jobz, uplo, n, ka, kb, ab, ldab, bb, ldbb, q, ldq,work, rwork, &
                     iinfo )
           ! solve the standard eigenvalue problem.
           ! reduce hermitian band matrix to tridiagonal form.
           indd = 1_ilp
           inde = indd + n
           indrwk = inde + n
           indwrk = 1_ilp
           if( wantz ) then
              vect = 'U'
           else
              vect = 'N'
           end if
           call stdlib_zhbtrd( vect, uplo, n, ka, ab, ldab, rwork( indd ),rwork( inde ), q, ldq, &
                     work( indwrk ), iinfo )
           ! if all eigenvalues are desired and abstol is less than or equal
           ! to zero, then call stdlib_dsterf or stdlib_zsteqr.  if this fails for some
           ! eigenvalue, then try stdlib_dstebz.
           test = .false.
           if( indeig ) then
              if( il==1_ilp .and. iu==n ) then
                 test = .true.
              end if
           end if
           if( ( alleig .or. test ) .and. ( abstol<=zero ) ) then
              call stdlib_dcopy( n, rwork( indd ), 1_ilp, w, 1_ilp )
              indee = indrwk + 2_ilp*n
              call stdlib_dcopy( n-1, rwork( inde ), 1_ilp, rwork( indee ), 1_ilp )
              if( .not.wantz ) then
                 call stdlib_dsterf( n, w, rwork( indee ), info )
              else
                 call stdlib_zlacpy( 'A', n, n, q, ldq, z, ldz )
                 call stdlib_zsteqr( jobz, n, w, rwork( indee ), z, ldz,rwork( indrwk ), info )
                           
                 if( info==0_ilp ) then
                    do i = 1, n
                       ifail( i ) = 0_ilp
                    end do
                 end if
              end if
              if( info==0_ilp ) then
                 m = n
                 go to 30
              end if
              info = 0_ilp
           end if
           ! otherwise, call stdlib_dstebz and, if eigenvectors are desired,
           ! call stdlib_zstein.
           if( wantz ) then
              order = 'B'
           else
              order = 'E'
           end if
           indibl = 1_ilp
           indisp = indibl + n
           indiwk = indisp + n
           call stdlib_dstebz( range, order, n, vl, vu, il, iu, abstol,rwork( indd ), rwork( inde &
           ), m, nsplit, w,iwork( indibl ), iwork( indisp ), rwork( indrwk ),iwork( indiwk ), &
                     info )
           if( wantz ) then
              call stdlib_zstein( n, rwork( indd ), rwork( inde ), m, w,iwork( indibl ), iwork( &
                        indisp ), z, ldz,rwork( indrwk ), iwork( indiwk ), ifail, info )
              ! apply unitary matrix used in reduction to tridiagonal
              ! form to eigenvectors returned by stdlib_zstein.
              do j = 1, m
                 call stdlib_zcopy( n, z( 1_ilp, j ), 1_ilp, work( 1_ilp ), 1_ilp )
                 call stdlib_zgemv( 'N', n, n, cone, q, ldq, work, 1_ilp, czero,z( 1_ilp, j ), 1_ilp )
              end do
           end if
           30 continue
           ! if eigenvalues are not in order, then sort them, along with
           ! eigenvectors.
           if( wantz ) then
              do j = 1, m - 1
                 i = 0_ilp
                 tmp1 = w( j )
                 do jj = j + 1, m
                    if( w( jj )<tmp1 ) then
                       i = jj
                       tmp1 = w( jj )
                    end if
                 end do
                 if( i/=0_ilp ) then
                    itmp1 = iwork( indibl+i-1 )
                    w( i ) = w( j )
                    iwork( indibl+i-1 ) = iwork( indibl+j-1 )
                    w( j ) = tmp1
                    iwork( indibl+j-1 ) = itmp1
                    call stdlib_zswap( n, z( 1_ilp, i ), 1_ilp, z( 1_ilp, j ), 1_ilp )
                    if( info/=0_ilp ) then
                       itmp1 = ifail( i )
                       ifail( i ) = ifail( j )
                       ifail( j ) = itmp1
                    end if
                 end if
              end do
           end if
           return
     end subroutine stdlib_zhbgvx




     pure module subroutine stdlib_chetrd( uplo, n, a, lda, d, e, tau, work, lwork, info )
     !! CHETRD reduces a complex Hermitian matrix A to real symmetric
     !! tridiagonal form T by a unitary similarity transformation:
     !! Q**H * A * Q = T.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, n
           ! Array Arguments 
           real(sp), intent(out) :: d(*), e(*)
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: tau(*), work(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: lquery, upper
           integer(ilp) :: i, iinfo, iws, j, kk, ldwork, lwkopt, nb, nbmin, nx
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           lquery = ( lwork==-1_ilp )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           else if( lwork<1_ilp .and. .not.lquery ) then
              info = -9_ilp
           end if
           if( info==0_ilp ) then
              ! determine the block size.
              nb = stdlib_ilaenv( 1_ilp, 'CHETRD', uplo, n, -1_ilp, -1_ilp, -1_ilp )
              lwkopt = n*nb
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CHETRD', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0_ilp ) then
              work( 1_ilp ) = 1_ilp
              return
           end if
           nx = n
           iws = 1_ilp
           if( nb>1_ilp .and. nb<n ) then
              ! determine when to cross over from blocked to unblocked code
              ! (last block is always handled by unblocked code).
              nx = max( nb, stdlib_ilaenv( 3_ilp, 'CHETRD', uplo, n, -1_ilp, -1_ilp, -1_ilp ) )
              if( nx<n ) then
                 ! determine if workspace is large enough for blocked code.
                 ldwork = n
                 iws = ldwork*nb
                 if( lwork<iws ) then
                    ! not enough workspace to use optimal nb:  determine the
                    ! minimum value of nb, and reduce nb or force use of
                    ! unblocked code by setting nx = n.
                    nb = max( lwork / ldwork, 1_ilp )
                    nbmin = stdlib_ilaenv( 2_ilp, 'CHETRD', uplo, n, -1_ilp, -1_ilp, -1_ilp )
                    if( nb<nbmin )nx = n
                 end if
              else
                 nx = n
              end if
           else
              nb = 1_ilp
           end if
           if( upper ) then
              ! reduce the upper triangle of a.
              ! columns 1:kk are handled by the unblocked method.
              kk = n - ( ( n-nx+nb-1 ) / nb )*nb
              do i = n - nb + 1, kk + 1, -nb
                 ! reduce columns i:i+nb-1 to tridiagonal form and form the
                 ! matrix w which is needed to update the unreduced part of
                 ! the matrix
                 call stdlib_clatrd( uplo, i+nb-1, nb, a, lda, e, tau, work,ldwork )
                 ! update the unreduced submatrix a(1:i-1,1:i-1), using an
                 ! update of the form:  a := a - v*w**h - w*v**h
                 call stdlib_cher2k( uplo, 'NO TRANSPOSE', i-1, nb, -cone,a( 1_ilp, i ), lda, work, &
                           ldwork, one, a, lda )
                 ! copy superdiagonal elements back into a, and diagonal
                 ! elements into d
                 do j = i, i + nb - 1
                    a( j-1, j ) = e( j-1 )
                    d( j ) = real( a( j, j ),KIND=sp)
                 end do
              end do
              ! use unblocked code to reduce the last or only block
              call stdlib_chetd2( uplo, kk, a, lda, d, e, tau, iinfo )
           else
              ! reduce the lower triangle of a
              do i = 1, n - nx, nb
                 ! reduce columns i:i+nb-1 to tridiagonal form and form the
                 ! matrix w which is needed to update the unreduced part of
                 ! the matrix
                 call stdlib_clatrd( uplo, n-i+1, nb, a( i, i ), lda, e( i ),tau( i ), work, &
                           ldwork )
                 ! update the unreduced submatrix a(i+nb:n,i+nb:n), using
                 ! an update of the form:  a := a - v*w**h - w*v**h
                 call stdlib_cher2k( uplo, 'NO TRANSPOSE', n-i-nb+1, nb, -cone,a( i+nb, i ), lda, &
                           work( nb+1 ), ldwork, one,a( i+nb, i+nb ), lda )
                 ! copy subdiagonal elements back into a, and diagonal
                 ! elements into d
                 do j = i, i + nb - 1
                    a( j+1, j ) = e( j )
                    d( j ) = real( a( j, j ),KIND=sp)
                 end do
              end do
              ! use unblocked code to reduce the last or only block
              call stdlib_chetd2( uplo, n-i+1, a( i, i ), lda, d( i ), e( i ),tau( i ), iinfo )
                        
           end if
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_chetrd

     pure module subroutine stdlib_zhetrd( uplo, n, a, lda, d, e, tau, work, lwork, info )
     !! ZHETRD reduces a complex Hermitian matrix A to real symmetric
     !! tridiagonal form T by a unitary similarity transformation:
     !! Q**H * A * Q = T.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, n
           ! Array Arguments 
           real(dp), intent(out) :: d(*), e(*)
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: tau(*), work(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: lquery, upper
           integer(ilp) :: i, iinfo, iws, j, kk, ldwork, lwkopt, nb, nbmin, nx
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           lquery = ( lwork==-1_ilp )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           else if( lwork<1_ilp .and. .not.lquery ) then
              info = -9_ilp
           end if
           if( info==0_ilp ) then
              ! determine the block size.
              nb = stdlib_ilaenv( 1_ilp, 'ZHETRD', uplo, n, -1_ilp, -1_ilp, -1_ilp )
              lwkopt = n*nb
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZHETRD', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0_ilp ) then
              work( 1_ilp ) = 1_ilp
              return
           end if
           nx = n
           iws = 1_ilp
           if( nb>1_ilp .and. nb<n ) then
              ! determine when to cross over from blocked to unblocked code
              ! (last block is always handled by unblocked code).
              nx = max( nb, stdlib_ilaenv( 3_ilp, 'ZHETRD', uplo, n, -1_ilp, -1_ilp, -1_ilp ) )
              if( nx<n ) then
                 ! determine if workspace is large enough for blocked code.
                 ldwork = n
                 iws = ldwork*nb
                 if( lwork<iws ) then
                    ! not enough workspace to use optimal nb:  determine the
                    ! minimum value of nb, and reduce nb or force use of
                    ! unblocked code by setting nx = n.
                    nb = max( lwork / ldwork, 1_ilp )
                    nbmin = stdlib_ilaenv( 2_ilp, 'ZHETRD', uplo, n, -1_ilp, -1_ilp, -1_ilp )
                    if( nb<nbmin )nx = n
                 end if
              else
                 nx = n
              end if
           else
              nb = 1_ilp
           end if
           if( upper ) then
              ! reduce the upper triangle of a.
              ! columns 1:kk are handled by the unblocked method.
              kk = n - ( ( n-nx+nb-1 ) / nb )*nb
              do i = n - nb + 1, kk + 1, -nb
                 ! reduce columns i:i+nb-1 to tridiagonal form and form the
                 ! matrix w which is needed to update the unreduced part of
                 ! the matrix
                 call stdlib_zlatrd( uplo, i+nb-1, nb, a, lda, e, tau, work,ldwork )
                 ! update the unreduced submatrix a(1:i-1,1:i-1), using an
                 ! update of the form:  a := a - v*w**h - w*v**h
                 call stdlib_zher2k( uplo, 'NO TRANSPOSE', i-1, nb, -cone,a( 1_ilp, i ), lda, work, &
                           ldwork, one, a, lda )
                 ! copy superdiagonal elements back into a, and diagonal
                 ! elements into d
                 do j = i, i + nb - 1
                    a( j-1, j ) = e( j-1 )
                    d( j ) = real( a( j, j ),KIND=dp)
                 end do
              end do
              ! use unblocked code to reduce the last or only block
              call stdlib_zhetd2( uplo, kk, a, lda, d, e, tau, iinfo )
           else
              ! reduce the lower triangle of a
              do i = 1, n - nx, nb
                 ! reduce columns i:i+nb-1 to tridiagonal form and form the
                 ! matrix w which is needed to update the unreduced part of
                 ! the matrix
                 call stdlib_zlatrd( uplo, n-i+1, nb, a( i, i ), lda, e( i ),tau( i ), work, &
                           ldwork )
                 ! update the unreduced submatrix a(i+nb:n,i+nb:n), using
                 ! an update of the form:  a := a - v*w**h - w*v**h
                 call stdlib_zher2k( uplo, 'NO TRANSPOSE', n-i-nb+1, nb, -cone,a( i+nb, i ), lda, &
                           work( nb+1 ), ldwork, one,a( i+nb, i+nb ), lda )
                 ! copy subdiagonal elements back into a, and diagonal
                 ! elements into d
                 do j = i, i + nb - 1
                    a( j+1, j ) = e( j )
                    d( j ) = real( a( j, j ),KIND=dp)
                 end do
              end do
              ! use unblocked code to reduce the last or only block
              call stdlib_zhetd2( uplo, n-i+1, a( i, i ), lda, d( i ), e( i ),tau( i ), iinfo )
                        
           end if
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_zhetrd




     pure module subroutine stdlib_chetd2( uplo, n, a, lda, d, e, tau, info )
     !! CHETD2 reduces a complex Hermitian matrix A to real symmetric
     !! tridiagonal form T by a unitary similarity transformation:
     !! Q**H * A * Q = T.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           real(sp), intent(out) :: d(*), e(*)
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: tau(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: i
           complex(sp) :: alpha, taui
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CHETD2', -info )
              return
           end if
           ! quick return if possible
           if( n<=0 )return
           if( upper ) then
              ! reduce the upper triangle of a
              a( n, n ) = real( a( n, n ),KIND=sp)
              do i = n - 1, 1, -1
                 ! generate elementary reflector h(i) = i - tau * v * v**h
                 ! to annihilate a(1:i-1,i+1)
                 alpha = a( i, i+1 )
                 call stdlib_clarfg( i, alpha, a( 1_ilp, i+1 ), 1_ilp, taui )
                 e( i ) = real( alpha,KIND=sp)
                 if( taui/=czero ) then
                    ! apply h(i) from both sides to a(1:i,1:i)
                    a( i, i+1 ) = cone
                    ! compute  x := tau * a * v  storing x in tau(1:i)
                    call stdlib_chemv( uplo, i, taui, a, lda, a( 1_ilp, i+1 ), 1_ilp, czero,tau, 1_ilp )
                              
                    ! compute  w := x - 1/2 * tau * (x**h * v) * v
                    alpha = -chalf*taui*stdlib_cdotc( i, tau, 1_ilp, a( 1_ilp, i+1 ), 1_ilp )
                    call stdlib_caxpy( i, alpha, a( 1_ilp, i+1 ), 1_ilp, tau, 1_ilp )
                    ! apply the transformation as a rank-2 update:
                       ! a := a - v * w**h - w * v**h
                    call stdlib_cher2( uplo, i, -cone, a( 1_ilp, i+1 ), 1_ilp, tau, 1_ilp, a,lda )
                 else
                    a( i, i ) = real( a( i, i ),KIND=sp)
                 end if
                 a( i, i+1 ) = e( i )
                 d( i+1 ) = real( a( i+1, i+1 ),KIND=sp)
                 tau( i ) = taui
              end do
              d( 1_ilp ) = real( a( 1_ilp, 1_ilp ),KIND=sp)
           else
              ! reduce the lower triangle of a
              a( 1_ilp, 1_ilp ) = real( a( 1_ilp, 1_ilp ),KIND=sp)
              do i = 1, n - 1
                 ! generate elementary reflector h(i) = i - tau * v * v**h
                 ! to annihilate a(i+2:n,i)
                 alpha = a( i+1, i )
                 call stdlib_clarfg( n-i, alpha, a( min( i+2, n ), i ), 1_ilp, taui )
                 e( i ) = real( alpha,KIND=sp)
                 if( taui/=czero ) then
                    ! apply h(i) from both sides to a(i+1:n,i+1:n)
                    a( i+1, i ) = cone
                    ! compute  x := tau * a * v  storing y in tau(i:n-1)
                    call stdlib_chemv( uplo, n-i, taui, a( i+1, i+1 ), lda,a( i+1, i ), 1_ilp, czero, &
                              tau( i ), 1_ilp )
                    ! compute  w := x - 1/2 * tau * (x**h * v) * v
                    alpha = -chalf*taui*stdlib_cdotc( n-i, tau( i ), 1_ilp, a( i+1, i ),1_ilp )
                    call stdlib_caxpy( n-i, alpha, a( i+1, i ), 1_ilp, tau( i ), 1_ilp )
                    ! apply the transformation as a rank-2 update:
                       ! a := a - v * w**h - w * v**h
                    call stdlib_cher2( uplo, n-i, -cone, a( i+1, i ), 1_ilp, tau( i ), 1_ilp,a( i+1, i+1 )&
                              , lda )
                 else
                    a( i+1, i+1 ) = real( a( i+1, i+1 ),KIND=sp)
                 end if
                 a( i+1, i ) = e( i )
                 d( i ) = real( a( i, i ),KIND=sp)
                 tau( i ) = taui
              end do
              d( n ) = real( a( n, n ),KIND=sp)
           end if
           return
     end subroutine stdlib_chetd2

     pure module subroutine stdlib_zhetd2( uplo, n, a, lda, d, e, tau, info )
     !! ZHETD2 reduces a complex Hermitian matrix A to real symmetric
     !! tridiagonal form T by a unitary similarity transformation:
     !! Q**H * A * Q = T.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           ! Array Arguments 
           real(dp), intent(out) :: d(*), e(*)
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: tau(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: i
           complex(dp) :: alpha, taui
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp
           upper = stdlib_lsame( uplo, 'U')
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZHETD2', -info )
              return
           end if
           ! quick return if possible
           if( n<=0 )return
           if( upper ) then
              ! reduce the upper triangle of a
              a( n, n ) = real( a( n, n ),KIND=dp)
              do i = n - 1, 1, -1
                 ! generate elementary reflector h(i) = i - tau * v * v**h
                 ! to annihilate a(1:i-1,i+1)
                 alpha = a( i, i+1 )
                 call stdlib_zlarfg( i, alpha, a( 1_ilp, i+1 ), 1_ilp, taui )
                 e( i ) = real( alpha,KIND=dp)
                 if( taui/=czero ) then
                    ! apply h(i) from both sides to a(1:i,1:i)
                    a( i, i+1 ) = cone
                    ! compute  x := tau * a * v  storing x in tau(1:i)
                    call stdlib_zhemv( uplo, i, taui, a, lda, a( 1_ilp, i+1 ), 1_ilp, czero,tau, 1_ilp )
                              
                    ! compute  w := x - 1/2 * tau * (x**h * v) * v
                    alpha = -chalf*taui*stdlib_zdotc( i, tau, 1_ilp, a( 1_ilp, i+1 ), 1_ilp )
                    call stdlib_zaxpy( i, alpha, a( 1_ilp, i+1 ), 1_ilp, tau, 1_ilp )
                    ! apply the transformation as a rank-2 update:
                       ! a := a - v * w**h - w * v**h
                    call stdlib_zher2( uplo, i, -cone, a( 1_ilp, i+1 ), 1_ilp, tau, 1_ilp, a,lda )
                 else
                    a( i, i ) = real( a( i, i ),KIND=dp)
                 end if
                 a( i, i+1 ) = e( i )
                 d( i+1 ) = real( a( i+1, i+1 ),KIND=dp)
                 tau( i ) = taui
              end do
              d( 1_ilp ) = real( a( 1_ilp, 1_ilp ),KIND=dp)
           else
              ! reduce the lower triangle of a
              a( 1_ilp, 1_ilp ) = real( a( 1_ilp, 1_ilp ),KIND=dp)
              do i = 1, n - 1
                 ! generate elementary reflector h(i) = i - tau * v * v**h
                 ! to annihilate a(i+2:n,i)
                 alpha = a( i+1, i )
                 call stdlib_zlarfg( n-i, alpha, a( min( i+2, n ), i ), 1_ilp, taui )
                 e( i ) = real( alpha,KIND=dp)
                 if( taui/=czero ) then
                    ! apply h(i) from both sides to a(i+1:n,i+1:n)
                    a( i+1, i ) = cone
                    ! compute  x := tau * a * v  storing y in tau(i:n-1)
                    call stdlib_zhemv( uplo, n-i, taui, a( i+1, i+1 ), lda,a( i+1, i ), 1_ilp, czero, &
                              tau( i ), 1_ilp )
                    ! compute  w := x - 1/2 * tau * (x**h * v) * v
                    alpha = -chalf*taui*stdlib_zdotc( n-i, tau( i ), 1_ilp, a( i+1, i ),1_ilp )
                    call stdlib_zaxpy( n-i, alpha, a( i+1, i ), 1_ilp, tau( i ), 1_ilp )
                    ! apply the transformation as a rank-2 update:
                       ! a := a - v * w**h - w * v**h
                    call stdlib_zher2( uplo, n-i, -cone, a( i+1, i ), 1_ilp, tau( i ), 1_ilp,a( i+1, i+1 )&
                              , lda )
                 else
                    a( i+1, i+1 ) = real( a( i+1, i+1 ),KIND=dp)
                 end if
                 a( i+1, i ) = e( i )
                 d( i ) = real( a( i, i ),KIND=dp)
                 tau( i ) = taui
              end do
              d( n ) = real( a( n, n ),KIND=dp)
           end if
           return
     end subroutine stdlib_zhetd2




     pure module subroutine stdlib_cungtr( uplo, n, a, lda, tau, work, lwork, info )
     !! CUNGTR generates a complex unitary matrix Q which is defined as the
     !! product of n-1 elementary reflectors of order N, as returned by
     !! CHETRD:
     !! if UPLO = 'U', Q = H(n-1) . . . H(2) H(1),
     !! if UPLO = 'L', Q = H(1) H(2) . . . H(n-1).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, n
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(in) :: tau(*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery, upper
           integer(ilp) :: i, iinfo, j, lwkopt, nb
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           else if( lwork<max( 1_ilp, n-1 ) .and. .not.lquery ) then
              info = -7_ilp
           end if
           if( info==0_ilp ) then
              if ( upper ) then
                nb = stdlib_ilaenv( 1_ilp, 'CUNGQL', ' ', n-1, n-1, n-1, -1_ilp )
              else
                nb = stdlib_ilaenv( 1_ilp, 'CUNGQR', ' ', n-1, n-1, n-1, -1_ilp )
              end if
              lwkopt = max( 1_ilp, n-1 )*nb
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CUNGTR', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0_ilp ) then
              work( 1_ilp ) = 1_ilp
              return
           end if
           if( upper ) then
              ! q was determined by a call to stdlib_chetrd with uplo = 'u'
              ! shift the vectors which define the elementary reflectors cone
              ! column to the left, and set the last row and column of q to
              ! those of the unit matrix
              do j = 1, n - 1
                 do i = 1, j - 1
                    a( i, j ) = a( i, j+1 )
                 end do
                 a( n, j ) = czero
              end do
              do i = 1, n - 1
                 a( i, n ) = czero
              end do
              a( n, n ) = cone
              ! generate q(1:n-1,1:n-1)
              call stdlib_cungql( n-1, n-1, n-1, a, lda, tau, work, lwork, iinfo )
           else
              ! q was determined by a call to stdlib_chetrd with uplo = 'l'.
              ! shift the vectors which define the elementary reflectors cone
              ! column to the right, and set the first row and column of q to
              ! those of the unit matrix
              do j = n, 2, -1
                 a( 1_ilp, j ) = czero
                 do i = j + 1, n
                    a( i, j ) = a( i, j-1 )
                 end do
              end do
              a( 1_ilp, 1_ilp ) = cone
              do i = 2, n
                 a( i, 1_ilp ) = czero
              end do
              if( n>1_ilp ) then
                 ! generate q(2:n,2:n)
                 call stdlib_cungqr( n-1, n-1, n-1, a( 2_ilp, 2_ilp ), lda, tau, work,lwork, iinfo )
                           
              end if
           end if
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_cungtr

     pure module subroutine stdlib_zungtr( uplo, n, a, lda, tau, work, lwork, info )
     !! ZUNGTR generates a complex unitary matrix Q which is defined as the
     !! product of n-1 elementary reflectors of order N, as returned by
     !! ZHETRD:
     !! if UPLO = 'U', Q = H(n-1) . . . H(2) H(1),
     !! if UPLO = 'L', Q = H(1) H(2) . . . H(n-1).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, n
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(in) :: tau(*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery, upper
           integer(ilp) :: i, iinfo, j, lwkopt, nb
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           lquery = ( lwork==-1_ilp )
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -4_ilp
           else if( lwork<max( 1_ilp, n-1 ) .and. .not.lquery ) then
              info = -7_ilp
           end if
           if( info==0_ilp ) then
              if( upper ) then
                 nb = stdlib_ilaenv( 1_ilp, 'ZUNGQL', ' ', n-1, n-1, n-1, -1_ilp )
              else
                 nb = stdlib_ilaenv( 1_ilp, 'ZUNGQR', ' ', n-1, n-1, n-1, -1_ilp )
              end if
              lwkopt = max( 1_ilp, n-1 )*nb
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZUNGTR', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0_ilp ) then
              work( 1_ilp ) = 1_ilp
              return
           end if
           if( upper ) then
              ! q was determined by a call to stdlib_zhetrd with uplo = 'u'
              ! shift the vectors which define the elementary reflectors cone
              ! column to the left, and set the last row and column of q to
              ! those of the unit matrix
              do j = 1, n - 1
                 do i = 1, j - 1
                    a( i, j ) = a( i, j+1 )
                 end do
                 a( n, j ) = czero
              end do
              do i = 1, n - 1
                 a( i, n ) = czero
              end do
              a( n, n ) = cone
              ! generate q(1:n-1,1:n-1)
              call stdlib_zungql( n-1, n-1, n-1, a, lda, tau, work, lwork, iinfo )
           else
              ! q was determined by a call to stdlib_zhetrd with uplo = 'l'.
              ! shift the vectors which define the elementary reflectors cone
              ! column to the right, and set the first row and column of q to
              ! those of the unit matrix
              do j = n, 2, -1
                 a( 1_ilp, j ) = czero
                 do i = j + 1, n
                    a( i, j ) = a( i, j-1 )
                 end do
              end do
              a( 1_ilp, 1_ilp ) = cone
              do i = 2, n
                 a( i, 1_ilp ) = czero
              end do
              if( n>1_ilp ) then
                 ! generate q(2:n,2:n)
                 call stdlib_zungqr( n-1, n-1, n-1, a( 2_ilp, 2_ilp ), lda, tau, work,lwork, iinfo )
                           
              end if
           end if
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_zungtr




     pure module subroutine stdlib_cunmtr( side, uplo, trans, m, n, a, lda, tau, c, ldc,work, lwork, &
     !! CUNMTR overwrites the general complex M-by-N matrix C with
     !! SIDE = 'L'     SIDE = 'R'
     !! TRANS = 'N':      Q * C          C * Q
     !! TRANS = 'C':      Q**H * C       C * Q**H
     !! where Q is a complex unitary matrix of order nq, with nq = m if
     !! SIDE = 'L' and nq = n if SIDE = 'R'. Q is defined as the product of
     !! nq-1 elementary reflectors, as returned by CHETRD:
     !! if UPLO = 'U', Q = H(nq-1) . . . H(2) H(1);
     !! if UPLO = 'L', Q = H(1) H(2) . . . H(nq-1).
               info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side, trans, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldc, lwork, m, n
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*), c(ldc,*)
           complex(sp), intent(in) :: tau(*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: left, lquery, upper
           integer(ilp) :: i1, i2, iinfo, lwkopt, mi, nb, ni, nq, nw
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           left = stdlib_lsame( side, 'L' )
           upper = stdlib_lsame( uplo, 'U' )
           lquery = ( lwork==-1_ilp )
           ! nq is the order of q and nw is the minimum dimension of work
           if( left ) then
              nq = m
              nw = max( 1_ilp, n )
           else
              nq = n
              nw = max( 1_ilp, m )
           end if
           if( .not.left .and. .not.stdlib_lsame( side, 'R' ) ) then
              info = -1_ilp
           else if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -2_ilp
           else if( .not.stdlib_lsame( trans, 'N' ) .and. .not.stdlib_lsame( trans, 'C' ) )&
                     then
              info = -3_ilp
           else if( m<0_ilp ) then
              info = -4_ilp
           else if( n<0_ilp ) then
              info = -5_ilp
           else if( lda<max( 1_ilp, nq ) ) then
              info = -7_ilp
           else if( ldc<max( 1_ilp, m ) ) then
              info = -10_ilp
           else if( lwork<nw .and. .not.lquery ) then
              info = -12_ilp
           end if
           if( info==0_ilp ) then
              if( upper ) then
                 if( left ) then
                    nb = stdlib_ilaenv( 1_ilp, 'CUNMQL', side // trans, m-1, n, m-1,-1_ilp )
                 else
                    nb = stdlib_ilaenv( 1_ilp, 'CUNMQL', side // trans, m, n-1, n-1,-1_ilp )
                 end if
              else
                 if( left ) then
                    nb = stdlib_ilaenv( 1_ilp, 'CUNMQR', side // trans, m-1, n, m-1,-1_ilp )
                 else
                    nb = stdlib_ilaenv( 1_ilp, 'CUNMQR', side // trans, m, n-1, n-1,-1_ilp )
                 end if
              end if
              lwkopt = nw*nb
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CUNMTR', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( m==0_ilp .or. n==0_ilp .or. nq==1_ilp ) then
              work( 1_ilp ) = 1_ilp
              return
           end if
           if( left ) then
              mi = m - 1_ilp
              ni = n
           else
              mi = m
              ni = n - 1_ilp
           end if
           if( upper ) then
              ! q was determined by a call to stdlib_chetrd with uplo = 'u'
              call stdlib_cunmql( side, trans, mi, ni, nq-1, a( 1_ilp, 2_ilp ), lda, tau, c,ldc, work, &
                        lwork, iinfo )
           else
              ! q was determined by a call to stdlib_chetrd with uplo = 'l'
              if( left ) then
                 i1 = 2_ilp
                 i2 = 1_ilp
              else
                 i1 = 1_ilp
                 i2 = 2_ilp
              end if
              call stdlib_cunmqr( side, trans, mi, ni, nq-1, a( 2_ilp, 1_ilp ), lda, tau,c( i1, i2 ), ldc,&
                         work, lwork, iinfo )
           end if
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_cunmtr

     pure module subroutine stdlib_zunmtr( side, uplo, trans, m, n, a, lda, tau, c, ldc,work, lwork, &
     !! ZUNMTR overwrites the general complex M-by-N matrix C with
     !! SIDE = 'L'     SIDE = 'R'
     !! TRANS = 'N':      Q * C          C * Q
     !! TRANS = 'C':      Q**H * C       C * Q**H
     !! where Q is a complex unitary matrix of order nq, with nq = m if
     !! SIDE = 'L' and nq = n if SIDE = 'R'. Q is defined as the product of
     !! nq-1 elementary reflectors, as returned by ZHETRD:
     !! if UPLO = 'U', Q = H(nq-1) . . . H(2) H(1);
     !! if UPLO = 'L', Q = H(1) H(2) . . . H(nq-1).
               info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side, trans, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldc, lwork, m, n
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*), c(ldc,*)
           complex(dp), intent(in) :: tau(*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: left, lquery, upper
           integer(ilp) :: i1, i2, iinfo, lwkopt, mi, nb, ni, nq, nw
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp
           left = stdlib_lsame( side, 'L' )
           upper = stdlib_lsame( uplo, 'U' )
           lquery = ( lwork==-1_ilp )
           ! nq is the order of q and nw is the minimum dimension of work
           if( left ) then
              nq = m
              nw = max( 1_ilp, n )
           else
              nq = n
              nw = max( 1_ilp, m )
           end if
           if( .not.left .and. .not.stdlib_lsame( side, 'R' ) ) then
              info = -1_ilp
           else if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -2_ilp
           else if( .not.stdlib_lsame( trans, 'N' ) .and. .not.stdlib_lsame( trans, 'C' ) )&
                     then
              info = -3_ilp
           else if( m<0_ilp ) then
              info = -4_ilp
           else if( n<0_ilp ) then
              info = -5_ilp
           else if( lda<max( 1_ilp, nq ) ) then
              info = -7_ilp
           else if( ldc<max( 1_ilp, m ) ) then
              info = -10_ilp
           else if( lwork<nw .and. .not.lquery ) then
              info = -12_ilp
           end if
           if( info==0_ilp ) then
              if( upper ) then
                 if( left ) then
                    nb = stdlib_ilaenv( 1_ilp, 'ZUNMQL', side // trans, m-1, n, m-1,-1_ilp )
                 else
                    nb = stdlib_ilaenv( 1_ilp, 'ZUNMQL', side // trans, m, n-1, n-1,-1_ilp )
                 end if
              else
                 if( left ) then
                    nb = stdlib_ilaenv( 1_ilp, 'ZUNMQR', side // trans, m-1, n, m-1,-1_ilp )
                 else
                    nb = stdlib_ilaenv( 1_ilp, 'ZUNMQR', side // trans, m, n-1, n-1,-1_ilp )
                 end if
              end if
              lwkopt = nw*nb
              work( 1_ilp ) = lwkopt
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZUNMTR', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( m==0_ilp .or. n==0_ilp .or. nq==1_ilp ) then
              work( 1_ilp ) = 1_ilp
              return
           end if
           if( left ) then
              mi = m - 1_ilp
              ni = n
           else
              mi = m
              ni = n - 1_ilp
           end if
           if( upper ) then
              ! q was determined by a call to stdlib_zhetrd with uplo = 'u'
              call stdlib_zunmql( side, trans, mi, ni, nq-1, a( 1_ilp, 2_ilp ), lda, tau, c,ldc, work, &
                        lwork, iinfo )
           else
              ! q was determined by a call to stdlib_zhetrd with uplo = 'l'
              if( left ) then
                 i1 = 2_ilp
                 i2 = 1_ilp
              else
                 i1 = 1_ilp
                 i2 = 2_ilp
              end if
              call stdlib_zunmqr( side, trans, mi, ni, nq-1, a( 2_ilp, 1_ilp ), lda, tau,c( i1, i2 ), ldc,&
                         work, lwork, iinfo )
           end if
           work( 1_ilp ) = lwkopt
           return
     end subroutine stdlib_zunmtr




     module subroutine stdlib_chetrd_he2hb( uplo, n, kd, a, lda, ab, ldab, tau,work, lwork, info )
     !! CHETRD_HE2HB reduces a complex Hermitian matrix A to complex Hermitian
     !! band-diagonal form AB by a unitary similarity transformation:
     !! Q**H * A * Q = AB.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldab, lwork, n, kd
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: ab(ldab,*), tau(*), work(*)
        ! =====================================================================
           ! Parameters 
           
           ! Local Scalars 
           logical(lk) :: lquery, upper
           integer(ilp) :: i, j, iinfo, lwmin, pn, pk, llk, ldt, ldw, lds2, lds1, ls2, ls1, lw, lt,&
                      tpos, wpos, s2pos, s1pos
           ! Intrinsic Functions 
           ! Executable Statements 
           ! determine the minimal workspace size required
           ! and test the input parameters
           info   = 0_ilp
           upper  = stdlib_lsame( uplo, 'U' )
           lquery = ( lwork==-1_ilp )
           lwmin  = stdlib_ilaenv2stage( 4_ilp, 'CHETRD_HE2HB', '', n, kd, -1_ilp, -1_ilp )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( kd<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldab<max( 1_ilp, kd+1 ) ) then
              info = -7_ilp
           else if( lwork<lwmin .and. .not.lquery ) then
              info = -10_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CHETRD_HE2HB', -info )
              return
           else if( lquery ) then
              work( 1_ilp ) = lwmin
              return
           end if
           ! quick return if possible
           ! copy the upper/lower portion of a into ab
           if( n<=kd+1 ) then
               if( upper ) then
                   do i = 1, n
                       llk = min( kd+1, i )
                       call stdlib_ccopy( llk, a( i-llk+1, i ), 1_ilp,ab( kd+1-llk+1, i ), 1_ilp )
                   end do
               else
                   do i = 1, n
                       llk = min( kd+1, n-i+1 )
                       call stdlib_ccopy( llk, a( i, i ), 1_ilp, ab( 1_ilp, i ), 1_ilp )
                   end do
               endif
               work( 1_ilp ) = 1_ilp
               return
           end if
           ! determine the pointer position for the workspace
           ldt    = kd
           lds1   = kd
           lt     = ldt*kd
           lw     = n*kd
           ls1    = lds1*kd
           ls2    = lwmin - lt - lw - ls1
            ! ls2 = n*max(kd,factoptnb)
           tpos   = 1_ilp
           wpos   = tpos  + lt
           s1pos  = wpos  + lw
           s2pos  = s1pos + ls1
           if( upper ) then
               ldw    = kd
               lds2   = kd
           else
               ldw    = n
               lds2   = n
           endif
           ! set the workspace of the triangular matrix t to czero once such a
           ! way every time t is generated the upper/lower portion will be always czero
           call stdlib_claset( "A", ldt, kd, czero, czero, work( tpos ), ldt )
           if( upper ) then
               do i = 1, n - kd, kd
                  pn = n-i-kd+1
                  pk = min( n-i-kd+1, kd )
                  ! compute the lq factorization of the current block
                  call stdlib_cgelqf( kd, pn, a( i, i+kd ), lda,tau( i ), work( s2pos ), ls2, &
                            iinfo )
                  ! copy the upper portion of a into ab
                  do j = i, i+pk-1
                     llk = min( kd, n-j ) + 1_ilp
                     call stdlib_ccopy( llk, a( j, j ), lda, ab( kd+1, j ), ldab-1 )
                  end do
                  call stdlib_claset( 'LOWER', pk, pk, czero, cone,a( i, i+kd ), lda )
                  ! form the matrix t
                  call stdlib_clarft( 'FORWARD', 'ROWWISE', pn, pk,a( i, i+kd ), lda, tau( i ),&
                            work( tpos ), ldt )
                  ! compute w:
                  call stdlib_cgemm( 'CONJUGATE', 'NO TRANSPOSE', pk, pn, pk,cone,  work( tpos ), &
                            ldt,a( i, i+kd ), lda,czero, work( s2pos ), lds2 )
                  call stdlib_chemm( 'RIGHT', uplo, pk, pn,cone,  a( i+kd, i+kd ), lda,work( &
                            s2pos ), lds2,czero, work( wpos ), ldw )
                  call stdlib_cgemm( 'NO TRANSPOSE', 'CONJUGATE', pk, pk, pn,cone,  work( wpos ), &
                            ldw,work( s2pos ), lds2,czero, work( s1pos ), lds1 )
                  call stdlib_cgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', pk, pn, pk,-chalf, work( &
                            s1pos ), lds1,a( i, i+kd ), lda,cone,   work( wpos ), ldw )
                  ! update the unreduced submatrix a(i+kd:n,i+kd:n), using
                  ! an update of the form:  a := a - v'*w - w'*v
                  call stdlib_cher2k( uplo, 'CONJUGATE', pn, pk,-cone, a( i, i+kd ), lda,work( &
                            wpos ), ldw,one, a( i+kd, i+kd ), lda )
               end do
              ! copy the upper band to ab which is the band storage matrix
              do j = n-kd+1, n
                 llk = min(kd, n-j) + 1_ilp
                 call stdlib_ccopy( llk, a( j, j ), lda, ab( kd+1, j ), ldab-1 )
              end do
           else
               ! reduce the lower triangle of a to lower band matrix
               loop_40: do i = 1, n - kd, kd
                  pn = n-i-kd+1
                  pk = min( n-i-kd+1, kd )
                  ! compute the qr factorization of the current block
                  call stdlib_cgeqrf( pn, kd, a( i+kd, i ), lda,tau( i ), work( s2pos ), ls2, &
                            iinfo )
                  ! copy the upper portion of a into ab
                  do j = i, i+pk-1
                     llk = min( kd, n-j ) + 1_ilp
                     call stdlib_ccopy( llk, a( j, j ), 1_ilp, ab( 1_ilp, j ), 1_ilp )
                  end do
                  call stdlib_claset( 'UPPER', pk, pk, czero, cone,a( i+kd, i ), lda )
                  ! form the matrix t
                  call stdlib_clarft( 'FORWARD', 'COLUMNWISE', pn, pk,a( i+kd, i ), lda, tau( i ),&
                            work( tpos ), ldt )
                  ! compute w:
                  call stdlib_cgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', pn, pk, pk,cone, a( i+kd, i )&
                            , lda,work( tpos ), ldt,czero, work( s2pos ), lds2 )
                  call stdlib_chemm( 'LEFT', uplo, pn, pk,cone, a( i+kd, i+kd ), lda,work( s2pos )&
                            , lds2,czero, work( wpos ), ldw )
                  call stdlib_cgemm( 'CONJUGATE', 'NO TRANSPOSE', pk, pk, pn,cone, work( s2pos ), &
                            lds2,work( wpos ), ldw,czero, work( s1pos ), lds1 )
                  call stdlib_cgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', pn, pk, pk,-chalf, a( i+kd, &
                            i ), lda,work( s1pos ), lds1,cone, work( wpos ), ldw )
                  ! update the unreduced submatrix a(i+kd:n,i+kd:n), using
                  ! an update of the form:  a := a - v*w' - w*v'
                  call stdlib_cher2k( uplo, 'NO TRANSPOSE', pn, pk,-cone, a( i+kd, i ), lda,work( &
                            wpos ), ldw,one, a( i+kd, i+kd ), lda )
                  ! ==================================================================
                  ! restore a for comparison and checking to be removed
                   ! do 45 j = i, i+pk-1
                      ! llk = min( kd, n-j ) + 1
                      ! call stdlib_ccopy( llk, ab( 1, j ), 1, a( j, j ), 1 )
                      45 continue
                  ! ==================================================================
               end do loop_40
              ! copy the lower band to ab which is the band storage matrix
              do j = n-kd+1, n
                 llk = min(kd, n-j) + 1_ilp
                 call stdlib_ccopy( llk, a( j, j ), 1_ilp, ab( 1_ilp, j ), 1_ilp )
              end do
           end if
           work( 1_ilp ) = lwmin
           return
     end subroutine stdlib_chetrd_he2hb

     module subroutine stdlib_zhetrd_he2hb( uplo, n, kd, a, lda, ab, ldab, tau,work, lwork, info )
     !! ZHETRD_HE2HB reduces a complex Hermitian matrix A to complex Hermitian
     !! band-diagonal form AB by a unitary similarity transformation:
     !! Q**H * A * Q = AB.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldab, lwork, n, kd
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: ab(ldab,*), tau(*), work(*)
        ! =====================================================================
           ! Parameters 
           
           ! Local Scalars 
           logical(lk) :: lquery, upper
           integer(ilp) :: i, j, iinfo, lwmin, pn, pk, llk, ldt, ldw, lds2, lds1, ls2, ls1, lw, lt,&
                      tpos, wpos, s2pos, s1pos
           ! Intrinsic Functions 
           ! Executable Statements 
           ! determine the minimal workspace size required
           ! and test the input parameters
           info   = 0_ilp
           upper  = stdlib_lsame( uplo, 'U' )
           lquery = ( lwork==-1_ilp )
           lwmin  = stdlib_ilaenv2stage( 4_ilp, 'ZHETRD_HE2HB', '', n, kd, -1_ilp, -1_ilp )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( kd<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldab<max( 1_ilp, kd+1 ) ) then
              info = -7_ilp
           else if( lwork<lwmin .and. .not.lquery ) then
              info = -10_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZHETRD_HE2HB', -info )
              return
           else if( lquery ) then
              work( 1_ilp ) = lwmin
              return
           end if
           ! quick return if possible
           ! copy the upper/lower portion of a into ab
           if( n<=kd+1 ) then
               if( upper ) then
                   do i = 1, n
                       llk = min( kd+1, i )
                       call stdlib_zcopy( llk, a( i-llk+1, i ), 1_ilp,ab( kd+1-llk+1, i ), 1_ilp )
                   end do
               else
                   do i = 1, n
                       llk = min( kd+1, n-i+1 )
                       call stdlib_zcopy( llk, a( i, i ), 1_ilp, ab( 1_ilp, i ), 1_ilp )
                   end do
               endif
               work( 1_ilp ) = 1_ilp
               return
           end if
           ! determine the pointer position for the workspace
           ldt    = kd
           lds1   = kd
           lt     = ldt*kd
           lw     = n*kd
           ls1    = lds1*kd
           ls2    = lwmin - lt - lw - ls1
            ! ls2 = n*max(kd,factoptnb)
           tpos   = 1_ilp
           wpos   = tpos  + lt
           s1pos  = wpos  + lw
           s2pos  = s1pos + ls1
           if( upper ) then
               ldw    = kd
               lds2   = kd
           else
               ldw    = n
               lds2   = n
           endif
           ! set the workspace of the triangular matrix t to czero once such a
           ! way every time t is generated the upper/lower portion will be always czero
           call stdlib_zlaset( "A", ldt, kd, czero, czero, work( tpos ), ldt )
           if( upper ) then
               do i = 1, n - kd, kd
                  pn = n-i-kd+1
                  pk = min( n-i-kd+1, kd )
                  ! compute the lq factorization of the current block
                  call stdlib_zgelqf( kd, pn, a( i, i+kd ), lda,tau( i ), work( s2pos ), ls2, &
                            iinfo )
                  ! copy the upper portion of a into ab
                  do j = i, i+pk-1
                     llk = min( kd, n-j ) + 1_ilp
                     call stdlib_zcopy( llk, a( j, j ), lda, ab( kd+1, j ), ldab-1 )
                  end do
                  call stdlib_zlaset( 'LOWER', pk, pk, czero, cone,a( i, i+kd ), lda )
                  ! form the matrix t
                  call stdlib_zlarft( 'FORWARD', 'ROWWISE', pn, pk,a( i, i+kd ), lda, tau( i ),&
                            work( tpos ), ldt )
                  ! compute w:
                  call stdlib_zgemm( 'CONJUGATE', 'NO TRANSPOSE', pk, pn, pk,cone,  work( tpos ), &
                            ldt,a( i, i+kd ), lda,czero, work( s2pos ), lds2 )
                  call stdlib_zhemm( 'RIGHT', uplo, pk, pn,cone,  a( i+kd, i+kd ), lda,work( &
                            s2pos ), lds2,czero, work( wpos ), ldw )
                  call stdlib_zgemm( 'NO TRANSPOSE', 'CONJUGATE', pk, pk, pn,cone,  work( wpos ), &
                            ldw,work( s2pos ), lds2,czero, work( s1pos ), lds1 )
                  call stdlib_zgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', pk, pn, pk,-chalf, work( &
                            s1pos ), lds1,a( i, i+kd ), lda,cone,   work( wpos ), ldw )
                  ! update the unreduced submatrix a(i+kd:n,i+kd:n), using
                  ! an update of the form:  a := a - v'*w - w'*v
                  call stdlib_zher2k( uplo, 'CONJUGATE', pn, pk,-cone, a( i, i+kd ), lda,work( &
                            wpos ), ldw,one, a( i+kd, i+kd ), lda )
               end do
              ! copy the upper band to ab which is the band storage matrix
              do j = n-kd+1, n
                 llk = min(kd, n-j) + 1_ilp
                 call stdlib_zcopy( llk, a( j, j ), lda, ab( kd+1, j ), ldab-1 )
              end do
           else
               ! reduce the lower triangle of a to lower band matrix
               loop_40: do i = 1, n - kd, kd
                  pn = n-i-kd+1
                  pk = min( n-i-kd+1, kd )
                  ! compute the qr factorization of the current block
                  call stdlib_zgeqrf( pn, kd, a( i+kd, i ), lda,tau( i ), work( s2pos ), ls2, &
                            iinfo )
                  ! copy the upper portion of a into ab
                  do j = i, i+pk-1
                     llk = min( kd, n-j ) + 1_ilp
                     call stdlib_zcopy( llk, a( j, j ), 1_ilp, ab( 1_ilp, j ), 1_ilp )
                  end do
                  call stdlib_zlaset( 'UPPER', pk, pk, czero, cone,a( i+kd, i ), lda )
                  ! form the matrix t
                  call stdlib_zlarft( 'FORWARD', 'COLUMNWISE', pn, pk,a( i+kd, i ), lda, tau( i ),&
                            work( tpos ), ldt )
                  ! compute w:
                  call stdlib_zgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', pn, pk, pk,cone, a( i+kd, i )&
                            , lda,work( tpos ), ldt,czero, work( s2pos ), lds2 )
                  call stdlib_zhemm( 'LEFT', uplo, pn, pk,cone, a( i+kd, i+kd ), lda,work( s2pos )&
                            , lds2,czero, work( wpos ), ldw )
                  call stdlib_zgemm( 'CONJUGATE', 'NO TRANSPOSE', pk, pk, pn,cone, work( s2pos ), &
                            lds2,work( wpos ), ldw,czero, work( s1pos ), lds1 )
                  call stdlib_zgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', pn, pk, pk,-chalf, a( i+kd, &
                            i ), lda,work( s1pos ), lds1,cone, work( wpos ), ldw )
                  ! update the unreduced submatrix a(i+kd:n,i+kd:n), using
                  ! an update of the form:  a := a - v*w' - w*v'
                  call stdlib_zher2k( uplo, 'NO TRANSPOSE', pn, pk,-cone, a( i+kd, i ), lda,work( &
                            wpos ), ldw,one, a( i+kd, i+kd ), lda )
                  ! ==================================================================
                  ! restore a for comparison and checking to be removed
                   ! do 45 j = i, i+pk-1
                      ! llk = min( kd, n-j ) + 1
                      ! call stdlib_zcopy( nlk, ab( 1, j ), 1, a( j, j ), 1 )
                      45 continue
                  ! ==================================================================
               end do loop_40
              ! copy the lower band to ab which is the band storage matrix
              do j = n-kd+1, n
                 llk = min(kd, n-j) + 1_ilp
                 call stdlib_zcopy( llk, a( j, j ), 1_ilp, ab( 1_ilp, j ), 1_ilp )
              end do
           end if
           work( 1_ilp ) = lwmin
           return
     end subroutine stdlib_zhetrd_he2hb




     module subroutine stdlib_chetrd_hb2st( stage1, vect, uplo, n, kd, ab, ldab,d, e, hous, lhous, &
     !! CHETRD_HB2ST reduces a complex Hermitian band matrix A to real symmetric
     !! tridiagonal form T by a unitary similarity transformation:
     !! Q**H * A * Q = T.
               work, lwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: stage1, uplo, vect
           integer(ilp), intent(in) :: n, kd, ldab, lhous, lwork
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           real(sp), intent(out) :: d(*), e(*)
           complex(sp), intent(inout) :: ab(ldab,*)
           complex(sp), intent(out) :: hous(*), work(*)
        ! =====================================================================
           ! Parameters 
           
           ! Local Scalars 
           logical(lk) :: lquery, wantq, upper, afters1
           integer(ilp) :: i, m, k, ib, sweepid, myid, shift, stt, st, ed, stind, edind, &
           blklastind, colpt, thed, stepercol, grsiz, thgrsiz, thgrnb, thgrid, nbtiles, ttype, &
           tid, nthreads, debug, abdpos, abofdpos, dpos, ofdpos, awpos, inda, indw, apos, sizea, &
                     lda, indv, indtau, sicev, sizetau, ldv, lhmin, lwmin
           real(sp) :: abstmp
           complex(sp) :: tmp
           ! Intrinsic Functions 
           ! Executable Statements 
           ! determine the minimal workspace size required.
           ! test the input parameters
           debug   = 0_ilp
           info    = 0_ilp
           afters1 = stdlib_lsame( stage1, 'Y' )
           wantq   = stdlib_lsame( vect, 'V' )
           upper   = stdlib_lsame( uplo, 'U' )
           lquery  = ( lwork==-1_ilp ) .or. ( lhous==-1_ilp )
           ! determine the block size, the workspace size and the hous size.
           ib     = stdlib_ilaenv2stage( 2_ilp, 'CHETRD_HB2ST', vect, n, kd, -1_ilp, -1_ilp )
           lhmin  = stdlib_ilaenv2stage( 3_ilp, 'CHETRD_HB2ST', vect, n, kd, ib, -1_ilp )
           lwmin  = stdlib_ilaenv2stage( 4_ilp, 'CHETRD_HB2ST', vect, n, kd, ib, -1_ilp )
           if( .not.afters1 .and. .not.stdlib_lsame( stage1, 'N' ) ) then
              info = -1_ilp
           else if( .not.stdlib_lsame( vect, 'N' ) ) then
              info = -2_ilp
           else if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( kd<0_ilp ) then
              info = -5_ilp
           else if( ldab<(kd+1) ) then
              info = -7_ilp
           else if( lhous<lhmin .and. .not.lquery ) then
              info = -11_ilp
           else if( lwork<lwmin .and. .not.lquery ) then
              info = -13_ilp
           end if
           if( info==0_ilp ) then
              hous( 1_ilp ) = lhmin
              work( 1_ilp ) = lwmin
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'CHETRD_HB2ST', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0_ilp ) then
               hous( 1_ilp ) = 1_ilp
               work( 1_ilp ) = 1_ilp
               return
           end if
           ! determine pointer position
           ldv      = kd + ib
           sizetau  = 2_ilp * n
           sicev    = 2_ilp * n
           indtau   = 1_ilp
           indv     = indtau + sizetau
           lda      = 2_ilp * kd + 1_ilp
           sizea    = lda * n
           inda     = 1_ilp
           indw     = inda + sizea
           nthreads = 1_ilp
           tid      = 0_ilp
           if( upper ) then
               apos     = inda + kd
               awpos    = inda
               dpos     = apos + kd
               ofdpos   = dpos - 1_ilp
               abdpos   = kd + 1_ilp
               abofdpos = kd
           else
               apos     = inda
               awpos    = inda + kd + 1_ilp
               dpos     = apos
               ofdpos   = dpos + 1_ilp
               abdpos   = 1_ilp
               abofdpos = 2_ilp
           endif
           ! case kd=0:
           ! the matrix is diagonal. we just copy it (convert to "real" for
           ! complex because d is double and the imaginary part should be 0)
           ! and store it in d. a sequential code here is better or
           ! in a parallel environment it might need two cores for d and e
           if( kd==0_ilp ) then
               do i = 1, n
                   d( i ) = real( ab( abdpos, i ),KIND=sp)
               end do
               do i = 1, n-1
                   e( i ) = zero
               end do
               hous( 1_ilp ) = 1_ilp
               work( 1_ilp ) = 1_ilp
               return
           end if
           ! case kd=1:
           ! the matrix is already tridiagonal. we have to make diagonal
           ! and offdiagonal elements real, and store them in d and e.
           ! for that, for real precision just copy the diag and offdiag
           ! to d and e while for the complex case the bulge chasing is
           ! performed to convert the hermetian tridiagonal to symmetric
           ! tridiagonal. a simpler conversion formula might be used, but then
           ! updating the q matrix will be required and based if q is generated
           ! or not this might complicate the story.
           if( kd==1_ilp ) then
               do i = 1, n
                   d( i ) = real( ab( abdpos, i ),KIND=sp)
               end do
               ! make off-diagonal elements real and copy them to e
               if( upper ) then
                   do i = 1, n - 1
                       tmp = ab( abofdpos, i+1 )
                       abstmp = abs( tmp )
                       ab( abofdpos, i+1 ) = abstmp
                       e( i ) = abstmp
                       if( abstmp/=zero ) then
                          tmp = tmp / abstmp
                       else
                          tmp = cone
                       end if
                       if( i<n-1 )ab( abofdpos, i+2 ) = ab( abofdpos, i+2 )*tmp
                        ! if( wantz ) then
                           ! call stdlib_cscal( n, conjg( tmp ), q( 1, i+1 ), 1 )
                        ! end if
                   end do
               else
                   do i = 1, n - 1
                      tmp = ab( abofdpos, i )
                      abstmp = abs( tmp )
                      ab( abofdpos, i ) = abstmp
                      e( i ) = abstmp
                      if( abstmp/=zero ) then
                         tmp = tmp / abstmp
                      else
                         tmp = cone
                      end if
                      if( i<n-1 )ab( abofdpos, i+1 ) = ab( abofdpos, i+1 )*tmp
                       ! if( wantq ) then
                          ! call stdlib_cscal( n, tmp, q( 1, i+1 ), 1 )
                       ! end if
                   end do
               endif
               hous( 1_ilp ) = 1_ilp
               work( 1_ilp ) = 1_ilp
               return
           end if
           ! main code start here.
           ! reduce the hermitian band of a to a tridiagonal matrix.
           thgrsiz   = n
           grsiz     = 1_ilp
           shift     = 3_ilp
           nbtiles   = ceiling( real(n,KIND=sp)/real(kd,KIND=sp) )
           stepercol = ceiling( real(shift,KIND=sp)/real(grsiz,KIND=sp) )
           thgrnb    = ceiling( real(n-1,KIND=sp)/real(thgrsiz,KIND=sp) )
           call stdlib_clacpy( "A", kd+1, n, ab, ldab, work( apos ), lda )
           call stdlib_claset( "A", kd,   n, czero, czero, work( awpos ), lda )
           ! openmp parallelisation start here
           !$OMP PARALLEL PRIVATE( TID, THGRID, BLKLASTIND ) &
           !$OMP&         PRIVATE( THED, I, M, K, ST, ED, STT, SWEEPID ) &
           !$OMP&         PRIVATE( MYID, TTYPE, COLPT, STIND, EDIND ) &
           !$OMP&         SHARED ( UPLO, WANTQ, INDV, INDTAU, HOUS, WORK) &
           !$OMP&         SHARED ( N, KD, IB, NBTILES, LDA, LDV, INDA ) &
           !$OMP&         SHARED ( STEPERCOL, THGRNB, THGRSIZ, GRSIZ, SHIFT )
           !$OMP MASTER
           ! main bulge chasing loop
           loop_100: do thgrid = 1, thgrnb
               stt  = (thgrid-1)*thgrsiz+1
               thed = min( (stt + thgrsiz -1_ilp), (n-1))
               loop_110: do i = stt, n-1
                   ed = min( i, thed )
                   if( stt>ed ) exit
                   loop_120: do m = 1, stepercol
                       st = stt
                       loop_130: do sweepid = st, ed
                           loop_140: do k = 1, grsiz
                               myid  = (i-sweepid)*(stepercol*grsiz)+ (m-1)*grsiz + k
                               if ( myid==1_ilp ) then
                                   ttype = 1_ilp
                               else
                                   ttype = mod( myid, 2_ilp ) + 2_ilp
                               endif
                               if( ttype==2_ilp ) then
                                   colpt      = (myid/2_ilp)*kd + sweepid
                                   stind      = colpt-kd+1
                                   edind      = min(colpt,n)
                                   blklastind = colpt
                               else
                                   colpt      = ((myid+1)/2_ilp)*kd + sweepid
                                   stind      = colpt-kd+1
                                   edind      = min(colpt,n)
                                   if( ( stind>=edind-1 ).and.( edind==n ) ) then
                                       blklastind = n
                                   else
                                       blklastind = 0_ilp
                                   endif
                               endif
                               ! call the kernel
                               !$ if( ttype/=1 ) then
                                   !$OMP TASK DEPEND(in:WORK(MYID+SHIFT-1)) &
                                   !$OMP&     DEPEND(in:WORK(MYID-1)) &
                                   !$OMP&     DEPEND(out:WORK(MYID))
                                   !$ tid      = omp_get_thread_num()
                                   !$ call stdlib_chb2st_kernels( uplo, wantq, ttype,stind, edind, &
                                   !$ sweepid, n, kd, ib,work ( inda ), lda,hous( indv ), hous( &
                                   !$ indtau ), ldv,work( indw + tid*kd ) )
                               !$OMP END TASK
                                   !$ else
                                   !$OMP TASK DEPEND(in:WORK(MYID+SHIFT-1)) &
                                   !$OMP&     DEPEND(out:WORK(MYID))
                                   !$ tid      = omp_get_thread_num()
                                   call stdlib_chb2st_kernels( uplo, wantq, ttype,stind, edind, &
                                   sweepid, n, kd, ib,work ( inda ), lda,hous( indv ), hous( &
                                            indtau ), ldv,work( indw + tid*kd ) )
                                   !$OMP END TASK
                               !$ endif
                               if ( blklastind>=(n-1) ) then
                                   stt = stt + 1_ilp
                                   exit
                               endif
                           end do loop_140
                       end do loop_130
                   end do loop_120
               end do loop_110
           end do loop_100
           !$OMP END MASTER
           !$OMP END PARALLEL
           ! copy the diagonal from a to d. note that d is real thus only
           ! the real part is needed, the imaginary part should be czero.
           do i = 1, n
               d( i ) = real( work( dpos+(i-1)*lda ),KIND=sp)
           end do
           ! copy the off diagonal from a to e. note that e is real thus only
           ! the real part is needed, the imaginary part should be czero.
           if( upper ) then
               do i = 1, n-1
                  e( i ) = real( work( ofdpos+i*lda ),KIND=sp)
               end do
           else
               do i = 1, n-1
                  e( i ) = real( work( ofdpos+(i-1)*lda ),KIND=sp)
               end do
           endif
           hous( 1_ilp ) = lhmin
           work( 1_ilp ) = lwmin
           return
     end subroutine stdlib_chetrd_hb2st

     module subroutine stdlib_zhetrd_hb2st( stage1, vect, uplo, n, kd, ab, ldab,d, e, hous, lhous, &
     !! ZHETRD_HB2ST reduces a complex Hermitian band matrix A to real symmetric
     !! tridiagonal form T by a unitary similarity transformation:
     !! Q**H * A * Q = T.
               work, lwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: stage1, uplo, vect
           integer(ilp), intent(in) :: n, kd, ldab, lhous, lwork
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           real(dp), intent(out) :: d(*), e(*)
           complex(dp), intent(inout) :: ab(ldab,*)
           complex(dp), intent(out) :: hous(*), work(*)
        ! =====================================================================
           ! Parameters 
           
           ! Local Scalars 
           logical(lk) :: lquery, wantq, upper, afters1
           integer(ilp) :: i, m, k, ib, sweepid, myid, shift, stt, st, ed, stind, edind, &
           blklastind, colpt, thed, stepercol, grsiz, thgrsiz, thgrnb, thgrid, nbtiles, ttype, &
           tid, nthreads, debug, abdpos, abofdpos, dpos, ofdpos, awpos, inda, indw, apos, sizea, &
                     lda, indv, indtau, sizev, sizetau, ldv, lhmin, lwmin
           real(dp) :: abstmp
           complex(dp) :: tmp
           ! Intrinsic Functions 
           ! Executable Statements 
           ! determine the minimal workspace size required.
           ! test the input parameters
           debug   = 0_ilp
           info    = 0_ilp
           afters1 = stdlib_lsame( stage1, 'Y' )
           wantq   = stdlib_lsame( vect, 'V' )
           upper   = stdlib_lsame( uplo, 'U' )
           lquery  = ( lwork==-1_ilp ) .or. ( lhous==-1_ilp )
           ! determine the block size, the workspace size and the hous size.
           ib     = stdlib_ilaenv2stage( 2_ilp, 'ZHETRD_HB2ST', vect, n, kd, -1_ilp, -1_ilp )
           lhmin  = stdlib_ilaenv2stage( 3_ilp, 'ZHETRD_HB2ST', vect, n, kd, ib, -1_ilp )
           lwmin  = stdlib_ilaenv2stage( 4_ilp, 'ZHETRD_HB2ST', vect, n, kd, ib, -1_ilp )
           if( .not.afters1 .and. .not.stdlib_lsame( stage1, 'N' ) ) then
              info = -1_ilp
           else if( .not.stdlib_lsame( vect, 'N' ) ) then
              info = -2_ilp
           else if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( kd<0_ilp ) then
              info = -5_ilp
           else if( ldab<(kd+1) ) then
              info = -7_ilp
           else if( lhous<lhmin .and. .not.lquery ) then
              info = -11_ilp
           else if( lwork<lwmin .and. .not.lquery ) then
              info = -13_ilp
           end if
           if( info==0_ilp ) then
              hous( 1_ilp ) = lhmin
              work( 1_ilp ) = lwmin
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'ZHETRD_HB2ST', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0_ilp ) then
               hous( 1_ilp ) = 1_ilp
               work( 1_ilp ) = 1_ilp
               return
           end if
           ! determine pointer position
           ldv      = kd + ib
           sizetau  = 2_ilp * n
           sizev    = 2_ilp * n
           indtau   = 1_ilp
           indv     = indtau + sizetau
           lda      = 2_ilp * kd + 1_ilp
           sizea    = lda * n
           inda     = 1_ilp
           indw     = inda + sizea
           nthreads = 1_ilp
           tid      = 0_ilp
           if( upper ) then
               apos     = inda + kd
               awpos    = inda
               dpos     = apos + kd
               ofdpos   = dpos - 1_ilp
               abdpos   = kd + 1_ilp
               abofdpos = kd
           else
               apos     = inda
               awpos    = inda + kd + 1_ilp
               dpos     = apos
               ofdpos   = dpos + 1_ilp
               abdpos   = 1_ilp
               abofdpos = 2_ilp
           endif
           ! case kd=0:
           ! the matrix is diagonal. we just copy it (convert to "real" for
           ! complex because d is double and the imaginary part should be 0)
           ! and store it in d. a sequential code here is better or
           ! in a parallel environment it might need two cores for d and e
           if( kd==0_ilp ) then
               do i = 1, n
                   d( i ) = real( ab( abdpos, i ),KIND=dp)
               end do
               do i = 1, n-1
                   e( i ) = zero
               end do
               hous( 1_ilp ) = 1_ilp
               work( 1_ilp ) = 1_ilp
               return
           end if
           ! case kd=1:
           ! the matrix is already tridiagonal. we have to make diagonal
           ! and offdiagonal elements real, and store them in d and e.
           ! for that, for real precision just copy the diag and offdiag
           ! to d and e while for the complex case the bulge chasing is
           ! performed to convert the hermetian tridiagonal to symmetric
           ! tridiagonal. a simpler conversion formula might be used, but then
           ! updating the q matrix will be required and based if q is generated
           ! or not this might complicate the story.
           if( kd==1_ilp ) then
               do i = 1, n
                   d( i ) = real( ab( abdpos, i ),KIND=dp)
               end do
               ! make off-diagonal elements real and copy them to e
               if( upper ) then
                   do i = 1, n - 1
                       tmp = ab( abofdpos, i+1 )
                       abstmp = abs( tmp )
                       ab( abofdpos, i+1 ) = abstmp
                       e( i ) = abstmp
                       if( abstmp/=zero ) then
                          tmp = tmp / abstmp
                       else
                          tmp = cone
                       end if
                       if( i<n-1 )ab( abofdpos, i+2 ) = ab( abofdpos, i+2 )*tmp
                        ! if( wantz ) then
                           ! call stdlib_zscal( n, conjg( tmp ), q( 1, i+1 ), 1 )
                        ! end if
                   end do
               else
                   do i = 1, n - 1
                      tmp = ab( abofdpos, i )
                      abstmp = abs( tmp )
                      ab( abofdpos, i ) = abstmp
                      e( i ) = abstmp
                      if( abstmp/=zero ) then
                         tmp = tmp / abstmp
                      else
                         tmp = cone
                      end if
                      if( i<n-1 )ab( abofdpos, i+1 ) = ab( abofdpos, i+1 )*tmp
                       ! if( wantq ) then
                          ! call stdlib_zscal( n, tmp, q( 1, i+1 ), 1 )
                       ! end if
                   end do
               endif
               hous( 1_ilp ) = 1_ilp
               work( 1_ilp ) = 1_ilp
               return
           end if
           ! main code start here.
           ! reduce the hermitian band of a to a tridiagonal matrix.
           thgrsiz   = n
           grsiz     = 1_ilp
           shift     = 3_ilp
           nbtiles   = ceiling( real(n,KIND=dp)/real(kd,KIND=dp) )
           stepercol = ceiling( real(shift,KIND=dp)/real(grsiz,KIND=dp) )
           thgrnb    = ceiling( real(n-1,KIND=dp)/real(thgrsiz,KIND=dp) )
           call stdlib_zlacpy( "A", kd+1, n, ab, ldab, work( apos ), lda )
           call stdlib_zlaset( "A", kd,   n, czero, czero, work( awpos ), lda )
           ! openmp parallelisation start here
           !$OMP PARALLEL PRIVATE( TID, THGRID, BLKLASTIND ) &
           !$OMP&         PRIVATE( THED, I, M, K, ST, ED, STT, SWEEPID ) &
           !$OMP&         PRIVATE( MYID, TTYPE, COLPT, STIND, EDIND ) &
           !$OMP&         SHARED ( UPLO, WANTQ, INDV, INDTAU, HOUS, WORK) &
           !$OMP&         SHARED ( N, KD, IB, NBTILES, LDA, LDV, INDA ) &
           !$OMP&         SHARED ( STEPERCOL, THGRNB, THGRSIZ, GRSIZ, SHIFT )
           !$OMP MASTER
           ! main bulge chasing loop
           loop_100: do thgrid = 1, thgrnb
               stt  = (thgrid-1)*thgrsiz+1
               thed = min( (stt + thgrsiz -1_ilp), (n-1))
               loop_110: do i = stt, n-1
                   ed = min( i, thed )
                   if( stt>ed ) exit
                   loop_120: do m = 1, stepercol
                       st = stt
                       loop_130: do sweepid = st, ed
                           loop_140: do k = 1, grsiz
                               myid  = (i-sweepid)*(stepercol*grsiz)+ (m-1)*grsiz + k
                               if ( myid==1_ilp ) then
                                   ttype = 1_ilp
                               else
                                   ttype = mod( myid, 2_ilp ) + 2_ilp
                               endif
                               if( ttype==2_ilp ) then
                                   colpt      = (myid/2_ilp)*kd + sweepid
                                   stind      = colpt-kd+1
                                   edind      = min(colpt,n)
                                   blklastind = colpt
                               else
                                   colpt      = ((myid+1)/2_ilp)*kd + sweepid
                                   stind      = colpt-kd+1
                                   edind      = min(colpt,n)
                                   if( ( stind>=edind-1 ).and.( edind==n ) ) then
                                       blklastind = n
                                   else
                                       blklastind = 0_ilp
                                   endif
                               endif
                               ! call the kernel
                               !$ if( ttype/=1 ) then
                                   !$OMP TASK DEPEND(in:WORK(MYID+SHIFT-1)) &
                                   !$OMP&     DEPEND(in:WORK(MYID-1)) &
                                   !$OMP&     DEPEND(out:WORK(MYID))
                                   !$ tid      = omp_get_thread_num()
                                   !$ call stdlib_zhb2st_kernels( uplo, wantq, ttype,stind, edind, &
                                   !$ sweepid, n, kd, ib,work ( inda ), lda,hous( indv ), hous( &
                                   !$ indtau ), ldv,work( indw + tid*kd ) )
                                   !$OMP END TASK
                               !$ else
                                   !$OMP TASK DEPEND(in:WORK(MYID+SHIFT-1)) &
                                   !$OMP&     DEPEND(out:WORK(MYID))
                                   !$ tid      = omp_get_thread_num()
                                   call stdlib_zhb2st_kernels( uplo, wantq, ttype,stind, edind, &
                                   sweepid, n, kd, ib,work ( inda ), lda,hous( indv ), hous( &
                                             indtau ), ldv,work( indw + tid*kd ) )
                               !$OMP END TASK
                               !$ endif
                               if ( blklastind>=(n-1) ) then
                                   stt = stt + 1_ilp
                                   exit
                               endif
                           end do loop_140
                       end do loop_130
                   end do loop_120
               end do loop_110
           end do loop_100
           !$OMP END MASTER
           !$OMP END PARALLEL
           ! copy the diagonal from a to d. note that d is real thus only
           ! the real part is needed, the imaginary part should be czero.
           do i = 1, n
               d( i ) = real( work( dpos+(i-1)*lda ),KIND=dp)
           end do
           ! copy the off diagonal from a to e. note that e is real thus only
           ! the real part is needed, the imaginary part should be czero.
           if( upper ) then
               do i = 1, n-1
                  e( i ) = real( work( ofdpos+i*lda ),KIND=dp)
               end do
           else
               do i = 1, n-1
                  e( i ) = real( work( ofdpos+(i-1)*lda ),KIND=dp)
               end do
           endif
           hous( 1_ilp ) = lhmin
           work( 1_ilp ) = lwmin
           return
     end subroutine stdlib_zhetrd_hb2st




     pure module subroutine stdlib_chb2st_kernels( uplo, wantz, ttype,st, ed, sweep, n, nb, ib,a, lda, &
     !! CHB2ST_KERNELS is an internal routine used by the CHETRD_HB2ST
     !! subroutine.
               v, tau, ldvt, work)
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           logical(lk), intent(in) :: wantz
           integer(ilp), intent(in) :: ttype, st, ed, sweep, n, nb, ib, lda, ldvt
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: v(*), tau(*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: i, j1, j2, lm, ln, vpos, taupos, dpos, ofdpos, ajeter
           complex(sp) :: ctmp
           ! Intrinsic Functions 
           ! Executable Statements 
           ajeter = ib + ldvt
           upper = stdlib_lsame( uplo, 'U' )
           if( upper ) then
               dpos    = 2_ilp * nb + 1_ilp
               ofdpos  = 2_ilp * nb
           else
               dpos    = 1_ilp
               ofdpos  = 2_ilp
           endif
           ! upper case
           if( upper ) then
               if( wantz ) then
                   vpos   = mod( sweep-1, 2_ilp ) * n + st
                   taupos = mod( sweep-1, 2_ilp ) * n + st
               else
                   vpos   = mod( sweep-1, 2_ilp ) * n + st
                   taupos = mod( sweep-1, 2_ilp ) * n + st
               endif
               if( ttype==1_ilp ) then
                   lm = ed - st + 1_ilp
                   v( vpos ) = cone
                   do i = 1, lm-1
                       v( vpos+i )         = conjg( a( ofdpos-i, st+i ) )
                       a( ofdpos-i, st+i ) = czero
                   end do
                   ctmp = conjg( a( ofdpos, st ) )
                   call stdlib_clarfg( lm, ctmp, v( vpos+1 ), 1_ilp,tau( taupos ) )
                   a( ofdpos, st ) = ctmp
                   lm = ed - st + 1_ilp
                   call stdlib_clarfy( uplo, lm, v( vpos ), 1_ilp,conjg( tau( taupos ) ),a( dpos, st )&
                             , lda-1, work)
               endif
               if( ttype==3_ilp ) then
                   lm = ed - st + 1_ilp
                   call stdlib_clarfy( uplo, lm, v( vpos ), 1_ilp,conjg( tau( taupos ) ),a( dpos, st )&
                             , lda-1, work)
               endif
               if( ttype==2_ilp ) then
                   j1 = ed+1
                   j2 = min( ed+nb, n )
                   ln = ed-st+1
                   lm = j2-j1+1
                   if( lm>0_ilp) then
                       call stdlib_clarfx( 'LEFT', ln, lm, v( vpos ),conjg( tau( taupos ) ),a( &
                                 dpos-nb, j1 ), lda-1, work)
                       if( wantz ) then
                           vpos   = mod( sweep-1, 2_ilp ) * n + j1
                           taupos = mod( sweep-1, 2_ilp ) * n + j1
                       else
                           vpos   = mod( sweep-1, 2_ilp ) * n + j1
                           taupos = mod( sweep-1, 2_ilp ) * n + j1
                       endif
                       v( vpos ) = cone
                       do i = 1, lm-1
                           v( vpos+i )          =conjg( a( dpos-nb-i, j1+i ) )
                           a( dpos-nb-i, j1+i ) = czero
                       end do
                       ctmp = conjg( a( dpos-nb, j1 ) )
                       call stdlib_clarfg( lm, ctmp, v( vpos+1 ), 1_ilp, tau( taupos ) )
                       a( dpos-nb, j1 ) = ctmp
                       call stdlib_clarfx( 'RIGHT', ln-1, lm, v( vpos ),tau( taupos ),a( dpos-nb+&
                                 1_ilp, j1 ), lda-1, work)
                   endif
               endif
           ! lower case
           else
               if( wantz ) then
                   vpos   = mod( sweep-1, 2_ilp ) * n + st
                   taupos = mod( sweep-1, 2_ilp ) * n + st
               else
                   vpos   = mod( sweep-1, 2_ilp ) * n + st
                   taupos = mod( sweep-1, 2_ilp ) * n + st
               endif
               if( ttype==1_ilp ) then
                   lm = ed - st + 1_ilp
                   v( vpos ) = cone
                   do i = 1, lm-1
                       v( vpos+i )         = a( ofdpos+i, st-1 )
                       a( ofdpos+i, st-1 ) = czero
                   end do
                   call stdlib_clarfg( lm, a( ofdpos, st-1 ), v( vpos+1 ), 1_ilp,tau( taupos ) )
                             
                   lm = ed - st + 1_ilp
                   call stdlib_clarfy( uplo, lm, v( vpos ), 1_ilp,conjg( tau( taupos ) ),a( dpos, st )&
                             , lda-1, work)
               endif
               if( ttype==3_ilp ) then
                   lm = ed - st + 1_ilp
                   call stdlib_clarfy( uplo, lm, v( vpos ), 1_ilp,conjg( tau( taupos ) ),a( dpos, st )&
                             , lda-1, work)
               endif
               if( ttype==2_ilp ) then
                   j1 = ed+1
                   j2 = min( ed+nb, n )
                   ln = ed-st+1
                   lm = j2-j1+1
                   if( lm>0_ilp) then
                       call stdlib_clarfx( 'RIGHT', lm, ln, v( vpos ),tau( taupos ), a( dpos+nb, &
                                 st ),lda-1, work)
                       if( wantz ) then
                           vpos   = mod( sweep-1, 2_ilp ) * n + j1
                           taupos = mod( sweep-1, 2_ilp ) * n + j1
                       else
                           vpos   = mod( sweep-1, 2_ilp ) * n + j1
                           taupos = mod( sweep-1, 2_ilp ) * n + j1
                       endif
                       v( vpos ) = cone
                       do i = 1, lm-1
                           v( vpos+i )        = a( dpos+nb+i, st )
                           a( dpos+nb+i, st ) = czero
                       end do
                       call stdlib_clarfg( lm, a( dpos+nb, st ), v( vpos+1 ), 1_ilp,tau( taupos ) )
                                 
                       call stdlib_clarfx( 'LEFT', lm, ln-1, v( vpos ),conjg( tau( taupos ) ),a( &
                                 dpos+nb-1, st+1 ), lda-1, work)
                   endif
               endif
           endif
           return
     end subroutine stdlib_chb2st_kernels

     pure module subroutine stdlib_zhb2st_kernels( uplo, wantz, ttype,st, ed, sweep, n, nb, ib,a, lda, &
     !! ZHB2ST_KERNELS is an internal routine used by the ZHETRD_HB2ST
     !! subroutine.
               v, tau, ldvt, work)
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           logical(lk), intent(in) :: wantz
           integer(ilp), intent(in) :: ttype, st, ed, sweep, n, nb, ib, lda, ldvt
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: v(*), tau(*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp) :: i, j1, j2, lm, ln, vpos, taupos, dpos, ofdpos, ajeter
           complex(dp) :: ctmp
           ! Intrinsic Functions 
           ! Executable Statements 
           ajeter = ib + ldvt
           upper = stdlib_lsame( uplo, 'U' )
           if( upper ) then
               dpos    = 2_ilp * nb + 1_ilp
               ofdpos  = 2_ilp * nb
           else
               dpos    = 1_ilp
               ofdpos  = 2_ilp
           endif
           ! upper case
           if( upper ) then
               if( wantz ) then
                   vpos   = mod( sweep-1, 2_ilp ) * n + st
                   taupos = mod( sweep-1, 2_ilp ) * n + st
               else
                   vpos   = mod( sweep-1, 2_ilp ) * n + st
                   taupos = mod( sweep-1, 2_ilp ) * n + st
               endif
               if( ttype==1_ilp ) then
                   lm = ed - st + 1_ilp
                   v( vpos ) = cone
                   do i = 1, lm-1
                       v( vpos+i )         = conjg( a( ofdpos-i, st+i ) )
                       a( ofdpos-i, st+i ) = czero
                   end do
                   ctmp = conjg( a( ofdpos, st ) )
                   call stdlib_zlarfg( lm, ctmp, v( vpos+1 ), 1_ilp,tau( taupos ) )
                   a( ofdpos, st ) = ctmp
                   lm = ed - st + 1_ilp
                   call stdlib_zlarfy( uplo, lm, v( vpos ), 1_ilp,conjg( tau( taupos ) ),a( dpos, st )&
                             , lda-1, work)
               endif
               if( ttype==3_ilp ) then
                   lm = ed - st + 1_ilp
                   call stdlib_zlarfy( uplo, lm, v( vpos ), 1_ilp,conjg( tau( taupos ) ),a( dpos, st )&
                             , lda-1, work)
               endif
               if( ttype==2_ilp ) then
                   j1 = ed+1
                   j2 = min( ed+nb, n )
                   ln = ed-st+1
                   lm = j2-j1+1
                   if( lm>0_ilp) then
                       call stdlib_zlarfx( 'LEFT', ln, lm, v( vpos ),conjg( tau( taupos ) ),a( &
                                 dpos-nb, j1 ), lda-1, work)
                       if( wantz ) then
                           vpos   = mod( sweep-1, 2_ilp ) * n + j1
                           taupos = mod( sweep-1, 2_ilp ) * n + j1
                       else
                           vpos   = mod( sweep-1, 2_ilp ) * n + j1
                           taupos = mod( sweep-1, 2_ilp ) * n + j1
                       endif
                       v( vpos ) = cone
                       do i = 1, lm-1
                           v( vpos+i )          =conjg( a( dpos-nb-i, j1+i ) )
                           a( dpos-nb-i, j1+i ) = czero
                       end do
                       ctmp = conjg( a( dpos-nb, j1 ) )
                       call stdlib_zlarfg( lm, ctmp, v( vpos+1 ), 1_ilp, tau( taupos ) )
                       a( dpos-nb, j1 ) = ctmp
                       call stdlib_zlarfx( 'RIGHT', ln-1, lm, v( vpos ),tau( taupos ),a( dpos-nb+&
                                 1_ilp, j1 ), lda-1, work)
                   endif
               endif
           ! lower case
           else
               if( wantz ) then
                   vpos   = mod( sweep-1, 2_ilp ) * n + st
                   taupos = mod( sweep-1, 2_ilp ) * n + st
               else
                   vpos   = mod( sweep-1, 2_ilp ) * n + st
                   taupos = mod( sweep-1, 2_ilp ) * n + st
               endif
               if( ttype==1_ilp ) then
                   lm = ed - st + 1_ilp
                   v( vpos ) = cone
                   do i = 1, lm-1
                       v( vpos+i )         = a( ofdpos+i, st-1 )
                       a( ofdpos+i, st-1 ) = czero
                   end do
                   call stdlib_zlarfg( lm, a( ofdpos, st-1 ), v( vpos+1 ), 1_ilp,tau( taupos ) )
                             
                   lm = ed - st + 1_ilp
                   call stdlib_zlarfy( uplo, lm, v( vpos ), 1_ilp,conjg( tau( taupos ) ),a( dpos, st )&
                             , lda-1, work)
               endif
               if( ttype==3_ilp ) then
                   lm = ed - st + 1_ilp
                   call stdlib_zlarfy( uplo, lm, v( vpos ), 1_ilp,conjg( tau( taupos ) ),a( dpos, st )&
                             , lda-1, work)
               endif
               if( ttype==2_ilp ) then
                   j1 = ed+1
                   j2 = min( ed+nb, n )
                   ln = ed-st+1
                   lm = j2-j1+1
                   if( lm>0_ilp) then
                       call stdlib_zlarfx( 'RIGHT', lm, ln, v( vpos ),tau( taupos ), a( dpos+nb, &
                                 st ),lda-1, work)
                       if( wantz ) then
                           vpos   = mod( sweep-1, 2_ilp ) * n + j1
                           taupos = mod( sweep-1, 2_ilp ) * n + j1
                       else
                           vpos   = mod( sweep-1, 2_ilp ) * n + j1
                           taupos = mod( sweep-1, 2_ilp ) * n + j1
                       endif
                       v( vpos ) = cone
                       do i = 1, lm-1
                           v( vpos+i )        = a( dpos+nb+i, st )
                           a( dpos+nb+i, st ) = czero
                       end do
                       call stdlib_zlarfg( lm, a( dpos+nb, st ), v( vpos+1 ), 1_ilp,tau( taupos ) )
                                 
                       call stdlib_zlarfx( 'LEFT', lm, ln-1, v( vpos ),conjg( tau( taupos ) ),a( &
                                 dpos+nb-1, st+1 ), lda-1, work)
                   endif
               endif
           endif
           return
     end subroutine stdlib_zhb2st_kernels




     module subroutine stdlib_ssytrd_sb2st( stage1, vect, uplo, n, kd, ab, ldab,d, e, hous, lhous, &
     !! SSYTRD_SB2ST reduces a real symmetric band matrix A to real symmetric
     !! tridiagonal form T by a orthogonal similarity transformation:
     !! Q**T * A * Q = T.
               work, lwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: stage1, uplo, vect
           integer(ilp), intent(in) :: n, kd, ldab, lhous, lwork
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           real(sp), intent(out) :: d(*), e(*)
           real(sp), intent(inout) :: ab(ldab,*)
           real(sp), intent(out) :: hous(*), work(*)
        ! =====================================================================
           ! Parameters 
           
           ! Local Scalars 
           logical(lk) :: lquery, wantq, upper, afters1
           integer(ilp) :: i, m, k, ib, sweepid, myid, shift, stt, st, ed, stind, edind, &
           blklastind, colpt, thed, stepercol, grsiz, thgrsiz, thgrnb, thgrid, nbtiles, ttype, &
           tid, nthreads, debug, abdpos, abofdpos, dpos, ofdpos, awpos, inda, indw, apos, sizea, &
                     lda, indv, indtau, sisev, sizetau, ldv, lhmin, lwmin
           ! Intrinsic Functions 
           ! Executable Statements 
           ! determine the minimal workspace size required.
           ! test the input parameters
           debug   = 0_ilp
           info    = 0_ilp
           afters1 = stdlib_lsame( stage1, 'Y' )
           wantq   = stdlib_lsame( vect, 'V' )
           upper   = stdlib_lsame( uplo, 'U' )
           lquery  = ( lwork==-1_ilp ) .or. ( lhous==-1_ilp )
           ! determine the block size, the workspace size and the hous size.
           ib     = stdlib_ilaenv2stage( 2_ilp, 'SSYTRD_SB2ST', vect, n, kd, -1_ilp, -1_ilp )
           lhmin  = stdlib_ilaenv2stage( 3_ilp, 'SSYTRD_SB2ST', vect, n, kd, ib, -1_ilp )
           lwmin  = stdlib_ilaenv2stage( 4_ilp, 'SSYTRD_SB2ST', vect, n, kd, ib, -1_ilp )
           if( .not.afters1 .and. .not.stdlib_lsame( stage1, 'N' ) ) then
              info = -1_ilp
           else if( .not.stdlib_lsame( vect, 'N' ) ) then
              info = -2_ilp
           else if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( kd<0_ilp ) then
              info = -5_ilp
           else if( ldab<(kd+1) ) then
              info = -7_ilp
           else if( lhous<lhmin .and. .not.lquery ) then
              info = -11_ilp
           else if( lwork<lwmin .and. .not.lquery ) then
              info = -13_ilp
           end if
           if( info==0_ilp ) then
              hous( 1_ilp ) = lhmin
              work( 1_ilp ) = lwmin
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SSYTRD_SB2ST', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0_ilp ) then
               hous( 1_ilp ) = 1_ilp
               work( 1_ilp ) = 1_ilp
               return
           end if
           ! determine pointer position
           ldv      = kd + ib
           sizetau  = 2_ilp * n
           sisev    = 2_ilp * n
           indtau   = 1_ilp
           indv     = indtau + sizetau
           lda      = 2_ilp * kd + 1_ilp
           sizea    = lda * n
           inda     = 1_ilp
           indw     = inda + sizea
           nthreads = 1_ilp
           tid      = 0_ilp
           if( upper ) then
               apos     = inda + kd
               awpos    = inda
               dpos     = apos + kd
               ofdpos   = dpos - 1_ilp
               abdpos   = kd + 1_ilp
               abofdpos = kd
           else
               apos     = inda
               awpos    = inda + kd + 1_ilp
               dpos     = apos
               ofdpos   = dpos + 1_ilp
               abdpos   = 1_ilp
               abofdpos = 2_ilp
           endif
           ! case kd=0:
           ! the matrix is diagonal. we just copy it (convert to "real" for
           ! real because d is double and the imaginary part should be 0)
           ! and store it in d. a sequential code here is better or
           ! in a parallel environment it might need two cores for d and e
           if( kd==0_ilp ) then
               do i = 1, n
                   d( i ) = ( ab( abdpos, i ) )
               end do
               do i = 1, n-1
                   e( i ) = zero
               end do
               hous( 1_ilp ) = 1_ilp
               work( 1_ilp ) = 1_ilp
               return
           end if
           ! case kd=1:
           ! the matrix is already tridiagonal. we have to make diagonal
           ! and offdiagonal elements real, and store them in d and e.
           ! for that, for real precision just copy the diag and offdiag
           ! to d and e while for the complex case the bulge chasing is
           ! performed to convert the hermetian tridiagonal to symmetric
           ! tridiagonal. a simpler conversion formula might be used, but then
           ! updating the q matrix will be required and based if q is generated
           ! or not this might complicate the story.
           if( kd==1_ilp ) then
               do i = 1, n
                   d( i ) = ( ab( abdpos, i ) )
               end do
               if( upper ) then
                   do i = 1, n-1
                      e( i ) = ( ab( abofdpos, i+1 ) )
                   end do
               else
                   do i = 1, n-1
                      e( i ) = ( ab( abofdpos, i ) )
                   end do
               endif
               hous( 1_ilp ) = 1_ilp
               work( 1_ilp ) = 1_ilp
               return
           end if
           ! main code start here.
           ! reduce the symmetric band of a to a tridiagonal matrix.
           thgrsiz   = n
           grsiz     = 1_ilp
           shift     = 3_ilp
           nbtiles   = ceiling( real(n,KIND=sp)/real(kd,KIND=sp) )
           stepercol = ceiling( real(shift,KIND=sp)/real(grsiz,KIND=sp) )
           thgrnb    = ceiling( real(n-1,KIND=sp)/real(thgrsiz,KIND=sp) )
           call stdlib_slacpy( "A", kd+1, n, ab, ldab, work( apos ), lda )
           call stdlib_slaset( "A", kd,   n, zero, zero, work( awpos ), lda )
           ! openmp parallelisation start here
           !$OMP PARALLEL PRIVATE( TID, THGRID, BLKLASTIND ) &
           !$OMP&         PRIVATE( THED, I, M, K, ST, ED, STT, SWEEPID ) &
           !$OMP&         PRIVATE( MYID, TTYPE, COLPT, STIND, EDIND ) &
           !$OMP&         SHARED ( UPLO, WANTQ, INDV, INDTAU, HOUS, WORK) &
           !$OMP&         SHARED ( N, KD, IB, NBTILES, LDA, LDV, INDA ) &
           !$OMP&         SHARED ( STEPERCOL, THGRNB, THGRSIZ, GRSIZ, SHIFT )
           !$OMP MASTER
           ! main bulge chasing loop
           loop_100: do thgrid = 1, thgrnb
               stt  = (thgrid-1)*thgrsiz+1
               thed = min( (stt + thgrsiz -1_ilp), (n-1))
               loop_110: do i = stt, n-1
                   ed = min( i, thed )
                   if( stt>ed ) exit
                   loop_120: do m = 1, stepercol
                       st = stt
                       loop_130: do sweepid = st, ed
                           loop_140: do k = 1, grsiz
                               myid  = (i-sweepid)*(stepercol*grsiz)+ (m-1)*grsiz + k
                               if ( myid==1_ilp ) then
                                   ttype = 1_ilp
                               else
                                   ttype = mod( myid, 2_ilp ) + 2_ilp
                               endif
                               if( ttype==2_ilp ) then
                                   colpt      = (myid/2_ilp)*kd + sweepid
                                   stind      = colpt-kd+1
                                   edind      = min(colpt,n)
                                   blklastind = colpt
                               else
                                   colpt      = ((myid+1)/2_ilp)*kd + sweepid
                                   stind      = colpt-kd+1
                                   edind      = min(colpt,n)
                                   if( ( stind>=edind-1 ).and.( edind==n ) ) then
                                       blklastind = n
                                   else
                                       blklastind = 0_ilp
                                   endif
                               endif
                               ! call the kernel
                               !$ if( ttype/=1 ) then
                                   !$OMP TASK DEPEND(in:WORK(MYID+SHIFT-1)) &
                                   !$OMP&     DEPEND(in:WORK(MYID-1)) &
                                   !$OMP&     DEPEND(out:WORK(MYID))
                                   !$ tid      = omp_get_thread_num()
                                   !$ call stdlib_ssb2st_kernels( uplo, wantq, ttype,stind, edind, &
                                   !$ sweepid, n, kd, ib,work ( inda ), lda,hous( indv ), hous( &
                                   !$ indtau ), ldv,work( indw + tid*kd ) )
                                   !$OMP END TASK
                               !$ else
                                   !$OMP TASK DEPEND(in:WORK(MYID+SHIFT-1)) &
                                   !$OMP&     DEPEND(out:WORK(MYID))
                                   !$ tid      = omp_get_thread_num()
                                   call stdlib_ssb2st_kernels( uplo, wantq, ttype,stind, edind, &
                                   sweepid, n, kd, ib,work ( inda ), lda,hous( indv ), hous( &
                                             indtau ), ldv,work( indw + tid*kd ) )
                                   !$OMP END TASK
                               !$ endif
                               if ( blklastind>=(n-1) ) then
                                   stt = stt + 1_ilp
                                   exit
                               endif
                           end do loop_140
                       end do loop_130
                   end do loop_120
               end do loop_110
           end do loop_100
           !$OMP END MASTER
           !$OMP END PARALLEL
           ! copy the diagonal from a to d. note that d is real thus only
           ! the real part is needed, the imaginary part should be zero.
           do i = 1, n
               d( i ) = ( work( dpos+(i-1)*lda ) )
           end do
           ! copy the off diagonal from a to e. note that e is real thus only
           ! the real part is needed, the imaginary part should be zero.
           if( upper ) then
               do i = 1, n-1
                  e( i ) = ( work( ofdpos+i*lda ) )
               end do
           else
               do i = 1, n-1
                  e( i ) = ( work( ofdpos+(i-1)*lda ) )
               end do
           endif
           hous( 1_ilp ) = lhmin
           work( 1_ilp ) = lwmin
           return
     end subroutine stdlib_ssytrd_sb2st

     module subroutine stdlib_dsytrd_sb2st( stage1, vect, uplo, n, kd, ab, ldab,d, e, hous, lhous, &
     !! DSYTRD_SB2ST reduces a real symmetric band matrix A to real symmetric
     !! tridiagonal form T by a orthogonal similarity transformation:
     !! Q**T * A * Q = T.
               work, lwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: stage1, uplo, vect
           integer(ilp), intent(in) :: n, kd, ldab, lhous, lwork
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           real(dp), intent(out) :: d(*), e(*)
           real(dp), intent(inout) :: ab(ldab,*)
           real(dp), intent(out) :: hous(*), work(*)
        ! =====================================================================
           ! Parameters 
           
           ! Local Scalars 
           logical(lk) :: lquery, wantq, upper, afters1
           integer(ilp) :: i, m, k, ib, sweepid, myid, shift, stt, st, ed, stind, edind, &
           blklastind, colpt, thed, stepercol, grsiz, thgrsiz, thgrnb, thgrid, nbtiles, ttype, &
           tid, nthreads, debug, abdpos, abofdpos, dpos, ofdpos, awpos, inda, indw, apos, sizea, &
                     lda, indv, indtau, sidev, sizetau, ldv, lhmin, lwmin
           ! Intrinsic Functions 
           ! Executable Statements 
           ! determine the minimal workspace size required.
           ! test the input parameters
           debug   = 0_ilp
           info    = 0_ilp
           afters1 = stdlib_lsame( stage1, 'Y' )
           wantq   = stdlib_lsame( vect, 'V' )
           upper   = stdlib_lsame( uplo, 'U' )
           lquery  = ( lwork==-1_ilp ) .or. ( lhous==-1_ilp )
           ! determine the block size, the workspace size and the hous size.
           ib     = stdlib_ilaenv2stage( 2_ilp, 'DSYTRD_SB2ST', vect, n, kd, -1_ilp, -1_ilp )
           lhmin  = stdlib_ilaenv2stage( 3_ilp, 'DSYTRD_SB2ST', vect, n, kd, ib, -1_ilp )
           lwmin  = stdlib_ilaenv2stage( 4_ilp, 'DSYTRD_SB2ST', vect, n, kd, ib, -1_ilp )
           if( .not.afters1 .and. .not.stdlib_lsame( stage1, 'N' ) ) then
              info = -1_ilp
           else if( .not.stdlib_lsame( vect, 'N' ) ) then
              info = -2_ilp
           else if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -3_ilp
           else if( n<0_ilp ) then
              info = -4_ilp
           else if( kd<0_ilp ) then
              info = -5_ilp
           else if( ldab<(kd+1) ) then
              info = -7_ilp
           else if( lhous<lhmin .and. .not.lquery ) then
              info = -11_ilp
           else if( lwork<lwmin .and. .not.lquery ) then
              info = -13_ilp
           end if
           if( info==0_ilp ) then
              hous( 1_ilp ) = lhmin
              work( 1_ilp ) = lwmin
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DSYTRD_SB2ST', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0_ilp ) then
               hous( 1_ilp ) = 1_ilp
               work( 1_ilp ) = 1_ilp
               return
           end if
           ! determine pointer position
           ldv      = kd + ib
           sizetau  = 2_ilp * n
           sidev    = 2_ilp * n
           indtau   = 1_ilp
           indv     = indtau + sizetau
           lda      = 2_ilp * kd + 1_ilp
           sizea    = lda * n
           inda     = 1_ilp
           indw     = inda + sizea
           nthreads = 1_ilp
           tid      = 0_ilp
           if( upper ) then
               apos     = inda + kd
               awpos    = inda
               dpos     = apos + kd
               ofdpos   = dpos - 1_ilp
               abdpos   = kd + 1_ilp
               abofdpos = kd
           else
               apos     = inda
               awpos    = inda + kd + 1_ilp
               dpos     = apos
               ofdpos   = dpos + 1_ilp
               abdpos   = 1_ilp
               abofdpos = 2_ilp
           endif
           ! case kd=0:
           ! the matrix is diagonal. we just copy it (convert to "real" for
           ! real because d is double and the imaginary part should be 0)
           ! and store it in d. a sequential code here is better or
           ! in a parallel environment it might need two cores for d and e
           if( kd==0_ilp ) then
               do i = 1, n
                   d( i ) = ( ab( abdpos, i ) )
               end do
               do i = 1, n-1
                   e( i ) = zero
               end do
               hous( 1_ilp ) = 1_ilp
               work( 1_ilp ) = 1_ilp
               return
           end if
           ! case kd=1:
           ! the matrix is already tridiagonal. we have to make diagonal
           ! and offdiagonal elements real, and store them in d and e.
           ! for that, for real precision just copy the diag and offdiag
           ! to d and e while for the complex case the bulge chasing is
           ! performed to convert the hermetian tridiagonal to symmetric
           ! tridiagonal. a simpler conversion formula might be used, but then
           ! updating the q matrix will be required and based if q is generated
           ! or not this might complicate the story.
           if( kd==1_ilp ) then
               do i = 1, n
                   d( i ) = ( ab( abdpos, i ) )
               end do
               if( upper ) then
                   do i = 1, n-1
                      e( i ) = ( ab( abofdpos, i+1 ) )
                   end do
               else
                   do i = 1, n-1
                      e( i ) = ( ab( abofdpos, i ) )
                   end do
               endif
               hous( 1_ilp ) = 1_ilp
               work( 1_ilp ) = 1_ilp
               return
           end if
           ! main code start here.
           ! reduce the symmetric band of a to a tridiagonal matrix.
           thgrsiz   = n
           grsiz     = 1_ilp
           shift     = 3_ilp
           nbtiles   = ceiling( real(n,KIND=dp)/real(kd,KIND=dp) )
           stepercol = ceiling( real(shift,KIND=dp)/real(grsiz,KIND=dp) )
           thgrnb    = ceiling( real(n-1,KIND=dp)/real(thgrsiz,KIND=dp) )
           call stdlib_dlacpy( "A", kd+1, n, ab, ldab, work( apos ), lda )
           call stdlib_dlaset( "A", kd,   n, zero, zero, work( awpos ), lda )
           ! openmp parallelisation start here
           !$OMP PARALLEL PRIVATE( TID, THGRID, BLKLASTIND ) &
           !$OMP&         PRIVATE( THED, I, M, K, ST, ED, STT, SWEEPID ) &
           !$OMP&         PRIVATE( MYID, TTYPE, COLPT, STIND, EDIND ) &
           !$OMP&         SHARED ( UPLO, WANTQ, INDV, INDTAU, HOUS, WORK) &
           !$OMP&         SHARED ( N, KD, IB, NBTILES, LDA, LDV, INDA ) &
           !$OMP&         SHARED ( STEPERCOL, THGRNB, THGRSIZ, GRSIZ, SHIFT )
           !$OMP MASTER
           ! main bulge chasing loop
           loop_100: do thgrid = 1, thgrnb
               stt  = (thgrid-1)*thgrsiz+1
               thed = min( (stt + thgrsiz -1_ilp), (n-1))
               loop_110: do i = stt, n-1
                   ed = min( i, thed )
                   if( stt>ed ) exit
                   loop_120: do m = 1, stepercol
                       st = stt
                       loop_130: do sweepid = st, ed
                           loop_140: do k = 1, grsiz
                               myid  = (i-sweepid)*(stepercol*grsiz)+ (m-1)*grsiz + k
                               if ( myid==1_ilp ) then
                                   ttype = 1_ilp
                               else
                                   ttype = mod( myid, 2_ilp ) + 2_ilp
                               endif
                               if( ttype==2_ilp ) then
                                   colpt      = (myid/2_ilp)*kd + sweepid
                                   stind      = colpt-kd+1
                                   edind      = min(colpt,n)
                                   blklastind = colpt
                               else
                                   colpt      = ((myid+1)/2_ilp)*kd + sweepid
                                   stind      = colpt-kd+1
                                   edind      = min(colpt,n)
                                   if( ( stind>=edind-1 ).and.( edind==n ) ) then
                                       blklastind = n
                                   else
                                       blklastind = 0_ilp
                                   endif
                               endif
                               ! call the kernel
                               !$ if( ttype/=1 ) then
                                   !$OMP TASK DEPEND(in:WORK(MYID+SHIFT-1)) &
                                   !$OMP&     DEPEND(in:WORK(MYID-1)) &
                                   !$OMP&     DEPEND(out:WORK(MYID))
                                   !$ tid      = omp_get_thread_num()
                                   !$ call stdlib_dsb2st_kernels( uplo, wantq, ttype,stind, edind, &
                                   !$ sweepid, n, kd, ib,work ( inda ), lda,hous( indv ), hous( &
                                   !$ indtau ), ldv,work( indw + tid*kd ) )
                               !$OMP END TASK
                                   !$ else
                                   !$OMP TASK DEPEND(in:WORK(MYID+SHIFT-1)) &
                                   !$OMP&     DEPEND(out:WORK(MYID))
                                   !$ tid      = omp_get_thread_num()
                                   call stdlib_dsb2st_kernels( uplo, wantq, ttype,stind, edind, &
                                   sweepid, n, kd, ib,work ( inda ), lda,hous( indv ), hous( &
                                             indtau ), ldv,work( indw + tid*kd ) )
                                   !$OMP END TASK
                               !$ endif
                               if ( blklastind>=(n-1) ) then
                                   stt = stt + 1_ilp
                                   exit
                               endif
                           end do loop_140
                       end do loop_130
                   end do loop_120
               end do loop_110
           end do loop_100
           !$OMP END MASTER
           !$OMP END PARALLEL
           ! copy the diagonal from a to d. note that d is real thus only
           ! the real part is needed, the imaginary part should be zero.
           do i = 1, n
               d( i ) = ( work( dpos+(i-1)*lda ) )
           end do
           ! copy the off diagonal from a to e. note that e is real thus only
           ! the real part is needed, the imaginary part should be zero.
           if( upper ) then
               do i = 1, n-1
                  e( i ) = ( work( ofdpos+i*lda ) )
               end do
           else
               do i = 1, n-1
                  e( i ) = ( work( ofdpos+(i-1)*lda ) )
               end do
           endif
           hous( 1_ilp ) = lhmin
           work( 1_ilp ) = lwmin
           return
     end subroutine stdlib_dsytrd_sb2st




     module subroutine stdlib_ssytrd_sy2sb( uplo, n, kd, a, lda, ab, ldab, tau,work, lwork, info )
     !! SSYTRD_SY2SB reduces a real symmetric matrix A to real symmetric
     !! band-diagonal form AB by a orthogonal similarity transformation:
     !! Q**T * A * Q = AB.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldab, lwork, n, kd
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: ab(ldab,*), tau(*), work(*)
        ! =====================================================================
           ! Parameters 
           
           ! Local Scalars 
           logical(lk) :: lquery, upper
           integer(ilp) :: i, j, iinfo, lwmin, pn, pk, llk, ldt, ldw, lds2, lds1, ls2, ls1, lw, lt,&
                      tpos, wpos, s2pos, s1pos
           ! Intrinsic Functions 
           ! Executable Statements 
           ! determine the minimal workspace size required
           ! and test the input parameters
           info   = 0_ilp
           upper  = stdlib_lsame( uplo, 'U' )
           lquery = ( lwork==-1_ilp )
           lwmin  = stdlib_ilaenv2stage( 4_ilp, 'SSYTRD_SY2SB', '', n, kd, -1_ilp, -1_ilp )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( kd<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldab<max( 1_ilp, kd+1 ) ) then
              info = -7_ilp
           else if( lwork<lwmin .and. .not.lquery ) then
              info = -10_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SSYTRD_SY2SB', -info )
              return
           else if( lquery ) then
              work( 1_ilp ) = lwmin
              return
           end if
           ! quick return if possible
           ! copy the upper/lower portion of a into ab
           if( n<=kd+1 ) then
               if( upper ) then
                   do i = 1, n
                       llk = min( kd+1, i )
                       call stdlib_scopy( llk, a( i-llk+1, i ), 1_ilp,ab( kd+1-llk+1, i ), 1_ilp )
                   end do
               else
                   do i = 1, n
                       llk = min( kd+1, n-i+1 )
                       call stdlib_scopy( llk, a( i, i ), 1_ilp, ab( 1_ilp, i ), 1_ilp )
                   end do
               endif
               work( 1_ilp ) = 1_ilp
               return
           end if
           ! determine the pointer position for the workspace
           ldt    = kd
           lds1   = kd
           lt     = ldt*kd
           lw     = n*kd
           ls1    = lds1*kd
           ls2    = lwmin - lt - lw - ls1
            ! ls2 = n*max(kd,factoptnb)
           tpos   = 1_ilp
           wpos   = tpos  + lt
           s1pos  = wpos  + lw
           s2pos  = s1pos + ls1
           if( upper ) then
               ldw    = kd
               lds2   = kd
           else
               ldw    = n
               lds2   = n
           endif
           ! set the workspace of the triangular matrix t to zero once such a
           ! way every time t is generated the upper/lower portion will be always zero
           call stdlib_slaset( "A", ldt, kd, zero, zero, work( tpos ), ldt )
           if( upper ) then
               do i = 1, n - kd, kd
                  pn = n-i-kd+1
                  pk = min( n-i-kd+1, kd )
                  ! compute the lq factorization of the current block
                  call stdlib_sgelqf( kd, pn, a( i, i+kd ), lda,tau( i ), work( s2pos ), ls2, &
                            iinfo )
                  ! copy the upper portion of a into ab
                  do j = i, i+pk-1
                     llk = min( kd, n-j ) + 1_ilp
                     call stdlib_scopy( llk, a( j, j ), lda, ab( kd+1, j ), ldab-1 )
                  end do
                  call stdlib_slaset( 'LOWER', pk, pk, zero, one,a( i, i+kd ), lda )
                  ! form the matrix t
                  call stdlib_slarft( 'FORWARD', 'ROWWISE', pn, pk,a( i, i+kd ), lda, tau( i ),&
                            work( tpos ), ldt )
                  ! compute w:
                  call stdlib_sgemm( 'CONJUGATE', 'NO TRANSPOSE', pk, pn, pk,one,  work( tpos ), &
                            ldt,a( i, i+kd ), lda,zero, work( s2pos ), lds2 )
                  call stdlib_ssymm( 'RIGHT', uplo, pk, pn,one,  a( i+kd, i+kd ), lda,work( s2pos &
                            ), lds2,zero, work( wpos ), ldw )
                  call stdlib_sgemm( 'NO TRANSPOSE', 'CONJUGATE', pk, pk, pn,one,  work( wpos ), &
                            ldw,work( s2pos ), lds2,zero, work( s1pos ), lds1 )
                  call stdlib_sgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', pk, pn, pk,-half, work( &
                            s1pos ), lds1,a( i, i+kd ), lda,one,   work( wpos ), ldw )
                  ! update the unreduced submatrix a(i+kd:n,i+kd:n), using
                  ! an update of the form:  a := a - v'*w - w'*v
                  call stdlib_ssyr2k( uplo, 'CONJUGATE', pn, pk,-one, a( i, i+kd ), lda,work( &
                            wpos ), ldw,one, a( i+kd, i+kd ), lda )
               end do
              ! copy the upper band to ab which is the band storage matrix
              do j = n-kd+1, n
                 llk = min(kd, n-j) + 1_ilp
                 call stdlib_scopy( llk, a( j, j ), lda, ab( kd+1, j ), ldab-1 )
              end do
           else
               ! reduce the lower triangle of a to lower band matrix
               loop_40: do i = 1, n - kd, kd
                  pn = n-i-kd+1
                  pk = min( n-i-kd+1, kd )
                  ! compute the qr factorization of the current block
                  call stdlib_sgeqrf( pn, kd, a( i+kd, i ), lda,tau( i ), work( s2pos ), ls2, &
                            iinfo )
                  ! copy the upper portion of a into ab
                  do j = i, i+pk-1
                     llk = min( kd, n-j ) + 1_ilp
                     call stdlib_scopy( llk, a( j, j ), 1_ilp, ab( 1_ilp, j ), 1_ilp )
                  end do
                  call stdlib_slaset( 'UPPER', pk, pk, zero, one,a( i+kd, i ), lda )
                  ! form the matrix t
                  call stdlib_slarft( 'FORWARD', 'COLUMNWISE', pn, pk,a( i+kd, i ), lda, tau( i ),&
                            work( tpos ), ldt )
                  ! compute w:
                  call stdlib_sgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', pn, pk, pk,one, a( i+kd, i ),&
                             lda,work( tpos ), ldt,zero, work( s2pos ), lds2 )
                  call stdlib_ssymm( 'LEFT', uplo, pn, pk,one, a( i+kd, i+kd ), lda,work( s2pos ),&
                             lds2,zero, work( wpos ), ldw )
                  call stdlib_sgemm( 'CONJUGATE', 'NO TRANSPOSE', pk, pk, pn,one, work( s2pos ), &
                            lds2,work( wpos ), ldw,zero, work( s1pos ), lds1 )
                  call stdlib_sgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', pn, pk, pk,-half, a( i+kd, i &
                            ), lda,work( s1pos ), lds1,one, work( wpos ), ldw )
                  ! update the unreduced submatrix a(i+kd:n,i+kd:n), using
                  ! an update of the form:  a := a - v*w' - w*v'
                  call stdlib_ssyr2k( uplo, 'NO TRANSPOSE', pn, pk,-one, a( i+kd, i ), lda,work( &
                            wpos ), ldw,one, a( i+kd, i+kd ), lda )
                  ! ==================================================================
                  ! restore a for comparison and checking to be removed
                   ! do 45 j = i, i+pk-1
                      ! llk = min( kd, n-j ) + 1
                      ! call stdlib_scopy( llk, ab( 1, j ), 1, a( j, j ), 1 )
                      45 continue
                  ! ==================================================================
               end do loop_40
              ! copy the lower band to ab which is the band storage matrix
              do j = n-kd+1, n
                 llk = min(kd, n-j) + 1_ilp
                 call stdlib_scopy( llk, a( j, j ), 1_ilp, ab( 1_ilp, j ), 1_ilp )
              end do
           end if
           work( 1_ilp ) = lwmin
           return
     end subroutine stdlib_ssytrd_sy2sb

     module subroutine stdlib_dsytrd_sy2sb( uplo, n, kd, a, lda, ab, ldab, tau,work, lwork, info )
     !! DSYTRD_SY2SB reduces a real symmetric matrix A to real symmetric
     !! band-diagonal form AB by a orthogonal similarity transformation:
     !! Q**T * A * Q = AB.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldab, lwork, n, kd
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: ab(ldab,*), tau(*), work(*)
        ! =====================================================================
           ! Parameters 
           
           ! Local Scalars 
           logical(lk) :: lquery, upper
           integer(ilp) :: i, j, iinfo, lwmin, pn, pk, llk, ldt, ldw, lds2, lds1, ls2, ls1, lw, lt,&
                      tpos, wpos, s2pos, s1pos
           ! Intrinsic Functions 
           ! Executable Statements 
           ! determine the minimal workspace size required
           ! and test the input parameters
           info   = 0_ilp
           upper  = stdlib_lsame( uplo, 'U' )
           lquery = ( lwork==-1_ilp )
           lwmin  = stdlib_ilaenv2stage( 4_ilp, 'DSYTRD_SY2SB', '', n, kd, -1_ilp, -1_ilp )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp
           else if( n<0_ilp ) then
              info = -2_ilp
           else if( kd<0_ilp ) then
              info = -3_ilp
           else if( lda<max( 1_ilp, n ) ) then
              info = -5_ilp
           else if( ldab<max( 1_ilp, kd+1 ) ) then
              info = -7_ilp
           else if( lwork<lwmin .and. .not.lquery ) then
              info = -10_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DSYTRD_SY2SB', -info )
              return
           else if( lquery ) then
              work( 1_ilp ) = lwmin
              return
           end if
           ! quick return if possible
           ! copy the upper/lower portion of a into ab
           if( n<=kd+1 ) then
               if( upper ) then
                   do i = 1, n
                       llk = min( kd+1, i )
                       call stdlib_dcopy( llk, a( i-llk+1, i ), 1_ilp,ab( kd+1-llk+1, i ), 1_ilp )
                   end do
               else
                   do i = 1, n
                       llk = min( kd+1, n-i+1 )
                       call stdlib_dcopy( llk, a( i, i ), 1_ilp, ab( 1_ilp, i ), 1_ilp )
                   end do
               endif
               work( 1_ilp ) = 1_ilp
               return
           end if
           ! determine the pointer position for the workspace
           ldt    = kd
           lds1   = kd
           lt     = ldt*kd
           lw     = n*kd
           ls1    = lds1*kd
           ls2    = lwmin - lt - lw - ls1
            ! ls2 = n*max(kd,factoptnb)
           tpos   = 1_ilp
           wpos   = tpos  + lt
           s1pos  = wpos  + lw
           s2pos  = s1pos + ls1
           if( upper ) then
               ldw    = kd
               lds2   = kd
           else
               ldw    = n
               lds2   = n
           endif
           ! set the workspace of the triangular matrix t to zero once such a
           ! way every time t is generated the upper/lower portion will be always zero
           call stdlib_dlaset( "A", ldt, kd, zero, zero, work( tpos ), ldt )
           if( upper ) then
               do i = 1, n - kd, kd
                  pn = n-i-kd+1
                  pk = min( n-i-kd+1, kd )
                  ! compute the lq factorization of the current block
                  call stdlib_dgelqf( kd, pn, a( i, i+kd ), lda,tau( i ), work( s2pos ), ls2, &
                            iinfo )
                  ! copy the upper portion of a into ab
                  do j = i, i+pk-1
                     llk = min( kd, n-j ) + 1_ilp
                     call stdlib_dcopy( llk, a( j, j ), lda, ab( kd+1, j ), ldab-1 )
                  end do
                  call stdlib_dlaset( 'LOWER', pk, pk, zero, one,a( i, i+kd ), lda )
                  ! form the matrix t
                  call stdlib_dlarft( 'FORWARD', 'ROWWISE', pn, pk,a( i, i+kd ), lda, tau( i ),&
                            work( tpos ), ldt )
                  ! compute w:
                  call stdlib_dgemm( 'CONJUGATE', 'NO TRANSPOSE', pk, pn, pk,one,  work( tpos ), &
                            ldt,a( i, i+kd ), lda,zero, work( s2pos ), lds2 )
                  call stdlib_dsymm( 'RIGHT', uplo, pk, pn,one,  a( i+kd, i+kd ), lda,work( s2pos &
                            ), lds2,zero, work( wpos ), ldw )
                  call stdlib_dgemm( 'NO TRANSPOSE', 'CONJUGATE', pk, pk, pn,one,  work( wpos ), &
                            ldw,work( s2pos ), lds2,zero, work( s1pos ), lds1 )
                  call stdlib_dgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', pk, pn, pk,-half, work( &
                            s1pos ), lds1,a( i, i+kd ), lda,one,   work( wpos ), ldw )
                  ! update the unreduced submatrix a(i+kd:n,i+kd:n), using
                  ! an update of the form:  a := a - v'*w - w'*v
                  call stdlib_dsyr2k( uplo, 'CONJUGATE', pn, pk,-one, a( i, i+kd ), lda,work( &
                            wpos ), ldw,one, a( i+kd, i+kd ), lda )
               end do
              ! copy the upper band to ab which is the band storage matrix
              do j = n-kd+1, n
                 llk = min(kd, n-j) + 1_ilp
                 call stdlib_dcopy( llk, a( j, j ), lda, ab( kd+1, j ), ldab-1 )
              end do
           else
               ! reduce the lower triangle of a to lower band matrix
               loop_40: do i = 1, n - kd, kd
                  pn = n-i-kd+1
                  pk = min( n-i-kd+1, kd )
                  ! compute the qr factorization of the current block
                  call stdlib_dgeqrf( pn, kd, a( i+kd, i ), lda,tau( i ), work( s2pos ), ls2, &
                            iinfo )
                  ! copy the upper portion of a into ab
                  do j = i, i+pk-1
                     llk = min( kd, n-j ) + 1_ilp
                     call stdlib_dcopy( llk, a( j, j ), 1_ilp, ab( 1_ilp, j ), 1_ilp )
                  end do
                  call stdlib_dlaset( 'UPPER', pk, pk, zero, one,a( i+kd, i ), lda )
                  ! form the matrix t
                  call stdlib_dlarft( 'FORWARD', 'COLUMNWISE', pn, pk,a( i+kd, i ), lda, tau( i ),&
                            work( tpos ), ldt )
                  ! compute w:
                  call stdlib_dgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', pn, pk, pk,one, a( i+kd, i ),&
                             lda,work( tpos ), ldt,zero, work( s2pos ), lds2 )
                  call stdlib_dsymm( 'LEFT', uplo, pn, pk,one, a( i+kd, i+kd ), lda,work( s2pos ),&
                             lds2,zero, work( wpos ), ldw )
                  call stdlib_dgemm( 'CONJUGATE', 'NO TRANSPOSE', pk, pk, pn,one, work( s2pos ), &
                            lds2,work( wpos ), ldw,zero, work( s1pos ), lds1 )
                  call stdlib_dgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', pn, pk, pk,-half, a( i+kd, i &
                            ), lda,work( s1pos ), lds1,one, work( wpos ), ldw )
                  ! update the unreduced submatrix a(i+kd:n,i+kd:n), using
                  ! an update of the form:  a := a - v*w' - w*v'
                  call stdlib_dsyr2k( uplo, 'NO TRANSPOSE', pn, pk,-one, a( i+kd, i ), lda,work( &
                            wpos ), ldw,one, a( i+kd, i+kd ), lda )
                  ! ==================================================================
                  ! restore a for comparison and checking to be removed
                   ! do 45 j = i, i+pk-1
                      ! llk = min( kd, n-j ) + 1
                      ! call stdlib_dcopy( llk, ab( 1, j ), 1, a( j, j ), 1 )
                      45 continue
                  ! ==================================================================
               end do loop_40
              ! copy the lower band to ab which is the band storage matrix
              do j = n-kd+1, n
                 llk = min(kd, n-j) + 1_ilp
                 call stdlib_dcopy( llk, a( j, j ), 1_ilp, ab( 1_ilp, j ), 1_ilp )
              end do
           end if
           work( 1_ilp ) = lwmin
           return
     end subroutine stdlib_dsytrd_sy2sb




     module subroutine stdlib_I64_ssygv( itype, jobz, uplo, n, a, lda, b, ldb, w, work,lwork, info )
     !! SSYGV computes all the eigenvalues, and optionally, the eigenvectors
     !! of a real generalized symmetric-definite eigenproblem, of the form
     !! A*x=(lambda)*B*x,  A*Bx=(lambda)*x,  or B*A*x=(lambda)*x.
     !! Here A and B are assumed to be symmetric and B is also
     !! positive definite.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: itype, lda, ldb, lwork, n
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*), b(ldb,*)
           real(sp), intent(out) :: w(*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery, upper, wantz
           character :: trans
           integer(ilp64) :: lwkmin, lwkopt, nb, neig
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           upper = stdlib_lsame( uplo, 'U' )
           lquery = ( lwork==-1_ilp64 )
           info = 0_ilp64
           if( itype<1_ilp64 .or. itype>3_ilp64 ) then
              info = -1_ilp64
           else if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -2_ilp64
           else if( .not.( upper .or. stdlib_lsame( uplo, 'L' ) ) ) then
              info = -3_ilp64
           else if( n<0_ilp64 ) then
              info = -4_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -6_ilp64
           else if( ldb<max( 1_ilp64, n ) ) then
              info = -8_ilp64
           end if
           if( info==0_ilp64 ) then
              lwkmin = max( 1_ilp64, 3_ilp64*n - 1_ilp64 )
              nb = stdlib_I64_ilaenv( 1_ilp64, 'SSYTRD', uplo, n, -1_ilp64, -1_ilp64, -1_ilp64 )
              lwkopt = max( lwkmin, ( nb + 2_ilp64 )*n )
              work( 1_ilp64 ) = lwkopt
              if( lwork<lwkmin .and. .not.lquery ) then
                 info = -11_ilp64
              end if
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'SSYGV ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! form a cholesky factorization of b.
           call stdlib_I64_spotrf( uplo, n, b, ldb, info )
           if( info/=0_ilp64 ) then
              info = n + info
              return
           end if
           ! transform problem to standard eigenvalue problem and solve.
           call stdlib_I64_ssygst( itype, uplo, n, a, lda, b, ldb, info )
           call stdlib_I64_ssyev( jobz, uplo, n, a, lda, w, work, lwork, info )
           if( wantz ) then
              ! backtransform eigenvectors to the original problem.
              neig = n
              if( info>0_ilp64 )neig = info - 1_ilp64
              if( itype==1_ilp64 .or. itype==2_ilp64 ) then
                 ! for a*x=(lambda)*b*x and a*b*x=(lambda)*x;
                 ! backtransform eigenvectors: x = inv(l)**t*y or inv(u)*y
                 if( upper ) then
                    trans = 'N'
                 else
                    trans = 'T'
                 end if
                 call stdlib_I64_strsm( 'LEFT', uplo, trans, 'NON-UNIT', n, neig, one,b, ldb, a, lda )
                           
              else if( itype==3_ilp64 ) then
                 ! for b*a*x=(lambda)*x;
                 ! backtransform eigenvectors: x = l*y or u**t*y
                 if( upper ) then
                    trans = 'T'
                 else
                    trans = 'N'
                 end if
                 call stdlib_I64_strmm( 'LEFT', uplo, trans, 'NON-UNIT', n, neig, one,b, ldb, a, lda )
                           
              end if
           end if
           work( 1_ilp64 ) = lwkopt
           return
     end subroutine stdlib_I64_ssygv

     module subroutine stdlib_I64_dsygv( itype, jobz, uplo, n, a, lda, b, ldb, w, work,lwork, info )
     !! DSYGV computes all the eigenvalues, and optionally, the eigenvectors
     !! of a real generalized symmetric-definite eigenproblem, of the form
     !! A*x=(lambda)*B*x,  A*Bx=(lambda)*x,  or B*A*x=(lambda)*x.
     !! Here A and B are assumed to be symmetric and B is also
     !! positive definite.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: itype, lda, ldb, lwork, n
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*), b(ldb,*)
           real(dp), intent(out) :: w(*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery, upper, wantz
           character :: trans
           integer(ilp64) :: lwkmin, lwkopt, nb, neig
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           upper = stdlib_lsame( uplo, 'U' )
           lquery = ( lwork==-1_ilp64 )
           info = 0_ilp64
           if( itype<1_ilp64 .or. itype>3_ilp64 ) then
              info = -1_ilp64
           else if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -2_ilp64
           else if( .not.( upper .or. stdlib_lsame( uplo, 'L' ) ) ) then
              info = -3_ilp64
           else if( n<0_ilp64 ) then
              info = -4_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -6_ilp64
           else if( ldb<max( 1_ilp64, n ) ) then
              info = -8_ilp64
           end if
           if( info==0_ilp64 ) then
              lwkmin = max( 1_ilp64, 3_ilp64*n - 1_ilp64 )
              nb = stdlib_I64_ilaenv( 1_ilp64, 'DSYTRD', uplo, n, -1_ilp64, -1_ilp64, -1_ilp64 )
              lwkopt = max( lwkmin, ( nb + 2_ilp64 )*n )
              work( 1_ilp64 ) = lwkopt
              if( lwork<lwkmin .and. .not.lquery ) then
                 info = -11_ilp64
              end if
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'DSYGV ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! form a cholesky factorization of b.
           call stdlib_I64_dpotrf( uplo, n, b, ldb, info )
           if( info/=0_ilp64 ) then
              info = n + info
              return
           end if
           ! transform problem to standard eigenvalue problem and solve.
           call stdlib_I64_dsygst( itype, uplo, n, a, lda, b, ldb, info )
           call stdlib_I64_dsyev( jobz, uplo, n, a, lda, w, work, lwork, info )
           if( wantz ) then
              ! backtransform eigenvectors to the original problem.
              neig = n
              if( info>0_ilp64 )neig = info - 1_ilp64
              if( itype==1_ilp64 .or. itype==2_ilp64 ) then
                 ! for a*x=(lambda)*b*x and a*b*x=(lambda)*x;
                 ! backtransform eigenvectors: x = inv(l)**t*y or inv(u)*y
                 if( upper ) then
                    trans = 'N'
                 else
                    trans = 'T'
                 end if
                 call stdlib_I64_dtrsm( 'LEFT', uplo, trans, 'NON-UNIT', n, neig, one,b, ldb, a, lda )
                           
              else if( itype==3_ilp64 ) then
                 ! for b*a*x=(lambda)*x;
                 ! backtransform eigenvectors: x = l*y or u**t*y
                 if( upper ) then
                    trans = 'T'
                 else
                    trans = 'N'
                 end if
                 call stdlib_I64_dtrmm( 'LEFT', uplo, trans, 'NON-UNIT', n, neig, one,b, ldb, a, lda )
                           
              end if
           end if
           work( 1_ilp64 ) = lwkopt
           return
     end subroutine stdlib_I64_dsygv




     module subroutine stdlib_I64_ssygvd( itype, jobz, uplo, n, a, lda, b, ldb, w, work,lwork, iwork, liwork,&
     !! SSYGVD computes all the eigenvalues, and optionally, the eigenvectors
     !! of a real generalized symmetric-definite eigenproblem, of the form
     !! A*x=(lambda)*B*x,  A*Bx=(lambda)*x,  or B*A*x=(lambda)*x.  Here A and
     !! B are assumed to be symmetric and B is also positive definite.
     !! If eigenvectors are desired, it uses a divide and conquer algorithm.
     !! The divide and conquer algorithm makes very mild assumptions about
     !! floating point arithmetic. It will work on machines with a guard
     !! digit in add/subtract, or on those binary machines without guard
     !! digits which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or
     !! Cray-2. It could conceivably fail on hexadecimal or decimal machines
     !! without guard digits, but we know of none.
                info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: itype, lda, ldb, liwork, lwork, n
           ! Array Arguments 
           integer(ilp64), intent(out) :: iwork(*)
           real(sp), intent(inout) :: a(lda,*), b(ldb,*)
           real(sp), intent(out) :: w(*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery, upper, wantz
           character :: trans
           integer(ilp64) :: liopt, liwmin, lopt, lwmin
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           upper = stdlib_lsame( uplo, 'U' )
           lquery = ( lwork==-1_ilp64 .or. liwork==-1_ilp64 )
           info = 0_ilp64
           if( n<=1_ilp64 ) then
              liwmin = 1_ilp64
              lwmin = 1_ilp64
           else if( wantz ) then
              liwmin = 3_ilp64 + 5_ilp64*n
              lwmin = 1_ilp64 + 6_ilp64*n + 2_ilp64*n**2_ilp64
           else
              liwmin = 1_ilp64
              lwmin = 2_ilp64*n + 1_ilp64
           end if
           lopt = lwmin
           liopt = liwmin
           if( itype<1_ilp64 .or. itype>3_ilp64 ) then
              info = -1_ilp64
           else if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -2_ilp64
           else if( .not.( upper .or. stdlib_lsame( uplo, 'L' ) ) ) then
              info = -3_ilp64
           else if( n<0_ilp64 ) then
              info = -4_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -6_ilp64
           else if( ldb<max( 1_ilp64, n ) ) then
              info = -8_ilp64
           end if
           if( info==0_ilp64 ) then
              work( 1_ilp64 ) = lopt
              iwork( 1_ilp64 ) = liopt
              if( lwork<lwmin .and. .not.lquery ) then
                 info = -11_ilp64
              else if( liwork<liwmin .and. .not.lquery ) then
                 info = -13_ilp64
              end if
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'SSYGVD', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! form a cholesky factorization of b.
           call stdlib_I64_spotrf( uplo, n, b, ldb, info )
           if( info/=0_ilp64 ) then
              info = n + info
              return
           end if
           ! transform problem to standard eigenvalue problem and solve.
           call stdlib_I64_ssygst( itype, uplo, n, a, lda, b, ldb, info )
           call stdlib_I64_ssyevd( jobz, uplo, n, a, lda, w, work, lwork, iwork, liwork,info )
           lopt = max( real( lopt,KIND=sp), real( work( 1_ilp64 ),KIND=sp) )
           liopt = max( real( liopt,KIND=sp), real( iwork( 1_ilp64 ),KIND=sp) )
           if( wantz .and. info==0_ilp64 ) then
              ! backtransform eigenvectors to the original problem.
              if( itype==1_ilp64 .or. itype==2_ilp64 ) then
                 ! for a*x=(lambda)*b*x and a*b*x=(lambda)*x;
                 ! backtransform eigenvectors: x = inv(l)**t*y or inv(u)*y
                 if( upper ) then
                    trans = 'N'
                 else
                    trans = 'T'
                 end if
                 call stdlib_I64_strsm( 'LEFT', uplo, trans, 'NON-UNIT', n, n, one,b, ldb, a, lda )
                           
              else if( itype==3_ilp64 ) then
                 ! for b*a*x=(lambda)*x;
                 ! backtransform eigenvectors: x = l*y or u**t*y
                 if( upper ) then
                    trans = 'T'
                 else
                    trans = 'N'
                 end if
                 call stdlib_I64_strmm( 'LEFT', uplo, trans, 'NON-UNIT', n, n, one,b, ldb, a, lda )
                           
              end if
           end if
           work( 1_ilp64 ) = lopt
           iwork( 1_ilp64 ) = liopt
           return
     end subroutine stdlib_I64_ssygvd

     module subroutine stdlib_I64_dsygvd( itype, jobz, uplo, n, a, lda, b, ldb, w, work,lwork, iwork, liwork,&
     !! DSYGVD computes all the eigenvalues, and optionally, the eigenvectors
     !! of a real generalized symmetric-definite eigenproblem, of the form
     !! A*x=(lambda)*B*x,  A*Bx=(lambda)*x,  or B*A*x=(lambda)*x.  Here A and
     !! B are assumed to be symmetric and B is also positive definite.
     !! If eigenvectors are desired, it uses a divide and conquer algorithm.
     !! The divide and conquer algorithm makes very mild assumptions about
     !! floating point arithmetic. It will work on machines with a guard
     !! digit in add/subtract, or on those binary machines without guard
     !! digits which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or
     !! Cray-2. It could conceivably fail on hexadecimal or decimal machines
     !! without guard digits, but we know of none.
                info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: itype, lda, ldb, liwork, lwork, n
           ! Array Arguments 
           integer(ilp64), intent(out) :: iwork(*)
           real(dp), intent(inout) :: a(lda,*), b(ldb,*)
           real(dp), intent(out) :: w(*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery, upper, wantz
           character :: trans
           integer(ilp64) :: liopt, liwmin, lopt, lwmin
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           upper = stdlib_lsame( uplo, 'U' )
           lquery = ( lwork==-1_ilp64 .or. liwork==-1_ilp64 )
           info = 0_ilp64
           if( n<=1_ilp64 ) then
              liwmin = 1_ilp64
              lwmin = 1_ilp64
           else if( wantz ) then
              liwmin = 3_ilp64 + 5_ilp64*n
              lwmin = 1_ilp64 + 6_ilp64*n + 2_ilp64*n**2_ilp64
           else
              liwmin = 1_ilp64
              lwmin = 2_ilp64*n + 1_ilp64
           end if
           lopt = lwmin
           liopt = liwmin
           if( itype<1_ilp64 .or. itype>3_ilp64 ) then
              info = -1_ilp64
           else if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -2_ilp64
           else if( .not.( upper .or. stdlib_lsame( uplo, 'L' ) ) ) then
              info = -3_ilp64
           else if( n<0_ilp64 ) then
              info = -4_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -6_ilp64
           else if( ldb<max( 1_ilp64, n ) ) then
              info = -8_ilp64
           end if
           if( info==0_ilp64 ) then
              work( 1_ilp64 ) = lopt
              iwork( 1_ilp64 ) = liopt
              if( lwork<lwmin .and. .not.lquery ) then
                 info = -11_ilp64
              else if( liwork<liwmin .and. .not.lquery ) then
                 info = -13_ilp64
              end if
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'DSYGVD', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! form a cholesky factorization of b.
           call stdlib_I64_dpotrf( uplo, n, b, ldb, info )
           if( info/=0_ilp64 ) then
              info = n + info
              return
           end if
           ! transform problem to standard eigenvalue problem and solve.
           call stdlib_I64_dsygst( itype, uplo, n, a, lda, b, ldb, info )
           call stdlib_I64_dsyevd( jobz, uplo, n, a, lda, w, work, lwork, iwork, liwork,info )
           lopt = max( real( lopt,KIND=dp), real( work( 1_ilp64 ),KIND=dp) )
           liopt = max( real( liopt,KIND=dp), real( iwork( 1_ilp64 ),KIND=dp) )
           if( wantz .and. info==0_ilp64 ) then
              ! backtransform eigenvectors to the original problem.
              if( itype==1_ilp64 .or. itype==2_ilp64 ) then
                 ! for a*x=(lambda)*b*x and a*b*x=(lambda)*x;
                 ! backtransform eigenvectors: x = inv(l)**t*y or inv(u)*y
                 if( upper ) then
                    trans = 'N'
                 else
                    trans = 'T'
                 end if
                 call stdlib_I64_dtrsm( 'LEFT', uplo, trans, 'NON-UNIT', n, n, one,b, ldb, a, lda )
                           
              else if( itype==3_ilp64 ) then
                 ! for b*a*x=(lambda)*x;
                 ! backtransform eigenvectors: x = l*y or u**t*y
                 if( upper ) then
                    trans = 'T'
                 else
                    trans = 'N'
                 end if
                 call stdlib_I64_dtrmm( 'LEFT', uplo, trans, 'NON-UNIT', n, n, one,b, ldb, a, lda )
                           
              end if
           end if
           work( 1_ilp64 ) = lopt
           iwork( 1_ilp64 ) = liopt
           return
     end subroutine stdlib_I64_dsygvd




     module subroutine stdlib_I64_ssygvx( itype, jobz, range, uplo, n, a, lda, b, ldb,vl, vu, il, iu, abstol,&
     !! SSYGVX computes selected eigenvalues, and optionally, eigenvectors
     !! of a real generalized symmetric-definite eigenproblem, of the form
     !! A*x=(lambda)*B*x,  A*Bx=(lambda)*x,  or B*A*x=(lambda)*x.  Here A
     !! and B are assumed to be symmetric and B is also positive definite.
     !! Eigenvalues and eigenvectors can be selected by specifying either a
     !! range of values or a range of indices for the desired eigenvalues.
                m, w, z, ldz, work,lwork, iwork, ifail, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, range, uplo
           integer(ilp64), intent(in) :: il, itype, iu, lda, ldb, ldz, lwork, n
           integer(ilp64), intent(out) :: info, m
           real(sp), intent(in) :: abstol, vl, vu
           ! Array Arguments 
           integer(ilp64), intent(out) :: ifail(*), iwork(*)
           real(sp), intent(inout) :: a(lda,*), b(ldb,*)
           real(sp), intent(out) :: w(*), work(*), z(ldz,*)
       ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: alleig, indeig, lquery, upper, valeig, wantz
           character :: trans
           integer(ilp64) :: lwkmin, lwkopt, nb
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           upper = stdlib_lsame( uplo, 'U' )
           wantz = stdlib_lsame( jobz, 'V' )
           alleig = stdlib_lsame( range, 'A' )
           valeig = stdlib_lsame( range, 'V' )
           indeig = stdlib_lsame( range, 'I' )
           lquery = ( lwork==-1_ilp64 )
           info = 0_ilp64
           if( itype<1_ilp64 .or. itype>3_ilp64 ) then
              info = -1_ilp64
           else if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -2_ilp64
           else if( .not.( alleig .or. valeig .or. indeig ) ) then
              info = -3_ilp64
           else if( .not.( upper .or. stdlib_lsame( uplo, 'L' ) ) ) then
              info = -4_ilp64
           else if( n<0_ilp64 ) then
              info = -5_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -7_ilp64
           else if( ldb<max( 1_ilp64, n ) ) then
              info = -9_ilp64
           else
              if( valeig ) then
                 if( n>0_ilp64 .and. vu<=vl )info = -11_ilp64
              else if( indeig ) then
                 if( il<1_ilp64 .or. il>max( 1_ilp64, n ) ) then
                    info = -12_ilp64
                 else if( iu<min( n, il ) .or. iu>n ) then
                    info = -13_ilp64
                 end if
              end if
           end if
           if (info==0_ilp64) then
              if (ldz<1_ilp64 .or. (wantz .and. ldz<n)) then
                 info = -18_ilp64
              end if
           end if
           if( info==0_ilp64 ) then
              lwkmin = max( 1_ilp64, 8_ilp64*n )
              nb = stdlib_I64_ilaenv( 1_ilp64, 'SSYTRD', uplo, n, -1_ilp64, -1_ilp64, -1_ilp64 )
              lwkopt = max( lwkmin, ( nb + 3_ilp64 )*n )
              work( 1_ilp64 ) = lwkopt
              if( lwork<lwkmin .and. .not.lquery ) then
                 info = -20_ilp64
              end if
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'SSYGVX', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           m = 0_ilp64
           if( n==0_ilp64 ) then
              return
           end if
           ! form a cholesky factorization of b.
           call stdlib_I64_spotrf( uplo, n, b, ldb, info )
           if( info/=0_ilp64 ) then
              info = n + info
              return
           end if
           ! transform problem to standard eigenvalue problem and solve.
           call stdlib_I64_ssygst( itype, uplo, n, a, lda, b, ldb, info )
           call stdlib_I64_ssyevx( jobz, range, uplo, n, a, lda, vl, vu, il, iu, abstol,m, w, z, ldz, &
                     work, lwork, iwork, ifail, info )
           if( wantz ) then
              ! backtransform eigenvectors to the original problem.
              if( info>0_ilp64 )m = info - 1_ilp64
              if( itype==1_ilp64 .or. itype==2_ilp64 ) then
                 ! for a*x=(lambda)*b*x and a*b*x=(lambda)*x;
                 ! backtransform eigenvectors: x = inv(l)**t*y or inv(u)*y
                 if( upper ) then
                    trans = 'N'
                 else
                    trans = 'T'
                 end if
                 call stdlib_I64_strsm( 'LEFT', uplo, trans, 'NON-UNIT', n, m, one, b,ldb, z, ldz )
                           
              else if( itype==3_ilp64 ) then
                 ! for b*a*x=(lambda)*x;
                 ! backtransform eigenvectors: x = l*y or u**t*y
                 if( upper ) then
                    trans = 'T'
                 else
                    trans = 'N'
                 end if
                 call stdlib_I64_strmm( 'LEFT', uplo, trans, 'NON-UNIT', n, m, one, b,ldb, z, ldz )
                           
              end if
           end if
           ! set work(1) to optimal workspace size.
           work( 1_ilp64 ) = lwkopt
           return
     end subroutine stdlib_I64_ssygvx

     module subroutine stdlib_I64_dsygvx( itype, jobz, range, uplo, n, a, lda, b, ldb,vl, vu, il, iu, abstol,&
     !! DSYGVX computes selected eigenvalues, and optionally, eigenvectors
     !! of a real generalized symmetric-definite eigenproblem, of the form
     !! A*x=(lambda)*B*x,  A*Bx=(lambda)*x,  or B*A*x=(lambda)*x.  Here A
     !! and B are assumed to be symmetric and B is also positive definite.
     !! Eigenvalues and eigenvectors can be selected by specifying either a
     !! range of values or a range of indices for the desired eigenvalues.
                m, w, z, ldz, work,lwork, iwork, ifail, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, range, uplo
           integer(ilp64), intent(in) :: il, itype, iu, lda, ldb, ldz, lwork, n
           integer(ilp64), intent(out) :: info, m
           real(dp), intent(in) :: abstol, vl, vu
           ! Array Arguments 
           integer(ilp64), intent(out) :: ifail(*), iwork(*)
           real(dp), intent(inout) :: a(lda,*), b(ldb,*)
           real(dp), intent(out) :: w(*), work(*), z(ldz,*)
       ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: alleig, indeig, lquery, upper, valeig, wantz
           character :: trans
           integer(ilp64) :: lwkmin, lwkopt, nb
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           upper = stdlib_lsame( uplo, 'U' )
           wantz = stdlib_lsame( jobz, 'V' )
           alleig = stdlib_lsame( range, 'A' )
           valeig = stdlib_lsame( range, 'V' )
           indeig = stdlib_lsame( range, 'I' )
           lquery = ( lwork==-1_ilp64 )
           info = 0_ilp64
           if( itype<1_ilp64 .or. itype>3_ilp64 ) then
              info = -1_ilp64
           else if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -2_ilp64
           else if( .not.( alleig .or. valeig .or. indeig ) ) then
              info = -3_ilp64
           else if( .not.( upper .or. stdlib_lsame( uplo, 'L' ) ) ) then
              info = -4_ilp64
           else if( n<0_ilp64 ) then
              info = -5_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -7_ilp64
           else if( ldb<max( 1_ilp64, n ) ) then
              info = -9_ilp64
           else
              if( valeig ) then
                 if( n>0_ilp64 .and. vu<=vl )info = -11_ilp64
              else if( indeig ) then
                 if( il<1_ilp64 .or. il>max( 1_ilp64, n ) ) then
                    info = -12_ilp64
                 else if( iu<min( n, il ) .or. iu>n ) then
                    info = -13_ilp64
                 end if
              end if
           end if
           if (info==0_ilp64) then
              if (ldz<1_ilp64 .or. (wantz .and. ldz<n)) then
                 info = -18_ilp64
              end if
           end if
           if( info==0_ilp64 ) then
              lwkmin = max( 1_ilp64, 8_ilp64*n )
              nb = stdlib_I64_ilaenv( 1_ilp64, 'DSYTRD', uplo, n, -1_ilp64, -1_ilp64, -1_ilp64 )
              lwkopt = max( lwkmin, ( nb + 3_ilp64 )*n )
              work( 1_ilp64 ) = lwkopt
              if( lwork<lwkmin .and. .not.lquery ) then
                 info = -20_ilp64
              end if
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'DSYGVX', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           m = 0_ilp64
           if( n==0_ilp64 ) then
              return
           end if
           ! form a cholesky factorization of b.
           call stdlib_I64_dpotrf( uplo, n, b, ldb, info )
           if( info/=0_ilp64 ) then
              info = n + info
              return
           end if
           ! transform problem to standard eigenvalue problem and solve.
           call stdlib_I64_dsygst( itype, uplo, n, a, lda, b, ldb, info )
           call stdlib_I64_dsyevx( jobz, range, uplo, n, a, lda, vl, vu, il, iu, abstol,m, w, z, ldz, &
                     work, lwork, iwork, ifail, info )
           if( wantz ) then
              ! backtransform eigenvectors to the original problem.
              if( info>0_ilp64 )m = info - 1_ilp64
              if( itype==1_ilp64 .or. itype==2_ilp64 ) then
                 ! for a*x=(lambda)*b*x and a*b*x=(lambda)*x;
                 ! backtransform eigenvectors: x = inv(l)**t*y or inv(u)*y
                 if( upper ) then
                    trans = 'N'
                 else
                    trans = 'T'
                 end if
                 call stdlib_I64_dtrsm( 'LEFT', uplo, trans, 'NON-UNIT', n, m, one, b,ldb, z, ldz )
                           
              else if( itype==3_ilp64 ) then
                 ! for b*a*x=(lambda)*x;
                 ! backtransform eigenvectors: x = l*y or u**t*y
                 if( upper ) then
                    trans = 'T'
                 else
                    trans = 'N'
                 end if
                 call stdlib_I64_dtrmm( 'LEFT', uplo, trans, 'NON-UNIT', n, m, one, b,ldb, z, ldz )
                           
              end if
           end if
           ! set work(1) to optimal workspace size.
           work( 1_ilp64 ) = lwkopt
           return
     end subroutine stdlib_I64_dsygvx




     module subroutine stdlib_I64_sspgv( itype, jobz, uplo, n, ap, bp, w, z, ldz, work,info )
     !! SSPGV computes all the eigenvalues and, optionally, the eigenvectors
     !! of a real generalized symmetric-definite eigenproblem, of the form
     !! A*x=(lambda)*B*x,  A*Bx=(lambda)*x,  or B*A*x=(lambda)*x.
     !! Here A and B are assumed to be symmetric, stored in packed format,
     !! and B is also positive definite.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: itype, ldz, n
           ! Array Arguments 
           real(sp), intent(inout) :: ap(*), bp(*)
           real(sp), intent(out) :: w(*), work(*), z(ldz,*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: upper, wantz
           character :: trans
           integer(ilp64) :: j, neig
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           upper = stdlib_lsame( uplo, 'U' )
           info = 0_ilp64
           if( itype<1_ilp64 .or. itype>3_ilp64 ) then
              info = -1_ilp64
           else if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -2_ilp64
           else if( .not.( upper .or. stdlib_lsame( uplo, 'L' ) ) ) then
              info = -3_ilp64
           else if( n<0_ilp64 ) then
              info = -4_ilp64
           else if( ldz<1_ilp64 .or. ( wantz .and. ldz<n ) ) then
              info = -9_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'SSPGV ', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! form a cholesky factorization of b.
           call stdlib_I64_spptrf( uplo, n, bp, info )
           if( info/=0_ilp64 ) then
              info = n + info
              return
           end if
           ! transform problem to standard eigenvalue problem and solve.
           call stdlib_I64_sspgst( itype, uplo, n, ap, bp, info )
           call stdlib_I64_sspev( jobz, uplo, n, ap, w, z, ldz, work, info )
           if( wantz ) then
              ! backtransform eigenvectors to the original problem.
              neig = n
              if( info>0_ilp64 )neig = info - 1_ilp64
              if( itype==1_ilp64 .or. itype==2_ilp64 ) then
                 ! for a*x=(lambda)*b*x and a*b*x=(lambda)*x;
                 ! backtransform eigenvectors: x = inv(l)**t*y or inv(u)*y
                 if( upper ) then
                    trans = 'N'
                 else
                    trans = 'T'
                 end if
                 do j = 1, neig
                    call stdlib_I64_stpsv( uplo, trans, 'NON-UNIT', n, bp, z( 1_ilp64, j ),1_ilp64 )
                 end do
              else if( itype==3_ilp64 ) then
                 ! for b*a*x=(lambda)*x;
                 ! backtransform eigenvectors: x = l*y or u**t*y
                 if( upper ) then
                    trans = 'T'
                 else
                    trans = 'N'
                 end if
                 do j = 1, neig
                    call stdlib_I64_stpmv( uplo, trans, 'NON-UNIT', n, bp, z( 1_ilp64, j ),1_ilp64 )
                 end do
              end if
           end if
           return
     end subroutine stdlib_I64_sspgv

     module subroutine stdlib_I64_dspgv( itype, jobz, uplo, n, ap, bp, w, z, ldz, work,info )
     !! DSPGV computes all the eigenvalues and, optionally, the eigenvectors
     !! of a real generalized symmetric-definite eigenproblem, of the form
     !! A*x=(lambda)*B*x,  A*Bx=(lambda)*x,  or B*A*x=(lambda)*x.
     !! Here A and B are assumed to be symmetric, stored in packed format,
     !! and B is also positive definite.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: itype, ldz, n
           ! Array Arguments 
           real(dp), intent(inout) :: ap(*), bp(*)
           real(dp), intent(out) :: w(*), work(*), z(ldz,*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: upper, wantz
           character :: trans
           integer(ilp64) :: j, neig
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           upper = stdlib_lsame( uplo, 'U' )
           info = 0_ilp64
           if( itype<1_ilp64 .or. itype>3_ilp64 ) then
              info = -1_ilp64
           else if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -2_ilp64
           else if( .not.( upper .or. stdlib_lsame( uplo, 'L' ) ) ) then
              info = -3_ilp64
           else if( n<0_ilp64 ) then
              info = -4_ilp64
           else if( ldz<1_ilp64 .or. ( wantz .and. ldz<n ) ) then
              info = -9_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'DSPGV ', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! form a cholesky factorization of b.
           call stdlib_I64_dpptrf( uplo, n, bp, info )
           if( info/=0_ilp64 ) then
              info = n + info
              return
           end if
           ! transform problem to standard eigenvalue problem and solve.
           call stdlib_I64_dspgst( itype, uplo, n, ap, bp, info )
           call stdlib_I64_dspev( jobz, uplo, n, ap, w, z, ldz, work, info )
           if( wantz ) then
              ! backtransform eigenvectors to the original problem.
              neig = n
              if( info>0_ilp64 )neig = info - 1_ilp64
              if( itype==1_ilp64 .or. itype==2_ilp64 ) then
                 ! for a*x=(lambda)*b*x and a*b*x=(lambda)*x;
                 ! backtransform eigenvectors: x = inv(l)**t*y or inv(u)*y
                 if( upper ) then
                    trans = 'N'
                 else
                    trans = 'T'
                 end if
                 do j = 1, neig
                    call stdlib_I64_dtpsv( uplo, trans, 'NON-UNIT', n, bp, z( 1_ilp64, j ),1_ilp64 )
                 end do
              else if( itype==3_ilp64 ) then
                 ! for b*a*x=(lambda)*x;
                 ! backtransform eigenvectors: x = l*y or u**t*y
                 if( upper ) then
                    trans = 'T'
                 else
                    trans = 'N'
                 end if
                 do j = 1, neig
                    call stdlib_I64_dtpmv( uplo, trans, 'NON-UNIT', n, bp, z( 1_ilp64, j ),1_ilp64 )
                 end do
              end if
           end if
           return
     end subroutine stdlib_I64_dspgv




     module subroutine stdlib_I64_sspgvd( itype, jobz, uplo, n, ap, bp, w, z, ldz, work,lwork, iwork, liwork,&
     !! SSPGVD computes all the eigenvalues, and optionally, the eigenvectors
     !! of a real generalized symmetric-definite eigenproblem, of the form
     !! A*x=(lambda)*B*x,  A*Bx=(lambda)*x,  or B*A*x=(lambda)*x.  Here A and
     !! B are assumed to be symmetric, stored in packed format, and B is also
     !! positive definite.
     !! If eigenvectors are desired, it uses a divide and conquer algorithm.
     !! The divide and conquer algorithm makes very mild assumptions about
     !! floating point arithmetic. It will work on machines with a guard
     !! digit in add/subtract, or on those binary machines without guard
     !! digits which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or
     !! Cray-2. It could conceivably fail on hexadecimal or decimal machines
     !! without guard digits, but we know of none.
                info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: itype, ldz, liwork, lwork, n
           ! Array Arguments 
           integer(ilp64), intent(out) :: iwork(*)
           real(sp), intent(inout) :: ap(*), bp(*)
           real(sp), intent(out) :: w(*), work(*), z(ldz,*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lquery, upper, wantz
           character :: trans
           integer(ilp64) :: j, liwmin, lwmin, neig
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           upper = stdlib_lsame( uplo, 'U' )
           lquery = ( lwork==-1_ilp64 .or. liwork==-1_ilp64 )
           info = 0_ilp64
           if( itype<1_ilp64 .or. itype>3_ilp64 ) then
              info = -1_ilp64
           else if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -2_ilp64
           else if( .not.( upper .or. stdlib_lsame( uplo, 'L' ) ) ) then
              info = -3_ilp64
           else if( n<0_ilp64 ) then
              info = -4_ilp64
           else if( ldz<1_ilp64 .or. ( wantz .and. ldz<n ) ) then
              info = -9_ilp64
           end if
           if( info==0_ilp64 ) then
              if( n<=1_ilp64 ) then
                 liwmin = 1_ilp64
                 lwmin = 1_ilp64
              else
                 if( wantz ) then
                    liwmin = 3_ilp64 + 5_ilp64*n
                    lwmin = 1_ilp64 + 6_ilp64*n + 2_ilp64*n**2_ilp64
                 else
                    liwmin = 1_ilp64
                    lwmin = 2_ilp64*n
                 end if
              end if
              work( 1_ilp64 ) = lwmin
              iwork( 1_ilp64 ) = liwmin
              if( lwork<lwmin .and. .not.lquery ) then
                 info = -11_ilp64
              else if( liwork<liwmin .and. .not.lquery ) then
                 info = -13_ilp64
              end if
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'SSPGVD', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! form a cholesky factorization of bp.
           call stdlib_I64_spptrf( uplo, n, bp, info )
           if( info/=0_ilp64 ) then
              info = n + info
              return
           end if
           ! transform problem to standard eigenvalue problem and solve.
           call stdlib_I64_sspgst( itype, uplo, n, ap, bp, info )
           call stdlib_I64_sspevd( jobz, uplo, n, ap, w, z, ldz, work, lwork, iwork,liwork, info )
                     
           lwmin = max( real( lwmin,KIND=sp), real( work( 1_ilp64 ),KIND=sp) )
           liwmin = max( real( liwmin,KIND=sp), real( iwork( 1_ilp64 ),KIND=sp) )
           if( wantz ) then
              ! backtransform eigenvectors to the original problem.
              neig = n
              if( info>0_ilp64 )neig = info - 1_ilp64
              if( itype==1_ilp64 .or. itype==2_ilp64 ) then
                 ! for a*x=(lambda)*b*x and a*b*x=(lambda)*x;
                 ! backtransform eigenvectors: x = inv(l)**t *y or inv(u)*y
                 if( upper ) then
                    trans = 'N'
                 else
                    trans = 'T'
                 end if
                 do j = 1, neig
                    call stdlib_I64_stpsv( uplo, trans, 'NON-UNIT', n, bp, z( 1_ilp64, j ),1_ilp64 )
                 end do
              else if( itype==3_ilp64 ) then
                 ! for b*a*x=(lambda)*x;
                 ! backtransform eigenvectors: x = l*y or u**t *y
                 if( upper ) then
                    trans = 'T'
                 else
                    trans = 'N'
                 end if
                 do j = 1, neig
                    call stdlib_I64_stpmv( uplo, trans, 'NON-UNIT', n, bp, z( 1_ilp64, j ),1_ilp64 )
                 end do
              end if
           end if
           work( 1_ilp64 ) = lwmin
           iwork( 1_ilp64 ) = liwmin
           return
     end subroutine stdlib_I64_sspgvd

     module subroutine stdlib_I64_dspgvd( itype, jobz, uplo, n, ap, bp, w, z, ldz, work,lwork, iwork, liwork,&
     !! DSPGVD computes all the eigenvalues, and optionally, the eigenvectors
     !! of a real generalized symmetric-definite eigenproblem, of the form
     !! A*x=(lambda)*B*x,  A*Bx=(lambda)*x,  or B*A*x=(lambda)*x.  Here A and
     !! B are assumed to be symmetric, stored in packed format, and B is also
     !! positive definite.
     !! If eigenvectors are desired, it uses a divide and conquer algorithm.
     !! The divide and conquer algorithm makes very mild assumptions about
     !! floating point arithmetic. It will work on machines with a guard
     !! digit in add/subtract, or on those binary machines without guard
     !! digits which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or
     !! Cray-2. It could conceivably fail on hexadecimal or decimal machines
     !! without guard digits, but we know of none.
                info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: itype, ldz, liwork, lwork, n
           ! Array Arguments 
           integer(ilp64), intent(out) :: iwork(*)
           real(dp), intent(inout) :: ap(*), bp(*)
           real(dp), intent(out) :: w(*), work(*), z(ldz,*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lquery, upper, wantz
           character :: trans
           integer(ilp64) :: j, liwmin, lwmin, neig
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           upper = stdlib_lsame( uplo, 'U' )
           lquery = ( lwork==-1_ilp64 .or. liwork==-1_ilp64 )
           info = 0_ilp64
           if( itype<1_ilp64 .or. itype>3_ilp64 ) then
              info = -1_ilp64
           else if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -2_ilp64
           else if( .not.( upper .or. stdlib_lsame( uplo, 'L' ) ) ) then
              info = -3_ilp64
           else if( n<0_ilp64 ) then
              info = -4_ilp64
           else if( ldz<1_ilp64 .or. ( wantz .and. ldz<n ) ) then
              info = -9_ilp64
           end if
           if( info==0_ilp64 ) then
              if( n<=1_ilp64 ) then
                 liwmin = 1_ilp64
                 lwmin = 1_ilp64
              else
                 if( wantz ) then
                    liwmin = 3_ilp64 + 5_ilp64*n
                    lwmin = 1_ilp64 + 6_ilp64*n + 2_ilp64*n**2_ilp64
                 else
                    liwmin = 1_ilp64
                    lwmin = 2_ilp64*n
                 end if
              end if
              work( 1_ilp64 ) = lwmin
              iwork( 1_ilp64 ) = liwmin
              if( lwork<lwmin .and. .not.lquery ) then
                 info = -11_ilp64
              else if( liwork<liwmin .and. .not.lquery ) then
                 info = -13_ilp64
              end if
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'DSPGVD', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! form a cholesky factorization of bp.
           call stdlib_I64_dpptrf( uplo, n, bp, info )
           if( info/=0_ilp64 ) then
              info = n + info
              return
           end if
           ! transform problem to standard eigenvalue problem and solve.
           call stdlib_I64_dspgst( itype, uplo, n, ap, bp, info )
           call stdlib_I64_dspevd( jobz, uplo, n, ap, w, z, ldz, work, lwork, iwork,liwork, info )
                     
           lwmin = max( real( lwmin,KIND=dp), real( work( 1_ilp64 ),KIND=dp) )
           liwmin = max( real( liwmin,KIND=dp), real( iwork( 1_ilp64 ),KIND=dp) )
           if( wantz ) then
              ! backtransform eigenvectors to the original problem.
              neig = n
              if( info>0_ilp64 )neig = info - 1_ilp64
              if( itype==1_ilp64 .or. itype==2_ilp64 ) then
                 ! for a*x=(lambda)*b*x and a*b*x=(lambda)*x;
                 ! backtransform eigenvectors: x = inv(l)**t *y or inv(u)*y
                 if( upper ) then
                    trans = 'N'
                 else
                    trans = 'T'
                 end if
                 do j = 1, neig
                    call stdlib_I64_dtpsv( uplo, trans, 'NON-UNIT', n, bp, z( 1_ilp64, j ),1_ilp64 )
                 end do
              else if( itype==3_ilp64 ) then
                 ! for b*a*x=(lambda)*x;
                 ! backtransform eigenvectors: x = l*y or u**t *y
                 if( upper ) then
                    trans = 'T'
                 else
                    trans = 'N'
                 end if
                 do j = 1, neig
                    call stdlib_I64_dtpmv( uplo, trans, 'NON-UNIT', n, bp, z( 1_ilp64, j ),1_ilp64 )
                 end do
              end if
           end if
           work( 1_ilp64 ) = lwmin
           iwork( 1_ilp64 ) = liwmin
           return
     end subroutine stdlib_I64_dspgvd




     module subroutine stdlib_I64_sspgvx( itype, jobz, range, uplo, n, ap, bp, vl, vu,il, iu, abstol, m, w, &
     !! SSPGVX computes selected eigenvalues, and optionally, eigenvectors
     !! of a real generalized symmetric-definite eigenproblem, of the form
     !! A*x=(lambda)*B*x,  A*Bx=(lambda)*x,  or B*A*x=(lambda)*x.  Here A
     !! and B are assumed to be symmetric, stored in packed storage, and B
     !! is also positive definite.  Eigenvalues and eigenvectors can be
     !! selected by specifying either a range of values or a range of indices
     !! for the desired eigenvalues.
               z, ldz, work, iwork,ifail, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, range, uplo
           integer(ilp64), intent(in) :: il, itype, iu, ldz, n
           integer(ilp64), intent(out) :: info, m
           real(sp), intent(in) :: abstol, vl, vu
           ! Array Arguments 
           integer(ilp64), intent(out) :: ifail(*), iwork(*)
           real(sp), intent(inout) :: ap(*), bp(*)
           real(sp), intent(out) :: w(*), work(*), z(ldz,*)
       ! =====================================================================
           ! Local Scalars 
           logical(lk) :: alleig, indeig, upper, valeig, wantz
           character :: trans
           integer(ilp64) :: j
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           upper = stdlib_lsame( uplo, 'U' )
           wantz = stdlib_lsame( jobz, 'V' )
           alleig = stdlib_lsame( range, 'A' )
           valeig = stdlib_lsame( range, 'V' )
           indeig = stdlib_lsame( range, 'I' )
           info = 0_ilp64
           if( itype<1_ilp64 .or. itype>3_ilp64 ) then
              info = -1_ilp64
           else if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -2_ilp64
           else if( .not.( alleig .or. valeig .or. indeig ) ) then
              info = -3_ilp64
           else if( .not.( upper .or. stdlib_lsame( uplo, 'L' ) ) ) then
              info = -4_ilp64
           else if( n<0_ilp64 ) then
              info = -5_ilp64
           else
              if( valeig ) then
                 if( n>0_ilp64 .and. vu<=vl ) then
                    info = -9_ilp64
                 end if
              else if( indeig ) then
                 if( il<1_ilp64 ) then
                    info = -10_ilp64
                 else if( iu<min( n, il ) .or. iu>n ) then
                    info = -11_ilp64
                 end if
              end if
           end if
           if( info==0_ilp64 ) then
              if( ldz<1_ilp64 .or. ( wantz .and. ldz<n ) ) then
                 info = -16_ilp64
              end if
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'SSPGVX', -info )
              return
           end if
           ! quick return if possible
           m = 0_ilp64
           if( n==0 )return
           ! form a cholesky factorization of b.
           call stdlib_I64_spptrf( uplo, n, bp, info )
           if( info/=0_ilp64 ) then
              info = n + info
              return
           end if
           ! transform problem to standard eigenvalue problem and solve.
           call stdlib_I64_sspgst( itype, uplo, n, ap, bp, info )
           call stdlib_I64_sspevx( jobz, range, uplo, n, ap, vl, vu, il, iu, abstol, m,w, z, ldz, &
                     work, iwork, ifail, info )
           if( wantz ) then
              ! backtransform eigenvectors to the original problem.
              if( info>0_ilp64 )m = info - 1_ilp64
              if( itype==1_ilp64 .or. itype==2_ilp64 ) then
                 ! for a*x=(lambda)*b*x and a*b*x=(lambda)*x;
                 ! backtransform eigenvectors: x = inv(l)**t*y or inv(u)*y
                 if( upper ) then
                    trans = 'N'
                 else
                    trans = 'T'
                 end if
                 do j = 1, m
                    call stdlib_I64_stpsv( uplo, trans, 'NON-UNIT', n, bp, z( 1_ilp64, j ),1_ilp64 )
                 end do
              else if( itype==3_ilp64 ) then
                 ! for b*a*x=(lambda)*x;
                 ! backtransform eigenvectors: x = l*y or u**t*y
                 if( upper ) then
                    trans = 'T'
                 else
                    trans = 'N'
                 end if
                 do j = 1, m
                    call stdlib_I64_stpmv( uplo, trans, 'NON-UNIT', n, bp, z( 1_ilp64, j ),1_ilp64 )
                 end do
              end if
           end if
           return
     end subroutine stdlib_I64_sspgvx

     module subroutine stdlib_I64_dspgvx( itype, jobz, range, uplo, n, ap, bp, vl, vu,il, iu, abstol, m, w, &
     !! DSPGVX computes selected eigenvalues, and optionally, eigenvectors
     !! of a real generalized symmetric-definite eigenproblem, of the form
     !! A*x=(lambda)*B*x,  A*Bx=(lambda)*x,  or B*A*x=(lambda)*x.  Here A
     !! and B are assumed to be symmetric, stored in packed storage, and B
     !! is also positive definite.  Eigenvalues and eigenvectors can be
     !! selected by specifying either a range of values or a range of indices
     !! for the desired eigenvalues.
               z, ldz, work, iwork,ifail, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, range, uplo
           integer(ilp64), intent(in) :: il, itype, iu, ldz, n
           integer(ilp64), intent(out) :: info, m
           real(dp), intent(in) :: abstol, vl, vu
           ! Array Arguments 
           integer(ilp64), intent(out) :: ifail(*), iwork(*)
           real(dp), intent(inout) :: ap(*), bp(*)
           real(dp), intent(out) :: w(*), work(*), z(ldz,*)
       ! =====================================================================
           ! Local Scalars 
           logical(lk) :: alleig, indeig, upper, valeig, wantz
           character :: trans
           integer(ilp64) :: j
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           upper = stdlib_lsame( uplo, 'U' )
           wantz = stdlib_lsame( jobz, 'V' )
           alleig = stdlib_lsame( range, 'A' )
           valeig = stdlib_lsame( range, 'V' )
           indeig = stdlib_lsame( range, 'I' )
           info = 0_ilp64
           if( itype<1_ilp64 .or. itype>3_ilp64 ) then
              info = -1_ilp64
           else if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -2_ilp64
           else if( .not.( alleig .or. valeig .or. indeig ) ) then
              info = -3_ilp64
           else if( .not.( upper .or. stdlib_lsame( uplo, 'L' ) ) ) then
              info = -4_ilp64
           else if( n<0_ilp64 ) then
              info = -5_ilp64
           else
              if( valeig ) then
                 if( n>0_ilp64 .and. vu<=vl ) then
                    info = -9_ilp64
                 end if
              else if( indeig ) then
                 if( il<1_ilp64 ) then
                    info = -10_ilp64
                 else if( iu<min( n, il ) .or. iu>n ) then
                    info = -11_ilp64
                 end if
              end if
           end if
           if( info==0_ilp64 ) then
              if( ldz<1_ilp64 .or. ( wantz .and. ldz<n ) ) then
                 info = -16_ilp64
              end if
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'DSPGVX', -info )
              return
           end if
           ! quick return if possible
           m = 0_ilp64
           if( n==0 )return
           ! form a cholesky factorization of b.
           call stdlib_I64_dpptrf( uplo, n, bp, info )
           if( info/=0_ilp64 ) then
              info = n + info
              return
           end if
           ! transform problem to standard eigenvalue problem and solve.
           call stdlib_I64_dspgst( itype, uplo, n, ap, bp, info )
           call stdlib_I64_dspevx( jobz, range, uplo, n, ap, vl, vu, il, iu, abstol, m,w, z, ldz, &
                     work, iwork, ifail, info )
           if( wantz ) then
              ! backtransform eigenvectors to the original problem.
              if( info>0_ilp64 )m = info - 1_ilp64
              if( itype==1_ilp64 .or. itype==2_ilp64 ) then
                 ! for a*x=(lambda)*b*x and a*b*x=(lambda)*x;
                 ! backtransform eigenvectors: x = inv(l)**t*y or inv(u)*y
                 if( upper ) then
                    trans = 'N'
                 else
                    trans = 'T'
                 end if
                 do j = 1, m
                    call stdlib_I64_dtpsv( uplo, trans, 'NON-UNIT', n, bp, z( 1_ilp64, j ),1_ilp64 )
                 end do
              else if( itype==3_ilp64 ) then
                 ! for b*a*x=(lambda)*x;
                 ! backtransform eigenvectors: x = l*y or u**t*y
                 if( upper ) then
                    trans = 'T'
                 else
                    trans = 'N'
                 end if
                 do j = 1, m
                    call stdlib_I64_dtpmv( uplo, trans, 'NON-UNIT', n, bp, z( 1_ilp64, j ),1_ilp64 )
                 end do
              end if
           end if
           return
     end subroutine stdlib_I64_dspgvx




     pure module subroutine stdlib_I64_ssbgv( jobz, uplo, n, ka, kb, ab, ldab, bb, ldbb, w, z,ldz, work, &
     !! SSBGV computes all the eigenvalues, and optionally, the eigenvectors
     !! of a real generalized symmetric-definite banded eigenproblem, of
     !! the form A*x=(lambda)*B*x. Here A and B are assumed to be symmetric
     !! and banded, and B is also positive definite.
               info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: ka, kb, ldab, ldbb, ldz, n
           ! Array Arguments 
           real(sp), intent(inout) :: ab(ldab,*), bb(ldbb,*)
           real(sp), intent(out) :: w(*), work(*), z(ldz,*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: upper, wantz
           character :: vect
           integer(ilp64) :: iinfo, inde, indwrk
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           upper = stdlib_lsame( uplo, 'U' )
           info = 0_ilp64
           if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -1_ilp64
           else if( .not.( upper .or. stdlib_lsame( uplo, 'L' ) ) ) then
              info = -2_ilp64
           else if( n<0_ilp64 ) then
              info = -3_ilp64
           else if( ka<0_ilp64 ) then
              info = -4_ilp64
           else if( kb<0_ilp64 .or. kb>ka ) then
              info = -5_ilp64
           else if( ldab<ka+1 ) then
              info = -7_ilp64
           else if( ldbb<kb+1 ) then
              info = -9_ilp64
           else if( ldz<1_ilp64 .or. ( wantz .and. ldz<n ) ) then
              info = -12_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'SSBGV ', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! form a split cholesky factorization of b.
           call stdlib_I64_spbstf( uplo, n, kb, bb, ldbb, info )
           if( info/=0_ilp64 ) then
              info = n + info
              return
           end if
           ! transform problem to standard eigenvalue problem.
           inde = 1_ilp64
           indwrk = inde + n
           call stdlib_I64_ssbgst( jobz, uplo, n, ka, kb, ab, ldab, bb, ldbb, z, ldz,work( indwrk ), &
                     iinfo )
           ! reduce to tridiagonal form.
           if( wantz ) then
              vect = 'U'
           else
              vect = 'N'
           end if
           call stdlib_I64_ssbtrd( vect, uplo, n, ka, ab, ldab, w, work( inde ), z, ldz,work( indwrk )&
                     , iinfo )
           ! for eigenvalues only, call stdlib_I64_ssterf.  for eigenvectors, call stdlib_I64_ssteqr.
           if( .not.wantz ) then
              call stdlib_I64_ssterf( n, w, work( inde ), info )
           else
              call stdlib_I64_ssteqr( jobz, n, w, work( inde ), z, ldz, work( indwrk ),info )
           end if
           return
     end subroutine stdlib_I64_ssbgv

     pure module subroutine stdlib_I64_dsbgv( jobz, uplo, n, ka, kb, ab, ldab, bb, ldbb, w, z,ldz, work, &
     !! DSBGV computes all the eigenvalues, and optionally, the eigenvectors
     !! of a real generalized symmetric-definite banded eigenproblem, of
     !! the form A*x=(lambda)*B*x. Here A and B are assumed to be symmetric
     !! and banded, and B is also positive definite.
               info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: ka, kb, ldab, ldbb, ldz, n
           ! Array Arguments 
           real(dp), intent(inout) :: ab(ldab,*), bb(ldbb,*)
           real(dp), intent(out) :: w(*), work(*), z(ldz,*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: upper, wantz
           character :: vect
           integer(ilp64) :: iinfo, inde, indwrk
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           upper = stdlib_lsame( uplo, 'U' )
           info = 0_ilp64
           if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -1_ilp64
           else if( .not.( upper .or. stdlib_lsame( uplo, 'L' ) ) ) then
              info = -2_ilp64
           else if( n<0_ilp64 ) then
              info = -3_ilp64
           else if( ka<0_ilp64 ) then
              info = -4_ilp64
           else if( kb<0_ilp64 .or. kb>ka ) then
              info = -5_ilp64
           else if( ldab<ka+1 ) then
              info = -7_ilp64
           else if( ldbb<kb+1 ) then
              info = -9_ilp64
           else if( ldz<1_ilp64 .or. ( wantz .and. ldz<n ) ) then
              info = -12_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'DSBGV ', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! form a split cholesky factorization of b.
           call stdlib_I64_dpbstf( uplo, n, kb, bb, ldbb, info )
           if( info/=0_ilp64 ) then
              info = n + info
              return
           end if
           ! transform problem to standard eigenvalue problem.
           inde = 1_ilp64
           indwrk = inde + n
           call stdlib_I64_dsbgst( jobz, uplo, n, ka, kb, ab, ldab, bb, ldbb, z, ldz,work( indwrk ), &
                     iinfo )
           ! reduce to tridiagonal form.
           if( wantz ) then
              vect = 'U'
           else
              vect = 'N'
           end if
           call stdlib_I64_dsbtrd( vect, uplo, n, ka, ab, ldab, w, work( inde ), z, ldz,work( indwrk )&
                     , iinfo )
           ! for eigenvalues only, call stdlib_I64_dsterf.  for eigenvectors, call stdlib_I64_ssteqr.
           if( .not.wantz ) then
              call stdlib_I64_dsterf( n, w, work( inde ), info )
           else
              call stdlib_I64_dsteqr( jobz, n, w, work( inde ), z, ldz, work( indwrk ),info )
           end if
           return
     end subroutine stdlib_I64_dsbgv




     pure module subroutine stdlib_I64_ssbgvd( jobz, uplo, n, ka, kb, ab, ldab, bb, ldbb, w,z, ldz, work, &
     !! SSBGVD computes all the eigenvalues, and optionally, the eigenvectors
     !! of a real generalized symmetric-definite banded eigenproblem, of the
     !! form A*x=(lambda)*B*x.  Here A and B are assumed to be symmetric and
     !! banded, and B is also positive definite.  If eigenvectors are
     !! desired, it uses a divide and conquer algorithm.
     !! The divide and conquer algorithm makes very mild assumptions about
     !! floating point arithmetic. It will work on machines with a guard
     !! digit in add/subtract, or on those binary machines without guard
     !! digits which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or
     !! Cray-2. It could conceivably fail on hexadecimal or decimal machines
     !! without guard digits, but we know of none.
               lwork, iwork, liwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: ka, kb, ldab, ldbb, ldz, liwork, lwork, n
           ! Array Arguments 
           integer(ilp64), intent(out) :: iwork(*)
           real(sp), intent(inout) :: ab(ldab,*), bb(ldbb,*)
           real(sp), intent(out) :: w(*), work(*), z(ldz,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery, upper, wantz
           character :: vect
           integer(ilp64) :: iinfo, inde, indwk2, indwrk, liwmin, llwrk2, lwmin
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           upper = stdlib_lsame( uplo, 'U' )
           lquery = ( lwork==-1_ilp64 .or. liwork==-1_ilp64 )
           info = 0_ilp64
           if( n<=1_ilp64 ) then
              liwmin = 1_ilp64
              lwmin = 1_ilp64
           else if( wantz ) then
              liwmin = 3_ilp64 + 5_ilp64*n
              lwmin = 1_ilp64 + 5_ilp64*n + 2_ilp64*n**2_ilp64
           else
              liwmin = 1_ilp64
              lwmin = 2_ilp64*n
           end if
           if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -1_ilp64
           else if( .not.( upper .or. stdlib_lsame( uplo, 'L' ) ) ) then
              info = -2_ilp64
           else if( n<0_ilp64 ) then
              info = -3_ilp64
           else if( ka<0_ilp64 ) then
              info = -4_ilp64
           else if( kb<0_ilp64 .or. kb>ka ) then
              info = -5_ilp64
           else if( ldab<ka+1 ) then
              info = -7_ilp64
           else if( ldbb<kb+1 ) then
              info = -9_ilp64
           else if( ldz<1_ilp64 .or. ( wantz .and. ldz<n ) ) then
              info = -12_ilp64
           end if
           if( info==0_ilp64 ) then
              work( 1_ilp64 ) = lwmin
              iwork( 1_ilp64 ) = liwmin
              if( lwork<lwmin .and. .not.lquery ) then
                 info = -14_ilp64
              else if( liwork<liwmin .and. .not.lquery ) then
                 info = -16_ilp64
              end if
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'SSBGVD', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! form a split cholesky factorization of b.
           call stdlib_I64_spbstf( uplo, n, kb, bb, ldbb, info )
           if( info/=0_ilp64 ) then
              info = n + info
              return
           end if
           ! transform problem to standard eigenvalue problem.
           inde = 1_ilp64
           indwrk = inde + n
           indwk2 = indwrk + n*n
           llwrk2 = lwork - indwk2 + 1_ilp64
           call stdlib_I64_ssbgst( jobz, uplo, n, ka, kb, ab, ldab, bb, ldbb, z, ldz,work, iinfo )
                     
           ! reduce to tridiagonal form.
           if( wantz ) then
              vect = 'U'
           else
              vect = 'N'
           end if
           call stdlib_I64_ssbtrd( vect, uplo, n, ka, ab, ldab, w, work( inde ), z, ldz,work( indwrk )&
                     , iinfo )
           ! for eigenvalues only, call stdlib_I64_ssterf. for eigenvectors, call stdlib_I64_sstedc.
           if( .not.wantz ) then
              call stdlib_I64_ssterf( n, w, work( inde ), info )
           else
              call stdlib_I64_sstedc( 'I', n, w, work( inde ), work( indwrk ), n,work( indwk2 ), &
                        llwrk2, iwork, liwork, info )
              call stdlib_I64_sgemm( 'N', 'N', n, n, n, one, z, ldz, work( indwrk ), n,zero, work( &
                        indwk2 ), n )
              call stdlib_I64_slacpy( 'A', n, n, work( indwk2 ), n, z, ldz )
           end if
           work( 1_ilp64 ) = lwmin
           iwork( 1_ilp64 ) = liwmin
           return
     end subroutine stdlib_I64_ssbgvd

     pure module subroutine stdlib_I64_dsbgvd( jobz, uplo, n, ka, kb, ab, ldab, bb, ldbb, w,z, ldz, work, &
     !! DSBGVD computes all the eigenvalues, and optionally, the eigenvectors
     !! of a real generalized symmetric-definite banded eigenproblem, of the
     !! form A*x=(lambda)*B*x.  Here A and B are assumed to be symmetric and
     !! banded, and B is also positive definite.  If eigenvectors are
     !! desired, it uses a divide and conquer algorithm.
     !! The divide and conquer algorithm makes very mild assumptions about
     !! floating point arithmetic. It will work on machines with a guard
     !! digit in add/subtract, or on those binary machines without guard
     !! digits which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or
     !! Cray-2. It could conceivably fail on hexadecimal or decimal machines
     !! without guard digits, but we know of none.
               lwork, iwork, liwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: ka, kb, ldab, ldbb, ldz, liwork, lwork, n
           ! Array Arguments 
           integer(ilp64), intent(out) :: iwork(*)
           real(dp), intent(inout) :: ab(ldab,*), bb(ldbb,*)
           real(dp), intent(out) :: w(*), work(*), z(ldz,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery, upper, wantz
           character :: vect
           integer(ilp64) :: iinfo, inde, indwk2, indwrk, liwmin, llwrk2, lwmin
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           upper = stdlib_lsame( uplo, 'U' )
           lquery = ( lwork==-1_ilp64 .or. liwork==-1_ilp64 )
           info = 0_ilp64
           if( n<=1_ilp64 ) then
              liwmin = 1_ilp64
              lwmin = 1_ilp64
           else if( wantz ) then
              liwmin = 3_ilp64 + 5_ilp64*n
              lwmin = 1_ilp64 + 5_ilp64*n + 2_ilp64*n**2_ilp64
           else
              liwmin = 1_ilp64
              lwmin = 2_ilp64*n
           end if
           if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -1_ilp64
           else if( .not.( upper .or. stdlib_lsame( uplo, 'L' ) ) ) then
              info = -2_ilp64
           else if( n<0_ilp64 ) then
              info = -3_ilp64
           else if( ka<0_ilp64 ) then
              info = -4_ilp64
           else if( kb<0_ilp64 .or. kb>ka ) then
              info = -5_ilp64
           else if( ldab<ka+1 ) then
              info = -7_ilp64
           else if( ldbb<kb+1 ) then
              info = -9_ilp64
           else if( ldz<1_ilp64 .or. ( wantz .and. ldz<n ) ) then
              info = -12_ilp64
           end if
           if( info==0_ilp64 ) then
              work( 1_ilp64 ) = lwmin
              iwork( 1_ilp64 ) = liwmin
              if( lwork<lwmin .and. .not.lquery ) then
                 info = -14_ilp64
              else if( liwork<liwmin .and. .not.lquery ) then
                 info = -16_ilp64
              end if
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'DSBGVD', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! form a split cholesky factorization of b.
           call stdlib_I64_dpbstf( uplo, n, kb, bb, ldbb, info )
           if( info/=0_ilp64 ) then
              info = n + info
              return
           end if
           ! transform problem to standard eigenvalue problem.
           inde = 1_ilp64
           indwrk = inde + n
           indwk2 = indwrk + n*n
           llwrk2 = lwork - indwk2 + 1_ilp64
           call stdlib_I64_dsbgst( jobz, uplo, n, ka, kb, ab, ldab, bb, ldbb, z, ldz,work, iinfo )
                     
           ! reduce to tridiagonal form.
           if( wantz ) then
              vect = 'U'
           else
              vect = 'N'
           end if
           call stdlib_I64_dsbtrd( vect, uplo, n, ka, ab, ldab, w, work( inde ), z, ldz,work( indwrk )&
                     , iinfo )
           ! for eigenvalues only, call stdlib_I64_dsterf. for eigenvectors, call stdlib_I64_sstedc.
           if( .not.wantz ) then
              call stdlib_I64_dsterf( n, w, work( inde ), info )
           else
              call stdlib_I64_dstedc( 'I', n, w, work( inde ), work( indwrk ), n,work( indwk2 ), &
                        llwrk2, iwork, liwork, info )
              call stdlib_I64_dgemm( 'N', 'N', n, n, n, one, z, ldz, work( indwrk ), n,zero, work( &
                        indwk2 ), n )
              call stdlib_I64_dlacpy( 'A', n, n, work( indwk2 ), n, z, ldz )
           end if
           work( 1_ilp64 ) = lwmin
           iwork( 1_ilp64 ) = liwmin
           return
     end subroutine stdlib_I64_dsbgvd




     pure module subroutine stdlib_I64_ssbgvx( jobz, range, uplo, n, ka, kb, ab, ldab, bb,ldbb, q, ldq, vl, &
     !! SSBGVX computes selected eigenvalues, and optionally, eigenvectors
     !! of a real generalized symmetric-definite banded eigenproblem, of
     !! the form A*x=(lambda)*B*x.  Here A and B are assumed to be symmetric
     !! and banded, and B is also positive definite.  Eigenvalues and
     !! eigenvectors can be selected by specifying either all eigenvalues,
     !! a range of values or a range of indices for the desired eigenvalues.
               vu, il, iu, abstol, m, w, z,ldz, work, iwork, ifail, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, range, uplo
           integer(ilp64), intent(in) :: il, iu, ka, kb, ldab, ldbb, ldq, ldz, n
           integer(ilp64), intent(out) :: info, m
           real(sp), intent(in) :: abstol, vl, vu
           ! Array Arguments 
           integer(ilp64), intent(out) :: ifail(*), iwork(*)
           real(sp), intent(inout) :: ab(ldab,*), bb(ldbb,*)
           real(sp), intent(out) :: q(ldq,*), w(*), work(*), z(ldz,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: alleig, indeig, test, upper, valeig, wantz
           character :: order, vect
           integer(ilp64) :: i, iinfo, indd, inde, indee, indibl, indisp, indiwo, indwrk, itmp1, j, &
                     jj, nsplit
           real(sp) :: tmp1
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           upper = stdlib_lsame( uplo, 'U' )
           alleig = stdlib_lsame( range, 'A' )
           valeig = stdlib_lsame( range, 'V' )
           indeig = stdlib_lsame( range, 'I' )
           info = 0_ilp64
           if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -1_ilp64
           else if( .not.( alleig .or. valeig .or. indeig ) ) then
              info = -2_ilp64
           else if( .not.( upper .or. stdlib_lsame( uplo, 'L' ) ) ) then
              info = -3_ilp64
           else if( n<0_ilp64 ) then
              info = -4_ilp64
           else if( ka<0_ilp64 ) then
              info = -5_ilp64
           else if( kb<0_ilp64 .or. kb>ka ) then
              info = -6_ilp64
           else if( ldab<ka+1 ) then
              info = -8_ilp64
           else if( ldbb<kb+1 ) then
              info = -10_ilp64
           else if( ldq<1_ilp64 .or. ( wantz .and. ldq<n ) ) then
              info = -12_ilp64
           else
              if( valeig ) then
                 if( n>0_ilp64 .and. vu<=vl )info = -14_ilp64
              else if( indeig ) then
                 if( il<1_ilp64 .or. il>max( 1_ilp64, n ) ) then
                    info = -15_ilp64
                 else if ( iu<min( n, il ) .or. iu>n ) then
                    info = -16_ilp64
                 end if
              end if
           end if
           if( info==0_ilp64) then
              if( ldz<1_ilp64 .or. ( wantz .and. ldz<n ) ) then
                 info = -21_ilp64
              end if
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'SSBGVX', -info )
              return
           end if
           ! quick return if possible
           m = 0_ilp64
           if( n==0 )return
           ! form a split cholesky factorization of b.
           call stdlib_I64_spbstf( uplo, n, kb, bb, ldbb, info )
           if( info/=0_ilp64 ) then
              info = n + info
              return
           end if
           ! transform problem to standard eigenvalue problem.
           call stdlib_I64_ssbgst( jobz, uplo, n, ka, kb, ab, ldab, bb, ldbb, q, ldq,work, iinfo )
                     
           ! reduce symmetric band matrix to tridiagonal form.
           indd = 1_ilp64
           inde = indd + n
           indwrk = inde + n
           if( wantz ) then
              vect = 'U'
           else
              vect = 'N'
           end if
           call stdlib_I64_ssbtrd( vect, uplo, n, ka, ab, ldab, work( indd ),work( inde ), q, ldq, &
                     work( indwrk ), iinfo )
           ! if all eigenvalues are desired and abstol is less than or equal
           ! to zero, then call stdlib_I64_ssterf or stdlib_I64_ssteqr.  if this fails for some
           ! eigenvalue, then try stdlib_I64_sstebz.
           test = .false.
           if( indeig ) then
              if( il==1_ilp64 .and. iu==n ) then
                 test = .true.
              end if
           end if
           if( ( alleig .or. test ) .and. ( abstol<=zero ) ) then
              call stdlib_I64_scopy( n, work( indd ), 1_ilp64, w, 1_ilp64 )
              indee = indwrk + 2_ilp64*n
              call stdlib_I64_scopy( n-1, work( inde ), 1_ilp64, work( indee ), 1_ilp64 )
              if( .not.wantz ) then
                 call stdlib_I64_ssterf( n, w, work( indee ), info )
              else
                 call stdlib_I64_slacpy( 'A', n, n, q, ldq, z, ldz )
                 call stdlib_I64_ssteqr( jobz, n, w, work( indee ), z, ldz,work( indwrk ), info )
                           
                 if( info==0_ilp64 ) then
                    do i = 1, n
                       ifail( i ) = 0_ilp64
                    end do
                 end if
              end if
              if( info==0_ilp64 ) then
                 m = n
                 go to 30
              end if
              info = 0_ilp64
           end if
           ! otherwise, call stdlib_I64_sstebz and, if eigenvectors are desired,
           ! call stdlib_I64_sstein.
           if( wantz ) then
              order = 'B'
           else
              order = 'E'
           end if
           indibl = 1_ilp64
           indisp = indibl + n
           indiwo = indisp + n
           call stdlib_I64_sstebz( range, order, n, vl, vu, il, iu, abstol,work( indd ), work( inde ),&
            m, nsplit, w,iwork( indibl ), iwork( indisp ), work( indwrk ),iwork( indiwo ), info )
                      
           if( wantz ) then
              call stdlib_I64_sstein( n, work( indd ), work( inde ), m, w,iwork( indibl ), iwork( &
                        indisp ), z, ldz,work( indwrk ), iwork( indiwo ), ifail, info )
              ! apply transformation matrix used in reduction to tridiagonal
              ! form to eigenvectors returned by stdlib_I64_sstein.
              do j = 1, m
                 call stdlib_I64_scopy( n, z( 1_ilp64, j ), 1_ilp64, work( 1_ilp64 ), 1_ilp64 )
                 call stdlib_I64_sgemv( 'N', n, n, one, q, ldq, work, 1_ilp64, zero,z( 1_ilp64, j ), 1_ilp64 )
              end do
           end if
           30 continue
           ! if eigenvalues are not in order, then sort them, along with
           ! eigenvectors.
           if( wantz ) then
              do j = 1, m - 1
                 i = 0_ilp64
                 tmp1 = w( j )
                 do jj = j + 1, m
                    if( w( jj )<tmp1 ) then
                       i = jj
                       tmp1 = w( jj )
                    end if
                 end do
                 if( i/=0_ilp64 ) then
                    itmp1 = iwork( indibl+i-1 )
                    w( i ) = w( j )
                    iwork( indibl+i-1 ) = iwork( indibl+j-1 )
                    w( j ) = tmp1
                    iwork( indibl+j-1 ) = itmp1
                    call stdlib_I64_sswap( n, z( 1_ilp64, i ), 1_ilp64, z( 1_ilp64, j ), 1_ilp64 )
                    if( info/=0_ilp64 ) then
                       itmp1 = ifail( i )
                       ifail( i ) = ifail( j )
                       ifail( j ) = itmp1
                    end if
                 end if
              end do
           end if
           return
     end subroutine stdlib_I64_ssbgvx

     pure module subroutine stdlib_I64_dsbgvx( jobz, range, uplo, n, ka, kb, ab, ldab, bb,ldbb, q, ldq, vl, &
     !! DSBGVX computes selected eigenvalues, and optionally, eigenvectors
     !! of a real generalized symmetric-definite banded eigenproblem, of
     !! the form A*x=(lambda)*B*x.  Here A and B are assumed to be symmetric
     !! and banded, and B is also positive definite.  Eigenvalues and
     !! eigenvectors can be selected by specifying either all eigenvalues,
     !! a range of values or a range of indices for the desired eigenvalues.
               vu, il, iu, abstol, m, w, z,ldz, work, iwork, ifail, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, range, uplo
           integer(ilp64), intent(in) :: il, iu, ka, kb, ldab, ldbb, ldq, ldz, n
           integer(ilp64), intent(out) :: info, m
           real(dp), intent(in) :: abstol, vl, vu
           ! Array Arguments 
           integer(ilp64), intent(out) :: ifail(*), iwork(*)
           real(dp), intent(inout) :: ab(ldab,*), bb(ldbb,*)
           real(dp), intent(out) :: q(ldq,*), w(*), work(*), z(ldz,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: alleig, indeig, test, upper, valeig, wantz
           character :: order, vect
           integer(ilp64) :: i, iinfo, indd, inde, indee, indibl, indisp, indiwo, indwrk, itmp1, j, &
                     jj, nsplit
           real(dp) :: tmp1
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           upper = stdlib_lsame( uplo, 'U' )
           alleig = stdlib_lsame( range, 'A' )
           valeig = stdlib_lsame( range, 'V' )
           indeig = stdlib_lsame( range, 'I' )
           info = 0_ilp64
           if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -1_ilp64
           else if( .not.( alleig .or. valeig .or. indeig ) ) then
              info = -2_ilp64
           else if( .not.( upper .or. stdlib_lsame( uplo, 'L' ) ) ) then
              info = -3_ilp64
           else if( n<0_ilp64 ) then
              info = -4_ilp64
           else if( ka<0_ilp64 ) then
              info = -5_ilp64
           else if( kb<0_ilp64 .or. kb>ka ) then
              info = -6_ilp64
           else if( ldab<ka+1 ) then
              info = -8_ilp64
           else if( ldbb<kb+1 ) then
              info = -10_ilp64
           else if( ldq<1_ilp64 .or. ( wantz .and. ldq<n ) ) then
              info = -12_ilp64
           else
              if( valeig ) then
                 if( n>0_ilp64 .and. vu<=vl )info = -14_ilp64
              else if( indeig ) then
                 if( il<1_ilp64 .or. il>max( 1_ilp64, n ) ) then
                    info = -15_ilp64
                 else if ( iu<min( n, il ) .or. iu>n ) then
                    info = -16_ilp64
                 end if
              end if
           end if
           if( info==0_ilp64) then
              if( ldz<1_ilp64 .or. ( wantz .and. ldz<n ) ) then
                 info = -21_ilp64
              end if
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'DSBGVX', -info )
              return
           end if
           ! quick return if possible
           m = 0_ilp64
           if( n==0 )return
           ! form a split cholesky factorization of b.
           call stdlib_I64_dpbstf( uplo, n, kb, bb, ldbb, info )
           if( info/=0_ilp64 ) then
              info = n + info
              return
           end if
           ! transform problem to standard eigenvalue problem.
           call stdlib_I64_dsbgst( jobz, uplo, n, ka, kb, ab, ldab, bb, ldbb, q, ldq,work, iinfo )
                     
           ! reduce symmetric band matrix to tridiagonal form.
           indd = 1_ilp64
           inde = indd + n
           indwrk = inde + n
           if( wantz ) then
              vect = 'U'
           else
              vect = 'N'
           end if
           call stdlib_I64_dsbtrd( vect, uplo, n, ka, ab, ldab, work( indd ),work( inde ), q, ldq, &
                     work( indwrk ), iinfo )
           ! if all eigenvalues are desired and abstol is less than or equal
           ! to zero, then call stdlib_I64_dsterf or stdlib_I64_ssteqr.  if this fails for some
           ! eigenvalue, then try stdlib_I64_dstebz.
           test = .false.
           if( indeig ) then
              if( il==1_ilp64 .and. iu==n ) then
                 test = .true.
              end if
           end if
           if( ( alleig .or. test ) .and. ( abstol<=zero ) ) then
              call stdlib_I64_dcopy( n, work( indd ), 1_ilp64, w, 1_ilp64 )
              indee = indwrk + 2_ilp64*n
              call stdlib_I64_dcopy( n-1, work( inde ), 1_ilp64, work( indee ), 1_ilp64 )
              if( .not.wantz ) then
                 call stdlib_I64_dsterf( n, w, work( indee ), info )
              else
                 call stdlib_I64_dlacpy( 'A', n, n, q, ldq, z, ldz )
                 call stdlib_I64_dsteqr( jobz, n, w, work( indee ), z, ldz,work( indwrk ), info )
                           
                 if( info==0_ilp64 ) then
                    do i = 1, n
                       ifail( i ) = 0_ilp64
                    end do
                 end if
              end if
              if( info==0_ilp64 ) then
                 m = n
                 go to 30
              end if
              info = 0_ilp64
           end if
           ! otherwise, call stdlib_I64_dstebz and, if eigenvectors are desired,
           ! call stdlib_I64_dstein.
           if( wantz ) then
              order = 'B'
           else
              order = 'E'
           end if
           indibl = 1_ilp64
           indisp = indibl + n
           indiwo = indisp + n
           call stdlib_I64_dstebz( range, order, n, vl, vu, il, iu, abstol,work( indd ), work( inde ),&
            m, nsplit, w,iwork( indibl ), iwork( indisp ), work( indwrk ),iwork( indiwo ), info )
                      
           if( wantz ) then
              call stdlib_I64_dstein( n, work( indd ), work( inde ), m, w,iwork( indibl ), iwork( &
                        indisp ), z, ldz,work( indwrk ), iwork( indiwo ), ifail, info )
              ! apply transformation matrix used in reduction to tridiagonal
              ! form to eigenvectors returned by stdlib_I64_dstein.
              do j = 1, m
                 call stdlib_I64_dcopy( n, z( 1_ilp64, j ), 1_ilp64, work( 1_ilp64 ), 1_ilp64 )
                 call stdlib_I64_dgemv( 'N', n, n, one, q, ldq, work, 1_ilp64, zero,z( 1_ilp64, j ), 1_ilp64 )
              end do
           end if
           30 continue
           ! if eigenvalues are not in order, then sort them, along with
           ! eigenvectors.
           if( wantz ) then
              do j = 1, m - 1
                 i = 0_ilp64
                 tmp1 = w( j )
                 do jj = j + 1, m
                    if( w( jj )<tmp1 ) then
                       i = jj
                       tmp1 = w( jj )
                    end if
                 end do
                 if( i/=0_ilp64 ) then
                    itmp1 = iwork( indibl+i-1 )
                    w( i ) = w( j )
                    iwork( indibl+i-1 ) = iwork( indibl+j-1 )
                    w( j ) = tmp1
                    iwork( indibl+j-1 ) = itmp1
                    call stdlib_I64_dswap( n, z( 1_ilp64, i ), 1_ilp64, z( 1_ilp64, j ), 1_ilp64 )
                    if( info/=0_ilp64 ) then
                       itmp1 = ifail( i )
                       ifail( i ) = ifail( j )
                       ifail( j ) = itmp1
                    end if
                 end if
              end do
           end if
           return
     end subroutine stdlib_I64_dsbgvx




     pure module subroutine stdlib_I64_ssytrd( uplo, n, a, lda, d, e, tau, work, lwork, info )
     !! SSYTRD reduces a real symmetric matrix A to real symmetric
     !! tridiagonal form T by an orthogonal similarity transformation:
     !! Q**T * A * Q = T.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, lwork, n
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: d(*), e(*), tau(*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery, upper
           integer(ilp64) :: i, iinfo, iws, j, kk, ldwork, lwkopt, nb, nbmin, nx
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp64
           upper = stdlib_lsame( uplo, 'U' )
           lquery = ( lwork==-1_ilp64 )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -2_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -4_ilp64
           else if( lwork<1_ilp64 .and. .not.lquery ) then
              info = -9_ilp64
           end if
           if( info==0_ilp64 ) then
              ! determine the block size.
              nb = stdlib_I64_ilaenv( 1_ilp64, 'SSYTRD', uplo, n, -1_ilp64, -1_ilp64, -1_ilp64 )
              lwkopt = n*nb
              work( 1_ilp64 ) = lwkopt
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'SSYTRD', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0_ilp64 ) then
              work( 1_ilp64 ) = 1_ilp64
              return
           end if
           nx = n
           iws = 1_ilp64
           if( nb>1_ilp64 .and. nb<n ) then
              ! determine when to cross over from blocked to unblocked code
              ! (last block is always handled by unblocked code).
              nx = max( nb, stdlib_I64_ilaenv( 3_ilp64, 'SSYTRD', uplo, n, -1_ilp64, -1_ilp64, -1_ilp64 ) )
              if( nx<n ) then
                 ! determine if workspace is large enough for blocked code.
                 ldwork = n
                 iws = ldwork*nb
                 if( lwork<iws ) then
                    ! not enough workspace to use optimal nb:  determine the
                    ! minimum value of nb, and reduce nb or force use of
                    ! unblocked code by setting nx = n.
                    nb = max( lwork / ldwork, 1_ilp64 )
                    nbmin = stdlib_I64_ilaenv( 2_ilp64, 'SSYTRD', uplo, n, -1_ilp64, -1_ilp64, -1_ilp64 )
                    if( nb<nbmin )nx = n
                 end if
              else
                 nx = n
              end if
           else
              nb = 1_ilp64
           end if
           if( upper ) then
              ! reduce the upper triangle of a.
              ! columns 1:kk are handled by the unblocked method.
              kk = n - ( ( n-nx+nb-1 ) / nb )*nb
              do i = n - nb + 1, kk + 1, -nb
                 ! reduce columns i:i+nb-1 to tridiagonal form and form the
                 ! matrix w which is needed to update the unreduced part of
                 ! the matrix
                 call stdlib_I64_slatrd( uplo, i+nb-1, nb, a, lda, e, tau, work,ldwork )
                 ! update the unreduced submatrix a(1:i-1,1:i-1), using an
                 ! update of the form:  a := a - v*w**t - w*v**t
                 call stdlib_I64_ssyr2k( uplo, 'NO TRANSPOSE', i-1, nb, -one, a( 1_ilp64, i ),lda, work, &
                           ldwork, one, a, lda )
                 ! copy superdiagonal elements back into a, and diagonal
                 ! elements into d
                 do j = i, i + nb - 1
                    a( j-1, j ) = e( j-1 )
                    d( j ) = a( j, j )
                 end do
              end do
              ! use unblocked code to reduce the last or only block
              call stdlib_I64_ssytd2( uplo, kk, a, lda, d, e, tau, iinfo )
           else
              ! reduce the lower triangle of a
              do i = 1, n - nx, nb
                 ! reduce columns i:i+nb-1 to tridiagonal form and form the
                 ! matrix w which is needed to update the unreduced part of
                 ! the matrix
                 call stdlib_I64_slatrd( uplo, n-i+1, nb, a( i, i ), lda, e( i ),tau( i ), work, &
                           ldwork )
                 ! update the unreduced submatrix a(i+ib:n,i+ib:n), using
                 ! an update of the form:  a := a - v*w**t - w*v**t
                 call stdlib_I64_ssyr2k( uplo, 'NO TRANSPOSE', n-i-nb+1, nb, -one,a( i+nb, i ), lda, &
                           work( nb+1 ), ldwork, one,a( i+nb, i+nb ), lda )
                 ! copy subdiagonal elements back into a, and diagonal
                 ! elements into d
                 do j = i, i + nb - 1
                    a( j+1, j ) = e( j )
                    d( j ) = a( j, j )
                 end do
              end do
              ! use unblocked code to reduce the last or only block
              call stdlib_I64_ssytd2( uplo, n-i+1, a( i, i ), lda, d( i ), e( i ),tau( i ), iinfo )
                        
           end if
           work( 1_ilp64 ) = lwkopt
           return
     end subroutine stdlib_I64_ssytrd

     pure module subroutine stdlib_I64_dsytrd( uplo, n, a, lda, d, e, tau, work, lwork, info )
     !! DSYTRD reduces a real symmetric matrix A to real symmetric
     !! tridiagonal form T by an orthogonal similarity transformation:
     !! Q**T * A * Q = T.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, lwork, n
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: d(*), e(*), tau(*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery, upper
           integer(ilp64) :: i, iinfo, iws, j, kk, ldwork, lwkopt, nb, nbmin, nx
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp64
           upper = stdlib_lsame( uplo, 'U' )
           lquery = ( lwork==-1_ilp64 )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -2_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -4_ilp64
           else if( lwork<1_ilp64 .and. .not.lquery ) then
              info = -9_ilp64
           end if
           if( info==0_ilp64 ) then
              ! determine the block size.
              nb = stdlib_I64_ilaenv( 1_ilp64, 'DSYTRD', uplo, n, -1_ilp64, -1_ilp64, -1_ilp64 )
              lwkopt = n*nb
              work( 1_ilp64 ) = lwkopt
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'DSYTRD', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0_ilp64 ) then
              work( 1_ilp64 ) = 1_ilp64
              return
           end if
           nx = n
           iws = 1_ilp64
           if( nb>1_ilp64 .and. nb<n ) then
              ! determine when to cross over from blocked to unblocked code
              ! (last block is always handled by unblocked code).
              nx = max( nb, stdlib_I64_ilaenv( 3_ilp64, 'DSYTRD', uplo, n, -1_ilp64, -1_ilp64, -1_ilp64 ) )
              if( nx<n ) then
                 ! determine if workspace is large enough for blocked code.
                 ldwork = n
                 iws = ldwork*nb
                 if( lwork<iws ) then
                    ! not enough workspace to use optimal nb:  determine the
                    ! minimum value of nb, and reduce nb or force use of
                    ! unblocked code by setting nx = n.
                    nb = max( lwork / ldwork, 1_ilp64 )
                    nbmin = stdlib_I64_ilaenv( 2_ilp64, 'DSYTRD', uplo, n, -1_ilp64, -1_ilp64, -1_ilp64 )
                    if( nb<nbmin )nx = n
                 end if
              else
                 nx = n
              end if
           else
              nb = 1_ilp64
           end if
           if( upper ) then
              ! reduce the upper triangle of a.
              ! columns 1:kk are handled by the unblocked method.
              kk = n - ( ( n-nx+nb-1 ) / nb )*nb
              do i = n - nb + 1, kk + 1, -nb
                 ! reduce columns i:i+nb-1 to tridiagonal form and form the
                 ! matrix w which is needed to update the unreduced part of
                 ! the matrix
                 call stdlib_I64_dlatrd( uplo, i+nb-1, nb, a, lda, e, tau, work,ldwork )
                 ! update the unreduced submatrix a(1:i-1,1:i-1), using an
                 ! update of the form:  a := a - v*w**t - w*v**t
                 call stdlib_I64_dsyr2k( uplo, 'NO TRANSPOSE', i-1, nb, -one, a( 1_ilp64, i ),lda, work, &
                           ldwork, one, a, lda )
                 ! copy superdiagonal elements back into a, and diagonal
                 ! elements into d
                 do j = i, i + nb - 1
                    a( j-1, j ) = e( j-1 )
                    d( j ) = a( j, j )
                 end do
              end do
              ! use unblocked code to reduce the last or only block
              call stdlib_I64_dsytd2( uplo, kk, a, lda, d, e, tau, iinfo )
           else
              ! reduce the lower triangle of a
              do i = 1, n - nx, nb
                 ! reduce columns i:i+nb-1 to tridiagonal form and form the
                 ! matrix w which is needed to update the unreduced part of
                 ! the matrix
                 call stdlib_I64_dlatrd( uplo, n-i+1, nb, a( i, i ), lda, e( i ),tau( i ), work, &
                           ldwork )
                 ! update the unreduced submatrix a(i+ib:n,i+ib:n), using
                 ! an update of the form:  a := a - v*w**t - w*v**t
                 call stdlib_I64_dsyr2k( uplo, 'NO TRANSPOSE', n-i-nb+1, nb, -one,a( i+nb, i ), lda, &
                           work( nb+1 ), ldwork, one,a( i+nb, i+nb ), lda )
                 ! copy subdiagonal elements back into a, and diagonal
                 ! elements into d
                 do j = i, i + nb - 1
                    a( j+1, j ) = e( j )
                    d( j ) = a( j, j )
                 end do
              end do
              ! use unblocked code to reduce the last or only block
              call stdlib_I64_dsytd2( uplo, n-i+1, a( i, i ), lda, d( i ), e( i ),tau( i ), iinfo )
                        
           end if
           work( 1_ilp64 ) = lwkopt
           return
     end subroutine stdlib_I64_dsytrd




     pure module subroutine stdlib_I64_ssytd2( uplo, n, a, lda, d, e, tau, info )
     !! SSYTD2 reduces a real symmetric matrix A to symmetric tridiagonal
     !! form T by an orthogonal similarity transformation: Q**T * A * Q = T.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, n
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: d(*), e(*), tau(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp64) :: i
           real(sp) :: alpha, taui
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp64
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -2_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -4_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'SSYTD2', -info )
              return
           end if
           ! quick return if possible
           if( n<=0 )return
           if( upper ) then
              ! reduce the upper triangle of a
              do i = n - 1, 1, -1
                 ! generate elementary reflector h(i) = i - tau * v * v**t
                 ! to annihilate a(1:i-1,i+1)
                 call stdlib_I64_slarfg( i, a( i, i+1 ), a( 1_ilp64, i+1 ), 1_ilp64, taui )
                 e( i ) = a( i, i+1 )
                 if( taui/=zero ) then
                    ! apply h(i) from both sides to a(1:i,1:i)
                    a( i, i+1 ) = one
                    ! compute  x := tau * a * v  storing x in tau(1:i)
                    call stdlib_I64_ssymv( uplo, i, taui, a, lda, a( 1_ilp64, i+1 ), 1_ilp64, zero,tau, 1_ilp64 )
                              
                    ! compute  w := x - 1/2 * tau * (x**t * v) * v
                    alpha = -half*taui*stdlib_I64_sdot( i, tau, 1_ilp64, a( 1_ilp64, i+1 ), 1_ilp64 )
                    call stdlib_I64_saxpy( i, alpha, a( 1_ilp64, i+1 ), 1_ilp64, tau, 1_ilp64 )
                    ! apply the transformation as a rank-2 update:
                       ! a := a - v * w**t - w * v**t
                    call stdlib_I64_ssyr2( uplo, i, -one, a( 1_ilp64, i+1 ), 1_ilp64, tau, 1_ilp64, a,lda )
                    a( i, i+1 ) = e( i )
                 end if
                 d( i+1 ) = a( i+1, i+1 )
                 tau( i ) = taui
              end do
              d( 1_ilp64 ) = a( 1_ilp64, 1_ilp64 )
           else
              ! reduce the lower triangle of a
              do i = 1, n - 1
                 ! generate elementary reflector h(i) = i - tau * v * v**t
                 ! to annihilate a(i+2:n,i)
                 call stdlib_I64_slarfg( n-i, a( i+1, i ), a( min( i+2, n ), i ), 1_ilp64,taui )
                 e( i ) = a( i+1, i )
                 if( taui/=zero ) then
                    ! apply h(i) from both sides to a(i+1:n,i+1:n)
                    a( i+1, i ) = one
                    ! compute  x := tau * a * v  storing y in tau(i:n-1)
                    call stdlib_I64_ssymv( uplo, n-i, taui, a( i+1, i+1 ), lda,a( i+1, i ), 1_ilp64, zero, &
                              tau( i ), 1_ilp64 )
                    ! compute  w := x - 1/2 * tau * (x**t * v) * v
                    alpha = -half*taui*stdlib_I64_sdot( n-i, tau( i ), 1_ilp64, a( i+1, i ),1_ilp64 )
                    call stdlib_I64_saxpy( n-i, alpha, a( i+1, i ), 1_ilp64, tau( i ), 1_ilp64 )
                    ! apply the transformation as a rank-2 update:
                       ! a := a - v * w**t - w * v**t
                    call stdlib_I64_ssyr2( uplo, n-i, -one, a( i+1, i ), 1_ilp64, tau( i ), 1_ilp64,a( i+1, i+1 ),&
                               lda )
                    a( i+1, i ) = e( i )
                 end if
                 d( i ) = a( i, i )
                 tau( i ) = taui
              end do
              d( n ) = a( n, n )
           end if
           return
     end subroutine stdlib_I64_ssytd2

     pure module subroutine stdlib_I64_dsytd2( uplo, n, a, lda, d, e, tau, info )
     !! DSYTD2 reduces a real symmetric matrix A to symmetric tridiagonal
     !! form T by an orthogonal similarity transformation: Q**T * A * Q = T.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, n
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: d(*), e(*), tau(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp64) :: i
           real(dp) :: alpha, taui
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp64
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -2_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -4_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'DSYTD2', -info )
              return
           end if
           ! quick return if possible
           if( n<=0 )return
           if( upper ) then
              ! reduce the upper triangle of a
              do i = n - 1, 1, -1
                 ! generate elementary reflector h(i) = i - tau * v * v**t
                 ! to annihilate a(1:i-1,i+1)
                 call stdlib_I64_dlarfg( i, a( i, i+1 ), a( 1_ilp64, i+1 ), 1_ilp64, taui )
                 e( i ) = a( i, i+1 )
                 if( taui/=zero ) then
                    ! apply h(i) from both sides to a(1:i,1:i)
                    a( i, i+1 ) = one
                    ! compute  x := tau * a * v  storing x in tau(1:i)
                    call stdlib_I64_dsymv( uplo, i, taui, a, lda, a( 1_ilp64, i+1 ), 1_ilp64, zero,tau, 1_ilp64 )
                              
                    ! compute  w := x - 1/2 * tau * (x**t * v) * v
                    alpha = -half*taui*stdlib_I64_ddot( i, tau, 1_ilp64, a( 1_ilp64, i+1 ), 1_ilp64 )
                    call stdlib_I64_daxpy( i, alpha, a( 1_ilp64, i+1 ), 1_ilp64, tau, 1_ilp64 )
                    ! apply the transformation as a rank-2 update:
                       ! a := a - v * w**t - w * v**t
                    call stdlib_I64_dsyr2( uplo, i, -one, a( 1_ilp64, i+1 ), 1_ilp64, tau, 1_ilp64, a,lda )
                    a( i, i+1 ) = e( i )
                 end if
                 d( i+1 ) = a( i+1, i+1 )
                 tau( i ) = taui
              end do
              d( 1_ilp64 ) = a( 1_ilp64, 1_ilp64 )
           else
              ! reduce the lower triangle of a
              do i = 1, n - 1
                 ! generate elementary reflector h(i) = i - tau * v * v**t
                 ! to annihilate a(i+2:n,i)
                 call stdlib_I64_dlarfg( n-i, a( i+1, i ), a( min( i+2, n ), i ), 1_ilp64,taui )
                 e( i ) = a( i+1, i )
                 if( taui/=zero ) then
                    ! apply h(i) from both sides to a(i+1:n,i+1:n)
                    a( i+1, i ) = one
                    ! compute  x := tau * a * v  storing y in tau(i:n-1)
                    call stdlib_I64_dsymv( uplo, n-i, taui, a( i+1, i+1 ), lda,a( i+1, i ), 1_ilp64, zero, &
                              tau( i ), 1_ilp64 )
                    ! compute  w := x - 1/2 * tau * (x**t * v) * v
                    alpha = -half*taui*stdlib_I64_ddot( n-i, tau( i ), 1_ilp64, a( i+1, i ),1_ilp64 )
                    call stdlib_I64_daxpy( n-i, alpha, a( i+1, i ), 1_ilp64, tau( i ), 1_ilp64 )
                    ! apply the transformation as a rank-2 update:
                       ! a := a - v * w**t - w * v**t
                    call stdlib_I64_dsyr2( uplo, n-i, -one, a( i+1, i ), 1_ilp64, tau( i ), 1_ilp64,a( i+1, i+1 ),&
                               lda )
                    a( i+1, i ) = e( i )
                 end if
                 d( i ) = a( i, i )
                 tau( i ) = taui
              end do
              d( n ) = a( n, n )
           end if
           return
     end subroutine stdlib_I64_dsytd2




     pure module subroutine stdlib_I64_sorgtr( uplo, n, a, lda, tau, work, lwork, info )
     !! SORGTR generates a real orthogonal matrix Q which is defined as the
     !! product of n-1 elementary reflectors of order N, as returned by
     !! SSYTRD:
     !! if UPLO = 'U', Q = H(n-1) . . . H(2) H(1),
     !! if UPLO = 'L', Q = H(1) H(2) . . . H(n-1).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, lwork, n
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(in) :: tau(*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery, upper
           integer(ilp64) :: i, iinfo, j, lwkopt, nb
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp64
           lquery = ( lwork==-1_ilp64 )
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -2_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -4_ilp64
           else if( lwork<max( 1_ilp64, n-1 ) .and. .not.lquery ) then
              info = -7_ilp64
           end if
           if( info==0_ilp64 ) then
              if ( upper ) then
                nb = stdlib_I64_ilaenv( 1_ilp64, 'SORGQL', ' ', n-1, n-1, n-1, -1_ilp64 )
              else
                nb = stdlib_I64_ilaenv( 1_ilp64, 'SORGQR', ' ', n-1, n-1, n-1, -1_ilp64 )
              end if
              lwkopt = max( 1_ilp64, n-1 )*nb
              work( 1_ilp64 ) = lwkopt
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'SORGTR', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0_ilp64 ) then
              work( 1_ilp64 ) = 1_ilp64
              return
           end if
           if( upper ) then
              ! q was determined by a call to stdlib_I64_ssytrd with uplo = 'u'
              ! shift the vectors which define the elementary reflectors one
              ! column to the left, and set the last row and column of q to
              ! those of the unit matrix
              do j = 1, n - 1
                 do i = 1, j - 1
                    a( i, j ) = a( i, j+1 )
                 end do
                 a( n, j ) = zero
              end do
              do i = 1, n - 1
                 a( i, n ) = zero
              end do
              a( n, n ) = one
              ! generate q(1:n-1,1:n-1)
              call stdlib_I64_sorgql( n-1, n-1, n-1, a, lda, tau, work, lwork, iinfo )
           else
              ! q was determined by a call to stdlib_I64_ssytrd with uplo = 'l'.
              ! shift the vectors which define the elementary reflectors one
              ! column to the right, and set the first row and column of q to
              ! those of the unit matrix
              do j = n, 2, -1
                 a( 1_ilp64, j ) = zero
                 do i = j + 1, n
                    a( i, j ) = a( i, j-1 )
                 end do
              end do
              a( 1_ilp64, 1_ilp64 ) = one
              do i = 2, n
                 a( i, 1_ilp64 ) = zero
              end do
              if( n>1_ilp64 ) then
                 ! generate q(2:n,2:n)
                 call stdlib_I64_sorgqr( n-1, n-1, n-1, a( 2_ilp64, 2_ilp64 ), lda, tau, work,lwork, iinfo )
                           
              end if
           end if
           work( 1_ilp64 ) = lwkopt
           return
     end subroutine stdlib_I64_sorgtr

     pure module subroutine stdlib_I64_dorgtr( uplo, n, a, lda, tau, work, lwork, info )
     !! DORGTR generates a real orthogonal matrix Q which is defined as the
     !! product of n-1 elementary reflectors of order N, as returned by
     !! DSYTRD:
     !! if UPLO = 'U', Q = H(n-1) . . . H(2) H(1),
     !! if UPLO = 'L', Q = H(1) H(2) . . . H(n-1).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, lwork, n
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(in) :: tau(*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery, upper
           integer(ilp64) :: i, iinfo, j, lwkopt, nb
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp64
           lquery = ( lwork==-1_ilp64 )
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -2_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -4_ilp64
           else if( lwork<max( 1_ilp64, n-1 ) .and. .not.lquery ) then
              info = -7_ilp64
           end if
           if( info==0_ilp64 ) then
              if( upper ) then
                 nb = stdlib_I64_ilaenv( 1_ilp64, 'DORGQL', ' ', n-1, n-1, n-1, -1_ilp64 )
              else
                 nb = stdlib_I64_ilaenv( 1_ilp64, 'DORGQR', ' ', n-1, n-1, n-1, -1_ilp64 )
              end if
              lwkopt = max( 1_ilp64, n-1 )*nb
              work( 1_ilp64 ) = lwkopt
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'DORGTR', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0_ilp64 ) then
              work( 1_ilp64 ) = 1_ilp64
              return
           end if
           if( upper ) then
              ! q was determined by a call to stdlib_I64_dsytrd with uplo = 'u'
              ! shift the vectors which define the elementary reflectors one
              ! column to the left, and set the last row and column of q to
              ! those of the unit matrix
              do j = 1, n - 1
                 do i = 1, j - 1
                    a( i, j ) = a( i, j+1 )
                 end do
                 a( n, j ) = zero
              end do
              do i = 1, n - 1
                 a( i, n ) = zero
              end do
              a( n, n ) = one
              ! generate q(1:n-1,1:n-1)
              call stdlib_I64_dorgql( n-1, n-1, n-1, a, lda, tau, work, lwork, iinfo )
           else
              ! q was determined by a call to stdlib_I64_dsytrd with uplo = 'l'.
              ! shift the vectors which define the elementary reflectors one
              ! column to the right, and set the first row and column of q to
              ! those of the unit matrix
              do j = n, 2, -1
                 a( 1_ilp64, j ) = zero
                 do i = j + 1, n
                    a( i, j ) = a( i, j-1 )
                 end do
              end do
              a( 1_ilp64, 1_ilp64 ) = one
              do i = 2, n
                 a( i, 1_ilp64 ) = zero
              end do
              if( n>1_ilp64 ) then
                 ! generate q(2:n,2:n)
                 call stdlib_I64_dorgqr( n-1, n-1, n-1, a( 2_ilp64, 2_ilp64 ), lda, tau, work,lwork, iinfo )
                           
              end if
           end if
           work( 1_ilp64 ) = lwkopt
           return
     end subroutine stdlib_I64_dorgtr




     pure module subroutine stdlib_I64_sormtr( side, uplo, trans, m, n, a, lda, tau, c, ldc,work, lwork, &
     !! SORMTR overwrites the general real M-by-N matrix C with
     !! SIDE = 'L'     SIDE = 'R'
     !! TRANS = 'N':      Q * C          C * Q
     !! TRANS = 'T':      Q**T * C       C * Q**T
     !! where Q is a real orthogonal matrix of order nq, with nq = m if
     !! SIDE = 'L' and nq = n if SIDE = 'R'. Q is defined as the product of
     !! nq-1 elementary reflectors, as returned by SSYTRD:
     !! if UPLO = 'U', Q = H(nq-1) . . . H(2) H(1);
     !! if UPLO = 'L', Q = H(1) H(2) . . . H(nq-1).
               info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side, trans, uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldc, lwork, m, n
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*), c(ldc,*)
           real(sp), intent(in) :: tau(*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: left, lquery, upper
           integer(ilp64) :: i1, i2, iinfo, lwkopt, mi, ni, nb, nq, nw
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp64
           left = stdlib_lsame( side, 'L' )
           upper = stdlib_lsame( uplo, 'U' )
           lquery = ( lwork==-1_ilp64 )
           ! nq is the order of q and nw is the minimum dimension of work
           if( left ) then
              nq = m
              nw = max( 1_ilp64, n )
           else
              nq = n
              nw = max( 1_ilp64, m )
           end if
           if( .not.left .and. .not.stdlib_lsame( side, 'R' ) ) then
              info = -1_ilp64
           else if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -2_ilp64
           else if( .not.stdlib_lsame( trans, 'N' ) .and. .not.stdlib_lsame( trans, 'T' ) )&
                     then
              info = -3_ilp64
           else if( m<0_ilp64 ) then
              info = -4_ilp64
           else if( n<0_ilp64 ) then
              info = -5_ilp64
           else if( lda<max( 1_ilp64, nq ) ) then
              info = -7_ilp64
           else if( ldc<max( 1_ilp64, m ) ) then
              info = -10_ilp64
           else if( lwork<nw .and. .not.lquery ) then
              info = -12_ilp64
           end if
           if( info==0_ilp64 ) then
              if( upper ) then
                 if( left ) then
                    nb = stdlib_I64_ilaenv( 1_ilp64, 'SORMQL', side // trans, m-1, n, m-1,-1_ilp64 )
                 else
                    nb = stdlib_I64_ilaenv( 1_ilp64, 'SORMQL', side // trans, m, n-1, n-1,-1_ilp64 )
                 end if
              else
                 if( left ) then
                    nb = stdlib_I64_ilaenv( 1_ilp64, 'SORMQR', side // trans, m-1, n, m-1,-1_ilp64 )
                 else
                    nb = stdlib_I64_ilaenv( 1_ilp64, 'SORMQR', side // trans, m, n-1, n-1,-1_ilp64 )
                 end if
              end if
              lwkopt = nw*nb
              work( 1_ilp64 ) = lwkopt
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'SORMTR', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( m==0_ilp64 .or. n==0_ilp64 .or. nq==1_ilp64 ) then
              work( 1_ilp64 ) = 1_ilp64
              return
           end if
           if( left ) then
              mi = m - 1_ilp64
              ni = n
           else
              mi = m
              ni = n - 1_ilp64
           end if
           if( upper ) then
              ! q was determined by a call to stdlib_I64_ssytrd with uplo = 'u'
              call stdlib_I64_sormql( side, trans, mi, ni, nq-1, a( 1_ilp64, 2_ilp64 ), lda, tau, c,ldc, work, &
                        lwork, iinfo )
           else
              ! q was determined by a call to stdlib_I64_ssytrd with uplo = 'l'
              if( left ) then
                 i1 = 2_ilp64
                 i2 = 1_ilp64
              else
                 i1 = 1_ilp64
                 i2 = 2_ilp64
              end if
              call stdlib_I64_sormqr( side, trans, mi, ni, nq-1, a( 2_ilp64, 1_ilp64 ), lda, tau,c( i1, i2 ), ldc,&
                         work, lwork, iinfo )
           end if
           work( 1_ilp64 ) = lwkopt
           return
     end subroutine stdlib_I64_sormtr

     pure module subroutine stdlib_I64_dormtr( side, uplo, trans, m, n, a, lda, tau, c, ldc,work, lwork, &
     !! DORMTR overwrites the general real M-by-N matrix C with
     !! SIDE = 'L'     SIDE = 'R'
     !! TRANS = 'N':      Q * C          C * Q
     !! TRANS = 'T':      Q**T * C       C * Q**T
     !! where Q is a real orthogonal matrix of order nq, with nq = m if
     !! SIDE = 'L' and nq = n if SIDE = 'R'. Q is defined as the product of
     !! nq-1 elementary reflectors, as returned by DSYTRD:
     !! if UPLO = 'U', Q = H(nq-1) . . . H(2) H(1);
     !! if UPLO = 'L', Q = H(1) H(2) . . . H(nq-1).
               info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side, trans, uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldc, lwork, m, n
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*), c(ldc,*)
           real(dp), intent(in) :: tau(*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: left, lquery, upper
           integer(ilp64) :: i1, i2, iinfo, lwkopt, mi, nb, ni, nq, nw
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp64
           left = stdlib_lsame( side, 'L' )
           upper = stdlib_lsame( uplo, 'U' )
           lquery = ( lwork==-1_ilp64 )
           ! nq is the order of q and nw is the minimum dimension of work
           if( left ) then
              nq = m
              nw = max( 1_ilp64, n )
           else
              nq = n
              nw = max( 1_ilp64, m )
           end if
           if( .not.left .and. .not.stdlib_lsame( side, 'R' ) ) then
              info = -1_ilp64
           else if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -2_ilp64
           else if( .not.stdlib_lsame( trans, 'N' ) .and. .not.stdlib_lsame( trans, 'T' ) )&
                     then
              info = -3_ilp64
           else if( m<0_ilp64 ) then
              info = -4_ilp64
           else if( n<0_ilp64 ) then
              info = -5_ilp64
           else if( lda<max( 1_ilp64, nq ) ) then
              info = -7_ilp64
           else if( ldc<max( 1_ilp64, m ) ) then
              info = -10_ilp64
           else if( lwork<nw .and. .not.lquery ) then
              info = -12_ilp64
           end if
           if( info==0_ilp64 ) then
              if( upper ) then
                 if( left ) then
                    nb = stdlib_I64_ilaenv( 1_ilp64, 'DORMQL', side // trans, m-1, n, m-1,-1_ilp64 )
                 else
                    nb = stdlib_I64_ilaenv( 1_ilp64, 'DORMQL', side // trans, m, n-1, n-1,-1_ilp64 )
                 end if
              else
                 if( left ) then
                    nb = stdlib_I64_ilaenv( 1_ilp64, 'DORMQR', side // trans, m-1, n, m-1,-1_ilp64 )
                 else
                    nb = stdlib_I64_ilaenv( 1_ilp64, 'DORMQR', side // trans, m, n-1, n-1,-1_ilp64 )
                 end if
              end if
              lwkopt = nw*nb
              work( 1_ilp64 ) = lwkopt
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'DORMTR', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( m==0_ilp64 .or. n==0_ilp64 .or. nq==1_ilp64 ) then
              work( 1_ilp64 ) = 1_ilp64
              return
           end if
           if( left ) then
              mi = m - 1_ilp64
              ni = n
           else
              mi = m
              ni = n - 1_ilp64
           end if
           if( upper ) then
              ! q was determined by a call to stdlib_I64_dsytrd with uplo = 'u'
              call stdlib_I64_dormql( side, trans, mi, ni, nq-1, a( 1_ilp64, 2_ilp64 ), lda, tau, c,ldc, work, &
                        lwork, iinfo )
           else
              ! q was determined by a call to stdlib_I64_dsytrd with uplo = 'l'
              if( left ) then
                 i1 = 2_ilp64
                 i2 = 1_ilp64
              else
                 i1 = 1_ilp64
                 i2 = 2_ilp64
              end if
              call stdlib_I64_dormqr( side, trans, mi, ni, nq-1, a( 2_ilp64, 1_ilp64 ), lda, tau,c( i1, i2 ), ldc,&
                         work, lwork, iinfo )
           end if
           work( 1_ilp64 ) = lwkopt
           return
     end subroutine stdlib_I64_dormtr




     pure module subroutine stdlib_I64_ssb2st_kernels( uplo, wantz, ttype,st, ed, sweep, n, nb, ib,a, lda, &
     !! SSB2ST_KERNELS is an internal routine used by the SSYTRD_SB2ST
     !! subroutine.
               v, tau, ldvt, work)
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           logical(lk), intent(in) :: wantz
           integer(ilp64), intent(in) :: ttype, st, ed, sweep, n, nb, ib, lda, ldvt
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: v(*), tau(*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp64) :: i, j1, j2, lm, ln, vpos, taupos, dpos, ofdpos, ajeter
           real(sp) :: ctmp
           ! Intrinsic Functions 
           ! Executable Statements 
           ajeter = ib + ldvt
           upper = stdlib_lsame( uplo, 'U' )
           if( upper ) then
               dpos    = 2_ilp64 * nb + 1_ilp64
               ofdpos  = 2_ilp64 * nb
           else
               dpos    = 1_ilp64
               ofdpos  = 2_ilp64
           endif
           ! upper case
           if( upper ) then
               if( wantz ) then
                   vpos   = mod( sweep-1, 2_ilp64 ) * n + st
                   taupos = mod( sweep-1, 2_ilp64 ) * n + st
               else
                   vpos   = mod( sweep-1, 2_ilp64 ) * n + st
                   taupos = mod( sweep-1, 2_ilp64 ) * n + st
               endif
               if( ttype==1_ilp64 ) then
                   lm = ed - st + 1_ilp64
                   v( vpos ) = one
                   do i = 1, lm-1
                       v( vpos+i )         = ( a( ofdpos-i, st+i ) )
                       a( ofdpos-i, st+i ) = zero
                   end do
                   ctmp = ( a( ofdpos, st ) )
                   call stdlib_I64_slarfg( lm, ctmp, v( vpos+1 ), 1_ilp64,tau( taupos ) )
                   a( ofdpos, st ) = ctmp
                   lm = ed - st + 1_ilp64
                   call stdlib_I64_slarfy( uplo, lm, v( vpos ), 1_ilp64,( tau( taupos ) ),a( dpos, st ), &
                             lda-1, work)
               endif
               if( ttype==3_ilp64 ) then
                   lm = ed - st + 1_ilp64
                   call stdlib_I64_slarfy( uplo, lm, v( vpos ), 1_ilp64,( tau( taupos ) ),a( dpos, st ), &
                             lda-1, work)
               endif
               if( ttype==2_ilp64 ) then
                   j1 = ed+1
                   j2 = min( ed+nb, n )
                   ln = ed-st+1
                   lm = j2-j1+1
                   if( lm>0_ilp64) then
                       call stdlib_I64_slarfx( 'LEFT', ln, lm, v( vpos ),( tau( taupos ) ),a( dpos-nb,&
                                  j1 ), lda-1, work)
                       if( wantz ) then
                           vpos   = mod( sweep-1, 2_ilp64 ) * n + j1
                           taupos = mod( sweep-1, 2_ilp64 ) * n + j1
                       else
                           vpos   = mod( sweep-1, 2_ilp64 ) * n + j1
                           taupos = mod( sweep-1, 2_ilp64 ) * n + j1
                       endif
                       v( vpos ) = one
                       do i = 1, lm-1
                           v( vpos+i )          =( a( dpos-nb-i, j1+i ) )
                           a( dpos-nb-i, j1+i ) = zero
                       end do
                       ctmp = ( a( dpos-nb, j1 ) )
                       call stdlib_I64_slarfg( lm, ctmp, v( vpos+1 ), 1_ilp64, tau( taupos ) )
                       a( dpos-nb, j1 ) = ctmp
                       call stdlib_I64_slarfx( 'RIGHT', ln-1, lm, v( vpos ),tau( taupos ),a( dpos-nb+&
                                 1_ilp64, j1 ), lda-1, work)
                   endif
               endif
           ! lower case
           else
               if( wantz ) then
                   vpos   = mod( sweep-1, 2_ilp64 ) * n + st
                   taupos = mod( sweep-1, 2_ilp64 ) * n + st
               else
                   vpos   = mod( sweep-1, 2_ilp64 ) * n + st
                   taupos = mod( sweep-1, 2_ilp64 ) * n + st
               endif
               if( ttype==1_ilp64 ) then
                   lm = ed - st + 1_ilp64
                   v( vpos ) = one
                   do i = 1, lm-1
                       v( vpos+i )         = a( ofdpos+i, st-1 )
                       a( ofdpos+i, st-1 ) = zero
                   end do
                   call stdlib_I64_slarfg( lm, a( ofdpos, st-1 ), v( vpos+1 ), 1_ilp64,tau( taupos ) )
                             
                   lm = ed - st + 1_ilp64
                   call stdlib_I64_slarfy( uplo, lm, v( vpos ), 1_ilp64,( tau( taupos ) ),a( dpos, st ), &
                             lda-1, work)
               endif
               if( ttype==3_ilp64 ) then
                   lm = ed - st + 1_ilp64
                   call stdlib_I64_slarfy( uplo, lm, v( vpos ), 1_ilp64,( tau( taupos ) ),a( dpos, st ), &
                             lda-1, work)
               endif
               if( ttype==2_ilp64 ) then
                   j1 = ed+1
                   j2 = min( ed+nb, n )
                   ln = ed-st+1
                   lm = j2-j1+1
                   if( lm>0_ilp64) then
                       call stdlib_I64_slarfx( 'RIGHT', lm, ln, v( vpos ),tau( taupos ), a( dpos+nb, &
                                 st ),lda-1, work)
                       if( wantz ) then
                           vpos   = mod( sweep-1, 2_ilp64 ) * n + j1
                           taupos = mod( sweep-1, 2_ilp64 ) * n + j1
                       else
                           vpos   = mod( sweep-1, 2_ilp64 ) * n + j1
                           taupos = mod( sweep-1, 2_ilp64 ) * n + j1
                       endif
                       v( vpos ) = one
                       do i = 1, lm-1
                           v( vpos+i )        = a( dpos+nb+i, st )
                           a( dpos+nb+i, st ) = zero
                       end do
                       call stdlib_I64_slarfg( lm, a( dpos+nb, st ), v( vpos+1 ), 1_ilp64,tau( taupos ) )
                                 
                       call stdlib_I64_slarfx( 'LEFT', lm, ln-1, v( vpos ),( tau( taupos ) ),a( dpos+&
                                 nb-1, st+1 ), lda-1, work)
                   endif
               endif
           endif
           return
     end subroutine stdlib_I64_ssb2st_kernels

     pure module subroutine stdlib_I64_dsb2st_kernels( uplo, wantz, ttype,st, ed, sweep, n, nb, ib,a, lda, &
     !! DSB2ST_KERNELS is an internal routine used by the DSYTRD_SB2ST
     !! subroutine.
               v, tau, ldvt, work)
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           logical(lk), intent(in) :: wantz
           integer(ilp64), intent(in) :: ttype, st, ed, sweep, n, nb, ib, lda, ldvt
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: v(*), tau(*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp64) :: i, j1, j2, lm, ln, vpos, taupos, dpos, ofdpos, ajeter
           real(dp) :: ctmp
           ! Intrinsic Functions 
           ! Executable Statements 
           ajeter = ib + ldvt
           upper = stdlib_lsame( uplo, 'U' )
           if( upper ) then
               dpos    = 2_ilp64 * nb + 1_ilp64
               ofdpos  = 2_ilp64 * nb
           else
               dpos    = 1_ilp64
               ofdpos  = 2_ilp64
           endif
           ! upper case
           if( upper ) then
               if( wantz ) then
                   vpos   = mod( sweep-1, 2_ilp64 ) * n + st
                   taupos = mod( sweep-1, 2_ilp64 ) * n + st
               else
                   vpos   = mod( sweep-1, 2_ilp64 ) * n + st
                   taupos = mod( sweep-1, 2_ilp64 ) * n + st
               endif
               if( ttype==1_ilp64 ) then
                   lm = ed - st + 1_ilp64
                   v( vpos ) = one
                   do i = 1, lm-1
                       v( vpos+i )         = ( a( ofdpos-i, st+i ) )
                       a( ofdpos-i, st+i ) = zero
                   end do
                   ctmp = ( a( ofdpos, st ) )
                   call stdlib_I64_dlarfg( lm, ctmp, v( vpos+1 ), 1_ilp64,tau( taupos ) )
                   a( ofdpos, st ) = ctmp
                   lm = ed - st + 1_ilp64
                   call stdlib_I64_dlarfy( uplo, lm, v( vpos ), 1_ilp64,( tau( taupos ) ),a( dpos, st ), &
                             lda-1, work)
               endif
               if( ttype==3_ilp64 ) then
                   lm = ed - st + 1_ilp64
                   call stdlib_I64_dlarfy( uplo, lm, v( vpos ), 1_ilp64,( tau( taupos ) ),a( dpos, st ), &
                             lda-1, work)
               endif
               if( ttype==2_ilp64 ) then
                   j1 = ed+1
                   j2 = min( ed+nb, n )
                   ln = ed-st+1
                   lm = j2-j1+1
                   if( lm>0_ilp64) then
                       call stdlib_I64_dlarfx( 'LEFT', ln, lm, v( vpos ),( tau( taupos ) ),a( dpos-nb,&
                                  j1 ), lda-1, work)
                       if( wantz ) then
                           vpos   = mod( sweep-1, 2_ilp64 ) * n + j1
                           taupos = mod( sweep-1, 2_ilp64 ) * n + j1
                       else
                           vpos   = mod( sweep-1, 2_ilp64 ) * n + j1
                           taupos = mod( sweep-1, 2_ilp64 ) * n + j1
                       endif
                       v( vpos ) = one
                       do i = 1, lm-1
                           v( vpos+i )          =( a( dpos-nb-i, j1+i ) )
                           a( dpos-nb-i, j1+i ) = zero
                       end do
                       ctmp = ( a( dpos-nb, j1 ) )
                       call stdlib_I64_dlarfg( lm, ctmp, v( vpos+1 ), 1_ilp64, tau( taupos ) )
                       a( dpos-nb, j1 ) = ctmp
                       call stdlib_I64_dlarfx( 'RIGHT', ln-1, lm, v( vpos ),tau( taupos ),a( dpos-nb+&
                                 1_ilp64, j1 ), lda-1, work)
                   endif
               endif
           ! lower case
           else
               if( wantz ) then
                   vpos   = mod( sweep-1, 2_ilp64 ) * n + st
                   taupos = mod( sweep-1, 2_ilp64 ) * n + st
               else
                   vpos   = mod( sweep-1, 2_ilp64 ) * n + st
                   taupos = mod( sweep-1, 2_ilp64 ) * n + st
               endif
               if( ttype==1_ilp64 ) then
                   lm = ed - st + 1_ilp64
                   v( vpos ) = one
                   do i = 1, lm-1
                       v( vpos+i )         = a( ofdpos+i, st-1 )
                       a( ofdpos+i, st-1 ) = zero
                   end do
                   call stdlib_I64_dlarfg( lm, a( ofdpos, st-1 ), v( vpos+1 ), 1_ilp64,tau( taupos ) )
                             
                   lm = ed - st + 1_ilp64
                   call stdlib_I64_dlarfy( uplo, lm, v( vpos ), 1_ilp64,( tau( taupos ) ),a( dpos, st ), &
                             lda-1, work)
               endif
               if( ttype==3_ilp64 ) then
                   lm = ed - st + 1_ilp64
                   call stdlib_I64_dlarfy( uplo, lm, v( vpos ), 1_ilp64,( tau( taupos ) ),a( dpos, st ), &
                             lda-1, work)
               endif
               if( ttype==2_ilp64 ) then
                   j1 = ed+1
                   j2 = min( ed+nb, n )
                   ln = ed-st+1
                   lm = j2-j1+1
                   if( lm>0_ilp64) then
                       call stdlib_I64_dlarfx( 'RIGHT', lm, ln, v( vpos ),tau( taupos ), a( dpos+nb, &
                                 st ),lda-1, work)
                       if( wantz ) then
                           vpos   = mod( sweep-1, 2_ilp64 ) * n + j1
                           taupos = mod( sweep-1, 2_ilp64 ) * n + j1
                       else
                           vpos   = mod( sweep-1, 2_ilp64 ) * n + j1
                           taupos = mod( sweep-1, 2_ilp64 ) * n + j1
                       endif
                       v( vpos ) = one
                       do i = 1, lm-1
                           v( vpos+i )        = a( dpos+nb+i, st )
                           a( dpos+nb+i, st ) = zero
                       end do
                       call stdlib_I64_dlarfg( lm, a( dpos+nb, st ), v( vpos+1 ), 1_ilp64,tau( taupos ) )
                                 
                       call stdlib_I64_dlarfx( 'LEFT', lm, ln-1, v( vpos ),( tau( taupos ) ),a( dpos+&
                                 nb-1, st+1 ), lda-1, work)
                   endif
               endif
           endif
           return
     end subroutine stdlib_I64_dsb2st_kernels




     module subroutine stdlib_I64_chegv( itype, jobz, uplo, n, a, lda, b, ldb, w, work,lwork, rwork, info )
     !! CHEGV computes all the eigenvalues, and optionally, the eigenvectors
     !! of a complex generalized Hermitian-definite eigenproblem, of the form
     !! A*x=(lambda)*B*x,  A*Bx=(lambda)*x,  or B*A*x=(lambda)*x.
     !! Here A and B are assumed to be Hermitian and B is also
     !! positive definite.
               
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: itype, lda, ldb, lwork, n
           ! Array Arguments 
           real(sp), intent(out) :: rwork(*), w(*)
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery, upper, wantz
           character :: trans
           integer(ilp64) :: lwkopt, nb, neig
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           upper = stdlib_lsame( uplo, 'U' )
           lquery = ( lwork== -1_ilp64 )
           info = 0_ilp64
           if( itype<1_ilp64 .or. itype>3_ilp64 ) then
              info = -1_ilp64
           else if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -2_ilp64
           else if( .not.( upper .or. stdlib_lsame( uplo, 'L' ) ) ) then
              info = -3_ilp64
           else if( n<0_ilp64 ) then
              info = -4_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -6_ilp64
           else if( ldb<max( 1_ilp64, n ) ) then
              info = -8_ilp64
           end if
           if( info==0_ilp64 ) then
              nb = stdlib_I64_ilaenv( 1_ilp64, 'CHETRD', uplo, n, -1_ilp64, -1_ilp64, -1_ilp64 )
              lwkopt = max( 1_ilp64, ( nb + 1_ilp64 )*n )
              work( 1_ilp64 ) = lwkopt
              if( lwork<max( 1_ilp64, 2_ilp64*n-1 ) .and. .not.lquery ) then
                 info = -11_ilp64
              end if
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'CHEGV ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! form a cholesky factorization of b.
           call stdlib_I64_cpotrf( uplo, n, b, ldb, info )
           if( info/=0_ilp64 ) then
              info = n + info
              return
           end if
           ! transform problem to standard eigenvalue problem and solve.
           call stdlib_I64_chegst( itype, uplo, n, a, lda, b, ldb, info )
           call stdlib_I64_cheev( jobz, uplo, n, a, lda, w, work, lwork, rwork, info )
           if( wantz ) then
              ! backtransform eigenvectors to the original problem.
              neig = n
              if( info>0_ilp64 )neig = info - 1_ilp64
              if( itype==1_ilp64 .or. itype==2_ilp64 ) then
                 ! for a*x=(lambda)*b*x and a*b*x=(lambda)*x;
                 ! backtransform eigenvectors: x = inv(l)**h*y or inv(u)*y
                 if( upper ) then
                    trans = 'N'
                 else
                    trans = 'C'
                 end if
                 call stdlib_I64_ctrsm( 'LEFT', uplo, trans, 'NON-UNIT', n, neig, cone,b, ldb, a, lda &
                           )
              else if( itype==3_ilp64 ) then
                 ! for b*a*x=(lambda)*x;
                 ! backtransform eigenvectors: x = l*y or u**h*y
                 if( upper ) then
                    trans = 'C'
                 else
                    trans = 'N'
                 end if
                 call stdlib_I64_ctrmm( 'LEFT', uplo, trans, 'NON-UNIT', n, neig, cone,b, ldb, a, lda &
                           )
              end if
           end if
           work( 1_ilp64 ) = lwkopt
           return
     end subroutine stdlib_I64_chegv

     module subroutine stdlib_I64_zhegv( itype, jobz, uplo, n, a, lda, b, ldb, w, work,lwork, rwork, info )
     !! ZHEGV computes all the eigenvalues, and optionally, the eigenvectors
     !! of a complex generalized Hermitian-definite eigenproblem, of the form
     !! A*x=(lambda)*B*x,  A*Bx=(lambda)*x,  or B*A*x=(lambda)*x.
     !! Here A and B are assumed to be Hermitian and B is also
     !! positive definite.
               
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: itype, lda, ldb, lwork, n
           ! Array Arguments 
           real(dp), intent(out) :: rwork(*), w(*)
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery, upper, wantz
           character :: trans
           integer(ilp64) :: lwkopt, nb, neig
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           upper = stdlib_lsame( uplo, 'U' )
           lquery = ( lwork==-1_ilp64 )
           info = 0_ilp64
           if( itype<1_ilp64 .or. itype>3_ilp64 ) then
              info = -1_ilp64
           else if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -2_ilp64
           else if( .not.( upper .or. stdlib_lsame( uplo, 'L' ) ) ) then
              info = -3_ilp64
           else if( n<0_ilp64 ) then
              info = -4_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -6_ilp64
           else if( ldb<max( 1_ilp64, n ) ) then
              info = -8_ilp64
           end if
           if( info==0_ilp64 ) then
              nb = stdlib_I64_ilaenv( 1_ilp64, 'ZHETRD', uplo, n, -1_ilp64, -1_ilp64, -1_ilp64 )
              lwkopt = max( 1_ilp64, ( nb + 1_ilp64 )*n )
              work( 1_ilp64 ) = lwkopt
              if( lwork<max( 1_ilp64, 2_ilp64*n - 1_ilp64 ) .and. .not.lquery ) then
                 info = -11_ilp64
              end if
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'ZHEGV ', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! form a cholesky factorization of b.
           call stdlib_I64_zpotrf( uplo, n, b, ldb, info )
           if( info/=0_ilp64 ) then
              info = n + info
              return
           end if
           ! transform problem to standard eigenvalue problem and solve.
           call stdlib_I64_zhegst( itype, uplo, n, a, lda, b, ldb, info )
           call stdlib_I64_zheev( jobz, uplo, n, a, lda, w, work, lwork, rwork, info )
           if( wantz ) then
              ! backtransform eigenvectors to the original problem.
              neig = n
              if( info>0_ilp64 )neig = info - 1_ilp64
              if( itype==1_ilp64 .or. itype==2_ilp64 ) then
                 ! for a*x=(lambda)*b*x and a*b*x=(lambda)*x;
                 ! backtransform eigenvectors: x = inv(l)**h *y or inv(u)*y
                 if( upper ) then
                    trans = 'N'
                 else
                    trans = 'C'
                 end if
                 call stdlib_I64_ztrsm( 'LEFT', uplo, trans, 'NON-UNIT', n, neig, cone,b, ldb, a, lda &
                           )
              else if( itype==3_ilp64 ) then
                 ! for b*a*x=(lambda)*x;
                 ! backtransform eigenvectors: x = l*y or u**h *y
                 if( upper ) then
                    trans = 'C'
                 else
                    trans = 'N'
                 end if
                 call stdlib_I64_ztrmm( 'LEFT', uplo, trans, 'NON-UNIT', n, neig, cone,b, ldb, a, lda &
                           )
              end if
           end if
           work( 1_ilp64 ) = lwkopt
           return
     end subroutine stdlib_I64_zhegv




     module subroutine stdlib_I64_chegvd( itype, jobz, uplo, n, a, lda, b, ldb, w, work,lwork, rwork, lrwork,&
     !! CHEGVD computes all the eigenvalues, and optionally, the eigenvectors
     !! of a complex generalized Hermitian-definite eigenproblem, of the form
     !! A*x=(lambda)*B*x,  A*Bx=(lambda)*x,  or B*A*x=(lambda)*x.  Here A and
     !! B are assumed to be Hermitian and B is also positive definite.
     !! If eigenvectors are desired, it uses a divide and conquer algorithm.
     !! The divide and conquer algorithm makes very mild assumptions about
     !! floating point arithmetic. It will work on machines with a guard
     !! digit in add/subtract, or on those binary machines without guard
     !! digits which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or
     !! Cray-2. It could conceivably fail on hexadecimal or decimal machines
     !! without guard digits, but we know of none.
                iwork, liwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: itype, lda, ldb, liwork, lrwork, lwork, n
           ! Array Arguments 
           integer(ilp64), intent(out) :: iwork(*)
           real(sp), intent(out) :: rwork(*), w(*)
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery, upper, wantz
           character :: trans
           integer(ilp64) :: liopt, liwmin, lopt, lropt, lrwmin, lwmin
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           upper = stdlib_lsame( uplo, 'U' )
           lquery = ( lwork==-1_ilp64 .or. lrwork==-1_ilp64 .or. liwork==-1_ilp64 )
           info = 0_ilp64
           if( n<=1_ilp64 ) then
              lwmin = 1_ilp64
              lrwmin = 1_ilp64
              liwmin = 1_ilp64
           else if( wantz ) then
              lwmin = 2_ilp64*n + n*n
              lrwmin = 1_ilp64 + 5_ilp64*n + 2_ilp64*n*n
              liwmin = 3_ilp64 + 5_ilp64*n
           else
              lwmin = n + 1_ilp64
              lrwmin = n
              liwmin = 1_ilp64
           end if
           lopt = lwmin
           lropt = lrwmin
           liopt = liwmin
           if( itype<1_ilp64 .or. itype>3_ilp64 ) then
              info = -1_ilp64
           else if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -2_ilp64
           else if( .not.( upper .or. stdlib_lsame( uplo, 'L' ) ) ) then
              info = -3_ilp64
           else if( n<0_ilp64 ) then
              info = -4_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -6_ilp64
           else if( ldb<max( 1_ilp64, n ) ) then
              info = -8_ilp64
           end if
           if( info==0_ilp64 ) then
              work( 1_ilp64 ) = lopt
              rwork( 1_ilp64 ) = lropt
              iwork( 1_ilp64 ) = liopt
              if( lwork<lwmin .and. .not.lquery ) then
                 info = -11_ilp64
              else if( lrwork<lrwmin .and. .not.lquery ) then
                 info = -13_ilp64
              else if( liwork<liwmin .and. .not.lquery ) then
                 info = -15_ilp64
              end if
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'CHEGVD', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! form a cholesky factorization of b.
           call stdlib_I64_cpotrf( uplo, n, b, ldb, info )
           if( info/=0_ilp64 ) then
              info = n + info
              return
           end if
           ! transform problem to standard eigenvalue problem and solve.
           call stdlib_I64_chegst( itype, uplo, n, a, lda, b, ldb, info )
           call stdlib_I64_cheevd( jobz, uplo, n, a, lda, w, work, lwork, rwork, lrwork,iwork, liwork,&
                      info )
           lopt = max( real( lopt,KIND=sp), real( work( 1_ilp64 ),KIND=sp) )
           lropt = max( real( lropt,KIND=sp), real( rwork( 1_ilp64 ),KIND=sp) )
           liopt = max( real( liopt,KIND=sp), real( iwork( 1_ilp64 ),KIND=sp) )
           if( wantz .and. info==0_ilp64 ) then
              ! backtransform eigenvectors to the original problem.
              if( itype==1_ilp64 .or. itype==2_ilp64 ) then
                 ! for a*x=(lambda)*b*x and a*b*x=(lambda)*x;
                 ! backtransform eigenvectors: x = inv(l)**h *y or inv(u)*y
                 if( upper ) then
                    trans = 'N'
                 else
                    trans = 'C'
                 end if
                 call stdlib_I64_ctrsm( 'LEFT', uplo, trans, 'NON-UNIT', n, n, cone,b, ldb, a, lda )
                           
              else if( itype==3_ilp64 ) then
                 ! for b*a*x=(lambda)*x;
                 ! backtransform eigenvectors: x = l*y or u**h *y
                 if( upper ) then
                    trans = 'C'
                 else
                    trans = 'N'
                 end if
                 call stdlib_I64_ctrmm( 'LEFT', uplo, trans, 'NON-UNIT', n, n, cone,b, ldb, a, lda )
                           
              end if
           end if
           work( 1_ilp64 ) = lopt
           rwork( 1_ilp64 ) = lropt
           iwork( 1_ilp64 ) = liopt
           return
     end subroutine stdlib_I64_chegvd

     module subroutine stdlib_I64_zhegvd( itype, jobz, uplo, n, a, lda, b, ldb, w, work,lwork, rwork, lrwork,&
     !! ZHEGVD computes all the eigenvalues, and optionally, the eigenvectors
     !! of a complex generalized Hermitian-definite eigenproblem, of the form
     !! A*x=(lambda)*B*x,  A*Bx=(lambda)*x,  or B*A*x=(lambda)*x.  Here A and
     !! B are assumed to be Hermitian and B is also positive definite.
     !! If eigenvectors are desired, it uses a divide and conquer algorithm.
     !! The divide and conquer algorithm makes very mild assumptions about
     !! floating point arithmetic. It will work on machines with a guard
     !! digit in add/subtract, or on those binary machines without guard
     !! digits which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or
     !! Cray-2. It could conceivably fail on hexadecimal or decimal machines
     !! without guard digits, but we know of none.
                iwork, liwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: itype, lda, ldb, liwork, lrwork, lwork, n
           ! Array Arguments 
           integer(ilp64), intent(out) :: iwork(*)
           real(dp), intent(out) :: rwork(*), w(*)
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery, upper, wantz
           character :: trans
           integer(ilp64) :: liopt, liwmin, lopt, lropt, lrwmin, lwmin
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           upper = stdlib_lsame( uplo, 'U' )
           lquery = ( lwork==-1_ilp64 .or. lrwork==-1_ilp64 .or. liwork==-1_ilp64 )
           info = 0_ilp64
           if( n<=1_ilp64 ) then
              lwmin = 1_ilp64
              lrwmin = 1_ilp64
              liwmin = 1_ilp64
           else if( wantz ) then
              lwmin = 2_ilp64*n + n*n
              lrwmin = 1_ilp64 + 5_ilp64*n + 2_ilp64*n*n
              liwmin = 3_ilp64 + 5_ilp64*n
           else
              lwmin = n + 1_ilp64
              lrwmin = n
              liwmin = 1_ilp64
           end if
           lopt = lwmin
           lropt = lrwmin
           liopt = liwmin
           if( itype<1_ilp64 .or. itype>3_ilp64 ) then
              info = -1_ilp64
           else if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -2_ilp64
           else if( .not.( upper .or. stdlib_lsame( uplo, 'L' ) ) ) then
              info = -3_ilp64
           else if( n<0_ilp64 ) then
              info = -4_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -6_ilp64
           else if( ldb<max( 1_ilp64, n ) ) then
              info = -8_ilp64
           end if
           if( info==0_ilp64 ) then
              work( 1_ilp64 ) = lopt
              rwork( 1_ilp64 ) = lropt
              iwork( 1_ilp64 ) = liopt
              if( lwork<lwmin .and. .not.lquery ) then
                 info = -11_ilp64
              else if( lrwork<lrwmin .and. .not.lquery ) then
                 info = -13_ilp64
              else if( liwork<liwmin .and. .not.lquery ) then
                 info = -15_ilp64
              end if
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'ZHEGVD', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! form a cholesky factorization of b.
           call stdlib_I64_zpotrf( uplo, n, b, ldb, info )
           if( info/=0_ilp64 ) then
              info = n + info
              return
           end if
           ! transform problem to standard eigenvalue problem and solve.
           call stdlib_I64_zhegst( itype, uplo, n, a, lda, b, ldb, info )
           call stdlib_I64_zheevd( jobz, uplo, n, a, lda, w, work, lwork, rwork, lrwork,iwork, liwork,&
                      info )
           lopt = max( real( lopt,KIND=dp), real( work( 1_ilp64 ),KIND=dp) )
           lropt = max( real( lropt,KIND=dp), real( rwork( 1_ilp64 ),KIND=dp) )
           liopt = max( real( liopt,KIND=dp), real( iwork( 1_ilp64 ),KIND=dp) )
           if( wantz .and. info==0_ilp64 ) then
              ! backtransform eigenvectors to the original problem.
              if( itype==1_ilp64 .or. itype==2_ilp64 ) then
                 ! for a*x=(lambda)*b*x and a*b*x=(lambda)*x;
                 ! backtransform eigenvectors: x = inv(l)**h *y or inv(u)*y
                 if( upper ) then
                    trans = 'N'
                 else
                    trans = 'C'
                 end if
                 call stdlib_I64_ztrsm( 'LEFT', uplo, trans, 'NON-UNIT', n, n, cone,b, ldb, a, lda )
                           
              else if( itype==3_ilp64 ) then
                 ! for b*a*x=(lambda)*x;
                 ! backtransform eigenvectors: x = l*y or u**h *y
                 if( upper ) then
                    trans = 'C'
                 else
                    trans = 'N'
                 end if
                 call stdlib_I64_ztrmm( 'LEFT', uplo, trans, 'NON-UNIT', n, n, cone,b, ldb, a, lda )
                           
              end if
           end if
           work( 1_ilp64 ) = lopt
           rwork( 1_ilp64 ) = lropt
           iwork( 1_ilp64 ) = liopt
           return
     end subroutine stdlib_I64_zhegvd




     module subroutine stdlib_I64_chegvx( itype, jobz, range, uplo, n, a, lda, b, ldb,vl, vu, il, iu, abstol,&
     !! CHEGVX computes selected eigenvalues, and optionally, eigenvectors
     !! of a complex generalized Hermitian-definite eigenproblem, of the form
     !! A*x=(lambda)*B*x,  A*Bx=(lambda)*x,  or B*A*x=(lambda)*x.  Here A and
     !! B are assumed to be Hermitian and B is also positive definite.
     !! Eigenvalues and eigenvectors can be selected by specifying either a
     !! range of values or a range of indices for the desired eigenvalues.
                m, w, z, ldz, work,lwork, rwork, iwork, ifail, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, range, uplo
           integer(ilp64), intent(in) :: il, itype, iu, lda, ldb, ldz, lwork, n
           integer(ilp64), intent(out) :: info, m
           real(sp), intent(in) :: abstol, vl, vu
           ! Array Arguments 
           integer(ilp64), intent(out) :: ifail(*), iwork(*)
           real(sp), intent(out) :: rwork(*), w(*)
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(sp), intent(out) :: work(*), z(ldz,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: alleig, indeig, lquery, upper, valeig, wantz
           character :: trans
           integer(ilp64) :: lwkopt, nb
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           upper = stdlib_lsame( uplo, 'U' )
           alleig = stdlib_lsame( range, 'A' )
           valeig = stdlib_lsame( range, 'V' )
           indeig = stdlib_lsame( range, 'I' )
           lquery = ( lwork==-1_ilp64 )
           info = 0_ilp64
           if( itype<1_ilp64 .or. itype>3_ilp64 ) then
              info = -1_ilp64
           else if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -2_ilp64
           else if( .not.( alleig .or. valeig .or. indeig ) ) then
              info = -3_ilp64
           else if( .not.( upper .or. stdlib_lsame( uplo, 'L' ) ) ) then
              info = -4_ilp64
           else if( n<0_ilp64 ) then
              info = -5_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -7_ilp64
           else if( ldb<max( 1_ilp64, n ) ) then
              info = -9_ilp64
           else
              if( valeig ) then
                 if( n>0_ilp64 .and. vu<=vl )info = -11_ilp64
              else if( indeig ) then
                 if( il<1_ilp64 .or. il>max( 1_ilp64, n ) ) then
                    info = -12_ilp64
                 else if( iu<min( n, il ) .or. iu>n ) then
                    info = -13_ilp64
                 end if
              end if
           end if
           if (info==0_ilp64) then
              if (ldz<1_ilp64 .or. (wantz .and. ldz<n)) then
                 info = -18_ilp64
              end if
           end if
           if( info==0_ilp64 ) then
              nb = stdlib_I64_ilaenv( 1_ilp64, 'CHETRD', uplo, n, -1_ilp64, -1_ilp64, -1_ilp64 )
              lwkopt = max( 1_ilp64, ( nb + 1_ilp64 )*n )
              work( 1_ilp64 ) = lwkopt
              if( lwork<max( 1_ilp64, 2_ilp64*n ) .and. .not.lquery ) then
                 info = -20_ilp64
              end if
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'CHEGVX', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           m = 0_ilp64
           if( n==0_ilp64 ) then
              return
           end if
           ! form a cholesky factorization of b.
           call stdlib_I64_cpotrf( uplo, n, b, ldb, info )
           if( info/=0_ilp64 ) then
              info = n + info
              return
           end if
           ! transform problem to standard eigenvalue problem and solve.
           call stdlib_I64_chegst( itype, uplo, n, a, lda, b, ldb, info )
           call stdlib_I64_cheevx( jobz, range, uplo, n, a, lda, vl, vu, il, iu, abstol,m, w, z, ldz, &
                     work, lwork, rwork, iwork, ifail,info )
           if( wantz ) then
              ! backtransform eigenvectors to the original problem.
              if( info>0_ilp64 )m = info - 1_ilp64
              if( itype==1_ilp64 .or. itype==2_ilp64 ) then
                 ! for a*x=(lambda)*b*x and a*b*x=(lambda)*x;
                 ! backtransform eigenvectors: x = inv(l)**h*y or inv(u)*y
                 if( upper ) then
                    trans = 'N'
                 else
                    trans = 'C'
                 end if
                 call stdlib_I64_ctrsm( 'LEFT', uplo, trans, 'NON-UNIT', n, m, cone, b,ldb, z, ldz )
                           
              else if( itype==3_ilp64 ) then
                 ! for b*a*x=(lambda)*x;
                 ! backtransform eigenvectors: x = l*y or u**h*y
                 if( upper ) then
                    trans = 'C'
                 else
                    trans = 'N'
                 end if
                 call stdlib_I64_ctrmm( 'LEFT', uplo, trans, 'NON-UNIT', n, m, cone, b,ldb, z, ldz )
                           
              end if
           end if
           ! set work(1) to optimal complex workspace size.
           work( 1_ilp64 ) = lwkopt
           return
     end subroutine stdlib_I64_chegvx

     module subroutine stdlib_I64_zhegvx( itype, jobz, range, uplo, n, a, lda, b, ldb,vl, vu, il, iu, abstol,&
     !! ZHEGVX computes selected eigenvalues, and optionally, eigenvectors
     !! of a complex generalized Hermitian-definite eigenproblem, of the form
     !! A*x=(lambda)*B*x,  A*Bx=(lambda)*x,  or B*A*x=(lambda)*x.  Here A and
     !! B are assumed to be Hermitian and B is also positive definite.
     !! Eigenvalues and eigenvectors can be selected by specifying either a
     !! range of values or a range of indices for the desired eigenvalues.
                m, w, z, ldz, work,lwork, rwork, iwork, ifail, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, range, uplo
           integer(ilp64), intent(in) :: il, itype, iu, lda, ldb, ldz, lwork, n
           integer(ilp64), intent(out) :: info, m
           real(dp), intent(in) :: abstol, vl, vu
           ! Array Arguments 
           integer(ilp64), intent(out) :: ifail(*), iwork(*)
           real(dp), intent(out) :: rwork(*), w(*)
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(dp), intent(out) :: work(*), z(ldz,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: alleig, indeig, lquery, upper, valeig, wantz
           character :: trans
           integer(ilp64) :: lwkopt, nb
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           upper = stdlib_lsame( uplo, 'U' )
           alleig = stdlib_lsame( range, 'A' )
           valeig = stdlib_lsame( range, 'V' )
           indeig = stdlib_lsame( range, 'I' )
           lquery = ( lwork==-1_ilp64 )
           info = 0_ilp64
           if( itype<1_ilp64 .or. itype>3_ilp64 ) then
              info = -1_ilp64
           else if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -2_ilp64
           else if( .not.( alleig .or. valeig .or. indeig ) ) then
              info = -3_ilp64
           else if( .not.( upper .or. stdlib_lsame( uplo, 'L' ) ) ) then
              info = -4_ilp64
           else if( n<0_ilp64 ) then
              info = -5_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -7_ilp64
           else if( ldb<max( 1_ilp64, n ) ) then
              info = -9_ilp64
           else
              if( valeig ) then
                 if( n>0_ilp64 .and. vu<=vl )info = -11_ilp64
              else if( indeig ) then
                 if( il<1_ilp64 .or. il>max( 1_ilp64, n ) ) then
                    info = -12_ilp64
                 else if( iu<min( n, il ) .or. iu>n ) then
                    info = -13_ilp64
                 end if
              end if
           end if
           if (info==0_ilp64) then
              if (ldz<1_ilp64 .or. (wantz .and. ldz<n)) then
                 info = -18_ilp64
              end if
           end if
           if( info==0_ilp64 ) then
              nb = stdlib_I64_ilaenv( 1_ilp64, 'ZHETRD', uplo, n, -1_ilp64, -1_ilp64, -1_ilp64 )
              lwkopt = max( 1_ilp64, ( nb + 1_ilp64 )*n )
              work( 1_ilp64 ) = lwkopt
              if( lwork<max( 1_ilp64, 2_ilp64*n ) .and. .not.lquery ) then
                 info = -20_ilp64
              end if
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'ZHEGVX', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           m = 0_ilp64
           if( n==0_ilp64 ) then
              return
           end if
           ! form a cholesky factorization of b.
           call stdlib_I64_zpotrf( uplo, n, b, ldb, info )
           if( info/=0_ilp64 ) then
              info = n + info
              return
           end if
           ! transform problem to standard eigenvalue problem and solve.
           call stdlib_I64_zhegst( itype, uplo, n, a, lda, b, ldb, info )
           call stdlib_I64_zheevx( jobz, range, uplo, n, a, lda, vl, vu, il, iu, abstol,m, w, z, ldz, &
                     work, lwork, rwork, iwork, ifail,info )
           if( wantz ) then
              ! backtransform eigenvectors to the original problem.
              if( info>0_ilp64 )m = info - 1_ilp64
              if( itype==1_ilp64 .or. itype==2_ilp64 ) then
                 ! for a*x=(lambda)*b*x and a*b*x=(lambda)*x;
                 ! backtransform eigenvectors: x = inv(l)**h *y or inv(u)*y
                 if( upper ) then
                    trans = 'N'
                 else
                    trans = 'C'
                 end if
                 call stdlib_I64_ztrsm( 'LEFT', uplo, trans, 'NON-UNIT', n, m, cone, b,ldb, z, ldz )
                           
              else if( itype==3_ilp64 ) then
                 ! for b*a*x=(lambda)*x;
                 ! backtransform eigenvectors: x = l*y or u**h *y
                 if( upper ) then
                    trans = 'C'
                 else
                    trans = 'N'
                 end if
                 call stdlib_I64_ztrmm( 'LEFT', uplo, trans, 'NON-UNIT', n, m, cone, b,ldb, z, ldz )
                           
              end if
           end if
           ! set work(1) to optimal complex workspace size.
           work( 1_ilp64 ) = lwkopt
           return
     end subroutine stdlib_I64_zhegvx




     module subroutine stdlib_I64_chpgv( itype, jobz, uplo, n, ap, bp, w, z, ldz, work,rwork, info )
     !! CHPGV computes all the eigenvalues and, optionally, the eigenvectors
     !! of a complex generalized Hermitian-definite eigenproblem, of the form
     !! A*x=(lambda)*B*x,  A*Bx=(lambda)*x,  or B*A*x=(lambda)*x.
     !! Here A and B are assumed to be Hermitian, stored in packed format,
     !! and B is also positive definite.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: itype, ldz, n
           ! Array Arguments 
           real(sp), intent(out) :: rwork(*), w(*)
           complex(sp), intent(inout) :: ap(*), bp(*)
           complex(sp), intent(out) :: work(*), z(ldz,*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: upper, wantz
           character :: trans
           integer(ilp64) :: j, neig
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           upper = stdlib_lsame( uplo, 'U' )
           info = 0_ilp64
           if( itype<1_ilp64 .or. itype>3_ilp64 ) then
              info = -1_ilp64
           else if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -2_ilp64
           else if( .not.( upper .or. stdlib_lsame( uplo, 'L' ) ) ) then
              info = -3_ilp64
           else if( n<0_ilp64 ) then
              info = -4_ilp64
           else if( ldz<1_ilp64 .or. ( wantz .and. ldz<n ) ) then
              info = -9_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'CHPGV ', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! form a cholesky factorization of b.
           call stdlib_I64_cpptrf( uplo, n, bp, info )
           if( info/=0_ilp64 ) then
              info = n + info
              return
           end if
           ! transform problem to standard eigenvalue problem and solve.
           call stdlib_I64_chpgst( itype, uplo, n, ap, bp, info )
           call stdlib_I64_chpev( jobz, uplo, n, ap, w, z, ldz, work, rwork, info )
           if( wantz ) then
              ! backtransform eigenvectors to the original problem.
              neig = n
              if( info>0_ilp64 )neig = info - 1_ilp64
              if( itype==1_ilp64 .or. itype==2_ilp64 ) then
                 ! for a*x=(lambda)*b*x and a*b*x=(lambda)*x;
                 ! backtransform eigenvectors: x = inv(l)**h*y or inv(u)*y
                 if( upper ) then
                    trans = 'N'
                 else
                    trans = 'C'
                 end if
                 do j = 1, neig
                    call stdlib_I64_ctpsv( uplo, trans, 'NON-UNIT', n, bp, z( 1_ilp64, j ),1_ilp64 )
                 end do
              else if( itype==3_ilp64 ) then
                 ! for b*a*x=(lambda)*x;
                 ! backtransform eigenvectors: x = l*y or u**h*y
                 if( upper ) then
                    trans = 'C'
                 else
                    trans = 'N'
                 end if
                 do j = 1, neig
                    call stdlib_I64_ctpmv( uplo, trans, 'NON-UNIT', n, bp, z( 1_ilp64, j ),1_ilp64 )
                 end do
              end if
           end if
           return
     end subroutine stdlib_I64_chpgv

     module subroutine stdlib_I64_zhpgv( itype, jobz, uplo, n, ap, bp, w, z, ldz, work,rwork, info )
     !! ZHPGV computes all the eigenvalues and, optionally, the eigenvectors
     !! of a complex generalized Hermitian-definite eigenproblem, of the form
     !! A*x=(lambda)*B*x,  A*Bx=(lambda)*x,  or B*A*x=(lambda)*x.
     !! Here A and B are assumed to be Hermitian, stored in packed format,
     !! and B is also positive definite.
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: itype, ldz, n
           ! Array Arguments 
           real(dp), intent(out) :: rwork(*), w(*)
           complex(dp), intent(inout) :: ap(*), bp(*)
           complex(dp), intent(out) :: work(*), z(ldz,*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: upper, wantz
           character :: trans
           integer(ilp64) :: j, neig
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           upper = stdlib_lsame( uplo, 'U' )
           info = 0_ilp64
           if( itype<1_ilp64 .or. itype>3_ilp64 ) then
              info = -1_ilp64
           else if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -2_ilp64
           else if( .not.( upper .or. stdlib_lsame( uplo, 'L' ) ) ) then
              info = -3_ilp64
           else if( n<0_ilp64 ) then
              info = -4_ilp64
           else if( ldz<1_ilp64 .or. ( wantz .and. ldz<n ) ) then
              info = -9_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'ZHPGV ', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! form a cholesky factorization of b.
           call stdlib_I64_zpptrf( uplo, n, bp, info )
           if( info/=0_ilp64 ) then
              info = n + info
              return
           end if
           ! transform problem to standard eigenvalue problem and solve.
           call stdlib_I64_zhpgst( itype, uplo, n, ap, bp, info )
           call stdlib_I64_zhpev( jobz, uplo, n, ap, w, z, ldz, work, rwork, info )
           if( wantz ) then
              ! backtransform eigenvectors to the original problem.
              neig = n
              if( info>0_ilp64 )neig = info - 1_ilp64
              if( itype==1_ilp64 .or. itype==2_ilp64 ) then
                 ! for a*x=(lambda)*b*x and a*b*x=(lambda)*x;
                 ! backtransform eigenvectors: x = inv(l)**h *y or inv(u)*y
                 if( upper ) then
                    trans = 'N'
                 else
                    trans = 'C'
                 end if
                 do j = 1, neig
                    call stdlib_I64_ztpsv( uplo, trans, 'NON-UNIT', n, bp, z( 1_ilp64, j ),1_ilp64 )
                 end do
              else if( itype==3_ilp64 ) then
                 ! for b*a*x=(lambda)*x;
                 ! backtransform eigenvectors: x = l*y or u**h *y
                 if( upper ) then
                    trans = 'C'
                 else
                    trans = 'N'
                 end if
                 do j = 1, neig
                    call stdlib_I64_ztpmv( uplo, trans, 'NON-UNIT', n, bp, z( 1_ilp64, j ),1_ilp64 )
                 end do
              end if
           end if
           return
     end subroutine stdlib_I64_zhpgv




     module subroutine stdlib_I64_chpgvd( itype, jobz, uplo, n, ap, bp, w, z, ldz, work,lwork, rwork, lrwork,&
     !! CHPGVD computes all the eigenvalues and, optionally, the eigenvectors
     !! of a complex generalized Hermitian-definite eigenproblem, of the form
     !! A*x=(lambda)*B*x,  A*Bx=(lambda)*x,  or B*A*x=(lambda)*x.  Here A and
     !! B are assumed to be Hermitian, stored in packed format, and B is also
     !! positive definite.
     !! If eigenvectors are desired, it uses a divide and conquer algorithm.
     !! The divide and conquer algorithm makes very mild assumptions about
     !! floating point arithmetic. It will work on machines with a guard
     !! digit in add/subtract, or on those binary machines without guard
     !! digits which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or
     !! Cray-2. It could conceivably fail on hexadecimal or decimal machines
     !! without guard digits, but we know of none.
                iwork, liwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: itype, ldz, liwork, lrwork, lwork, n
           ! Array Arguments 
           integer(ilp64), intent(out) :: iwork(*)
           real(sp), intent(out) :: rwork(*), w(*)
           complex(sp), intent(inout) :: ap(*), bp(*)
           complex(sp), intent(out) :: work(*), z(ldz,*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lquery, upper, wantz
           character :: trans
           integer(ilp64) :: j, liwmin, lrwmin, lwmin, neig
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           upper = stdlib_lsame( uplo, 'U' )
           lquery = ( lwork==-1_ilp64 .or. lrwork==-1_ilp64 .or. liwork==-1_ilp64 )
           info = 0_ilp64
           if( itype<1_ilp64 .or. itype>3_ilp64 ) then
              info = -1_ilp64
           else if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -2_ilp64
           else if( .not.( upper .or. stdlib_lsame( uplo, 'L' ) ) ) then
              info = -3_ilp64
           else if( n<0_ilp64 ) then
              info = -4_ilp64
           else if( ldz<1_ilp64 .or. ( wantz .and. ldz<n ) ) then
              info = -9_ilp64
           end if
           if( info==0_ilp64 ) then
              if( n<=1_ilp64 ) then
                 lwmin = 1_ilp64
                 liwmin = 1_ilp64
                 lrwmin = 1_ilp64
              else
                 if( wantz ) then
                    lwmin = 2_ilp64*n
                    lrwmin = 1_ilp64 + 5_ilp64*n + 2_ilp64*n**2_ilp64
                    liwmin = 3_ilp64 + 5_ilp64*n
                 else
                    lwmin = n
                    lrwmin = n
                    liwmin = 1_ilp64
                 end if
              end if
              work( 1_ilp64 ) = lwmin
              rwork( 1_ilp64 ) = lrwmin
              iwork( 1_ilp64 ) = liwmin
              if( lwork<lwmin .and. .not.lquery ) then
                 info = -11_ilp64
              else if( lrwork<lrwmin .and. .not.lquery ) then
                 info = -13_ilp64
              else if( liwork<liwmin .and. .not.lquery ) then
                 info = -15_ilp64
              end if
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'CHPGVD', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! form a cholesky factorization of b.
           call stdlib_I64_cpptrf( uplo, n, bp, info )
           if( info/=0_ilp64 ) then
              info = n + info
              return
           end if
           ! transform problem to standard eigenvalue problem and solve.
           call stdlib_I64_chpgst( itype, uplo, n, ap, bp, info )
           call stdlib_I64_chpevd( jobz, uplo, n, ap, w, z, ldz, work, lwork, rwork,lrwork, iwork, &
                     liwork, info )
           lwmin = max( real( lwmin,KIND=sp), real( work( 1_ilp64 ),KIND=sp) )
           lrwmin = max( real( lrwmin,KIND=sp), real( rwork( 1_ilp64 ),KIND=sp) )
           liwmin = max( real( liwmin,KIND=sp), real( iwork( 1_ilp64 ),KIND=sp) )
           if( wantz ) then
              ! backtransform eigenvectors to the original problem.
              neig = n
              if( info>0_ilp64 )neig = info - 1_ilp64
              if( itype==1_ilp64 .or. itype==2_ilp64 ) then
                 ! for a*x=(lambda)*b*x and a*b*x=(lambda)*x;
                 ! backtransform eigenvectors: x = inv(l)**h *y or inv(u)*y
                 if( upper ) then
                    trans = 'N'
                 else
                    trans = 'C'
                 end if
                 do j = 1, neig
                    call stdlib_I64_ctpsv( uplo, trans, 'NON-UNIT', n, bp, z( 1_ilp64, j ),1_ilp64 )
                 end do
              else if( itype==3_ilp64 ) then
                 ! for b*a*x=(lambda)*x;
                 ! backtransform eigenvectors: x = l*y or u**h *y
                 if( upper ) then
                    trans = 'C'
                 else
                    trans = 'N'
                 end if
                 do j = 1, neig
                    call stdlib_I64_ctpmv( uplo, trans, 'NON-UNIT', n, bp, z( 1_ilp64, j ),1_ilp64 )
                 end do
              end if
           end if
           work( 1_ilp64 ) = lwmin
           rwork( 1_ilp64 ) = lrwmin
           iwork( 1_ilp64 ) = liwmin
           return
     end subroutine stdlib_I64_chpgvd

     module subroutine stdlib_I64_zhpgvd( itype, jobz, uplo, n, ap, bp, w, z, ldz, work,lwork, rwork, lrwork,&
     !! ZHPGVD computes all the eigenvalues and, optionally, the eigenvectors
     !! of a complex generalized Hermitian-definite eigenproblem, of the form
     !! A*x=(lambda)*B*x,  A*Bx=(lambda)*x,  or B*A*x=(lambda)*x.  Here A and
     !! B are assumed to be Hermitian, stored in packed format, and B is also
     !! positive definite.
     !! If eigenvectors are desired, it uses a divide and conquer algorithm.
     !! The divide and conquer algorithm makes very mild assumptions about
     !! floating point arithmetic. It will work on machines with a guard
     !! digit in add/subtract, or on those binary machines without guard
     !! digits which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or
     !! Cray-2. It could conceivably fail on hexadecimal or decimal machines
     !! without guard digits, but we know of none.
                iwork, liwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: itype, ldz, liwork, lrwork, lwork, n
           ! Array Arguments 
           integer(ilp64), intent(out) :: iwork(*)
           real(dp), intent(out) :: rwork(*), w(*)
           complex(dp), intent(inout) :: ap(*), bp(*)
           complex(dp), intent(out) :: work(*), z(ldz,*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: lquery, upper, wantz
           character :: trans
           integer(ilp64) :: j, liwmin, lrwmin, lwmin, neig
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           upper = stdlib_lsame( uplo, 'U' )
           lquery = ( lwork==-1_ilp64 .or. lrwork==-1_ilp64 .or. liwork==-1_ilp64 )
           info = 0_ilp64
           if( itype<1_ilp64 .or. itype>3_ilp64 ) then
              info = -1_ilp64
           else if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -2_ilp64
           else if( .not.( upper .or. stdlib_lsame( uplo, 'L' ) ) ) then
              info = -3_ilp64
           else if( n<0_ilp64 ) then
              info = -4_ilp64
           else if( ldz<1_ilp64 .or. ( wantz .and. ldz<n ) ) then
              info = -9_ilp64
           end if
           if( info==0_ilp64 ) then
              if( n<=1_ilp64 ) then
                 lwmin = 1_ilp64
                 liwmin = 1_ilp64
                 lrwmin = 1_ilp64
              else
                 if( wantz ) then
                    lwmin = 2_ilp64*n
                    lrwmin = 1_ilp64 + 5_ilp64*n + 2_ilp64*n**2_ilp64
                    liwmin = 3_ilp64 + 5_ilp64*n
                 else
                    lwmin = n
                    lrwmin = n
                    liwmin = 1_ilp64
                 end if
              end if
              work( 1_ilp64 ) = lwmin
              rwork( 1_ilp64 ) = lrwmin
              iwork( 1_ilp64 ) = liwmin
              if( lwork<lwmin .and. .not.lquery ) then
                 info = -11_ilp64
              else if( lrwork<lrwmin .and. .not.lquery ) then
                 info = -13_ilp64
              else if( liwork<liwmin .and. .not.lquery ) then
                 info = -15_ilp64
              end if
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'ZHPGVD', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! form a cholesky factorization of b.
           call stdlib_I64_zpptrf( uplo, n, bp, info )
           if( info/=0_ilp64 ) then
              info = n + info
              return
           end if
           ! transform problem to standard eigenvalue problem and solve.
           call stdlib_I64_zhpgst( itype, uplo, n, ap, bp, info )
           call stdlib_I64_zhpevd( jobz, uplo, n, ap, w, z, ldz, work, lwork, rwork,lrwork, iwork, &
                     liwork, info )
           lwmin = max( real( lwmin,KIND=dp), real( work( 1_ilp64 ),KIND=dp) )
           lrwmin = max( real( lrwmin,KIND=dp), real( rwork( 1_ilp64 ),KIND=dp) )
           liwmin = max( real( liwmin,KIND=dp), real( iwork( 1_ilp64 ),KIND=dp) )
           if( wantz ) then
              ! backtransform eigenvectors to the original problem.
              neig = n
              if( info>0_ilp64 )neig = info - 1_ilp64
              if( itype==1_ilp64 .or. itype==2_ilp64 ) then
                 ! for a*x=(lambda)*b*x and a*b*x=(lambda)*x;
                 ! backtransform eigenvectors: x = inv(l)**h *y or inv(u)*y
                 if( upper ) then
                    trans = 'N'
                 else
                    trans = 'C'
                 end if
                 do j = 1, neig
                    call stdlib_I64_ztpsv( uplo, trans, 'NON-UNIT', n, bp, z( 1_ilp64, j ),1_ilp64 )
                 end do
              else if( itype==3_ilp64 ) then
                 ! for b*a*x=(lambda)*x;
                 ! backtransform eigenvectors: x = l*y or u**h *y
                 if( upper ) then
                    trans = 'C'
                 else
                    trans = 'N'
                 end if
                 do j = 1, neig
                    call stdlib_I64_ztpmv( uplo, trans, 'NON-UNIT', n, bp, z( 1_ilp64, j ),1_ilp64 )
                 end do
              end if
           end if
           work( 1_ilp64 ) = lwmin
           rwork( 1_ilp64 ) = lrwmin
           iwork( 1_ilp64 ) = liwmin
           return
     end subroutine stdlib_I64_zhpgvd




     module subroutine stdlib_I64_chpgvx( itype, jobz, range, uplo, n, ap, bp, vl, vu,il, iu, abstol, m, w, &
     !! CHPGVX computes selected eigenvalues and, optionally, eigenvectors
     !! of a complex generalized Hermitian-definite eigenproblem, of the form
     !! A*x=(lambda)*B*x,  A*Bx=(lambda)*x,  or B*A*x=(lambda)*x.  Here A and
     !! B are assumed to be Hermitian, stored in packed format, and B is also
     !! positive definite.  Eigenvalues and eigenvectors can be selected by
     !! specifying either a range of values or a range of indices for the
     !! desired eigenvalues.
               z, ldz, work, rwork,iwork, ifail, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, range, uplo
           integer(ilp64), intent(in) :: il, itype, iu, ldz, n
           integer(ilp64), intent(out) :: info, m
           real(sp), intent(in) :: abstol, vl, vu
           ! Array Arguments 
           integer(ilp64), intent(out) :: ifail(*), iwork(*)
           real(sp), intent(out) :: rwork(*), w(*)
           complex(sp), intent(inout) :: ap(*), bp(*)
           complex(sp), intent(out) :: work(*), z(ldz,*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: alleig, indeig, upper, valeig, wantz
           character :: trans
           integer(ilp64) :: j
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           upper = stdlib_lsame( uplo, 'U' )
           alleig = stdlib_lsame( range, 'A' )
           valeig = stdlib_lsame( range, 'V' )
           indeig = stdlib_lsame( range, 'I' )
           info = 0_ilp64
           if( itype<1_ilp64 .or. itype>3_ilp64 ) then
              info = -1_ilp64
           else if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -2_ilp64
           else if( .not.( alleig .or. valeig .or. indeig ) ) then
              info = -3_ilp64
           else if( .not.( upper .or. stdlib_lsame( uplo, 'L' ) ) ) then
              info = -4_ilp64
           else if( n<0_ilp64 ) then
              info = -5_ilp64
           else
              if( valeig ) then
                 if( n>0_ilp64 .and. vu<=vl ) then
                    info = -9_ilp64
                 end if
              else if( indeig ) then
                 if( il<1_ilp64 ) then
                    info = -10_ilp64
                 else if( iu<min( n, il ) .or. iu>n ) then
                    info = -11_ilp64
                 end if
              end if
           end if
           if( info==0_ilp64 ) then
              if( ldz<1_ilp64 .or. ( wantz .and. ldz<n ) ) then
                 info = -16_ilp64
              end if
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'CHPGVX', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! form a cholesky factorization of b.
           call stdlib_I64_cpptrf( uplo, n, bp, info )
           if( info/=0_ilp64 ) then
              info = n + info
              return
           end if
           ! transform problem to standard eigenvalue problem and solve.
           call stdlib_I64_chpgst( itype, uplo, n, ap, bp, info )
           call stdlib_I64_chpevx( jobz, range, uplo, n, ap, vl, vu, il, iu, abstol, m,w, z, ldz, &
                     work, rwork, iwork, ifail, info )
           if( wantz ) then
              ! backtransform eigenvectors to the original problem.
              if( info>0_ilp64 )m = info - 1_ilp64
              if( itype==1_ilp64 .or. itype==2_ilp64 ) then
                 ! for a*x=(lambda)*b*x and a*b*x=(lambda)*x;
                 ! backtransform eigenvectors: x = inv(l)**h*y or inv(u)*y
                 if( upper ) then
                    trans = 'N'
                 else
                    trans = 'C'
                 end if
                 do j = 1, m
                    call stdlib_I64_ctpsv( uplo, trans, 'NON-UNIT', n, bp, z( 1_ilp64, j ),1_ilp64 )
                 end do
              else if( itype==3_ilp64 ) then
                 ! for b*a*x=(lambda)*x;
                 ! backtransform eigenvectors: x = l*y or u**h*y
                 if( upper ) then
                    trans = 'C'
                 else
                    trans = 'N'
                 end if
                 do j = 1, m
                    call stdlib_I64_ctpmv( uplo, trans, 'NON-UNIT', n, bp, z( 1_ilp64, j ),1_ilp64 )
                 end do
              end if
           end if
           return
     end subroutine stdlib_I64_chpgvx

     module subroutine stdlib_I64_zhpgvx( itype, jobz, range, uplo, n, ap, bp, vl, vu,il, iu, abstol, m, w, &
     !! ZHPGVX computes selected eigenvalues and, optionally, eigenvectors
     !! of a complex generalized Hermitian-definite eigenproblem, of the form
     !! A*x=(lambda)*B*x,  A*Bx=(lambda)*x,  or B*A*x=(lambda)*x.  Here A and
     !! B are assumed to be Hermitian, stored in packed format, and B is also
     !! positive definite.  Eigenvalues and eigenvectors can be selected by
     !! specifying either a range of values or a range of indices for the
     !! desired eigenvalues.
               z, ldz, work, rwork,iwork, ifail, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, range, uplo
           integer(ilp64), intent(in) :: il, itype, iu, ldz, n
           integer(ilp64), intent(out) :: info, m
           real(dp), intent(in) :: abstol, vl, vu
           ! Array Arguments 
           integer(ilp64), intent(out) :: ifail(*), iwork(*)
           real(dp), intent(out) :: rwork(*), w(*)
           complex(dp), intent(inout) :: ap(*), bp(*)
           complex(dp), intent(out) :: work(*), z(ldz,*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: alleig, indeig, upper, valeig, wantz
           character :: trans
           integer(ilp64) :: j
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           upper = stdlib_lsame( uplo, 'U' )
           alleig = stdlib_lsame( range, 'A' )
           valeig = stdlib_lsame( range, 'V' )
           indeig = stdlib_lsame( range, 'I' )
           info = 0_ilp64
           if( itype<1_ilp64 .or. itype>3_ilp64 ) then
              info = -1_ilp64
           else if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -2_ilp64
           else if( .not.( alleig .or. valeig .or. indeig ) ) then
              info = -3_ilp64
           else if( .not.( upper .or. stdlib_lsame( uplo, 'L' ) ) ) then
              info = -4_ilp64
           else if( n<0_ilp64 ) then
              info = -5_ilp64
           else
              if( valeig ) then
                 if( n>0_ilp64 .and. vu<=vl ) then
                    info = -9_ilp64
                 end if
              else if( indeig ) then
                 if( il<1_ilp64 ) then
                    info = -10_ilp64
                 else if( iu<min( n, il ) .or. iu>n ) then
                    info = -11_ilp64
                 end if
              end if
           end if
           if( info==0_ilp64 ) then
              if( ldz<1_ilp64 .or. ( wantz .and. ldz<n ) ) then
                 info = -16_ilp64
              end if
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'ZHPGVX', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! form a cholesky factorization of b.
           call stdlib_I64_zpptrf( uplo, n, bp, info )
           if( info/=0_ilp64 ) then
              info = n + info
              return
           end if
           ! transform problem to standard eigenvalue problem and solve.
           call stdlib_I64_zhpgst( itype, uplo, n, ap, bp, info )
           call stdlib_I64_zhpevx( jobz, range, uplo, n, ap, vl, vu, il, iu, abstol, m,w, z, ldz, &
                     work, rwork, iwork, ifail, info )
           if( wantz ) then
              ! backtransform eigenvectors to the original problem.
              if( info>0_ilp64 )m = info - 1_ilp64
              if( itype==1_ilp64 .or. itype==2_ilp64 ) then
                 ! for a*x=(lambda)*b*x and a*b*x=(lambda)*x;
                 ! backtransform eigenvectors: x = inv(l)**h *y or inv(u)*y
                 if( upper ) then
                    trans = 'N'
                 else
                    trans = 'C'
                 end if
                 do j = 1, m
                    call stdlib_I64_ztpsv( uplo, trans, 'NON-UNIT', n, bp, z( 1_ilp64, j ),1_ilp64 )
                 end do
              else if( itype==3_ilp64 ) then
                 ! for b*a*x=(lambda)*x;
                 ! backtransform eigenvectors: x = l*y or u**h *y
                 if( upper ) then
                    trans = 'C'
                 else
                    trans = 'N'
                 end if
                 do j = 1, m
                    call stdlib_I64_ztpmv( uplo, trans, 'NON-UNIT', n, bp, z( 1_ilp64, j ),1_ilp64 )
                 end do
              end if
           end if
           return
     end subroutine stdlib_I64_zhpgvx




     pure module subroutine stdlib_I64_chbgv( jobz, uplo, n, ka, kb, ab, ldab, bb, ldbb, w, z,ldz, work, &
     !! CHBGV computes all the eigenvalues, and optionally, the eigenvectors
     !! of a complex generalized Hermitian-definite banded eigenproblem, of
     !! the form A*x=(lambda)*B*x. Here A and B are assumed to be Hermitian
     !! and banded, and B is also positive definite.
               rwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: ka, kb, ldab, ldbb, ldz, n
           ! Array Arguments 
           real(sp), intent(out) :: rwork(*), w(*)
           complex(sp), intent(inout) :: ab(ldab,*), bb(ldbb,*)
           complex(sp), intent(out) :: work(*), z(ldz,*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: upper, wantz
           character :: vect
           integer(ilp64) :: iinfo, inde, indwrk
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           upper = stdlib_lsame( uplo, 'U' )
           info = 0_ilp64
           if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -1_ilp64
           else if( .not.( upper .or. stdlib_lsame( uplo, 'L' ) ) ) then
              info = -2_ilp64
           else if( n<0_ilp64 ) then
              info = -3_ilp64
           else if( ka<0_ilp64 ) then
              info = -4_ilp64
           else if( kb<0_ilp64 .or. kb>ka ) then
              info = -5_ilp64
           else if( ldab<ka+1 ) then
              info = -7_ilp64
           else if( ldbb<kb+1 ) then
              info = -9_ilp64
           else if( ldz<1_ilp64 .or. ( wantz .and. ldz<n ) ) then
              info = -12_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'CHBGV ', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! form a split cholesky factorization of b.
           call stdlib_I64_cpbstf( uplo, n, kb, bb, ldbb, info )
           if( info/=0_ilp64 ) then
              info = n + info
              return
           end if
           ! transform problem to standard eigenvalue problem.
           inde = 1_ilp64
           indwrk = inde + n
           call stdlib_I64_chbgst( jobz, uplo, n, ka, kb, ab, ldab, bb, ldbb, z, ldz,work, rwork( &
                     indwrk ), iinfo )
           ! reduce to tridiagonal form.
           if( wantz ) then
              vect = 'U'
           else
              vect = 'N'
           end if
           call stdlib_I64_chbtrd( vect, uplo, n, ka, ab, ldab, w, rwork( inde ), z,ldz, work, iinfo )
                     
           ! for eigenvalues only, call stdlib_I64_ssterf.  for eigenvectors, call stdlib_I64_csteqr.
           if( .not.wantz ) then
              call stdlib_I64_ssterf( n, w, rwork( inde ), info )
           else
              call stdlib_I64_csteqr( jobz, n, w, rwork( inde ), z, ldz,rwork( indwrk ), info )
                        
           end if
           return
     end subroutine stdlib_I64_chbgv

     pure module subroutine stdlib_I64_zhbgv( jobz, uplo, n, ka, kb, ab, ldab, bb, ldbb, w, z,ldz, work, &
     !! ZHBGV computes all the eigenvalues, and optionally, the eigenvectors
     !! of a complex generalized Hermitian-definite banded eigenproblem, of
     !! the form A*x=(lambda)*B*x. Here A and B are assumed to be Hermitian
     !! and banded, and B is also positive definite.
               rwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: ka, kb, ldab, ldbb, ldz, n
           ! Array Arguments 
           real(dp), intent(out) :: rwork(*), w(*)
           complex(dp), intent(inout) :: ab(ldab,*), bb(ldbb,*)
           complex(dp), intent(out) :: work(*), z(ldz,*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: upper, wantz
           character :: vect
           integer(ilp64) :: iinfo, inde, indwrk
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           upper = stdlib_lsame( uplo, 'U' )
           info = 0_ilp64
           if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -1_ilp64
           else if( .not.( upper .or. stdlib_lsame( uplo, 'L' ) ) ) then
              info = -2_ilp64
           else if( n<0_ilp64 ) then
              info = -3_ilp64
           else if( ka<0_ilp64 ) then
              info = -4_ilp64
           else if( kb<0_ilp64 .or. kb>ka ) then
              info = -5_ilp64
           else if( ldab<ka+1 ) then
              info = -7_ilp64
           else if( ldbb<kb+1 ) then
              info = -9_ilp64
           else if( ldz<1_ilp64 .or. ( wantz .and. ldz<n ) ) then
              info = -12_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'ZHBGV ', -info )
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! form a split cholesky factorization of b.
           call stdlib_I64_zpbstf( uplo, n, kb, bb, ldbb, info )
           if( info/=0_ilp64 ) then
              info = n + info
              return
           end if
           ! transform problem to standard eigenvalue problem.
           inde = 1_ilp64
           indwrk = inde + n
           call stdlib_I64_zhbgst( jobz, uplo, n, ka, kb, ab, ldab, bb, ldbb, z, ldz,work, rwork( &
                     indwrk ), iinfo )
           ! reduce to tridiagonal form.
           if( wantz ) then
              vect = 'U'
           else
              vect = 'N'
           end if
           call stdlib_I64_zhbtrd( vect, uplo, n, ka, ab, ldab, w, rwork( inde ), z,ldz, work, iinfo )
                     
           ! for eigenvalues only, call stdlib_I64_dsterf.  for eigenvectors, call stdlib_I64_zsteqr.
           if( .not.wantz ) then
              call stdlib_I64_dsterf( n, w, rwork( inde ), info )
           else
              call stdlib_I64_zsteqr( jobz, n, w, rwork( inde ), z, ldz,rwork( indwrk ), info )
                        
           end if
           return
     end subroutine stdlib_I64_zhbgv




     pure module subroutine stdlib_I64_chbgvd( jobz, uplo, n, ka, kb, ab, ldab, bb, ldbb, w,z, ldz, work, &
     !! CHBGVD computes all the eigenvalues, and optionally, the eigenvectors
     !! of a complex generalized Hermitian-definite banded eigenproblem, of
     !! the form A*x=(lambda)*B*x. Here A and B are assumed to be Hermitian
     !! and banded, and B is also positive definite.  If eigenvectors are
     !! desired, it uses a divide and conquer algorithm.
     !! The divide and conquer algorithm makes very mild assumptions about
     !! floating point arithmetic. It will work on machines with a guard
     !! digit in add/subtract, or on those binary machines without guard
     !! digits which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or
     !! Cray-2. It could conceivably fail on hexadecimal or decimal machines
     !! without guard digits, but we know of none.
               lwork, rwork, lrwork, iwork,liwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: ka, kb, ldab, ldbb, ldz, liwork, lrwork, lwork, n
           ! Array Arguments 
           integer(ilp64), intent(out) :: iwork(*)
           real(sp), intent(out) :: rwork(*), w(*)
           complex(sp), intent(inout) :: ab(ldab,*), bb(ldbb,*)
           complex(sp), intent(out) :: work(*), z(ldz,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery, upper, wantz
           character :: vect
           integer(ilp64) :: iinfo, inde, indwk2, indwrk, liwmin, llrwk, llwk2, lrwmin, &
                     lwmin
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           upper = stdlib_lsame( uplo, 'U' )
           lquery = ( lwork==-1_ilp64 .or. lrwork==-1_ilp64 .or. liwork==-1_ilp64 )
           info = 0_ilp64
           if( n<=1_ilp64 ) then
              lwmin = 1_ilp64+n
              lrwmin = 1_ilp64+n
              liwmin = 1_ilp64
           else if( wantz ) then
              lwmin = 2_ilp64*n**2_ilp64
              lrwmin = 1_ilp64 + 5_ilp64*n + 2_ilp64*n**2_ilp64
              liwmin = 3_ilp64 + 5_ilp64*n
           else
              lwmin = n
              lrwmin = n
              liwmin = 1_ilp64
           end if
           if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -1_ilp64
           else if( .not.( upper .or. stdlib_lsame( uplo, 'L' ) ) ) then
              info = -2_ilp64
           else if( n<0_ilp64 ) then
              info = -3_ilp64
           else if( ka<0_ilp64 ) then
              info = -4_ilp64
           else if( kb<0_ilp64 .or. kb>ka ) then
              info = -5_ilp64
           else if( ldab<ka+1 ) then
              info = -7_ilp64
           else if( ldbb<kb+1 ) then
              info = -9_ilp64
           else if( ldz<1_ilp64 .or. ( wantz .and. ldz<n ) ) then
              info = -12_ilp64
           end if
           if( info==0_ilp64 ) then
              work( 1_ilp64 ) = lwmin
              rwork( 1_ilp64 ) = lrwmin
              iwork( 1_ilp64 ) = liwmin
              if( lwork<lwmin .and. .not.lquery ) then
                 info = -14_ilp64
              else if( lrwork<lrwmin .and. .not.lquery ) then
                 info = -16_ilp64
              else if( liwork<liwmin .and. .not.lquery ) then
                 info = -18_ilp64
              end if
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'CHBGVD', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! form a split cholesky factorization of b.
           call stdlib_I64_cpbstf( uplo, n, kb, bb, ldbb, info )
           if( info/=0_ilp64 ) then
              info = n + info
              return
           end if
           ! transform problem to standard eigenvalue problem.
           inde = 1_ilp64
           indwrk = inde + n
           indwk2 = 1_ilp64 + n*n
           llwk2 = lwork - indwk2 + 2_ilp64
           llrwk = lrwork - indwrk + 2_ilp64
           call stdlib_I64_chbgst( jobz, uplo, n, ka, kb, ab, ldab, bb, ldbb, z, ldz,work, rwork, &
                     iinfo )
           ! reduce hermitian band matrix to tridiagonal form.
           if( wantz ) then
              vect = 'U'
           else
              vect = 'N'
           end if
           call stdlib_I64_chbtrd( vect, uplo, n, ka, ab, ldab, w, rwork( inde ), z,ldz, work, iinfo )
                     
           ! for eigenvalues only, call stdlib_I64_ssterf.  for eigenvectors, call stdlib_I64_cstedc.
           if( .not.wantz ) then
              call stdlib_I64_ssterf( n, w, rwork( inde ), info )
           else
              call stdlib_I64_cstedc( 'I', n, w, rwork( inde ), work, n, work( indwk2 ),llwk2, rwork( &
                        indwrk ), llrwk, iwork, liwork,info )
              call stdlib_I64_cgemm( 'N', 'N', n, n, n, cone, z, ldz, work, n, czero,work( indwk2 ), &
                        n )
              call stdlib_I64_clacpy( 'A', n, n, work( indwk2 ), n, z, ldz )
           end if
           work( 1_ilp64 ) = lwmin
           rwork( 1_ilp64 ) = lrwmin
           iwork( 1_ilp64 ) = liwmin
           return
     end subroutine stdlib_I64_chbgvd

     pure module subroutine stdlib_I64_zhbgvd( jobz, uplo, n, ka, kb, ab, ldab, bb, ldbb, w,z, ldz, work, &
     !! ZHBGVD computes all the eigenvalues, and optionally, the eigenvectors
     !! of a complex generalized Hermitian-definite banded eigenproblem, of
     !! the form A*x=(lambda)*B*x. Here A and B are assumed to be Hermitian
     !! and banded, and B is also positive definite.  If eigenvectors are
     !! desired, it uses a divide and conquer algorithm.
     !! The divide and conquer algorithm makes very mild assumptions about
     !! floating point arithmetic. It will work on machines with a guard
     !! digit in add/subtract, or on those binary machines without guard
     !! digits which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or
     !! Cray-2. It could conceivably fail on hexadecimal or decimal machines
     !! without guard digits, but we know of none.
               lwork, rwork, lrwork, iwork,liwork, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: ka, kb, ldab, ldbb, ldz, liwork, lrwork, lwork, n
           ! Array Arguments 
           integer(ilp64), intent(out) :: iwork(*)
           real(dp), intent(out) :: rwork(*), w(*)
           complex(dp), intent(inout) :: ab(ldab,*), bb(ldbb,*)
           complex(dp), intent(out) :: work(*), z(ldz,*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery, upper, wantz
           character :: vect
           integer(ilp64) :: iinfo, inde, indwk2, indwrk, liwmin, llrwk, llwk2, lrwmin, &
                     lwmin
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           upper = stdlib_lsame( uplo, 'U' )
           lquery = ( lwork==-1_ilp64 .or. lrwork==-1_ilp64 .or. liwork==-1_ilp64 )
           info = 0_ilp64
           if( n<=1_ilp64 ) then
              lwmin = 1_ilp64+n
              lrwmin = 1_ilp64+n
              liwmin = 1_ilp64
           else if( wantz ) then
              lwmin = 2_ilp64*n**2_ilp64
              lrwmin = 1_ilp64 + 5_ilp64*n + 2_ilp64*n**2_ilp64
              liwmin = 3_ilp64 + 5_ilp64*n
           else
              lwmin = n
              lrwmin = n
              liwmin = 1_ilp64
           end if
           if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -1_ilp64
           else if( .not.( upper .or. stdlib_lsame( uplo, 'L' ) ) ) then
              info = -2_ilp64
           else if( n<0_ilp64 ) then
              info = -3_ilp64
           else if( ka<0_ilp64 ) then
              info = -4_ilp64
           else if( kb<0_ilp64 .or. kb>ka ) then
              info = -5_ilp64
           else if( ldab<ka+1 ) then
              info = -7_ilp64
           else if( ldbb<kb+1 ) then
              info = -9_ilp64
           else if( ldz<1_ilp64 .or. ( wantz .and. ldz<n ) ) then
              info = -12_ilp64
           end if
           if( info==0_ilp64 ) then
              work( 1_ilp64 ) = lwmin
              rwork( 1_ilp64 ) = lrwmin
              iwork( 1_ilp64 ) = liwmin
              if( lwork<lwmin .and. .not.lquery ) then
                 info = -14_ilp64
              else if( lrwork<lrwmin .and. .not.lquery ) then
                 info = -16_ilp64
              else if( liwork<liwmin .and. .not.lquery ) then
                 info = -18_ilp64
              end if
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'ZHBGVD', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0 )return
           ! form a split cholesky factorization of b.
           call stdlib_I64_zpbstf( uplo, n, kb, bb, ldbb, info )
           if( info/=0_ilp64 ) then
              info = n + info
              return
           end if
           ! transform problem to standard eigenvalue problem.
           inde = 1_ilp64
           indwrk = inde + n
           indwk2 = 1_ilp64 + n*n
           llwk2 = lwork - indwk2 + 2_ilp64
           llrwk = lrwork - indwrk + 2_ilp64
           call stdlib_I64_zhbgst( jobz, uplo, n, ka, kb, ab, ldab, bb, ldbb, z, ldz,work, rwork, &
                     iinfo )
           ! reduce hermitian band matrix to tridiagonal form.
           if( wantz ) then
              vect = 'U'
           else
              vect = 'N'
           end if
           call stdlib_I64_zhbtrd( vect, uplo, n, ka, ab, ldab, w, rwork( inde ), z,ldz, work, iinfo )
                     
           ! for eigenvalues only, call stdlib_I64_dsterf.  for eigenvectors, call stdlib_I64_zstedc.
           if( .not.wantz ) then
              call stdlib_I64_dsterf( n, w, rwork( inde ), info )
           else
              call stdlib_I64_zstedc( 'I', n, w, rwork( inde ), work, n, work( indwk2 ),llwk2, rwork( &
                        indwrk ), llrwk, iwork, liwork,info )
              call stdlib_I64_zgemm( 'N', 'N', n, n, n, cone, z, ldz, work, n, czero,work( indwk2 ), &
                        n )
              call stdlib_I64_zlacpy( 'A', n, n, work( indwk2 ), n, z, ldz )
           end if
           work( 1_ilp64 ) = lwmin
           rwork( 1_ilp64 ) = lrwmin
           iwork( 1_ilp64 ) = liwmin
           return
     end subroutine stdlib_I64_zhbgvd




     pure module subroutine stdlib_I64_chbgvx( jobz, range, uplo, n, ka, kb, ab, ldab, bb,ldbb, q, ldq, vl, &
     !! CHBGVX computes all the eigenvalues, and optionally, the eigenvectors
     !! of a complex generalized Hermitian-definite banded eigenproblem, of
     !! the form A*x=(lambda)*B*x. Here A and B are assumed to be Hermitian
     !! and banded, and B is also positive definite.  Eigenvalues and
     !! eigenvectors can be selected by specifying either all eigenvalues,
     !! a range of values or a range of indices for the desired eigenvalues.
               vu, il, iu, abstol, m, w, z,ldz, work, rwork, iwork, ifail, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, range, uplo
           integer(ilp64), intent(in) :: il, iu, ka, kb, ldab, ldbb, ldq, ldz, n
           integer(ilp64), intent(out) :: info, m
           real(sp), intent(in) :: abstol, vl, vu
           ! Array Arguments 
           integer(ilp64), intent(out) :: ifail(*), iwork(*)
           real(sp), intent(out) :: rwork(*), w(*)
           complex(sp), intent(inout) :: ab(ldab,*), bb(ldbb,*)
           complex(sp), intent(out) :: q(ldq,*), work(*), z(ldz,*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: alleig, indeig, test, upper, valeig, wantz
           character :: order, vect
           integer(ilp64) :: i, iinfo, indd, inde, indee, indibl, indisp, indiwk, indrwk, indwrk, &
                     itmp1, j, jj, nsplit
           real(sp) :: tmp1
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           upper = stdlib_lsame( uplo, 'U' )
           alleig = stdlib_lsame( range, 'A' )
           valeig = stdlib_lsame( range, 'V' )
           indeig = stdlib_lsame( range, 'I' )
           info = 0_ilp64
           if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -1_ilp64
           else if( .not.( alleig .or. valeig .or. indeig ) ) then
              info = -2_ilp64
           else if( .not.( upper .or. stdlib_lsame( uplo, 'L' ) ) ) then
              info = -3_ilp64
           else if( n<0_ilp64 ) then
              info = -4_ilp64
           else if( ka<0_ilp64 ) then
              info = -5_ilp64
           else if( kb<0_ilp64 .or. kb>ka ) then
              info = -6_ilp64
           else if( ldab<ka+1 ) then
              info = -8_ilp64
           else if( ldbb<kb+1 ) then
              info = -10_ilp64
           else if( ldq<1_ilp64 .or. ( wantz .and. ldq<n ) ) then
              info = -12_ilp64
           else
              if( valeig ) then
                 if( n>0_ilp64 .and. vu<=vl )info = -14_ilp64
              else if( indeig ) then
                 if( il<1_ilp64 .or. il>max( 1_ilp64, n ) ) then
                    info = -15_ilp64
                 else if ( iu<min( n, il ) .or. iu>n ) then
                    info = -16_ilp64
                 end if
              end if
           end if
           if( info==0_ilp64) then
              if( ldz<1_ilp64 .or. ( wantz .and. ldz<n ) ) then
                 info = -21_ilp64
              end if
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'CHBGVX', -info )
              return
           end if
           ! quick return if possible
           m = 0_ilp64
           if( n==0 )return
           ! form a split cholesky factorization of b.
           call stdlib_I64_cpbstf( uplo, n, kb, bb, ldbb, info )
           if( info/=0_ilp64 ) then
              info = n + info
              return
           end if
           ! transform problem to standard eigenvalue problem.
           call stdlib_I64_chbgst( jobz, uplo, n, ka, kb, ab, ldab, bb, ldbb, q, ldq,work, rwork, &
                     iinfo )
           ! solve the standard eigenvalue problem.
           ! reduce hermitian band matrix to tridiagonal form.
           indd = 1_ilp64
           inde = indd + n
           indrwk = inde + n
           indwrk = 1_ilp64
           if( wantz ) then
              vect = 'U'
           else
              vect = 'N'
           end if
           call stdlib_I64_chbtrd( vect, uplo, n, ka, ab, ldab, rwork( indd ),rwork( inde ), q, ldq, &
                     work( indwrk ), iinfo )
           ! if all eigenvalues are desired and abstol is less than or equal
           ! to zero, then call stdlib_I64_ssterf or stdlib_I64_csteqr.  if this fails for some
           ! eigenvalue, then try stdlib_I64_sstebz.
           test = .false.
           if( indeig ) then
              if( il==1_ilp64 .and. iu==n ) then
                 test = .true.
              end if
           end if
           if( ( alleig .or. test ) .and. ( abstol<=zero ) ) then
              call stdlib_I64_scopy( n, rwork( indd ), 1_ilp64, w, 1_ilp64 )
              indee = indrwk + 2_ilp64*n
              call stdlib_I64_scopy( n-1, rwork( inde ), 1_ilp64, rwork( indee ), 1_ilp64 )
              if( .not.wantz ) then
                 call stdlib_I64_ssterf( n, w, rwork( indee ), info )
              else
                 call stdlib_I64_clacpy( 'A', n, n, q, ldq, z, ldz )
                 call stdlib_I64_csteqr( jobz, n, w, rwork( indee ), z, ldz,rwork( indrwk ), info )
                           
                 if( info==0_ilp64 ) then
                    do i = 1, n
                       ifail( i ) = 0_ilp64
                    end do
                 end if
              end if
              if( info==0_ilp64 ) then
                 m = n
                 go to 30
              end if
              info = 0_ilp64
           end if
           ! otherwise, call stdlib_I64_sstebz and, if eigenvectors are desired,
           ! call stdlib_I64_cstein.
           if( wantz ) then
              order = 'B'
           else
              order = 'E'
           end if
           indibl = 1_ilp64
           indisp = indibl + n
           indiwk = indisp + n
           call stdlib_I64_sstebz( range, order, n, vl, vu, il, iu, abstol,rwork( indd ), rwork( inde &
           ), m, nsplit, w,iwork( indibl ), iwork( indisp ), rwork( indrwk ),iwork( indiwk ), &
                     info )
           if( wantz ) then
              call stdlib_I64_cstein( n, rwork( indd ), rwork( inde ), m, w,iwork( indibl ), iwork( &
                        indisp ), z, ldz,rwork( indrwk ), iwork( indiwk ), ifail, info )
              ! apply unitary matrix used in reduction to tridiagonal
              ! form to eigenvectors returned by stdlib_I64_cstein.
              do j = 1, m
                 call stdlib_I64_ccopy( n, z( 1_ilp64, j ), 1_ilp64, work( 1_ilp64 ), 1_ilp64 )
                 call stdlib_I64_cgemv( 'N', n, n, cone, q, ldq, work, 1_ilp64, czero,z( 1_ilp64, j ), 1_ilp64 )
              end do
           end if
           30 continue
           ! if eigenvalues are not in order, then sort them, along with
           ! eigenvectors.
           if( wantz ) then
              do j = 1, m - 1
                 i = 0_ilp64
                 tmp1 = w( j )
                 do jj = j + 1, m
                    if( w( jj )<tmp1 ) then
                       i = jj
                       tmp1 = w( jj )
                    end if
                 end do
                 if( i/=0_ilp64 ) then
                    itmp1 = iwork( indibl+i-1 )
                    w( i ) = w( j )
                    iwork( indibl+i-1 ) = iwork( indibl+j-1 )
                    w( j ) = tmp1
                    iwork( indibl+j-1 ) = itmp1
                    call stdlib_I64_cswap( n, z( 1_ilp64, i ), 1_ilp64, z( 1_ilp64, j ), 1_ilp64 )
                    if( info/=0_ilp64 ) then
                       itmp1 = ifail( i )
                       ifail( i ) = ifail( j )
                       ifail( j ) = itmp1
                    end if
                 end if
              end do
           end if
           return
     end subroutine stdlib_I64_chbgvx

     pure module subroutine stdlib_I64_zhbgvx( jobz, range, uplo, n, ka, kb, ab, ldab, bb,ldbb, q, ldq, vl, &
     !! ZHBGVX computes all the eigenvalues, and optionally, the eigenvectors
     !! of a complex generalized Hermitian-definite banded eigenproblem, of
     !! the form A*x=(lambda)*B*x. Here A and B are assumed to be Hermitian
     !! and banded, and B is also positive definite.  Eigenvalues and
     !! eigenvectors can be selected by specifying either all eigenvalues,
     !! a range of values or a range of indices for the desired eigenvalues.
               vu, il, iu, abstol, m, w, z,ldz, work, rwork, iwork, ifail, info )
        ! -- lapack driver routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobz, range, uplo
           integer(ilp64), intent(in) :: il, iu, ka, kb, ldab, ldbb, ldq, ldz, n
           integer(ilp64), intent(out) :: info, m
           real(dp), intent(in) :: abstol, vl, vu
           ! Array Arguments 
           integer(ilp64), intent(out) :: ifail(*), iwork(*)
           real(dp), intent(out) :: rwork(*), w(*)
           complex(dp), intent(inout) :: ab(ldab,*), bb(ldbb,*)
           complex(dp), intent(out) :: q(ldq,*), work(*), z(ldz,*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: alleig, indeig, test, upper, valeig, wantz
           character :: order, vect
           integer(ilp64) :: i, iinfo, indd, inde, indee, indibl, indisp, indiwk, indrwk, indwrk, &
                     itmp1, j, jj, nsplit
           real(dp) :: tmp1
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           wantz = stdlib_lsame( jobz, 'V' )
           upper = stdlib_lsame( uplo, 'U' )
           alleig = stdlib_lsame( range, 'A' )
           valeig = stdlib_lsame( range, 'V' )
           indeig = stdlib_lsame( range, 'I' )
           info = 0_ilp64
           if( .not.( wantz .or. stdlib_lsame( jobz, 'N' ) ) ) then
              info = -1_ilp64
           else if( .not.( alleig .or. valeig .or. indeig ) ) then
              info = -2_ilp64
           else if( .not.( upper .or. stdlib_lsame( uplo, 'L' ) ) ) then
              info = -3_ilp64
           else if( n<0_ilp64 ) then
              info = -4_ilp64
           else if( ka<0_ilp64 ) then
              info = -5_ilp64
           else if( kb<0_ilp64 .or. kb>ka ) then
              info = -6_ilp64
           else if( ldab<ka+1 ) then
              info = -8_ilp64
           else if( ldbb<kb+1 ) then
              info = -10_ilp64
           else if( ldq<1_ilp64 .or. ( wantz .and. ldq<n ) ) then
              info = -12_ilp64
           else
              if( valeig ) then
                 if( n>0_ilp64 .and. vu<=vl )info = -14_ilp64
              else if( indeig ) then
                 if( il<1_ilp64 .or. il>max( 1_ilp64, n ) ) then
                    info = -15_ilp64
                 else if ( iu<min( n, il ) .or. iu>n ) then
                    info = -16_ilp64
                 end if
              end if
           end if
           if( info==0_ilp64) then
              if( ldz<1_ilp64 .or. ( wantz .and. ldz<n ) ) then
                 info = -21_ilp64
              end if
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'ZHBGVX', -info )
              return
           end if
           ! quick return if possible
           m = 0_ilp64
           if( n==0 )return
           ! form a split cholesky factorization of b.
           call stdlib_I64_zpbstf( uplo, n, kb, bb, ldbb, info )
           if( info/=0_ilp64 ) then
              info = n + info
              return
           end if
           ! transform problem to standard eigenvalue problem.
           call stdlib_I64_zhbgst( jobz, uplo, n, ka, kb, ab, ldab, bb, ldbb, q, ldq,work, rwork, &
                     iinfo )
           ! solve the standard eigenvalue problem.
           ! reduce hermitian band matrix to tridiagonal form.
           indd = 1_ilp64
           inde = indd + n
           indrwk = inde + n
           indwrk = 1_ilp64
           if( wantz ) then
              vect = 'U'
           else
              vect = 'N'
           end if
           call stdlib_I64_zhbtrd( vect, uplo, n, ka, ab, ldab, rwork( indd ),rwork( inde ), q, ldq, &
                     work( indwrk ), iinfo )
           ! if all eigenvalues are desired and abstol is less than or equal
           ! to zero, then call stdlib_I64_dsterf or stdlib_I64_zsteqr.  if this fails for some
           ! eigenvalue, then try stdlib_I64_dstebz.
           test = .false.
           if( indeig ) then
              if( il==1_ilp64 .and. iu==n ) then
                 test = .true.
              end if
           end if
           if( ( alleig .or. test ) .and. ( abstol<=zero ) ) then
              call stdlib_I64_dcopy( n, rwork( indd ), 1_ilp64, w, 1_ilp64 )
              indee = indrwk + 2_ilp64*n
              call stdlib_I64_dcopy( n-1, rwork( inde ), 1_ilp64, rwork( indee ), 1_ilp64 )
              if( .not.wantz ) then
                 call stdlib_I64_dsterf( n, w, rwork( indee ), info )
              else
                 call stdlib_I64_zlacpy( 'A', n, n, q, ldq, z, ldz )
                 call stdlib_I64_zsteqr( jobz, n, w, rwork( indee ), z, ldz,rwork( indrwk ), info )
                           
                 if( info==0_ilp64 ) then
                    do i = 1, n
                       ifail( i ) = 0_ilp64
                    end do
                 end if
              end if
              if( info==0_ilp64 ) then
                 m = n
                 go to 30
              end if
              info = 0_ilp64
           end if
           ! otherwise, call stdlib_I64_dstebz and, if eigenvectors are desired,
           ! call stdlib_I64_zstein.
           if( wantz ) then
              order = 'B'
           else
              order = 'E'
           end if
           indibl = 1_ilp64
           indisp = indibl + n
           indiwk = indisp + n
           call stdlib_I64_dstebz( range, order, n, vl, vu, il, iu, abstol,rwork( indd ), rwork( inde &
           ), m, nsplit, w,iwork( indibl ), iwork( indisp ), rwork( indrwk ),iwork( indiwk ), &
                     info )
           if( wantz ) then
              call stdlib_I64_zstein( n, rwork( indd ), rwork( inde ), m, w,iwork( indibl ), iwork( &
                        indisp ), z, ldz,rwork( indrwk ), iwork( indiwk ), ifail, info )
              ! apply unitary matrix used in reduction to tridiagonal
              ! form to eigenvectors returned by stdlib_I64_zstein.
              do j = 1, m
                 call stdlib_I64_zcopy( n, z( 1_ilp64, j ), 1_ilp64, work( 1_ilp64 ), 1_ilp64 )
                 call stdlib_I64_zgemv( 'N', n, n, cone, q, ldq, work, 1_ilp64, czero,z( 1_ilp64, j ), 1_ilp64 )
              end do
           end if
           30 continue
           ! if eigenvalues are not in order, then sort them, along with
           ! eigenvectors.
           if( wantz ) then
              do j = 1, m - 1
                 i = 0_ilp64
                 tmp1 = w( j )
                 do jj = j + 1, m
                    if( w( jj )<tmp1 ) then
                       i = jj
                       tmp1 = w( jj )
                    end if
                 end do
                 if( i/=0_ilp64 ) then
                    itmp1 = iwork( indibl+i-1 )
                    w( i ) = w( j )
                    iwork( indibl+i-1 ) = iwork( indibl+j-1 )
                    w( j ) = tmp1
                    iwork( indibl+j-1 ) = itmp1
                    call stdlib_I64_zswap( n, z( 1_ilp64, i ), 1_ilp64, z( 1_ilp64, j ), 1_ilp64 )
                    if( info/=0_ilp64 ) then
                       itmp1 = ifail( i )
                       ifail( i ) = ifail( j )
                       ifail( j ) = itmp1
                    end if
                 end if
              end do
           end if
           return
     end subroutine stdlib_I64_zhbgvx




     pure module subroutine stdlib_I64_chetrd( uplo, n, a, lda, d, e, tau, work, lwork, info )
     !! CHETRD reduces a complex Hermitian matrix A to real symmetric
     !! tridiagonal form T by a unitary similarity transformation:
     !! Q**H * A * Q = T.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, lwork, n
           ! Array Arguments 
           real(sp), intent(out) :: d(*), e(*)
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: tau(*), work(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: lquery, upper
           integer(ilp64) :: i, iinfo, iws, j, kk, ldwork, lwkopt, nb, nbmin, nx
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp64
           upper = stdlib_lsame( uplo, 'U' )
           lquery = ( lwork==-1_ilp64 )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -2_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -4_ilp64
           else if( lwork<1_ilp64 .and. .not.lquery ) then
              info = -9_ilp64
           end if
           if( info==0_ilp64 ) then
              ! determine the block size.
              nb = stdlib_I64_ilaenv( 1_ilp64, 'CHETRD', uplo, n, -1_ilp64, -1_ilp64, -1_ilp64 )
              lwkopt = n*nb
              work( 1_ilp64 ) = lwkopt
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'CHETRD', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0_ilp64 ) then
              work( 1_ilp64 ) = 1_ilp64
              return
           end if
           nx = n
           iws = 1_ilp64
           if( nb>1_ilp64 .and. nb<n ) then
              ! determine when to cross over from blocked to unblocked code
              ! (last block is always handled by unblocked code).
              nx = max( nb, stdlib_I64_ilaenv( 3_ilp64, 'CHETRD', uplo, n, -1_ilp64, -1_ilp64, -1_ilp64 ) )
              if( nx<n ) then
                 ! determine if workspace is large enough for blocked code.
                 ldwork = n
                 iws = ldwork*nb
                 if( lwork<iws ) then
                    ! not enough workspace to use optimal nb:  determine the
                    ! minimum value of nb, and reduce nb or force use of
                    ! unblocked code by setting nx = n.
                    nb = max( lwork / ldwork, 1_ilp64 )
                    nbmin = stdlib_I64_ilaenv( 2_ilp64, 'CHETRD', uplo, n, -1_ilp64, -1_ilp64, -1_ilp64 )
                    if( nb<nbmin )nx = n
                 end if
              else
                 nx = n
              end if
           else
              nb = 1_ilp64
           end if
           if( upper ) then
              ! reduce the upper triangle of a.
              ! columns 1:kk are handled by the unblocked method.
              kk = n - ( ( n-nx+nb-1 ) / nb )*nb
              do i = n - nb + 1, kk + 1, -nb
                 ! reduce columns i:i+nb-1 to tridiagonal form and form the
                 ! matrix w which is needed to update the unreduced part of
                 ! the matrix
                 call stdlib_I64_clatrd( uplo, i+nb-1, nb, a, lda, e, tau, work,ldwork )
                 ! update the unreduced submatrix a(1:i-1,1:i-1), using an
                 ! update of the form:  a := a - v*w**h - w*v**h
                 call stdlib_I64_cher2k( uplo, 'NO TRANSPOSE', i-1, nb, -cone,a( 1_ilp64, i ), lda, work, &
                           ldwork, one, a, lda )
                 ! copy superdiagonal elements back into a, and diagonal
                 ! elements into d
                 do j = i, i + nb - 1
                    a( j-1, j ) = e( j-1 )
                    d( j ) = real( a( j, j ),KIND=sp)
                 end do
              end do
              ! use unblocked code to reduce the last or only block
              call stdlib_I64_chetd2( uplo, kk, a, lda, d, e, tau, iinfo )
           else
              ! reduce the lower triangle of a
              do i = 1, n - nx, nb
                 ! reduce columns i:i+nb-1 to tridiagonal form and form the
                 ! matrix w which is needed to update the unreduced part of
                 ! the matrix
                 call stdlib_I64_clatrd( uplo, n-i+1, nb, a( i, i ), lda, e( i ),tau( i ), work, &
                           ldwork )
                 ! update the unreduced submatrix a(i+nb:n,i+nb:n), using
                 ! an update of the form:  a := a - v*w**h - w*v**h
                 call stdlib_I64_cher2k( uplo, 'NO TRANSPOSE', n-i-nb+1, nb, -cone,a( i+nb, i ), lda, &
                           work( nb+1 ), ldwork, one,a( i+nb, i+nb ), lda )
                 ! copy subdiagonal elements back into a, and diagonal
                 ! elements into d
                 do j = i, i + nb - 1
                    a( j+1, j ) = e( j )
                    d( j ) = real( a( j, j ),KIND=sp)
                 end do
              end do
              ! use unblocked code to reduce the last or only block
              call stdlib_I64_chetd2( uplo, n-i+1, a( i, i ), lda, d( i ), e( i ),tau( i ), iinfo )
                        
           end if
           work( 1_ilp64 ) = lwkopt
           return
     end subroutine stdlib_I64_chetrd

     pure module subroutine stdlib_I64_zhetrd( uplo, n, a, lda, d, e, tau, work, lwork, info )
     !! ZHETRD reduces a complex Hermitian matrix A to real symmetric
     !! tridiagonal form T by a unitary similarity transformation:
     !! Q**H * A * Q = T.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, lwork, n
           ! Array Arguments 
           real(dp), intent(out) :: d(*), e(*)
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: tau(*), work(*)
        ! =====================================================================
           
           
           ! Local Scalars 
           logical(lk) :: lquery, upper
           integer(ilp64) :: i, iinfo, iws, j, kk, ldwork, lwkopt, nb, nbmin, nx
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp64
           upper = stdlib_lsame( uplo, 'U' )
           lquery = ( lwork==-1_ilp64 )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -2_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -4_ilp64
           else if( lwork<1_ilp64 .and. .not.lquery ) then
              info = -9_ilp64
           end if
           if( info==0_ilp64 ) then
              ! determine the block size.
              nb = stdlib_I64_ilaenv( 1_ilp64, 'ZHETRD', uplo, n, -1_ilp64, -1_ilp64, -1_ilp64 )
              lwkopt = n*nb
              work( 1_ilp64 ) = lwkopt
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'ZHETRD', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0_ilp64 ) then
              work( 1_ilp64 ) = 1_ilp64
              return
           end if
           nx = n
           iws = 1_ilp64
           if( nb>1_ilp64 .and. nb<n ) then
              ! determine when to cross over from blocked to unblocked code
              ! (last block is always handled by unblocked code).
              nx = max( nb, stdlib_I64_ilaenv( 3_ilp64, 'ZHETRD', uplo, n, -1_ilp64, -1_ilp64, -1_ilp64 ) )
              if( nx<n ) then
                 ! determine if workspace is large enough for blocked code.
                 ldwork = n
                 iws = ldwork*nb
                 if( lwork<iws ) then
                    ! not enough workspace to use optimal nb:  determine the
                    ! minimum value of nb, and reduce nb or force use of
                    ! unblocked code by setting nx = n.
                    nb = max( lwork / ldwork, 1_ilp64 )
                    nbmin = stdlib_I64_ilaenv( 2_ilp64, 'ZHETRD', uplo, n, -1_ilp64, -1_ilp64, -1_ilp64 )
                    if( nb<nbmin )nx = n
                 end if
              else
                 nx = n
              end if
           else
              nb = 1_ilp64
           end if
           if( upper ) then
              ! reduce the upper triangle of a.
              ! columns 1:kk are handled by the unblocked method.
              kk = n - ( ( n-nx+nb-1 ) / nb )*nb
              do i = n - nb + 1, kk + 1, -nb
                 ! reduce columns i:i+nb-1 to tridiagonal form and form the
                 ! matrix w which is needed to update the unreduced part of
                 ! the matrix
                 call stdlib_I64_zlatrd( uplo, i+nb-1, nb, a, lda, e, tau, work,ldwork )
                 ! update the unreduced submatrix a(1:i-1,1:i-1), using an
                 ! update of the form:  a := a - v*w**h - w*v**h
                 call stdlib_I64_zher2k( uplo, 'NO TRANSPOSE', i-1, nb, -cone,a( 1_ilp64, i ), lda, work, &
                           ldwork, one, a, lda )
                 ! copy superdiagonal elements back into a, and diagonal
                 ! elements into d
                 do j = i, i + nb - 1
                    a( j-1, j ) = e( j-1 )
                    d( j ) = real( a( j, j ),KIND=dp)
                 end do
              end do
              ! use unblocked code to reduce the last or only block
              call stdlib_I64_zhetd2( uplo, kk, a, lda, d, e, tau, iinfo )
           else
              ! reduce the lower triangle of a
              do i = 1, n - nx, nb
                 ! reduce columns i:i+nb-1 to tridiagonal form and form the
                 ! matrix w which is needed to update the unreduced part of
                 ! the matrix
                 call stdlib_I64_zlatrd( uplo, n-i+1, nb, a( i, i ), lda, e( i ),tau( i ), work, &
                           ldwork )
                 ! update the unreduced submatrix a(i+nb:n,i+nb:n), using
                 ! an update of the form:  a := a - v*w**h - w*v**h
                 call stdlib_I64_zher2k( uplo, 'NO TRANSPOSE', n-i-nb+1, nb, -cone,a( i+nb, i ), lda, &
                           work( nb+1 ), ldwork, one,a( i+nb, i+nb ), lda )
                 ! copy subdiagonal elements back into a, and diagonal
                 ! elements into d
                 do j = i, i + nb - 1
                    a( j+1, j ) = e( j )
                    d( j ) = real( a( j, j ),KIND=dp)
                 end do
              end do
              ! use unblocked code to reduce the last or only block
              call stdlib_I64_zhetd2( uplo, n-i+1, a( i, i ), lda, d( i ), e( i ),tau( i ), iinfo )
                        
           end if
           work( 1_ilp64 ) = lwkopt
           return
     end subroutine stdlib_I64_zhetrd




     pure module subroutine stdlib_I64_chetd2( uplo, n, a, lda, d, e, tau, info )
     !! CHETD2 reduces a complex Hermitian matrix A to real symmetric
     !! tridiagonal form T by a unitary similarity transformation:
     !! Q**H * A * Q = T.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, n
           ! Array Arguments 
           real(sp), intent(out) :: d(*), e(*)
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: tau(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp64) :: i
           complex(sp) :: alpha, taui
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp64
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -2_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -4_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'CHETD2', -info )
              return
           end if
           ! quick return if possible
           if( n<=0 )return
           if( upper ) then
              ! reduce the upper triangle of a
              a( n, n ) = real( a( n, n ),KIND=sp)
              do i = n - 1, 1, -1
                 ! generate elementary reflector h(i) = i - tau * v * v**h
                 ! to annihilate a(1:i-1,i+1)
                 alpha = a( i, i+1 )
                 call stdlib_I64_clarfg( i, alpha, a( 1_ilp64, i+1 ), 1_ilp64, taui )
                 e( i ) = real( alpha,KIND=sp)
                 if( taui/=czero ) then
                    ! apply h(i) from both sides to a(1:i,1:i)
                    a( i, i+1 ) = cone
                    ! compute  x := tau * a * v  storing x in tau(1:i)
                    call stdlib_I64_chemv( uplo, i, taui, a, lda, a( 1_ilp64, i+1 ), 1_ilp64, czero,tau, 1_ilp64 )
                              
                    ! compute  w := x - 1/2 * tau * (x**h * v) * v
                    alpha = -chalf*taui*stdlib_I64_cdotc( i, tau, 1_ilp64, a( 1_ilp64, i+1 ), 1_ilp64 )
                    call stdlib_I64_caxpy( i, alpha, a( 1_ilp64, i+1 ), 1_ilp64, tau, 1_ilp64 )
                    ! apply the transformation as a rank-2 update:
                       ! a := a - v * w**h - w * v**h
                    call stdlib_I64_cher2( uplo, i, -cone, a( 1_ilp64, i+1 ), 1_ilp64, tau, 1_ilp64, a,lda )
                 else
                    a( i, i ) = real( a( i, i ),KIND=sp)
                 end if
                 a( i, i+1 ) = e( i )
                 d( i+1 ) = real( a( i+1, i+1 ),KIND=sp)
                 tau( i ) = taui
              end do
              d( 1_ilp64 ) = real( a( 1_ilp64, 1_ilp64 ),KIND=sp)
           else
              ! reduce the lower triangle of a
              a( 1_ilp64, 1_ilp64 ) = real( a( 1_ilp64, 1_ilp64 ),KIND=sp)
              do i = 1, n - 1
                 ! generate elementary reflector h(i) = i - tau * v * v**h
                 ! to annihilate a(i+2:n,i)
                 alpha = a( i+1, i )
                 call stdlib_I64_clarfg( n-i, alpha, a( min( i+2, n ), i ), 1_ilp64, taui )
                 e( i ) = real( alpha,KIND=sp)
                 if( taui/=czero ) then
                    ! apply h(i) from both sides to a(i+1:n,i+1:n)
                    a( i+1, i ) = cone
                    ! compute  x := tau * a * v  storing y in tau(i:n-1)
                    call stdlib_I64_chemv( uplo, n-i, taui, a( i+1, i+1 ), lda,a( i+1, i ), 1_ilp64, czero, &
                              tau( i ), 1_ilp64 )
                    ! compute  w := x - 1/2 * tau * (x**h * v) * v
                    alpha = -chalf*taui*stdlib_I64_cdotc( n-i, tau( i ), 1_ilp64, a( i+1, i ),1_ilp64 )
                    call stdlib_I64_caxpy( n-i, alpha, a( i+1, i ), 1_ilp64, tau( i ), 1_ilp64 )
                    ! apply the transformation as a rank-2 update:
                       ! a := a - v * w**h - w * v**h
                    call stdlib_I64_cher2( uplo, n-i, -cone, a( i+1, i ), 1_ilp64, tau( i ), 1_ilp64,a( i+1, i+1 )&
                              , lda )
                 else
                    a( i+1, i+1 ) = real( a( i+1, i+1 ),KIND=sp)
                 end if
                 a( i+1, i ) = e( i )
                 d( i ) = real( a( i, i ),KIND=sp)
                 tau( i ) = taui
              end do
              d( n ) = real( a( n, n ),KIND=sp)
           end if
           return
     end subroutine stdlib_I64_chetd2

     pure module subroutine stdlib_I64_zhetd2( uplo, n, a, lda, d, e, tau, info )
     !! ZHETD2 reduces a complex Hermitian matrix A to real symmetric
     !! tridiagonal form T by a unitary similarity transformation:
     !! Q**H * A * Q = T.
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, n
           ! Array Arguments 
           real(dp), intent(out) :: d(*), e(*)
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: tau(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp64) :: i
           complex(dp) :: alpha, taui
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters
           info = 0_ilp64
           upper = stdlib_lsame( uplo, 'U')
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -2_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -4_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'ZHETD2', -info )
              return
           end if
           ! quick return if possible
           if( n<=0 )return
           if( upper ) then
              ! reduce the upper triangle of a
              a( n, n ) = real( a( n, n ),KIND=dp)
              do i = n - 1, 1, -1
                 ! generate elementary reflector h(i) = i - tau * v * v**h
                 ! to annihilate a(1:i-1,i+1)
                 alpha = a( i, i+1 )
                 call stdlib_I64_zlarfg( i, alpha, a( 1_ilp64, i+1 ), 1_ilp64, taui )
                 e( i ) = real( alpha,KIND=dp)
                 if( taui/=czero ) then
                    ! apply h(i) from both sides to a(1:i,1:i)
                    a( i, i+1 ) = cone
                    ! compute  x := tau * a * v  storing x in tau(1:i)
                    call stdlib_I64_zhemv( uplo, i, taui, a, lda, a( 1_ilp64, i+1 ), 1_ilp64, czero,tau, 1_ilp64 )
                              
                    ! compute  w := x - 1/2 * tau * (x**h * v) * v
                    alpha = -chalf*taui*stdlib_I64_zdotc( i, tau, 1_ilp64, a( 1_ilp64, i+1 ), 1_ilp64 )
                    call stdlib_I64_zaxpy( i, alpha, a( 1_ilp64, i+1 ), 1_ilp64, tau, 1_ilp64 )
                    ! apply the transformation as a rank-2 update:
                       ! a := a - v * w**h - w * v**h
                    call stdlib_I64_zher2( uplo, i, -cone, a( 1_ilp64, i+1 ), 1_ilp64, tau, 1_ilp64, a,lda )
                 else
                    a( i, i ) = real( a( i, i ),KIND=dp)
                 end if
                 a( i, i+1 ) = e( i )
                 d( i+1 ) = real( a( i+1, i+1 ),KIND=dp)
                 tau( i ) = taui
              end do
              d( 1_ilp64 ) = real( a( 1_ilp64, 1_ilp64 ),KIND=dp)
           else
              ! reduce the lower triangle of a
              a( 1_ilp64, 1_ilp64 ) = real( a( 1_ilp64, 1_ilp64 ),KIND=dp)
              do i = 1, n - 1
                 ! generate elementary reflector h(i) = i - tau * v * v**h
                 ! to annihilate a(i+2:n,i)
                 alpha = a( i+1, i )
                 call stdlib_I64_zlarfg( n-i, alpha, a( min( i+2, n ), i ), 1_ilp64, taui )
                 e( i ) = real( alpha,KIND=dp)
                 if( taui/=czero ) then
                    ! apply h(i) from both sides to a(i+1:n,i+1:n)
                    a( i+1, i ) = cone
                    ! compute  x := tau * a * v  storing y in tau(i:n-1)
                    call stdlib_I64_zhemv( uplo, n-i, taui, a( i+1, i+1 ), lda,a( i+1, i ), 1_ilp64, czero, &
                              tau( i ), 1_ilp64 )
                    ! compute  w := x - 1/2 * tau * (x**h * v) * v
                    alpha = -chalf*taui*stdlib_I64_zdotc( n-i, tau( i ), 1_ilp64, a( i+1, i ),1_ilp64 )
                    call stdlib_I64_zaxpy( n-i, alpha, a( i+1, i ), 1_ilp64, tau( i ), 1_ilp64 )
                    ! apply the transformation as a rank-2 update:
                       ! a := a - v * w**h - w * v**h
                    call stdlib_I64_zher2( uplo, n-i, -cone, a( i+1, i ), 1_ilp64, tau( i ), 1_ilp64,a( i+1, i+1 )&
                              , lda )
                 else
                    a( i+1, i+1 ) = real( a( i+1, i+1 ),KIND=dp)
                 end if
                 a( i+1, i ) = e( i )
                 d( i ) = real( a( i, i ),KIND=dp)
                 tau( i ) = taui
              end do
              d( n ) = real( a( n, n ),KIND=dp)
           end if
           return
     end subroutine stdlib_I64_zhetd2




     pure module subroutine stdlib_I64_cungtr( uplo, n, a, lda, tau, work, lwork, info )
     !! CUNGTR generates a complex unitary matrix Q which is defined as the
     !! product of n-1 elementary reflectors of order N, as returned by
     !! CHETRD:
     !! if UPLO = 'U', Q = H(n-1) . . . H(2) H(1),
     !! if UPLO = 'L', Q = H(1) H(2) . . . H(n-1).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, lwork, n
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(in) :: tau(*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery, upper
           integer(ilp64) :: i, iinfo, j, lwkopt, nb
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp64
           lquery = ( lwork==-1_ilp64 )
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -2_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -4_ilp64
           else if( lwork<max( 1_ilp64, n-1 ) .and. .not.lquery ) then
              info = -7_ilp64
           end if
           if( info==0_ilp64 ) then
              if ( upper ) then
                nb = stdlib_I64_ilaenv( 1_ilp64, 'CUNGQL', ' ', n-1, n-1, n-1, -1_ilp64 )
              else
                nb = stdlib_I64_ilaenv( 1_ilp64, 'CUNGQR', ' ', n-1, n-1, n-1, -1_ilp64 )
              end if
              lwkopt = max( 1_ilp64, n-1 )*nb
              work( 1_ilp64 ) = lwkopt
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'CUNGTR', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0_ilp64 ) then
              work( 1_ilp64 ) = 1_ilp64
              return
           end if
           if( upper ) then
              ! q was determined by a call to stdlib_I64_chetrd with uplo = 'u'
              ! shift the vectors which define the elementary reflectors cone
              ! column to the left, and set the last row and column of q to
              ! those of the unit matrix
              do j = 1, n - 1
                 do i = 1, j - 1
                    a( i, j ) = a( i, j+1 )
                 end do
                 a( n, j ) = czero
              end do
              do i = 1, n - 1
                 a( i, n ) = czero
              end do
              a( n, n ) = cone
              ! generate q(1:n-1,1:n-1)
              call stdlib_I64_cungql( n-1, n-1, n-1, a, lda, tau, work, lwork, iinfo )
           else
              ! q was determined by a call to stdlib_I64_chetrd with uplo = 'l'.
              ! shift the vectors which define the elementary reflectors cone
              ! column to the right, and set the first row and column of q to
              ! those of the unit matrix
              do j = n, 2, -1
                 a( 1_ilp64, j ) = czero
                 do i = j + 1, n
                    a( i, j ) = a( i, j-1 )
                 end do
              end do
              a( 1_ilp64, 1_ilp64 ) = cone
              do i = 2, n
                 a( i, 1_ilp64 ) = czero
              end do
              if( n>1_ilp64 ) then
                 ! generate q(2:n,2:n)
                 call stdlib_I64_cungqr( n-1, n-1, n-1, a( 2_ilp64, 2_ilp64 ), lda, tau, work,lwork, iinfo )
                           
              end if
           end if
           work( 1_ilp64 ) = lwkopt
           return
     end subroutine stdlib_I64_cungtr

     pure module subroutine stdlib_I64_zungtr( uplo, n, a, lda, tau, work, lwork, info )
     !! ZUNGTR generates a complex unitary matrix Q which is defined as the
     !! product of n-1 elementary reflectors of order N, as returned by
     !! ZHETRD:
     !! if UPLO = 'U', Q = H(n-1) . . . H(2) H(1),
     !! if UPLO = 'L', Q = H(1) H(2) . . . H(n-1).
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, lwork, n
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(in) :: tau(*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: lquery, upper
           integer(ilp64) :: i, iinfo, j, lwkopt, nb
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp64
           lquery = ( lwork==-1_ilp64 )
           upper = stdlib_lsame( uplo, 'U' )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -2_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -4_ilp64
           else if( lwork<max( 1_ilp64, n-1 ) .and. .not.lquery ) then
              info = -7_ilp64
           end if
           if( info==0_ilp64 ) then
              if( upper ) then
                 nb = stdlib_I64_ilaenv( 1_ilp64, 'ZUNGQL', ' ', n-1, n-1, n-1, -1_ilp64 )
              else
                 nb = stdlib_I64_ilaenv( 1_ilp64, 'ZUNGQR', ' ', n-1, n-1, n-1, -1_ilp64 )
              end if
              lwkopt = max( 1_ilp64, n-1 )*nb
              work( 1_ilp64 ) = lwkopt
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'ZUNGTR', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0_ilp64 ) then
              work( 1_ilp64 ) = 1_ilp64
              return
           end if
           if( upper ) then
              ! q was determined by a call to stdlib_I64_zhetrd with uplo = 'u'
              ! shift the vectors which define the elementary reflectors cone
              ! column to the left, and set the last row and column of q to
              ! those of the unit matrix
              do j = 1, n - 1
                 do i = 1, j - 1
                    a( i, j ) = a( i, j+1 )
                 end do
                 a( n, j ) = czero
              end do
              do i = 1, n - 1
                 a( i, n ) = czero
              end do
              a( n, n ) = cone
              ! generate q(1:n-1,1:n-1)
              call stdlib_I64_zungql( n-1, n-1, n-1, a, lda, tau, work, lwork, iinfo )
           else
              ! q was determined by a call to stdlib_I64_zhetrd with uplo = 'l'.
              ! shift the vectors which define the elementary reflectors cone
              ! column to the right, and set the first row and column of q to
              ! those of the unit matrix
              do j = n, 2, -1
                 a( 1_ilp64, j ) = czero
                 do i = j + 1, n
                    a( i, j ) = a( i, j-1 )
                 end do
              end do
              a( 1_ilp64, 1_ilp64 ) = cone
              do i = 2, n
                 a( i, 1_ilp64 ) = czero
              end do
              if( n>1_ilp64 ) then
                 ! generate q(2:n,2:n)
                 call stdlib_I64_zungqr( n-1, n-1, n-1, a( 2_ilp64, 2_ilp64 ), lda, tau, work,lwork, iinfo )
                           
              end if
           end if
           work( 1_ilp64 ) = lwkopt
           return
     end subroutine stdlib_I64_zungtr




     pure module subroutine stdlib_I64_cunmtr( side, uplo, trans, m, n, a, lda, tau, c, ldc,work, lwork, &
     !! CUNMTR overwrites the general complex M-by-N matrix C with
     !! SIDE = 'L'     SIDE = 'R'
     !! TRANS = 'N':      Q * C          C * Q
     !! TRANS = 'C':      Q**H * C       C * Q**H
     !! where Q is a complex unitary matrix of order nq, with nq = m if
     !! SIDE = 'L' and nq = n if SIDE = 'R'. Q is defined as the product of
     !! nq-1 elementary reflectors, as returned by CHETRD:
     !! if UPLO = 'U', Q = H(nq-1) . . . H(2) H(1);
     !! if UPLO = 'L', Q = H(1) H(2) . . . H(nq-1).
               info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side, trans, uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldc, lwork, m, n
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*), c(ldc,*)
           complex(sp), intent(in) :: tau(*)
           complex(sp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: left, lquery, upper
           integer(ilp64) :: i1, i2, iinfo, lwkopt, mi, nb, ni, nq, nw
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp64
           left = stdlib_lsame( side, 'L' )
           upper = stdlib_lsame( uplo, 'U' )
           lquery = ( lwork==-1_ilp64 )
           ! nq is the order of q and nw is the minimum dimension of work
           if( left ) then
              nq = m
              nw = max( 1_ilp64, n )
           else
              nq = n
              nw = max( 1_ilp64, m )
           end if
           if( .not.left .and. .not.stdlib_lsame( side, 'R' ) ) then
              info = -1_ilp64
           else if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -2_ilp64
           else if( .not.stdlib_lsame( trans, 'N' ) .and. .not.stdlib_lsame( trans, 'C' ) )&
                     then
              info = -3_ilp64
           else if( m<0_ilp64 ) then
              info = -4_ilp64
           else if( n<0_ilp64 ) then
              info = -5_ilp64
           else if( lda<max( 1_ilp64, nq ) ) then
              info = -7_ilp64
           else if( ldc<max( 1_ilp64, m ) ) then
              info = -10_ilp64
           else if( lwork<nw .and. .not.lquery ) then
              info = -12_ilp64
           end if
           if( info==0_ilp64 ) then
              if( upper ) then
                 if( left ) then
                    nb = stdlib_I64_ilaenv( 1_ilp64, 'CUNMQL', side // trans, m-1, n, m-1,-1_ilp64 )
                 else
                    nb = stdlib_I64_ilaenv( 1_ilp64, 'CUNMQL', side // trans, m, n-1, n-1,-1_ilp64 )
                 end if
              else
                 if( left ) then
                    nb = stdlib_I64_ilaenv( 1_ilp64, 'CUNMQR', side // trans, m-1, n, m-1,-1_ilp64 )
                 else
                    nb = stdlib_I64_ilaenv( 1_ilp64, 'CUNMQR', side // trans, m, n-1, n-1,-1_ilp64 )
                 end if
              end if
              lwkopt = nw*nb
              work( 1_ilp64 ) = lwkopt
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'CUNMTR', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( m==0_ilp64 .or. n==0_ilp64 .or. nq==1_ilp64 ) then
              work( 1_ilp64 ) = 1_ilp64
              return
           end if
           if( left ) then
              mi = m - 1_ilp64
              ni = n
           else
              mi = m
              ni = n - 1_ilp64
           end if
           if( upper ) then
              ! q was determined by a call to stdlib_I64_chetrd with uplo = 'u'
              call stdlib_I64_cunmql( side, trans, mi, ni, nq-1, a( 1_ilp64, 2_ilp64 ), lda, tau, c,ldc, work, &
                        lwork, iinfo )
           else
              ! q was determined by a call to stdlib_I64_chetrd with uplo = 'l'
              if( left ) then
                 i1 = 2_ilp64
                 i2 = 1_ilp64
              else
                 i1 = 1_ilp64
                 i2 = 2_ilp64
              end if
              call stdlib_I64_cunmqr( side, trans, mi, ni, nq-1, a( 2_ilp64, 1_ilp64 ), lda, tau,c( i1, i2 ), ldc,&
                         work, lwork, iinfo )
           end if
           work( 1_ilp64 ) = lwkopt
           return
     end subroutine stdlib_I64_cunmtr

     pure module subroutine stdlib_I64_zunmtr( side, uplo, trans, m, n, a, lda, tau, c, ldc,work, lwork, &
     !! ZUNMTR overwrites the general complex M-by-N matrix C with
     !! SIDE = 'L'     SIDE = 'R'
     !! TRANS = 'N':      Q * C          C * Q
     !! TRANS = 'C':      Q**H * C       C * Q**H
     !! where Q is a complex unitary matrix of order nq, with nq = m if
     !! SIDE = 'L' and nq = n if SIDE = 'R'. Q is defined as the product of
     !! nq-1 elementary reflectors, as returned by ZHETRD:
     !! if UPLO = 'U', Q = H(nq-1) . . . H(2) H(1);
     !! if UPLO = 'L', Q = H(1) H(2) . . . H(nq-1).
               info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: side, trans, uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldc, lwork, m, n
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*), c(ldc,*)
           complex(dp), intent(in) :: tau(*)
           complex(dp), intent(out) :: work(*)
        ! =====================================================================
           ! Local Scalars 
           logical(lk) :: left, lquery, upper
           integer(ilp64) :: i1, i2, iinfo, lwkopt, mi, nb, ni, nq, nw
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input arguments
           info = 0_ilp64
           left = stdlib_lsame( side, 'L' )
           upper = stdlib_lsame( uplo, 'U' )
           lquery = ( lwork==-1_ilp64 )
           ! nq is the order of q and nw is the minimum dimension of work
           if( left ) then
              nq = m
              nw = max( 1_ilp64, n )
           else
              nq = n
              nw = max( 1_ilp64, m )
           end if
           if( .not.left .and. .not.stdlib_lsame( side, 'R' ) ) then
              info = -1_ilp64
           else if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -2_ilp64
           else if( .not.stdlib_lsame( trans, 'N' ) .and. .not.stdlib_lsame( trans, 'C' ) )&
                     then
              info = -3_ilp64
           else if( m<0_ilp64 ) then
              info = -4_ilp64
           else if( n<0_ilp64 ) then
              info = -5_ilp64
           else if( lda<max( 1_ilp64, nq ) ) then
              info = -7_ilp64
           else if( ldc<max( 1_ilp64, m ) ) then
              info = -10_ilp64
           else if( lwork<nw .and. .not.lquery ) then
              info = -12_ilp64
           end if
           if( info==0_ilp64 ) then
              if( upper ) then
                 if( left ) then
                    nb = stdlib_I64_ilaenv( 1_ilp64, 'ZUNMQL', side // trans, m-1, n, m-1,-1_ilp64 )
                 else
                    nb = stdlib_I64_ilaenv( 1_ilp64, 'ZUNMQL', side // trans, m, n-1, n-1,-1_ilp64 )
                 end if
              else
                 if( left ) then
                    nb = stdlib_I64_ilaenv( 1_ilp64, 'ZUNMQR', side // trans, m-1, n, m-1,-1_ilp64 )
                 else
                    nb = stdlib_I64_ilaenv( 1_ilp64, 'ZUNMQR', side // trans, m, n-1, n-1,-1_ilp64 )
                 end if
              end if
              lwkopt = nw*nb
              work( 1_ilp64 ) = lwkopt
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'ZUNMTR', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( m==0_ilp64 .or. n==0_ilp64 .or. nq==1_ilp64 ) then
              work( 1_ilp64 ) = 1_ilp64
              return
           end if
           if( left ) then
              mi = m - 1_ilp64
              ni = n
           else
              mi = m
              ni = n - 1_ilp64
           end if
           if( upper ) then
              ! q was determined by a call to stdlib_I64_zhetrd with uplo = 'u'
              call stdlib_I64_zunmql( side, trans, mi, ni, nq-1, a( 1_ilp64, 2_ilp64 ), lda, tau, c,ldc, work, &
                        lwork, iinfo )
           else
              ! q was determined by a call to stdlib_I64_zhetrd with uplo = 'l'
              if( left ) then
                 i1 = 2_ilp64
                 i2 = 1_ilp64
              else
                 i1 = 1_ilp64
                 i2 = 2_ilp64
              end if
              call stdlib_I64_zunmqr( side, trans, mi, ni, nq-1, a( 2_ilp64, 1_ilp64 ), lda, tau,c( i1, i2 ), ldc,&
                         work, lwork, iinfo )
           end if
           work( 1_ilp64 ) = lwkopt
           return
     end subroutine stdlib_I64_zunmtr




     module subroutine stdlib_I64_chetrd_he2hb( uplo, n, kd, a, lda, ab, ldab, tau,work, lwork, info )
     !! CHETRD_HE2HB reduces a complex Hermitian matrix A to complex Hermitian
     !! band-diagonal form AB by a unitary similarity transformation:
     !! Q**H * A * Q = AB.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldab, lwork, n, kd
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: ab(ldab,*), tau(*), work(*)
        ! =====================================================================
           ! Parameters 
           
           ! Local Scalars 
           logical(lk) :: lquery, upper
           integer(ilp64) :: i, j, iinfo, lwmin, pn, pk, llk, ldt, ldw, lds2, lds1, ls2, ls1, lw, lt,&
                      tpos, wpos, s2pos, s1pos
           ! Intrinsic Functions 
           ! Executable Statements 
           ! determine the minimal workspace size required
           ! and test the input parameters
           info   = 0_ilp64
           upper  = stdlib_lsame( uplo, 'U' )
           lquery = ( lwork==-1_ilp64 )
           lwmin  = stdlib_I64_ilaenv2stage( 4_ilp64, 'CHETRD_HE2HB', '', n, kd, -1_ilp64, -1_ilp64 )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -2_ilp64
           else if( kd<0_ilp64 ) then
              info = -3_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -5_ilp64
           else if( ldab<max( 1_ilp64, kd+1 ) ) then
              info = -7_ilp64
           else if( lwork<lwmin .and. .not.lquery ) then
              info = -10_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'CHETRD_HE2HB', -info )
              return
           else if( lquery ) then
              work( 1_ilp64 ) = lwmin
              return
           end if
           ! quick return if possible
           ! copy the upper/lower portion of a into ab
           if( n<=kd+1 ) then
               if( upper ) then
                   do i = 1, n
                       llk = min( kd+1, i )
                       call stdlib_I64_ccopy( llk, a( i-llk+1, i ), 1_ilp64,ab( kd+1-llk+1, i ), 1_ilp64 )
                   end do
               else
                   do i = 1, n
                       llk = min( kd+1, n-i+1 )
                       call stdlib_I64_ccopy( llk, a( i, i ), 1_ilp64, ab( 1_ilp64, i ), 1_ilp64 )
                   end do
               endif
               work( 1_ilp64 ) = 1_ilp64
               return
           end if
           ! determine the pointer position for the workspace
           ldt    = kd
           lds1   = kd
           lt     = ldt*kd
           lw     = n*kd
           ls1    = lds1*kd
           ls2    = lwmin - lt - lw - ls1
            ! ls2 = n*max(kd,factoptnb)
           tpos   = 1_ilp64
           wpos   = tpos  + lt
           s1pos  = wpos  + lw
           s2pos  = s1pos + ls1
           if( upper ) then
               ldw    = kd
               lds2   = kd
           else
               ldw    = n
               lds2   = n
           endif
           ! set the workspace of the triangular matrix t to czero once such a
           ! way every time t is generated the upper/lower portion will be always czero
           call stdlib_I64_claset( "A", ldt, kd, czero, czero, work( tpos ), ldt )
           if( upper ) then
               do i = 1, n - kd, kd
                  pn = n-i-kd+1
                  pk = min( n-i-kd+1, kd )
                  ! compute the lq factorization of the current block
                  call stdlib_I64_cgelqf( kd, pn, a( i, i+kd ), lda,tau( i ), work( s2pos ), ls2, &
                            iinfo )
                  ! copy the upper portion of a into ab
                  do j = i, i+pk-1
                     llk = min( kd, n-j ) + 1_ilp64
                     call stdlib_I64_ccopy( llk, a( j, j ), lda, ab( kd+1, j ), ldab-1 )
                  end do
                  call stdlib_I64_claset( 'LOWER', pk, pk, czero, cone,a( i, i+kd ), lda )
                  ! form the matrix t
                  call stdlib_I64_clarft( 'FORWARD', 'ROWWISE', pn, pk,a( i, i+kd ), lda, tau( i ),&
                            work( tpos ), ldt )
                  ! compute w:
                  call stdlib_I64_cgemm( 'CONJUGATE', 'NO TRANSPOSE', pk, pn, pk,cone,  work( tpos ), &
                            ldt,a( i, i+kd ), lda,czero, work( s2pos ), lds2 )
                  call stdlib_I64_chemm( 'RIGHT', uplo, pk, pn,cone,  a( i+kd, i+kd ), lda,work( &
                            s2pos ), lds2,czero, work( wpos ), ldw )
                  call stdlib_I64_cgemm( 'NO TRANSPOSE', 'CONJUGATE', pk, pk, pn,cone,  work( wpos ), &
                            ldw,work( s2pos ), lds2,czero, work( s1pos ), lds1 )
                  call stdlib_I64_cgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', pk, pn, pk,-chalf, work( &
                            s1pos ), lds1,a( i, i+kd ), lda,cone,   work( wpos ), ldw )
                  ! update the unreduced submatrix a(i+kd:n,i+kd:n), using
                  ! an update of the form:  a := a - v'*w - w'*v
                  call stdlib_I64_cher2k( uplo, 'CONJUGATE', pn, pk,-cone, a( i, i+kd ), lda,work( &
                            wpos ), ldw,one, a( i+kd, i+kd ), lda )
               end do
              ! copy the upper band to ab which is the band storage matrix
              do j = n-kd+1, n
                 llk = min(kd, n-j) + 1_ilp64
                 call stdlib_I64_ccopy( llk, a( j, j ), lda, ab( kd+1, j ), ldab-1 )
              end do
           else
               ! reduce the lower triangle of a to lower band matrix
               loop_40: do i = 1, n - kd, kd
                  pn = n-i-kd+1
                  pk = min( n-i-kd+1, kd )
                  ! compute the qr factorization of the current block
                  call stdlib_I64_cgeqrf( pn, kd, a( i+kd, i ), lda,tau( i ), work( s2pos ), ls2, &
                            iinfo )
                  ! copy the upper portion of a into ab
                  do j = i, i+pk-1
                     llk = min( kd, n-j ) + 1_ilp64
                     call stdlib_I64_ccopy( llk, a( j, j ), 1_ilp64, ab( 1_ilp64, j ), 1_ilp64 )
                  end do
                  call stdlib_I64_claset( 'UPPER', pk, pk, czero, cone,a( i+kd, i ), lda )
                  ! form the matrix t
                  call stdlib_I64_clarft( 'FORWARD', 'COLUMNWISE', pn, pk,a( i+kd, i ), lda, tau( i ),&
                            work( tpos ), ldt )
                  ! compute w:
                  call stdlib_I64_cgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', pn, pk, pk,cone, a( i+kd, i )&
                            , lda,work( tpos ), ldt,czero, work( s2pos ), lds2 )
                  call stdlib_I64_chemm( 'LEFT', uplo, pn, pk,cone, a( i+kd, i+kd ), lda,work( s2pos )&
                            , lds2,czero, work( wpos ), ldw )
                  call stdlib_I64_cgemm( 'CONJUGATE', 'NO TRANSPOSE', pk, pk, pn,cone, work( s2pos ), &
                            lds2,work( wpos ), ldw,czero, work( s1pos ), lds1 )
                  call stdlib_I64_cgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', pn, pk, pk,-chalf, a( i+kd, &
                            i ), lda,work( s1pos ), lds1,cone, work( wpos ), ldw )
                  ! update the unreduced submatrix a(i+kd:n,i+kd:n), using
                  ! an update of the form:  a := a - v*w' - w*v'
                  call stdlib_I64_cher2k( uplo, 'NO TRANSPOSE', pn, pk,-cone, a( i+kd, i ), lda,work( &
                            wpos ), ldw,one, a( i+kd, i+kd ), lda )
                  ! ==================================================================
                  ! restore a for comparison and checking to be removed
                   ! do 45 j = i, i+pk-1
                      ! llk = min( kd, n-j ) + 1
                      ! call stdlib_I64_ccopy( llk, ab( 1, j ), 1, a( j, j ), 1 )
                      45 continue
                  ! ==================================================================
               end do loop_40
              ! copy the lower band to ab which is the band storage matrix
              do j = n-kd+1, n
                 llk = min(kd, n-j) + 1_ilp64
                 call stdlib_I64_ccopy( llk, a( j, j ), 1_ilp64, ab( 1_ilp64, j ), 1_ilp64 )
              end do
           end if
           work( 1_ilp64 ) = lwmin
           return
     end subroutine stdlib_I64_chetrd_he2hb

     module subroutine stdlib_I64_zhetrd_he2hb( uplo, n, kd, a, lda, ab, ldab, tau,work, lwork, info )
     !! ZHETRD_HE2HB reduces a complex Hermitian matrix A to complex Hermitian
     !! band-diagonal form AB by a unitary similarity transformation:
     !! Q**H * A * Q = AB.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldab, lwork, n, kd
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: ab(ldab,*), tau(*), work(*)
        ! =====================================================================
           ! Parameters 
           
           ! Local Scalars 
           logical(lk) :: lquery, upper
           integer(ilp64) :: i, j, iinfo, lwmin, pn, pk, llk, ldt, ldw, lds2, lds1, ls2, ls1, lw, lt,&
                      tpos, wpos, s2pos, s1pos
           ! Intrinsic Functions 
           ! Executable Statements 
           ! determine the minimal workspace size required
           ! and test the input parameters
           info   = 0_ilp64
           upper  = stdlib_lsame( uplo, 'U' )
           lquery = ( lwork==-1_ilp64 )
           lwmin  = stdlib_I64_ilaenv2stage( 4_ilp64, 'ZHETRD_HE2HB', '', n, kd, -1_ilp64, -1_ilp64 )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -2_ilp64
           else if( kd<0_ilp64 ) then
              info = -3_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -5_ilp64
           else if( ldab<max( 1_ilp64, kd+1 ) ) then
              info = -7_ilp64
           else if( lwork<lwmin .and. .not.lquery ) then
              info = -10_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'ZHETRD_HE2HB', -info )
              return
           else if( lquery ) then
              work( 1_ilp64 ) = lwmin
              return
           end if
           ! quick return if possible
           ! copy the upper/lower portion of a into ab
           if( n<=kd+1 ) then
               if( upper ) then
                   do i = 1, n
                       llk = min( kd+1, i )
                       call stdlib_I64_zcopy( llk, a( i-llk+1, i ), 1_ilp64,ab( kd+1-llk+1, i ), 1_ilp64 )
                   end do
               else
                   do i = 1, n
                       llk = min( kd+1, n-i+1 )
                       call stdlib_I64_zcopy( llk, a( i, i ), 1_ilp64, ab( 1_ilp64, i ), 1_ilp64 )
                   end do
               endif
               work( 1_ilp64 ) = 1_ilp64
               return
           end if
           ! determine the pointer position for the workspace
           ldt    = kd
           lds1   = kd
           lt     = ldt*kd
           lw     = n*kd
           ls1    = lds1*kd
           ls2    = lwmin - lt - lw - ls1
            ! ls2 = n*max(kd,factoptnb)
           tpos   = 1_ilp64
           wpos   = tpos  + lt
           s1pos  = wpos  + lw
           s2pos  = s1pos + ls1
           if( upper ) then
               ldw    = kd
               lds2   = kd
           else
               ldw    = n
               lds2   = n
           endif
           ! set the workspace of the triangular matrix t to czero once such a
           ! way every time t is generated the upper/lower portion will be always czero
           call stdlib_I64_zlaset( "A", ldt, kd, czero, czero, work( tpos ), ldt )
           if( upper ) then
               do i = 1, n - kd, kd
                  pn = n-i-kd+1
                  pk = min( n-i-kd+1, kd )
                  ! compute the lq factorization of the current block
                  call stdlib_I64_zgelqf( kd, pn, a( i, i+kd ), lda,tau( i ), work( s2pos ), ls2, &
                            iinfo )
                  ! copy the upper portion of a into ab
                  do j = i, i+pk-1
                     llk = min( kd, n-j ) + 1_ilp64
                     call stdlib_I64_zcopy( llk, a( j, j ), lda, ab( kd+1, j ), ldab-1 )
                  end do
                  call stdlib_I64_zlaset( 'LOWER', pk, pk, czero, cone,a( i, i+kd ), lda )
                  ! form the matrix t
                  call stdlib_I64_zlarft( 'FORWARD', 'ROWWISE', pn, pk,a( i, i+kd ), lda, tau( i ),&
                            work( tpos ), ldt )
                  ! compute w:
                  call stdlib_I64_zgemm( 'CONJUGATE', 'NO TRANSPOSE', pk, pn, pk,cone,  work( tpos ), &
                            ldt,a( i, i+kd ), lda,czero, work( s2pos ), lds2 )
                  call stdlib_I64_zhemm( 'RIGHT', uplo, pk, pn,cone,  a( i+kd, i+kd ), lda,work( &
                            s2pos ), lds2,czero, work( wpos ), ldw )
                  call stdlib_I64_zgemm( 'NO TRANSPOSE', 'CONJUGATE', pk, pk, pn,cone,  work( wpos ), &
                            ldw,work( s2pos ), lds2,czero, work( s1pos ), lds1 )
                  call stdlib_I64_zgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', pk, pn, pk,-chalf, work( &
                            s1pos ), lds1,a( i, i+kd ), lda,cone,   work( wpos ), ldw )
                  ! update the unreduced submatrix a(i+kd:n,i+kd:n), using
                  ! an update of the form:  a := a - v'*w - w'*v
                  call stdlib_I64_zher2k( uplo, 'CONJUGATE', pn, pk,-cone, a( i, i+kd ), lda,work( &
                            wpos ), ldw,one, a( i+kd, i+kd ), lda )
               end do
              ! copy the upper band to ab which is the band storage matrix
              do j = n-kd+1, n
                 llk = min(kd, n-j) + 1_ilp64
                 call stdlib_I64_zcopy( llk, a( j, j ), lda, ab( kd+1, j ), ldab-1 )
              end do
           else
               ! reduce the lower triangle of a to lower band matrix
               loop_40: do i = 1, n - kd, kd
                  pn = n-i-kd+1
                  pk = min( n-i-kd+1, kd )
                  ! compute the qr factorization of the current block
                  call stdlib_I64_zgeqrf( pn, kd, a( i+kd, i ), lda,tau( i ), work( s2pos ), ls2, &
                            iinfo )
                  ! copy the upper portion of a into ab
                  do j = i, i+pk-1
                     llk = min( kd, n-j ) + 1_ilp64
                     call stdlib_I64_zcopy( llk, a( j, j ), 1_ilp64, ab( 1_ilp64, j ), 1_ilp64 )
                  end do
                  call stdlib_I64_zlaset( 'UPPER', pk, pk, czero, cone,a( i+kd, i ), lda )
                  ! form the matrix t
                  call stdlib_I64_zlarft( 'FORWARD', 'COLUMNWISE', pn, pk,a( i+kd, i ), lda, tau( i ),&
                            work( tpos ), ldt )
                  ! compute w:
                  call stdlib_I64_zgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', pn, pk, pk,cone, a( i+kd, i )&
                            , lda,work( tpos ), ldt,czero, work( s2pos ), lds2 )
                  call stdlib_I64_zhemm( 'LEFT', uplo, pn, pk,cone, a( i+kd, i+kd ), lda,work( s2pos )&
                            , lds2,czero, work( wpos ), ldw )
                  call stdlib_I64_zgemm( 'CONJUGATE', 'NO TRANSPOSE', pk, pk, pn,cone, work( s2pos ), &
                            lds2,work( wpos ), ldw,czero, work( s1pos ), lds1 )
                  call stdlib_I64_zgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', pn, pk, pk,-chalf, a( i+kd, &
                            i ), lda,work( s1pos ), lds1,cone, work( wpos ), ldw )
                  ! update the unreduced submatrix a(i+kd:n,i+kd:n), using
                  ! an update of the form:  a := a - v*w' - w*v'
                  call stdlib_I64_zher2k( uplo, 'NO TRANSPOSE', pn, pk,-cone, a( i+kd, i ), lda,work( &
                            wpos ), ldw,one, a( i+kd, i+kd ), lda )
                  ! ==================================================================
                  ! restore a for comparison and checking to be removed
                   ! do 45 j = i, i+pk-1
                      ! llk = min( kd, n-j ) + 1
                      ! call stdlib_I64_zcopy( nlk, ab( 1, j ), 1, a( j, j ), 1 )
                      45 continue
                  ! ==================================================================
               end do loop_40
              ! copy the lower band to ab which is the band storage matrix
              do j = n-kd+1, n
                 llk = min(kd, n-j) + 1_ilp64
                 call stdlib_I64_zcopy( llk, a( j, j ), 1_ilp64, ab( 1_ilp64, j ), 1_ilp64 )
              end do
           end if
           work( 1_ilp64 ) = lwmin
           return
     end subroutine stdlib_I64_zhetrd_he2hb




     module subroutine stdlib_I64_chetrd_hb2st( stage1, vect, uplo, n, kd, ab, ldab,d, e, hous, lhous, &
     !! CHETRD_HB2ST reduces a complex Hermitian band matrix A to real symmetric
     !! tridiagonal form T by a unitary similarity transformation:
     !! Q**H * A * Q = T.
               work, lwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: stage1, uplo, vect
           integer(ilp64), intent(in) :: n, kd, ldab, lhous, lwork
           integer(ilp64), intent(out) :: info
           ! Array Arguments 
           real(sp), intent(out) :: d(*), e(*)
           complex(sp), intent(inout) :: ab(ldab,*)
           complex(sp), intent(out) :: hous(*), work(*)
        ! =====================================================================
           ! Parameters 
           
           ! Local Scalars 
           logical(lk) :: lquery, wantq, upper, afters1
           integer(ilp64) :: i, m, k, ib, sweepid, myid, shift, stt, st, ed, stind, edind, &
           blklastind, colpt, thed, stepercol, grsiz, thgrsiz, thgrnb, thgrid, nbtiles, ttype, &
           tid, nthreads, debug, abdpos, abofdpos, dpos, ofdpos, awpos, inda, indw, apos, sizea, &
                     lda, indv, indtau, sicev, sizetau, ldv, lhmin, lwmin
           real(sp) :: abstmp
           complex(sp) :: tmp
           ! Intrinsic Functions 
           ! Executable Statements 
           ! determine the minimal workspace size required.
           ! test the input parameters
           debug   = 0_ilp64
           info    = 0_ilp64
           afters1 = stdlib_lsame( stage1, 'Y' )
           wantq   = stdlib_lsame( vect, 'V' )
           upper   = stdlib_lsame( uplo, 'U' )
           lquery  = ( lwork==-1_ilp64 ) .or. ( lhous==-1_ilp64 )
           ! determine the block size, the workspace size and the hous size.
           ib     = stdlib_I64_ilaenv2stage( 2_ilp64, 'CHETRD_HB2ST', vect, n, kd, -1_ilp64, -1_ilp64 )
           lhmin  = stdlib_I64_ilaenv2stage( 3_ilp64, 'CHETRD_HB2ST', vect, n, kd, ib, -1_ilp64 )
           lwmin  = stdlib_I64_ilaenv2stage( 4_ilp64, 'CHETRD_HB2ST', vect, n, kd, ib, -1_ilp64 )
           if( .not.afters1 .and. .not.stdlib_lsame( stage1, 'N' ) ) then
              info = -1_ilp64
           else if( .not.stdlib_lsame( vect, 'N' ) ) then
              info = -2_ilp64
           else if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -3_ilp64
           else if( n<0_ilp64 ) then
              info = -4_ilp64
           else if( kd<0_ilp64 ) then
              info = -5_ilp64
           else if( ldab<(kd+1) ) then
              info = -7_ilp64
           else if( lhous<lhmin .and. .not.lquery ) then
              info = -11_ilp64
           else if( lwork<lwmin .and. .not.lquery ) then
              info = -13_ilp64
           end if
           if( info==0_ilp64 ) then
              hous( 1_ilp64 ) = lhmin
              work( 1_ilp64 ) = lwmin
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'CHETRD_HB2ST', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0_ilp64 ) then
               hous( 1_ilp64 ) = 1_ilp64
               work( 1_ilp64 ) = 1_ilp64
               return
           end if
           ! determine pointer position
           ldv      = kd + ib
           sizetau  = 2_ilp64 * n
           sicev    = 2_ilp64 * n
           indtau   = 1_ilp64
           indv     = indtau + sizetau
           lda      = 2_ilp64 * kd + 1_ilp64
           sizea    = lda * n
           inda     = 1_ilp64
           indw     = inda + sizea
           nthreads = 1_ilp64
           tid      = 0_ilp64
           if( upper ) then
               apos     = inda + kd
               awpos    = inda
               dpos     = apos + kd
               ofdpos   = dpos - 1_ilp64
               abdpos   = kd + 1_ilp64
               abofdpos = kd
           else
               apos     = inda
               awpos    = inda + kd + 1_ilp64
               dpos     = apos
               ofdpos   = dpos + 1_ilp64
               abdpos   = 1_ilp64
               abofdpos = 2_ilp64
           endif
           ! case kd=0:
           ! the matrix is diagonal. we just copy it (convert to "real" for
           ! complex because d is double and the imaginary part should be 0)
           ! and store it in d. a sequential code here is better or
           ! in a parallel environment it might need two cores for d and e
           if( kd==0_ilp64 ) then
               do i = 1, n
                   d( i ) = real( ab( abdpos, i ),KIND=sp)
               end do
               do i = 1, n-1
                   e( i ) = zero
               end do
               hous( 1_ilp64 ) = 1_ilp64
               work( 1_ilp64 ) = 1_ilp64
               return
           end if
           ! case kd=1:
           ! the matrix is already tridiagonal. we have to make diagonal
           ! and offdiagonal elements real, and store them in d and e.
           ! for that, for real precision just copy the diag and offdiag
           ! to d and e while for the complex case the bulge chasing is
           ! performed to convert the hermetian tridiagonal to symmetric
           ! tridiagonal. a simpler conversion formula might be used, but then
           ! updating the q matrix will be required and based if q is generated
           ! or not this might complicate the story.
           if( kd==1_ilp64 ) then
               do i = 1, n
                   d( i ) = real( ab( abdpos, i ),KIND=sp)
               end do
               ! make off-diagonal elements real and copy them to e
               if( upper ) then
                   do i = 1, n - 1
                       tmp = ab( abofdpos, i+1 )
                       abstmp = abs( tmp )
                       ab( abofdpos, i+1 ) = abstmp
                       e( i ) = abstmp
                       if( abstmp/=zero ) then
                          tmp = tmp / abstmp
                       else
                          tmp = cone
                       end if
                       if( i<n-1 )ab( abofdpos, i+2 ) = ab( abofdpos, i+2 )*tmp
                        ! if( wantz ) then
                           ! call stdlib_I64_cscal( n, conjg( tmp ), q( 1, i+1 ), 1 )
                        ! end if
                   end do
               else
                   do i = 1, n - 1
                      tmp = ab( abofdpos, i )
                      abstmp = abs( tmp )
                      ab( abofdpos, i ) = abstmp
                      e( i ) = abstmp
                      if( abstmp/=zero ) then
                         tmp = tmp / abstmp
                      else
                         tmp = cone
                      end if
                      if( i<n-1 )ab( abofdpos, i+1 ) = ab( abofdpos, i+1 )*tmp
                       ! if( wantq ) then
                          ! call stdlib_I64_cscal( n, tmp, q( 1, i+1 ), 1 )
                       ! end if
                   end do
               endif
               hous( 1_ilp64 ) = 1_ilp64
               work( 1_ilp64 ) = 1_ilp64
               return
           end if
           ! main code start here.
           ! reduce the hermitian band of a to a tridiagonal matrix.
           thgrsiz   = n
           grsiz     = 1_ilp64
           shift     = 3_ilp64
           nbtiles   = ceiling( real(n,KIND=sp)/real(kd,KIND=sp) )
           stepercol = ceiling( real(shift,KIND=sp)/real(grsiz,KIND=sp) )
           thgrnb    = ceiling( real(n-1,KIND=sp)/real(thgrsiz,KIND=sp) )
           call stdlib_I64_clacpy( "A", kd+1, n, ab, ldab, work( apos ), lda )
           call stdlib_I64_claset( "A", kd,   n, czero, czero, work( awpos ), lda )
           ! openmp parallelisation start here
           !$OMP PARALLEL PRIVATE( TID, THGRID, BLKLASTIND ) &
           !$OMP&         PRIVATE( THED, I, M, K, ST, ED, STT, SWEEPID ) &
           !$OMP&         PRIVATE( MYID, TTYPE, COLPT, STIND, EDIND ) &
           !$OMP&         SHARED ( UPLO, WANTQ, INDV, INDTAU, HOUS, WORK) &
           !$OMP&         SHARED ( N, KD, IB, NBTILES, LDA, LDV, INDA ) &
           !$OMP&         SHARED ( STEPERCOL, THGRNB, THGRSIZ, GRSIZ, SHIFT )
           !$OMP MASTER
           ! main bulge chasing loop
           loop_100: do thgrid = 1, thgrnb
               stt  = (thgrid-1)*thgrsiz+1
               thed = min( (stt + thgrsiz -1_ilp64), (n-1))
               loop_110: do i = stt, n-1
                   ed = min( i, thed )
                   if( stt>ed ) exit
                   loop_120: do m = 1, stepercol
                       st = stt
                       loop_130: do sweepid = st, ed
                           loop_140: do k = 1, grsiz
                               myid  = (i-sweepid)*(stepercol*grsiz)+ (m-1)*grsiz + k
                               if ( myid==1_ilp64 ) then
                                   ttype = 1_ilp64
                               else
                                   ttype = mod( myid, 2_ilp64 ) + 2_ilp64
                               endif
                               if( ttype==2_ilp64 ) then
                                   colpt      = (myid/2_ilp64)*kd + sweepid
                                   stind      = colpt-kd+1
                                   edind      = min(colpt,n)
                                   blklastind = colpt
                               else
                                   colpt      = ((myid+1)/2_ilp64)*kd + sweepid
                                   stind      = colpt-kd+1
                                   edind      = min(colpt,n)
                                   if( ( stind>=edind-1 ).and.( edind==n ) ) then
                                       blklastind = n
                                   else
                                       blklastind = 0_ilp64
                                   endif
                               endif
                               ! call the kernel
                               !$ if( ttype/=1 ) then
                                   !$OMP TASK DEPEND(in:WORK(MYID+SHIFT-1)) &
                                   !$OMP&     DEPEND(in:WORK(MYID-1)) &
                                   !$OMP&     DEPEND(out:WORK(MYID))
                                   !$ tid      = omp_get_thread_num()
                                   !$ call stdlib_I64_chb2st_kernels( uplo, wantq, ttype,stind, edind, &
                                   !$ sweepid, n, kd, ib,work ( inda ), lda,hous( indv ), hous( &
                                   !$ indtau ), ldv,work( indw + tid*kd ) )
                               !$OMP END TASK
                                   !$ else
                                   !$OMP TASK DEPEND(in:WORK(MYID+SHIFT-1)) &
                                   !$OMP&     DEPEND(out:WORK(MYID))
                                   !$ tid      = omp_get_thread_num()
                                   call stdlib_I64_chb2st_kernels( uplo, wantq, ttype,stind, edind, &
                                   sweepid, n, kd, ib,work ( inda ), lda,hous( indv ), hous( &
                                            indtau ), ldv,work( indw + tid*kd ) )
                                   !$OMP END TASK
                               !$ endif
                               if ( blklastind>=(n-1) ) then
                                   stt = stt + 1_ilp64
                                   exit
                               endif
                           end do loop_140
                       end do loop_130
                   end do loop_120
               end do loop_110
           end do loop_100
           !$OMP END MASTER
           !$OMP END PARALLEL
           ! copy the diagonal from a to d. note that d is real thus only
           ! the real part is needed, the imaginary part should be czero.
           do i = 1, n
               d( i ) = real( work( dpos+(i-1)*lda ),KIND=sp)
           end do
           ! copy the off diagonal from a to e. note that e is real thus only
           ! the real part is needed, the imaginary part should be czero.
           if( upper ) then
               do i = 1, n-1
                  e( i ) = real( work( ofdpos+i*lda ),KIND=sp)
               end do
           else
               do i = 1, n-1
                  e( i ) = real( work( ofdpos+(i-1)*lda ),KIND=sp)
               end do
           endif
           hous( 1_ilp64 ) = lhmin
           work( 1_ilp64 ) = lwmin
           return
     end subroutine stdlib_I64_chetrd_hb2st

     module subroutine stdlib_I64_zhetrd_hb2st( stage1, vect, uplo, n, kd, ab, ldab,d, e, hous, lhous, &
     !! ZHETRD_HB2ST reduces a complex Hermitian band matrix A to real symmetric
     !! tridiagonal form T by a unitary similarity transformation:
     !! Q**H * A * Q = T.
               work, lwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: stage1, uplo, vect
           integer(ilp64), intent(in) :: n, kd, ldab, lhous, lwork
           integer(ilp64), intent(out) :: info
           ! Array Arguments 
           real(dp), intent(out) :: d(*), e(*)
           complex(dp), intent(inout) :: ab(ldab,*)
           complex(dp), intent(out) :: hous(*), work(*)
        ! =====================================================================
           ! Parameters 
           
           ! Local Scalars 
           logical(lk) :: lquery, wantq, upper, afters1
           integer(ilp64) :: i, m, k, ib, sweepid, myid, shift, stt, st, ed, stind, edind, &
           blklastind, colpt, thed, stepercol, grsiz, thgrsiz, thgrnb, thgrid, nbtiles, ttype, &
           tid, nthreads, debug, abdpos, abofdpos, dpos, ofdpos, awpos, inda, indw, apos, sizea, &
                     lda, indv, indtau, sizev, sizetau, ldv, lhmin, lwmin
           real(dp) :: abstmp
           complex(dp) :: tmp
           ! Intrinsic Functions 
           ! Executable Statements 
           ! determine the minimal workspace size required.
           ! test the input parameters
           debug   = 0_ilp64
           info    = 0_ilp64
           afters1 = stdlib_lsame( stage1, 'Y' )
           wantq   = stdlib_lsame( vect, 'V' )
           upper   = stdlib_lsame( uplo, 'U' )
           lquery  = ( lwork==-1_ilp64 ) .or. ( lhous==-1_ilp64 )
           ! determine the block size, the workspace size and the hous size.
           ib     = stdlib_I64_ilaenv2stage( 2_ilp64, 'ZHETRD_HB2ST', vect, n, kd, -1_ilp64, -1_ilp64 )
           lhmin  = stdlib_I64_ilaenv2stage( 3_ilp64, 'ZHETRD_HB2ST', vect, n, kd, ib, -1_ilp64 )
           lwmin  = stdlib_I64_ilaenv2stage( 4_ilp64, 'ZHETRD_HB2ST', vect, n, kd, ib, -1_ilp64 )
           if( .not.afters1 .and. .not.stdlib_lsame( stage1, 'N' ) ) then
              info = -1_ilp64
           else if( .not.stdlib_lsame( vect, 'N' ) ) then
              info = -2_ilp64
           else if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -3_ilp64
           else if( n<0_ilp64 ) then
              info = -4_ilp64
           else if( kd<0_ilp64 ) then
              info = -5_ilp64
           else if( ldab<(kd+1) ) then
              info = -7_ilp64
           else if( lhous<lhmin .and. .not.lquery ) then
              info = -11_ilp64
           else if( lwork<lwmin .and. .not.lquery ) then
              info = -13_ilp64
           end if
           if( info==0_ilp64 ) then
              hous( 1_ilp64 ) = lhmin
              work( 1_ilp64 ) = lwmin
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'ZHETRD_HB2ST', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0_ilp64 ) then
               hous( 1_ilp64 ) = 1_ilp64
               work( 1_ilp64 ) = 1_ilp64
               return
           end if
           ! determine pointer position
           ldv      = kd + ib
           sizetau  = 2_ilp64 * n
           sizev    = 2_ilp64 * n
           indtau   = 1_ilp64
           indv     = indtau + sizetau
           lda      = 2_ilp64 * kd + 1_ilp64
           sizea    = lda * n
           inda     = 1_ilp64
           indw     = inda + sizea
           nthreads = 1_ilp64
           tid      = 0_ilp64
           if( upper ) then
               apos     = inda + kd
               awpos    = inda
               dpos     = apos + kd
               ofdpos   = dpos - 1_ilp64
               abdpos   = kd + 1_ilp64
               abofdpos = kd
           else
               apos     = inda
               awpos    = inda + kd + 1_ilp64
               dpos     = apos
               ofdpos   = dpos + 1_ilp64
               abdpos   = 1_ilp64
               abofdpos = 2_ilp64
           endif
           ! case kd=0:
           ! the matrix is diagonal. we just copy it (convert to "real" for
           ! complex because d is double and the imaginary part should be 0)
           ! and store it in d. a sequential code here is better or
           ! in a parallel environment it might need two cores for d and e
           if( kd==0_ilp64 ) then
               do i = 1, n
                   d( i ) = real( ab( abdpos, i ),KIND=dp)
               end do
               do i = 1, n-1
                   e( i ) = zero
               end do
               hous( 1_ilp64 ) = 1_ilp64
               work( 1_ilp64 ) = 1_ilp64
               return
           end if
           ! case kd=1:
           ! the matrix is already tridiagonal. we have to make diagonal
           ! and offdiagonal elements real, and store them in d and e.
           ! for that, for real precision just copy the diag and offdiag
           ! to d and e while for the complex case the bulge chasing is
           ! performed to convert the hermetian tridiagonal to symmetric
           ! tridiagonal. a simpler conversion formula might be used, but then
           ! updating the q matrix will be required and based if q is generated
           ! or not this might complicate the story.
           if( kd==1_ilp64 ) then
               do i = 1, n
                   d( i ) = real( ab( abdpos, i ),KIND=dp)
               end do
               ! make off-diagonal elements real and copy them to e
               if( upper ) then
                   do i = 1, n - 1
                       tmp = ab( abofdpos, i+1 )
                       abstmp = abs( tmp )
                       ab( abofdpos, i+1 ) = abstmp
                       e( i ) = abstmp
                       if( abstmp/=zero ) then
                          tmp = tmp / abstmp
                       else
                          tmp = cone
                       end if
                       if( i<n-1 )ab( abofdpos, i+2 ) = ab( abofdpos, i+2 )*tmp
                        ! if( wantz ) then
                           ! call stdlib_I64_zscal( n, conjg( tmp ), q( 1, i+1 ), 1 )
                        ! end if
                   end do
               else
                   do i = 1, n - 1
                      tmp = ab( abofdpos, i )
                      abstmp = abs( tmp )
                      ab( abofdpos, i ) = abstmp
                      e( i ) = abstmp
                      if( abstmp/=zero ) then
                         tmp = tmp / abstmp
                      else
                         tmp = cone
                      end if
                      if( i<n-1 )ab( abofdpos, i+1 ) = ab( abofdpos, i+1 )*tmp
                       ! if( wantq ) then
                          ! call stdlib_I64_zscal( n, tmp, q( 1, i+1 ), 1 )
                       ! end if
                   end do
               endif
               hous( 1_ilp64 ) = 1_ilp64
               work( 1_ilp64 ) = 1_ilp64
               return
           end if
           ! main code start here.
           ! reduce the hermitian band of a to a tridiagonal matrix.
           thgrsiz   = n
           grsiz     = 1_ilp64
           shift     = 3_ilp64
           nbtiles   = ceiling( real(n,KIND=dp)/real(kd,KIND=dp) )
           stepercol = ceiling( real(shift,KIND=dp)/real(grsiz,KIND=dp) )
           thgrnb    = ceiling( real(n-1,KIND=dp)/real(thgrsiz,KIND=dp) )
           call stdlib_I64_zlacpy( "A", kd+1, n, ab, ldab, work( apos ), lda )
           call stdlib_I64_zlaset( "A", kd,   n, czero, czero, work( awpos ), lda )
           ! openmp parallelisation start here
           !$OMP PARALLEL PRIVATE( TID, THGRID, BLKLASTIND ) &
           !$OMP&         PRIVATE( THED, I, M, K, ST, ED, STT, SWEEPID ) &
           !$OMP&         PRIVATE( MYID, TTYPE, COLPT, STIND, EDIND ) &
           !$OMP&         SHARED ( UPLO, WANTQ, INDV, INDTAU, HOUS, WORK) &
           !$OMP&         SHARED ( N, KD, IB, NBTILES, LDA, LDV, INDA ) &
           !$OMP&         SHARED ( STEPERCOL, THGRNB, THGRSIZ, GRSIZ, SHIFT )
           !$OMP MASTER
           ! main bulge chasing loop
           loop_100: do thgrid = 1, thgrnb
               stt  = (thgrid-1)*thgrsiz+1
               thed = min( (stt + thgrsiz -1_ilp64), (n-1))
               loop_110: do i = stt, n-1
                   ed = min( i, thed )
                   if( stt>ed ) exit
                   loop_120: do m = 1, stepercol
                       st = stt
                       loop_130: do sweepid = st, ed
                           loop_140: do k = 1, grsiz
                               myid  = (i-sweepid)*(stepercol*grsiz)+ (m-1)*grsiz + k
                               if ( myid==1_ilp64 ) then
                                   ttype = 1_ilp64
                               else
                                   ttype = mod( myid, 2_ilp64 ) + 2_ilp64
                               endif
                               if( ttype==2_ilp64 ) then
                                   colpt      = (myid/2_ilp64)*kd + sweepid
                                   stind      = colpt-kd+1
                                   edind      = min(colpt,n)
                                   blklastind = colpt
                               else
                                   colpt      = ((myid+1)/2_ilp64)*kd + sweepid
                                   stind      = colpt-kd+1
                                   edind      = min(colpt,n)
                                   if( ( stind>=edind-1 ).and.( edind==n ) ) then
                                       blklastind = n
                                   else
                                       blklastind = 0_ilp64
                                   endif
                               endif
                               ! call the kernel
                               !$ if( ttype/=1 ) then
                                   !$OMP TASK DEPEND(in:WORK(MYID+SHIFT-1)) &
                                   !$OMP&     DEPEND(in:WORK(MYID-1)) &
                                   !$OMP&     DEPEND(out:WORK(MYID))
                                   !$ tid      = omp_get_thread_num()
                                   !$ call stdlib_I64_zhb2st_kernels( uplo, wantq, ttype,stind, edind, &
                                   !$ sweepid, n, kd, ib,work ( inda ), lda,hous( indv ), hous( &
                                   !$ indtau ), ldv,work( indw + tid*kd ) )
                                   !$OMP END TASK
                               !$ else
                                   !$OMP TASK DEPEND(in:WORK(MYID+SHIFT-1)) &
                                   !$OMP&     DEPEND(out:WORK(MYID))
                                   !$ tid      = omp_get_thread_num()
                                   call stdlib_I64_zhb2st_kernels( uplo, wantq, ttype,stind, edind, &
                                   sweepid, n, kd, ib,work ( inda ), lda,hous( indv ), hous( &
                                             indtau ), ldv,work( indw + tid*kd ) )
                               !$OMP END TASK
                               !$ endif
                               if ( blklastind>=(n-1) ) then
                                   stt = stt + 1_ilp64
                                   exit
                               endif
                           end do loop_140
                       end do loop_130
                   end do loop_120
               end do loop_110
           end do loop_100
           !$OMP END MASTER
           !$OMP END PARALLEL
           ! copy the diagonal from a to d. note that d is real thus only
           ! the real part is needed, the imaginary part should be czero.
           do i = 1, n
               d( i ) = real( work( dpos+(i-1)*lda ),KIND=dp)
           end do
           ! copy the off diagonal from a to e. note that e is real thus only
           ! the real part is needed, the imaginary part should be czero.
           if( upper ) then
               do i = 1, n-1
                  e( i ) = real( work( ofdpos+i*lda ),KIND=dp)
               end do
           else
               do i = 1, n-1
                  e( i ) = real( work( ofdpos+(i-1)*lda ),KIND=dp)
               end do
           endif
           hous( 1_ilp64 ) = lhmin
           work( 1_ilp64 ) = lwmin
           return
     end subroutine stdlib_I64_zhetrd_hb2st




     pure module subroutine stdlib_I64_chb2st_kernels( uplo, wantz, ttype,st, ed, sweep, n, nb, ib,a, lda, &
     !! CHB2ST_KERNELS is an internal routine used by the CHETRD_HB2ST
     !! subroutine.
               v, tau, ldvt, work)
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           logical(lk), intent(in) :: wantz
           integer(ilp64), intent(in) :: ttype, st, ed, sweep, n, nb, ib, lda, ldvt
           ! Array Arguments 
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: v(*), tau(*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp64) :: i, j1, j2, lm, ln, vpos, taupos, dpos, ofdpos, ajeter
           complex(sp) :: ctmp
           ! Intrinsic Functions 
           ! Executable Statements 
           ajeter = ib + ldvt
           upper = stdlib_lsame( uplo, 'U' )
           if( upper ) then
               dpos    = 2_ilp64 * nb + 1_ilp64
               ofdpos  = 2_ilp64 * nb
           else
               dpos    = 1_ilp64
               ofdpos  = 2_ilp64
           endif
           ! upper case
           if( upper ) then
               if( wantz ) then
                   vpos   = mod( sweep-1, 2_ilp64 ) * n + st
                   taupos = mod( sweep-1, 2_ilp64 ) * n + st
               else
                   vpos   = mod( sweep-1, 2_ilp64 ) * n + st
                   taupos = mod( sweep-1, 2_ilp64 ) * n + st
               endif
               if( ttype==1_ilp64 ) then
                   lm = ed - st + 1_ilp64
                   v( vpos ) = cone
                   do i = 1, lm-1
                       v( vpos+i )         = conjg( a( ofdpos-i, st+i ) )
                       a( ofdpos-i, st+i ) = czero
                   end do
                   ctmp = conjg( a( ofdpos, st ) )
                   call stdlib_I64_clarfg( lm, ctmp, v( vpos+1 ), 1_ilp64,tau( taupos ) )
                   a( ofdpos, st ) = ctmp
                   lm = ed - st + 1_ilp64
                   call stdlib_I64_clarfy( uplo, lm, v( vpos ), 1_ilp64,conjg( tau( taupos ) ),a( dpos, st )&
                             , lda-1, work)
               endif
               if( ttype==3_ilp64 ) then
                   lm = ed - st + 1_ilp64
                   call stdlib_I64_clarfy( uplo, lm, v( vpos ), 1_ilp64,conjg( tau( taupos ) ),a( dpos, st )&
                             , lda-1, work)
               endif
               if( ttype==2_ilp64 ) then
                   j1 = ed+1
                   j2 = min( ed+nb, n )
                   ln = ed-st+1
                   lm = j2-j1+1
                   if( lm>0_ilp64) then
                       call stdlib_I64_clarfx( 'LEFT', ln, lm, v( vpos ),conjg( tau( taupos ) ),a( &
                                 dpos-nb, j1 ), lda-1, work)
                       if( wantz ) then
                           vpos   = mod( sweep-1, 2_ilp64 ) * n + j1
                           taupos = mod( sweep-1, 2_ilp64 ) * n + j1
                       else
                           vpos   = mod( sweep-1, 2_ilp64 ) * n + j1
                           taupos = mod( sweep-1, 2_ilp64 ) * n + j1
                       endif
                       v( vpos ) = cone
                       do i = 1, lm-1
                           v( vpos+i )          =conjg( a( dpos-nb-i, j1+i ) )
                           a( dpos-nb-i, j1+i ) = czero
                       end do
                       ctmp = conjg( a( dpos-nb, j1 ) )
                       call stdlib_I64_clarfg( lm, ctmp, v( vpos+1 ), 1_ilp64, tau( taupos ) )
                       a( dpos-nb, j1 ) = ctmp
                       call stdlib_I64_clarfx( 'RIGHT', ln-1, lm, v( vpos ),tau( taupos ),a( dpos-nb+&
                                 1_ilp64, j1 ), lda-1, work)
                   endif
               endif
           ! lower case
           else
               if( wantz ) then
                   vpos   = mod( sweep-1, 2_ilp64 ) * n + st
                   taupos = mod( sweep-1, 2_ilp64 ) * n + st
               else
                   vpos   = mod( sweep-1, 2_ilp64 ) * n + st
                   taupos = mod( sweep-1, 2_ilp64 ) * n + st
               endif
               if( ttype==1_ilp64 ) then
                   lm = ed - st + 1_ilp64
                   v( vpos ) = cone
                   do i = 1, lm-1
                       v( vpos+i )         = a( ofdpos+i, st-1 )
                       a( ofdpos+i, st-1 ) = czero
                   end do
                   call stdlib_I64_clarfg( lm, a( ofdpos, st-1 ), v( vpos+1 ), 1_ilp64,tau( taupos ) )
                             
                   lm = ed - st + 1_ilp64
                   call stdlib_I64_clarfy( uplo, lm, v( vpos ), 1_ilp64,conjg( tau( taupos ) ),a( dpos, st )&
                             , lda-1, work)
               endif
               if( ttype==3_ilp64 ) then
                   lm = ed - st + 1_ilp64
                   call stdlib_I64_clarfy( uplo, lm, v( vpos ), 1_ilp64,conjg( tau( taupos ) ),a( dpos, st )&
                             , lda-1, work)
               endif
               if( ttype==2_ilp64 ) then
                   j1 = ed+1
                   j2 = min( ed+nb, n )
                   ln = ed-st+1
                   lm = j2-j1+1
                   if( lm>0_ilp64) then
                       call stdlib_I64_clarfx( 'RIGHT', lm, ln, v( vpos ),tau( taupos ), a( dpos+nb, &
                                 st ),lda-1, work)
                       if( wantz ) then
                           vpos   = mod( sweep-1, 2_ilp64 ) * n + j1
                           taupos = mod( sweep-1, 2_ilp64 ) * n + j1
                       else
                           vpos   = mod( sweep-1, 2_ilp64 ) * n + j1
                           taupos = mod( sweep-1, 2_ilp64 ) * n + j1
                       endif
                       v( vpos ) = cone
                       do i = 1, lm-1
                           v( vpos+i )        = a( dpos+nb+i, st )
                           a( dpos+nb+i, st ) = czero
                       end do
                       call stdlib_I64_clarfg( lm, a( dpos+nb, st ), v( vpos+1 ), 1_ilp64,tau( taupos ) )
                                 
                       call stdlib_I64_clarfx( 'LEFT', lm, ln-1, v( vpos ),conjg( tau( taupos ) ),a( &
                                 dpos+nb-1, st+1 ), lda-1, work)
                   endif
               endif
           endif
           return
     end subroutine stdlib_I64_chb2st_kernels

     pure module subroutine stdlib_I64_zhb2st_kernels( uplo, wantz, ttype,st, ed, sweep, n, nb, ib,a, lda, &
     !! ZHB2ST_KERNELS is an internal routine used by the ZHETRD_HB2ST
     !! subroutine.
               v, tau, ldvt, work)
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           logical(lk), intent(in) :: wantz
           integer(ilp64), intent(in) :: ttype, st, ed, sweep, n, nb, ib, lda, ldvt
           ! Array Arguments 
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: v(*), tau(*), work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: upper
           integer(ilp64) :: i, j1, j2, lm, ln, vpos, taupos, dpos, ofdpos, ajeter
           complex(dp) :: ctmp
           ! Intrinsic Functions 
           ! Executable Statements 
           ajeter = ib + ldvt
           upper = stdlib_lsame( uplo, 'U' )
           if( upper ) then
               dpos    = 2_ilp64 * nb + 1_ilp64
               ofdpos  = 2_ilp64 * nb
           else
               dpos    = 1_ilp64
               ofdpos  = 2_ilp64
           endif
           ! upper case
           if( upper ) then
               if( wantz ) then
                   vpos   = mod( sweep-1, 2_ilp64 ) * n + st
                   taupos = mod( sweep-1, 2_ilp64 ) * n + st
               else
                   vpos   = mod( sweep-1, 2_ilp64 ) * n + st
                   taupos = mod( sweep-1, 2_ilp64 ) * n + st
               endif
               if( ttype==1_ilp64 ) then
                   lm = ed - st + 1_ilp64
                   v( vpos ) = cone
                   do i = 1, lm-1
                       v( vpos+i )         = conjg( a( ofdpos-i, st+i ) )
                       a( ofdpos-i, st+i ) = czero
                   end do
                   ctmp = conjg( a( ofdpos, st ) )
                   call stdlib_I64_zlarfg( lm, ctmp, v( vpos+1 ), 1_ilp64,tau( taupos ) )
                   a( ofdpos, st ) = ctmp
                   lm = ed - st + 1_ilp64
                   call stdlib_I64_zlarfy( uplo, lm, v( vpos ), 1_ilp64,conjg( tau( taupos ) ),a( dpos, st )&
                             , lda-1, work)
               endif
               if( ttype==3_ilp64 ) then
                   lm = ed - st + 1_ilp64
                   call stdlib_I64_zlarfy( uplo, lm, v( vpos ), 1_ilp64,conjg( tau( taupos ) ),a( dpos, st )&
                             , lda-1, work)
               endif
               if( ttype==2_ilp64 ) then
                   j1 = ed+1
                   j2 = min( ed+nb, n )
                   ln = ed-st+1
                   lm = j2-j1+1
                   if( lm>0_ilp64) then
                       call stdlib_I64_zlarfx( 'LEFT', ln, lm, v( vpos ),conjg( tau( taupos ) ),a( &
                                 dpos-nb, j1 ), lda-1, work)
                       if( wantz ) then
                           vpos   = mod( sweep-1, 2_ilp64 ) * n + j1
                           taupos = mod( sweep-1, 2_ilp64 ) * n + j1
                       else
                           vpos   = mod( sweep-1, 2_ilp64 ) * n + j1
                           taupos = mod( sweep-1, 2_ilp64 ) * n + j1
                       endif
                       v( vpos ) = cone
                       do i = 1, lm-1
                           v( vpos+i )          =conjg( a( dpos-nb-i, j1+i ) )
                           a( dpos-nb-i, j1+i ) = czero
                       end do
                       ctmp = conjg( a( dpos-nb, j1 ) )
                       call stdlib_I64_zlarfg( lm, ctmp, v( vpos+1 ), 1_ilp64, tau( taupos ) )
                       a( dpos-nb, j1 ) = ctmp
                       call stdlib_I64_zlarfx( 'RIGHT', ln-1, lm, v( vpos ),tau( taupos ),a( dpos-nb+&
                                 1_ilp64, j1 ), lda-1, work)
                   endif
               endif
           ! lower case
           else
               if( wantz ) then
                   vpos   = mod( sweep-1, 2_ilp64 ) * n + st
                   taupos = mod( sweep-1, 2_ilp64 ) * n + st
               else
                   vpos   = mod( sweep-1, 2_ilp64 ) * n + st
                   taupos = mod( sweep-1, 2_ilp64 ) * n + st
               endif
               if( ttype==1_ilp64 ) then
                   lm = ed - st + 1_ilp64
                   v( vpos ) = cone
                   do i = 1, lm-1
                       v( vpos+i )         = a( ofdpos+i, st-1 )
                       a( ofdpos+i, st-1 ) = czero
                   end do
                   call stdlib_I64_zlarfg( lm, a( ofdpos, st-1 ), v( vpos+1 ), 1_ilp64,tau( taupos ) )
                             
                   lm = ed - st + 1_ilp64
                   call stdlib_I64_zlarfy( uplo, lm, v( vpos ), 1_ilp64,conjg( tau( taupos ) ),a( dpos, st )&
                             , lda-1, work)
               endif
               if( ttype==3_ilp64 ) then
                   lm = ed - st + 1_ilp64
                   call stdlib_I64_zlarfy( uplo, lm, v( vpos ), 1_ilp64,conjg( tau( taupos ) ),a( dpos, st )&
                             , lda-1, work)
               endif
               if( ttype==2_ilp64 ) then
                   j1 = ed+1
                   j2 = min( ed+nb, n )
                   ln = ed-st+1
                   lm = j2-j1+1
                   if( lm>0_ilp64) then
                       call stdlib_I64_zlarfx( 'RIGHT', lm, ln, v( vpos ),tau( taupos ), a( dpos+nb, &
                                 st ),lda-1, work)
                       if( wantz ) then
                           vpos   = mod( sweep-1, 2_ilp64 ) * n + j1
                           taupos = mod( sweep-1, 2_ilp64 ) * n + j1
                       else
                           vpos   = mod( sweep-1, 2_ilp64 ) * n + j1
                           taupos = mod( sweep-1, 2_ilp64 ) * n + j1
                       endif
                       v( vpos ) = cone
                       do i = 1, lm-1
                           v( vpos+i )        = a( dpos+nb+i, st )
                           a( dpos+nb+i, st ) = czero
                       end do
                       call stdlib_I64_zlarfg( lm, a( dpos+nb, st ), v( vpos+1 ), 1_ilp64,tau( taupos ) )
                                 
                       call stdlib_I64_zlarfx( 'LEFT', lm, ln-1, v( vpos ),conjg( tau( taupos ) ),a( &
                                 dpos+nb-1, st+1 ), lda-1, work)
                   endif
               endif
           endif
           return
     end subroutine stdlib_I64_zhb2st_kernels




     module subroutine stdlib_I64_ssytrd_sb2st( stage1, vect, uplo, n, kd, ab, ldab,d, e, hous, lhous, &
     !! SSYTRD_SB2ST reduces a real symmetric band matrix A to real symmetric
     !! tridiagonal form T by a orthogonal similarity transformation:
     !! Q**T * A * Q = T.
               work, lwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: stage1, uplo, vect
           integer(ilp64), intent(in) :: n, kd, ldab, lhous, lwork
           integer(ilp64), intent(out) :: info
           ! Array Arguments 
           real(sp), intent(out) :: d(*), e(*)
           real(sp), intent(inout) :: ab(ldab,*)
           real(sp), intent(out) :: hous(*), work(*)
        ! =====================================================================
           ! Parameters 
           
           ! Local Scalars 
           logical(lk) :: lquery, wantq, upper, afters1
           integer(ilp64) :: i, m, k, ib, sweepid, myid, shift, stt, st, ed, stind, edind, &
           blklastind, colpt, thed, stepercol, grsiz, thgrsiz, thgrnb, thgrid, nbtiles, ttype, &
           tid, nthreads, debug, abdpos, abofdpos, dpos, ofdpos, awpos, inda, indw, apos, sizea, &
                     lda, indv, indtau, sisev, sizetau, ldv, lhmin, lwmin
           ! Intrinsic Functions 
           ! Executable Statements 
           ! determine the minimal workspace size required.
           ! test the input parameters
           debug   = 0_ilp64
           info    = 0_ilp64
           afters1 = stdlib_lsame( stage1, 'Y' )
           wantq   = stdlib_lsame( vect, 'V' )
           upper   = stdlib_lsame( uplo, 'U' )
           lquery  = ( lwork==-1_ilp64 ) .or. ( lhous==-1_ilp64 )
           ! determine the block size, the workspace size and the hous size.
           ib     = stdlib_I64_ilaenv2stage( 2_ilp64, 'SSYTRD_SB2ST', vect, n, kd, -1_ilp64, -1_ilp64 )
           lhmin  = stdlib_I64_ilaenv2stage( 3_ilp64, 'SSYTRD_SB2ST', vect, n, kd, ib, -1_ilp64 )
           lwmin  = stdlib_I64_ilaenv2stage( 4_ilp64, 'SSYTRD_SB2ST', vect, n, kd, ib, -1_ilp64 )
           if( .not.afters1 .and. .not.stdlib_lsame( stage1, 'N' ) ) then
              info = -1_ilp64
           else if( .not.stdlib_lsame( vect, 'N' ) ) then
              info = -2_ilp64
           else if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -3_ilp64
           else if( n<0_ilp64 ) then
              info = -4_ilp64
           else if( kd<0_ilp64 ) then
              info = -5_ilp64
           else if( ldab<(kd+1) ) then
              info = -7_ilp64
           else if( lhous<lhmin .and. .not.lquery ) then
              info = -11_ilp64
           else if( lwork<lwmin .and. .not.lquery ) then
              info = -13_ilp64
           end if
           if( info==0_ilp64 ) then
              hous( 1_ilp64 ) = lhmin
              work( 1_ilp64 ) = lwmin
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'SSYTRD_SB2ST', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0_ilp64 ) then
               hous( 1_ilp64 ) = 1_ilp64
               work( 1_ilp64 ) = 1_ilp64
               return
           end if
           ! determine pointer position
           ldv      = kd + ib
           sizetau  = 2_ilp64 * n
           sisev    = 2_ilp64 * n
           indtau   = 1_ilp64
           indv     = indtau + sizetau
           lda      = 2_ilp64 * kd + 1_ilp64
           sizea    = lda * n
           inda     = 1_ilp64
           indw     = inda + sizea
           nthreads = 1_ilp64
           tid      = 0_ilp64
           if( upper ) then
               apos     = inda + kd
               awpos    = inda
               dpos     = apos + kd
               ofdpos   = dpos - 1_ilp64
               abdpos   = kd + 1_ilp64
               abofdpos = kd
           else
               apos     = inda
               awpos    = inda + kd + 1_ilp64
               dpos     = apos
               ofdpos   = dpos + 1_ilp64
               abdpos   = 1_ilp64
               abofdpos = 2_ilp64
           endif
           ! case kd=0:
           ! the matrix is diagonal. we just copy it (convert to "real" for
           ! real because d is double and the imaginary part should be 0)
           ! and store it in d. a sequential code here is better or
           ! in a parallel environment it might need two cores for d and e
           if( kd==0_ilp64 ) then
               do i = 1, n
                   d( i ) = ( ab( abdpos, i ) )
               end do
               do i = 1, n-1
                   e( i ) = zero
               end do
               hous( 1_ilp64 ) = 1_ilp64
               work( 1_ilp64 ) = 1_ilp64
               return
           end if
           ! case kd=1:
           ! the matrix is already tridiagonal. we have to make diagonal
           ! and offdiagonal elements real, and store them in d and e.
           ! for that, for real precision just copy the diag and offdiag
           ! to d and e while for the complex case the bulge chasing is
           ! performed to convert the hermetian tridiagonal to symmetric
           ! tridiagonal. a simpler conversion formula might be used, but then
           ! updating the q matrix will be required and based if q is generated
           ! or not this might complicate the story.
           if( kd==1_ilp64 ) then
               do i = 1, n
                   d( i ) = ( ab( abdpos, i ) )
               end do
               if( upper ) then
                   do i = 1, n-1
                      e( i ) = ( ab( abofdpos, i+1 ) )
                   end do
               else
                   do i = 1, n-1
                      e( i ) = ( ab( abofdpos, i ) )
                   end do
               endif
               hous( 1_ilp64 ) = 1_ilp64
               work( 1_ilp64 ) = 1_ilp64
               return
           end if
           ! main code start here.
           ! reduce the symmetric band of a to a tridiagonal matrix.
           thgrsiz   = n
           grsiz     = 1_ilp64
           shift     = 3_ilp64
           nbtiles   = ceiling( real(n,KIND=sp)/real(kd,KIND=sp) )
           stepercol = ceiling( real(shift,KIND=sp)/real(grsiz,KIND=sp) )
           thgrnb    = ceiling( real(n-1,KIND=sp)/real(thgrsiz,KIND=sp) )
           call stdlib_I64_slacpy( "A", kd+1, n, ab, ldab, work( apos ), lda )
           call stdlib_I64_slaset( "A", kd,   n, zero, zero, work( awpos ), lda )
           ! openmp parallelisation start here
           !$OMP PARALLEL PRIVATE( TID, THGRID, BLKLASTIND ) &
           !$OMP&         PRIVATE( THED, I, M, K, ST, ED, STT, SWEEPID ) &
           !$OMP&         PRIVATE( MYID, TTYPE, COLPT, STIND, EDIND ) &
           !$OMP&         SHARED ( UPLO, WANTQ, INDV, INDTAU, HOUS, WORK) &
           !$OMP&         SHARED ( N, KD, IB, NBTILES, LDA, LDV, INDA ) &
           !$OMP&         SHARED ( STEPERCOL, THGRNB, THGRSIZ, GRSIZ, SHIFT )
           !$OMP MASTER
           ! main bulge chasing loop
           loop_100: do thgrid = 1, thgrnb
               stt  = (thgrid-1)*thgrsiz+1
               thed = min( (stt + thgrsiz -1_ilp64), (n-1))
               loop_110: do i = stt, n-1
                   ed = min( i, thed )
                   if( stt>ed ) exit
                   loop_120: do m = 1, stepercol
                       st = stt
                       loop_130: do sweepid = st, ed
                           loop_140: do k = 1, grsiz
                               myid  = (i-sweepid)*(stepercol*grsiz)+ (m-1)*grsiz + k
                               if ( myid==1_ilp64 ) then
                                   ttype = 1_ilp64
                               else
                                   ttype = mod( myid, 2_ilp64 ) + 2_ilp64
                               endif
                               if( ttype==2_ilp64 ) then
                                   colpt      = (myid/2_ilp64)*kd + sweepid
                                   stind      = colpt-kd+1
                                   edind      = min(colpt,n)
                                   blklastind = colpt
                               else
                                   colpt      = ((myid+1)/2_ilp64)*kd + sweepid
                                   stind      = colpt-kd+1
                                   edind      = min(colpt,n)
                                   if( ( stind>=edind-1 ).and.( edind==n ) ) then
                                       blklastind = n
                                   else
                                       blklastind = 0_ilp64
                                   endif
                               endif
                               ! call the kernel
                               !$ if( ttype/=1 ) then
                                   !$OMP TASK DEPEND(in:WORK(MYID+SHIFT-1)) &
                                   !$OMP&     DEPEND(in:WORK(MYID-1)) &
                                   !$OMP&     DEPEND(out:WORK(MYID))
                                   !$ tid      = omp_get_thread_num()
                                   !$ call stdlib_I64_ssb2st_kernels( uplo, wantq, ttype,stind, edind, &
                                   !$ sweepid, n, kd, ib,work ( inda ), lda,hous( indv ), hous( &
                                   !$ indtau ), ldv,work( indw + tid*kd ) )
                                   !$OMP END TASK
                               !$ else
                                   !$OMP TASK DEPEND(in:WORK(MYID+SHIFT-1)) &
                                   !$OMP&     DEPEND(out:WORK(MYID))
                                   !$ tid      = omp_get_thread_num()
                                   call stdlib_I64_ssb2st_kernels( uplo, wantq, ttype,stind, edind, &
                                   sweepid, n, kd, ib,work ( inda ), lda,hous( indv ), hous( &
                                             indtau ), ldv,work( indw + tid*kd ) )
                                   !$OMP END TASK
                               !$ endif
                               if ( blklastind>=(n-1) ) then
                                   stt = stt + 1_ilp64
                                   exit
                               endif
                           end do loop_140
                       end do loop_130
                   end do loop_120
               end do loop_110
           end do loop_100
           !$OMP END MASTER
           !$OMP END PARALLEL
           ! copy the diagonal from a to d. note that d is real thus only
           ! the real part is needed, the imaginary part should be zero.
           do i = 1, n
               d( i ) = ( work( dpos+(i-1)*lda ) )
           end do
           ! copy the off diagonal from a to e. note that e is real thus only
           ! the real part is needed, the imaginary part should be zero.
           if( upper ) then
               do i = 1, n-1
                  e( i ) = ( work( ofdpos+i*lda ) )
               end do
           else
               do i = 1, n-1
                  e( i ) = ( work( ofdpos+(i-1)*lda ) )
               end do
           endif
           hous( 1_ilp64 ) = lhmin
           work( 1_ilp64 ) = lwmin
           return
     end subroutine stdlib_I64_ssytrd_sb2st

     module subroutine stdlib_I64_dsytrd_sb2st( stage1, vect, uplo, n, kd, ab, ldab,d, e, hous, lhous, &
     !! DSYTRD_SB2ST reduces a real symmetric band matrix A to real symmetric
     !! tridiagonal form T by a orthogonal similarity transformation:
     !! Q**T * A * Q = T.
               work, lwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: stage1, uplo, vect
           integer(ilp64), intent(in) :: n, kd, ldab, lhous, lwork
           integer(ilp64), intent(out) :: info
           ! Array Arguments 
           real(dp), intent(out) :: d(*), e(*)
           real(dp), intent(inout) :: ab(ldab,*)
           real(dp), intent(out) :: hous(*), work(*)
        ! =====================================================================
           ! Parameters 
           
           ! Local Scalars 
           logical(lk) :: lquery, wantq, upper, afters1
           integer(ilp64) :: i, m, k, ib, sweepid, myid, shift, stt, st, ed, stind, edind, &
           blklastind, colpt, thed, stepercol, grsiz, thgrsiz, thgrnb, thgrid, nbtiles, ttype, &
           tid, nthreads, debug, abdpos, abofdpos, dpos, ofdpos, awpos, inda, indw, apos, sizea, &
                     lda, indv, indtau, sidev, sizetau, ldv, lhmin, lwmin
           ! Intrinsic Functions 
           ! Executable Statements 
           ! determine the minimal workspace size required.
           ! test the input parameters
           debug   = 0_ilp64
           info    = 0_ilp64
           afters1 = stdlib_lsame( stage1, 'Y' )
           wantq   = stdlib_lsame( vect, 'V' )
           upper   = stdlib_lsame( uplo, 'U' )
           lquery  = ( lwork==-1_ilp64 ) .or. ( lhous==-1_ilp64 )
           ! determine the block size, the workspace size and the hous size.
           ib     = stdlib_I64_ilaenv2stage( 2_ilp64, 'DSYTRD_SB2ST', vect, n, kd, -1_ilp64, -1_ilp64 )
           lhmin  = stdlib_I64_ilaenv2stage( 3_ilp64, 'DSYTRD_SB2ST', vect, n, kd, ib, -1_ilp64 )
           lwmin  = stdlib_I64_ilaenv2stage( 4_ilp64, 'DSYTRD_SB2ST', vect, n, kd, ib, -1_ilp64 )
           if( .not.afters1 .and. .not.stdlib_lsame( stage1, 'N' ) ) then
              info = -1_ilp64
           else if( .not.stdlib_lsame( vect, 'N' ) ) then
              info = -2_ilp64
           else if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -3_ilp64
           else if( n<0_ilp64 ) then
              info = -4_ilp64
           else if( kd<0_ilp64 ) then
              info = -5_ilp64
           else if( ldab<(kd+1) ) then
              info = -7_ilp64
           else if( lhous<lhmin .and. .not.lquery ) then
              info = -11_ilp64
           else if( lwork<lwmin .and. .not.lquery ) then
              info = -13_ilp64
           end if
           if( info==0_ilp64 ) then
              hous( 1_ilp64 ) = lhmin
              work( 1_ilp64 ) = lwmin
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'DSYTRD_SB2ST', -info )
              return
           else if( lquery ) then
              return
           end if
           ! quick return if possible
           if( n==0_ilp64 ) then
               hous( 1_ilp64 ) = 1_ilp64
               work( 1_ilp64 ) = 1_ilp64
               return
           end if
           ! determine pointer position
           ldv      = kd + ib
           sizetau  = 2_ilp64 * n
           sidev    = 2_ilp64 * n
           indtau   = 1_ilp64
           indv     = indtau + sizetau
           lda      = 2_ilp64 * kd + 1_ilp64
           sizea    = lda * n
           inda     = 1_ilp64
           indw     = inda + sizea
           nthreads = 1_ilp64
           tid      = 0_ilp64
           if( upper ) then
               apos     = inda + kd
               awpos    = inda
               dpos     = apos + kd
               ofdpos   = dpos - 1_ilp64
               abdpos   = kd + 1_ilp64
               abofdpos = kd
           else
               apos     = inda
               awpos    = inda + kd + 1_ilp64
               dpos     = apos
               ofdpos   = dpos + 1_ilp64
               abdpos   = 1_ilp64
               abofdpos = 2_ilp64
           endif
           ! case kd=0:
           ! the matrix is diagonal. we just copy it (convert to "real" for
           ! real because d is double and the imaginary part should be 0)
           ! and store it in d. a sequential code here is better or
           ! in a parallel environment it might need two cores for d and e
           if( kd==0_ilp64 ) then
               do i = 1, n
                   d( i ) = ( ab( abdpos, i ) )
               end do
               do i = 1, n-1
                   e( i ) = zero
               end do
               hous( 1_ilp64 ) = 1_ilp64
               work( 1_ilp64 ) = 1_ilp64
               return
           end if
           ! case kd=1:
           ! the matrix is already tridiagonal. we have to make diagonal
           ! and offdiagonal elements real, and store them in d and e.
           ! for that, for real precision just copy the diag and offdiag
           ! to d and e while for the complex case the bulge chasing is
           ! performed to convert the hermetian tridiagonal to symmetric
           ! tridiagonal. a simpler conversion formula might be used, but then
           ! updating the q matrix will be required and based if q is generated
           ! or not this might complicate the story.
           if( kd==1_ilp64 ) then
               do i = 1, n
                   d( i ) = ( ab( abdpos, i ) )
               end do
               if( upper ) then
                   do i = 1, n-1
                      e( i ) = ( ab( abofdpos, i+1 ) )
                   end do
               else
                   do i = 1, n-1
                      e( i ) = ( ab( abofdpos, i ) )
                   end do
               endif
               hous( 1_ilp64 ) = 1_ilp64
               work( 1_ilp64 ) = 1_ilp64
               return
           end if
           ! main code start here.
           ! reduce the symmetric band of a to a tridiagonal matrix.
           thgrsiz   = n
           grsiz     = 1_ilp64
           shift     = 3_ilp64
           nbtiles   = ceiling( real(n,KIND=dp)/real(kd,KIND=dp) )
           stepercol = ceiling( real(shift,KIND=dp)/real(grsiz,KIND=dp) )
           thgrnb    = ceiling( real(n-1,KIND=dp)/real(thgrsiz,KIND=dp) )
           call stdlib_I64_dlacpy( "A", kd+1, n, ab, ldab, work( apos ), lda )
           call stdlib_I64_dlaset( "A", kd,   n, zero, zero, work( awpos ), lda )
           ! openmp parallelisation start here
           !$OMP PARALLEL PRIVATE( TID, THGRID, BLKLASTIND ) &
           !$OMP&         PRIVATE( THED, I, M, K, ST, ED, STT, SWEEPID ) &
           !$OMP&         PRIVATE( MYID, TTYPE, COLPT, STIND, EDIND ) &
           !$OMP&         SHARED ( UPLO, WANTQ, INDV, INDTAU, HOUS, WORK) &
           !$OMP&         SHARED ( N, KD, IB, NBTILES, LDA, LDV, INDA ) &
           !$OMP&         SHARED ( STEPERCOL, THGRNB, THGRSIZ, GRSIZ, SHIFT )
           !$OMP MASTER
           ! main bulge chasing loop
           loop_100: do thgrid = 1, thgrnb
               stt  = (thgrid-1)*thgrsiz+1
               thed = min( (stt + thgrsiz -1_ilp64), (n-1))
               loop_110: do i = stt, n-1
                   ed = min( i, thed )
                   if( stt>ed ) exit
                   loop_120: do m = 1, stepercol
                       st = stt
                       loop_130: do sweepid = st, ed
                           loop_140: do k = 1, grsiz
                               myid  = (i-sweepid)*(stepercol*grsiz)+ (m-1)*grsiz + k
                               if ( myid==1_ilp64 ) then
                                   ttype = 1_ilp64
                               else
                                   ttype = mod( myid, 2_ilp64 ) + 2_ilp64
                               endif
                               if( ttype==2_ilp64 ) then
                                   colpt      = (myid/2_ilp64)*kd + sweepid
                                   stind      = colpt-kd+1
                                   edind      = min(colpt,n)
                                   blklastind = colpt
                               else
                                   colpt      = ((myid+1)/2_ilp64)*kd + sweepid
                                   stind      = colpt-kd+1
                                   edind      = min(colpt,n)
                                   if( ( stind>=edind-1 ).and.( edind==n ) ) then
                                       blklastind = n
                                   else
                                       blklastind = 0_ilp64
                                   endif
                               endif
                               ! call the kernel
                               !$ if( ttype/=1 ) then
                                   !$OMP TASK DEPEND(in:WORK(MYID+SHIFT-1)) &
                                   !$OMP&     DEPEND(in:WORK(MYID-1)) &
                                   !$OMP&     DEPEND(out:WORK(MYID))
                                   !$ tid      = omp_get_thread_num()
                                   !$ call stdlib_I64_dsb2st_kernels( uplo, wantq, ttype,stind, edind, &
                                   !$ sweepid, n, kd, ib,work ( inda ), lda,hous( indv ), hous( &
                                   !$ indtau ), ldv,work( indw + tid*kd ) )
                               !$OMP END TASK
                                   !$ else
                                   !$OMP TASK DEPEND(in:WORK(MYID+SHIFT-1)) &
                                   !$OMP&     DEPEND(out:WORK(MYID))
                                   !$ tid      = omp_get_thread_num()
                                   call stdlib_I64_dsb2st_kernels( uplo, wantq, ttype,stind, edind, &
                                   sweepid, n, kd, ib,work ( inda ), lda,hous( indv ), hous( &
                                             indtau ), ldv,work( indw + tid*kd ) )
                                   !$OMP END TASK
                               !$ endif
                               if ( blklastind>=(n-1) ) then
                                   stt = stt + 1_ilp64
                                   exit
                               endif
                           end do loop_140
                       end do loop_130
                   end do loop_120
               end do loop_110
           end do loop_100
           !$OMP END MASTER
           !$OMP END PARALLEL
           ! copy the diagonal from a to d. note that d is real thus only
           ! the real part is needed, the imaginary part should be zero.
           do i = 1, n
               d( i ) = ( work( dpos+(i-1)*lda ) )
           end do
           ! copy the off diagonal from a to e. note that e is real thus only
           ! the real part is needed, the imaginary part should be zero.
           if( upper ) then
               do i = 1, n-1
                  e( i ) = ( work( ofdpos+i*lda ) )
               end do
           else
               do i = 1, n-1
                  e( i ) = ( work( ofdpos+(i-1)*lda ) )
               end do
           endif
           hous( 1_ilp64 ) = lhmin
           work( 1_ilp64 ) = lwmin
           return
     end subroutine stdlib_I64_dsytrd_sb2st




     module subroutine stdlib_I64_ssytrd_sy2sb( uplo, n, kd, a, lda, ab, ldab, tau,work, lwork, info )
     !! SSYTRD_SY2SB reduces a real symmetric matrix A to real symmetric
     !! band-diagonal form AB by a orthogonal similarity transformation:
     !! Q**T * A * Q = AB.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldab, lwork, n, kd
           ! Array Arguments 
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: ab(ldab,*), tau(*), work(*)
        ! =====================================================================
           ! Parameters 
           
           ! Local Scalars 
           logical(lk) :: lquery, upper
           integer(ilp64) :: i, j, iinfo, lwmin, pn, pk, llk, ldt, ldw, lds2, lds1, ls2, ls1, lw, lt,&
                      tpos, wpos, s2pos, s1pos
           ! Intrinsic Functions 
           ! Executable Statements 
           ! determine the minimal workspace size required
           ! and test the input parameters
           info   = 0_ilp64
           upper  = stdlib_lsame( uplo, 'U' )
           lquery = ( lwork==-1_ilp64 )
           lwmin  = stdlib_I64_ilaenv2stage( 4_ilp64, 'SSYTRD_SY2SB', '', n, kd, -1_ilp64, -1_ilp64 )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -2_ilp64
           else if( kd<0_ilp64 ) then
              info = -3_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -5_ilp64
           else if( ldab<max( 1_ilp64, kd+1 ) ) then
              info = -7_ilp64
           else if( lwork<lwmin .and. .not.lquery ) then
              info = -10_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'SSYTRD_SY2SB', -info )
              return
           else if( lquery ) then
              work( 1_ilp64 ) = lwmin
              return
           end if
           ! quick return if possible
           ! copy the upper/lower portion of a into ab
           if( n<=kd+1 ) then
               if( upper ) then
                   do i = 1, n
                       llk = min( kd+1, i )
                       call stdlib_I64_scopy( llk, a( i-llk+1, i ), 1_ilp64,ab( kd+1-llk+1, i ), 1_ilp64 )
                   end do
               else
                   do i = 1, n
                       llk = min( kd+1, n-i+1 )
                       call stdlib_I64_scopy( llk, a( i, i ), 1_ilp64, ab( 1_ilp64, i ), 1_ilp64 )
                   end do
               endif
               work( 1_ilp64 ) = 1_ilp64
               return
           end if
           ! determine the pointer position for the workspace
           ldt    = kd
           lds1   = kd
           lt     = ldt*kd
           lw     = n*kd
           ls1    = lds1*kd
           ls2    = lwmin - lt - lw - ls1
            ! ls2 = n*max(kd,factoptnb)
           tpos   = 1_ilp64
           wpos   = tpos  + lt
           s1pos  = wpos  + lw
           s2pos  = s1pos + ls1
           if( upper ) then
               ldw    = kd
               lds2   = kd
           else
               ldw    = n
               lds2   = n
           endif
           ! set the workspace of the triangular matrix t to zero once such a
           ! way every time t is generated the upper/lower portion will be always zero
           call stdlib_I64_slaset( "A", ldt, kd, zero, zero, work( tpos ), ldt )
           if( upper ) then
               do i = 1, n - kd, kd
                  pn = n-i-kd+1
                  pk = min( n-i-kd+1, kd )
                  ! compute the lq factorization of the current block
                  call stdlib_I64_sgelqf( kd, pn, a( i, i+kd ), lda,tau( i ), work( s2pos ), ls2, &
                            iinfo )
                  ! copy the upper portion of a into ab
                  do j = i, i+pk-1
                     llk = min( kd, n-j ) + 1_ilp64
                     call stdlib_I64_scopy( llk, a( j, j ), lda, ab( kd+1, j ), ldab-1 )
                  end do
                  call stdlib_I64_slaset( 'LOWER', pk, pk, zero, one,a( i, i+kd ), lda )
                  ! form the matrix t
                  call stdlib_I64_slarft( 'FORWARD', 'ROWWISE', pn, pk,a( i, i+kd ), lda, tau( i ),&
                            work( tpos ), ldt )
                  ! compute w:
                  call stdlib_I64_sgemm( 'CONJUGATE', 'NO TRANSPOSE', pk, pn, pk,one,  work( tpos ), &
                            ldt,a( i, i+kd ), lda,zero, work( s2pos ), lds2 )
                  call stdlib_I64_ssymm( 'RIGHT', uplo, pk, pn,one,  a( i+kd, i+kd ), lda,work( s2pos &
                            ), lds2,zero, work( wpos ), ldw )
                  call stdlib_I64_sgemm( 'NO TRANSPOSE', 'CONJUGATE', pk, pk, pn,one,  work( wpos ), &
                            ldw,work( s2pos ), lds2,zero, work( s1pos ), lds1 )
                  call stdlib_I64_sgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', pk, pn, pk,-half, work( &
                            s1pos ), lds1,a( i, i+kd ), lda,one,   work( wpos ), ldw )
                  ! update the unreduced submatrix a(i+kd:n,i+kd:n), using
                  ! an update of the form:  a := a - v'*w - w'*v
                  call stdlib_I64_ssyr2k( uplo, 'CONJUGATE', pn, pk,-one, a( i, i+kd ), lda,work( &
                            wpos ), ldw,one, a( i+kd, i+kd ), lda )
               end do
              ! copy the upper band to ab which is the band storage matrix
              do j = n-kd+1, n
                 llk = min(kd, n-j) + 1_ilp64
                 call stdlib_I64_scopy( llk, a( j, j ), lda, ab( kd+1, j ), ldab-1 )
              end do
           else
               ! reduce the lower triangle of a to lower band matrix
               loop_40: do i = 1, n - kd, kd
                  pn = n-i-kd+1
                  pk = min( n-i-kd+1, kd )
                  ! compute the qr factorization of the current block
                  call stdlib_I64_sgeqrf( pn, kd, a( i+kd, i ), lda,tau( i ), work( s2pos ), ls2, &
                            iinfo )
                  ! copy the upper portion of a into ab
                  do j = i, i+pk-1
                     llk = min( kd, n-j ) + 1_ilp64
                     call stdlib_I64_scopy( llk, a( j, j ), 1_ilp64, ab( 1_ilp64, j ), 1_ilp64 )
                  end do
                  call stdlib_I64_slaset( 'UPPER', pk, pk, zero, one,a( i+kd, i ), lda )
                  ! form the matrix t
                  call stdlib_I64_slarft( 'FORWARD', 'COLUMNWISE', pn, pk,a( i+kd, i ), lda, tau( i ),&
                            work( tpos ), ldt )
                  ! compute w:
                  call stdlib_I64_sgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', pn, pk, pk,one, a( i+kd, i ),&
                             lda,work( tpos ), ldt,zero, work( s2pos ), lds2 )
                  call stdlib_I64_ssymm( 'LEFT', uplo, pn, pk,one, a( i+kd, i+kd ), lda,work( s2pos ),&
                             lds2,zero, work( wpos ), ldw )
                  call stdlib_I64_sgemm( 'CONJUGATE', 'NO TRANSPOSE', pk, pk, pn,one, work( s2pos ), &
                            lds2,work( wpos ), ldw,zero, work( s1pos ), lds1 )
                  call stdlib_I64_sgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', pn, pk, pk,-half, a( i+kd, i &
                            ), lda,work( s1pos ), lds1,one, work( wpos ), ldw )
                  ! update the unreduced submatrix a(i+kd:n,i+kd:n), using
                  ! an update of the form:  a := a - v*w' - w*v'
                  call stdlib_I64_ssyr2k( uplo, 'NO TRANSPOSE', pn, pk,-one, a( i+kd, i ), lda,work( &
                            wpos ), ldw,one, a( i+kd, i+kd ), lda )
                  ! ==================================================================
                  ! restore a for comparison and checking to be removed
                   ! do 45 j = i, i+pk-1
                      ! llk = min( kd, n-j ) + 1
                      ! call stdlib_I64_scopy( llk, ab( 1, j ), 1, a( j, j ), 1 )
                      45 continue
                  ! ==================================================================
               end do loop_40
              ! copy the lower band to ab which is the band storage matrix
              do j = n-kd+1, n
                 llk = min(kd, n-j) + 1_ilp64
                 call stdlib_I64_scopy( llk, a( j, j ), 1_ilp64, ab( 1_ilp64, j ), 1_ilp64 )
              end do
           end if
           work( 1_ilp64 ) = lwmin
           return
     end subroutine stdlib_I64_ssytrd_sy2sb

     module subroutine stdlib_I64_dsytrd_sy2sb( uplo, n, kd, a, lda, ab, ldab, tau,work, lwork, info )
     !! DSYTRD_SY2SB reduces a real symmetric matrix A to real symmetric
     !! band-diagonal form AB by a orthogonal similarity transformation:
     !! Q**T * A * Q = AB.
               
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldab, lwork, n, kd
           ! Array Arguments 
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: ab(ldab,*), tau(*), work(*)
        ! =====================================================================
           ! Parameters 
           
           ! Local Scalars 
           logical(lk) :: lquery, upper
           integer(ilp64) :: i, j, iinfo, lwmin, pn, pk, llk, ldt, ldw, lds2, lds1, ls2, ls1, lw, lt,&
                      tpos, wpos, s2pos, s1pos
           ! Intrinsic Functions 
           ! Executable Statements 
           ! determine the minimal workspace size required
           ! and test the input parameters
           info   = 0_ilp64
           upper  = stdlib_lsame( uplo, 'U' )
           lquery = ( lwork==-1_ilp64 )
           lwmin  = stdlib_I64_ilaenv2stage( 4_ilp64, 'DSYTRD_SY2SB', '', n, kd, -1_ilp64, -1_ilp64 )
           if( .not.upper .and. .not.stdlib_lsame( uplo, 'L' ) ) then
              info = -1_ilp64
           else if( n<0_ilp64 ) then
              info = -2_ilp64
           else if( kd<0_ilp64 ) then
              info = -3_ilp64
           else if( lda<max( 1_ilp64, n ) ) then
              info = -5_ilp64
           else if( ldab<max( 1_ilp64, kd+1 ) ) then
              info = -7_ilp64
           else if( lwork<lwmin .and. .not.lquery ) then
              info = -10_ilp64
           end if
           if( info/=0_ilp64 ) then
              call stdlib_I64_xerbla( 'DSYTRD_SY2SB', -info )
              return
           else if( lquery ) then
              work( 1_ilp64 ) = lwmin
              return
           end if
           ! quick return if possible
           ! copy the upper/lower portion of a into ab
           if( n<=kd+1 ) then
               if( upper ) then
                   do i = 1, n
                       llk = min( kd+1, i )
                       call stdlib_I64_dcopy( llk, a( i-llk+1, i ), 1_ilp64,ab( kd+1-llk+1, i ), 1_ilp64 )
                   end do
               else
                   do i = 1, n
                       llk = min( kd+1, n-i+1 )
                       call stdlib_I64_dcopy( llk, a( i, i ), 1_ilp64, ab( 1_ilp64, i ), 1_ilp64 )
                   end do
               endif
               work( 1_ilp64 ) = 1_ilp64
               return
           end if
           ! determine the pointer position for the workspace
           ldt    = kd
           lds1   = kd
           lt     = ldt*kd
           lw     = n*kd
           ls1    = lds1*kd
           ls2    = lwmin - lt - lw - ls1
            ! ls2 = n*max(kd,factoptnb)
           tpos   = 1_ilp64
           wpos   = tpos  + lt
           s1pos  = wpos  + lw
           s2pos  = s1pos + ls1
           if( upper ) then
               ldw    = kd
               lds2   = kd
           else
               ldw    = n
               lds2   = n
           endif
           ! set the workspace of the triangular matrix t to zero once such a
           ! way every time t is generated the upper/lower portion will be always zero
           call stdlib_I64_dlaset( "A", ldt, kd, zero, zero, work( tpos ), ldt )
           if( upper ) then
               do i = 1, n - kd, kd
                  pn = n-i-kd+1
                  pk = min( n-i-kd+1, kd )
                  ! compute the lq factorization of the current block
                  call stdlib_I64_dgelqf( kd, pn, a( i, i+kd ), lda,tau( i ), work( s2pos ), ls2, &
                            iinfo )
                  ! copy the upper portion of a into ab
                  do j = i, i+pk-1
                     llk = min( kd, n-j ) + 1_ilp64
                     call stdlib_I64_dcopy( llk, a( j, j ), lda, ab( kd+1, j ), ldab-1 )
                  end do
                  call stdlib_I64_dlaset( 'LOWER', pk, pk, zero, one,a( i, i+kd ), lda )
                  ! form the matrix t
                  call stdlib_I64_dlarft( 'FORWARD', 'ROWWISE', pn, pk,a( i, i+kd ), lda, tau( i ),&
                            work( tpos ), ldt )
                  ! compute w:
                  call stdlib_I64_dgemm( 'CONJUGATE', 'NO TRANSPOSE', pk, pn, pk,one,  work( tpos ), &
                            ldt,a( i, i+kd ), lda,zero, work( s2pos ), lds2 )
                  call stdlib_I64_dsymm( 'RIGHT', uplo, pk, pn,one,  a( i+kd, i+kd ), lda,work( s2pos &
                            ), lds2,zero, work( wpos ), ldw )
                  call stdlib_I64_dgemm( 'NO TRANSPOSE', 'CONJUGATE', pk, pk, pn,one,  work( wpos ), &
                            ldw,work( s2pos ), lds2,zero, work( s1pos ), lds1 )
                  call stdlib_I64_dgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', pk, pn, pk,-half, work( &
                            s1pos ), lds1,a( i, i+kd ), lda,one,   work( wpos ), ldw )
                  ! update the unreduced submatrix a(i+kd:n,i+kd:n), using
                  ! an update of the form:  a := a - v'*w - w'*v
                  call stdlib_I64_dsyr2k( uplo, 'CONJUGATE', pn, pk,-one, a( i, i+kd ), lda,work( &
                            wpos ), ldw,one, a( i+kd, i+kd ), lda )
               end do
              ! copy the upper band to ab which is the band storage matrix
              do j = n-kd+1, n
                 llk = min(kd, n-j) + 1_ilp64
                 call stdlib_I64_dcopy( llk, a( j, j ), lda, ab( kd+1, j ), ldab-1 )
              end do
           else
               ! reduce the lower triangle of a to lower band matrix
               loop_40: do i = 1, n - kd, kd
                  pn = n-i-kd+1
                  pk = min( n-i-kd+1, kd )
                  ! compute the qr factorization of the current block
                  call stdlib_I64_dgeqrf( pn, kd, a( i+kd, i ), lda,tau( i ), work( s2pos ), ls2, &
                            iinfo )
                  ! copy the upper portion of a into ab
                  do j = i, i+pk-1
                     llk = min( kd, n-j ) + 1_ilp64
                     call stdlib_I64_dcopy( llk, a( j, j ), 1_ilp64, ab( 1_ilp64, j ), 1_ilp64 )
                  end do
                  call stdlib_I64_dlaset( 'UPPER', pk, pk, zero, one,a( i+kd, i ), lda )
                  ! form the matrix t
                  call stdlib_I64_dlarft( 'FORWARD', 'COLUMNWISE', pn, pk,a( i+kd, i ), lda, tau( i ),&
                            work( tpos ), ldt )
                  ! compute w:
                  call stdlib_I64_dgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', pn, pk, pk,one, a( i+kd, i ),&
                             lda,work( tpos ), ldt,zero, work( s2pos ), lds2 )
                  call stdlib_I64_dsymm( 'LEFT', uplo, pn, pk,one, a( i+kd, i+kd ), lda,work( s2pos ),&
                             lds2,zero, work( wpos ), ldw )
                  call stdlib_I64_dgemm( 'CONJUGATE', 'NO TRANSPOSE', pk, pk, pn,one, work( s2pos ), &
                            lds2,work( wpos ), ldw,zero, work( s1pos ), lds1 )
                  call stdlib_I64_dgemm( 'NO TRANSPOSE', 'NO TRANSPOSE', pn, pk, pk,-half, a( i+kd, i &
                            ), lda,work( s1pos ), lds1,one, work( wpos ), ldw )
                  ! update the unreduced submatrix a(i+kd:n,i+kd:n), using
                  ! an update of the form:  a := a - v*w' - w*v'
                  call stdlib_I64_dsyr2k( uplo, 'NO TRANSPOSE', pn, pk,-one, a( i+kd, i ), lda,work( &
                            wpos ), ldw,one, a( i+kd, i+kd ), lda )
                  ! ==================================================================
                  ! restore a for comparison and checking to be removed
                   ! do 45 j = i, i+pk-1
                      ! llk = min( kd, n-j ) + 1
                      ! call stdlib_I64_dcopy( llk, ab( 1, j ), 1, a( j, j ), 1 )
                      45 continue
                  ! ==================================================================
               end do loop_40
              ! copy the lower band to ab which is the band storage matrix
              do j = n-kd+1, n
                 llk = min(kd, n-j) + 1_ilp64
                 call stdlib_I64_dcopy( llk, a( j, j ), 1_ilp64, ab( 1_ilp64, j ), 1_ilp64 )
              end do
           end if
           work( 1_ilp64 ) = lwmin
           return
     end subroutine stdlib_I64_dsytrd_sy2sb



end submodule stdlib_lapack_eigv_sym
