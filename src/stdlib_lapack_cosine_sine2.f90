submodule(stdlib_lapack_eig_svd_lsq) stdlib_lapack_cosine_sine2
  implicit none


  contains

     recursive module subroutine stdlib_sorcsd( jobu1, jobu2, jobv1t, jobv2t, trans,signs, m, p, q, x11, &
     !! SORCSD computes the CS decomposition of an M-by-M partitioned
     !! orthogonal matrix X:
     !! [  I  0  0 |  0  0  0 ]
     !! [  0  C  0 |  0 -S  0 ]
     !! [ X11 | X12 ]   [ U1 |    ] [  0  0  0 |  0  0 -I ] [ V1 |    ]**T
     !! X = [-----------] = [---------] [---------------------] [---------]   .
     !! [ X21 | X22 ]   [    | U2 ] [  0  0  0 |  I  0  0 ] [    | V2 ]
     !! [  0  S  0 |  0  C  0 ]
     !! [  0  0  I |  0  0  0 ]
     !! X11 is P-by-Q. The orthogonal matrices U1, U2, V1, and V2 are P-by-P,
     !! (M-P)-by-(M-P), Q-by-Q, and (M-Q)-by-(M-Q), respectively. C and S are
     !! R-by-R nonnegative diagonal matrices satisfying C^2 + S^2 = I, in
     !! which R = MIN(P,M-P,Q,M-Q).
     ldx11, x12,ldx12, x21, ldx21, x22, ldx22, theta,u1, ldu1, u2, ldu2, v1t, ldv1t, v2t,ldv2t, &
               work, lwork, iwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobu1, jobu2, jobv1t, jobv2t, signs, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldu1, ldu2, ldv1t, ldv2t, ldx11, ldx12, ldx21, ldx22, &
                     lwork, m, p, q
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(out) :: theta(*)
           real(sp), intent(out) :: u1(ldu1,*), u2(ldu2,*), v1t(ldv1t,*), v2t(ldv2t,*), work(*)
                     
           real(sp), intent(inout) :: x11(ldx11,*), x12(ldx12,*), x21(ldx21,*), x22(ldx22,*)
                     
        ! ===================================================================
           
           ! Local Arrays 
           real(sp) :: dummy(1_ilp)
           ! Local Scalars 
           character :: transt, signst
           integer(ilp) :: childinfo, i, ib11d, ib11e, ib12d, ib12e, ib21d, ib21e, ib22d, ib22e, &
           ibbcsd, iorbdb, iorglq, iorgqr, iphi, itaup1, itaup2, itauq1, itauq2, j, lbbcsdwork, &
           lbbcsdworkmin, lbbcsdworkopt, lorbdbwork, lorbdbworkmin, lorbdbworkopt, lorglqwork, &
           lorglqworkmin, lorglqworkopt, lorgqrwork, lorgqrworkmin, lorgqrworkopt, lworkmin, &
                     lworkopt
           logical(lk) :: colmajor, defaultsigns, lquery, wantu1, wantu2, wantv1t, wantv2t
           ! Intrinsic Functions
           ! Executable Statements 
           ! test input arguments
           info = 0_ilp
           wantu1 = stdlib_lsame( jobu1, 'Y' )
           wantu2 = stdlib_lsame( jobu2, 'Y' )
           wantv1t = stdlib_lsame( jobv1t, 'Y' )
           wantv2t = stdlib_lsame( jobv2t, 'Y' )
           colmajor = .not. stdlib_lsame( trans, 'T' )
           defaultsigns = .not. stdlib_lsame( signs, 'O' )
           lquery = lwork == -1_ilp
           if( m < 0_ilp ) then
              info = -7_ilp
           else if( p < 0_ilp .or. p > m ) then
              info = -8_ilp
           else if( q < 0_ilp .or. q > m ) then
              info = -9_ilp
           else if ( colmajor .and.  ldx11 < max( 1_ilp, p ) ) then
             info = -11_ilp
           else if (.not. colmajor .and. ldx11 < max( 1_ilp, q ) ) then
             info = -11_ilp
           else if (colmajor .and. ldx12 < max( 1_ilp, p ) ) then
             info = -13_ilp
           else if (.not. colmajor .and. ldx12 < max( 1_ilp, m-q ) ) then
             info = -13_ilp
           else if (colmajor .and. ldx21 < max( 1_ilp, m-p ) ) then
             info = -15_ilp
           else if (.not. colmajor .and. ldx21 < max( 1_ilp, q ) ) then
             info = -15_ilp
           else if (colmajor .and. ldx22 < max( 1_ilp, m-p ) ) then
             info = -17_ilp
           else if (.not. colmajor .and. ldx22 < max( 1_ilp, m-q ) ) then
             info = -17_ilp
           else if( wantu1 .and. ldu1 < p ) then
              info = -20_ilp
           else if( wantu2 .and. ldu2 < m-p ) then
              info = -22_ilp
           else if( wantv1t .and. ldv1t < q ) then
              info = -24_ilp
           else if( wantv2t .and. ldv2t < m-q ) then
              info = -26_ilp
           end if
           ! work with transpose if convenient
           if( info == 0_ilp .and. min( p, m-p ) < min( q, m-q ) ) then
              if( colmajor ) then
                 transt = 'T'
              else
                 transt = 'N'
              end if
              if( defaultsigns ) then
                 signst = 'O'
              else
                 signst = 'D'
              end if
              call stdlib_sorcsd( jobv1t, jobv2t, jobu1, jobu2, transt, signst, m,q, p, x11, &
              ldx11, x21, ldx21, x12, ldx12, x22,ldx22, theta, v1t, ldv1t, v2t, ldv2t, u1, ldu1,&
                        u2, ldu2, work, lwork, iwork, info )
              return
           end if
           ! work with permutation [ 0 i; i 0 ] * x * [ 0 i; i 0 ] if
           ! convenient
           if( info == 0_ilp .and. m-q < q ) then
              if( defaultsigns ) then
                 signst = 'O'
              else
                 signst = 'D'
              end if
              call stdlib_sorcsd( jobu2, jobu1, jobv2t, jobv1t, trans, signst, m,m-p, m-q, x22, &
              ldx22, x21, ldx21, x12, ldx12, x11,ldx11, theta, u2, ldu2, u1, ldu1, v2t, ldv2t, &
                        v1t,ldv1t, work, lwork, iwork, info )
              return
           end if
           ! compute workspace
           if( info == 0_ilp ) then
              iphi = 2_ilp
              itaup1 = iphi + max( 1_ilp, q - 1_ilp )
              itaup2 = itaup1 + max( 1_ilp, p )
              itauq1 = itaup2 + max( 1_ilp, m - p )
              itauq2 = itauq1 + max( 1_ilp, q )
              iorgqr = itauq2 + max( 1_ilp, m - q )
              call stdlib_sorgqr( m-q, m-q, m-q, dummy, max(1_ilp,m-q), dummy, work, -1_ilp,childinfo )
                        
              lorgqrworkopt = int( work(1_ilp),KIND=ilp)
              lorgqrworkmin = max( 1_ilp, m - q )
              iorglq = itauq2 + max( 1_ilp, m - q )
              call stdlib_sorglq( m-q, m-q, m-q, dummy, max(1_ilp,m-q), dummy, work, -1_ilp,childinfo )
                        
              lorglqworkopt = int( work(1_ilp),KIND=ilp)
              lorglqworkmin = max( 1_ilp, m - q )
              iorbdb = itauq2 + max( 1_ilp, m - q )
              call stdlib_sorbdb( trans, signs, m, p, q, x11, ldx11, x12, ldx12,x21, ldx21, x22, &
                        ldx22, dummy, dummy, dummy, dummy, dummy,dummy,work,-1_ilp,childinfo )
              lorbdbworkopt = int( work(1_ilp),KIND=ilp)
              lorbdbworkmin = lorbdbworkopt
              ib11d = itauq2 + max( 1_ilp, m - q )
              ib11e = ib11d + max( 1_ilp, q )
              ib12d = ib11e + max( 1_ilp, q - 1_ilp )
              ib12e = ib12d + max( 1_ilp, q )
              ib21d = ib12e + max( 1_ilp, q - 1_ilp )
              ib21e = ib21d + max( 1_ilp, q )
              ib22d = ib21e + max( 1_ilp, q - 1_ilp )
              ib22e = ib22d + max( 1_ilp, q )
              ibbcsd = ib22e + max( 1_ilp, q - 1_ilp )
              call stdlib_sbbcsd( jobu1, jobu2, jobv1t, jobv2t, trans, m, p, q,dummy, dummy, u1, &
              ldu1, u2, ldu2, v1t, ldv1t, v2t,ldv2t, dummy, dummy, dummy, dummy, dummy, dummy,&
                        dummy, dummy, work, -1_ilp, childinfo )
              lbbcsdworkopt = int( work(1_ilp),KIND=ilp)
              lbbcsdworkmin = lbbcsdworkopt
              lworkopt = max( iorgqr + lorgqrworkopt, iorglq + lorglqworkopt,iorbdb + &
                        lorbdbworkopt, ibbcsd + lbbcsdworkopt ) - 1_ilp
              lworkmin = max( iorgqr + lorgqrworkmin, iorglq + lorglqworkmin,iorbdb + &
                        lorbdbworkopt, ibbcsd + lbbcsdworkmin ) - 1_ilp
              work(1_ilp) = max(lworkopt,lworkmin)
              if( lwork < lworkmin .and. .not. lquery ) then
                 info = -22_ilp
              else
                 lorgqrwork = lwork - iorgqr + 1_ilp
                 lorglqwork = lwork - iorglq + 1_ilp
                 lorbdbwork = lwork - iorbdb + 1_ilp
                 lbbcsdwork = lwork - ibbcsd + 1_ilp
              end if
           end if
           ! abort if any illegal arguments
           if( info /= 0_ilp ) then
              call stdlib_xerbla( 'SORCSD', -info )
              return
           else if( lquery ) then
              return
           end if
           ! transform to bidiagonal block form
           call stdlib_sorbdb( trans, signs, m, p, q, x11, ldx11, x12, ldx12, x21,ldx21, x22, &
           ldx22, theta, work(iphi), work(itaup1),work(itaup2), work(itauq1), work(itauq2),work(&
                     iorbdb), lorbdbwork, childinfo )
           ! accumulate householder reflectors
           if( colmajor ) then
              if( wantu1 .and. p > 0_ilp ) then
                 call stdlib_slacpy( 'L', p, q, x11, ldx11, u1, ldu1 )
                 call stdlib_sorgqr( p, p, q, u1, ldu1, work(itaup1), work(iorgqr),lorgqrwork, &
                           info)
              end if
              if( wantu2 .and. m-p > 0_ilp ) then
                 call stdlib_slacpy( 'L', m-p, q, x21, ldx21, u2, ldu2 )
                 call stdlib_sorgqr( m-p, m-p, q, u2, ldu2, work(itaup2),work(iorgqr), lorgqrwork,&
                            info )
              end if
              if( wantv1t .and. q > 0_ilp ) then
                 call stdlib_slacpy( 'U', q-1, q-1, x11(1_ilp,2_ilp), ldx11, v1t(2_ilp,2_ilp),ldv1t )
                 v1t(1_ilp, 1_ilp) = one
                 do j = 2, q
                    v1t(1_ilp,j) = zero
                    v1t(j,1_ilp) = zero
                 end do
                 call stdlib_sorglq( q-1, q-1, q-1, v1t(2_ilp,2_ilp), ldv1t, work(itauq1),work(iorglq), &
                           lorglqwork, info )
              end if
              if( wantv2t .and. m-q > 0_ilp ) then
                 call stdlib_slacpy( 'U', p, m-q, x12, ldx12, v2t, ldv2t )
                 call stdlib_slacpy( 'U', m-p-q, m-p-q, x22(q+1,p+1), ldx22,v2t(p+1,p+1), ldv2t )
                           
                 call stdlib_sorglq( m-q, m-q, m-q, v2t, ldv2t, work(itauq2),work(iorglq), &
                           lorglqwork, info )
              end if
           else
              if( wantu1 .and. p > 0_ilp ) then
                 call stdlib_slacpy( 'U', q, p, x11, ldx11, u1, ldu1 )
                 call stdlib_sorglq( p, p, q, u1, ldu1, work(itaup1), work(iorglq),lorglqwork, &
                           info)
              end if
              if( wantu2 .and. m-p > 0_ilp ) then
                 call stdlib_slacpy( 'U', q, m-p, x21, ldx21, u2, ldu2 )
                 call stdlib_sorglq( m-p, m-p, q, u2, ldu2, work(itaup2),work(iorglq), lorglqwork,&
                            info )
              end if
              if( wantv1t .and. q > 0_ilp ) then
                 call stdlib_slacpy( 'L', q-1, q-1, x11(2_ilp,1_ilp), ldx11, v1t(2_ilp,2_ilp),ldv1t )
                 v1t(1_ilp, 1_ilp) = one
                 do j = 2, q
                    v1t(1_ilp,j) = zero
                    v1t(j,1_ilp) = zero
                 end do
                 call stdlib_sorgqr( q-1, q-1, q-1, v1t(2_ilp,2_ilp), ldv1t, work(itauq1),work(iorgqr), &
                           lorgqrwork, info )
              end if
              if( wantv2t .and. m-q > 0_ilp ) then
                 call stdlib_slacpy( 'L', m-q, p, x12, ldx12, v2t, ldv2t )
                 call stdlib_slacpy( 'L', m-p-q, m-p-q, x22(p+1,q+1), ldx22,v2t(p+1,p+1), ldv2t )
                           
                 call stdlib_sorgqr( m-q, m-q, m-q, v2t, ldv2t, work(itauq2),work(iorgqr), &
                           lorgqrwork, info )
              end if
           end if
           ! compute the csd of the matrix in bidiagonal-block form
           call stdlib_sbbcsd( jobu1, jobu2, jobv1t, jobv2t, trans, m, p, q, theta,work(iphi), u1,&
            ldu1, u2, ldu2, v1t, ldv1t, v2t,ldv2t, work(ib11d), work(ib11e), work(ib12d),work(&
            ib12e), work(ib21d), work(ib21e), work(ib22d),work(ib22e), work(ibbcsd), lbbcsdwork, &
                      info )
           ! permute rows and columns to place identity submatrices in top-
           ! left corner of (1,1)-block and/or bottom-right corner of (1,2)-
           ! block and/or bottom-right corner of (2,1)-block and/or top-left
           ! corner of (2,2)-block
           if( q > 0_ilp .and. wantu2 ) then
              do i = 1, q
                 iwork(i) = m - p - q + i
              end do
              do i = q + 1, m - p
                 iwork(i) = i - q
              end do
              if( colmajor ) then
                 call stdlib_slapmt( .false., m-p, m-p, u2, ldu2, iwork )
              else
                 call stdlib_slapmr( .false., m-p, m-p, u2, ldu2, iwork )
              end if
           end if
           if( m > 0_ilp .and. wantv2t ) then
              do i = 1, p
                 iwork(i) = m - p - q + i
              end do
              do i = p + 1, m - q
                 iwork(i) = i - p
              end do
              if( .not. colmajor ) then
                 call stdlib_slapmt( .false., m-q, m-q, v2t, ldv2t, iwork )
              else
                 call stdlib_slapmr( .false., m-q, m-q, v2t, ldv2t, iwork )
              end if
           end if
           return
           ! end stdlib_sorcsd
     end subroutine stdlib_sorcsd

     recursive module subroutine stdlib_dorcsd( jobu1, jobu2, jobv1t, jobv2t, trans,signs, m, p, q, x11, &
     !! DORCSD computes the CS decomposition of an M-by-M partitioned
     !! orthogonal matrix X:
     !! [  I  0  0 |  0  0  0 ]
     !! [  0  C  0 |  0 -S  0 ]
     !! [ X11 | X12 ]   [ U1 |    ] [  0  0  0 |  0  0 -I ] [ V1 |    ]**T
     !! X = [-----------] = [---------] [---------------------] [---------]   .
     !! [ X21 | X22 ]   [    | U2 ] [  0  0  0 |  I  0  0 ] [    | V2 ]
     !! [  0  S  0 |  0  C  0 ]
     !! [  0  0  I |  0  0  0 ]
     !! X11 is P-by-Q. The orthogonal matrices U1, U2, V1, and V2 are P-by-P,
     !! (M-P)-by-(M-P), Q-by-Q, and (M-Q)-by-(M-Q), respectively. C and S are
     !! R-by-R nonnegative diagonal matrices satisfying C^2 + S^2 = I, in
     !! which R = MIN(P,M-P,Q,M-Q).
     ldx11, x12,ldx12, x21, ldx21, x22, ldx22, theta,u1, ldu1, u2, ldu2, v1t, ldv1t, v2t,ldv2t, &
               work, lwork, iwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobu1, jobu2, jobv1t, jobv2t, signs, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldu1, ldu2, ldv1t, ldv2t, ldx11, ldx12, ldx21, ldx22, &
                     lwork, m, p, q
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(out) :: theta(*)
           real(dp), intent(out) :: u1(ldu1,*), u2(ldu2,*), v1t(ldv1t,*), v2t(ldv2t,*), work(*)
                     
           real(dp), intent(inout) :: x11(ldx11,*), x12(ldx12,*), x21(ldx21,*), x22(ldx22,*)
                     
        ! ===================================================================
           
           ! Local Scalars 
           character :: transt, signst
           integer(ilp) :: childinfo, i, ib11d, ib11e, ib12d, ib12e, ib21d, ib21e, ib22d, ib22e, &
           ibbcsd, iorbdb, iorglq, iorgqr, iphi, itaup1, itaup2, itauq1, itauq2, j, lbbcsdwork, &
           lbbcsdworkmin, lbbcsdworkopt, lorbdbwork, lorbdbworkmin, lorbdbworkopt, lorglqwork, &
           lorglqworkmin, lorglqworkopt, lorgqrwork, lorgqrworkmin, lorgqrworkopt, lworkmin, &
                     lworkopt
           logical(lk) :: colmajor, defaultsigns, lquery, wantu1, wantu2, wantv1t, wantv2t
           ! Intrinsic Functions
           ! Executable Statements 
           ! test input arguments
           info = 0_ilp
           wantu1 = stdlib_lsame( jobu1, 'Y' )
           wantu2 = stdlib_lsame( jobu2, 'Y' )
           wantv1t = stdlib_lsame( jobv1t, 'Y' )
           wantv2t = stdlib_lsame( jobv2t, 'Y' )
           colmajor = .not. stdlib_lsame( trans, 'T' )
           defaultsigns = .not. stdlib_lsame( signs, 'O' )
           lquery = lwork == -1_ilp
           if( m < 0_ilp ) then
              info = -7_ilp
           else if( p < 0_ilp .or. p > m ) then
              info = -8_ilp
           else if( q < 0_ilp .or. q > m ) then
              info = -9_ilp
           else if ( colmajor .and.  ldx11 < max( 1_ilp, p ) ) then
             info = -11_ilp
           else if (.not. colmajor .and. ldx11 < max( 1_ilp, q ) ) then
             info = -11_ilp
           else if (colmajor .and. ldx12 < max( 1_ilp, p ) ) then
             info = -13_ilp
           else if (.not. colmajor .and. ldx12 < max( 1_ilp, m-q ) ) then
             info = -13_ilp
           else if (colmajor .and. ldx21 < max( 1_ilp, m-p ) ) then
             info = -15_ilp
           else if (.not. colmajor .and. ldx21 < max( 1_ilp, q ) ) then
             info = -15_ilp
           else if (colmajor .and. ldx22 < max( 1_ilp, m-p ) ) then
             info = -17_ilp
           else if (.not. colmajor .and. ldx22 < max( 1_ilp, m-q ) ) then
             info = -17_ilp
           else if( wantu1 .and. ldu1 < p ) then
              info = -20_ilp
           else if( wantu2 .and. ldu2 < m-p ) then
              info = -22_ilp
           else if( wantv1t .and. ldv1t < q ) then
              info = -24_ilp
           else if( wantv2t .and. ldv2t < m-q ) then
              info = -26_ilp
           end if
           ! work with transpose if convenient
           if( info == 0_ilp .and. min( p, m-p ) < min( q, m-q ) ) then
              if( colmajor ) then
                 transt = 'T'
              else
                 transt = 'N'
              end if
              if( defaultsigns ) then
                 signst = 'O'
              else
                 signst = 'D'
              end if
              call stdlib_dorcsd( jobv1t, jobv2t, jobu1, jobu2, transt, signst, m,q, p, x11, &
              ldx11, x21, ldx21, x12, ldx12, x22,ldx22, theta, v1t, ldv1t, v2t, ldv2t, u1, ldu1,&
                        u2, ldu2, work, lwork, iwork, info )
              return
           end if
           ! work with permutation [ 0 i; i 0 ] * x * [ 0 i; i 0 ] if
           ! convenient
           if( info == 0_ilp .and. m-q < q ) then
              if( defaultsigns ) then
                 signst = 'O'
              else
                 signst = 'D'
              end if
              call stdlib_dorcsd( jobu2, jobu1, jobv2t, jobv1t, trans, signst, m,m-p, m-q, x22, &
              ldx22, x21, ldx21, x12, ldx12, x11,ldx11, theta, u2, ldu2, u1, ldu1, v2t, ldv2t, &
                        v1t,ldv1t, work, lwork, iwork, info )
              return
           end if
           ! compute workspace
           if( info == 0_ilp ) then
              iphi = 2_ilp
              itaup1 = iphi + max( 1_ilp, q - 1_ilp )
              itaup2 = itaup1 + max( 1_ilp, p )
              itauq1 = itaup2 + max( 1_ilp, m - p )
              itauq2 = itauq1 + max( 1_ilp, q )
              iorgqr = itauq2 + max( 1_ilp, m - q )
              call stdlib_dorgqr( m-q, m-q, m-q, u1, max(1_ilp,m-q), u1, work, -1_ilp,childinfo )
              lorgqrworkopt = int( work(1_ilp),KIND=ilp)
              lorgqrworkmin = max( 1_ilp, m - q )
              iorglq = itauq2 + max( 1_ilp, m - q )
              call stdlib_dorglq( m-q, m-q, m-q, u1, max(1_ilp,m-q), u1, work, -1_ilp,childinfo )
              lorglqworkopt = int( work(1_ilp),KIND=ilp)
              lorglqworkmin = max( 1_ilp, m - q )
              iorbdb = itauq2 + max( 1_ilp, m - q )
              call stdlib_dorbdb( trans, signs, m, p, q, x11, ldx11, x12, ldx12,x21, ldx21, x22, &
                        ldx22, theta, v1t, u1, u2, v1t,v2t, work, -1_ilp, childinfo )
              lorbdbworkopt = int( work(1_ilp),KIND=ilp)
              lorbdbworkmin = lorbdbworkopt
              ib11d = itauq2 + max( 1_ilp, m - q )
              ib11e = ib11d + max( 1_ilp, q )
              ib12d = ib11e + max( 1_ilp, q - 1_ilp )
              ib12e = ib12d + max( 1_ilp, q )
              ib21d = ib12e + max( 1_ilp, q - 1_ilp )
              ib21e = ib21d + max( 1_ilp, q )
              ib22d = ib21e + max( 1_ilp, q - 1_ilp )
              ib22e = ib22d + max( 1_ilp, q )
              ibbcsd = ib22e + max( 1_ilp, q - 1_ilp )
              call stdlib_dbbcsd( jobu1, jobu2, jobv1t, jobv2t, trans, m, p, q,theta, theta, u1, &
              ldu1, u2, ldu2, v1t, ldv1t, v2t,ldv2t, u1, u1, u1, u1, u1, u1, u1, u1, work, -1_ilp,&
                        childinfo )
              lbbcsdworkopt = int( work(1_ilp),KIND=ilp)
              lbbcsdworkmin = lbbcsdworkopt
              lworkopt = max( iorgqr + lorgqrworkopt, iorglq + lorglqworkopt,iorbdb + &
                        lorbdbworkopt, ibbcsd + lbbcsdworkopt ) - 1_ilp
              lworkmin = max( iorgqr + lorgqrworkmin, iorglq + lorglqworkmin,iorbdb + &
                        lorbdbworkopt, ibbcsd + lbbcsdworkmin ) - 1_ilp
              work(1_ilp) = max(lworkopt,lworkmin)
              if( lwork < lworkmin .and. .not. lquery ) then
                 info = -22_ilp
              else
                 lorgqrwork = lwork - iorgqr + 1_ilp
                 lorglqwork = lwork - iorglq + 1_ilp
                 lorbdbwork = lwork - iorbdb + 1_ilp
                 lbbcsdwork = lwork - ibbcsd + 1_ilp
              end if
           end if
           ! abort if any illegal arguments
           if( info /= 0_ilp ) then
              call stdlib_xerbla( 'DORCSD', -info )
              return
           else if( lquery ) then
              return
           end if
           ! transform to bidiagonal block form
           call stdlib_dorbdb( trans, signs, m, p, q, x11, ldx11, x12, ldx12, x21,ldx21, x22, &
           ldx22, theta, work(iphi), work(itaup1),work(itaup2), work(itauq1), work(itauq2),work(&
                     iorbdb), lorbdbwork, childinfo )
           ! accumulate householder reflectors
           if( colmajor ) then
              if( wantu1 .and. p > 0_ilp ) then
                 call stdlib_dlacpy( 'L', p, q, x11, ldx11, u1, ldu1 )
                 call stdlib_dorgqr( p, p, q, u1, ldu1, work(itaup1), work(iorgqr),lorgqrwork, &
                           info)
              end if
              if( wantu2 .and. m-p > 0_ilp ) then
                 call stdlib_dlacpy( 'L', m-p, q, x21, ldx21, u2, ldu2 )
                 call stdlib_dorgqr( m-p, m-p, q, u2, ldu2, work(itaup2),work(iorgqr), lorgqrwork,&
                            info )
              end if
              if( wantv1t .and. q > 0_ilp ) then
                 call stdlib_dlacpy( 'U', q-1, q-1, x11(1_ilp,2_ilp), ldx11, v1t(2_ilp,2_ilp),ldv1t )
                 v1t(1_ilp, 1_ilp) = one
                 do j = 2, q
                    v1t(1_ilp,j) = zero
                    v1t(j,1_ilp) = zero
                 end do
                 call stdlib_dorglq( q-1, q-1, q-1, v1t(2_ilp,2_ilp), ldv1t, work(itauq1),work(iorglq), &
                           lorglqwork, info )
              end if
              if( wantv2t .and. m-q > 0_ilp ) then
                 call stdlib_dlacpy( 'U', p, m-q, x12, ldx12, v2t, ldv2t )
                 if (m-p > q) then
                    call stdlib_dlacpy( 'U', m-p-q, m-p-q, x22(q+1,p+1), ldx22,v2t(p+1,p+1), &
                              ldv2t )
                 end if
                 if (m > q) then
                    call stdlib_dorglq( m-q, m-q, m-q, v2t, ldv2t, work(itauq2),work(iorglq), &
                              lorglqwork, info )
                 end if
              end if
           else
              if( wantu1 .and. p > 0_ilp ) then
                 call stdlib_dlacpy( 'U', q, p, x11, ldx11, u1, ldu1 )
                 call stdlib_dorglq( p, p, q, u1, ldu1, work(itaup1), work(iorglq),lorglqwork, &
                           info)
              end if
              if( wantu2 .and. m-p > 0_ilp ) then
                 call stdlib_dlacpy( 'U', q, m-p, x21, ldx21, u2, ldu2 )
                 call stdlib_dorglq( m-p, m-p, q, u2, ldu2, work(itaup2),work(iorglq), lorglqwork,&
                            info )
              end if
              if( wantv1t .and. q > 0_ilp ) then
                 call stdlib_dlacpy( 'L', q-1, q-1, x11(2_ilp,1_ilp), ldx11, v1t(2_ilp,2_ilp),ldv1t )
                 v1t(1_ilp, 1_ilp) = one
                 do j = 2, q
                    v1t(1_ilp,j) = zero
                    v1t(j,1_ilp) = zero
                 end do
                 call stdlib_dorgqr( q-1, q-1, q-1, v1t(2_ilp,2_ilp), ldv1t, work(itauq1),work(iorgqr), &
                           lorgqrwork, info )
              end if
              if( wantv2t .and. m-q > 0_ilp ) then
                 call stdlib_dlacpy( 'L', m-q, p, x12, ldx12, v2t, ldv2t )
                 call stdlib_dlacpy( 'L', m-p-q, m-p-q, x22(p+1,q+1), ldx22,v2t(p+1,p+1), ldv2t )
                           
                 call stdlib_dorgqr( m-q, m-q, m-q, v2t, ldv2t, work(itauq2),work(iorgqr), &
                           lorgqrwork, info )
              end if
           end if
           ! compute the csd of the matrix in bidiagonal-block form
           call stdlib_dbbcsd( jobu1, jobu2, jobv1t, jobv2t, trans, m, p, q, theta,work(iphi), u1,&
            ldu1, u2, ldu2, v1t, ldv1t, v2t,ldv2t, work(ib11d), work(ib11e), work(ib12d),work(&
            ib12e), work(ib21d), work(ib21e), work(ib22d),work(ib22e), work(ibbcsd), lbbcsdwork, &
                      info )
           ! permute rows and columns to place identity submatrices in top-
           ! left corner of (1,1)-block and/or bottom-right corner of (1,2)-
           ! block and/or bottom-right corner of (2,1)-block and/or top-left
           ! corner of (2,2)-block
           if( q > 0_ilp .and. wantu2 ) then
              do i = 1, q
                 iwork(i) = m - p - q + i
              end do
              do i = q + 1, m - p
                 iwork(i) = i - q
              end do
              if( colmajor ) then
                 call stdlib_dlapmt( .false., m-p, m-p, u2, ldu2, iwork )
              else
                 call stdlib_dlapmr( .false., m-p, m-p, u2, ldu2, iwork )
              end if
           end if
           if( m > 0_ilp .and. wantv2t ) then
              do i = 1, p
                 iwork(i) = m - p - q + i
              end do
              do i = p + 1, m - q
                 iwork(i) = i - p
              end do
              if( .not. colmajor ) then
                 call stdlib_dlapmt( .false., m-q, m-q, v2t, ldv2t, iwork )
              else
                 call stdlib_dlapmr( .false., m-q, m-q, v2t, ldv2t, iwork )
              end if
           end if
           return
           ! end stdlib_dorcsd
     end subroutine stdlib_dorcsd




     module subroutine stdlib_sorcsd2by1( jobu1, jobu2, jobv1t, m, p, q, x11, ldx11,x21, ldx21, theta, &
     !! SORCSD2BY1 computes the CS decomposition of an M-by-Q matrix X with
     !! orthonormal columns that has been partitioned into a 2-by-1 block
     !! structure:
     !! [  I1 0  0 ]
     !! [  0  C  0 ]
     !! [ X11 ]   [ U1 |    ] [  0  0  0 ]
     !! X = [-----] = [---------] [----------] V1**T .
     !! [ X21 ]   [    | U2 ] [  0  0  0 ]
     !! [  0  S  0 ]
     !! [  0  0  I2]
     !! X11 is P-by-Q. The orthogonal matrices U1, U2, and V1 are P-by-P,
     !! (M-P)-by-(M-P), and Q-by-Q, respectively. C and S are R-by-R
     !! nonnegative diagonal matrices satisfying C^2 + S^2 = I, in which
     !! R = MIN(P,M-P,Q,M-Q). I1 is a K1-by-K1 identity matrix and I2 is a
     !! K2-by-K2 identity matrix, where K1 = MAX(Q+P-M,0), K2 = MAX(Q-P,0).
               u1, ldu1, u2, ldu2, v1t,ldv1t, work, lwork, iwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobu1, jobu2, jobv1t
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldu1, ldu2, ldv1t, lwork, ldx11, ldx21, m, p, q
           ! Array Arguments 
           real(sp), intent(out) :: theta(*)
           real(sp), intent(out) :: u1(ldu1,*), u2(ldu2,*), v1t(ldv1t,*), work(*)
           real(sp), intent(inout) :: x11(ldx11,*), x21(ldx21,*)
           integer(ilp), intent(out) :: iwork(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: childinfo, i, ib11d, ib11e, ib12d, ib12e, ib21d, ib21e, ib22d, ib22e, &
           ibbcsd, iorbdb, iorglq, iorgqr, iphi, itaup1, itaup2, itauq1, j, lbbcsd, lorbdb, &
           lorglq, lorglqmin, lorglqopt, lorgqr, lorgqrmin, lorgqropt, lworkmin, lworkopt, &
                     r
           logical(lk) :: lquery, wantu1, wantu2, wantv1t
           ! Local Arrays 
           real(sp) :: dum1(1_ilp), dum2(1_ilp,1_ilp)
           ! Intrinsic Function 
           ! Executable Statements 
           ! test input arguments
           info = 0_ilp
           wantu1 = stdlib_lsame( jobu1, 'Y' )
           wantu2 = stdlib_lsame( jobu2, 'Y' )
           wantv1t = stdlib_lsame( jobv1t, 'Y' )
           lquery = lwork == -1_ilp
           if( m < 0_ilp ) then
              info = -4_ilp
           else if( p < 0_ilp .or. p > m ) then
              info = -5_ilp
           else if( q < 0_ilp .or. q > m ) then
              info = -6_ilp
           else if( ldx11 < max( 1_ilp, p ) ) then
              info = -8_ilp
           else if( ldx21 < max( 1_ilp, m-p ) ) then
              info = -10_ilp
           else if( wantu1 .and. ldu1 < max( 1_ilp, p ) ) then
              info = -13_ilp
           else if( wantu2 .and. ldu2 < max( 1_ilp, m - p ) ) then
              info = -15_ilp
           else if( wantv1t .and. ldv1t < max( 1_ilp, q ) ) then
              info = -17_ilp
           end if
           r = min( p, m-p, q, m-q )
           ! compute workspace
             ! work layout:
           ! |-------------------------------------------------------|
           ! | lworkopt (1)                                          |
           ! |-------------------------------------------------------|
           ! | phi (max(1,r-1))                                      |
           ! |-------------------------------------------------------|
           ! | taup1 (max(1,p))                        | b11d (r)    |
           ! | taup2 (max(1,m-p))                      | b11e (r-1)  |
           ! | tauq1 (max(1,q))                        | b12d (r)    |
           ! |-----------------------------------------| b12e (r-1)  |
           ! | stdlib_sorbdb work | stdlib_sorgqr work | stdlib_sorglq work | b21d (r)    |
           ! |             |             |             | b21e (r-1)  |
           ! |             |             |             | b22d (r)    |
           ! |             |             |             | b22e (r-1)  |
           ! |             |             |             | stdlib_sbbcsd work |
           ! |-------------------------------------------------------|
           if( info == 0_ilp ) then
              iphi = 2_ilp
              ib11d = iphi + max( 1_ilp, r-1 )
              ib11e = ib11d + max( 1_ilp, r )
              ib12d = ib11e + max( 1_ilp, r - 1_ilp )
              ib12e = ib12d + max( 1_ilp, r )
              ib21d = ib12e + max( 1_ilp, r - 1_ilp )
              ib21e = ib21d + max( 1_ilp, r )
              ib22d = ib21e + max( 1_ilp, r - 1_ilp )
              ib22e = ib22d + max( 1_ilp, r )
              ibbcsd = ib22e + max( 1_ilp, r - 1_ilp )
              itaup1 = iphi + max( 1_ilp, r-1 )
              itaup2 = itaup1 + max( 1_ilp, p )
              itauq1 = itaup2 + max( 1_ilp, m-p )
              iorbdb = itauq1 + max( 1_ilp, q )
              iorgqr = itauq1 + max( 1_ilp, q )
              iorglq = itauq1 + max( 1_ilp, q )
              lorgqrmin = 1_ilp
              lorgqropt = 1_ilp
              lorglqmin = 1_ilp
              lorglqopt = 1_ilp
              if( r == q ) then
                 call stdlib_sorbdb1( m, p, q, x11, ldx11, x21, ldx21, theta,dum1, dum1, dum1, &
                           dum1, work, -1_ilp,childinfo )
                 lorbdb = int( work(1_ilp),KIND=ilp)
                 if( wantu1 .and. p > 0_ilp ) then
                    call stdlib_sorgqr( p, p, q, u1, ldu1, dum1, work(1_ilp), -1_ilp,childinfo )
                    lorgqrmin = max( lorgqrmin, p )
                    lorgqropt = max( lorgqropt, int( work(1_ilp),KIND=ilp) )
                 endif
                 if( wantu2 .and. m-p > 0_ilp ) then
                    call stdlib_sorgqr( m-p, m-p, q, u2, ldu2, dum1, work(1_ilp), -1_ilp,childinfo )
                              
                    lorgqrmin = max( lorgqrmin, m-p )
                    lorgqropt = max( lorgqropt, int( work(1_ilp),KIND=ilp) )
                 end if
                 if( wantv1t .and. q > 0_ilp ) then
                    call stdlib_sorglq( q-1, q-1, q-1, v1t, ldv1t,dum1, work(1_ilp), -1_ilp, childinfo )
                              
                    lorglqmin = max( lorglqmin, q-1 )
                    lorglqopt = max( lorglqopt, int( work(1_ilp),KIND=ilp) )
                 end if
                 call stdlib_sbbcsd( jobu1, jobu2, jobv1t, 'N', 'N', m, p, q, theta,dum1, u1, &
                 ldu1, u2, ldu2, v1t, ldv1t, dum2,1_ilp, dum1, dum1, dum1, dum1, dum1,dum1, dum1, &
                           dum1, work(1_ilp), -1_ilp, childinfo)
                 lbbcsd = int( work(1_ilp),KIND=ilp)
              else if( r == p ) then
                 call stdlib_sorbdb2( m, p, q, x11, ldx11, x21, ldx21, theta,dum1, dum1, dum1, &
                           dum1, work(1_ilp), -1_ilp,childinfo )
                 lorbdb = int( work(1_ilp),KIND=ilp)
                 if( wantu1 .and. p > 0_ilp ) then
                    call stdlib_sorgqr( p-1, p-1, p-1, u1(2_ilp,2_ilp), ldu1, dum1,work(1_ilp), -1_ilp, childinfo &
                              )
                    lorgqrmin = max( lorgqrmin, p-1 )
                    lorgqropt = max( lorgqropt, int( work(1_ilp),KIND=ilp) )
                 end if
                 if( wantu2 .and. m-p > 0_ilp ) then
                    call stdlib_sorgqr( m-p, m-p, q, u2, ldu2, dum1, work(1_ilp), -1_ilp,childinfo )
                              
                    lorgqrmin = max( lorgqrmin, m-p )
                    lorgqropt = max( lorgqropt, int( work(1_ilp),KIND=ilp) )
                 end if
                 if( wantv1t .and. q > 0_ilp ) then
                    call stdlib_sorglq( q, q, r, v1t, ldv1t, dum1, work(1_ilp), -1_ilp,childinfo )
                    lorglqmin = max( lorglqmin, q )
                    lorglqopt = max( lorglqopt, int( work(1_ilp),KIND=ilp) )
                 end if
                 call stdlib_sbbcsd( jobv1t, 'N', jobu1, jobu2, 'T', m, q, p, theta,dum1, v1t, &
                 ldv1t, dum2, 1_ilp, u1, ldu1, u2,ldu2, dum1, dum1, dum1, dum1, dum1,dum1, dum1, dum1,&
                            work(1_ilp), -1_ilp, childinfo)
                 lbbcsd = int( work(1_ilp),KIND=ilp)
              else if( r == m-p ) then
                 call stdlib_sorbdb3( m, p, q, x11, ldx11, x21, ldx21, theta,dum1, dum1, dum1, &
                           dum1, work(1_ilp), -1_ilp,childinfo )
                 lorbdb = int( work(1_ilp),KIND=ilp)
                 if( wantu1 .and. p > 0_ilp ) then
                    call stdlib_sorgqr( p, p, q, u1, ldu1, dum1, work(1_ilp), -1_ilp,childinfo )
                    lorgqrmin = max( lorgqrmin, p )
                    lorgqropt = max( lorgqropt, int( work(1_ilp),KIND=ilp) )
                 end if
                 if( wantu2 .and. m-p > 0_ilp ) then
                    call stdlib_sorgqr( m-p-1, m-p-1, m-p-1, u2(2_ilp,2_ilp), ldu2, dum1,work(1_ilp), -1_ilp, &
                              childinfo )
                    lorgqrmin = max( lorgqrmin, m-p-1 )
                    lorgqropt = max( lorgqropt, int( work(1_ilp),KIND=ilp) )
                 end if
                 if( wantv1t .and. q > 0_ilp ) then
                    call stdlib_sorglq( q, q, r, v1t, ldv1t, dum1, work(1_ilp), -1_ilp,childinfo )
                    lorglqmin = max( lorglqmin, q )
                    lorglqopt = max( lorglqopt, int( work(1_ilp),KIND=ilp) )
                 end if
                 call stdlib_sbbcsd( 'N', jobv1t, jobu2, jobu1, 'T', m, m-q, m-p,theta, dum1, &
                 dum2, 1_ilp, v1t, ldv1t, u2, ldu2,u1, ldu1, dum1, dum1, dum1, dum1,dum1, dum1, dum1, &
                           dum1, work(1_ilp), -1_ilp,childinfo )
                 lbbcsd = int( work(1_ilp),KIND=ilp)
              else
                 call stdlib_sorbdb4( m, p, q, x11, ldx11, x21, ldx21, theta,dum1, dum1, dum1, &
                           dum1, dum1,work(1_ilp), -1_ilp, childinfo )
                 lorbdb = m + int( work(1_ilp),KIND=ilp)
                 if( wantu1 .and. p > 0_ilp ) then
                    call stdlib_sorgqr( p, p, m-q, u1, ldu1, dum1, work(1_ilp), -1_ilp,childinfo )
                    lorgqrmin = max( lorgqrmin, p )
                    lorgqropt = max( lorgqropt, int( work(1_ilp),KIND=ilp) )
                 end if
                 if( wantu2 .and. m-p > 0_ilp ) then
                    call stdlib_sorgqr( m-p, m-p, m-q, u2, ldu2, dum1, work(1_ilp),-1_ilp, childinfo )
                              
                    lorgqrmin = max( lorgqrmin, m-p )
                    lorgqropt = max( lorgqropt, int( work(1_ilp),KIND=ilp) )
                 end if
                 if( wantv1t .and. q > 0_ilp ) then
                    call stdlib_sorglq( q, q, q, v1t, ldv1t, dum1, work(1_ilp), -1_ilp,childinfo )
                    lorglqmin = max( lorglqmin, q )
                    lorglqopt = max( lorglqopt, int( work(1_ilp),KIND=ilp) )
                 end if
                 call stdlib_sbbcsd( jobu2, jobu1, 'N', jobv1t, 'N', m, m-p, m-q,theta, dum1, u2, &
                 ldu2, u1, ldu1, dum2, 1_ilp,v1t, ldv1t, dum1, dum1, dum1, dum1,dum1, dum1, dum1, &
                           dum1, work(1_ilp), -1_ilp,childinfo )
                 lbbcsd = int( work(1_ilp),KIND=ilp)
              end if
              lworkmin = max( iorbdb+lorbdb-1,iorgqr+lorgqrmin-1,iorglq+lorglqmin-1,ibbcsd+lbbcsd-&
                        1_ilp )
              lworkopt = max( iorbdb+lorbdb-1,iorgqr+lorgqropt-1,iorglq+lorglqopt-1,ibbcsd+lbbcsd-&
                        1_ilp )
              work(1_ilp) = lworkopt
              if( lwork < lworkmin .and. .not.lquery ) then
                 info = -19_ilp
              end if
           end if
           if( info /= 0_ilp ) then
              call stdlib_xerbla( 'SORCSD2BY1', -info )
              return
           else if( lquery ) then
              return
           end if
           lorgqr = lwork-iorgqr+1
           lorglq = lwork-iorglq+1
           ! handle four cases separately: r = q, r = p, r = m-p, and r = m-q,
           ! in which r = min(p,m-p,q,m-q)
           if( r == q ) then
              ! case 1: r = q
              ! simultaneously bidiagonalize x11 and x21
              call stdlib_sorbdb1( m, p, q, x11, ldx11, x21, ldx21, theta,work(iphi), work(itaup1)&
                        , work(itaup2),work(itauq1), work(iorbdb), lorbdb, childinfo )
              ! accumulate householder reflectors
              if( wantu1 .and. p > 0_ilp ) then
                 call stdlib_slacpy( 'L', p, q, x11, ldx11, u1, ldu1 )
                 call stdlib_sorgqr( p, p, q, u1, ldu1, work(itaup1), work(iorgqr),lorgqr, &
                           childinfo )
              end if
              if( wantu2 .and. m-p > 0_ilp ) then
                 call stdlib_slacpy( 'L', m-p, q, x21, ldx21, u2, ldu2 )
                 call stdlib_sorgqr( m-p, m-p, q, u2, ldu2, work(itaup2),work(iorgqr), lorgqr, &
                           childinfo )
              end if
              if( wantv1t .and. q > 0_ilp ) then
                 v1t(1_ilp,1_ilp) = one
                 do j = 2, q
                    v1t(1_ilp,j) = zero
                    v1t(j,1_ilp) = zero
                 end do
                 call stdlib_slacpy( 'U', q-1, q-1, x21(1_ilp,2_ilp), ldx21, v1t(2_ilp,2_ilp),ldv1t )
                 call stdlib_sorglq( q-1, q-1, q-1, v1t(2_ilp,2_ilp), ldv1t, work(itauq1),work(iorglq), &
                           lorglq, childinfo )
              end if
              ! simultaneously diagonalize x11 and x21.
              call stdlib_sbbcsd( jobu1, jobu2, jobv1t, 'N', 'N', m, p, q, theta,work(iphi), u1, &
              ldu1, u2, ldu2, v1t, ldv1t,dum2, 1_ilp, work(ib11d), work(ib11e), work(ib12d),work(&
              ib12e), work(ib21d), work(ib21e),work(ib22d), work(ib22e), work(ibbcsd), lbbcsd,&
                        childinfo )
              ! permute rows and columns to place zero submatrices in
              ! preferred positions
              if( q > 0_ilp .and. wantu2 ) then
                 do i = 1, q
                    iwork(i) = m - p - q + i
                 end do
                 do i = q + 1, m - p
                    iwork(i) = i - q
                 end do
                 call stdlib_slapmt( .false., m-p, m-p, u2, ldu2, iwork )
              end if
           else if( r == p ) then
              ! case 2: r = p
              ! simultaneously bidiagonalize x11 and x21
              call stdlib_sorbdb2( m, p, q, x11, ldx11, x21, ldx21, theta,work(iphi), work(itaup1)&
                        , work(itaup2),work(itauq1), work(iorbdb), lorbdb, childinfo )
              ! accumulate householder reflectors
              if( wantu1 .and. p > 0_ilp ) then
                 u1(1_ilp,1_ilp) = one
                 do j = 2, p
                    u1(1_ilp,j) = zero
                    u1(j,1_ilp) = zero
                 end do
                 call stdlib_slacpy( 'L', p-1, p-1, x11(2_ilp,1_ilp), ldx11, u1(2_ilp,2_ilp), ldu1 )
                 call stdlib_sorgqr( p-1, p-1, p-1, u1(2_ilp,2_ilp), ldu1, work(itaup1),work(iorgqr), &
                           lorgqr, childinfo )
              end if
              if( wantu2 .and. m-p > 0_ilp ) then
                 call stdlib_slacpy( 'L', m-p, q, x21, ldx21, u2, ldu2 )
                 call stdlib_sorgqr( m-p, m-p, q, u2, ldu2, work(itaup2),work(iorgqr), lorgqr, &
                           childinfo )
              end if
              if( wantv1t .and. q > 0_ilp ) then
                 call stdlib_slacpy( 'U', p, q, x11, ldx11, v1t, ldv1t )
                 call stdlib_sorglq( q, q, r, v1t, ldv1t, work(itauq1),work(iorglq), lorglq, &
                           childinfo )
              end if
              ! simultaneously diagonalize x11 and x21.
              call stdlib_sbbcsd( jobv1t, 'N', jobu1, jobu2, 'T', m, q, p, theta,work(iphi), v1t, &
              ldv1t, dum1, 1_ilp, u1, ldu1, u2,ldu2, work(ib11d), work(ib11e), work(ib12d),work(ib12e)&
              , work(ib21d), work(ib21e),work(ib22d), work(ib22e), work(ibbcsd), lbbcsd,childinfo &
                        )
              ! permute rows and columns to place identity submatrices in
              ! preferred positions
              if( q > 0_ilp .and. wantu2 ) then
                 do i = 1, q
                    iwork(i) = m - p - q + i
                 end do
                 do i = q + 1, m - p
                    iwork(i) = i - q
                 end do
                 call stdlib_slapmt( .false., m-p, m-p, u2, ldu2, iwork )
              end if
           else if( r == m-p ) then
              ! case 3: r = m-p
              ! simultaneously bidiagonalize x11 and x21
              call stdlib_sorbdb3( m, p, q, x11, ldx11, x21, ldx21, theta,work(iphi), work(itaup1)&
                        , work(itaup2),work(itauq1), work(iorbdb), lorbdb, childinfo )
              ! accumulate householder reflectors
              if( wantu1 .and. p > 0_ilp ) then
                 call stdlib_slacpy( 'L', p, q, x11, ldx11, u1, ldu1 )
                 call stdlib_sorgqr( p, p, q, u1, ldu1, work(itaup1), work(iorgqr),lorgqr, &
                           childinfo )
              end if
              if( wantu2 .and. m-p > 0_ilp ) then
                 u2(1_ilp,1_ilp) = one
                 do j = 2, m-p
                    u2(1_ilp,j) = zero
                    u2(j,1_ilp) = zero
                 end do
                 call stdlib_slacpy( 'L', m-p-1, m-p-1, x21(2_ilp,1_ilp), ldx21, u2(2_ilp,2_ilp),ldu2 )
                 call stdlib_sorgqr( m-p-1, m-p-1, m-p-1, u2(2_ilp,2_ilp), ldu2,work(itaup2), work(iorgqr)&
                           , lorgqr, childinfo )
              end if
              if( wantv1t .and. q > 0_ilp ) then
                 call stdlib_slacpy( 'U', m-p, q, x21, ldx21, v1t, ldv1t )
                 call stdlib_sorglq( q, q, r, v1t, ldv1t, work(itauq1),work(iorglq), lorglq, &
                           childinfo )
              end if
              ! simultaneously diagonalize x11 and x21.
              call stdlib_sbbcsd( 'N', jobv1t, jobu2, jobu1, 'T', m, m-q, m-p,theta, work(iphi), &
              dum1, 1_ilp, v1t, ldv1t, u2,ldu2, u1, ldu1, work(ib11d), work(ib11e),work(ib12d), work(&
              ib12e), work(ib21d),work(ib21e), work(ib22d), work(ib22e),work(ibbcsd), lbbcsd, &
                        childinfo )
              ! permute rows and columns to place identity submatrices in
              ! preferred positions
              if( q > r ) then
                 do i = 1, r
                    iwork(i) = q - r + i
                 end do
                 do i = r + 1, q
                    iwork(i) = i - r
                 end do
                 if( wantu1 ) then
                    call stdlib_slapmt( .false., p, q, u1, ldu1, iwork )
                 end if
                 if( wantv1t ) then
                    call stdlib_slapmr( .false., q, q, v1t, ldv1t, iwork )
                 end if
              end if
           else
              ! case 4: r = m-q
              ! simultaneously bidiagonalize x11 and x21
              call stdlib_sorbdb4( m, p, q, x11, ldx11, x21, ldx21, theta,work(iphi), work(itaup1)&
              , work(itaup2),work(itauq1), work(iorbdb), work(iorbdb+m),lorbdb-m, childinfo )
                        
              ! accumulate householder reflectors
              if( wantu2 .and. m-p > 0_ilp ) then
                 call stdlib_scopy( m-p, work(iorbdb+p), 1_ilp, u2, 1_ilp )
              end if
              if( wantu1 .and. p > 0_ilp ) then
                 call stdlib_scopy( p, work(iorbdb), 1_ilp, u1, 1_ilp )
                 do j = 2, p
                    u1(1_ilp,j) = zero
                 end do
                 call stdlib_slacpy( 'L', p-1, m-q-1, x11(2_ilp,1_ilp), ldx11, u1(2_ilp,2_ilp),ldu1 )
                 call stdlib_sorgqr( p, p, m-q, u1, ldu1, work(itaup1),work(iorgqr), lorgqr, &
                           childinfo )
              end if
              if( wantu2 .and. m-p > 0_ilp ) then
                 do j = 2, m-p
                    u2(1_ilp,j) = zero
                 end do
                 call stdlib_slacpy( 'L', m-p-1, m-q-1, x21(2_ilp,1_ilp), ldx21, u2(2_ilp,2_ilp),ldu2 )
                 call stdlib_sorgqr( m-p, m-p, m-q, u2, ldu2, work(itaup2),work(iorgqr), lorgqr, &
                           childinfo )
              end if
              if( wantv1t .and. q > 0_ilp ) then
                 call stdlib_slacpy( 'U', m-q, q, x21, ldx21, v1t, ldv1t )
                 call stdlib_slacpy( 'U', p-(m-q), q-(m-q), x11(m-q+1,m-q+1), ldx11,v1t(m-q+1,m-q+&
                           1_ilp), ldv1t )
                 call stdlib_slacpy( 'U', -p+q, q-p, x21(m-q+1,p+1), ldx21,v1t(p+1,p+1), ldv1t )
                           
                 call stdlib_sorglq( q, q, q, v1t, ldv1t, work(itauq1),work(iorglq), lorglq, &
                           childinfo )
              end if
              ! simultaneously diagonalize x11 and x21.
              call stdlib_sbbcsd( jobu2, jobu1, 'N', jobv1t, 'N', m, m-p, m-q,theta, work(iphi), &
              u2, ldu2, u1, ldu1, dum1, 1_ilp,v1t, ldv1t, work(ib11d), work(ib11e), work(ib12d),work(&
              ib12e), work(ib21d), work(ib21e),work(ib22d), work(ib22e), work(ibbcsd), lbbcsd,&
                        childinfo )
              ! permute rows and columns to place identity submatrices in
              ! preferred positions
              if( p > r ) then
                 do i = 1, r
                    iwork(i) = p - r + i
                 end do
                 do i = r + 1, p
                    iwork(i) = i - r
                 end do
                 if( wantu1 ) then
                    call stdlib_slapmt( .false., p, p, u1, ldu1, iwork )
                 end if
                 if( wantv1t ) then
                    call stdlib_slapmr( .false., p, q, v1t, ldv1t, iwork )
                 end if
              end if
           end if
           return
     end subroutine stdlib_sorcsd2by1

     module subroutine stdlib_dorcsd2by1( jobu1, jobu2, jobv1t, m, p, q, x11, ldx11,x21, ldx21, theta, &
     !! DORCSD2BY1 computes the CS decomposition of an M-by-Q matrix X with
     !! orthonormal columns that has been partitioned into a 2-by-1 block
     !! structure:
     !! [  I1 0  0 ]
     !! [  0  C  0 ]
     !! [ X11 ]   [ U1 |    ] [  0  0  0 ]
     !! X = [-----] = [---------] [----------] V1**T .
     !! [ X21 ]   [    | U2 ] [  0  0  0 ]
     !! [  0  S  0 ]
     !! [  0  0  I2]
     !! X11 is P-by-Q. The orthogonal matrices U1, U2, and V1 are P-by-P,
     !! (M-P)-by-(M-P), and Q-by-Q, respectively. C and S are R-by-R
     !! nonnegative diagonal matrices satisfying C^2 + S^2 = I, in which
     !! R = MIN(P,M-P,Q,M-Q). I1 is a K1-by-K1 identity matrix and I2 is a
     !! K2-by-K2 identity matrix, where K1 = MAX(Q+P-M,0), K2 = MAX(Q-P,0).
               u1, ldu1, u2, ldu2, v1t,ldv1t, work, lwork, iwork, info )
        ! -- lapack computational routine (3.5.0_dp) --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: jobu1, jobu2, jobv1t
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldu1, ldu2, ldv1t, lwork, ldx11, ldx21, m, p, q
           ! Array Arguments 
           real(dp), intent(out) :: theta(*)
           real(dp), intent(out) :: u1(ldu1,*), u2(ldu2,*), v1t(ldv1t,*), work(*)
           real(dp), intent(inout) :: x11(ldx11,*), x21(ldx21,*)
           integer(ilp), intent(out) :: iwork(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: childinfo, i, ib11d, ib11e, ib12d, ib12e, ib21d, ib21e, ib22d, ib22e, &
           ibbcsd, iorbdb, iorglq, iorgqr, iphi, itaup1, itaup2, itauq1, j, lbbcsd, lorbdb, &
           lorglq, lorglqmin, lorglqopt, lorgqr, lorgqrmin, lorgqropt, lworkmin, lworkopt, &
                     r
           logical(lk) :: lquery, wantu1, wantu2, wantv1t
           ! Local Arrays 
           real(dp) :: dum1(1_ilp), dum2(1_ilp,1_ilp)
           ! Intrinsic Function 
           ! Executable Statements 
           ! test input arguments
           info = 0_ilp
           wantu1 = stdlib_lsame( jobu1, 'Y' )
           wantu2 = stdlib_lsame( jobu2, 'Y' )
           wantv1t = stdlib_lsame( jobv1t, 'Y' )
           lquery = lwork == -1_ilp
           if( m < 0_ilp ) then
              info = -4_ilp
           else if( p < 0_ilp .or. p > m ) then
              info = -5_ilp
           else if( q < 0_ilp .or. q > m ) then
              info = -6_ilp
           else if( ldx11 < max( 1_ilp, p ) ) then
              info = -8_ilp
           else if( ldx21 < max( 1_ilp, m-p ) ) then
              info = -10_ilp
           else if( wantu1 .and. ldu1 < max( 1_ilp, p ) ) then
              info = -13_ilp
           else if( wantu2 .and. ldu2 < max( 1_ilp, m - p ) ) then
              info = -15_ilp
           else if( wantv1t .and. ldv1t < max( 1_ilp, q ) ) then
              info = -17_ilp
           end if
           r = min( p, m-p, q, m-q )
           ! compute workspace
             ! work layout:
           ! |-------------------------------------------------------|
           ! | lworkopt (1)                                          |
           ! |-------------------------------------------------------|
           ! | phi (max(1,r-1))                                      |
           ! |-------------------------------------------------------|
           ! | taup1 (max(1,p))                        | b11d (r)    |
           ! | taup2 (max(1,m-p))                      | b11e (r-1)  |
           ! | tauq1 (max(1,q))                        | b12d (r)    |
           ! |-----------------------------------------| b12e (r-1)  |
           ! | stdlib_dorbdb work | stdlib_dorgqr work | stdlib_dorglq work | b21d (r)    |
           ! |             |             |             | b21e (r-1)  |
           ! |             |             |             | b22d (r)    |
           ! |             |             |             | b22e (r-1)  |
           ! |             |             |             | stdlib_dbbcsd work |
           ! |-------------------------------------------------------|
           if( info == 0_ilp ) then
              iphi = 2_ilp
              ib11d = iphi + max( 1_ilp, r-1 )
              ib11e = ib11d + max( 1_ilp, r )
              ib12d = ib11e + max( 1_ilp, r - 1_ilp )
              ib12e = ib12d + max( 1_ilp, r )
              ib21d = ib12e + max( 1_ilp, r - 1_ilp )
              ib21e = ib21d + max( 1_ilp, r )
              ib22d = ib21e + max( 1_ilp, r - 1_ilp )
              ib22e = ib22d + max( 1_ilp, r )
              ibbcsd = ib22e + max( 1_ilp, r - 1_ilp )
              itaup1 = iphi + max( 1_ilp, r-1 )
              itaup2 = itaup1 + max( 1_ilp, p )
              itauq1 = itaup2 + max( 1_ilp, m-p )
              iorbdb = itauq1 + max( 1_ilp, q )
              iorgqr = itauq1 + max( 1_ilp, q )
              iorglq = itauq1 + max( 1_ilp, q )
              lorgqrmin = 1_ilp
              lorgqropt = 1_ilp
              lorglqmin = 1_ilp
              lorglqopt = 1_ilp
              if( r == q ) then
                 call stdlib_dorbdb1( m, p, q, x11, ldx11, x21, ldx21, theta,dum1, dum1, dum1, &
                           dum1, work,-1_ilp, childinfo )
                 lorbdb = int( work(1_ilp),KIND=ilp)
                 if( wantu1 .and. p > 0_ilp ) then
                    call stdlib_dorgqr( p, p, q, u1, ldu1, dum1, work(1_ilp), -1_ilp,childinfo )
                    lorgqrmin = max( lorgqrmin, p )
                    lorgqropt = max( lorgqropt, int( work(1_ilp),KIND=ilp) )
                 endif
                 if( wantu2 .and. m-p > 0_ilp ) then
                    call stdlib_dorgqr( m-p, m-p, q, u2, ldu2, dum1, work(1_ilp),-1_ilp, childinfo )
                              
                    lorgqrmin = max( lorgqrmin, m-p )
                    lorgqropt = max( lorgqropt, int( work(1_ilp),KIND=ilp) )
                 end if
                 if( wantv1t .and. q > 0_ilp ) then
                    call stdlib_dorglq( q-1, q-1, q-1, v1t, ldv1t,dum1, work(1_ilp), -1_ilp, childinfo )
                              
                    lorglqmin = max( lorglqmin, q-1 )
                    lorglqopt = max( lorglqopt, int( work(1_ilp),KIND=ilp) )
                 end if
                 call stdlib_dbbcsd( jobu1, jobu2, jobv1t, 'N', 'N', m, p, q, theta,dum1, u1, &
                 ldu1, u2, ldu2, v1t, ldv1t,dum2, 1_ilp, dum1, dum1, dum1,dum1, dum1, dum1, dum1,dum1,&
                            work(1_ilp), -1_ilp, childinfo )
                 lbbcsd = int( work(1_ilp),KIND=ilp)
              else if( r == p ) then
                 call stdlib_dorbdb2( m, p, q, x11, ldx11, x21, ldx21, theta,dum1, dum1, dum1, &
                           dum1,work(1_ilp), -1_ilp, childinfo )
                 lorbdb = int( work(1_ilp),KIND=ilp)
                 if( wantu1 .and. p > 0_ilp ) then
                    call stdlib_dorgqr( p-1, p-1, p-1, u1(2_ilp,2_ilp), ldu1, dum1,work(1_ilp), -1_ilp, childinfo &
                              )
                    lorgqrmin = max( lorgqrmin, p-1 )
                    lorgqropt = max( lorgqropt, int( work(1_ilp),KIND=ilp) )
                 end if
                 if( wantu2 .and. m-p > 0_ilp ) then
                    call stdlib_dorgqr( m-p, m-p, q, u2, ldu2, dum1, work(1_ilp),-1_ilp, childinfo )
                              
                    lorgqrmin = max( lorgqrmin, m-p )
                    lorgqropt = max( lorgqropt, int( work(1_ilp),KIND=ilp) )
                 end if
                 if( wantv1t .and. q > 0_ilp ) then
                    call stdlib_dorglq( q, q, r, v1t, ldv1t, dum1, work(1_ilp), -1_ilp,childinfo )
                    lorglqmin = max( lorglqmin, q )
                    lorglqopt = max( lorglqopt, int( work(1_ilp),KIND=ilp) )
                 end if
                 call stdlib_dbbcsd( jobv1t, 'N', jobu1, jobu2, 'T', m, q, p, theta,dum1, v1t, &
                 ldv1t, dum2, 1_ilp, u1, ldu1,u2, ldu2, dum1, dum1, dum1,dum1, dum1, dum1, dum1,dum1, &
                           work(1_ilp), -1_ilp, childinfo )
                 lbbcsd = int( work(1_ilp),KIND=ilp)
              else if( r == m-p ) then
                 call stdlib_dorbdb3( m, p, q, x11, ldx11, x21, ldx21, theta,dum1, dum1, dum1, &
                           dum1,work(1_ilp), -1_ilp, childinfo )
                 lorbdb = int( work(1_ilp),KIND=ilp)
                 if( wantu1 .and. p > 0_ilp ) then
                    call stdlib_dorgqr( p, p, q, u1, ldu1, dum1, work(1_ilp), -1_ilp,childinfo )
                    lorgqrmin = max( lorgqrmin, p )
                    lorgqropt = max( lorgqropt, int( work(1_ilp),KIND=ilp) )
                 end if
                 if( wantu2 .and. m-p > 0_ilp ) then
                    call stdlib_dorgqr( m-p-1, m-p-1, m-p-1, u2(2_ilp,2_ilp), ldu2,dum1, work(1_ilp), -1_ilp, &
                              childinfo )
                    lorgqrmin = max( lorgqrmin, m-p-1 )
                    lorgqropt = max( lorgqropt, int( work(1_ilp),KIND=ilp) )
                 end if
                 if( wantv1t .and. q > 0_ilp ) then
                    call stdlib_dorglq( q, q, r, v1t, ldv1t, dum1, work(1_ilp), -1_ilp,childinfo )
                    lorglqmin = max( lorglqmin, q )
                    lorglqopt = max( lorglqopt, int( work(1_ilp),KIND=ilp) )
                 end if
                 call stdlib_dbbcsd( 'N', jobv1t, jobu2, jobu1, 'T', m, m-q, m-p,theta, dum1, &
                 dum2, 1_ilp, v1t, ldv1t, u2,ldu2, u1, ldu1, dum1, dum1, dum1,dum1, dum1, dum1, dum1,&
                           dum1, work(1_ilp), -1_ilp, childinfo )
                 lbbcsd = int( work(1_ilp),KIND=ilp)
              else
                 call stdlib_dorbdb4( m, p, q, x11, ldx11, x21, ldx21, theta,dum1, dum1, dum1, &
                           dum1,dum1, work(1_ilp), -1_ilp, childinfo )
                 lorbdb = m + int( work(1_ilp),KIND=ilp)
                 if( wantu1 .and. p > 0_ilp ) then
                    call stdlib_dorgqr( p, p, m-q, u1, ldu1, dum1, work(1_ilp), -1_ilp,childinfo )
                    lorgqrmin = max( lorgqrmin, p )
                    lorgqropt = max( lorgqropt, int( work(1_ilp),KIND=ilp) )
                 end if
                 if( wantu2 .and. m-p > 0_ilp ) then
                    call stdlib_dorgqr( m-p, m-p, m-q, u2, ldu2, dum1, work(1_ilp),-1_ilp, childinfo )
                              
                    lorgqrmin = max( lorgqrmin, m-p )
                    lorgqropt = max( lorgqropt, int( work(1_ilp),KIND=ilp) )
                 end if
                 if( wantv1t .and. q > 0_ilp ) then
                    call stdlib_dorglq( q, q, q, v1t, ldv1t, dum1, work(1_ilp), -1_ilp,childinfo )
                    lorglqmin = max( lorglqmin, q )
                    lorglqopt = max( lorglqopt, int( work(1_ilp),KIND=ilp) )
                 end if
                 call stdlib_dbbcsd( jobu2, jobu1, 'N', jobv1t, 'N', m, m-p, m-q,theta, dum1, u2, &
                 ldu2, u1, ldu1, dum2,1_ilp, v1t, ldv1t, dum1, dum1, dum1,dum1, dum1, dum1, dum1,dum1,&
                            work(1_ilp), -1_ilp, childinfo )
                 lbbcsd = int( work(1_ilp),KIND=ilp)
              end if
              lworkmin = max( iorbdb+lorbdb-1,iorgqr+lorgqrmin-1,iorglq+lorglqmin-1,ibbcsd+lbbcsd-&
                        1_ilp )
              lworkopt = max( iorbdb+lorbdb-1,iorgqr+lorgqropt-1,iorglq+lorglqopt-1,ibbcsd+lbbcsd-&
                        1_ilp )
              work(1_ilp) = lworkopt
              if( lwork < lworkmin .and. .not.lquery ) then
                 info = -19_ilp
              end if
           end if
           if( info /= 0_ilp ) then
              call stdlib_xerbla( 'DORCSD2BY1', -info )
              return
           else if( lquery ) then
              return
           end if
           lorgqr = lwork-iorgqr+1
           lorglq = lwork-iorglq+1
           ! handle four cases separately: r = q, r = p, r = m-p, and r = m-q,
           ! in which r = min(p,m-p,q,m-q)
           if( r == q ) then
              ! case 1: r = q
              ! simultaneously bidiagonalize x11 and x21
              call stdlib_dorbdb1( m, p, q, x11, ldx11, x21, ldx21, theta,work(iphi), work(itaup1)&
                        , work(itaup2),work(itauq1), work(iorbdb), lorbdb, childinfo )
              ! accumulate householder reflectors
              if( wantu1 .and. p > 0_ilp ) then
                 call stdlib_dlacpy( 'L', p, q, x11, ldx11, u1, ldu1 )
                 call stdlib_dorgqr( p, p, q, u1, ldu1, work(itaup1), work(iorgqr),lorgqr, &
                           childinfo )
              end if
              if( wantu2 .and. m-p > 0_ilp ) then
                 call stdlib_dlacpy( 'L', m-p, q, x21, ldx21, u2, ldu2 )
                 call stdlib_dorgqr( m-p, m-p, q, u2, ldu2, work(itaup2),work(iorgqr), lorgqr, &
                           childinfo )
              end if
              if( wantv1t .and. q > 0_ilp ) then
                 v1t(1_ilp,1_ilp) = one
                 do j = 2, q
                    v1t(1_ilp,j) = zero
                    v1t(j,1_ilp) = zero
                 end do
                 call stdlib_dlacpy( 'U', q-1, q-1, x21(1_ilp,2_ilp), ldx21, v1t(2_ilp,2_ilp),ldv1t )
                 call stdlib_dorglq( q-1, q-1, q-1, v1t(2_ilp,2_ilp), ldv1t, work(itauq1),work(iorglq), &
                           lorglq, childinfo )
              end if
              ! simultaneously diagonalize x11 and x21.
              call stdlib_dbbcsd( jobu1, jobu2, jobv1t, 'N', 'N', m, p, q, theta,work(iphi), u1, &
              ldu1, u2, ldu2, v1t, ldv1t,dum2, 1_ilp, work(ib11d), work(ib11e),work(ib12d), work(&
              ib12e), work(ib21d),work(ib21e), work(ib22d), work(ib22e),work(ibbcsd), lbbcsd, &
                        childinfo )
              ! permute rows and columns to place zero submatrices in
              ! preferred positions
              if( q > 0_ilp .and. wantu2 ) then
                 do i = 1, q
                    iwork(i) = m - p - q + i
                 end do
                 do i = q + 1, m - p
                    iwork(i) = i - q
                 end do
                 call stdlib_dlapmt( .false., m-p, m-p, u2, ldu2, iwork )
              end if
           else if( r == p ) then
              ! case 2: r = p
              ! simultaneously bidiagonalize x11 and x21
              call stdlib_dorbdb2( m, p, q, x11, ldx11, x21, ldx21, theta,work(iphi), work(itaup1)&
                        , work(itaup2),work(itauq1), work(iorbdb), lorbdb, childinfo )
              ! accumulate householder reflectors
              if( wantu1 .and. p > 0_ilp ) then
                 u1(1_ilp,1_ilp) = one
                 do j = 2, p
                    u1(1_ilp,j) = zero
                    u1(j,1_ilp) = zero
                 end do
                 call stdlib_dlacpy( 'L', p-1, p-1, x11(2_ilp,1_ilp), ldx11, u1(2_ilp,2_ilp), ldu1 )
                 call stdlib_dorgqr( p-1, p-1, p-1, u1(2_ilp,2_ilp), ldu1, work(itaup1),work(iorgqr), &
                           lorgqr, childinfo )
              end if
              if( wantu2 .and. m-p > 0_ilp ) then
                 call stdlib_dlacpy( 'L', m-p, q, x21, ldx21, u2, ldu2 )
                 call stdlib_dorgqr( m-p, m-p, q, u2, ldu2, work(itaup2),work(iorgqr), lorgqr, &
                           childinfo )
              end if
              if( wantv1t .and. q > 0_ilp ) then
                 call stdlib_dlacpy( 'U', p, q, x11, ldx11, v1t, ldv1t )
                 call stdlib_dorglq( q, q, r, v1t, ldv1t, work(itauq1),work(iorglq), lorglq, &
                           childinfo )
              end if
              ! simultaneously diagonalize x11 and x21.
              call stdlib_dbbcsd( jobv1t, 'N', jobu1, jobu2, 'T', m, q, p, theta,work(iphi), v1t, &
              ldv1t, dum2, 1_ilp, u1, ldu1, u2,ldu2, work(ib11d), work(ib11e), work(ib12d),work(ib12e)&
              , work(ib21d), work(ib21e),work(ib22d), work(ib22e), work(ibbcsd), lbbcsd,childinfo &
                        )
              ! permute rows and columns to place identity submatrices in
              ! preferred positions
              if( q > 0_ilp .and. wantu2 ) then
                 do i = 1, q
                    iwork(i) = m - p - q + i
                 end do
                 do i = q + 1, m - p
                    iwork(i) = i - q
                 end do
                 call stdlib_dlapmt( .false., m-p, m-p, u2, ldu2, iwork )
              end if
           else if( r == m-p ) then
              ! case 3: r = m-p
              ! simultaneously bidiagonalize x11 and x21
              call stdlib_dorbdb3( m, p, q, x11, ldx11, x21, ldx21, theta,work(iphi), work(itaup1)&
                        , work(itaup2),work(itauq1), work(iorbdb), lorbdb, childinfo )
              ! accumulate householder reflectors
              if( wantu1 .and. p > 0_ilp ) then
                 call stdlib_dlacpy( 'L', p, q, x11, ldx11, u1, ldu1 )
                 call stdlib_dorgqr( p, p, q, u1, ldu1, work(itaup1), work(iorgqr),lorgqr, &
                           childinfo )
              end if
              if( wantu2 .and. m-p > 0_ilp ) then
                 u2(1_ilp,1_ilp) = one
                 do j = 2, m-p
                    u2(1_ilp,j) = zero
                    u2(j,1_ilp) = zero
                 end do
                 call stdlib_dlacpy( 'L', m-p-1, m-p-1, x21(2_ilp,1_ilp), ldx21, u2(2_ilp,2_ilp),ldu2 )
                 call stdlib_dorgqr( m-p-1, m-p-1, m-p-1, u2(2_ilp,2_ilp), ldu2,work(itaup2), work(iorgqr)&
                           , lorgqr, childinfo )
              end if
              if( wantv1t .and. q > 0_ilp ) then
                 call stdlib_dlacpy( 'U', m-p, q, x21, ldx21, v1t, ldv1t )
                 call stdlib_dorglq( q, q, r, v1t, ldv1t, work(itauq1),work(iorglq), lorglq, &
                           childinfo )
              end if
              ! simultaneously diagonalize x11 and x21.
              call stdlib_dbbcsd( 'N', jobv1t, jobu2, jobu1, 'T', m, m-q, m-p,theta, work(iphi), &
              dum2, 1_ilp, v1t, ldv1t, u2,ldu2, u1, ldu1, work(ib11d), work(ib11e),work(ib12d), work(&
              ib12e), work(ib21d),work(ib21e), work(ib22d), work(ib22e),work(ibbcsd), lbbcsd, &
                        childinfo )
              ! permute rows and columns to place identity submatrices in
              ! preferred positions
              if( q > r ) then
                 do i = 1, r
                    iwork(i) = q - r + i
                 end do
                 do i = r + 1, q
                    iwork(i) = i - r
                 end do
                 if( wantu1 ) then
                    call stdlib_dlapmt( .false., p, q, u1, ldu1, iwork )
                 end if
                 if( wantv1t ) then
                    call stdlib_dlapmr( .false., q, q, v1t, ldv1t, iwork )
                 end if
              end if
           else
              ! case 4: r = m-q
              ! simultaneously bidiagonalize x11 and x21
              call stdlib_dorbdb4( m, p, q, x11, ldx11, x21, ldx21, theta,work(iphi), work(itaup1)&
              , work(itaup2),work(itauq1), work(iorbdb), work(iorbdb+m),lorbdb-m, childinfo )
                        
              ! accumulate householder reflectors
              if( wantu2 .and. m-p > 0_ilp ) then
                 call stdlib_dcopy( m-p, work(iorbdb+p), 1_ilp, u2, 1_ilp )
              end if
              if( wantu1 .and. p > 0_ilp ) then
                 call stdlib_dcopy( p, work(iorbdb), 1_ilp, u1, 1_ilp )
                 do j = 2, p
                    u1(1_ilp,j) = zero
                 end do
                 call stdlib_dlacpy( 'L', p-1, m-q-1, x11(2_ilp,1_ilp), ldx11, u1(2_ilp,2_ilp),ldu1 )
                 call stdlib_dorgqr( p, p, m-q, u1, ldu1, work(itaup1),work(iorgqr), lorgqr, &
                           childinfo )
              end if
              if( wantu2 .and. m-p > 0_ilp ) then
                 do j = 2, m-p
                    u2(1_ilp,j) = zero
                 end do
                 call stdlib_dlacpy( 'L', m-p-1, m-q-1, x21(2_ilp,1_ilp), ldx21, u2(2_ilp,2_ilp),ldu2 )
                 call stdlib_dorgqr( m-p, m-p, m-q, u2, ldu2, work(itaup2),work(iorgqr), lorgqr, &
                           childinfo )
              end if
              if( wantv1t .and. q > 0_ilp ) then
                 call stdlib_dlacpy( 'U', m-q, q, x21, ldx21, v1t, ldv1t )
                 call stdlib_dlacpy( 'U', p-(m-q), q-(m-q), x11(m-q+1,m-q+1), ldx11,v1t(m-q+1,m-q+&
                           1_ilp), ldv1t )
                 call stdlib_dlacpy( 'U', -p+q, q-p, x21(m-q+1,p+1), ldx21,v1t(p+1,p+1), ldv1t )
                           
                 call stdlib_dorglq( q, q, q, v1t, ldv1t, work(itauq1),work(iorglq), lorglq, &
                           childinfo )
              end if
              ! simultaneously diagonalize x11 and x21.
              call stdlib_dbbcsd( jobu2, jobu1, 'N', jobv1t, 'N', m, m-p, m-q,theta, work(iphi), &
              u2, ldu2, u1, ldu1, dum2,1_ilp, v1t, ldv1t, work(ib11d), work(ib11e),work(ib12d), work(&
              ib12e), work(ib21d),work(ib21e), work(ib22d), work(ib22e),work(ibbcsd), lbbcsd, &
                        childinfo )
              ! permute rows and columns to place identity submatrices in
              ! preferred positions
              if( p > r ) then
                 do i = 1, r
                    iwork(i) = p - r + i
                 end do
                 do i = r + 1, p
                    iwork(i) = i - r
                 end do
                 if( wantu1 ) then
                    call stdlib_dlapmt( .false., p, p, u1, ldu1, iwork )
                 end if
                 if( wantv1t ) then
                    call stdlib_dlapmr( .false., p, q, v1t, ldv1t, iwork )
                 end if
              end if
           end if
           return
     end subroutine stdlib_dorcsd2by1




     module subroutine stdlib_sorbdb( trans, signs, m, p, q, x11, ldx11, x12, ldx12,x21, ldx21, x22, &
     !! SORBDB simultaneously bidiagonalizes the blocks of an M-by-M
     !! partitioned orthogonal matrix X:
     !! [ B11 | B12 0  0 ]
     !! [ X11 | X12 ]   [ P1 |    ] [  0  |  0 -I  0 ] [ Q1 |    ]**T
     !! X = [-----------] = [---------] [----------------] [---------]   .
     !! [ X21 | X22 ]   [    | P2 ] [ B21 | B22 0  0 ] [    | Q2 ]
     !! [  0  |  0  0  I ]
     !! X11 is P-by-Q. Q must be no larger than P, M-P, or M-Q. (If this is
     !! not the case, then X must be transposed and/or permuted. This can be
     !! done in constant time using the TRANS and SIGNS options. See SORCSD
     !! for details.)
     !! The orthogonal matrices P1, P2, Q1, and Q2 are P-by-P, (M-P)-by-
     !! (M-P), Q-by-Q, and (M-Q)-by-(M-Q), respectively. They are
     !! represented implicitly by Householder vectors.
     !! B11, B12, B21, and B22 are Q-by-Q bidiagonal matrices represented
     !! implicitly by angles THETA, PHI.
               ldx22, theta, phi, taup1,taup2, tauq1, tauq2, work, lwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: signs, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldx11, ldx12, ldx21, ldx22, lwork, m, p, q
           ! Array Arguments 
           real(sp), intent(out) :: phi(*), theta(*)
           real(sp), intent(out) :: taup1(*), taup2(*), tauq1(*), tauq2(*), work(*)
           real(sp), intent(inout) :: x11(ldx11,*), x12(ldx12,*), x21(ldx21,*), x22(ldx22,*)
                     
        ! ====================================================================
           ! Parameters 
           
           
           
           ! Local Scalars 
           logical(lk) :: colmajor, lquery
           integer(ilp) :: i, lworkmin, lworkopt
           real(sp) :: z1, z2, z3, z4
           ! Intrinsic Functions
           ! Executable Statements 
           ! test input arguments
           info = 0_ilp
           colmajor = .not. stdlib_lsame( trans, 'T' )
           if( .not. stdlib_lsame( signs, 'O' ) ) then
              z1 = one
              z2 = one
              z3 = one
              z4 = one
           else
              z1 = one
              z2 = -one
              z3 = one
              z4 = -one
           end if
           lquery = lwork == -1_ilp
           if( m < 0_ilp ) then
              info = -3_ilp
           else if( p < 0_ilp .or. p > m ) then
              info = -4_ilp
           else if( q < 0_ilp .or. q > p .or. q > m-p .or.q > m-q ) then
              info = -5_ilp
           else if( colmajor .and. ldx11 < max( 1_ilp, p ) ) then
              info = -7_ilp
           else if( .not.colmajor .and. ldx11 < max( 1_ilp, q ) ) then
              info = -7_ilp
           else if( colmajor .and. ldx12 < max( 1_ilp, p ) ) then
              info = -9_ilp
           else if( .not.colmajor .and. ldx12 < max( 1_ilp, m-q ) ) then
              info = -9_ilp
           else if( colmajor .and. ldx21 < max( 1_ilp, m-p ) ) then
              info = -11_ilp
           else if( .not.colmajor .and. ldx21 < max( 1_ilp, q ) ) then
              info = -11_ilp
           else if( colmajor .and. ldx22 < max( 1_ilp, m-p ) ) then
              info = -13_ilp
           else if( .not.colmajor .and. ldx22 < max( 1_ilp, m-q ) ) then
              info = -13_ilp
           end if
           ! compute workspace
           if( info == 0_ilp ) then
              lworkopt = m - q
              lworkmin = m - q
              work(1_ilp) = lworkopt
              if( lwork < lworkmin .and. .not. lquery ) then
                 info = -21_ilp
              end if
           end if
           if( info /= 0_ilp ) then
              call stdlib_xerbla( 'XORBDB', -info )
              return
           else if( lquery ) then
              return
           end if
           ! handle column-major and row-major separately
           if( colmajor ) then
              ! reduce columns 1, ..., q of x11, x12, x21, and x22
              do i = 1, q
                 if( i == 1_ilp ) then
                    call stdlib_sscal( p-i+1, z1, x11(i,i), 1_ilp )
                 else
                    call stdlib_sscal( p-i+1, z1*cos(phi(i-1)), x11(i,i), 1_ilp )
                    call stdlib_saxpy( p-i+1, -z1*z3*z4*sin(phi(i-1)), x12(i,i-1),1_ilp, x11(i,i), 1_ilp )
                              
                 end if
                 if( i == 1_ilp ) then
                    call stdlib_sscal( m-p-i+1, z2, x21(i,i), 1_ilp )
                 else
                    call stdlib_sscal( m-p-i+1, z2*cos(phi(i-1)), x21(i,i), 1_ilp )
                    call stdlib_saxpy( m-p-i+1, -z2*z3*z4*sin(phi(i-1)), x22(i,i-1),1_ilp, x21(i,i), &
                              1_ilp )
                 end if
                 theta(i) = atan2( stdlib_snrm2( m-p-i+1, x21(i,i), 1_ilp ),stdlib_snrm2( p-i+1, x11(&
                           i,i), 1_ilp ) )
                 if( p > i ) then
                    call stdlib_slarfgp( p-i+1, x11(i,i), x11(i+1,i), 1_ilp, taup1(i) )
                 else if( p == i ) then
                    call stdlib_slarfgp( p-i+1, x11(i,i), x11(i,i), 1_ilp, taup1(i) )
                 end if
                 x11(i,i) = one
                 if ( m-p > i ) then
                    call stdlib_slarfgp( m-p-i+1, x21(i,i), x21(i+1,i), 1_ilp,taup2(i) )
                 else if ( m-p == i ) then
                    call stdlib_slarfgp( m-p-i+1, x21(i,i), x21(i,i), 1_ilp, taup2(i) )
                 end if
                 x21(i,i) = one
                 if ( q > i ) then
                    call stdlib_slarf( 'L', p-i+1, q-i, x11(i,i), 1_ilp, taup1(i),x11(i,i+1), ldx11, &
                              work )
                 end if
                 if ( m-q+1 > i ) then
                    call stdlib_slarf( 'L', p-i+1, m-q-i+1, x11(i,i), 1_ilp, taup1(i),x12(i,i), ldx12,&
                               work )
                 end if
                 if ( q > i ) then
                    call stdlib_slarf( 'L', m-p-i+1, q-i, x21(i,i), 1_ilp, taup2(i),x21(i,i+1), ldx21,&
                               work )
                 end if
                 if ( m-q+1 > i ) then
                    call stdlib_slarf( 'L', m-p-i+1, m-q-i+1, x21(i,i), 1_ilp, taup2(i),x22(i,i), &
                              ldx22, work )
                 end if
                 if( i < q ) then
                    call stdlib_sscal( q-i, -z1*z3*sin(theta(i)), x11(i,i+1),ldx11 )
                    call stdlib_saxpy( q-i, z2*z3*cos(theta(i)), x21(i,i+1), ldx21,x11(i,i+1), &
                              ldx11 )
                 end if
                 call stdlib_sscal( m-q-i+1, -z1*z4*sin(theta(i)), x12(i,i), ldx12 )
                 call stdlib_saxpy( m-q-i+1, z2*z4*cos(theta(i)), x22(i,i), ldx22,x12(i,i), ldx12 &
                           )
                 if( i < q )phi(i) = atan2( stdlib_snrm2( q-i, x11(i,i+1), ldx11 ),stdlib_snrm2( &
                           m-q-i+1, x12(i,i), ldx12 ) )
                 if( i < q ) then
                    if ( q-i == 1_ilp ) then
                       call stdlib_slarfgp( q-i, x11(i,i+1), x11(i,i+1), ldx11,tauq1(i) )
                    else
                       call stdlib_slarfgp( q-i, x11(i,i+1), x11(i,i+2), ldx11,tauq1(i) )
                    end if
                    x11(i,i+1) = one
                 end if
                 if ( q+i-1 < m ) then
                    if ( m-q == i ) then
                       call stdlib_slarfgp( m-q-i+1, x12(i,i), x12(i,i), ldx12,tauq2(i) )
                    else
                       call stdlib_slarfgp( m-q-i+1, x12(i,i), x12(i,i+1), ldx12,tauq2(i) )
                                 
                    end if
                 end if
                 x12(i,i) = one
                 if( i < q ) then
                    call stdlib_slarf( 'R', p-i, q-i, x11(i,i+1), ldx11, tauq1(i),x11(i+1,i+1), &
                              ldx11, work )
                    call stdlib_slarf( 'R', m-p-i, q-i, x11(i,i+1), ldx11, tauq1(i),x21(i+1,i+1), &
                              ldx21, work )
                 end if
                 if ( p > i ) then
                    call stdlib_slarf( 'R', p-i, m-q-i+1, x12(i,i), ldx12, tauq2(i),x12(i+1,i), &
                              ldx12, work )
                 end if
                 if ( m-p > i ) then
                    call stdlib_slarf( 'R', m-p-i, m-q-i+1, x12(i,i), ldx12,tauq2(i), x22(i+1,i), &
                              ldx22, work )
                 end if
              end do
              ! reduce columns q + 1, ..., p of x12, x22
              do i = q + 1, p
                 call stdlib_sscal( m-q-i+1, -z1*z4, x12(i,i), ldx12 )
                 if ( i >= m-q ) then
                    call stdlib_slarfgp( m-q-i+1, x12(i,i), x12(i,i), ldx12,tauq2(i) )
                 else
                    call stdlib_slarfgp( m-q-i+1, x12(i,i), x12(i,i+1), ldx12,tauq2(i) )
                 end if
                 x12(i,i) = one
                 if ( p > i ) then
                    call stdlib_slarf( 'R', p-i, m-q-i+1, x12(i,i), ldx12, tauq2(i),x12(i+1,i), &
                              ldx12, work )
                 end if
                 if( m-p-q >= 1_ilp )call stdlib_slarf( 'R', m-p-q, m-q-i+1, x12(i,i), ldx12,tauq2(i),&
                            x22(q+1,i), ldx22, work )
              end do
              ! reduce columns p + 1, ..., m - q of x12, x22
              do i = 1, m - p - q
                 call stdlib_sscal( m-p-q-i+1, z2*z4, x22(q+i,p+i), ldx22 )
                 if ( i == m-p-q ) then
                    call stdlib_slarfgp( m-p-q-i+1, x22(q+i,p+i), x22(q+i,p+i),ldx22, tauq2(p+i) )
                              
                 else
                    call stdlib_slarfgp( m-p-q-i+1, x22(q+i,p+i), x22(q+i,p+i+1),ldx22, tauq2(p+i)&
                               )
                 end if
                 x22(q+i,p+i) = one
                 if ( i < m-p-q ) then
                    call stdlib_slarf( 'R', m-p-q-i, m-p-q-i+1, x22(q+i,p+i), ldx22,tauq2(p+i), &
                              x22(q+i+1,p+i), ldx22, work )
                 end if
              end do
           else
              ! reduce columns 1, ..., q of x11, x12, x21, x22
              do i = 1, q
                 if( i == 1_ilp ) then
                    call stdlib_sscal( p-i+1, z1, x11(i,i), ldx11 )
                 else
                    call stdlib_sscal( p-i+1, z1*cos(phi(i-1)), x11(i,i), ldx11 )
                    call stdlib_saxpy( p-i+1, -z1*z3*z4*sin(phi(i-1)), x12(i-1,i),ldx12, x11(i,i),&
                               ldx11 )
                 end if
                 if( i == 1_ilp ) then
                    call stdlib_sscal( m-p-i+1, z2, x21(i,i), ldx21 )
                 else
                    call stdlib_sscal( m-p-i+1, z2*cos(phi(i-1)), x21(i,i), ldx21 )
                    call stdlib_saxpy( m-p-i+1, -z2*z3*z4*sin(phi(i-1)), x22(i-1,i),ldx22, x21(i,&
                              i), ldx21 )
                 end if
                 theta(i) = atan2( stdlib_snrm2( m-p-i+1, x21(i,i), ldx21 ),stdlib_snrm2( p-i+1, &
                           x11(i,i), ldx11 ) )
                 call stdlib_slarfgp( p-i+1, x11(i,i), x11(i,i+1), ldx11, taup1(i) )
                 x11(i,i) = one
                 if ( i == m-p ) then
                    call stdlib_slarfgp( m-p-i+1, x21(i,i), x21(i,i), ldx21,taup2(i) )
                 else
                    call stdlib_slarfgp( m-p-i+1, x21(i,i), x21(i,i+1), ldx21,taup2(i) )
                 end if
                 x21(i,i) = one
                 if ( q > i ) then
                    call stdlib_slarf( 'R', q-i, p-i+1, x11(i,i), ldx11, taup1(i),x11(i+1,i), &
                              ldx11, work )
                 end if
                 if ( m-q+1 > i ) then
                    call stdlib_slarf( 'R', m-q-i+1, p-i+1, x11(i,i), ldx11,taup1(i), x12(i,i), &
                              ldx12, work )
                 end if
                 if ( q > i ) then
                    call stdlib_slarf( 'R', q-i, m-p-i+1, x21(i,i), ldx21, taup2(i),x21(i+1,i), &
                              ldx21, work )
                 end if
                 if ( m-q+1 > i ) then
                    call stdlib_slarf( 'R', m-q-i+1, m-p-i+1, x21(i,i), ldx21,taup2(i), x22(i,i), &
                              ldx22, work )
                 end if
                 if( i < q ) then
                    call stdlib_sscal( q-i, -z1*z3*sin(theta(i)), x11(i+1,i), 1_ilp )
                    call stdlib_saxpy( q-i, z2*z3*cos(theta(i)), x21(i+1,i), 1_ilp,x11(i+1,i), 1_ilp )
                              
                 end if
                 call stdlib_sscal( m-q-i+1, -z1*z4*sin(theta(i)), x12(i,i), 1_ilp )
                 call stdlib_saxpy( m-q-i+1, z2*z4*cos(theta(i)), x22(i,i), 1_ilp,x12(i,i), 1_ilp )
                           
                 if( i < q )phi(i) = atan2( stdlib_snrm2( q-i, x11(i+1,i), 1_ilp ),stdlib_snrm2( m-q-&
                           i+1, x12(i,i), 1_ilp ) )
                 if( i < q ) then
                    if ( q-i == 1_ilp) then
                       call stdlib_slarfgp( q-i, x11(i+1,i), x11(i+1,i), 1_ilp,tauq1(i) )
                    else
                       call stdlib_slarfgp( q-i, x11(i+1,i), x11(i+2,i), 1_ilp,tauq1(i) )
                    end if
                    x11(i+1,i) = one
                 end if
                 if ( m-q > i ) then
                    call stdlib_slarfgp( m-q-i+1, x12(i,i), x12(i+1,i), 1_ilp,tauq2(i) )
                 else
                    call stdlib_slarfgp( m-q-i+1, x12(i,i), x12(i,i), 1_ilp,tauq2(i) )
                 end if
                 x12(i,i) = one
                 if( i < q ) then
                    call stdlib_slarf( 'L', q-i, p-i, x11(i+1,i), 1_ilp, tauq1(i),x11(i+1,i+1), ldx11,&
                               work )
                    call stdlib_slarf( 'L', q-i, m-p-i, x11(i+1,i), 1_ilp, tauq1(i),x21(i+1,i+1), &
                              ldx21, work )
                 end if
                 call stdlib_slarf( 'L', m-q-i+1, p-i, x12(i,i), 1_ilp, tauq2(i),x12(i,i+1), ldx12, &
                           work )
                 if ( m-p-i > 0_ilp ) then
                    call stdlib_slarf( 'L', m-q-i+1, m-p-i, x12(i,i), 1_ilp, tauq2(i),x22(i,i+1), &
                              ldx22, work )
                 end if
              end do
              ! reduce columns q + 1, ..., p of x12, x22
              do i = q + 1, p
                 call stdlib_sscal( m-q-i+1, -z1*z4, x12(i,i), 1_ilp )
                 call stdlib_slarfgp( m-q-i+1, x12(i,i), x12(i+1,i), 1_ilp, tauq2(i) )
                 x12(i,i) = one
                 if ( p > i ) then
                    call stdlib_slarf( 'L', m-q-i+1, p-i, x12(i,i), 1_ilp, tauq2(i),x12(i,i+1), ldx12,&
                               work )
                 end if
                 if( m-p-q >= 1_ilp )call stdlib_slarf( 'L', m-q-i+1, m-p-q, x12(i,i), 1_ilp, tauq2(i),&
                           x22(i,q+1), ldx22, work )
              end do
              ! reduce columns p + 1, ..., m - q of x12, x22
              do i = 1, m - p - q
                 call stdlib_sscal( m-p-q-i+1, z2*z4, x22(p+i,q+i), 1_ilp )
                 if ( m-p-q == i ) then
                    call stdlib_slarfgp( m-p-q-i+1, x22(p+i,q+i), x22(p+i,q+i), 1_ilp,tauq2(p+i) )
                              
                    x22(p+i,q+i) = one
                 else
                    call stdlib_slarfgp( m-p-q-i+1, x22(p+i,q+i), x22(p+i+1,q+i), 1_ilp,tauq2(p+i) )
                              
                    x22(p+i,q+i) = one
                    call stdlib_slarf( 'L', m-p-q-i+1, m-p-q-i, x22(p+i,q+i), 1_ilp,tauq2(p+i), x22(p+&
                              i,q+i+1), ldx22, work )
                 end if
              end do
           end if
           return
     end subroutine stdlib_sorbdb

     module subroutine stdlib_dorbdb( trans, signs, m, p, q, x11, ldx11, x12, ldx12,x21, ldx21, x22, &
     !! DORBDB simultaneously bidiagonalizes the blocks of an M-by-M
     !! partitioned orthogonal matrix X:
     !! [ B11 | B12 0  0 ]
     !! [ X11 | X12 ]   [ P1 |    ] [  0  |  0 -I  0 ] [ Q1 |    ]**T
     !! X = [-----------] = [---------] [----------------] [---------]   .
     !! [ X21 | X22 ]   [    | P2 ] [ B21 | B22 0  0 ] [    | Q2 ]
     !! [  0  |  0  0  I ]
     !! X11 is P-by-Q. Q must be no larger than P, M-P, or M-Q. (If this is
     !! not the case, then X must be transposed and/or permuted. This can be
     !! done in constant time using the TRANS and SIGNS options. See DORCSD
     !! for details.)
     !! The orthogonal matrices P1, P2, Q1, and Q2 are P-by-P, (M-P)-by-
     !! (M-P), Q-by-Q, and (M-Q)-by-(M-Q), respectively. They are
     !! represented implicitly by Householder vectors.
     !! B11, B12, B21, and B22 are Q-by-Q bidiagonal matrices represented
     !! implicitly by angles THETA, PHI.
               ldx22, theta, phi, taup1,taup2, tauq1, tauq2, work, lwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: signs, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldx11, ldx12, ldx21, ldx22, lwork, m, p, q
           ! Array Arguments 
           real(dp), intent(out) :: phi(*), theta(*)
           real(dp), intent(out) :: taup1(*), taup2(*), tauq1(*), tauq2(*), work(*)
           real(dp), intent(inout) :: x11(ldx11,*), x12(ldx12,*), x21(ldx21,*), x22(ldx22,*)
                     
        ! ====================================================================
           ! Parameters 
           
           
           
           ! Local Scalars 
           logical(lk) :: colmajor, lquery
           integer(ilp) :: i, lworkmin, lworkopt
           real(dp) :: z1, z2, z3, z4
           ! Intrinsic Functions
           ! Executable Statements 
           ! test input arguments
           info = 0_ilp
           colmajor = .not. stdlib_lsame( trans, 'T' )
           if( .not. stdlib_lsame( signs, 'O' ) ) then
              z1 = one
              z2 = one
              z3 = one
              z4 = one
           else
              z1 = one
              z2 = -one
              z3 = one
              z4 = -one
           end if
           lquery = lwork == -1_ilp
           if( m < 0_ilp ) then
              info = -3_ilp
           else if( p < 0_ilp .or. p > m ) then
              info = -4_ilp
           else if( q < 0_ilp .or. q > p .or. q > m-p .or.q > m-q ) then
              info = -5_ilp
           else if( colmajor .and. ldx11 < max( 1_ilp, p ) ) then
              info = -7_ilp
           else if( .not.colmajor .and. ldx11 < max( 1_ilp, q ) ) then
              info = -7_ilp
           else if( colmajor .and. ldx12 < max( 1_ilp, p ) ) then
              info = -9_ilp
           else if( .not.colmajor .and. ldx12 < max( 1_ilp, m-q ) ) then
              info = -9_ilp
           else if( colmajor .and. ldx21 < max( 1_ilp, m-p ) ) then
              info = -11_ilp
           else if( .not.colmajor .and. ldx21 < max( 1_ilp, q ) ) then
              info = -11_ilp
           else if( colmajor .and. ldx22 < max( 1_ilp, m-p ) ) then
              info = -13_ilp
           else if( .not.colmajor .and. ldx22 < max( 1_ilp, m-q ) ) then
              info = -13_ilp
           end if
           ! compute workspace
           if( info == 0_ilp ) then
              lworkopt = m - q
              lworkmin = m - q
              work(1_ilp) = lworkopt
              if( lwork < lworkmin .and. .not. lquery ) then
                 info = -21_ilp
              end if
           end if
           if( info /= 0_ilp ) then
              call stdlib_xerbla( 'XORBDB', -info )
              return
           else if( lquery ) then
              return
           end if
           ! handle column-major and row-major separately
           if( colmajor ) then
              ! reduce columns 1, ..., q of x11, x12, x21, and x22
              do i = 1, q
                 if( i == 1_ilp ) then
                    call stdlib_dscal( p-i+1, z1, x11(i,i), 1_ilp )
                 else
                    call stdlib_dscal( p-i+1, z1*cos(phi(i-1)), x11(i,i), 1_ilp )
                    call stdlib_daxpy( p-i+1, -z1*z3*z4*sin(phi(i-1)), x12(i,i-1),1_ilp, x11(i,i), 1_ilp )
                              
                 end if
                 if( i == 1_ilp ) then
                    call stdlib_dscal( m-p-i+1, z2, x21(i,i), 1_ilp )
                 else
                    call stdlib_dscal( m-p-i+1, z2*cos(phi(i-1)), x21(i,i), 1_ilp )
                    call stdlib_daxpy( m-p-i+1, -z2*z3*z4*sin(phi(i-1)), x22(i,i-1),1_ilp, x21(i,i), &
                              1_ilp )
                 end if
                 theta(i) = atan2( stdlib_dnrm2( m-p-i+1, x21(i,i), 1_ilp ),stdlib_dnrm2( p-i+1, x11(&
                           i,i), 1_ilp ) )
                 if( p > i ) then
                    call stdlib_dlarfgp( p-i+1, x11(i,i), x11(i+1,i), 1_ilp, taup1(i) )
                 else if( p == i ) then
                    call stdlib_dlarfgp( p-i+1, x11(i,i), x11(i,i), 1_ilp, taup1(i) )
                 end if
                 x11(i,i) = one
                 if ( m-p > i ) then
                    call stdlib_dlarfgp( m-p-i+1, x21(i,i), x21(i+1,i), 1_ilp,taup2(i) )
                 else if ( m-p == i ) then
                    call stdlib_dlarfgp( m-p-i+1, x21(i,i), x21(i,i), 1_ilp, taup2(i) )
                 end if
                 x21(i,i) = one
                 if ( q > i ) then
                    call stdlib_dlarf( 'L', p-i+1, q-i, x11(i,i), 1_ilp, taup1(i),x11(i,i+1), ldx11, &
                              work )
                 end if
                 if ( m-q+1 > i ) then
                    call stdlib_dlarf( 'L', p-i+1, m-q-i+1, x11(i,i), 1_ilp, taup1(i),x12(i,i), ldx12,&
                               work )
                 end if
                 if ( q > i ) then
                    call stdlib_dlarf( 'L', m-p-i+1, q-i, x21(i,i), 1_ilp, taup2(i),x21(i,i+1), ldx21,&
                               work )
                 end if
                 if ( m-q+1 > i ) then
                    call stdlib_dlarf( 'L', m-p-i+1, m-q-i+1, x21(i,i), 1_ilp, taup2(i),x22(i,i), &
                              ldx22, work )
                 end if
                 if( i < q ) then
                    call stdlib_dscal( q-i, -z1*z3*sin(theta(i)), x11(i,i+1),ldx11 )
                    call stdlib_daxpy( q-i, z2*z3*cos(theta(i)), x21(i,i+1), ldx21,x11(i,i+1), &
                              ldx11 )
                 end if
                 call stdlib_dscal( m-q-i+1, -z1*z4*sin(theta(i)), x12(i,i), ldx12 )
                 call stdlib_daxpy( m-q-i+1, z2*z4*cos(theta(i)), x22(i,i), ldx22,x12(i,i), ldx12 &
                           )
                 if( i < q )phi(i) = atan2( stdlib_dnrm2( q-i, x11(i,i+1), ldx11 ),stdlib_dnrm2( &
                           m-q-i+1, x12(i,i), ldx12 ) )
                 if( i < q ) then
                    if ( q-i == 1_ilp ) then
                       call stdlib_dlarfgp( q-i, x11(i,i+1), x11(i,i+1), ldx11,tauq1(i) )
                    else
                       call stdlib_dlarfgp( q-i, x11(i,i+1), x11(i,i+2), ldx11,tauq1(i) )
                    end if
                    x11(i,i+1) = one
                 end if
                 if ( q+i-1 < m ) then
                    if ( m-q == i ) then
                       call stdlib_dlarfgp( m-q-i+1, x12(i,i), x12(i,i), ldx12,tauq2(i) )
                    else
                       call stdlib_dlarfgp( m-q-i+1, x12(i,i), x12(i,i+1), ldx12,tauq2(i) )
                                 
                    end if
                 end if
                 x12(i,i) = one
                 if( i < q ) then
                    call stdlib_dlarf( 'R', p-i, q-i, x11(i,i+1), ldx11, tauq1(i),x11(i+1,i+1), &
                              ldx11, work )
                    call stdlib_dlarf( 'R', m-p-i, q-i, x11(i,i+1), ldx11, tauq1(i),x21(i+1,i+1), &
                              ldx21, work )
                 end if
                 if ( p > i ) then
                    call stdlib_dlarf( 'R', p-i, m-q-i+1, x12(i,i), ldx12, tauq2(i),x12(i+1,i), &
                              ldx12, work )
                 end if
                 if ( m-p > i ) then
                    call stdlib_dlarf( 'R', m-p-i, m-q-i+1, x12(i,i), ldx12,tauq2(i), x22(i+1,i), &
                              ldx22, work )
                 end if
              end do
              ! reduce columns q + 1, ..., p of x12, x22
              do i = q + 1, p
                 call stdlib_dscal( m-q-i+1, -z1*z4, x12(i,i), ldx12 )
                 if ( i >= m-q ) then
                    call stdlib_dlarfgp( m-q-i+1, x12(i,i), x12(i,i), ldx12,tauq2(i) )
                 else
                    call stdlib_dlarfgp( m-q-i+1, x12(i,i), x12(i,i+1), ldx12,tauq2(i) )
                 end if
                 x12(i,i) = one
                 if ( p > i ) then
                    call stdlib_dlarf( 'R', p-i, m-q-i+1, x12(i,i), ldx12, tauq2(i),x12(i+1,i), &
                              ldx12, work )
                 end if
                 if( m-p-q >= 1_ilp )call stdlib_dlarf( 'R', m-p-q, m-q-i+1, x12(i,i), ldx12,tauq2(i),&
                            x22(q+1,i), ldx22, work )
              end do
              ! reduce columns p + 1, ..., m - q of x12, x22
              do i = 1, m - p - q
                 call stdlib_dscal( m-p-q-i+1, z2*z4, x22(q+i,p+i), ldx22 )
                 if ( i == m-p-q ) then
                    call stdlib_dlarfgp( m-p-q-i+1, x22(q+i,p+i), x22(q+i,p+i),ldx22, tauq2(p+i) )
                              
                 else
                    call stdlib_dlarfgp( m-p-q-i+1, x22(q+i,p+i), x22(q+i,p+i+1),ldx22, tauq2(p+i)&
                               )
                 end if
                 x22(q+i,p+i) = one
                 if ( i < m-p-q ) then
                    call stdlib_dlarf( 'R', m-p-q-i, m-p-q-i+1, x22(q+i,p+i), ldx22,tauq2(p+i), &
                              x22(q+i+1,p+i), ldx22, work )
                 end if
              end do
           else
              ! reduce columns 1, ..., q of x11, x12, x21, x22
              do i = 1, q
                 if( i == 1_ilp ) then
                    call stdlib_dscal( p-i+1, z1, x11(i,i), ldx11 )
                 else
                    call stdlib_dscal( p-i+1, z1*cos(phi(i-1)), x11(i,i), ldx11 )
                    call stdlib_daxpy( p-i+1, -z1*z3*z4*sin(phi(i-1)), x12(i-1,i),ldx12, x11(i,i),&
                               ldx11 )
                 end if
                 if( i == 1_ilp ) then
                    call stdlib_dscal( m-p-i+1, z2, x21(i,i), ldx21 )
                 else
                    call stdlib_dscal( m-p-i+1, z2*cos(phi(i-1)), x21(i,i), ldx21 )
                    call stdlib_daxpy( m-p-i+1, -z2*z3*z4*sin(phi(i-1)), x22(i-1,i),ldx22, x21(i,&
                              i), ldx21 )
                 end if
                 theta(i) = atan2( stdlib_dnrm2( m-p-i+1, x21(i,i), ldx21 ),stdlib_dnrm2( p-i+1, &
                           x11(i,i), ldx11 ) )
                 call stdlib_dlarfgp( p-i+1, x11(i,i), x11(i,i+1), ldx11, taup1(i) )
                 x11(i,i) = one
                 if ( i == m-p ) then
                    call stdlib_dlarfgp( m-p-i+1, x21(i,i), x21(i,i), ldx21,taup2(i) )
                 else
                    call stdlib_dlarfgp( m-p-i+1, x21(i,i), x21(i,i+1), ldx21,taup2(i) )
                 end if
                 x21(i,i) = one
                 if ( q > i ) then
                    call stdlib_dlarf( 'R', q-i, p-i+1, x11(i,i), ldx11, taup1(i),x11(i+1,i), &
                              ldx11, work )
                 end if
                 if ( m-q+1 > i ) then
                    call stdlib_dlarf( 'R', m-q-i+1, p-i+1, x11(i,i), ldx11,taup1(i), x12(i,i), &
                              ldx12, work )
                 end if
                 if ( q > i ) then
                    call stdlib_dlarf( 'R', q-i, m-p-i+1, x21(i,i), ldx21, taup2(i),x21(i+1,i), &
                              ldx21, work )
                 end if
                 if ( m-q+1 > i ) then
                    call stdlib_dlarf( 'R', m-q-i+1, m-p-i+1, x21(i,i), ldx21,taup2(i), x22(i,i), &
                              ldx22, work )
                 end if
                 if( i < q ) then
                    call stdlib_dscal( q-i, -z1*z3*sin(theta(i)), x11(i+1,i), 1_ilp )
                    call stdlib_daxpy( q-i, z2*z3*cos(theta(i)), x21(i+1,i), 1_ilp,x11(i+1,i), 1_ilp )
                              
                 end if
                 call stdlib_dscal( m-q-i+1, -z1*z4*sin(theta(i)), x12(i,i), 1_ilp )
                 call stdlib_daxpy( m-q-i+1, z2*z4*cos(theta(i)), x22(i,i), 1_ilp,x12(i,i), 1_ilp )
                           
                 if( i < q )phi(i) = atan2( stdlib_dnrm2( q-i, x11(i+1,i), 1_ilp ),stdlib_dnrm2( m-q-&
                           i+1, x12(i,i), 1_ilp ) )
                 if( i < q ) then
                    if ( q-i == 1_ilp) then
                       call stdlib_dlarfgp( q-i, x11(i+1,i), x11(i+1,i), 1_ilp,tauq1(i) )
                    else
                       call stdlib_dlarfgp( q-i, x11(i+1,i), x11(i+2,i), 1_ilp,tauq1(i) )
                    end if
                    x11(i+1,i) = one
                 end if
                 if ( m-q > i ) then
                    call stdlib_dlarfgp( m-q-i+1, x12(i,i), x12(i+1,i), 1_ilp,tauq2(i) )
                 else
                    call stdlib_dlarfgp( m-q-i+1, x12(i,i), x12(i,i), 1_ilp,tauq2(i) )
                 end if
                 x12(i,i) = one
                 if( i < q ) then
                    call stdlib_dlarf( 'L', q-i, p-i, x11(i+1,i), 1_ilp, tauq1(i),x11(i+1,i+1), ldx11,&
                               work )
                    call stdlib_dlarf( 'L', q-i, m-p-i, x11(i+1,i), 1_ilp, tauq1(i),x21(i+1,i+1), &
                              ldx21, work )
                 end if
                 call stdlib_dlarf( 'L', m-q-i+1, p-i, x12(i,i), 1_ilp, tauq2(i),x12(i,i+1), ldx12, &
                           work )
                 if ( m-p-i > 0_ilp ) then
                    call stdlib_dlarf( 'L', m-q-i+1, m-p-i, x12(i,i), 1_ilp, tauq2(i),x22(i,i+1), &
                              ldx22, work )
                 end if
              end do
              ! reduce columns q + 1, ..., p of x12, x22
              do i = q + 1, p
                 call stdlib_dscal( m-q-i+1, -z1*z4, x12(i,i), 1_ilp )
                 call stdlib_dlarfgp( m-q-i+1, x12(i,i), x12(i+1,i), 1_ilp, tauq2(i) )
                 x12(i,i) = one
                 if ( p > i ) then
                    call stdlib_dlarf( 'L', m-q-i+1, p-i, x12(i,i), 1_ilp, tauq2(i),x12(i,i+1), ldx12,&
                               work )
                 end if
                 if( m-p-q >= 1_ilp )call stdlib_dlarf( 'L', m-q-i+1, m-p-q, x12(i,i), 1_ilp, tauq2(i),&
                           x22(i,q+1), ldx22, work )
              end do
              ! reduce columns p + 1, ..., m - q of x12, x22
              do i = 1, m - p - q
                 call stdlib_dscal( m-p-q-i+1, z2*z4, x22(p+i,q+i), 1_ilp )
                 if ( m-p-q == i ) then
                    call stdlib_dlarfgp( m-p-q-i+1, x22(p+i,q+i), x22(p+i,q+i), 1_ilp,tauq2(p+i) )
                              
                 else
                    call stdlib_dlarfgp( m-p-q-i+1, x22(p+i,q+i), x22(p+i+1,q+i), 1_ilp,tauq2(p+i) )
                              
                    call stdlib_dlarf( 'L', m-p-q-i+1, m-p-q-i, x22(p+i,q+i), 1_ilp,tauq2(p+i), x22(p+&
                              i,q+i+1), ldx22, work )
                 end if
                 x22(p+i,q+i) = one
              end do
           end if
           return
     end subroutine stdlib_dorbdb




     module subroutine stdlib_sorbdb1( m, p, q, x11, ldx11, x21, ldx21, theta, phi,taup1, taup2, tauq1, &
     !! SORBDB1 simultaneously bidiagonalizes the blocks of a tall and skinny
     !! matrix X with orthonomal columns:
     !! [ B11 ]
     !! [ X11 ]   [ P1 |    ] [  0  ]
     !! [-----] = [---------] [-----] Q1**T .
     !! [ X21 ]   [    | P2 ] [ B21 ]
     !! [  0  ]
     !! X11 is P-by-Q, and X21 is (M-P)-by-Q. Q must be no larger than P,
     !! M-P, or M-Q. Routines SORBDB2, SORBDB3, and SORBDB4 handle cases in
     !! which Q is not the minimum dimension.
     !! The orthogonal matrices P1, P2, and Q1 are P-by-P, (M-P)-by-(M-P),
     !! and (M-Q)-by-(M-Q), respectively. They are represented implicitly by
     !! Householder vectors.
     !! B11 and B12 are Q-by-Q bidiagonal matrices represented implicitly by
     !! angles THETA, PHI.
               work, lwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lwork, m, p, q, ldx11, ldx21
           ! Array Arguments 
           real(sp), intent(out) :: phi(*), theta(*)
           real(sp), intent(out) :: taup1(*), taup2(*), tauq1(*), work(*)
           real(sp), intent(inout) :: x11(ldx11,*), x21(ldx21,*)
        ! ====================================================================
           
           ! Local Scalars 
           real(sp) :: c, s
           integer(ilp) :: childinfo, i, ilarf, iorbdb5, llarf, lorbdb5, lworkmin, &
                     lworkopt
           logical(lk) :: lquery
           ! Intrinsic Function 
           ! Executable Statements 
           ! test input arguments
           info = 0_ilp
           lquery = lwork == -1_ilp
           if( m < 0_ilp ) then
              info = -1_ilp
           else if( p < q .or. m-p < q ) then
              info = -2_ilp
           else if( q < 0_ilp .or. m-q < q ) then
              info = -3_ilp
           else if( ldx11 < max( 1_ilp, p ) ) then
              info = -5_ilp
           else if( ldx21 < max( 1_ilp, m-p ) ) then
              info = -7_ilp
           end if
           ! compute workspace
           if( info == 0_ilp ) then
              ilarf = 2_ilp
              llarf = max( p-1, m-p-1, q-1 )
              iorbdb5 = 2_ilp
              lorbdb5 = q-2
              lworkopt = max( ilarf+llarf-1, iorbdb5+lorbdb5-1 )
              lworkmin = lworkopt
              work(1_ilp) = lworkopt
              if( lwork < lworkmin .and. .not.lquery ) then
                info = -14_ilp
              end if
           end if
           if( info /= 0_ilp ) then
              call stdlib_xerbla( 'SORBDB1', -info )
              return
           else if( lquery ) then
              return
           end if
           ! reduce columns 1, ..., q of x11 and x21
           do i = 1, q
              call stdlib_slarfgp( p-i+1, x11(i,i), x11(i+1,i), 1_ilp, taup1(i) )
              call stdlib_slarfgp( m-p-i+1, x21(i,i), x21(i+1,i), 1_ilp, taup2(i) )
              theta(i) = atan2( x21(i,i), x11(i,i) )
              c = cos( theta(i) )
              s = sin( theta(i) )
              x11(i,i) = one
              x21(i,i) = one
              call stdlib_slarf( 'L', p-i+1, q-i, x11(i,i), 1_ilp, taup1(i), x11(i,i+1),ldx11, work(&
                        ilarf) )
              call stdlib_slarf( 'L', m-p-i+1, q-i, x21(i,i), 1_ilp, taup2(i),x21(i,i+1), ldx21, work(&
                        ilarf) )
              if( i < q ) then
                 call stdlib_srot( q-i, x11(i,i+1), ldx11, x21(i,i+1), ldx21, c, s )
                 call stdlib_slarfgp( q-i, x21(i,i+1), x21(i,i+2), ldx21, tauq1(i) )
                 s = x21(i,i+1)
                 x21(i,i+1) = one
                 call stdlib_slarf( 'R', p-i, q-i, x21(i,i+1), ldx21, tauq1(i),x11(i+1,i+1), &
                           ldx11, work(ilarf) )
                 call stdlib_slarf( 'R', m-p-i, q-i, x21(i,i+1), ldx21, tauq1(i),x21(i+1,i+1), &
                           ldx21, work(ilarf) )
                 c = sqrt( stdlib_snrm2( p-i, x11(i+1,i+1), 1_ilp )**2_ilp+ stdlib_snrm2( m-p-i, x21(i+1,&
                           i+1), 1_ilp )**2_ilp )
                 phi(i) = atan2( s, c )
                 call stdlib_sorbdb5( p-i, m-p-i, q-i-1, x11(i+1,i+1), 1_ilp,x21(i+1,i+1), 1_ilp, x11(i+1,&
                           i+2), ldx11,x21(i+1,i+2), ldx21, work(iorbdb5), lorbdb5,childinfo )
              end if
           end do
           return
     end subroutine stdlib_sorbdb1

     module subroutine stdlib_dorbdb1( m, p, q, x11, ldx11, x21, ldx21, theta, phi,taup1, taup2, tauq1, &
     !! DORBDB1 simultaneously bidiagonalizes the blocks of a tall and skinny
     !! matrix X with orthonomal columns:
     !! [ B11 ]
     !! [ X11 ]   [ P1 |    ] [  0  ]
     !! [-----] = [---------] [-----] Q1**T .
     !! [ X21 ]   [    | P2 ] [ B21 ]
     !! [  0  ]
     !! X11 is P-by-Q, and X21 is (M-P)-by-Q. Q must be no larger than P,
     !! M-P, or M-Q. Routines DORBDB2, DORBDB3, and DORBDB4 handle cases in
     !! which Q is not the minimum dimension.
     !! The orthogonal matrices P1, P2, and Q1 are P-by-P, (M-P)-by-(M-P),
     !! and (M-Q)-by-(M-Q), respectively. They are represented implicitly by
     !! Householder vectors.
     !! B11 and B12 are Q-by-Q bidiagonal matrices represented implicitly by
     !! angles THETA, PHI.
               work, lwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lwork, m, p, q, ldx11, ldx21
           ! Array Arguments 
           real(dp), intent(out) :: phi(*), theta(*)
           real(dp), intent(out) :: taup1(*), taup2(*), tauq1(*), work(*)
           real(dp), intent(inout) :: x11(ldx11,*), x21(ldx21,*)
        ! ====================================================================
           
           ! Local Scalars 
           real(dp) :: c, s
           integer(ilp) :: childinfo, i, ilarf, iorbdb5, llarf, lorbdb5, lworkmin, &
                     lworkopt
           logical(lk) :: lquery
           ! Intrinsic Function 
           ! Executable Statements 
           ! test input arguments
           info = 0_ilp
           lquery = lwork == -1_ilp
           if( m < 0_ilp ) then
              info = -1_ilp
           else if( p < q .or. m-p < q ) then
              info = -2_ilp
           else if( q < 0_ilp .or. m-q < q ) then
              info = -3_ilp
           else if( ldx11 < max( 1_ilp, p ) ) then
              info = -5_ilp
           else if( ldx21 < max( 1_ilp, m-p ) ) then
              info = -7_ilp
           end if
           ! compute workspace
           if( info == 0_ilp ) then
              ilarf = 2_ilp
              llarf = max( p-1, m-p-1, q-1 )
              iorbdb5 = 2_ilp
              lorbdb5 = q-2
              lworkopt = max( ilarf+llarf-1, iorbdb5+lorbdb5-1 )
              lworkmin = lworkopt
              work(1_ilp) = lworkopt
              if( lwork < lworkmin .and. .not.lquery ) then
                info = -14_ilp
              end if
           end if
           if( info /= 0_ilp ) then
              call stdlib_xerbla( 'DORBDB1', -info )
              return
           else if( lquery ) then
              return
           end if
           ! reduce columns 1, ..., q of x11 and x21
           do i = 1, q
              call stdlib_dlarfgp( p-i+1, x11(i,i), x11(i+1,i), 1_ilp, taup1(i) )
              call stdlib_dlarfgp( m-p-i+1, x21(i,i), x21(i+1,i), 1_ilp, taup2(i) )
              theta(i) = atan2( x21(i,i), x11(i,i) )
              c = cos( theta(i) )
              s = sin( theta(i) )
              x11(i,i) = one
              x21(i,i) = one
              call stdlib_dlarf( 'L', p-i+1, q-i, x11(i,i), 1_ilp, taup1(i), x11(i,i+1),ldx11, work(&
                        ilarf) )
              call stdlib_dlarf( 'L', m-p-i+1, q-i, x21(i,i), 1_ilp, taup2(i),x21(i,i+1), ldx21, work(&
                        ilarf) )
              if( i < q ) then
                 call stdlib_drot( q-i, x11(i,i+1), ldx11, x21(i,i+1), ldx21, c, s )
                 call stdlib_dlarfgp( q-i, x21(i,i+1), x21(i,i+2), ldx21, tauq1(i) )
                 s = x21(i,i+1)
                 x21(i,i+1) = one
                 call stdlib_dlarf( 'R', p-i, q-i, x21(i,i+1), ldx21, tauq1(i),x11(i+1,i+1), &
                           ldx11, work(ilarf) )
                 call stdlib_dlarf( 'R', m-p-i, q-i, x21(i,i+1), ldx21, tauq1(i),x21(i+1,i+1), &
                           ldx21, work(ilarf) )
                 c = sqrt( stdlib_dnrm2( p-i, x11(i+1,i+1), 1_ilp )**2_ilp+ stdlib_dnrm2( m-p-i, x21(i+1,&
                           i+1), 1_ilp )**2_ilp )
                 phi(i) = atan2( s, c )
                 call stdlib_dorbdb5( p-i, m-p-i, q-i-1, x11(i+1,i+1), 1_ilp,x21(i+1,i+1), 1_ilp, x11(i+1,&
                           i+2), ldx11,x21(i+1,i+2), ldx21, work(iorbdb5), lorbdb5,childinfo )
              end if
           end do
           return
     end subroutine stdlib_dorbdb1




     module subroutine stdlib_sorbdb2( m, p, q, x11, ldx11, x21, ldx21, theta, phi,taup1, taup2, tauq1, &
     !! SORBDB2 simultaneously bidiagonalizes the blocks of a tall and skinny
     !! matrix X with orthonomal columns:
     !! [ B11 ]
     !! [ X11 ]   [ P1 |    ] [  0  ]
     !! [-----] = [---------] [-----] Q1**T .
     !! [ X21 ]   [    | P2 ] [ B21 ]
     !! [  0  ]
     !! X11 is P-by-Q, and X21 is (M-P)-by-Q. P must be no larger than M-P,
     !! Q, or M-Q. Routines SORBDB1, SORBDB3, and SORBDB4 handle cases in
     !! which P is not the minimum dimension.
     !! The orthogonal matrices P1, P2, and Q1 are P-by-P, (M-P)-by-(M-P),
     !! and (M-Q)-by-(M-Q), respectively. They are represented implicitly by
     !! Householder vectors.
     !! B11 and B12 are P-by-P bidiagonal matrices represented implicitly by
     !! angles THETA, PHI.
               work, lwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lwork, m, p, q, ldx11, ldx21
           ! Array Arguments 
           real(sp), intent(out) :: phi(*), theta(*)
           real(sp), intent(out) :: taup1(*), taup2(*), tauq1(*), work(*)
           real(sp), intent(inout) :: x11(ldx11,*), x21(ldx21,*)
        ! ====================================================================
           
           ! Local Scalars 
           real(sp) :: c, s
           integer(ilp) :: childinfo, i, ilarf, iorbdb5, llarf, lorbdb5, lworkmin, &
                     lworkopt
           logical(lk) :: lquery
           ! Intrinsic Function 
           ! Executable Statements 
           ! test input arguments
           info = 0_ilp
           lquery = lwork == -1_ilp
           if( m < 0_ilp ) then
              info = -1_ilp
           else if( p < 0_ilp .or. p > m-p ) then
              info = -2_ilp
           else if( q < 0_ilp .or. q < p .or. m-q < p ) then
              info = -3_ilp
           else if( ldx11 < max( 1_ilp, p ) ) then
              info = -5_ilp
           else if( ldx21 < max( 1_ilp, m-p ) ) then
              info = -7_ilp
           end if
           ! compute workspace
           if( info == 0_ilp ) then
              ilarf = 2_ilp
              llarf = max( p-1, m-p, q-1 )
              iorbdb5 = 2_ilp
              lorbdb5 = q-1
              lworkopt = max( ilarf+llarf-1, iorbdb5+lorbdb5-1 )
              lworkmin = lworkopt
              work(1_ilp) = lworkopt
              if( lwork < lworkmin .and. .not.lquery ) then
                info = -14_ilp
              end if
           end if
           if( info /= 0_ilp ) then
              call stdlib_xerbla( 'SORBDB2', -info )
              return
           else if( lquery ) then
              return
           end if
           ! reduce rows 1, ..., p of x11 and x21
           do i = 1, p
              if( i > 1_ilp ) then
                 call stdlib_srot( q-i+1, x11(i,i), ldx11, x21(i-1,i), ldx21, c, s )
              end if
              call stdlib_slarfgp( q-i+1, x11(i,i), x11(i,i+1), ldx11, tauq1(i) )
              c = x11(i,i)
              x11(i,i) = one
              call stdlib_slarf( 'R', p-i, q-i+1, x11(i,i), ldx11, tauq1(i),x11(i+1,i), ldx11, &
                        work(ilarf) )
              call stdlib_slarf( 'R', m-p-i+1, q-i+1, x11(i,i), ldx11, tauq1(i),x21(i,i), ldx21, &
                        work(ilarf) )
              s = sqrt( stdlib_snrm2( p-i, x11(i+1,i), 1_ilp )**2_ilp+ stdlib_snrm2( m-p-i+1, x21(i,i), 1_ilp &
                        )**2_ilp )
              theta(i) = atan2( s, c )
              call stdlib_sorbdb5( p-i, m-p-i+1, q-i, x11(i+1,i), 1_ilp, x21(i,i), 1_ilp,x11(i+1,i+1), &
                        ldx11, x21(i,i+1), ldx21,work(iorbdb5), lorbdb5, childinfo )
              call stdlib_sscal( p-i, negone, x11(i+1,i), 1_ilp )
              call stdlib_slarfgp( m-p-i+1, x21(i,i), x21(i+1,i), 1_ilp, taup2(i) )
              if( i < p ) then
                 call stdlib_slarfgp( p-i, x11(i+1,i), x11(i+2,i), 1_ilp, taup1(i) )
                 phi(i) = atan2( x11(i+1,i), x21(i,i) )
                 c = cos( phi(i) )
                 s = sin( phi(i) )
                 x11(i+1,i) = one
                 call stdlib_slarf( 'L', p-i, q-i, x11(i+1,i), 1_ilp, taup1(i),x11(i+1,i+1), ldx11, &
                           work(ilarf) )
              end if
              x21(i,i) = one
              call stdlib_slarf( 'L', m-p-i+1, q-i, x21(i,i), 1_ilp, taup2(i),x21(i,i+1), ldx21, work(&
                        ilarf) )
           end do
           ! reduce the bottom-right portion of x21 to the identity matrix
           do i = p + 1, q
              call stdlib_slarfgp( m-p-i+1, x21(i,i), x21(i+1,i), 1_ilp, taup2(i) )
              x21(i,i) = one
              call stdlib_slarf( 'L', m-p-i+1, q-i, x21(i,i), 1_ilp, taup2(i),x21(i,i+1), ldx21, work(&
                        ilarf) )
           end do
           return
     end subroutine stdlib_sorbdb2

     module subroutine stdlib_dorbdb2( m, p, q, x11, ldx11, x21, ldx21, theta, phi,taup1, taup2, tauq1, &
     !! DORBDB2 simultaneously bidiagonalizes the blocks of a tall and skinny
     !! matrix X with orthonomal columns:
     !! [ B11 ]
     !! [ X11 ]   [ P1 |    ] [  0  ]
     !! [-----] = [---------] [-----] Q1**T .
     !! [ X21 ]   [    | P2 ] [ B21 ]
     !! [  0  ]
     !! X11 is P-by-Q, and X21 is (M-P)-by-Q. P must be no larger than M-P,
     !! Q, or M-Q. Routines DORBDB1, DORBDB3, and DORBDB4 handle cases in
     !! which P is not the minimum dimension.
     !! The orthogonal matrices P1, P2, and Q1 are P-by-P, (M-P)-by-(M-P),
     !! and (M-Q)-by-(M-Q), respectively. They are represented implicitly by
     !! Householder vectors.
     !! B11 and B12 are P-by-P bidiagonal matrices represented implicitly by
     !! angles THETA, PHI.
               work, lwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lwork, m, p, q, ldx11, ldx21
           ! Array Arguments 
           real(dp), intent(out) :: phi(*), theta(*)
           real(dp), intent(out) :: taup1(*), taup2(*), tauq1(*), work(*)
           real(dp), intent(inout) :: x11(ldx11,*), x21(ldx21,*)
        ! ====================================================================
           
           ! Local Scalars 
           real(dp) :: c, s
           integer(ilp) :: childinfo, i, ilarf, iorbdb5, llarf, lorbdb5, lworkmin, &
                     lworkopt
           logical(lk) :: lquery
           ! Intrinsic Function 
           ! Executable Statements 
           ! test input arguments
           info = 0_ilp
           lquery = lwork == -1_ilp
           if( m < 0_ilp ) then
              info = -1_ilp
           else if( p < 0_ilp .or. p > m-p ) then
              info = -2_ilp
           else if( q < 0_ilp .or. q < p .or. m-q < p ) then
              info = -3_ilp
           else if( ldx11 < max( 1_ilp, p ) ) then
              info = -5_ilp
           else if( ldx21 < max( 1_ilp, m-p ) ) then
              info = -7_ilp
           end if
           ! compute workspace
           if( info == 0_ilp ) then
              ilarf = 2_ilp
              llarf = max( p-1, m-p, q-1 )
              iorbdb5 = 2_ilp
              lorbdb5 = q-1
              lworkopt = max( ilarf+llarf-1, iorbdb5+lorbdb5-1 )
              lworkmin = lworkopt
              work(1_ilp) = lworkopt
              if( lwork < lworkmin .and. .not.lquery ) then
                info = -14_ilp
              end if
           end if
           if( info /= 0_ilp ) then
              call stdlib_xerbla( 'DORBDB2', -info )
              return
           else if( lquery ) then
              return
           end if
           ! reduce rows 1, ..., p of x11 and x21
           do i = 1, p
              if( i > 1_ilp ) then
                 call stdlib_drot( q-i+1, x11(i,i), ldx11, x21(i-1,i), ldx21, c, s )
              end if
              call stdlib_dlarfgp( q-i+1, x11(i,i), x11(i,i+1), ldx11, tauq1(i) )
              c = x11(i,i)
              x11(i,i) = one
              call stdlib_dlarf( 'R', p-i, q-i+1, x11(i,i), ldx11, tauq1(i),x11(i+1,i), ldx11, &
                        work(ilarf) )
              call stdlib_dlarf( 'R', m-p-i+1, q-i+1, x11(i,i), ldx11, tauq1(i),x21(i,i), ldx21, &
                        work(ilarf) )
              s = sqrt( stdlib_dnrm2( p-i, x11(i+1,i), 1_ilp )**2_ilp+ stdlib_dnrm2( m-p-i+1, x21(i,i), 1_ilp &
                        )**2_ilp )
              theta(i) = atan2( s, c )
              call stdlib_dorbdb5( p-i, m-p-i+1, q-i, x11(i+1,i), 1_ilp, x21(i,i), 1_ilp,x11(i+1,i+1), &
                        ldx11, x21(i,i+1), ldx21,work(iorbdb5), lorbdb5, childinfo )
              call stdlib_dscal( p-i, negone, x11(i+1,i), 1_ilp )
              call stdlib_dlarfgp( m-p-i+1, x21(i,i), x21(i+1,i), 1_ilp, taup2(i) )
              if( i < p ) then
                 call stdlib_dlarfgp( p-i, x11(i+1,i), x11(i+2,i), 1_ilp, taup1(i) )
                 phi(i) = atan2( x11(i+1,i), x21(i,i) )
                 c = cos( phi(i) )
                 s = sin( phi(i) )
                 x11(i+1,i) = one
                 call stdlib_dlarf( 'L', p-i, q-i, x11(i+1,i), 1_ilp, taup1(i),x11(i+1,i+1), ldx11, &
                           work(ilarf) )
              end if
              x21(i,i) = one
              call stdlib_dlarf( 'L', m-p-i+1, q-i, x21(i,i), 1_ilp, taup2(i),x21(i,i+1), ldx21, work(&
                        ilarf) )
           end do
           ! reduce the bottom-right portion of x21 to the identity matrix
           do i = p + 1, q
              call stdlib_dlarfgp( m-p-i+1, x21(i,i), x21(i+1,i), 1_ilp, taup2(i) )
              x21(i,i) = one
              call stdlib_dlarf( 'L', m-p-i+1, q-i, x21(i,i), 1_ilp, taup2(i),x21(i,i+1), ldx21, work(&
                        ilarf) )
           end do
           return
     end subroutine stdlib_dorbdb2




     module subroutine stdlib_sorbdb3( m, p, q, x11, ldx11, x21, ldx21, theta, phi,taup1, taup2, tauq1, &
     !! SORBDB3 simultaneously bidiagonalizes the blocks of a tall and skinny
     !! matrix X with orthonomal columns:
     !! [ B11 ]
     !! [ X11 ]   [ P1 |    ] [  0  ]
     !! [-----] = [---------] [-----] Q1**T .
     !! [ X21 ]   [    | P2 ] [ B21 ]
     !! [  0  ]
     !! X11 is P-by-Q, and X21 is (M-P)-by-Q. M-P must be no larger than P,
     !! Q, or M-Q. Routines SORBDB1, SORBDB2, and SORBDB4 handle cases in
     !! which M-P is not the minimum dimension.
     !! The orthogonal matrices P1, P2, and Q1 are P-by-P, (M-P)-by-(M-P),
     !! and (M-Q)-by-(M-Q), respectively. They are represented implicitly by
     !! Householder vectors.
     !! B11 and B12 are (M-P)-by-(M-P) bidiagonal matrices represented
     !! implicitly by angles THETA, PHI.
               work, lwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lwork, m, p, q, ldx11, ldx21
           ! Array Arguments 
           real(sp), intent(out) :: phi(*), theta(*)
           real(sp), intent(out) :: taup1(*), taup2(*), tauq1(*), work(*)
           real(sp), intent(inout) :: x11(ldx11,*), x21(ldx21,*)
        ! ====================================================================
           
           ! Local Scalars 
           real(sp) :: c, s
           integer(ilp) :: childinfo, i, ilarf, iorbdb5, llarf, lorbdb5, lworkmin, &
                     lworkopt
           logical(lk) :: lquery
           ! Intrinsic Function 
           ! Executable Statements 
           ! test input arguments
           info = 0_ilp
           lquery = lwork == -1_ilp
           if( m < 0_ilp ) then
              info = -1_ilp
           else if( 2_ilp*p < m .or. p > m ) then
              info = -2_ilp
           else if( q < m-p .or. m-q < m-p ) then
              info = -3_ilp
           else if( ldx11 < max( 1_ilp, p ) ) then
              info = -5_ilp
           else if( ldx21 < max( 1_ilp, m-p ) ) then
              info = -7_ilp
           end if
           ! compute workspace
           if( info == 0_ilp ) then
              ilarf = 2_ilp
              llarf = max( p, m-p-1, q-1 )
              iorbdb5 = 2_ilp
              lorbdb5 = q-1
              lworkopt = max( ilarf+llarf-1, iorbdb5+lorbdb5-1 )
              lworkmin = lworkopt
              work(1_ilp) = lworkopt
              if( lwork < lworkmin .and. .not.lquery ) then
                info = -14_ilp
              end if
           end if
           if( info /= 0_ilp ) then
              call stdlib_xerbla( 'SORBDB3', -info )
              return
           else if( lquery ) then
              return
           end if
           ! reduce rows 1, ..., m-p of x11 and x21
           do i = 1, m-p
              if( i > 1_ilp ) then
                 call stdlib_srot( q-i+1, x11(i-1,i), ldx11, x21(i,i), ldx11, c, s )
              end if
              call stdlib_slarfgp( q-i+1, x21(i,i), x21(i,i+1), ldx21, tauq1(i) )
              s = x21(i,i)
              x21(i,i) = one
              call stdlib_slarf( 'R', p-i+1, q-i+1, x21(i,i), ldx21, tauq1(i),x11(i,i), ldx11, &
                        work(ilarf) )
              call stdlib_slarf( 'R', m-p-i, q-i+1, x21(i,i), ldx21, tauq1(i),x21(i+1,i), ldx21, &
                        work(ilarf) )
              c = sqrt( stdlib_snrm2( p-i+1, x11(i,i), 1_ilp )**2_ilp+ stdlib_snrm2( m-p-i, x21(i+1,i), 1_ilp &
                        )**2_ilp )
              theta(i) = atan2( s, c )
              call stdlib_sorbdb5( p-i+1, m-p-i, q-i, x11(i,i), 1_ilp, x21(i+1,i), 1_ilp,x11(i,i+1), &
                        ldx11, x21(i+1,i+1), ldx21,work(iorbdb5), lorbdb5, childinfo )
              call stdlib_slarfgp( p-i+1, x11(i,i), x11(i+1,i), 1_ilp, taup1(i) )
              if( i < m-p ) then
                 call stdlib_slarfgp( m-p-i, x21(i+1,i), x21(i+2,i), 1_ilp, taup2(i) )
                 phi(i) = atan2( x21(i+1,i), x11(i,i) )
                 c = cos( phi(i) )
                 s = sin( phi(i) )
                 x21(i+1,i) = one
                 call stdlib_slarf( 'L', m-p-i, q-i, x21(i+1,i), 1_ilp, taup2(i),x21(i+1,i+1), ldx21, &
                           work(ilarf) )
              end if
              x11(i,i) = one
              call stdlib_slarf( 'L', p-i+1, q-i, x11(i,i), 1_ilp, taup1(i), x11(i,i+1),ldx11, work(&
                        ilarf) )
           end do
           ! reduce the bottom-right portion of x11 to the identity matrix
           do i = m-p + 1, q
              call stdlib_slarfgp( p-i+1, x11(i,i), x11(i+1,i), 1_ilp, taup1(i) )
              x11(i,i) = one
              call stdlib_slarf( 'L', p-i+1, q-i, x11(i,i), 1_ilp, taup1(i), x11(i,i+1),ldx11, work(&
                        ilarf) )
           end do
           return
     end subroutine stdlib_sorbdb3

     module subroutine stdlib_dorbdb3( m, p, q, x11, ldx11, x21, ldx21, theta, phi,taup1, taup2, tauq1, &
     !! DORBDB3 simultaneously bidiagonalizes the blocks of a tall and skinny
     !! matrix X with orthonomal columns:
     !! [ B11 ]
     !! [ X11 ]   [ P1 |    ] [  0  ]
     !! [-----] = [---------] [-----] Q1**T .
     !! [ X21 ]   [    | P2 ] [ B21 ]
     !! [  0  ]
     !! X11 is P-by-Q, and X21 is (M-P)-by-Q. M-P must be no larger than P,
     !! Q, or M-Q. Routines DORBDB1, DORBDB2, and DORBDB4 handle cases in
     !! which M-P is not the minimum dimension.
     !! The orthogonal matrices P1, P2, and Q1 are P-by-P, (M-P)-by-(M-P),
     !! and (M-Q)-by-(M-Q), respectively. They are represented implicitly by
     !! Householder vectors.
     !! B11 and B12 are (M-P)-by-(M-P) bidiagonal matrices represented
     !! implicitly by angles THETA, PHI.
               work, lwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lwork, m, p, q, ldx11, ldx21
           ! Array Arguments 
           real(dp), intent(out) :: phi(*), theta(*)
           real(dp), intent(out) :: taup1(*), taup2(*), tauq1(*), work(*)
           real(dp), intent(inout) :: x11(ldx11,*), x21(ldx21,*)
        ! ====================================================================
           
           ! Local Scalars 
           real(dp) :: c, s
           integer(ilp) :: childinfo, i, ilarf, iorbdb5, llarf, lorbdb5, lworkmin, &
                     lworkopt
           logical(lk) :: lquery
           ! Intrinsic Function 
           ! Executable Statements 
           ! test input arguments
           info = 0_ilp
           lquery = lwork == -1_ilp
           if( m < 0_ilp ) then
              info = -1_ilp
           else if( 2_ilp*p < m .or. p > m ) then
              info = -2_ilp
           else if( q < m-p .or. m-q < m-p ) then
              info = -3_ilp
           else if( ldx11 < max( 1_ilp, p ) ) then
              info = -5_ilp
           else if( ldx21 < max( 1_ilp, m-p ) ) then
              info = -7_ilp
           end if
           ! compute workspace
           if( info == 0_ilp ) then
              ilarf = 2_ilp
              llarf = max( p, m-p-1, q-1 )
              iorbdb5 = 2_ilp
              lorbdb5 = q-1
              lworkopt = max( ilarf+llarf-1, iorbdb5+lorbdb5-1 )
              lworkmin = lworkopt
              work(1_ilp) = lworkopt
              if( lwork < lworkmin .and. .not.lquery ) then
                info = -14_ilp
              end if
           end if
           if( info /= 0_ilp ) then
              call stdlib_xerbla( 'DORBDB3', -info )
              return
           else if( lquery ) then
              return
           end if
           ! reduce rows 1, ..., m-p of x11 and x21
           do i = 1, m-p
              if( i > 1_ilp ) then
                 call stdlib_drot( q-i+1, x11(i-1,i), ldx11, x21(i,i), ldx11, c, s )
              end if
              call stdlib_dlarfgp( q-i+1, x21(i,i), x21(i,i+1), ldx21, tauq1(i) )
              s = x21(i,i)
              x21(i,i) = one
              call stdlib_dlarf( 'R', p-i+1, q-i+1, x21(i,i), ldx21, tauq1(i),x11(i,i), ldx11, &
                        work(ilarf) )
              call stdlib_dlarf( 'R', m-p-i, q-i+1, x21(i,i), ldx21, tauq1(i),x21(i+1,i), ldx21, &
                        work(ilarf) )
              c = sqrt( stdlib_dnrm2( p-i+1, x11(i,i), 1_ilp )**2_ilp+ stdlib_dnrm2( m-p-i, x21(i+1,i), 1_ilp &
                        )**2_ilp )
              theta(i) = atan2( s, c )
              call stdlib_dorbdb5( p-i+1, m-p-i, q-i, x11(i,i), 1_ilp, x21(i+1,i), 1_ilp,x11(i,i+1), &
                        ldx11, x21(i+1,i+1), ldx21,work(iorbdb5), lorbdb5, childinfo )
              call stdlib_dlarfgp( p-i+1, x11(i,i), x11(i+1,i), 1_ilp, taup1(i) )
              if( i < m-p ) then
                 call stdlib_dlarfgp( m-p-i, x21(i+1,i), x21(i+2,i), 1_ilp, taup2(i) )
                 phi(i) = atan2( x21(i+1,i), x11(i,i) )
                 c = cos( phi(i) )
                 s = sin( phi(i) )
                 x21(i+1,i) = one
                 call stdlib_dlarf( 'L', m-p-i, q-i, x21(i+1,i), 1_ilp, taup2(i),x21(i+1,i+1), ldx21, &
                           work(ilarf) )
              end if
              x11(i,i) = one
              call stdlib_dlarf( 'L', p-i+1, q-i, x11(i,i), 1_ilp, taup1(i), x11(i,i+1),ldx11, work(&
                        ilarf) )
           end do
           ! reduce the bottom-right portion of x11 to the identity matrix
           do i = m-p + 1, q
              call stdlib_dlarfgp( p-i+1, x11(i,i), x11(i+1,i), 1_ilp, taup1(i) )
              x11(i,i) = one
              call stdlib_dlarf( 'L', p-i+1, q-i, x11(i,i), 1_ilp, taup1(i), x11(i,i+1),ldx11, work(&
                        ilarf) )
           end do
           return
     end subroutine stdlib_dorbdb3




     module subroutine stdlib_sorbdb4( m, p, q, x11, ldx11, x21, ldx21, theta, phi,taup1, taup2, tauq1, &
     !! SORBDB4 simultaneously bidiagonalizes the blocks of a tall and skinny
     !! matrix X with orthonomal columns:
     !! [ B11 ]
     !! [ X11 ]   [ P1 |    ] [  0  ]
     !! [-----] = [---------] [-----] Q1**T .
     !! [ X21 ]   [    | P2 ] [ B21 ]
     !! [  0  ]
     !! X11 is P-by-Q, and X21 is (M-P)-by-Q. M-Q must be no larger than P,
     !! M-P, or Q. Routines SORBDB1, SORBDB2, and SORBDB3 handle cases in
     !! which M-Q is not the minimum dimension.
     !! The orthogonal matrices P1, P2, and Q1 are P-by-P, (M-P)-by-(M-P),
     !! and (M-Q)-by-(M-Q), respectively. They are represented implicitly by
     !! Householder vectors.
     !! B11 and B12 are (M-Q)-by-(M-Q) bidiagonal matrices represented
     !! implicitly by angles THETA, PHI.
               phantom, work, lwork,info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lwork, m, p, q, ldx11, ldx21
           ! Array Arguments 
           real(sp), intent(out) :: phi(*), theta(*)
           real(sp), intent(out) :: phantom(*), taup1(*), taup2(*), tauq1(*), work(*)
           real(sp), intent(inout) :: x11(ldx11,*), x21(ldx21,*)
        ! ====================================================================
           
           ! Local Scalars 
           real(sp) :: c, s
           integer(ilp) :: childinfo, i, ilarf, iorbdb5, j, llarf, lorbdb5, lworkmin, &
                     lworkopt
           logical(lk) :: lquery
           ! Intrinsic Function 
           ! Executable Statements 
           ! test input arguments
           info = 0_ilp
           lquery = lwork == -1_ilp
           if( m < 0_ilp ) then
              info = -1_ilp
           else if( p < m-q .or. m-p < m-q ) then
              info = -2_ilp
           else if( q < m-q .or. q > m ) then
              info = -3_ilp
           else if( ldx11 < max( 1_ilp, p ) ) then
              info = -5_ilp
           else if( ldx21 < max( 1_ilp, m-p ) ) then
              info = -7_ilp
           end if
           ! compute workspace
           if( info == 0_ilp ) then
              ilarf = 2_ilp
              llarf = max( q-1, p-1, m-p-1 )
              iorbdb5 = 2_ilp
              lorbdb5 = q
              lworkopt = ilarf + llarf - 1_ilp
              lworkopt = max( lworkopt, iorbdb5 + lorbdb5 - 1_ilp )
              lworkmin = lworkopt
              work(1_ilp) = lworkopt
              if( lwork < lworkmin .and. .not.lquery ) then
                info = -14_ilp
              end if
           end if
           if( info /= 0_ilp ) then
              call stdlib_xerbla( 'SORBDB4', -info )
              return
           else if( lquery ) then
              return
           end if
           ! reduce columns 1, ..., m-q of x11 and x21
           do i = 1, m-q
              if( i == 1_ilp ) then
                 do j = 1, m
                    phantom(j) = zero
                 end do
                 call stdlib_sorbdb5( p, m-p, q, phantom(1_ilp), 1_ilp, phantom(p+1), 1_ilp,x11, ldx11, x21, &
                           ldx21, work(iorbdb5),lorbdb5, childinfo )
                 call stdlib_sscal( p, negone, phantom(1_ilp), 1_ilp )
                 call stdlib_slarfgp( p, phantom(1_ilp), phantom(2_ilp), 1_ilp, taup1(1_ilp) )
                 call stdlib_slarfgp( m-p, phantom(p+1), phantom(p+2), 1_ilp, taup2(1_ilp) )
                 theta(i) = atan2( phantom(1_ilp), phantom(p+1) )
                 c = cos( theta(i) )
                 s = sin( theta(i) )
                 phantom(1_ilp) = one
                 phantom(p+1) = one
                 call stdlib_slarf( 'L', p, q, phantom(1_ilp), 1_ilp, taup1(1_ilp), x11, ldx11,work(ilarf) )
                           
                 call stdlib_slarf( 'L', m-p, q, phantom(p+1), 1_ilp, taup2(1_ilp), x21,ldx21, work(ilarf)&
                            )
              else
                 call stdlib_sorbdb5( p-i+1, m-p-i+1, q-i+1, x11(i,i-1), 1_ilp,x21(i,i-1), 1_ilp, x11(i,i)&
                           , ldx11, x21(i,i),ldx21, work(iorbdb5), lorbdb5, childinfo )
                 call stdlib_sscal( p-i+1, negone, x11(i,i-1), 1_ilp )
                 call stdlib_slarfgp( p-i+1, x11(i,i-1), x11(i+1,i-1), 1_ilp, taup1(i) )
                 call stdlib_slarfgp( m-p-i+1, x21(i,i-1), x21(i+1,i-1), 1_ilp,taup2(i) )
                 theta(i) = atan2( x11(i,i-1), x21(i,i-1) )
                 c = cos( theta(i) )
                 s = sin( theta(i) )
                 x11(i,i-1) = one
                 x21(i,i-1) = one
                 call stdlib_slarf( 'L', p-i+1, q-i+1, x11(i,i-1), 1_ilp, taup1(i),x11(i,i), ldx11, &
                           work(ilarf) )
                 call stdlib_slarf( 'L', m-p-i+1, q-i+1, x21(i,i-1), 1_ilp, taup2(i),x21(i,i), ldx21, &
                           work(ilarf) )
              end if
              call stdlib_srot( q-i+1, x11(i,i), ldx11, x21(i,i), ldx21, s, -c )
              call stdlib_slarfgp( q-i+1, x21(i,i), x21(i,i+1), ldx21, tauq1(i) )
              c = x21(i,i)
              x21(i,i) = one
              call stdlib_slarf( 'R', p-i, q-i+1, x21(i,i), ldx21, tauq1(i),x11(i+1,i), ldx11, &
                        work(ilarf) )
              call stdlib_slarf( 'R', m-p-i, q-i+1, x21(i,i), ldx21, tauq1(i),x21(i+1,i), ldx21, &
                        work(ilarf) )
              if( i < m-q ) then
                 s = sqrt( stdlib_snrm2( p-i, x11(i+1,i), 1_ilp )**2_ilp+ stdlib_snrm2( m-p-i, x21(i+1,i),&
                            1_ilp )**2_ilp )
                 phi(i) = atan2( s, c )
              end if
           end do
           ! reduce the bottom-right portion of x11 to [ i 0 ]
           do i = m - q + 1, p
              call stdlib_slarfgp( q-i+1, x11(i,i), x11(i,i+1), ldx11, tauq1(i) )
              x11(i,i) = one
              call stdlib_slarf( 'R', p-i, q-i+1, x11(i,i), ldx11, tauq1(i),x11(i+1,i), ldx11, &
                        work(ilarf) )
              call stdlib_slarf( 'R', q-p, q-i+1, x11(i,i), ldx11, tauq1(i),x21(m-q+1,i), ldx21, &
                        work(ilarf) )
           end do
           ! reduce the bottom-right portion of x21 to [ 0 i ]
           do i = p + 1, q
              call stdlib_slarfgp( q-i+1, x21(m-q+i-p,i), x21(m-q+i-p,i+1), ldx21,tauq1(i) )
                        
              x21(m-q+i-p,i) = one
              call stdlib_slarf( 'R', q-i, q-i+1, x21(m-q+i-p,i), ldx21, tauq1(i),x21(m-q+i-p+1,i)&
                        , ldx21, work(ilarf) )
           end do
           return
     end subroutine stdlib_sorbdb4

     module subroutine stdlib_dorbdb4( m, p, q, x11, ldx11, x21, ldx21, theta, phi,taup1, taup2, tauq1, &
     !! DORBDB4 simultaneously bidiagonalizes the blocks of a tall and skinny
     !! matrix X with orthonomal columns:
     !! [ B11 ]
     !! [ X11 ]   [ P1 |    ] [  0  ]
     !! [-----] = [---------] [-----] Q1**T .
     !! [ X21 ]   [    | P2 ] [ B21 ]
     !! [  0  ]
     !! X11 is P-by-Q, and X21 is (M-P)-by-Q. M-Q must be no larger than P,
     !! M-P, or Q. Routines DORBDB1, DORBDB2, and DORBDB3 handle cases in
     !! which M-Q is not the minimum dimension.
     !! The orthogonal matrices P1, P2, and Q1 are P-by-P, (M-P)-by-(M-P),
     !! and (M-Q)-by-(M-Q), respectively. They are represented implicitly by
     !! Householder vectors.
     !! B11 and B12 are (M-Q)-by-(M-Q) bidiagonal matrices represented
     !! implicitly by angles THETA, PHI.
               phantom, work, lwork,info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lwork, m, p, q, ldx11, ldx21
           ! Array Arguments 
           real(dp), intent(out) :: phi(*), theta(*)
           real(dp), intent(out) :: phantom(*), taup1(*), taup2(*), tauq1(*), work(*)
           real(dp), intent(inout) :: x11(ldx11,*), x21(ldx21,*)
        ! ====================================================================
           
           ! Local Scalars 
           real(dp) :: c, s
           integer(ilp) :: childinfo, i, ilarf, iorbdb5, j, llarf, lorbdb5, lworkmin, &
                     lworkopt
           logical(lk) :: lquery
           ! Intrinsic Function 
           ! Executable Statements 
           ! test input arguments
           info = 0_ilp
           lquery = lwork == -1_ilp
           if( m < 0_ilp ) then
              info = -1_ilp
           else if( p < m-q .or. m-p < m-q ) then
              info = -2_ilp
           else if( q < m-q .or. q > m ) then
              info = -3_ilp
           else if( ldx11 < max( 1_ilp, p ) ) then
              info = -5_ilp
           else if( ldx21 < max( 1_ilp, m-p ) ) then
              info = -7_ilp
           end if
           ! compute workspace
           if( info == 0_ilp ) then
              ilarf = 2_ilp
              llarf = max( q-1, p-1, m-p-1 )
              iorbdb5 = 2_ilp
              lorbdb5 = q
              lworkopt = ilarf + llarf - 1_ilp
              lworkopt = max( lworkopt, iorbdb5 + lorbdb5 - 1_ilp )
              lworkmin = lworkopt
              work(1_ilp) = lworkopt
              if( lwork < lworkmin .and. .not.lquery ) then
                info = -14_ilp
              end if
           end if
           if( info /= 0_ilp ) then
              call stdlib_xerbla( 'DORBDB4', -info )
              return
           else if( lquery ) then
              return
           end if
           ! reduce columns 1, ..., m-q of x11 and x21
           do i = 1, m-q
              if( i == 1_ilp ) then
                 do j = 1, m
                    phantom(j) = zero
                 end do
                 call stdlib_dorbdb5( p, m-p, q, phantom(1_ilp), 1_ilp, phantom(p+1), 1_ilp,x11, ldx11, x21, &
                           ldx21, work(iorbdb5),lorbdb5, childinfo )
                 call stdlib_dscal( p, negone, phantom(1_ilp), 1_ilp )
                 call stdlib_dlarfgp( p, phantom(1_ilp), phantom(2_ilp), 1_ilp, taup1(1_ilp) )
                 call stdlib_dlarfgp( m-p, phantom(p+1), phantom(p+2), 1_ilp, taup2(1_ilp) )
                 theta(i) = atan2( phantom(1_ilp), phantom(p+1) )
                 c = cos( theta(i) )
                 s = sin( theta(i) )
                 phantom(1_ilp) = one
                 phantom(p+1) = one
                 call stdlib_dlarf( 'L', p, q, phantom(1_ilp), 1_ilp, taup1(1_ilp), x11, ldx11,work(ilarf) )
                           
                 call stdlib_dlarf( 'L', m-p, q, phantom(p+1), 1_ilp, taup2(1_ilp), x21,ldx21, work(ilarf)&
                            )
              else
                 call stdlib_dorbdb5( p-i+1, m-p-i+1, q-i+1, x11(i,i-1), 1_ilp,x21(i,i-1), 1_ilp, x11(i,i)&
                           , ldx11, x21(i,i),ldx21, work(iorbdb5), lorbdb5, childinfo )
                 call stdlib_dscal( p-i+1, negone, x11(i,i-1), 1_ilp )
                 call stdlib_dlarfgp( p-i+1, x11(i,i-1), x11(i+1,i-1), 1_ilp, taup1(i) )
                 call stdlib_dlarfgp( m-p-i+1, x21(i,i-1), x21(i+1,i-1), 1_ilp,taup2(i) )
                 theta(i) = atan2( x11(i,i-1), x21(i,i-1) )
                 c = cos( theta(i) )
                 s = sin( theta(i) )
                 x11(i,i-1) = one
                 x21(i,i-1) = one
                 call stdlib_dlarf( 'L', p-i+1, q-i+1, x11(i,i-1), 1_ilp, taup1(i),x11(i,i), ldx11, &
                           work(ilarf) )
                 call stdlib_dlarf( 'L', m-p-i+1, q-i+1, x21(i,i-1), 1_ilp, taup2(i),x21(i,i), ldx21, &
                           work(ilarf) )
              end if
              call stdlib_drot( q-i+1, x11(i,i), ldx11, x21(i,i), ldx21, s, -c )
              call stdlib_dlarfgp( q-i+1, x21(i,i), x21(i,i+1), ldx21, tauq1(i) )
              c = x21(i,i)
              x21(i,i) = one
              call stdlib_dlarf( 'R', p-i, q-i+1, x21(i,i), ldx21, tauq1(i),x11(i+1,i), ldx11, &
                        work(ilarf) )
              call stdlib_dlarf( 'R', m-p-i, q-i+1, x21(i,i), ldx21, tauq1(i),x21(i+1,i), ldx21, &
                        work(ilarf) )
              if( i < m-q ) then
                 s = sqrt( stdlib_dnrm2( p-i, x11(i+1,i), 1_ilp )**2_ilp+ stdlib_dnrm2( m-p-i, x21(i+1,i),&
                            1_ilp )**2_ilp )
                 phi(i) = atan2( s, c )
              end if
           end do
           ! reduce the bottom-right portion of x11 to [ i 0 ]
           do i = m - q + 1, p
              call stdlib_dlarfgp( q-i+1, x11(i,i), x11(i,i+1), ldx11, tauq1(i) )
              x11(i,i) = one
              call stdlib_dlarf( 'R', p-i, q-i+1, x11(i,i), ldx11, tauq1(i),x11(i+1,i), ldx11, &
                        work(ilarf) )
              call stdlib_dlarf( 'R', q-p, q-i+1, x11(i,i), ldx11, tauq1(i),x21(m-q+1,i), ldx21, &
                        work(ilarf) )
           end do
           ! reduce the bottom-right portion of x21 to [ 0 i ]
           do i = p + 1, q
              call stdlib_dlarfgp( q-i+1, x21(m-q+i-p,i), x21(m-q+i-p,i+1), ldx21,tauq1(i) )
                        
              x21(m-q+i-p,i) = one
              call stdlib_dlarf( 'R', q-i, q-i+1, x21(m-q+i-p,i), ldx21, tauq1(i),x21(m-q+i-p+1,i)&
                        , ldx21, work(ilarf) )
           end do
           return
     end subroutine stdlib_dorbdb4




     pure module subroutine stdlib_sorbdb5( m1, m2, n, x1, incx1, x2, incx2, q1, ldq1, q2,ldq2, work, &
     !! SORBDB5 orthogonalizes the column vector
     !! X = [ X1 ]
     !! [ X2 ]
     !! with respect to the columns of
     !! Q = [ Q1 ] .
     !! [ Q2 ]
     !! The columns of Q must be orthonormal.
     !! If the projection is zero according to Kahan's "twice is enough"
     !! criterion, then some other vector from the orthogonal complement
     !! is returned. This vector is chosen in an arbitrary but deterministic
     !! way.
               lwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx1, incx2, ldq1, ldq2, lwork, m1, m2, n
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           real(sp), intent(in) :: q1(ldq1,*), q2(ldq2,*)
           real(sp), intent(out) :: work(*)
           real(sp), intent(inout) :: x1(*), x2(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: childinfo, i, j
           ! Intrinsic Function 
           ! Executable Statements 
           ! test input arguments
           info = 0_ilp
           if( m1 < 0_ilp ) then
              info = -1_ilp
           else if( m2 < 0_ilp ) then
              info = -2_ilp
           else if( n < 0_ilp ) then
              info = -3_ilp
           else if( incx1 < 1_ilp ) then
              info = -5_ilp
           else if( incx2 < 1_ilp ) then
              info = -7_ilp
           else if( ldq1 < max( 1_ilp, m1 ) ) then
              info = -9_ilp
           else if( ldq2 < max( 1_ilp, m2 ) ) then
              info = -11_ilp
           else if( lwork < n ) then
              info = -13_ilp
           end if
           if( info /= 0_ilp ) then
              call stdlib_xerbla( 'SORBDB5', -info )
              return
           end if
           ! project x onto the orthogonal complement of q
           call stdlib_sorbdb6( m1, m2, n, x1, incx1, x2, incx2, q1, ldq1, q2, ldq2,work, lwork, &
                     childinfo )
           ! if the projection is nonzero, then return
           if( stdlib_snrm2(m1,x1,incx1) /= zero.or. stdlib_snrm2(m2,x2,incx2) /= zero ) &
                     then
              return
           end if
           ! project each standard basis vector e_1,...,e_m1 in turn, stopping
           ! when a nonzero projection is found
           do i = 1, m1
              do j = 1, m1
                 x1(j) = zero
              end do
              x1(i) = one
              do j = 1, m2
                 x2(j) = zero
              end do
              call stdlib_sorbdb6( m1, m2, n, x1, incx1, x2, incx2, q1, ldq1, q2,ldq2, work, &
                        lwork, childinfo )
              if( stdlib_snrm2(m1,x1,incx1) /= zero.or. stdlib_snrm2(m2,x2,incx2) /= zero ) &
                        then
                 return
              end if
           end do
           ! project each standard basis vector e_(m1+1),...,e_(m1+m2) in turn,
           ! stopping when a nonzero projection is found
           do i = 1, m2
              do j = 1, m1
                 x1(j) = zero
              end do
              do j = 1, m2
                 x2(j) = zero
              end do
              x2(i) = one
              call stdlib_sorbdb6( m1, m2, n, x1, incx1, x2, incx2, q1, ldq1, q2,ldq2, work, &
                        lwork, childinfo )
              if( stdlib_snrm2(m1,x1,incx1) /= zero.or. stdlib_snrm2(m2,x2,incx2) /= zero ) &
                        then
                 return
              end if
           end do
           return
     end subroutine stdlib_sorbdb5

     pure module subroutine stdlib_dorbdb5( m1, m2, n, x1, incx1, x2, incx2, q1, ldq1, q2,ldq2, work, &
     !! DORBDB5 orthogonalizes the column vector
     !! X = [ X1 ]
     !! [ X2 ]
     !! with respect to the columns of
     !! Q = [ Q1 ] .
     !! [ Q2 ]
     !! The columns of Q must be orthonormal.
     !! If the projection is zero according to Kahan's "twice is enough"
     !! criterion, then some other vector from the orthogonal complement
     !! is returned. This vector is chosen in an arbitrary but deterministic
     !! way.
               lwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx1, incx2, ldq1, ldq2, lwork, m1, m2, n
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           real(dp), intent(in) :: q1(ldq1,*), q2(ldq2,*)
           real(dp), intent(out) :: work(*)
           real(dp), intent(inout) :: x1(*), x2(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: childinfo, i, j
           ! Intrinsic Function 
           ! Executable Statements 
           ! test input arguments
           info = 0_ilp
           if( m1 < 0_ilp ) then
              info = -1_ilp
           else if( m2 < 0_ilp ) then
              info = -2_ilp
           else if( n < 0_ilp ) then
              info = -3_ilp
           else if( incx1 < 1_ilp ) then
              info = -5_ilp
           else if( incx2 < 1_ilp ) then
              info = -7_ilp
           else if( ldq1 < max( 1_ilp, m1 ) ) then
              info = -9_ilp
           else if( ldq2 < max( 1_ilp, m2 ) ) then
              info = -11_ilp
           else if( lwork < n ) then
              info = -13_ilp
           end if
           if( info /= 0_ilp ) then
              call stdlib_xerbla( 'DORBDB5', -info )
              return
           end if
           ! project x onto the orthogonal complement of q
           call stdlib_dorbdb6( m1, m2, n, x1, incx1, x2, incx2, q1, ldq1, q2, ldq2,work, lwork, &
                     childinfo )
           ! if the projection is nonzero, then return
           if( stdlib_dnrm2(m1,x1,incx1) /= zero.or. stdlib_dnrm2(m2,x2,incx2) /= zero ) &
                     then
              return
           end if
           ! project each standard basis vector e_1,...,e_m1 in turn, stopping
           ! when a nonzero projection is found
           do i = 1, m1
              do j = 1, m1
                 x1(j) = zero
              end do
              x1(i) = one
              do j = 1, m2
                 x2(j) = zero
              end do
              call stdlib_dorbdb6( m1, m2, n, x1, incx1, x2, incx2, q1, ldq1, q2,ldq2, work, &
                        lwork, childinfo )
              if( stdlib_dnrm2(m1,x1,incx1) /= zero.or. stdlib_dnrm2(m2,x2,incx2) /= zero ) &
                        then
                 return
              end if
           end do
           ! project each standard basis vector e_(m1+1),...,e_(m1+m2) in turn,
           ! stopping when a nonzero projection is found
           do i = 1, m2
              do j = 1, m1
                 x1(j) = zero
              end do
              do j = 1, m2
                 x2(j) = zero
              end do
              x2(i) = one
              call stdlib_dorbdb6( m1, m2, n, x1, incx1, x2, incx2, q1, ldq1, q2,ldq2, work, &
                        lwork, childinfo )
              if( stdlib_dnrm2(m1,x1,incx1) /= zero.or. stdlib_dnrm2(m2,x2,incx2) /= zero ) &
                        then
                 return
              end if
           end do
           return
     end subroutine stdlib_dorbdb5




     pure module subroutine stdlib_sorbdb6( m1, m2, n, x1, incx1, x2, incx2, q1, ldq1, q2,ldq2, work, &
     !! SORBDB6 orthogonalizes the column vector
     !! X = [ X1 ]
     !! [ X2 ]
     !! with respect to the columns of
     !! Q = [ Q1 ] .
     !! [ Q2 ]
     !! The columns of Q must be orthonormal.
     !! If the projection is zero according to Kahan's "twice is enough"
     !! criterion, then the zero vector is returned.
               lwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx1, incx2, ldq1, ldq2, lwork, m1, m2, n
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           real(sp), intent(in) :: q1(ldq1,*), q2(ldq2,*)
           real(sp), intent(out) :: work(*)
           real(sp), intent(inout) :: x1(*), x2(*)
        ! =====================================================================
           ! Parameters 
           real(sp), parameter :: alphasq = 0.01_sp
           
           
           
           ! Local Scalars 
           integer(ilp) :: i
           real(sp) :: normsq1, normsq2, scl1, scl2, ssq1, ssq2
           ! Intrinsic Function 
           ! Executable Statements 
           ! test input arguments
           info = 0_ilp
           if( m1 < 0_ilp ) then
              info = -1_ilp
           else if( m2 < 0_ilp ) then
              info = -2_ilp
           else if( n < 0_ilp ) then
              info = -3_ilp
           else if( incx1 < 1_ilp ) then
              info = -5_ilp
           else if( incx2 < 1_ilp ) then
              info = -7_ilp
           else if( ldq1 < max( 1_ilp, m1 ) ) then
              info = -9_ilp
           else if( ldq2 < max( 1_ilp, m2 ) ) then
              info = -11_ilp
           else if( lwork < n ) then
              info = -13_ilp
           end if
           if( info /= 0_ilp ) then
              call stdlib_xerbla( 'SORBDB6', -info )
              return
           end if
           ! first, project x onto the orthogonal complement of q's column
           ! space
           scl1 = zero
           ssq1 = one
           call stdlib_slassq( m1, x1, incx1, scl1, ssq1 )
           scl2 = zero
           ssq2 = one
           call stdlib_slassq( m2, x2, incx2, scl2, ssq2 )
           normsq1 = scl1**2_ilp*ssq1 + scl2**2_ilp*ssq2
           if( m1 == 0_ilp ) then
              do i = 1, n
                 work(i) = zero
              end do
           else
              call stdlib_sgemv( 'C', m1, n, one, q1, ldq1, x1, incx1, zero, work,1_ilp )
           end if
           call stdlib_sgemv( 'C', m2, n, one, q2, ldq2, x2, incx2, one, work, 1_ilp )
           call stdlib_sgemv( 'N', m1, n, negone, q1, ldq1, work, 1_ilp, one, x1,incx1 )
           call stdlib_sgemv( 'N', m2, n, negone, q2, ldq2, work, 1_ilp, one, x2,incx2 )
           scl1 = zero
           ssq1 = one
           call stdlib_slassq( m1, x1, incx1, scl1, ssq1 )
           scl2 = zero
           ssq2 = one
           call stdlib_slassq( m2, x2, incx2, scl2, ssq2 )
           normsq2 = scl1**2_ilp*ssq1 + scl2**2_ilp*ssq2
           ! if projection is sufficiently large in norm, then stop.
           ! if projection is zero, then stop.
           ! otherwise, project again.
           if( normsq2 >= alphasq*normsq1 ) then
              return
           end if
           if( normsq2 == zero ) then
              return
           end if
           normsq1 = normsq2
           do i = 1, n
              work(i) = zero
           end do
           if( m1 == 0_ilp ) then
              do i = 1, n
                 work(i) = zero
              end do
           else
              call stdlib_sgemv( 'C', m1, n, one, q1, ldq1, x1, incx1, zero, work,1_ilp )
           end if
           call stdlib_sgemv( 'C', m2, n, one, q2, ldq2, x2, incx2, one, work, 1_ilp )
           call stdlib_sgemv( 'N', m1, n, negone, q1, ldq1, work, 1_ilp, one, x1,incx1 )
           call stdlib_sgemv( 'N', m2, n, negone, q2, ldq2, work, 1_ilp, one, x2,incx2 )
           scl1 = zero
           ssq1 = one
           call stdlib_slassq( m1, x1, incx1, scl1, ssq1 )
           scl2 = zero
           ssq2 = one
           call stdlib_slassq( m1, x1, incx1, scl1, ssq1 )
           normsq2 = scl1**2_ilp*ssq1 + scl2**2_ilp*ssq2
           ! if second projection is sufficiently large in norm, then do
           ! nothing more. alternatively, if it shrunk significantly, then
           ! truncate it to zero.
           if( normsq2 < alphasq*normsq1 ) then
              do i = 1, m1
                 x1(i) = zero
              end do
              do i = 1, m2
                 x2(i) = zero
              end do
           end if
           return
     end subroutine stdlib_sorbdb6

     pure module subroutine stdlib_dorbdb6( m1, m2, n, x1, incx1, x2, incx2, q1, ldq1, q2,ldq2, work, &
     !! DORBDB6 orthogonalizes the column vector
     !! X = [ X1 ]
     !! [ X2 ]
     !! with respect to the columns of
     !! Q = [ Q1 ] .
     !! [ Q2 ]
     !! The columns of Q must be orthonormal.
     !! If the projection is zero according to Kahan's "twice is enough"
     !! criterion, then the zero vector is returned.
               lwork, info )
        ! -- lapack computational routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: incx1, incx2, ldq1, ldq2, lwork, m1, m2, n
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           real(dp), intent(in) :: q1(ldq1,*), q2(ldq2,*)
           real(dp), intent(out) :: work(*)
           real(dp), intent(inout) :: x1(*), x2(*)
        ! =====================================================================
           ! Parameters 
           real(dp), parameter :: alphasq = 0.01_dp
           
           
           
           ! Local Scalars 
           integer(ilp) :: i
           real(dp) :: normsq1, normsq2, scl1, scl2, ssq1, ssq2
           ! Intrinsic Function 
           ! Executable Statements 
           ! test input arguments
           info = 0_ilp
           if( m1 < 0_ilp ) then
              info = -1_ilp
           else if( m2 < 0_ilp ) then
              info = -2_ilp
           else if( n < 0_ilp ) then
              info = -3_ilp
           else if( incx1 < 1_ilp ) then
              info = -5_ilp
           else if( incx2 < 1_ilp ) then
              info = -7_ilp
           else if( ldq1 < max( 1_ilp, m1 ) ) then
              info = -9_ilp
           else if( ldq2 < max( 1_ilp, m2 ) ) then
              info = -11_ilp
           else if( lwork < n ) then
              info = -13_ilp
           end if
           if( info /= 0_ilp ) then
              call stdlib_xerbla( 'DORBDB6', -info )
              return
           end if
           ! first, project x onto the orthogonal complement of q's column
           ! space
           scl1 = zero
           ssq1 = one
           call stdlib_dlassq( m1, x1, incx1, scl1, ssq1 )
           scl2 = zero
           ssq2 = one
           call stdlib_dlassq( m2, x2, incx2, scl2, ssq2 )
           normsq1 = scl1**2_ilp*ssq1 + scl2**2_ilp*ssq2
           if( m1 == 0_ilp ) then
              do i = 1, n
                 work(i) = zero
              end do
           else
              call stdlib_dgemv( 'C', m1, n, one, q1, ldq1, x1, incx1, zero, work,1_ilp )
           end if
           call stdlib_dgemv( 'C', m2, n, one, q2, ldq2, x2, incx2, one, work, 1_ilp )
           call stdlib_dgemv( 'N', m1, n, negone, q1, ldq1, work, 1_ilp, one, x1,incx1 )
           call stdlib_dgemv( 'N', m2, n, negone, q2, ldq2, work, 1_ilp, one, x2,incx2 )
           scl1 = zero
           ssq1 = one
           call stdlib_dlassq( m1, x1, incx1, scl1, ssq1 )
           scl2 = zero
           ssq2 = one
           call stdlib_dlassq( m2, x2, incx2, scl2, ssq2 )
           normsq2 = scl1**2_ilp*ssq1 + scl2**2_ilp*ssq2
           ! if projection is sufficiently large in norm, then stop.
           ! if projection is zero, then stop.
           ! otherwise, project again.
           if( normsq2 >= alphasq*normsq1 ) then
              return
           end if
           if( normsq2 == zero ) then
              return
           end if
           normsq1 = normsq2
           do i = 1, n
              work(i) = zero
           end do
           if( m1 == 0_ilp ) then
              do i = 1, n
                 work(i) = zero
              end do
           else
              call stdlib_dgemv( 'C', m1, n, one, q1, ldq1, x1, incx1, zero, work,1_ilp )
           end if
           call stdlib_dgemv( 'C', m2, n, one, q2, ldq2, x2, incx2, one, work, 1_ilp )
           call stdlib_dgemv( 'N', m1, n, negone, q1, ldq1, work, 1_ilp, one, x1,incx1 )
           call stdlib_dgemv( 'N', m2, n, negone, q2, ldq2, work, 1_ilp, one, x2,incx2 )
           scl1 = zero
           ssq1 = one
           call stdlib_dlassq( m1, x1, incx1, scl1, ssq1 )
           scl2 = zero
           ssq2 = one
           call stdlib_dlassq( m1, x1, incx1, scl1, ssq1 )
           normsq2 = scl1**2_ilp*ssq1 + scl2**2_ilp*ssq2
           ! if second projection is sufficiently large in norm, then do
           ! nothing more. alternatively, if it shrunk significantly, then
           ! truncate it to zero.
           if( normsq2 < alphasq*normsq1 ) then
              do i = 1, m1
                 x1(i) = zero
              end do
              do i = 1, m2
                 x2(i) = zero
              end do
           end if
           return
     end subroutine stdlib_dorbdb6




     pure module subroutine stdlib_slapmr( forwrd, m, n, x, ldx, k )
     !! SLAPMR rearranges the rows of the M by N matrix X as specified
     !! by the permutation K(1),K(2),...,K(M) of the integers 1,...,M.
     !! If FORWRD = .TRUE.,  forward permutation:
     !! X(K(I),*) is moved X(I,*) for I = 1,2,...,M.
     !! If FORWRD = .FALSE., backward permutation:
     !! X(I,*) is moved to X(K(I),*) for I = 1,2,...,M.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           logical(lk), intent(in) :: forwrd
           integer(ilp), intent(in) :: ldx, m, n
           ! Array Arguments 
           integer(ilp), intent(inout) :: k(*)
           real(sp), intent(inout) :: x(ldx,*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, in, j, jj
           real(sp) :: temp
           ! Executable Statements 
           if( m<=1 )return
           do i = 1, m
              k( i ) = -k( i )
           end do
           if( forwrd ) then
              ! forward permutation
              do i = 1, m
                 if( k( i )>0 )go to 40
                 j = i
                 k( j ) = -k( j )
                 in = k( j )
                 20 continue
                 if( k( in )>0 )go to 40
                 do jj = 1, n
                    temp = x( j, jj )
                    x( j, jj ) = x( in, jj )
                    x( in, jj ) = temp
                 end do
                 k( in ) = -k( in )
                 j = in
                 in = k( in )
                 go to 20
                 40 continue
              end do
           else
              ! backward permutation
              do i = 1, m
                 if( k( i )>0 )go to 80
                 k( i ) = -k( i )
                 j = k( i )
                 60 continue
                 if( j==i )go to 80
                 do jj = 1, n
                    temp = x( i, jj )
                    x( i, jj ) = x( j, jj )
                    x( j, jj ) = temp
                 end do
                 k( j ) = -k( j )
                 j = k( j )
                 go to 60
                 80 continue
              end do
           end if
           return
     end subroutine stdlib_slapmr

     pure module subroutine stdlib_dlapmr( forwrd, m, n, x, ldx, k )
     !! DLAPMR rearranges the rows of the M by N matrix X as specified
     !! by the permutation K(1),K(2),...,K(M) of the integers 1,...,M.
     !! If FORWRD = .TRUE.,  forward permutation:
     !! X(K(I),*) is moved X(I,*) for I = 1,2,...,M.
     !! If FORWRD = .FALSE., backward permutation:
     !! X(I,*) is moved to X(K(I),*) for I = 1,2,...,M.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           logical(lk), intent(in) :: forwrd
           integer(ilp), intent(in) :: ldx, m, n
           ! Array Arguments 
           integer(ilp), intent(inout) :: k(*)
           real(dp), intent(inout) :: x(ldx,*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, in, j, jj
           real(dp) :: temp
           ! Executable Statements 
           if( m<=1 )return
           do i = 1, m
              k( i ) = -k( i )
           end do
           if( forwrd ) then
              ! forward permutation
              do i = 1, m
                 if( k( i )>0 )go to 40
                 j = i
                 k( j ) = -k( j )
                 in = k( j )
                 20 continue
                 if( k( in )>0 )go to 40
                 do jj = 1, n
                    temp = x( j, jj )
                    x( j, jj ) = x( in, jj )
                    x( in, jj ) = temp
                 end do
                 k( in ) = -k( in )
                 j = in
                 in = k( in )
                 go to 20
                 40 continue
              end do
           else
              ! backward permutation
              do i = 1, m
                 if( k( i )>0 )go to 80
                 k( i ) = -k( i )
                 j = k( i )
                 60 continue
                 if( j==i )go to 80
                 do jj = 1, n
                    temp = x( i, jj )
                    x( i, jj ) = x( j, jj )
                    x( j, jj ) = temp
                 end do
                 k( j ) = -k( j )
                 j = k( j )
                 go to 60
                 80 continue
              end do
           end if
           return
     end subroutine stdlib_dlapmr


     pure module subroutine stdlib_clapmr( forwrd, m, n, x, ldx, k )
     !! CLAPMR rearranges the rows of the M by N matrix X as specified
     !! by the permutation K(1),K(2),...,K(M) of the integers 1,...,M.
     !! If FORWRD = .TRUE.,  forward permutation:
     !! X(K(I),*) is moved X(I,*) for I = 1,2,...,M.
     !! If FORWRD = .FALSE., backward permutation:
     !! X(I,*) is moved to X(K(I),*) for I = 1,2,...,M.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           logical(lk), intent(in) :: forwrd
           integer(ilp), intent(in) :: ldx, m, n
           ! Array Arguments 
           integer(ilp), intent(inout) :: k(*)
           complex(sp), intent(inout) :: x(ldx,*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, in, j, jj
           complex(sp) :: temp
           ! Executable Statements 
           if( m<=1 )return
           do i = 1, m
              k( i ) = -k( i )
           end do
           if( forwrd ) then
              ! forward permutation
              do i = 1, m
                 if( k( i )>0 )go to 40
                 j = i
                 k( j ) = -k( j )
                 in = k( j )
                 20 continue
                 if( k( in )>0 )go to 40
                 do jj = 1, n
                    temp = x( j, jj )
                    x( j, jj ) = x( in, jj )
                    x( in, jj ) = temp
                 end do
                 k( in ) = -k( in )
                 j = in
                 in = k( in )
                 go to 20
                 40 continue
              end do
           else
              ! backward permutation
              do i = 1, m
                 if( k( i )>0 )go to 80
                 k( i ) = -k( i )
                 j = k( i )
                 60 continue
                 if( j==i )go to 80
                 do jj = 1, n
                    temp = x( i, jj )
                    x( i, jj ) = x( j, jj )
                    x( j, jj ) = temp
                 end do
                 k( j ) = -k( j )
                 j = k( j )
                 go to 60
                 80 continue
              end do
           end if
           return
     end subroutine stdlib_clapmr

     pure module subroutine stdlib_zlapmr( forwrd, m, n, x, ldx, k )
     !! ZLAPMR rearranges the rows of the M by N matrix X as specified
     !! by the permutation K(1),K(2),...,K(M) of the integers 1,...,M.
     !! If FORWRD = .TRUE.,  forward permutation:
     !! X(K(I),*) is moved X(I,*) for I = 1,2,...,M.
     !! If FORWRD = .FALSE., backward permutation:
     !! X(I,*) is moved to X(K(I),*) for I = 1,2,...,M.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           logical(lk), intent(in) :: forwrd
           integer(ilp), intent(in) :: ldx, m, n
           ! Array Arguments 
           integer(ilp), intent(inout) :: k(*)
           complex(dp), intent(inout) :: x(ldx,*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, in, j, jj
           complex(dp) :: temp
           ! Executable Statements 
           if( m<=1 )return
           do i = 1, m
              k( i ) = -k( i )
           end do
           if( forwrd ) then
              ! forward permutation
              do i = 1, m
                 if( k( i )>0 )go to 40
                 j = i
                 k( j ) = -k( j )
                 in = k( j )
                 20 continue
                 if( k( in )>0 )go to 40
                 do jj = 1, n
                    temp = x( j, jj )
                    x( j, jj ) = x( in, jj )
                    x( in, jj ) = temp
                 end do
                 k( in ) = -k( in )
                 j = in
                 in = k( in )
                 go to 20
                 40 continue
              end do
           else
              ! backward permutation
              do i = 1, m
                 if( k( i )>0 )go to 80
                 k( i ) = -k( i )
                 j = k( i )
                 60 continue
                 if( j==i )go to 80
                 do jj = 1, n
                    temp = x( i, jj )
                    x( i, jj ) = x( j, jj )
                    x( j, jj ) = temp
                 end do
                 k( j ) = -k( j )
                 j = k( j )
                 go to 60
                 80 continue
              end do
           end if
           return
     end subroutine stdlib_zlapmr




     pure module subroutine stdlib_slapmt( forwrd, m, n, x, ldx, k )
     !! SLAPMT rearranges the columns of the M by N matrix X as specified
     !! by the permutation K(1),K(2),...,K(N) of the integers 1,...,N.
     !! If FORWRD = .TRUE.,  forward permutation:
     !! X(*,K(J)) is moved X(*,J) for J = 1,2,...,N.
     !! If FORWRD = .FALSE., backward permutation:
     !! X(*,J) is moved to X(*,K(J)) for J = 1,2,...,N.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           logical(lk), intent(in) :: forwrd
           integer(ilp), intent(in) :: ldx, m, n
           ! Array Arguments 
           integer(ilp), intent(inout) :: k(*)
           real(sp), intent(inout) :: x(ldx,*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, ii, j, in
           real(sp) :: temp
           ! Executable Statements 
           if( n<=1 )return
           do i = 1, n
              k( i ) = -k( i )
           end do
           if( forwrd ) then
              ! forward permutation
              do i = 1, n
                 if( k( i )>0 )go to 40
                 j = i
                 k( j ) = -k( j )
                 in = k( j )
                 20 continue
                 if( k( in )>0 )go to 40
                 do ii = 1, m
                    temp = x( ii, j )
                    x( ii, j ) = x( ii, in )
                    x( ii, in ) = temp
                 end do
                 k( in ) = -k( in )
                 j = in
                 in = k( in )
                 go to 20
                 40 continue
              end do
           else
              ! backward permutation
              do i = 1, n
                 if( k( i )>0 )go to 100
                 k( i ) = -k( i )
                 j = k( i )
                 80 continue
                 if( j==i )go to 100
                 do ii = 1, m
                    temp = x( ii, i )
                    x( ii, i ) = x( ii, j )
                    x( ii, j ) = temp
                 end do
                 k( j ) = -k( j )
                 j = k( j )
                 go to 80
                 100 continue
              end do
           end if
           return
     end subroutine stdlib_slapmt

     pure module subroutine stdlib_dlapmt( forwrd, m, n, x, ldx, k )
     !! DLAPMT rearranges the columns of the M by N matrix X as specified
     !! by the permutation K(1),K(2),...,K(N) of the integers 1,...,N.
     !! If FORWRD = .TRUE.,  forward permutation:
     !! X(*,K(J)) is moved X(*,J) for J = 1,2,...,N.
     !! If FORWRD = .FALSE., backward permutation:
     !! X(*,J) is moved to X(*,K(J)) for J = 1,2,...,N.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           logical(lk), intent(in) :: forwrd
           integer(ilp), intent(in) :: ldx, m, n
           ! Array Arguments 
           integer(ilp), intent(inout) :: k(*)
           real(dp), intent(inout) :: x(ldx,*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, ii, in, j
           real(dp) :: temp
           ! Executable Statements 
           if( n<=1 )return
           do i = 1, n
              k( i ) = -k( i )
           end do
           if( forwrd ) then
              ! forward permutation
              do i = 1, n
                 if( k( i )>0 )go to 40
                 j = i
                 k( j ) = -k( j )
                 in = k( j )
                 20 continue
                 if( k( in )>0 )go to 40
                 do ii = 1, m
                    temp = x( ii, j )
                    x( ii, j ) = x( ii, in )
                    x( ii, in ) = temp
                 end do
                 k( in ) = -k( in )
                 j = in
                 in = k( in )
                 go to 20
                 40 continue
              end do
           else
              ! backward permutation
              do i = 1, n
                 if( k( i )>0 )go to 80
                 k( i ) = -k( i )
                 j = k( i )
                 60 continue
                 if( j==i )go to 80
                 do ii = 1, m
                    temp = x( ii, i )
                    x( ii, i ) = x( ii, j )
                    x( ii, j ) = temp
                 end do
                 k( j ) = -k( j )
                 j = k( j )
                 go to 60
                 80 continue
              end do
           end if
           return
     end subroutine stdlib_dlapmt


     pure module subroutine stdlib_clapmt( forwrd, m, n, x, ldx, k )
     !! CLAPMT rearranges the columns of the M by N matrix X as specified
     !! by the permutation K(1),K(2),...,K(N) of the integers 1,...,N.
     !! If FORWRD = .TRUE.,  forward permutation:
     !! X(*,K(J)) is moved X(*,J) for J = 1,2,...,N.
     !! If FORWRD = .FALSE., backward permutation:
     !! X(*,J) is moved to X(*,K(J)) for J = 1,2,...,N.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           logical(lk), intent(in) :: forwrd
           integer(ilp), intent(in) :: ldx, m, n
           ! Array Arguments 
           integer(ilp), intent(inout) :: k(*)
           complex(sp), intent(inout) :: x(ldx,*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, ii, j, in
           complex(sp) :: temp
           ! Executable Statements 
           if( n<=1 )return
           do i = 1, n
              k( i ) = -k( i )
           end do
           if( forwrd ) then
              ! forward permutation
              do i = 1, n
                 if( k( i )>0 )go to 40
                 j = i
                 k( j ) = -k( j )
                 in = k( j )
                 20 continue
                 if( k( in )>0 )go to 40
                 do ii = 1, m
                    temp = x( ii, j )
                    x( ii, j ) = x( ii, in )
                    x( ii, in ) = temp
                 end do
                 k( in ) = -k( in )
                 j = in
                 in = k( in )
                 go to 20
                 40 continue
              end do
           else
              ! backward permutation
              do i = 1, n
                 if( k( i )>0 )go to 100
                 k( i ) = -k( i )
                 j = k( i )
                 80 continue
                 if( j==i )go to 100
                 do ii = 1, m
                    temp = x( ii, i )
                    x( ii, i ) = x( ii, j )
                    x( ii, j ) = temp
                 end do
                 k( j ) = -k( j )
                 j = k( j )
                 go to 80
                 100 continue
              end do
           end if
           return
     end subroutine stdlib_clapmt

     pure module subroutine stdlib_zlapmt( forwrd, m, n, x, ldx, k )
     !! ZLAPMT rearranges the columns of the M by N matrix X as specified
     !! by the permutation K(1),K(2),...,K(N) of the integers 1,...,N.
     !! If FORWRD = .TRUE.,  forward permutation:
     !! X(*,K(J)) is moved X(*,J) for J = 1,2,...,N.
     !! If FORWRD = .FALSE., backward permutation:
     !! X(*,J) is moved to X(*,K(J)) for J = 1,2,...,N.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           logical(lk), intent(in) :: forwrd
           integer(ilp), intent(in) :: ldx, m, n
           ! Array Arguments 
           integer(ilp), intent(inout) :: k(*)
           complex(dp), intent(inout) :: x(ldx,*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, ii, in, j
           complex(dp) :: temp
           ! Executable Statements 
           if( n<=1 )return
           do i = 1, n
              k( i ) = -k( i )
           end do
           if( forwrd ) then
              ! forward permutation
              do i = 1, n
                 if( k( i )>0 )go to 40
                 j = i
                 k( j ) = -k( j )
                 in = k( j )
                 20 continue
                 if( k( in )>0 )go to 40
                 do ii = 1, m
                    temp = x( ii, j )
                    x( ii, j ) = x( ii, in )
                    x( ii, in ) = temp
                 end do
                 k( in ) = -k( in )
                 j = in
                 in = k( in )
                 go to 20
                 40 continue
              end do
           else
              ! backward permutation
              do i = 1, n
                 if( k( i )>0 )go to 80
                 k( i ) = -k( i )
                 j = k( i )
                 60 continue
                 if( j==i )go to 80
                 do ii = 1, m
                    temp = x( ii, i )
                    x( ii, i ) = x( ii, j )
                    x( ii, j ) = temp
                 end do
                 k( j ) = -k( j )
                 j = k( j )
                 go to 60
                 80 continue
              end do
           end if
           return
     end subroutine stdlib_zlapmt



end submodule stdlib_lapack_cosine_sine2
