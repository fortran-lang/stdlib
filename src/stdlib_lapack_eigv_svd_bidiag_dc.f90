submodule(stdlib_lapack_eig_svd_lsq) stdlib_lapack_eigv_svd_bidiag_dc
  implicit none


  contains

     pure module subroutine stdlib_slasd0( n, sqre, d, e, u, ldu, vt, ldvt, smlsiz, iwork,work, info )
     !! Using a divide and conquer approach, SLASD0: computes the singular
     !! value decomposition (SVD) of a real upper bidiagonal N-by-M
     !! matrix B with diagonal D and offdiagonal E, where M = N + SQRE.
     !! The algorithm computes orthogonal matrices U and VT such that
     !! B = U * S * VT. The singular values S are overwritten on D.
     !! A related subroutine, SLASDA, computes only the singular values,
     !! and optionally, the singular vectors in compact form.
               
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldu, ldvt, n, smlsiz, sqre
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(inout) :: d(*), e(*)
           real(sp), intent(out) :: u(ldu,*), vt(ldvt,*), work(*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, i1, ic, idxq, idxqc, im1, inode, itemp, iwk, j, lf, ll, lvl, m, ncc,&
                      nd, ndb1, ndiml, ndimr, nl, nlf, nlp1, nlvl, nr, nrf, nrp1, sqrei
           real(sp) :: alpha, beta
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( n<0_ilp ) then
              info = -1_ilp
           else if( ( sqre<0_ilp ) .or. ( sqre>1_ilp ) ) then
              info = -2_ilp
           end if
           m = n + sqre
           if( ldu<n ) then
              info = -6_ilp
           else if( ldvt<m ) then
              info = -8_ilp
           else if( smlsiz<3_ilp ) then
              info = -9_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SLASD0', -info )
              return
           end if
           ! if the input matrix is too small, call stdlib_slasdq to find the svd.
           if( n<=smlsiz ) then
              call stdlib_slasdq( 'U', sqre, n, m, n, 0_ilp, d, e, vt, ldvt, u, ldu, u,ldu, work, &
                        info )
              return
           end if
           ! set up the computation tree.
           inode = 1_ilp
           ndiml = inode + n
           ndimr = ndiml + n
           idxq = ndimr + n
           iwk = idxq + n
           call stdlib_slasdt( n, nlvl, nd, iwork( inode ), iwork( ndiml ),iwork( ndimr ), smlsiz &
                     )
           ! for the nodes on bottom level of the tree, solve
           ! their subproblems by stdlib_slasdq.
           ndb1 = ( nd+1 ) / 2_ilp
           ncc = 0_ilp
           loop_30: do i = ndb1, nd
           ! ic : center row of each node
           ! nl : number of rows of left  subproblem
           ! nr : number of rows of right subproblem
           ! nlf: starting row of the left   subproblem
           ! nrf: starting row of the right  subproblem
              i1 = i - 1_ilp
              ic = iwork( inode+i1 )
              nl = iwork( ndiml+i1 )
              nlp1 = nl + 1_ilp
              nr = iwork( ndimr+i1 )
              nrp1 = nr + 1_ilp
              nlf = ic - nl
              nrf = ic + 1_ilp
              sqrei = 1_ilp
              call stdlib_slasdq( 'U', sqrei, nl, nlp1, nl, ncc, d( nlf ), e( nlf ),vt( nlf, nlf )&
                        , ldvt, u( nlf, nlf ), ldu,u( nlf, nlf ), ldu, work, info )
              if( info/=0_ilp ) then
                 return
              end if
              itemp = idxq + nlf - 2_ilp
              do j = 1, nl
                 iwork( itemp+j ) = j
              end do
              if( i==nd ) then
                 sqrei = sqre
              else
                 sqrei = 1_ilp
              end if
              nrp1 = nr + sqrei
              call stdlib_slasdq( 'U', sqrei, nr, nrp1, nr, ncc, d( nrf ), e( nrf ),vt( nrf, nrf )&
                        , ldvt, u( nrf, nrf ), ldu,u( nrf, nrf ), ldu, work, info )
              if( info/=0_ilp ) then
                 return
              end if
              itemp = idxq + ic
              do j = 1, nr
                 iwork( itemp+j-1 ) = j
              end do
           end do loop_30
           ! now conquer each subproblem bottom-up.
           loop_50: do lvl = nlvl, 1, -1
              ! find the first node lf and last node ll on the
              ! current level lvl.
              if( lvl==1_ilp ) then
                 lf = 1_ilp
                 ll = 1_ilp
              else
                 lf = 2_ilp**( lvl-1 )
                 ll = 2_ilp*lf - 1_ilp
              end if
              do i = lf, ll
                 im1 = i - 1_ilp
                 ic = iwork( inode+im1 )
                 nl = iwork( ndiml+im1 )
                 nr = iwork( ndimr+im1 )
                 nlf = ic - nl
                 if( ( sqre==0_ilp ) .and. ( i==ll ) ) then
                    sqrei = sqre
                 else
                    sqrei = 1_ilp
                 end if
                 idxqc = idxq + nlf - 1_ilp
                 alpha = d( ic )
                 beta = e( ic )
                 call stdlib_slasd1( nl, nr, sqrei, d( nlf ), alpha, beta,u( nlf, nlf ), ldu, vt( &
                           nlf, nlf ), ldvt,iwork( idxqc ), iwork( iwk ), work, info )
           ! report the possible convergence failure.
                 if( info/=0_ilp ) then
                    return
                 end if
              end do
           end do loop_50
           return
     end subroutine stdlib_slasd0

     pure module subroutine stdlib_dlasd0( n, sqre, d, e, u, ldu, vt, ldvt, smlsiz, iwork,work, info )
     !! Using a divide and conquer approach, DLASD0: computes the singular
     !! value decomposition (SVD) of a real upper bidiagonal N-by-M
     !! matrix B with diagonal D and offdiagonal E, where M = N + SQRE.
     !! The algorithm computes orthogonal matrices U and VT such that
     !! B = U * S * VT. The singular values S are overwritten on D.
     !! A related subroutine, DLASDA, computes only the singular values,
     !! and optionally, the singular vectors in compact form.
               
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldu, ldvt, n, smlsiz, sqre
           ! Array Arguments 
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(inout) :: d(*), e(*)
           real(dp), intent(out) :: u(ldu,*), vt(ldvt,*), work(*)
        ! =====================================================================
           ! Local Scalars 
           integer(ilp) :: i, i1, ic, idxq, idxqc, im1, inode, itemp, iwk, j, lf, ll, lvl, m, ncc,&
                      nd, ndb1, ndiml, ndimr, nl, nlf, nlp1, nlvl, nr, nrf, nrp1, sqrei
           real(dp) :: alpha, beta
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( n<0_ilp ) then
              info = -1_ilp
           else if( ( sqre<0_ilp ) .or. ( sqre>1_ilp ) ) then
              info = -2_ilp
           end if
           m = n + sqre
           if( ldu<n ) then
              info = -6_ilp
           else if( ldvt<m ) then
              info = -8_ilp
           else if( smlsiz<3_ilp ) then
              info = -9_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DLASD0', -info )
              return
           end if
           ! if the input matrix is too small, call stdlib_dlasdq to find the svd.
           if( n<=smlsiz ) then
              call stdlib_dlasdq( 'U', sqre, n, m, n, 0_ilp, d, e, vt, ldvt, u, ldu, u,ldu, work, &
                        info )
              return
           end if
           ! set up the computation tree.
           inode = 1_ilp
           ndiml = inode + n
           ndimr = ndiml + n
           idxq = ndimr + n
           iwk = idxq + n
           call stdlib_dlasdt( n, nlvl, nd, iwork( inode ), iwork( ndiml ),iwork( ndimr ), smlsiz &
                     )
           ! for the nodes on bottom level of the tree, solve
           ! their subproblems by stdlib_dlasdq.
           ndb1 = ( nd+1 ) / 2_ilp
           ncc = 0_ilp
           loop_30: do i = ndb1, nd
           ! ic : center row of each node
           ! nl : number of rows of left  subproblem
           ! nr : number of rows of right subproblem
           ! nlf: starting row of the left   subproblem
           ! nrf: starting row of the right  subproblem
              i1 = i - 1_ilp
              ic = iwork( inode+i1 )
              nl = iwork( ndiml+i1 )
              nlp1 = nl + 1_ilp
              nr = iwork( ndimr+i1 )
              nrp1 = nr + 1_ilp
              nlf = ic - nl
              nrf = ic + 1_ilp
              sqrei = 1_ilp
              call stdlib_dlasdq( 'U', sqrei, nl, nlp1, nl, ncc, d( nlf ), e( nlf ),vt( nlf, nlf )&
                        , ldvt, u( nlf, nlf ), ldu,u( nlf, nlf ), ldu, work, info )
              if( info/=0_ilp ) then
                 return
              end if
              itemp = idxq + nlf - 2_ilp
              do j = 1, nl
                 iwork( itemp+j ) = j
              end do
              if( i==nd ) then
                 sqrei = sqre
              else
                 sqrei = 1_ilp
              end if
              nrp1 = nr + sqrei
              call stdlib_dlasdq( 'U', sqrei, nr, nrp1, nr, ncc, d( nrf ), e( nrf ),vt( nrf, nrf )&
                        , ldvt, u( nrf, nrf ), ldu,u( nrf, nrf ), ldu, work, info )
              if( info/=0_ilp ) then
                 return
              end if
              itemp = idxq + ic
              do j = 1, nr
                 iwork( itemp+j-1 ) = j
              end do
           end do loop_30
           ! now conquer each subproblem bottom-up.
           loop_50: do lvl = nlvl, 1, -1
              ! find the first node lf and last node ll on the
              ! current level lvl.
              if( lvl==1_ilp ) then
                 lf = 1_ilp
                 ll = 1_ilp
              else
                 lf = 2_ilp**( lvl-1 )
                 ll = 2_ilp*lf - 1_ilp
              end if
              do i = lf, ll
                 im1 = i - 1_ilp
                 ic = iwork( inode+im1 )
                 nl = iwork( ndiml+im1 )
                 nr = iwork( ndimr+im1 )
                 nlf = ic - nl
                 if( ( sqre==0_ilp ) .and. ( i==ll ) ) then
                    sqrei = sqre
                 else
                    sqrei = 1_ilp
                 end if
                 idxqc = idxq + nlf - 1_ilp
                 alpha = d( ic )
                 beta = e( ic )
                 call stdlib_dlasd1( nl, nr, sqrei, d( nlf ), alpha, beta,u( nlf, nlf ), ldu, vt( &
                           nlf, nlf ), ldvt,iwork( idxqc ), iwork( iwk ), work, info )
              ! report the possible convergence failure.
                 if( info/=0_ilp ) then
                    return
                 end if
              end do
           end do loop_50
           return
     end subroutine stdlib_dlasd0




     pure module subroutine stdlib_slasdt( n, lvl, nd, inode, ndiml, ndimr, msub )
     !! SLASDT creates a tree of subproblems for bidiagonal divide and
     !! conquer.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: lvl, nd
           integer(ilp), intent(in) :: msub, n
           ! Array Arguments 
           integer(ilp), intent(out) :: inode(*), ndiml(*), ndimr(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, il, ir, llst, maxn, ncrnt, nlvl
           real(sp) :: temp
           ! Intrinsic Functions 
           ! Executable Statements 
           ! find the number of levels on the tree.
           maxn = max( 1_ilp, n )
           temp = log( real( maxn,KIND=sp) / real( msub+1,KIND=sp) ) / log( two )
           lvl = int( temp,KIND=ilp) + 1_ilp
           i = n / 2_ilp
           inode( 1_ilp ) = i + 1_ilp
           ndiml( 1_ilp ) = i
           ndimr( 1_ilp ) = n - i - 1_ilp
           il = 0_ilp
           ir = 1_ilp
           llst = 1_ilp
           do nlvl = 1, lvl - 1
              ! constructing the tree at (nlvl+1)-st level. the number of
              ! nodes created on this level is llst * 2.
              do i = 0, llst - 1
                 il = il + 2_ilp
                 ir = ir + 2_ilp
                 ncrnt = llst + i
                 ndiml( il ) = ndiml( ncrnt ) / 2_ilp
                 ndimr( il ) = ndiml( ncrnt ) - ndiml( il ) - 1_ilp
                 inode( il ) = inode( ncrnt ) - ndimr( il ) - 1_ilp
                 ndiml( ir ) = ndimr( ncrnt ) / 2_ilp
                 ndimr( ir ) = ndimr( ncrnt ) - ndiml( ir ) - 1_ilp
                 inode( ir ) = inode( ncrnt ) + ndiml( ir ) + 1_ilp
              end do
              llst = llst*2_ilp
           end do
           nd = llst*2_ilp - 1_ilp
           return
     end subroutine stdlib_slasdt

     pure module subroutine stdlib_dlasdt( n, lvl, nd, inode, ndiml, ndimr, msub )
     !! DLASDT creates a tree of subproblems for bidiagonal divide and
     !! conquer.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: lvl, nd
           integer(ilp), intent(in) :: msub, n
           ! Array Arguments 
           integer(ilp), intent(out) :: inode(*), ndiml(*), ndimr(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, il, ir, llst, maxn, ncrnt, nlvl
           real(dp) :: temp
           ! Intrinsic Functions 
           ! Executable Statements 
           ! find the number of levels on the tree.
           maxn = max( 1_ilp, n )
           temp = log( real( maxn,KIND=dp) / real( msub+1,KIND=dp) ) / log( two )
           lvl = int( temp,KIND=ilp) + 1_ilp
           i = n / 2_ilp
           inode( 1_ilp ) = i + 1_ilp
           ndiml( 1_ilp ) = i
           ndimr( 1_ilp ) = n - i - 1_ilp
           il = 0_ilp
           ir = 1_ilp
           llst = 1_ilp
           do nlvl = 1, lvl - 1
              ! constructing the tree at (nlvl+1)-st level. the number of
              ! nodes created on this level is llst * 2.
              do i = 0, llst - 1
                 il = il + 2_ilp
                 ir = ir + 2_ilp
                 ncrnt = llst + i
                 ndiml( il ) = ndiml( ncrnt ) / 2_ilp
                 ndimr( il ) = ndiml( ncrnt ) - ndiml( il ) - 1_ilp
                 inode( il ) = inode( ncrnt ) - ndimr( il ) - 1_ilp
                 ndiml( ir ) = ndimr( ncrnt ) / 2_ilp
                 ndimr( ir ) = ndimr( ncrnt ) - ndiml( ir ) - 1_ilp
                 inode( ir ) = inode( ncrnt ) + ndiml( ir ) + 1_ilp
              end do
              llst = llst*2_ilp
           end do
           nd = llst*2_ilp - 1_ilp
           return
     end subroutine stdlib_dlasdt




     pure module subroutine stdlib_slasd1( nl, nr, sqre, d, alpha, beta, u, ldu, vt, ldvt,idxq, iwork, &
     !! SLASD1 computes the SVD of an upper bidiagonal N-by-M matrix B,
     !! where N = NL + NR + 1 and M = N + SQRE. SLASD1 is called from SLASD0.
     !! A related subroutine SLASD7 handles the case in which the singular
     !! values (and the singular vectors in factored form) are desired.
     !! SLASD1 computes the SVD as follows:
     !! ( D1(in)    0    0       0 )
     !! B = U(in) * (   Z1**T   a   Z2**T    b ) * VT(in)
     !! (   0       0   D2(in)   0 )
     !! = U(out) * ( D(out) 0) * VT(out)
     !! where Z**T = (Z1**T a Z2**T b) = u**T VT**T, and u is a vector of dimension M
     !! with ALPHA and BETA in the NL+1 and NL+2 th entries and zeros
     !! elsewhere; and the entry b is empty if SQRE = 0.
     !! The left singular vectors of the original matrix are stored in U, and
     !! the transpose of the right singular vectors are stored in VT, and the
     !! singular values are in D.  The algorithm consists of three stages:
     !! The first stage consists of deflating the size of the problem
     !! when there are multiple singular values or when there are zeros in
     !! the Z vector.  For each such occurrence the dimension of the
     !! secular equation problem is reduced by one.  This stage is
     !! performed by the routine SLASD2.
     !! The second stage consists of calculating the updated
     !! singular values. This is done by finding the square roots of the
     !! roots of the secular equation via the routine SLASD4 (as called
     !! by SLASD3). This routine also calculates the singular vectors of
     !! the current problem.
     !! The final stage consists of computing the updated singular vectors
     !! directly using the updated singular values.  The singular vectors
     !! for the current problem are multiplied with the singular vectors
     !! from the overall problem.
               work, info )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldu, ldvt, nl, nr, sqre
           real(sp), intent(inout) :: alpha, beta
           ! Array Arguments 
           integer(ilp), intent(inout) :: idxq(*)
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(inout) :: d(*), u(ldu,*), vt(ldvt,*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: coltyp, i, idx, idxc, idxp, iq, isigma, iu2, ivt2, iz, k, ldq, ldu2, &
                     ldvt2, m, n, n1, n2
           real(sp) :: orgnrm
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( nl<1_ilp ) then
              info = -1_ilp
           else if( nr<1_ilp ) then
              info = -2_ilp
           else if( ( sqre<0_ilp ) .or. ( sqre>1_ilp ) ) then
              info = -3_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SLASD1', -info )
              return
           end if
           n = nl + nr + 1_ilp
           m = n + sqre
           ! the following values are for bookkeeping purposes only.  they are
           ! integer pointers which indicate the portion of the workspace
           ! used by a particular array in stdlib_slasd2 and stdlib_slasd3.
           ldu2 = n
           ldvt2 = m
           iz = 1_ilp
           isigma = iz + m
           iu2 = isigma + n
           ivt2 = iu2 + ldu2*n
           iq = ivt2 + ldvt2*m
           idx = 1_ilp
           idxc = idx + n
           coltyp = idxc + n
           idxp = coltyp + n
           ! scale.
           orgnrm = max( abs( alpha ), abs( beta ) )
           d( nl+1 ) = zero
           do i = 1, n
              if( abs( d( i ) )>orgnrm ) then
                 orgnrm = abs( d( i ) )
              end if
           end do
           call stdlib_slascl( 'G', 0_ilp, 0_ilp, orgnrm, one, n, 1_ilp, d, n, info )
           alpha = alpha / orgnrm
           beta = beta / orgnrm
           ! deflate singular values.
           call stdlib_slasd2( nl, nr, sqre, k, d, work( iz ), alpha, beta, u, ldu,vt, ldvt, work(&
            isigma ), work( iu2 ), ldu2,work( ivt2 ), ldvt2, iwork( idxp ), iwork( idx ),iwork( &
                      idxc ), idxq, iwork( coltyp ), info )
           ! solve secular equation and update singular vectors.
           ldq = k
           call stdlib_slasd3( nl, nr, sqre, k, d, work( iq ), ldq, work( isigma ),u, ldu, work( &
           iu2 ), ldu2, vt, ldvt, work( ivt2 ),ldvt2, iwork( idxc ), iwork( coltyp ), work( iz ),&
                     info )
           ! report the possible convergence failure.
           if( info/=0_ilp ) then
              return
           end if
           ! unscale.
           call stdlib_slascl( 'G', 0_ilp, 0_ilp, one, orgnrm, n, 1_ilp, d, n, info )
           ! prepare the idxq sorting permutation.
           n1 = k
           n2 = n - k
           call stdlib_slamrg( n1, n2, d, 1_ilp, -1_ilp, idxq )
           return
     end subroutine stdlib_slasd1

     pure module subroutine stdlib_dlasd1( nl, nr, sqre, d, alpha, beta, u, ldu, vt, ldvt,idxq, iwork, &
     !! DLASD1 computes the SVD of an upper bidiagonal N-by-M matrix B,
     !! where N = NL + NR + 1 and M = N + SQRE. DLASD1 is called from DLASD0.
     !! A related subroutine DLASD7 handles the case in which the singular
     !! values (and the singular vectors in factored form) are desired.
     !! DLASD1 computes the SVD as follows:
     !! ( D1(in)    0    0       0 )
     !! B = U(in) * (   Z1**T   a   Z2**T    b ) * VT(in)
     !! (   0       0   D2(in)   0 )
     !! = U(out) * ( D(out) 0) * VT(out)
     !! where Z**T = (Z1**T a Z2**T b) = u**T VT**T, and u is a vector of dimension M
     !! with ALPHA and BETA in the NL+1 and NL+2 th entries and zeros
     !! elsewhere; and the entry b is empty if SQRE = 0.
     !! The left singular vectors of the original matrix are stored in U, and
     !! the transpose of the right singular vectors are stored in VT, and the
     !! singular values are in D.  The algorithm consists of three stages:
     !! The first stage consists of deflating the size of the problem
     !! when there are multiple singular values or when there are zeros in
     !! the Z vector.  For each such occurrence the dimension of the
     !! secular equation problem is reduced by one.  This stage is
     !! performed by the routine DLASD2.
     !! The second stage consists of calculating the updated
     !! singular values. This is done by finding the square roots of the
     !! roots of the secular equation via the routine DLASD4 (as called
     !! by DLASD3). This routine also calculates the singular vectors of
     !! the current problem.
     !! The final stage consists of computing the updated singular vectors
     !! directly using the updated singular values.  The singular vectors
     !! for the current problem are multiplied with the singular vectors
     !! from the overall problem.
               work, info )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldu, ldvt, nl, nr, sqre
           real(dp), intent(inout) :: alpha, beta
           ! Array Arguments 
           integer(ilp), intent(inout) :: idxq(*)
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(inout) :: d(*), u(ldu,*), vt(ldvt,*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: coltyp, i, idx, idxc, idxp, iq, isigma, iu2, ivt2, iz, k, ldq, ldu2, &
                     ldvt2, m, n, n1, n2
           real(dp) :: orgnrm
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( nl<1_ilp ) then
              info = -1_ilp
           else if( nr<1_ilp ) then
              info = -2_ilp
           else if( ( sqre<0_ilp ) .or. ( sqre>1_ilp ) ) then
              info = -3_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DLASD1', -info )
              return
           end if
           n = nl + nr + 1_ilp
           m = n + sqre
           ! the following values are for bookkeeping purposes only.  they are
           ! integer pointers which indicate the portion of the workspace
           ! used by a particular array in stdlib_dlasd2 and stdlib_dlasd3.
           ldu2 = n
           ldvt2 = m
           iz = 1_ilp
           isigma = iz + m
           iu2 = isigma + n
           ivt2 = iu2 + ldu2*n
           iq = ivt2 + ldvt2*m
           idx = 1_ilp
           idxc = idx + n
           coltyp = idxc + n
           idxp = coltyp + n
           ! scale.
           orgnrm = max( abs( alpha ), abs( beta ) )
           d( nl+1 ) = zero
           do i = 1, n
              if( abs( d( i ) )>orgnrm ) then
                 orgnrm = abs( d( i ) )
              end if
           end do
           call stdlib_dlascl( 'G', 0_ilp, 0_ilp, orgnrm, one, n, 1_ilp, d, n, info )
           alpha = alpha / orgnrm
           beta = beta / orgnrm
           ! deflate singular values.
           call stdlib_dlasd2( nl, nr, sqre, k, d, work( iz ), alpha, beta, u, ldu,vt, ldvt, work(&
            isigma ), work( iu2 ), ldu2,work( ivt2 ), ldvt2, iwork( idxp ), iwork( idx ),iwork( &
                      idxc ), idxq, iwork( coltyp ), info )
           ! solve secular equation and update singular vectors.
           ldq = k
           call stdlib_dlasd3( nl, nr, sqre, k, d, work( iq ), ldq, work( isigma ),u, ldu, work( &
           iu2 ), ldu2, vt, ldvt, work( ivt2 ),ldvt2, iwork( idxc ), iwork( coltyp ), work( iz ),&
                     info )
           ! report the convergence failure.
           if( info/=0_ilp ) then
              return
           end if
           ! unscale.
           call stdlib_dlascl( 'G', 0_ilp, 0_ilp, one, orgnrm, n, 1_ilp, d, n, info )
           ! prepare the idxq sorting permutation.
           n1 = k
           n2 = n - k
           call stdlib_dlamrg( n1, n2, d, 1_ilp, -1_ilp, idxq )
           return
     end subroutine stdlib_dlasd1




     pure module subroutine stdlib_slasd2( nl, nr, sqre, k, d, z, alpha, beta, u, ldu, vt,ldvt, dsigma, &
     !! SLASD2 merges the two sets of singular values together into a single
     !! sorted set.  Then it tries to deflate the size of the problem.
     !! There are two ways in which deflation can occur:  when two or more
     !! singular values are close together or if there is a tiny entry in the
     !! Z vector.  For each such occurrence the order of the related secular
     !! equation problem is reduced by one.
     !! SLASD2 is called from SLASD1.
               u2, ldu2, vt2, ldvt2, idxp, idx,idxc, idxq, coltyp, info )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info, k
           integer(ilp), intent(in) :: ldu, ldu2, ldvt, ldvt2, nl, nr, sqre
           real(sp), intent(in) :: alpha, beta
           ! Array Arguments 
           integer(ilp), intent(out) :: coltyp(*), idx(*), idxc(*), idxp(*)
           integer(ilp), intent(inout) :: idxq(*)
           real(sp), intent(inout) :: d(*), u(ldu,*), vt(ldvt,*)
           real(sp), intent(out) :: dsigma(*), u2(ldu2,*), vt2(ldvt2,*), z(*)
        ! =====================================================================
           
           ! Local Arrays 
           integer(ilp) :: ctot(4_ilp), psm(4_ilp)
           ! Local Scalars 
           integer(ilp) :: ct, i, idxi, idxj, idxjp, j, jp, jprev, k2, m, n, nlp1, nlp2
           real(sp) :: c, eps, hlftol, s, tau, tol, z1
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( nl<1_ilp ) then
              info = -1_ilp
           else if( nr<1_ilp ) then
              info = -2_ilp
           else if( ( sqre/=1_ilp ) .and. ( sqre/=0_ilp ) ) then
              info = -3_ilp
           end if
           n = nl + nr + 1_ilp
           m = n + sqre
           if( ldu<n ) then
              info = -10_ilp
           else if( ldvt<m ) then
              info = -12_ilp
           else if( ldu2<n ) then
              info = -15_ilp
           else if( ldvt2<m ) then
              info = -17_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SLASD2', -info )
              return
           end if
           nlp1 = nl + 1_ilp
           nlp2 = nl + 2_ilp
           ! generate the first part of the vector z; and move the singular
           ! values in the first part of d one position backward.
           z1 = alpha*vt( nlp1, nlp1 )
           z( 1_ilp ) = z1
           do i = nl, 1, -1
              z( i+1 ) = alpha*vt( i, nlp1 )
              d( i+1 ) = d( i )
              idxq( i+1 ) = idxq( i ) + 1_ilp
           end do
           ! generate the second part of the vector z.
           do i = nlp2, m
              z( i ) = beta*vt( i, nlp2 )
           end do
           ! initialize some reference arrays.
           do i = 2, nlp1
              coltyp( i ) = 1_ilp
           end do
           do i = nlp2, n
              coltyp( i ) = 2_ilp
           end do
           ! sort the singular values into increasing order
           do i = nlp2, n
              idxq( i ) = idxq( i ) + nlp1
           end do
           ! dsigma, idxc, idxc, and the first column of u2
           ! are used as storage space.
           do i = 2, n
              dsigma( i ) = d( idxq( i ) )
              u2( i, 1_ilp ) = z( idxq( i ) )
              idxc( i ) = coltyp( idxq( i ) )
           end do
           call stdlib_slamrg( nl, nr, dsigma( 2_ilp ), 1_ilp, 1_ilp, idx( 2_ilp ) )
           do i = 2, n
              idxi = 1_ilp + idx( i )
              d( i ) = dsigma( idxi )
              z( i ) = u2( idxi, 1_ilp )
              coltyp( i ) = idxc( idxi )
           end do
           ! calculate the allowable deflation tolerance
           eps = stdlib_slamch( 'EPSILON' )
           tol = max( abs( alpha ), abs( beta ) )
           tol = eight*eps*max( abs( d( n ) ), tol )
           ! there are 2 kinds of deflation -- first a value in the z-vector
           ! is small, second two (or more) singular values are very close
           ! together (their difference is small).
           ! if the value in the z-vector is small, we simply permute the
           ! array so that the corresponding singular value is moved to the
           ! end.
           ! if two values in the d-vector are close, we perform a two-sided
           ! rotation designed to make one of the corresponding z-vector
           ! entries zero, and then permute the array so that the deflated
           ! singular value is moved to the end.
           ! if there are multiple singular values then the problem deflates.
           ! here the number of equal singular values are found.  as each equal
           ! singular value is found, an elementary reflector is computed to
           ! rotate the corresponding singular subspace so that the
           ! corresponding components of z are zero in this new basis.
           k = 1_ilp
           k2 = n + 1_ilp
           do j = 2, n
              if( abs( z( j ) )<=tol ) then
                 ! deflate due to small z component.
                 k2 = k2 - 1_ilp
                 idxp( k2 ) = j
                 coltyp( j ) = 4_ilp
                 if( j==n )go to 120
              else
                 jprev = j
                 go to 90
              end if
           end do
           90 continue
           j = jprev
           100 continue
           j = j + 1_ilp
           if( j>n )go to 110
           if( abs( z( j ) )<=tol ) then
              ! deflate due to small z component.
              k2 = k2 - 1_ilp
              idxp( k2 ) = j
              coltyp( j ) = 4_ilp
           else
              ! check if singular values are close enough to allow deflation.
              if( abs( d( j )-d( jprev ) )<=tol ) then
                 ! deflation is possible.
                 s = z( jprev )
                 c = z( j )
                 ! find sqrt(a**2+b**2) without overflow or
                 ! destructive underflow.
                 tau = stdlib_slapy2( c, s )
                 c = c / tau
                 s = -s / tau
                 z( j ) = tau
                 z( jprev ) = zero
                 ! apply back the givens rotation to the left and right
                 ! singular vector matrices.
                 idxjp = idxq( idx( jprev )+1_ilp )
                 idxj = idxq( idx( j )+1_ilp )
                 if( idxjp<=nlp1 ) then
                    idxjp = idxjp - 1_ilp
                 end if
                 if( idxj<=nlp1 ) then
                    idxj = idxj - 1_ilp
                 end if
                 call stdlib_srot( n, u( 1_ilp, idxjp ), 1_ilp, u( 1_ilp, idxj ), 1_ilp, c, s )
                 call stdlib_srot( m, vt( idxjp, 1_ilp ), ldvt, vt( idxj, 1_ilp ), ldvt, c,s )
                 if( coltyp( j )/=coltyp( jprev ) ) then
                    coltyp( j ) = 3_ilp
                 end if
                 coltyp( jprev ) = 4_ilp
                 k2 = k2 - 1_ilp
                 idxp( k2 ) = jprev
                 jprev = j
              else
                 k = k + 1_ilp
                 u2( k, 1_ilp ) = z( jprev )
                 dsigma( k ) = d( jprev )
                 idxp( k ) = jprev
                 jprev = j
              end if
           end if
           go to 100
           110 continue
           ! record the last singular value.
           k = k + 1_ilp
           u2( k, 1_ilp ) = z( jprev )
           dsigma( k ) = d( jprev )
           idxp( k ) = jprev
           120 continue
           ! count up the total number of the various types of columns, then
           ! form a permutation which positions the four column types into
           ! four groups of uniform structure (although one or more of these
           ! groups may be empty).
           do j = 1, 4
              ctot( j ) = 0_ilp
           end do
           do j = 2, n
              ct = coltyp( j )
              ctot( ct ) = ctot( ct ) + 1_ilp
           end do
           ! psm(*) = position in submatrix (of types 1 through 4)
           psm( 1_ilp ) = 2_ilp
           psm( 2_ilp ) = 2_ilp + ctot( 1_ilp )
           psm( 3_ilp ) = psm( 2_ilp ) + ctot( 2_ilp )
           psm( 4_ilp ) = psm( 3_ilp ) + ctot( 3_ilp )
           ! fill out the idxc array so that the permutation which it induces
           ! will place all type-1 columns first, all type-2 columns next,
           ! then all type-3's, and finally all type-4's, starting from the
           ! second column. this applies similarly to the rows of vt.
           do j = 2, n
              jp = idxp( j )
              ct = coltyp( jp )
              idxc( psm( ct ) ) = j
              psm( ct ) = psm( ct ) + 1_ilp
           end do
           ! sort the singular values and corresponding singular vectors into
           ! dsigma, u2, and vt2 respectively.  the singular values/vectors
           ! which were not deflated go into the first k slots of dsigma, u2,
           ! and vt2 respectively, while those which were deflated go into the
           ! last n - k slots, except that the first column/row will be treated
           ! separately.
           do j = 2, n
              jp = idxp( j )
              dsigma( j ) = d( jp )
              idxj = idxq( idx( idxp( idxc( j ) ) )+1_ilp )
              if( idxj<=nlp1 ) then
                 idxj = idxj - 1_ilp
              end if
              call stdlib_scopy( n, u( 1_ilp, idxj ), 1_ilp, u2( 1_ilp, j ), 1_ilp )
              call stdlib_scopy( m, vt( idxj, 1_ilp ), ldvt, vt2( j, 1_ilp ), ldvt2 )
           end do
           ! determine dsigma(1), dsigma(2) and z(1)
           dsigma( 1_ilp ) = zero
           hlftol = tol / two
           if( abs( dsigma( 2_ilp ) )<=hlftol )dsigma( 2_ilp ) = hlftol
           if( m>n ) then
              z( 1_ilp ) = stdlib_slapy2( z1, z( m ) )
              if( z( 1_ilp )<=tol ) then
                 c = one
                 s = zero
                 z( 1_ilp ) = tol
              else
                 c = z1 / z( 1_ilp )
                 s = z( m ) / z( 1_ilp )
              end if
           else
              if( abs( z1 )<=tol ) then
                 z( 1_ilp ) = tol
              else
                 z( 1_ilp ) = z1
              end if
           end if
           ! move the rest of the updating row to z.
           call stdlib_scopy( k-1, u2( 2_ilp, 1_ilp ), 1_ilp, z( 2_ilp ), 1_ilp )
           ! determine the first column of u2, the first row of vt2 and the
           ! last row of vt.
           call stdlib_slaset( 'A', n, 1_ilp, zero, zero, u2, ldu2 )
           u2( nlp1, 1_ilp ) = one
           if( m>n ) then
              do i = 1, nlp1
                 vt( m, i ) = -s*vt( nlp1, i )
                 vt2( 1_ilp, i ) = c*vt( nlp1, i )
              end do
              do i = nlp2, m
                 vt2( 1_ilp, i ) = s*vt( m, i )
                 vt( m, i ) = c*vt( m, i )
              end do
           else
              call stdlib_scopy( m, vt( nlp1, 1_ilp ), ldvt, vt2( 1_ilp, 1_ilp ), ldvt2 )
           end if
           if( m>n ) then
              call stdlib_scopy( m, vt( m, 1_ilp ), ldvt, vt2( m, 1_ilp ), ldvt2 )
           end if
           ! the deflated singular values and their corresponding vectors go
           ! into the back of d, u, and v respectively.
           if( n>k ) then
              call stdlib_scopy( n-k, dsigma( k+1 ), 1_ilp, d( k+1 ), 1_ilp )
              call stdlib_slacpy( 'A', n, n-k, u2( 1_ilp, k+1 ), ldu2, u( 1_ilp, k+1 ),ldu )
              call stdlib_slacpy( 'A', n-k, m, vt2( k+1, 1_ilp ), ldvt2, vt( k+1, 1_ilp ),ldvt )
           end if
           ! copy ctot into coltyp for referencing in stdlib_slasd3.
           do j = 1, 4
              coltyp( j ) = ctot( j )
           end do
           return
     end subroutine stdlib_slasd2

     pure module subroutine stdlib_dlasd2( nl, nr, sqre, k, d, z, alpha, beta, u, ldu, vt,ldvt, dsigma, &
     !! DLASD2 merges the two sets of singular values together into a single
     !! sorted set.  Then it tries to deflate the size of the problem.
     !! There are two ways in which deflation can occur:  when two or more
     !! singular values are close together or if there is a tiny entry in the
     !! Z vector.  For each such occurrence the order of the related secular
     !! equation problem is reduced by one.
     !! DLASD2 is called from DLASD1.
               u2, ldu2, vt2, ldvt2, idxp, idx,idxc, idxq, coltyp, info )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info, k
           integer(ilp), intent(in) :: ldu, ldu2, ldvt, ldvt2, nl, nr, sqre
           real(dp), intent(in) :: alpha, beta
           ! Array Arguments 
           integer(ilp), intent(out) :: coltyp(*), idx(*), idxc(*), idxp(*)
           integer(ilp), intent(inout) :: idxq(*)
           real(dp), intent(inout) :: d(*), u(ldu,*), vt(ldvt,*)
           real(dp), intent(out) :: dsigma(*), u2(ldu2,*), vt2(ldvt2,*), z(*)
        ! =====================================================================
           
           ! Local Arrays 
           integer(ilp) :: ctot(4_ilp), psm(4_ilp)
           ! Local Scalars 
           integer(ilp) :: ct, i, idxi, idxj, idxjp, j, jp, jprev, k2, m, n, nlp1, nlp2
           real(dp) :: c, eps, hlftol, s, tau, tol, z1
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( nl<1_ilp ) then
              info = -1_ilp
           else if( nr<1_ilp ) then
              info = -2_ilp
           else if( ( sqre/=1_ilp ) .and. ( sqre/=0_ilp ) ) then
              info = -3_ilp
           end if
           n = nl + nr + 1_ilp
           m = n + sqre
           if( ldu<n ) then
              info = -10_ilp
           else if( ldvt<m ) then
              info = -12_ilp
           else if( ldu2<n ) then
              info = -15_ilp
           else if( ldvt2<m ) then
              info = -17_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DLASD2', -info )
              return
           end if
           nlp1 = nl + 1_ilp
           nlp2 = nl + 2_ilp
           ! generate the first part of the vector z; and move the singular
           ! values in the first part of d one position backward.
           z1 = alpha*vt( nlp1, nlp1 )
           z( 1_ilp ) = z1
           do i = nl, 1, -1
              z( i+1 ) = alpha*vt( i, nlp1 )
              d( i+1 ) = d( i )
              idxq( i+1 ) = idxq( i ) + 1_ilp
           end do
           ! generate the second part of the vector z.
           do i = nlp2, m
              z( i ) = beta*vt( i, nlp2 )
           end do
           ! initialize some reference arrays.
           do i = 2, nlp1
              coltyp( i ) = 1_ilp
           end do
           do i = nlp2, n
              coltyp( i ) = 2_ilp
           end do
           ! sort the singular values into increasing order
           do i = nlp2, n
              idxq( i ) = idxq( i ) + nlp1
           end do
           ! dsigma, idxc, idxc, and the first column of u2
           ! are used as storage space.
           do i = 2, n
              dsigma( i ) = d( idxq( i ) )
              u2( i, 1_ilp ) = z( idxq( i ) )
              idxc( i ) = coltyp( idxq( i ) )
           end do
           call stdlib_dlamrg( nl, nr, dsigma( 2_ilp ), 1_ilp, 1_ilp, idx( 2_ilp ) )
           do i = 2, n
              idxi = 1_ilp + idx( i )
              d( i ) = dsigma( idxi )
              z( i ) = u2( idxi, 1_ilp )
              coltyp( i ) = idxc( idxi )
           end do
           ! calculate the allowable deflation tolerance
           eps = stdlib_dlamch( 'EPSILON' )
           tol = max( abs( alpha ), abs( beta ) )
           tol = eight*eps*max( abs( d( n ) ), tol )
           ! there are 2 kinds of deflation -- first a value in the z-vector
           ! is small, second two (or more) singular values are very close
           ! together (their difference is small).
           ! if the value in the z-vector is small, we simply permute the
           ! array so that the corresponding singular value is moved to the
           ! end.
           ! if two values in the d-vector are close, we perform a two-sided
           ! rotation designed to make one of the corresponding z-vector
           ! entries zero, and then permute the array so that the deflated
           ! singular value is moved to the end.
           ! if there are multiple singular values then the problem deflates.
           ! here the number of equal singular values are found.  as each equal
           ! singular value is found, an elementary reflector is computed to
           ! rotate the corresponding singular subspace so that the
           ! corresponding components of z are zero in this new basis.
           k = 1_ilp
           k2 = n + 1_ilp
           do j = 2, n
              if( abs( z( j ) )<=tol ) then
                 ! deflate due to small z component.
                 k2 = k2 - 1_ilp
                 idxp( k2 ) = j
                 coltyp( j ) = 4_ilp
                 if( j==n )go to 120
              else
                 jprev = j
                 go to 90
              end if
           end do
           90 continue
           j = jprev
           100 continue
           j = j + 1_ilp
           if( j>n )go to 110
           if( abs( z( j ) )<=tol ) then
              ! deflate due to small z component.
              k2 = k2 - 1_ilp
              idxp( k2 ) = j
              coltyp( j ) = 4_ilp
           else
              ! check if singular values are close enough to allow deflation.
              if( abs( d( j )-d( jprev ) )<=tol ) then
                 ! deflation is possible.
                 s = z( jprev )
                 c = z( j )
                 ! find sqrt(a**2+b**2) without overflow or
                 ! destructive underflow.
                 tau = stdlib_dlapy2( c, s )
                 c = c / tau
                 s = -s / tau
                 z( j ) = tau
                 z( jprev ) = zero
                 ! apply back the givens rotation to the left and right
                 ! singular vector matrices.
                 idxjp = idxq( idx( jprev )+1_ilp )
                 idxj = idxq( idx( j )+1_ilp )
                 if( idxjp<=nlp1 ) then
                    idxjp = idxjp - 1_ilp
                 end if
                 if( idxj<=nlp1 ) then
                    idxj = idxj - 1_ilp
                 end if
                 call stdlib_drot( n, u( 1_ilp, idxjp ), 1_ilp, u( 1_ilp, idxj ), 1_ilp, c, s )
                 call stdlib_drot( m, vt( idxjp, 1_ilp ), ldvt, vt( idxj, 1_ilp ), ldvt, c,s )
                 if( coltyp( j )/=coltyp( jprev ) ) then
                    coltyp( j ) = 3_ilp
                 end if
                 coltyp( jprev ) = 4_ilp
                 k2 = k2 - 1_ilp
                 idxp( k2 ) = jprev
                 jprev = j
              else
                 k = k + 1_ilp
                 u2( k, 1_ilp ) = z( jprev )
                 dsigma( k ) = d( jprev )
                 idxp( k ) = jprev
                 jprev = j
              end if
           end if
           go to 100
           110 continue
           ! record the last singular value.
           k = k + 1_ilp
           u2( k, 1_ilp ) = z( jprev )
           dsigma( k ) = d( jprev )
           idxp( k ) = jprev
           120 continue
           ! count up the total number of the various types of columns, then
           ! form a permutation which positions the four column types into
           ! four groups of uniform structure (although one or more of these
           ! groups may be empty).
           do j = 1, 4
              ctot( j ) = 0_ilp
           end do
           do j = 2, n
              ct = coltyp( j )
              ctot( ct ) = ctot( ct ) + 1_ilp
           end do
           ! psm(*) = position in submatrix (of types 1 through 4)
           psm( 1_ilp ) = 2_ilp
           psm( 2_ilp ) = 2_ilp + ctot( 1_ilp )
           psm( 3_ilp ) = psm( 2_ilp ) + ctot( 2_ilp )
           psm( 4_ilp ) = psm( 3_ilp ) + ctot( 3_ilp )
           ! fill out the idxc array so that the permutation which it induces
           ! will place all type-1 columns first, all type-2 columns next,
           ! then all type-3's, and finally all type-4's, starting from the
           ! second column. this applies similarly to the rows of vt.
           do j = 2, n
              jp = idxp( j )
              ct = coltyp( jp )
              idxc( psm( ct ) ) = j
              psm( ct ) = psm( ct ) + 1_ilp
           end do
           ! sort the singular values and corresponding singular vectors into
           ! dsigma, u2, and vt2 respectively.  the singular values/vectors
           ! which were not deflated go into the first k slots of dsigma, u2,
           ! and vt2 respectively, while those which were deflated go into the
           ! last n - k slots, except that the first column/row will be treated
           ! separately.
           do j = 2, n
              jp = idxp( j )
              dsigma( j ) = d( jp )
              idxj = idxq( idx( idxp( idxc( j ) ) )+1_ilp )
              if( idxj<=nlp1 ) then
                 idxj = idxj - 1_ilp
              end if
              call stdlib_dcopy( n, u( 1_ilp, idxj ), 1_ilp, u2( 1_ilp, j ), 1_ilp )
              call stdlib_dcopy( m, vt( idxj, 1_ilp ), ldvt, vt2( j, 1_ilp ), ldvt2 )
           end do
           ! determine dsigma(1), dsigma(2) and z(1)
           dsigma( 1_ilp ) = zero
           hlftol = tol / two
           if( abs( dsigma( 2_ilp ) )<=hlftol )dsigma( 2_ilp ) = hlftol
           if( m>n ) then
              z( 1_ilp ) = stdlib_dlapy2( z1, z( m ) )
              if( z( 1_ilp )<=tol ) then
                 c = one
                 s = zero
                 z( 1_ilp ) = tol
              else
                 c = z1 / z( 1_ilp )
                 s = z( m ) / z( 1_ilp )
              end if
           else
              if( abs( z1 )<=tol ) then
                 z( 1_ilp ) = tol
              else
                 z( 1_ilp ) = z1
              end if
           end if
           ! move the rest of the updating row to z.
           call stdlib_dcopy( k-1, u2( 2_ilp, 1_ilp ), 1_ilp, z( 2_ilp ), 1_ilp )
           ! determine the first column of u2, the first row of vt2 and the
           ! last row of vt.
           call stdlib_dlaset( 'A', n, 1_ilp, zero, zero, u2, ldu2 )
           u2( nlp1, 1_ilp ) = one
           if( m>n ) then
              do i = 1, nlp1
                 vt( m, i ) = -s*vt( nlp1, i )
                 vt2( 1_ilp, i ) = c*vt( nlp1, i )
              end do
              do i = nlp2, m
                 vt2( 1_ilp, i ) = s*vt( m, i )
                 vt( m, i ) = c*vt( m, i )
              end do
           else
              call stdlib_dcopy( m, vt( nlp1, 1_ilp ), ldvt, vt2( 1_ilp, 1_ilp ), ldvt2 )
           end if
           if( m>n ) then
              call stdlib_dcopy( m, vt( m, 1_ilp ), ldvt, vt2( m, 1_ilp ), ldvt2 )
           end if
           ! the deflated singular values and their corresponding vectors go
           ! into the back of d, u, and v respectively.
           if( n>k ) then
              call stdlib_dcopy( n-k, dsigma( k+1 ), 1_ilp, d( k+1 ), 1_ilp )
              call stdlib_dlacpy( 'A', n, n-k, u2( 1_ilp, k+1 ), ldu2, u( 1_ilp, k+1 ),ldu )
              call stdlib_dlacpy( 'A', n-k, m, vt2( k+1, 1_ilp ), ldvt2, vt( k+1, 1_ilp ),ldvt )
           end if
           ! copy ctot into coltyp for referencing in stdlib_dlasd3.
           do j = 1, 4
              coltyp( j ) = ctot( j )
           end do
           return
     end subroutine stdlib_dlasd2




     pure module subroutine stdlib_slasd3( nl, nr, sqre, k, d, q, ldq, dsigma, u, ldu, u2,ldu2, vt, ldvt,&
     !! SLASD3 finds all the square roots of the roots of the secular
     !! equation, as defined by the values in D and Z.  It makes the
     !! appropriate calls to SLASD4 and then updates the singular
     !! vectors by matrix multiplication.
     !! This code makes very mild assumptions about floating point
     !! arithmetic. It will work on machines with a guard digit in
     !! add/subtract, or on those binary machines without guard digits
     !! which subtract like the Cray XMP, Cray YMP, Cray C 90, or Cray 2.
     !! It could conceivably fail on hexadecimal or decimal machines
     !! without guard digits, but we know of none.
     !! SLASD3 is called from SLASD1.
                vt2, ldvt2, idxc, ctot, z,info )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, ldq, ldu, ldu2, ldvt, ldvt2, nl, nr, sqre
           ! Array Arguments 
           integer(ilp), intent(in) :: ctot(*), idxc(*)
           real(sp), intent(out) :: d(*), q(ldq,*), u(ldu,*), vt(ldvt,*)
           real(sp), intent(inout) :: dsigma(*), vt2(ldvt2,*), z(*)
           real(sp), intent(in) :: u2(ldu2,*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: ctemp, i, j, jc, ktemp, m, n, nlp1, nlp2, nrp1
           real(sp) :: rho, temp
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( nl<1_ilp ) then
              info = -1_ilp
           else if( nr<1_ilp ) then
              info = -2_ilp
           else if( ( sqre/=1_ilp ) .and. ( sqre/=0_ilp ) ) then
              info = -3_ilp
           end if
           n = nl + nr + 1_ilp
           m = n + sqre
           nlp1 = nl + 1_ilp
           nlp2 = nl + 2_ilp
           if( ( k<1_ilp ) .or. ( k>n ) ) then
              info = -4_ilp
           else if( ldq<k ) then
              info = -7_ilp
           else if( ldu<n ) then
              info = -10_ilp
           else if( ldu2<n ) then
              info = -12_ilp
           else if( ldvt<m ) then
              info = -14_ilp
           else if( ldvt2<m ) then
              info = -16_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SLASD3', -info )
              return
           end if
           ! quick return if possible
           if( k==1_ilp ) then
              d( 1_ilp ) = abs( z( 1_ilp ) )
              call stdlib_scopy( m, vt2( 1_ilp, 1_ilp ), ldvt2, vt( 1_ilp, 1_ilp ), ldvt )
              if( z( 1_ilp )>zero ) then
                 call stdlib_scopy( n, u2( 1_ilp, 1_ilp ), 1_ilp, u( 1_ilp, 1_ilp ), 1_ilp )
              else
                 do i = 1, n
                    u( i, 1_ilp ) = -u2( i, 1_ilp )
                 end do
              end if
              return
           end if
           ! modify values dsigma(i) to make sure all dsigma(i)-dsigma(j) can
           ! be computed with high relative accuracy (barring over/underflow).
           ! this is a problem on machines without a guard digit in
           ! add/subtract (cray xmp, cray ymp, cray c 90 and cray 2).
           ! the following code replaces dsigma(i) by 2*dsigma(i)-dsigma(i),
           ! which on any of these machines zeros out the bottommost
           ! bit of dsigma(i) if it is 1; this makes the subsequent
           ! subtractions dsigma(i)-dsigma(j) unproblematic when cancellation
           ! occurs. on binary machines with a guard digit (almost all
           ! machines) it does not change dsigma(i) at all. on hexadecimal
           ! and decimal machines with a guard digit, it slightly
           ! changes the bottommost bits of dsigma(i). it does not account
           ! for hexadecimal or decimal machines without guard digits
           ! (we know of none). we use a subroutine call to compute
           ! 2*dsigma(i) to prevent optimizing compilers from eliminating
           ! this code.
           do i = 1, k
              dsigma( i ) = stdlib_slamc3( dsigma( i ), dsigma( i ) ) - dsigma( i )
           end do
           ! keep a copy of z.
           call stdlib_scopy( k, z, 1_ilp, q, 1_ilp )
           ! normalize z.
           rho = stdlib_snrm2( k, z, 1_ilp )
           call stdlib_slascl( 'G', 0_ilp, 0_ilp, rho, one, k, 1_ilp, z, k, info )
           rho = rho*rho
           ! find the new singular values.
           do j = 1, k
              call stdlib_slasd4( k, j, dsigma, z, u( 1_ilp, j ), rho, d( j ),vt( 1_ilp, j ), info )
                        
              ! if the zero finder fails, report the convergence failure.
              if( info/=0_ilp ) then
                 return
              end if
           end do
           ! compute updated z.
           do i = 1, k
              z( i ) = u( i, k )*vt( i, k )
              do j = 1, i - 1
                 z( i ) = z( i )*( u( i, j )*vt( i, j ) /( dsigma( i )-dsigma( j ) ) /( dsigma( i &
                           )+dsigma( j ) ) )
              end do
              do j = i, k - 1
                 z( i ) = z( i )*( u( i, j )*vt( i, j ) /( dsigma( i )-dsigma( j+1 ) ) /( dsigma( &
                           i )+dsigma( j+1 ) ) )
              end do
              z( i ) = sign( sqrt( abs( z( i ) ) ), q( i, 1_ilp ) )
           end do
           ! compute left singular vectors of the modified diagonal matrix,
           ! and store related information for the right singular vectors.
           do i = 1, k
              vt( 1_ilp, i ) = z( 1_ilp ) / u( 1_ilp, i ) / vt( 1_ilp, i )
              u( 1_ilp, i ) = negone
              do j = 2, k
                 vt( j, i ) = z( j ) / u( j, i ) / vt( j, i )
                 u( j, i ) = dsigma( j )*vt( j, i )
              end do
              temp = stdlib_snrm2( k, u( 1_ilp, i ), 1_ilp )
              q( 1_ilp, i ) = u( 1_ilp, i ) / temp
              do j = 2, k
                 jc = idxc( j )
                 q( j, i ) = u( jc, i ) / temp
              end do
           end do
           ! update the left singular vector matrix.
           if( k==2_ilp ) then
              call stdlib_sgemm( 'N', 'N', n, k, k, one, u2, ldu2, q, ldq, zero, u,ldu )
              go to 100
           end if
           if( ctot( 1_ilp )>0_ilp ) then
              call stdlib_sgemm( 'N', 'N', nl, k, ctot( 1_ilp ), one, u2( 1_ilp, 2_ilp ), ldu2,q( 2_ilp, 1_ilp ), ldq,&
                         zero, u( 1_ilp, 1_ilp ), ldu )
              if( ctot( 3_ilp )>0_ilp ) then
                 ktemp = 2_ilp + ctot( 1_ilp ) + ctot( 2_ilp )
                 call stdlib_sgemm( 'N', 'N', nl, k, ctot( 3_ilp ), one, u2( 1_ilp, ktemp ),ldu2, q( &
                           ktemp, 1_ilp ), ldq, one, u( 1_ilp, 1_ilp ), ldu )
              end if
           else if( ctot( 3_ilp )>0_ilp ) then
              ktemp = 2_ilp + ctot( 1_ilp ) + ctot( 2_ilp )
              call stdlib_sgemm( 'N', 'N', nl, k, ctot( 3_ilp ), one, u2( 1_ilp, ktemp ),ldu2, q( ktemp, &
                        1_ilp ), ldq, zero, u( 1_ilp, 1_ilp ), ldu )
           else
              call stdlib_slacpy( 'F', nl, k, u2, ldu2, u, ldu )
           end if
           call stdlib_scopy( k, q( 1_ilp, 1_ilp ), ldq, u( nlp1, 1_ilp ), ldu )
           ktemp = 2_ilp + ctot( 1_ilp )
           ctemp = ctot( 2_ilp ) + ctot( 3_ilp )
           call stdlib_sgemm( 'N', 'N', nr, k, ctemp, one, u2( nlp2, ktemp ), ldu2,q( ktemp, 1_ilp ), &
                     ldq, zero, u( nlp2, 1_ilp ), ldu )
           ! generate the right singular vectors.
           100 continue
           do i = 1, k
              temp = stdlib_snrm2( k, vt( 1_ilp, i ), 1_ilp )
              q( i, 1_ilp ) = vt( 1_ilp, i ) / temp
              do j = 2, k
                 jc = idxc( j )
                 q( i, j ) = vt( jc, i ) / temp
              end do
           end do
           ! update the right singular vector matrix.
           if( k==2_ilp ) then
              call stdlib_sgemm( 'N', 'N', k, m, k, one, q, ldq, vt2, ldvt2, zero,vt, ldvt )
                        
              return
           end if
           ktemp = 1_ilp + ctot( 1_ilp )
           call stdlib_sgemm( 'N', 'N', k, nlp1, ktemp, one, q( 1_ilp, 1_ilp ), ldq,vt2( 1_ilp, 1_ilp ), ldvt2, &
                     zero, vt( 1_ilp, 1_ilp ), ldvt )
           ktemp = 2_ilp + ctot( 1_ilp ) + ctot( 2_ilp )
           if( ktemp<=ldvt2 )call stdlib_sgemm( 'N', 'N', k, nlp1, ctot( 3_ilp ), one, q( 1_ilp, ktemp ),&
                     ldq, vt2( ktemp, 1_ilp ), ldvt2, one, vt( 1_ilp, 1_ilp ),ldvt )
           ktemp = ctot( 1_ilp ) + 1_ilp
           nrp1 = nr + sqre
           if( ktemp>1_ilp ) then
              do i = 1, k
                 q( i, ktemp ) = q( i, 1_ilp )
              end do
              do i = nlp2, m
                 vt2( ktemp, i ) = vt2( 1_ilp, i )
              end do
           end if
           ctemp = 1_ilp + ctot( 2_ilp ) + ctot( 3_ilp )
           call stdlib_sgemm( 'N', 'N', k, nrp1, ctemp, one, q( 1_ilp, ktemp ), ldq,vt2( ktemp, nlp2 )&
                     , ldvt2, zero, vt( 1_ilp, nlp2 ), ldvt )
           return
     end subroutine stdlib_slasd3

     pure module subroutine stdlib_dlasd3( nl, nr, sqre, k, d, q, ldq, dsigma, u, ldu, u2,ldu2, vt, ldvt,&
     !! DLASD3 finds all the square roots of the roots of the secular
     !! equation, as defined by the values in D and Z.  It makes the
     !! appropriate calls to DLASD4 and then updates the singular
     !! vectors by matrix multiplication.
     !! This code makes very mild assumptions about floating point
     !! arithmetic. It will work on machines with a guard digit in
     !! add/subtract, or on those binary machines without guard digits
     !! which subtract like the Cray XMP, Cray YMP, Cray C 90, or Cray 2.
     !! It could conceivably fail on hexadecimal or decimal machines
     !! without guard digits, but we know of none.
     !! DLASD3 is called from DLASD1.
                vt2, ldvt2, idxc, ctot, z,info )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, ldq, ldu, ldu2, ldvt, ldvt2, nl, nr, sqre
           ! Array Arguments 
           integer(ilp), intent(in) :: ctot(*), idxc(*)
           real(dp), intent(out) :: d(*), q(ldq,*), u(ldu,*), vt(ldvt,*)
           real(dp), intent(inout) :: dsigma(*), vt2(ldvt2,*), z(*)
           real(dp), intent(in) :: u2(ldu2,*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: ctemp, i, j, jc, ktemp, m, n, nlp1, nlp2, nrp1
           real(dp) :: rho, temp
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( nl<1_ilp ) then
              info = -1_ilp
           else if( nr<1_ilp ) then
              info = -2_ilp
           else if( ( sqre/=1_ilp ) .and. ( sqre/=0_ilp ) ) then
              info = -3_ilp
           end if
           n = nl + nr + 1_ilp
           m = n + sqre
           nlp1 = nl + 1_ilp
           nlp2 = nl + 2_ilp
           if( ( k<1_ilp ) .or. ( k>n ) ) then
              info = -4_ilp
           else if( ldq<k ) then
              info = -7_ilp
           else if( ldu<n ) then
              info = -10_ilp
           else if( ldu2<n ) then
              info = -12_ilp
           else if( ldvt<m ) then
              info = -14_ilp
           else if( ldvt2<m ) then
              info = -16_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DLASD3', -info )
              return
           end if
           ! quick return if possible
           if( k==1_ilp ) then
              d( 1_ilp ) = abs( z( 1_ilp ) )
              call stdlib_dcopy( m, vt2( 1_ilp, 1_ilp ), ldvt2, vt( 1_ilp, 1_ilp ), ldvt )
              if( z( 1_ilp )>zero ) then
                 call stdlib_dcopy( n, u2( 1_ilp, 1_ilp ), 1_ilp, u( 1_ilp, 1_ilp ), 1_ilp )
              else
                 do i = 1, n
                    u( i, 1_ilp ) = -u2( i, 1_ilp )
                 end do
              end if
              return
           end if
           ! modify values dsigma(i) to make sure all dsigma(i)-dsigma(j) can
           ! be computed with high relative accuracy (barring over/underflow).
           ! this is a problem on machines without a guard digit in
           ! add/subtract (cray xmp, cray ymp, cray c 90 and cray 2).
           ! the following code replaces dsigma(i) by 2*dsigma(i)-dsigma(i),
           ! which on any of these machines zeros out the bottommost
           ! bit of dsigma(i) if it is 1; this makes the subsequent
           ! subtractions dsigma(i)-dsigma(j) unproblematic when cancellation
           ! occurs. on binary machines with a guard digit (almost all
           ! machines) it does not change dsigma(i) at all. on hexadecimal
           ! and decimal machines with a guard digit, it slightly
           ! changes the bottommost bits of dsigma(i). it does not account
           ! for hexadecimal or decimal machines without guard digits
           ! (we know of none). we use a subroutine call to compute
           ! 2*dsigma(i) to prevent optimizing compilers from eliminating
           ! this code.
           do i = 1, k
              dsigma( i ) = stdlib_dlamc3( dsigma( i ), dsigma( i ) ) - dsigma( i )
           end do
           ! keep a copy of z.
           call stdlib_dcopy( k, z, 1_ilp, q, 1_ilp )
           ! normalize z.
           rho = stdlib_dnrm2( k, z, 1_ilp )
           call stdlib_dlascl( 'G', 0_ilp, 0_ilp, rho, one, k, 1_ilp, z, k, info )
           rho = rho*rho
           ! find the new singular values.
           do j = 1, k
              call stdlib_dlasd4( k, j, dsigma, z, u( 1_ilp, j ), rho, d( j ),vt( 1_ilp, j ), info )
                        
              ! if the zero finder fails, report the convergence failure.
              if( info/=0_ilp ) then
                 return
              end if
           end do
           ! compute updated z.
           do i = 1, k
              z( i ) = u( i, k )*vt( i, k )
              do j = 1, i - 1
                 z( i ) = z( i )*( u( i, j )*vt( i, j ) /( dsigma( i )-dsigma( j ) ) /( dsigma( i &
                           )+dsigma( j ) ) )
              end do
              do j = i, k - 1
                 z( i ) = z( i )*( u( i, j )*vt( i, j ) /( dsigma( i )-dsigma( j+1 ) ) /( dsigma( &
                           i )+dsigma( j+1 ) ) )
              end do
              z( i ) = sign( sqrt( abs( z( i ) ) ), q( i, 1_ilp ) )
           end do
           ! compute left singular vectors of the modified diagonal matrix,
           ! and store related information for the right singular vectors.
           do i = 1, k
              vt( 1_ilp, i ) = z( 1_ilp ) / u( 1_ilp, i ) / vt( 1_ilp, i )
              u( 1_ilp, i ) = negone
              do j = 2, k
                 vt( j, i ) = z( j ) / u( j, i ) / vt( j, i )
                 u( j, i ) = dsigma( j )*vt( j, i )
              end do
              temp = stdlib_dnrm2( k, u( 1_ilp, i ), 1_ilp )
              q( 1_ilp, i ) = u( 1_ilp, i ) / temp
              do j = 2, k
                 jc = idxc( j )
                 q( j, i ) = u( jc, i ) / temp
              end do
           end do
           ! update the left singular vector matrix.
           if( k==2_ilp ) then
              call stdlib_dgemm( 'N', 'N', n, k, k, one, u2, ldu2, q, ldq, zero, u,ldu )
              go to 100
           end if
           if( ctot( 1_ilp )>0_ilp ) then
              call stdlib_dgemm( 'N', 'N', nl, k, ctot( 1_ilp ), one, u2( 1_ilp, 2_ilp ), ldu2,q( 2_ilp, 1_ilp ), ldq,&
                         zero, u( 1_ilp, 1_ilp ), ldu )
              if( ctot( 3_ilp )>0_ilp ) then
                 ktemp = 2_ilp + ctot( 1_ilp ) + ctot( 2_ilp )
                 call stdlib_dgemm( 'N', 'N', nl, k, ctot( 3_ilp ), one, u2( 1_ilp, ktemp ),ldu2, q( &
                           ktemp, 1_ilp ), ldq, one, u( 1_ilp, 1_ilp ), ldu )
              end if
           else if( ctot( 3_ilp )>0_ilp ) then
              ktemp = 2_ilp + ctot( 1_ilp ) + ctot( 2_ilp )
              call stdlib_dgemm( 'N', 'N', nl, k, ctot( 3_ilp ), one, u2( 1_ilp, ktemp ),ldu2, q( ktemp, &
                        1_ilp ), ldq, zero, u( 1_ilp, 1_ilp ), ldu )
           else
              call stdlib_dlacpy( 'F', nl, k, u2, ldu2, u, ldu )
           end if
           call stdlib_dcopy( k, q( 1_ilp, 1_ilp ), ldq, u( nlp1, 1_ilp ), ldu )
           ktemp = 2_ilp + ctot( 1_ilp )
           ctemp = ctot( 2_ilp ) + ctot( 3_ilp )
           call stdlib_dgemm( 'N', 'N', nr, k, ctemp, one, u2( nlp2, ktemp ), ldu2,q( ktemp, 1_ilp ), &
                     ldq, zero, u( nlp2, 1_ilp ), ldu )
           ! generate the right singular vectors.
           100 continue
           do i = 1, k
              temp = stdlib_dnrm2( k, vt( 1_ilp, i ), 1_ilp )
              q( i, 1_ilp ) = vt( 1_ilp, i ) / temp
              do j = 2, k
                 jc = idxc( j )
                 q( i, j ) = vt( jc, i ) / temp
              end do
           end do
           ! update the right singular vector matrix.
           if( k==2_ilp ) then
              call stdlib_dgemm( 'N', 'N', k, m, k, one, q, ldq, vt2, ldvt2, zero,vt, ldvt )
                        
              return
           end if
           ktemp = 1_ilp + ctot( 1_ilp )
           call stdlib_dgemm( 'N', 'N', k, nlp1, ktemp, one, q( 1_ilp, 1_ilp ), ldq,vt2( 1_ilp, 1_ilp ), ldvt2, &
                     zero, vt( 1_ilp, 1_ilp ), ldvt )
           ktemp = 2_ilp + ctot( 1_ilp ) + ctot( 2_ilp )
           if( ktemp<=ldvt2 )call stdlib_dgemm( 'N', 'N', k, nlp1, ctot( 3_ilp ), one, q( 1_ilp, ktemp ),&
                     ldq, vt2( ktemp, 1_ilp ), ldvt2, one, vt( 1_ilp, 1_ilp ),ldvt )
           ktemp = ctot( 1_ilp ) + 1_ilp
           nrp1 = nr + sqre
           if( ktemp>1_ilp ) then
              do i = 1, k
                 q( i, ktemp ) = q( i, 1_ilp )
              end do
              do i = nlp2, m
                 vt2( ktemp, i ) = vt2( 1_ilp, i )
              end do
           end if
           ctemp = 1_ilp + ctot( 2_ilp ) + ctot( 3_ilp )
           call stdlib_dgemm( 'N', 'N', k, nrp1, ctemp, one, q( 1_ilp, ktemp ), ldq,vt2( ktemp, nlp2 )&
                     , ldvt2, zero, vt( 1_ilp, nlp2 ), ldvt )
           return
     end subroutine stdlib_dlasd3




     pure module subroutine stdlib_slasd4( n, i, d, z, delta, rho, sigma, work, info )
     !! This subroutine computes the square root of the I-th updated
     !! eigenvalue of a positive symmetric rank-one modification to
     !! a positive diagonal matrix whose entries are given as the squares
     !! of the corresponding entries in the array d, and that
     !! 0 <= D(i) < D(j)  for  i < j
     !! and that RHO > 0. This is arranged by the calling routine, and is
     !! no loss in generality.  The rank-one modified system is thus
     !! diag( D ) * diag( D ) +  RHO * Z * Z_transpose.
     !! where we assume the Euclidean norm of Z is 1.
     !! The method consists of approximating the rational functions in the
     !! secular equation by simpler interpolating rational functions.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: i, n
           integer(ilp), intent(out) :: info
           real(sp), intent(in) :: rho
           real(sp), intent(out) :: sigma
           ! Array Arguments 
           real(sp), intent(in) :: d(*), z(*)
           real(sp), intent(out) :: delta(*), work(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: maxit = 400_ilp
           
           
           ! Local Scalars 
           logical(lk) :: orgati, swtch, swtch3, geomavg
           integer(ilp) :: ii, iim1, iip1, ip1, iter, j, niter
           real(sp) :: a, b, c, delsq, delsq2, sq2, dphi, dpsi, dtiim, dtiip, dtipsq, dtisq, &
           dtnsq, dtnsq1, dw, eps, erretm, eta, phi, prew, psi, rhoinv, sglb, sgub, tau, tau2, &
                     temp, temp1, temp2, w
           ! Local Arrays 
           real(sp) :: dd(3_ilp), zz(3_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! since this routine is called in an inner loop, we do no argument
           ! checking.
           ! quick return for n=1 and 2.
           info = 0_ilp
           if( n==1_ilp ) then
              ! presumably, i=1 upon entry
              sigma = sqrt( d( 1_ilp )*d( 1_ilp )+rho*z( 1_ilp )*z( 1_ilp ) )
              delta( 1_ilp ) = one
              work( 1_ilp ) = one
              return
           end if
           if( n==2_ilp ) then
              call stdlib_slasd5( i, d, z, delta, rho, sigma, work )
              return
           end if
           ! compute machine epsilon
           eps = stdlib_slamch( 'EPSILON' )
           rhoinv = one / rho
           tau2= zero
           ! the case i = n
           if( i==n ) then
              ! initialize some basic variables
              ii = n - 1_ilp
              niter = 1_ilp
              ! calculate initial guess
              temp = rho / two
              ! if ||z||_2 is not one, then temp should be set to
              ! rho * ||z||_2^2 / two
              temp1 = temp / ( d( n )+sqrt( d( n )*d( n )+temp ) )
              do j = 1, n
                 work( j ) = d( j ) + d( n ) + temp1
                 delta( j ) = ( d( j )-d( n ) ) - temp1
              end do
              psi = zero
              do j = 1, n - 2
                 psi = psi + z( j )*z( j ) / ( delta( j )*work( j ) )
              end do
              c = rhoinv + psi
              w = c + z( ii )*z( ii ) / ( delta( ii )*work( ii ) ) +z( n )*z( n ) / ( delta( n )&
                        *work( n ) )
              if( w<=zero ) then
                 temp1 = sqrt( d( n )*d( n )+rho )
                 temp = z( n-1 )*z( n-1 ) / ( ( d( n-1 )+temp1 )*( d( n )-d( n-1 )+rho / ( d( n )+&
                           temp1 ) ) ) +z( n )*z( n ) / rho
                 ! the following tau2 is to approximate
                 ! sigma_n^2 - d( n )*d( n )
                 if( c<=temp ) then
                    tau = rho
                 else
                    delsq = ( d( n )-d( n-1 ) )*( d( n )+d( n-1 ) )
                    a = -c*delsq + z( n-1 )*z( n-1 ) + z( n )*z( n )
                    b = z( n )*z( n )*delsq
                    if( a<zero ) then
                       tau2 = two*b / ( sqrt( a*a+four*b*c )-a )
                    else
                       tau2 = ( a+sqrt( a*a+four*b*c ) ) / ( two*c )
                    end if
                    tau = tau2 / ( d( n )+sqrt( d( n )*d( n )+tau2 ) )
                 end if
                 ! it can be proved that
                     ! d(n)^2+rho/2 <= sigma_n^2 < d(n)^2+tau2 <= d(n)^2+rho
              else
                 delsq = ( d( n )-d( n-1 ) )*( d( n )+d( n-1 ) )
                 a = -c*delsq + z( n-1 )*z( n-1 ) + z( n )*z( n )
                 b = z( n )*z( n )*delsq
                 ! the following tau2 is to approximate
                 ! sigma_n^2 - d( n )*d( n )
                 if( a<zero ) then
                    tau2 = two*b / ( sqrt( a*a+four*b*c )-a )
                 else
                    tau2 = ( a+sqrt( a*a+four*b*c ) ) / ( two*c )
                 end if
                 tau = tau2 / ( d( n )+sqrt( d( n )*d( n )+tau2 ) )
                 ! it can be proved that
                 ! d(n)^2 < d(n)^2+tau2 < sigma(n)^2 < d(n)^2+rho/2
              end if
              ! the following tau is to approximate sigma_n - d( n )
               ! tau = tau2 / ( d( n )+sqrt( d( n )*d( n )+tau2 ) )
              sigma = d( n ) + tau
              do j = 1, n
                 delta( j ) = ( d( j )-d( n ) ) - tau
                 work( j ) = d( j ) + d( n ) + tau
              end do
              ! evaluate psi and the derivative dpsi
              dpsi = zero
              psi = zero
              erretm = zero
              do j = 1, ii
                 temp = z( j ) / ( delta( j )*work( j ) )
                 psi = psi + z( j )*temp
                 dpsi = dpsi + temp*temp
                 erretm = erretm + psi
              end do
              erretm = abs( erretm )
              ! evaluate phi and the derivative dphi
              temp = z( n ) / ( delta( n )*work( n ) )
              phi = z( n )*temp
              dphi = temp*temp
              erretm = eight*( -phi-psi ) + erretm - phi + rhoinv
          ! $          + abs( tau2 )*( dpsi+dphi )
              w = rhoinv + phi + psi
              ! test for convergence
              if( abs( w )<=eps*erretm ) then
                 go to 240
              end if
              ! calculate the new step
              niter = niter + 1_ilp
              dtnsq1 = work( n-1 )*delta( n-1 )
              dtnsq = work( n )*delta( n )
              c = w - dtnsq1*dpsi - dtnsq*dphi
              a = ( dtnsq+dtnsq1 )*w - dtnsq*dtnsq1*( dpsi+dphi )
              b = dtnsq*dtnsq1*w
              if( c<zero )c = abs( c )
              if( c==zero ) then
                 eta = rho - sigma*sigma
              else if( a>=zero ) then
                 eta = ( a+sqrt( abs( a*a-four*b*c ) ) ) / ( two*c )
              else
                 eta = two*b / ( a-sqrt( abs( a*a-four*b*c ) ) )
              end if
              ! note, eta should be positive if w is negative, and
              ! eta should be negative otherwise. however,
              ! if for some reason caused by roundoff, eta*w > 0,
              ! we simply use one newton step instead. this way
              ! will guarantee eta*w < 0.
              if( w*eta>zero )eta = -w / ( dpsi+dphi )
              temp = eta - dtnsq
              if( temp>rho )eta = rho + dtnsq
              eta = eta / ( sigma+sqrt( eta+sigma*sigma ) )
              tau = tau + eta
              sigma = sigma + eta
              do j = 1, n
                 delta( j ) = delta( j ) - eta
                 work( j ) = work( j ) + eta
              end do
              ! evaluate psi and the derivative dpsi
              dpsi = zero
              psi = zero
              erretm = zero
              do j = 1, ii
                 temp = z( j ) / ( work( j )*delta( j ) )
                 psi = psi + z( j )*temp
                 dpsi = dpsi + temp*temp
                 erretm = erretm + psi
              end do
              erretm = abs( erretm )
              ! evaluate phi and the derivative dphi
              tau2 = work( n )*delta( n )
              temp = z( n ) / tau2
              phi = z( n )*temp
              dphi = temp*temp
              erretm = eight*( -phi-psi ) + erretm - phi + rhoinv
          ! $          + abs( tau2 )*( dpsi+dphi )
              w = rhoinv + phi + psi
              ! main loop to update the values of the array   delta
              iter = niter + 1_ilp
              loop_90: do niter = iter, maxit
                 ! test for convergence
                 if( abs( w )<=eps*erretm ) then
                    go to 240
                 end if
                 ! calculate the new step
                 dtnsq1 = work( n-1 )*delta( n-1 )
                 dtnsq = work( n )*delta( n )
                 c = w - dtnsq1*dpsi - dtnsq*dphi
                 a = ( dtnsq+dtnsq1 )*w - dtnsq1*dtnsq*( dpsi+dphi )
                 b = dtnsq1*dtnsq*w
                 if( a>=zero ) then
                    eta = ( a+sqrt( abs( a*a-four*b*c ) ) ) / ( two*c )
                 else
                    eta = two*b / ( a-sqrt( abs( a*a-four*b*c ) ) )
                 end if
                 ! note, eta should be positive if w is negative, and
                 ! eta should be negative otherwise. however,
                 ! if for some reason caused by roundoff, eta*w > 0,
                 ! we simply use one newton step instead. this way
                 ! will guarantee eta*w < 0.
                 if( w*eta>zero )eta = -w / ( dpsi+dphi )
                 temp = eta - dtnsq
                 if( temp<=zero )eta = eta / two
                 eta = eta / ( sigma+sqrt( eta+sigma*sigma ) )
                 tau = tau + eta
                 sigma = sigma + eta
                 do j = 1, n
                    delta( j ) = delta( j ) - eta
                    work( j ) = work( j ) + eta
                 end do
                 ! evaluate psi and the derivative dpsi
                 dpsi = zero
                 psi = zero
                 erretm = zero
                 do j = 1, ii
                    temp = z( j ) / ( work( j )*delta( j ) )
                    psi = psi + z( j )*temp
                    dpsi = dpsi + temp*temp
                    erretm = erretm + psi
                 end do
                 erretm = abs( erretm )
                 ! evaluate phi and the derivative dphi
                 tau2 = work( n )*delta( n )
                 temp = z( n ) / tau2
                 phi = z( n )*temp
                 dphi = temp*temp
                 erretm = eight*( -phi-psi ) + erretm - phi + rhoinv
          ! $             + abs( tau2 )*( dpsi+dphi )
                 w = rhoinv + phi + psi
              end do loop_90
              ! return with info = 1, niter = maxit and not converged
              info = 1_ilp
              go to 240
              ! end for the case i = n
           else
              ! the case for i < n
              niter = 1_ilp
              ip1 = i + 1_ilp
              ! calculate initial guess
              delsq = ( d( ip1 )-d( i ) )*( d( ip1 )+d( i ) )
              delsq2 = delsq / two
              sq2=sqrt( ( d( i )*d( i )+d( ip1 )*d( ip1 ) ) / two )
              temp = delsq2 / ( d( i )+sq2 )
              do j = 1, n
                 work( j ) = d( j ) + d( i ) + temp
                 delta( j ) = ( d( j )-d( i ) ) - temp
              end do
              psi = zero
              do j = 1, i - 1
                 psi = psi + z( j )*z( j ) / ( work( j )*delta( j ) )
              end do
              phi = zero
              do j = n, i + 2, -1
                 phi = phi + z( j )*z( j ) / ( work( j )*delta( j ) )
              end do
              c = rhoinv + psi + phi
              w = c + z( i )*z( i ) / ( work( i )*delta( i ) ) +z( ip1 )*z( ip1 ) / ( work( ip1 )&
                        *delta( ip1 ) )
              geomavg = .false.
              if( w>zero ) then
                 ! d(i)^2 < the ith sigma^2 < (d(i)^2+d(i+1)^2)/2
                 ! we choose d(i) as origin.
                 orgati = .true.
                 ii = i
                 sglb = zero
                 sgub = delsq2  / ( d( i )+sq2 )
                 a = c*delsq + z( i )*z( i ) + z( ip1 )*z( ip1 )
                 b = z( i )*z( i )*delsq
                 if( a>zero ) then
                    tau2 = two*b / ( a+sqrt( abs( a*a-four*b*c ) ) )
                 else
                    tau2 = ( a-sqrt( abs( a*a-four*b*c ) ) ) / ( two*c )
                 end if
                 ! tau2 now is an estimation of sigma^2 - d( i )^2. the
                 ! following, however, is the corresponding estimation of
                 ! sigma - d( i ).
                 tau = tau2 / ( d( i )+sqrt( d( i )*d( i )+tau2 ) )
                 temp = sqrt(eps)
                 if( (d(i)<=temp*d(ip1)).and.(abs(z(i))<=temp).and.(d(i)>zero) ) then
                    tau = min( ten*d(i), sgub )
                    geomavg = .true.
                 end if
              else
                 ! (d(i)^2+d(i+1)^2)/2 <= the ith sigma^2 < d(i+1)^2/2
                 ! we choose d(i+1) as origin.
                 orgati = .false.
                 ii = ip1
                 sglb = -delsq2  / ( d( ii )+sq2 )
                 sgub = zero
                 a = c*delsq - z( i )*z( i ) - z( ip1 )*z( ip1 )
                 b = z( ip1 )*z( ip1 )*delsq
                 if( a<zero ) then
                    tau2 = two*b / ( a-sqrt( abs( a*a+four*b*c ) ) )
                 else
                    tau2 = -( a+sqrt( abs( a*a+four*b*c ) ) ) / ( two*c )
                 end if
                 ! tau2 now is an estimation of sigma^2 - d( ip1 )^2. the
                 ! following, however, is the corresponding estimation of
                 ! sigma - d( ip1 ).
                 tau = tau2 / ( d( ip1 )+sqrt( abs( d( ip1 )*d( ip1 )+tau2 ) ) )
              end if
              sigma = d( ii ) + tau
              do j = 1, n
                 work( j ) = d( j ) + d( ii ) + tau
                 delta( j ) = ( d( j )-d( ii ) ) - tau
              end do
              iim1 = ii - 1_ilp
              iip1 = ii + 1_ilp
              ! evaluate psi and the derivative dpsi
              dpsi = zero
              psi = zero
              erretm = zero
              do j = 1, iim1
                 temp = z( j ) / ( work( j )*delta( j ) )
                 psi = psi + z( j )*temp
                 dpsi = dpsi + temp*temp
                 erretm = erretm + psi
              end do
              erretm = abs( erretm )
              ! evaluate phi and the derivative dphi
              dphi = zero
              phi = zero
              do j = n, iip1, -1
                 temp = z( j ) / ( work( j )*delta( j ) )
                 phi = phi + z( j )*temp
                 dphi = dphi + temp*temp
                 erretm = erretm + phi
              end do
              w = rhoinv + phi + psi
              ! w is the value of the secular function with
              ! its ii-th element removed.
              swtch3 = .false.
              if( orgati ) then
                 if( w<zero )swtch3 = .true.
              else
                 if( w>zero )swtch3 = .true.
              end if
              if( ii==1_ilp .or. ii==n )swtch3 = .false.
              temp = z( ii ) / ( work( ii )*delta( ii ) )
              dw = dpsi + dphi + temp*temp
              temp = z( ii )*temp
              w = w + temp
              erretm = eight*( phi-psi ) + erretm + two*rhoinv+ three*abs( temp )
          ! $          + abs( tau2 )*dw
              ! test for convergence
              if( abs( w )<=eps*erretm ) then
                 go to 240
              end if
              if( w<=zero ) then
                 sglb = max( sglb, tau )
              else
                 sgub = min( sgub, tau )
              end if
              ! calculate the new step
              niter = niter + 1_ilp
              if( .not.swtch3 ) then
                 dtipsq = work( ip1 )*delta( ip1 )
                 dtisq = work( i )*delta( i )
                 if( orgati ) then
                    c = w - dtipsq*dw + delsq*( z( i ) / dtisq )**2_ilp
                 else
                    c = w - dtisq*dw - delsq*( z( ip1 ) / dtipsq )**2_ilp
                 end if
                 a = ( dtipsq+dtisq )*w - dtipsq*dtisq*dw
                 b = dtipsq*dtisq*w
                 if( c==zero ) then
                    if( a==zero ) then
                       if( orgati ) then
                          a = z( i )*z( i ) + dtipsq*dtipsq*( dpsi+dphi )
                       else
                          a = z( ip1 )*z( ip1 ) + dtisq*dtisq*( dpsi+dphi )
                       end if
                    end if
                    eta = b / a
                 else if( a<=zero ) then
                    eta = ( a-sqrt( abs( a*a-four*b*c ) ) ) / ( two*c )
                 else
                    eta = two*b / ( a+sqrt( abs( a*a-four*b*c ) ) )
                 end if
              else
                 ! interpolation using three most relevant poles
                 dtiim = work( iim1 )*delta( iim1 )
                 dtiip = work( iip1 )*delta( iip1 )
                 temp = rhoinv + psi + phi
                 if( orgati ) then
                    temp1 = z( iim1 ) / dtiim
                    temp1 = temp1*temp1
                    c = ( temp - dtiip*( dpsi+dphi ) ) -( d( iim1 )-d( iip1 ) )*( d( iim1 )+d( &
                              iip1 ) )*temp1
                    zz( 1_ilp ) = z( iim1 )*z( iim1 )
                    if( dpsi<temp1 ) then
                       zz( 3_ilp ) = dtiip*dtiip*dphi
                    else
                       zz( 3_ilp ) = dtiip*dtiip*( ( dpsi-temp1 )+dphi )
                    end if
                 else
                    temp1 = z( iip1 ) / dtiip
                    temp1 = temp1*temp1
                    c = ( temp - dtiim*( dpsi+dphi ) ) -( d( iip1 )-d( iim1 ) )*( d( iim1 )+d( &
                              iip1 ) )*temp1
                    if( dphi<temp1 ) then
                       zz( 1_ilp ) = dtiim*dtiim*dpsi
                    else
                       zz( 1_ilp ) = dtiim*dtiim*( dpsi+( dphi-temp1 ) )
                    end if
                    zz( 3_ilp ) = z( iip1 )*z( iip1 )
                 end if
                 zz( 2_ilp ) = z( ii )*z( ii )
                 dd( 1_ilp ) = dtiim
                 dd( 2_ilp ) = delta( ii )*work( ii )
                 dd( 3_ilp ) = dtiip
                 call stdlib_slaed6( niter, orgati, c, dd, zz, w, eta, info )
                 if( info/=0_ilp ) then
                    ! if info is not 0, i.e., stdlib_slaed6 failed, switch back
                    ! to 2 pole interpolation.
                    swtch3 = .false.
                    info = 0_ilp
                    dtipsq = work( ip1 )*delta( ip1 )
                    dtisq = work( i )*delta( i )
                    if( orgati ) then
                       c = w - dtipsq*dw + delsq*( z( i ) / dtisq )**2_ilp
                    else
                       c = w - dtisq*dw - delsq*( z( ip1 ) / dtipsq )**2_ilp
                    end if
                    a = ( dtipsq+dtisq )*w - dtipsq*dtisq*dw
                    b = dtipsq*dtisq*w
                    if( c==zero ) then
                       if( a==zero ) then
                          if( orgati ) then
                             a = z( i )*z( i ) + dtipsq*dtipsq*( dpsi+dphi )
                          else
                             a = z( ip1 )*z( ip1 ) + dtisq*dtisq*( dpsi+dphi)
                          end if
                       end if
                       eta = b / a
                    else if( a<=zero ) then
                       eta = ( a-sqrt( abs( a*a-four*b*c ) ) ) / ( two*c )
                    else
                       eta = two*b / ( a+sqrt( abs( a*a-four*b*c ) ) )
                    end if
                 end if
              end if
              ! note, eta should be positive if w is negative, and
              ! eta should be negative otherwise. however,
              ! if for some reason caused by roundoff, eta*w > 0,
              ! we simply use one newton step instead. this way
              ! will guarantee eta*w < 0.
              if( w*eta>=zero )eta = -w / dw
              eta = eta / ( sigma+sqrt( sigma*sigma+eta ) )
              temp = tau + eta
              if( temp>sgub .or. temp<sglb ) then
                 if( w<zero ) then
                    eta = ( sgub-tau ) / two
                 else
                    eta = ( sglb-tau ) / two
                 end if
                 if( geomavg ) then
                    if( w < zero ) then
                       if( tau > zero ) then
                          eta = sqrt(sgub*tau)-tau
                       end if
                    else
                       if( sglb > zero ) then
                          eta = sqrt(sglb*tau)-tau
                       end if
                    end if
                 end if
              end if
              prew = w
              tau = tau + eta
              sigma = sigma + eta
              do j = 1, n
                 work( j ) = work( j ) + eta
                 delta( j ) = delta( j ) - eta
              end do
              ! evaluate psi and the derivative dpsi
              dpsi = zero
              psi = zero
              erretm = zero
              do j = 1, iim1
                 temp = z( j ) / ( work( j )*delta( j ) )
                 psi = psi + z( j )*temp
                 dpsi = dpsi + temp*temp
                 erretm = erretm + psi
              end do
              erretm = abs( erretm )
              ! evaluate phi and the derivative dphi
              dphi = zero
              phi = zero
              do j = n, iip1, -1
                 temp = z( j ) / ( work( j )*delta( j ) )
                 phi = phi + z( j )*temp
                 dphi = dphi + temp*temp
                 erretm = erretm + phi
              end do
              tau2 = work( ii )*delta( ii )
              temp = z( ii ) / tau2
              dw = dpsi + dphi + temp*temp
              temp = z( ii )*temp
              w = rhoinv + phi + psi + temp
              erretm = eight*( phi-psi ) + erretm + two*rhoinv+ three*abs( temp )
          ! $          + abs( tau2 )*dw
              swtch = .false.
              if( orgati ) then
                 if( -w>abs( prew ) / ten )swtch = .true.
              else
                 if( w>abs( prew ) / ten )swtch = .true.
              end if
              ! main loop to update the values of the array   delta and work
              iter = niter + 1_ilp
              loop_230: do niter = iter, maxit
                 ! test for convergence
                 if( abs( w )<=eps*erretm ) then
           ! $          .or. (sgub-sglb)<=eight*abs(sgub+sglb) ) then
                    go to 240
                 end if
                 if( w<=zero ) then
                    sglb = max( sglb, tau )
                 else
                    sgub = min( sgub, tau )
                 end if
                 ! calculate the new step
                 if( .not.swtch3 ) then
                    dtipsq = work( ip1 )*delta( ip1 )
                    dtisq = work( i )*delta( i )
                    if( .not.swtch ) then
                       if( orgati ) then
                          c = w - dtipsq*dw + delsq*( z( i ) / dtisq )**2_ilp
                       else
                          c = w - dtisq*dw - delsq*( z( ip1 ) / dtipsq )**2_ilp
                       end if
                    else
                       temp = z( ii ) / ( work( ii )*delta( ii ) )
                       if( orgati ) then
                          dpsi = dpsi + temp*temp
                       else
                          dphi = dphi + temp*temp
                       end if
                       c = w - dtisq*dpsi - dtipsq*dphi
                    end if
                    a = ( dtipsq+dtisq )*w - dtipsq*dtisq*dw
                    b = dtipsq*dtisq*w
                    if( c==zero ) then
                       if( a==zero ) then
                          if( .not.swtch ) then
                             if( orgati ) then
                                a = z( i )*z( i ) + dtipsq*dtipsq*( dpsi+dphi )
                             else
                                a = z( ip1 )*z( ip1 ) +dtisq*dtisq*( dpsi+dphi )
                             end if
                          else
                             a = dtisq*dtisq*dpsi + dtipsq*dtipsq*dphi
                          end if
                       end if
                       eta = b / a
                    else if( a<=zero ) then
                       eta = ( a-sqrt( abs( a*a-four*b*c ) ) ) / ( two*c )
                    else
                       eta = two*b / ( a+sqrt( abs( a*a-four*b*c ) ) )
                    end if
                 else
                    ! interpolation using three most relevant poles
                    dtiim = work( iim1 )*delta( iim1 )
                    dtiip = work( iip1 )*delta( iip1 )
                    temp = rhoinv + psi + phi
                    if( swtch ) then
                       c = temp - dtiim*dpsi - dtiip*dphi
                       zz( 1_ilp ) = dtiim*dtiim*dpsi
                       zz( 3_ilp ) = dtiip*dtiip*dphi
                    else
                       if( orgati ) then
                          temp1 = z( iim1 ) / dtiim
                          temp1 = temp1*temp1
                          temp2 = ( d( iim1 )-d( iip1 ) )*( d( iim1 )+d( iip1 ) )*temp1
                          c = temp - dtiip*( dpsi+dphi ) - temp2
                          zz( 1_ilp ) = z( iim1 )*z( iim1 )
                          if( dpsi<temp1 ) then
                             zz( 3_ilp ) = dtiip*dtiip*dphi
                          else
                             zz( 3_ilp ) = dtiip*dtiip*( ( dpsi-temp1 )+dphi )
                          end if
                       else
                          temp1 = z( iip1 ) / dtiip
                          temp1 = temp1*temp1
                          temp2 = ( d( iip1 )-d( iim1 ) )*( d( iim1 )+d( iip1 ) )*temp1
                          c = temp - dtiim*( dpsi+dphi ) - temp2
                          if( dphi<temp1 ) then
                             zz( 1_ilp ) = dtiim*dtiim*dpsi
                          else
                             zz( 1_ilp ) = dtiim*dtiim*( dpsi+( dphi-temp1 ) )
                          end if
                          zz( 3_ilp ) = z( iip1 )*z( iip1 )
                       end if
                    end if
                    dd( 1_ilp ) = dtiim
                    dd( 2_ilp ) = delta( ii )*work( ii )
                    dd( 3_ilp ) = dtiip
                    call stdlib_slaed6( niter, orgati, c, dd, zz, w, eta, info )
                    if( info/=0_ilp ) then
                       ! if info is not 0, i.e., stdlib_slaed6 failed, switch
                       ! back to two pole interpolation
                       swtch3 = .false.
                       info = 0_ilp
                       dtipsq = work( ip1 )*delta( ip1 )
                       dtisq = work( i )*delta( i )
                       if( .not.swtch ) then
                          if( orgati ) then
                             c = w - dtipsq*dw + delsq*( z( i )/dtisq )**2_ilp
                          else
                             c = w - dtisq*dw - delsq*( z( ip1 )/dtipsq )**2_ilp
                          end if
                       else
                          temp = z( ii ) / ( work( ii )*delta( ii ) )
                          if( orgati ) then
                             dpsi = dpsi + temp*temp
                          else
                             dphi = dphi + temp*temp
                          end if
                          c = w - dtisq*dpsi - dtipsq*dphi
                       end if
                       a = ( dtipsq+dtisq )*w - dtipsq*dtisq*dw
                       b = dtipsq*dtisq*w
                       if( c==zero ) then
                          if( a==zero ) then
                             if( .not.swtch ) then
                                if( orgati ) then
                                   a = z( i )*z( i ) + dtipsq*dtipsq*( dpsi+dphi )
                                else
                                   a = z( ip1 )*z( ip1 ) +dtisq*dtisq*( dpsi+dphi )
                                end if
                             else
                                a = dtisq*dtisq*dpsi + dtipsq*dtipsq*dphi
                             end if
                          end if
                          eta = b / a
                       else if( a<=zero ) then
                          eta = ( a-sqrt( abs( a*a-four*b*c ) ) ) / ( two*c )
                       else
                          eta = two*b / ( a+sqrt( abs( a*a-four*b*c ) ) )
                       end if
                    end if
                 end if
                 ! note, eta should be positive if w is negative, and
                 ! eta should be negative otherwise. however,
                 ! if for some reason caused by roundoff, eta*w > 0,
                 ! we simply use one newton step instead. this way
                 ! will guarantee eta*w < 0.
                 if( w*eta>=zero )eta = -w / dw
                 eta = eta / ( sigma+sqrt( sigma*sigma+eta ) )
                 temp=tau+eta
                 if( temp>sgub .or. temp<sglb ) then
                    if( w<zero ) then
                       eta = ( sgub-tau ) / two
                    else
                       eta = ( sglb-tau ) / two
                    end if
                    if( geomavg ) then
                       if( w < zero ) then
                          if( tau > zero ) then
                             eta = sqrt(sgub*tau)-tau
                          end if
                       else
                          if( sglb > zero ) then
                             eta = sqrt(sglb*tau)-tau
                          end if
                       end if
                    end if
                 end if
                 prew = w
                 tau = tau + eta
                 sigma = sigma + eta
                 do j = 1, n
                    work( j ) = work( j ) + eta
                    delta( j ) = delta( j ) - eta
                 end do
                 ! evaluate psi and the derivative dpsi
                 dpsi = zero
                 psi = zero
                 erretm = zero
                 do j = 1, iim1
                    temp = z( j ) / ( work( j )*delta( j ) )
                    psi = psi + z( j )*temp
                    dpsi = dpsi + temp*temp
                    erretm = erretm + psi
                 end do
                 erretm = abs( erretm )
                 ! evaluate phi and the derivative dphi
                 dphi = zero
                 phi = zero
                 do j = n, iip1, -1
                    temp = z( j ) / ( work( j )*delta( j ) )
                    phi = phi + z( j )*temp
                    dphi = dphi + temp*temp
                    erretm = erretm + phi
                 end do
                 tau2 = work( ii )*delta( ii )
                 temp = z( ii ) / tau2
                 dw = dpsi + dphi + temp*temp
                 temp = z( ii )*temp
                 w = rhoinv + phi + psi + temp
                 erretm = eight*( phi-psi ) + erretm + two*rhoinv+ three*abs( temp )
          ! $             + abs( tau2 )*dw
                 if( w*prew>zero .and. abs( w )>abs( prew ) / ten )swtch = .not.swtch
              end do loop_230
              ! return with info = 1, niter = maxit and not converged
              info = 1_ilp
           end if
           240 continue
           return
     end subroutine stdlib_slasd4

     pure module subroutine stdlib_dlasd4( n, i, d, z, delta, rho, sigma, work, info )
     !! This subroutine computes the square root of the I-th updated
     !! eigenvalue of a positive symmetric rank-one modification to
     !! a positive diagonal matrix whose entries are given as the squares
     !! of the corresponding entries in the array d, and that
     !! 0 <= D(i) < D(j)  for  i < j
     !! and that RHO > 0. This is arranged by the calling routine, and is
     !! no loss in generality.  The rank-one modified system is thus
     !! diag( D ) * diag( D ) +  RHO * Z * Z_transpose.
     !! where we assume the Euclidean norm of Z is 1.
     !! The method consists of approximating the rational functions in the
     !! secular equation by simpler interpolating rational functions.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: i, n
           integer(ilp), intent(out) :: info
           real(dp), intent(in) :: rho
           real(dp), intent(out) :: sigma
           ! Array Arguments 
           real(dp), intent(in) :: d(*), z(*)
           real(dp), intent(out) :: delta(*), work(*)
        ! =====================================================================
           ! Parameters 
           integer(ilp), parameter :: maxit = 400_ilp
           
           
           ! Local Scalars 
           logical(lk) :: orgati, swtch, swtch3, geomavg
           integer(ilp) :: ii, iim1, iip1, ip1, iter, j, niter
           real(dp) :: a, b, c, delsq, delsq2, sq2, dphi, dpsi, dtiim, dtiip, dtipsq, dtisq, &
           dtnsq, dtnsq1, dw, eps, erretm, eta, phi, prew, psi, rhoinv, sglb, sgub, tau, tau2, &
                     temp, temp1, temp2, w
           ! Local Arrays 
           real(dp) :: dd(3_ilp), zz(3_ilp)
           ! Intrinsic Functions 
           ! Executable Statements 
           ! since this routine is called in an inner loop, we do no argument
           ! checking.
           ! quick return for n=1 and 2.
           info = 0_ilp
           if( n==1_ilp ) then
              ! presumably, i=1 upon entry
              sigma = sqrt( d( 1_ilp )*d( 1_ilp )+rho*z( 1_ilp )*z( 1_ilp ) )
              delta( 1_ilp ) = one
              work( 1_ilp ) = one
              return
           end if
           if( n==2_ilp ) then
              call stdlib_dlasd5( i, d, z, delta, rho, sigma, work )
              return
           end if
           ! compute machine epsilon
           eps = stdlib_dlamch( 'EPSILON' )
           rhoinv = one / rho
           tau2= zero
           ! the case i = n
           if( i==n ) then
              ! initialize some basic variables
              ii = n - 1_ilp
              niter = 1_ilp
              ! calculate initial guess
              temp = rho / two
              ! if ||z||_2 is not one, then temp should be set to
              ! rho * ||z||_2^2 / two
              temp1 = temp / ( d( n )+sqrt( d( n )*d( n )+temp ) )
              do j = 1, n
                 work( j ) = d( j ) + d( n ) + temp1
                 delta( j ) = ( d( j )-d( n ) ) - temp1
              end do
              psi = zero
              do j = 1, n - 2
                 psi = psi + z( j )*z( j ) / ( delta( j )*work( j ) )
              end do
              c = rhoinv + psi
              w = c + z( ii )*z( ii ) / ( delta( ii )*work( ii ) ) +z( n )*z( n ) / ( delta( n )&
                        *work( n ) )
              if( w<=zero ) then
                 temp1 = sqrt( d( n )*d( n )+rho )
                 temp = z( n-1 )*z( n-1 ) / ( ( d( n-1 )+temp1 )*( d( n )-d( n-1 )+rho / ( d( n )+&
                           temp1 ) ) ) +z( n )*z( n ) / rho
                 ! the following tau2 is to approximate
                 ! sigma_n^2 - d( n )*d( n )
                 if( c<=temp ) then
                    tau = rho
                 else
                    delsq = ( d( n )-d( n-1 ) )*( d( n )+d( n-1 ) )
                    a = -c*delsq + z( n-1 )*z( n-1 ) + z( n )*z( n )
                    b = z( n )*z( n )*delsq
                    if( a<zero ) then
                       tau2 = two*b / ( sqrt( a*a+four*b*c )-a )
                    else
                       tau2 = ( a+sqrt( a*a+four*b*c ) ) / ( two*c )
                    end if
                    tau = tau2 / ( d( n )+sqrt( d( n )*d( n )+tau2 ) )
                 end if
                 ! it can be proved that
                     ! d(n)^2+rho/2 <= sigma_n^2 < d(n)^2+tau2 <= d(n)^2+rho
              else
                 delsq = ( d( n )-d( n-1 ) )*( d( n )+d( n-1 ) )
                 a = -c*delsq + z( n-1 )*z( n-1 ) + z( n )*z( n )
                 b = z( n )*z( n )*delsq
                 ! the following tau2 is to approximate
                 ! sigma_n^2 - d( n )*d( n )
                 if( a<zero ) then
                    tau2 = two*b / ( sqrt( a*a+four*b*c )-a )
                 else
                    tau2 = ( a+sqrt( a*a+four*b*c ) ) / ( two*c )
                 end if
                 tau = tau2 / ( d( n )+sqrt( d( n )*d( n )+tau2 ) )
                 ! it can be proved that
                 ! d(n)^2 < d(n)^2+tau2 < sigma(n)^2 < d(n)^2+rho/2
              end if
              ! the following tau is to approximate sigma_n - d( n )
               ! tau = tau2 / ( d( n )+sqrt( d( n )*d( n )+tau2 ) )
              sigma = d( n ) + tau
              do j = 1, n
                 delta( j ) = ( d( j )-d( n ) ) - tau
                 work( j ) = d( j ) + d( n ) + tau
              end do
              ! evaluate psi and the derivative dpsi
              dpsi = zero
              psi = zero
              erretm = zero
              do j = 1, ii
                 temp = z( j ) / ( delta( j )*work( j ) )
                 psi = psi + z( j )*temp
                 dpsi = dpsi + temp*temp
                 erretm = erretm + psi
              end do
              erretm = abs( erretm )
              ! evaluate phi and the derivative dphi
              temp = z( n ) / ( delta( n )*work( n ) )
              phi = z( n )*temp
              dphi = temp*temp
              erretm = eight*( -phi-psi ) + erretm - phi + rhoinv
          ! $          + abs( tau2 )*( dpsi+dphi )
              w = rhoinv + phi + psi
              ! test for convergence
              if( abs( w )<=eps*erretm ) then
                 go to 240
              end if
              ! calculate the new step
              niter = niter + 1_ilp
              dtnsq1 = work( n-1 )*delta( n-1 )
              dtnsq = work( n )*delta( n )
              c = w - dtnsq1*dpsi - dtnsq*dphi
              a = ( dtnsq+dtnsq1 )*w - dtnsq*dtnsq1*( dpsi+dphi )
              b = dtnsq*dtnsq1*w
              if( c<zero )c = abs( c )
              if( c==zero ) then
                 eta = rho - sigma*sigma
              else if( a>=zero ) then
                 eta = ( a+sqrt( abs( a*a-four*b*c ) ) ) / ( two*c )
              else
                 eta = two*b / ( a-sqrt( abs( a*a-four*b*c ) ) )
              end if
              ! note, eta should be positive if w is negative, and
              ! eta should be negative otherwise. however,
              ! if for some reason caused by roundoff, eta*w > 0,
              ! we simply use one newton step instead. this way
              ! will guarantee eta*w < 0.
              if( w*eta>zero )eta = -w / ( dpsi+dphi )
              temp = eta - dtnsq
              if( temp>rho )eta = rho + dtnsq
              eta = eta / ( sigma+sqrt( eta+sigma*sigma ) )
              tau = tau + eta
              sigma = sigma + eta
              do j = 1, n
                 delta( j ) = delta( j ) - eta
                 work( j ) = work( j ) + eta
              end do
              ! evaluate psi and the derivative dpsi
              dpsi = zero
              psi = zero
              erretm = zero
              do j = 1, ii
                 temp = z( j ) / ( work( j )*delta( j ) )
                 psi = psi + z( j )*temp
                 dpsi = dpsi + temp*temp
                 erretm = erretm + psi
              end do
              erretm = abs( erretm )
              ! evaluate phi and the derivative dphi
              tau2 = work( n )*delta( n )
              temp = z( n ) / tau2
              phi = z( n )*temp
              dphi = temp*temp
              erretm = eight*( -phi-psi ) + erretm - phi + rhoinv
          ! $          + abs( tau2 )*( dpsi+dphi )
              w = rhoinv + phi + psi
              ! main loop to update the values of the array   delta
              iter = niter + 1_ilp
              loop_90: do niter = iter, maxit
                 ! test for convergence
                 if( abs( w )<=eps*erretm ) then
                    go to 240
                 end if
                 ! calculate the new step
                 dtnsq1 = work( n-1 )*delta( n-1 )
                 dtnsq = work( n )*delta( n )
                 c = w - dtnsq1*dpsi - dtnsq*dphi
                 a = ( dtnsq+dtnsq1 )*w - dtnsq1*dtnsq*( dpsi+dphi )
                 b = dtnsq1*dtnsq*w
                 if( a>=zero ) then
                    eta = ( a+sqrt( abs( a*a-four*b*c ) ) ) / ( two*c )
                 else
                    eta = two*b / ( a-sqrt( abs( a*a-four*b*c ) ) )
                 end if
                 ! note, eta should be positive if w is negative, and
                 ! eta should be negative otherwise. however,
                 ! if for some reason caused by roundoff, eta*w > 0,
                 ! we simply use one newton step instead. this way
                 ! will guarantee eta*w < 0.
                 if( w*eta>zero )eta = -w / ( dpsi+dphi )
                 temp = eta - dtnsq
                 if( temp<=zero )eta = eta / two
                 eta = eta / ( sigma+sqrt( eta+sigma*sigma ) )
                 tau = tau + eta
                 sigma = sigma + eta
                 do j = 1, n
                    delta( j ) = delta( j ) - eta
                    work( j ) = work( j ) + eta
                 end do
                 ! evaluate psi and the derivative dpsi
                 dpsi = zero
                 psi = zero
                 erretm = zero
                 do j = 1, ii
                    temp = z( j ) / ( work( j )*delta( j ) )
                    psi = psi + z( j )*temp
                    dpsi = dpsi + temp*temp
                    erretm = erretm + psi
                 end do
                 erretm = abs( erretm )
                 ! evaluate phi and the derivative dphi
                 tau2 = work( n )*delta( n )
                 temp = z( n ) / tau2
                 phi = z( n )*temp
                 dphi = temp*temp
                 erretm = eight*( -phi-psi ) + erretm - phi + rhoinv
          ! $             + abs( tau2 )*( dpsi+dphi )
                 w = rhoinv + phi + psi
              end do loop_90
              ! return with info = 1, niter = maxit and not converged
              info = 1_ilp
              go to 240
              ! end for the case i = n
           else
              ! the case for i < n
              niter = 1_ilp
              ip1 = i + 1_ilp
              ! calculate initial guess
              delsq = ( d( ip1 )-d( i ) )*( d( ip1 )+d( i ) )
              delsq2 = delsq / two
              sq2=sqrt( ( d( i )*d( i )+d( ip1 )*d( ip1 ) ) / two )
              temp = delsq2 / ( d( i )+sq2 )
              do j = 1, n
                 work( j ) = d( j ) + d( i ) + temp
                 delta( j ) = ( d( j )-d( i ) ) - temp
              end do
              psi = zero
              do j = 1, i - 1
                 psi = psi + z( j )*z( j ) / ( work( j )*delta( j ) )
              end do
              phi = zero
              do j = n, i + 2, -1
                 phi = phi + z( j )*z( j ) / ( work( j )*delta( j ) )
              end do
              c = rhoinv + psi + phi
              w = c + z( i )*z( i ) / ( work( i )*delta( i ) ) +z( ip1 )*z( ip1 ) / ( work( ip1 )&
                        *delta( ip1 ) )
              geomavg = .false.
              if( w>zero ) then
                 ! d(i)^2 < the ith sigma^2 < (d(i)^2+d(i+1)^2)/2
                 ! we choose d(i) as origin.
                 orgati = .true.
                 ii = i
                 sglb = zero
                 sgub = delsq2  / ( d( i )+sq2 )
                 a = c*delsq + z( i )*z( i ) + z( ip1 )*z( ip1 )
                 b = z( i )*z( i )*delsq
                 if( a>zero ) then
                    tau2 = two*b / ( a+sqrt( abs( a*a-four*b*c ) ) )
                 else
                    tau2 = ( a-sqrt( abs( a*a-four*b*c ) ) ) / ( two*c )
                 end if
                 ! tau2 now is an estimation of sigma^2 - d( i )^2. the
                 ! following, however, is the corresponding estimation of
                 ! sigma - d( i ).
                 tau = tau2 / ( d( i )+sqrt( d( i )*d( i )+tau2 ) )
                 temp = sqrt(eps)
                 if( (d(i)<=temp*d(ip1)).and.(abs(z(i))<=temp).and.(d(i)>zero) ) then
                    tau = min( ten*d(i), sgub )
                    geomavg = .true.
                 end if
              else
                 ! (d(i)^2+d(i+1)^2)/2 <= the ith sigma^2 < d(i+1)^2/2
                 ! we choose d(i+1) as origin.
                 orgati = .false.
                 ii = ip1
                 sglb = -delsq2  / ( d( ii )+sq2 )
                 sgub = zero
                 a = c*delsq - z( i )*z( i ) - z( ip1 )*z( ip1 )
                 b = z( ip1 )*z( ip1 )*delsq
                 if( a<zero ) then
                    tau2 = two*b / ( a-sqrt( abs( a*a+four*b*c ) ) )
                 else
                    tau2 = -( a+sqrt( abs( a*a+four*b*c ) ) ) / ( two*c )
                 end if
                 ! tau2 now is an estimation of sigma^2 - d( ip1 )^2. the
                 ! following, however, is the corresponding estimation of
                 ! sigma - d( ip1 ).
                 tau = tau2 / ( d( ip1 )+sqrt( abs( d( ip1 )*d( ip1 )+tau2 ) ) )
              end if
              sigma = d( ii ) + tau
              do j = 1, n
                 work( j ) = d( j ) + d( ii ) + tau
                 delta( j ) = ( d( j )-d( ii ) ) - tau
              end do
              iim1 = ii - 1_ilp
              iip1 = ii + 1_ilp
              ! evaluate psi and the derivative dpsi
              dpsi = zero
              psi = zero
              erretm = zero
              do j = 1, iim1
                 temp = z( j ) / ( work( j )*delta( j ) )
                 psi = psi + z( j )*temp
                 dpsi = dpsi + temp*temp
                 erretm = erretm + psi
              end do
              erretm = abs( erretm )
              ! evaluate phi and the derivative dphi
              dphi = zero
              phi = zero
              do j = n, iip1, -1
                 temp = z( j ) / ( work( j )*delta( j ) )
                 phi = phi + z( j )*temp
                 dphi = dphi + temp*temp
                 erretm = erretm + phi
              end do
              w = rhoinv + phi + psi
              ! w is the value of the secular function with
              ! its ii-th element removed.
              swtch3 = .false.
              if( orgati ) then
                 if( w<zero )swtch3 = .true.
              else
                 if( w>zero )swtch3 = .true.
              end if
              if( ii==1_ilp .or. ii==n )swtch3 = .false.
              temp = z( ii ) / ( work( ii )*delta( ii ) )
              dw = dpsi + dphi + temp*temp
              temp = z( ii )*temp
              w = w + temp
              erretm = eight*( phi-psi ) + erretm + two*rhoinv+ three*abs( temp )
          ! $          + abs( tau2 )*dw
              ! test for convergence
              if( abs( w )<=eps*erretm ) then
                 go to 240
              end if
              if( w<=zero ) then
                 sglb = max( sglb, tau )
              else
                 sgub = min( sgub, tau )
              end if
              ! calculate the new step
              niter = niter + 1_ilp
              if( .not.swtch3 ) then
                 dtipsq = work( ip1 )*delta( ip1 )
                 dtisq = work( i )*delta( i )
                 if( orgati ) then
                    c = w - dtipsq*dw + delsq*( z( i ) / dtisq )**2_ilp
                 else
                    c = w - dtisq*dw - delsq*( z( ip1 ) / dtipsq )**2_ilp
                 end if
                 a = ( dtipsq+dtisq )*w - dtipsq*dtisq*dw
                 b = dtipsq*dtisq*w
                 if( c==zero ) then
                    if( a==zero ) then
                       if( orgati ) then
                          a = z( i )*z( i ) + dtipsq*dtipsq*( dpsi+dphi )
                       else
                          a = z( ip1 )*z( ip1 ) + dtisq*dtisq*( dpsi+dphi )
                       end if
                    end if
                    eta = b / a
                 else if( a<=zero ) then
                    eta = ( a-sqrt( abs( a*a-four*b*c ) ) ) / ( two*c )
                 else
                    eta = two*b / ( a+sqrt( abs( a*a-four*b*c ) ) )
                 end if
              else
                 ! interpolation using three most relevant poles
                 dtiim = work( iim1 )*delta( iim1 )
                 dtiip = work( iip1 )*delta( iip1 )
                 temp = rhoinv + psi + phi
                 if( orgati ) then
                    temp1 = z( iim1 ) / dtiim
                    temp1 = temp1*temp1
                    c = ( temp - dtiip*( dpsi+dphi ) ) -( d( iim1 )-d( iip1 ) )*( d( iim1 )+d( &
                              iip1 ) )*temp1
                    zz( 1_ilp ) = z( iim1 )*z( iim1 )
                    if( dpsi<temp1 ) then
                       zz( 3_ilp ) = dtiip*dtiip*dphi
                    else
                       zz( 3_ilp ) = dtiip*dtiip*( ( dpsi-temp1 )+dphi )
                    end if
                 else
                    temp1 = z( iip1 ) / dtiip
                    temp1 = temp1*temp1
                    c = ( temp - dtiim*( dpsi+dphi ) ) -( d( iip1 )-d( iim1 ) )*( d( iim1 )+d( &
                              iip1 ) )*temp1
                    if( dphi<temp1 ) then
                       zz( 1_ilp ) = dtiim*dtiim*dpsi
                    else
                       zz( 1_ilp ) = dtiim*dtiim*( dpsi+( dphi-temp1 ) )
                    end if
                    zz( 3_ilp ) = z( iip1 )*z( iip1 )
                 end if
                 zz( 2_ilp ) = z( ii )*z( ii )
                 dd( 1_ilp ) = dtiim
                 dd( 2_ilp ) = delta( ii )*work( ii )
                 dd( 3_ilp ) = dtiip
                 call stdlib_dlaed6( niter, orgati, c, dd, zz, w, eta, info )
                 if( info/=0_ilp ) then
                    ! if info is not 0, i.e., stdlib_dlaed6 failed, switch back
                    ! to 2 pole interpolation.
                    swtch3 = .false.
                    info = 0_ilp
                    dtipsq = work( ip1 )*delta( ip1 )
                    dtisq = work( i )*delta( i )
                    if( orgati ) then
                       c = w - dtipsq*dw + delsq*( z( i ) / dtisq )**2_ilp
                    else
                       c = w - dtisq*dw - delsq*( z( ip1 ) / dtipsq )**2_ilp
                    end if
                    a = ( dtipsq+dtisq )*w - dtipsq*dtisq*dw
                    b = dtipsq*dtisq*w
                    if( c==zero ) then
                       if( a==zero ) then
                          if( orgati ) then
                             a = z( i )*z( i ) + dtipsq*dtipsq*( dpsi+dphi )
                          else
                             a = z( ip1 )*z( ip1 ) + dtisq*dtisq*( dpsi+dphi)
                          end if
                       end if
                       eta = b / a
                    else if( a<=zero ) then
                       eta = ( a-sqrt( abs( a*a-four*b*c ) ) ) / ( two*c )
                    else
                       eta = two*b / ( a+sqrt( abs( a*a-four*b*c ) ) )
                    end if
                 end if
              end if
              ! note, eta should be positive if w is negative, and
              ! eta should be negative otherwise. however,
              ! if for some reason caused by roundoff, eta*w > 0,
              ! we simply use one newton step instead. this way
              ! will guarantee eta*w < 0.
              if( w*eta>=zero )eta = -w / dw
              eta = eta / ( sigma+sqrt( sigma*sigma+eta ) )
              temp = tau + eta
              if( temp>sgub .or. temp<sglb ) then
                 if( w<zero ) then
                    eta = ( sgub-tau ) / two
                 else
                    eta = ( sglb-tau ) / two
                 end if
                 if( geomavg ) then
                    if( w < zero ) then
                       if( tau > zero ) then
                          eta = sqrt(sgub*tau)-tau
                       end if
                    else
                       if( sglb > zero ) then
                          eta = sqrt(sglb*tau)-tau
                       end if
                    end if
                 end if
              end if
              prew = w
              tau = tau + eta
              sigma = sigma + eta
              do j = 1, n
                 work( j ) = work( j ) + eta
                 delta( j ) = delta( j ) - eta
              end do
              ! evaluate psi and the derivative dpsi
              dpsi = zero
              psi = zero
              erretm = zero
              do j = 1, iim1
                 temp = z( j ) / ( work( j )*delta( j ) )
                 psi = psi + z( j )*temp
                 dpsi = dpsi + temp*temp
                 erretm = erretm + psi
              end do
              erretm = abs( erretm )
              ! evaluate phi and the derivative dphi
              dphi = zero
              phi = zero
              do j = n, iip1, -1
                 temp = z( j ) / ( work( j )*delta( j ) )
                 phi = phi + z( j )*temp
                 dphi = dphi + temp*temp
                 erretm = erretm + phi
              end do
              tau2 = work( ii )*delta( ii )
              temp = z( ii ) / tau2
              dw = dpsi + dphi + temp*temp
              temp = z( ii )*temp
              w = rhoinv + phi + psi + temp
              erretm = eight*( phi-psi ) + erretm + two*rhoinv+ three*abs( temp )
          ! $          + abs( tau2 )*dw
              swtch = .false.
              if( orgati ) then
                 if( -w>abs( prew ) / ten )swtch = .true.
              else
                 if( w>abs( prew ) / ten )swtch = .true.
              end if
              ! main loop to update the values of the array   delta and work
              iter = niter + 1_ilp
              loop_230: do niter = iter, maxit
                 ! test for convergence
                 if( abs( w )<=eps*erretm ) then
           ! $          .or. (sgub-sglb)<=eight*abs(sgub+sglb) ) then
                    go to 240
                 end if
                 if( w<=zero ) then
                    sglb = max( sglb, tau )
                 else
                    sgub = min( sgub, tau )
                 end if
                 ! calculate the new step
                 if( .not.swtch3 ) then
                    dtipsq = work( ip1 )*delta( ip1 )
                    dtisq = work( i )*delta( i )
                    if( .not.swtch ) then
                       if( orgati ) then
                          c = w - dtipsq*dw + delsq*( z( i ) / dtisq )**2_ilp
                       else
                          c = w - dtisq*dw - delsq*( z( ip1 ) / dtipsq )**2_ilp
                       end if
                    else
                       temp = z( ii ) / ( work( ii )*delta( ii ) )
                       if( orgati ) then
                          dpsi = dpsi + temp*temp
                       else
                          dphi = dphi + temp*temp
                       end if
                       c = w - dtisq*dpsi - dtipsq*dphi
                    end if
                    a = ( dtipsq+dtisq )*w - dtipsq*dtisq*dw
                    b = dtipsq*dtisq*w
                    if( c==zero ) then
                       if( a==zero ) then
                          if( .not.swtch ) then
                             if( orgati ) then
                                a = z( i )*z( i ) + dtipsq*dtipsq*( dpsi+dphi )
                             else
                                a = z( ip1 )*z( ip1 ) +dtisq*dtisq*( dpsi+dphi )
                             end if
                          else
                             a = dtisq*dtisq*dpsi + dtipsq*dtipsq*dphi
                          end if
                       end if
                       eta = b / a
                    else if( a<=zero ) then
                       eta = ( a-sqrt( abs( a*a-four*b*c ) ) ) / ( two*c )
                    else
                       eta = two*b / ( a+sqrt( abs( a*a-four*b*c ) ) )
                    end if
                 else
                    ! interpolation using three most relevant poles
                    dtiim = work( iim1 )*delta( iim1 )
                    dtiip = work( iip1 )*delta( iip1 )
                    temp = rhoinv + psi + phi
                    if( swtch ) then
                       c = temp - dtiim*dpsi - dtiip*dphi
                       zz( 1_ilp ) = dtiim*dtiim*dpsi
                       zz( 3_ilp ) = dtiip*dtiip*dphi
                    else
                       if( orgati ) then
                          temp1 = z( iim1 ) / dtiim
                          temp1 = temp1*temp1
                          temp2 = ( d( iim1 )-d( iip1 ) )*( d( iim1 )+d( iip1 ) )*temp1
                          c = temp - dtiip*( dpsi+dphi ) - temp2
                          zz( 1_ilp ) = z( iim1 )*z( iim1 )
                          if( dpsi<temp1 ) then
                             zz( 3_ilp ) = dtiip*dtiip*dphi
                          else
                             zz( 3_ilp ) = dtiip*dtiip*( ( dpsi-temp1 )+dphi )
                          end if
                       else
                          temp1 = z( iip1 ) / dtiip
                          temp1 = temp1*temp1
                          temp2 = ( d( iip1 )-d( iim1 ) )*( d( iim1 )+d( iip1 ) )*temp1
                          c = temp - dtiim*( dpsi+dphi ) - temp2
                          if( dphi<temp1 ) then
                             zz( 1_ilp ) = dtiim*dtiim*dpsi
                          else
                             zz( 1_ilp ) = dtiim*dtiim*( dpsi+( dphi-temp1 ) )
                          end if
                          zz( 3_ilp ) = z( iip1 )*z( iip1 )
                       end if
                    end if
                    dd( 1_ilp ) = dtiim
                    dd( 2_ilp ) = delta( ii )*work( ii )
                    dd( 3_ilp ) = dtiip
                    call stdlib_dlaed6( niter, orgati, c, dd, zz, w, eta, info )
                    if( info/=0_ilp ) then
                       ! if info is not 0, i.e., stdlib_dlaed6 failed, switch
                       ! back to two pole interpolation
                       swtch3 = .false.
                       info = 0_ilp
                       dtipsq = work( ip1 )*delta( ip1 )
                       dtisq = work( i )*delta( i )
                       if( .not.swtch ) then
                          if( orgati ) then
                             c = w - dtipsq*dw + delsq*( z( i )/dtisq )**2_ilp
                          else
                             c = w - dtisq*dw - delsq*( z( ip1 )/dtipsq )**2_ilp
                          end if
                       else
                          temp = z( ii ) / ( work( ii )*delta( ii ) )
                          if( orgati ) then
                             dpsi = dpsi + temp*temp
                          else
                             dphi = dphi + temp*temp
                          end if
                          c = w - dtisq*dpsi - dtipsq*dphi
                       end if
                       a = ( dtipsq+dtisq )*w - dtipsq*dtisq*dw
                       b = dtipsq*dtisq*w
                       if( c==zero ) then
                          if( a==zero ) then
                             if( .not.swtch ) then
                                if( orgati ) then
                                   a = z( i )*z( i ) + dtipsq*dtipsq*( dpsi+dphi )
                                else
                                   a = z( ip1 )*z( ip1 ) +dtisq*dtisq*( dpsi+dphi )
                                end if
                             else
                                a = dtisq*dtisq*dpsi + dtipsq*dtipsq*dphi
                             end if
                          end if
                          eta = b / a
                       else if( a<=zero ) then
                          eta = ( a-sqrt( abs( a*a-four*b*c ) ) ) / ( two*c )
                       else
                          eta = two*b / ( a+sqrt( abs( a*a-four*b*c ) ) )
                       end if
                    end if
                 end if
                 ! note, eta should be positive if w is negative, and
                 ! eta should be negative otherwise. however,
                 ! if for some reason caused by roundoff, eta*w > 0,
                 ! we simply use one newton step instead. this way
                 ! will guarantee eta*w < 0.
                 if( w*eta>=zero )eta = -w / dw
                 eta = eta / ( sigma+sqrt( sigma*sigma+eta ) )
                 temp=tau+eta
                 if( temp>sgub .or. temp<sglb ) then
                    if( w<zero ) then
                       eta = ( sgub-tau ) / two
                    else
                       eta = ( sglb-tau ) / two
                    end if
                    if( geomavg ) then
                       if( w < zero ) then
                          if( tau > zero ) then
                             eta = sqrt(sgub*tau)-tau
                          end if
                       else
                          if( sglb > zero ) then
                             eta = sqrt(sglb*tau)-tau
                          end if
                       end if
                    end if
                 end if
                 prew = w
                 tau = tau + eta
                 sigma = sigma + eta
                 do j = 1, n
                    work( j ) = work( j ) + eta
                    delta( j ) = delta( j ) - eta
                 end do
                 ! evaluate psi and the derivative dpsi
                 dpsi = zero
                 psi = zero
                 erretm = zero
                 do j = 1, iim1
                    temp = z( j ) / ( work( j )*delta( j ) )
                    psi = psi + z( j )*temp
                    dpsi = dpsi + temp*temp
                    erretm = erretm + psi
                 end do
                 erretm = abs( erretm )
                 ! evaluate phi and the derivative dphi
                 dphi = zero
                 phi = zero
                 do j = n, iip1, -1
                    temp = z( j ) / ( work( j )*delta( j ) )
                    phi = phi + z( j )*temp
                    dphi = dphi + temp*temp
                    erretm = erretm + phi
                 end do
                 tau2 = work( ii )*delta( ii )
                 temp = z( ii ) / tau2
                 dw = dpsi + dphi + temp*temp
                 temp = z( ii )*temp
                 w = rhoinv + phi + psi + temp
                 erretm = eight*( phi-psi ) + erretm + two*rhoinv+ three*abs( temp )
          ! $             + abs( tau2 )*dw
                 if( w*prew>zero .and. abs( w )>abs( prew ) / ten )swtch = .not.swtch
              end do loop_230
              ! return with info = 1, niter = maxit and not converged
              info = 1_ilp
           end if
           240 continue
           return
     end subroutine stdlib_dlasd4




     pure module subroutine stdlib_slasd5( i, d, z, delta, rho, dsigma, work )
     !! This subroutine computes the square root of the I-th eigenvalue
     !! of a positive symmetric rank-one modification of a 2-by-2 diagonal
     !! matrix
     !! diag( D ) * diag( D ) +  RHO * Z * transpose(Z) .
     !! The diagonal entries in the array D are assumed to satisfy
     !! 0 <= D(i) < D(j)  for  i < j .
     !! We also assume RHO > 0 and that the Euclidean norm of the vector
     !! Z is one.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: i
           real(sp), intent(out) :: dsigma
           real(sp), intent(in) :: rho
           ! Array Arguments 
           real(sp), intent(in) :: d(2_ilp), z(2_ilp)
           real(sp), intent(out) :: delta(2_ilp), work(2_ilp)
        ! =====================================================================
           
           ! Local Scalars 
           real(sp) :: b, c, del, delsq, tau, w
           ! Intrinsic Functions 
           ! Executable Statements 
           del = d( 2_ilp ) - d( 1_ilp )
           delsq = del*( d( 2_ilp )+d( 1_ilp ) )
           if( i==1_ilp ) then
              w = one + four*rho*( z( 2_ilp )*z( 2_ilp ) / ( d( 1_ilp )+three*d( 2_ilp ) )-z( 1_ilp )*z( 1_ilp ) / ( &
                        three*d( 1_ilp )+d( 2_ilp ) ) ) / del
              if( w>zero ) then
                 b = delsq + rho*( z( 1_ilp )*z( 1_ilp )+z( 2_ilp )*z( 2_ilp ) )
                 c = rho*z( 1_ilp )*z( 1_ilp )*delsq
                 ! b > zero, always
                 ! the following tau is dsigma * dsigma - d( 1 ) * d( 1 )
                 tau = two*c / ( b+sqrt( abs( b*b-four*c ) ) )
                 ! the following tau is dsigma - d( 1 )
                 tau = tau / ( d( 1_ilp )+sqrt( d( 1_ilp )*d( 1_ilp )+tau ) )
                 dsigma = d( 1_ilp ) + tau
                 delta( 1_ilp ) = -tau
                 delta( 2_ilp ) = del - tau
                 work( 1_ilp ) = two*d( 1_ilp ) + tau
                 work( 2_ilp ) = ( d( 1_ilp )+tau ) + d( 2_ilp )
                 ! delta( 1 ) = -z( 1 ) / tau
                 ! delta( 2 ) = z( 2 ) / ( del-tau )
              else
                 b = -delsq + rho*( z( 1_ilp )*z( 1_ilp )+z( 2_ilp )*z( 2_ilp ) )
                 c = rho*z( 2_ilp )*z( 2_ilp )*delsq
                 ! the following tau is dsigma * dsigma - d( 2 ) * d( 2 )
                 if( b>zero ) then
                    tau = -two*c / ( b+sqrt( b*b+four*c ) )
                 else
                    tau = ( b-sqrt( b*b+four*c ) ) / two
                 end if
                 ! the following tau is dsigma - d( 2 )
                 tau = tau / ( d( 2_ilp )+sqrt( abs( d( 2_ilp )*d( 2_ilp )+tau ) ) )
                 dsigma = d( 2_ilp ) + tau
                 delta( 1_ilp ) = -( del+tau )
                 delta( 2_ilp ) = -tau
                 work( 1_ilp ) = d( 1_ilp ) + tau + d( 2_ilp )
                 work( 2_ilp ) = two*d( 2_ilp ) + tau
                 ! delta( 1 ) = -z( 1 ) / ( del+tau )
                 ! delta( 2 ) = -z( 2 ) / tau
              end if
              ! temp = sqrt( delta( 1 )*delta( 1 )+delta( 2 )*delta( 2 ) )
              ! delta( 1 ) = delta( 1 ) / temp
              ! delta( 2 ) = delta( 2 ) / temp
           else
              ! now i=2
              b = -delsq + rho*( z( 1_ilp )*z( 1_ilp )+z( 2_ilp )*z( 2_ilp ) )
              c = rho*z( 2_ilp )*z( 2_ilp )*delsq
              ! the following tau is dsigma * dsigma - d( 2 ) * d( 2 )
              if( b>zero ) then
                 tau = ( b+sqrt( b*b+four*c ) ) / two
              else
                 tau = two*c / ( -b+sqrt( b*b+four*c ) )
              end if
              ! the following tau is dsigma - d( 2 )
              tau = tau / ( d( 2_ilp )+sqrt( d( 2_ilp )*d( 2_ilp )+tau ) )
              dsigma = d( 2_ilp ) + tau
              delta( 1_ilp ) = -( del+tau )
              delta( 2_ilp ) = -tau
              work( 1_ilp ) = d( 1_ilp ) + tau + d( 2_ilp )
              work( 2_ilp ) = two*d( 2_ilp ) + tau
              ! delta( 1 ) = -z( 1 ) / ( del+tau )
              ! delta( 2 ) = -z( 2 ) / tau
              ! temp = sqrt( delta( 1 )*delta( 1 )+delta( 2 )*delta( 2 ) )
              ! delta( 1 ) = delta( 1 ) / temp
              ! delta( 2 ) = delta( 2 ) / temp
           end if
           return
     end subroutine stdlib_slasd5

     pure module subroutine stdlib_dlasd5( i, d, z, delta, rho, dsigma, work )
     !! This subroutine computes the square root of the I-th eigenvalue
     !! of a positive symmetric rank-one modification of a 2-by-2 diagonal
     !! matrix
     !! diag( D ) * diag( D ) +  RHO * Z * transpose(Z) .
     !! The diagonal entries in the array D are assumed to satisfy
     !! 0 <= D(i) < D(j)  for  i < j .
     !! We also assume RHO > 0 and that the Euclidean norm of the vector
     !! Z is one.
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: i
           real(dp), intent(out) :: dsigma
           real(dp), intent(in) :: rho
           ! Array Arguments 
           real(dp), intent(in) :: d(2_ilp), z(2_ilp)
           real(dp), intent(out) :: delta(2_ilp), work(2_ilp)
        ! =====================================================================
           
           ! Local Scalars 
           real(dp) :: b, c, del, delsq, tau, w
           ! Intrinsic Functions 
           ! Executable Statements 
           del = d( 2_ilp ) - d( 1_ilp )
           delsq = del*( d( 2_ilp )+d( 1_ilp ) )
           if( i==1_ilp ) then
              w = one + four*rho*( z( 2_ilp )*z( 2_ilp ) / ( d( 1_ilp )+three*d( 2_ilp ) )-z( 1_ilp )*z( 1_ilp ) / ( &
                        three*d( 1_ilp )+d( 2_ilp ) ) ) / del
              if( w>zero ) then
                 b = delsq + rho*( z( 1_ilp )*z( 1_ilp )+z( 2_ilp )*z( 2_ilp ) )
                 c = rho*z( 1_ilp )*z( 1_ilp )*delsq
                 ! b > zero, always
                 ! the following tau is dsigma * dsigma - d( 1 ) * d( 1 )
                 tau = two*c / ( b+sqrt( abs( b*b-four*c ) ) )
                 ! the following tau is dsigma - d( 1 )
                 tau = tau / ( d( 1_ilp )+sqrt( d( 1_ilp )*d( 1_ilp )+tau ) )
                 dsigma = d( 1_ilp ) + tau
                 delta( 1_ilp ) = -tau
                 delta( 2_ilp ) = del - tau
                 work( 1_ilp ) = two*d( 1_ilp ) + tau
                 work( 2_ilp ) = ( d( 1_ilp )+tau ) + d( 2_ilp )
                 ! delta( 1 ) = -z( 1 ) / tau
                 ! delta( 2 ) = z( 2 ) / ( del-tau )
              else
                 b = -delsq + rho*( z( 1_ilp )*z( 1_ilp )+z( 2_ilp )*z( 2_ilp ) )
                 c = rho*z( 2_ilp )*z( 2_ilp )*delsq
                 ! the following tau is dsigma * dsigma - d( 2 ) * d( 2 )
                 if( b>zero ) then
                    tau = -two*c / ( b+sqrt( b*b+four*c ) )
                 else
                    tau = ( b-sqrt( b*b+four*c ) ) / two
                 end if
                 ! the following tau is dsigma - d( 2 )
                 tau = tau / ( d( 2_ilp )+sqrt( abs( d( 2_ilp )*d( 2_ilp )+tau ) ) )
                 dsigma = d( 2_ilp ) + tau
                 delta( 1_ilp ) = -( del+tau )
                 delta( 2_ilp ) = -tau
                 work( 1_ilp ) = d( 1_ilp ) + tau + d( 2_ilp )
                 work( 2_ilp ) = two*d( 2_ilp ) + tau
                 ! delta( 1 ) = -z( 1 ) / ( del+tau )
                 ! delta( 2 ) = -z( 2 ) / tau
              end if
              ! temp = sqrt( delta( 1 )*delta( 1 )+delta( 2 )*delta( 2 ) )
              ! delta( 1 ) = delta( 1 ) / temp
              ! delta( 2 ) = delta( 2 ) / temp
           else
              ! now i=2
              b = -delsq + rho*( z( 1_ilp )*z( 1_ilp )+z( 2_ilp )*z( 2_ilp ) )
              c = rho*z( 2_ilp )*z( 2_ilp )*delsq
              ! the following tau is dsigma * dsigma - d( 2 ) * d( 2 )
              if( b>zero ) then
                 tau = ( b+sqrt( b*b+four*c ) ) / two
              else
                 tau = two*c / ( -b+sqrt( b*b+four*c ) )
              end if
              ! the following tau is dsigma - d( 2 )
              tau = tau / ( d( 2_ilp )+sqrt( d( 2_ilp )*d( 2_ilp )+tau ) )
              dsigma = d( 2_ilp ) + tau
              delta( 1_ilp ) = -( del+tau )
              delta( 2_ilp ) = -tau
              work( 1_ilp ) = d( 1_ilp ) + tau + d( 2_ilp )
              work( 2_ilp ) = two*d( 2_ilp ) + tau
              ! delta( 1 ) = -z( 1 ) / ( del+tau )
              ! delta( 2 ) = -z( 2 ) / tau
              ! temp = sqrt( delta( 1 )*delta( 1 )+delta( 2 )*delta( 2 ) )
              ! delta( 1 ) = delta( 1 ) / temp
              ! delta( 2 ) = delta( 2 ) / temp
           end if
           return
     end subroutine stdlib_dlasd5




     pure module subroutine stdlib_slasdq( uplo, sqre, n, ncvt, nru, ncc, d, e, vt, ldvt,u, ldu, c, ldc, &
     !! SLASDQ computes the singular value decomposition (SVD) of a real
     !! (upper or lower) bidiagonal matrix with diagonal D and offdiagonal
     !! E, accumulating the transformations if desired. Letting B denote
     !! the input bidiagonal matrix, the algorithm computes orthogonal
     !! matrices Q and P such that B = Q * S * P**T (P**T denotes the transpose
     !! of P). The singular values S are overwritten on D.
     !! The input matrix U  is changed to U  * Q  if desired.
     !! The input matrix VT is changed to P**T * VT if desired.
     !! The input matrix C  is changed to Q**T * C  if desired.
     !! See "Computing  Small Singular Values of Bidiagonal Matrices With
     !! Guaranteed High Relative Accuracy," by J. Demmel and W. Kahan,
     !! LAPACK Working Note #3, for a detailed description of the algorithm.
               work, info )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldc, ldu, ldvt, n, ncc, ncvt, nru, sqre
           ! Array Arguments 
           real(sp), intent(inout) :: c(ldc,*), d(*), e(*), u(ldu,*), vt(ldvt,*)
           real(sp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: rotate
           integer(ilp) :: i, isub, iuplo, j, np1, sqre1
           real(sp) :: cs, r, smin, sn
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           iuplo = 0_ilp
           if( stdlib_lsame( uplo, 'U' ) )iuplo = 1_ilp
           if( stdlib_lsame( uplo, 'L' ) )iuplo = 2_ilp
           if( iuplo==0_ilp ) then
              info = -1_ilp
           else if( ( sqre<0_ilp ) .or. ( sqre>1_ilp ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( ncvt<0_ilp ) then
              info = -4_ilp
           else if( nru<0_ilp ) then
              info = -5_ilp
           else if( ncc<0_ilp ) then
              info = -6_ilp
           else if( ( ncvt==0_ilp .and. ldvt<1_ilp ) .or.( ncvt>0_ilp .and. ldvt<max( 1_ilp, n ) ) ) then
              info = -10_ilp
           else if( ldu<max( 1_ilp, nru ) ) then
              info = -12_ilp
           else if( ( ncc==0_ilp .and. ldc<1_ilp ) .or.( ncc>0_ilp .and. ldc<max( 1_ilp, n ) ) ) then
              info = -14_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SLASDQ', -info )
              return
           end if
           if( n==0 )return
           ! rotate is true if any singular vectors desired, false otherwise
           rotate = ( ncvt>0_ilp ) .or. ( nru>0_ilp ) .or. ( ncc>0_ilp )
           np1 = n + 1_ilp
           sqre1 = sqre
           ! if matrix non-square upper bidiagonal, rotate to be lower
           ! bidiagonal.  the rotations are on the right.
           if( ( iuplo==1_ilp ) .and. ( sqre1==1_ilp ) ) then
              do i = 1, n - 1
                 call stdlib_slartg( d( i ), e( i ), cs, sn, r )
                 d( i ) = r
                 e( i ) = sn*d( i+1 )
                 d( i+1 ) = cs*d( i+1 )
                 if( rotate ) then
                    work( i ) = cs
                    work( n+i ) = sn
                 end if
              end do
              call stdlib_slartg( d( n ), e( n ), cs, sn, r )
              d( n ) = r
              e( n ) = zero
              if( rotate ) then
                 work( n ) = cs
                 work( n+n ) = sn
              end if
              iuplo = 2_ilp
              sqre1 = 0_ilp
              ! update singular vectors if desired.
              if( ncvt>0_ilp )call stdlib_slasr( 'L', 'V', 'F', np1, ncvt, work( 1_ilp ),work( np1 ), vt, &
                        ldvt )
           end if
           ! if matrix lower bidiagonal, rotate to be upper bidiagonal
           ! by applying givens rotations on the left.
           if( iuplo==2_ilp ) then
              do i = 1, n - 1
                 call stdlib_slartg( d( i ), e( i ), cs, sn, r )
                 d( i ) = r
                 e( i ) = sn*d( i+1 )
                 d( i+1 ) = cs*d( i+1 )
                 if( rotate ) then
                    work( i ) = cs
                    work( n+i ) = sn
                 end if
              end do
              ! if matrix (n+1)-by-n lower bidiagonal, one additional
              ! rotation is needed.
              if( sqre1==1_ilp ) then
                 call stdlib_slartg( d( n ), e( n ), cs, sn, r )
                 d( n ) = r
                 if( rotate ) then
                    work( n ) = cs
                    work( n+n ) = sn
                 end if
              end if
              ! update singular vectors if desired.
              if( nru>0_ilp ) then
                 if( sqre1==0_ilp ) then
                    call stdlib_slasr( 'R', 'V', 'F', nru, n, work( 1_ilp ),work( np1 ), u, ldu )
                              
                 else
                    call stdlib_slasr( 'R', 'V', 'F', nru, np1, work( 1_ilp ),work( np1 ), u, ldu )
                              
                 end if
              end if
              if( ncc>0_ilp ) then
                 if( sqre1==0_ilp ) then
                    call stdlib_slasr( 'L', 'V', 'F', n, ncc, work( 1_ilp ),work( np1 ), c, ldc )
                              
                 else
                    call stdlib_slasr( 'L', 'V', 'F', np1, ncc, work( 1_ilp ),work( np1 ), c, ldc )
                              
                 end if
              end if
           end if
           ! call stdlib_sbdsqr to compute the svd of the reduced real
           ! n-by-n upper bidiagonal matrix.
           call stdlib_sbdsqr( 'U', n, ncvt, nru, ncc, d, e, vt, ldvt, u, ldu, c,ldc, work, info )
                     
           ! sort the singular values into ascending order (insertion sort on
           ! singular values, but only one transposition per singular vector)
           do i = 1, n
              ! scan for smallest d(i).
              isub = i
              smin = d( i )
              do j = i + 1, n
                 if( d( j )<smin ) then
                    isub = j
                    smin = d( j )
                 end if
              end do
              if( isub/=i ) then
                 ! swap singular values and vectors.
                 d( isub ) = d( i )
                 d( i ) = smin
                 if( ncvt>0_ilp )call stdlib_sswap( ncvt, vt( isub, 1_ilp ), ldvt, vt( i, 1_ilp ), ldvt )
                           
                 if( nru>0_ilp )call stdlib_sswap( nru, u( 1_ilp, isub ), 1_ilp, u( 1_ilp, i ), 1_ilp )
                 if( ncc>0_ilp )call stdlib_sswap( ncc, c( isub, 1_ilp ), ldc, c( i, 1_ilp ), ldc )
              end if
           end do
           return
     end subroutine stdlib_slasdq

     pure module subroutine stdlib_dlasdq( uplo, sqre, n, ncvt, nru, ncc, d, e, vt, ldvt,u, ldu, c, ldc, &
     !! DLASDQ computes the singular value decomposition (SVD) of a real
     !! (upper or lower) bidiagonal matrix with diagonal D and offdiagonal
     !! E, accumulating the transformations if desired. Letting B denote
     !! the input bidiagonal matrix, the algorithm computes orthogonal
     !! matrices Q and P such that B = Q * S * P**T (P**T denotes the transpose
     !! of P). The singular values S are overwritten on D.
     !! The input matrix U  is changed to U  * Q  if desired.
     !! The input matrix VT is changed to P**T * VT if desired.
     !! The input matrix C  is changed to Q**T * C  if desired.
     !! See "Computing  Small Singular Values of Bidiagonal Matrices With
     !! Guaranteed High Relative Accuracy," by J. Demmel and W. Kahan,
     !! LAPACK Working Note #3, for a detailed description of the algorithm.
               work, info )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldc, ldu, ldvt, n, ncc, ncvt, nru, sqre
           ! Array Arguments 
           real(dp), intent(inout) :: c(ldc,*), d(*), e(*), u(ldu,*), vt(ldvt,*)
           real(dp), intent(out) :: work(*)
        ! =====================================================================
           
           ! Local Scalars 
           logical(lk) :: rotate
           integer(ilp) :: i, isub, iuplo, j, np1, sqre1
           real(dp) :: cs, r, smin, sn
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           iuplo = 0_ilp
           if( stdlib_lsame( uplo, 'U' ) )iuplo = 1_ilp
           if( stdlib_lsame( uplo, 'L' ) )iuplo = 2_ilp
           if( iuplo==0_ilp ) then
              info = -1_ilp
           else if( ( sqre<0_ilp ) .or. ( sqre>1_ilp ) ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( ncvt<0_ilp ) then
              info = -4_ilp
           else if( nru<0_ilp ) then
              info = -5_ilp
           else if( ncc<0_ilp ) then
              info = -6_ilp
           else if( ( ncvt==0_ilp .and. ldvt<1_ilp ) .or.( ncvt>0_ilp .and. ldvt<max( 1_ilp, n ) ) ) then
              info = -10_ilp
           else if( ldu<max( 1_ilp, nru ) ) then
              info = -12_ilp
           else if( ( ncc==0_ilp .and. ldc<1_ilp ) .or.( ncc>0_ilp .and. ldc<max( 1_ilp, n ) ) ) then
              info = -14_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DLASDQ', -info )
              return
           end if
           if( n==0 )return
           ! rotate is true if any singular vectors desired, false otherwise
           rotate = ( ncvt>0_ilp ) .or. ( nru>0_ilp ) .or. ( ncc>0_ilp )
           np1 = n + 1_ilp
           sqre1 = sqre
           ! if matrix non-square upper bidiagonal, rotate to be lower
           ! bidiagonal.  the rotations are on the right.
           if( ( iuplo==1_ilp ) .and. ( sqre1==1_ilp ) ) then
              do i = 1, n - 1
                 call stdlib_dlartg( d( i ), e( i ), cs, sn, r )
                 d( i ) = r
                 e( i ) = sn*d( i+1 )
                 d( i+1 ) = cs*d( i+1 )
                 if( rotate ) then
                    work( i ) = cs
                    work( n+i ) = sn
                 end if
              end do
              call stdlib_dlartg( d( n ), e( n ), cs, sn, r )
              d( n ) = r
              e( n ) = zero
              if( rotate ) then
                 work( n ) = cs
                 work( n+n ) = sn
              end if
              iuplo = 2_ilp
              sqre1 = 0_ilp
              ! update singular vectors if desired.
              if( ncvt>0_ilp )call stdlib_dlasr( 'L', 'V', 'F', np1, ncvt, work( 1_ilp ),work( np1 ), vt, &
                        ldvt )
           end if
           ! if matrix lower bidiagonal, rotate to be upper bidiagonal
           ! by applying givens rotations on the left.
           if( iuplo==2_ilp ) then
              do i = 1, n - 1
                 call stdlib_dlartg( d( i ), e( i ), cs, sn, r )
                 d( i ) = r
                 e( i ) = sn*d( i+1 )
                 d( i+1 ) = cs*d( i+1 )
                 if( rotate ) then
                    work( i ) = cs
                    work( n+i ) = sn
                 end if
              end do
              ! if matrix (n+1)-by-n lower bidiagonal, one additional
              ! rotation is needed.
              if( sqre1==1_ilp ) then
                 call stdlib_dlartg( d( n ), e( n ), cs, sn, r )
                 d( n ) = r
                 if( rotate ) then
                    work( n ) = cs
                    work( n+n ) = sn
                 end if
              end if
              ! update singular vectors if desired.
              if( nru>0_ilp ) then
                 if( sqre1==0_ilp ) then
                    call stdlib_dlasr( 'R', 'V', 'F', nru, n, work( 1_ilp ),work( np1 ), u, ldu )
                              
                 else
                    call stdlib_dlasr( 'R', 'V', 'F', nru, np1, work( 1_ilp ),work( np1 ), u, ldu )
                              
                 end if
              end if
              if( ncc>0_ilp ) then
                 if( sqre1==0_ilp ) then
                    call stdlib_dlasr( 'L', 'V', 'F', n, ncc, work( 1_ilp ),work( np1 ), c, ldc )
                              
                 else
                    call stdlib_dlasr( 'L', 'V', 'F', np1, ncc, work( 1_ilp ),work( np1 ), c, ldc )
                              
                 end if
              end if
           end if
           ! call stdlib_dbdsqr to compute the svd of the reduced real
           ! n-by-n upper bidiagonal matrix.
           call stdlib_dbdsqr( 'U', n, ncvt, nru, ncc, d, e, vt, ldvt, u, ldu, c,ldc, work, info )
                     
           ! sort the singular values into ascending order (insertion sort on
           ! singular values, but only one transposition per singular vector)
           do i = 1, n
              ! scan for smallest d(i).
              isub = i
              smin = d( i )
              do j = i + 1, n
                 if( d( j )<smin ) then
                    isub = j
                    smin = d( j )
                 end if
              end do
              if( isub/=i ) then
                 ! swap singular values and vectors.
                 d( isub ) = d( i )
                 d( i ) = smin
                 if( ncvt>0_ilp )call stdlib_dswap( ncvt, vt( isub, 1_ilp ), ldvt, vt( i, 1_ilp ), ldvt )
                           
                 if( nru>0_ilp )call stdlib_dswap( nru, u( 1_ilp, isub ), 1_ilp, u( 1_ilp, i ), 1_ilp )
                 if( ncc>0_ilp )call stdlib_dswap( ncc, c( isub, 1_ilp ), ldc, c( i, 1_ilp ), ldc )
              end if
           end do
           return
     end subroutine stdlib_dlasdq




     pure module subroutine stdlib_slasda( icompq, smlsiz, n, sqre, d, e, u, ldu, vt, k,difl, difr, z, &
     !! Using a divide and conquer approach, SLASDA: computes the singular
     !! value decomposition (SVD) of a real upper bidiagonal N-by-M matrix
     !! B with diagonal D and offdiagonal E, where M = N + SQRE. The
     !! algorithm computes the singular values in the SVD B = U * S * VT.
     !! The orthogonal matrices U and VT are optionally computed in
     !! compact form.
     !! A related subroutine, SLASD0, computes the singular values and
     !! the singular vectors in explicit form.
               poles, givptr, givcol, ldgcol,perm, givnum, c, s, work, iwork, info )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: icompq, ldgcol, ldu, n, smlsiz, sqre
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           integer(ilp), intent(out) :: givcol(ldgcol,*), givptr(*), iwork(*), k(*), perm(ldgcol,&
                     *)
           real(sp), intent(out) :: c(*), difl(ldu,*), difr(ldu,*), givnum(ldu,*), poles(ldu,*), &
                     s(*), u(ldu,*), vt(ldu,*), work(*), z(ldu,*)
           real(sp), intent(inout) :: d(*), e(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, i1, ic, idxq, idxqi, im1, inode, itemp, iwk, j, lf, ll, lvl, lvl2, &
           m, ncc, nd, ndb1, ndiml, ndimr, nl, nlf, nlp1, nlvl, nr, nrf, nrp1, nru, nwork1, &
                     nwork2, smlszp, sqrei, vf, vfi, vl, vli
           real(sp) :: alpha, beta
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( ( icompq<0_ilp ) .or. ( icompq>1_ilp ) ) then
              info = -1_ilp
           else if( smlsiz<3_ilp ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( ( sqre<0_ilp ) .or. ( sqre>1_ilp ) ) then
              info = -4_ilp
           else if( ldu<( n+sqre ) ) then
              info = -8_ilp
           else if( ldgcol<n ) then
              info = -17_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SLASDA', -info )
              return
           end if
           m = n + sqre
           ! if the input matrix is too small, call stdlib_slasdq to find the svd.
           if( n<=smlsiz ) then
              if( icompq==0_ilp ) then
                 call stdlib_slasdq( 'U', sqre, n, 0_ilp, 0_ilp, 0_ilp, d, e, vt, ldu, u, ldu,u, ldu, work, &
                           info )
              else
                 call stdlib_slasdq( 'U', sqre, n, m, n, 0_ilp, d, e, vt, ldu, u, ldu,u, ldu, work, &
                           info )
              end if
              return
           end if
           ! book-keeping and  set up the computation tree.
           inode = 1_ilp
           ndiml = inode + n
           ndimr = ndiml + n
           idxq = ndimr + n
           iwk = idxq + n
           ncc = 0_ilp
           nru = 0_ilp
           smlszp = smlsiz + 1_ilp
           vf = 1_ilp
           vl = vf + m
           nwork1 = vl + m
           nwork2 = nwork1 + smlszp*smlszp
           call stdlib_slasdt( n, nlvl, nd, iwork( inode ), iwork( ndiml ),iwork( ndimr ), smlsiz &
                     )
           ! for the nodes on bottom level of the tree, solve
           ! their subproblems by stdlib_slasdq.
           ndb1 = ( nd+1 ) / 2_ilp
           loop_30: do i = ndb1, nd
              ! ic : center row of each node
              ! nl : number of rows of left  subproblem
              ! nr : number of rows of right subproblem
              ! nlf: starting row of the left   subproblem
              ! nrf: starting row of the right  subproblem
              i1 = i - 1_ilp
              ic = iwork( inode+i1 )
              nl = iwork( ndiml+i1 )
              nlp1 = nl + 1_ilp
              nr = iwork( ndimr+i1 )
              nlf = ic - nl
              nrf = ic + 1_ilp
              idxqi = idxq + nlf - 2_ilp
              vfi = vf + nlf - 1_ilp
              vli = vl + nlf - 1_ilp
              sqrei = 1_ilp
              if( icompq==0_ilp ) then
                 call stdlib_slaset( 'A', nlp1, nlp1, zero, one, work( nwork1 ),smlszp )
                 call stdlib_slasdq( 'U', sqrei, nl, nlp1, nru, ncc, d( nlf ),e( nlf ), work( &
                 nwork1 ), smlszp,work( nwork2 ), nl, work( nwork2 ), nl,work( nwork2 ), info )
                           
                 itemp = nwork1 + nl*smlszp
                 call stdlib_scopy( nlp1, work( nwork1 ), 1_ilp, work( vfi ), 1_ilp )
                 call stdlib_scopy( nlp1, work( itemp ), 1_ilp, work( vli ), 1_ilp )
              else
                 call stdlib_slaset( 'A', nl, nl, zero, one, u( nlf, 1_ilp ), ldu )
                 call stdlib_slaset( 'A', nlp1, nlp1, zero, one, vt( nlf, 1_ilp ), ldu )
                 call stdlib_slasdq( 'U', sqrei, nl, nlp1, nl, ncc, d( nlf ),e( nlf ), vt( nlf, 1_ilp &
                           ), ldu, u( nlf, 1_ilp ), ldu,u( nlf, 1_ilp ), ldu, work( nwork1 ), info )
                 call stdlib_scopy( nlp1, vt( nlf, 1_ilp ), 1_ilp, work( vfi ), 1_ilp )
                 call stdlib_scopy( nlp1, vt( nlf, nlp1 ), 1_ilp, work( vli ), 1_ilp )
              end if
              if( info/=0_ilp ) then
                 return
              end if
              do j = 1, nl
                 iwork( idxqi+j ) = j
              end do
              if( ( i==nd ) .and. ( sqre==0_ilp ) ) then
                 sqrei = 0_ilp
              else
                 sqrei = 1_ilp
              end if
              idxqi = idxqi + nlp1
              vfi = vfi + nlp1
              vli = vli + nlp1
              nrp1 = nr + sqrei
              if( icompq==0_ilp ) then
                 call stdlib_slaset( 'A', nrp1, nrp1, zero, one, work( nwork1 ),smlszp )
                 call stdlib_slasdq( 'U', sqrei, nr, nrp1, nru, ncc, d( nrf ),e( nrf ), work( &
                 nwork1 ), smlszp,work( nwork2 ), nr, work( nwork2 ), nr,work( nwork2 ), info )
                           
                 itemp = nwork1 + ( nrp1-1 )*smlszp
                 call stdlib_scopy( nrp1, work( nwork1 ), 1_ilp, work( vfi ), 1_ilp )
                 call stdlib_scopy( nrp1, work( itemp ), 1_ilp, work( vli ), 1_ilp )
              else
                 call stdlib_slaset( 'A', nr, nr, zero, one, u( nrf, 1_ilp ), ldu )
                 call stdlib_slaset( 'A', nrp1, nrp1, zero, one, vt( nrf, 1_ilp ), ldu )
                 call stdlib_slasdq( 'U', sqrei, nr, nrp1, nr, ncc, d( nrf ),e( nrf ), vt( nrf, 1_ilp &
                           ), ldu, u( nrf, 1_ilp ), ldu,u( nrf, 1_ilp ), ldu, work( nwork1 ), info )
                 call stdlib_scopy( nrp1, vt( nrf, 1_ilp ), 1_ilp, work( vfi ), 1_ilp )
                 call stdlib_scopy( nrp1, vt( nrf, nrp1 ), 1_ilp, work( vli ), 1_ilp )
              end if
              if( info/=0_ilp ) then
                 return
              end if
              do j = 1, nr
                 iwork( idxqi+j ) = j
              end do
           end do loop_30
           ! now conquer each subproblem bottom-up.
           j = 2_ilp**nlvl
           loop_50: do lvl = nlvl, 1, -1
              lvl2 = lvl*2_ilp - 1_ilp
              ! find the first node lf and last node ll on
              ! the current level lvl.
              if( lvl==1_ilp ) then
                 lf = 1_ilp
                 ll = 1_ilp
              else
                 lf = 2_ilp**( lvl-1 )
                 ll = 2_ilp*lf - 1_ilp
              end if
              loop_40: do i = lf, ll
                 im1 = i - 1_ilp
                 ic = iwork( inode+im1 )
                 nl = iwork( ndiml+im1 )
                 nr = iwork( ndimr+im1 )
                 nlf = ic - nl
                 nrf = ic + 1_ilp
                 if( i==ll ) then
                    sqrei = sqre
                 else
                    sqrei = 1_ilp
                 end if
                 vfi = vf + nlf - 1_ilp
                 vli = vl + nlf - 1_ilp
                 idxqi = idxq + nlf - 1_ilp
                 alpha = d( ic )
                 beta = e( ic )
                 if( icompq==0_ilp ) then
                    call stdlib_slasd6( icompq, nl, nr, sqrei, d( nlf ),work( vfi ), work( vli ), &
                    alpha, beta,iwork( idxqi ), perm, givptr( 1_ilp ), givcol,ldgcol, givnum, ldu, &
                    poles, difl, difr, z,k( 1_ilp ), c( 1_ilp ), s( 1_ilp ), work( nwork1 ),iwork( iwk ), &
                              info )
                 else
                    j = j - 1_ilp
                    call stdlib_slasd6( icompq, nl, nr, sqrei, d( nlf ),work( vfi ), work( vli ), &
                    alpha, beta,iwork( idxqi ), perm( nlf, lvl ),givptr( j ), givcol( nlf, lvl2 ),&
                     ldgcol,givnum( nlf, lvl2 ), ldu,poles( nlf, lvl2 ), difl( nlf, lvl ),difr( &
                     nlf, lvl2 ), z( nlf, lvl ), k( j ),c( j ), s( j ), work( nwork1 ),iwork( iwk &
                               ), info )
                 end if
                 if( info/=0_ilp ) then
                    return
                 end if
              end do loop_40
           end do loop_50
           return
     end subroutine stdlib_slasda

     pure module subroutine stdlib_dlasda( icompq, smlsiz, n, sqre, d, e, u, ldu, vt, k,difl, difr, z, &
     !! Using a divide and conquer approach, DLASDA: computes the singular
     !! value decomposition (SVD) of a real upper bidiagonal N-by-M matrix
     !! B with diagonal D and offdiagonal E, where M = N + SQRE. The
     !! algorithm computes the singular values in the SVD B = U * S * VT.
     !! The orthogonal matrices U and VT are optionally computed in
     !! compact form.
     !! A related subroutine, DLASD0, computes the singular values and
     !! the singular vectors in explicit form.
               poles, givptr, givcol, ldgcol,perm, givnum, c, s, work, iwork, info )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: icompq, ldgcol, ldu, n, smlsiz, sqre
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           integer(ilp), intent(out) :: givcol(ldgcol,*), givptr(*), iwork(*), k(*), perm(ldgcol,&
                     *)
           real(dp), intent(out) :: c(*), difl(ldu,*), difr(ldu,*), givnum(ldu,*), poles(ldu,*), &
                     s(*), u(ldu,*), vt(ldu,*), work(*), z(ldu,*)
           real(dp), intent(inout) :: d(*), e(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, i1, ic, idxq, idxqi, im1, inode, itemp, iwk, j, lf, ll, lvl, lvl2, &
           m, ncc, nd, ndb1, ndiml, ndimr, nl, nlf, nlp1, nlvl, nr, nrf, nrp1, nru, nwork1, &
                     nwork2, smlszp, sqrei, vf, vfi, vl, vli
           real(dp) :: alpha, beta
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( ( icompq<0_ilp ) .or. ( icompq>1_ilp ) ) then
              info = -1_ilp
           else if( smlsiz<3_ilp ) then
              info = -2_ilp
           else if( n<0_ilp ) then
              info = -3_ilp
           else if( ( sqre<0_ilp ) .or. ( sqre>1_ilp ) ) then
              info = -4_ilp
           else if( ldu<( n+sqre ) ) then
              info = -8_ilp
           else if( ldgcol<n ) then
              info = -17_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DLASDA', -info )
              return
           end if
           m = n + sqre
           ! if the input matrix is too small, call stdlib_dlasdq to find the svd.
           if( n<=smlsiz ) then
              if( icompq==0_ilp ) then
                 call stdlib_dlasdq( 'U', sqre, n, 0_ilp, 0_ilp, 0_ilp, d, e, vt, ldu, u, ldu,u, ldu, work, &
                           info )
              else
                 call stdlib_dlasdq( 'U', sqre, n, m, n, 0_ilp, d, e, vt, ldu, u, ldu,u, ldu, work, &
                           info )
              end if
              return
           end if
           ! book-keeping and  set up the computation tree.
           inode = 1_ilp
           ndiml = inode + n
           ndimr = ndiml + n
           idxq = ndimr + n
           iwk = idxq + n
           ncc = 0_ilp
           nru = 0_ilp
           smlszp = smlsiz + 1_ilp
           vf = 1_ilp
           vl = vf + m
           nwork1 = vl + m
           nwork2 = nwork1 + smlszp*smlszp
           call stdlib_dlasdt( n, nlvl, nd, iwork( inode ), iwork( ndiml ),iwork( ndimr ), smlsiz &
                     )
           ! for the nodes on bottom level of the tree, solve
           ! their subproblems by stdlib_dlasdq.
           ndb1 = ( nd+1 ) / 2_ilp
           loop_30: do i = ndb1, nd
              ! ic : center row of each node
              ! nl : number of rows of left  subproblem
              ! nr : number of rows of right subproblem
              ! nlf: starting row of the left   subproblem
              ! nrf: starting row of the right  subproblem
              i1 = i - 1_ilp
              ic = iwork( inode+i1 )
              nl = iwork( ndiml+i1 )
              nlp1 = nl + 1_ilp
              nr = iwork( ndimr+i1 )
              nlf = ic - nl
              nrf = ic + 1_ilp
              idxqi = idxq + nlf - 2_ilp
              vfi = vf + nlf - 1_ilp
              vli = vl + nlf - 1_ilp
              sqrei = 1_ilp
              if( icompq==0_ilp ) then
                 call stdlib_dlaset( 'A', nlp1, nlp1, zero, one, work( nwork1 ),smlszp )
                 call stdlib_dlasdq( 'U', sqrei, nl, nlp1, nru, ncc, d( nlf ),e( nlf ), work( &
                 nwork1 ), smlszp,work( nwork2 ), nl, work( nwork2 ), nl,work( nwork2 ), info )
                           
                 itemp = nwork1 + nl*smlszp
                 call stdlib_dcopy( nlp1, work( nwork1 ), 1_ilp, work( vfi ), 1_ilp )
                 call stdlib_dcopy( nlp1, work( itemp ), 1_ilp, work( vli ), 1_ilp )
              else
                 call stdlib_dlaset( 'A', nl, nl, zero, one, u( nlf, 1_ilp ), ldu )
                 call stdlib_dlaset( 'A', nlp1, nlp1, zero, one, vt( nlf, 1_ilp ), ldu )
                 call stdlib_dlasdq( 'U', sqrei, nl, nlp1, nl, ncc, d( nlf ),e( nlf ), vt( nlf, 1_ilp &
                           ), ldu, u( nlf, 1_ilp ), ldu,u( nlf, 1_ilp ), ldu, work( nwork1 ), info )
                 call stdlib_dcopy( nlp1, vt( nlf, 1_ilp ), 1_ilp, work( vfi ), 1_ilp )
                 call stdlib_dcopy( nlp1, vt( nlf, nlp1 ), 1_ilp, work( vli ), 1_ilp )
              end if
              if( info/=0_ilp ) then
                 return
              end if
              do j = 1, nl
                 iwork( idxqi+j ) = j
              end do
              if( ( i==nd ) .and. ( sqre==0_ilp ) ) then
                 sqrei = 0_ilp
              else
                 sqrei = 1_ilp
              end if
              idxqi = idxqi + nlp1
              vfi = vfi + nlp1
              vli = vli + nlp1
              nrp1 = nr + sqrei
              if( icompq==0_ilp ) then
                 call stdlib_dlaset( 'A', nrp1, nrp1, zero, one, work( nwork1 ),smlszp )
                 call stdlib_dlasdq( 'U', sqrei, nr, nrp1, nru, ncc, d( nrf ),e( nrf ), work( &
                 nwork1 ), smlszp,work( nwork2 ), nr, work( nwork2 ), nr,work( nwork2 ), info )
                           
                 itemp = nwork1 + ( nrp1-1 )*smlszp
                 call stdlib_dcopy( nrp1, work( nwork1 ), 1_ilp, work( vfi ), 1_ilp )
                 call stdlib_dcopy( nrp1, work( itemp ), 1_ilp, work( vli ), 1_ilp )
              else
                 call stdlib_dlaset( 'A', nr, nr, zero, one, u( nrf, 1_ilp ), ldu )
                 call stdlib_dlaset( 'A', nrp1, nrp1, zero, one, vt( nrf, 1_ilp ), ldu )
                 call stdlib_dlasdq( 'U', sqrei, nr, nrp1, nr, ncc, d( nrf ),e( nrf ), vt( nrf, 1_ilp &
                           ), ldu, u( nrf, 1_ilp ), ldu,u( nrf, 1_ilp ), ldu, work( nwork1 ), info )
                 call stdlib_dcopy( nrp1, vt( nrf, 1_ilp ), 1_ilp, work( vfi ), 1_ilp )
                 call stdlib_dcopy( nrp1, vt( nrf, nrp1 ), 1_ilp, work( vli ), 1_ilp )
              end if
              if( info/=0_ilp ) then
                 return
              end if
              do j = 1, nr
                 iwork( idxqi+j ) = j
              end do
           end do loop_30
           ! now conquer each subproblem bottom-up.
           j = 2_ilp**nlvl
           loop_50: do lvl = nlvl, 1, -1
              lvl2 = lvl*2_ilp - 1_ilp
              ! find the first node lf and last node ll on
              ! the current level lvl.
              if( lvl==1_ilp ) then
                 lf = 1_ilp
                 ll = 1_ilp
              else
                 lf = 2_ilp**( lvl-1 )
                 ll = 2_ilp*lf - 1_ilp
              end if
              loop_40: do i = lf, ll
                 im1 = i - 1_ilp
                 ic = iwork( inode+im1 )
                 nl = iwork( ndiml+im1 )
                 nr = iwork( ndimr+im1 )
                 nlf = ic - nl
                 nrf = ic + 1_ilp
                 if( i==ll ) then
                    sqrei = sqre
                 else
                    sqrei = 1_ilp
                 end if
                 vfi = vf + nlf - 1_ilp
                 vli = vl + nlf - 1_ilp
                 idxqi = idxq + nlf - 1_ilp
                 alpha = d( ic )
                 beta = e( ic )
                 if( icompq==0_ilp ) then
                    call stdlib_dlasd6( icompq, nl, nr, sqrei, d( nlf ),work( vfi ), work( vli ), &
                    alpha, beta,iwork( idxqi ), perm, givptr( 1_ilp ), givcol,ldgcol, givnum, ldu, &
                    poles, difl, difr, z,k( 1_ilp ), c( 1_ilp ), s( 1_ilp ), work( nwork1 ),iwork( iwk ), &
                              info )
                 else
                    j = j - 1_ilp
                    call stdlib_dlasd6( icompq, nl, nr, sqrei, d( nlf ),work( vfi ), work( vli ), &
                    alpha, beta,iwork( idxqi ), perm( nlf, lvl ),givptr( j ), givcol( nlf, lvl2 ),&
                     ldgcol,givnum( nlf, lvl2 ), ldu,poles( nlf, lvl2 ), difl( nlf, lvl ),difr( &
                     nlf, lvl2 ), z( nlf, lvl ), k( j ),c( j ), s( j ), work( nwork1 ),iwork( iwk &
                               ), info )
                 end if
                 if( info/=0_ilp ) then
                    return
                 end if
              end do loop_40
           end do loop_50
           return
     end subroutine stdlib_dlasda




     pure module subroutine stdlib_slasd6( icompq, nl, nr, sqre, d, vf, vl, alpha, beta,idxq, perm, &
     !! SLASD6 computes the SVD of an updated upper bidiagonal matrix B
     !! obtained by merging two smaller ones by appending a row. This
     !! routine is used only for the problem which requires all singular
     !! values and optionally singular vector matrices in factored form.
     !! B is an N-by-M matrix with N = NL + NR + 1 and M = N + SQRE.
     !! A related subroutine, SLASD1, handles the case in which all singular
     !! values and singular vectors of the bidiagonal matrix are desired.
     !! SLASD6 computes the SVD as follows:
     !! ( D1(in)    0    0       0 )
     !! B = U(in) * (   Z1**T   a   Z2**T    b ) * VT(in)
     !! (   0       0   D2(in)   0 )
     !! = U(out) * ( D(out) 0) * VT(out)
     !! where Z**T = (Z1**T a Z2**T b) = u**T VT**T, and u is a vector of dimension M
     !! with ALPHA and BETA in the NL+1 and NL+2 th entries and zeros
     !! elsewhere; and the entry b is empty if SQRE = 0.
     !! The singular values of B can be computed using D1, D2, the first
     !! components of all the right singular vectors of the lower block, and
     !! the last components of all the right singular vectors of the upper
     !! block. These components are stored and updated in VF and VL,
     !! respectively, in SLASD6. Hence U and VT are not explicitly
     !! referenced.
     !! The singular values are stored in D. The algorithm consists of two
     !! stages:
     !! The first stage consists of deflating the size of the problem
     !! when there are multiple singular values or if there is a zero
     !! in the Z vector. For each such occurrence the dimension of the
     !! secular equation problem is reduced by one. This stage is
     !! performed by the routine SLASD7.
     !! The second stage consists of calculating the updated
     !! singular values. This is done by finding the roots of the
     !! secular equation via the routine SLASD4 (as called by SLASD8).
     !! This routine also updates VF and VL and computes the distances
     !! between the updated singular values and the old singular
     !! values.
     !! SLASD6 is called from SLASDA.
     givptr, givcol, ldgcol, givnum,ldgnum, poles, difl, difr, z, k, c, s, work,iwork, info )
               
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: givptr, info, k
           integer(ilp), intent(in) :: icompq, ldgcol, ldgnum, nl, nr, sqre
           real(sp), intent(inout) :: alpha, beta
           real(sp), intent(out) :: c, s
           ! Array Arguments 
           integer(ilp), intent(out) :: givcol(ldgcol,*), iwork(*), perm(*)
           integer(ilp), intent(inout) :: idxq(*)
           real(sp), intent(inout) :: d(*), vf(*), vl(*)
           real(sp), intent(out) :: difl(*), difr(*), givnum(ldgnum,*), poles(ldgnum,*), work(*), &
                     z(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, idx, idxc, idxp, isigma, ivfw, ivlw, iw, m, n, n1, n2
           real(sp) :: orgnrm
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           n = nl + nr + 1_ilp
           m = n + sqre
           if( ( icompq<0_ilp ) .or. ( icompq>1_ilp ) ) then
              info = -1_ilp
           else if( nl<1_ilp ) then
              info = -2_ilp
           else if( nr<1_ilp ) then
              info = -3_ilp
           else if( ( sqre<0_ilp ) .or. ( sqre>1_ilp ) ) then
              info = -4_ilp
           else if( ldgcol<n ) then
              info = -14_ilp
           else if( ldgnum<n ) then
              info = -16_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SLASD6', -info )
              return
           end if
           ! the following values are for bookkeeping purposes only.  they are
           ! integer pointers which indicate the portion of the workspace
           ! used by a particular array in stdlib_slasd7 and stdlib_slasd8.
           isigma = 1_ilp
           iw = isigma + n
           ivfw = iw + m
           ivlw = ivfw + m
           idx = 1_ilp
           idxc = idx + n
           idxp = idxc + n
           ! scale.
           orgnrm = max( abs( alpha ), abs( beta ) )
           d( nl+1 ) = zero
           do i = 1, n
              if( abs( d( i ) )>orgnrm ) then
                 orgnrm = abs( d( i ) )
              end if
           end do
           call stdlib_slascl( 'G', 0_ilp, 0_ilp, orgnrm, one, n, 1_ilp, d, n, info )
           alpha = alpha / orgnrm
           beta = beta / orgnrm
           ! sort and deflate singular values.
           call stdlib_slasd7( icompq, nl, nr, sqre, k, d, z, work( iw ), vf,work( ivfw ), vl, &
           work( ivlw ), alpha, beta,work( isigma ), iwork( idx ), iwork( idxp ), idxq,perm, &
                     givptr, givcol, ldgcol, givnum, ldgnum, c, s,info )
           ! solve secular equation, compute difl, difr, and update vf, vl.
           call stdlib_slasd8( icompq, k, d, z, vf, vl, difl, difr, ldgnum,work( isigma ), work( &
                     iw ), info )
           ! report the possible convergence failure.
           if( info/=0_ilp ) then
              return
           end if
           ! save the poles if icompq = 1.
           if( icompq==1_ilp ) then
              call stdlib_scopy( k, d, 1_ilp, poles( 1_ilp, 1_ilp ), 1_ilp )
              call stdlib_scopy( k, work( isigma ), 1_ilp, poles( 1_ilp, 2_ilp ), 1_ilp )
           end if
           ! unscale.
           call stdlib_slascl( 'G', 0_ilp, 0_ilp, one, orgnrm, n, 1_ilp, d, n, info )
           ! prepare the idxq sorting permutation.
           n1 = k
           n2 = n - k
           call stdlib_slamrg( n1, n2, d, 1_ilp, -1_ilp, idxq )
           return
     end subroutine stdlib_slasd6

     pure module subroutine stdlib_dlasd6( icompq, nl, nr, sqre, d, vf, vl, alpha, beta,idxq, perm, &
     !! DLASD6 computes the SVD of an updated upper bidiagonal matrix B
     !! obtained by merging two smaller ones by appending a row. This
     !! routine is used only for the problem which requires all singular
     !! values and optionally singular vector matrices in factored form.
     !! B is an N-by-M matrix with N = NL + NR + 1 and M = N + SQRE.
     !! A related subroutine, DLASD1, handles the case in which all singular
     !! values and singular vectors of the bidiagonal matrix are desired.
     !! DLASD6 computes the SVD as follows:
     !! ( D1(in)    0    0       0 )
     !! B = U(in) * (   Z1**T   a   Z2**T    b ) * VT(in)
     !! (   0       0   D2(in)   0 )
     !! = U(out) * ( D(out) 0) * VT(out)
     !! where Z**T = (Z1**T a Z2**T b) = u**T VT**T, and u is a vector of dimension M
     !! with ALPHA and BETA in the NL+1 and NL+2 th entries and zeros
     !! elsewhere; and the entry b is empty if SQRE = 0.
     !! The singular values of B can be computed using D1, D2, the first
     !! components of all the right singular vectors of the lower block, and
     !! the last components of all the right singular vectors of the upper
     !! block. These components are stored and updated in VF and VL,
     !! respectively, in DLASD6. Hence U and VT are not explicitly
     !! referenced.
     !! The singular values are stored in D. The algorithm consists of two
     !! stages:
     !! The first stage consists of deflating the size of the problem
     !! when there are multiple singular values or if there is a zero
     !! in the Z vector. For each such occurrence the dimension of the
     !! secular equation problem is reduced by one. This stage is
     !! performed by the routine DLASD7.
     !! The second stage consists of calculating the updated
     !! singular values. This is done by finding the roots of the
     !! secular equation via the routine DLASD4 (as called by DLASD8).
     !! This routine also updates VF and VL and computes the distances
     !! between the updated singular values and the old singular
     !! values.
     !! DLASD6 is called from DLASDA.
     givptr, givcol, ldgcol, givnum,ldgnum, poles, difl, difr, z, k, c, s, work,iwork, info )
               
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: givptr, info, k
           integer(ilp), intent(in) :: icompq, ldgcol, ldgnum, nl, nr, sqre
           real(dp), intent(inout) :: alpha, beta
           real(dp), intent(out) :: c, s
           ! Array Arguments 
           integer(ilp), intent(out) :: givcol(ldgcol,*), iwork(*), perm(*)
           integer(ilp), intent(inout) :: idxq(*)
           real(dp), intent(inout) :: d(*), vf(*), vl(*)
           real(dp), intent(out) :: difl(*), difr(*), givnum(ldgnum,*), poles(ldgnum,*), work(*), &
                     z(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, idx, idxc, idxp, isigma, ivfw, ivlw, iw, m, n, n1, n2
           real(dp) :: orgnrm
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           n = nl + nr + 1_ilp
           m = n + sqre
           if( ( icompq<0_ilp ) .or. ( icompq>1_ilp ) ) then
              info = -1_ilp
           else if( nl<1_ilp ) then
              info = -2_ilp
           else if( nr<1_ilp ) then
              info = -3_ilp
           else if( ( sqre<0_ilp ) .or. ( sqre>1_ilp ) ) then
              info = -4_ilp
           else if( ldgcol<n ) then
              info = -14_ilp
           else if( ldgnum<n ) then
              info = -16_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DLASD6', -info )
              return
           end if
           ! the following values are for bookkeeping purposes only.  they are
           ! integer pointers which indicate the portion of the workspace
           ! used by a particular array in stdlib_dlasd7 and stdlib_dlasd8.
           isigma = 1_ilp
           iw = isigma + n
           ivfw = iw + m
           ivlw = ivfw + m
           idx = 1_ilp
           idxc = idx + n
           idxp = idxc + n
           ! scale.
           orgnrm = max( abs( alpha ), abs( beta ) )
           d( nl+1 ) = zero
           do i = 1, n
              if( abs( d( i ) )>orgnrm ) then
                 orgnrm = abs( d( i ) )
              end if
           end do
           call stdlib_dlascl( 'G', 0_ilp, 0_ilp, orgnrm, one, n, 1_ilp, d, n, info )
           alpha = alpha / orgnrm
           beta = beta / orgnrm
           ! sort and deflate singular values.
           call stdlib_dlasd7( icompq, nl, nr, sqre, k, d, z, work( iw ), vf,work( ivfw ), vl, &
           work( ivlw ), alpha, beta,work( isigma ), iwork( idx ), iwork( idxp ), idxq,perm, &
                     givptr, givcol, ldgcol, givnum, ldgnum, c, s,info )
           ! solve secular equation, compute difl, difr, and update vf, vl.
           call stdlib_dlasd8( icompq, k, d, z, vf, vl, difl, difr, ldgnum,work( isigma ), work( &
                     iw ), info )
           ! report the possible convergence failure.
           if( info/=0_ilp ) then
              return
           end if
           ! save the poles if icompq = 1.
           if( icompq==1_ilp ) then
              call stdlib_dcopy( k, d, 1_ilp, poles( 1_ilp, 1_ilp ), 1_ilp )
              call stdlib_dcopy( k, work( isigma ), 1_ilp, poles( 1_ilp, 2_ilp ), 1_ilp )
           end if
           ! unscale.
           call stdlib_dlascl( 'G', 0_ilp, 0_ilp, one, orgnrm, n, 1_ilp, d, n, info )
           ! prepare the idxq sorting permutation.
           n1 = k
           n2 = n - k
           call stdlib_dlamrg( n1, n2, d, 1_ilp, -1_ilp, idxq )
           return
     end subroutine stdlib_dlasd6




     pure module subroutine stdlib_slasd7( icompq, nl, nr, sqre, k, d, z, zw, vf, vfw, vl,vlw, alpha, &
     !! SLASD7 merges the two sets of singular values together into a single
     !! sorted set. Then it tries to deflate the size of the problem. There
     !! are two ways in which deflation can occur:  when two or more singular
     !! values are close together or if there is a tiny entry in the Z
     !! vector. For each such occurrence the order of the related
     !! secular equation problem is reduced by one.
     !! SLASD7 is called from SLASD6.
     beta, dsigma, idx, idxp, idxq,perm, givptr, givcol, ldgcol, givnum, ldgnum,c, s, info )
               
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: givptr, info, k
           integer(ilp), intent(in) :: icompq, ldgcol, ldgnum, nl, nr, sqre
           real(sp), intent(in) :: alpha, beta
           real(sp), intent(out) :: c, s
           ! Array Arguments 
           integer(ilp), intent(out) :: givcol(ldgcol,*), idx(*), idxp(*), perm(*)
           integer(ilp), intent(inout) :: idxq(*)
           real(sp), intent(inout) :: d(*), vf(*), vl(*)
           real(sp), intent(out) :: dsigma(*), givnum(ldgnum,*), vfw(*), vlw(*), z(*), zw(*)
                     
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, idxi, idxj, idxjp, j, jp, jprev, k2, m, n, nlp1, nlp2
           real(sp) :: eps, hlftol, tau, tol, z1
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           n = nl + nr + 1_ilp
           m = n + sqre
           if( ( icompq<0_ilp ) .or. ( icompq>1_ilp ) ) then
              info = -1_ilp
           else if( nl<1_ilp ) then
              info = -2_ilp
           else if( nr<1_ilp ) then
              info = -3_ilp
           else if( ( sqre<0_ilp ) .or. ( sqre>1_ilp ) ) then
              info = -4_ilp
           else if( ldgcol<n ) then
              info = -22_ilp
           else if( ldgnum<n ) then
              info = -24_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SLASD7', -info )
              return
           end if
           nlp1 = nl + 1_ilp
           nlp2 = nl + 2_ilp
           if( icompq==1_ilp ) then
              givptr = 0_ilp
           end if
           ! generate the first part of the vector z and move the singular
           ! values in the first part of d one position backward.
           z1 = alpha*vl( nlp1 )
           vl( nlp1 ) = zero
           tau = vf( nlp1 )
           do i = nl, 1, -1
              z( i+1 ) = alpha*vl( i )
              vl( i ) = zero
              vf( i+1 ) = vf( i )
              d( i+1 ) = d( i )
              idxq( i+1 ) = idxq( i ) + 1_ilp
           end do
           vf( 1_ilp ) = tau
           ! generate the second part of the vector z.
           do i = nlp2, m
              z( i ) = beta*vf( i )
              vf( i ) = zero
           end do
           ! sort the singular values into increasing order
           do i = nlp2, n
              idxq( i ) = idxq( i ) + nlp1
           end do
           ! dsigma, idxc, idxc, and zw are used as storage space.
           do i = 2, n
              dsigma( i ) = d( idxq( i ) )
              zw( i ) = z( idxq( i ) )
              vfw( i ) = vf( idxq( i ) )
              vlw( i ) = vl( idxq( i ) )
           end do
           call stdlib_slamrg( nl, nr, dsigma( 2_ilp ), 1_ilp, 1_ilp, idx( 2_ilp ) )
           do i = 2, n
              idxi = 1_ilp + idx( i )
              d( i ) = dsigma( idxi )
              z( i ) = zw( idxi )
              vf( i ) = vfw( idxi )
              vl( i ) = vlw( idxi )
           end do
           ! calculate the allowable deflation tolerance
           eps = stdlib_slamch( 'EPSILON' )
           tol = max( abs( alpha ), abs( beta ) )
           tol = eight*eight*eps*max( abs( d( n ) ), tol )
           ! there are 2 kinds of deflation -- first a value in the z-vector
           ! is small, second two (or more) singular values are very close
           ! together (their difference is small).
           ! if the value in the z-vector is small, we simply permute the
           ! array so that the corresponding singular value is moved to the
           ! end.
           ! if two values in the d-vector are close, we perform a two-sided
           ! rotation designed to make one of the corresponding z-vector
           ! entries zero, and then permute the array so that the deflated
           ! singular value is moved to the end.
           ! if there are multiple singular values then the problem deflates.
           ! here the number of equal singular values are found.  as each equal
           ! singular value is found, an elementary reflector is computed to
           ! rotate the corresponding singular subspace so that the
           ! corresponding components of z are zero in this new basis.
           k = 1_ilp
           k2 = n + 1_ilp
           do j = 2, n
              if( abs( z( j ) )<=tol ) then
                 ! deflate due to small z component.
                 k2 = k2 - 1_ilp
                 idxp( k2 ) = j
                 if( j==n )go to 100
              else
                 jprev = j
                 go to 70
              end if
           end do
           70 continue
           j = jprev
           80 continue
           j = j + 1_ilp
           if( j>n )go to 90
           if( abs( z( j ) )<=tol ) then
              ! deflate due to small z component.
              k2 = k2 - 1_ilp
              idxp( k2 ) = j
           else
              ! check if singular values are close enough to allow deflation.
              if( abs( d( j )-d( jprev ) )<=tol ) then
                 ! deflation is possible.
                 s = z( jprev )
                 c = z( j )
                 ! find sqrt(a**2+b**2) without overflow or
                 ! destructive underflow.
                 tau = stdlib_slapy2( c, s )
                 z( j ) = tau
                 z( jprev ) = zero
                 c = c / tau
                 s = -s / tau
                 ! record the appropriate givens rotation
                 if( icompq==1_ilp ) then
                    givptr = givptr + 1_ilp
                    idxjp = idxq( idx( jprev )+1_ilp )
                    idxj = idxq( idx( j )+1_ilp )
                    if( idxjp<=nlp1 ) then
                       idxjp = idxjp - 1_ilp
                    end if
                    if( idxj<=nlp1 ) then
                       idxj = idxj - 1_ilp
                    end if
                    givcol( givptr, 2_ilp ) = idxjp
                    givcol( givptr, 1_ilp ) = idxj
                    givnum( givptr, 2_ilp ) = c
                    givnum( givptr, 1_ilp ) = s
                 end if
                 call stdlib_srot( 1_ilp, vf( jprev ), 1_ilp, vf( j ), 1_ilp, c, s )
                 call stdlib_srot( 1_ilp, vl( jprev ), 1_ilp, vl( j ), 1_ilp, c, s )
                 k2 = k2 - 1_ilp
                 idxp( k2 ) = jprev
                 jprev = j
              else
                 k = k + 1_ilp
                 zw( k ) = z( jprev )
                 dsigma( k ) = d( jprev )
                 idxp( k ) = jprev
                 jprev = j
              end if
           end if
           go to 80
           90 continue
           ! record the last singular value.
           k = k + 1_ilp
           zw( k ) = z( jprev )
           dsigma( k ) = d( jprev )
           idxp( k ) = jprev
           100 continue
           ! sort the singular values into dsigma. the singular values which
           ! were not deflated go into the first k slots of dsigma, except
           ! that dsigma(1) is treated separately.
           do j = 2, n
              jp = idxp( j )
              dsigma( j ) = d( jp )
              vfw( j ) = vf( jp )
              vlw( j ) = vl( jp )
           end do
           if( icompq==1_ilp ) then
              do j = 2, n
                 jp = idxp( j )
                 perm( j ) = idxq( idx( jp )+1_ilp )
                 if( perm( j )<=nlp1 ) then
                    perm( j ) = perm( j ) - 1_ilp
                 end if
              end do
           end if
           ! the deflated singular values go back into the last n - k slots of
           ! d.
           call stdlib_scopy( n-k, dsigma( k+1 ), 1_ilp, d( k+1 ), 1_ilp )
           ! determine dsigma(1), dsigma(2), z(1), vf(1), vl(1), vf(m), and
           ! vl(m).
           dsigma( 1_ilp ) = zero
           hlftol = tol / two
           if( abs( dsigma( 2_ilp ) )<=hlftol )dsigma( 2_ilp ) = hlftol
           if( m>n ) then
              z( 1_ilp ) = stdlib_slapy2( z1, z( m ) )
              if( z( 1_ilp )<=tol ) then
                 c = one
                 s = zero
                 z( 1_ilp ) = tol
              else
                 c = z1 / z( 1_ilp )
                 s = -z( m ) / z( 1_ilp )
              end if
              call stdlib_srot( 1_ilp, vf( m ), 1_ilp, vf( 1_ilp ), 1_ilp, c, s )
              call stdlib_srot( 1_ilp, vl( m ), 1_ilp, vl( 1_ilp ), 1_ilp, c, s )
           else
              if( abs( z1 )<=tol ) then
                 z( 1_ilp ) = tol
              else
                 z( 1_ilp ) = z1
              end if
           end if
           ! restore z, vf, and vl.
           call stdlib_scopy( k-1, zw( 2_ilp ), 1_ilp, z( 2_ilp ), 1_ilp )
           call stdlib_scopy( n-1, vfw( 2_ilp ), 1_ilp, vf( 2_ilp ), 1_ilp )
           call stdlib_scopy( n-1, vlw( 2_ilp ), 1_ilp, vl( 2_ilp ), 1_ilp )
           return
     end subroutine stdlib_slasd7

     pure module subroutine stdlib_dlasd7( icompq, nl, nr, sqre, k, d, z, zw, vf, vfw, vl,vlw, alpha, &
     !! DLASD7 merges the two sets of singular values together into a single
     !! sorted set. Then it tries to deflate the size of the problem. There
     !! are two ways in which deflation can occur:  when two or more singular
     !! values are close together or if there is a tiny entry in the Z
     !! vector. For each such occurrence the order of the related
     !! secular equation problem is reduced by one.
     !! DLASD7 is called from DLASD6.
     beta, dsigma, idx, idxp, idxq,perm, givptr, givcol, ldgcol, givnum, ldgnum,c, s, info )
               
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(out) :: givptr, info, k
           integer(ilp), intent(in) :: icompq, ldgcol, ldgnum, nl, nr, sqre
           real(dp), intent(in) :: alpha, beta
           real(dp), intent(out) :: c, s
           ! Array Arguments 
           integer(ilp), intent(out) :: givcol(ldgcol,*), idx(*), idxp(*), perm(*)
           integer(ilp), intent(inout) :: idxq(*)
           real(dp), intent(inout) :: d(*), vf(*), vl(*)
           real(dp), intent(out) :: dsigma(*), givnum(ldgnum,*), vfw(*), vlw(*), z(*), zw(*)
                     
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, idxi, idxj, idxjp, j, jp, jprev, k2, m, n, nlp1, nlp2
           real(dp) :: eps, hlftol, tau, tol, z1
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           n = nl + nr + 1_ilp
           m = n + sqre
           if( ( icompq<0_ilp ) .or. ( icompq>1_ilp ) ) then
              info = -1_ilp
           else if( nl<1_ilp ) then
              info = -2_ilp
           else if( nr<1_ilp ) then
              info = -3_ilp
           else if( ( sqre<0_ilp ) .or. ( sqre>1_ilp ) ) then
              info = -4_ilp
           else if( ldgcol<n ) then
              info = -22_ilp
           else if( ldgnum<n ) then
              info = -24_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DLASD7', -info )
              return
           end if
           nlp1 = nl + 1_ilp
           nlp2 = nl + 2_ilp
           if( icompq==1_ilp ) then
              givptr = 0_ilp
           end if
           ! generate the first part of the vector z and move the singular
           ! values in the first part of d one position backward.
           z1 = alpha*vl( nlp1 )
           vl( nlp1 ) = zero
           tau = vf( nlp1 )
           do i = nl, 1, -1
              z( i+1 ) = alpha*vl( i )
              vl( i ) = zero
              vf( i+1 ) = vf( i )
              d( i+1 ) = d( i )
              idxq( i+1 ) = idxq( i ) + 1_ilp
           end do
           vf( 1_ilp ) = tau
           ! generate the second part of the vector z.
           do i = nlp2, m
              z( i ) = beta*vf( i )
              vf( i ) = zero
           end do
           ! sort the singular values into increasing order
           do i = nlp2, n
              idxq( i ) = idxq( i ) + nlp1
           end do
           ! dsigma, idxc, idxc, and zw are used as storage space.
           do i = 2, n
              dsigma( i ) = d( idxq( i ) )
              zw( i ) = z( idxq( i ) )
              vfw( i ) = vf( idxq( i ) )
              vlw( i ) = vl( idxq( i ) )
           end do
           call stdlib_dlamrg( nl, nr, dsigma( 2_ilp ), 1_ilp, 1_ilp, idx( 2_ilp ) )
           do i = 2, n
              idxi = 1_ilp + idx( i )
              d( i ) = dsigma( idxi )
              z( i ) = zw( idxi )
              vf( i ) = vfw( idxi )
              vl( i ) = vlw( idxi )
           end do
           ! calculate the allowable deflation tolerance
           eps = stdlib_dlamch( 'EPSILON' )
           tol = max( abs( alpha ), abs( beta ) )
           tol = eight*eight*eps*max( abs( d( n ) ), tol )
           ! there are 2 kinds of deflation -- first a value in the z-vector
           ! is small, second two (or more) singular values are very close
           ! together (their difference is small).
           ! if the value in the z-vector is small, we simply permute the
           ! array so that the corresponding singular value is moved to the
           ! end.
           ! if two values in the d-vector are close, we perform a two-sided
           ! rotation designed to make one of the corresponding z-vector
           ! entries zero, and then permute the array so that the deflated
           ! singular value is moved to the end.
           ! if there are multiple singular values then the problem deflates.
           ! here the number of equal singular values are found.  as each equal
           ! singular value is found, an elementary reflector is computed to
           ! rotate the corresponding singular subspace so that the
           ! corresponding components of z are zero in this new basis.
           k = 1_ilp
           k2 = n + 1_ilp
           do j = 2, n
              if( abs( z( j ) )<=tol ) then
                 ! deflate due to small z component.
                 k2 = k2 - 1_ilp
                 idxp( k2 ) = j
                 if( j==n )go to 100
              else
                 jprev = j
                 go to 70
              end if
           end do
           70 continue
           j = jprev
           80 continue
           j = j + 1_ilp
           if( j>n )go to 90
           if( abs( z( j ) )<=tol ) then
              ! deflate due to small z component.
              k2 = k2 - 1_ilp
              idxp( k2 ) = j
           else
              ! check if singular values are close enough to allow deflation.
              if( abs( d( j )-d( jprev ) )<=tol ) then
                 ! deflation is possible.
                 s = z( jprev )
                 c = z( j )
                 ! find sqrt(a**2+b**2) without overflow or
                 ! destructive underflow.
                 tau = stdlib_dlapy2( c, s )
                 z( j ) = tau
                 z( jprev ) = zero
                 c = c / tau
                 s = -s / tau
                 ! record the appropriate givens rotation
                 if( icompq==1_ilp ) then
                    givptr = givptr + 1_ilp
                    idxjp = idxq( idx( jprev )+1_ilp )
                    idxj = idxq( idx( j )+1_ilp )
                    if( idxjp<=nlp1 ) then
                       idxjp = idxjp - 1_ilp
                    end if
                    if( idxj<=nlp1 ) then
                       idxj = idxj - 1_ilp
                    end if
                    givcol( givptr, 2_ilp ) = idxjp
                    givcol( givptr, 1_ilp ) = idxj
                    givnum( givptr, 2_ilp ) = c
                    givnum( givptr, 1_ilp ) = s
                 end if
                 call stdlib_drot( 1_ilp, vf( jprev ), 1_ilp, vf( j ), 1_ilp, c, s )
                 call stdlib_drot( 1_ilp, vl( jprev ), 1_ilp, vl( j ), 1_ilp, c, s )
                 k2 = k2 - 1_ilp
                 idxp( k2 ) = jprev
                 jprev = j
              else
                 k = k + 1_ilp
                 zw( k ) = z( jprev )
                 dsigma( k ) = d( jprev )
                 idxp( k ) = jprev
                 jprev = j
              end if
           end if
           go to 80
           90 continue
           ! record the last singular value.
           k = k + 1_ilp
           zw( k ) = z( jprev )
           dsigma( k ) = d( jprev )
           idxp( k ) = jprev
           100 continue
           ! sort the singular values into dsigma. the singular values which
           ! were not deflated go into the first k slots of dsigma, except
           ! that dsigma(1) is treated separately.
           do j = 2, n
              jp = idxp( j )
              dsigma( j ) = d( jp )
              vfw( j ) = vf( jp )
              vlw( j ) = vl( jp )
           end do
           if( icompq==1_ilp ) then
              do j = 2, n
                 jp = idxp( j )
                 perm( j ) = idxq( idx( jp )+1_ilp )
                 if( perm( j )<=nlp1 ) then
                    perm( j ) = perm( j ) - 1_ilp
                 end if
              end do
           end if
           ! the deflated singular values go back into the last n - k slots of
           ! d.
           call stdlib_dcopy( n-k, dsigma( k+1 ), 1_ilp, d( k+1 ), 1_ilp )
           ! determine dsigma(1), dsigma(2), z(1), vf(1), vl(1), vf(m), and
           ! vl(m).
           dsigma( 1_ilp ) = zero
           hlftol = tol / two
           if( abs( dsigma( 2_ilp ) )<=hlftol )dsigma( 2_ilp ) = hlftol
           if( m>n ) then
              z( 1_ilp ) = stdlib_dlapy2( z1, z( m ) )
              if( z( 1_ilp )<=tol ) then
                 c = one
                 s = zero
                 z( 1_ilp ) = tol
              else
                 c = z1 / z( 1_ilp )
                 s = -z( m ) / z( 1_ilp )
              end if
              call stdlib_drot( 1_ilp, vf( m ), 1_ilp, vf( 1_ilp ), 1_ilp, c, s )
              call stdlib_drot( 1_ilp, vl( m ), 1_ilp, vl( 1_ilp ), 1_ilp, c, s )
           else
              if( abs( z1 )<=tol ) then
                 z( 1_ilp ) = tol
              else
                 z( 1_ilp ) = z1
              end if
           end if
           ! restore z, vf, and vl.
           call stdlib_dcopy( k-1, zw( 2_ilp ), 1_ilp, z( 2_ilp ), 1_ilp )
           call stdlib_dcopy( n-1, vfw( 2_ilp ), 1_ilp, vf( 2_ilp ), 1_ilp )
           call stdlib_dcopy( n-1, vlw( 2_ilp ), 1_ilp, vl( 2_ilp ), 1_ilp )
           return
     end subroutine stdlib_dlasd7




     pure module subroutine stdlib_slasd8( icompq, k, d, z, vf, vl, difl, difr, lddifr,dsigma, work, &
     !! SLASD8 finds the square roots of the roots of the secular equation,
     !! as defined by the values in DSIGMA and Z. It makes the appropriate
     !! calls to SLASD4, and stores, for each  element in D, the distance
     !! to its two nearest poles (elements in DSIGMA). It also updates
     !! the arrays VF and VL, the first and last components of all the
     !! right singular vectors of the original bidiagonal matrix.
     !! SLASD8 is called from SLASD6.
               info )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_sp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: icompq, k, lddifr
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           real(sp), intent(out) :: d(*), difl(*), difr(lddifr,*), work(*)
           real(sp), intent(inout) :: dsigma(*), vf(*), vl(*), z(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, iwk1, iwk2, iwk2i, iwk3, iwk3i, j
           real(sp) :: diflj, difrj, dj, dsigj, dsigjp, rho, temp
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( ( icompq<0_ilp ) .or. ( icompq>1_ilp ) ) then
              info = -1_ilp
           else if( k<1_ilp ) then
              info = -2_ilp
           else if( lddifr<k ) then
              info = -9_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'SLASD8', -info )
              return
           end if
           ! quick return if possible
           if( k==1_ilp ) then
              d( 1_ilp ) = abs( z( 1_ilp ) )
              difl( 1_ilp ) = d( 1_ilp )
              if( icompq==1_ilp ) then
                 difl( 2_ilp ) = one
                 difr( 1_ilp, 2_ilp ) = one
              end if
              return
           end if
           ! modify values dsigma(i) to make sure all dsigma(i)-dsigma(j) can
           ! be computed with high relative accuracy (barring over/underflow).
           ! this is a problem on machines without a guard digit in
           ! add/subtract (cray xmp, cray ymp, cray c 90 and cray 2).
           ! the following code replaces dsigma(i) by 2*dsigma(i)-dsigma(i),
           ! which on any of these machines zeros out the bottommost
           ! bit of dsigma(i) if it is 1; this makes the subsequent
           ! subtractions dsigma(i)-dsigma(j) unproblematic when cancellation
           ! occurs. on binary machines with a guard digit (almost all
           ! machines) it does not change dsigma(i) at all. on hexadecimal
           ! and decimal machines with a guard digit, it slightly
           ! changes the bottommost bits of dsigma(i). it does not account
           ! for hexadecimal or decimal machines without guard digits
           ! (we know of none). we use a subroutine call to compute
           ! 2*dlambda(i) to prevent optimizing compilers from eliminating
           ! this code.
           do i = 1, k
              dsigma( i ) = stdlib_slamc3( dsigma( i ), dsigma( i ) ) - dsigma( i )
           end do
           ! book keeping.
           iwk1 = 1_ilp
           iwk2 = iwk1 + k
           iwk3 = iwk2 + k
           iwk2i = iwk2 - 1_ilp
           iwk3i = iwk3 - 1_ilp
           ! normalize z.
           rho = stdlib_snrm2( k, z, 1_ilp )
           call stdlib_slascl( 'G', 0_ilp, 0_ilp, rho, one, k, 1_ilp, z, k, info )
           rho = rho*rho
           ! initialize work(iwk3).
           call stdlib_slaset( 'A', k, 1_ilp, one, one, work( iwk3 ), k )
           ! compute the updated singular values, the arrays difl, difr,
           ! and the updated z.
           do j = 1, k
              call stdlib_slasd4( k, j, dsigma, z, work( iwk1 ), rho, d( j ),work( iwk2 ), info )
                        
              ! if the root finder fails, report the convergence failure.
              if( info/=0_ilp ) then
                 return
              end if
              work( iwk3i+j ) = work( iwk3i+j )*work( j )*work( iwk2i+j )
              difl( j ) = -work( j )
              difr( j, 1_ilp ) = -work( j+1 )
              do i = 1, j - 1
                 work( iwk3i+i ) = work( iwk3i+i )*work( i )*work( iwk2i+i ) / ( dsigma( i )-&
                           dsigma( j ) ) / ( dsigma( i )+dsigma( j ) )
              end do
              do i = j + 1, k
                 work( iwk3i+i ) = work( iwk3i+i )*work( i )*work( iwk2i+i ) / ( dsigma( i )-&
                           dsigma( j ) ) / ( dsigma( i )+dsigma( j ) )
              end do
           end do
           ! compute updated z.
           do i = 1, k
              z( i ) = sign( sqrt( abs( work( iwk3i+i ) ) ), z( i ) )
           end do
           ! update vf and vl.
           do j = 1, k
              diflj = difl( j )
              dj = d( j )
              dsigj = -dsigma( j )
              if( j<k ) then
                 difrj = -difr( j, 1_ilp )
                 dsigjp = -dsigma( j+1 )
              end if
              work( j ) = -z( j ) / diflj / ( dsigma( j )+dj )
              do i = 1, j - 1
                 work( i ) = z( i ) / ( stdlib_slamc3( dsigma( i ), dsigj )-diflj )/ ( dsigma( i )&
                           +dj )
              end do
              do i = j + 1, k
                 work( i ) = z( i ) / ( stdlib_slamc3( dsigma( i ), dsigjp )+difrj )/ ( dsigma( i &
                           )+dj )
              end do
              temp = stdlib_snrm2( k, work, 1_ilp )
              work( iwk2i+j ) = stdlib_sdot( k, work, 1_ilp, vf, 1_ilp ) / temp
              work( iwk3i+j ) = stdlib_sdot( k, work, 1_ilp, vl, 1_ilp ) / temp
              if( icompq==1_ilp ) then
                 difr( j, 2_ilp ) = temp
              end if
           end do
           call stdlib_scopy( k, work( iwk2 ), 1_ilp, vf, 1_ilp )
           call stdlib_scopy( k, work( iwk3 ), 1_ilp, vl, 1_ilp )
           return
     end subroutine stdlib_slasd8

     pure module subroutine stdlib_dlasd8( icompq, k, d, z, vf, vl, difl, difr, lddifr,dsigma, work, &
     !! DLASD8 finds the square roots of the roots of the secular equation,
     !! as defined by the values in DSIGMA and Z. It makes the appropriate
     !! calls to DLASD4, and stores, for each  element in D, the distance
     !! to its two nearest poles (elements in DSIGMA). It also updates
     !! the arrays VF and VL, the first and last components of all the
     !! right singular vectors of the original bidiagonal matrix.
     !! DLASD8 is called from DLASD6.
               info )
        ! -- lapack auxiliary routine --
        ! -- lapack is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           use stdlib_blas_constants_dp, only: negone, zero, half, one, two, three, four, eight, ten, czero, chalf, cone, cnegone
           ! Scalar Arguments 
           integer(ilp), intent(in) :: icompq, k, lddifr
           integer(ilp), intent(out) :: info
           ! Array Arguments 
           real(dp), intent(out) :: d(*), difl(*), difr(lddifr,*), work(*)
           real(dp), intent(inout) :: dsigma(*), vf(*), vl(*), z(*)
        ! =====================================================================
           
           ! Local Scalars 
           integer(ilp) :: i, iwk1, iwk2, iwk2i, iwk3, iwk3i, j
           real(dp) :: diflj, difrj, dj, dsigj, dsigjp, rho, temp
           ! Intrinsic Functions 
           ! Executable Statements 
           ! test the input parameters.
           info = 0_ilp
           if( ( icompq<0_ilp ) .or. ( icompq>1_ilp ) ) then
              info = -1_ilp
           else if( k<1_ilp ) then
              info = -2_ilp
           else if( lddifr<k ) then
              info = -9_ilp
           end if
           if( info/=0_ilp ) then
              call stdlib_xerbla( 'DLASD8', -info )
              return
           end if
           ! quick return if possible
           if( k==1_ilp ) then
              d( 1_ilp ) = abs( z( 1_ilp ) )
              difl( 1_ilp ) = d( 1_ilp )
              if( icompq==1_ilp ) then
                 difl( 2_ilp ) = one
                 difr( 1_ilp, 2_ilp ) = one
              end if
              return
           end if
           ! modify values dsigma(i) to make sure all dsigma(i)-dsigma(j) can
           ! be computed with high relative accuracy (barring over/underflow).
           ! this is a problem on machines without a guard digit in
           ! add/subtract (cray xmp, cray ymp, cray c 90 and cray 2).
           ! the following code replaces dsigma(i) by 2*dsigma(i)-dsigma(i),
           ! which on any of these machines zeros out the bottommost
           ! bit of dsigma(i) if it is 1; this makes the subsequent
           ! subtractions dsigma(i)-dsigma(j) unproblematic when cancellation
           ! occurs. on binary machines with a guard digit (almost all
           ! machines) it does not change dsigma(i) at all. on hexadecimal
           ! and decimal machines with a guard digit, it slightly
           ! changes the bottommost bits of dsigma(i). it does not account
           ! for hexadecimal or decimal machines without guard digits
           ! (we know of none). we use a subroutine call to compute
           ! 2*dlambda(i) to prevent optimizing compilers from eliminating
           ! this code.
           do i = 1, k
              dsigma( i ) = stdlib_dlamc3( dsigma( i ), dsigma( i ) ) - dsigma( i )
           end do
           ! book keeping.
           iwk1 = 1_ilp
           iwk2 = iwk1 + k
           iwk3 = iwk2 + k
           iwk2i = iwk2 - 1_ilp
           iwk3i = iwk3 - 1_ilp
           ! normalize z.
           rho = stdlib_dnrm2( k, z, 1_ilp )
           call stdlib_dlascl( 'G', 0_ilp, 0_ilp, rho, one, k, 1_ilp, z, k, info )
           rho = rho*rho
           ! initialize work(iwk3).
           call stdlib_dlaset( 'A', k, 1_ilp, one, one, work( iwk3 ), k )
           ! compute the updated singular values, the arrays difl, difr,
           ! and the updated z.
           do j = 1, k
              call stdlib_dlasd4( k, j, dsigma, z, work( iwk1 ), rho, d( j ),work( iwk2 ), info )
                        
              ! if the root finder fails, report the convergence failure.
              if( info/=0_ilp ) then
                 return
              end if
              work( iwk3i+j ) = work( iwk3i+j )*work( j )*work( iwk2i+j )
              difl( j ) = -work( j )
              difr( j, 1_ilp ) = -work( j+1 )
              do i = 1, j - 1
                 work( iwk3i+i ) = work( iwk3i+i )*work( i )*work( iwk2i+i ) / ( dsigma( i )-&
                           dsigma( j ) ) / ( dsigma( i )+dsigma( j ) )
              end do
              do i = j + 1, k
                 work( iwk3i+i ) = work( iwk3i+i )*work( i )*work( iwk2i+i ) / ( dsigma( i )-&
                           dsigma( j ) ) / ( dsigma( i )+dsigma( j ) )
              end do
           end do
           ! compute updated z.
           do i = 1, k
              z( i ) = sign( sqrt( abs( work( iwk3i+i ) ) ), z( i ) )
           end do
           ! update vf and vl.
           do j = 1, k
              diflj = difl( j )
              dj = d( j )
              dsigj = -dsigma( j )
              if( j<k ) then
                 difrj = -difr( j, 1_ilp )
                 dsigjp = -dsigma( j+1 )
              end if
              work( j ) = -z( j ) / diflj / ( dsigma( j )+dj )
              do i = 1, j - 1
                 work( i ) = z( i ) / ( stdlib_dlamc3( dsigma( i ), dsigj )-diflj )/ ( dsigma( i )&
                           +dj )
              end do
              do i = j + 1, k
                 work( i ) = z( i ) / ( stdlib_dlamc3( dsigma( i ), dsigjp )+difrj )/ ( dsigma( i &
                           )+dj )
              end do
              temp = stdlib_dnrm2( k, work, 1_ilp )
              work( iwk2i+j ) = stdlib_ddot( k, work, 1_ilp, vf, 1_ilp ) / temp
              work( iwk3i+j ) = stdlib_ddot( k, work, 1_ilp, vl, 1_ilp ) / temp
              if( icompq==1_ilp ) then
                 difr( j, 2_ilp ) = temp
              end if
           end do
           call stdlib_dcopy( k, work( iwk2 ), 1_ilp, vf, 1_ilp )
           call stdlib_dcopy( k, work( iwk3 ), 1_ilp, vl, 1_ilp )
           return
     end subroutine stdlib_dlasd8



end submodule stdlib_lapack_eigv_svd_bidiag_dc
