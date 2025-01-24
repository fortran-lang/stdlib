module stdlib_lapack_eig_svd_lsq
  use stdlib_linalg_constants
  use stdlib_linalg_lapack_aux
  use stdlib_linalg_blas
  use stdlib_lapack_base
  use stdlib_lapack_orthogonal_factors
  use stdlib_lapack_solve
  implicit none

interface 
     pure module subroutine stdlib_sgebrd( m, n, a, lda, d, e, tauq, taup, work, lwork,info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, m, n
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: d(*), e(*), taup(*), tauq(*), work(*)
     end subroutine stdlib_sgebrd

     pure module subroutine stdlib_dgebrd( m, n, a, lda, d, e, tauq, taup, work, lwork,info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, m, n
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: d(*), e(*), taup(*), tauq(*), work(*)
     end subroutine stdlib_dgebrd


     pure module subroutine stdlib_cgebrd( m, n, a, lda, d, e, tauq, taup, work, lwork,info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, m, n
           real(sp), intent(out) :: d(*), e(*)
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: taup(*), tauq(*), work(*)
     end subroutine stdlib_cgebrd

     pure module subroutine stdlib_zgebrd( m, n, a, lda, d, e, tauq, taup, work, lwork,info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, m, n
           real(dp), intent(out) :: d(*), e(*)
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: taup(*), tauq(*), work(*)
     end subroutine stdlib_zgebrd


end interface 


interface 
     pure module subroutine stdlib_sgebd2( m, n, a, lda, d, e, tauq, taup, work, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: d(*), e(*), taup(*), tauq(*), work(*)
     end subroutine stdlib_sgebd2

     pure module subroutine stdlib_dgebd2( m, n, a, lda, d, e, tauq, taup, work, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: d(*), e(*), taup(*), tauq(*), work(*)
     end subroutine stdlib_dgebd2


     pure module subroutine stdlib_cgebd2( m, n, a, lda, d, e, tauq, taup, work, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n
           real(sp), intent(out) :: d(*), e(*)
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: taup(*), tauq(*), work(*)
     end subroutine stdlib_cgebd2

     pure module subroutine stdlib_zgebd2( m, n, a, lda, d, e, tauq, taup, work, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n
           real(dp), intent(out) :: d(*), e(*)
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: taup(*), tauq(*), work(*)
     end subroutine stdlib_zgebd2


end interface 


interface 
     pure module subroutine stdlib_sgbbrd( vect, m, n, ncc, kl, ku, ab, ldab, d, e, q,ldq, pt, ldpt, c, &
               ldc, work, info )
           character, intent(in) :: vect
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kl, ku, ldab, ldc, ldpt, ldq, m, n, ncc
           real(sp), intent(inout) :: ab(ldab,*), c(ldc,*)
           real(sp), intent(out) :: d(*), e(*), pt(ldpt,*), q(ldq,*), work(*)
     end subroutine stdlib_sgbbrd

     pure module subroutine stdlib_dgbbrd( vect, m, n, ncc, kl, ku, ab, ldab, d, e, q,ldq, pt, ldpt, c, &
               ldc, work, info )
           character, intent(in) :: vect
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kl, ku, ldab, ldc, ldpt, ldq, m, n, ncc
           real(dp), intent(inout) :: ab(ldab,*), c(ldc,*)
           real(dp), intent(out) :: d(*), e(*), pt(ldpt,*), q(ldq,*), work(*)
     end subroutine stdlib_dgbbrd


     pure module subroutine stdlib_cgbbrd( vect, m, n, ncc, kl, ku, ab, ldab, d, e, q,ldq, pt, ldpt, c, &
               ldc, work, rwork, info )
           character, intent(in) :: vect
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kl, ku, ldab, ldc, ldpt, ldq, m, n, ncc
           real(sp), intent(out) :: d(*), e(*), rwork(*)
           complex(sp), intent(inout) :: ab(ldab,*), c(ldc,*)
           complex(sp), intent(out) :: pt(ldpt,*), q(ldq,*), work(*)
     end subroutine stdlib_cgbbrd

     pure module subroutine stdlib_zgbbrd( vect, m, n, ncc, kl, ku, ab, ldab, d, e, q,ldq, pt, ldpt, c, &
               ldc, work, rwork, info )
           character, intent(in) :: vect
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kl, ku, ldab, ldc, ldpt, ldq, m, n, ncc
           real(dp), intent(out) :: d(*), e(*), rwork(*)
           complex(dp), intent(inout) :: ab(ldab,*), c(ldc,*)
           complex(dp), intent(out) :: pt(ldpt,*), q(ldq,*), work(*)
     end subroutine stdlib_zgbbrd


end interface 


interface 
     pure module subroutine stdlib_sgsvj0( jobv, m, n, a, lda, d, sva, mv, v, ldv, eps,sfmin, tol, &
               nsweep, work, lwork, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldv, lwork, m, mv, n, nsweep
           real(sp), intent(in) :: eps, sfmin, tol
           character, intent(in) :: jobv
           real(sp), intent(inout) :: a(lda,*), sva(n), d(n), v(ldv,*)
           real(sp), intent(out) :: work(lwork)
     end subroutine stdlib_sgsvj0

     pure module subroutine stdlib_dgsvj0( jobv, m, n, a, lda, d, sva, mv, v, ldv, eps,sfmin, tol, &
               nsweep, work, lwork, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldv, lwork, m, mv, n, nsweep
           real(dp), intent(in) :: eps, sfmin, tol
           character, intent(in) :: jobv
           real(dp), intent(inout) :: a(lda,*), sva(n), d(n), v(ldv,*)
           real(dp), intent(out) :: work(lwork)
     end subroutine stdlib_dgsvj0


     pure module subroutine stdlib_cgsvj0( jobv, m, n, a, lda, d, sva, mv, v, ldv, eps,sfmin, tol, &
               nsweep, work, lwork, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldv, lwork, m, mv, n, nsweep
           real(sp), intent(in) :: eps, sfmin, tol
           character, intent(in) :: jobv
           complex(sp), intent(inout) :: a(lda,*), d(n), v(ldv,*)
           complex(sp), intent(out) :: work(lwork)
           real(sp), intent(inout) :: sva(n)
     end subroutine stdlib_cgsvj0

     pure module subroutine stdlib_zgsvj0( jobv, m, n, a, lda, d, sva, mv, v, ldv, eps,sfmin, tol, &
               nsweep, work, lwork, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldv, lwork, m, mv, n, nsweep
           real(dp), intent(in) :: eps, sfmin, tol
           character, intent(in) :: jobv
           complex(dp), intent(inout) :: a(lda,*), d(n), v(ldv,*)
           complex(dp), intent(out) :: work(lwork)
           real(dp), intent(inout) :: sva(n)
     end subroutine stdlib_zgsvj0


end interface 


interface 
     pure module subroutine stdlib_sgsvj1( jobv, m, n, n1, a, lda, d, sva, mv, v, ldv,eps, sfmin, tol, &
               nsweep, work, lwork, info )
           real(sp), intent(in) :: eps, sfmin, tol
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldv, lwork, m, mv, n, n1, nsweep
           character, intent(in) :: jobv
           real(sp), intent(inout) :: a(lda,*), d(n), sva(n), v(ldv,*)
           real(sp), intent(out) :: work(lwork)
     end subroutine stdlib_sgsvj1

     pure module subroutine stdlib_dgsvj1( jobv, m, n, n1, a, lda, d, sva, mv, v, ldv,eps, sfmin, tol, &
               nsweep, work, lwork, info )
           real(dp), intent(in) :: eps, sfmin, tol
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldv, lwork, m, mv, n, n1, nsweep
           character, intent(in) :: jobv
           real(dp), intent(inout) :: a(lda,*), d(n), sva(n), v(ldv,*)
           real(dp), intent(out) :: work(lwork)
     end subroutine stdlib_dgsvj1


     pure module subroutine stdlib_cgsvj1( jobv, m, n, n1, a, lda, d, sva, mv, v, ldv,eps, sfmin, tol, &
               nsweep, work, lwork, info )
           real(sp), intent(in) :: eps, sfmin, tol
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldv, lwork, m, mv, n, n1, nsweep
           character, intent(in) :: jobv
           complex(sp), intent(inout) :: a(lda,*), d(n), v(ldv,*)
           complex(sp), intent(out) :: work(lwork)
           real(sp), intent(inout) :: sva(n)
     end subroutine stdlib_cgsvj1

     pure module subroutine stdlib_zgsvj1( jobv, m, n, n1, a, lda, d, sva, mv, v, ldv,eps, sfmin, tol, &
               nsweep, work, lwork, info )
           real(dp), intent(in) :: eps, sfmin, tol
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldv, lwork, m, mv, n, n1, nsweep
           character, intent(in) :: jobv
           complex(dp), intent(inout) :: a(lda,*), d(n), v(ldv,*)
           complex(dp), intent(out) :: work(lwork)
           real(dp), intent(inout) :: sva(n)
     end subroutine stdlib_zgsvj1


end interface 


interface 
     pure module subroutine stdlib_stgsja( jobu, jobv, jobq, m, p, n, k, l, a, lda, b,ldb, tola, tolb, &
               alpha, beta, u, ldu, v, ldv,q, ldq, work, ncycle, info )
           character, intent(in) :: jobq, jobu, jobv
           integer(ilp), intent(out) :: info, ncycle
           integer(ilp), intent(in) :: k, l, lda, ldb, ldq, ldu, ldv, m, n, p
           real(sp), intent(in) :: tola, tolb
           real(sp), intent(inout) :: a(lda,*), b(ldb,*), q(ldq,*), u(ldu,*), v(ldv,*)
           real(sp), intent(out) :: alpha(*), beta(*), work(*)
     end subroutine stdlib_stgsja

     pure module subroutine stdlib_dtgsja( jobu, jobv, jobq, m, p, n, k, l, a, lda, b,ldb, tola, tolb, &
               alpha, beta, u, ldu, v, ldv,q, ldq, work, ncycle, info )
           character, intent(in) :: jobq, jobu, jobv
           integer(ilp), intent(out) :: info, ncycle
           integer(ilp), intent(in) :: k, l, lda, ldb, ldq, ldu, ldv, m, n, p
           real(dp), intent(in) :: tola, tolb
           real(dp), intent(inout) :: a(lda,*), b(ldb,*), q(ldq,*), u(ldu,*), v(ldv,*)
           real(dp), intent(out) :: alpha(*), beta(*), work(*)
     end subroutine stdlib_dtgsja


     pure module subroutine stdlib_ctgsja( jobu, jobv, jobq, m, p, n, k, l, a, lda, b,ldb, tola, tolb, &
               alpha, beta, u, ldu, v, ldv,q, ldq, work, ncycle, info )
           character, intent(in) :: jobq, jobu, jobv
           integer(ilp), intent(out) :: info, ncycle
           integer(ilp), intent(in) :: k, l, lda, ldb, ldq, ldu, ldv, m, n, p
           real(sp), intent(in) :: tola, tolb
           real(sp), intent(out) :: alpha(*), beta(*)
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*), q(ldq,*), u(ldu,*), v(ldv,*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_ctgsja

     pure module subroutine stdlib_ztgsja( jobu, jobv, jobq, m, p, n, k, l, a, lda, b,ldb, tola, tolb, &
               alpha, beta, u, ldu, v, ldv,q, ldq, work, ncycle, info )
           character, intent(in) :: jobq, jobu, jobv
           integer(ilp), intent(out) :: info, ncycle
           integer(ilp), intent(in) :: k, l, lda, ldb, ldq, ldu, ldv, m, n, p
           real(dp), intent(in) :: tola, tolb
           real(dp), intent(out) :: alpha(*), beta(*)
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*), q(ldq,*), u(ldu,*), v(ldv,*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_ztgsja


end interface 


interface 
     pure module subroutine stdlib_cungbr( vect, m, n, k, a, lda, tau, work, lwork, info )
           character, intent(in) :: vect
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, lwork, m, n
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(in) :: tau(*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_cungbr

     pure module subroutine stdlib_zungbr( vect, m, n, k, a, lda, tau, work, lwork, info )
           character, intent(in) :: vect
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, lwork, m, n
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(in) :: tau(*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_zungbr


end interface 


interface 
     pure module subroutine stdlib_sorgbr( vect, m, n, k, a, lda, tau, work, lwork, info )
           character, intent(in) :: vect
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, lwork, m, n
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(in) :: tau(*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_sorgbr

     pure module subroutine stdlib_dorgbr( vect, m, n, k, a, lda, tau, work, lwork, info )
           character, intent(in) :: vect
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, lwork, m, n
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(in) :: tau(*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_dorgbr


end interface 


interface 
     pure module subroutine stdlib_cunmbr( vect, side, trans, m, n, k, a, lda, tau, c,ldc, work, lwork, &
               info )
           character, intent(in) :: side, trans, vect
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, ldc, lwork, m, n
           complex(sp), intent(inout) :: a(lda,*), c(ldc,*)
           complex(sp), intent(in) :: tau(*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_cunmbr

     pure module subroutine stdlib_zunmbr( vect, side, trans, m, n, k, a, lda, tau, c,ldc, work, lwork, &
               info )
           character, intent(in) :: side, trans, vect
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, ldc, lwork, m, n
           complex(dp), intent(inout) :: a(lda,*), c(ldc,*)
           complex(dp), intent(in) :: tau(*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_zunmbr


end interface 


interface 
     pure module subroutine stdlib_sormbr( vect, side, trans, m, n, k, a, lda, tau, c,ldc, work, lwork, &
               info )
           character, intent(in) :: side, trans, vect
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, ldc, lwork, m, n
           real(sp), intent(inout) :: a(lda,*), c(ldc,*)
           real(sp), intent(in) :: tau(*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_sormbr

     pure module subroutine stdlib_dormbr( vect, side, trans, m, n, k, a, lda, tau, c,ldc, work, lwork, &
               info )
           character, intent(in) :: side, trans, vect
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, ldc, lwork, m, n
           real(dp), intent(inout) :: a(lda,*), c(ldc,*)
           real(dp), intent(in) :: tau(*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_dormbr


end interface 


interface 
     pure module subroutine stdlib_slabrd( m, n, nb, a, lda, d, e, tauq, taup, x, ldx, y,ldy )
           integer(ilp), intent(in) :: lda, ldx, ldy, m, n, nb
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: d(*), e(*), taup(*), tauq(*), x(ldx,*), y(ldy,*)
     end subroutine stdlib_slabrd

     pure module subroutine stdlib_dlabrd( m, n, nb, a, lda, d, e, tauq, taup, x, ldx, y,ldy )
           integer(ilp), intent(in) :: lda, ldx, ldy, m, n, nb
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: d(*), e(*), taup(*), tauq(*), x(ldx,*), y(ldy,*)
     end subroutine stdlib_dlabrd


     pure module subroutine stdlib_clabrd( m, n, nb, a, lda, d, e, tauq, taup, x, ldx, y,ldy )
           integer(ilp), intent(in) :: lda, ldx, ldy, m, n, nb
           real(sp), intent(out) :: d(*), e(*)
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: taup(*), tauq(*), x(ldx,*), y(ldy,*)
     end subroutine stdlib_clabrd

     pure module subroutine stdlib_zlabrd( m, n, nb, a, lda, d, e, tauq, taup, x, ldx, y,ldy )
           integer(ilp), intent(in) :: lda, ldx, ldy, m, n, nb
           real(dp), intent(out) :: d(*), e(*)
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: taup(*), tauq(*), x(ldx,*), y(ldy,*)
     end subroutine stdlib_zlabrd


end interface 


interface 
     pure module subroutine stdlib_slas2( f, g, h, ssmin, ssmax )
           real(sp), intent(in) :: f, g, h
           real(sp), intent(out) :: ssmax, ssmin
     end subroutine stdlib_slas2

     pure module subroutine stdlib_dlas2( f, g, h, ssmin, ssmax )
           real(dp), intent(in) :: f, g, h
           real(dp), intent(out) :: ssmax, ssmin
     end subroutine stdlib_dlas2


end interface 


interface 
     pure module subroutine stdlib_slasv2( f, g, h, ssmin, ssmax, snr, csr, snl, csl )
           real(sp), intent(out) :: csl, csr, snl, snr, ssmax, ssmin
           real(sp), intent(in) :: f, g, h
     end subroutine stdlib_slasv2

     pure module subroutine stdlib_dlasv2( f, g, h, ssmin, ssmax, snr, csr, snl, csl )
           real(dp), intent(out) :: csl, csr, snl, snr, ssmax, ssmin
           real(dp), intent(in) :: f, g, h
     end subroutine stdlib_dlasv2


end interface 


interface 
     pure module subroutine stdlib_slartgs( x, y, sigma, cs, sn )
           real(sp), intent(out) :: cs, sn
           real(sp), intent(in) :: sigma, x, y
     end subroutine stdlib_slartgs

     pure module subroutine stdlib_dlartgs( x, y, sigma, cs, sn )
           real(dp), intent(out) :: cs, sn
           real(dp), intent(in) :: sigma, x, y
     end subroutine stdlib_dlartgs


end interface 


interface 
     pure module subroutine stdlib_slags2( upper, a1, a2, a3, b1, b2, b3, csu, snu, csv,snv, csq, snq )
               
           logical(lk), intent(in) :: upper
           real(sp), intent(in) :: a1, a2, a3, b1, b2, b3
           real(sp), intent(out) :: csq, csu, csv, snq, snu, snv
     end subroutine stdlib_slags2

     pure module subroutine stdlib_dlags2( upper, a1, a2, a3, b1, b2, b3, csu, snu, csv,snv, csq, snq )
               
           logical(lk), intent(in) :: upper
           real(dp), intent(in) :: a1, a2, a3, b1, b2, b3
           real(dp), intent(out) :: csq, csu, csv, snq, snu, snv
     end subroutine stdlib_dlags2


     pure module subroutine stdlib_clags2( upper, a1, a2, a3, b1, b2, b3, csu, snu, csv,snv, csq, snq )
               
           logical(lk), intent(in) :: upper
           real(sp), intent(in) :: a1, a3, b1, b3
           real(sp), intent(out) :: csq, csu, csv
           complex(sp), intent(in) :: a2, b2
           complex(sp), intent(out) :: snq, snu, snv
     end subroutine stdlib_clags2

     pure module subroutine stdlib_zlags2( upper, a1, a2, a3, b1, b2, b3, csu, snu, csv,snv, csq, snq )
               
           logical(lk), intent(in) :: upper
           real(dp), intent(in) :: a1, a3, b1, b3
           real(dp), intent(out) :: csq, csu, csv
           complex(dp), intent(in) :: a2, b2
           complex(dp), intent(out) :: snq, snu, snv
     end subroutine stdlib_zlags2


end interface 


interface 
     pure module subroutine stdlib_slapll( n, x, incx, y, incy, ssmin )
           integer(ilp), intent(in) :: incx, incy, n
           real(sp), intent(out) :: ssmin
           real(sp), intent(inout) :: x(*), y(*)
     end subroutine stdlib_slapll

     pure module subroutine stdlib_dlapll( n, x, incx, y, incy, ssmin )
           integer(ilp), intent(in) :: incx, incy, n
           real(dp), intent(out) :: ssmin
           real(dp), intent(inout) :: x(*), y(*)
     end subroutine stdlib_dlapll


     pure module subroutine stdlib_clapll( n, x, incx, y, incy, ssmin )
           integer(ilp), intent(in) :: incx, incy, n
           real(sp), intent(out) :: ssmin
           complex(sp), intent(inout) :: x(*), y(*)
     end subroutine stdlib_clapll

     pure module subroutine stdlib_zlapll( n, x, incx, y, incy, ssmin )
           integer(ilp), intent(in) :: incx, incy, n
           real(dp), intent(out) :: ssmin
           complex(dp), intent(inout) :: x(*), y(*)
     end subroutine stdlib_zlapll


end interface 


interface 
     pure module subroutine stdlib_ssygst( itype, uplo, n, a, lda, b, ldb, info )
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: itype, lda, ldb, n
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(in) :: b(ldb,*)
     end subroutine stdlib_ssygst

     pure module subroutine stdlib_dsygst( itype, uplo, n, a, lda, b, ldb, info )
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: itype, lda, ldb, n
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(in) :: b(ldb,*)
     end subroutine stdlib_dsygst


end interface 


interface 
     pure module subroutine stdlib_ssygs2( itype, uplo, n, a, lda, b, ldb, info )
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: itype, lda, ldb, n
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(in) :: b(ldb,*)
     end subroutine stdlib_ssygs2

     pure module subroutine stdlib_dsygs2( itype, uplo, n, a, lda, b, ldb, info )
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: itype, lda, ldb, n
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(in) :: b(ldb,*)
     end subroutine stdlib_dsygs2


end interface 


interface 
     pure module subroutine stdlib_sspgst( itype, uplo, n, ap, bp, info )
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: itype, n
           real(sp), intent(inout) :: ap(*)
           real(sp), intent(in) :: bp(*)
     end subroutine stdlib_sspgst

     pure module subroutine stdlib_dspgst( itype, uplo, n, ap, bp, info )
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: itype, n
           real(dp), intent(inout) :: ap(*)
           real(dp), intent(in) :: bp(*)
     end subroutine stdlib_dspgst


end interface 


interface 
     pure module subroutine stdlib_ssbgst( vect, uplo, n, ka, kb, ab, ldab, bb, ldbb, x,ldx, work, info )
               
           character, intent(in) :: uplo, vect
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ka, kb, ldab, ldbb, ldx, n
           real(sp), intent(inout) :: ab(ldab,*)
           real(sp), intent(in) :: bb(ldbb,*)
           real(sp), intent(out) :: work(*), x(ldx,*)
     end subroutine stdlib_ssbgst

     pure module subroutine stdlib_dsbgst( vect, uplo, n, ka, kb, ab, ldab, bb, ldbb, x,ldx, work, info )
               
           character, intent(in) :: uplo, vect
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ka, kb, ldab, ldbb, ldx, n
           real(dp), intent(inout) :: ab(ldab,*)
           real(dp), intent(in) :: bb(ldbb,*)
           real(dp), intent(out) :: work(*), x(ldx,*)
     end subroutine stdlib_dsbgst


end interface 


interface 
     pure module subroutine stdlib_chegst( itype, uplo, n, a, lda, b, ldb, info )
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: itype, lda, ldb, n
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*)
     end subroutine stdlib_chegst

     pure module subroutine stdlib_zhegst( itype, uplo, n, a, lda, b, ldb, info )
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: itype, lda, ldb, n
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*)
     end subroutine stdlib_zhegst


end interface 


interface 
     pure module subroutine stdlib_chegs2( itype, uplo, n, a, lda, b, ldb, info )
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: itype, lda, ldb, n
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*)
     end subroutine stdlib_chegs2

     pure module subroutine stdlib_zhegs2( itype, uplo, n, a, lda, b, ldb, info )
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: itype, lda, ldb, n
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*)
     end subroutine stdlib_zhegs2


end interface 


interface 
     pure module subroutine stdlib_chpgst( itype, uplo, n, ap, bp, info )
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: itype, n
           complex(sp), intent(inout) :: ap(*)
           complex(sp), intent(in) :: bp(*)
     end subroutine stdlib_chpgst

     pure module subroutine stdlib_zhpgst( itype, uplo, n, ap, bp, info )
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: itype, n
           complex(dp), intent(inout) :: ap(*)
           complex(dp), intent(in) :: bp(*)
     end subroutine stdlib_zhpgst


end interface 


interface 
     pure module subroutine stdlib_chbgst( vect, uplo, n, ka, kb, ab, ldab, bb, ldbb, x,ldx, work, rwork,&
                info )
           character, intent(in) :: uplo, vect
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ka, kb, ldab, ldbb, ldx, n
           real(sp), intent(out) :: rwork(*)
           complex(sp), intent(inout) :: ab(ldab,*)
           complex(sp), intent(in) :: bb(ldbb,*)
           complex(sp), intent(out) :: work(*), x(ldx,*)
     end subroutine stdlib_chbgst

     pure module subroutine stdlib_zhbgst( vect, uplo, n, ka, kb, ab, ldab, bb, ldbb, x,ldx, work, rwork,&
                info )
           character, intent(in) :: uplo, vect
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ka, kb, ldab, ldbb, ldx, n
           real(dp), intent(out) :: rwork(*)
           complex(dp), intent(inout) :: ab(ldab,*)
           complex(dp), intent(in) :: bb(ldbb,*)
           complex(dp), intent(out) :: work(*), x(ldx,*)
     end subroutine stdlib_zhbgst


end interface 


interface 
     pure module subroutine stdlib_spbstf( uplo, n, kd, ab, ldab, info )
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kd, ldab, n
           real(sp), intent(inout) :: ab(ldab,*)
     end subroutine stdlib_spbstf

     pure module subroutine stdlib_dpbstf( uplo, n, kd, ab, ldab, info )
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kd, ldab, n
           real(dp), intent(inout) :: ab(ldab,*)
     end subroutine stdlib_dpbstf


     pure module subroutine stdlib_cpbstf( uplo, n, kd, ab, ldab, info )
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kd, ldab, n
           complex(sp), intent(inout) :: ab(ldab,*)
     end subroutine stdlib_cpbstf

     pure module subroutine stdlib_zpbstf( uplo, n, kd, ab, ldab, info )
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kd, ldab, n
           complex(dp), intent(inout) :: ab(ldab,*)
     end subroutine stdlib_zpbstf


end interface 


interface 
     pure module subroutine stdlib_slag2( a, lda, b, ldb, safmin, scale1, scale2, wr1,wr2, wi )
           integer(ilp), intent(in) :: lda, ldb
           real(sp), intent(in) :: safmin
           real(sp), intent(out) :: scale1, scale2, wi, wr1, wr2
           real(sp), intent(in) :: a(lda,*), b(ldb,*)
     end subroutine stdlib_slag2

     pure module subroutine stdlib_dlag2( a, lda, b, ldb, safmin, scale1, scale2, wr1,wr2, wi )
           integer(ilp), intent(in) :: lda, ldb
           real(dp), intent(in) :: safmin
           real(dp), intent(out) :: scale1, scale2, wi, wr1, wr2
           real(dp), intent(in) :: a(lda,*), b(ldb,*)
     end subroutine stdlib_dlag2



end interface 


interface 
     pure module subroutine stdlib_sorm22( side, trans, m, n, n1, n2, q, ldq, c, ldc,work, lwork, info )
               
           character, intent(in) :: side, trans
           integer(ilp), intent(in) :: m, n, n1, n2, ldq, ldc, lwork
           integer(ilp), intent(out) :: info
           real(sp), intent(in) :: q(ldq,*)
           real(sp), intent(inout) :: c(ldc,*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_sorm22

     pure module subroutine stdlib_dorm22( side, trans, m, n, n1, n2, q, ldq, c, ldc,work, lwork, info )
               
           character, intent(in) :: side, trans
           integer(ilp), intent(in) :: m, n, n1, n2, ldq, ldc, lwork
           integer(ilp), intent(out) :: info
           real(dp), intent(in) :: q(ldq,*)
           real(dp), intent(inout) :: c(ldc,*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_dorm22


end interface 


interface 
     pure module subroutine stdlib_sdisna( job, m, n, d, sep, info )
           character, intent(in) :: job
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: m, n
           real(sp), intent(in) :: d(*)
           real(sp), intent(out) :: sep(*)
     end subroutine stdlib_sdisna

     pure module subroutine stdlib_ddisna( job, m, n, d, sep, info )
           character, intent(in) :: job
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: m, n
           real(dp), intent(in) :: d(*)
           real(dp), intent(out) :: sep(*)
     end subroutine stdlib_ddisna


end interface 


interface 
     pure module subroutine stdlib_slatrd( uplo, n, nb, a, lda, e, tau, w, ldw )
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: lda, ldw, n, nb
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: e(*), tau(*), w(ldw,*)
     end subroutine stdlib_slatrd

     pure module subroutine stdlib_dlatrd( uplo, n, nb, a, lda, e, tau, w, ldw )
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: lda, ldw, n, nb
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: e(*), tau(*), w(ldw,*)
     end subroutine stdlib_dlatrd


     pure module subroutine stdlib_clatrd( uplo, n, nb, a, lda, e, tau, w, ldw )
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: lda, ldw, n, nb
           real(sp), intent(out) :: e(*)
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: tau(*), w(ldw,*)
     end subroutine stdlib_clatrd

     pure module subroutine stdlib_zlatrd( uplo, n, nb, a, lda, e, tau, w, ldw )
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: lda, ldw, n, nb
           real(dp), intent(out) :: e(*)
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: tau(*), w(ldw,*)
     end subroutine stdlib_zlatrd


end interface 


interface 
     pure module subroutine stdlib_slae2( a, b, c, rt1, rt2 )
           real(sp), intent(in) :: a, b, c
           real(sp), intent(out) :: rt1, rt2
     end subroutine stdlib_slae2

     pure module subroutine stdlib_dlae2( a, b, c, rt1, rt2 )
           real(dp), intent(in) :: a, b, c
           real(dp), intent(out) :: rt1, rt2
     end subroutine stdlib_dlae2


end interface 


interface 
     pure module subroutine stdlib_claesy( a, b, c, rt1, rt2, evscal, cs1, sn1 )
           complex(sp), intent(in) :: a, b, c
           complex(sp), intent(out) :: cs1, evscal, rt1, rt2, sn1
     end subroutine stdlib_claesy

     pure module subroutine stdlib_zlaesy( a, b, c, rt1, rt2, evscal, cs1, sn1 )
           complex(dp), intent(in) :: a, b, c
           complex(dp), intent(out) :: cs1, evscal, rt1, rt2, sn1
     end subroutine stdlib_zlaesy


end interface 


interface 
     pure module subroutine stdlib_slaev2( a, b, c, rt1, rt2, cs1, sn1 )
           real(sp), intent(in) :: a, b, c
           real(sp), intent(out) :: cs1, rt1, rt2, sn1
     end subroutine stdlib_slaev2

     pure module subroutine stdlib_dlaev2( a, b, c, rt1, rt2, cs1, sn1 )
           real(dp), intent(in) :: a, b, c
           real(dp), intent(out) :: cs1, rt1, rt2, sn1
     end subroutine stdlib_dlaev2


     pure module subroutine stdlib_claev2( a, b, c, rt1, rt2, cs1, sn1 )
           real(sp), intent(out) :: cs1, rt1, rt2
           complex(sp), intent(in) :: a, b, c
           complex(sp), intent(out) :: sn1
     end subroutine stdlib_claev2

     pure module subroutine stdlib_zlaev2( a, b, c, rt1, rt2, cs1, sn1 )
           real(dp), intent(out) :: cs1, rt1, rt2
           complex(dp), intent(in) :: a, b, c
           complex(dp), intent(out) :: sn1
     end subroutine stdlib_zlaev2


end interface 


interface 
     pure module subroutine stdlib_slagtf( n, a, lambda, b, c, tol, d, in, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           real(sp), intent(in) :: lambda, tol
           integer(ilp), intent(out) :: in(*)
           real(sp), intent(inout) :: a(*), b(*), c(*)
           real(sp), intent(out) :: d(*)
     end subroutine stdlib_slagtf

     pure module subroutine stdlib_dlagtf( n, a, lambda, b, c, tol, d, in, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           real(dp), intent(in) :: lambda, tol
           integer(ilp), intent(out) :: in(*)
           real(dp), intent(inout) :: a(*), b(*), c(*)
           real(dp), intent(out) :: d(*)
     end subroutine stdlib_dlagtf


end interface 


interface 
     pure module subroutine stdlib_slagts( job, n, a, b, c, d, in, y, tol, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: job, n
           real(sp), intent(inout) :: tol
           integer(ilp), intent(in) :: in(*)
           real(sp), intent(in) :: a(*), b(*), c(*), d(*)
           real(sp), intent(inout) :: y(*)
     end subroutine stdlib_slagts

     pure module subroutine stdlib_dlagts( job, n, a, b, c, d, in, y, tol, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: job, n
           real(dp), intent(inout) :: tol
           integer(ilp), intent(in) :: in(*)
           real(dp), intent(in) :: a(*), b(*), c(*), d(*)
           real(dp), intent(inout) :: y(*)
     end subroutine stdlib_dlagts


end interface 


interface 
     pure module subroutine stdlib_ssptrd( uplo, n, ap, d, e, tau, info )
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           real(sp), intent(inout) :: ap(*)
           real(sp), intent(out) :: d(*), e(*), tau(*)
     end subroutine stdlib_ssptrd

     pure module subroutine stdlib_dsptrd( uplo, n, ap, d, e, tau, info )
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           real(dp), intent(inout) :: ap(*)
           real(dp), intent(out) :: d(*), e(*), tau(*)
     end subroutine stdlib_dsptrd


end interface 


interface 
     pure module subroutine stdlib_sopgtr( uplo, n, ap, tau, q, ldq, work, info )
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldq, n
           real(sp), intent(in) :: ap(*), tau(*)
           real(sp), intent(out) :: q(ldq,*), work(*)
     end subroutine stdlib_sopgtr

     pure module subroutine stdlib_dopgtr( uplo, n, ap, tau, q, ldq, work, info )
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldq, n
           real(dp), intent(in) :: ap(*), tau(*)
           real(dp), intent(out) :: q(ldq,*), work(*)
     end subroutine stdlib_dopgtr


end interface 


interface 
     pure module subroutine stdlib_sopmtr( side, uplo, trans, m, n, ap, tau, c, ldc, work,info )
           character, intent(in) :: side, trans, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldc, m, n
           real(sp), intent(inout) :: ap(*), c(ldc,*)
           real(sp), intent(in) :: tau(*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_sopmtr

     pure module subroutine stdlib_dopmtr( side, uplo, trans, m, n, ap, tau, c, ldc, work,info )
           character, intent(in) :: side, trans, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldc, m, n
           real(dp), intent(inout) :: ap(*), c(ldc,*)
           real(dp), intent(in) :: tau(*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_dopmtr


end interface 


interface 
     pure module subroutine stdlib_ssbtrd( vect, uplo, n, kd, ab, ldab, d, e, q, ldq,work, info )
           character, intent(in) :: uplo, vect
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kd, ldab, ldq, n
           real(sp), intent(inout) :: ab(ldab,*), q(ldq,*)
           real(sp), intent(out) :: d(*), e(*), work(*)
     end subroutine stdlib_ssbtrd

     pure module subroutine stdlib_dsbtrd( vect, uplo, n, kd, ab, ldab, d, e, q, ldq,work, info )
           character, intent(in) :: uplo, vect
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kd, ldab, ldq, n
           real(dp), intent(inout) :: ab(ldab,*), q(ldq,*)
           real(dp), intent(out) :: d(*), e(*), work(*)
     end subroutine stdlib_dsbtrd


end interface 


interface 
     pure module subroutine stdlib_chptrd( uplo, n, ap, d, e, tau, info )
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           real(sp), intent(out) :: d(*), e(*)
           complex(sp), intent(inout) :: ap(*)
           complex(sp), intent(out) :: tau(*)
     end subroutine stdlib_chptrd

     pure module subroutine stdlib_zhptrd( uplo, n, ap, d, e, tau, info )
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           real(dp), intent(out) :: d(*), e(*)
           complex(dp), intent(inout) :: ap(*)
           complex(dp), intent(out) :: tau(*)
     end subroutine stdlib_zhptrd


end interface 


interface 
     pure module subroutine stdlib_cupgtr( uplo, n, ap, tau, q, ldq, work, info )
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldq, n
           complex(sp), intent(in) :: ap(*), tau(*)
           complex(sp), intent(out) :: q(ldq,*), work(*)
     end subroutine stdlib_cupgtr

     pure module subroutine stdlib_zupgtr( uplo, n, ap, tau, q, ldq, work, info )
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldq, n
           complex(dp), intent(in) :: ap(*), tau(*)
           complex(dp), intent(out) :: q(ldq,*), work(*)
     end subroutine stdlib_zupgtr


end interface 


interface 
     pure module subroutine stdlib_cupmtr( side, uplo, trans, m, n, ap, tau, c, ldc, work,info )
           character, intent(in) :: side, trans, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldc, m, n
           complex(sp), intent(inout) :: ap(*), c(ldc,*)
           complex(sp), intent(in) :: tau(*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_cupmtr

     pure module subroutine stdlib_zupmtr( side, uplo, trans, m, n, ap, tau, c, ldc, work,info )
           character, intent(in) :: side, trans, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldc, m, n
           complex(dp), intent(inout) :: ap(*), c(ldc,*)
           complex(dp), intent(in) :: tau(*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_zupmtr


end interface 


interface 
     pure module subroutine stdlib_chbtrd( vect, uplo, n, kd, ab, ldab, d, e, q, ldq,work, info )
           character, intent(in) :: uplo, vect
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kd, ldab, ldq, n
           real(sp), intent(out) :: d(*), e(*)
           complex(sp), intent(inout) :: ab(ldab,*), q(ldq,*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_chbtrd

     pure module subroutine stdlib_zhbtrd( vect, uplo, n, kd, ab, ldab, d, e, q, ldq,work, info )
           character, intent(in) :: uplo, vect
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kd, ldab, ldq, n
           real(dp), intent(out) :: d(*), e(*)
           complex(dp), intent(inout) :: ab(ldab,*), q(ldq,*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_zhbtrd


end interface 


interface 
     module subroutine stdlib_sgeev( jobvl, jobvr, n, a, lda, wr, wi, vl, ldvl, vr,ldvr, work, lwork, &
               info )
           character, intent(in) :: jobvl, jobvr
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldvl, ldvr, lwork, n
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: vl(ldvl,*), vr(ldvr,*), wi(*), work(*), wr(*)
     end subroutine stdlib_sgeev

     module subroutine stdlib_dgeev( jobvl, jobvr, n, a, lda, wr, wi, vl, ldvl, vr,ldvr, work, lwork, &
               info )
           character, intent(in) :: jobvl, jobvr
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldvl, ldvr, lwork, n
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: vl(ldvl,*), vr(ldvr,*), wi(*), work(*), wr(*)
     end subroutine stdlib_dgeev


     module subroutine stdlib_cgeev( jobvl, jobvr, n, a, lda, w, vl, ldvl, vr, ldvr,work, lwork, rwork, &
               info )
           character, intent(in) :: jobvl, jobvr
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldvl, ldvr, lwork, n
           real(sp), intent(out) :: rwork(*)
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: vl(ldvl,*), vr(ldvr,*), w(*), work(*)
     end subroutine stdlib_cgeev

     module subroutine stdlib_zgeev( jobvl, jobvr, n, a, lda, w, vl, ldvl, vr, ldvr,work, lwork, rwork, &
               info )
           character, intent(in) :: jobvl, jobvr
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldvl, ldvr, lwork, n
           real(dp), intent(out) :: rwork(*)
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: vl(ldvl,*), vr(ldvr,*), w(*), work(*)
     end subroutine stdlib_zgeev


end interface 


interface 
     module subroutine stdlib_sgeevx( balanc, jobvl, jobvr, sense, n, a, lda, wr, wi,vl, ldvl, vr, ldvr, &
               ilo, ihi, scale, abnrm,rconde, rcondv, work, lwork, iwork, info )
           character, intent(in) :: balanc, jobvl, jobvr, sense
           integer(ilp), intent(out) :: ihi, ilo, info
           integer(ilp), intent(in) :: lda, ldvl, ldvr, lwork, n
           real(sp), intent(out) :: abnrm
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: rconde(*), rcondv(*), scale(*), vl(ldvl,*), vr(ldvr,*), wi(*),&
                      work(*), wr(*)
     end subroutine stdlib_sgeevx

     module subroutine stdlib_dgeevx( balanc, jobvl, jobvr, sense, n, a, lda, wr, wi,vl, ldvl, vr, ldvr, &
               ilo, ihi, scale, abnrm,rconde, rcondv, work, lwork, iwork, info )
           character, intent(in) :: balanc, jobvl, jobvr, sense
           integer(ilp), intent(out) :: ihi, ilo, info
           integer(ilp), intent(in) :: lda, ldvl, ldvr, lwork, n
           real(dp), intent(out) :: abnrm
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: rconde(*), rcondv(*), scale(*), vl(ldvl,*), vr(ldvr,*), wi(*),&
                      work(*), wr(*)
     end subroutine stdlib_dgeevx


     module subroutine stdlib_cgeevx( balanc, jobvl, jobvr, sense, n, a, lda, w, vl,ldvl, vr, ldvr, ilo, &
               ihi, scale, abnrm, rconde,rcondv, work, lwork, rwork, info )
           character, intent(in) :: balanc, jobvl, jobvr, sense
           integer(ilp), intent(out) :: ihi, ilo, info
           integer(ilp), intent(in) :: lda, ldvl, ldvr, lwork, n
           real(sp), intent(out) :: abnrm
           real(sp), intent(out) :: rconde(*), rcondv(*), rwork(*), scale(*)
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: vl(ldvl,*), vr(ldvr,*), w(*), work(*)
     end subroutine stdlib_cgeevx

     module subroutine stdlib_zgeevx( balanc, jobvl, jobvr, sense, n, a, lda, w, vl,ldvl, vr, ldvr, ilo, &
               ihi, scale, abnrm, rconde,rcondv, work, lwork, rwork, info )
           character, intent(in) :: balanc, jobvl, jobvr, sense
           integer(ilp), intent(out) :: ihi, ilo, info
           integer(ilp), intent(in) :: lda, ldvl, ldvr, lwork, n
           real(dp), intent(out) :: abnrm
           real(dp), intent(out) :: rconde(*), rcondv(*), rwork(*), scale(*)
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: vl(ldvl,*), vr(ldvr,*), w(*), work(*)
     end subroutine stdlib_zgeevx


end interface 


interface 
     module subroutine stdlib_sgees( jobvs, sort, select, n, a, lda, sdim, wr, wi,vs, ldvs, work, lwork, &
               bwork, info )
           character, intent(in) :: jobvs, sort
           integer(ilp), intent(out) :: info, sdim
           integer(ilp), intent(in) :: lda, ldvs, lwork, n
           logical(lk), intent(out) :: bwork(*)
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: vs(ldvs,*), wi(*), work(*), wr(*)
           procedure(stdlib_select_s) :: select
     end subroutine stdlib_sgees

     module subroutine stdlib_dgees( jobvs, sort, select, n, a, lda, sdim, wr, wi,vs, ldvs, work, lwork, &
               bwork, info )
           character, intent(in) :: jobvs, sort
           integer(ilp), intent(out) :: info, sdim
           integer(ilp), intent(in) :: lda, ldvs, lwork, n
           logical(lk), intent(out) :: bwork(*)
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: vs(ldvs,*), wi(*), work(*), wr(*)
           procedure(stdlib_select_d) :: select
     end subroutine stdlib_dgees


     module subroutine stdlib_cgees( jobvs, sort, select, n, a, lda, sdim, w, vs,ldvs, work, lwork, &
               rwork, bwork, info )
           character, intent(in) :: jobvs, sort
           integer(ilp), intent(out) :: info, sdim
           integer(ilp), intent(in) :: lda, ldvs, lwork, n
           logical(lk), intent(out) :: bwork(*)
           real(sp), intent(out) :: rwork(*)
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: vs(ldvs,*), w(*), work(*)
           procedure(stdlib_select_c) :: select
     end subroutine stdlib_cgees

     module subroutine stdlib_zgees( jobvs, sort, select, n, a, lda, sdim, w, vs,ldvs, work, lwork, &
               rwork, bwork, info )
           character, intent(in) :: jobvs, sort
           integer(ilp), intent(out) :: info, sdim
           integer(ilp), intent(in) :: lda, ldvs, lwork, n
           logical(lk), intent(out) :: bwork(*)
           real(dp), intent(out) :: rwork(*)
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: vs(ldvs,*), w(*), work(*)
           procedure(stdlib_select_z) :: select
     end subroutine stdlib_zgees


end interface 


interface 
     module subroutine stdlib_sgeesx( jobvs, sort, select, sense, n, a, lda, sdim,wr, wi, vs, ldvs, &
               rconde, rcondv, work, lwork,iwork, liwork, bwork, info )
           character, intent(in) :: jobvs, sense, sort
           integer(ilp), intent(out) :: info, sdim
           integer(ilp), intent(in) :: lda, ldvs, liwork, lwork, n
           real(sp), intent(out) :: rconde, rcondv
           logical(lk), intent(out) :: bwork(*)
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: vs(ldvs,*), wi(*), work(*), wr(*)
           procedure(stdlib_select_s) :: select
     end subroutine stdlib_sgeesx

     module subroutine stdlib_dgeesx( jobvs, sort, select, sense, n, a, lda, sdim,wr, wi, vs, ldvs, &
               rconde, rcondv, work, lwork,iwork, liwork, bwork, info )
           character, intent(in) :: jobvs, sense, sort
           integer(ilp), intent(out) :: info, sdim
           integer(ilp), intent(in) :: lda, ldvs, liwork, lwork, n
           real(dp), intent(out) :: rconde, rcondv
           logical(lk), intent(out) :: bwork(*)
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: vs(ldvs,*), wi(*), work(*), wr(*)
           procedure(stdlib_select_d) :: select
     end subroutine stdlib_dgeesx


     module subroutine stdlib_cgeesx( jobvs, sort, select, sense, n, a, lda, sdim, w,vs, ldvs, rconde, &
               rcondv, work, lwork, rwork,bwork, info )
           character, intent(in) :: jobvs, sense, sort
           integer(ilp), intent(out) :: info, sdim
           integer(ilp), intent(in) :: lda, ldvs, lwork, n
           real(sp), intent(out) :: rconde, rcondv
           logical(lk), intent(out) :: bwork(*)
           real(sp), intent(out) :: rwork(*)
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: vs(ldvs,*), w(*), work(*)
           procedure(stdlib_select_c) :: select
     end subroutine stdlib_cgeesx

     module subroutine stdlib_zgeesx( jobvs, sort, select, sense, n, a, lda, sdim, w,vs, ldvs, rconde, &
               rcondv, work, lwork, rwork,bwork, info )
           character, intent(in) :: jobvs, sense, sort
           integer(ilp), intent(out) :: info, sdim
           integer(ilp), intent(in) :: lda, ldvs, lwork, n
           real(dp), intent(out) :: rconde, rcondv
           logical(lk), intent(out) :: bwork(*)
           real(dp), intent(out) :: rwork(*)
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: vs(ldvs,*), w(*), work(*)
           procedure(stdlib_select_z) :: select
     end subroutine stdlib_zgeesx


end interface 


interface 
     module subroutine stdlib_sggev3( jobvl, jobvr, n, a, lda, b, ldb, alphar,alphai, beta, vl, ldvl, vr,&
                ldvr, work, lwork,info )
           character, intent(in) :: jobvl, jobvr
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, ldvl, ldvr, lwork, n
           real(sp), intent(inout) :: a(lda,*), b(ldb,*)
           real(sp), intent(out) :: alphai(*), alphar(*), beta(*), vl(ldvl,*), vr(ldvr,*), work(*)
                     
     end subroutine stdlib_sggev3

     module subroutine stdlib_dggev3( jobvl, jobvr, n, a, lda, b, ldb, alphar,alphai, beta, vl, ldvl, vr,&
                ldvr, work, lwork,info )
           character, intent(in) :: jobvl, jobvr
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, ldvl, ldvr, lwork, n
           real(dp), intent(inout) :: a(lda,*), b(ldb,*)
           real(dp), intent(out) :: alphai(*), alphar(*), beta(*), vl(ldvl,*), vr(ldvr,*), work(*)
                     
     end subroutine stdlib_dggev3


     module subroutine stdlib_cggev3( jobvl, jobvr, n, a, lda, b, ldb, alpha, beta,vl, ldvl, vr, ldvr, &
               work, lwork, rwork, info )
           character, intent(in) :: jobvl, jobvr
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, ldvl, ldvr, lwork, n
           real(sp), intent(out) :: rwork(*)
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(sp), intent(out) :: alpha(*), beta(*), vl(ldvl,*), vr(ldvr,*), work(*)
     end subroutine stdlib_cggev3

     module subroutine stdlib_zggev3( jobvl, jobvr, n, a, lda, b, ldb, alpha, beta,vl, ldvl, vr, ldvr, &
               work, lwork, rwork, info )
           character, intent(in) :: jobvl, jobvr
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, ldvl, ldvr, lwork, n
           real(dp), intent(out) :: rwork(*)
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(dp), intent(out) :: alpha(*), beta(*), vl(ldvl,*), vr(ldvr,*), work(*)
     end subroutine stdlib_zggev3


end interface 


interface 
     module subroutine stdlib_sggev( jobvl, jobvr, n, a, lda, b, ldb, alphar, alphai,beta, vl, ldvl, vr, &
               ldvr, work, lwork, info )
           character, intent(in) :: jobvl, jobvr
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, ldvl, ldvr, lwork, n
           real(sp), intent(inout) :: a(lda,*), b(ldb,*)
           real(sp), intent(out) :: alphai(*), alphar(*), beta(*), vl(ldvl,*), vr(ldvr,*), work(*)
                     
     end subroutine stdlib_sggev

     module subroutine stdlib_dggev( jobvl, jobvr, n, a, lda, b, ldb, alphar, alphai,beta, vl, ldvl, vr, &
               ldvr, work, lwork, info )
           character, intent(in) :: jobvl, jobvr
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, ldvl, ldvr, lwork, n
           real(dp), intent(inout) :: a(lda,*), b(ldb,*)
           real(dp), intent(out) :: alphai(*), alphar(*), beta(*), vl(ldvl,*), vr(ldvr,*), work(*)
                     
     end subroutine stdlib_dggev


     module subroutine stdlib_cggev( jobvl, jobvr, n, a, lda, b, ldb, alpha, beta,vl, ldvl, vr, ldvr, &
               work, lwork, rwork, info )
           character, intent(in) :: jobvl, jobvr
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, ldvl, ldvr, lwork, n
           real(sp), intent(out) :: rwork(*)
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(sp), intent(out) :: alpha(*), beta(*), vl(ldvl,*), vr(ldvr,*), work(*)
     end subroutine stdlib_cggev

     module subroutine stdlib_zggev( jobvl, jobvr, n, a, lda, b, ldb, alpha, beta,vl, ldvl, vr, ldvr, &
               work, lwork, rwork, info )
           character, intent(in) :: jobvl, jobvr
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, ldvl, ldvr, lwork, n
           real(dp), intent(out) :: rwork(*)
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(dp), intent(out) :: alpha(*), beta(*), vl(ldvl,*), vr(ldvr,*), work(*)
     end subroutine stdlib_zggev


end interface 


interface 
     module subroutine stdlib_sggevx( balanc, jobvl, jobvr, sense, n, a, lda, b, ldb,alphar, alphai, &
     beta, vl, ldvl, vr, ldvr, ilo,ihi, lscale, rscale, abnrm, bbnrm, rconde,rcondv, work, lwork, &
               iwork, bwork, info )
           character, intent(in) :: balanc, jobvl, jobvr, sense
           integer(ilp), intent(out) :: ihi, ilo, info
           integer(ilp), intent(in) :: lda, ldb, ldvl, ldvr, lwork, n
           real(sp), intent(out) :: abnrm, bbnrm
           logical(lk), intent(out) :: bwork(*)
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(inout) :: a(lda,*), b(ldb,*)
           real(sp), intent(out) :: alphai(*), alphar(*), beta(*), lscale(*), rconde(*), rcondv(*)&
                     , rscale(*), vl(ldvl,*), vr(ldvr,*), work(*)
     end subroutine stdlib_sggevx

     module subroutine stdlib_dggevx( balanc, jobvl, jobvr, sense, n, a, lda, b, ldb,alphar, alphai, &
     beta, vl, ldvl, vr, ldvr, ilo,ihi, lscale, rscale, abnrm, bbnrm, rconde,rcondv, work, lwork, &
               iwork, bwork, info )
           character, intent(in) :: balanc, jobvl, jobvr, sense
           integer(ilp), intent(out) :: ihi, ilo, info
           integer(ilp), intent(in) :: lda, ldb, ldvl, ldvr, lwork, n
           real(dp), intent(out) :: abnrm, bbnrm
           logical(lk), intent(out) :: bwork(*)
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(inout) :: a(lda,*), b(ldb,*)
           real(dp), intent(out) :: alphai(*), alphar(*), beta(*), lscale(*), rconde(*), rcondv(*)&
                     , rscale(*), vl(ldvl,*), vr(ldvr,*), work(*)
     end subroutine stdlib_dggevx


     module subroutine stdlib_cggevx( balanc, jobvl, jobvr, sense, n, a, lda, b, ldb,alpha, beta, vl, &
     ldvl, vr, ldvr, ilo, ihi,lscale, rscale, abnrm, bbnrm, rconde, rcondv,work, lwork, rwork, &
               iwork, bwork, info )
           character, intent(in) :: balanc, jobvl, jobvr, sense
           integer(ilp), intent(out) :: ihi, ilo, info
           integer(ilp), intent(in) :: lda, ldb, ldvl, ldvr, lwork, n
           real(sp), intent(out) :: abnrm, bbnrm
           logical(lk), intent(out) :: bwork(*)
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(out) :: lscale(*), rconde(*), rcondv(*), rscale(*), rwork(*)
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(sp), intent(out) :: alpha(*), beta(*), vl(ldvl,*), vr(ldvr,*), work(*)
     end subroutine stdlib_cggevx

     module subroutine stdlib_zggevx( balanc, jobvl, jobvr, sense, n, a, lda, b, ldb,alpha, beta, vl, &
     ldvl, vr, ldvr, ilo, ihi,lscale, rscale, abnrm, bbnrm, rconde, rcondv,work, lwork, rwork, &
               iwork, bwork, info )
           character, intent(in) :: balanc, jobvl, jobvr, sense
           integer(ilp), intent(out) :: ihi, ilo, info
           integer(ilp), intent(in) :: lda, ldb, ldvl, ldvr, lwork, n
           real(dp), intent(out) :: abnrm, bbnrm
           logical(lk), intent(out) :: bwork(*)
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(out) :: lscale(*), rconde(*), rcondv(*), rscale(*), rwork(*)
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(dp), intent(out) :: alpha(*), beta(*), vl(ldvl,*), vr(ldvr,*), work(*)
     end subroutine stdlib_zggevx


end interface 


interface 
     module subroutine stdlib_sgges3( jobvsl, jobvsr, sort, selctg, n, a, lda, b,ldb, sdim, alphar, &
               alphai, beta, vsl, ldvsl,vsr, ldvsr, work, lwork, bwork, info )
           character, intent(in) :: jobvsl, jobvsr, sort
           integer(ilp), intent(out) :: info, sdim
           integer(ilp), intent(in) :: lda, ldb, ldvsl, ldvsr, lwork, n
           logical(lk), intent(out) :: bwork(*)
           real(sp), intent(inout) :: a(lda,*), b(ldb,*)
           real(sp), intent(out) :: alphai(*), alphar(*), beta(*), vsl(ldvsl,*), vsr(ldvsr,*), &
                     work(*)
           procedure(stdlib_selctg_s) :: selctg
     end subroutine stdlib_sgges3

     module subroutine stdlib_dgges3( jobvsl, jobvsr, sort, selctg, n, a, lda, b,ldb, sdim, alphar, &
               alphai, beta, vsl, ldvsl,vsr, ldvsr, work, lwork, bwork, info )
           character, intent(in) :: jobvsl, jobvsr, sort
           integer(ilp), intent(out) :: info, sdim
           integer(ilp), intent(in) :: lda, ldb, ldvsl, ldvsr, lwork, n
           logical(lk), intent(out) :: bwork(*)
           real(dp), intent(inout) :: a(lda,*), b(ldb,*)
           real(dp), intent(out) :: alphai(*), alphar(*), beta(*), vsl(ldvsl,*), vsr(ldvsr,*), &
                     work(*)
           procedure(stdlib_selctg_d) :: selctg
     end subroutine stdlib_dgges3


     module subroutine stdlib_cgges3( jobvsl, jobvsr, sort, selctg, n, a, lda, b,ldb, sdim, alpha, beta, &
               vsl, ldvsl, vsr, ldvsr,work, lwork, rwork, bwork, info )
           character, intent(in) :: jobvsl, jobvsr, sort
           integer(ilp), intent(out) :: info, sdim
           integer(ilp), intent(in) :: lda, ldb, ldvsl, ldvsr, lwork, n
           logical(lk), intent(out) :: bwork(*)
           real(sp), intent(out) :: rwork(*)
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(sp), intent(out) :: alpha(*), beta(*), vsl(ldvsl,*), vsr(ldvsr,*), work(*)
                     
           procedure(stdlib_selctg_c) :: selctg
     end subroutine stdlib_cgges3

     module subroutine stdlib_zgges3( jobvsl, jobvsr, sort, selctg, n, a, lda, b,ldb, sdim, alpha, beta, &
               vsl, ldvsl, vsr, ldvsr,work, lwork, rwork, bwork, info )
           character, intent(in) :: jobvsl, jobvsr, sort
           integer(ilp), intent(out) :: info, sdim
           integer(ilp), intent(in) :: lda, ldb, ldvsl, ldvsr, lwork, n
           logical(lk), intent(out) :: bwork(*)
           real(dp), intent(out) :: rwork(*)
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(dp), intent(out) :: alpha(*), beta(*), vsl(ldvsl,*), vsr(ldvsr,*), work(*)
                     
           procedure(stdlib_selctg_z) :: selctg
     end subroutine stdlib_zgges3


end interface 


interface 
     module subroutine stdlib_sgges( jobvsl, jobvsr, sort, selctg, n, a, lda, b, ldb,sdim, alphar, &
               alphai, beta, vsl, ldvsl, vsr,ldvsr, work, lwork, bwork, info )
           character, intent(in) :: jobvsl, jobvsr, sort
           integer(ilp), intent(out) :: info, sdim
           integer(ilp), intent(in) :: lda, ldb, ldvsl, ldvsr, lwork, n
           logical(lk), intent(out) :: bwork(*)
           real(sp), intent(inout) :: a(lda,*), b(ldb,*)
           real(sp), intent(out) :: alphai(*), alphar(*), beta(*), vsl(ldvsl,*), vsr(ldvsr,*), &
                     work(*)
           procedure(stdlib_selctg_s) :: selctg
     end subroutine stdlib_sgges

     module subroutine stdlib_dgges( jobvsl, jobvsr, sort, selctg, n, a, lda, b, ldb,sdim, alphar, &
               alphai, beta, vsl, ldvsl, vsr,ldvsr, work, lwork, bwork, info )
           character, intent(in) :: jobvsl, jobvsr, sort
           integer(ilp), intent(out) :: info, sdim
           integer(ilp), intent(in) :: lda, ldb, ldvsl, ldvsr, lwork, n
           logical(lk), intent(out) :: bwork(*)
           real(dp), intent(inout) :: a(lda,*), b(ldb,*)
           real(dp), intent(out) :: alphai(*), alphar(*), beta(*), vsl(ldvsl,*), vsr(ldvsr,*), &
                     work(*)
           procedure(stdlib_selctg_d) :: selctg
     end subroutine stdlib_dgges


     module subroutine stdlib_cgges( jobvsl, jobvsr, sort, selctg, n, a, lda, b, ldb,sdim, alpha, beta, &
               vsl, ldvsl, vsr, ldvsr, work,lwork, rwork, bwork, info )
           character, intent(in) :: jobvsl, jobvsr, sort
           integer(ilp), intent(out) :: info, sdim
           integer(ilp), intent(in) :: lda, ldb, ldvsl, ldvsr, lwork, n
           logical(lk), intent(out) :: bwork(*)
           real(sp), intent(out) :: rwork(*)
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(sp), intent(out) :: alpha(*), beta(*), vsl(ldvsl,*), vsr(ldvsr,*), work(*)
                     
           procedure(stdlib_selctg_c) :: selctg
     end subroutine stdlib_cgges

     module subroutine stdlib_zgges( jobvsl, jobvsr, sort, selctg, n, a, lda, b, ldb,sdim, alpha, beta, &
               vsl, ldvsl, vsr, ldvsr, work,lwork, rwork, bwork, info )
           character, intent(in) :: jobvsl, jobvsr, sort
           integer(ilp), intent(out) :: info, sdim
           integer(ilp), intent(in) :: lda, ldb, ldvsl, ldvsr, lwork, n
           logical(lk), intent(out) :: bwork(*)
           real(dp), intent(out) :: rwork(*)
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(dp), intent(out) :: alpha(*), beta(*), vsl(ldvsl,*), vsr(ldvsr,*), work(*)
                     
           procedure(stdlib_selctg_z) :: selctg
     end subroutine stdlib_zgges


end interface 


interface 
     module subroutine stdlib_sggesx( jobvsl, jobvsr, sort, selctg, sense, n, a, lda,b, ldb, sdim, &
     alphar, alphai, beta, vsl, ldvsl,vsr, ldvsr, rconde, rcondv, work, lwork, iwork,liwork, &
               bwork, info )
           character, intent(in) :: jobvsl, jobvsr, sense, sort
           integer(ilp), intent(out) :: info, sdim
           integer(ilp), intent(in) :: lda, ldb, ldvsl, ldvsr, liwork, lwork, n
           logical(lk), intent(out) :: bwork(*)
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(inout) :: a(lda,*), b(ldb,*)
           real(sp), intent(out) :: alphai(*), alphar(*), beta(*), rconde(2_ilp), rcondv(2_ilp), vsl(&
                     ldvsl,*), vsr(ldvsr,*), work(*)
           procedure(stdlib_selctg_s) :: selctg
     end subroutine stdlib_sggesx

     module subroutine stdlib_dggesx( jobvsl, jobvsr, sort, selctg, sense, n, a, lda,b, ldb, sdim, &
     alphar, alphai, beta, vsl, ldvsl,vsr, ldvsr, rconde, rcondv, work, lwork, iwork,liwork, &
               bwork, info )
           character, intent(in) :: jobvsl, jobvsr, sense, sort
           integer(ilp), intent(out) :: info, sdim
           integer(ilp), intent(in) :: lda, ldb, ldvsl, ldvsr, liwork, lwork, n
           logical(lk), intent(out) :: bwork(*)
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(inout) :: a(lda,*), b(ldb,*)
           real(dp), intent(out) :: alphai(*), alphar(*), beta(*), rconde(2_ilp), rcondv(2_ilp), vsl(&
                     ldvsl,*), vsr(ldvsr,*), work(*)
           procedure(stdlib_selctg_d) :: selctg
     end subroutine stdlib_dggesx


     module subroutine stdlib_cggesx( jobvsl, jobvsr, sort, selctg, sense, n, a, lda,b, ldb, sdim, alpha,&
      beta, vsl, ldvsl, vsr,ldvsr, rconde, rcondv, work, lwork, rwork,iwork, liwork, bwork, info )
                
           character, intent(in) :: jobvsl, jobvsr, sense, sort
           integer(ilp), intent(out) :: info, sdim
           integer(ilp), intent(in) :: lda, ldb, ldvsl, ldvsr, liwork, lwork, n
           logical(lk), intent(out) :: bwork(*)
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(out) :: rconde(2_ilp), rcondv(2_ilp), rwork(*)
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(sp), intent(out) :: alpha(*), beta(*), vsl(ldvsl,*), vsr(ldvsr,*), work(*)
                     
           procedure(stdlib_selctg_c) :: selctg
     end subroutine stdlib_cggesx

     module subroutine stdlib_zggesx( jobvsl, jobvsr, sort, selctg, sense, n, a, lda,b, ldb, sdim, alpha,&
      beta, vsl, ldvsl, vsr,ldvsr, rconde, rcondv, work, lwork, rwork,iwork, liwork, bwork, info )
                
           character, intent(in) :: jobvsl, jobvsr, sense, sort
           integer(ilp), intent(out) :: info, sdim
           integer(ilp), intent(in) :: lda, ldb, ldvsl, ldvsr, liwork, lwork, n
           logical(lk), intent(out) :: bwork(*)
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(out) :: rconde(2_ilp), rcondv(2_ilp), rwork(*)
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(dp), intent(out) :: alpha(*), beta(*), vsl(ldvsl,*), vsr(ldvsr,*), work(*)
                     
           procedure(stdlib_selctg_z) :: selctg
     end subroutine stdlib_zggesx


end interface 


interface 
     pure module subroutine stdlib_sgebal( job, n, a, lda, ilo, ihi, scale, info )
           character, intent(in) :: job
           integer(ilp), intent(out) :: ihi, ilo, info
           integer(ilp), intent(in) :: lda, n
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: scale(*)
     end subroutine stdlib_sgebal

     pure module subroutine stdlib_dgebal( job, n, a, lda, ilo, ihi, scale, info )
           character, intent(in) :: job
           integer(ilp), intent(out) :: ihi, ilo, info
           integer(ilp), intent(in) :: lda, n
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: scale(*)
     end subroutine stdlib_dgebal


     pure module subroutine stdlib_cgebal( job, n, a, lda, ilo, ihi, scale, info )
           character, intent(in) :: job
           integer(ilp), intent(out) :: ihi, ilo, info
           integer(ilp), intent(in) :: lda, n
           real(sp), intent(out) :: scale(*)
           complex(sp), intent(inout) :: a(lda,*)
     end subroutine stdlib_cgebal

     pure module subroutine stdlib_zgebal( job, n, a, lda, ilo, ihi, scale, info )
           character, intent(in) :: job
           integer(ilp), intent(out) :: ihi, ilo, info
           integer(ilp), intent(in) :: lda, n
           real(dp), intent(out) :: scale(*)
           complex(dp), intent(inout) :: a(lda,*)
     end subroutine stdlib_zgebal


end interface 


interface 
     pure module subroutine stdlib_sgehrd( n, ilo, ihi, a, lda, tau, work, lwork, info )
           integer(ilp), intent(in) :: ihi, ilo, lda, lwork, n
           integer(ilp), intent(out) :: info
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_sgehrd

     pure module subroutine stdlib_dgehrd( n, ilo, ihi, a, lda, tau, work, lwork, info )
           integer(ilp), intent(in) :: ihi, ilo, lda, lwork, n
           integer(ilp), intent(out) :: info
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_dgehrd


     pure module subroutine stdlib_cgehrd( n, ilo, ihi, a, lda, tau, work, lwork, info )
           integer(ilp), intent(in) :: ihi, ilo, lda, lwork, n
           integer(ilp), intent(out) :: info
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_cgehrd

     pure module subroutine stdlib_zgehrd( n, ilo, ihi, a, lda, tau, work, lwork, info )
           integer(ilp), intent(in) :: ihi, ilo, lda, lwork, n
           integer(ilp), intent(out) :: info
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_zgehrd


end interface 


interface 
     pure module subroutine stdlib_sgehd2( n, ilo, ihi, a, lda, tau, work, info )
           integer(ilp), intent(in) :: ihi, ilo, lda, n
           integer(ilp), intent(out) :: info
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_sgehd2

     pure module subroutine stdlib_dgehd2( n, ilo, ihi, a, lda, tau, work, info )
           integer(ilp), intent(in) :: ihi, ilo, lda, n
           integer(ilp), intent(out) :: info
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_dgehd2


     pure module subroutine stdlib_cgehd2( n, ilo, ihi, a, lda, tau, work, info )
           integer(ilp), intent(in) :: ihi, ilo, lda, n
           integer(ilp), intent(out) :: info
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_cgehd2

     pure module subroutine stdlib_zgehd2( n, ilo, ihi, a, lda, tau, work, info )
           integer(ilp), intent(in) :: ihi, ilo, lda, n
           integer(ilp), intent(out) :: info
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_zgehd2


end interface 


interface 
     pure module subroutine stdlib_sgebak( job, side, n, ilo, ihi, scale, m, v, ldv,info )
           character, intent(in) :: job, side
           integer(ilp), intent(in) :: ihi, ilo, ldv, m, n
           integer(ilp), intent(out) :: info
           real(sp), intent(inout) :: v(ldv,*)
           real(sp), intent(in) :: scale(*)
     end subroutine stdlib_sgebak

     pure module subroutine stdlib_dgebak( job, side, n, ilo, ihi, scale, m, v, ldv,info )
           character, intent(in) :: job, side
           integer(ilp), intent(in) :: ihi, ilo, ldv, m, n
           integer(ilp), intent(out) :: info
           real(dp), intent(in) :: scale(*)
           real(dp), intent(inout) :: v(ldv,*)
     end subroutine stdlib_dgebak


     pure module subroutine stdlib_cgebak( job, side, n, ilo, ihi, scale, m, v, ldv,info )
           character, intent(in) :: job, side
           integer(ilp), intent(in) :: ihi, ilo, ldv, m, n
           integer(ilp), intent(out) :: info
           real(sp), intent(in) :: scale(*)
           complex(sp), intent(inout) :: v(ldv,*)
     end subroutine stdlib_cgebak

     pure module subroutine stdlib_zgebak( job, side, n, ilo, ihi, scale, m, v, ldv,info )
           character, intent(in) :: job, side
           integer(ilp), intent(in) :: ihi, ilo, ldv, m, n
           integer(ilp), intent(out) :: info
           real(dp), intent(in) :: scale(*)
           complex(dp), intent(inout) :: v(ldv,*)
     end subroutine stdlib_zgebak


end interface 


interface 
     pure module subroutine stdlib_slahr2( n, k, nb, a, lda, tau, t, ldt, y, ldy )
           integer(ilp), intent(in) :: k, lda, ldt, ldy, n, nb
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: t(ldt,nb), tau(nb), y(ldy,nb)
     end subroutine stdlib_slahr2

     pure module subroutine stdlib_dlahr2( n, k, nb, a, lda, tau, t, ldt, y, ldy )
           integer(ilp), intent(in) :: k, lda, ldt, ldy, n, nb
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: t(ldt,nb), tau(nb), y(ldy,nb)
     end subroutine stdlib_dlahr2


     pure module subroutine stdlib_clahr2( n, k, nb, a, lda, tau, t, ldt, y, ldy )
           integer(ilp), intent(in) :: k, lda, ldt, ldy, n, nb
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: t(ldt,nb), tau(nb), y(ldy,nb)
     end subroutine stdlib_clahr2

     pure module subroutine stdlib_zlahr2( n, k, nb, a, lda, tau, t, ldt, y, ldy )
           integer(ilp), intent(in) :: k, lda, ldt, ldy, n, nb
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: t(ldt,nb), tau(nb), y(ldy,nb)
     end subroutine stdlib_zlahr2


end interface 


interface 
     pure module subroutine stdlib_cunghr( n, ilo, ihi, a, lda, tau, work, lwork, info )
           integer(ilp), intent(in) :: ihi, ilo, lda, lwork, n
           integer(ilp), intent(out) :: info
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(in) :: tau(*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_cunghr

     pure module subroutine stdlib_zunghr( n, ilo, ihi, a, lda, tau, work, lwork, info )
           integer(ilp), intent(in) :: ihi, ilo, lda, lwork, n
           integer(ilp), intent(out) :: info
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(in) :: tau(*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_zunghr


end interface 


interface 
     pure module subroutine stdlib_cunmhr( side, trans, m, n, ilo, ihi, a, lda, tau, c,ldc, work, lwork, &
               info )
           character, intent(in) :: side, trans
           integer(ilp), intent(in) :: ihi, ilo, lda, ldc, lwork, m, n
           integer(ilp), intent(out) :: info
           complex(sp), intent(inout) :: a(lda,*), c(ldc,*)
           complex(sp), intent(in) :: tau(*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_cunmhr

     pure module subroutine stdlib_zunmhr( side, trans, m, n, ilo, ihi, a, lda, tau, c,ldc, work, lwork, &
               info )
           character, intent(in) :: side, trans
           integer(ilp), intent(in) :: ihi, ilo, lda, ldc, lwork, m, n
           integer(ilp), intent(out) :: info
           complex(dp), intent(inout) :: a(lda,*), c(ldc,*)
           complex(dp), intent(in) :: tau(*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_zunmhr


end interface 


interface 
     pure module subroutine stdlib_sorghr( n, ilo, ihi, a, lda, tau, work, lwork, info )
           integer(ilp), intent(in) :: ihi, ilo, lda, lwork, n
           integer(ilp), intent(out) :: info
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(in) :: tau(*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_sorghr

     pure module subroutine stdlib_dorghr( n, ilo, ihi, a, lda, tau, work, lwork, info )
           integer(ilp), intent(in) :: ihi, ilo, lda, lwork, n
           integer(ilp), intent(out) :: info
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(in) :: tau(*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_dorghr


end interface 


interface 
     pure module subroutine stdlib_sormhr( side, trans, m, n, ilo, ihi, a, lda, tau, c,ldc, work, lwork, &
               info )
           character, intent(in) :: side, trans
           integer(ilp), intent(in) :: ihi, ilo, lda, ldc, lwork, m, n
           integer(ilp), intent(out) :: info
           real(sp), intent(inout) :: a(lda,*), c(ldc,*)
           real(sp), intent(in) :: tau(*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_sormhr

     pure module subroutine stdlib_dormhr( side, trans, m, n, ilo, ihi, a, lda, tau, c,ldc, work, lwork, &
               info )
           character, intent(in) :: side, trans
           integer(ilp), intent(in) :: ihi, ilo, lda, ldc, lwork, m, n
           integer(ilp), intent(out) :: info
           real(dp), intent(inout) :: a(lda,*), c(ldc,*)
           real(dp), intent(in) :: tau(*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_dormhr


end interface 


interface 
     module subroutine stdlib_shseqr( job, compz, n, ilo, ihi, h, ldh, wr, wi, z,ldz, work, lwork, info )
               
           integer(ilp), intent(in) :: ihi, ilo, ldh, ldz, lwork, n
           integer(ilp), intent(out) :: info
           character, intent(in) :: compz, job
           real(sp), intent(inout) :: h(ldh,*), z(ldz,*)
           real(sp), intent(out) :: wi(*), work(*), wr(*)
     end subroutine stdlib_shseqr

     module subroutine stdlib_dhseqr( job, compz, n, ilo, ihi, h, ldh, wr, wi, z,ldz, work, lwork, info )
               
           integer(ilp), intent(in) :: ihi, ilo, ldh, ldz, lwork, n
           integer(ilp), intent(out) :: info
           character, intent(in) :: compz, job
           real(dp), intent(inout) :: h(ldh,*), z(ldz,*)
           real(dp), intent(out) :: wi(*), work(*), wr(*)
     end subroutine stdlib_dhseqr


     pure module subroutine stdlib_chseqr( job, compz, n, ilo, ihi, h, ldh, w, z, ldz,work, lwork, info )
               
           integer(ilp), intent(in) :: ihi, ilo, ldh, ldz, lwork, n
           integer(ilp), intent(out) :: info
           character, intent(in) :: compz, job
           complex(sp), intent(inout) :: h(ldh,*), z(ldz,*)
           complex(sp), intent(out) :: w(*), work(*)
     end subroutine stdlib_chseqr

     pure module subroutine stdlib_zhseqr( job, compz, n, ilo, ihi, h, ldh, w, z, ldz,work, lwork, info )
               
           integer(ilp), intent(in) :: ihi, ilo, ldh, ldz, lwork, n
           integer(ilp), intent(out) :: info
           character, intent(in) :: compz, job
           complex(dp), intent(inout) :: h(ldh,*), z(ldz,*)
           complex(dp), intent(out) :: w(*), work(*)
     end subroutine stdlib_zhseqr


end interface 


interface 
     module subroutine stdlib_shsein( side, eigsrc, initv, select, n, h, ldh, wr, wi,vl, ldvl, vr, ldvr, &
               mm, m, work, ifaill,ifailr, info )
           character, intent(in) :: eigsrc, initv, side
           integer(ilp), intent(out) :: info, m
           integer(ilp), intent(in) :: ldh, ldvl, ldvr, mm, n
           logical(lk), intent(inout) :: select(*)
           integer(ilp), intent(out) :: ifaill(*), ifailr(*)
           real(sp), intent(in) :: h(ldh,*), wi(*)
           real(sp), intent(inout) :: vl(ldvl,*), vr(ldvr,*), wr(*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_shsein

     module subroutine stdlib_dhsein( side, eigsrc, initv, select, n, h, ldh, wr, wi,vl, ldvl, vr, ldvr, &
               mm, m, work, ifaill,ifailr, info )
           character, intent(in) :: eigsrc, initv, side
           integer(ilp), intent(out) :: info, m
           integer(ilp), intent(in) :: ldh, ldvl, ldvr, mm, n
           logical(lk), intent(inout) :: select(*)
           integer(ilp), intent(out) :: ifaill(*), ifailr(*)
           real(dp), intent(in) :: h(ldh,*), wi(*)
           real(dp), intent(inout) :: vl(ldvl,*), vr(ldvr,*), wr(*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_dhsein


     module subroutine stdlib_chsein( side, eigsrc, initv, select, n, h, ldh, w, vl,ldvl, vr, ldvr, mm, &
               m, work, rwork, ifaill,ifailr, info )
           character, intent(in) :: eigsrc, initv, side
           integer(ilp), intent(out) :: info, m
           integer(ilp), intent(in) :: ldh, ldvl, ldvr, mm, n
           logical(lk), intent(in) :: select(*)
           integer(ilp), intent(out) :: ifaill(*), ifailr(*)
           real(sp), intent(out) :: rwork(*)
           complex(sp), intent(in) :: h(ldh,*)
           complex(sp), intent(inout) :: vl(ldvl,*), vr(ldvr,*), w(*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_chsein

     module subroutine stdlib_zhsein( side, eigsrc, initv, select, n, h, ldh, w, vl,ldvl, vr, ldvr, mm, &
               m, work, rwork, ifaill,ifailr, info )
           character, intent(in) :: eigsrc, initv, side
           integer(ilp), intent(out) :: info, m
           integer(ilp), intent(in) :: ldh, ldvl, ldvr, mm, n
           logical(lk), intent(in) :: select(*)
           integer(ilp), intent(out) :: ifaill(*), ifailr(*)
           real(dp), intent(out) :: rwork(*)
           complex(dp), intent(in) :: h(ldh,*)
           complex(dp), intent(inout) :: vl(ldvl,*), vr(ldvr,*), w(*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_zhsein


end interface 


interface 
     pure module subroutine stdlib_strevc( side, howmny, select, n, t, ldt, vl, ldvl, vr,ldvr, mm, m, &
               work, info )
           character, intent(in) :: howmny, side
           integer(ilp), intent(out) :: info, m
           integer(ilp), intent(in) :: ldt, ldvl, ldvr, mm, n
           logical(lk), intent(inout) :: select(*)
           real(sp), intent(in) :: t(ldt,*)
           real(sp), intent(inout) :: vl(ldvl,*), vr(ldvr,*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_strevc

     pure module subroutine stdlib_dtrevc( side, howmny, select, n, t, ldt, vl, ldvl, vr,ldvr, mm, m, &
               work, info )
           character, intent(in) :: howmny, side
           integer(ilp), intent(out) :: info, m
           integer(ilp), intent(in) :: ldt, ldvl, ldvr, mm, n
           logical(lk), intent(inout) :: select(*)
           real(dp), intent(in) :: t(ldt,*)
           real(dp), intent(inout) :: vl(ldvl,*), vr(ldvr,*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_dtrevc


     pure module subroutine stdlib_ctrevc( side, howmny, select, n, t, ldt, vl, ldvl, vr,ldvr, mm, m, &
               work, rwork, info )
           character, intent(in) :: howmny, side
           integer(ilp), intent(out) :: info, m
           integer(ilp), intent(in) :: ldt, ldvl, ldvr, mm, n
           logical(lk), intent(in) :: select(*)
           real(sp), intent(out) :: rwork(*)
           complex(sp), intent(inout) :: t(ldt,*), vl(ldvl,*), vr(ldvr,*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_ctrevc

     pure module subroutine stdlib_ztrevc( side, howmny, select, n, t, ldt, vl, ldvl, vr,ldvr, mm, m, &
               work, rwork, info )
           character, intent(in) :: howmny, side
           integer(ilp), intent(out) :: info, m
           integer(ilp), intent(in) :: ldt, ldvl, ldvr, mm, n
           logical(lk), intent(in) :: select(*)
           real(dp), intent(out) :: rwork(*)
           complex(dp), intent(inout) :: t(ldt,*), vl(ldvl,*), vr(ldvr,*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_ztrevc


end interface 


interface 
     pure module subroutine stdlib_strevc3( side, howmny, select, n, t, ldt, vl, ldvl,vr, ldvr, mm, m, &
               work, lwork, info )
           character, intent(in) :: howmny, side
           integer(ilp), intent(out) :: info, m
           integer(ilp), intent(in) :: ldt, ldvl, ldvr, lwork, mm, n
           logical(lk), intent(inout) :: select(*)
           real(sp), intent(in) :: t(ldt,*)
           real(sp), intent(inout) :: vl(ldvl,*), vr(ldvr,*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_strevc3

     pure module subroutine stdlib_dtrevc3( side, howmny, select, n, t, ldt, vl, ldvl,vr, ldvr, mm, m, &
               work, lwork, info )
           character, intent(in) :: howmny, side
           integer(ilp), intent(out) :: info, m
           integer(ilp), intent(in) :: ldt, ldvl, ldvr, lwork, mm, n
           logical(lk), intent(inout) :: select(*)
           real(dp), intent(in) :: t(ldt,*)
           real(dp), intent(inout) :: vl(ldvl,*), vr(ldvr,*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_dtrevc3


     pure module subroutine stdlib_ctrevc3( side, howmny, select, n, t, ldt, vl, ldvl, vr,ldvr, mm, m, &
               work, lwork, rwork, lrwork, info)
           character, intent(in) :: howmny, side
           integer(ilp), intent(out) :: info, m
           integer(ilp), intent(in) :: ldt, ldvl, ldvr, lwork, lrwork, mm, n
           logical(lk), intent(in) :: select(*)
           real(sp), intent(out) :: rwork(*)
           complex(sp), intent(inout) :: t(ldt,*), vl(ldvl,*), vr(ldvr,*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_ctrevc3

     pure module subroutine stdlib_ztrevc3( side, howmny, select, n, t, ldt, vl, ldvl, vr,ldvr, mm, m, &
               work, lwork, rwork, lrwork, info)
           character, intent(in) :: howmny, side
           integer(ilp), intent(out) :: info, m
           integer(ilp), intent(in) :: ldt, ldvl, ldvr, lwork, lrwork, mm, n
           logical(lk), intent(in) :: select(*)
           real(dp), intent(out) :: rwork(*)
           complex(dp), intent(inout) :: t(ldt,*), vl(ldvl,*), vr(ldvr,*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_ztrevc3


end interface 


interface 
     pure module subroutine stdlib_slaln2( ltrans, na, nw, smin, ca, a, lda, d1, d2, b,ldb, wr, wi, x, &
               ldx, scale, xnorm, info )
           logical(lk), intent(in) :: ltrans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, ldx, na, nw
           real(sp), intent(in) :: ca, d1, d2, smin, wi, wr
           real(sp), intent(out) :: scale, xnorm
           real(sp), intent(in) :: a(lda,*), b(ldb,*)
           real(sp), intent(out) :: x(ldx,*)
     end subroutine stdlib_slaln2

     pure module subroutine stdlib_dlaln2( ltrans, na, nw, smin, ca, a, lda, d1, d2, b,ldb, wr, wi, x, &
               ldx, scale, xnorm, info )
           logical(lk), intent(in) :: ltrans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, ldx, na, nw
           real(dp), intent(in) :: ca, d1, d2, smin, wi, wr
           real(dp), intent(out) :: scale, xnorm
           real(dp), intent(in) :: a(lda,*), b(ldb,*)
           real(dp), intent(out) :: x(ldx,*)
     end subroutine stdlib_dlaln2


end interface 


interface 
     module subroutine stdlib_strsyl( trana, tranb, isgn, m, n, a, lda, b, ldb, c,ldc, scale, info )
               
           character, intent(in) :: trana, tranb
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: isgn, lda, ldb, ldc, m, n
           real(sp), intent(out) :: scale
           real(sp), intent(in) :: a(lda,*), b(ldb,*)
           real(sp), intent(inout) :: c(ldc,*)
     end subroutine stdlib_strsyl

     module subroutine stdlib_dtrsyl( trana, tranb, isgn, m, n, a, lda, b, ldb, c,ldc, scale, info )
               
           character, intent(in) :: trana, tranb
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: isgn, lda, ldb, ldc, m, n
           real(dp), intent(out) :: scale
           real(dp), intent(in) :: a(lda,*), b(ldb,*)
           real(dp), intent(inout) :: c(ldc,*)
     end subroutine stdlib_dtrsyl


     module subroutine stdlib_ctrsyl( trana, tranb, isgn, m, n, a, lda, b, ldb, c,ldc, scale, info )
               
           character, intent(in) :: trana, tranb
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: isgn, lda, ldb, ldc, m, n
           real(sp), intent(out) :: scale
           complex(sp), intent(in) :: a(lda,*), b(ldb,*)
           complex(sp), intent(inout) :: c(ldc,*)
     end subroutine stdlib_ctrsyl

     module subroutine stdlib_ztrsyl( trana, tranb, isgn, m, n, a, lda, b, ldb, c,ldc, scale, info )
               
           character, intent(in) :: trana, tranb
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: isgn, lda, ldb, ldc, m, n
           real(dp), intent(out) :: scale
           complex(dp), intent(in) :: a(lda,*), b(ldb,*)
           complex(dp), intent(inout) :: c(ldc,*)
     end subroutine stdlib_ztrsyl


end interface 


interface 
     pure module subroutine stdlib_slasy2( ltranl, ltranr, isgn, n1, n2, tl, ldtl, tr,ldtr, b, ldb, &
               scale, x, ldx, xnorm, info )
           logical(lk), intent(in) :: ltranl, ltranr
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: isgn, ldb, ldtl, ldtr, ldx, n1, n2
           real(sp), intent(out) :: scale, xnorm
           real(sp), intent(in) :: b(ldb,*), tl(ldtl,*), tr(ldtr,*)
           real(sp), intent(out) :: x(ldx,*)
     end subroutine stdlib_slasy2

     pure module subroutine stdlib_dlasy2( ltranl, ltranr, isgn, n1, n2, tl, ldtl, tr,ldtr, b, ldb, &
               scale, x, ldx, xnorm, info )
           logical(lk), intent(in) :: ltranl, ltranr
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: isgn, ldb, ldtl, ldtr, ldx, n1, n2
           real(dp), intent(out) :: scale, xnorm
           real(dp), intent(in) :: b(ldb,*), tl(ldtl,*), tr(ldtr,*)
           real(dp), intent(out) :: x(ldx,*)
     end subroutine stdlib_dlasy2


end interface 


interface 
     module subroutine stdlib_strsna( job, howmny, select, n, t, ldt, vl, ldvl, vr,ldvr, s, sep, mm, m, &
               work, ldwork, iwork,info )
           character, intent(in) :: howmny, job
           integer(ilp), intent(out) :: info, m
           integer(ilp), intent(in) :: ldt, ldvl, ldvr, ldwork, mm, n
           logical(lk), intent(in) :: select(*)
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(out) :: s(*), sep(*), work(ldwork,*)
           real(sp), intent(in) :: t(ldt,*), vl(ldvl,*), vr(ldvr,*)
     end subroutine stdlib_strsna

     module subroutine stdlib_dtrsna( job, howmny, select, n, t, ldt, vl, ldvl, vr,ldvr, s, sep, mm, m, &
               work, ldwork, iwork,info )
           character, intent(in) :: howmny, job
           integer(ilp), intent(out) :: info, m
           integer(ilp), intent(in) :: ldt, ldvl, ldvr, ldwork, mm, n
           logical(lk), intent(in) :: select(*)
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(out) :: s(*), sep(*), work(ldwork,*)
           real(dp), intent(in) :: t(ldt,*), vl(ldvl,*), vr(ldvr,*)
     end subroutine stdlib_dtrsna


     pure module subroutine stdlib_ctrsna( job, howmny, select, n, t, ldt, vl, ldvl, vr,ldvr, s, sep, mm,&
                m, work, ldwork, rwork,info )
           character, intent(in) :: howmny, job
           integer(ilp), intent(out) :: info, m
           integer(ilp), intent(in) :: ldt, ldvl, ldvr, ldwork, mm, n
           logical(lk), intent(in) :: select(*)
           real(sp), intent(out) :: rwork(*), s(*), sep(*)
           complex(sp), intent(in) :: t(ldt,*), vl(ldvl,*), vr(ldvr,*)
           complex(sp), intent(out) :: work(ldwork,*)
     end subroutine stdlib_ctrsna

     pure module subroutine stdlib_ztrsna( job, howmny, select, n, t, ldt, vl, ldvl, vr,ldvr, s, sep, mm,&
                m, work, ldwork, rwork,info )
           character, intent(in) :: howmny, job
           integer(ilp), intent(out) :: info, m
           integer(ilp), intent(in) :: ldt, ldvl, ldvr, ldwork, mm, n
           logical(lk), intent(in) :: select(*)
           real(dp), intent(out) :: rwork(*), s(*), sep(*)
           complex(dp), intent(in) :: t(ldt,*), vl(ldvl,*), vr(ldvr,*)
           complex(dp), intent(out) :: work(ldwork,*)
     end subroutine stdlib_ztrsna


end interface 


interface 
     module subroutine stdlib_strexc( compq, n, t, ldt, q, ldq, ifst, ilst, work,info )
           character, intent(in) :: compq
           integer(ilp), intent(inout) :: ifst, ilst
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldq, ldt, n
           real(sp), intent(inout) :: q(ldq,*), t(ldt,*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_strexc

     module subroutine stdlib_dtrexc( compq, n, t, ldt, q, ldq, ifst, ilst, work,info )
           character, intent(in) :: compq
           integer(ilp), intent(inout) :: ifst, ilst
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldq, ldt, n
           real(dp), intent(inout) :: q(ldq,*), t(ldt,*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_dtrexc


     pure module subroutine stdlib_ctrexc( compq, n, t, ldt, q, ldq, ifst, ilst, info )
           character, intent(in) :: compq
           integer(ilp), intent(in) :: ifst, ilst, ldq, ldt, n
           integer(ilp), intent(out) :: info
           complex(sp), intent(inout) :: q(ldq,*), t(ldt,*)
     end subroutine stdlib_ctrexc

     pure module subroutine stdlib_ztrexc( compq, n, t, ldt, q, ldq, ifst, ilst, info )
           character, intent(in) :: compq
           integer(ilp), intent(in) :: ifst, ilst, ldq, ldt, n
           integer(ilp), intent(out) :: info
           complex(dp), intent(inout) :: q(ldq,*), t(ldt,*)
     end subroutine stdlib_ztrexc


end interface 


interface 
     module subroutine stdlib_strsen( job, compq, select, n, t, ldt, q, ldq, wr, wi,m, s, sep, work, &
               lwork, iwork, liwork, info )
           character, intent(in) :: compq, job
           integer(ilp), intent(out) :: info, m
           integer(ilp), intent(in) :: ldq, ldt, liwork, lwork, n
           real(sp), intent(out) :: s, sep
           logical(lk), intent(in) :: select(*)
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(inout) :: q(ldq,*), t(ldt,*)
           real(sp), intent(out) :: wi(*), work(*), wr(*)
     end subroutine stdlib_strsen

     module subroutine stdlib_dtrsen( job, compq, select, n, t, ldt, q, ldq, wr, wi,m, s, sep, work, &
               lwork, iwork, liwork, info )
           character, intent(in) :: compq, job
           integer(ilp), intent(out) :: info, m
           integer(ilp), intent(in) :: ldq, ldt, liwork, lwork, n
           real(dp), intent(out) :: s, sep
           logical(lk), intent(in) :: select(*)
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(inout) :: q(ldq,*), t(ldt,*)
           real(dp), intent(out) :: wi(*), work(*), wr(*)
     end subroutine stdlib_dtrsen


     module subroutine stdlib_ctrsen( job, compq, select, n, t, ldt, q, ldq, w, m, s,sep, work, lwork, &
               info )
           character, intent(in) :: compq, job
           integer(ilp), intent(out) :: info, m
           integer(ilp), intent(in) :: ldq, ldt, lwork, n
           real(sp), intent(out) :: s, sep
           logical(lk), intent(in) :: select(*)
           complex(sp), intent(inout) :: q(ldq,*), t(ldt,*)
           complex(sp), intent(out) :: w(*), work(*)
     end subroutine stdlib_ctrsen

     module subroutine stdlib_ztrsen( job, compq, select, n, t, ldt, q, ldq, w, m, s,sep, work, lwork, &
               info )
           character, intent(in) :: compq, job
           integer(ilp), intent(out) :: info, m
           integer(ilp), intent(in) :: ldq, ldt, lwork, n
           real(dp), intent(out) :: s, sep
           logical(lk), intent(in) :: select(*)
           complex(dp), intent(inout) :: q(ldq,*), t(ldt,*)
           complex(dp), intent(out) :: w(*), work(*)
     end subroutine stdlib_ztrsen


end interface 


interface 
     module subroutine stdlib_slaexc( wantq, n, t, ldt, q, ldq, j1, n1, n2, work,info )
           logical(lk), intent(in) :: wantq
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: j1, ldq, ldt, n, n1, n2
           real(sp), intent(inout) :: q(ldq,*), t(ldt,*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_slaexc

     module subroutine stdlib_dlaexc( wantq, n, t, ldt, q, ldq, j1, n1, n2, work,info )
           logical(lk), intent(in) :: wantq
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: j1, ldq, ldt, n, n1, n2
           real(dp), intent(inout) :: q(ldq,*), t(ldt,*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_dlaexc


end interface 


interface 
     pure module subroutine stdlib_slanv2( a, b, c, d, rt1r, rt1i, rt2r, rt2i, cs, sn )
           real(sp), intent(inout) :: a, b, c, d
           real(sp), intent(out) :: cs, rt1i, rt1r, rt2i, rt2r, sn
     end subroutine stdlib_slanv2

     pure module subroutine stdlib_dlanv2( a, b, c, d, rt1r, rt1i, rt2r, rt2i, cs, sn )
           real(dp), intent(inout) :: a, b, c, d
           real(dp), intent(out) :: cs, rt1i, rt1r, rt2i, rt2r, sn
     end subroutine stdlib_dlanv2


end interface 


interface 
     pure module subroutine stdlib_slaein( rightv, noinit, n, h, ldh, wr, wi, vr, vi, b,ldb, work, eps3, &
               smlnum, bignum, info )
           logical(lk), intent(in) :: noinit, rightv
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, ldh, n
           real(sp), intent(in) :: bignum, eps3, smlnum, wi, wr
           real(sp), intent(out) :: b(ldb,*), work(*)
           real(sp), intent(in) :: h(ldh,*)
           real(sp), intent(inout) :: vi(*), vr(*)
     end subroutine stdlib_slaein

     pure module subroutine stdlib_dlaein( rightv, noinit, n, h, ldh, wr, wi, vr, vi, b,ldb, work, eps3, &
               smlnum, bignum, info )
           logical(lk), intent(in) :: noinit, rightv
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, ldh, n
           real(dp), intent(in) :: bignum, eps3, smlnum, wi, wr
           real(dp), intent(out) :: b(ldb,*), work(*)
           real(dp), intent(in) :: h(ldh,*)
           real(dp), intent(inout) :: vi(*), vr(*)
     end subroutine stdlib_dlaein


     pure module subroutine stdlib_claein( rightv, noinit, n, h, ldh, w, v, b, ldb, rwork,eps3, smlnum, &
               info )
           logical(lk), intent(in) :: noinit, rightv
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, ldh, n
           real(sp), intent(in) :: eps3, smlnum
           complex(sp), intent(in) :: w
           real(sp), intent(out) :: rwork(*)
           complex(sp), intent(out) :: b(ldb,*)
           complex(sp), intent(in) :: h(ldh,*)
           complex(sp), intent(inout) :: v(*)
     end subroutine stdlib_claein

     pure module subroutine stdlib_zlaein( rightv, noinit, n, h, ldh, w, v, b, ldb, rwork,eps3, smlnum, &
               info )
           logical(lk), intent(in) :: noinit, rightv
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldb, ldh, n
           real(dp), intent(in) :: eps3, smlnum
           complex(dp), intent(in) :: w
           real(dp), intent(out) :: rwork(*)
           complex(dp), intent(out) :: b(ldb,*)
           complex(dp), intent(in) :: h(ldh,*)
           complex(dp), intent(inout) :: v(*)
     end subroutine stdlib_zlaein


end interface 


interface 
     module subroutine stdlib_slaqtr( ltran, lreal, n, t, ldt, b, w, scale, x, work,info )
           logical(lk), intent(in) :: lreal, ltran
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldt, n
           real(sp), intent(out) :: scale
           real(sp), intent(in) :: w
           real(sp), intent(in) :: b(*), t(ldt,*)
           real(sp), intent(out) :: work(*)
           real(sp), intent(inout) :: x(*)
     end subroutine stdlib_slaqtr

     module subroutine stdlib_dlaqtr( ltran, lreal, n, t, ldt, b, w, scale, x, work,info )
           logical(lk), intent(in) :: lreal, ltran
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldt, n
           real(dp), intent(out) :: scale
           real(dp), intent(in) :: w
           real(dp), intent(in) :: b(*), t(ldt,*)
           real(dp), intent(out) :: work(*)
           real(dp), intent(inout) :: x(*)
     end subroutine stdlib_dlaqtr


end interface 


interface 
     pure module subroutine stdlib_slahqr( wantt, wantz, n, ilo, ihi, h, ldh, wr, wi,iloz, ihiz, z, ldz, &
               info )
           integer(ilp), intent(in) :: ihi, ihiz, ilo, iloz, ldh, ldz, n
           integer(ilp), intent(out) :: info
           logical(lk), intent(in) :: wantt, wantz
           real(sp), intent(inout) :: h(ldh,*), z(ldz,*)
           real(sp), intent(out) :: wi(*), wr(*)
     end subroutine stdlib_slahqr

     pure module subroutine stdlib_dlahqr( wantt, wantz, n, ilo, ihi, h, ldh, wr, wi,iloz, ihiz, z, ldz, &
               info )
           integer(ilp), intent(in) :: ihi, ihiz, ilo, iloz, ldh, ldz, n
           integer(ilp), intent(out) :: info
           logical(lk), intent(in) :: wantt, wantz
           real(dp), intent(inout) :: h(ldh,*), z(ldz,*)
           real(dp), intent(out) :: wi(*), wr(*)
     end subroutine stdlib_dlahqr


     pure module subroutine stdlib_clahqr( wantt, wantz, n, ilo, ihi, h, ldh, w, iloz,ihiz, z, ldz, info &
               )
           integer(ilp), intent(in) :: ihi, ihiz, ilo, iloz, ldh, ldz, n
           integer(ilp), intent(out) :: info
           logical(lk), intent(in) :: wantt, wantz
           complex(sp), intent(inout) :: h(ldh,*), z(ldz,*)
           complex(sp), intent(out) :: w(*)
     end subroutine stdlib_clahqr

     pure module subroutine stdlib_zlahqr( wantt, wantz, n, ilo, ihi, h, ldh, w, iloz,ihiz, z, ldz, info &
               )
           integer(ilp), intent(in) :: ihi, ihiz, ilo, iloz, ldh, ldz, n
           integer(ilp), intent(out) :: info
           logical(lk), intent(in) :: wantt, wantz
           complex(dp), intent(inout) :: h(ldh,*), z(ldz,*)
           complex(dp), intent(out) :: w(*)
     end subroutine stdlib_zlahqr


end interface 


interface 
     module subroutine stdlib_slaqr0( wantt, wantz, n, ilo, ihi, h, ldh, wr, wi,iloz, ihiz, z, ldz, work,&
                lwork, info )
           integer(ilp), intent(in) :: ihi, ihiz, ilo, iloz, ldh, ldz, lwork, n
           integer(ilp), intent(out) :: info
           logical(lk), intent(in) :: wantt, wantz
           real(sp), intent(inout) :: h(ldh,*), z(ldz,*)
           real(sp), intent(out) :: wi(*), work(*), wr(*)
     end subroutine stdlib_slaqr0

     module subroutine stdlib_dlaqr0( wantt, wantz, n, ilo, ihi, h, ldh, wr, wi,iloz, ihiz, z, ldz, work,&
                lwork, info )
           integer(ilp), intent(in) :: ihi, ihiz, ilo, iloz, ldh, ldz, lwork, n
           integer(ilp), intent(out) :: info
           logical(lk), intent(in) :: wantt, wantz
           real(dp), intent(inout) :: h(ldh,*), z(ldz,*)
           real(dp), intent(out) :: wi(*), work(*), wr(*)
     end subroutine stdlib_dlaqr0


     pure module subroutine stdlib_claqr0( wantt, wantz, n, ilo, ihi, h, ldh, w, iloz,ihiz, z, ldz, work,&
                lwork, info )
           integer(ilp), intent(in) :: ihi, ihiz, ilo, iloz, ldh, ldz, lwork, n
           integer(ilp), intent(out) :: info
           logical(lk), intent(in) :: wantt, wantz
           complex(sp), intent(inout) :: h(ldh,*), z(ldz,*)
           complex(sp), intent(out) :: w(*), work(*)
     end subroutine stdlib_claqr0

     pure module subroutine stdlib_zlaqr0( wantt, wantz, n, ilo, ihi, h, ldh, w, iloz,ihiz, z, ldz, work,&
                lwork, info )
           integer(ilp), intent(in) :: ihi, ihiz, ilo, iloz, ldh, ldz, lwork, n
           integer(ilp), intent(out) :: info
           logical(lk), intent(in) :: wantt, wantz
           complex(dp), intent(inout) :: h(ldh,*), z(ldz,*)
           complex(dp), intent(out) :: w(*), work(*)
     end subroutine stdlib_zlaqr0


end interface 


interface 
     pure module subroutine stdlib_slaqr1( n, h, ldh, sr1, si1, sr2, si2, v )
           real(sp), intent(in) :: si1, si2, sr1, sr2
           integer(ilp), intent(in) :: ldh, n
           real(sp), intent(in) :: h(ldh,*)
           real(sp), intent(out) :: v(*)
     end subroutine stdlib_slaqr1

     pure module subroutine stdlib_dlaqr1( n, h, ldh, sr1, si1, sr2, si2, v )
           real(dp), intent(in) :: si1, si2, sr1, sr2
           integer(ilp), intent(in) :: ldh, n
           real(dp), intent(in) :: h(ldh,*)
           real(dp), intent(out) :: v(*)
     end subroutine stdlib_dlaqr1


     pure module subroutine stdlib_claqr1( n, h, ldh, s1, s2, v )
           complex(sp), intent(in) :: s1, s2
           integer(ilp), intent(in) :: ldh, n
           complex(sp), intent(in) :: h(ldh,*)
           complex(sp), intent(out) :: v(*)
     end subroutine stdlib_claqr1

     pure module subroutine stdlib_zlaqr1( n, h, ldh, s1, s2, v )
           complex(dp), intent(in) :: s1, s2
           integer(ilp), intent(in) :: ldh, n
           complex(dp), intent(in) :: h(ldh,*)
           complex(dp), intent(out) :: v(*)
     end subroutine stdlib_zlaqr1


end interface 


interface 
     module subroutine stdlib_slaqr2( wantt, wantz, n, ktop, kbot, nw, h, ldh, iloz,ihiz, z, ldz, ns, nd,&
                sr, si, v, ldv, nh, t,ldt, nv, wv, ldwv, work, lwork )
           integer(ilp), intent(in) :: ihiz, iloz, kbot, ktop, ldh, ldt, ldv, ldwv, ldz, lwork, n,&
                      nh, nv, nw
           integer(ilp), intent(out) :: nd, ns
           logical(lk), intent(in) :: wantt, wantz
           real(sp), intent(inout) :: h(ldh,*), z(ldz,*)
           real(sp), intent(out) :: si(*), sr(*), t(ldt,*), v(ldv,*), work(*), wv(ldwv,*)
     end subroutine stdlib_slaqr2

     module subroutine stdlib_dlaqr2( wantt, wantz, n, ktop, kbot, nw, h, ldh, iloz,ihiz, z, ldz, ns, nd,&
                sr, si, v, ldv, nh, t,ldt, nv, wv, ldwv, work, lwork )
           integer(ilp), intent(in) :: ihiz, iloz, kbot, ktop, ldh, ldt, ldv, ldwv, ldz, lwork, n,&
                      nh, nv, nw
           integer(ilp), intent(out) :: nd, ns
           logical(lk), intent(in) :: wantt, wantz
           real(dp), intent(inout) :: h(ldh,*), z(ldz,*)
           real(dp), intent(out) :: si(*), sr(*), t(ldt,*), v(ldv,*), work(*), wv(ldwv,*)
     end subroutine stdlib_dlaqr2


     pure module subroutine stdlib_claqr2( wantt, wantz, n, ktop, kbot, nw, h, ldh, iloz,ihiz, z, ldz, &
               ns, nd, sh, v, ldv, nh, t, ldt,nv, wv, ldwv, work, lwork )
           integer(ilp), intent(in) :: ihiz, iloz, kbot, ktop, ldh, ldt, ldv, ldwv, ldz, lwork, n,&
                      nh, nv, nw
           integer(ilp), intent(out) :: nd, ns
           logical(lk), intent(in) :: wantt, wantz
           complex(sp), intent(inout) :: h(ldh,*), z(ldz,*)
           complex(sp), intent(out) :: sh(*), t(ldt,*), v(ldv,*), work(*), wv(ldwv,*)
     end subroutine stdlib_claqr2

     pure module subroutine stdlib_zlaqr2( wantt, wantz, n, ktop, kbot, nw, h, ldh, iloz,ihiz, z, ldz, &
               ns, nd, sh, v, ldv, nh, t, ldt,nv, wv, ldwv, work, lwork )
           integer(ilp), intent(in) :: ihiz, iloz, kbot, ktop, ldh, ldt, ldv, ldwv, ldz, lwork, n,&
                      nh, nv, nw
           integer(ilp), intent(out) :: nd, ns
           logical(lk), intent(in) :: wantt, wantz
           complex(dp), intent(inout) :: h(ldh,*), z(ldz,*)
           complex(dp), intent(out) :: sh(*), t(ldt,*), v(ldv,*), work(*), wv(ldwv,*)
     end subroutine stdlib_zlaqr2


end interface 


interface 
     module subroutine stdlib_slaqr3( wantt, wantz, n, ktop, kbot, nw, h, ldh, iloz,ihiz, z, ldz, ns, nd,&
                sr, si, v, ldv, nh, t,ldt, nv, wv, ldwv, work, lwork )
           integer(ilp), intent(in) :: ihiz, iloz, kbot, ktop, ldh, ldt, ldv, ldwv, ldz, lwork, n,&
                      nh, nv, nw
           integer(ilp), intent(out) :: nd, ns
           logical(lk), intent(in) :: wantt, wantz
           real(sp), intent(inout) :: h(ldh,*), z(ldz,*)
           real(sp), intent(out) :: si(*), sr(*), t(ldt,*), v(ldv,*), work(*), wv(ldwv,*)
     end subroutine stdlib_slaqr3

     module subroutine stdlib_dlaqr3( wantt, wantz, n, ktop, kbot, nw, h, ldh, iloz,ihiz, z, ldz, ns, nd,&
                sr, si, v, ldv, nh, t,ldt, nv, wv, ldwv, work, lwork )
           integer(ilp), intent(in) :: ihiz, iloz, kbot, ktop, ldh, ldt, ldv, ldwv, ldz, lwork, n,&
                      nh, nv, nw
           integer(ilp), intent(out) :: nd, ns
           logical(lk), intent(in) :: wantt, wantz
           real(dp), intent(inout) :: h(ldh,*), z(ldz,*)
           real(dp), intent(out) :: si(*), sr(*), t(ldt,*), v(ldv,*), work(*), wv(ldwv,*)
     end subroutine stdlib_dlaqr3


     pure module subroutine stdlib_claqr3( wantt, wantz, n, ktop, kbot, nw, h, ldh, iloz,ihiz, z, ldz, &
               ns, nd, sh, v, ldv, nh, t, ldt,nv, wv, ldwv, work, lwork )
           integer(ilp), intent(in) :: ihiz, iloz, kbot, ktop, ldh, ldt, ldv, ldwv, ldz, lwork, n,&
                      nh, nv, nw
           integer(ilp), intent(out) :: nd, ns
           logical(lk), intent(in) :: wantt, wantz
           complex(sp), intent(inout) :: h(ldh,*), z(ldz,*)
           complex(sp), intent(out) :: sh(*), t(ldt,*), v(ldv,*), work(*), wv(ldwv,*)
     end subroutine stdlib_claqr3

     pure module subroutine stdlib_zlaqr3( wantt, wantz, n, ktop, kbot, nw, h, ldh, iloz,ihiz, z, ldz, &
               ns, nd, sh, v, ldv, nh, t, ldt,nv, wv, ldwv, work, lwork )
           integer(ilp), intent(in) :: ihiz, iloz, kbot, ktop, ldh, ldt, ldv, ldwv, ldz, lwork, n,&
                      nh, nv, nw
           integer(ilp), intent(out) :: nd, ns
           logical(lk), intent(in) :: wantt, wantz
           complex(dp), intent(inout) :: h(ldh,*), z(ldz,*)
           complex(dp), intent(out) :: sh(*), t(ldt,*), v(ldv,*), work(*), wv(ldwv,*)
     end subroutine stdlib_zlaqr3


end interface 


interface 
     module subroutine stdlib_slaqr4( wantt, wantz, n, ilo, ihi, h, ldh, wr, wi,iloz, ihiz, z, ldz, work,&
                lwork, info )
           integer(ilp), intent(in) :: ihi, ihiz, ilo, iloz, ldh, ldz, lwork, n
           integer(ilp), intent(out) :: info
           logical(lk), intent(in) :: wantt, wantz
           real(sp), intent(inout) :: h(ldh,*), z(ldz,*)
           real(sp), intent(out) :: wi(*), work(*), wr(*)
     end subroutine stdlib_slaqr4

     module subroutine stdlib_dlaqr4( wantt, wantz, n, ilo, ihi, h, ldh, wr, wi,iloz, ihiz, z, ldz, work,&
                lwork, info )
           integer(ilp), intent(in) :: ihi, ihiz, ilo, iloz, ldh, ldz, lwork, n
           integer(ilp), intent(out) :: info
           logical(lk), intent(in) :: wantt, wantz
           real(dp), intent(inout) :: h(ldh,*), z(ldz,*)
           real(dp), intent(out) :: wi(*), work(*), wr(*)
     end subroutine stdlib_dlaqr4


     pure module subroutine stdlib_claqr4( wantt, wantz, n, ilo, ihi, h, ldh, w, iloz,ihiz, z, ldz, work,&
                lwork, info )
           integer(ilp), intent(in) :: ihi, ihiz, ilo, iloz, ldh, ldz, lwork, n
           integer(ilp), intent(out) :: info
           logical(lk), intent(in) :: wantt, wantz
           complex(sp), intent(inout) :: h(ldh,*), z(ldz,*)
           complex(sp), intent(out) :: w(*), work(*)
     end subroutine stdlib_claqr4

     pure module subroutine stdlib_zlaqr4( wantt, wantz, n, ilo, ihi, h, ldh, w, iloz,ihiz, z, ldz, work,&
                lwork, info )
           integer(ilp), intent(in) :: ihi, ihiz, ilo, iloz, ldh, ldz, lwork, n
           integer(ilp), intent(out) :: info
           logical(lk), intent(in) :: wantt, wantz
           complex(dp), intent(inout) :: h(ldh,*), z(ldz,*)
           complex(dp), intent(out) :: w(*), work(*)
     end subroutine stdlib_zlaqr4


end interface 


interface 
     pure module subroutine stdlib_slaqr5( wantt, wantz, kacc22, n, ktop, kbot, nshfts,sr, si, h, ldh, &
               iloz, ihiz, z, ldz, v, ldv, u,ldu, nv, wv, ldwv, nh, wh, ldwh )
           integer(ilp), intent(in) :: ihiz, iloz, kacc22, kbot, ktop, ldh, ldu, ldv, ldwh, ldwv, &
                     ldz, n, nh, nshfts, nv
           logical(lk), intent(in) :: wantt, wantz
           real(sp), intent(inout) :: h(ldh,*), si(*), sr(*), z(ldz,*)
           real(sp), intent(out) :: u(ldu,*), v(ldv,*), wh(ldwh,*), wv(ldwv,*)
     end subroutine stdlib_slaqr5

     pure module subroutine stdlib_dlaqr5( wantt, wantz, kacc22, n, ktop, kbot, nshfts,sr, si, h, ldh, &
               iloz, ihiz, z, ldz, v, ldv, u,ldu, nv, wv, ldwv, nh, wh, ldwh )
           integer(ilp), intent(in) :: ihiz, iloz, kacc22, kbot, ktop, ldh, ldu, ldv, ldwh, ldwv, &
                     ldz, n, nh, nshfts, nv
           logical(lk), intent(in) :: wantt, wantz
           real(dp), intent(inout) :: h(ldh,*), si(*), sr(*), z(ldz,*)
           real(dp), intent(out) :: u(ldu,*), v(ldv,*), wh(ldwh,*), wv(ldwv,*)
     end subroutine stdlib_dlaqr5


     pure module subroutine stdlib_claqr5( wantt, wantz, kacc22, n, ktop, kbot, nshfts, s,h, ldh, iloz, &
               ihiz, z, ldz, v, ldv, u, ldu, nv,wv, ldwv, nh, wh, ldwh )
           integer(ilp), intent(in) :: ihiz, iloz, kacc22, kbot, ktop, ldh, ldu, ldv, ldwh, ldwv, &
                     ldz, n, nh, nshfts, nv
           logical(lk), intent(in) :: wantt, wantz
           complex(sp), intent(inout) :: h(ldh,*), s(*), z(ldz,*)
           complex(sp), intent(out) :: u(ldu,*), v(ldv,*), wh(ldwh,*), wv(ldwv,*)
     end subroutine stdlib_claqr5

     pure module subroutine stdlib_zlaqr5( wantt, wantz, kacc22, n, ktop, kbot, nshfts, s,h, ldh, iloz, &
               ihiz, z, ldz, v, ldv, u, ldu, nv,wv, ldwv, nh, wh, ldwh )
           integer(ilp), intent(in) :: ihiz, iloz, kacc22, kbot, ktop, ldh, ldu, ldv, ldwh, ldwv, &
                     ldz, n, nh, nshfts, nv
           logical(lk), intent(in) :: wantt, wantz
           complex(dp), intent(inout) :: h(ldh,*), s(*), z(ldz,*)
           complex(dp), intent(out) :: u(ldu,*), v(ldv,*), wh(ldwh,*), wv(ldwv,*)
     end subroutine stdlib_zlaqr5


end interface 


interface 
     recursive module subroutine stdlib_slaqz0( wants, wantq, wantz, n, ilo, ihi, a,lda, b, ldb, alphar, &
               alphai, beta,q, ldq, z, ldz, work, lwork, rec,info )
           character, intent( in ) :: wants, wantq, wantz
           integer(ilp), intent( in ) :: n, ilo, ihi, lda, ldb, ldq, ldz, lwork,rec
           integer(ilp), intent( out ) :: info
           real(sp), intent( inout ) :: a( lda, * ), b( ldb, * ), q( ldq, * ),z( ldz, * ), alphar(&
                      * ), alphai( * ), beta( * ), work( * )
     end subroutine stdlib_slaqz0

     recursive module subroutine stdlib_dlaqz0( wants, wantq, wantz, n, ilo, ihi, a,lda, b, ldb, alphar, &
               alphai, beta,q, ldq, z, ldz, work, lwork, rec,info )
           character, intent( in ) :: wants, wantq, wantz
           integer(ilp), intent( in ) :: n, ilo, ihi, lda, ldb, ldq, ldz, lwork,rec
           integer(ilp), intent( out ) :: info
           real(dp), intent( inout ) :: a( lda, * ), b( ldb, * ),q( ldq, * ), z( ldz, * ), alphar(&
                      * ),alphai( * ), beta( * ), work( * )
     end subroutine stdlib_dlaqz0


     recursive module subroutine stdlib_claqz0( wants, wantq, wantz, n, ilo, ihi, a,lda, b, ldb, alpha, &
               beta, q, ldq, z,ldz, work, lwork, rwork, rec,info )
           character, intent( in ) :: wants, wantq, wantz
           integer(ilp), intent( in ) :: n, ilo, ihi, lda, ldb, ldq, ldz, lwork,rec
           integer(ilp), intent( out ) :: info
           complex(sp), intent( inout ) :: a( lda, * ), b( ldb, * ), q( ldq, * ),z( ldz, * ), &
                     alpha( * ), beta( * ), work( * )
           real(sp), intent( out ) :: rwork( * )
           
     end subroutine stdlib_claqz0

     recursive module subroutine stdlib_zlaqz0( wants, wantq, wantz, n, ilo, ihi, a,lda, b, ldb, alpha, &
               beta, q, ldq, z,ldz, work, lwork, rwork, rec,info )
           character, intent( in ) :: wants, wantq, wantz
           integer(ilp), intent( in ) :: n, ilo, ihi, lda, ldb, ldq, ldz, lwork,rec
           integer(ilp), intent( out ) :: info
           complex(dp), intent( inout ) :: a( lda, * ), b( ldb, * ), q( ldq,* ), z( ldz, * ), &
                     alpha( * ), beta( * ), work( * )
           real(dp), intent( out ) :: rwork( * )
           
     end subroutine stdlib_zlaqz0


end interface 


interface 
     pure module subroutine stdlib_slaqz1( a, lda, b, ldb, sr1, sr2, si, beta1, beta2,v )
           integer(ilp), intent( in ) :: lda, ldb
           real(sp), intent( in ) :: a( lda, * ), b( ldb, * ), sr1, sr2, si,beta1, beta2
           real(sp), intent( out ) :: v( * )
     end subroutine stdlib_slaqz1

     pure module subroutine stdlib_dlaqz1( a, lda, b, ldb, sr1, sr2, si, beta1, beta2,v )
           integer(ilp), intent( in ) :: lda, ldb
           real(dp), intent( in ) :: a( lda, * ), b( ldb, * ), sr1,sr2, si, beta1, beta2
           real(dp), intent( out ) :: v( * )
     end subroutine stdlib_dlaqz1


     pure module subroutine stdlib_claqz1( ilq, ilz, k, istartm, istopm, ihi, a, lda, b,ldb, nq, qstart, &
               q, ldq, nz, zstart, z, ldz )
           logical(lk), intent( in ) :: ilq, ilz
           integer(ilp), intent( in ) :: k, lda, ldb, ldq, ldz, istartm, istopm,nq, nz, qstart, &
                     zstart, ihi
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*), q(ldq,*), z(ldz,*)
           
     end subroutine stdlib_claqz1

     pure module subroutine stdlib_zlaqz1( ilq, ilz, k, istartm, istopm, ihi, a, lda, b,ldb, nq, qstart, &
               q, ldq, nz, zstart, z, ldz )
           logical(lk), intent( in ) :: ilq, ilz
           integer(ilp), intent( in ) :: k, lda, ldb, ldq, ldz, istartm, istopm,nq, nz, qstart, &
                     zstart, ihi
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*), q(ldq,*), z(ldz,*)
           
     end subroutine stdlib_zlaqz1


end interface 


interface 
     pure module subroutine stdlib_slaqz2( ilq, ilz, k, istartm, istopm, ihi, a, lda, b,ldb, nq, qstart, &
               q, ldq, nz, zstart, z, ldz )
           logical(lk), intent( in ) :: ilq, ilz
           integer(ilp), intent( in ) :: k, lda, ldb, ldq, ldz, istartm, istopm,nq, nz, qstart, &
                     zstart, ihi
           real(sp), intent(inout) :: a(lda,*), b(ldb,*), q(ldq,*), z(ldz,*)
     end subroutine stdlib_slaqz2

     pure module subroutine stdlib_dlaqz2( ilq, ilz, k, istartm, istopm, ihi, a, lda, b,ldb, nq, qstart, &
               q, ldq, nz, zstart, z, ldz )
           logical(lk), intent( in ) :: ilq, ilz
           integer(ilp), intent( in ) :: k, lda, ldb, ldq, ldz, istartm, istopm,nq, nz, qstart, &
                     zstart, ihi
           real(dp), intent(inout) :: a(lda,*), b(ldb,*), q(ldq,*), z(ldz,*)
     end subroutine stdlib_dlaqz2


     recursive module subroutine stdlib_claqz2( ilschur, ilq, ilz, n, ilo, ihi, nw,a, lda, b, ldb, q, &
               ldq, z, ldz, ns,nd, alpha, beta, qc, ldqc, zc, ldzc,work, lwork, rwork, rec, info )
           logical(lk), intent( in ) :: ilschur, ilq, ilz
           integer(ilp), intent( in ) :: n, ilo, ihi, nw, lda, ldb, ldq, ldz,ldqc, ldzc, lwork, &
                     rec
           complex(sp), intent( inout ) :: a( lda, * ), b( ldb, * ), q( ldq, * ),z( ldz, * ), &
                     alpha( * ), beta( * )
           integer(ilp), intent( out ) :: ns, nd, info
           complex(sp), intent(inout) :: qc(ldqc,*), zc(ldzc,*)
           complex(sp), intent(out) :: work(*)
           real(sp), intent(out) :: rwork(*)
           
     end subroutine stdlib_claqz2

     recursive module subroutine stdlib_zlaqz2( ilschur, ilq, ilz, n, ilo, ihi, nw,a, lda, b, ldb, q, &
               ldq, z, ldz, ns,nd, alpha, beta, qc, ldqc, zc, ldzc,work, lwork, rwork, rec, info )
           logical(lk), intent( in ) :: ilschur, ilq, ilz
           integer(ilp), intent( in ) :: n, ilo, ihi, nw, lda, ldb, ldq, ldz,ldqc, ldzc, lwork, &
                     rec
           complex(dp), intent( inout ) :: a( lda, * ), b( ldb, * ), q( ldq,* ), z( ldz, * ), &
                     alpha( * ), beta( * )
           integer(ilp), intent( out ) :: ns, nd, info
           complex(dp), intent(inout) :: qc(ldqc,*), zc(ldzc,*)
           complex(dp), intent(out) :: work(*)
           real(dp), intent(out) :: rwork(*)
           
     end subroutine stdlib_zlaqz2


end interface 


interface 
     recursive module subroutine stdlib_slaqz3( ilschur, ilq, ilz, n, ilo, ihi, nw,a, lda, b, ldb, q, &
               ldq, z, ldz, ns,nd, alphar, alphai, beta, qc, ldqc,zc, ldzc, work, lwork, rec, info )
           logical(lk), intent( in ) :: ilschur, ilq, ilz
           integer(ilp), intent( in ) :: n, ilo, ihi, nw, lda, ldb, ldq, ldz,ldqc, ldzc, lwork, &
                     rec
           real(sp), intent( inout ) :: a( lda, * ), b( ldb, * ), q( ldq, * ),z( ldz, * ), alphar(&
                      * ), alphai( * ), beta( * )
           integer(ilp), intent( out ) :: ns, nd, info
           real(sp), intent(inout) :: qc(ldqc,*), zc(ldzc,*)
           real(sp), intent(out) :: work(*)
           
     end subroutine stdlib_slaqz3

     recursive module subroutine stdlib_dlaqz3( ilschur, ilq, ilz, n, ilo, ihi, nw,a, lda, b, ldb, q, &
               ldq, z, ldz, ns,nd, alphar, alphai, beta, qc, ldqc,zc, ldzc, work, lwork, rec, info )
           logical(lk), intent( in ) :: ilschur, ilq, ilz
           integer(ilp), intent( in ) :: n, ilo, ihi, nw, lda, ldb, ldq, ldz,ldqc, ldzc, lwork, &
                     rec
           real(dp), intent( inout ) :: a( lda, * ), b( ldb, * ),q( ldq, * ), z( ldz, * ), alphar(&
                      * ),alphai( * ), beta( * )
           integer(ilp), intent( out ) :: ns, nd, info
           real(dp), intent(inout) :: qc(ldqc,*), zc(ldzc,*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_dlaqz3


     pure module subroutine stdlib_claqz3( ilschur, ilq, ilz, n, ilo, ihi, nshifts,nblock_desired, alpha,&
                beta, a, lda, b, ldb,q, ldq, z, ldz, qc, ldqc, zc, ldzc, work,lwork, info )
           logical(lk), intent( in ) :: ilschur, ilq, ilz
           integer(ilp), intent( in ) :: n, ilo, ihi, lda, ldb, ldq, ldz, lwork,nshifts, &
                     nblock_desired, ldqc, ldzc
           complex(sp), intent( inout ) :: a( lda, * ), b( ldb, * ), q( ldq, * ),z( ldz, * ), qc( &
                     ldqc, * ), zc( ldzc, * ), work( * ),alpha( * ), beta( * )
           integer(ilp), intent( out ) :: info
           
     end subroutine stdlib_claqz3

     pure module subroutine stdlib_zlaqz3( ilschur, ilq, ilz, n, ilo, ihi, nshifts,nblock_desired, alpha,&
                beta, a, lda, b, ldb,q, ldq, z, ldz, qc, ldqc, zc, ldzc, work,lwork, info )
           logical(lk), intent( in ) :: ilschur, ilq, ilz
           integer(ilp), intent( in ) :: n, ilo, ihi, lda, ldb, ldq, ldz, lwork,nshifts, &
                     nblock_desired, ldqc, ldzc
           complex(dp), intent( inout ) :: a( lda, * ), b( ldb, * ), q( ldq,* ), z( ldz, * ), qc( &
                     ldqc, * ), zc( ldzc, * ), work( * ),alpha( * ), beta( * )
           integer(ilp), intent( out ) :: info
           
     end subroutine stdlib_zlaqz3


end interface 


interface 
     pure module subroutine stdlib_slaqz4( ilschur, ilq, ilz, n, ilo, ihi, nshifts,nblock_desired, sr, &
               si, ss, a, lda, b, ldb, q,ldq, z, ldz, qc, ldqc, zc, ldzc, work, lwork,info )
           logical(lk), intent( in ) :: ilschur, ilq, ilz
           integer(ilp), intent( in ) :: n, ilo, ihi, lda, ldb, ldq, ldz, lwork,nshifts, &
                     nblock_desired, ldqc, ldzc
           real(sp), intent( inout ) :: a( lda, * ), b( ldb, * ), q( ldq, * ),z( ldz, * ), qc( &
                     ldqc, * ), zc( ldzc, * ), work( * ), sr( * ),si( * ), ss( * )
           integer(ilp), intent( out ) :: info
     end subroutine stdlib_slaqz4

     pure module subroutine stdlib_dlaqz4( ilschur, ilq, ilz, n, ilo, ihi, nshifts,nblock_desired, sr, &
               si, ss, a, lda, b, ldb, q,ldq, z, ldz, qc, ldqc, zc, ldzc, work, lwork,info )
           logical(lk), intent( in ) :: ilschur, ilq, ilz
           integer(ilp), intent( in ) :: n, ilo, ihi, lda, ldb, ldq, ldz, lwork,nshifts, &
                     nblock_desired, ldqc, ldzc
           real(dp), intent( inout ) :: a( lda, * ), b( ldb, * ),q( ldq, * ), z( ldz, * ), qc( &
                     ldqc, * ),zc( ldzc, * ), work( * ), sr( * ), si( * ),ss( * )
           integer(ilp), intent( out ) :: info
     end subroutine stdlib_dlaqz4


end interface 


interface 
     pure module subroutine stdlib_sggbal( job, n, a, lda, b, ldb, ilo, ihi, lscale,rscale, work, info )
               
           character, intent(in) :: job
           integer(ilp), intent(out) :: ihi, ilo, info
           integer(ilp), intent(in) :: lda, ldb, n
           real(sp), intent(inout) :: a(lda,*), b(ldb,*)
           real(sp), intent(out) :: lscale(*), rscale(*), work(*)
     end subroutine stdlib_sggbal

     pure module subroutine stdlib_dggbal( job, n, a, lda, b, ldb, ilo, ihi, lscale,rscale, work, info )
               
           character, intent(in) :: job
           integer(ilp), intent(out) :: ihi, ilo, info
           integer(ilp), intent(in) :: lda, ldb, n
           real(dp), intent(inout) :: a(lda,*), b(ldb,*)
           real(dp), intent(out) :: lscale(*), rscale(*), work(*)
     end subroutine stdlib_dggbal


     pure module subroutine stdlib_cggbal( job, n, a, lda, b, ldb, ilo, ihi, lscale,rscale, work, info )
               
           character, intent(in) :: job
           integer(ilp), intent(out) :: ihi, ilo, info
           integer(ilp), intent(in) :: lda, ldb, n
           real(sp), intent(out) :: lscale(*), rscale(*), work(*)
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*)
     end subroutine stdlib_cggbal

     pure module subroutine stdlib_zggbal( job, n, a, lda, b, ldb, ilo, ihi, lscale,rscale, work, info )
               
           character, intent(in) :: job
           integer(ilp), intent(out) :: ihi, ilo, info
           integer(ilp), intent(in) :: lda, ldb, n
           real(dp), intent(out) :: lscale(*), rscale(*), work(*)
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*)
     end subroutine stdlib_zggbal


end interface 


interface 
     pure module subroutine stdlib_sgghrd( compq, compz, n, ilo, ihi, a, lda, b, ldb, q,ldq, z, ldz, &
               info )
           character, intent(in) :: compq, compz
           integer(ilp), intent(in) :: ihi, ilo, lda, ldb, ldq, ldz, n
           integer(ilp), intent(out) :: info
           real(sp), intent(inout) :: a(lda,*), b(ldb,*), q(ldq,*), z(ldz,*)
     end subroutine stdlib_sgghrd

     pure module subroutine stdlib_dgghrd( compq, compz, n, ilo, ihi, a, lda, b, ldb, q,ldq, z, ldz, &
               info )
           character, intent(in) :: compq, compz
           integer(ilp), intent(in) :: ihi, ilo, lda, ldb, ldq, ldz, n
           integer(ilp), intent(out) :: info
           real(dp), intent(inout) :: a(lda,*), b(ldb,*), q(ldq,*), z(ldz,*)
     end subroutine stdlib_dgghrd


     pure module subroutine stdlib_cgghrd( compq, compz, n, ilo, ihi, a, lda, b, ldb, q,ldq, z, ldz, &
               info )
           character, intent(in) :: compq, compz
           integer(ilp), intent(in) :: ihi, ilo, lda, ldb, ldq, ldz, n
           integer(ilp), intent(out) :: info
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*), q(ldq,*), z(ldz,*)
     end subroutine stdlib_cgghrd

     pure module subroutine stdlib_zgghrd( compq, compz, n, ilo, ihi, a, lda, b, ldb, q,ldq, z, ldz, &
               info )
           character, intent(in) :: compq, compz
           integer(ilp), intent(in) :: ihi, ilo, lda, ldb, ldq, ldz, n
           integer(ilp), intent(out) :: info
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*), q(ldq,*), z(ldz,*)
     end subroutine stdlib_zgghrd


end interface 


interface 
     pure module subroutine stdlib_sgghd3( compq, compz, n, ilo, ihi, a, lda, b, ldb, q,ldq, z, ldz, &
               work, lwork, info )
           character, intent(in) :: compq, compz
           integer(ilp), intent(in) :: ihi, ilo, lda, ldb, ldq, ldz, n, lwork
           integer(ilp), intent(out) :: info
           real(sp), intent(inout) :: a(lda,*), b(ldb,*), q(ldq,*), z(ldz,*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_sgghd3

     pure module subroutine stdlib_dgghd3( compq, compz, n, ilo, ihi, a, lda, b, ldb, q,ldq, z, ldz, &
               work, lwork, info )
           character, intent(in) :: compq, compz
           integer(ilp), intent(in) :: ihi, ilo, lda, ldb, ldq, ldz, n, lwork
           integer(ilp), intent(out) :: info
           real(dp), intent(inout) :: a(lda,*), b(ldb,*), q(ldq,*), z(ldz,*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_dgghd3


     pure module subroutine stdlib_cgghd3( compq, compz, n, ilo, ihi, a, lda, b, ldb, q,ldq, z, ldz, &
               work, lwork, info )
           character, intent(in) :: compq, compz
           integer(ilp), intent(in) :: ihi, ilo, lda, ldb, ldq, ldz, n, lwork
           integer(ilp), intent(out) :: info
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*), q(ldq,*), z(ldz,*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_cgghd3

     pure module subroutine stdlib_zgghd3( compq, compz, n, ilo, ihi, a, lda, b, ldb, q,ldq, z, ldz, &
               work, lwork, info )
           character, intent(in) :: compq, compz
           integer(ilp), intent(in) :: ihi, ilo, lda, ldb, ldq, ldz, n, lwork
           integer(ilp), intent(out) :: info
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*), q(ldq,*), z(ldz,*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_zgghd3


end interface 


interface 
     module subroutine stdlib_shgeqz( job, compq, compz, n, ilo, ihi, h, ldh, t, ldt,alphar, alphai, &
               beta, q, ldq, z, ldz, work,lwork, info )
           character, intent(in) :: compq, compz, job
           integer(ilp), intent(in) :: ihi, ilo, ldh, ldq, ldt, ldz, lwork, n
           integer(ilp), intent(out) :: info
           real(sp), intent(out) :: alphai(*), alphar(*), beta(*), work(*)
           real(sp), intent(inout) :: h(ldh,*), q(ldq,*), t(ldt,*), z(ldz,*)
     end subroutine stdlib_shgeqz

     module subroutine stdlib_dhgeqz( job, compq, compz, n, ilo, ihi, h, ldh, t, ldt,alphar, alphai, &
               beta, q, ldq, z, ldz, work,lwork, info )
           character, intent(in) :: compq, compz, job
           integer(ilp), intent(in) :: ihi, ilo, ldh, ldq, ldt, ldz, lwork, n
           integer(ilp), intent(out) :: info
           real(dp), intent(out) :: alphai(*), alphar(*), beta(*), work(*)
           real(dp), intent(inout) :: h(ldh,*), q(ldq,*), t(ldt,*), z(ldz,*)
     end subroutine stdlib_dhgeqz


     module subroutine stdlib_chgeqz( job, compq, compz, n, ilo, ihi, h, ldh, t, ldt,alpha, beta, q, ldq,&
                z, ldz, work, lwork,rwork, info )
           character, intent(in) :: compq, compz, job
           integer(ilp), intent(in) :: ihi, ilo, ldh, ldq, ldt, ldz, lwork, n
           integer(ilp), intent(out) :: info
           real(sp), intent(out) :: rwork(*)
           complex(sp), intent(out) :: alpha(*), beta(*), work(*)
           complex(sp), intent(inout) :: h(ldh,*), q(ldq,*), t(ldt,*), z(ldz,*)
     end subroutine stdlib_chgeqz

     module subroutine stdlib_zhgeqz( job, compq, compz, n, ilo, ihi, h, ldh, t, ldt,alpha, beta, q, ldq,&
                z, ldz, work, lwork,rwork, info )
           character, intent(in) :: compq, compz, job
           integer(ilp), intent(in) :: ihi, ilo, ldh, ldq, ldt, ldz, lwork, n
           integer(ilp), intent(out) :: info
           real(dp), intent(out) :: rwork(*)
           complex(dp), intent(out) :: alpha(*), beta(*), work(*)
           complex(dp), intent(inout) :: h(ldh,*), q(ldq,*), t(ldt,*), z(ldz,*)
     end subroutine stdlib_zhgeqz


end interface 


interface 
     pure module subroutine stdlib_sggbak( job, side, n, ilo, ihi, lscale, rscale, m, v,ldv, info )
               
           character, intent(in) :: job, side
           integer(ilp), intent(in) :: ihi, ilo, ldv, m, n
           integer(ilp), intent(out) :: info
           real(sp), intent(in) :: lscale(*), rscale(*)
           real(sp), intent(inout) :: v(ldv,*)
     end subroutine stdlib_sggbak

     pure module subroutine stdlib_dggbak( job, side, n, ilo, ihi, lscale, rscale, m, v,ldv, info )
               
           character, intent(in) :: job, side
           integer(ilp), intent(in) :: ihi, ilo, ldv, m, n
           integer(ilp), intent(out) :: info
           real(dp), intent(in) :: lscale(*), rscale(*)
           real(dp), intent(inout) :: v(ldv,*)
     end subroutine stdlib_dggbak


     pure module subroutine stdlib_cggbak( job, side, n, ilo, ihi, lscale, rscale, m, v,ldv, info )
               
           character, intent(in) :: job, side
           integer(ilp), intent(in) :: ihi, ilo, ldv, m, n
           integer(ilp), intent(out) :: info
           real(sp), intent(in) :: lscale(*), rscale(*)
           complex(sp), intent(inout) :: v(ldv,*)
     end subroutine stdlib_cggbak

     pure module subroutine stdlib_zggbak( job, side, n, ilo, ihi, lscale, rscale, m, v,ldv, info )
               
           character, intent(in) :: job, side
           integer(ilp), intent(in) :: ihi, ilo, ldv, m, n
           integer(ilp), intent(out) :: info
           real(dp), intent(in) :: lscale(*), rscale(*)
           complex(dp), intent(inout) :: v(ldv,*)
     end subroutine stdlib_zggbak


end interface 


interface 
     pure module subroutine stdlib_stgsen( ijob, wantq, wantz, select, n, a, lda, b, ldb,alphar, alphai, &
               beta, q, ldq, z, ldz, m, pl,pr, dif, work, lwork, iwork, liwork, info )
           logical(lk), intent(in) :: wantq, wantz
           integer(ilp), intent(in) :: ijob, lda, ldb, ldq, ldz, liwork, lwork, n
           integer(ilp), intent(out) :: info, m
           real(sp), intent(out) :: pl, pr
           logical(lk), intent(in) :: select(*)
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(inout) :: a(lda,*), b(ldb,*), q(ldq,*), z(ldz,*)
           real(sp), intent(out) :: alphai(*), alphar(*), beta(*), dif(*), work(*)
     end subroutine stdlib_stgsen

     pure module subroutine stdlib_dtgsen( ijob, wantq, wantz, select, n, a, lda, b, ldb,alphar, alphai, &
               beta, q, ldq, z, ldz, m, pl,pr, dif, work, lwork, iwork, liwork, info )
           logical(lk), intent(in) :: wantq, wantz
           integer(ilp), intent(in) :: ijob, lda, ldb, ldq, ldz, liwork, lwork, n
           integer(ilp), intent(out) :: info, m
           real(dp), intent(out) :: pl, pr
           logical(lk), intent(in) :: select(*)
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(inout) :: a(lda,*), b(ldb,*), q(ldq,*), z(ldz,*)
           real(dp), intent(out) :: alphai(*), alphar(*), beta(*), dif(*), work(*)
     end subroutine stdlib_dtgsen


     pure module subroutine stdlib_ctgsen( ijob, wantq, wantz, select, n, a, lda, b, ldb,alpha, beta, q, &
               ldq, z, ldz, m, pl, pr, dif,work, lwork, iwork, liwork, info )
           logical(lk), intent(in) :: wantq, wantz
           integer(ilp), intent(in) :: ijob, lda, ldb, ldq, ldz, liwork, lwork, n
           integer(ilp), intent(out) :: info, m
           real(sp), intent(out) :: pl, pr
           logical(lk), intent(in) :: select(*)
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(out) :: dif(*)
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*), q(ldq,*), z(ldz,*)
           complex(sp), intent(out) :: alpha(*), beta(*), work(*)
     end subroutine stdlib_ctgsen

     pure module subroutine stdlib_ztgsen( ijob, wantq, wantz, select, n, a, lda, b, ldb,alpha, beta, q, &
               ldq, z, ldz, m, pl, pr, dif,work, lwork, iwork, liwork, info )
           logical(lk), intent(in) :: wantq, wantz
           integer(ilp), intent(in) :: ijob, lda, ldb, ldq, ldz, liwork, lwork, n
           integer(ilp), intent(out) :: info, m
           real(dp), intent(out) :: pl, pr
           logical(lk), intent(in) :: select(*)
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(out) :: dif(*)
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*), q(ldq,*), z(ldz,*)
           complex(dp), intent(out) :: alpha(*), beta(*), work(*)
     end subroutine stdlib_ztgsen


end interface 


interface 
     pure module subroutine stdlib_stgsna( job, howmny, select, n, a, lda, b, ldb, vl,ldvl, vr, ldvr, s, &
               dif, mm, m, work, lwork,iwork, info )
           character, intent(in) :: howmny, job
           integer(ilp), intent(out) :: info, m
           integer(ilp), intent(in) :: lda, ldb, ldvl, ldvr, lwork, mm, n
           logical(lk), intent(in) :: select(*)
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(in) :: a(lda,*), b(ldb,*), vl(ldvl,*), vr(ldvr,*)
           real(sp), intent(out) :: dif(*), s(*), work(*)
     end subroutine stdlib_stgsna

     pure module subroutine stdlib_dtgsna( job, howmny, select, n, a, lda, b, ldb, vl,ldvl, vr, ldvr, s, &
               dif, mm, m, work, lwork,iwork, info )
           character, intent(in) :: howmny, job
           integer(ilp), intent(out) :: info, m
           integer(ilp), intent(in) :: lda, ldb, ldvl, ldvr, lwork, mm, n
           logical(lk), intent(in) :: select(*)
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(in) :: a(lda,*), b(ldb,*), vl(ldvl,*), vr(ldvr,*)
           real(dp), intent(out) :: dif(*), s(*), work(*)
     end subroutine stdlib_dtgsna


     pure module subroutine stdlib_ctgsna( job, howmny, select, n, a, lda, b, ldb, vl,ldvl, vr, ldvr, s, &
               dif, mm, m, work, lwork,iwork, info )
           character, intent(in) :: howmny, job
           integer(ilp), intent(out) :: info, m
           integer(ilp), intent(in) :: lda, ldb, ldvl, ldvr, lwork, mm, n
           logical(lk), intent(in) :: select(*)
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(out) :: dif(*), s(*)
           complex(sp), intent(in) :: a(lda,*), b(ldb,*), vl(ldvl,*), vr(ldvr,*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_ctgsna

     pure module subroutine stdlib_ztgsna( job, howmny, select, n, a, lda, b, ldb, vl,ldvl, vr, ldvr, s, &
               dif, mm, m, work, lwork,iwork, info )
           character, intent(in) :: howmny, job
           integer(ilp), intent(out) :: info, m
           integer(ilp), intent(in) :: lda, ldb, ldvl, ldvr, lwork, mm, n
           logical(lk), intent(in) :: select(*)
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(out) :: dif(*), s(*)
           complex(dp), intent(in) :: a(lda,*), b(ldb,*), vl(ldvl,*), vr(ldvr,*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_ztgsna


end interface 


interface 
     pure module subroutine stdlib_stgsyl( trans, ijob, m, n, a, lda, b, ldb, c, ldc, d,ldd, e, lde, f, &
               ldf, scale, dif, work, lwork,iwork, info )
           character, intent(in) :: trans
           integer(ilp), intent(in) :: ijob, lda, ldb, ldc, ldd, lde, ldf, lwork, m, n
           integer(ilp), intent(out) :: info
           real(sp), intent(out) :: dif, scale
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(in) :: a(lda,*), b(ldb,*), d(ldd,*), e(lde,*)
           real(sp), intent(inout) :: c(ldc,*), f(ldf,*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_stgsyl

     pure module subroutine stdlib_dtgsyl( trans, ijob, m, n, a, lda, b, ldb, c, ldc, d,ldd, e, lde, f, &
               ldf, scale, dif, work, lwork,iwork, info )
           character, intent(in) :: trans
           integer(ilp), intent(in) :: ijob, lda, ldb, ldc, ldd, lde, ldf, lwork, m, n
           integer(ilp), intent(out) :: info
           real(dp), intent(out) :: dif, scale
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(in) :: a(lda,*), b(ldb,*), d(ldd,*), e(lde,*)
           real(dp), intent(inout) :: c(ldc,*), f(ldf,*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_dtgsyl


     pure module subroutine stdlib_ctgsyl( trans, ijob, m, n, a, lda, b, ldb, c, ldc, d,ldd, e, lde, f, &
               ldf, scale, dif, work, lwork,iwork, info )
           character, intent(in) :: trans
           integer(ilp), intent(in) :: ijob, lda, ldb, ldc, ldd, lde, ldf, lwork, m, n
           integer(ilp), intent(out) :: info
           real(sp), intent(out) :: dif, scale
           integer(ilp), intent(out) :: iwork(*)
           complex(sp), intent(in) :: a(lda,*), b(ldb,*), d(ldd,*), e(lde,*)
           complex(sp), intent(inout) :: c(ldc,*), f(ldf,*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_ctgsyl

     pure module subroutine stdlib_ztgsyl( trans, ijob, m, n, a, lda, b, ldb, c, ldc, d,ldd, e, lde, f, &
               ldf, scale, dif, work, lwork,iwork, info )
           character, intent(in) :: trans
           integer(ilp), intent(in) :: ijob, lda, ldb, ldc, ldd, lde, ldf, lwork, m, n
           integer(ilp), intent(out) :: info
           real(dp), intent(out) :: dif, scale
           integer(ilp), intent(out) :: iwork(*)
           complex(dp), intent(in) :: a(lda,*), b(ldb,*), d(ldd,*), e(lde,*)
           complex(dp), intent(inout) :: c(ldc,*), f(ldf,*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_ztgsyl


end interface 


interface 
     pure module subroutine stdlib_stgsy2( trans, ijob, m, n, a, lda, b, ldb, c, ldc, d,ldd, e, lde, f, &
               ldf, scale, rdsum, rdscal,iwork, pq, info )
           character, intent(in) :: trans
           integer(ilp), intent(in) :: ijob, lda, ldb, ldc, ldd, lde, ldf, m, n
           integer(ilp), intent(out) :: info, pq
           real(sp), intent(inout) :: rdscal, rdsum
           real(sp), intent(out) :: scale
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(in) :: a(lda,*), b(ldb,*), d(ldd,*), e(lde,*)
           real(sp), intent(inout) :: c(ldc,*), f(ldf,*)
     end subroutine stdlib_stgsy2

     pure module subroutine stdlib_dtgsy2( trans, ijob, m, n, a, lda, b, ldb, c, ldc, d,ldd, e, lde, f, &
               ldf, scale, rdsum, rdscal,iwork, pq, info )
           character, intent(in) :: trans
           integer(ilp), intent(in) :: ijob, lda, ldb, ldc, ldd, lde, ldf, m, n
           integer(ilp), intent(out) :: info, pq
           real(dp), intent(inout) :: rdscal, rdsum
           real(dp), intent(out) :: scale
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(in) :: a(lda,*), b(ldb,*), d(ldd,*), e(lde,*)
           real(dp), intent(inout) :: c(ldc,*), f(ldf,*)
     end subroutine stdlib_dtgsy2


     pure module subroutine stdlib_ctgsy2( trans, ijob, m, n, a, lda, b, ldb, c, ldc, d,ldd, e, lde, f, &
               ldf, scale, rdsum, rdscal,info )
           character, intent(in) :: trans
           integer(ilp), intent(in) :: ijob, lda, ldb, ldc, ldd, lde, ldf, m, n
           integer(ilp), intent(out) :: info
           real(sp), intent(inout) :: rdscal, rdsum
           real(sp), intent(out) :: scale
           complex(sp), intent(in) :: a(lda,*), b(ldb,*), d(ldd,*), e(lde,*)
           complex(sp), intent(inout) :: c(ldc,*), f(ldf,*)
     end subroutine stdlib_ctgsy2

     pure module subroutine stdlib_ztgsy2( trans, ijob, m, n, a, lda, b, ldb, c, ldc, d,ldd, e, lde, f, &
               ldf, scale, rdsum, rdscal,info )
           character, intent(in) :: trans
           integer(ilp), intent(in) :: ijob, lda, ldb, ldc, ldd, lde, ldf, m, n
           integer(ilp), intent(out) :: info
           real(dp), intent(inout) :: rdscal, rdsum
           real(dp), intent(out) :: scale
           complex(dp), intent(in) :: a(lda,*), b(ldb,*), d(ldd,*), e(lde,*)
           complex(dp), intent(inout) :: c(ldc,*), f(ldf,*)
     end subroutine stdlib_ztgsy2


end interface 


interface 
     pure module subroutine stdlib_slagv2( a, lda, b, ldb, alphar, alphai, beta, csl, snl,csr, snr )
               
           integer(ilp), intent(in) :: lda, ldb
           real(sp), intent(out) :: csl, csr, snl, snr
           real(sp), intent(inout) :: a(lda,*), b(ldb,*)
           real(sp), intent(out) :: alphai(2_ilp), alphar(2_ilp), beta(2_ilp)
     end subroutine stdlib_slagv2

     pure module subroutine stdlib_dlagv2( a, lda, b, ldb, alphar, alphai, beta, csl, snl,csr, snr )
               
           integer(ilp), intent(in) :: lda, ldb
           real(dp), intent(out) :: csl, csr, snl, snr
           real(dp), intent(inout) :: a(lda,*), b(ldb,*)
           real(dp), intent(out) :: alphai(2_ilp), alphar(2_ilp), beta(2_ilp)
     end subroutine stdlib_dlagv2


end interface 


interface 
     pure module subroutine stdlib_stgevc( side, howmny, select, n, s, lds, p, ldp, vl,ldvl, vr, ldvr, &
               mm, m, work, info )
           character, intent(in) :: howmny, side
           integer(ilp), intent(out) :: info, m
           integer(ilp), intent(in) :: ldp, lds, ldvl, ldvr, mm, n
           logical(lk), intent(in) :: select(*)
           real(sp), intent(in) :: p(ldp,*), s(lds,*)
           real(sp), intent(inout) :: vl(ldvl,*), vr(ldvr,*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_stgevc

     pure module subroutine stdlib_dtgevc( side, howmny, select, n, s, lds, p, ldp, vl,ldvl, vr, ldvr, &
               mm, m, work, info )
           character, intent(in) :: howmny, side
           integer(ilp), intent(out) :: info, m
           integer(ilp), intent(in) :: ldp, lds, ldvl, ldvr, mm, n
           logical(lk), intent(in) :: select(*)
           real(dp), intent(in) :: p(ldp,*), s(lds,*)
           real(dp), intent(inout) :: vl(ldvl,*), vr(ldvr,*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_dtgevc


     pure module subroutine stdlib_ctgevc( side, howmny, select, n, s, lds, p, ldp, vl,ldvl, vr, ldvr, &
               mm, m, work, rwork, info )
           character, intent(in) :: howmny, side
           integer(ilp), intent(out) :: info, m
           integer(ilp), intent(in) :: ldp, lds, ldvl, ldvr, mm, n
           logical(lk), intent(in) :: select(*)
           real(sp), intent(out) :: rwork(*)
           complex(sp), intent(in) :: p(ldp,*), s(lds,*)
           complex(sp), intent(inout) :: vl(ldvl,*), vr(ldvr,*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_ctgevc

     pure module subroutine stdlib_ztgevc( side, howmny, select, n, s, lds, p, ldp, vl,ldvl, vr, ldvr, &
               mm, m, work, rwork, info )
           character, intent(in) :: howmny, side
           integer(ilp), intent(out) :: info, m
           integer(ilp), intent(in) :: ldp, lds, ldvl, ldvr, mm, n
           logical(lk), intent(in) :: select(*)
           real(dp), intent(out) :: rwork(*)
           complex(dp), intent(in) :: p(ldp,*), s(lds,*)
           complex(dp), intent(inout) :: vl(ldvl,*), vr(ldvr,*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_ztgevc


end interface 


interface 
     pure module subroutine stdlib_stgexc( wantq, wantz, n, a, lda, b, ldb, q, ldq, z,ldz, ifst, ilst, &
               work, lwork, info )
           logical(lk), intent(in) :: wantq, wantz
           integer(ilp), intent(inout) :: ifst, ilst
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, ldq, ldz, lwork, n
           real(sp), intent(inout) :: a(lda,*), b(ldb,*), q(ldq,*), z(ldz,*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_stgexc

     pure module subroutine stdlib_dtgexc( wantq, wantz, n, a, lda, b, ldb, q, ldq, z,ldz, ifst, ilst, &
               work, lwork, info )
           logical(lk), intent(in) :: wantq, wantz
           integer(ilp), intent(inout) :: ifst, ilst
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, ldq, ldz, lwork, n
           real(dp), intent(inout) :: a(lda,*), b(ldb,*), q(ldq,*), z(ldz,*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_dtgexc


     pure module subroutine stdlib_ctgexc( wantq, wantz, n, a, lda, b, ldb, q, ldq, z,ldz, ifst, ilst, &
               info )
           logical(lk), intent(in) :: wantq, wantz
           integer(ilp), intent(in) :: ifst, lda, ldb, ldq, ldz, n
           integer(ilp), intent(inout) :: ilst
           integer(ilp), intent(out) :: info
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*), q(ldq,*), z(ldz,*)
     end subroutine stdlib_ctgexc

     pure module subroutine stdlib_ztgexc( wantq, wantz, n, a, lda, b, ldb, q, ldq, z,ldz, ifst, ilst, &
               info )
           logical(lk), intent(in) :: wantq, wantz
           integer(ilp), intent(in) :: ifst, lda, ldb, ldq, ldz, n
           integer(ilp), intent(inout) :: ilst
           integer(ilp), intent(out) :: info
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*), q(ldq,*), z(ldz,*)
     end subroutine stdlib_ztgexc


end interface 


interface 
     pure module subroutine stdlib_stgex2( wantq, wantz, n, a, lda, b, ldb, q, ldq, z,ldz, j1, n1, n2, &
               work, lwork, info )
           logical(lk), intent(in) :: wantq, wantz
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: j1, lda, ldb, ldq, ldz, lwork, n, n1, n2
           real(sp), intent(inout) :: a(lda,*), b(ldb,*), q(ldq,*), z(ldz,*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_stgex2

     pure module subroutine stdlib_dtgex2( wantq, wantz, n, a, lda, b, ldb, q, ldq, z,ldz, j1, n1, n2, &
               work, lwork, info )
           logical(lk), intent(in) :: wantq, wantz
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: j1, lda, ldb, ldq, ldz, lwork, n, n1, n2
           real(dp), intent(inout) :: a(lda,*), b(ldb,*), q(ldq,*), z(ldz,*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_dtgex2


     pure module subroutine stdlib_ctgex2( wantq, wantz, n, a, lda, b, ldb, q, ldq, z,ldz, j1, info )
               
           logical(lk), intent(in) :: wantq, wantz
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: j1, lda, ldb, ldq, ldz, n
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*), q(ldq,*), z(ldz,*)
     end subroutine stdlib_ctgex2

     pure module subroutine stdlib_ztgex2( wantq, wantz, n, a, lda, b, ldb, q, ldq, z,ldz, j1, info )
               
           logical(lk), intent(in) :: wantq, wantz
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: j1, lda, ldb, ldq, ldz, n
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*), q(ldq,*), z(ldz,*)
     end subroutine stdlib_ztgex2


end interface 


interface 
     pure module subroutine stdlib_slaebz( ijob, nitmax, n, mmax, minp, nbmin, abstol,reltol, pivmin, d, &
               e, e2, nval, ab, c, mout,nab, work, iwork, info )
           integer(ilp), intent(in) :: ijob, minp, mmax, n, nbmin, nitmax
           integer(ilp), intent(out) :: info, mout
           real(sp), intent(in) :: abstol, pivmin, reltol
           integer(ilp), intent(out) :: iwork(*)
           integer(ilp), intent(inout) :: nab(mmax,*), nval(*)
           real(sp), intent(inout) :: ab(mmax,*), c(*)
           real(sp), intent(in) :: d(*), e(*), e2(*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_slaebz

     pure module subroutine stdlib_dlaebz( ijob, nitmax, n, mmax, minp, nbmin, abstol,reltol, pivmin, d, &
               e, e2, nval, ab, c, mout,nab, work, iwork, info )
           integer(ilp), intent(in) :: ijob, minp, mmax, n, nbmin, nitmax
           integer(ilp), intent(out) :: info, mout
           real(dp), intent(in) :: abstol, pivmin, reltol
           integer(ilp), intent(out) :: iwork(*)
           integer(ilp), intent(inout) :: nab(mmax,*), nval(*)
           real(dp), intent(inout) :: ab(mmax,*), c(*)
           real(dp), intent(in) :: d(*), e(*), e2(*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_dlaebz


end interface 


interface 
     pure integer(ilp) module function stdlib_slaneg( n, d, lld, sigma, pivmin, r )
           integer(ilp), intent(in) :: n, r
           real(sp), intent(in) :: pivmin, sigma
           real(sp), intent(in) :: d(*), lld(*)
     end function stdlib_slaneg

     pure integer(ilp) module function stdlib_dlaneg( n, d, lld, sigma, pivmin, r )
           integer(ilp), intent(in) :: n, r
           real(dp), intent(in) :: pivmin, sigma
           real(dp), intent(in) :: d(*), lld(*)
     end function stdlib_dlaneg


end interface 


interface 
     pure module subroutine stdlib_slaed0( icompq, qsiz, n, d, e, q, ldq, qstore, ldqs,work, iwork, info &
               )
           integer(ilp), intent(in) :: icompq, ldq, ldqs, n, qsiz
           integer(ilp), intent(out) :: info
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(inout) :: d(*), e(*), q(ldq,*)
           real(sp), intent(out) :: qstore(ldqs,*), work(*)
     end subroutine stdlib_slaed0

     pure module subroutine stdlib_dlaed0( icompq, qsiz, n, d, e, q, ldq, qstore, ldqs,work, iwork, info &
               )
           integer(ilp), intent(in) :: icompq, ldq, ldqs, n, qsiz
           integer(ilp), intent(out) :: info
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(inout) :: d(*), e(*), q(ldq,*)
           real(dp), intent(out) :: qstore(ldqs,*), work(*)
     end subroutine stdlib_dlaed0


     pure module subroutine stdlib_claed0( qsiz, n, d, e, q, ldq, qstore, ldqs, rwork,iwork, info )
               
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldq, ldqs, n, qsiz
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(inout) :: d(*), e(*)
           real(sp), intent(out) :: rwork(*)
           complex(sp), intent(inout) :: q(ldq,*)
           complex(sp), intent(out) :: qstore(ldqs,*)
     end subroutine stdlib_claed0

     pure module subroutine stdlib_zlaed0( qsiz, n, d, e, q, ldq, qstore, ldqs, rwork,iwork, info )
               
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldq, ldqs, n, qsiz
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(inout) :: d(*), e(*)
           real(dp), intent(out) :: rwork(*)
           complex(dp), intent(inout) :: q(ldq,*)
           complex(dp), intent(out) :: qstore(ldqs,*)
     end subroutine stdlib_zlaed0


end interface 


interface 
     pure module subroutine stdlib_slaed1( n, d, q, ldq, indxq, rho, cutpnt, work, iwork,info )
           integer(ilp), intent(in) :: cutpnt, ldq, n
           integer(ilp), intent(out) :: info
           real(sp), intent(inout) :: rho
           integer(ilp), intent(inout) :: indxq(*)
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(inout) :: d(*), q(ldq,*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_slaed1

     pure module subroutine stdlib_dlaed1( n, d, q, ldq, indxq, rho, cutpnt, work, iwork,info )
           integer(ilp), intent(in) :: cutpnt, ldq, n
           integer(ilp), intent(out) :: info
           real(dp), intent(inout) :: rho
           integer(ilp), intent(inout) :: indxq(*)
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(inout) :: d(*), q(ldq,*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_dlaed1


end interface 


interface 
     pure module subroutine stdlib_slaed2( k, n, n1, d, q, ldq, indxq, rho, z, dlamda, w,q2, indx, indxc,&
                indxp, coltyp, info )
           integer(ilp), intent(out) :: info, k
           integer(ilp), intent(in) :: ldq, n, n1
           real(sp), intent(inout) :: rho
           integer(ilp), intent(out) :: coltyp(*), indx(*), indxc(*), indxp(*)
           integer(ilp), intent(inout) :: indxq(*)
           real(sp), intent(inout) :: d(*), q(ldq,*), z(*)
           real(sp), intent(out) :: dlamda(*), q2(*), w(*)
     end subroutine stdlib_slaed2

     pure module subroutine stdlib_dlaed2( k, n, n1, d, q, ldq, indxq, rho, z, dlamda, w,q2, indx, indxc,&
                indxp, coltyp, info )
           integer(ilp), intent(out) :: info, k
           integer(ilp), intent(in) :: ldq, n, n1
           real(dp), intent(inout) :: rho
           integer(ilp), intent(out) :: coltyp(*), indx(*), indxc(*), indxp(*)
           integer(ilp), intent(inout) :: indxq(*)
           real(dp), intent(inout) :: d(*), q(ldq,*), z(*)
           real(dp), intent(out) :: dlamda(*), q2(*), w(*)
     end subroutine stdlib_dlaed2


end interface 


interface 
     pure module subroutine stdlib_slaed3( k, n, n1, d, q, ldq, rho, dlamda, q2, indx,ctot, w, s, info )
               
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, ldq, n, n1
           real(sp), intent(in) :: rho
           integer(ilp), intent(in) :: ctot(*), indx(*)
           real(sp), intent(out) :: d(*), q(ldq,*), s(*)
           real(sp), intent(inout) :: dlamda(*), w(*)
           real(sp), intent(in) :: q2(*)
     end subroutine stdlib_slaed3

     pure module subroutine stdlib_dlaed3( k, n, n1, d, q, ldq, rho, dlamda, q2, indx,ctot, w, s, info )
               
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, ldq, n, n1
           real(dp), intent(in) :: rho
           integer(ilp), intent(in) :: ctot(*), indx(*)
           real(dp), intent(out) :: d(*), q(ldq,*), s(*)
           real(dp), intent(inout) :: dlamda(*), w(*)
           real(dp), intent(in) :: q2(*)
     end subroutine stdlib_dlaed3


end interface 


interface 
     pure module subroutine stdlib_slaed4( n, i, d, z, delta, rho, dlam, info )
           integer(ilp), intent(in) :: i, n
           integer(ilp), intent(out) :: info
           real(sp), intent(out) :: dlam
           real(sp), intent(in) :: rho
           real(sp), intent(in) :: d(*), z(*)
           real(sp), intent(out) :: delta(*)
     end subroutine stdlib_slaed4

     pure module subroutine stdlib_dlaed4( n, i, d, z, delta, rho, dlam, info )
           integer(ilp), intent(in) :: i, n
           integer(ilp), intent(out) :: info
           real(dp), intent(out) :: dlam
           real(dp), intent(in) :: rho
           real(dp), intent(in) :: d(*), z(*)
           real(dp), intent(out) :: delta(*)
     end subroutine stdlib_dlaed4


end interface 


interface 
     pure module subroutine stdlib_slaed5( i, d, z, delta, rho, dlam )
           integer(ilp), intent(in) :: i
           real(sp), intent(out) :: dlam
           real(sp), intent(in) :: rho
           real(sp), intent(in) :: d(2_ilp), z(2_ilp)
           real(sp), intent(out) :: delta(2_ilp)
     end subroutine stdlib_slaed5

     pure module subroutine stdlib_dlaed5( i, d, z, delta, rho, dlam )
           integer(ilp), intent(in) :: i
           real(dp), intent(out) :: dlam
           real(dp), intent(in) :: rho
           real(dp), intent(in) :: d(2_ilp), z(2_ilp)
           real(dp), intent(out) :: delta(2_ilp)
     end subroutine stdlib_dlaed5


end interface 


interface 
     pure module subroutine stdlib_slaed6( kniter, orgati, rho, d, z, finit, tau, info )
           logical(lk), intent(in) :: orgati
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kniter
           real(sp), intent(in) :: finit, rho
           real(sp), intent(out) :: tau
           real(sp), intent(in) :: d(3_ilp), z(3_ilp)
     end subroutine stdlib_slaed6

     pure module subroutine stdlib_dlaed6( kniter, orgati, rho, d, z, finit, tau, info )
           logical(lk), intent(in) :: orgati
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kniter
           real(dp), intent(in) :: finit, rho
           real(dp), intent(out) :: tau
           real(dp), intent(in) :: d(3_ilp), z(3_ilp)
     end subroutine stdlib_dlaed6


end interface 


interface 
     pure module subroutine stdlib_slaed7( icompq, n, qsiz, tlvls, curlvl, curpbm, d, q,ldq, indxq, rho, &
               cutpnt, qstore, qptr, prmptr,perm, givptr, givcol, givnum, work, iwork,info )
           integer(ilp), intent(in) :: curlvl, curpbm, cutpnt, icompq, ldq, n, qsiz, tlvls
           integer(ilp), intent(out) :: info
           real(sp), intent(inout) :: rho
           integer(ilp), intent(inout) :: givcol(2_ilp,*), givptr(*), perm(*), prmptr(*), qptr(*)
                     
           integer(ilp), intent(out) :: indxq(*), iwork(*)
           real(sp), intent(inout) :: d(*), givnum(2_ilp,*), q(ldq,*), qstore(*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_slaed7

     pure module subroutine stdlib_dlaed7( icompq, n, qsiz, tlvls, curlvl, curpbm, d, q,ldq, indxq, rho, &
               cutpnt, qstore, qptr, prmptr,perm, givptr, givcol, givnum, work, iwork,info )
           integer(ilp), intent(in) :: curlvl, curpbm, cutpnt, icompq, ldq, n, qsiz, tlvls
           integer(ilp), intent(out) :: info
           real(dp), intent(inout) :: rho
           integer(ilp), intent(inout) :: givcol(2_ilp,*), givptr(*), perm(*), prmptr(*), qptr(*)
                     
           integer(ilp), intent(out) :: indxq(*), iwork(*)
           real(dp), intent(inout) :: d(*), givnum(2_ilp,*), q(ldq,*), qstore(*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_dlaed7


     pure module subroutine stdlib_claed7( n, cutpnt, qsiz, tlvls, curlvl, curpbm, d, q,ldq, rho, indxq, &
               qstore, qptr, prmptr, perm,givptr, givcol, givnum, work, rwork, iwork,info )
           integer(ilp), intent(in) :: curlvl, curpbm, cutpnt, ldq, n, qsiz, tlvls
           integer(ilp), intent(out) :: info
           real(sp), intent(inout) :: rho
           integer(ilp), intent(inout) :: givcol(2_ilp,*), givptr(*), perm(*), prmptr(*), qptr(*)
                     
           integer(ilp), intent(out) :: indxq(*), iwork(*)
           real(sp), intent(inout) :: d(*), givnum(2_ilp,*), qstore(*)
           real(sp), intent(out) :: rwork(*)
           complex(sp), intent(inout) :: q(ldq,*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_claed7

     pure module subroutine stdlib_zlaed7( n, cutpnt, qsiz, tlvls, curlvl, curpbm, d, q,ldq, rho, indxq, &
               qstore, qptr, prmptr, perm,givptr, givcol, givnum, work, rwork, iwork,info )
           integer(ilp), intent(in) :: curlvl, curpbm, cutpnt, ldq, n, qsiz, tlvls
           integer(ilp), intent(out) :: info
           real(dp), intent(inout) :: rho
           integer(ilp), intent(inout) :: givcol(2_ilp,*), givptr(*), perm(*), prmptr(*), qptr(*)
                     
           integer(ilp), intent(out) :: indxq(*), iwork(*)
           real(dp), intent(inout) :: d(*), givnum(2_ilp,*), qstore(*)
           real(dp), intent(out) :: rwork(*)
           complex(dp), intent(inout) :: q(ldq,*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_zlaed7


end interface 


interface 
     pure module subroutine stdlib_slaed8( icompq, k, n, qsiz, d, q, ldq, indxq, rho,cutpnt, z, dlamda, &
               q2, ldq2, w, perm, givptr,givcol, givnum, indxp, indx, info )
           integer(ilp), intent(in) :: cutpnt, icompq, ldq, ldq2, n, qsiz
           integer(ilp), intent(out) :: givptr, info, k
           real(sp), intent(inout) :: rho
           integer(ilp), intent(out) :: givcol(2_ilp,*), indx(*), indxp(*), perm(*)
           integer(ilp), intent(inout) :: indxq(*)
           real(sp), intent(inout) :: d(*), q(ldq,*), z(*)
           real(sp), intent(out) :: dlamda(*), givnum(2_ilp,*), q2(ldq2,*), w(*)
     end subroutine stdlib_slaed8

     pure module subroutine stdlib_dlaed8( icompq, k, n, qsiz, d, q, ldq, indxq, rho,cutpnt, z, dlamda, &
               q2, ldq2, w, perm, givptr,givcol, givnum, indxp, indx, info )
           integer(ilp), intent(in) :: cutpnt, icompq, ldq, ldq2, n, qsiz
           integer(ilp), intent(out) :: givptr, info, k
           real(dp), intent(inout) :: rho
           integer(ilp), intent(out) :: givcol(2_ilp,*), indx(*), indxp(*), perm(*)
           integer(ilp), intent(inout) :: indxq(*)
           real(dp), intent(inout) :: d(*), q(ldq,*), z(*)
           real(dp), intent(out) :: dlamda(*), givnum(2_ilp,*), q2(ldq2,*), w(*)
     end subroutine stdlib_dlaed8


     pure module subroutine stdlib_claed8( k, n, qsiz, q, ldq, d, rho, cutpnt, z, dlamda,q2, ldq2, w, &
               indxp, indx, indxq, perm, givptr,givcol, givnum, info )
           integer(ilp), intent(in) :: cutpnt, ldq, ldq2, n, qsiz
           integer(ilp), intent(out) :: givptr, info, k
           real(sp), intent(inout) :: rho
           integer(ilp), intent(out) :: givcol(2_ilp,*), indx(*), indxp(*), perm(*)
           integer(ilp), intent(inout) :: indxq(*)
           real(sp), intent(inout) :: d(*), z(*)
           real(sp), intent(out) :: dlamda(*), givnum(2_ilp,*), w(*)
           complex(sp), intent(inout) :: q(ldq,*)
           complex(sp), intent(out) :: q2(ldq2,*)
     end subroutine stdlib_claed8

     pure module subroutine stdlib_zlaed8( k, n, qsiz, q, ldq, d, rho, cutpnt, z, dlamda,q2, ldq2, w, &
               indxp, indx, indxq, perm, givptr,givcol, givnum, info )
           integer(ilp), intent(in) :: cutpnt, ldq, ldq2, n, qsiz
           integer(ilp), intent(out) :: givptr, info, k
           real(dp), intent(inout) :: rho
           integer(ilp), intent(out) :: givcol(2_ilp,*), indx(*), indxp(*), perm(*)
           integer(ilp), intent(inout) :: indxq(*)
           real(dp), intent(inout) :: d(*), z(*)
           real(dp), intent(out) :: dlamda(*), givnum(2_ilp,*), w(*)
           complex(dp), intent(inout) :: q(ldq,*)
           complex(dp), intent(out) :: q2(ldq2,*)
     end subroutine stdlib_zlaed8


end interface 


interface 
     pure module subroutine stdlib_slaed9( k, kstart, kstop, n, d, q, ldq, rho, dlamda, w,s, lds, info )
               
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, kstart, kstop, ldq, lds, n
           real(sp), intent(in) :: rho
           real(sp), intent(out) :: d(*), q(ldq,*), s(lds,*)
           real(sp), intent(inout) :: dlamda(*), w(*)
     end subroutine stdlib_slaed9

     pure module subroutine stdlib_dlaed9( k, kstart, kstop, n, d, q, ldq, rho, dlamda, w,s, lds, info )
               
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, kstart, kstop, ldq, lds, n
           real(dp), intent(in) :: rho
           real(dp), intent(out) :: d(*), q(ldq,*), s(lds,*)
           real(dp), intent(inout) :: dlamda(*), w(*)
     end subroutine stdlib_dlaed9


end interface 


interface 
     pure module subroutine stdlib_slamrg( n1, n2, a, strd1, strd2, index )
           integer(ilp), intent(in) :: n1, n2, strd1, strd2
           integer(ilp), intent(out) :: index(*)
           real(sp), intent(in) :: a(*)
     end subroutine stdlib_slamrg

     pure module subroutine stdlib_dlamrg( n1, n2, a, dtrd1, dtrd2, index )
           integer(ilp), intent(in) :: dtrd1, dtrd2, n1, n2
           integer(ilp), intent(out) :: index(*)
           real(dp), intent(in) :: a(*)
     end subroutine stdlib_dlamrg


end interface 


interface 
     pure module subroutine stdlib_slaeda( n, tlvls, curlvl, curpbm, prmptr, perm, givptr,givcol, givnum,&
                q, qptr, z, ztemp, info )
           integer(ilp), intent(in) :: curlvl, curpbm, n, tlvls
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: givcol(2_ilp,*), givptr(*), perm(*), prmptr(*), qptr(*)
           real(sp), intent(in) :: givnum(2_ilp,*), q(*)
           real(sp), intent(out) :: z(*), ztemp(*)
     end subroutine stdlib_slaeda

     pure module subroutine stdlib_dlaeda( n, tlvls, curlvl, curpbm, prmptr, perm, givptr,givcol, givnum,&
                q, qptr, z, ztemp, info )
           integer(ilp), intent(in) :: curlvl, curpbm, n, tlvls
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: givcol(2_ilp,*), givptr(*), perm(*), prmptr(*), qptr(*)
           real(dp), intent(in) :: givnum(2_ilp,*), q(*)
           real(dp), intent(out) :: z(*), ztemp(*)
     end subroutine stdlib_dlaeda


end interface 


interface 
     pure module subroutine stdlib_slarra( n, d, e, e2, spltol, tnrm,nsplit, isplit, info )
           integer(ilp), intent(out) :: info, nsplit
           integer(ilp), intent(in) :: n
           real(sp), intent(in) :: spltol, tnrm
           integer(ilp), intent(out) :: isplit(*)
           real(sp), intent(in) :: d(*)
           real(sp), intent(inout) :: e(*), e2(*)
     end subroutine stdlib_slarra

     pure module subroutine stdlib_dlarra( n, d, e, e2, spltol, tnrm,nsplit, isplit, info )
           integer(ilp), intent(out) :: info, nsplit
           integer(ilp), intent(in) :: n
           real(dp), intent(in) :: spltol, tnrm
           integer(ilp), intent(out) :: isplit(*)
           real(dp), intent(in) :: d(*)
           real(dp), intent(inout) :: e(*), e2(*)
     end subroutine stdlib_dlarra


end interface 


interface 
     pure module subroutine stdlib_slarrb( n, d, lld, ifirst, ilast, rtol1,rtol2, offset, w, wgap, werr, &
               work, iwork,pivmin, spdiam, twist, info )
           integer(ilp), intent(in) :: ifirst, ilast, n, offset, twist
           integer(ilp), intent(out) :: info
           real(sp), intent(in) :: pivmin, rtol1, rtol2, spdiam
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(in) :: d(*), lld(*)
           real(sp), intent(inout) :: w(*), werr(*), wgap(*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_slarrb

     pure module subroutine stdlib_dlarrb( n, d, lld, ifirst, ilast, rtol1,rtol2, offset, w, wgap, werr, &
               work, iwork,pivmin, spdiam, twist, info )
           integer(ilp), intent(in) :: ifirst, ilast, n, offset, twist
           integer(ilp), intent(out) :: info
           real(dp), intent(in) :: pivmin, rtol1, rtol2, spdiam
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(in) :: d(*), lld(*)
           real(dp), intent(inout) :: w(*), werr(*), wgap(*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_dlarrb


end interface 


interface 
     pure module subroutine stdlib_slarrc( jobt, n, vl, vu, d, e, pivmin,eigcnt, lcnt, rcnt, info )
               
           character, intent(in) :: jobt
           integer(ilp), intent(out) :: eigcnt, info, lcnt, rcnt
           integer(ilp), intent(in) :: n
           real(sp), intent(in) :: pivmin, vl, vu
           real(sp), intent(in) :: d(*), e(*)
     end subroutine stdlib_slarrc

     pure module subroutine stdlib_dlarrc( jobt, n, vl, vu, d, e, pivmin,eigcnt, lcnt, rcnt, info )
               
           character, intent(in) :: jobt
           integer(ilp), intent(out) :: eigcnt, info, lcnt, rcnt
           integer(ilp), intent(in) :: n
           real(dp), intent(in) :: pivmin, vl, vu
           real(dp), intent(in) :: d(*), e(*)
     end subroutine stdlib_dlarrc


end interface 


interface 
     pure module subroutine stdlib_slarrd( range, order, n, vl, vu, il, iu, gers,reltol, d, e, e2, &
               pivmin, nsplit, isplit,m, w, werr, wl, wu, iblock, indexw,work, iwork, info )
           character, intent(in) :: order, range
           integer(ilp), intent(in) :: il, iu, n, nsplit
           integer(ilp), intent(out) :: info, m
           real(sp), intent(in) :: pivmin, reltol, vl, vu
           real(sp), intent(out) :: wl, wu
           integer(ilp), intent(out) :: iblock(*), indexw(*), iwork(*)
           integer(ilp), intent(in) :: isplit(*)
           real(sp), intent(in) :: d(*), e(*), e2(*), gers(*)
           real(sp), intent(out) :: w(*), werr(*), work(*)
     end subroutine stdlib_slarrd

     pure module subroutine stdlib_dlarrd( range, order, n, vl, vu, il, iu, gers,reltol, d, e, e2, &
               pivmin, nsplit, isplit,m, w, werr, wl, wu, iblock, indexw,work, iwork, info )
           character, intent(in) :: order, range
           integer(ilp), intent(in) :: il, iu, n, nsplit
           integer(ilp), intent(out) :: info, m
           real(dp), intent(in) :: pivmin, reltol, vl, vu
           real(dp), intent(out) :: wl, wu
           integer(ilp), intent(out) :: iblock(*), indexw(*), iwork(*)
           integer(ilp), intent(in) :: isplit(*)
           real(dp), intent(in) :: d(*), e(*), e2(*), gers(*)
           real(dp), intent(out) :: w(*), werr(*), work(*)
     end subroutine stdlib_dlarrd


end interface 


interface 
     pure module subroutine stdlib_slarre( range, n, vl, vu, il, iu, d, e, e2,rtol1, rtol2, spltol, &
               nsplit, isplit, m,w, werr, wgap, iblock, indexw, gers, pivmin,work, iwork, info )
           character, intent(in) :: range
           integer(ilp), intent(in) :: il, iu, n
           integer(ilp), intent(out) :: info, m, nsplit
           real(sp), intent(out) :: pivmin
           real(sp), intent(in) :: rtol1, rtol2, spltol
           real(sp), intent(inout) :: vl, vu
           integer(ilp), intent(out) :: iblock(*), isplit(*), iwork(*), indexw(*)
           real(sp), intent(inout) :: d(*), e(*), e2(*)
           real(sp), intent(out) :: gers(*), w(*), werr(*), wgap(*), work(*)
     end subroutine stdlib_slarre

     pure module subroutine stdlib_dlarre( range, n, vl, vu, il, iu, d, e, e2,rtol1, rtol2, spltol, &
               nsplit, isplit, m,w, werr, wgap, iblock, indexw, gers, pivmin,work, iwork, info )
           character, intent(in) :: range
           integer(ilp), intent(in) :: il, iu, n
           integer(ilp), intent(out) :: info, m, nsplit
           real(dp), intent(out) :: pivmin
           real(dp), intent(in) :: rtol1, rtol2, spltol
           real(dp), intent(inout) :: vl, vu
           integer(ilp), intent(out) :: iblock(*), isplit(*), iwork(*), indexw(*)
           real(dp), intent(inout) :: d(*), e(*), e2(*)
           real(dp), intent(out) :: gers(*), w(*), werr(*), wgap(*), work(*)
     end subroutine stdlib_dlarre


end interface 


interface 
     pure module subroutine stdlib_slarrf( n, d, l, ld, clstrt, clend,w, wgap, werr,spdiam, clgapl, &
               clgapr, pivmin, sigma,dplus, lplus, work, info )
           integer(ilp), intent(in) :: clstrt, clend, n
           integer(ilp), intent(out) :: info
           real(sp), intent(in) :: clgapl, clgapr, pivmin, spdiam
           real(sp), intent(out) :: sigma
           real(sp), intent(in) :: d(*), l(*), ld(*), w(*), werr(*)
           real(sp), intent(out) :: dplus(*), lplus(*), work(*)
           real(sp), intent(inout) :: wgap(*)
     end subroutine stdlib_slarrf

     pure module subroutine stdlib_dlarrf( n, d, l, ld, clstrt, clend,w, wgap, werr,spdiam, clgapl, &
               clgapr, pivmin, sigma,dplus, lplus, work, info )
           integer(ilp), intent(in) :: clstrt, clend, n
           integer(ilp), intent(out) :: info
           real(dp), intent(in) :: clgapl, clgapr, pivmin, spdiam
           real(dp), intent(out) :: sigma
           real(dp), intent(in) :: d(*), l(*), ld(*), w(*), werr(*)
           real(dp), intent(out) :: dplus(*), lplus(*), work(*)
           real(dp), intent(inout) :: wgap(*)
     end subroutine stdlib_dlarrf


end interface 


interface 
     pure module subroutine stdlib_slarrj( n, d, e2, ifirst, ilast,rtol, offset, w, werr, work, iwork,&
               pivmin, spdiam, info )
           integer(ilp), intent(in) :: ifirst, ilast, n, offset
           integer(ilp), intent(out) :: info
           real(sp), intent(in) :: pivmin, rtol, spdiam
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(in) :: d(*), e2(*)
           real(sp), intent(inout) :: w(*), werr(*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_slarrj

     pure module subroutine stdlib_dlarrj( n, d, e2, ifirst, ilast,rtol, offset, w, werr, work, iwork,&
               pivmin, spdiam, info )
           integer(ilp), intent(in) :: ifirst, ilast, n, offset
           integer(ilp), intent(out) :: info
           real(dp), intent(in) :: pivmin, rtol, spdiam
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(in) :: d(*), e2(*)
           real(dp), intent(inout) :: w(*), werr(*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_dlarrj


end interface 


interface 
     pure module subroutine stdlib_slarrk( n, iw, gl, gu,d, e2, pivmin, reltol, w, werr, info)
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: iw, n
           real(sp), intent(in) :: pivmin, reltol, gl, gu
           real(sp), intent(out) :: w, werr
           real(sp), intent(in) :: d(*), e2(*)
     end subroutine stdlib_slarrk

     pure module subroutine stdlib_dlarrk( n, iw, gl, gu,d, e2, pivmin, reltol, w, werr, info)
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: iw, n
           real(dp), intent(in) :: pivmin, reltol, gl, gu
           real(dp), intent(out) :: w, werr
           real(dp), intent(in) :: d(*), e2(*)
     end subroutine stdlib_dlarrk


end interface 


interface 
     pure module subroutine stdlib_slarrr( n, d, e, info )
           integer(ilp), intent(in) :: n
           integer(ilp), intent(out) :: info
           real(sp), intent(in) :: d(*)
           real(sp), intent(inout) :: e(*)
     end subroutine stdlib_slarrr

     pure module subroutine stdlib_dlarrr( n, d, e, info )
           integer(ilp), intent(in) :: n
           integer(ilp), intent(out) :: info
           real(dp), intent(in) :: d(*)
           real(dp), intent(inout) :: e(*)
     end subroutine stdlib_dlarrr


end interface 


interface 
     pure module subroutine stdlib_slarrv( n, vl, vu, d, l, pivmin,isplit, m, dol, dou, minrgp,rtol1, &
               rtol2, w, werr, wgap,iblock, indexw, gers, z, ldz, isuppz,work, iwork, info )
           integer(ilp), intent(in) :: dol, dou, ldz, m, n
           integer(ilp), intent(out) :: info
           real(sp), intent(in) :: minrgp, pivmin, vl, vu
           real(sp), intent(inout) :: rtol1, rtol2
           integer(ilp), intent(in) :: iblock(*), indexw(*), isplit(*)
           integer(ilp), intent(out) :: isuppz(*), iwork(*)
           real(sp), intent(inout) :: d(*), l(*), w(*), werr(*), wgap(*)
           real(sp), intent(in) :: gers(*)
           real(sp), intent(out) :: work(*)
           real(sp), intent(out) :: z(ldz,*)
     end subroutine stdlib_slarrv

     pure module subroutine stdlib_dlarrv( n, vl, vu, d, l, pivmin,isplit, m, dol, dou, minrgp,rtol1, &
               rtol2, w, werr, wgap,iblock, indexw, gers, z, ldz, isuppz,work, iwork, info )
           integer(ilp), intent(in) :: dol, dou, ldz, m, n
           integer(ilp), intent(out) :: info
           real(dp), intent(in) :: minrgp, pivmin, vl, vu
           real(dp), intent(inout) :: rtol1, rtol2
           integer(ilp), intent(in) :: iblock(*), indexw(*), isplit(*)
           integer(ilp), intent(out) :: isuppz(*), iwork(*)
           real(dp), intent(inout) :: d(*), l(*), w(*), werr(*), wgap(*)
           real(dp), intent(in) :: gers(*)
           real(dp), intent(out) :: work(*)
           real(dp), intent(out) :: z(ldz,*)
     end subroutine stdlib_dlarrv


     pure module subroutine stdlib_clarrv( n, vl, vu, d, l, pivmin,isplit, m, dol, dou, minrgp,rtol1, &
               rtol2, w, werr, wgap,iblock, indexw, gers, z, ldz, isuppz,work, iwork, info )
           integer(ilp), intent(in) :: dol, dou, ldz, m, n
           integer(ilp), intent(out) :: info
           real(sp), intent(in) :: minrgp, pivmin, vl, vu
           real(sp), intent(inout) :: rtol1, rtol2
           integer(ilp), intent(in) :: iblock(*), indexw(*), isplit(*)
           integer(ilp), intent(out) :: isuppz(*), iwork(*)
           real(sp), intent(inout) :: d(*), l(*), w(*), werr(*), wgap(*)
           real(sp), intent(in) :: gers(*)
           real(sp), intent(out) :: work(*)
           complex(sp), intent(out) :: z(ldz,*)
     end subroutine stdlib_clarrv

     pure module subroutine stdlib_zlarrv( n, vl, vu, d, l, pivmin,isplit, m, dol, dou, minrgp,rtol1, &
               rtol2, w, werr, wgap,iblock, indexw, gers, z, ldz, isuppz,work, iwork, info )
           integer(ilp), intent(in) :: dol, dou, ldz, m, n
           integer(ilp), intent(out) :: info
           real(dp), intent(in) :: minrgp, pivmin, vl, vu
           real(dp), intent(inout) :: rtol1, rtol2
           integer(ilp), intent(in) :: iblock(*), indexw(*), isplit(*)
           integer(ilp), intent(out) :: isuppz(*), iwork(*)
           real(dp), intent(inout) :: d(*), l(*), w(*), werr(*), wgap(*)
           real(dp), intent(in) :: gers(*)
           real(dp), intent(out) :: work(*)
           complex(dp), intent(out) :: z(ldz,*)
     end subroutine stdlib_zlarrv


end interface 


interface 
     pure module subroutine stdlib_slar1v( n, b1, bn, lambda, d, l, ld, lld,pivmin, gaptol, z, wantnc, &
               negcnt, ztz, mingma,r, isuppz, nrminv, resid, rqcorr, work )
           logical(lk), intent(in) :: wantnc
           integer(ilp), intent(in) :: b1, bn, n
           integer(ilp), intent(out) :: negcnt
           integer(ilp), intent(inout) :: r
           real(sp), intent(in) :: gaptol, lambda, pivmin
           real(sp), intent(out) :: mingma, nrminv, resid, rqcorr, ztz
           integer(ilp), intent(out) :: isuppz(*)
           real(sp), intent(in) :: d(*), l(*), ld(*), lld(*)
           real(sp), intent(out) :: work(*)
           real(sp), intent(inout) :: z(*)
     end subroutine stdlib_slar1v

     pure module subroutine stdlib_dlar1v( n, b1, bn, lambda, d, l, ld, lld,pivmin, gaptol, z, wantnc, &
               negcnt, ztz, mingma,r, isuppz, nrminv, resid, rqcorr, work )
           logical(lk), intent(in) :: wantnc
           integer(ilp), intent(in) :: b1, bn, n
           integer(ilp), intent(out) :: negcnt
           integer(ilp), intent(inout) :: r
           real(dp), intent(in) :: gaptol, lambda, pivmin
           real(dp), intent(out) :: mingma, nrminv, resid, rqcorr, ztz
           integer(ilp), intent(out) :: isuppz(*)
           real(dp), intent(in) :: d(*), l(*), ld(*), lld(*)
           real(dp), intent(out) :: work(*)
           real(dp), intent(inout) :: z(*)
     end subroutine stdlib_dlar1v


     pure module subroutine stdlib_clar1v( n, b1, bn, lambda, d, l, ld, lld,pivmin, gaptol, z, wantnc, &
               negcnt, ztz, mingma,r, isuppz, nrminv, resid, rqcorr, work )
           logical(lk), intent(in) :: wantnc
           integer(ilp), intent(in) :: b1, bn, n
           integer(ilp), intent(out) :: negcnt
           integer(ilp), intent(inout) :: r
           real(sp), intent(in) :: gaptol, lambda, pivmin
           real(sp), intent(out) :: mingma, nrminv, resid, rqcorr, ztz
           integer(ilp), intent(out) :: isuppz(*)
           real(sp), intent(in) :: d(*), l(*), ld(*), lld(*)
           real(sp), intent(out) :: work(*)
           complex(sp), intent(inout) :: z(*)
     end subroutine stdlib_clar1v

     pure module subroutine stdlib_zlar1v( n, b1, bn, lambda, d, l, ld, lld,pivmin, gaptol, z, wantnc, &
               negcnt, ztz, mingma,r, isuppz, nrminv, resid, rqcorr, work )
           logical(lk), intent(in) :: wantnc
           integer(ilp), intent(in) :: b1, bn, n
           integer(ilp), intent(out) :: negcnt
           integer(ilp), intent(inout) :: r
           real(dp), intent(in) :: gaptol, lambda, pivmin
           real(dp), intent(out) :: mingma, nrminv, resid, rqcorr, ztz
           integer(ilp), intent(out) :: isuppz(*)
           real(dp), intent(in) :: d(*), l(*), ld(*), lld(*)
           real(dp), intent(out) :: work(*)
           complex(dp), intent(inout) :: z(*)
     end subroutine stdlib_zlar1v


end interface 


interface 
     pure module subroutine stdlib_sstev( jobz, n, d, e, z, ldz, work, info )
           character, intent(in) :: jobz
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldz, n
           real(sp), intent(inout) :: d(*), e(*)
           real(sp), intent(out) :: work(*), z(ldz,*)
     end subroutine stdlib_sstev

     pure module subroutine stdlib_dstev( jobz, n, d, e, z, ldz, work, info )
           character, intent(in) :: jobz
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldz, n
           real(dp), intent(inout) :: d(*), e(*)
           real(dp), intent(out) :: work(*), z(ldz,*)
     end subroutine stdlib_dstev


end interface 


interface 
     pure module subroutine stdlib_sstevd( jobz, n, d, e, z, ldz, work, lwork, iwork,liwork, info )
               
           character, intent(in) :: jobz
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldz, liwork, lwork, n
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(inout) :: d(*), e(*)
           real(sp), intent(out) :: work(*), z(ldz,*)
     end subroutine stdlib_sstevd

     pure module subroutine stdlib_dstevd( jobz, n, d, e, z, ldz, work, lwork, iwork,liwork, info )
               
           character, intent(in) :: jobz
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldz, liwork, lwork, n
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(inout) :: d(*), e(*)
           real(dp), intent(out) :: work(*), z(ldz,*)
     end subroutine stdlib_dstevd


end interface 


interface 
     pure module subroutine stdlib_sstevr( jobz, range, n, d, e, vl, vu, il, iu, abstol,m, w, z, ldz, &
               isuppz, work, lwork, iwork,liwork, info )
           character, intent(in) :: jobz, range
           integer(ilp), intent(in) :: il, iu, ldz, liwork, lwork, n
           integer(ilp), intent(out) :: info, m
           real(sp), intent(in) :: abstol, vl, vu
           integer(ilp), intent(out) :: isuppz(*), iwork(*)
           real(sp), intent(inout) :: d(*), e(*)
           real(sp), intent(out) :: w(*), work(*), z(ldz,*)
     end subroutine stdlib_sstevr

     pure module subroutine stdlib_dstevr( jobz, range, n, d, e, vl, vu, il, iu, abstol,m, w, z, ldz, &
               isuppz, work, lwork, iwork,liwork, info )
           character, intent(in) :: jobz, range
           integer(ilp), intent(in) :: il, iu, ldz, liwork, lwork, n
           integer(ilp), intent(out) :: info, m
           real(dp), intent(in) :: abstol, vl, vu
           integer(ilp), intent(out) :: isuppz(*), iwork(*)
           real(dp), intent(inout) :: d(*), e(*)
           real(dp), intent(out) :: w(*), work(*), z(ldz,*)
     end subroutine stdlib_dstevr


end interface 


interface 
     pure module subroutine stdlib_sstevx( jobz, range, n, d, e, vl, vu, il, iu, abstol,m, w, z, ldz, &
               work, iwork, ifail, info )
           character, intent(in) :: jobz, range
           integer(ilp), intent(in) :: il, iu, ldz, n
           integer(ilp), intent(out) :: info, m
           real(sp), intent(in) :: abstol, vl, vu
           integer(ilp), intent(out) :: ifail(*), iwork(*)
           real(sp), intent(inout) :: d(*), e(*)
           real(sp), intent(out) :: w(*), work(*), z(ldz,*)
     end subroutine stdlib_sstevx

     pure module subroutine stdlib_dstevx( jobz, range, n, d, e, vl, vu, il, iu, abstol,m, w, z, ldz, &
               work, iwork, ifail, info )
           character, intent(in) :: jobz, range
           integer(ilp), intent(in) :: il, iu, ldz, n
           integer(ilp), intent(out) :: info, m
           real(dp), intent(in) :: abstol, vl, vu
           integer(ilp), intent(out) :: ifail(*), iwork(*)
           real(dp), intent(inout) :: d(*), e(*)
           real(dp), intent(out) :: w(*), work(*), z(ldz,*)
     end subroutine stdlib_dstevx


end interface 


interface 
     pure module subroutine stdlib_spteqr( compz, n, d, e, z, ldz, work, info )
           character, intent(in) :: compz
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldz, n
           real(sp), intent(inout) :: d(*), e(*), z(ldz,*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_spteqr

     pure module subroutine stdlib_dpteqr( compz, n, d, e, z, ldz, work, info )
           character, intent(in) :: compz
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldz, n
           real(dp), intent(inout) :: d(*), e(*), z(ldz,*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_dpteqr


     pure module subroutine stdlib_cpteqr( compz, n, d, e, z, ldz, work, info )
           character, intent(in) :: compz
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldz, n
           real(sp), intent(inout) :: d(*), e(*)
           real(sp), intent(out) :: work(*)
           complex(sp), intent(inout) :: z(ldz,*)
     end subroutine stdlib_cpteqr

     pure module subroutine stdlib_zpteqr( compz, n, d, e, z, ldz, work, info )
           character, intent(in) :: compz
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldz, n
           real(dp), intent(inout) :: d(*), e(*)
           real(dp), intent(out) :: work(*)
           complex(dp), intent(inout) :: z(ldz,*)
     end subroutine stdlib_zpteqr


end interface 


interface 
     pure module subroutine stdlib_sstebz( range, order, n, vl, vu, il, iu, abstol, d, e,m, nsplit, w, &
               iblock, isplit, work, iwork,info )
           character, intent(in) :: order, range
           integer(ilp), intent(in) :: il, iu, n
           integer(ilp), intent(out) :: info, m, nsplit
           real(sp), intent(in) :: abstol, vl, vu
           integer(ilp), intent(out) :: iblock(*), isplit(*), iwork(*)
           real(sp), intent(in) :: d(*), e(*)
           real(sp), intent(out) :: w(*), work(*)
     end subroutine stdlib_sstebz

     pure module subroutine stdlib_dstebz( range, order, n, vl, vu, il, iu, abstol, d, e,m, nsplit, w, &
               iblock, isplit, work, iwork,info )
           character, intent(in) :: order, range
           integer(ilp), intent(in) :: il, iu, n
           integer(ilp), intent(out) :: info, m, nsplit
           real(dp), intent(in) :: abstol, vl, vu
           integer(ilp), intent(out) :: iblock(*), isplit(*), iwork(*)
           real(dp), intent(in) :: d(*), e(*)
           real(dp), intent(out) :: w(*), work(*)
     end subroutine stdlib_dstebz


end interface 


interface 
     pure module subroutine stdlib_ssterf( n, d, e, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           real(sp), intent(inout) :: d(*), e(*)
     end subroutine stdlib_ssterf

     pure module subroutine stdlib_dsterf( n, d, e, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           real(dp), intent(inout) :: d(*), e(*)
     end subroutine stdlib_dsterf


end interface 


interface 
     pure module subroutine stdlib_sstedc( compz, n, d, e, z, ldz, work, lwork, iwork,liwork, info )
               
           character, intent(in) :: compz
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldz, liwork, lwork, n
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(inout) :: d(*), e(*), z(ldz,*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_sstedc

     pure module subroutine stdlib_dstedc( compz, n, d, e, z, ldz, work, lwork, iwork,liwork, info )
               
           character, intent(in) :: compz
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldz, liwork, lwork, n
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(inout) :: d(*), e(*), z(ldz,*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_dstedc


     pure module subroutine stdlib_cstedc( compz, n, d, e, z, ldz, work, lwork, rwork,lrwork, iwork, &
               liwork, info )
           character, intent(in) :: compz
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldz, liwork, lrwork, lwork, n
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(inout) :: d(*), e(*)
           real(sp), intent(out) :: rwork(*)
           complex(sp), intent(out) :: work(*)
           complex(sp), intent(inout) :: z(ldz,*)
     end subroutine stdlib_cstedc

     pure module subroutine stdlib_zstedc( compz, n, d, e, z, ldz, work, lwork, rwork,lrwork, iwork, &
               liwork, info )
           character, intent(in) :: compz
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldz, liwork, lrwork, lwork, n
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(inout) :: d(*), e(*)
           real(dp), intent(out) :: rwork(*)
           complex(dp), intent(out) :: work(*)
           complex(dp), intent(inout) :: z(ldz,*)
     end subroutine stdlib_zstedc


end interface 


interface 
     pure module subroutine stdlib_sstegr( jobz, range, n, d, e, vl, vu, il, iu,abstol, m, w, z, ldz, &
               isuppz, work, lwork, iwork,liwork, info )
           character, intent(in) :: jobz, range
           integer(ilp), intent(in) :: il, iu, ldz, liwork, lwork, n
           integer(ilp), intent(out) :: info, m
           real(sp), intent(in) :: abstol, vl, vu
           integer(ilp), intent(out) :: isuppz(*), iwork(*)
           real(sp), intent(inout) :: d(*), e(*)
           real(sp), intent(out) :: w(*), work(*)
           real(sp), intent(out) :: z(ldz,*)
     end subroutine stdlib_sstegr

     pure module subroutine stdlib_dstegr( jobz, range, n, d, e, vl, vu, il, iu,abstol, m, w, z, ldz, &
               isuppz, work, lwork, iwork,liwork, info )
           character, intent(in) :: jobz, range
           integer(ilp), intent(in) :: il, iu, ldz, liwork, lwork, n
           integer(ilp), intent(out) :: info, m
           real(dp), intent(in) :: abstol, vl, vu
           integer(ilp), intent(out) :: isuppz(*), iwork(*)
           real(dp), intent(inout) :: d(*), e(*)
           real(dp), intent(out) :: w(*), work(*)
           real(dp), intent(out) :: z(ldz,*)
     end subroutine stdlib_dstegr


     pure module subroutine stdlib_cstegr( jobz, range, n, d, e, vl, vu, il, iu,abstol, m, w, z, ldz, &
               isuppz, work, lwork, iwork,liwork, info )
           character, intent(in) :: jobz, range
           integer(ilp), intent(in) :: il, iu, ldz, liwork, lwork, n
           integer(ilp), intent(out) :: info, m
           real(sp), intent(in) :: abstol, vl, vu
           integer(ilp), intent(out) :: isuppz(*), iwork(*)
           real(sp), intent(inout) :: d(*), e(*)
           real(sp), intent(out) :: w(*), work(*)
           complex(sp), intent(out) :: z(ldz,*)
     end subroutine stdlib_cstegr

     pure module subroutine stdlib_zstegr( jobz, range, n, d, e, vl, vu, il, iu,abstol, m, w, z, ldz, &
               isuppz, work, lwork, iwork,liwork, info )
           character, intent(in) :: jobz, range
           integer(ilp), intent(in) :: il, iu, ldz, liwork, lwork, n
           integer(ilp), intent(out) :: info, m
           real(dp), intent(in) :: abstol, vl, vu
           integer(ilp), intent(out) :: isuppz(*), iwork(*)
           real(dp), intent(inout) :: d(*), e(*)
           real(dp), intent(out) :: w(*), work(*)
           complex(dp), intent(out) :: z(ldz,*)
     end subroutine stdlib_zstegr


end interface 


interface 
     pure module subroutine stdlib_sstein( n, d, e, m, w, iblock, isplit, z, ldz, work,iwork, ifail, &
               info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldz, m, n
           integer(ilp), intent(in) :: iblock(*), isplit(*)
           integer(ilp), intent(out) :: ifail(*), iwork(*)
           real(sp), intent(in) :: d(*), e(*), w(*)
           real(sp), intent(out) :: work(*), z(ldz,*)
     end subroutine stdlib_sstein

     pure module subroutine stdlib_dstein( n, d, e, m, w, iblock, isplit, z, ldz, work,iwork, ifail, &
               info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldz, m, n
           integer(ilp), intent(in) :: iblock(*), isplit(*)
           integer(ilp), intent(out) :: ifail(*), iwork(*)
           real(dp), intent(in) :: d(*), e(*), w(*)
           real(dp), intent(out) :: work(*), z(ldz,*)
     end subroutine stdlib_dstein


     pure module subroutine stdlib_cstein( n, d, e, m, w, iblock, isplit, z, ldz, work,iwork, ifail, &
               info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldz, m, n
           integer(ilp), intent(in) :: iblock(*), isplit(*)
           integer(ilp), intent(out) :: ifail(*), iwork(*)
           real(sp), intent(in) :: d(*), e(*), w(*)
           real(sp), intent(out) :: work(*)
           complex(sp), intent(out) :: z(ldz,*)
     end subroutine stdlib_cstein

     pure module subroutine stdlib_zstein( n, d, e, m, w, iblock, isplit, z, ldz, work,iwork, ifail, &
               info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldz, m, n
           integer(ilp), intent(in) :: iblock(*), isplit(*)
           integer(ilp), intent(out) :: ifail(*), iwork(*)
           real(dp), intent(in) :: d(*), e(*), w(*)
           real(dp), intent(out) :: work(*)
           complex(dp), intent(out) :: z(ldz,*)
     end subroutine stdlib_zstein


end interface 


interface 
     pure module subroutine stdlib_sstemr( jobz, range, n, d, e, vl, vu, il, iu,m, w, z, ldz, nzc, &
               isuppz, tryrac, work, lwork,iwork, liwork, info )
           character, intent(in) :: jobz, range
           logical(lk), intent(inout) :: tryrac
           integer(ilp), intent(in) :: il, iu, ldz, nzc, liwork, lwork, n
           integer(ilp), intent(out) :: info, m
           real(sp), intent(in) :: vl, vu
           integer(ilp), intent(out) :: isuppz(*), iwork(*)
           real(sp), intent(inout) :: d(*), e(*)
           real(sp), intent(out) :: w(*), work(*)
           real(sp), intent(out) :: z(ldz,*)
     end subroutine stdlib_sstemr

     pure module subroutine stdlib_dstemr( jobz, range, n, d, e, vl, vu, il, iu,m, w, z, ldz, nzc, &
               isuppz, tryrac, work, lwork,iwork, liwork, info )
           character, intent(in) :: jobz, range
           logical(lk), intent(inout) :: tryrac
           integer(ilp), intent(in) :: il, iu, ldz, nzc, liwork, lwork, n
           integer(ilp), intent(out) :: info, m
           real(dp), intent(in) :: vl, vu
           integer(ilp), intent(out) :: isuppz(*), iwork(*)
           real(dp), intent(inout) :: d(*), e(*)
           real(dp), intent(out) :: w(*), work(*)
           real(dp), intent(out) :: z(ldz,*)
     end subroutine stdlib_dstemr


     pure module subroutine stdlib_cstemr( jobz, range, n, d, e, vl, vu, il, iu,m, w, z, ldz, nzc, &
               isuppz, tryrac, work, lwork,iwork, liwork, info )
           character, intent(in) :: jobz, range
           logical(lk), intent(inout) :: tryrac
           integer(ilp), intent(in) :: il, iu, ldz, nzc, liwork, lwork, n
           integer(ilp), intent(out) :: info, m
           real(sp), intent(in) :: vl, vu
           integer(ilp), intent(out) :: isuppz(*), iwork(*)
           real(sp), intent(inout) :: d(*), e(*)
           real(sp), intent(out) :: w(*), work(*)
           complex(sp), intent(out) :: z(ldz,*)
     end subroutine stdlib_cstemr

     pure module subroutine stdlib_zstemr( jobz, range, n, d, e, vl, vu, il, iu,m, w, z, ldz, nzc, &
               isuppz, tryrac, work, lwork,iwork, liwork, info )
           character, intent(in) :: jobz, range
           logical(lk), intent(inout) :: tryrac
           integer(ilp), intent(in) :: il, iu, ldz, nzc, liwork, lwork, n
           integer(ilp), intent(out) :: info, m
           real(dp), intent(in) :: vl, vu
           integer(ilp), intent(out) :: isuppz(*), iwork(*)
           real(dp), intent(inout) :: d(*), e(*)
           real(dp), intent(out) :: w(*), work(*)
           complex(dp), intent(out) :: z(ldz,*)
     end subroutine stdlib_zstemr


end interface 


interface 
     pure module subroutine stdlib_ssteqr( compz, n, d, e, z, ldz, work, info )
           character, intent(in) :: compz
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldz, n
           real(sp), intent(inout) :: d(*), e(*), z(ldz,*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_ssteqr

     pure module subroutine stdlib_dsteqr( compz, n, d, e, z, ldz, work, info )
           character, intent(in) :: compz
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldz, n
           real(dp), intent(inout) :: d(*), e(*), z(ldz,*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_dsteqr


     pure module subroutine stdlib_csteqr( compz, n, d, e, z, ldz, work, info )
           character, intent(in) :: compz
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldz, n
           real(sp), intent(inout) :: d(*), e(*)
           real(sp), intent(out) :: work(*)
           complex(sp), intent(inout) :: z(ldz,*)
     end subroutine stdlib_csteqr

     pure module subroutine stdlib_zsteqr( compz, n, d, e, z, ldz, work, info )
           character, intent(in) :: compz
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldz, n
           real(dp), intent(inout) :: d(*), e(*)
           real(dp), intent(out) :: work(*)
           complex(dp), intent(inout) :: z(ldz,*)
     end subroutine stdlib_zsteqr


end interface 


interface 
     pure module subroutine stdlib_slasd0( n, sqre, d, e, u, ldu, vt, ldvt, smlsiz, iwork,work, info )
               
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldu, ldvt, n, smlsiz, sqre
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(inout) :: d(*), e(*)
           real(sp), intent(out) :: u(ldu,*), vt(ldvt,*), work(*)
     end subroutine stdlib_slasd0

     pure module subroutine stdlib_dlasd0( n, sqre, d, e, u, ldu, vt, ldvt, smlsiz, iwork,work, info )
               
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldu, ldvt, n, smlsiz, sqre
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(inout) :: d(*), e(*)
           real(dp), intent(out) :: u(ldu,*), vt(ldvt,*), work(*)
     end subroutine stdlib_dlasd0


end interface 


interface 
     pure module subroutine stdlib_slasdt( n, lvl, nd, inode, ndiml, ndimr, msub )
           integer(ilp), intent(out) :: lvl, nd
           integer(ilp), intent(in) :: msub, n
           integer(ilp), intent(out) :: inode(*), ndiml(*), ndimr(*)
     end subroutine stdlib_slasdt

     pure module subroutine stdlib_dlasdt( n, lvl, nd, inode, ndiml, ndimr, msub )
           integer(ilp), intent(out) :: lvl, nd
           integer(ilp), intent(in) :: msub, n
           integer(ilp), intent(out) :: inode(*), ndiml(*), ndimr(*)
     end subroutine stdlib_dlasdt


end interface 


interface 
     pure module subroutine stdlib_slasd1( nl, nr, sqre, d, alpha, beta, u, ldu, vt, ldvt,idxq, iwork, &
               work, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldu, ldvt, nl, nr, sqre
           real(sp), intent(inout) :: alpha, beta
           integer(ilp), intent(inout) :: idxq(*)
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(inout) :: d(*), u(ldu,*), vt(ldvt,*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_slasd1

     pure module subroutine stdlib_dlasd1( nl, nr, sqre, d, alpha, beta, u, ldu, vt, ldvt,idxq, iwork, &
               work, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldu, ldvt, nl, nr, sqre
           real(dp), intent(inout) :: alpha, beta
           integer(ilp), intent(inout) :: idxq(*)
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(inout) :: d(*), u(ldu,*), vt(ldvt,*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_dlasd1


end interface 


interface 
     pure module subroutine stdlib_slasd2( nl, nr, sqre, k, d, z, alpha, beta, u, ldu, vt,ldvt, dsigma, &
               u2, ldu2, vt2, ldvt2, idxp, idx,idxc, idxq, coltyp, info )
           integer(ilp), intent(out) :: info, k
           integer(ilp), intent(in) :: ldu, ldu2, ldvt, ldvt2, nl, nr, sqre
           real(sp), intent(in) :: alpha, beta
           integer(ilp), intent(out) :: coltyp(*), idx(*), idxc(*), idxp(*)
           integer(ilp), intent(inout) :: idxq(*)
           real(sp), intent(inout) :: d(*), u(ldu,*), vt(ldvt,*)
           real(sp), intent(out) :: dsigma(*), u2(ldu2,*), vt2(ldvt2,*), z(*)
     end subroutine stdlib_slasd2

     pure module subroutine stdlib_dlasd2( nl, nr, sqre, k, d, z, alpha, beta, u, ldu, vt,ldvt, dsigma, &
               u2, ldu2, vt2, ldvt2, idxp, idx,idxc, idxq, coltyp, info )
           integer(ilp), intent(out) :: info, k
           integer(ilp), intent(in) :: ldu, ldu2, ldvt, ldvt2, nl, nr, sqre
           real(dp), intent(in) :: alpha, beta
           integer(ilp), intent(out) :: coltyp(*), idx(*), idxc(*), idxp(*)
           integer(ilp), intent(inout) :: idxq(*)
           real(dp), intent(inout) :: d(*), u(ldu,*), vt(ldvt,*)
           real(dp), intent(out) :: dsigma(*), u2(ldu2,*), vt2(ldvt2,*), z(*)
     end subroutine stdlib_dlasd2


end interface 


interface 
     pure module subroutine stdlib_slasd3( nl, nr, sqre, k, d, q, ldq, dsigma, u, ldu, u2,ldu2, vt, ldvt,&
                vt2, ldvt2, idxc, ctot, z,info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, ldq, ldu, ldu2, ldvt, ldvt2, nl, nr, sqre
           integer(ilp), intent(in) :: ctot(*), idxc(*)
           real(sp), intent(out) :: d(*), q(ldq,*), u(ldu,*), vt(ldvt,*)
           real(sp), intent(inout) :: dsigma(*), vt2(ldvt2,*), z(*)
           real(sp), intent(in) :: u2(ldu2,*)
     end subroutine stdlib_slasd3

     pure module subroutine stdlib_dlasd3( nl, nr, sqre, k, d, q, ldq, dsigma, u, ldu, u2,ldu2, vt, ldvt,&
                vt2, ldvt2, idxc, ctot, z,info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, ldq, ldu, ldu2, ldvt, ldvt2, nl, nr, sqre
           integer(ilp), intent(in) :: ctot(*), idxc(*)
           real(dp), intent(out) :: d(*), q(ldq,*), u(ldu,*), vt(ldvt,*)
           real(dp), intent(inout) :: dsigma(*), vt2(ldvt2,*), z(*)
           real(dp), intent(in) :: u2(ldu2,*)
     end subroutine stdlib_dlasd3


end interface 


interface 
     pure module subroutine stdlib_slasd4( n, i, d, z, delta, rho, sigma, work, info )
           integer(ilp), intent(in) :: i, n
           integer(ilp), intent(out) :: info
           real(sp), intent(in) :: rho
           real(sp), intent(out) :: sigma
           real(sp), intent(in) :: d(*), z(*)
           real(sp), intent(out) :: delta(*), work(*)
     end subroutine stdlib_slasd4

     pure module subroutine stdlib_dlasd4( n, i, d, z, delta, rho, sigma, work, info )
           integer(ilp), intent(in) :: i, n
           integer(ilp), intent(out) :: info
           real(dp), intent(in) :: rho
           real(dp), intent(out) :: sigma
           real(dp), intent(in) :: d(*), z(*)
           real(dp), intent(out) :: delta(*), work(*)
     end subroutine stdlib_dlasd4


end interface 


interface 
     pure module subroutine stdlib_slasd5( i, d, z, delta, rho, dsigma, work )
           integer(ilp), intent(in) :: i
           real(sp), intent(out) :: dsigma
           real(sp), intent(in) :: rho
           real(sp), intent(in) :: d(2_ilp), z(2_ilp)
           real(sp), intent(out) :: delta(2_ilp), work(2_ilp)
     end subroutine stdlib_slasd5

     pure module subroutine stdlib_dlasd5( i, d, z, delta, rho, dsigma, work )
           integer(ilp), intent(in) :: i
           real(dp), intent(out) :: dsigma
           real(dp), intent(in) :: rho
           real(dp), intent(in) :: d(2_ilp), z(2_ilp)
           real(dp), intent(out) :: delta(2_ilp), work(2_ilp)
     end subroutine stdlib_dlasd5


end interface 


interface 
     pure module subroutine stdlib_slasdq( uplo, sqre, n, ncvt, nru, ncc, d, e, vt, ldvt,u, ldu, c, ldc, &
               work, info )
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldc, ldu, ldvt, n, ncc, ncvt, nru, sqre
           real(sp), intent(inout) :: c(ldc,*), d(*), e(*), u(ldu,*), vt(ldvt,*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_slasdq

     pure module subroutine stdlib_dlasdq( uplo, sqre, n, ncvt, nru, ncc, d, e, vt, ldvt,u, ldu, c, ldc, &
               work, info )
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldc, ldu, ldvt, n, ncc, ncvt, nru, sqre
           real(dp), intent(inout) :: c(ldc,*), d(*), e(*), u(ldu,*), vt(ldvt,*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_dlasdq


end interface 


interface 
     pure module subroutine stdlib_slasda( icompq, smlsiz, n, sqre, d, e, u, ldu, vt, k,difl, difr, z, &
               poles, givptr, givcol, ldgcol,perm, givnum, c, s, work, iwork, info )
           integer(ilp), intent(in) :: icompq, ldgcol, ldu, n, smlsiz, sqre
           integer(ilp), intent(out) :: info
           integer(ilp), intent(out) :: givcol(ldgcol,*), givptr(*), iwork(*), k(*), perm(ldgcol,&
                     *)
           real(sp), intent(out) :: c(*), difl(ldu,*), difr(ldu,*), givnum(ldu,*), poles(ldu,*), &
                     s(*), u(ldu,*), vt(ldu,*), work(*), z(ldu,*)
           real(sp), intent(inout) :: d(*), e(*)
     end subroutine stdlib_slasda

     pure module subroutine stdlib_dlasda( icompq, smlsiz, n, sqre, d, e, u, ldu, vt, k,difl, difr, z, &
               poles, givptr, givcol, ldgcol,perm, givnum, c, s, work, iwork, info )
           integer(ilp), intent(in) :: icompq, ldgcol, ldu, n, smlsiz, sqre
           integer(ilp), intent(out) :: info
           integer(ilp), intent(out) :: givcol(ldgcol,*), givptr(*), iwork(*), k(*), perm(ldgcol,&
                     *)
           real(dp), intent(out) :: c(*), difl(ldu,*), difr(ldu,*), givnum(ldu,*), poles(ldu,*), &
                     s(*), u(ldu,*), vt(ldu,*), work(*), z(ldu,*)
           real(dp), intent(inout) :: d(*), e(*)
     end subroutine stdlib_dlasda


end interface 


interface 
     pure module subroutine stdlib_slasd6( icompq, nl, nr, sqre, d, vf, vl, alpha, beta,idxq, perm, &
     givptr, givcol, ldgcol, givnum,ldgnum, poles, difl, difr, z, k, c, s, work,iwork, info )
               
           integer(ilp), intent(out) :: givptr, info, k
           integer(ilp), intent(in) :: icompq, ldgcol, ldgnum, nl, nr, sqre
           real(sp), intent(inout) :: alpha, beta
           real(sp), intent(out) :: c, s
           integer(ilp), intent(out) :: givcol(ldgcol,*), iwork(*), perm(*)
           integer(ilp), intent(inout) :: idxq(*)
           real(sp), intent(inout) :: d(*), vf(*), vl(*)
           real(sp), intent(out) :: difl(*), difr(*), givnum(ldgnum,*), poles(ldgnum,*), work(*), &
                     z(*)
     end subroutine stdlib_slasd6

     pure module subroutine stdlib_dlasd6( icompq, nl, nr, sqre, d, vf, vl, alpha, beta,idxq, perm, &
     givptr, givcol, ldgcol, givnum,ldgnum, poles, difl, difr, z, k, c, s, work,iwork, info )
               
           integer(ilp), intent(out) :: givptr, info, k
           integer(ilp), intent(in) :: icompq, ldgcol, ldgnum, nl, nr, sqre
           real(dp), intent(inout) :: alpha, beta
           real(dp), intent(out) :: c, s
           integer(ilp), intent(out) :: givcol(ldgcol,*), iwork(*), perm(*)
           integer(ilp), intent(inout) :: idxq(*)
           real(dp), intent(inout) :: d(*), vf(*), vl(*)
           real(dp), intent(out) :: difl(*), difr(*), givnum(ldgnum,*), poles(ldgnum,*), work(*), &
                     z(*)
     end subroutine stdlib_dlasd6


end interface 


interface 
     pure module subroutine stdlib_slasd7( icompq, nl, nr, sqre, k, d, z, zw, vf, vfw, vl,vlw, alpha, &
     beta, dsigma, idx, idxp, idxq,perm, givptr, givcol, ldgcol, givnum, ldgnum,c, s, info )
               
           integer(ilp), intent(out) :: givptr, info, k
           integer(ilp), intent(in) :: icompq, ldgcol, ldgnum, nl, nr, sqre
           real(sp), intent(in) :: alpha, beta
           real(sp), intent(out) :: c, s
           integer(ilp), intent(out) :: givcol(ldgcol,*), idx(*), idxp(*), perm(*)
           integer(ilp), intent(inout) :: idxq(*)
           real(sp), intent(inout) :: d(*), vf(*), vl(*)
           real(sp), intent(out) :: dsigma(*), givnum(ldgnum,*), vfw(*), vlw(*), z(*), zw(*)
                     
     end subroutine stdlib_slasd7

     pure module subroutine stdlib_dlasd7( icompq, nl, nr, sqre, k, d, z, zw, vf, vfw, vl,vlw, alpha, &
     beta, dsigma, idx, idxp, idxq,perm, givptr, givcol, ldgcol, givnum, ldgnum,c, s, info )
               
           integer(ilp), intent(out) :: givptr, info, k
           integer(ilp), intent(in) :: icompq, ldgcol, ldgnum, nl, nr, sqre
           real(dp), intent(in) :: alpha, beta
           real(dp), intent(out) :: c, s
           integer(ilp), intent(out) :: givcol(ldgcol,*), idx(*), idxp(*), perm(*)
           integer(ilp), intent(inout) :: idxq(*)
           real(dp), intent(inout) :: d(*), vf(*), vl(*)
           real(dp), intent(out) :: dsigma(*), givnum(ldgnum,*), vfw(*), vlw(*), z(*), zw(*)
                     
     end subroutine stdlib_dlasd7


end interface 


interface 
     pure module subroutine stdlib_slasd8( icompq, k, d, z, vf, vl, difl, difr, lddifr,dsigma, work, &
               info )
           integer(ilp), intent(in) :: icompq, k, lddifr
           integer(ilp), intent(out) :: info
           real(sp), intent(out) :: d(*), difl(*), difr(lddifr,*), work(*)
           real(sp), intent(inout) :: dsigma(*), vf(*), vl(*), z(*)
     end subroutine stdlib_slasd8

     pure module subroutine stdlib_dlasd8( icompq, k, d, z, vf, vl, difl, difr, lddifr,dsigma, work, &
               info )
           integer(ilp), intent(in) :: icompq, k, lddifr
           integer(ilp), intent(out) :: info
           real(dp), intent(out) :: d(*), difl(*), difr(lddifr,*), work(*)
           real(dp), intent(inout) :: dsigma(*), vf(*), vl(*), z(*)
     end subroutine stdlib_dlasd8


end interface 


interface 
     module subroutine stdlib_sgesvd( jobu, jobvt, m, n, a, lda, s, u, ldu, vt, ldvt,work, lwork, info )
               
           character, intent(in) :: jobu, jobvt
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldu, ldvt, lwork, m, n
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: s(*), u(ldu,*), vt(ldvt,*), work(*)
     end subroutine stdlib_sgesvd

     module subroutine stdlib_dgesvd( jobu, jobvt, m, n, a, lda, s, u, ldu,vt, ldvt, work, lwork, info )
               
           character, intent(in) :: jobu, jobvt
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldu, ldvt, lwork, m, n
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: s(*), u(ldu,*), vt(ldvt,*), work(*)
     end subroutine stdlib_dgesvd


     module subroutine stdlib_cgesvd( jobu, jobvt, m, n, a, lda, s, u, ldu, vt, ldvt,work, lwork, rwork, &
               info )
           character, intent(in) :: jobu, jobvt
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldu, ldvt, lwork, m, n
           real(sp), intent(out) :: rwork(*), s(*)
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: u(ldu,*), vt(ldvt,*), work(*)
     end subroutine stdlib_cgesvd

     module subroutine stdlib_zgesvd( jobu, jobvt, m, n, a, lda, s, u, ldu,vt, ldvt, work, lwork, rwork, &
               info )
           character, intent(in) :: jobu, jobvt
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldu, ldvt, lwork, m, n
           real(dp), intent(out) :: rwork(*), s(*)
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: u(ldu,*), vt(ldvt,*), work(*)
     end subroutine stdlib_zgesvd


end interface 


interface 
     module subroutine stdlib_sgesvdq( joba, jobp, jobr, jobu, jobv, m, n, a, lda,s, u, ldu, v, ldv, &
               numrank, iwork, liwork,work, lwork, rwork, lrwork, info )
           character, intent(in) :: joba, jobp, jobr, jobu, jobv
           integer(ilp), intent(in) :: m, n, lda, ldu, ldv, liwork, lrwork
           integer(ilp), intent(out) :: numrank, info
           integer(ilp), intent(inout) :: lwork
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: u(ldu,*), v(ldv,*), work(*)
           real(sp), intent(out) :: s(*), rwork(*)
           integer(ilp), intent(out) :: iwork(*)
     end subroutine stdlib_sgesvdq

     module subroutine stdlib_dgesvdq( joba, jobp, jobr, jobu, jobv, m, n, a, lda,s, u, ldu, v, ldv, &
               numrank, iwork, liwork,work, lwork, rwork, lrwork, info )
           character, intent(in) :: joba, jobp, jobr, jobu, jobv
           integer(ilp), intent(in) :: m, n, lda, ldu, ldv, liwork, lrwork
           integer(ilp), intent(out) :: numrank, info
           integer(ilp), intent(inout) :: lwork
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: u(ldu,*), v(ldv,*), work(*)
           real(dp), intent(out) :: s(*), rwork(*)
           integer(ilp), intent(out) :: iwork(*)
     end subroutine stdlib_dgesvdq


     module subroutine stdlib_cgesvdq( joba, jobp, jobr, jobu, jobv, m, n, a, lda,s, u, ldu, v, ldv, &
               numrank, iwork, liwork,cwork, lcwork, rwork, lrwork, info )
           character, intent(in) :: joba, jobp, jobr, jobu, jobv
           integer(ilp), intent(in) :: m, n, lda, ldu, ldv, liwork, lrwork
           integer(ilp), intent(out) :: numrank, info
           integer(ilp), intent(inout) :: lcwork
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: u(ldu,*), v(ldv,*), cwork(*)
           real(sp), intent(out) :: s(*), rwork(*)
           integer(ilp), intent(out) :: iwork(*)
     end subroutine stdlib_cgesvdq

     module subroutine stdlib_zgesvdq( joba, jobp, jobr, jobu, jobv, m, n, a, lda,s, u, ldu, v, ldv, &
               numrank, iwork, liwork,cwork, lcwork, rwork, lrwork, info )
           character, intent(in) :: joba, jobp, jobr, jobu, jobv
           integer(ilp), intent(in) :: m, n, lda, ldu, ldv, liwork, lrwork
           integer(ilp), intent(out) :: numrank, info
           integer(ilp), intent(inout) :: lcwork
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: u(ldu,*), v(ldv,*), cwork(*)
           real(dp), intent(out) :: s(*), rwork(*)
           integer(ilp), intent(out) :: iwork(*)
     end subroutine stdlib_zgesvdq


end interface 


interface 
     module subroutine stdlib_sgesdd( jobz, m, n, a, lda, s, u, ldu, vt, ldvt,work, lwork, iwork, info )
               
           character, intent(in) :: jobz
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldu, ldvt, lwork, m, n
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: s(*), u(ldu,*), vt(ldvt,*), work(*)
     end subroutine stdlib_sgesdd

     module subroutine stdlib_dgesdd( jobz, m, n, a, lda, s, u, ldu, vt, ldvt,work, lwork, iwork, info )
               
           character, intent(in) :: jobz
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldu, ldvt, lwork, m, n
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: s(*), u(ldu,*), vt(ldvt,*), work(*)
     end subroutine stdlib_dgesdd


     module subroutine stdlib_cgesdd( jobz, m, n, a, lda, s, u, ldu, vt, ldvt,work, lwork, rwork, iwork, &
               info )
           character, intent(in) :: jobz
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldu, ldvt, lwork, m, n
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(out) :: rwork(*), s(*)
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: u(ldu,*), vt(ldvt,*), work(*)
     end subroutine stdlib_cgesdd

     module subroutine stdlib_zgesdd( jobz, m, n, a, lda, s, u, ldu, vt, ldvt,work, lwork, rwork, iwork, &
               info )
           character, intent(in) :: jobz
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldu, ldvt, lwork, m, n
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(out) :: rwork(*), s(*)
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: u(ldu,*), vt(ldvt,*), work(*)
     end subroutine stdlib_zgesdd


end interface 


interface 
     pure module subroutine stdlib_sgejsv( joba, jobu, jobv, jobr, jobt, jobp,m, n, a, lda, sva, u, ldu, &
               v, ldv,work, lwork, iwork, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldu, ldv, lwork, m, n
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: sva(n), u(ldu,*), v(ldv,*), work(lwork)
           integer(ilp), intent(out) :: iwork(*)
           character, intent(in) :: joba, jobp, jobr, jobt, jobu, jobv
     end subroutine stdlib_sgejsv

     pure module subroutine stdlib_dgejsv( joba, jobu, jobv, jobr, jobt, jobp,m, n, a, lda, sva, u, ldu, &
               v, ldv,work, lwork, iwork, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldu, ldv, lwork, m, n
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: sva(n), u(ldu,*), v(ldv,*), work(lwork)
           integer(ilp), intent(out) :: iwork(*)
           character, intent(in) :: joba, jobp, jobr, jobt, jobu, jobv
     end subroutine stdlib_dgejsv


     pure module subroutine stdlib_cgejsv( joba, jobu, jobv, jobr, jobt, jobp,m, n, a, lda, sva, u, ldu, &
               v, ldv,cwork, lwork, rwork, lrwork, iwork, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldu, ldv, lwork, lrwork, m, n
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: u(ldu,*), v(ldv,*), cwork(lwork)
           real(sp), intent(out) :: sva(n), rwork(lrwork)
           integer(ilp), intent(out) :: iwork(*)
           character, intent(in) :: joba, jobp, jobr, jobt, jobu, jobv
     end subroutine stdlib_cgejsv

     pure module subroutine stdlib_zgejsv( joba, jobu, jobv, jobr, jobt, jobp,m, n, a, lda, sva, u, ldu, &
               v, ldv,cwork, lwork, rwork, lrwork, iwork, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldu, ldv, lwork, lrwork, m, n
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: u(ldu,*), v(ldv,*), cwork(lwork)
           real(dp), intent(out) :: sva(n), rwork(lrwork)
           integer(ilp), intent(out) :: iwork(*)
           character, intent(in) :: joba, jobp, jobr, jobt, jobu, jobv
     end subroutine stdlib_zgejsv


end interface 


interface 
     pure module subroutine stdlib_sgesvj( joba, jobu, jobv, m, n, a, lda, sva, mv, v,ldv, work, lwork, &
               info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldv, lwork, m, mv, n
           character, intent(in) :: joba, jobu, jobv
           real(sp), intent(inout) :: a(lda,*), v(ldv,*), work(lwork)
           real(sp), intent(out) :: sva(n)
     end subroutine stdlib_sgesvj

     pure module subroutine stdlib_dgesvj( joba, jobu, jobv, m, n, a, lda, sva, mv, v,ldv, work, lwork, &
               info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldv, lwork, m, mv, n
           character, intent(in) :: joba, jobu, jobv
           real(dp), intent(inout) :: a(lda,*), v(ldv,*), work(lwork)
           real(dp), intent(out) :: sva(n)
     end subroutine stdlib_dgesvj


     pure module subroutine stdlib_cgesvj( joba, jobu, jobv, m, n, a, lda, sva, mv, v,ldv, cwork, lwork, &
               rwork, lrwork, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldv, lwork, lrwork, m, mv, n
           character, intent(in) :: joba, jobu, jobv
           complex(sp), intent(inout) :: a(lda,*), v(ldv,*), cwork(lwork)
           real(sp), intent(inout) :: rwork(lrwork)
           real(sp), intent(out) :: sva(n)
     end subroutine stdlib_cgesvj

     pure module subroutine stdlib_zgesvj( joba, jobu, jobv, m, n, a, lda, sva, mv, v,ldv, cwork, lwork, &
               rwork, lrwork, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldv, lwork, lrwork, m, mv, n
           character, intent(in) :: joba, jobu, jobv
           complex(dp), intent(inout) :: a(lda,*), v(ldv,*), cwork(lwork)
           real(dp), intent(inout) :: rwork(lrwork)
           real(dp), intent(out) :: sva(n)
     end subroutine stdlib_zgesvj


end interface 


interface 
     pure module subroutine stdlib_sbdsqr( uplo, n, ncvt, nru, ncc, d, e, vt, ldvt, u,ldu, c, ldc, work, &
               info )
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldc, ldu, ldvt, n, ncc, ncvt, nru
           real(sp), intent(inout) :: c(ldc,*), d(*), e(*), u(ldu,*), vt(ldvt,*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_sbdsqr

     pure module subroutine stdlib_dbdsqr( uplo, n, ncvt, nru, ncc, d, e, vt, ldvt, u,ldu, c, ldc, work, &
               info )
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldc, ldu, ldvt, n, ncc, ncvt, nru
           real(dp), intent(inout) :: c(ldc,*), d(*), e(*), u(ldu,*), vt(ldvt,*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_dbdsqr


     pure module subroutine stdlib_cbdsqr( uplo, n, ncvt, nru, ncc, d, e, vt, ldvt, u,ldu, c, ldc, rwork,&
                info )
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldc, ldu, ldvt, n, ncc, ncvt, nru
           real(sp), intent(inout) :: d(*), e(*)
           real(sp), intent(out) :: rwork(*)
           complex(sp), intent(inout) :: c(ldc,*), u(ldu,*), vt(ldvt,*)
     end subroutine stdlib_cbdsqr

     pure module subroutine stdlib_zbdsqr( uplo, n, ncvt, nru, ncc, d, e, vt, ldvt, u,ldu, c, ldc, rwork,&
                info )
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldc, ldu, ldvt, n, ncc, ncvt, nru
           real(dp), intent(inout) :: d(*), e(*)
           real(dp), intent(out) :: rwork(*)
           complex(dp), intent(inout) :: c(ldc,*), u(ldu,*), vt(ldvt,*)
     end subroutine stdlib_zbdsqr


end interface 


interface 
     pure module subroutine stdlib_sbdsdc( uplo, compq, n, d, e, u, ldu, vt, ldvt, q, iq,work, iwork, &
               info )
           character, intent(in) :: compq, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldu, ldvt, n
           integer(ilp), intent(out) :: iq(*), iwork(*)
           real(sp), intent(inout) :: d(*), e(*)
           real(sp), intent(out) :: q(*), u(ldu,*), vt(ldvt,*), work(*)
     end subroutine stdlib_sbdsdc

     pure module subroutine stdlib_dbdsdc( uplo, compq, n, d, e, u, ldu, vt, ldvt, q, iq,work, iwork, &
               info )
           character, intent(in) :: compq, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldu, ldvt, n
           integer(ilp), intent(out) :: iq(*), iwork(*)
           real(dp), intent(inout) :: d(*), e(*)
           real(dp), intent(out) :: q(*), u(ldu,*), vt(ldvt,*), work(*)
     end subroutine stdlib_dbdsdc


end interface 


interface 
     module subroutine stdlib_ssyev( jobz, uplo, n, a, lda, w, work, lwork, info )
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, n
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: w(*), work(*)
     end subroutine stdlib_ssyev

     module subroutine stdlib_dsyev( jobz, uplo, n, a, lda, w, work, lwork, info )
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, n
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: w(*), work(*)
     end subroutine stdlib_dsyev


end interface 


interface 
     module subroutine stdlib_ssyevd( jobz, uplo, n, a, lda, w, work, lwork, iwork,liwork, info )
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, liwork, lwork, n
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: w(*), work(*)
     end subroutine stdlib_ssyevd

     module subroutine stdlib_dsyevd( jobz, uplo, n, a, lda, w, work, lwork, iwork,liwork, info )
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, liwork, lwork, n
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: w(*), work(*)
     end subroutine stdlib_dsyevd


end interface 


interface 
     module subroutine stdlib_ssyevr( jobz, range, uplo, n, a, lda, vl, vu, il, iu,abstol, m, w, z, ldz, &
               isuppz, work, lwork,iwork, liwork, info )
           character, intent(in) :: jobz, range, uplo
           integer(ilp), intent(in) :: il, iu, lda, ldz, liwork, lwork, n
           integer(ilp), intent(out) :: info, m
           real(sp), intent(in) :: abstol, vl, vu
           integer(ilp), intent(out) :: isuppz(*), iwork(*)
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: w(*), work(*), z(ldz,*)
     end subroutine stdlib_ssyevr

     module subroutine stdlib_dsyevr( jobz, range, uplo, n, a, lda, vl, vu, il, iu,abstol, m, w, z, ldz, &
               isuppz, work, lwork,iwork, liwork, info )
           character, intent(in) :: jobz, range, uplo
           integer(ilp), intent(in) :: il, iu, lda, ldz, liwork, lwork, n
           integer(ilp), intent(out) :: info, m
           real(dp), intent(in) :: abstol, vl, vu
           integer(ilp), intent(out) :: isuppz(*), iwork(*)
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: w(*), work(*), z(ldz,*)
     end subroutine stdlib_dsyevr


end interface 


interface 
     module subroutine stdlib_ssyevx( jobz, range, uplo, n, a, lda, vl, vu, il, iu,abstol, m, w, z, ldz, &
               work, lwork, iwork,ifail, info )
           character, intent(in) :: jobz, range, uplo
           integer(ilp), intent(in) :: il, iu, lda, ldz, lwork, n
           integer(ilp), intent(out) :: info, m
           real(sp), intent(in) :: abstol, vl, vu
           integer(ilp), intent(out) :: ifail(*), iwork(*)
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: w(*), work(*), z(ldz,*)
     end subroutine stdlib_ssyevx

     module subroutine stdlib_dsyevx( jobz, range, uplo, n, a, lda, vl, vu, il, iu,abstol, m, w, z, ldz, &
               work, lwork, iwork,ifail, info )
           character, intent(in) :: jobz, range, uplo
           integer(ilp), intent(in) :: il, iu, lda, ldz, lwork, n
           integer(ilp), intent(out) :: info, m
           real(dp), intent(in) :: abstol, vl, vu
           integer(ilp), intent(out) :: ifail(*), iwork(*)
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: w(*), work(*), z(ldz,*)
     end subroutine stdlib_dsyevx


end interface 


interface 
     module subroutine stdlib_sspev( jobz, uplo, n, ap, w, z, ldz, work, info )
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldz, n
           real(sp), intent(inout) :: ap(*)
           real(sp), intent(out) :: w(*), work(*), z(ldz,*)
     end subroutine stdlib_sspev

     module subroutine stdlib_dspev( jobz, uplo, n, ap, w, z, ldz, work, info )
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldz, n
           real(dp), intent(inout) :: ap(*)
           real(dp), intent(out) :: w(*), work(*), z(ldz,*)
     end subroutine stdlib_dspev


end interface 


interface 
     module subroutine stdlib_sspevd( jobz, uplo, n, ap, w, z, ldz, work, lwork,iwork, liwork, info )
               
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldz, liwork, lwork, n
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(inout) :: ap(*)
           real(sp), intent(out) :: w(*), work(*), z(ldz,*)
     end subroutine stdlib_sspevd

     module subroutine stdlib_dspevd( jobz, uplo, n, ap, w, z, ldz, work, lwork,iwork, liwork, info )
               
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldz, liwork, lwork, n
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(inout) :: ap(*)
           real(dp), intent(out) :: w(*), work(*), z(ldz,*)
     end subroutine stdlib_dspevd


end interface 


interface 
     module subroutine stdlib_sspevx( jobz, range, uplo, n, ap, vl, vu, il, iu,abstol, m, w, z, ldz, &
               work, iwork, ifail,info )
           character, intent(in) :: jobz, range, uplo
           integer(ilp), intent(in) :: il, iu, ldz, n
           integer(ilp), intent(out) :: info, m
           real(sp), intent(in) :: abstol, vl, vu
           integer(ilp), intent(out) :: ifail(*), iwork(*)
           real(sp), intent(inout) :: ap(*)
           real(sp), intent(out) :: w(*), work(*), z(ldz,*)
     end subroutine stdlib_sspevx

     module subroutine stdlib_dspevx( jobz, range, uplo, n, ap, vl, vu, il, iu,abstol, m, w, z, ldz, &
               work, iwork, ifail,info )
           character, intent(in) :: jobz, range, uplo
           integer(ilp), intent(in) :: il, iu, ldz, n
           integer(ilp), intent(out) :: info, m
           real(dp), intent(in) :: abstol, vl, vu
           integer(ilp), intent(out) :: ifail(*), iwork(*)
           real(dp), intent(inout) :: ap(*)
           real(dp), intent(out) :: w(*), work(*), z(ldz,*)
     end subroutine stdlib_dspevx


end interface 


interface 
     module subroutine stdlib_ssbev( jobz, uplo, n, kd, ab, ldab, w, z, ldz, work,info )
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kd, ldab, ldz, n
           real(sp), intent(inout) :: ab(ldab,*)
           real(sp), intent(out) :: w(*), work(*), z(ldz,*)
     end subroutine stdlib_ssbev

     module subroutine stdlib_dsbev( jobz, uplo, n, kd, ab, ldab, w, z, ldz, work,info )
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kd, ldab, ldz, n
           real(dp), intent(inout) :: ab(ldab,*)
           real(dp), intent(out) :: w(*), work(*), z(ldz,*)
     end subroutine stdlib_dsbev


end interface 


interface 
     module subroutine stdlib_ssbevd( jobz, uplo, n, kd, ab, ldab, w, z, ldz, work,lwork, iwork, liwork, &
               info )
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kd, ldab, ldz, liwork, lwork, n
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(inout) :: ab(ldab,*)
           real(sp), intent(out) :: w(*), work(*), z(ldz,*)
     end subroutine stdlib_ssbevd

     module subroutine stdlib_dsbevd( jobz, uplo, n, kd, ab, ldab, w, z, ldz, work,lwork, iwork, liwork, &
               info )
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kd, ldab, ldz, liwork, lwork, n
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(inout) :: ab(ldab,*)
           real(dp), intent(out) :: w(*), work(*), z(ldz,*)
     end subroutine stdlib_dsbevd


end interface 


interface 
     module subroutine stdlib_ssbevx( jobz, range, uplo, n, kd, ab, ldab, q, ldq, vl,vu, il, iu, abstol, &
               m, w, z, ldz, work, iwork,ifail, info )
           character, intent(in) :: jobz, range, uplo
           integer(ilp), intent(in) :: il, iu, kd, ldab, ldq, ldz, n
           integer(ilp), intent(out) :: info, m
           real(sp), intent(in) :: abstol, vl, vu
           integer(ilp), intent(out) :: ifail(*), iwork(*)
           real(sp), intent(inout) :: ab(ldab,*)
           real(sp), intent(out) :: q(ldq,*), w(*), work(*), z(ldz,*)
     end subroutine stdlib_ssbevx

     module subroutine stdlib_dsbevx( jobz, range, uplo, n, kd, ab, ldab, q, ldq, vl,vu, il, iu, abstol, &
               m, w, z, ldz, work, iwork,ifail, info )
           character, intent(in) :: jobz, range, uplo
           integer(ilp), intent(in) :: il, iu, kd, ldab, ldq, ldz, n
           integer(ilp), intent(out) :: info, m
           real(dp), intent(in) :: abstol, vl, vu
           integer(ilp), intent(out) :: ifail(*), iwork(*)
           real(dp), intent(inout) :: ab(ldab,*)
           real(dp), intent(out) :: q(ldq,*), w(*), work(*), z(ldz,*)
     end subroutine stdlib_dsbevx


end interface 


interface 
     module subroutine stdlib_cheev( jobz, uplo, n, a, lda, w, work, lwork, rwork,info )
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, n
           real(sp), intent(out) :: rwork(*), w(*)
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_cheev

     module subroutine stdlib_zheev( jobz, uplo, n, a, lda, w, work, lwork, rwork,info )
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, n
           real(dp), intent(out) :: rwork(*), w(*)
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_zheev


end interface 


interface 
     module subroutine stdlib_cheevd( jobz, uplo, n, a, lda, w, work, lwork, rwork,lrwork, iwork, liwork,&
                info )
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, liwork, lrwork, lwork, n
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(out) :: rwork(*), w(*)
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_cheevd

     module subroutine stdlib_zheevd( jobz, uplo, n, a, lda, w, work, lwork, rwork,lrwork, iwork, liwork,&
                info )
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, liwork, lrwork, lwork, n
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(out) :: rwork(*), w(*)
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_zheevd


end interface 


interface 
     module subroutine stdlib_cheevr( jobz, range, uplo, n, a, lda, vl, vu, il, iu,abstol, m, w, z, ldz, &
               isuppz, work, lwork,rwork, lrwork, iwork, liwork, info )
           character, intent(in) :: jobz, range, uplo
           integer(ilp), intent(in) :: il, iu, lda, ldz, liwork, lrwork, lwork, n
           integer(ilp), intent(out) :: info, m
           real(sp), intent(in) :: abstol, vl, vu
           integer(ilp), intent(out) :: isuppz(*), iwork(*)
           real(sp), intent(out) :: rwork(*), w(*)
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: work(*), z(ldz,*)
     end subroutine stdlib_cheevr

     module subroutine stdlib_zheevr( jobz, range, uplo, n, a, lda, vl, vu, il, iu,abstol, m, w, z, ldz, &
               isuppz, work, lwork,rwork, lrwork, iwork, liwork, info )
           character, intent(in) :: jobz, range, uplo
           integer(ilp), intent(in) :: il, iu, lda, ldz, liwork, lrwork, lwork, n
           integer(ilp), intent(out) :: info, m
           real(dp), intent(in) :: abstol, vl, vu
           integer(ilp), intent(out) :: isuppz(*), iwork(*)
           real(dp), intent(out) :: rwork(*), w(*)
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: work(*), z(ldz,*)
     end subroutine stdlib_zheevr


end interface 


interface 
     module subroutine stdlib_cheevx( jobz, range, uplo, n, a, lda, vl, vu, il, iu,abstol, m, w, z, ldz, &
               work, lwork, rwork,iwork, ifail, info )
           character, intent(in) :: jobz, range, uplo
           integer(ilp), intent(in) :: il, iu, lda, ldz, lwork, n
           integer(ilp), intent(out) :: info, m
           real(sp), intent(in) :: abstol, vl, vu
           integer(ilp), intent(out) :: ifail(*), iwork(*)
           real(sp), intent(out) :: rwork(*), w(*)
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: work(*), z(ldz,*)
     end subroutine stdlib_cheevx

     module subroutine stdlib_zheevx( jobz, range, uplo, n, a, lda, vl, vu, il, iu,abstol, m, w, z, ldz, &
               work, lwork, rwork,iwork, ifail, info )
           character, intent(in) :: jobz, range, uplo
           integer(ilp), intent(in) :: il, iu, lda, ldz, lwork, n
           integer(ilp), intent(out) :: info, m
           real(dp), intent(in) :: abstol, vl, vu
           integer(ilp), intent(out) :: ifail(*), iwork(*)
           real(dp), intent(out) :: rwork(*), w(*)
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: work(*), z(ldz,*)
     end subroutine stdlib_zheevx


end interface 


interface 
     module subroutine stdlib_chpev( jobz, uplo, n, ap, w, z, ldz, work, rwork,info )
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldz, n
           real(sp), intent(out) :: rwork(*), w(*)
           complex(sp), intent(inout) :: ap(*)
           complex(sp), intent(out) :: work(*), z(ldz,*)
     end subroutine stdlib_chpev

     module subroutine stdlib_zhpev( jobz, uplo, n, ap, w, z, ldz, work, rwork,info )
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldz, n
           real(dp), intent(out) :: rwork(*), w(*)
           complex(dp), intent(inout) :: ap(*)
           complex(dp), intent(out) :: work(*), z(ldz,*)
     end subroutine stdlib_zhpev


end interface 


interface 
     module subroutine stdlib_chpevd( jobz, uplo, n, ap, w, z, ldz, work, lwork,rwork, lrwork, iwork, &
               liwork, info )
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldz, liwork, lrwork, lwork, n
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(out) :: rwork(*), w(*)
           complex(sp), intent(inout) :: ap(*)
           complex(sp), intent(out) :: work(*), z(ldz,*)
     end subroutine stdlib_chpevd

     module subroutine stdlib_zhpevd( jobz, uplo, n, ap, w, z, ldz, work, lwork,rwork, lrwork, iwork, &
               liwork, info )
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldz, liwork, lrwork, lwork, n
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(out) :: rwork(*), w(*)
           complex(dp), intent(inout) :: ap(*)
           complex(dp), intent(out) :: work(*), z(ldz,*)
     end subroutine stdlib_zhpevd


end interface 


interface 
     module subroutine stdlib_chpevx( jobz, range, uplo, n, ap, vl, vu, il, iu,abstol, m, w, z, ldz, &
               work, rwork, iwork,ifail, info )
           character, intent(in) :: jobz, range, uplo
           integer(ilp), intent(in) :: il, iu, ldz, n
           integer(ilp), intent(out) :: info, m
           real(sp), intent(in) :: abstol, vl, vu
           integer(ilp), intent(out) :: ifail(*), iwork(*)
           real(sp), intent(out) :: rwork(*), w(*)
           complex(sp), intent(inout) :: ap(*)
           complex(sp), intent(out) :: work(*), z(ldz,*)
     end subroutine stdlib_chpevx

     module subroutine stdlib_zhpevx( jobz, range, uplo, n, ap, vl, vu, il, iu,abstol, m, w, z, ldz, &
               work, rwork, iwork,ifail, info )
           character, intent(in) :: jobz, range, uplo
           integer(ilp), intent(in) :: il, iu, ldz, n
           integer(ilp), intent(out) :: info, m
           real(dp), intent(in) :: abstol, vl, vu
           integer(ilp), intent(out) :: ifail(*), iwork(*)
           real(dp), intent(out) :: rwork(*), w(*)
           complex(dp), intent(inout) :: ap(*)
           complex(dp), intent(out) :: work(*), z(ldz,*)
     end subroutine stdlib_zhpevx


end interface 


interface 
     module subroutine stdlib_chbev( jobz, uplo, n, kd, ab, ldab, w, z, ldz, work,rwork, info )
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kd, ldab, ldz, n
           real(sp), intent(out) :: rwork(*), w(*)
           complex(sp), intent(inout) :: ab(ldab,*)
           complex(sp), intent(out) :: work(*), z(ldz,*)
     end subroutine stdlib_chbev

     module subroutine stdlib_zhbev( jobz, uplo, n, kd, ab, ldab, w, z, ldz, work,rwork, info )
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kd, ldab, ldz, n
           real(dp), intent(out) :: rwork(*), w(*)
           complex(dp), intent(inout) :: ab(ldab,*)
           complex(dp), intent(out) :: work(*), z(ldz,*)
     end subroutine stdlib_zhbev


end interface 


interface 
     module subroutine stdlib_chbevd( jobz, uplo, n, kd, ab, ldab, w, z, ldz, work,lwork, rwork, lrwork, &
               iwork, liwork, info )
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kd, ldab, ldz, liwork, lrwork, lwork, n
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(out) :: rwork(*), w(*)
           complex(sp), intent(inout) :: ab(ldab,*)
           complex(sp), intent(out) :: work(*), z(ldz,*)
     end subroutine stdlib_chbevd

     module subroutine stdlib_zhbevd( jobz, uplo, n, kd, ab, ldab, w, z, ldz, work,lwork, rwork, lrwork, &
               iwork, liwork, info )
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kd, ldab, ldz, liwork, lrwork, lwork, n
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(out) :: rwork(*), w(*)
           complex(dp), intent(inout) :: ab(ldab,*)
           complex(dp), intent(out) :: work(*), z(ldz,*)
     end subroutine stdlib_zhbevd


end interface 


interface 
     module subroutine stdlib_chbevx( jobz, range, uplo, n, kd, ab, ldab, q, ldq, vl,vu, il, iu, abstol, &
               m, w, z, ldz, work, rwork,iwork, ifail, info )
           character, intent(in) :: jobz, range, uplo
           integer(ilp), intent(in) :: il, iu, kd, ldab, ldq, ldz, n
           integer(ilp), intent(out) :: info, m
           real(sp), intent(in) :: abstol, vl, vu
           integer(ilp), intent(out) :: ifail(*), iwork(*)
           real(sp), intent(out) :: rwork(*), w(*)
           complex(sp), intent(inout) :: ab(ldab,*)
           complex(sp), intent(out) :: q(ldq,*), work(*), z(ldz,*)
     end subroutine stdlib_chbevx

     module subroutine stdlib_zhbevx( jobz, range, uplo, n, kd, ab, ldab, q, ldq, vl,vu, il, iu, abstol, &
               m, w, z, ldz, work, rwork,iwork, ifail, info )
           character, intent(in) :: jobz, range, uplo
           integer(ilp), intent(in) :: il, iu, kd, ldab, ldq, ldz, n
           integer(ilp), intent(out) :: info, m
           real(dp), intent(in) :: abstol, vl, vu
           integer(ilp), intent(out) :: ifail(*), iwork(*)
           real(dp), intent(out) :: rwork(*), w(*)
           complex(dp), intent(inout) :: ab(ldab,*)
           complex(dp), intent(out) :: q(ldq,*), work(*), z(ldz,*)
     end subroutine stdlib_zhbevx


end interface 


interface 
     pure module subroutine stdlib_slasq1( n, d, e, work, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           real(sp), intent(inout) :: d(*), e(*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_slasq1

     pure module subroutine stdlib_dlasq1( n, d, e, work, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           real(dp), intent(inout) :: d(*), e(*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_dlasq1


end interface 


interface 
     pure module subroutine stdlib_slasq2( n, z, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           real(sp), intent(inout) :: z(*)
     end subroutine stdlib_slasq2

     pure module subroutine stdlib_dlasq2( n, z, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           real(dp), intent(inout) :: z(*)
     end subroutine stdlib_dlasq2


end interface 


interface 
     pure module subroutine stdlib_slasq3( i0, n0, z, pp, dmin, sigma, desig, qmax, nfail,iter, ndiv, &
               ieee, ttype, dmin1, dmin2, dn, dn1,dn2, g, tau )
           logical(lk), intent(in) :: ieee
           integer(ilp), intent(in) :: i0
           integer(ilp), intent(inout) :: iter, n0, ndiv, nfail, pp
           real(sp), intent(inout) :: desig, dmin1, dmin2, dn, dn1, dn2, g, qmax, tau
           real(sp), intent(out) :: dmin, sigma
           integer(ilp), intent(inout) :: ttype
           real(sp), intent(inout) :: z(*)
     end subroutine stdlib_slasq3

     pure module subroutine stdlib_dlasq3( i0, n0, z, pp, dmin, sigma, desig, qmax, nfail,iter, ndiv, &
               ieee, ttype, dmin1, dmin2, dn, dn1,dn2, g, tau )
           logical(lk), intent(in) :: ieee
           integer(ilp), intent(in) :: i0
           integer(ilp), intent(inout) :: iter, n0, ndiv, nfail, pp
           real(dp), intent(inout) :: desig, dmin1, dmin2, dn, dn1, dn2, g, qmax, tau
           real(dp), intent(out) :: dmin, sigma
           integer(ilp), intent(inout) :: ttype
           real(dp), intent(inout) :: z(*)
     end subroutine stdlib_dlasq3


end interface 


interface 
     pure module subroutine stdlib_slasq4( i0, n0, z, pp, n0in, dmin, dmin1, dmin2, dn,dn1, dn2, tau, &
               ttype, g )
           integer(ilp), intent(in) :: i0, n0, n0in, pp
           integer(ilp), intent(out) :: ttype
           real(sp), intent(in) :: dmin, dmin1, dmin2, dn, dn1, dn2
           real(sp), intent(inout) :: g
           real(sp), intent(out) :: tau
           real(sp), intent(in) :: z(*)
     end subroutine stdlib_slasq4

     pure module subroutine stdlib_dlasq4( i0, n0, z, pp, n0in, dmin, dmin1, dmin2, dn,dn1, dn2, tau, &
               ttype, g )
           integer(ilp), intent(in) :: i0, n0, n0in, pp
           integer(ilp), intent(out) :: ttype
           real(dp), intent(in) :: dmin, dmin1, dmin2, dn, dn1, dn2
           real(dp), intent(inout) :: g
           real(dp), intent(out) :: tau
           real(dp), intent(in) :: z(*)
     end subroutine stdlib_dlasq4


end interface 


interface 
     pure module subroutine stdlib_slasq5( i0, n0, z, pp, tau, sigma, dmin, dmin1, dmin2,dn, dnm1, dnm2, &
               ieee, eps )
           logical(lk), intent(in) :: ieee
           integer(ilp), intent(in) :: i0, n0, pp
           real(sp), intent(out) :: dmin, dmin1, dmin2, dn, dnm1, dnm2
           real(sp), intent(inout) :: tau
           real(sp), intent(in) :: sigma, eps
           real(sp), intent(inout) :: z(*)
     end subroutine stdlib_slasq5

     pure module subroutine stdlib_dlasq5( i0, n0, z, pp, tau, sigma, dmin, dmin1, dmin2,dn, dnm1, dnm2, &
               ieee, eps )
           logical(lk), intent(in) :: ieee
           integer(ilp), intent(in) :: i0, n0, pp
           real(dp), intent(out) :: dmin, dmin1, dmin2, dn, dnm1, dnm2
           real(dp), intent(inout) :: tau
           real(dp), intent(in) :: sigma, eps
           real(dp), intent(inout) :: z(*)
     end subroutine stdlib_dlasq5


end interface 


interface 
     pure module subroutine stdlib_slasq6( i0, n0, z, pp, dmin, dmin1, dmin2, dn,dnm1, dnm2 )
           integer(ilp), intent(in) :: i0, n0, pp
           real(sp), intent(out) :: dmin, dmin1, dmin2, dn, dnm1, dnm2
           real(sp), intent(inout) :: z(*)
     end subroutine stdlib_slasq6

     pure module subroutine stdlib_dlasq6( i0, n0, z, pp, dmin, dmin1, dmin2, dn,dnm1, dnm2 )
           integer(ilp), intent(in) :: i0, n0, pp
           real(dp), intent(out) :: dmin, dmin1, dmin2, dn, dnm1, dnm2
           real(dp), intent(inout) :: z(*)
     end subroutine stdlib_dlasq6


end interface 


interface 
     pure module subroutine stdlib_sbbcsd( jobu1, jobu2, jobv1t, jobv2t, trans, m, p, q,theta, phi, u1, &
     ldu1, u2, ldu2, v1t, ldv1t,v2t, ldv2t, b11d, b11e, b12d, b12e, b21d, b21e,b22d, b22e, work, &
               lwork, info )
           character, intent(in) :: jobu1, jobu2, jobv1t, jobv2t, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldu1, ldu2, ldv1t, ldv2t, lwork, m, p, q
           real(sp), intent(out) :: b11d(*), b11e(*), b12d(*), b12e(*), b21d(*), b21e(*), b22d(*),&
                      b22e(*), work(*)
           real(sp), intent(inout) :: phi(*), theta(*)
           real(sp), intent(inout) :: u1(ldu1,*), u2(ldu2,*), v1t(ldv1t,*), v2t(ldv2t,*)
     end subroutine stdlib_sbbcsd

     pure module subroutine stdlib_dbbcsd( jobu1, jobu2, jobv1t, jobv2t, trans, m, p, q,theta, phi, u1, &
     ldu1, u2, ldu2, v1t, ldv1t,v2t, ldv2t, b11d, b11e, b12d, b12e, b21d, b21e,b22d, b22e, work, &
               lwork, info )
           character, intent(in) :: jobu1, jobu2, jobv1t, jobv2t, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldu1, ldu2, ldv1t, ldv2t, lwork, m, p, q
           real(dp), intent(out) :: b11d(*), b11e(*), b12d(*), b12e(*), b21d(*), b21e(*), b22d(*),&
                      b22e(*), work(*)
           real(dp), intent(inout) :: phi(*), theta(*)
           real(dp), intent(inout) :: u1(ldu1,*), u2(ldu2,*), v1t(ldv1t,*), v2t(ldv2t,*)
     end subroutine stdlib_dbbcsd


     pure module subroutine stdlib_cbbcsd( jobu1, jobu2, jobv1t, jobv2t, trans, m, p, q,theta, phi, u1, &
     ldu1, u2, ldu2, v1t, ldv1t,v2t, ldv2t, b11d, b11e, b12d, b12e, b21d, b21e,b22d, b22e, rwork, &
               lrwork, info )
     
           character, intent(in) :: jobu1, jobu2, jobv1t, jobv2t, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldu1, ldu2, ldv1t, ldv2t, lrwork, m, p, q
           real(sp), intent(out) :: b11d(*), b11e(*), b12d(*), b12e(*), b21d(*), b21e(*), b22d(*),&
                      b22e(*), rwork(*)
           real(sp), intent(inout) :: phi(*), theta(*)
           complex(sp), intent(inout) :: u1(ldu1,*), u2(ldu2,*), v1t(ldv1t,*), v2t(ldv2t,*)
                     
     end subroutine stdlib_cbbcsd

     pure module subroutine stdlib_zbbcsd( jobu1, jobu2, jobv1t, jobv2t, trans, m, p, q,theta, phi, u1, &
     ldu1, u2, ldu2, v1t, ldv1t,v2t, ldv2t, b11d, b11e, b12d, b12e, b21d, b21e,b22d, b22e, rwork, &
               lrwork, info )
           character, intent(in) :: jobu1, jobu2, jobv1t, jobv2t, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldu1, ldu2, ldv1t, ldv2t, lrwork, m, p, q
           real(dp), intent(out) :: b11d(*), b11e(*), b12d(*), b12e(*), b21d(*), b21e(*), b22d(*),&
                      b22e(*), rwork(*)
           real(dp), intent(inout) :: phi(*), theta(*)
           complex(dp), intent(inout) :: u1(ldu1,*), u2(ldu2,*), v1t(ldv1t,*), v2t(ldv2t,*)
                     
     end subroutine stdlib_zbbcsd


end interface 


interface 
     recursive module subroutine stdlib_cuncsd( jobu1, jobu2, jobv1t, jobv2t, trans,signs, m, p, q, x11, &
     ldx11, x12,ldx12, x21, ldx21, x22, ldx22, theta,u1, ldu1, u2, ldu2, v1t, ldv1t, v2t,ldv2t, &
               work, lwork, rwork, lrwork,iwork, info )
           character, intent(in) :: jobu1, jobu2, jobv1t, jobv2t, signs, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldu1, ldu2, ldv1t, ldv2t, ldx11, ldx12, ldx21, ldx22, &
                     lrwork, lwork, m, p, q
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(out) :: theta(*)
           real(sp), intent(out) :: rwork(*)
           complex(sp), intent(out) :: u1(ldu1,*), u2(ldu2,*), v1t(ldv1t,*), v2t(ldv2t,*), work(*)
                     
           complex(sp), intent(inout) :: x11(ldx11,*), x12(ldx12,*), x21(ldx21,*), x22(ldx22,*)
                     
     end subroutine stdlib_cuncsd

     recursive module subroutine stdlib_zuncsd( jobu1, jobu2, jobv1t, jobv2t, trans,signs, m, p, q, x11, &
     ldx11, x12,ldx12, x21, ldx21, x22, ldx22, theta,u1, ldu1, u2, ldu2, v1t, ldv1t, v2t,ldv2t, &
               work, lwork, rwork, lrwork,iwork, info )
           character, intent(in) :: jobu1, jobu2, jobv1t, jobv2t, signs, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldu1, ldu2, ldv1t, ldv2t, ldx11, ldx12, ldx21, ldx22, &
                     lrwork, lwork, m, p, q
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(out) :: theta(*)
           real(dp), intent(out) :: rwork(*)
           complex(dp), intent(out) :: u1(ldu1,*), u2(ldu2,*), v1t(ldv1t,*), v2t(ldv2t,*), work(*)
                     
           complex(dp), intent(inout) :: x11(ldx11,*), x12(ldx12,*), x21(ldx21,*), x22(ldx22,*)
                     
     end subroutine stdlib_zuncsd


end interface 


interface 
     module subroutine stdlib_cuncsd2by1( jobu1, jobu2, jobv1t, m, p, q, x11, ldx11,x21, ldx21, theta, &
               u1, ldu1, u2, ldu2, v1t,ldv1t, work, lwork, rwork, lrwork, iwork,info )
           character, intent(in) :: jobu1, jobu2, jobv1t
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldu1, ldu2, ldv1t, lwork, ldx11, ldx21, m, p, q
           integer(ilp), intent(in) :: lrwork
           integer(ilp) :: lrworkmin, lrworkopt
           real(sp), intent(out) :: rwork(*)
           real(sp), intent(out) :: theta(*)
           complex(sp), intent(out) :: u1(ldu1,*), u2(ldu2,*), v1t(ldv1t,*), work(*)
           complex(sp), intent(inout) :: x11(ldx11,*), x21(ldx21,*)
           integer(ilp), intent(out) :: iwork(*)
     end subroutine stdlib_cuncsd2by1

     module subroutine stdlib_zuncsd2by1( jobu1, jobu2, jobv1t, m, p, q, x11, ldx11,x21, ldx21, theta, &
               u1, ldu1, u2, ldu2, v1t,ldv1t, work, lwork, rwork, lrwork, iwork,info )
           character, intent(in) :: jobu1, jobu2, jobv1t
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldu1, ldu2, ldv1t, lwork, ldx11, ldx21, m, p, q
           integer(ilp), intent(in) :: lrwork
           integer(ilp) :: lrworkmin, lrworkopt
           real(dp), intent(out) :: rwork(*)
           real(dp), intent(out) :: theta(*)
           complex(dp), intent(out) :: u1(ldu1,*), u2(ldu2,*), v1t(ldv1t,*), work(*)
           complex(dp), intent(inout) :: x11(ldx11,*), x21(ldx21,*)
           integer(ilp), intent(out) :: iwork(*)
     end subroutine stdlib_zuncsd2by1


end interface 


interface 
     module subroutine stdlib_cunbdb( trans, signs, m, p, q, x11, ldx11, x12, ldx12,x21, ldx21, x22, &
               ldx22, theta, phi, taup1,taup2, tauq1, tauq2, work, lwork, info )
           character, intent(in) :: signs, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldx11, ldx12, ldx21, ldx22, lwork, m, p, q
           real(sp), intent(out) :: phi(*), theta(*)
           complex(sp), intent(out) :: taup1(*), taup2(*), tauq1(*), tauq2(*), work(*)
           complex(sp), intent(inout) :: x11(ldx11,*), x12(ldx12,*), x21(ldx21,*), x22(ldx22,*)
                     
     end subroutine stdlib_cunbdb

     module subroutine stdlib_zunbdb( trans, signs, m, p, q, x11, ldx11, x12, ldx12,x21, ldx21, x22, &
               ldx22, theta, phi, taup1,taup2, tauq1, tauq2, work, lwork, info )
           character, intent(in) :: signs, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldx11, ldx12, ldx21, ldx22, lwork, m, p, q
           real(dp), intent(out) :: phi(*), theta(*)
           complex(dp), intent(out) :: taup1(*), taup2(*), tauq1(*), tauq2(*), work(*)
           complex(dp), intent(inout) :: x11(ldx11,*), x12(ldx12,*), x21(ldx21,*), x22(ldx22,*)
                     
     end subroutine stdlib_zunbdb


end interface 


interface 
     module subroutine stdlib_cunbdb1( m, p, q, x11, ldx11, x21, ldx21, theta, phi,taup1, taup2, tauq1, &
               work, lwork, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lwork, m, p, q, ldx11, ldx21
           real(sp), intent(out) :: phi(*), theta(*)
           complex(sp), intent(out) :: taup1(*), taup2(*), tauq1(*), work(*)
           complex(sp), intent(inout) :: x11(ldx11,*), x21(ldx21,*)
     end subroutine stdlib_cunbdb1

     module subroutine stdlib_zunbdb1( m, p, q, x11, ldx11, x21, ldx21, theta, phi,taup1, taup2, tauq1, &
               work, lwork, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lwork, m, p, q, ldx11, ldx21
           real(dp), intent(out) :: phi(*), theta(*)
           complex(dp), intent(out) :: taup1(*), taup2(*), tauq1(*), work(*)
           complex(dp), intent(inout) :: x11(ldx11,*), x21(ldx21,*)
     end subroutine stdlib_zunbdb1


end interface 


interface 
     module subroutine stdlib_cunbdb2( m, p, q, x11, ldx11, x21, ldx21, theta, phi,taup1, taup2, tauq1, &
               work, lwork, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lwork, m, p, q, ldx11, ldx21
           real(sp), intent(out) :: phi(*), theta(*)
           complex(sp), intent(out) :: taup1(*), taup2(*), tauq1(*), work(*)
           complex(sp), intent(inout) :: x11(ldx11,*), x21(ldx21,*)
     end subroutine stdlib_cunbdb2

     module subroutine stdlib_zunbdb2( m, p, q, x11, ldx11, x21, ldx21, theta, phi,taup1, taup2, tauq1, &
               work, lwork, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lwork, m, p, q, ldx11, ldx21
           real(dp), intent(out) :: phi(*), theta(*)
           complex(dp), intent(out) :: taup1(*), taup2(*), tauq1(*), work(*)
           complex(dp), intent(inout) :: x11(ldx11,*), x21(ldx21,*)
     end subroutine stdlib_zunbdb2


end interface 


interface 
     module subroutine stdlib_cunbdb3( m, p, q, x11, ldx11, x21, ldx21, theta, phi,taup1, taup2, tauq1, &
               work, lwork, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lwork, m, p, q, ldx11, ldx21
           real(sp), intent(out) :: phi(*), theta(*)
           complex(sp), intent(out) :: taup1(*), taup2(*), tauq1(*), work(*)
           complex(sp), intent(inout) :: x11(ldx11,*), x21(ldx21,*)
     end subroutine stdlib_cunbdb3

     module subroutine stdlib_zunbdb3( m, p, q, x11, ldx11, x21, ldx21, theta, phi,taup1, taup2, tauq1, &
               work, lwork, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lwork, m, p, q, ldx11, ldx21
           real(dp), intent(out) :: phi(*), theta(*)
           complex(dp), intent(out) :: taup1(*), taup2(*), tauq1(*), work(*)
           complex(dp), intent(inout) :: x11(ldx11,*), x21(ldx21,*)
     end subroutine stdlib_zunbdb3


end interface 


interface 
     module subroutine stdlib_cunbdb4( m, p, q, x11, ldx11, x21, ldx21, theta, phi,taup1, taup2, tauq1, &
               phantom, work, lwork,info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lwork, m, p, q, ldx11, ldx21
           real(sp), intent(out) :: phi(*), theta(*)
           complex(sp), intent(out) :: phantom(*), taup1(*), taup2(*), tauq1(*), work(*)
           complex(sp), intent(inout) :: x11(ldx11,*), x21(ldx21,*)
     end subroutine stdlib_cunbdb4

     module subroutine stdlib_zunbdb4( m, p, q, x11, ldx11, x21, ldx21, theta, phi,taup1, taup2, tauq1, &
               phantom, work, lwork,info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lwork, m, p, q, ldx11, ldx21
           real(dp), intent(out) :: phi(*), theta(*)
           complex(dp), intent(out) :: phantom(*), taup1(*), taup2(*), tauq1(*), work(*)
           complex(dp), intent(inout) :: x11(ldx11,*), x21(ldx21,*)
     end subroutine stdlib_zunbdb4


end interface 


interface 
     pure module subroutine stdlib_cunbdb5( m1, m2, n, x1, incx1, x2, incx2, q1, ldq1, q2,ldq2, work, &
               lwork, info )
           integer(ilp), intent(in) :: incx1, incx2, ldq1, ldq2, lwork, m1, m2, n
           integer(ilp), intent(out) :: info
           complex(sp), intent(in) :: q1(ldq1,*), q2(ldq2,*)
           complex(sp), intent(out) :: work(*)
           complex(sp), intent(inout) :: x1(*), x2(*)
     end subroutine stdlib_cunbdb5

     pure module subroutine stdlib_zunbdb5( m1, m2, n, x1, incx1, x2, incx2, q1, ldq1, q2,ldq2, work, &
               lwork, info )
           integer(ilp), intent(in) :: incx1, incx2, ldq1, ldq2, lwork, m1, m2, n
           integer(ilp), intent(out) :: info
           complex(dp), intent(in) :: q1(ldq1,*), q2(ldq2,*)
           complex(dp), intent(out) :: work(*)
           complex(dp), intent(inout) :: x1(*), x2(*)
     end subroutine stdlib_zunbdb5


end interface 


interface 
     pure module subroutine stdlib_cunbdb6( m1, m2, n, x1, incx1, x2, incx2, q1, ldq1, q2,ldq2, work, lwork, info )
               
           integer(ilp), intent(in) :: incx1, incx2, ldq1, ldq2, lwork, m1, m2, n
           integer(ilp), intent(out) :: info
           complex(sp), intent(in) :: q1(ldq1,*), q2(ldq2,*)
           complex(sp), intent(out) :: work(*)
           complex(sp), intent(inout) :: x1(*), x2(*)
     end subroutine stdlib_cunbdb6

     pure module subroutine stdlib_zunbdb6( m1, m2, n, x1, incx1, x2, incx2, q1, ldq1, q2,ldq2, work, &
               lwork, info )
           integer(ilp), intent(in) :: incx1, incx2, ldq1, ldq2, lwork, m1, m2, n
           integer(ilp), intent(out) :: info
           complex(dp), intent(in) :: q1(ldq1,*), q2(ldq2,*)
           complex(dp), intent(out) :: work(*)
           complex(dp), intent(inout) :: x1(*), x2(*)
     end subroutine stdlib_zunbdb6


end interface 


interface 
     recursive module subroutine stdlib_sorcsd( jobu1, jobu2, jobv1t, jobv2t, trans,signs, m, p, q, x11, &
     ldx11, x12,ldx12, x21, ldx21, x22, ldx22, theta,u1, ldu1, u2, ldu2, v1t, ldv1t, v2t,ldv2t, &
               work, lwork, iwork, info )
           character, intent(in) :: jobu1, jobu2, jobv1t, jobv2t, signs, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldu1, ldu2, ldv1t, ldv2t, ldx11, ldx12, ldx21, ldx22, &
                     lwork, m, p, q
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(out) :: theta(*)
           real(sp), intent(out) :: u1(ldu1,*), u2(ldu2,*), v1t(ldv1t,*), v2t(ldv2t,*), work(*)
                     
           real(sp), intent(inout) :: x11(ldx11,*), x12(ldx12,*), x21(ldx21,*), x22(ldx22,*)
                     
     end subroutine stdlib_sorcsd

     recursive module subroutine stdlib_dorcsd( jobu1, jobu2, jobv1t, jobv2t, trans,signs, m, p, q, x11, &
     ldx11, x12,ldx12, x21, ldx21, x22, ldx22, theta,u1, ldu1, u2, ldu2, v1t, ldv1t, v2t,ldv2t, &
               work, lwork, iwork, info )
           character, intent(in) :: jobu1, jobu2, jobv1t, jobv2t, signs, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldu1, ldu2, ldv1t, ldv2t, ldx11, ldx12, ldx21, ldx22, &
                     lwork, m, p, q
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(out) :: theta(*)
           real(dp), intent(out) :: u1(ldu1,*), u2(ldu2,*), v1t(ldv1t,*), v2t(ldv2t,*), work(*)
                     
           real(dp), intent(inout) :: x11(ldx11,*), x12(ldx12,*), x21(ldx21,*), x22(ldx22,*)
                     
     end subroutine stdlib_dorcsd


end interface 


interface 
     module subroutine stdlib_sorcsd2by1( jobu1, jobu2, jobv1t, m, p, q, x11, ldx11,x21, ldx21, theta, &
               u1, ldu1, u2, ldu2, v1t,ldv1t, work, lwork, iwork, info )
           character, intent(in) :: jobu1, jobu2, jobv1t
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldu1, ldu2, ldv1t, lwork, ldx11, ldx21, m, p, q
           real(sp), intent(out) :: theta(*)
           real(sp), intent(out) :: u1(ldu1,*), u2(ldu2,*), v1t(ldv1t,*), work(*)
           real(sp), intent(inout) :: x11(ldx11,*), x21(ldx21,*)
           integer(ilp), intent(out) :: iwork(*)
     end subroutine stdlib_sorcsd2by1

     module subroutine stdlib_dorcsd2by1( jobu1, jobu2, jobv1t, m, p, q, x11, ldx11,x21, ldx21, theta, &
               u1, ldu1, u2, ldu2, v1t,ldv1t, work, lwork, iwork, info )
           character, intent(in) :: jobu1, jobu2, jobv1t
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldu1, ldu2, ldv1t, lwork, ldx11, ldx21, m, p, q
           real(dp), intent(out) :: theta(*)
           real(dp), intent(out) :: u1(ldu1,*), u2(ldu2,*), v1t(ldv1t,*), work(*)
           real(dp), intent(inout) :: x11(ldx11,*), x21(ldx21,*)
           integer(ilp), intent(out) :: iwork(*)
     end subroutine stdlib_dorcsd2by1


end interface 


interface 
     module subroutine stdlib_sorbdb( trans, signs, m, p, q, x11, ldx11, x12, ldx12,x21, ldx21, x22, &
               ldx22, theta, phi, taup1,taup2, tauq1, tauq2, work, lwork, info )
           character, intent(in) :: signs, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldx11, ldx12, ldx21, ldx22, lwork, m, p, q
           real(sp), intent(out) :: phi(*), theta(*)
           real(sp), intent(out) :: taup1(*), taup2(*), tauq1(*), tauq2(*), work(*)
           real(sp), intent(inout) :: x11(ldx11,*), x12(ldx12,*), x21(ldx21,*), x22(ldx22,*)
                     
     end subroutine stdlib_sorbdb

     module subroutine stdlib_dorbdb( trans, signs, m, p, q, x11, ldx11, x12, ldx12,x21, ldx21, x22, &
               ldx22, theta, phi, taup1,taup2, tauq1, tauq2, work, lwork, info )
           character, intent(in) :: signs, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ldx11, ldx12, ldx21, ldx22, lwork, m, p, q
           real(dp), intent(out) :: phi(*), theta(*)
           real(dp), intent(out) :: taup1(*), taup2(*), tauq1(*), tauq2(*), work(*)
           real(dp), intent(inout) :: x11(ldx11,*), x12(ldx12,*), x21(ldx21,*), x22(ldx22,*)
                     
     end subroutine stdlib_dorbdb


end interface 


interface 
     module subroutine stdlib_sorbdb1( m, p, q, x11, ldx11, x21, ldx21, theta, phi,taup1, taup2, tauq1, &
               work, lwork, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lwork, m, p, q, ldx11, ldx21
           real(sp), intent(out) :: phi(*), theta(*)
           real(sp), intent(out) :: taup1(*), taup2(*), tauq1(*), work(*)
           real(sp), intent(inout) :: x11(ldx11,*), x21(ldx21,*)
     end subroutine stdlib_sorbdb1

     module subroutine stdlib_dorbdb1( m, p, q, x11, ldx11, x21, ldx21, theta, phi,taup1, taup2, tauq1, &
               work, lwork, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lwork, m, p, q, ldx11, ldx21
           real(dp), intent(out) :: phi(*), theta(*)
           real(dp), intent(out) :: taup1(*), taup2(*), tauq1(*), work(*)
           real(dp), intent(inout) :: x11(ldx11,*), x21(ldx21,*)
     end subroutine stdlib_dorbdb1


end interface 


interface 
     module subroutine stdlib_sorbdb2( m, p, q, x11, ldx11, x21, ldx21, theta, phi,taup1, taup2, tauq1, &
               work, lwork, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lwork, m, p, q, ldx11, ldx21
           real(sp), intent(out) :: phi(*), theta(*)
           real(sp), intent(out) :: taup1(*), taup2(*), tauq1(*), work(*)
           real(sp), intent(inout) :: x11(ldx11,*), x21(ldx21,*)
     end subroutine stdlib_sorbdb2

     module subroutine stdlib_dorbdb2( m, p, q, x11, ldx11, x21, ldx21, theta, phi,taup1, taup2, tauq1, &
               work, lwork, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lwork, m, p, q, ldx11, ldx21
           real(dp), intent(out) :: phi(*), theta(*)
           real(dp), intent(out) :: taup1(*), taup2(*), tauq1(*), work(*)
           real(dp), intent(inout) :: x11(ldx11,*), x21(ldx21,*)
     end subroutine stdlib_dorbdb2


end interface 


interface 
     module subroutine stdlib_sorbdb3( m, p, q, x11, ldx11, x21, ldx21, theta, phi,taup1, taup2, tauq1, &
               work, lwork, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lwork, m, p, q, ldx11, ldx21
           real(sp), intent(out) :: phi(*), theta(*)
           real(sp), intent(out) :: taup1(*), taup2(*), tauq1(*), work(*)
           real(sp), intent(inout) :: x11(ldx11,*), x21(ldx21,*)
     end subroutine stdlib_sorbdb3

     module subroutine stdlib_dorbdb3( m, p, q, x11, ldx11, x21, ldx21, theta, phi,taup1, taup2, tauq1, &
               work, lwork, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lwork, m, p, q, ldx11, ldx21
           real(dp), intent(out) :: phi(*), theta(*)
           real(dp), intent(out) :: taup1(*), taup2(*), tauq1(*), work(*)
           real(dp), intent(inout) :: x11(ldx11,*), x21(ldx21,*)
     end subroutine stdlib_dorbdb3


end interface 


interface 
     module subroutine stdlib_sorbdb4( m, p, q, x11, ldx11, x21, ldx21, theta, phi,taup1, taup2, tauq1, &
               phantom, work, lwork,info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lwork, m, p, q, ldx11, ldx21
           real(sp), intent(out) :: phi(*), theta(*)
           real(sp), intent(out) :: phantom(*), taup1(*), taup2(*), tauq1(*), work(*)
           real(sp), intent(inout) :: x11(ldx11,*), x21(ldx21,*)
     end subroutine stdlib_sorbdb4

     module subroutine stdlib_dorbdb4( m, p, q, x11, ldx11, x21, ldx21, theta, phi,taup1, taup2, tauq1, &
               phantom, work, lwork,info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lwork, m, p, q, ldx11, ldx21
           real(dp), intent(out) :: phi(*), theta(*)
           real(dp), intent(out) :: phantom(*), taup1(*), taup2(*), tauq1(*), work(*)
           real(dp), intent(inout) :: x11(ldx11,*), x21(ldx21,*)
     end subroutine stdlib_dorbdb4


end interface 


interface 
     pure module subroutine stdlib_sorbdb5( m1, m2, n, x1, incx1, x2, incx2, q1, ldq1, q2,ldq2, work, &
               lwork, info )
           integer(ilp), intent(in) :: incx1, incx2, ldq1, ldq2, lwork, m1, m2, n
           integer(ilp), intent(out) :: info
           real(sp), intent(in) :: q1(ldq1,*), q2(ldq2,*)
           real(sp), intent(out) :: work(*)
           real(sp), intent(inout) :: x1(*), x2(*)
     end subroutine stdlib_sorbdb5

     pure module subroutine stdlib_dorbdb5( m1, m2, n, x1, incx1, x2, incx2, q1, ldq1, q2,ldq2, work, &
               lwork, info )
           integer(ilp), intent(in) :: incx1, incx2, ldq1, ldq2, lwork, m1, m2, n
           integer(ilp), intent(out) :: info
           real(dp), intent(in) :: q1(ldq1,*), q2(ldq2,*)
           real(dp), intent(out) :: work(*)
           real(dp), intent(inout) :: x1(*), x2(*)
     end subroutine stdlib_dorbdb5


end interface 


interface 
     pure module subroutine stdlib_sorbdb6( m1, m2, n, x1, incx1, x2, incx2, q1, ldq1, q2,ldq2, work, &
               lwork, info )
           integer(ilp), intent(in) :: incx1, incx2, ldq1, ldq2, lwork, m1, m2, n
           integer(ilp), intent(out) :: info
           real(sp), intent(in) :: q1(ldq1,*), q2(ldq2,*)
           real(sp), intent(out) :: work(*)
           real(sp), intent(inout) :: x1(*), x2(*)
     end subroutine stdlib_sorbdb6

     pure module subroutine stdlib_dorbdb6( m1, m2, n, x1, incx1, x2, incx2, q1, ldq1, q2,ldq2, work, &
               lwork, info )
           integer(ilp), intent(in) :: incx1, incx2, ldq1, ldq2, lwork, m1, m2, n
           integer(ilp), intent(out) :: info
           real(dp), intent(in) :: q1(ldq1,*), q2(ldq2,*)
           real(dp), intent(out) :: work(*)
           real(dp), intent(inout) :: x1(*), x2(*)
     end subroutine stdlib_dorbdb6


end interface 


interface 
     pure module subroutine stdlib_slapmr( forwrd, m, n, x, ldx, k )
           logical(lk), intent(in) :: forwrd
           integer(ilp), intent(in) :: ldx, m, n
           integer(ilp), intent(inout) :: k(*)
           real(sp), intent(inout) :: x(ldx,*)
     end subroutine stdlib_slapmr

     pure module subroutine stdlib_dlapmr( forwrd, m, n, x, ldx, k )
           logical(lk), intent(in) :: forwrd
           integer(ilp), intent(in) :: ldx, m, n
           integer(ilp), intent(inout) :: k(*)
           real(dp), intent(inout) :: x(ldx,*)
     end subroutine stdlib_dlapmr


     pure module subroutine stdlib_clapmr( forwrd, m, n, x, ldx, k )
           logical(lk), intent(in) :: forwrd
           integer(ilp), intent(in) :: ldx, m, n
           integer(ilp), intent(inout) :: k(*)
           complex(sp), intent(inout) :: x(ldx,*)
     end subroutine stdlib_clapmr

     pure module subroutine stdlib_zlapmr( forwrd, m, n, x, ldx, k )
           logical(lk), intent(in) :: forwrd
           integer(ilp), intent(in) :: ldx, m, n
           integer(ilp), intent(inout) :: k(*)
           complex(dp), intent(inout) :: x(ldx,*)
     end subroutine stdlib_zlapmr


end interface 


interface 
     pure module subroutine stdlib_slapmt( forwrd, m, n, x, ldx, k )
           logical(lk), intent(in) :: forwrd
           integer(ilp), intent(in) :: ldx, m, n
           integer(ilp), intent(inout) :: k(*)
           real(sp), intent(inout) :: x(ldx,*)
     end subroutine stdlib_slapmt

     pure module subroutine stdlib_dlapmt( forwrd, m, n, x, ldx, k )
           logical(lk), intent(in) :: forwrd
           integer(ilp), intent(in) :: ldx, m, n
           integer(ilp), intent(inout) :: k(*)
           real(dp), intent(inout) :: x(ldx,*)
     end subroutine stdlib_dlapmt


     pure module subroutine stdlib_clapmt( forwrd, m, n, x, ldx, k )
           logical(lk), intent(in) :: forwrd
           integer(ilp), intent(in) :: ldx, m, n
           integer(ilp), intent(inout) :: k(*)
           complex(sp), intent(inout) :: x(ldx,*)
     end subroutine stdlib_clapmt

     pure module subroutine stdlib_zlapmt( forwrd, m, n, x, ldx, k )
           logical(lk), intent(in) :: forwrd
           integer(ilp), intent(in) :: ldx, m, n
           integer(ilp), intent(inout) :: k(*)
           complex(dp), intent(inout) :: x(ldx,*)
     end subroutine stdlib_zlapmt


end interface 


interface 
     module subroutine stdlib_ssygv( itype, jobz, uplo, n, a, lda, b, ldb, w, work,lwork, info )
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: itype, lda, ldb, lwork, n
           real(sp), intent(inout) :: a(lda,*), b(ldb,*)
           real(sp), intent(out) :: w(*), work(*)
     end subroutine stdlib_ssygv

     module subroutine stdlib_dsygv( itype, jobz, uplo, n, a, lda, b, ldb, w, work,lwork, info )
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: itype, lda, ldb, lwork, n
           real(dp), intent(inout) :: a(lda,*), b(ldb,*)
           real(dp), intent(out) :: w(*), work(*)
     end subroutine stdlib_dsygv


end interface 


interface 
     module subroutine stdlib_ssygvd( itype, jobz, uplo, n, a, lda, b, ldb, w, work,lwork, iwork, liwork,&
                info )
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: itype, lda, ldb, liwork, lwork, n
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(inout) :: a(lda,*), b(ldb,*)
           real(sp), intent(out) :: w(*), work(*)
     end subroutine stdlib_ssygvd

     module subroutine stdlib_dsygvd( itype, jobz, uplo, n, a, lda, b, ldb, w, work,lwork, iwork, liwork,&
                info )
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: itype, lda, ldb, liwork, lwork, n
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(inout) :: a(lda,*), b(ldb,*)
           real(dp), intent(out) :: w(*), work(*)
     end subroutine stdlib_dsygvd


end interface 


interface 
     module subroutine stdlib_ssygvx( itype, jobz, range, uplo, n, a, lda, b, ldb,vl, vu, il, iu, abstol,&
                m, w, z, ldz, work,lwork, iwork, ifail, info )
           character, intent(in) :: jobz, range, uplo
           integer(ilp), intent(in) :: il, itype, iu, lda, ldb, ldz, lwork, n
           integer(ilp), intent(out) :: info, m
           real(sp), intent(in) :: abstol, vl, vu
           integer(ilp), intent(out) :: ifail(*), iwork(*)
           real(sp), intent(inout) :: a(lda,*), b(ldb,*)
           real(sp), intent(out) :: w(*), work(*), z(ldz,*)
     end subroutine stdlib_ssygvx

     module subroutine stdlib_dsygvx( itype, jobz, range, uplo, n, a, lda, b, ldb,vl, vu, il, iu, abstol,&
                m, w, z, ldz, work,lwork, iwork, ifail, info )
           character, intent(in) :: jobz, range, uplo
           integer(ilp), intent(in) :: il, itype, iu, lda, ldb, ldz, lwork, n
           integer(ilp), intent(out) :: info, m
           real(dp), intent(in) :: abstol, vl, vu
           integer(ilp), intent(out) :: ifail(*), iwork(*)
           real(dp), intent(inout) :: a(lda,*), b(ldb,*)
           real(dp), intent(out) :: w(*), work(*), z(ldz,*)
     end subroutine stdlib_dsygvx


end interface 


interface 
     module subroutine stdlib_sspgv( itype, jobz, uplo, n, ap, bp, w, z, ldz, work,info )
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: itype, ldz, n
           real(sp), intent(inout) :: ap(*), bp(*)
           real(sp), intent(out) :: w(*), work(*), z(ldz,*)
     end subroutine stdlib_sspgv

     module subroutine stdlib_dspgv( itype, jobz, uplo, n, ap, bp, w, z, ldz, work,info )
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: itype, ldz, n
           real(dp), intent(inout) :: ap(*), bp(*)
           real(dp), intent(out) :: w(*), work(*), z(ldz,*)
     end subroutine stdlib_dspgv


end interface 


interface 
     module subroutine stdlib_sspgvd( itype, jobz, uplo, n, ap, bp, w, z, ldz, work,lwork, iwork, liwork,&
                info )
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: itype, ldz, liwork, lwork, n
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(inout) :: ap(*), bp(*)
           real(sp), intent(out) :: w(*), work(*), z(ldz,*)
     end subroutine stdlib_sspgvd

     module subroutine stdlib_dspgvd( itype, jobz, uplo, n, ap, bp, w, z, ldz, work,lwork, iwork, liwork,&
                info )
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: itype, ldz, liwork, lwork, n
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(inout) :: ap(*), bp(*)
           real(dp), intent(out) :: w(*), work(*), z(ldz,*)
     end subroutine stdlib_dspgvd


end interface 


interface 
     module subroutine stdlib_sspgvx( itype, jobz, range, uplo, n, ap, bp, vl, vu,il, iu, abstol, m, w, &
               z, ldz, work, iwork,ifail, info )
           character, intent(in) :: jobz, range, uplo
           integer(ilp), intent(in) :: il, itype, iu, ldz, n
           integer(ilp), intent(out) :: info, m
           real(sp), intent(in) :: abstol, vl, vu
           integer(ilp), intent(out) :: ifail(*), iwork(*)
           real(sp), intent(inout) :: ap(*), bp(*)
           real(sp), intent(out) :: w(*), work(*), z(ldz,*)
     end subroutine stdlib_sspgvx

     module subroutine stdlib_dspgvx( itype, jobz, range, uplo, n, ap, bp, vl, vu,il, iu, abstol, m, w, &
               z, ldz, work, iwork,ifail, info )
           character, intent(in) :: jobz, range, uplo
           integer(ilp), intent(in) :: il, itype, iu, ldz, n
           integer(ilp), intent(out) :: info, m
           real(dp), intent(in) :: abstol, vl, vu
           integer(ilp), intent(out) :: ifail(*), iwork(*)
           real(dp), intent(inout) :: ap(*), bp(*)
           real(dp), intent(out) :: w(*), work(*), z(ldz,*)
     end subroutine stdlib_dspgvx


end interface 


interface 
     pure module subroutine stdlib_ssbgv( jobz, uplo, n, ka, kb, ab, ldab, bb, ldbb, w, z,ldz, work, &
               info )
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ka, kb, ldab, ldbb, ldz, n
           real(sp), intent(inout) :: ab(ldab,*), bb(ldbb,*)
           real(sp), intent(out) :: w(*), work(*), z(ldz,*)
     end subroutine stdlib_ssbgv

     pure module subroutine stdlib_dsbgv( jobz, uplo, n, ka, kb, ab, ldab, bb, ldbb, w, z,ldz, work, &
               info )
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ka, kb, ldab, ldbb, ldz, n
           real(dp), intent(inout) :: ab(ldab,*), bb(ldbb,*)
           real(dp), intent(out) :: w(*), work(*), z(ldz,*)
     end subroutine stdlib_dsbgv


end interface 


interface 
     pure module subroutine stdlib_ssbgvd( jobz, uplo, n, ka, kb, ab, ldab, bb, ldbb, w,z, ldz, work, &
               lwork, iwork, liwork, info )
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ka, kb, ldab, ldbb, ldz, liwork, lwork, n
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(inout) :: ab(ldab,*), bb(ldbb,*)
           real(sp), intent(out) :: w(*), work(*), z(ldz,*)
     end subroutine stdlib_ssbgvd

     pure module subroutine stdlib_dsbgvd( jobz, uplo, n, ka, kb, ab, ldab, bb, ldbb, w,z, ldz, work, &
               lwork, iwork, liwork, info )
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ka, kb, ldab, ldbb, ldz, liwork, lwork, n
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(inout) :: ab(ldab,*), bb(ldbb,*)
           real(dp), intent(out) :: w(*), work(*), z(ldz,*)
     end subroutine stdlib_dsbgvd


end interface 


interface 
     pure module subroutine stdlib_ssbgvx( jobz, range, uplo, n, ka, kb, ab, ldab, bb,ldbb, q, ldq, vl, &
               vu, il, iu, abstol, m, w, z,ldz, work, iwork, ifail, info )
           character, intent(in) :: jobz, range, uplo
           integer(ilp), intent(in) :: il, iu, ka, kb, ldab, ldbb, ldq, ldz, n
           integer(ilp), intent(out) :: info, m
           real(sp), intent(in) :: abstol, vl, vu
           integer(ilp), intent(out) :: ifail(*), iwork(*)
           real(sp), intent(inout) :: ab(ldab,*), bb(ldbb,*)
           real(sp), intent(out) :: q(ldq,*), w(*), work(*), z(ldz,*)
     end subroutine stdlib_ssbgvx

     pure module subroutine stdlib_dsbgvx( jobz, range, uplo, n, ka, kb, ab, ldab, bb,ldbb, q, ldq, vl, &
               vu, il, iu, abstol, m, w, z,ldz, work, iwork, ifail, info )
           character, intent(in) :: jobz, range, uplo
           integer(ilp), intent(in) :: il, iu, ka, kb, ldab, ldbb, ldq, ldz, n
           integer(ilp), intent(out) :: info, m
           real(dp), intent(in) :: abstol, vl, vu
           integer(ilp), intent(out) :: ifail(*), iwork(*)
           real(dp), intent(inout) :: ab(ldab,*), bb(ldbb,*)
           real(dp), intent(out) :: q(ldq,*), w(*), work(*), z(ldz,*)
     end subroutine stdlib_dsbgvx


end interface 


interface 
     pure module subroutine stdlib_ssytrd( uplo, n, a, lda, d, e, tau, work, lwork, info )
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, n
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: d(*), e(*), tau(*), work(*)
     end subroutine stdlib_ssytrd

     pure module subroutine stdlib_dsytrd( uplo, n, a, lda, d, e, tau, work, lwork, info )
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, n
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: d(*), e(*), tau(*), work(*)
     end subroutine stdlib_dsytrd


end interface 


interface 
     pure module subroutine stdlib_ssytd2( uplo, n, a, lda, d, e, tau, info )
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: d(*), e(*), tau(*)
     end subroutine stdlib_ssytd2

     pure module subroutine stdlib_dsytd2( uplo, n, a, lda, d, e, tau, info )
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: d(*), e(*), tau(*)
     end subroutine stdlib_dsytd2


end interface 


interface 
     pure module subroutine stdlib_sorgtr( uplo, n, a, lda, tau, work, lwork, info )
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, n
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(in) :: tau(*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_sorgtr

     pure module subroutine stdlib_dorgtr( uplo, n, a, lda, tau, work, lwork, info )
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, n
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(in) :: tau(*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_dorgtr


end interface 


interface 
     pure module subroutine stdlib_sormtr( side, uplo, trans, m, n, a, lda, tau, c, ldc,work, lwork, &
               info )
           character, intent(in) :: side, trans, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldc, lwork, m, n
           real(sp), intent(inout) :: a(lda,*), c(ldc,*)
           real(sp), intent(in) :: tau(*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_sormtr

     pure module subroutine stdlib_dormtr( side, uplo, trans, m, n, a, lda, tau, c, ldc,work, lwork, &
               info )
           character, intent(in) :: side, trans, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldc, lwork, m, n
           real(dp), intent(inout) :: a(lda,*), c(ldc,*)
           real(dp), intent(in) :: tau(*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_dormtr


end interface 


interface 
     pure module subroutine stdlib_ssb2st_kernels( uplo, wantz, ttype,st, ed, sweep, n, nb, ib,a, lda, &
               v, tau, ldvt, work)
           character, intent(in) :: uplo
           logical(lk), intent(in) :: wantz
           integer(ilp), intent(in) :: ttype, st, ed, sweep, n, nb, ib, lda, ldvt
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: v(*), tau(*), work(*)
     end subroutine stdlib_ssb2st_kernels

     pure module subroutine stdlib_dsb2st_kernels( uplo, wantz, ttype,st, ed, sweep, n, nb, ib,a, lda, &
               v, tau, ldvt, work)
           character, intent(in) :: uplo
           logical(lk), intent(in) :: wantz
           integer(ilp), intent(in) :: ttype, st, ed, sweep, n, nb, ib, lda, ldvt
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: v(*), tau(*), work(*)
     end subroutine stdlib_dsb2st_kernels


end interface 


interface 
     module subroutine stdlib_chegv( itype, jobz, uplo, n, a, lda, b, ldb, w, work,lwork, rwork, info )
               
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: itype, lda, ldb, lwork, n
           real(sp), intent(out) :: rwork(*), w(*)
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_chegv

     module subroutine stdlib_zhegv( itype, jobz, uplo, n, a, lda, b, ldb, w, work,lwork, rwork, info )
               
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: itype, lda, ldb, lwork, n
           real(dp), intent(out) :: rwork(*), w(*)
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_zhegv


end interface 


interface 
     module subroutine stdlib_chegvd( itype, jobz, uplo, n, a, lda, b, ldb, w, work,lwork, rwork, lrwork,&
                iwork, liwork, info )
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: itype, lda, ldb, liwork, lrwork, lwork, n
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(out) :: rwork(*), w(*)
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_chegvd

     module subroutine stdlib_zhegvd( itype, jobz, uplo, n, a, lda, b, ldb, w, work,lwork, rwork, lrwork,&
                iwork, liwork, info )
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: itype, lda, ldb, liwork, lrwork, lwork, n
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(out) :: rwork(*), w(*)
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_zhegvd


end interface 


interface 
     module subroutine stdlib_chegvx( itype, jobz, range, uplo, n, a, lda, b, ldb,vl, vu, il, iu, abstol,&
                m, w, z, ldz, work,lwork, rwork, iwork, ifail, info )
           character, intent(in) :: jobz, range, uplo
           integer(ilp), intent(in) :: il, itype, iu, lda, ldb, ldz, lwork, n
           integer(ilp), intent(out) :: info, m
           real(sp), intent(in) :: abstol, vl, vu
           integer(ilp), intent(out) :: ifail(*), iwork(*)
           real(sp), intent(out) :: rwork(*), w(*)
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(sp), intent(out) :: work(*), z(ldz,*)
     end subroutine stdlib_chegvx

     module subroutine stdlib_zhegvx( itype, jobz, range, uplo, n, a, lda, b, ldb,vl, vu, il, iu, abstol,&
                m, w, z, ldz, work,lwork, rwork, iwork, ifail, info )
           character, intent(in) :: jobz, range, uplo
           integer(ilp), intent(in) :: il, itype, iu, lda, ldb, ldz, lwork, n
           integer(ilp), intent(out) :: info, m
           real(dp), intent(in) :: abstol, vl, vu
           integer(ilp), intent(out) :: ifail(*), iwork(*)
           real(dp), intent(out) :: rwork(*), w(*)
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(dp), intent(out) :: work(*), z(ldz,*)
     end subroutine stdlib_zhegvx


end interface 


interface 
     module subroutine stdlib_chpgv( itype, jobz, uplo, n, ap, bp, w, z, ldz, work,rwork, info )
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: itype, ldz, n
           real(sp), intent(out) :: rwork(*), w(*)
           complex(sp), intent(inout) :: ap(*), bp(*)
           complex(sp), intent(out) :: work(*), z(ldz,*)
     end subroutine stdlib_chpgv

     module subroutine stdlib_zhpgv( itype, jobz, uplo, n, ap, bp, w, z, ldz, work,rwork, info )
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: itype, ldz, n
           real(dp), intent(out) :: rwork(*), w(*)
           complex(dp), intent(inout) :: ap(*), bp(*)
           complex(dp), intent(out) :: work(*), z(ldz,*)
     end subroutine stdlib_zhpgv


end interface 


interface 
     module subroutine stdlib_chpgvd( itype, jobz, uplo, n, ap, bp, w, z, ldz, work,lwork, rwork, lrwork,&
                iwork, liwork, info )
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: itype, ldz, liwork, lrwork, lwork, n
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(out) :: rwork(*), w(*)
           complex(sp), intent(inout) :: ap(*), bp(*)
           complex(sp), intent(out) :: work(*), z(ldz,*)
     end subroutine stdlib_chpgvd

     module subroutine stdlib_zhpgvd( itype, jobz, uplo, n, ap, bp, w, z, ldz, work,lwork, rwork, lrwork,&
                iwork, liwork, info )
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: itype, ldz, liwork, lrwork, lwork, n
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(out) :: rwork(*), w(*)
           complex(dp), intent(inout) :: ap(*), bp(*)
           complex(dp), intent(out) :: work(*), z(ldz,*)
     end subroutine stdlib_zhpgvd


end interface 


interface 
     module subroutine stdlib_chpgvx( itype, jobz, range, uplo, n, ap, bp, vl, vu,il, iu, abstol, m, w, &
               z, ldz, work, rwork,iwork, ifail, info )
           character, intent(in) :: jobz, range, uplo
           integer(ilp), intent(in) :: il, itype, iu, ldz, n
           integer(ilp), intent(out) :: info, m
           real(sp), intent(in) :: abstol, vl, vu
           integer(ilp), intent(out) :: ifail(*), iwork(*)
           real(sp), intent(out) :: rwork(*), w(*)
           complex(sp), intent(inout) :: ap(*), bp(*)
           complex(sp), intent(out) :: work(*), z(ldz,*)
     end subroutine stdlib_chpgvx

     module subroutine stdlib_zhpgvx( itype, jobz, range, uplo, n, ap, bp, vl, vu,il, iu, abstol, m, w, &
               z, ldz, work, rwork,iwork, ifail, info )
           character, intent(in) :: jobz, range, uplo
           integer(ilp), intent(in) :: il, itype, iu, ldz, n
           integer(ilp), intent(out) :: info, m
           real(dp), intent(in) :: abstol, vl, vu
           integer(ilp), intent(out) :: ifail(*), iwork(*)
           real(dp), intent(out) :: rwork(*), w(*)
           complex(dp), intent(inout) :: ap(*), bp(*)
           complex(dp), intent(out) :: work(*), z(ldz,*)
     end subroutine stdlib_zhpgvx


end interface 


interface 
     pure module subroutine stdlib_chbgv( jobz, uplo, n, ka, kb, ab, ldab, bb, ldbb, w, z,ldz, work, &
               rwork, info )
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ka, kb, ldab, ldbb, ldz, n
           real(sp), intent(out) :: rwork(*), w(*)
           complex(sp), intent(inout) :: ab(ldab,*), bb(ldbb,*)
           complex(sp), intent(out) :: work(*), z(ldz,*)
     end subroutine stdlib_chbgv

     pure module subroutine stdlib_zhbgv( jobz, uplo, n, ka, kb, ab, ldab, bb, ldbb, w, z,ldz, work, &
               rwork, info )
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ka, kb, ldab, ldbb, ldz, n
           real(dp), intent(out) :: rwork(*), w(*)
           complex(dp), intent(inout) :: ab(ldab,*), bb(ldbb,*)
           complex(dp), intent(out) :: work(*), z(ldz,*)
     end subroutine stdlib_zhbgv


end interface 


interface 
     pure module subroutine stdlib_chbgvd( jobz, uplo, n, ka, kb, ab, ldab, bb, ldbb, w,z, ldz, work, &
               lwork, rwork, lrwork, iwork,liwork, info )
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ka, kb, ldab, ldbb, ldz, liwork, lrwork, lwork, n
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(out) :: rwork(*), w(*)
           complex(sp), intent(inout) :: ab(ldab,*), bb(ldbb,*)
           complex(sp), intent(out) :: work(*), z(ldz,*)
     end subroutine stdlib_chbgvd

     pure module subroutine stdlib_zhbgvd( jobz, uplo, n, ka, kb, ab, ldab, bb, ldbb, w,z, ldz, work, &
               lwork, rwork, lrwork, iwork,liwork, info )
           character, intent(in) :: jobz, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ka, kb, ldab, ldbb, ldz, liwork, lrwork, lwork, n
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(out) :: rwork(*), w(*)
           complex(dp), intent(inout) :: ab(ldab,*), bb(ldbb,*)
           complex(dp), intent(out) :: work(*), z(ldz,*)
     end subroutine stdlib_zhbgvd


end interface 


interface 
     pure module subroutine stdlib_chbgvx( jobz, range, uplo, n, ka, kb, ab, ldab, bb,ldbb, q, ldq, vl, &
               vu, il, iu, abstol, m, w, z,ldz, work, rwork, iwork, ifail, info )
           character, intent(in) :: jobz, range, uplo
           integer(ilp), intent(in) :: il, iu, ka, kb, ldab, ldbb, ldq, ldz, n
           integer(ilp), intent(out) :: info, m
           real(sp), intent(in) :: abstol, vl, vu
           integer(ilp), intent(out) :: ifail(*), iwork(*)
           real(sp), intent(out) :: rwork(*), w(*)
           complex(sp), intent(inout) :: ab(ldab,*), bb(ldbb,*)
           complex(sp), intent(out) :: q(ldq,*), work(*), z(ldz,*)
     end subroutine stdlib_chbgvx

     pure module subroutine stdlib_zhbgvx( jobz, range, uplo, n, ka, kb, ab, ldab, bb,ldbb, q, ldq, vl, &
               vu, il, iu, abstol, m, w, z,ldz, work, rwork, iwork, ifail, info )
           character, intent(in) :: jobz, range, uplo
           integer(ilp), intent(in) :: il, iu, ka, kb, ldab, ldbb, ldq, ldz, n
           integer(ilp), intent(out) :: info, m
           real(dp), intent(in) :: abstol, vl, vu
           integer(ilp), intent(out) :: ifail(*), iwork(*)
           real(dp), intent(out) :: rwork(*), w(*)
           complex(dp), intent(inout) :: ab(ldab,*), bb(ldbb,*)
           complex(dp), intent(out) :: q(ldq,*), work(*), z(ldz,*)
     end subroutine stdlib_zhbgvx


end interface 


interface 
     pure module subroutine stdlib_chetrd( uplo, n, a, lda, d, e, tau, work, lwork, info )
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, n
           real(sp), intent(out) :: d(*), e(*)
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_chetrd

     pure module subroutine stdlib_zhetrd( uplo, n, a, lda, d, e, tau, work, lwork, info )
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, n
           real(dp), intent(out) :: d(*), e(*)
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_zhetrd


end interface 


interface 
     pure module subroutine stdlib_chetd2( uplo, n, a, lda, d, e, tau, info )
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           real(sp), intent(out) :: d(*), e(*)
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: tau(*)
     end subroutine stdlib_chetd2

     pure module subroutine stdlib_zhetd2( uplo, n, a, lda, d, e, tau, info )
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, n
           real(dp), intent(out) :: d(*), e(*)
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: tau(*)
     end subroutine stdlib_zhetd2


end interface 


interface 
     pure module subroutine stdlib_cungtr( uplo, n, a, lda, tau, work, lwork, info )
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, n
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(in) :: tau(*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_cungtr

     pure module subroutine stdlib_zungtr( uplo, n, a, lda, tau, work, lwork, info )
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, n
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(in) :: tau(*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_zungtr


end interface 


interface 
     pure module subroutine stdlib_cunmtr( side, uplo, trans, m, n, a, lda, tau, c, ldc,work, lwork, &
               info )
           character, intent(in) :: side, trans, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldc, lwork, m, n
           complex(sp), intent(inout) :: a(lda,*), c(ldc,*)
           complex(sp), intent(in) :: tau(*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_cunmtr

     pure module subroutine stdlib_zunmtr( side, uplo, trans, m, n, a, lda, tau, c, ldc,work, lwork, &
               info )
           character, intent(in) :: side, trans, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldc, lwork, m, n
           complex(dp), intent(inout) :: a(lda,*), c(ldc,*)
           complex(dp), intent(in) :: tau(*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_zunmtr


end interface 


interface 
     module subroutine stdlib_chetrd_he2hb( uplo, n, kd, a, lda, ab, ldab, tau,work, lwork, info )
               
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldab, lwork, n, kd
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: ab(ldab,*), tau(*), work(*)
     end subroutine stdlib_chetrd_he2hb

     module subroutine stdlib_zhetrd_he2hb( uplo, n, kd, a, lda, ab, ldab, tau,work, lwork, info )
               
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldab, lwork, n, kd
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: ab(ldab,*), tau(*), work(*)
     end subroutine stdlib_zhetrd_he2hb


end interface 


interface 
     module subroutine stdlib_chetrd_hb2st( stage1, vect, uplo, n, kd, ab, ldab,d, e, hous, lhous, &
               work, lwork, info )
           character, intent(in) :: stage1, uplo, vect
           integer(ilp), intent(in) :: n, kd, ldab, lhous, lwork
           integer(ilp), intent(out) :: info
           real(sp), intent(out) :: d(*), e(*)
           complex(sp), intent(inout) :: ab(ldab,*)
           complex(sp), intent(out) :: hous(*), work(*)
     end subroutine stdlib_chetrd_hb2st

     module subroutine stdlib_zhetrd_hb2st( stage1, vect, uplo, n, kd, ab, ldab,d, e, hous, lhous, &
               work, lwork, info )
           character, intent(in) :: stage1, uplo, vect
           integer(ilp), intent(in) :: n, kd, ldab, lhous, lwork
           integer(ilp), intent(out) :: info
           real(dp), intent(out) :: d(*), e(*)
           complex(dp), intent(inout) :: ab(ldab,*)
           complex(dp), intent(out) :: hous(*), work(*)
     end subroutine stdlib_zhetrd_hb2st


end interface 


interface 
     pure module subroutine stdlib_chb2st_kernels( uplo, wantz, ttype,st, ed, sweep, n, nb, ib,a, lda, &
               v, tau, ldvt, work)
           character, intent(in) :: uplo
           logical(lk), intent(in) :: wantz
           integer(ilp), intent(in) :: ttype, st, ed, sweep, n, nb, ib, lda, ldvt
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: v(*), tau(*), work(*)
     end subroutine stdlib_chb2st_kernels

     pure module subroutine stdlib_zhb2st_kernels( uplo, wantz, ttype,st, ed, sweep, n, nb, ib,a, lda, &
               v, tau, ldvt, work)
           character, intent(in) :: uplo
           logical(lk), intent(in) :: wantz
           integer(ilp), intent(in) :: ttype, st, ed, sweep, n, nb, ib, lda, ldvt
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: v(*), tau(*), work(*)
     end subroutine stdlib_zhb2st_kernels


end interface 


interface 
     module subroutine stdlib_ssytrd_sb2st( stage1, vect, uplo, n, kd, ab, ldab,d, e, hous, lhous, &
               work, lwork, info )
           character, intent(in) :: stage1, uplo, vect
           integer(ilp), intent(in) :: n, kd, ldab, lhous, lwork
           integer(ilp), intent(out) :: info
           real(sp), intent(out) :: d(*), e(*)
           real(sp), intent(inout) :: ab(ldab,*)
           real(sp), intent(out) :: hous(*), work(*)
     end subroutine stdlib_ssytrd_sb2st

     module subroutine stdlib_dsytrd_sb2st( stage1, vect, uplo, n, kd, ab, ldab,d, e, hous, lhous, &
               work, lwork, info )
           character, intent(in) :: stage1, uplo, vect
           integer(ilp), intent(in) :: n, kd, ldab, lhous, lwork
           integer(ilp), intent(out) :: info
           real(dp), intent(out) :: d(*), e(*)
           real(dp), intent(inout) :: ab(ldab,*)
           real(dp), intent(out) :: hous(*), work(*)
     end subroutine stdlib_dsytrd_sb2st


end interface 


interface 
     module subroutine stdlib_ssytrd_sy2sb( uplo, n, kd, a, lda, ab, ldab, tau,work, lwork, info )
               
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldab, lwork, n, kd
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: ab(ldab,*), tau(*), work(*)
     end subroutine stdlib_ssytrd_sy2sb

     module subroutine stdlib_dsytrd_sy2sb( uplo, n, kd, a, lda, ab, ldab, tau,work, lwork, info )
               
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldab, lwork, n, kd
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: ab(ldab,*), tau(*), work(*)
     end subroutine stdlib_dsytrd_sy2sb


end interface 


interface 
     pure module subroutine stdlib_slaic1( job, j, x, sest, w, gamma, sestpr, s, c )
           integer(ilp), intent(in) :: j, job
           real(sp), intent(out) :: c, s, sestpr
           real(sp), intent(in) :: gamma, sest
           real(sp), intent(in) :: w(j), x(j)
     end subroutine stdlib_slaic1

     pure module subroutine stdlib_dlaic1( job, j, x, sest, w, gamma, sestpr, s, c )
           integer(ilp), intent(in) :: j, job
           real(dp), intent(out) :: c, s, sestpr
           real(dp), intent(in) :: gamma, sest
           real(dp), intent(in) :: w(j), x(j)
     end subroutine stdlib_dlaic1


     pure module subroutine stdlib_claic1( job, j, x, sest, w, gamma, sestpr, s, c )
           integer(ilp), intent(in) :: j, job
           real(sp), intent(in) :: sest
           real(sp), intent(out) :: sestpr
           complex(sp), intent(out) :: c, s
           complex(sp), intent(in) :: gamma
           complex(sp), intent(in) :: w(j), x(j)
     end subroutine stdlib_claic1

     pure module subroutine stdlib_zlaic1( job, j, x, sest, w, gamma, sestpr, s, c )
           integer(ilp), intent(in) :: j, job
           real(dp), intent(in) :: sest
           real(dp), intent(out) :: sestpr
           complex(dp), intent(out) :: c, s
           complex(dp), intent(in) :: gamma
           complex(dp), intent(in) :: w(j), x(j)
     end subroutine stdlib_zlaic1


end interface 


interface 
     pure module subroutine stdlib_slals0( icompq, nl, nr, sqre, nrhs, b, ldb, bx, ldbx,perm, givptr, &
               givcol, ldgcol, givnum, ldgnum,poles, difl, difr, z, k, c, s, work, info )
           integer(ilp), intent(in) :: givptr, icompq, k, ldb, ldbx, ldgcol, ldgnum, nl, nr, nrhs,&
                      sqre
           integer(ilp), intent(out) :: info
           real(sp), intent(in) :: c, s
           integer(ilp), intent(in) :: givcol(ldgcol,*), perm(*)
           real(sp), intent(inout) :: b(ldb,*)
           real(sp), intent(out) :: bx(ldbx,*), work(*)
           real(sp), intent(in) :: difl(*), difr(ldgnum,*), givnum(ldgnum,*), poles(ldgnum,*), z(&
                     *)
     end subroutine stdlib_slals0

     pure module subroutine stdlib_dlals0( icompq, nl, nr, sqre, nrhs, b, ldb, bx, ldbx,perm, givptr, &
               givcol, ldgcol, givnum, ldgnum,poles, difl, difr, z, k, c, s, work, info )
           integer(ilp), intent(in) :: givptr, icompq, k, ldb, ldbx, ldgcol, ldgnum, nl, nr, nrhs,&
                      sqre
           integer(ilp), intent(out) :: info
           real(dp), intent(in) :: c, s
           integer(ilp), intent(in) :: givcol(ldgcol,*), perm(*)
           real(dp), intent(inout) :: b(ldb,*)
           real(dp), intent(out) :: bx(ldbx,*), work(*)
           real(dp), intent(in) :: difl(*), difr(ldgnum,*), givnum(ldgnum,*), poles(ldgnum,*), z(&
                     *)
     end subroutine stdlib_dlals0


     pure module subroutine stdlib_clals0( icompq, nl, nr, sqre, nrhs, b, ldb, bx, ldbx,perm, givptr, &
               givcol, ldgcol, givnum, ldgnum,poles, difl, difr, z, k, c, s, rwork, info )
           integer(ilp), intent(in) :: givptr, icompq, k, ldb, ldbx, ldgcol, ldgnum, nl, nr, nrhs,&
                      sqre
           integer(ilp), intent(out) :: info
           real(sp), intent(in) :: c, s
           integer(ilp), intent(in) :: givcol(ldgcol,*), perm(*)
           real(sp), intent(in) :: difl(*), difr(ldgnum,*), givnum(ldgnum,*), poles(ldgnum,*), z(&
                     *)
           real(sp), intent(out) :: rwork(*)
           complex(sp), intent(inout) :: b(ldb,*)
           complex(sp), intent(out) :: bx(ldbx,*)
     end subroutine stdlib_clals0

     pure module subroutine stdlib_zlals0( icompq, nl, nr, sqre, nrhs, b, ldb, bx, ldbx,perm, givptr, &
               givcol, ldgcol, givnum, ldgnum,poles, difl, difr, z, k, c, s, rwork, info )
           integer(ilp), intent(in) :: givptr, icompq, k, ldb, ldbx, ldgcol, ldgnum, nl, nr, nrhs,&
                      sqre
           integer(ilp), intent(out) :: info
           real(dp), intent(in) :: c, s
           integer(ilp), intent(in) :: givcol(ldgcol,*), perm(*)
           real(dp), intent(in) :: difl(*), difr(ldgnum,*), givnum(ldgnum,*), poles(ldgnum,*), z(&
                     *)
           real(dp), intent(out) :: rwork(*)
           complex(dp), intent(inout) :: b(ldb,*)
           complex(dp), intent(out) :: bx(ldbx,*)
     end subroutine stdlib_zlals0


end interface 


interface 
     pure module subroutine stdlib_slalsa( icompq, smlsiz, n, nrhs, b, ldb, bx, ldbx, u,ldu, vt, k, difl,&
                difr, z, poles, givptr,givcol, ldgcol, perm, givnum, c, s, work,iwork, info )
           integer(ilp), intent(in) :: icompq, ldb, ldbx, ldgcol, ldu, n, nrhs, smlsiz
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: givcol(ldgcol,*), givptr(*), k(*), perm(ldgcol,*)
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(inout) :: b(ldb,*)
           real(sp), intent(out) :: bx(ldbx,*), work(*)
           real(sp), intent(in) :: c(*), difl(ldu,*), difr(ldu,*), givnum(ldu,*), poles(ldu,*), s(&
                     *), u(ldu,*), vt(ldu,*), z(ldu,*)
     end subroutine stdlib_slalsa

     pure module subroutine stdlib_dlalsa( icompq, smlsiz, n, nrhs, b, ldb, bx, ldbx, u,ldu, vt, k, difl,&
                difr, z, poles, givptr,givcol, ldgcol, perm, givnum, c, s, work,iwork, info )
           integer(ilp), intent(in) :: icompq, ldb, ldbx, ldgcol, ldu, n, nrhs, smlsiz
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: givcol(ldgcol,*), givptr(*), k(*), perm(ldgcol,*)
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(inout) :: b(ldb,*)
           real(dp), intent(out) :: bx(ldbx,*), work(*)
           real(dp), intent(in) :: c(*), difl(ldu,*), difr(ldu,*), givnum(ldu,*), poles(ldu,*), s(&
                     *), u(ldu,*), vt(ldu,*), z(ldu,*)
     end subroutine stdlib_dlalsa


     pure module subroutine stdlib_clalsa( icompq, smlsiz, n, nrhs, b, ldb, bx, ldbx, u,ldu, vt, k, difl,&
                difr, z, poles, givptr,givcol, ldgcol, perm, givnum, c, s, rwork,iwork, info )
           integer(ilp), intent(in) :: icompq, ldb, ldbx, ldgcol, ldu, n, nrhs, smlsiz
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: givcol(ldgcol,*), givptr(*), k(*), perm(ldgcol,*)
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(in) :: c(*), difl(ldu,*), difr(ldu,*), givnum(ldu,*), poles(ldu,*), s(&
                     *), u(ldu,*), vt(ldu,*), z(ldu,*)
           real(sp), intent(out) :: rwork(*)
           complex(sp), intent(inout) :: b(ldb,*)
           complex(sp), intent(out) :: bx(ldbx,*)
     end subroutine stdlib_clalsa

     pure module subroutine stdlib_zlalsa( icompq, smlsiz, n, nrhs, b, ldb, bx, ldbx, u,ldu, vt, k, difl,&
                difr, z, poles, givptr,givcol, ldgcol, perm, givnum, c, s, rwork,iwork, info )
           integer(ilp), intent(in) :: icompq, ldb, ldbx, ldgcol, ldu, n, nrhs, smlsiz
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: givcol(ldgcol,*), givptr(*), k(*), perm(ldgcol,*)
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(in) :: c(*), difl(ldu,*), difr(ldu,*), givnum(ldu,*), poles(ldu,*), s(&
                     *), u(ldu,*), vt(ldu,*), z(ldu,*)
           real(dp), intent(out) :: rwork(*)
           complex(dp), intent(inout) :: b(ldb,*)
           complex(dp), intent(out) :: bx(ldbx,*)
     end subroutine stdlib_zlalsa


end interface 


interface 
     pure module subroutine stdlib_slalsd( uplo, smlsiz, n, nrhs, d, e, b, ldb, rcond,rank, work, iwork, &
               info )
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info, rank
           integer(ilp), intent(in) :: ldb, n, nrhs, smlsiz
           real(sp), intent(in) :: rcond
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(inout) :: b(ldb,*), d(*), e(*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_slalsd

     pure module subroutine stdlib_dlalsd( uplo, smlsiz, n, nrhs, d, e, b, ldb, rcond,rank, work, iwork, &
               info )
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info, rank
           integer(ilp), intent(in) :: ldb, n, nrhs, smlsiz
           real(dp), intent(in) :: rcond
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(inout) :: b(ldb,*), d(*), e(*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_dlalsd


     pure module subroutine stdlib_clalsd( uplo, smlsiz, n, nrhs, d, e, b, ldb, rcond,rank, work, rwork, &
               iwork, info )
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info, rank
           integer(ilp), intent(in) :: ldb, n, nrhs, smlsiz
           real(sp), intent(in) :: rcond
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(inout) :: d(*), e(*)
           real(sp), intent(out) :: rwork(*)
           complex(sp), intent(inout) :: b(ldb,*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_clalsd

     pure module subroutine stdlib_zlalsd( uplo, smlsiz, n, nrhs, d, e, b, ldb, rcond,rank, work, rwork, &
               iwork, info )
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info, rank
           integer(ilp), intent(in) :: ldb, n, nrhs, smlsiz
           real(dp), intent(in) :: rcond
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(inout) :: d(*), e(*)
           real(dp), intent(out) :: rwork(*)
           complex(dp), intent(inout) :: b(ldb,*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_zlalsd


end interface 


interface 
     module subroutine stdlib_sgelss( m, n, nrhs, a, lda, b, ldb, s, rcond, rank,work, lwork, info )
               
           integer(ilp), intent(out) :: info, rank
           integer(ilp), intent(in) :: lda, ldb, lwork, m, n, nrhs
           real(sp), intent(in) :: rcond
           real(sp), intent(inout) :: a(lda,*), b(ldb,*)
           real(sp), intent(out) :: s(*), work(*)
     end subroutine stdlib_sgelss

     module subroutine stdlib_dgelss( m, n, nrhs, a, lda, b, ldb, s, rcond, rank,work, lwork, info )
               
           integer(ilp), intent(out) :: info, rank
           integer(ilp), intent(in) :: lda, ldb, lwork, m, n, nrhs
           real(dp), intent(in) :: rcond
           real(dp), intent(inout) :: a(lda,*), b(ldb,*)
           real(dp), intent(out) :: s(*), work(*)
     end subroutine stdlib_dgelss


     module subroutine stdlib_cgelss( m, n, nrhs, a, lda, b, ldb, s, rcond, rank,work, lwork, rwork, &
               info )
           integer(ilp), intent(out) :: info, rank
           integer(ilp), intent(in) :: lda, ldb, lwork, m, n, nrhs
           real(sp), intent(in) :: rcond
           real(sp), intent(out) :: rwork(*), s(*)
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_cgelss

     module subroutine stdlib_zgelss( m, n, nrhs, a, lda, b, ldb, s, rcond, rank,work, lwork, rwork, &
               info )
           integer(ilp), intent(out) :: info, rank
           integer(ilp), intent(in) :: lda, ldb, lwork, m, n, nrhs
           real(dp), intent(in) :: rcond
           real(dp), intent(out) :: rwork(*), s(*)
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_zgelss


end interface 


interface 
     module subroutine stdlib_sgelsy( m, n, nrhs, a, lda, b, ldb, jpvt, rcond, rank,work, lwork, info )
               
           integer(ilp), intent(out) :: info, rank
           integer(ilp), intent(in) :: lda, ldb, lwork, m, n, nrhs
           real(sp), intent(in) :: rcond
           integer(ilp), intent(inout) :: jpvt(*)
           real(sp), intent(inout) :: a(lda,*), b(ldb,*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_sgelsy

     module subroutine stdlib_dgelsy( m, n, nrhs, a, lda, b, ldb, jpvt, rcond, rank,work, lwork, info )
               
           integer(ilp), intent(out) :: info, rank
           integer(ilp), intent(in) :: lda, ldb, lwork, m, n, nrhs
           real(dp), intent(in) :: rcond
           integer(ilp), intent(inout) :: jpvt(*)
           real(dp), intent(inout) :: a(lda,*), b(ldb,*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_dgelsy


     module subroutine stdlib_cgelsy( m, n, nrhs, a, lda, b, ldb, jpvt, rcond, rank,work, lwork, rwork, &
               info )
           integer(ilp), intent(out) :: info, rank
           integer(ilp), intent(in) :: lda, ldb, lwork, m, n, nrhs
           real(sp), intent(in) :: rcond
           integer(ilp), intent(inout) :: jpvt(*)
           real(sp), intent(out) :: rwork(*)
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_cgelsy

     module subroutine stdlib_zgelsy( m, n, nrhs, a, lda, b, ldb, jpvt, rcond, rank,work, lwork, rwork, &
               info )
           integer(ilp), intent(out) :: info, rank
           integer(ilp), intent(in) :: lda, ldb, lwork, m, n, nrhs
           real(dp), intent(in) :: rcond
           integer(ilp), intent(inout) :: jpvt(*)
           real(dp), intent(out) :: rwork(*)
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_zgelsy


end interface 


interface 
     module subroutine stdlib_sgels( trans, m, n, nrhs, a, lda, b, ldb, work, lwork,info )
           character, intent(in) :: trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, lwork, m, n, nrhs
           real(sp), intent(inout) :: a(lda,*), b(ldb,*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_sgels

     module subroutine stdlib_dgels( trans, m, n, nrhs, a, lda, b, ldb, work, lwork,info )
           character, intent(in) :: trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, lwork, m, n, nrhs
           real(dp), intent(inout) :: a(lda,*), b(ldb,*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_dgels


     module subroutine stdlib_cgels( trans, m, n, nrhs, a, lda, b, ldb, work, lwork,info )
           character, intent(in) :: trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, lwork, m, n, nrhs
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_cgels

     module subroutine stdlib_zgels( trans, m, n, nrhs, a, lda, b, ldb, work, lwork,info )
           character, intent(in) :: trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, lwork, m, n, nrhs
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_zgels


end interface 


interface 
     module subroutine stdlib_sgelsd( m, n, nrhs, a, lda, b, ldb, s, rcond,rank, work, lwork, iwork, &
               info )
           integer(ilp), intent(out) :: info, rank
           integer(ilp), intent(in) :: lda, ldb, lwork, m, n, nrhs
           real(sp), intent(in) :: rcond
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(inout) :: a(lda,*), b(ldb,*)
           real(sp), intent(out) :: s(*), work(*)
     end subroutine stdlib_sgelsd

     module subroutine stdlib_dgelsd( m, n, nrhs, a, lda, b, ldb, s, rcond, rank,work, lwork, iwork, &
               info )
           integer(ilp), intent(out) :: info, rank
           integer(ilp), intent(in) :: lda, ldb, lwork, m, n, nrhs
           real(dp), intent(in) :: rcond
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(inout) :: a(lda,*), b(ldb,*)
           real(dp), intent(out) :: s(*), work(*)
     end subroutine stdlib_dgelsd


     module subroutine stdlib_cgelsd( m, n, nrhs, a, lda, b, ldb, s, rcond, rank,work, lwork, rwork, &
               iwork, info )
           integer(ilp), intent(out) :: info, rank
           integer(ilp), intent(in) :: lda, ldb, lwork, m, n, nrhs
           real(sp), intent(in) :: rcond
           integer(ilp), intent(out) :: iwork(*)
           real(sp), intent(out) :: rwork(*), s(*)
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_cgelsd

     module subroutine stdlib_zgelsd( m, n, nrhs, a, lda, b, ldb, s, rcond, rank,work, lwork, rwork, &
               iwork, info )
           integer(ilp), intent(out) :: info, rank
           integer(ilp), intent(in) :: lda, ldb, lwork, m, n, nrhs
           real(dp), intent(in) :: rcond
           integer(ilp), intent(out) :: iwork(*)
           real(dp), intent(out) :: rwork(*), s(*)
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_zgelsd


end interface 


interface 
     module subroutine stdlib_sgetsls( trans, m, n, nrhs, a, lda, b, ldb,work, lwork, info )
           character, intent(in) :: trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, lwork, m, n, nrhs
           real(sp), intent(inout) :: a(lda,*), b(ldb,*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_sgetsls

     module subroutine stdlib_dgetsls( trans, m, n, nrhs, a, lda, b, ldb,work, lwork, info )
           character, intent(in) :: trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, lwork, m, n, nrhs
           real(dp), intent(inout) :: a(lda,*), b(ldb,*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_dgetsls


     module subroutine stdlib_cgetsls( trans, m, n, nrhs, a, lda, b, ldb,work, lwork, info )
           character, intent(in) :: trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, lwork, m, n, nrhs
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_cgetsls

     module subroutine stdlib_zgetsls( trans, m, n, nrhs, a, lda, b, ldb,work, lwork, info )
           character, intent(in) :: trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, lwork, m, n, nrhs
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_zgetsls


end interface 


interface 
     pure module subroutine stdlib_sgglse( m, n, p, a, lda, b, ldb, c, d, x, work, lwork,info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, lwork, m, n, p
           real(sp), intent(inout) :: a(lda,*), b(ldb,*), c(*), d(*)
           real(sp), intent(out) :: work(*), x(*)
     end subroutine stdlib_sgglse

     pure module subroutine stdlib_dgglse( m, n, p, a, lda, b, ldb, c, d, x, work, lwork,info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, lwork, m, n, p
           real(dp), intent(inout) :: a(lda,*), b(ldb,*), c(*), d(*)
           real(dp), intent(out) :: work(*), x(*)
     end subroutine stdlib_dgglse


     pure module subroutine stdlib_cgglse( m, n, p, a, lda, b, ldb, c, d, x, work, lwork,info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, lwork, m, n, p
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*), c(*), d(*)
           complex(sp), intent(out) :: work(*), x(*)
     end subroutine stdlib_cgglse

     pure module subroutine stdlib_zgglse( m, n, p, a, lda, b, ldb, c, d, x, work, lwork,info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, lwork, m, n, p
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*), c(*), d(*)
           complex(dp), intent(out) :: work(*), x(*)
     end subroutine stdlib_zgglse


end interface 


interface 
     pure module subroutine stdlib_sggglm( n, m, p, a, lda, b, ldb, d, x, y, work, lwork,info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, lwork, m, n, p
           real(sp), intent(inout) :: a(lda,*), b(ldb,*), d(*)
           real(sp), intent(out) :: work(*), x(*), y(*)
     end subroutine stdlib_sggglm

     pure module subroutine stdlib_dggglm( n, m, p, a, lda, b, ldb, d, x, y, work, lwork,info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, lwork, m, n, p
           real(dp), intent(inout) :: a(lda,*), b(ldb,*), d(*)
           real(dp), intent(out) :: work(*), x(*), y(*)
     end subroutine stdlib_dggglm


     pure module subroutine stdlib_cggglm( n, m, p, a, lda, b, ldb, d, x, y, work, lwork,info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, lwork, m, n, p
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*), d(*)
           complex(sp), intent(out) :: work(*), x(*), y(*)
     end subroutine stdlib_cggglm

     pure module subroutine stdlib_zggglm( n, m, p, a, lda, b, ldb, d, x, y, work, lwork,info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, lwork, m, n, p
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*), d(*)
           complex(dp), intent(out) :: work(*), x(*), y(*)
     end subroutine stdlib_zggglm


end interface 

end module stdlib_lapack_eig_svd_lsq
