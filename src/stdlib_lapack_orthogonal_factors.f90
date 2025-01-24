module stdlib_lapack_orthogonal_factors
  use stdlib_linalg_constants
  use stdlib_linalg_lapack_aux
  use stdlib_linalg_blas
  use stdlib_lapack_base
  implicit none

interface 
     pure module subroutine stdlib_stzrzf( m, n, a, lda, tau, work, lwork, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, m, n
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_stzrzf

     pure module subroutine stdlib_dtzrzf( m, n, a, lda, tau, work, lwork, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, m, n
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_dtzrzf


     pure module subroutine stdlib_ctzrzf( m, n, a, lda, tau, work, lwork, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, m, n
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_ctzrzf

     pure module subroutine stdlib_ztzrzf( m, n, a, lda, tau, work, lwork, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, m, n
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_ztzrzf


     pure module subroutine stdlib_I64_stzrzf( m, n, a, lda, tau, work, lwork, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, lwork, m, n
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_I64_stzrzf

     pure module subroutine stdlib_I64_dtzrzf( m, n, a, lda, tau, work, lwork, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, lwork, m, n
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_I64_dtzrzf


     pure module subroutine stdlib_I64_ctzrzf( m, n, a, lda, tau, work, lwork, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, lwork, m, n
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_I64_ctzrzf

     pure module subroutine stdlib_I64_ztzrzf( m, n, a, lda, tau, work, lwork, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, lwork, m, n
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_I64_ztzrzf


end interface 


interface 
     pure module subroutine stdlib_cunmrz( side, trans, m, n, k, l, a, lda, tau, c, ldc,work, lwork, &
               info )
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, l, lda, ldc, lwork, m, n
           complex(sp), intent(inout) :: a(lda,*), c(ldc,*)
           complex(sp), intent(in) :: tau(*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_cunmrz

     pure module subroutine stdlib_zunmrz( side, trans, m, n, k, l, a, lda, tau, c, ldc,work, lwork, &
               info )
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, l, lda, ldc, lwork, m, n
           complex(dp), intent(inout) :: a(lda,*), c(ldc,*)
           complex(dp), intent(in) :: tau(*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_zunmrz


     pure module subroutine stdlib_I64_cunmrz( side, trans, m, n, k, l, a, lda, tau, c, ldc,work, lwork, &
               info )
           character, intent(in) :: side, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, l, lda, ldc, lwork, m, n
           complex(sp), intent(inout) :: a(lda,*), c(ldc,*)
           complex(sp), intent(in) :: tau(*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_I64_cunmrz

     pure module subroutine stdlib_I64_zunmrz( side, trans, m, n, k, l, a, lda, tau, c, ldc,work, lwork, &
               info )
           character, intent(in) :: side, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, l, lda, ldc, lwork, m, n
           complex(dp), intent(inout) :: a(lda,*), c(ldc,*)
           complex(dp), intent(in) :: tau(*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_I64_zunmrz


end interface 


interface 
     pure module subroutine stdlib_sormrz( side, trans, m, n, k, l, a, lda, tau, c, ldc,work, lwork, &
               info )
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, l, lda, ldc, lwork, m, n
           real(sp), intent(inout) :: a(lda,*), c(ldc,*)
           real(sp), intent(in) :: tau(*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_sormrz

     pure module subroutine stdlib_dormrz( side, trans, m, n, k, l, a, lda, tau, c, ldc,work, lwork, &
               info )
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, l, lda, ldc, lwork, m, n
           real(dp), intent(inout) :: a(lda,*), c(ldc,*)
           real(dp), intent(in) :: tau(*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_dormrz


     pure module subroutine stdlib_I64_sormrz( side, trans, m, n, k, l, a, lda, tau, c, ldc,work, lwork, &
               info )
           character, intent(in) :: side, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, l, lda, ldc, lwork, m, n
           real(sp), intent(inout) :: a(lda,*), c(ldc,*)
           real(sp), intent(in) :: tau(*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_I64_sormrz

     pure module subroutine stdlib_I64_dormrz( side, trans, m, n, k, l, a, lda, tau, c, ldc,work, lwork, &
               info )
           character, intent(in) :: side, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, l, lda, ldc, lwork, m, n
           real(dp), intent(inout) :: a(lda,*), c(ldc,*)
           real(dp), intent(in) :: tau(*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_I64_dormrz


end interface 


interface 
     pure module subroutine stdlib_cunmr3( side, trans, m, n, k, l, a, lda, tau, c, ldc,work, info )
               
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, l, lda, ldc, m, n
           complex(sp), intent(in) :: a(lda,*), tau(*)
           complex(sp), intent(inout) :: c(ldc,*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_cunmr3

     pure module subroutine stdlib_zunmr3( side, trans, m, n, k, l, a, lda, tau, c, ldc,work, info )
               
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, l, lda, ldc, m, n
           complex(dp), intent(in) :: a(lda,*), tau(*)
           complex(dp), intent(inout) :: c(ldc,*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_zunmr3


     pure module subroutine stdlib_I64_cunmr3( side, trans, m, n, k, l, a, lda, tau, c, ldc,work, info )
               
           character, intent(in) :: side, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, l, lda, ldc, m, n
           complex(sp), intent(in) :: a(lda,*), tau(*)
           complex(sp), intent(inout) :: c(ldc,*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_I64_cunmr3

     pure module subroutine stdlib_I64_zunmr3( side, trans, m, n, k, l, a, lda, tau, c, ldc,work, info )
               
           character, intent(in) :: side, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, l, lda, ldc, m, n
           complex(dp), intent(in) :: a(lda,*), tau(*)
           complex(dp), intent(inout) :: c(ldc,*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_I64_zunmr3


end interface 


interface 
     pure module subroutine stdlib_sormr3( side, trans, m, n, k, l, a, lda, tau, c, ldc,work, info )
               
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, l, lda, ldc, m, n
           real(sp), intent(in) :: a(lda,*), tau(*)
           real(sp), intent(inout) :: c(ldc,*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_sormr3

     pure module subroutine stdlib_dormr3( side, trans, m, n, k, l, a, lda, tau, c, ldc,work, info )
               
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, l, lda, ldc, m, n
           real(dp), intent(in) :: a(lda,*), tau(*)
           real(dp), intent(inout) :: c(ldc,*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_dormr3


     pure module subroutine stdlib_I64_sormr3( side, trans, m, n, k, l, a, lda, tau, c, ldc,work, info )
               
           character, intent(in) :: side, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, l, lda, ldc, m, n
           real(sp), intent(in) :: a(lda,*), tau(*)
           real(sp), intent(inout) :: c(ldc,*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_I64_sormr3

     pure module subroutine stdlib_I64_dormr3( side, trans, m, n, k, l, a, lda, tau, c, ldc,work, info )
               
           character, intent(in) :: side, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, l, lda, ldc, m, n
           real(dp), intent(in) :: a(lda,*), tau(*)
           real(dp), intent(inout) :: c(ldc,*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_I64_dormr3


end interface 


interface 
     pure module subroutine stdlib_slarz( side, m, n, l, v, incv, tau, c, ldc, work )
           character, intent(in) :: side
           integer(ilp), intent(in) :: incv, l, ldc, m, n
           real(sp), intent(in) :: tau
           real(sp), intent(inout) :: c(ldc,*)
           real(sp), intent(in) :: v(*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_slarz

     pure module subroutine stdlib_dlarz( side, m, n, l, v, incv, tau, c, ldc, work )
           character, intent(in) :: side
           integer(ilp), intent(in) :: incv, l, ldc, m, n
           real(dp), intent(in) :: tau
           real(dp), intent(inout) :: c(ldc,*)
           real(dp), intent(in) :: v(*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_dlarz


     pure module subroutine stdlib_clarz( side, m, n, l, v, incv, tau, c, ldc, work )
           character, intent(in) :: side
           integer(ilp), intent(in) :: incv, l, ldc, m, n
           complex(sp), intent(in) :: tau
           complex(sp), intent(inout) :: c(ldc,*)
           complex(sp), intent(in) :: v(*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_clarz

     pure module subroutine stdlib_zlarz( side, m, n, l, v, incv, tau, c, ldc, work )
           character, intent(in) :: side
           integer(ilp), intent(in) :: incv, l, ldc, m, n
           complex(dp), intent(in) :: tau
           complex(dp), intent(inout) :: c(ldc,*)
           complex(dp), intent(in) :: v(*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_zlarz


     pure module subroutine stdlib_I64_slarz( side, m, n, l, v, incv, tau, c, ldc, work )
           character, intent(in) :: side
           integer(ilp64), intent(in) :: incv, l, ldc, m, n
           real(sp), intent(in) :: tau
           real(sp), intent(inout) :: c(ldc,*)
           real(sp), intent(in) :: v(*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_I64_slarz

     pure module subroutine stdlib_I64_dlarz( side, m, n, l, v, incv, tau, c, ldc, work )
           character, intent(in) :: side
           integer(ilp64), intent(in) :: incv, l, ldc, m, n
           real(dp), intent(in) :: tau
           real(dp), intent(inout) :: c(ldc,*)
           real(dp), intent(in) :: v(*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_I64_dlarz


     pure module subroutine stdlib_I64_clarz( side, m, n, l, v, incv, tau, c, ldc, work )
           character, intent(in) :: side
           integer(ilp64), intent(in) :: incv, l, ldc, m, n
           complex(sp), intent(in) :: tau
           complex(sp), intent(inout) :: c(ldc,*)
           complex(sp), intent(in) :: v(*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_I64_clarz

     pure module subroutine stdlib_I64_zlarz( side, m, n, l, v, incv, tau, c, ldc, work )
           character, intent(in) :: side
           integer(ilp64), intent(in) :: incv, l, ldc, m, n
           complex(dp), intent(in) :: tau
           complex(dp), intent(inout) :: c(ldc,*)
           complex(dp), intent(in) :: v(*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_I64_zlarz


end interface 


interface 
     pure module subroutine stdlib_slarzb( side, trans, direct, storev, m, n, k, l, v,ldv, t, ldt, c, &
               ldc, work, ldwork )
           character, intent(in) :: direct, side, storev, trans
           integer(ilp), intent(in) :: k, l, ldc, ldt, ldv, ldwork, m, n
           real(sp), intent(inout) :: c(ldc,*), t(ldt,*), v(ldv,*)
           real(sp), intent(out) :: work(ldwork,*)
     end subroutine stdlib_slarzb

     pure module subroutine stdlib_dlarzb( side, trans, direct, storev, m, n, k, l, v,ldv, t, ldt, c, &
               ldc, work, ldwork )
           character, intent(in) :: direct, side, storev, trans
           integer(ilp), intent(in) :: k, l, ldc, ldt, ldv, ldwork, m, n
           real(dp), intent(inout) :: c(ldc,*), t(ldt,*), v(ldv,*)
           real(dp), intent(out) :: work(ldwork,*)
     end subroutine stdlib_dlarzb


     pure module subroutine stdlib_clarzb( side, trans, direct, storev, m, n, k, l, v,ldv, t, ldt, c, &
               ldc, work, ldwork )
           character, intent(in) :: direct, side, storev, trans
           integer(ilp), intent(in) :: k, l, ldc, ldt, ldv, ldwork, m, n
           complex(sp), intent(inout) :: c(ldc,*), t(ldt,*), v(ldv,*)
           complex(sp), intent(out) :: work(ldwork,*)
     end subroutine stdlib_clarzb

     pure module subroutine stdlib_zlarzb( side, trans, direct, storev, m, n, k, l, v,ldv, t, ldt, c, &
               ldc, work, ldwork )
           character, intent(in) :: direct, side, storev, trans
           integer(ilp), intent(in) :: k, l, ldc, ldt, ldv, ldwork, m, n
           complex(dp), intent(inout) :: c(ldc,*), t(ldt,*), v(ldv,*)
           complex(dp), intent(out) :: work(ldwork,*)
     end subroutine stdlib_zlarzb


     pure module subroutine stdlib_I64_slarzb( side, trans, direct, storev, m, n, k, l, v,ldv, t, ldt, c, &
               ldc, work, ldwork )
           character, intent(in) :: direct, side, storev, trans
           integer(ilp64), intent(in) :: k, l, ldc, ldt, ldv, ldwork, m, n
           real(sp), intent(inout) :: c(ldc,*), t(ldt,*), v(ldv,*)
           real(sp), intent(out) :: work(ldwork,*)
     end subroutine stdlib_I64_slarzb

     pure module subroutine stdlib_I64_dlarzb( side, trans, direct, storev, m, n, k, l, v,ldv, t, ldt, c, &
               ldc, work, ldwork )
           character, intent(in) :: direct, side, storev, trans
           integer(ilp64), intent(in) :: k, l, ldc, ldt, ldv, ldwork, m, n
           real(dp), intent(inout) :: c(ldc,*), t(ldt,*), v(ldv,*)
           real(dp), intent(out) :: work(ldwork,*)
     end subroutine stdlib_I64_dlarzb


     pure module subroutine stdlib_I64_clarzb( side, trans, direct, storev, m, n, k, l, v,ldv, t, ldt, c, &
               ldc, work, ldwork )
           character, intent(in) :: direct, side, storev, trans
           integer(ilp64), intent(in) :: k, l, ldc, ldt, ldv, ldwork, m, n
           complex(sp), intent(inout) :: c(ldc,*), t(ldt,*), v(ldv,*)
           complex(sp), intent(out) :: work(ldwork,*)
     end subroutine stdlib_I64_clarzb

     pure module subroutine stdlib_I64_zlarzb( side, trans, direct, storev, m, n, k, l, v,ldv, t, ldt, c, &
               ldc, work, ldwork )
           character, intent(in) :: direct, side, storev, trans
           integer(ilp64), intent(in) :: k, l, ldc, ldt, ldv, ldwork, m, n
           complex(dp), intent(inout) :: c(ldc,*), t(ldt,*), v(ldv,*)
           complex(dp), intent(out) :: work(ldwork,*)
     end subroutine stdlib_I64_zlarzb


end interface 


interface 
     pure module subroutine stdlib_slarzt( direct, storev, n, k, v, ldv, tau, t, ldt )
           character, intent(in) :: direct, storev
           integer(ilp), intent(in) :: k, ldt, ldv, n
           real(sp), intent(out) :: t(ldt,*)
           real(sp), intent(in) :: tau(*)
           real(sp), intent(inout) :: v(ldv,*)
     end subroutine stdlib_slarzt

     pure module subroutine stdlib_dlarzt( direct, storev, n, k, v, ldv, tau, t, ldt )
           character, intent(in) :: direct, storev
           integer(ilp), intent(in) :: k, ldt, ldv, n
           real(dp), intent(out) :: t(ldt,*)
           real(dp), intent(in) :: tau(*)
           real(dp), intent(inout) :: v(ldv,*)
     end subroutine stdlib_dlarzt


     pure module subroutine stdlib_clarzt( direct, storev, n, k, v, ldv, tau, t, ldt )
           character, intent(in) :: direct, storev
           integer(ilp), intent(in) :: k, ldt, ldv, n
           complex(sp), intent(out) :: t(ldt,*)
           complex(sp), intent(in) :: tau(*)
           complex(sp), intent(inout) :: v(ldv,*)
     end subroutine stdlib_clarzt

     pure module subroutine stdlib_zlarzt( direct, storev, n, k, v, ldv, tau, t, ldt )
           character, intent(in) :: direct, storev
           integer(ilp), intent(in) :: k, ldt, ldv, n
           complex(dp), intent(out) :: t(ldt,*)
           complex(dp), intent(in) :: tau(*)
           complex(dp), intent(inout) :: v(ldv,*)
     end subroutine stdlib_zlarzt


     pure module subroutine stdlib_I64_slarzt( direct, storev, n, k, v, ldv, tau, t, ldt )
           character, intent(in) :: direct, storev
           integer(ilp64), intent(in) :: k, ldt, ldv, n
           real(sp), intent(out) :: t(ldt,*)
           real(sp), intent(in) :: tau(*)
           real(sp), intent(inout) :: v(ldv,*)
     end subroutine stdlib_I64_slarzt

     pure module subroutine stdlib_I64_dlarzt( direct, storev, n, k, v, ldv, tau, t, ldt )
           character, intent(in) :: direct, storev
           integer(ilp64), intent(in) :: k, ldt, ldv, n
           real(dp), intent(out) :: t(ldt,*)
           real(dp), intent(in) :: tau(*)
           real(dp), intent(inout) :: v(ldv,*)
     end subroutine stdlib_I64_dlarzt


     pure module subroutine stdlib_I64_clarzt( direct, storev, n, k, v, ldv, tau, t, ldt )
           character, intent(in) :: direct, storev
           integer(ilp64), intent(in) :: k, ldt, ldv, n
           complex(sp), intent(out) :: t(ldt,*)
           complex(sp), intent(in) :: tau(*)
           complex(sp), intent(inout) :: v(ldv,*)
     end subroutine stdlib_I64_clarzt

     pure module subroutine stdlib_I64_zlarzt( direct, storev, n, k, v, ldv, tau, t, ldt )
           character, intent(in) :: direct, storev
           integer(ilp64), intent(in) :: k, ldt, ldv, n
           complex(dp), intent(out) :: t(ldt,*)
           complex(dp), intent(in) :: tau(*)
           complex(dp), intent(inout) :: v(ldv,*)
     end subroutine stdlib_I64_zlarzt


end interface 


interface 
     pure module subroutine stdlib_slatrz( m, n, l, a, lda, tau, work )
           integer(ilp), intent(in) :: l, lda, m, n
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_slatrz

     pure module subroutine stdlib_dlatrz( m, n, l, a, lda, tau, work )
           integer(ilp), intent(in) :: l, lda, m, n
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_dlatrz


     pure module subroutine stdlib_clatrz( m, n, l, a, lda, tau, work )
           integer(ilp), intent(in) :: l, lda, m, n
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_clatrz

     pure module subroutine stdlib_zlatrz( m, n, l, a, lda, tau, work )
           integer(ilp), intent(in) :: l, lda, m, n
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_zlatrz


     pure module subroutine stdlib_I64_slatrz( m, n, l, a, lda, tau, work )
           integer(ilp64), intent(in) :: l, lda, m, n
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_I64_slatrz

     pure module subroutine stdlib_I64_dlatrz( m, n, l, a, lda, tau, work )
           integer(ilp64), intent(in) :: l, lda, m, n
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_I64_dlatrz


     pure module subroutine stdlib_I64_clatrz( m, n, l, a, lda, tau, work )
           integer(ilp64), intent(in) :: l, lda, m, n
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_I64_clatrz

     pure module subroutine stdlib_I64_zlatrz( m, n, l, a, lda, tau, work )
           integer(ilp64), intent(in) :: l, lda, m, n
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_I64_zlatrz


end interface 


interface 
     pure module subroutine stdlib_sgeqr( m, n, a, lda, t, tsize, work, lwork,info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n, tsize, lwork
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: t(*), work(*)
     end subroutine stdlib_sgeqr

     pure module subroutine stdlib_dgeqr( m, n, a, lda, t, tsize, work, lwork,info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n, tsize, lwork
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: t(*), work(*)
     end subroutine stdlib_dgeqr


     pure module subroutine stdlib_cgeqr( m, n, a, lda, t, tsize, work, lwork,info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n, tsize, lwork
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: t(*), work(*)
     end subroutine stdlib_cgeqr

     pure module subroutine stdlib_zgeqr( m, n, a, lda, t, tsize, work, lwork,info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n, tsize, lwork
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: t(*), work(*)
     end subroutine stdlib_zgeqr


     pure module subroutine stdlib_I64_sgeqr( m, n, a, lda, t, tsize, work, lwork,info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, m, n, tsize, lwork
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: t(*), work(*)
     end subroutine stdlib_I64_sgeqr

     pure module subroutine stdlib_I64_dgeqr( m, n, a, lda, t, tsize, work, lwork,info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, m, n, tsize, lwork
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: t(*), work(*)
     end subroutine stdlib_I64_dgeqr


     pure module subroutine stdlib_I64_cgeqr( m, n, a, lda, t, tsize, work, lwork,info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, m, n, tsize, lwork
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: t(*), work(*)
     end subroutine stdlib_I64_cgeqr

     pure module subroutine stdlib_I64_zgeqr( m, n, a, lda, t, tsize, work, lwork,info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, m, n, tsize, lwork
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: t(*), work(*)
     end subroutine stdlib_I64_zgeqr


end interface 


interface 
     pure module subroutine stdlib_sgemqr( side, trans, m, n, k, a, lda, t, tsize,c, ldc, work, lwork, &
               info )
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n, k, tsize, lwork, ldc
           real(sp), intent(in) :: a(lda,*), t(*)
           real(sp), intent(inout) :: c(ldc,*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_sgemqr

     pure module subroutine stdlib_dgemqr( side, trans, m, n, k, a, lda, t, tsize,c, ldc, work, lwork, &
               info )
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n, k, tsize, lwork, ldc
           real(dp), intent(in) :: a(lda,*), t(*)
           real(dp), intent(inout) :: c(ldc,*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_dgemqr


     pure module subroutine stdlib_cgemqr( side, trans, m, n, k, a, lda, t, tsize,c, ldc, work, lwork, &
               info )
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n, k, tsize, lwork, ldc
           complex(sp), intent(in) :: a(lda,*), t(*)
           complex(sp), intent(inout) :: c(ldc,*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_cgemqr

     pure module subroutine stdlib_zgemqr( side, trans, m, n, k, a, lda, t, tsize,c, ldc, work, lwork, &
               info )
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n, k, tsize, lwork, ldc
           complex(dp), intent(in) :: a(lda,*), t(*)
           complex(dp), intent(inout) :: c(ldc,*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_zgemqr


     pure module subroutine stdlib_I64_sgemqr( side, trans, m, n, k, a, lda, t, tsize,c, ldc, work, lwork, &
               info )
           character, intent(in) :: side, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, m, n, k, tsize, lwork, ldc
           real(sp), intent(in) :: a(lda,*), t(*)
           real(sp), intent(inout) :: c(ldc,*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_I64_sgemqr

     pure module subroutine stdlib_I64_dgemqr( side, trans, m, n, k, a, lda, t, tsize,c, ldc, work, lwork, &
               info )
           character, intent(in) :: side, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, m, n, k, tsize, lwork, ldc
           real(dp), intent(in) :: a(lda,*), t(*)
           real(dp), intent(inout) :: c(ldc,*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_I64_dgemqr


     pure module subroutine stdlib_I64_cgemqr( side, trans, m, n, k, a, lda, t, tsize,c, ldc, work, lwork, &
               info )
           character, intent(in) :: side, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, m, n, k, tsize, lwork, ldc
           complex(sp), intent(in) :: a(lda,*), t(*)
           complex(sp), intent(inout) :: c(ldc,*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_I64_cgemqr

     pure module subroutine stdlib_I64_zgemqr( side, trans, m, n, k, a, lda, t, tsize,c, ldc, work, lwork, &
               info )
           character, intent(in) :: side, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, m, n, k, tsize, lwork, ldc
           complex(dp), intent(in) :: a(lda,*), t(*)
           complex(dp), intent(inout) :: c(ldc,*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_I64_zgemqr


end interface 


interface 
     pure module subroutine stdlib_sgeqrf( m, n, a, lda, tau, work, lwork, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, m, n
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_sgeqrf

     pure module subroutine stdlib_dgeqrf( m, n, a, lda, tau, work, lwork, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, m, n
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_dgeqrf


     pure module subroutine stdlib_cgeqrf( m, n, a, lda, tau, work, lwork, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, m, n
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_cgeqrf

     pure module subroutine stdlib_zgeqrf( m, n, a, lda, tau, work, lwork, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, m, n
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_zgeqrf


     pure module subroutine stdlib_I64_sgeqrf( m, n, a, lda, tau, work, lwork, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, lwork, m, n
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_I64_sgeqrf

     pure module subroutine stdlib_I64_dgeqrf( m, n, a, lda, tau, work, lwork, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, lwork, m, n
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_I64_dgeqrf


     pure module subroutine stdlib_I64_cgeqrf( m, n, a, lda, tau, work, lwork, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, lwork, m, n
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_I64_cgeqrf

     pure module subroutine stdlib_I64_zgeqrf( m, n, a, lda, tau, work, lwork, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, lwork, m, n
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_I64_zgeqrf


end interface 


interface 
     pure module subroutine stdlib_sgeqr2( m, n, a, lda, tau, work, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_sgeqr2

     pure module subroutine stdlib_dgeqr2( m, n, a, lda, tau, work, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_dgeqr2


     pure module subroutine stdlib_cgeqr2( m, n, a, lda, tau, work, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_cgeqr2

     pure module subroutine stdlib_zgeqr2( m, n, a, lda, tau, work, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_zgeqr2


     pure module subroutine stdlib_I64_sgeqr2( m, n, a, lda, tau, work, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, m, n
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_I64_sgeqr2

     pure module subroutine stdlib_I64_dgeqr2( m, n, a, lda, tau, work, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, m, n
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_I64_dgeqr2


     pure module subroutine stdlib_I64_cgeqr2( m, n, a, lda, tau, work, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, m, n
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_I64_cgeqr2

     pure module subroutine stdlib_I64_zgeqr2( m, n, a, lda, tau, work, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, m, n
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_I64_zgeqr2


end interface 


interface 
     pure module subroutine stdlib_cungqr( m, n, k, a, lda, tau, work, lwork, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, lwork, m, n
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(in) :: tau(*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_cungqr

     pure module subroutine stdlib_zungqr( m, n, k, a, lda, tau, work, lwork, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, lwork, m, n
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(in) :: tau(*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_zungqr


     pure module subroutine stdlib_I64_cungqr( m, n, k, a, lda, tau, work, lwork, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, lda, lwork, m, n
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(in) :: tau(*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_I64_cungqr

     pure module subroutine stdlib_I64_zungqr( m, n, k, a, lda, tau, work, lwork, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, lda, lwork, m, n
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(in) :: tau(*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_I64_zungqr


end interface 


interface 
     pure module subroutine stdlib_cung2r( m, n, k, a, lda, tau, work, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, m, n
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(in) :: tau(*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_cung2r

     pure module subroutine stdlib_zung2r( m, n, k, a, lda, tau, work, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, m, n
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(in) :: tau(*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_zung2r


     pure module subroutine stdlib_I64_cung2r( m, n, k, a, lda, tau, work, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, lda, m, n
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(in) :: tau(*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_I64_cung2r

     pure module subroutine stdlib_I64_zung2r( m, n, k, a, lda, tau, work, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, lda, m, n
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(in) :: tau(*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_I64_zung2r


end interface 


interface 
     pure module subroutine stdlib_cunmqr( side, trans, m, n, k, a, lda, tau, c, ldc,work, lwork, info )
               
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, ldc, lwork, m, n
           complex(sp), intent(inout) :: a(lda,*), c(ldc,*)
           complex(sp), intent(in) :: tau(*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_cunmqr

     pure module subroutine stdlib_zunmqr( side, trans, m, n, k, a, lda, tau, c, ldc,work, lwork, info )
               
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, ldc, lwork, m, n
           complex(dp), intent(inout) :: a(lda,*), c(ldc,*)
           complex(dp), intent(in) :: tau(*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_zunmqr


     pure module subroutine stdlib_I64_cunmqr( side, trans, m, n, k, a, lda, tau, c, ldc,work, lwork, info )
               
           character, intent(in) :: side, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, lda, ldc, lwork, m, n
           complex(sp), intent(inout) :: a(lda,*), c(ldc,*)
           complex(sp), intent(in) :: tau(*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_I64_cunmqr

     pure module subroutine stdlib_I64_zunmqr( side, trans, m, n, k, a, lda, tau, c, ldc,work, lwork, info )
               
           character, intent(in) :: side, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, lda, ldc, lwork, m, n
           complex(dp), intent(inout) :: a(lda,*), c(ldc,*)
           complex(dp), intent(in) :: tau(*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_I64_zunmqr


end interface 


interface 
     pure module subroutine stdlib_cunm2r( side, trans, m, n, k, a, lda, tau, c, ldc,work, info )
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, ldc, m, n
           complex(sp), intent(inout) :: a(lda,*), c(ldc,*)
           complex(sp), intent(in) :: tau(*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_cunm2r

     pure module subroutine stdlib_zunm2r( side, trans, m, n, k, a, lda, tau, c, ldc,work, info )
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, ldc, m, n
           complex(dp), intent(inout) :: a(lda,*), c(ldc,*)
           complex(dp), intent(in) :: tau(*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_zunm2r


     pure module subroutine stdlib_I64_cunm2r( side, trans, m, n, k, a, lda, tau, c, ldc,work, info )
           character, intent(in) :: side, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, lda, ldc, m, n
           complex(sp), intent(inout) :: a(lda,*), c(ldc,*)
           complex(sp), intent(in) :: tau(*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_I64_cunm2r

     pure module subroutine stdlib_I64_zunm2r( side, trans, m, n, k, a, lda, tau, c, ldc,work, info )
           character, intent(in) :: side, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, lda, ldc, m, n
           complex(dp), intent(inout) :: a(lda,*), c(ldc,*)
           complex(dp), intent(in) :: tau(*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_I64_zunm2r


end interface 


interface 
     pure module subroutine stdlib_sorgqr( m, n, k, a, lda, tau, work, lwork, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, lwork, m, n
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(in) :: tau(*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_sorgqr

     pure module subroutine stdlib_dorgqr( m, n, k, a, lda, tau, work, lwork, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, lwork, m, n
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(in) :: tau(*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_dorgqr


     pure module subroutine stdlib_I64_sorgqr( m, n, k, a, lda, tau, work, lwork, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, lda, lwork, m, n
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(in) :: tau(*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_I64_sorgqr

     pure module subroutine stdlib_I64_dorgqr( m, n, k, a, lda, tau, work, lwork, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, lda, lwork, m, n
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(in) :: tau(*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_I64_dorgqr


end interface 


interface 
     pure module subroutine stdlib_sorg2r( m, n, k, a, lda, tau, work, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, m, n
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(in) :: tau(*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_sorg2r

     pure module subroutine stdlib_dorg2r( m, n, k, a, lda, tau, work, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, m, n
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(in) :: tau(*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_dorg2r


     pure module subroutine stdlib_I64_sorg2r( m, n, k, a, lda, tau, work, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, lda, m, n
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(in) :: tau(*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_I64_sorg2r

     pure module subroutine stdlib_I64_dorg2r( m, n, k, a, lda, tau, work, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, lda, m, n
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(in) :: tau(*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_I64_dorg2r


end interface 


interface 
     pure module subroutine stdlib_sormqr( side, trans, m, n, k, a, lda, tau, c, ldc,work, lwork, info )
               
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, ldc, lwork, m, n
           real(sp), intent(inout) :: a(lda,*), c(ldc,*)
           real(sp), intent(in) :: tau(*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_sormqr

     pure module subroutine stdlib_dormqr( side, trans, m, n, k, a, lda, tau, c, ldc,work, lwork, info )
               
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, ldc, lwork, m, n
           real(dp), intent(inout) :: a(lda,*), c(ldc,*)
           real(dp), intent(in) :: tau(*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_dormqr


     pure module subroutine stdlib_I64_sormqr( side, trans, m, n, k, a, lda, tau, c, ldc,work, lwork, info )
               
           character, intent(in) :: side, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, lda, ldc, lwork, m, n
           real(sp), intent(inout) :: a(lda,*), c(ldc,*)
           real(sp), intent(in) :: tau(*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_I64_sormqr

     pure module subroutine stdlib_I64_dormqr( side, trans, m, n, k, a, lda, tau, c, ldc,work, lwork, info )
               
           character, intent(in) :: side, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, lda, ldc, lwork, m, n
           real(dp), intent(inout) :: a(lda,*), c(ldc,*)
           real(dp), intent(in) :: tau(*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_I64_dormqr


end interface 


interface 
     pure module subroutine stdlib_sorm2r( side, trans, m, n, k, a, lda, tau, c, ldc,work, info )
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, ldc, m, n
           real(sp), intent(inout) :: a(lda,*), c(ldc,*)
           real(sp), intent(in) :: tau(*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_sorm2r

     pure module subroutine stdlib_dorm2r( side, trans, m, n, k, a, lda, tau, c, ldc,work, info )
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, ldc, m, n
           real(dp), intent(inout) :: a(lda,*), c(ldc,*)
           real(dp), intent(in) :: tau(*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_dorm2r


     pure module subroutine stdlib_I64_sorm2r( side, trans, m, n, k, a, lda, tau, c, ldc,work, info )
           character, intent(in) :: side, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, lda, ldc, m, n
           real(sp), intent(inout) :: a(lda,*), c(ldc,*)
           real(sp), intent(in) :: tau(*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_I64_sorm2r

     pure module subroutine stdlib_I64_dorm2r( side, trans, m, n, k, a, lda, tau, c, ldc,work, info )
           character, intent(in) :: side, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, lda, ldc, m, n
           real(dp), intent(inout) :: a(lda,*), c(ldc,*)
           real(dp), intent(in) :: tau(*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_I64_dorm2r


end interface 


interface 
     pure module subroutine stdlib_sgeqrt( m, n, nb, a, lda, t, ldt, work, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldt, m, n, nb
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: t(ldt,*), work(*)
     end subroutine stdlib_sgeqrt

     pure module subroutine stdlib_dgeqrt( m, n, nb, a, lda, t, ldt, work, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldt, m, n, nb
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: t(ldt,*), work(*)
     end subroutine stdlib_dgeqrt


     pure module subroutine stdlib_cgeqrt( m, n, nb, a, lda, t, ldt, work, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldt, m, n, nb
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: t(ldt,*), work(*)
     end subroutine stdlib_cgeqrt

     pure module subroutine stdlib_zgeqrt( m, n, nb, a, lda, t, ldt, work, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldt, m, n, nb
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: t(ldt,*), work(*)
     end subroutine stdlib_zgeqrt


     pure module subroutine stdlib_I64_sgeqrt( m, n, nb, a, lda, t, ldt, work, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldt, m, n, nb
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: t(ldt,*), work(*)
     end subroutine stdlib_I64_sgeqrt

     pure module subroutine stdlib_I64_dgeqrt( m, n, nb, a, lda, t, ldt, work, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldt, m, n, nb
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: t(ldt,*), work(*)
     end subroutine stdlib_I64_dgeqrt


     pure module subroutine stdlib_I64_cgeqrt( m, n, nb, a, lda, t, ldt, work, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldt, m, n, nb
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: t(ldt,*), work(*)
     end subroutine stdlib_I64_cgeqrt

     pure module subroutine stdlib_I64_zgeqrt( m, n, nb, a, lda, t, ldt, work, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldt, m, n, nb
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: t(ldt,*), work(*)
     end subroutine stdlib_I64_zgeqrt


end interface 


interface 
     pure module subroutine stdlib_sgeqrt2( m, n, a, lda, t, ldt, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldt, m, n
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: t(ldt,*)
     end subroutine stdlib_sgeqrt2

     pure module subroutine stdlib_dgeqrt2( m, n, a, lda, t, ldt, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldt, m, n
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: t(ldt,*)
     end subroutine stdlib_dgeqrt2


     pure module subroutine stdlib_cgeqrt2( m, n, a, lda, t, ldt, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldt, m, n
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: t(ldt,*)
     end subroutine stdlib_cgeqrt2

     pure module subroutine stdlib_zgeqrt2( m, n, a, lda, t, ldt, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldt, m, n
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: t(ldt,*)
     end subroutine stdlib_zgeqrt2


     pure module subroutine stdlib_I64_sgeqrt2( m, n, a, lda, t, ldt, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldt, m, n
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: t(ldt,*)
     end subroutine stdlib_I64_sgeqrt2

     pure module subroutine stdlib_I64_dgeqrt2( m, n, a, lda, t, ldt, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldt, m, n
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: t(ldt,*)
     end subroutine stdlib_I64_dgeqrt2


     pure module subroutine stdlib_I64_cgeqrt2( m, n, a, lda, t, ldt, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldt, m, n
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: t(ldt,*)
     end subroutine stdlib_I64_cgeqrt2

     pure module subroutine stdlib_I64_zgeqrt2( m, n, a, lda, t, ldt, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldt, m, n
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: t(ldt,*)
     end subroutine stdlib_I64_zgeqrt2


end interface 


interface 
     pure recursive module subroutine stdlib_sgeqrt3( m, n, a, lda, t, ldt, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n, ldt
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: t(ldt,*)
     end subroutine stdlib_sgeqrt3

     pure recursive module subroutine stdlib_dgeqrt3( m, n, a, lda, t, ldt, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n, ldt
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: t(ldt,*)
     end subroutine stdlib_dgeqrt3


     pure recursive module subroutine stdlib_cgeqrt3( m, n, a, lda, t, ldt, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n, ldt
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: t(ldt,*)
     end subroutine stdlib_cgeqrt3

     pure recursive module subroutine stdlib_zgeqrt3( m, n, a, lda, t, ldt, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n, ldt
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: t(ldt,*)
     end subroutine stdlib_zgeqrt3


     pure recursive module subroutine stdlib_I64_sgeqrt3( m, n, a, lda, t, ldt, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, m, n, ldt
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: t(ldt,*)
     end subroutine stdlib_I64_sgeqrt3

     pure recursive module subroutine stdlib_I64_dgeqrt3( m, n, a, lda, t, ldt, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, m, n, ldt
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: t(ldt,*)
     end subroutine stdlib_I64_dgeqrt3


     pure recursive module subroutine stdlib_I64_cgeqrt3( m, n, a, lda, t, ldt, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, m, n, ldt
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: t(ldt,*)
     end subroutine stdlib_I64_cgeqrt3

     pure recursive module subroutine stdlib_I64_zgeqrt3( m, n, a, lda, t, ldt, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, m, n, ldt
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: t(ldt,*)
     end subroutine stdlib_I64_zgeqrt3


end interface 


interface 
     pure module subroutine stdlib_sgemqrt( side, trans, m, n, k, nb, v, ldv, t, ldt,c, ldc, work, info )
               
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, ldv, ldc, m, n, nb, ldt
           real(sp), intent(in) :: v(ldv,*), t(ldt,*)
           real(sp), intent(inout) :: c(ldc,*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_sgemqrt

     pure module subroutine stdlib_dgemqrt( side, trans, m, n, k, nb, v, ldv, t, ldt,c, ldc, work, info )
               
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, ldv, ldc, m, n, nb, ldt
           real(dp), intent(in) :: v(ldv,*), t(ldt,*)
           real(dp), intent(inout) :: c(ldc,*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_dgemqrt


     pure module subroutine stdlib_cgemqrt( side, trans, m, n, k, nb, v, ldv, t, ldt,c, ldc, work, info )
               
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, ldv, ldc, m, n, nb, ldt
           complex(sp), intent(in) :: v(ldv,*), t(ldt,*)
           complex(sp), intent(inout) :: c(ldc,*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_cgemqrt

     pure module subroutine stdlib_zgemqrt( side, trans, m, n, k, nb, v, ldv, t, ldt,c, ldc, work, info )
               
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, ldv, ldc, m, n, nb, ldt
           complex(dp), intent(in) :: v(ldv,*), t(ldt,*)
           complex(dp), intent(inout) :: c(ldc,*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_zgemqrt


     pure module subroutine stdlib_I64_sgemqrt( side, trans, m, n, k, nb, v, ldv, t, ldt,c, ldc, work, info )
               
           character, intent(in) :: side, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, ldv, ldc, m, n, nb, ldt
           real(sp), intent(in) :: v(ldv,*), t(ldt,*)
           real(sp), intent(inout) :: c(ldc,*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_I64_sgemqrt

     pure module subroutine stdlib_I64_dgemqrt( side, trans, m, n, k, nb, v, ldv, t, ldt,c, ldc, work, info )
               
           character, intent(in) :: side, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, ldv, ldc, m, n, nb, ldt
           real(dp), intent(in) :: v(ldv,*), t(ldt,*)
           real(dp), intent(inout) :: c(ldc,*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_I64_dgemqrt


     pure module subroutine stdlib_I64_cgemqrt( side, trans, m, n, k, nb, v, ldv, t, ldt,c, ldc, work, info )
               
           character, intent(in) :: side, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, ldv, ldc, m, n, nb, ldt
           complex(sp), intent(in) :: v(ldv,*), t(ldt,*)
           complex(sp), intent(inout) :: c(ldc,*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_I64_cgemqrt

     pure module subroutine stdlib_I64_zgemqrt( side, trans, m, n, k, nb, v, ldv, t, ldt,c, ldc, work, info )
               
           character, intent(in) :: side, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, ldv, ldc, m, n, nb, ldt
           complex(dp), intent(in) :: v(ldv,*), t(ldt,*)
           complex(dp), intent(inout) :: c(ldc,*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_I64_zgemqrt


end interface 


interface 
     module subroutine stdlib_sgeqrfp( m, n, a, lda, tau, work, lwork, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, m, n
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_sgeqrfp

     module subroutine stdlib_dgeqrfp( m, n, a, lda, tau, work, lwork, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, m, n
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_dgeqrfp


     module subroutine stdlib_cgeqrfp( m, n, a, lda, tau, work, lwork, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, m, n
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_cgeqrfp

     module subroutine stdlib_zgeqrfp( m, n, a, lda, tau, work, lwork, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, m, n
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_zgeqrfp


     module subroutine stdlib_I64_sgeqrfp( m, n, a, lda, tau, work, lwork, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, lwork, m, n
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_I64_sgeqrfp

     module subroutine stdlib_I64_dgeqrfp( m, n, a, lda, tau, work, lwork, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, lwork, m, n
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_I64_dgeqrfp


     module subroutine stdlib_I64_cgeqrfp( m, n, a, lda, tau, work, lwork, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, lwork, m, n
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_I64_cgeqrfp

     module subroutine stdlib_I64_zgeqrfp( m, n, a, lda, tau, work, lwork, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, lwork, m, n
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_I64_zgeqrfp


end interface 


interface 
     module subroutine stdlib_sgeqr2p( m, n, a, lda, tau, work, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_sgeqr2p

     module subroutine stdlib_dgeqr2p( m, n, a, lda, tau, work, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_dgeqr2p


     module subroutine stdlib_cgeqr2p( m, n, a, lda, tau, work, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_cgeqr2p

     module subroutine stdlib_zgeqr2p( m, n, a, lda, tau, work, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_zgeqr2p


     module subroutine stdlib_I64_sgeqr2p( m, n, a, lda, tau, work, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, m, n
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_I64_sgeqr2p

     module subroutine stdlib_I64_dgeqr2p( m, n, a, lda, tau, work, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, m, n
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_I64_dgeqr2p


     module subroutine stdlib_I64_cgeqr2p( m, n, a, lda, tau, work, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, m, n
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_I64_cgeqr2p

     module subroutine stdlib_I64_zgeqr2p( m, n, a, lda, tau, work, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, m, n
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_I64_zgeqr2p


end interface 


interface 
     pure module subroutine stdlib_sgeqp3( m, n, a, lda, jpvt, tau, work, lwork, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, m, n
           integer(ilp), intent(inout) :: jpvt(*)
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_sgeqp3

     pure module subroutine stdlib_dgeqp3( m, n, a, lda, jpvt, tau, work, lwork, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, m, n
           integer(ilp), intent(inout) :: jpvt(*)
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_dgeqp3


     pure module subroutine stdlib_cgeqp3( m, n, a, lda, jpvt, tau, work, lwork, rwork,info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, m, n
           integer(ilp), intent(inout) :: jpvt(*)
           real(sp), intent(out) :: rwork(*)
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_cgeqp3

     pure module subroutine stdlib_zgeqp3( m, n, a, lda, jpvt, tau, work, lwork, rwork,info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, m, n
           integer(ilp), intent(inout) :: jpvt(*)
           real(dp), intent(out) :: rwork(*)
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_zgeqp3


     pure module subroutine stdlib_I64_sgeqp3( m, n, a, lda, jpvt, tau, work, lwork, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, lwork, m, n
           integer(ilp64), intent(inout) :: jpvt(*)
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_I64_sgeqp3

     pure module subroutine stdlib_I64_dgeqp3( m, n, a, lda, jpvt, tau, work, lwork, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, lwork, m, n
           integer(ilp64), intent(inout) :: jpvt(*)
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_I64_dgeqp3


     pure module subroutine stdlib_I64_cgeqp3( m, n, a, lda, jpvt, tau, work, lwork, rwork,info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, lwork, m, n
           integer(ilp64), intent(inout) :: jpvt(*)
           real(sp), intent(out) :: rwork(*)
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_I64_cgeqp3

     pure module subroutine stdlib_I64_zgeqp3( m, n, a, lda, jpvt, tau, work, lwork, rwork,info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, lwork, m, n
           integer(ilp64), intent(inout) :: jpvt(*)
           real(dp), intent(out) :: rwork(*)
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_I64_zgeqp3


end interface 


interface 
     pure module subroutine stdlib_slaqp2( m, n, offset, a, lda, jpvt, tau, vn1, vn2,work )
           integer(ilp), intent(in) :: lda, m, n, offset
           integer(ilp), intent(inout) :: jpvt(*)
           real(sp), intent(inout) :: a(lda,*), vn1(*), vn2(*)
           real(sp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_slaqp2

     pure module subroutine stdlib_dlaqp2( m, n, offset, a, lda, jpvt, tau, vn1, vn2,work )
           integer(ilp), intent(in) :: lda, m, n, offset
           integer(ilp), intent(inout) :: jpvt(*)
           real(dp), intent(inout) :: a(lda,*), vn1(*), vn2(*)
           real(dp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_dlaqp2


     pure module subroutine stdlib_claqp2( m, n, offset, a, lda, jpvt, tau, vn1, vn2,work )
           integer(ilp), intent(in) :: lda, m, n, offset
           integer(ilp), intent(inout) :: jpvt(*)
           real(sp), intent(inout) :: vn1(*), vn2(*)
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_claqp2

     pure module subroutine stdlib_zlaqp2( m, n, offset, a, lda, jpvt, tau, vn1, vn2,work )
           integer(ilp), intent(in) :: lda, m, n, offset
           integer(ilp), intent(inout) :: jpvt(*)
           real(dp), intent(inout) :: vn1(*), vn2(*)
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_zlaqp2


     pure module subroutine stdlib_I64_slaqp2( m, n, offset, a, lda, jpvt, tau, vn1, vn2,work )
           integer(ilp64), intent(in) :: lda, m, n, offset
           integer(ilp64), intent(inout) :: jpvt(*)
           real(sp), intent(inout) :: a(lda,*), vn1(*), vn2(*)
           real(sp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_I64_slaqp2

     pure module subroutine stdlib_I64_dlaqp2( m, n, offset, a, lda, jpvt, tau, vn1, vn2,work )
           integer(ilp64), intent(in) :: lda, m, n, offset
           integer(ilp64), intent(inout) :: jpvt(*)
           real(dp), intent(inout) :: a(lda,*), vn1(*), vn2(*)
           real(dp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_I64_dlaqp2


     pure module subroutine stdlib_I64_claqp2( m, n, offset, a, lda, jpvt, tau, vn1, vn2,work )
           integer(ilp64), intent(in) :: lda, m, n, offset
           integer(ilp64), intent(inout) :: jpvt(*)
           real(sp), intent(inout) :: vn1(*), vn2(*)
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_I64_claqp2

     pure module subroutine stdlib_I64_zlaqp2( m, n, offset, a, lda, jpvt, tau, vn1, vn2,work )
           integer(ilp64), intent(in) :: lda, m, n, offset
           integer(ilp64), intent(inout) :: jpvt(*)
           real(dp), intent(inout) :: vn1(*), vn2(*)
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_I64_zlaqp2


end interface 


interface 
     pure module subroutine stdlib_slaqps( m, n, offset, nb, kb, a, lda, jpvt, tau, vn1,vn2, auxv, f, &
               ldf )
           integer(ilp), intent(out) :: kb
           integer(ilp), intent(in) :: lda, ldf, m, n, nb, offset
           integer(ilp), intent(inout) :: jpvt(*)
           real(sp), intent(inout) :: a(lda,*), auxv(*), f(ldf,*), vn1(*), vn2(*)
           real(sp), intent(out) :: tau(*)
     end subroutine stdlib_slaqps

     pure module subroutine stdlib_dlaqps( m, n, offset, nb, kb, a, lda, jpvt, tau, vn1,vn2, auxv, f, &
               ldf )
           integer(ilp), intent(out) :: kb
           integer(ilp), intent(in) :: lda, ldf, m, n, nb, offset
           integer(ilp), intent(inout) :: jpvt(*)
           real(dp), intent(inout) :: a(lda,*), auxv(*), f(ldf,*), vn1(*), vn2(*)
           real(dp), intent(out) :: tau(*)
     end subroutine stdlib_dlaqps


     pure module subroutine stdlib_claqps( m, n, offset, nb, kb, a, lda, jpvt, tau, vn1,vn2, auxv, f, &
               ldf )
           integer(ilp), intent(out) :: kb
           integer(ilp), intent(in) :: lda, ldf, m, n, nb, offset
           integer(ilp), intent(inout) :: jpvt(*)
           real(sp), intent(inout) :: vn1(*), vn2(*)
           complex(sp), intent(inout) :: a(lda,*), auxv(*), f(ldf,*)
           complex(sp), intent(out) :: tau(*)
     end subroutine stdlib_claqps

     pure module subroutine stdlib_zlaqps( m, n, offset, nb, kb, a, lda, jpvt, tau, vn1,vn2, auxv, f, &
               ldf )
           integer(ilp), intent(out) :: kb
           integer(ilp), intent(in) :: lda, ldf, m, n, nb, offset
           integer(ilp), intent(inout) :: jpvt(*)
           real(dp), intent(inout) :: vn1(*), vn2(*)
           complex(dp), intent(inout) :: a(lda,*), auxv(*), f(ldf,*)
           complex(dp), intent(out) :: tau(*)
     end subroutine stdlib_zlaqps


     pure module subroutine stdlib_I64_slaqps( m, n, offset, nb, kb, a, lda, jpvt, tau, vn1,vn2, auxv, f, &
               ldf )
           integer(ilp64), intent(out) :: kb
           integer(ilp64), intent(in) :: lda, ldf, m, n, nb, offset
           integer(ilp64), intent(inout) :: jpvt(*)
           real(sp), intent(inout) :: a(lda,*), auxv(*), f(ldf,*), vn1(*), vn2(*)
           real(sp), intent(out) :: tau(*)
     end subroutine stdlib_I64_slaqps

     pure module subroutine stdlib_I64_dlaqps( m, n, offset, nb, kb, a, lda, jpvt, tau, vn1,vn2, auxv, f, &
               ldf )
           integer(ilp64), intent(out) :: kb
           integer(ilp64), intent(in) :: lda, ldf, m, n, nb, offset
           integer(ilp64), intent(inout) :: jpvt(*)
           real(dp), intent(inout) :: a(lda,*), auxv(*), f(ldf,*), vn1(*), vn2(*)
           real(dp), intent(out) :: tau(*)
     end subroutine stdlib_I64_dlaqps


     pure module subroutine stdlib_I64_claqps( m, n, offset, nb, kb, a, lda, jpvt, tau, vn1,vn2, auxv, f, &
               ldf )
           integer(ilp64), intent(out) :: kb
           integer(ilp64), intent(in) :: lda, ldf, m, n, nb, offset
           integer(ilp64), intent(inout) :: jpvt(*)
           real(sp), intent(inout) :: vn1(*), vn2(*)
           complex(sp), intent(inout) :: a(lda,*), auxv(*), f(ldf,*)
           complex(sp), intent(out) :: tau(*)
     end subroutine stdlib_I64_claqps

     pure module subroutine stdlib_I64_zlaqps( m, n, offset, nb, kb, a, lda, jpvt, tau, vn1,vn2, auxv, f, &
               ldf )
           integer(ilp64), intent(out) :: kb
           integer(ilp64), intent(in) :: lda, ldf, m, n, nb, offset
           integer(ilp64), intent(inout) :: jpvt(*)
           real(dp), intent(inout) :: vn1(*), vn2(*)
           complex(dp), intent(inout) :: a(lda,*), auxv(*), f(ldf,*)
           complex(dp), intent(out) :: tau(*)
     end subroutine stdlib_I64_zlaqps


end interface 


interface 
     pure module subroutine stdlib_slatsqr( m, n, mb, nb, a, lda, t, ldt, work,lwork, info)
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n, mb, nb, ldt, lwork
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: work(*), t(ldt,*)
     end subroutine stdlib_slatsqr

     pure module subroutine stdlib_dlatsqr( m, n, mb, nb, a, lda, t, ldt, work,lwork, info)
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n, mb, nb, ldt, lwork
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: work(*), t(ldt,*)
     end subroutine stdlib_dlatsqr


     pure module subroutine stdlib_clatsqr( m, n, mb, nb, a, lda, t, ldt, work,lwork, info)
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n, mb, nb, ldt, lwork
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: work(*), t(ldt,*)
     end subroutine stdlib_clatsqr

     pure module subroutine stdlib_zlatsqr( m, n, mb, nb, a, lda, t, ldt, work,lwork, info)
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n, mb, nb, ldt, lwork
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: work(*), t(ldt,*)
     end subroutine stdlib_zlatsqr


     pure module subroutine stdlib_I64_slatsqr( m, n, mb, nb, a, lda, t, ldt, work,lwork, info)
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, m, n, mb, nb, ldt, lwork
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: work(*), t(ldt,*)
     end subroutine stdlib_I64_slatsqr

     pure module subroutine stdlib_I64_dlatsqr( m, n, mb, nb, a, lda, t, ldt, work,lwork, info)
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, m, n, mb, nb, ldt, lwork
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: work(*), t(ldt,*)
     end subroutine stdlib_I64_dlatsqr


     pure module subroutine stdlib_I64_clatsqr( m, n, mb, nb, a, lda, t, ldt, work,lwork, info)
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, m, n, mb, nb, ldt, lwork
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: work(*), t(ldt,*)
     end subroutine stdlib_I64_clatsqr

     pure module subroutine stdlib_I64_zlatsqr( m, n, mb, nb, a, lda, t, ldt, work,lwork, info)
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, m, n, mb, nb, ldt, lwork
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: work(*), t(ldt,*)
     end subroutine stdlib_I64_zlatsqr


end interface 


interface 
     pure module subroutine stdlib_cungtsqr( m, n, mb, nb, a, lda, t, ldt, work, lwork,info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldt, lwork, m, n, mb, nb
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(in) :: t(ldt,*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_cungtsqr

     pure module subroutine stdlib_zungtsqr( m, n, mb, nb, a, lda, t, ldt, work, lwork,info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldt, lwork, m, n, mb, nb
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(in) :: t(ldt,*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_zungtsqr


     pure module subroutine stdlib_I64_cungtsqr( m, n, mb, nb, a, lda, t, ldt, work, lwork,info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldt, lwork, m, n, mb, nb
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(in) :: t(ldt,*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_I64_cungtsqr

     pure module subroutine stdlib_I64_zungtsqr( m, n, mb, nb, a, lda, t, ldt, work, lwork,info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldt, lwork, m, n, mb, nb
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(in) :: t(ldt,*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_I64_zungtsqr


end interface 


interface 
     pure module subroutine stdlib_cungtsqr_row( m, n, mb, nb, a, lda, t, ldt, work,lwork, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldt, lwork, m, n, mb, nb
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(in) :: t(ldt,*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_cungtsqr_row

     pure module subroutine stdlib_zungtsqr_row( m, n, mb, nb, a, lda, t, ldt, work,lwork, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldt, lwork, m, n, mb, nb
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(in) :: t(ldt,*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_zungtsqr_row


     pure module subroutine stdlib_I64_cungtsqr_row( m, n, mb, nb, a, lda, t, ldt, work,lwork, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldt, lwork, m, n, mb, nb
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(in) :: t(ldt,*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_I64_cungtsqr_row

     pure module subroutine stdlib_I64_zungtsqr_row( m, n, mb, nb, a, lda, t, ldt, work,lwork, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldt, lwork, m, n, mb, nb
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(in) :: t(ldt,*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_I64_zungtsqr_row


end interface 


interface 
     pure module subroutine stdlib_sorgtsqr( m, n, mb, nb, a, lda, t, ldt, work, lwork,info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldt, lwork, m, n, mb, nb
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(in) :: t(ldt,*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_sorgtsqr

     pure module subroutine stdlib_dorgtsqr( m, n, mb, nb, a, lda, t, ldt, work, lwork,info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldt, lwork, m, n, mb, nb
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(in) :: t(ldt,*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_dorgtsqr


     pure module subroutine stdlib_I64_sorgtsqr( m, n, mb, nb, a, lda, t, ldt, work, lwork,info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldt, lwork, m, n, mb, nb
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(in) :: t(ldt,*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_I64_sorgtsqr

     pure module subroutine stdlib_I64_dorgtsqr( m, n, mb, nb, a, lda, t, ldt, work, lwork,info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldt, lwork, m, n, mb, nb
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(in) :: t(ldt,*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_I64_dorgtsqr


end interface 


interface 
     pure module subroutine stdlib_sorgtsqr_row( m, n, mb, nb, a, lda, t, ldt, work,lwork, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldt, lwork, m, n, mb, nb
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(in) :: t(ldt,*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_sorgtsqr_row

     pure module subroutine stdlib_dorgtsqr_row( m, n, mb, nb, a, lda, t, ldt, work,lwork, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldt, lwork, m, n, mb, nb
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(in) :: t(ldt,*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_dorgtsqr_row


     pure module subroutine stdlib_I64_sorgtsqr_row( m, n, mb, nb, a, lda, t, ldt, work,lwork, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldt, lwork, m, n, mb, nb
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(in) :: t(ldt,*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_I64_sorgtsqr_row

     pure module subroutine stdlib_I64_dorgtsqr_row( m, n, mb, nb, a, lda, t, ldt, work,lwork, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldt, lwork, m, n, mb, nb
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(in) :: t(ldt,*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_I64_dorgtsqr_row


end interface 


interface 
     pure module subroutine stdlib_slarfb_gett( ident, m, n, k, t, ldt, a, lda, b, ldb,work, ldwork )
               
           character, intent(in) :: ident
           integer(ilp), intent(in) :: k, lda, ldb, ldt, ldwork, m, n
           real(sp), intent(inout) :: a(lda,*), b(ldb,*)
           real(sp), intent(in) :: t(ldt,*)
           real(sp), intent(out) :: work(ldwork,*)
     end subroutine stdlib_slarfb_gett

     pure module subroutine stdlib_dlarfb_gett( ident, m, n, k, t, ldt, a, lda, b, ldb,work, ldwork )
               
           character, intent(in) :: ident
           integer(ilp), intent(in) :: k, lda, ldb, ldt, ldwork, m, n
           real(dp), intent(inout) :: a(lda,*), b(ldb,*)
           real(dp), intent(in) :: t(ldt,*)
           real(dp), intent(out) :: work(ldwork,*)
     end subroutine stdlib_dlarfb_gett


     pure module subroutine stdlib_clarfb_gett( ident, m, n, k, t, ldt, a, lda, b, ldb,work, ldwork )
               
           character, intent(in) :: ident
           integer(ilp), intent(in) :: k, lda, ldb, ldt, ldwork, m, n
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(sp), intent(in) :: t(ldt,*)
           complex(sp), intent(out) :: work(ldwork,*)
     end subroutine stdlib_clarfb_gett

     pure module subroutine stdlib_zlarfb_gett( ident, m, n, k, t, ldt, a, lda, b, ldb,work, ldwork )
               
           character, intent(in) :: ident
           integer(ilp), intent(in) :: k, lda, ldb, ldt, ldwork, m, n
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(dp), intent(in) :: t(ldt,*)
           complex(dp), intent(out) :: work(ldwork,*)
     end subroutine stdlib_zlarfb_gett


     pure module subroutine stdlib_I64_slarfb_gett( ident, m, n, k, t, ldt, a, lda, b, ldb,work, ldwork )
               
           character, intent(in) :: ident
           integer(ilp64), intent(in) :: k, lda, ldb, ldt, ldwork, m, n
           real(sp), intent(inout) :: a(lda,*), b(ldb,*)
           real(sp), intent(in) :: t(ldt,*)
           real(sp), intent(out) :: work(ldwork,*)
     end subroutine stdlib_I64_slarfb_gett

     pure module subroutine stdlib_I64_dlarfb_gett( ident, m, n, k, t, ldt, a, lda, b, ldb,work, ldwork )
               
           character, intent(in) :: ident
           integer(ilp64), intent(in) :: k, lda, ldb, ldt, ldwork, m, n
           real(dp), intent(inout) :: a(lda,*), b(ldb,*)
           real(dp), intent(in) :: t(ldt,*)
           real(dp), intent(out) :: work(ldwork,*)
     end subroutine stdlib_I64_dlarfb_gett


     pure module subroutine stdlib_I64_clarfb_gett( ident, m, n, k, t, ldt, a, lda, b, ldb,work, ldwork )
               
           character, intent(in) :: ident
           integer(ilp64), intent(in) :: k, lda, ldb, ldt, ldwork, m, n
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(sp), intent(in) :: t(ldt,*)
           complex(sp), intent(out) :: work(ldwork,*)
     end subroutine stdlib_I64_clarfb_gett

     pure module subroutine stdlib_I64_zlarfb_gett( ident, m, n, k, t, ldt, a, lda, b, ldb,work, ldwork )
               
           character, intent(in) :: ident
           integer(ilp64), intent(in) :: k, lda, ldb, ldt, ldwork, m, n
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(dp), intent(in) :: t(ldt,*)
           complex(dp), intent(out) :: work(ldwork,*)
     end subroutine stdlib_I64_zlarfb_gett


end interface 


interface 
     pure module subroutine stdlib_slamtsqr( side, trans, m, n, k, mb, nb, a, lda, t,ldt, c, ldc, work, &
               lwork, info )
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n, k, mb, nb, ldt, lwork, ldc
           real(sp), intent(in) :: a(lda,*), t(ldt,*)
           real(sp), intent(out) :: work(*)
           real(sp), intent(inout) :: c(ldc,*)
     end subroutine stdlib_slamtsqr

     pure module subroutine stdlib_dlamtsqr( side, trans, m, n, k, mb, nb, a, lda, t,ldt, c, ldc, work, &
               lwork, info )
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n, k, mb, nb, ldt, lwork, ldc
           real(dp), intent(in) :: a(lda,*), t(ldt,*)
           real(dp), intent(out) :: work(*)
           real(dp), intent(inout) :: c(ldc,*)
     end subroutine stdlib_dlamtsqr


     pure module subroutine stdlib_clamtsqr( side, trans, m, n, k, mb, nb, a, lda, t,ldt, c, ldc, work, &
               lwork, info )
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n, k, mb, nb, ldt, lwork, ldc
           complex(sp), intent(in) :: a(lda,*), t(ldt,*)
           complex(sp), intent(out) :: work(*)
           complex(sp), intent(inout) :: c(ldc,*)
     end subroutine stdlib_clamtsqr

     pure module subroutine stdlib_zlamtsqr( side, trans, m, n, k, mb, nb, a, lda, t,ldt, c, ldc, work, &
               lwork, info )
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n, k, mb, nb, ldt, lwork, ldc
           complex(dp), intent(in) :: a(lda,*), t(ldt,*)
           complex(dp), intent(out) :: work(*)
           complex(dp), intent(inout) :: c(ldc,*)
     end subroutine stdlib_zlamtsqr


     pure module subroutine stdlib_I64_slamtsqr( side, trans, m, n, k, mb, nb, a, lda, t,ldt, c, ldc, work, &
               lwork, info )
           character, intent(in) :: side, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, m, n, k, mb, nb, ldt, lwork, ldc
           real(sp), intent(in) :: a(lda,*), t(ldt,*)
           real(sp), intent(out) :: work(*)
           real(sp), intent(inout) :: c(ldc,*)
     end subroutine stdlib_I64_slamtsqr

     pure module subroutine stdlib_I64_dlamtsqr( side, trans, m, n, k, mb, nb, a, lda, t,ldt, c, ldc, work, &
               lwork, info )
           character, intent(in) :: side, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, m, n, k, mb, nb, ldt, lwork, ldc
           real(dp), intent(in) :: a(lda,*), t(ldt,*)
           real(dp), intent(out) :: work(*)
           real(dp), intent(inout) :: c(ldc,*)
     end subroutine stdlib_I64_dlamtsqr


     pure module subroutine stdlib_I64_clamtsqr( side, trans, m, n, k, mb, nb, a, lda, t,ldt, c, ldc, work, &
               lwork, info )
           character, intent(in) :: side, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, m, n, k, mb, nb, ldt, lwork, ldc
           complex(sp), intent(in) :: a(lda,*), t(ldt,*)
           complex(sp), intent(out) :: work(*)
           complex(sp), intent(inout) :: c(ldc,*)
     end subroutine stdlib_I64_clamtsqr

     pure module subroutine stdlib_I64_zlamtsqr( side, trans, m, n, k, mb, nb, a, lda, t,ldt, c, ldc, work, &
               lwork, info )
           character, intent(in) :: side, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, m, n, k, mb, nb, ldt, lwork, ldc
           complex(dp), intent(in) :: a(lda,*), t(ldt,*)
           complex(dp), intent(out) :: work(*)
           complex(dp), intent(inout) :: c(ldc,*)
     end subroutine stdlib_I64_zlamtsqr


end interface 


interface 
     pure module subroutine stdlib_sgetsqrhrt( m, n, mb1, nb1, nb2, a, lda, t, ldt, work,lwork, info )
               
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldt, lwork, m, n, nb1, nb2, mb1
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: t(ldt,*), work(*)
     end subroutine stdlib_sgetsqrhrt

     pure module subroutine stdlib_dgetsqrhrt( m, n, mb1, nb1, nb2, a, lda, t, ldt, work,lwork, info )
               
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldt, lwork, m, n, nb1, nb2, mb1
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: t(ldt,*), work(*)
     end subroutine stdlib_dgetsqrhrt


     pure module subroutine stdlib_cgetsqrhrt( m, n, mb1, nb1, nb2, a, lda, t, ldt, work,lwork, info )
               
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldt, lwork, m, n, nb1, nb2, mb1
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: t(ldt,*), work(*)
     end subroutine stdlib_cgetsqrhrt

     pure module subroutine stdlib_zgetsqrhrt( m, n, mb1, nb1, nb2, a, lda, t, ldt, work,lwork, info )
               
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldt, lwork, m, n, nb1, nb2, mb1
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: t(ldt,*), work(*)
     end subroutine stdlib_zgetsqrhrt


     pure module subroutine stdlib_I64_sgetsqrhrt( m, n, mb1, nb1, nb2, a, lda, t, ldt, work,lwork, info )
               
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldt, lwork, m, n, nb1, nb2, mb1
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: t(ldt,*), work(*)
     end subroutine stdlib_I64_sgetsqrhrt

     pure module subroutine stdlib_I64_dgetsqrhrt( m, n, mb1, nb1, nb2, a, lda, t, ldt, work,lwork, info )
               
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldt, lwork, m, n, nb1, nb2, mb1
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: t(ldt,*), work(*)
     end subroutine stdlib_I64_dgetsqrhrt


     pure module subroutine stdlib_I64_cgetsqrhrt( m, n, mb1, nb1, nb2, a, lda, t, ldt, work,lwork, info )
               
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldt, lwork, m, n, nb1, nb2, mb1
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: t(ldt,*), work(*)
     end subroutine stdlib_I64_cgetsqrhrt

     pure module subroutine stdlib_I64_zgetsqrhrt( m, n, mb1, nb1, nb2, a, lda, t, ldt, work,lwork, info )
               
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldt, lwork, m, n, nb1, nb2, mb1
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: t(ldt,*), work(*)
     end subroutine stdlib_I64_zgetsqrhrt


end interface 


interface 
     pure module subroutine stdlib_cunhr_col( m, n, nb, a, lda, t, ldt, d, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldt, m, n, nb
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: d(*), t(ldt,*)
     end subroutine stdlib_cunhr_col

     pure module subroutine stdlib_zunhr_col( m, n, nb, a, lda, t, ldt, d, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldt, m, n, nb
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: d(*), t(ldt,*)
     end subroutine stdlib_zunhr_col


     pure module subroutine stdlib_I64_cunhr_col( m, n, nb, a, lda, t, ldt, d, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldt, m, n, nb
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: d(*), t(ldt,*)
     end subroutine stdlib_I64_cunhr_col

     pure module subroutine stdlib_I64_zunhr_col( m, n, nb, a, lda, t, ldt, d, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldt, m, n, nb
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: d(*), t(ldt,*)
     end subroutine stdlib_I64_zunhr_col


end interface 


interface 
     pure module subroutine stdlib_sorhr_col( m, n, nb, a, lda, t, ldt, d, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldt, m, n, nb
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: d(*), t(ldt,*)
     end subroutine stdlib_sorhr_col

     pure module subroutine stdlib_dorhr_col( m, n, nb, a, lda, t, ldt, d, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldt, m, n, nb
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: d(*), t(ldt,*)
     end subroutine stdlib_dorhr_col


     pure module subroutine stdlib_I64_sorhr_col( m, n, nb, a, lda, t, ldt, d, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldt, m, n, nb
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: d(*), t(ldt,*)
     end subroutine stdlib_I64_sorhr_col

     pure module subroutine stdlib_I64_dorhr_col( m, n, nb, a, lda, t, ldt, d, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldt, m, n, nb
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: d(*), t(ldt,*)
     end subroutine stdlib_I64_dorhr_col


end interface 


interface 
     pure module subroutine stdlib_claunhr_col_getrfnp( m, n, a, lda, d, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: d(*)
     end subroutine stdlib_claunhr_col_getrfnp

     pure module subroutine stdlib_zlaunhr_col_getrfnp( m, n, a, lda, d, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: d(*)
     end subroutine stdlib_zlaunhr_col_getrfnp


     pure module subroutine stdlib_I64_claunhr_col_getrfnp( m, n, a, lda, d, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, m, n
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: d(*)
     end subroutine stdlib_I64_claunhr_col_getrfnp

     pure module subroutine stdlib_I64_zlaunhr_col_getrfnp( m, n, a, lda, d, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, m, n
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: d(*)
     end subroutine stdlib_I64_zlaunhr_col_getrfnp


end interface 


interface 
     pure module subroutine stdlib_slaorhr_col_getrfnp( m, n, a, lda, d, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: d(*)
     end subroutine stdlib_slaorhr_col_getrfnp

     pure module subroutine stdlib_dlaorhr_col_getrfnp( m, n, a, lda, d, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: d(*)
     end subroutine stdlib_dlaorhr_col_getrfnp


     pure module subroutine stdlib_I64_slaorhr_col_getrfnp( m, n, a, lda, d, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, m, n
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: d(*)
     end subroutine stdlib_I64_slaorhr_col_getrfnp

     pure module subroutine stdlib_I64_dlaorhr_col_getrfnp( m, n, a, lda, d, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, m, n
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: d(*)
     end subroutine stdlib_I64_dlaorhr_col_getrfnp


end interface 


interface 
     pure recursive module subroutine stdlib_claunhr_col_getrfnp2( m, n, a, lda, d, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: d(*)
     end subroutine stdlib_claunhr_col_getrfnp2

     pure recursive module subroutine stdlib_zlaunhr_col_getrfnp2( m, n, a, lda, d, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: d(*)
     end subroutine stdlib_zlaunhr_col_getrfnp2


     pure recursive module subroutine stdlib_I64_claunhr_col_getrfnp2( m, n, a, lda, d, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, m, n
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: d(*)
     end subroutine stdlib_I64_claunhr_col_getrfnp2

     pure recursive module subroutine stdlib_I64_zlaunhr_col_getrfnp2( m, n, a, lda, d, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, m, n
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: d(*)
     end subroutine stdlib_I64_zlaunhr_col_getrfnp2


end interface 


interface 
     pure recursive module subroutine stdlib_slaorhr_col_getrfnp2( m, n, a, lda, d, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: d(*)
     end subroutine stdlib_slaorhr_col_getrfnp2

     pure recursive module subroutine stdlib_dlaorhr_col_getrfnp2( m, n, a, lda, d, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: d(*)
     end subroutine stdlib_dlaorhr_col_getrfnp2


     pure recursive module subroutine stdlib_I64_slaorhr_col_getrfnp2( m, n, a, lda, d, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, m, n
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: d(*)
     end subroutine stdlib_I64_slaorhr_col_getrfnp2

     pure recursive module subroutine stdlib_I64_dlaorhr_col_getrfnp2( m, n, a, lda, d, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, m, n
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: d(*)
     end subroutine stdlib_I64_dlaorhr_col_getrfnp2


end interface 


interface 
     pure module subroutine stdlib_stpqrt( m, n, l, nb, a, lda, b, ldb, t, ldt, work,info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, ldt, n, m, l, nb
           real(sp), intent(inout) :: a(lda,*), b(ldb,*)
           real(sp), intent(out) :: t(ldt,*), work(*)
     end subroutine stdlib_stpqrt

     pure module subroutine stdlib_dtpqrt( m, n, l, nb, a, lda, b, ldb, t, ldt, work,info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, ldt, n, m, l, nb
           real(dp), intent(inout) :: a(lda,*), b(ldb,*)
           real(dp), intent(out) :: t(ldt,*), work(*)
     end subroutine stdlib_dtpqrt


     pure module subroutine stdlib_ctpqrt( m, n, l, nb, a, lda, b, ldb, t, ldt, work,info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, ldt, n, m, l, nb
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(sp), intent(out) :: t(ldt,*), work(*)
     end subroutine stdlib_ctpqrt

     pure module subroutine stdlib_ztpqrt( m, n, l, nb, a, lda, b, ldb, t, ldt, work,info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, ldt, n, m, l, nb
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(dp), intent(out) :: t(ldt,*), work(*)
     end subroutine stdlib_ztpqrt


     pure module subroutine stdlib_I64_stpqrt( m, n, l, nb, a, lda, b, ldb, t, ldt, work,info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldb, ldt, n, m, l, nb
           real(sp), intent(inout) :: a(lda,*), b(ldb,*)
           real(sp), intent(out) :: t(ldt,*), work(*)
     end subroutine stdlib_I64_stpqrt

     pure module subroutine stdlib_I64_dtpqrt( m, n, l, nb, a, lda, b, ldb, t, ldt, work,info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldb, ldt, n, m, l, nb
           real(dp), intent(inout) :: a(lda,*), b(ldb,*)
           real(dp), intent(out) :: t(ldt,*), work(*)
     end subroutine stdlib_I64_dtpqrt


     pure module subroutine stdlib_I64_ctpqrt( m, n, l, nb, a, lda, b, ldb, t, ldt, work,info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldb, ldt, n, m, l, nb
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(sp), intent(out) :: t(ldt,*), work(*)
     end subroutine stdlib_I64_ctpqrt

     pure module subroutine stdlib_I64_ztpqrt( m, n, l, nb, a, lda, b, ldb, t, ldt, work,info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldb, ldt, n, m, l, nb
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(dp), intent(out) :: t(ldt,*), work(*)
     end subroutine stdlib_I64_ztpqrt


end interface 


interface 
     pure module subroutine stdlib_stpqrt2( m, n, l, a, lda, b, ldb, t, ldt, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, ldt, n, m, l
           real(sp), intent(inout) :: a(lda,*), b(ldb,*)
           real(sp), intent(out) :: t(ldt,*)
     end subroutine stdlib_stpqrt2

     pure module subroutine stdlib_dtpqrt2( m, n, l, a, lda, b, ldb, t, ldt, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, ldt, n, m, l
           real(dp), intent(inout) :: a(lda,*), b(ldb,*)
           real(dp), intent(out) :: t(ldt,*)
     end subroutine stdlib_dtpqrt2


     pure module subroutine stdlib_ctpqrt2( m, n, l, a, lda, b, ldb, t, ldt, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, ldt, n, m, l
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(sp), intent(out) :: t(ldt,*)
     end subroutine stdlib_ctpqrt2

     pure module subroutine stdlib_ztpqrt2( m, n, l, a, lda, b, ldb, t, ldt, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, ldt, n, m, l
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(dp), intent(out) :: t(ldt,*)
     end subroutine stdlib_ztpqrt2


     pure module subroutine stdlib_I64_stpqrt2( m, n, l, a, lda, b, ldb, t, ldt, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldb, ldt, n, m, l
           real(sp), intent(inout) :: a(lda,*), b(ldb,*)
           real(sp), intent(out) :: t(ldt,*)
     end subroutine stdlib_I64_stpqrt2

     pure module subroutine stdlib_I64_dtpqrt2( m, n, l, a, lda, b, ldb, t, ldt, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldb, ldt, n, m, l
           real(dp), intent(inout) :: a(lda,*), b(ldb,*)
           real(dp), intent(out) :: t(ldt,*)
     end subroutine stdlib_I64_dtpqrt2


     pure module subroutine stdlib_I64_ctpqrt2( m, n, l, a, lda, b, ldb, t, ldt, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldb, ldt, n, m, l
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(sp), intent(out) :: t(ldt,*)
     end subroutine stdlib_I64_ctpqrt2

     pure module subroutine stdlib_I64_ztpqrt2( m, n, l, a, lda, b, ldb, t, ldt, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldb, ldt, n, m, l
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(dp), intent(out) :: t(ldt,*)
     end subroutine stdlib_I64_ztpqrt2


end interface 


interface 
     pure module subroutine stdlib_stpmqrt( side, trans, m, n, k, l, nb, v, ldv, t, ldt,a, lda, b, ldb, &
               work, info )
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, ldv, lda, ldb, m, n, l, nb, ldt
           real(sp), intent(in) :: v(ldv,*), t(ldt,*)
           real(sp), intent(inout) :: a(lda,*), b(ldb,*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_stpmqrt

     pure module subroutine stdlib_dtpmqrt( side, trans, m, n, k, l, nb, v, ldv, t, ldt,a, lda, b, ldb, &
               work, info )
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, ldv, lda, ldb, m, n, l, nb, ldt
           real(dp), intent(in) :: v(ldv,*), t(ldt,*)
           real(dp), intent(inout) :: a(lda,*), b(ldb,*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_dtpmqrt


     pure module subroutine stdlib_ctpmqrt( side, trans, m, n, k, l, nb, v, ldv, t, ldt,a, lda, b, ldb, &
               work, info )
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, ldv, lda, ldb, m, n, l, nb, ldt
           complex(sp), intent(in) :: v(ldv,*), t(ldt,*)
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_ctpmqrt

     pure module subroutine stdlib_ztpmqrt( side, trans, m, n, k, l, nb, v, ldv, t, ldt,a, lda, b, ldb, &
               work, info )
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, ldv, lda, ldb, m, n, l, nb, ldt
           complex(dp), intent(in) :: v(ldv,*), t(ldt,*)
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_ztpmqrt


     pure module subroutine stdlib_I64_stpmqrt( side, trans, m, n, k, l, nb, v, ldv, t, ldt,a, lda, b, ldb, &
               work, info )
           character, intent(in) :: side, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, ldv, lda, ldb, m, n, l, nb, ldt
           real(sp), intent(in) :: v(ldv,*), t(ldt,*)
           real(sp), intent(inout) :: a(lda,*), b(ldb,*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_I64_stpmqrt

     pure module subroutine stdlib_I64_dtpmqrt( side, trans, m, n, k, l, nb, v, ldv, t, ldt,a, lda, b, ldb, &
               work, info )
           character, intent(in) :: side, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, ldv, lda, ldb, m, n, l, nb, ldt
           real(dp), intent(in) :: v(ldv,*), t(ldt,*)
           real(dp), intent(inout) :: a(lda,*), b(ldb,*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_I64_dtpmqrt


     pure module subroutine stdlib_I64_ctpmqrt( side, trans, m, n, k, l, nb, v, ldv, t, ldt,a, lda, b, ldb, &
               work, info )
           character, intent(in) :: side, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, ldv, lda, ldb, m, n, l, nb, ldt
           complex(sp), intent(in) :: v(ldv,*), t(ldt,*)
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_I64_ctpmqrt

     pure module subroutine stdlib_I64_ztpmqrt( side, trans, m, n, k, l, nb, v, ldv, t, ldt,a, lda, b, ldb, &
               work, info )
           character, intent(in) :: side, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, ldv, lda, ldb, m, n, l, nb, ldt
           complex(dp), intent(in) :: v(ldv,*), t(ldt,*)
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_I64_ztpmqrt


end interface 


interface 
     pure module subroutine stdlib_stprfb( side, trans, direct, storev, m, n, k, l,v, ldv, t, ldt, a, &
               lda, b, ldb, work, ldwork )
           character, intent(in) :: direct, side, storev, trans
           integer(ilp), intent(in) :: k, l, lda, ldb, ldt, ldv, ldwork, m, n
           real(sp), intent(inout) :: a(lda,*), b(ldb,*)
           real(sp), intent(in) :: t(ldt,*), v(ldv,*)
           real(sp), intent(out) :: work(ldwork,*)
     end subroutine stdlib_stprfb

     pure module subroutine stdlib_dtprfb( side, trans, direct, storev, m, n, k, l,v, ldv, t, ldt, a, &
               lda, b, ldb, work, ldwork )
           character, intent(in) :: direct, side, storev, trans
           integer(ilp), intent(in) :: k, l, lda, ldb, ldt, ldv, ldwork, m, n
           real(dp), intent(inout) :: a(lda,*), b(ldb,*)
           real(dp), intent(in) :: t(ldt,*), v(ldv,*)
           real(dp), intent(out) :: work(ldwork,*)
     end subroutine stdlib_dtprfb


     pure module subroutine stdlib_ctprfb( side, trans, direct, storev, m, n, k, l,v, ldv, t, ldt, a, &
               lda, b, ldb, work, ldwork )
           character, intent(in) :: direct, side, storev, trans
           integer(ilp), intent(in) :: k, l, lda, ldb, ldt, ldv, ldwork, m, n
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(sp), intent(in) :: t(ldt,*), v(ldv,*)
           complex(sp), intent(out) :: work(ldwork,*)
     end subroutine stdlib_ctprfb

     pure module subroutine stdlib_ztprfb( side, trans, direct, storev, m, n, k, l,v, ldv, t, ldt, a, &
               lda, b, ldb, work, ldwork )
           character, intent(in) :: direct, side, storev, trans
           integer(ilp), intent(in) :: k, l, lda, ldb, ldt, ldv, ldwork, m, n
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(dp), intent(in) :: t(ldt,*), v(ldv,*)
           complex(dp), intent(out) :: work(ldwork,*)
     end subroutine stdlib_ztprfb


     pure module subroutine stdlib_I64_stprfb( side, trans, direct, storev, m, n, k, l,v, ldv, t, ldt, a, &
               lda, b, ldb, work, ldwork )
           character, intent(in) :: direct, side, storev, trans
           integer(ilp64), intent(in) :: k, l, lda, ldb, ldt, ldv, ldwork, m, n
           real(sp), intent(inout) :: a(lda,*), b(ldb,*)
           real(sp), intent(in) :: t(ldt,*), v(ldv,*)
           real(sp), intent(out) :: work(ldwork,*)
     end subroutine stdlib_I64_stprfb

     pure module subroutine stdlib_I64_dtprfb( side, trans, direct, storev, m, n, k, l,v, ldv, t, ldt, a, &
               lda, b, ldb, work, ldwork )
           character, intent(in) :: direct, side, storev, trans
           integer(ilp64), intent(in) :: k, l, lda, ldb, ldt, ldv, ldwork, m, n
           real(dp), intent(inout) :: a(lda,*), b(ldb,*)
           real(dp), intent(in) :: t(ldt,*), v(ldv,*)
           real(dp), intent(out) :: work(ldwork,*)
     end subroutine stdlib_I64_dtprfb


     pure module subroutine stdlib_I64_ctprfb( side, trans, direct, storev, m, n, k, l,v, ldv, t, ldt, a, &
               lda, b, ldb, work, ldwork )
           character, intent(in) :: direct, side, storev, trans
           integer(ilp64), intent(in) :: k, l, lda, ldb, ldt, ldv, ldwork, m, n
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(sp), intent(in) :: t(ldt,*), v(ldv,*)
           complex(sp), intent(out) :: work(ldwork,*)
     end subroutine stdlib_I64_ctprfb

     pure module subroutine stdlib_I64_ztprfb( side, trans, direct, storev, m, n, k, l,v, ldv, t, ldt, a, &
               lda, b, ldb, work, ldwork )
           character, intent(in) :: direct, side, storev, trans
           integer(ilp64), intent(in) :: k, l, lda, ldb, ldt, ldv, ldwork, m, n
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(dp), intent(in) :: t(ldt,*), v(ldv,*)
           complex(dp), intent(out) :: work(ldwork,*)
     end subroutine stdlib_I64_ztprfb


end interface 


interface 
     pure module subroutine stdlib_sggqrf( n, m, p, a, lda, taua, b, ldb, taub, work,lwork, info )
               
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, lwork, m, n, p
           real(sp), intent(inout) :: a(lda,*), b(ldb,*)
           real(sp), intent(out) :: taua(*), taub(*), work(*)
     end subroutine stdlib_sggqrf

     pure module subroutine stdlib_dggqrf( n, m, p, a, lda, taua, b, ldb, taub, work,lwork, info )
               
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, lwork, m, n, p
           real(dp), intent(inout) :: a(lda,*), b(ldb,*)
           real(dp), intent(out) :: taua(*), taub(*), work(*)
     end subroutine stdlib_dggqrf


     pure module subroutine stdlib_cggqrf( n, m, p, a, lda, taua, b, ldb, taub, work,lwork, info )
               
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, lwork, m, n, p
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(sp), intent(out) :: taua(*), taub(*), work(*)
     end subroutine stdlib_cggqrf

     pure module subroutine stdlib_zggqrf( n, m, p, a, lda, taua, b, ldb, taub, work,lwork, info )
               
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, lwork, m, n, p
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(dp), intent(out) :: taua(*), taub(*), work(*)
     end subroutine stdlib_zggqrf


     pure module subroutine stdlib_I64_sggqrf( n, m, p, a, lda, taua, b, ldb, taub, work,lwork, info )
               
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldb, lwork, m, n, p
           real(sp), intent(inout) :: a(lda,*), b(ldb,*)
           real(sp), intent(out) :: taua(*), taub(*), work(*)
     end subroutine stdlib_I64_sggqrf

     pure module subroutine stdlib_I64_dggqrf( n, m, p, a, lda, taua, b, ldb, taub, work,lwork, info )
               
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldb, lwork, m, n, p
           real(dp), intent(inout) :: a(lda,*), b(ldb,*)
           real(dp), intent(out) :: taua(*), taub(*), work(*)
     end subroutine stdlib_I64_dggqrf


     pure module subroutine stdlib_I64_cggqrf( n, m, p, a, lda, taua, b, ldb, taub, work,lwork, info )
               
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldb, lwork, m, n, p
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(sp), intent(out) :: taua(*), taub(*), work(*)
     end subroutine stdlib_I64_cggqrf

     pure module subroutine stdlib_I64_zggqrf( n, m, p, a, lda, taua, b, ldb, taub, work,lwork, info )
               
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldb, lwork, m, n, p
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(dp), intent(out) :: taua(*), taub(*), work(*)
     end subroutine stdlib_I64_zggqrf


end interface 


interface 
     pure module subroutine stdlib_sgerqf( m, n, a, lda, tau, work, lwork, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, m, n
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_sgerqf

     pure module subroutine stdlib_dgerqf( m, n, a, lda, tau, work, lwork, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, m, n
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_dgerqf


     pure module subroutine stdlib_cgerqf( m, n, a, lda, tau, work, lwork, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, m, n
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_cgerqf

     pure module subroutine stdlib_zgerqf( m, n, a, lda, tau, work, lwork, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, m, n
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_zgerqf


     pure module subroutine stdlib_I64_sgerqf( m, n, a, lda, tau, work, lwork, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, lwork, m, n
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_I64_sgerqf

     pure module subroutine stdlib_I64_dgerqf( m, n, a, lda, tau, work, lwork, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, lwork, m, n
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_I64_dgerqf


     pure module subroutine stdlib_I64_cgerqf( m, n, a, lda, tau, work, lwork, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, lwork, m, n
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_I64_cgerqf

     pure module subroutine stdlib_I64_zgerqf( m, n, a, lda, tau, work, lwork, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, lwork, m, n
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_I64_zgerqf


end interface 


interface 
     pure module subroutine stdlib_sgerq2( m, n, a, lda, tau, work, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_sgerq2

     pure module subroutine stdlib_dgerq2( m, n, a, lda, tau, work, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_dgerq2


     pure module subroutine stdlib_cgerq2( m, n, a, lda, tau, work, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_cgerq2

     pure module subroutine stdlib_zgerq2( m, n, a, lda, tau, work, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_zgerq2


     pure module subroutine stdlib_I64_sgerq2( m, n, a, lda, tau, work, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, m, n
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_I64_sgerq2

     pure module subroutine stdlib_I64_dgerq2( m, n, a, lda, tau, work, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, m, n
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_I64_dgerq2


     pure module subroutine stdlib_I64_cgerq2( m, n, a, lda, tau, work, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, m, n
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_I64_cgerq2

     pure module subroutine stdlib_I64_zgerq2( m, n, a, lda, tau, work, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, m, n
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_I64_zgerq2


end interface 


interface 
     pure module subroutine stdlib_cungrq( m, n, k, a, lda, tau, work, lwork, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, lwork, m, n
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(in) :: tau(*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_cungrq

     pure module subroutine stdlib_zungrq( m, n, k, a, lda, tau, work, lwork, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, lwork, m, n
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(in) :: tau(*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_zungrq


     pure module subroutine stdlib_I64_cungrq( m, n, k, a, lda, tau, work, lwork, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, lda, lwork, m, n
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(in) :: tau(*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_I64_cungrq

     pure module subroutine stdlib_I64_zungrq( m, n, k, a, lda, tau, work, lwork, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, lda, lwork, m, n
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(in) :: tau(*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_I64_zungrq


end interface 


interface 
     pure module subroutine stdlib_cunmrq( side, trans, m, n, k, a, lda, tau, c, ldc,work, lwork, info )
               
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, ldc, lwork, m, n
           complex(sp), intent(inout) :: a(lda,*), c(ldc,*)
           complex(sp), intent(in) :: tau(*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_cunmrq

     pure module subroutine stdlib_zunmrq( side, trans, m, n, k, a, lda, tau, c, ldc,work, lwork, info )
               
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, ldc, lwork, m, n
           complex(dp), intent(inout) :: a(lda,*), c(ldc,*)
           complex(dp), intent(in) :: tau(*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_zunmrq


     pure module subroutine stdlib_I64_cunmrq( side, trans, m, n, k, a, lda, tau, c, ldc,work, lwork, info )
               
           character, intent(in) :: side, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, lda, ldc, lwork, m, n
           complex(sp), intent(inout) :: a(lda,*), c(ldc,*)
           complex(sp), intent(in) :: tau(*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_I64_cunmrq

     pure module subroutine stdlib_I64_zunmrq( side, trans, m, n, k, a, lda, tau, c, ldc,work, lwork, info )
               
           character, intent(in) :: side, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, lda, ldc, lwork, m, n
           complex(dp), intent(inout) :: a(lda,*), c(ldc,*)
           complex(dp), intent(in) :: tau(*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_I64_zunmrq


end interface 


interface 
     pure module subroutine stdlib_cunmr2( side, trans, m, n, k, a, lda, tau, c, ldc,work, info )
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, ldc, m, n
           complex(sp), intent(inout) :: a(lda,*), c(ldc,*)
           complex(sp), intent(in) :: tau(*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_cunmr2

     pure module subroutine stdlib_zunmr2( side, trans, m, n, k, a, lda, tau, c, ldc,work, info )
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, ldc, m, n
           complex(dp), intent(inout) :: a(lda,*), c(ldc,*)
           complex(dp), intent(in) :: tau(*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_zunmr2


     pure module subroutine stdlib_I64_cunmr2( side, trans, m, n, k, a, lda, tau, c, ldc,work, info )
           character, intent(in) :: side, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, lda, ldc, m, n
           complex(sp), intent(inout) :: a(lda,*), c(ldc,*)
           complex(sp), intent(in) :: tau(*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_I64_cunmr2

     pure module subroutine stdlib_I64_zunmr2( side, trans, m, n, k, a, lda, tau, c, ldc,work, info )
           character, intent(in) :: side, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, lda, ldc, m, n
           complex(dp), intent(inout) :: a(lda,*), c(ldc,*)
           complex(dp), intent(in) :: tau(*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_I64_zunmr2


end interface 


interface 
     pure module subroutine stdlib_cungr2( m, n, k, a, lda, tau, work, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, m, n
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(in) :: tau(*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_cungr2

     pure module subroutine stdlib_zungr2( m, n, k, a, lda, tau, work, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, m, n
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(in) :: tau(*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_zungr2


     pure module subroutine stdlib_I64_cungr2( m, n, k, a, lda, tau, work, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, lda, m, n
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(in) :: tau(*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_I64_cungr2

     pure module subroutine stdlib_I64_zungr2( m, n, k, a, lda, tau, work, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, lda, m, n
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(in) :: tau(*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_I64_zungr2


end interface 


interface 
     pure module subroutine stdlib_sorgrq( m, n, k, a, lda, tau, work, lwork, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, lwork, m, n
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(in) :: tau(*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_sorgrq

     pure module subroutine stdlib_dorgrq( m, n, k, a, lda, tau, work, lwork, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, lwork, m, n
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(in) :: tau(*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_dorgrq


     pure module subroutine stdlib_I64_sorgrq( m, n, k, a, lda, tau, work, lwork, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, lda, lwork, m, n
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(in) :: tau(*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_I64_sorgrq

     pure module subroutine stdlib_I64_dorgrq( m, n, k, a, lda, tau, work, lwork, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, lda, lwork, m, n
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(in) :: tau(*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_I64_dorgrq


end interface 


interface 
     pure module subroutine stdlib_sormrq( side, trans, m, n, k, a, lda, tau, c, ldc,work, lwork, info )
               
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, ldc, lwork, m, n
           real(sp), intent(inout) :: a(lda,*), c(ldc,*)
           real(sp), intent(in) :: tau(*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_sormrq

     pure module subroutine stdlib_dormrq( side, trans, m, n, k, a, lda, tau, c, ldc,work, lwork, info )
               
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, ldc, lwork, m, n
           real(dp), intent(inout) :: a(lda,*), c(ldc,*)
           real(dp), intent(in) :: tau(*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_dormrq


     pure module subroutine stdlib_I64_sormrq( side, trans, m, n, k, a, lda, tau, c, ldc,work, lwork, info )
               
           character, intent(in) :: side, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, lda, ldc, lwork, m, n
           real(sp), intent(inout) :: a(lda,*), c(ldc,*)
           real(sp), intent(in) :: tau(*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_I64_sormrq

     pure module subroutine stdlib_I64_dormrq( side, trans, m, n, k, a, lda, tau, c, ldc,work, lwork, info )
               
           character, intent(in) :: side, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, lda, ldc, lwork, m, n
           real(dp), intent(inout) :: a(lda,*), c(ldc,*)
           real(dp), intent(in) :: tau(*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_I64_dormrq


end interface 


interface 
     pure module subroutine stdlib_sormr2( side, trans, m, n, k, a, lda, tau, c, ldc,work, info )
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, ldc, m, n
           real(sp), intent(inout) :: a(lda,*), c(ldc,*)
           real(sp), intent(in) :: tau(*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_sormr2

     pure module subroutine stdlib_dormr2( side, trans, m, n, k, a, lda, tau, c, ldc,work, info )
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, ldc, m, n
           real(dp), intent(inout) :: a(lda,*), c(ldc,*)
           real(dp), intent(in) :: tau(*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_dormr2


     pure module subroutine stdlib_I64_sormr2( side, trans, m, n, k, a, lda, tau, c, ldc,work, info )
           character, intent(in) :: side, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, lda, ldc, m, n
           real(sp), intent(inout) :: a(lda,*), c(ldc,*)
           real(sp), intent(in) :: tau(*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_I64_sormr2

     pure module subroutine stdlib_I64_dormr2( side, trans, m, n, k, a, lda, tau, c, ldc,work, info )
           character, intent(in) :: side, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, lda, ldc, m, n
           real(dp), intent(inout) :: a(lda,*), c(ldc,*)
           real(dp), intent(in) :: tau(*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_I64_dormr2


end interface 


interface 
     pure module subroutine stdlib_sorgr2( m, n, k, a, lda, tau, work, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, m, n
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(in) :: tau(*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_sorgr2

     pure module subroutine stdlib_dorgr2( m, n, k, a, lda, tau, work, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, m, n
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(in) :: tau(*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_dorgr2


     pure module subroutine stdlib_I64_sorgr2( m, n, k, a, lda, tau, work, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, lda, m, n
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(in) :: tau(*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_I64_sorgr2

     pure module subroutine stdlib_I64_dorgr2( m, n, k, a, lda, tau, work, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, lda, m, n
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(in) :: tau(*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_I64_dorgr2


end interface 


interface 
     pure module subroutine stdlib_sggrqf( m, p, n, a, lda, taua, b, ldb, taub, work,lwork, info )
               
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, lwork, m, n, p
           real(sp), intent(inout) :: a(lda,*), b(ldb,*)
           real(sp), intent(out) :: taua(*), taub(*), work(*)
     end subroutine stdlib_sggrqf

     pure module subroutine stdlib_dggrqf( m, p, n, a, lda, taua, b, ldb, taub, work,lwork, info )
               
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, lwork, m, n, p
           real(dp), intent(inout) :: a(lda,*), b(ldb,*)
           real(dp), intent(out) :: taua(*), taub(*), work(*)
     end subroutine stdlib_dggrqf


     pure module subroutine stdlib_cggrqf( m, p, n, a, lda, taua, b, ldb, taub, work,lwork, info )
               
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, lwork, m, n, p
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(sp), intent(out) :: taua(*), taub(*), work(*)
     end subroutine stdlib_cggrqf

     pure module subroutine stdlib_zggrqf( m, p, n, a, lda, taua, b, ldb, taub, work,lwork, info )
               
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, lwork, m, n, p
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(dp), intent(out) :: taua(*), taub(*), work(*)
     end subroutine stdlib_zggrqf


     pure module subroutine stdlib_I64_sggrqf( m, p, n, a, lda, taua, b, ldb, taub, work,lwork, info )
               
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldb, lwork, m, n, p
           real(sp), intent(inout) :: a(lda,*), b(ldb,*)
           real(sp), intent(out) :: taua(*), taub(*), work(*)
     end subroutine stdlib_I64_sggrqf

     pure module subroutine stdlib_I64_dggrqf( m, p, n, a, lda, taua, b, ldb, taub, work,lwork, info )
               
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldb, lwork, m, n, p
           real(dp), intent(inout) :: a(lda,*), b(ldb,*)
           real(dp), intent(out) :: taua(*), taub(*), work(*)
     end subroutine stdlib_I64_dggrqf


     pure module subroutine stdlib_I64_cggrqf( m, p, n, a, lda, taua, b, ldb, taub, work,lwork, info )
               
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldb, lwork, m, n, p
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(sp), intent(out) :: taua(*), taub(*), work(*)
     end subroutine stdlib_I64_cggrqf

     pure module subroutine stdlib_I64_zggrqf( m, p, n, a, lda, taua, b, ldb, taub, work,lwork, info )
               
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldb, lwork, m, n, p
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(dp), intent(out) :: taua(*), taub(*), work(*)
     end subroutine stdlib_I64_zggrqf


end interface 


interface 
     pure module subroutine stdlib_sgelq( m, n, a, lda, t, tsize, work, lwork,info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n, tsize, lwork
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: t(*), work(*)
     end subroutine stdlib_sgelq

     pure module subroutine stdlib_dgelq( m, n, a, lda, t, tsize, work, lwork,info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n, tsize, lwork
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: t(*), work(*)
     end subroutine stdlib_dgelq


     pure module subroutine stdlib_cgelq( m, n, a, lda, t, tsize, work, lwork,info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n, tsize, lwork
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: t(*), work(*)
     end subroutine stdlib_cgelq

     pure module subroutine stdlib_zgelq( m, n, a, lda, t, tsize, work, lwork,info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n, tsize, lwork
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: t(*), work(*)
     end subroutine stdlib_zgelq


     pure module subroutine stdlib_I64_sgelq( m, n, a, lda, t, tsize, work, lwork,info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, m, n, tsize, lwork
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: t(*), work(*)
     end subroutine stdlib_I64_sgelq

     pure module subroutine stdlib_I64_dgelq( m, n, a, lda, t, tsize, work, lwork,info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, m, n, tsize, lwork
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: t(*), work(*)
     end subroutine stdlib_I64_dgelq


     pure module subroutine stdlib_I64_cgelq( m, n, a, lda, t, tsize, work, lwork,info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, m, n, tsize, lwork
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: t(*), work(*)
     end subroutine stdlib_I64_cgelq

     pure module subroutine stdlib_I64_zgelq( m, n, a, lda, t, tsize, work, lwork,info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, m, n, tsize, lwork
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: t(*), work(*)
     end subroutine stdlib_I64_zgelq


end interface 


interface 
     pure module subroutine stdlib_sgemlq( side, trans, m, n, k, a, lda, t, tsize,c, ldc, work, lwork, &
               info )
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n, k, tsize, lwork, ldc
           real(sp), intent(in) :: a(lda,*), t(*)
           real(sp), intent(inout) :: c(ldc,*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_sgemlq

     pure module subroutine stdlib_dgemlq( side, trans, m, n, k, a, lda, t, tsize,c, ldc, work, lwork, &
               info )
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n, k, tsize, lwork, ldc
           real(dp), intent(in) :: a(lda,*), t(*)
           real(dp), intent(inout) :: c(ldc,*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_dgemlq


     pure module subroutine stdlib_cgemlq( side, trans, m, n, k, a, lda, t, tsize,c, ldc, work, lwork, &
               info )
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n, k, tsize, lwork, ldc
           complex(sp), intent(in) :: a(lda,*), t(*)
           complex(sp), intent(inout) :: c(ldc,*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_cgemlq

     pure module subroutine stdlib_zgemlq( side, trans, m, n, k, a, lda, t, tsize,c, ldc, work, lwork, &
               info )
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n, k, tsize, lwork, ldc
           complex(dp), intent(in) :: a(lda,*), t(*)
           complex(dp), intent(inout) :: c(ldc,*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_zgemlq


     pure module subroutine stdlib_I64_sgemlq( side, trans, m, n, k, a, lda, t, tsize,c, ldc, work, lwork, &
               info )
           character, intent(in) :: side, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, m, n, k, tsize, lwork, ldc
           real(sp), intent(in) :: a(lda,*), t(*)
           real(sp), intent(inout) :: c(ldc,*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_I64_sgemlq

     pure module subroutine stdlib_I64_dgemlq( side, trans, m, n, k, a, lda, t, tsize,c, ldc, work, lwork, &
               info )
           character, intent(in) :: side, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, m, n, k, tsize, lwork, ldc
           real(dp), intent(in) :: a(lda,*), t(*)
           real(dp), intent(inout) :: c(ldc,*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_I64_dgemlq


     pure module subroutine stdlib_I64_cgemlq( side, trans, m, n, k, a, lda, t, tsize,c, ldc, work, lwork, &
               info )
           character, intent(in) :: side, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, m, n, k, tsize, lwork, ldc
           complex(sp), intent(in) :: a(lda,*), t(*)
           complex(sp), intent(inout) :: c(ldc,*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_I64_cgemlq

     pure module subroutine stdlib_I64_zgemlq( side, trans, m, n, k, a, lda, t, tsize,c, ldc, work, lwork, &
               info )
           character, intent(in) :: side, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, m, n, k, tsize, lwork, ldc
           complex(dp), intent(in) :: a(lda,*), t(*)
           complex(dp), intent(inout) :: c(ldc,*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_I64_zgemlq


end interface 


interface 
     pure module subroutine stdlib_sgelqf( m, n, a, lda, tau, work, lwork, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, m, n
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_sgelqf

     pure module subroutine stdlib_dgelqf( m, n, a, lda, tau, work, lwork, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, m, n
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_dgelqf


     pure module subroutine stdlib_cgelqf( m, n, a, lda, tau, work, lwork, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, m, n
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_cgelqf

     pure module subroutine stdlib_zgelqf( m, n, a, lda, tau, work, lwork, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, m, n
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_zgelqf


     pure module subroutine stdlib_I64_sgelqf( m, n, a, lda, tau, work, lwork, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, lwork, m, n
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_I64_sgelqf

     pure module subroutine stdlib_I64_dgelqf( m, n, a, lda, tau, work, lwork, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, lwork, m, n
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_I64_dgelqf


     pure module subroutine stdlib_I64_cgelqf( m, n, a, lda, tau, work, lwork, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, lwork, m, n
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_I64_cgelqf

     pure module subroutine stdlib_I64_zgelqf( m, n, a, lda, tau, work, lwork, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, lwork, m, n
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_I64_zgelqf


end interface 


interface 
     pure module subroutine stdlib_sgelq2( m, n, a, lda, tau, work, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_sgelq2

     pure module subroutine stdlib_dgelq2( m, n, a, lda, tau, work, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_dgelq2


     pure module subroutine stdlib_cgelq2( m, n, a, lda, tau, work, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_cgelq2

     pure module subroutine stdlib_zgelq2( m, n, a, lda, tau, work, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_zgelq2


     pure module subroutine stdlib_I64_sgelq2( m, n, a, lda, tau, work, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, m, n
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_I64_sgelq2

     pure module subroutine stdlib_I64_dgelq2( m, n, a, lda, tau, work, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, m, n
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_I64_dgelq2


     pure module subroutine stdlib_I64_cgelq2( m, n, a, lda, tau, work, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, m, n
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_I64_cgelq2

     pure module subroutine stdlib_I64_zgelq2( m, n, a, lda, tau, work, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, m, n
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_I64_zgelq2


end interface 


interface 
     pure module subroutine stdlib_cunglq( m, n, k, a, lda, tau, work, lwork, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, lwork, m, n
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(in) :: tau(*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_cunglq

     pure module subroutine stdlib_zunglq( m, n, k, a, lda, tau, work, lwork, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, lwork, m, n
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(in) :: tau(*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_zunglq


     pure module subroutine stdlib_I64_cunglq( m, n, k, a, lda, tau, work, lwork, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, lda, lwork, m, n
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(in) :: tau(*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_I64_cunglq

     pure module subroutine stdlib_I64_zunglq( m, n, k, a, lda, tau, work, lwork, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, lda, lwork, m, n
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(in) :: tau(*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_I64_zunglq


end interface 


interface 
     pure module subroutine stdlib_cungl2( m, n, k, a, lda, tau, work, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, m, n
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(in) :: tau(*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_cungl2

     pure module subroutine stdlib_zungl2( m, n, k, a, lda, tau, work, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, m, n
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(in) :: tau(*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_zungl2


     pure module subroutine stdlib_I64_cungl2( m, n, k, a, lda, tau, work, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, lda, m, n
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(in) :: tau(*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_I64_cungl2

     pure module subroutine stdlib_I64_zungl2( m, n, k, a, lda, tau, work, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, lda, m, n
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(in) :: tau(*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_I64_zungl2


end interface 


interface 
     pure module subroutine stdlib_cunmlq( side, trans, m, n, k, a, lda, tau, c, ldc,work, lwork, info )
               
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, ldc, lwork, m, n
           complex(sp), intent(inout) :: a(lda,*), c(ldc,*)
           complex(sp), intent(in) :: tau(*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_cunmlq

     pure module subroutine stdlib_zunmlq( side, trans, m, n, k, a, lda, tau, c, ldc,work, lwork, info )
               
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, ldc, lwork, m, n
           complex(dp), intent(inout) :: a(lda,*), c(ldc,*)
           complex(dp), intent(in) :: tau(*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_zunmlq


     pure module subroutine stdlib_I64_cunmlq( side, trans, m, n, k, a, lda, tau, c, ldc,work, lwork, info )
               
           character, intent(in) :: side, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, lda, ldc, lwork, m, n
           complex(sp), intent(inout) :: a(lda,*), c(ldc,*)
           complex(sp), intent(in) :: tau(*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_I64_cunmlq

     pure module subroutine stdlib_I64_zunmlq( side, trans, m, n, k, a, lda, tau, c, ldc,work, lwork, info )
               
           character, intent(in) :: side, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, lda, ldc, lwork, m, n
           complex(dp), intent(inout) :: a(lda,*), c(ldc,*)
           complex(dp), intent(in) :: tau(*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_I64_zunmlq


end interface 


interface 
     pure module subroutine stdlib_cunml2( side, trans, m, n, k, a, lda, tau, c, ldc,work, info )
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, ldc, m, n
           complex(sp), intent(inout) :: a(lda,*), c(ldc,*)
           complex(sp), intent(in) :: tau(*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_cunml2

     pure module subroutine stdlib_zunml2( side, trans, m, n, k, a, lda, tau, c, ldc,work, info )
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, ldc, m, n
           complex(dp), intent(inout) :: a(lda,*), c(ldc,*)
           complex(dp), intent(in) :: tau(*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_zunml2


     pure module subroutine stdlib_I64_cunml2( side, trans, m, n, k, a, lda, tau, c, ldc,work, info )
           character, intent(in) :: side, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, lda, ldc, m, n
           complex(sp), intent(inout) :: a(lda,*), c(ldc,*)
           complex(sp), intent(in) :: tau(*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_I64_cunml2

     pure module subroutine stdlib_I64_zunml2( side, trans, m, n, k, a, lda, tau, c, ldc,work, info )
           character, intent(in) :: side, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, lda, ldc, m, n
           complex(dp), intent(inout) :: a(lda,*), c(ldc,*)
           complex(dp), intent(in) :: tau(*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_I64_zunml2


end interface 


interface 
     pure module subroutine stdlib_sorglq( m, n, k, a, lda, tau, work, lwork, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, lwork, m, n
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(in) :: tau(*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_sorglq

     pure module subroutine stdlib_dorglq( m, n, k, a, lda, tau, work, lwork, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, lwork, m, n
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(in) :: tau(*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_dorglq


     pure module subroutine stdlib_I64_sorglq( m, n, k, a, lda, tau, work, lwork, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, lda, lwork, m, n
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(in) :: tau(*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_I64_sorglq

     pure module subroutine stdlib_I64_dorglq( m, n, k, a, lda, tau, work, lwork, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, lda, lwork, m, n
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(in) :: tau(*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_I64_dorglq


end interface 


interface 
     pure module subroutine stdlib_sorgl2( m, n, k, a, lda, tau, work, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, m, n
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(in) :: tau(*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_sorgl2

     pure module subroutine stdlib_dorgl2( m, n, k, a, lda, tau, work, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, m, n
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(in) :: tau(*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_dorgl2


     pure module subroutine stdlib_I64_sorgl2( m, n, k, a, lda, tau, work, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, lda, m, n
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(in) :: tau(*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_I64_sorgl2

     pure module subroutine stdlib_I64_dorgl2( m, n, k, a, lda, tau, work, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, lda, m, n
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(in) :: tau(*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_I64_dorgl2


end interface 


interface 
     pure module subroutine stdlib_sormlq( side, trans, m, n, k, a, lda, tau, c, ldc,work, lwork, info )
               
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, ldc, lwork, m, n
           real(sp), intent(inout) :: a(lda,*), c(ldc,*)
           real(sp), intent(in) :: tau(*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_sormlq

     pure module subroutine stdlib_dormlq( side, trans, m, n, k, a, lda, tau, c, ldc,work, lwork, info )
               
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, ldc, lwork, m, n
           real(dp), intent(inout) :: a(lda,*), c(ldc,*)
           real(dp), intent(in) :: tau(*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_dormlq


     pure module subroutine stdlib_I64_sormlq( side, trans, m, n, k, a, lda, tau, c, ldc,work, lwork, info )
               
           character, intent(in) :: side, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, lda, ldc, lwork, m, n
           real(sp), intent(inout) :: a(lda,*), c(ldc,*)
           real(sp), intent(in) :: tau(*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_I64_sormlq

     pure module subroutine stdlib_I64_dormlq( side, trans, m, n, k, a, lda, tau, c, ldc,work, lwork, info )
               
           character, intent(in) :: side, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, lda, ldc, lwork, m, n
           real(dp), intent(inout) :: a(lda,*), c(ldc,*)
           real(dp), intent(in) :: tau(*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_I64_dormlq


end interface 


interface 
     pure module subroutine stdlib_sorml2( side, trans, m, n, k, a, lda, tau, c, ldc,work, info )
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, ldc, m, n
           real(sp), intent(inout) :: a(lda,*), c(ldc,*)
           real(sp), intent(in) :: tau(*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_sorml2

     pure module subroutine stdlib_dorml2( side, trans, m, n, k, a, lda, tau, c, ldc,work, info )
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, ldc, m, n
           real(dp), intent(inout) :: a(lda,*), c(ldc,*)
           real(dp), intent(in) :: tau(*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_dorml2


     pure module subroutine stdlib_I64_sorml2( side, trans, m, n, k, a, lda, tau, c, ldc,work, info )
           character, intent(in) :: side, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, lda, ldc, m, n
           real(sp), intent(inout) :: a(lda,*), c(ldc,*)
           real(sp), intent(in) :: tau(*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_I64_sorml2

     pure module subroutine stdlib_I64_dorml2( side, trans, m, n, k, a, lda, tau, c, ldc,work, info )
           character, intent(in) :: side, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, lda, ldc, m, n
           real(dp), intent(inout) :: a(lda,*), c(ldc,*)
           real(dp), intent(in) :: tau(*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_I64_dorml2


end interface 


interface 
     pure module subroutine stdlib_sgelqt( m, n, mb, a, lda, t, ldt, work, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldt, m, n, mb
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: t(ldt,*), work(*)
     end subroutine stdlib_sgelqt

     pure module subroutine stdlib_dgelqt( m, n, mb, a, lda, t, ldt, work, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldt, m, n, mb
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: t(ldt,*), work(*)
     end subroutine stdlib_dgelqt


     pure module subroutine stdlib_cgelqt( m, n, mb, a, lda, t, ldt, work, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldt, m, n, mb
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: t(ldt,*), work(*)
     end subroutine stdlib_cgelqt

     pure module subroutine stdlib_zgelqt( m, n, mb, a, lda, t, ldt, work, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldt, m, n, mb
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: t(ldt,*), work(*)
     end subroutine stdlib_zgelqt


     pure module subroutine stdlib_I64_sgelqt( m, n, mb, a, lda, t, ldt, work, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldt, m, n, mb
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: t(ldt,*), work(*)
     end subroutine stdlib_I64_sgelqt

     pure module subroutine stdlib_I64_dgelqt( m, n, mb, a, lda, t, ldt, work, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldt, m, n, mb
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: t(ldt,*), work(*)
     end subroutine stdlib_I64_dgelqt


     pure module subroutine stdlib_I64_cgelqt( m, n, mb, a, lda, t, ldt, work, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldt, m, n, mb
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: t(ldt,*), work(*)
     end subroutine stdlib_I64_cgelqt

     pure module subroutine stdlib_I64_zgelqt( m, n, mb, a, lda, t, ldt, work, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldt, m, n, mb
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: t(ldt,*), work(*)
     end subroutine stdlib_I64_zgelqt


end interface 


interface 
     pure recursive module subroutine stdlib_sgelqt3( m, n, a, lda, t, ldt, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n, ldt
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: t(ldt,*)
     end subroutine stdlib_sgelqt3

     pure recursive module subroutine stdlib_dgelqt3( m, n, a, lda, t, ldt, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n, ldt
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: t(ldt,*)
     end subroutine stdlib_dgelqt3


     pure recursive module subroutine stdlib_cgelqt3( m, n, a, lda, t, ldt, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n, ldt
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: t(ldt,*)
     end subroutine stdlib_cgelqt3

     pure recursive module subroutine stdlib_zgelqt3( m, n, a, lda, t, ldt, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n, ldt
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: t(ldt,*)
     end subroutine stdlib_zgelqt3


     pure recursive module subroutine stdlib_I64_sgelqt3( m, n, a, lda, t, ldt, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, m, n, ldt
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: t(ldt,*)
     end subroutine stdlib_I64_sgelqt3

     pure recursive module subroutine stdlib_I64_dgelqt3( m, n, a, lda, t, ldt, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, m, n, ldt
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: t(ldt,*)
     end subroutine stdlib_I64_dgelqt3


     pure recursive module subroutine stdlib_I64_cgelqt3( m, n, a, lda, t, ldt, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, m, n, ldt
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: t(ldt,*)
     end subroutine stdlib_I64_cgelqt3

     pure recursive module subroutine stdlib_I64_zgelqt3( m, n, a, lda, t, ldt, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, m, n, ldt
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: t(ldt,*)
     end subroutine stdlib_I64_zgelqt3


end interface 


interface 
     pure module subroutine stdlib_sgemlqt( side, trans, m, n, k, mb, v, ldv, t, ldt,c, ldc, work, info )
               
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, ldv, ldc, m, n, mb, ldt
           real(sp), intent(in) :: v(ldv,*), t(ldt,*)
           real(sp), intent(inout) :: c(ldc,*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_sgemlqt

     pure module subroutine stdlib_dgemlqt( side, trans, m, n, k, mb, v, ldv, t, ldt,c, ldc, work, info )
               
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, ldv, ldc, m, n, mb, ldt
           real(dp), intent(in) :: v(ldv,*), t(ldt,*)
           real(dp), intent(inout) :: c(ldc,*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_dgemlqt


     pure module subroutine stdlib_cgemlqt( side, trans, m, n, k, mb, v, ldv, t, ldt,c, ldc, work, info )
               
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, ldv, ldc, m, n, mb, ldt
           complex(sp), intent(in) :: v(ldv,*), t(ldt,*)
           complex(sp), intent(inout) :: c(ldc,*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_cgemlqt

     pure module subroutine stdlib_zgemlqt( side, trans, m, n, k, mb, v, ldv, t, ldt,c, ldc, work, info )
               
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, ldv, ldc, m, n, mb, ldt
           complex(dp), intent(in) :: v(ldv,*), t(ldt,*)
           complex(dp), intent(inout) :: c(ldc,*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_zgemlqt


     pure module subroutine stdlib_I64_sgemlqt( side, trans, m, n, k, mb, v, ldv, t, ldt,c, ldc, work, info )
               
           character, intent(in) :: side, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, ldv, ldc, m, n, mb, ldt
           real(sp), intent(in) :: v(ldv,*), t(ldt,*)
           real(sp), intent(inout) :: c(ldc,*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_I64_sgemlqt

     pure module subroutine stdlib_I64_dgemlqt( side, trans, m, n, k, mb, v, ldv, t, ldt,c, ldc, work, info )
               
           character, intent(in) :: side, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, ldv, ldc, m, n, mb, ldt
           real(dp), intent(in) :: v(ldv,*), t(ldt,*)
           real(dp), intent(inout) :: c(ldc,*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_I64_dgemlqt


     pure module subroutine stdlib_I64_cgemlqt( side, trans, m, n, k, mb, v, ldv, t, ldt,c, ldc, work, info )
               
           character, intent(in) :: side, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, ldv, ldc, m, n, mb, ldt
           complex(sp), intent(in) :: v(ldv,*), t(ldt,*)
           complex(sp), intent(inout) :: c(ldc,*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_I64_cgemlqt

     pure module subroutine stdlib_I64_zgemlqt( side, trans, m, n, k, mb, v, ldv, t, ldt,c, ldc, work, info )
               
           character, intent(in) :: side, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, ldv, ldc, m, n, mb, ldt
           complex(dp), intent(in) :: v(ldv,*), t(ldt,*)
           complex(dp), intent(inout) :: c(ldc,*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_I64_zgemlqt


end interface 


interface 
     pure module subroutine stdlib_slaswlq( m, n, mb, nb, a, lda, t, ldt, work, lwork,info)
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n, mb, nb, lwork, ldt
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: work(*), t(ldt,*)
     end subroutine stdlib_slaswlq

     pure module subroutine stdlib_dlaswlq( m, n, mb, nb, a, lda, t, ldt, work, lwork,info)
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n, mb, nb, lwork, ldt
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: work(*), t(ldt,*)
     end subroutine stdlib_dlaswlq


     pure module subroutine stdlib_claswlq( m, n, mb, nb, a, lda, t, ldt, work, lwork,info)
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n, mb, nb, lwork, ldt
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: work(*), t(ldt,*)
     end subroutine stdlib_claswlq

     pure module subroutine stdlib_zlaswlq( m, n, mb, nb, a, lda, t, ldt, work, lwork,info)
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n, mb, nb, lwork, ldt
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: work(*), t(ldt,*)
     end subroutine stdlib_zlaswlq


     pure module subroutine stdlib_I64_slaswlq( m, n, mb, nb, a, lda, t, ldt, work, lwork,info)
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, m, n, mb, nb, lwork, ldt
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: work(*), t(ldt,*)
     end subroutine stdlib_I64_slaswlq

     pure module subroutine stdlib_I64_dlaswlq( m, n, mb, nb, a, lda, t, ldt, work, lwork,info)
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, m, n, mb, nb, lwork, ldt
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: work(*), t(ldt,*)
     end subroutine stdlib_I64_dlaswlq


     pure module subroutine stdlib_I64_claswlq( m, n, mb, nb, a, lda, t, ldt, work, lwork,info)
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, m, n, mb, nb, lwork, ldt
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: work(*), t(ldt,*)
     end subroutine stdlib_I64_claswlq

     pure module subroutine stdlib_I64_zlaswlq( m, n, mb, nb, a, lda, t, ldt, work, lwork,info)
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, m, n, mb, nb, lwork, ldt
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: work(*), t(ldt,*)
     end subroutine stdlib_I64_zlaswlq


end interface 


interface 
     pure module subroutine stdlib_slamswlq( side, trans, m, n, k, mb, nb, a, lda, t,ldt, c, ldc, work, &
               lwork, info )
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n, k, mb, nb, ldt, lwork, ldc
           real(sp), intent(in) :: a(lda,*), t(ldt,*)
           real(sp), intent(out) :: work(*)
           real(sp), intent(inout) :: c(ldc,*)
     end subroutine stdlib_slamswlq

     pure module subroutine stdlib_dlamswlq( side, trans, m, n, k, mb, nb, a, lda, t,ldt, c, ldc, work, &
               lwork, info )
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n, k, mb, nb, ldt, lwork, ldc
           real(dp), intent(in) :: a(lda,*), t(ldt,*)
           real(dp), intent(out) :: work(*)
           real(dp), intent(inout) :: c(ldc,*)
     end subroutine stdlib_dlamswlq


     pure module subroutine stdlib_clamswlq( side, trans, m, n, k, mb, nb, a, lda, t,ldt, c, ldc, work, &
               lwork, info )
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n, k, mb, nb, ldt, lwork, ldc
           complex(sp), intent(in) :: a(lda,*), t(ldt,*)
           complex(sp), intent(out) :: work(*)
           complex(sp), intent(inout) :: c(ldc,*)
     end subroutine stdlib_clamswlq

     pure module subroutine stdlib_zlamswlq( side, trans, m, n, k, mb, nb, a, lda, t,ldt, c, ldc, work, &
               lwork, info )
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n, k, mb, nb, ldt, lwork, ldc
           complex(dp), intent(in) :: a(lda,*), t(ldt,*)
           complex(dp), intent(out) :: work(*)
           complex(dp), intent(inout) :: c(ldc,*)
     end subroutine stdlib_zlamswlq


     pure module subroutine stdlib_I64_slamswlq( side, trans, m, n, k, mb, nb, a, lda, t,ldt, c, ldc, work, &
               lwork, info )
           character, intent(in) :: side, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, m, n, k, mb, nb, ldt, lwork, ldc
           real(sp), intent(in) :: a(lda,*), t(ldt,*)
           real(sp), intent(out) :: work(*)
           real(sp), intent(inout) :: c(ldc,*)
     end subroutine stdlib_I64_slamswlq

     pure module subroutine stdlib_I64_dlamswlq( side, trans, m, n, k, mb, nb, a, lda, t,ldt, c, ldc, work, &
               lwork, info )
           character, intent(in) :: side, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, m, n, k, mb, nb, ldt, lwork, ldc
           real(dp), intent(in) :: a(lda,*), t(ldt,*)
           real(dp), intent(out) :: work(*)
           real(dp), intent(inout) :: c(ldc,*)
     end subroutine stdlib_I64_dlamswlq


     pure module subroutine stdlib_I64_clamswlq( side, trans, m, n, k, mb, nb, a, lda, t,ldt, c, ldc, work, &
               lwork, info )
           character, intent(in) :: side, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, m, n, k, mb, nb, ldt, lwork, ldc
           complex(sp), intent(in) :: a(lda,*), t(ldt,*)
           complex(sp), intent(out) :: work(*)
           complex(sp), intent(inout) :: c(ldc,*)
     end subroutine stdlib_I64_clamswlq

     pure module subroutine stdlib_I64_zlamswlq( side, trans, m, n, k, mb, nb, a, lda, t,ldt, c, ldc, work, &
               lwork, info )
           character, intent(in) :: side, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, m, n, k, mb, nb, ldt, lwork, ldc
           complex(dp), intent(in) :: a(lda,*), t(ldt,*)
           complex(dp), intent(out) :: work(*)
           complex(dp), intent(inout) :: c(ldc,*)
     end subroutine stdlib_I64_zlamswlq


end interface 


interface 
     pure module subroutine stdlib_stplqt( m, n, l, mb, a, lda, b, ldb, t, ldt, work,info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, ldt, n, m, l, mb
           real(sp), intent(inout) :: a(lda,*), b(ldb,*)
           real(sp), intent(out) :: t(ldt,*), work(*)
     end subroutine stdlib_stplqt

     pure module subroutine stdlib_dtplqt( m, n, l, mb, a, lda, b, ldb, t, ldt, work,info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, ldt, n, m, l, mb
           real(dp), intent(inout) :: a(lda,*), b(ldb,*)
           real(dp), intent(out) :: t(ldt,*), work(*)
     end subroutine stdlib_dtplqt


     pure module subroutine stdlib_ctplqt( m, n, l, mb, a, lda, b, ldb, t, ldt, work,info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, ldt, n, m, l, mb
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(sp), intent(out) :: t(ldt,*), work(*)
     end subroutine stdlib_ctplqt

     pure module subroutine stdlib_ztplqt( m, n, l, mb, a, lda, b, ldb, t, ldt, work,info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, ldt, n, m, l, mb
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(dp), intent(out) :: t(ldt,*), work(*)
     end subroutine stdlib_ztplqt


     pure module subroutine stdlib_I64_stplqt( m, n, l, mb, a, lda, b, ldb, t, ldt, work,info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldb, ldt, n, m, l, mb
           real(sp), intent(inout) :: a(lda,*), b(ldb,*)
           real(sp), intent(out) :: t(ldt,*), work(*)
     end subroutine stdlib_I64_stplqt

     pure module subroutine stdlib_I64_dtplqt( m, n, l, mb, a, lda, b, ldb, t, ldt, work,info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldb, ldt, n, m, l, mb
           real(dp), intent(inout) :: a(lda,*), b(ldb,*)
           real(dp), intent(out) :: t(ldt,*), work(*)
     end subroutine stdlib_I64_dtplqt


     pure module subroutine stdlib_I64_ctplqt( m, n, l, mb, a, lda, b, ldb, t, ldt, work,info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldb, ldt, n, m, l, mb
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(sp), intent(out) :: t(ldt,*), work(*)
     end subroutine stdlib_I64_ctplqt

     pure module subroutine stdlib_I64_ztplqt( m, n, l, mb, a, lda, b, ldb, t, ldt, work,info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldb, ldt, n, m, l, mb
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(dp), intent(out) :: t(ldt,*), work(*)
     end subroutine stdlib_I64_ztplqt


end interface 


interface 
     pure module subroutine stdlib_stplqt2( m, n, l, a, lda, b, ldb, t, ldt, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, ldt, n, m, l
           real(sp), intent(inout) :: a(lda,*), b(ldb,*)
           real(sp), intent(out) :: t(ldt,*)
     end subroutine stdlib_stplqt2

     pure module subroutine stdlib_dtplqt2( m, n, l, a, lda, b, ldb, t, ldt, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, ldt, n, m, l
           real(dp), intent(inout) :: a(lda,*), b(ldb,*)
           real(dp), intent(out) :: t(ldt,*)
     end subroutine stdlib_dtplqt2


     pure module subroutine stdlib_ctplqt2( m, n, l, a, lda, b, ldb, t, ldt, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, ldt, n, m, l
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(sp), intent(out) :: t(ldt,*)
     end subroutine stdlib_ctplqt2

     pure module subroutine stdlib_ztplqt2( m, n, l, a, lda, b, ldb, t, ldt, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldb, ldt, n, m, l
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(dp), intent(out) :: t(ldt,*)
     end subroutine stdlib_ztplqt2


     pure module subroutine stdlib_I64_stplqt2( m, n, l, a, lda, b, ldb, t, ldt, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldb, ldt, n, m, l
           real(sp), intent(inout) :: a(lda,*), b(ldb,*)
           real(sp), intent(out) :: t(ldt,*)
     end subroutine stdlib_I64_stplqt2

     pure module subroutine stdlib_I64_dtplqt2( m, n, l, a, lda, b, ldb, t, ldt, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldb, ldt, n, m, l
           real(dp), intent(inout) :: a(lda,*), b(ldb,*)
           real(dp), intent(out) :: t(ldt,*)
     end subroutine stdlib_I64_dtplqt2


     pure module subroutine stdlib_I64_ctplqt2( m, n, l, a, lda, b, ldb, t, ldt, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldb, ldt, n, m, l
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(sp), intent(out) :: t(ldt,*)
     end subroutine stdlib_I64_ctplqt2

     pure module subroutine stdlib_I64_ztplqt2( m, n, l, a, lda, b, ldb, t, ldt, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, ldb, ldt, n, m, l
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(dp), intent(out) :: t(ldt,*)
     end subroutine stdlib_I64_ztplqt2


end interface 


interface 
     pure module subroutine stdlib_stpmlqt( side, trans, m, n, k, l, mb, v, ldv, t, ldt,a, lda, b, ldb, &
               work, info )
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, ldv, lda, ldb, m, n, l, mb, ldt
           real(sp), intent(in) :: v(ldv,*), t(ldt,*)
           real(sp), intent(inout) :: a(lda,*), b(ldb,*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_stpmlqt

     pure module subroutine stdlib_dtpmlqt( side, trans, m, n, k, l, mb, v, ldv, t, ldt,a, lda, b, ldb, &
               work, info )
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, ldv, lda, ldb, m, n, l, mb, ldt
           real(dp), intent(in) :: v(ldv,*), t(ldt,*)
           real(dp), intent(inout) :: a(lda,*), b(ldb,*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_dtpmlqt


     pure module subroutine stdlib_ctpmlqt( side, trans, m, n, k, l, mb, v, ldv, t, ldt,a, lda, b, ldb, &
               work, info )
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, ldv, lda, ldb, m, n, l, mb, ldt
           complex(sp), intent(in) :: v(ldv,*), t(ldt,*)
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_ctpmlqt

     pure module subroutine stdlib_ztpmlqt( side, trans, m, n, k, l, mb, v, ldv, t, ldt,a, lda, b, ldb, &
               work, info )
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, ldv, lda, ldb, m, n, l, mb, ldt
           complex(dp), intent(in) :: v(ldv,*), t(ldt,*)
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_ztpmlqt


     pure module subroutine stdlib_I64_stpmlqt( side, trans, m, n, k, l, mb, v, ldv, t, ldt,a, lda, b, ldb, &
               work, info )
           character, intent(in) :: side, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, ldv, lda, ldb, m, n, l, mb, ldt
           real(sp), intent(in) :: v(ldv,*), t(ldt,*)
           real(sp), intent(inout) :: a(lda,*), b(ldb,*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_I64_stpmlqt

     pure module subroutine stdlib_I64_dtpmlqt( side, trans, m, n, k, l, mb, v, ldv, t, ldt,a, lda, b, ldb, &
               work, info )
           character, intent(in) :: side, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, ldv, lda, ldb, m, n, l, mb, ldt
           real(dp), intent(in) :: v(ldv,*), t(ldt,*)
           real(dp), intent(inout) :: a(lda,*), b(ldb,*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_I64_dtpmlqt


     pure module subroutine stdlib_I64_ctpmlqt( side, trans, m, n, k, l, mb, v, ldv, t, ldt,a, lda, b, ldb, &
               work, info )
           character, intent(in) :: side, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, ldv, lda, ldb, m, n, l, mb, ldt
           complex(sp), intent(in) :: v(ldv,*), t(ldt,*)
           complex(sp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_I64_ctpmlqt

     pure module subroutine stdlib_I64_ztpmlqt( side, trans, m, n, k, l, mb, v, ldv, t, ldt,a, lda, b, ldb, &
               work, info )
           character, intent(in) :: side, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, ldv, lda, ldb, m, n, l, mb, ldt
           complex(dp), intent(in) :: v(ldv,*), t(ldt,*)
           complex(dp), intent(inout) :: a(lda,*), b(ldb,*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_I64_ztpmlqt


end interface 


interface 
     pure module subroutine stdlib_sgeqlf( m, n, a, lda, tau, work, lwork, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, m, n
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_sgeqlf

     pure module subroutine stdlib_dgeqlf( m, n, a, lda, tau, work, lwork, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, m, n
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_dgeqlf


     pure module subroutine stdlib_cgeqlf( m, n, a, lda, tau, work, lwork, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, m, n
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_cgeqlf

     pure module subroutine stdlib_zgeqlf( m, n, a, lda, tau, work, lwork, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, lwork, m, n
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_zgeqlf


     pure module subroutine stdlib_I64_sgeqlf( m, n, a, lda, tau, work, lwork, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, lwork, m, n
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_I64_sgeqlf

     pure module subroutine stdlib_I64_dgeqlf( m, n, a, lda, tau, work, lwork, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, lwork, m, n
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_I64_dgeqlf


     pure module subroutine stdlib_I64_cgeqlf( m, n, a, lda, tau, work, lwork, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, lwork, m, n
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_I64_cgeqlf

     pure module subroutine stdlib_I64_zgeqlf( m, n, a, lda, tau, work, lwork, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, lwork, m, n
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_I64_zgeqlf


end interface 


interface 
     pure module subroutine stdlib_sgeql2( m, n, a, lda, tau, work, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_sgeql2

     pure module subroutine stdlib_dgeql2( m, n, a, lda, tau, work, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_dgeql2


     pure module subroutine stdlib_cgeql2( m, n, a, lda, tau, work, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_cgeql2

     pure module subroutine stdlib_zgeql2( m, n, a, lda, tau, work, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, m, n
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_zgeql2


     pure module subroutine stdlib_I64_sgeql2( m, n, a, lda, tau, work, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, m, n
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_I64_sgeql2

     pure module subroutine stdlib_I64_dgeql2( m, n, a, lda, tau, work, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, m, n
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_I64_dgeql2


     pure module subroutine stdlib_I64_cgeql2( m, n, a, lda, tau, work, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, m, n
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_I64_cgeql2

     pure module subroutine stdlib_I64_zgeql2( m, n, a, lda, tau, work, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: lda, m, n
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(out) :: tau(*), work(*)
     end subroutine stdlib_I64_zgeql2


end interface 


interface 
     pure module subroutine stdlib_cungql( m, n, k, a, lda, tau, work, lwork, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, lwork, m, n
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(in) :: tau(*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_cungql

     pure module subroutine stdlib_zungql( m, n, k, a, lda, tau, work, lwork, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, lwork, m, n
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(in) :: tau(*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_zungql


     pure module subroutine stdlib_I64_cungql( m, n, k, a, lda, tau, work, lwork, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, lda, lwork, m, n
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(in) :: tau(*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_I64_cungql

     pure module subroutine stdlib_I64_zungql( m, n, k, a, lda, tau, work, lwork, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, lda, lwork, m, n
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(in) :: tau(*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_I64_zungql


end interface 


interface 
     pure module subroutine stdlib_cunmql( side, trans, m, n, k, a, lda, tau, c, ldc,work, lwork, info )
               
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, ldc, lwork, m, n
           complex(sp), intent(inout) :: a(lda,*), c(ldc,*)
           complex(sp), intent(in) :: tau(*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_cunmql

     pure module subroutine stdlib_zunmql( side, trans, m, n, k, a, lda, tau, c, ldc,work, lwork, info )
               
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, ldc, lwork, m, n
           complex(dp), intent(inout) :: a(lda,*), c(ldc,*)
           complex(dp), intent(in) :: tau(*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_zunmql


     pure module subroutine stdlib_I64_cunmql( side, trans, m, n, k, a, lda, tau, c, ldc,work, lwork, info )
               
           character, intent(in) :: side, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, lda, ldc, lwork, m, n
           complex(sp), intent(inout) :: a(lda,*), c(ldc,*)
           complex(sp), intent(in) :: tau(*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_I64_cunmql

     pure module subroutine stdlib_I64_zunmql( side, trans, m, n, k, a, lda, tau, c, ldc,work, lwork, info )
               
           character, intent(in) :: side, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, lda, ldc, lwork, m, n
           complex(dp), intent(inout) :: a(lda,*), c(ldc,*)
           complex(dp), intent(in) :: tau(*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_I64_zunmql


end interface 


interface 
     pure module subroutine stdlib_cung2l( m, n, k, a, lda, tau, work, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, m, n
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(in) :: tau(*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_cung2l

     pure module subroutine stdlib_zung2l( m, n, k, a, lda, tau, work, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, m, n
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(in) :: tau(*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_zung2l


     pure module subroutine stdlib_I64_cung2l( m, n, k, a, lda, tau, work, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, lda, m, n
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(in) :: tau(*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_I64_cung2l

     pure module subroutine stdlib_I64_zung2l( m, n, k, a, lda, tau, work, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, lda, m, n
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(in) :: tau(*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_I64_zung2l


end interface 


interface 
     pure module subroutine stdlib_cunm2l( side, trans, m, n, k, a, lda, tau, c, ldc,work, info )
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, ldc, m, n
           complex(sp), intent(inout) :: a(lda,*), c(ldc,*)
           complex(sp), intent(in) :: tau(*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_cunm2l

     pure module subroutine stdlib_zunm2l( side, trans, m, n, k, a, lda, tau, c, ldc,work, info )
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, ldc, m, n
           complex(dp), intent(inout) :: a(lda,*), c(ldc,*)
           complex(dp), intent(in) :: tau(*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_zunm2l


     pure module subroutine stdlib_I64_cunm2l( side, trans, m, n, k, a, lda, tau, c, ldc,work, info )
           character, intent(in) :: side, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, lda, ldc, m, n
           complex(sp), intent(inout) :: a(lda,*), c(ldc,*)
           complex(sp), intent(in) :: tau(*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_I64_cunm2l

     pure module subroutine stdlib_I64_zunm2l( side, trans, m, n, k, a, lda, tau, c, ldc,work, info )
           character, intent(in) :: side, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, lda, ldc, m, n
           complex(dp), intent(inout) :: a(lda,*), c(ldc,*)
           complex(dp), intent(in) :: tau(*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_I64_zunm2l


end interface 


interface 
     pure module subroutine stdlib_sorgql( m, n, k, a, lda, tau, work, lwork, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, lwork, m, n
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(in) :: tau(*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_sorgql

     pure module subroutine stdlib_dorgql( m, n, k, a, lda, tau, work, lwork, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, lwork, m, n
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(in) :: tau(*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_dorgql


     pure module subroutine stdlib_I64_sorgql( m, n, k, a, lda, tau, work, lwork, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, lda, lwork, m, n
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(in) :: tau(*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_I64_sorgql

     pure module subroutine stdlib_I64_dorgql( m, n, k, a, lda, tau, work, lwork, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, lda, lwork, m, n
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(in) :: tau(*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_I64_dorgql


end interface 


interface 
     pure module subroutine stdlib_sormql( side, trans, m, n, k, a, lda, tau, c, ldc,work, lwork, info )
               
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, ldc, lwork, m, n
           real(sp), intent(inout) :: a(lda,*), c(ldc,*)
           real(sp), intent(in) :: tau(*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_sormql

     pure module subroutine stdlib_dormql( side, trans, m, n, k, a, lda, tau, c, ldc,work, lwork, info )
               
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, ldc, lwork, m, n
           real(dp), intent(inout) :: a(lda,*), c(ldc,*)
           real(dp), intent(in) :: tau(*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_dormql


     pure module subroutine stdlib_I64_sormql( side, trans, m, n, k, a, lda, tau, c, ldc,work, lwork, info )
               
           character, intent(in) :: side, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, lda, ldc, lwork, m, n
           real(sp), intent(inout) :: a(lda,*), c(ldc,*)
           real(sp), intent(in) :: tau(*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_I64_sormql

     pure module subroutine stdlib_I64_dormql( side, trans, m, n, k, a, lda, tau, c, ldc,work, lwork, info )
               
           character, intent(in) :: side, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, lda, ldc, lwork, m, n
           real(dp), intent(inout) :: a(lda,*), c(ldc,*)
           real(dp), intent(in) :: tau(*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_I64_dormql


end interface 


interface 
     pure module subroutine stdlib_sorg2l( m, n, k, a, lda, tau, work, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, m, n
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(in) :: tau(*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_sorg2l

     pure module subroutine stdlib_dorg2l( m, n, k, a, lda, tau, work, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, m, n
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(in) :: tau(*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_dorg2l


     pure module subroutine stdlib_I64_sorg2l( m, n, k, a, lda, tau, work, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, lda, m, n
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(in) :: tau(*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_I64_sorg2l

     pure module subroutine stdlib_I64_dorg2l( m, n, k, a, lda, tau, work, info )
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, lda, m, n
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(in) :: tau(*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_I64_dorg2l


end interface 


interface 
     pure module subroutine stdlib_sorm2l( side, trans, m, n, k, a, lda, tau, c, ldc,work, info )
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, ldc, m, n
           real(sp), intent(inout) :: a(lda,*), c(ldc,*)
           real(sp), intent(in) :: tau(*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_sorm2l

     pure module subroutine stdlib_dorm2l( side, trans, m, n, k, a, lda, tau, c, ldc,work, info )
           character, intent(in) :: side, trans
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: k, lda, ldc, m, n
           real(dp), intent(inout) :: a(lda,*), c(ldc,*)
           real(dp), intent(in) :: tau(*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_dorm2l


     pure module subroutine stdlib_I64_sorm2l( side, trans, m, n, k, a, lda, tau, c, ldc,work, info )
           character, intent(in) :: side, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, lda, ldc, m, n
           real(sp), intent(inout) :: a(lda,*), c(ldc,*)
           real(sp), intent(in) :: tau(*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_I64_sorm2l

     pure module subroutine stdlib_I64_dorm2l( side, trans, m, n, k, a, lda, tau, c, ldc,work, info )
           character, intent(in) :: side, trans
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: k, lda, ldc, m, n
           real(dp), intent(inout) :: a(lda,*), c(ldc,*)
           real(dp), intent(in) :: tau(*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_I64_dorm2l


end interface 


interface 
     pure module subroutine stdlib_cunm22( side, trans, m, n, n1, n2, q, ldq, c, ldc,work, lwork, info )
               
           character, intent(in) :: side, trans
           integer(ilp), intent(in) :: m, n, n1, n2, ldq, ldc, lwork
           integer(ilp), intent(out) :: info
           complex(sp), intent(in) :: q(ldq,*)
           complex(sp), intent(inout) :: c(ldc,*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_cunm22

     pure module subroutine stdlib_zunm22( side, trans, m, n, n1, n2, q, ldq, c, ldc,work, lwork, info )
               
           character, intent(in) :: side, trans
           integer(ilp), intent(in) :: m, n, n1, n2, ldq, ldc, lwork
           integer(ilp), intent(out) :: info
           complex(dp), intent(in) :: q(ldq,*)
           complex(dp), intent(inout) :: c(ldc,*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_zunm22


     pure module subroutine stdlib_I64_cunm22( side, trans, m, n, n1, n2, q, ldq, c, ldc,work, lwork, info )
               
           character, intent(in) :: side, trans
           integer(ilp64), intent(in) :: m, n, n1, n2, ldq, ldc, lwork
           integer(ilp64), intent(out) :: info
           complex(sp), intent(in) :: q(ldq,*)
           complex(sp), intent(inout) :: c(ldc,*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_I64_cunm22

     pure module subroutine stdlib_I64_zunm22( side, trans, m, n, n1, n2, q, ldq, c, ldc,work, lwork, info )
               
           character, intent(in) :: side, trans
           integer(ilp64), intent(in) :: m, n, n1, n2, ldq, ldc, lwork
           integer(ilp64), intent(out) :: info
           complex(dp), intent(in) :: q(ldq,*)
           complex(dp), intent(inout) :: c(ldc,*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_I64_zunm22


end interface 

end module stdlib_lapack_orthogonal_factors
