module stdlib_lapack_base
  use stdlib_linalg_constants
  use stdlib_linalg_lapack_aux
  use stdlib_linalg_blas
  implicit none

interface 
     pure real(sp) module function stdlib_slamch( cmach )
           character, intent(in) :: cmach
     end function stdlib_slamch

     pure real(dp) module function stdlib_dlamch( cmach )
           character, intent(in) :: cmach
     end function stdlib_dlamch


end interface 


interface 
     pure real(sp) module function stdlib_slamc3( a, b )
           real(sp), intent(in) :: a, b
     end function stdlib_slamc3

     pure real(dp) module function stdlib_dlamc3( a, b )
           real(dp), intent(in) :: a, b
     end function stdlib_dlamc3


end interface 


interface 
     pure module subroutine stdlib_slabad( small, large )
           real(sp), intent(inout) :: large, small
     end subroutine stdlib_slabad

     pure module subroutine stdlib_dlabad( small, large )
           real(dp), intent(inout) :: large, small
     end subroutine stdlib_dlabad


end interface 


interface 
     pure real(sp) module function stdlib_scsum1( n, cx, incx )
           integer(ilp), intent(in) :: incx, n
           complex(sp), intent(in) :: cx(*)
     end function stdlib_scsum1

end interface 


interface 
     pure real(dp) module function stdlib_dzsum1( n, cx, incx )
           integer(ilp), intent(in) :: incx, n
           complex(dp), intent(in) :: cx(*)
     end function stdlib_dzsum1


end interface 


interface 
     pure module subroutine stdlib_slaqsb( uplo, n, kd, ab, ldab, s, scond, amax, equed )
           character, intent(out) :: equed
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: kd, ldab, n
           real(sp), intent(in) :: amax, scond
           real(sp), intent(inout) :: ab(ldab,*)
           real(sp), intent(in) :: s(*)
     end subroutine stdlib_slaqsb

     pure module subroutine stdlib_dlaqsb( uplo, n, kd, ab, ldab, s, scond, amax, equed )
           character, intent(out) :: equed
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: kd, ldab, n
           real(dp), intent(in) :: amax, scond
           real(dp), intent(inout) :: ab(ldab,*)
           real(dp), intent(in) :: s(*)
     end subroutine stdlib_dlaqsb


     pure module subroutine stdlib_claqsb( uplo, n, kd, ab, ldab, s, scond, amax, equed )
           character, intent(out) :: equed
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: kd, ldab, n
           real(sp), intent(in) :: amax, scond
           real(sp), intent(in) :: s(*)
           complex(sp), intent(inout) :: ab(ldab,*)
     end subroutine stdlib_claqsb

     pure module subroutine stdlib_zlaqsb( uplo, n, kd, ab, ldab, s, scond, amax, equed )
           character, intent(out) :: equed
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: kd, ldab, n
           real(dp), intent(in) :: amax, scond
           real(dp), intent(in) :: s(*)
           complex(dp), intent(inout) :: ab(ldab,*)
     end subroutine stdlib_zlaqsb


end interface 


interface 
     pure module subroutine stdlib_sladiv1( a, b, c, d, p, q )
           real(sp), intent(inout) :: a
           real(sp), intent(in) :: b, c, d
           real(sp), intent(out) :: p, q
     end subroutine stdlib_sladiv1

     pure module subroutine stdlib_dladiv1( a, b, c, d, p, q )
           real(dp), intent(inout) :: a
           real(dp), intent(in) :: b, c, d
           real(dp), intent(out) :: p, q
     end subroutine stdlib_dladiv1


end interface 


interface 
     pure real(sp) module function stdlib_sladiv2( a, b, c, d, r, t )
           real(sp), intent(in) :: a, b, c, d, r, t
     end function stdlib_sladiv2

     pure real(dp) module function stdlib_dladiv2( a, b, c, d, r, t )
           real(dp), intent(in) :: a, b, c, d, r, t
     end function stdlib_dladiv2


end interface 


interface 
     pure module subroutine stdlib_crot( n, cx, incx, cy, incy, c, s )
           integer(ilp), intent(in) :: incx, incy, n
           real(sp), intent(in) :: c
           complex(sp), intent(in) :: s
           complex(sp), intent(inout) :: cx(*), cy(*)
     end subroutine stdlib_crot

     pure module subroutine stdlib_zrot( n, cx, incx, cy, incy, c, s )
           integer(ilp), intent(in) :: incx, incy, n
           real(dp), intent(in) :: c
           complex(dp), intent(in) :: s
           complex(dp), intent(inout) :: cx(*), cy(*)
     end subroutine stdlib_zrot


end interface 


interface 
     pure module subroutine stdlib_slaset( uplo, m, n, alpha, beta, a, lda )
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: lda, m, n
           real(sp), intent(in) :: alpha, beta
           real(sp), intent(out) :: a(lda,*)
     end subroutine stdlib_slaset

     pure module subroutine stdlib_dlaset( uplo, m, n, alpha, beta, a, lda )
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: lda, m, n
           real(dp), intent(in) :: alpha, beta
           real(dp), intent(out) :: a(lda,*)
     end subroutine stdlib_dlaset


     pure module subroutine stdlib_claset( uplo, m, n, alpha, beta, a, lda )
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: lda, m, n
           complex(sp), intent(in) :: alpha, beta
           complex(sp), intent(out) :: a(lda,*)
     end subroutine stdlib_claset

     pure module subroutine stdlib_zlaset( uplo, m, n, alpha, beta, a, lda )
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: lda, m, n
           complex(dp), intent(in) :: alpha, beta
           complex(dp), intent(out) :: a(lda,*)
     end subroutine stdlib_zlaset


end interface 


interface 
     pure module subroutine stdlib_slarnv( idist, iseed, n, x )
           integer(ilp), intent(in) :: idist, n
           integer(ilp), intent(inout) :: iseed(4_ilp)
           real(sp), intent(out) :: x(*)
     end subroutine stdlib_slarnv

     pure module subroutine stdlib_dlarnv( idist, iseed, n, x )
           integer(ilp), intent(in) :: idist, n
           integer(ilp), intent(inout) :: iseed(4_ilp)
           real(dp), intent(out) :: x(*)
     end subroutine stdlib_dlarnv


     pure module subroutine stdlib_clarnv( idist, iseed, n, x )
           integer(ilp), intent(in) :: idist, n
           integer(ilp), intent(inout) :: iseed(4_ilp)
           complex(sp), intent(out) :: x(*)
     end subroutine stdlib_clarnv

     pure module subroutine stdlib_zlarnv( idist, iseed, n, x )
           integer(ilp), intent(in) :: idist, n
           integer(ilp), intent(inout) :: iseed(4_ilp)
           complex(dp), intent(out) :: x(*)
     end subroutine stdlib_zlarnv


end interface 


interface 
     pure module subroutine stdlib_slaruv( iseed, n, x )
           integer(ilp), intent(in) :: n
           integer(ilp), intent(inout) :: iseed(4_ilp)
           real(sp), intent(out) :: x(n)
     end subroutine stdlib_slaruv

     pure module subroutine stdlib_dlaruv( iseed, n, x )
           integer(ilp), intent(in) :: n
           integer(ilp), intent(inout) :: iseed(4_ilp)
           real(dp), intent(out) :: x(n)
     end subroutine stdlib_dlaruv


end interface 


interface 
     pure module subroutine stdlib_slacpy( uplo, m, n, a, lda, b, ldb )
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: lda, ldb, m, n
           real(sp), intent(in) :: a(lda,*)
           real(sp), intent(out) :: b(ldb,*)
     end subroutine stdlib_slacpy

     pure module subroutine stdlib_dlacpy( uplo, m, n, a, lda, b, ldb )
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: lda, ldb, m, n
           real(dp), intent(in) :: a(lda,*)
           real(dp), intent(out) :: b(ldb,*)
     end subroutine stdlib_dlacpy


     pure module subroutine stdlib_clacpy( uplo, m, n, a, lda, b, ldb )
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: lda, ldb, m, n
           complex(sp), intent(in) :: a(lda,*)
           complex(sp), intent(out) :: b(ldb,*)
     end subroutine stdlib_clacpy

     pure module subroutine stdlib_zlacpy( uplo, m, n, a, lda, b, ldb )
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: lda, ldb, m, n
           complex(dp), intent(in) :: a(lda,*)
           complex(dp), intent(out) :: b(ldb,*)
     end subroutine stdlib_zlacpy


end interface 


interface 
     pure module subroutine stdlib_clacp2( uplo, m, n, a, lda, b, ldb )
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: lda, ldb, m, n
           real(sp), intent(in) :: a(lda,*)
           complex(sp), intent(out) :: b(ldb,*)
     end subroutine stdlib_clacp2

     pure module subroutine stdlib_zlacp2( uplo, m, n, a, lda, b, ldb )
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: lda, ldb, m, n
           real(dp), intent(in) :: a(lda,*)
           complex(dp), intent(out) :: b(ldb,*)
     end subroutine stdlib_zlacp2


end interface 


interface 
     pure module subroutine stdlib_stfttp( transr, uplo, n, arf, ap, info )
           character, intent(in) :: transr, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           real(sp), intent(out) :: ap(0_ilp:*)
           real(sp), intent(in) :: arf(0_ilp:*)
     end subroutine stdlib_stfttp

     pure module subroutine stdlib_dtfttp( transr, uplo, n, arf, ap, info )
           character, intent(in) :: transr, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           real(dp), intent(out) :: ap(0_ilp:*)
           real(dp), intent(in) :: arf(0_ilp:*)
     end subroutine stdlib_dtfttp


     pure module subroutine stdlib_ctfttp( transr, uplo, n, arf, ap, info )
           character, intent(in) :: transr, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           complex(sp), intent(out) :: ap(0_ilp:*)
           complex(sp), intent(in) :: arf(0_ilp:*)
     end subroutine stdlib_ctfttp

     pure module subroutine stdlib_ztfttp( transr, uplo, n, arf, ap, info )
           character, intent(in) :: transr, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           complex(dp), intent(out) :: ap(0_ilp:*)
           complex(dp), intent(in) :: arf(0_ilp:*)
     end subroutine stdlib_ztfttp


end interface 


interface 
     pure module subroutine stdlib_stfttr( transr, uplo, n, arf, a, lda, info )
           character, intent(in) :: transr, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n, lda
           real(sp), intent(out) :: a(0_ilp:lda-1,0_ilp:*)
           real(sp), intent(in) :: arf(0_ilp:*)
     end subroutine stdlib_stfttr

     pure module subroutine stdlib_dtfttr( transr, uplo, n, arf, a, lda, info )
           character, intent(in) :: transr, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n, lda
           real(dp), intent(out) :: a(0_ilp:lda-1,0_ilp:*)
           real(dp), intent(in) :: arf(0_ilp:*)
     end subroutine stdlib_dtfttr


     pure module subroutine stdlib_ctfttr( transr, uplo, n, arf, a, lda, info )
           character, intent(in) :: transr, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n, lda
           complex(sp), intent(out) :: a(0_ilp:lda-1,0_ilp:*)
           complex(sp), intent(in) :: arf(0_ilp:*)
     end subroutine stdlib_ctfttr

     pure module subroutine stdlib_ztfttr( transr, uplo, n, arf, a, lda, info )
           character, intent(in) :: transr, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n, lda
           complex(dp), intent(out) :: a(0_ilp:lda-1,0_ilp:*)
           complex(dp), intent(in) :: arf(0_ilp:*)
     end subroutine stdlib_ztfttr


end interface 


interface 
     pure module subroutine stdlib_stpttf( transr, uplo, n, ap, arf, info )
           character, intent(in) :: transr, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           real(sp), intent(in) :: ap(0_ilp:*)
           real(sp), intent(out) :: arf(0_ilp:*)
     end subroutine stdlib_stpttf

     pure module subroutine stdlib_dtpttf( transr, uplo, n, ap, arf, info )
           character, intent(in) :: transr, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           real(dp), intent(in) :: ap(0_ilp:*)
           real(dp), intent(out) :: arf(0_ilp:*)
     end subroutine stdlib_dtpttf


     pure module subroutine stdlib_ctpttf( transr, uplo, n, ap, arf, info )
           character, intent(in) :: transr, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           complex(sp), intent(in) :: ap(0_ilp:*)
           complex(sp), intent(out) :: arf(0_ilp:*)
     end subroutine stdlib_ctpttf

     pure module subroutine stdlib_ztpttf( transr, uplo, n, ap, arf, info )
           character, intent(in) :: transr, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           complex(dp), intent(in) :: ap(0_ilp:*)
           complex(dp), intent(out) :: arf(0_ilp:*)
     end subroutine stdlib_ztpttf


end interface 


interface 
     pure module subroutine stdlib_stpttr( uplo, n, ap, a, lda, info )
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n, lda
           real(sp), intent(out) :: a(lda,*)
           real(sp), intent(in) :: ap(*)
     end subroutine stdlib_stpttr

     pure module subroutine stdlib_dtpttr( uplo, n, ap, a, lda, info )
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n, lda
           real(dp), intent(out) :: a(lda,*)
           real(dp), intent(in) :: ap(*)
     end subroutine stdlib_dtpttr


     pure module subroutine stdlib_ctpttr( uplo, n, ap, a, lda, info )
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n, lda
           complex(sp), intent(out) :: a(lda,*)
           complex(sp), intent(in) :: ap(*)
     end subroutine stdlib_ctpttr

     pure module subroutine stdlib_ztpttr( uplo, n, ap, a, lda, info )
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n, lda
           complex(dp), intent(out) :: a(lda,*)
           complex(dp), intent(in) :: ap(*)
     end subroutine stdlib_ztpttr


end interface 


interface 
     pure module subroutine stdlib_strttf( transr, uplo, n, a, lda, arf, info )
           character, intent(in) :: transr, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n, lda
           real(sp), intent(in) :: a(0_ilp:lda-1,0_ilp:*)
           real(sp), intent(out) :: arf(0_ilp:*)
     end subroutine stdlib_strttf

     pure module subroutine stdlib_dtrttf( transr, uplo, n, a, lda, arf, info )
           character, intent(in) :: transr, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n, lda
           real(dp), intent(in) :: a(0_ilp:lda-1,0_ilp:*)
           real(dp), intent(out) :: arf(0_ilp:*)
     end subroutine stdlib_dtrttf


     pure module subroutine stdlib_ctrttf( transr, uplo, n, a, lda, arf, info )
           character, intent(in) :: transr, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n, lda
           complex(sp), intent(in) :: a(0_ilp:lda-1,0_ilp:*)
           complex(sp), intent(out) :: arf(0_ilp:*)
     end subroutine stdlib_ctrttf

     pure module subroutine stdlib_ztrttf( transr, uplo, n, a, lda, arf, info )
           character, intent(in) :: transr, uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n, lda
           complex(dp), intent(in) :: a(0_ilp:lda-1,0_ilp:*)
           complex(dp), intent(out) :: arf(0_ilp:*)
     end subroutine stdlib_ztrttf


end interface 


interface 
     pure module subroutine stdlib_strttp( uplo, n, a, lda, ap, info )
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n, lda
           real(sp), intent(in) :: a(lda,*)
           real(sp), intent(out) :: ap(*)
     end subroutine stdlib_strttp

     pure module subroutine stdlib_dtrttp( uplo, n, a, lda, ap, info )
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n, lda
           real(dp), intent(in) :: a(lda,*)
           real(dp), intent(out) :: ap(*)
     end subroutine stdlib_dtrttp


     pure module subroutine stdlib_ctrttp( uplo, n, a, lda, ap, info )
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n, lda
           complex(sp), intent(in) :: a(lda,*)
           complex(sp), intent(out) :: ap(*)
     end subroutine stdlib_ctrttp

     pure module subroutine stdlib_ztrttp( uplo, n, a, lda, ap, info )
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n, lda
           complex(dp), intent(in) :: a(lda,*)
           complex(dp), intent(out) :: ap(*)
     end subroutine stdlib_ztrttp


end interface 


interface 
     pure module subroutine stdlib_dlag2s( m, n, a, lda, sa, ldsa, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldsa, m, n
           real(sp), intent(out) :: sa(ldsa,*)
           real(dp), intent(in) :: a(lda,*)
     end subroutine stdlib_dlag2s


end interface 


interface 
     pure module subroutine stdlib_dlat2s( uplo, n, a, lda, sa, ldsa, info )
           character, intent(in) :: uplo
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldsa, n
           real(sp), intent(out) :: sa(ldsa,*)
           real(dp), intent(in) :: a(lda,*)
     end subroutine stdlib_dlat2s


end interface 


interface 
     pure module subroutine stdlib_slag2d( m, n, sa, ldsa, a, lda, info )
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: lda, ldsa, m, n
           real(sp), intent(in) :: sa(ldsa,*)
           real(dp), intent(out) :: a(lda,*)
     end subroutine stdlib_slag2d

end interface 


interface 
     pure logical(lk) module function stdlib_sisnan( sin )
           real(sp), intent(in) :: sin
     end function stdlib_sisnan

     pure logical(lk) module function stdlib_disnan( din )
           real(dp), intent(in) :: din
     end function stdlib_disnan


end interface 


interface 
     pure logical(lk) module function stdlib_slaisnan( sin1, sin2 )
           real(sp), intent(in) :: sin1, sin2
     end function stdlib_slaisnan

     pure logical(lk) module function stdlib_dlaisnan( din1, din2 )
           real(dp), intent(in) :: din1, din2
     end function stdlib_dlaisnan


end interface 


interface 
     pure module subroutine stdlib_sladiv( a, b, c, d, p, q )
           real(sp), intent(in) :: a, b, c, d
           real(sp), intent(out) :: p, q
     end subroutine stdlib_sladiv

     pure module subroutine stdlib_dladiv( a, b, c, d, p, q )
           real(dp), intent(in) :: a, b, c, d
           real(dp), intent(out) :: p, q
     end subroutine stdlib_dladiv


     pure complex(sp) module function stdlib_cladiv( x, y )
           complex(sp), intent(in) :: x, y
     end function stdlib_cladiv

     pure complex(dp)     module function stdlib_zladiv( x, y )
           complex(dp), intent(in) :: x, y
     end function stdlib_zladiv


end interface 


interface 
     pure real(sp) module function stdlib_slapy2( x, y )
           real(sp), intent(in) :: x, y
     end function stdlib_slapy2

     pure real(dp) module function stdlib_dlapy2( x, y )
           real(dp), intent(in) :: x, y
     end function stdlib_dlapy2


end interface 


interface 
     pure real(sp) module function stdlib_slapy3( x, y, z )
           real(sp), intent(in) :: x, y, z
     end function stdlib_slapy3

     pure real(dp) module function stdlib_dlapy3( x, y, z )
           real(dp), intent(in) :: x, y, z
     end function stdlib_dlapy3


end interface 


interface 
     pure module subroutine stdlib_clacgv( n, x, incx )
           integer(ilp), intent(in) :: incx, n
           complex(sp), intent(inout) :: x(*)
     end subroutine stdlib_clacgv

     pure module subroutine stdlib_zlacgv( n, x, incx )
           integer(ilp), intent(in) :: incx, n
           complex(dp), intent(inout) :: x(*)
     end subroutine stdlib_zlacgv


end interface 


interface 
     pure module subroutine stdlib_slasrt( id, n, d, info )
           character, intent(in) :: id
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           real(sp), intent(inout) :: d(*)
     end subroutine stdlib_slasrt

     pure module subroutine stdlib_dlasrt( id, n, d, info )
           character, intent(in) :: id
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: n
           real(dp), intent(inout) :: d(*)
     end subroutine stdlib_dlasrt


end interface 


interface 
     pure module subroutine stdlib_slassq( n, x, incx, scl, sumsq )
     integer(ilp), intent(in) :: incx, n
        real(sp), intent(inout) :: scl, sumsq
        real(sp), intent(in) :: x(*)
     end subroutine stdlib_slassq

     pure module subroutine stdlib_dlassq( n, x, incx, scl, sumsq )
     integer(ilp), intent(in) :: incx, n
        real(dp), intent(inout) :: scl, sumsq
        real(dp), intent(in) :: x(*)
     end subroutine stdlib_dlassq


     pure module subroutine stdlib_classq( n, x, incx, scl, sumsq )
     integer(ilp), intent(in) :: incx, n
        real(sp), intent(inout) :: scl, sumsq
        complex(sp), intent(in) :: x(*)
     end subroutine stdlib_classq

     pure module subroutine stdlib_zlassq( n, x, incx, scl, sumsq )
     integer(ilp), intent(in) :: incx, n
        real(dp), intent(inout) :: scl, sumsq
        complex(dp), intent(in) :: x(*)
     end subroutine stdlib_zlassq


end interface 


interface 
     pure module subroutine stdlib_srscl( n, sa, sx, incx )
           integer(ilp), intent(in) :: incx, n
           real(sp), intent(in) :: sa
           real(sp), intent(inout) :: sx(*)
     end subroutine stdlib_srscl

     pure module subroutine stdlib_drscl( n, sa, sx, incx )
           integer(ilp), intent(in) :: incx, n
           real(dp), intent(in) :: sa
           real(dp), intent(inout) :: sx(*)
     end subroutine stdlib_drscl


end interface 


interface 
     pure module subroutine stdlib_csrscl( n, sa, sx, incx )
           integer(ilp), intent(in) :: incx, n
           real(sp), intent(in) :: sa
           complex(sp), intent(inout) :: sx(*)
     end subroutine stdlib_csrscl

end interface 


interface 
     pure module subroutine stdlib_zdrscl( n, sa, sx, incx )
           integer(ilp), intent(in) :: incx, n
           real(dp), intent(in) :: sa
           complex(dp), intent(inout) :: sx(*)
     end subroutine stdlib_zdrscl


end interface 


interface 
     pure module subroutine stdlib_slascl( type, kl, ku, cfrom, cto, m, n, a, lda, info )
           character, intent(in) :: type
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kl, ku, lda, m, n
           real(sp), intent(in) :: cfrom, cto
           real(sp), intent(inout) :: a(lda,*)
     end subroutine stdlib_slascl

     pure module subroutine stdlib_dlascl( type, kl, ku, cfrom, cto, m, n, a, lda, info )
           character, intent(in) :: type
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kl, ku, lda, m, n
           real(dp), intent(in) :: cfrom, cto
           real(dp), intent(inout) :: a(lda,*)
     end subroutine stdlib_dlascl


     pure module subroutine stdlib_clascl( type, kl, ku, cfrom, cto, m, n, a, lda, info )
           character, intent(in) :: type
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kl, ku, lda, m, n
           real(sp), intent(in) :: cfrom, cto
           complex(sp), intent(inout) :: a(lda,*)
     end subroutine stdlib_clascl

     pure module subroutine stdlib_zlascl( type, kl, ku, cfrom, cto, m, n, a, lda, info )
           character, intent(in) :: type
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: kl, ku, lda, m, n
           real(dp), intent(in) :: cfrom, cto
           complex(dp), intent(inout) :: a(lda,*)
     end subroutine stdlib_zlascl


end interface 


interface 
     module subroutine stdlib_sla_geamv( trans, m, n, alpha, a, lda, x, incx, beta,y, incy )
           real(sp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, lda, m, n, trans
           real(sp), intent(in) :: a(lda,*), x(*)
           real(sp), intent(inout) :: y(*)
     end subroutine stdlib_sla_geamv

     module subroutine stdlib_dla_geamv ( trans, m, n, alpha, a, lda, x, incx, beta,y, incy )
           real(dp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, lda, m, n, trans
           real(dp), intent(in) :: a(lda,*), x(*)
           real(dp), intent(inout) :: y(*)
     end subroutine stdlib_dla_geamv


     module subroutine stdlib_cla_geamv( trans, m, n, alpha, a, lda, x, incx, beta,y, incy )
           real(sp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, lda, m, n
           integer(ilp), intent(in) :: trans
           complex(sp), intent(in) :: a(lda,*), x(*)
           real(sp), intent(inout) :: y(*)
     end subroutine stdlib_cla_geamv

     module subroutine stdlib_zla_geamv( trans, m, n, alpha, a, lda, x, incx, beta,y, incy )
           real(dp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, lda, m, n
           integer(ilp), intent(in) :: trans
           complex(dp), intent(in) :: a(lda,*), x(*)
           real(dp), intent(inout) :: y(*)
     end subroutine stdlib_zla_geamv


end interface 


interface 
     module subroutine stdlib_sla_gbamv( trans, m, n, kl, ku, alpha, ab, ldab, x,incx, beta, y, incy )
               
           real(sp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, ldab, m, n, kl, ku, trans
           real(sp), intent(in) :: ab(ldab,*), x(*)
           real(sp), intent(inout) :: y(*)
     end subroutine stdlib_sla_gbamv

     module subroutine stdlib_dla_gbamv( trans, m, n, kl, ku, alpha, ab, ldab, x,incx, beta, y, incy )
               
           real(dp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, ldab, m, n, kl, ku, trans
           real(dp), intent(in) :: ab(ldab,*), x(*)
           real(dp), intent(inout) :: y(*)
     end subroutine stdlib_dla_gbamv


     module subroutine stdlib_cla_gbamv( trans, m, n, kl, ku, alpha, ab, ldab, x,incx, beta, y, incy )
               
           real(sp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, ldab, m, n, kl, ku, trans
           complex(sp), intent(in) :: ab(ldab,*), x(*)
           real(sp), intent(inout) :: y(*)
     end subroutine stdlib_cla_gbamv

     module subroutine stdlib_zla_gbamv( trans, m, n, kl, ku, alpha, ab, ldab, x,incx, beta, y, incy )
               
           real(dp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, ldab, m, n, kl, ku, trans
           complex(dp), intent(in) :: ab(ldab,*), x(*)
           real(dp), intent(inout) :: y(*)
     end subroutine stdlib_zla_gbamv


end interface 


interface 
     module subroutine stdlib_cla_heamv( uplo, n, alpha, a, lda, x, incx, beta, y,incy )
           real(sp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, lda, n, uplo
           complex(sp), intent(in) :: a(lda,*), x(*)
           real(sp), intent(inout) :: y(*)
     end subroutine stdlib_cla_heamv

     module subroutine stdlib_zla_heamv( uplo, n, alpha, a, lda, x, incx, beta, y,incy )
           real(dp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, lda, n, uplo
           complex(dp), intent(in) :: a(lda,*), x(*)
           real(dp), intent(inout) :: y(*)
     end subroutine stdlib_zla_heamv


end interface 


interface 
     pure module subroutine stdlib_sla_wwaddw( n, x, y, w )
           integer(ilp), intent(in) :: n
           real(sp), intent(inout) :: x(*), y(*)
           real(sp), intent(in) :: w(*)
     end subroutine stdlib_sla_wwaddw

     pure module subroutine stdlib_dla_wwaddw( n, x, y, w )
           integer(ilp), intent(in) :: n
           real(dp), intent(inout) :: x(*), y(*)
           real(dp), intent(in) :: w(*)
     end subroutine stdlib_dla_wwaddw


     pure module subroutine stdlib_cla_wwaddw( n, x, y, w )
           integer(ilp), intent(in) :: n
           complex(sp), intent(inout) :: x(*), y(*)
           complex(sp), intent(in) :: w(*)
     end subroutine stdlib_cla_wwaddw

     pure module subroutine stdlib_zla_wwaddw( n, x, y, w )
           integer(ilp), intent(in) :: n
           complex(dp), intent(inout) :: x(*), y(*)
           complex(dp), intent(in) :: w(*)
     end subroutine stdlib_zla_wwaddw


end interface 


interface 
     pure module subroutine stdlib_cspmv( uplo, n, alpha, ap, x, incx, beta, y, incy )
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: incx, incy, n
           complex(sp), intent(in) :: alpha, beta
           complex(sp), intent(in) :: ap(*), x(*)
           complex(sp), intent(inout) :: y(*)
     end subroutine stdlib_cspmv

     pure module subroutine stdlib_zspmv( uplo, n, alpha, ap, x, incx, beta, y, incy )
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: incx, incy, n
           complex(dp), intent(in) :: alpha, beta
           complex(dp), intent(in) :: ap(*), x(*)
           complex(dp), intent(inout) :: y(*)
     end subroutine stdlib_zspmv


end interface 


interface 
     pure module subroutine stdlib_cspr( uplo, n, alpha, x, incx, ap )
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: incx, n
           complex(sp), intent(in) :: alpha
           complex(sp), intent(inout) :: ap(*)
           complex(sp), intent(in) :: x(*)
     end subroutine stdlib_cspr

     pure module subroutine stdlib_zspr( uplo, n, alpha, x, incx, ap )
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: incx, n
           complex(dp), intent(in) :: alpha
           complex(dp), intent(inout) :: ap(*)
           complex(dp), intent(in) :: x(*)
     end subroutine stdlib_zspr


end interface 


interface 
     pure module subroutine stdlib_csymv( uplo, n, alpha, a, lda, x, incx, beta, y, incy )
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: incx, incy, lda, n
           complex(sp), intent(in) :: alpha, beta
           complex(sp), intent(in) :: a(lda,*), x(*)
           complex(sp), intent(inout) :: y(*)
     end subroutine stdlib_csymv

     pure module subroutine stdlib_zsymv( uplo, n, alpha, a, lda, x, incx, beta, y, incy )
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: incx, incy, lda, n
           complex(dp), intent(in) :: alpha, beta
           complex(dp), intent(in) :: a(lda,*), x(*)
           complex(dp), intent(inout) :: y(*)
     end subroutine stdlib_zsymv


end interface 


interface 
     pure module subroutine stdlib_csyr( uplo, n, alpha, x, incx, a, lda )
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: incx, lda, n
           complex(sp), intent(in) :: alpha
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(in) :: x(*)
     end subroutine stdlib_csyr

     pure module subroutine stdlib_zsyr( uplo, n, alpha, x, incx, a, lda )
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: incx, lda, n
           complex(dp), intent(in) :: alpha
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(in) :: x(*)
     end subroutine stdlib_zsyr


end interface 


interface 
     pure module subroutine stdlib_slagtm( trans, n, nrhs, alpha, dl, d, du, x, ldx, beta,b, ldb )
               
           character, intent(in) :: trans
           integer(ilp), intent(in) :: ldb, ldx, n, nrhs
           real(sp), intent(in) :: alpha, beta
           real(sp), intent(inout) :: b(ldb,*)
           real(sp), intent(in) :: d(*), dl(*), du(*), x(ldx,*)
     end subroutine stdlib_slagtm

     pure module subroutine stdlib_dlagtm( trans, n, nrhs, alpha, dl, d, du, x, ldx, beta,b, ldb )
               
           character, intent(in) :: trans
           integer(ilp), intent(in) :: ldb, ldx, n, nrhs
           real(dp), intent(in) :: alpha, beta
           real(dp), intent(inout) :: b(ldb,*)
           real(dp), intent(in) :: d(*), dl(*), du(*), x(ldx,*)
     end subroutine stdlib_dlagtm


     pure module subroutine stdlib_clagtm( trans, n, nrhs, alpha, dl, d, du, x, ldx, beta,b, ldb )
               
           character, intent(in) :: trans
           integer(ilp), intent(in) :: ldb, ldx, n, nrhs
           real(sp), intent(in) :: alpha, beta
           complex(sp), intent(inout) :: b(ldb,*)
           complex(sp), intent(in) :: d(*), dl(*), du(*), x(ldx,*)
     end subroutine stdlib_clagtm

     pure module subroutine stdlib_zlagtm( trans, n, nrhs, alpha, dl, d, du, x, ldx, beta,b, ldb )
               
           character, intent(in) :: trans
           integer(ilp), intent(in) :: ldb, ldx, n, nrhs
           real(dp), intent(in) :: alpha, beta
           complex(dp), intent(inout) :: b(ldb,*)
           complex(dp), intent(in) :: d(*), dl(*), du(*), x(ldx,*)
     end subroutine stdlib_zlagtm


end interface 


interface 
     pure module subroutine stdlib_clacrm( m, n, a, lda, b, ldb, c, ldc, rwork )
           integer(ilp), intent(in) :: lda, ldb, ldc, m, n
           real(sp), intent(in) :: b(ldb,*)
           real(sp), intent(out) :: rwork(*)
           complex(sp), intent(in) :: a(lda,*)
           complex(sp), intent(out) :: c(ldc,*)
     end subroutine stdlib_clacrm

     pure module subroutine stdlib_zlacrm( m, n, a, lda, b, ldb, c, ldc, rwork )
           integer(ilp), intent(in) :: lda, ldb, ldc, m, n
           real(dp), intent(in) :: b(ldb,*)
           real(dp), intent(out) :: rwork(*)
           complex(dp), intent(in) :: a(lda,*)
           complex(dp), intent(out) :: c(ldc,*)
     end subroutine stdlib_zlacrm


end interface 


interface 
     pure module subroutine stdlib_clarcm( m, n, a, lda, b, ldb, c, ldc, rwork )
           integer(ilp), intent(in) :: lda, ldb, ldc, m, n
           real(sp), intent(in) :: a(lda,*)
           real(sp), intent(out) :: rwork(*)
           complex(sp), intent(in) :: b(ldb,*)
           complex(sp), intent(out) :: c(ldc,*)
     end subroutine stdlib_clarcm

     pure module subroutine stdlib_zlarcm( m, n, a, lda, b, ldb, c, ldc, rwork )
           integer(ilp), intent(in) :: lda, ldb, ldc, m, n
           real(dp), intent(in) :: a(lda,*)
           real(dp), intent(out) :: rwork(*)
           complex(dp), intent(in) :: b(ldb,*)
           complex(dp), intent(out) :: c(ldc,*)
     end subroutine stdlib_zlarcm


end interface 


interface 
     pure module subroutine stdlib_chfrk( transr, uplo, trans, n, k, alpha, a, lda, beta,c )
           real(sp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: k, lda, n
           character, intent(in) :: trans, transr, uplo
           complex(sp), intent(in) :: a(lda,*)
           complex(sp), intent(inout) :: c(*)
     end subroutine stdlib_chfrk

     pure module subroutine stdlib_zhfrk( transr, uplo, trans, n, k, alpha, a, lda, beta,c )
           real(dp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: k, lda, n
           character, intent(in) :: trans, transr, uplo
           complex(dp), intent(in) :: a(lda,*)
           complex(dp), intent(inout) :: c(*)
     end subroutine stdlib_zhfrk


end interface 


interface 
     pure module subroutine stdlib_stfsm( transr, side, uplo, trans, diag, m, n, alpha, a,b, ldb )
               
           character, intent(in) :: transr, diag, side, trans, uplo
           integer(ilp), intent(in) :: ldb, m, n
           real(sp), intent(in) :: alpha
           real(sp), intent(in) :: a(0_ilp:*)
           real(sp), intent(inout) :: b(0_ilp:ldb-1,0_ilp:*)
     end subroutine stdlib_stfsm

     pure module subroutine stdlib_dtfsm( transr, side, uplo, trans, diag, m, n, alpha, a,b, ldb )
               
           character, intent(in) :: transr, diag, side, trans, uplo
           integer(ilp), intent(in) :: ldb, m, n
           real(dp), intent(in) :: alpha
           real(dp), intent(in) :: a(0_ilp:*)
           real(dp), intent(inout) :: b(0_ilp:ldb-1,0_ilp:*)
     end subroutine stdlib_dtfsm


     pure module subroutine stdlib_ctfsm( transr, side, uplo, trans, diag, m, n, alpha, a,b, ldb )
               
           character, intent(in) :: transr, diag, side, trans, uplo
           integer(ilp), intent(in) :: ldb, m, n
           complex(sp), intent(in) :: alpha
           complex(sp), intent(in) :: a(0_ilp:*)
           complex(sp), intent(inout) :: b(0_ilp:ldb-1,0_ilp:*)
     end subroutine stdlib_ctfsm

     pure module subroutine stdlib_ztfsm( transr, side, uplo, trans, diag, m, n, alpha, a,b, ldb )
               
           character, intent(in) :: transr, diag, side, trans, uplo
           integer(ilp), intent(in) :: ldb, m, n
           complex(dp), intent(in) :: alpha
           complex(dp), intent(in) :: a(0_ilp:*)
           complex(dp), intent(inout) :: b(0_ilp:ldb-1,0_ilp:*)
     end subroutine stdlib_ztfsm


end interface 


interface 
     pure module subroutine stdlib_ssfrk( transr, uplo, trans, n, k, alpha, a, lda, beta,c )
           real(sp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: k, lda, n
           character, intent(in) :: trans, transr, uplo
           real(sp), intent(in) :: a(lda,*)
           real(sp), intent(inout) :: c(*)
     end subroutine stdlib_ssfrk

     pure module subroutine stdlib_dsfrk( transr, uplo, trans, n, k, alpha, a, lda, beta,c )
           real(dp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: k, lda, n
           character, intent(in) :: trans, transr, uplo
           real(dp), intent(in) :: a(lda,*)
           real(dp), intent(inout) :: c(*)
     end subroutine stdlib_dsfrk


end interface 


interface 
     real(sp) module function stdlib_slange( norm, m, n, a, lda, work )
           character, intent(in) :: norm
           integer(ilp), intent(in) :: lda, m, n
           real(sp), intent(in) :: a(lda,*)
           real(sp), intent(out) :: work(*)
     end function stdlib_slange

     real(dp) module function stdlib_dlange( norm, m, n, a, lda, work )
           character, intent(in) :: norm
           integer(ilp), intent(in) :: lda, m, n
           real(dp), intent(in) :: a(lda,*)
           real(dp), intent(out) :: work(*)
     end function stdlib_dlange


     real(sp) module function stdlib_clange( norm, m, n, a, lda, work )
           character, intent(in) :: norm
           integer(ilp), intent(in) :: lda, m, n
           real(sp), intent(out) :: work(*)
           complex(sp), intent(in) :: a(lda,*)
     end function stdlib_clange

     real(dp) module function stdlib_zlange( norm, m, n, a, lda, work )
           character, intent(in) :: norm
           integer(ilp), intent(in) :: lda, m, n
           real(dp), intent(out) :: work(*)
           complex(dp), intent(in) :: a(lda,*)
     end function stdlib_zlange


end interface 


interface 
     real(sp) module function stdlib_slangb( norm, n, kl, ku, ab, ldab,work )
           character, intent(in) :: norm
           integer(ilp), intent(in) :: kl, ku, ldab, n
           real(sp), intent(in) :: ab(ldab,*)
           real(sp), intent(out) :: work(*)
     end function stdlib_slangb

     real(dp) module function stdlib_dlangb( norm, n, kl, ku, ab, ldab,work )
           character, intent(in) :: norm
           integer(ilp), intent(in) :: kl, ku, ldab, n
           real(dp), intent(in) :: ab(ldab,*)
           real(dp), intent(out) :: work(*)
     end function stdlib_dlangb


     real(sp) module function stdlib_clangb( norm, n, kl, ku, ab, ldab,work )
           character, intent(in) :: norm
           integer(ilp), intent(in) :: kl, ku, ldab, n
           real(sp), intent(out) :: work(*)
           complex(sp), intent(in) :: ab(ldab,*)
     end function stdlib_clangb

     real(dp) module function stdlib_zlangb( norm, n, kl, ku, ab, ldab,work )
           character, intent(in) :: norm
           integer(ilp), intent(in) :: kl, ku, ldab, n
           real(dp), intent(out) :: work(*)
           complex(dp), intent(in) :: ab(ldab,*)
     end function stdlib_zlangb


end interface 


interface 
     pure real(sp) module function stdlib_slangt( norm, n, dl, d, du )
           character, intent(in) :: norm
           integer(ilp), intent(in) :: n
           real(sp), intent(in) :: d(*), dl(*), du(*)
     end function stdlib_slangt

     pure real(dp) module function stdlib_dlangt( norm, n, dl, d, du )
           character, intent(in) :: norm
           integer(ilp), intent(in) :: n
           real(dp), intent(in) :: d(*), dl(*), du(*)
     end function stdlib_dlangt


     pure real(sp) module function stdlib_clangt( norm, n, dl, d, du )
           character, intent(in) :: norm
           integer(ilp), intent(in) :: n
           complex(sp), intent(in) :: d(*), dl(*), du(*)
     end function stdlib_clangt

     pure real(dp) module function stdlib_zlangt( norm, n, dl, d, du )
           character, intent(in) :: norm
           integer(ilp), intent(in) :: n
           complex(dp), intent(in) :: d(*), dl(*), du(*)
     end function stdlib_zlangt


end interface 


interface 
     real(sp) module function stdlib_slanhs( norm, n, a, lda, work )
           character, intent(in) :: norm
           integer(ilp), intent(in) :: lda, n
           real(sp), intent(in) :: a(lda,*)
           real(sp), intent(out) :: work(*)
     end function stdlib_slanhs

     real(dp) module function stdlib_dlanhs( norm, n, a, lda, work )
           character, intent(in) :: norm
           integer(ilp), intent(in) :: lda, n
           real(dp), intent(in) :: a(lda,*)
           real(dp), intent(out) :: work(*)
     end function stdlib_dlanhs


     real(sp) module function stdlib_clanhs( norm, n, a, lda, work )
           character, intent(in) :: norm
           integer(ilp), intent(in) :: lda, n
           real(sp), intent(out) :: work(*)
           complex(sp), intent(in) :: a(lda,*)
     end function stdlib_clanhs

     real(dp) module function stdlib_zlanhs( norm, n, a, lda, work )
           character, intent(in) :: norm
           integer(ilp), intent(in) :: lda, n
           real(dp), intent(out) :: work(*)
           complex(dp), intent(in) :: a(lda,*)
     end function stdlib_zlanhs


end interface 


interface 
     real(sp) module function stdlib_clanhf( norm, transr, uplo, n, a, work )
           character, intent(in) :: norm, transr, uplo
           integer(ilp), intent(in) :: n
           real(sp), intent(out) :: work(0_ilp:*)
           complex(sp), intent(in) :: a(0_ilp:*)
     end function stdlib_clanhf

     real(dp) module function stdlib_zlanhf( norm, transr, uplo, n, a, work )
           character, intent(in) :: norm, transr, uplo
           integer(ilp), intent(in) :: n
           real(dp), intent(out) :: work(0_ilp:*)
           complex(dp), intent(in) :: a(0_ilp:*)
     end function stdlib_zlanhf


end interface 


interface 
     real(sp) module function stdlib_slansf( norm, transr, uplo, n, a, work )
           character, intent(in) :: norm, transr, uplo
           integer(ilp), intent(in) :: n
           real(sp), intent(in) :: a(0_ilp:*)
           real(sp), intent(out) :: work(0_ilp:*)
     end function stdlib_slansf

     real(dp) module function stdlib_dlansf( norm, transr, uplo, n, a, work )
           character, intent(in) :: norm, transr, uplo
           integer(ilp), intent(in) :: n
           real(dp), intent(in) :: a(0_ilp:*)
           real(dp), intent(out) :: work(0_ilp:*)
     end function stdlib_dlansf


end interface 


interface 
     real(sp) module function stdlib_clanhp( norm, uplo, n, ap, work )
           character, intent(in) :: norm, uplo
           integer(ilp), intent(in) :: n
           real(sp), intent(out) :: work(*)
           complex(sp), intent(in) :: ap(*)
     end function stdlib_clanhp

     real(dp) module function stdlib_zlanhp( norm, uplo, n, ap, work )
           character, intent(in) :: norm, uplo
           integer(ilp), intent(in) :: n
           real(dp), intent(out) :: work(*)
           complex(dp), intent(in) :: ap(*)
     end function stdlib_zlanhp


end interface 


interface 
     real(sp) module function stdlib_slansp( norm, uplo, n, ap, work )
           character, intent(in) :: norm, uplo
           integer(ilp), intent(in) :: n
           real(sp), intent(in) :: ap(*)
           real(sp), intent(out) :: work(*)
     end function stdlib_slansp

     real(dp) module function stdlib_dlansp( norm, uplo, n, ap, work )
           character, intent(in) :: norm, uplo
           integer(ilp), intent(in) :: n
           real(dp), intent(in) :: ap(*)
           real(dp), intent(out) :: work(*)
     end function stdlib_dlansp


     real(sp) module function stdlib_clansp( norm, uplo, n, ap, work )
           character, intent(in) :: norm, uplo
           integer(ilp), intent(in) :: n
           real(sp), intent(out) :: work(*)
           complex(sp), intent(in) :: ap(*)
     end function stdlib_clansp

     real(dp) module function stdlib_zlansp( norm, uplo, n, ap, work )
           character, intent(in) :: norm, uplo
           integer(ilp), intent(in) :: n
           real(dp), intent(out) :: work(*)
           complex(dp), intent(in) :: ap(*)
     end function stdlib_zlansp


end interface 


interface 
     real(sp) module function stdlib_clanhb( norm, uplo, n, k, ab, ldab,work )
           character, intent(in) :: norm, uplo
           integer(ilp), intent(in) :: k, ldab, n
           real(sp), intent(out) :: work(*)
           complex(sp), intent(in) :: ab(ldab,*)
     end function stdlib_clanhb

     real(dp) module function stdlib_zlanhb( norm, uplo, n, k, ab, ldab,work )
           character, intent(in) :: norm, uplo
           integer(ilp), intent(in) :: k, ldab, n
           real(dp), intent(out) :: work(*)
           complex(dp), intent(in) :: ab(ldab,*)
     end function stdlib_zlanhb


end interface 


interface 
     real(sp) module function stdlib_slansb( norm, uplo, n, k, ab, ldab,work )
           character, intent(in) :: norm, uplo
           integer(ilp), intent(in) :: k, ldab, n
           real(sp), intent(in) :: ab(ldab,*)
           real(sp), intent(out) :: work(*)
     end function stdlib_slansb

     real(dp) module function stdlib_dlansb( norm, uplo, n, k, ab, ldab,work )
           character, intent(in) :: norm, uplo
           integer(ilp), intent(in) :: k, ldab, n
           real(dp), intent(in) :: ab(ldab,*)
           real(dp), intent(out) :: work(*)
     end function stdlib_dlansb


     real(sp) module function stdlib_clansb( norm, uplo, n, k, ab, ldab,work )
           character, intent(in) :: norm, uplo
           integer(ilp), intent(in) :: k, ldab, n
           real(sp), intent(out) :: work(*)
           complex(sp), intent(in) :: ab(ldab,*)
     end function stdlib_clansb

     real(dp) module function stdlib_zlansb( norm, uplo, n, k, ab, ldab,work )
           character, intent(in) :: norm, uplo
           integer(ilp), intent(in) :: k, ldab, n
           real(dp), intent(out) :: work(*)
           complex(dp), intent(in) :: ab(ldab,*)
     end function stdlib_zlansb


end interface 


interface 
     pure real(sp) module function stdlib_clanht( norm, n, d, e )
           character, intent(in) :: norm
           integer(ilp), intent(in) :: n
           real(sp), intent(in) :: d(*)
           complex(sp), intent(in) :: e(*)
     end function stdlib_clanht

     pure real(dp) module function stdlib_zlanht( norm, n, d, e )
           character, intent(in) :: norm
           integer(ilp), intent(in) :: n
           real(dp), intent(in) :: d(*)
           complex(dp), intent(in) :: e(*)
     end function stdlib_zlanht


end interface 


interface 
     pure real(sp) module function stdlib_slanst( norm, n, d, e )
           character, intent(in) :: norm
           integer(ilp), intent(in) :: n
           real(sp), intent(in) :: d(*), e(*)
     end function stdlib_slanst

     pure real(dp) module function stdlib_dlanst( norm, n, d, e )
           character, intent(in) :: norm
           integer(ilp), intent(in) :: n
           real(dp), intent(in) :: d(*), e(*)
     end function stdlib_dlanst


end interface 


interface 
     real(sp) module function stdlib_slantr( norm, uplo, diag, m, n, a, lda,work )
           character, intent(in) :: diag, norm, uplo
           integer(ilp), intent(in) :: lda, m, n
           real(sp), intent(in) :: a(lda,*)
           real(sp), intent(out) :: work(*)
     end function stdlib_slantr

     real(dp) module function stdlib_dlantr( norm, uplo, diag, m, n, a, lda,work )
           character, intent(in) :: diag, norm, uplo
           integer(ilp), intent(in) :: lda, m, n
           real(dp), intent(in) :: a(lda,*)
           real(dp), intent(out) :: work(*)
     end function stdlib_dlantr


     real(sp) module function stdlib_clantr( norm, uplo, diag, m, n, a, lda,work )
           character, intent(in) :: diag, norm, uplo
           integer(ilp), intent(in) :: lda, m, n
           real(sp), intent(out) :: work(*)
           complex(sp), intent(in) :: a(lda,*)
     end function stdlib_clantr

     real(dp) module function stdlib_zlantr( norm, uplo, diag, m, n, a, lda,work )
           character, intent(in) :: diag, norm, uplo
           integer(ilp), intent(in) :: lda, m, n
           real(dp), intent(out) :: work(*)
           complex(dp), intent(in) :: a(lda,*)
     end function stdlib_zlantr


end interface 


interface 
     real(sp) module function stdlib_slantp( norm, uplo, diag, n, ap, work )
           character, intent(in) :: diag, norm, uplo
           integer(ilp), intent(in) :: n
           real(sp), intent(in) :: ap(*)
           real(sp), intent(out) :: work(*)
     end function stdlib_slantp

     real(dp) module function stdlib_dlantp( norm, uplo, diag, n, ap, work )
           character, intent(in) :: diag, norm, uplo
           integer(ilp), intent(in) :: n
           real(dp), intent(in) :: ap(*)
           real(dp), intent(out) :: work(*)
     end function stdlib_dlantp


     real(sp) module function stdlib_clantp( norm, uplo, diag, n, ap, work )
           character, intent(in) :: diag, norm, uplo
           integer(ilp), intent(in) :: n
           real(sp), intent(out) :: work(*)
           complex(sp), intent(in) :: ap(*)
     end function stdlib_clantp

     real(dp) module function stdlib_zlantp( norm, uplo, diag, n, ap, work )
           character, intent(in) :: diag, norm, uplo
           integer(ilp), intent(in) :: n
           real(dp), intent(out) :: work(*)
           complex(dp), intent(in) :: ap(*)
     end function stdlib_zlantp


end interface 


interface 
     real(sp) module function stdlib_slantb( norm, uplo, diag, n, k, ab,ldab, work )
           character, intent(in) :: diag, norm, uplo
           integer(ilp), intent(in) :: k, ldab, n
           real(sp), intent(in) :: ab(ldab,*)
           real(sp), intent(out) :: work(*)
     end function stdlib_slantb

     real(dp) module function stdlib_dlantb( norm, uplo, diag, n, k, ab,ldab, work )
           character, intent(in) :: diag, norm, uplo
           integer(ilp), intent(in) :: k, ldab, n
           real(dp), intent(in) :: ab(ldab,*)
           real(dp), intent(out) :: work(*)
     end function stdlib_dlantb


     real(sp) module function stdlib_clantb( norm, uplo, diag, n, k, ab,ldab, work )
           character, intent(in) :: diag, norm, uplo
           integer(ilp), intent(in) :: k, ldab, n
           real(sp), intent(out) :: work(*)
           complex(sp), intent(in) :: ab(ldab,*)
     end function stdlib_clantb

     real(dp) module function stdlib_zlantb( norm, uplo, diag, n, k, ab,ldab, work )
           character, intent(in) :: diag, norm, uplo
           integer(ilp), intent(in) :: k, ldab, n
           real(dp), intent(out) :: work(*)
           complex(dp), intent(in) :: ab(ldab,*)
     end function stdlib_zlantb


end interface 


interface 
     real(sp) module function stdlib_slansy( norm, uplo, n, a, lda, work )
           character, intent(in) :: norm, uplo
           integer(ilp), intent(in) :: lda, n
           real(sp), intent(in) :: a(lda,*)
           real(sp), intent(out) :: work(*)
     end function stdlib_slansy

     real(dp) module function stdlib_dlansy( norm, uplo, n, a, lda, work )
           character, intent(in) :: norm, uplo
           integer(ilp), intent(in) :: lda, n
           real(dp), intent(in) :: a(lda,*)
           real(dp), intent(out) :: work(*)
     end function stdlib_dlansy


     real(sp) module function stdlib_clansy( norm, uplo, n, a, lda, work )
           character, intent(in) :: norm, uplo
           integer(ilp), intent(in) :: lda, n
           real(sp), intent(out) :: work(*)
           complex(sp), intent(in) :: a(lda,*)
     end function stdlib_clansy

     real(dp) module function stdlib_zlansy( norm, uplo, n, a, lda, work )
           character, intent(in) :: norm, uplo
           integer(ilp), intent(in) :: lda, n
           real(dp), intent(out) :: work(*)
           complex(dp), intent(in) :: a(lda,*)
     end function stdlib_zlansy


end interface 


interface 
     real(sp) module function stdlib_clanhe( norm, uplo, n, a, lda, work )
           character, intent(in) :: norm, uplo
           integer(ilp), intent(in) :: lda, n
           real(sp), intent(out) :: work(*)
           complex(sp), intent(in) :: a(lda,*)
     end function stdlib_clanhe

     real(dp) module function stdlib_zlanhe( norm, uplo, n, a, lda, work )
           character, intent(in) :: norm, uplo
           integer(ilp), intent(in) :: lda, n
           real(dp), intent(out) :: work(*)
           complex(dp), intent(in) :: a(lda,*)
     end function stdlib_zlanhe


end interface 


interface 
     pure module subroutine stdlib_slartg( f, g, c, s, r )
        real(sp), intent(out) :: c, r, s
        real(sp), intent(in) :: f, g
     end subroutine stdlib_slartg

     pure module subroutine stdlib_dlartg( f, g, c, s, r )
        real(dp), intent(out) :: c, r, s
        real(dp), intent(in) :: f, g
     end subroutine stdlib_dlartg


     pure module subroutine stdlib_clartg( f, g, c, s, r )
        real(sp), intent(out) :: c
        complex(sp), intent(in) :: f, g
        complex(sp), intent(out) :: r, s
     end subroutine stdlib_clartg

     pure module subroutine stdlib_zlartg( f, g, c, s, r )
        real(dp), intent(out) :: c
        complex(dp), intent(in) :: f, g
        complex(dp), intent(out) :: r, s
     end subroutine stdlib_zlartg


end interface 


interface 
     pure module subroutine stdlib_slartgp( f, g, cs, sn, r )
           real(sp), intent(out) :: cs, r, sn
           real(sp), intent(in) :: f, g
     end subroutine stdlib_slartgp

     pure module subroutine stdlib_dlartgp( f, g, cs, sn, r )
           real(dp), intent(out) :: cs, r, sn
           real(dp), intent(in) :: f, g
     end subroutine stdlib_dlartgp


end interface 


interface 
     pure module subroutine stdlib_slasr( side, pivot, direct, m, n, c, s, a, lda )
           character, intent(in) :: direct, pivot, side
           integer(ilp), intent(in) :: lda, m, n
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(in) :: c(*), s(*)
     end subroutine stdlib_slasr

     pure module subroutine stdlib_dlasr( side, pivot, direct, m, n, c, s, a, lda )
           character, intent(in) :: direct, pivot, side
           integer(ilp), intent(in) :: lda, m, n
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(in) :: c(*), s(*)
     end subroutine stdlib_dlasr


     pure module subroutine stdlib_clasr( side, pivot, direct, m, n, c, s, a, lda )
           character, intent(in) :: direct, pivot, side
           integer(ilp), intent(in) :: lda, m, n
           real(sp), intent(in) :: c(*), s(*)
           complex(sp), intent(inout) :: a(lda,*)
     end subroutine stdlib_clasr

     pure module subroutine stdlib_zlasr( side, pivot, direct, m, n, c, s, a, lda )
           character, intent(in) :: direct, pivot, side
           integer(ilp), intent(in) :: lda, m, n
           real(dp), intent(in) :: c(*), s(*)
           complex(dp), intent(inout) :: a(lda,*)
     end subroutine stdlib_zlasr


end interface 


interface 
     pure module subroutine stdlib_slargv( n, x, incx, y, incy, c, incc )
           integer(ilp), intent(in) :: incc, incx, incy, n
           real(sp), intent(out) :: c(*)
           real(sp), intent(inout) :: x(*), y(*)
     end subroutine stdlib_slargv

     pure module subroutine stdlib_dlargv( n, x, incx, y, incy, c, incc )
           integer(ilp), intent(in) :: incc, incx, incy, n
           real(dp), intent(out) :: c(*)
           real(dp), intent(inout) :: x(*), y(*)
     end subroutine stdlib_dlargv


     pure module subroutine stdlib_clargv( n, x, incx, y, incy, c, incc )
           integer(ilp), intent(in) :: incc, incx, incy, n
           real(sp), intent(out) :: c(*)
           complex(sp), intent(inout) :: x(*), y(*)
     end subroutine stdlib_clargv

     pure module subroutine stdlib_zlargv( n, x, incx, y, incy, c, incc )
           integer(ilp), intent(in) :: incc, incx, incy, n
           real(dp), intent(out) :: c(*)
           complex(dp), intent(inout) :: x(*), y(*)
     end subroutine stdlib_zlargv


end interface 


interface 
     pure module subroutine stdlib_slartv( n, x, incx, y, incy, c, s, incc )
           integer(ilp), intent(in) :: incc, incx, incy, n
           real(sp), intent(in) :: c(*), s(*)
           real(sp), intent(inout) :: x(*), y(*)
     end subroutine stdlib_slartv

     pure module subroutine stdlib_dlartv( n, x, incx, y, incy, c, s, incc )
           integer(ilp), intent(in) :: incc, incx, incy, n
           real(dp), intent(in) :: c(*), s(*)
           real(dp), intent(inout) :: x(*), y(*)
     end subroutine stdlib_dlartv


     pure module subroutine stdlib_clartv( n, x, incx, y, incy, c, s, incc )
           integer(ilp), intent(in) :: incc, incx, incy, n
           real(sp), intent(in) :: c(*)
           complex(sp), intent(in) :: s(*)
           complex(sp), intent(inout) :: x(*), y(*)
     end subroutine stdlib_clartv

     pure module subroutine stdlib_zlartv( n, x, incx, y, incy, c, s, incc )
           integer(ilp), intent(in) :: incc, incx, incy, n
           real(dp), intent(in) :: c(*)
           complex(dp), intent(in) :: s(*)
           complex(dp), intent(inout) :: x(*), y(*)
     end subroutine stdlib_zlartv


end interface 


interface 
     pure module subroutine stdlib_slar2v( n, x, y, z, incx, c, s, incc )
           integer(ilp), intent(in) :: incc, incx, n
           real(sp), intent(in) :: c(*), s(*)
           real(sp), intent(inout) :: x(*), y(*), z(*)
     end subroutine stdlib_slar2v

     pure module subroutine stdlib_dlar2v( n, x, y, z, incx, c, s, incc )
           integer(ilp), intent(in) :: incc, incx, n
           real(dp), intent(in) :: c(*), s(*)
           real(dp), intent(inout) :: x(*), y(*), z(*)
     end subroutine stdlib_dlar2v


     pure module subroutine stdlib_clar2v( n, x, y, z, incx, c, s, incc )
           integer(ilp), intent(in) :: incc, incx, n
           real(sp), intent(in) :: c(*)
           complex(sp), intent(in) :: s(*)
           complex(sp), intent(inout) :: x(*), y(*), z(*)
     end subroutine stdlib_clar2v

     pure module subroutine stdlib_zlar2v( n, x, y, z, incx, c, s, incc )
           integer(ilp), intent(in) :: incc, incx, n
           real(dp), intent(in) :: c(*)
           complex(dp), intent(in) :: s(*)
           complex(dp), intent(inout) :: x(*), y(*), z(*)
     end subroutine stdlib_zlar2v


end interface 


interface 
     pure module subroutine stdlib_clacrt( n, cx, incx, cy, incy, c, s )
           integer(ilp), intent(in) :: incx, incy, n
           complex(sp), intent(in) :: c, s
           complex(sp), intent(inout) :: cx(*), cy(*)
     end subroutine stdlib_clacrt

     pure module subroutine stdlib_zlacrt( n, cx, incx, cy, incy, c, s )
           integer(ilp), intent(in) :: incx, incy, n
           complex(dp), intent(in) :: c, s
           complex(dp), intent(inout) :: cx(*), cy(*)
     end subroutine stdlib_zlacrt


end interface 


interface 
     pure module subroutine stdlib_slarf( side, m, n, v, incv, tau, c, ldc, work )
           character, intent(in) :: side
           integer(ilp), intent(in) :: incv, ldc, m, n
           real(sp), intent(in) :: tau
           real(sp), intent(inout) :: c(ldc,*)
           real(sp), intent(in) :: v(*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_slarf

     pure module subroutine stdlib_dlarf( side, m, n, v, incv, tau, c, ldc, work )
           character, intent(in) :: side
           integer(ilp), intent(in) :: incv, ldc, m, n
           real(dp), intent(in) :: tau
           real(dp), intent(inout) :: c(ldc,*)
           real(dp), intent(in) :: v(*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_dlarf


     pure module subroutine stdlib_clarf( side, m, n, v, incv, tau, c, ldc, work )
           character, intent(in) :: side
           integer(ilp), intent(in) :: incv, ldc, m, n
           complex(sp), intent(in) :: tau
           complex(sp), intent(inout) :: c(ldc,*)
           complex(sp), intent(in) :: v(*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_clarf

     pure module subroutine stdlib_zlarf( side, m, n, v, incv, tau, c, ldc, work )
           character, intent(in) :: side
           integer(ilp), intent(in) :: incv, ldc, m, n
           complex(dp), intent(in) :: tau
           complex(dp), intent(inout) :: c(ldc,*)
           complex(dp), intent(in) :: v(*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_zlarf


end interface 


interface 
     pure module subroutine stdlib_slarfx( side, m, n, v, tau, c, ldc, work )
           character, intent(in) :: side
           integer(ilp), intent(in) :: ldc, m, n
           real(sp), intent(in) :: tau
           real(sp), intent(inout) :: c(ldc,*)
           real(sp), intent(in) :: v(*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_slarfx

     pure module subroutine stdlib_dlarfx( side, m, n, v, tau, c, ldc, work )
           character, intent(in) :: side
           integer(ilp), intent(in) :: ldc, m, n
           real(dp), intent(in) :: tau
           real(dp), intent(inout) :: c(ldc,*)
           real(dp), intent(in) :: v(*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_dlarfx


     pure module subroutine stdlib_clarfx( side, m, n, v, tau, c, ldc, work )
           character, intent(in) :: side
           integer(ilp), intent(in) :: ldc, m, n
           complex(sp), intent(in) :: tau
           complex(sp), intent(inout) :: c(ldc,*)
           complex(sp), intent(in) :: v(*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_clarfx

     pure module subroutine stdlib_zlarfx( side, m, n, v, tau, c, ldc, work )
           character, intent(in) :: side
           integer(ilp), intent(in) :: ldc, m, n
           complex(dp), intent(in) :: tau
           complex(dp), intent(inout) :: c(ldc,*)
           complex(dp), intent(in) :: v(*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_zlarfx


end interface 


interface 
     pure module subroutine stdlib_slarfy( uplo, n, v, incv, tau, c, ldc, work )
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: incv, ldc, n
           real(sp), intent(in) :: tau
           real(sp), intent(inout) :: c(ldc,*)
           real(sp), intent(in) :: v(*)
           real(sp), intent(out) :: work(*)
     end subroutine stdlib_slarfy

     pure module subroutine stdlib_dlarfy( uplo, n, v, incv, tau, c, ldc, work )
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: incv, ldc, n
           real(dp), intent(in) :: tau
           real(dp), intent(inout) :: c(ldc,*)
           real(dp), intent(in) :: v(*)
           real(dp), intent(out) :: work(*)
     end subroutine stdlib_dlarfy


     pure module subroutine stdlib_clarfy( uplo, n, v, incv, tau, c, ldc, work )
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: incv, ldc, n
           complex(sp), intent(in) :: tau
           complex(sp), intent(inout) :: c(ldc,*)
           complex(sp), intent(in) :: v(*)
           complex(sp), intent(out) :: work(*)
     end subroutine stdlib_clarfy

     pure module subroutine stdlib_zlarfy( uplo, n, v, incv, tau, c, ldc, work )
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: incv, ldc, n
           complex(dp), intent(in) :: tau
           complex(dp), intent(inout) :: c(ldc,*)
           complex(dp), intent(in) :: v(*)
           complex(dp), intent(out) :: work(*)
     end subroutine stdlib_zlarfy


end interface 


interface 
     pure module subroutine stdlib_slarfb( side, trans, direct, storev, m, n, k, v, ldv,t, ldt, c, ldc, &
               work, ldwork )
           character, intent(in) :: direct, side, storev, trans
           integer(ilp), intent(in) :: k, ldc, ldt, ldv, ldwork, m, n
           real(sp), intent(inout) :: c(ldc,*)
           real(sp), intent(in) :: t(ldt,*), v(ldv,*)
           real(sp), intent(out) :: work(ldwork,*)
     end subroutine stdlib_slarfb

     pure module subroutine stdlib_dlarfb( side, trans, direct, storev, m, n, k, v, ldv,t, ldt, c, ldc, &
               work, ldwork )
           character, intent(in) :: direct, side, storev, trans
           integer(ilp), intent(in) :: k, ldc, ldt, ldv, ldwork, m, n
           real(dp), intent(inout) :: c(ldc,*)
           real(dp), intent(in) :: t(ldt,*), v(ldv,*)
           real(dp), intent(out) :: work(ldwork,*)
     end subroutine stdlib_dlarfb


     pure module subroutine stdlib_clarfb( side, trans, direct, storev, m, n, k, v, ldv,t, ldt, c, ldc, &
     work, ldwork )
               
           character, intent(in) :: direct, side, storev, trans
           integer(ilp), intent(in) :: k, ldc, ldt, ldv, ldwork, m, n
           complex(sp), intent(inout) :: c(ldc,*)
           complex(sp), intent(in) :: t(ldt,*), v(ldv,*)
           complex(sp), intent(out) :: work(ldwork,*)
     end subroutine stdlib_clarfb

     pure module subroutine stdlib_zlarfb( side, trans, direct, storev, m, n, k, v, ldv,t, ldt, c, ldc, &
               work, ldwork )
           character, intent(in) :: direct, side, storev, trans
           integer(ilp), intent(in) :: k, ldc, ldt, ldv, ldwork, m, n
           complex(dp), intent(inout) :: c(ldc,*)
           complex(dp), intent(in) :: t(ldt,*), v(ldv,*)
           complex(dp), intent(out) :: work(ldwork,*)
     end subroutine stdlib_zlarfb


end interface 


interface 
     pure module subroutine stdlib_slarfg( n, alpha, x, incx, tau )
           integer(ilp), intent(in) :: incx, n
           real(sp), intent(inout) :: alpha
           real(sp), intent(out) :: tau
           real(sp), intent(inout) :: x(*)
     end subroutine stdlib_slarfg

     pure module subroutine stdlib_dlarfg( n, alpha, x, incx, tau )
           integer(ilp), intent(in) :: incx, n
           real(dp), intent(inout) :: alpha
           real(dp), intent(out) :: tau
           real(dp), intent(inout) :: x(*)
     end subroutine stdlib_dlarfg


     pure module subroutine stdlib_clarfg( n, alpha, x, incx, tau )
           integer(ilp), intent(in) :: incx, n
           complex(sp), intent(inout) :: alpha
           complex(sp), intent(out) :: tau
           complex(sp), intent(inout) :: x(*)
     end subroutine stdlib_clarfg

     pure module subroutine stdlib_zlarfg( n, alpha, x, incx, tau )
           integer(ilp), intent(in) :: incx, n
           complex(dp), intent(inout) :: alpha
           complex(dp), intent(out) :: tau
           complex(dp), intent(inout) :: x(*)
     end subroutine stdlib_zlarfg


end interface 


interface 
     module subroutine stdlib_slarfgp( n, alpha, x, incx, tau )
           integer(ilp), intent(in) :: incx, n
           real(sp), intent(inout) :: alpha
           real(sp), intent(out) :: tau
           real(sp), intent(inout) :: x(*)
     end subroutine stdlib_slarfgp

     module subroutine stdlib_dlarfgp( n, alpha, x, incx, tau )
           integer(ilp), intent(in) :: incx, n
           real(dp), intent(inout) :: alpha
           real(dp), intent(out) :: tau
           real(dp), intent(inout) :: x(*)
     end subroutine stdlib_dlarfgp


     module subroutine stdlib_clarfgp( n, alpha, x, incx, tau )
           integer(ilp), intent(in) :: incx, n
           complex(sp), intent(inout) :: alpha
           complex(sp), intent(out) :: tau
           complex(sp), intent(inout) :: x(*)
     end subroutine stdlib_clarfgp

     module subroutine stdlib_zlarfgp( n, alpha, x, incx, tau )
           integer(ilp), intent(in) :: incx, n
           complex(dp), intent(inout) :: alpha
           complex(dp), intent(out) :: tau
           complex(dp), intent(inout) :: x(*)
     end subroutine stdlib_zlarfgp


end interface 


interface 
     pure module subroutine stdlib_slarft( direct, storev, n, k, v, ldv, tau, t, ldt )
           character, intent(in) :: direct, storev
           integer(ilp), intent(in) :: k, ldt, ldv, n
           real(sp), intent(out) :: t(ldt,*)
           real(sp), intent(in) :: tau(*), v(ldv,*)
     end subroutine stdlib_slarft

     pure module subroutine stdlib_dlarft( direct, storev, n, k, v, ldv, tau, t, ldt )
           character, intent(in) :: direct, storev
           integer(ilp), intent(in) :: k, ldt, ldv, n
           real(dp), intent(out) :: t(ldt,*)
           real(dp), intent(in) :: tau(*), v(ldv,*)
     end subroutine stdlib_dlarft


     pure module subroutine stdlib_clarft( direct, storev, n, k, v, ldv, tau, t, ldt )
           character, intent(in) :: direct, storev
           integer(ilp), intent(in) :: k, ldt, ldv, n
           complex(sp), intent(out) :: t(ldt,*)
           complex(sp), intent(in) :: tau(*), v(ldv,*)
     end subroutine stdlib_clarft

     pure module subroutine stdlib_zlarft( direct, storev, n, k, v, ldv, tau, t, ldt )
           character, intent(in) :: direct, storev
           integer(ilp), intent(in) :: k, ldt, ldv, n
           complex(dp), intent(out) :: t(ldt,*)
           complex(dp), intent(in) :: tau(*), v(ldv,*)
     end subroutine stdlib_zlarft


end interface 

end module stdlib_lapack_base
