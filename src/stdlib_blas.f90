module stdlib_blas
  use stdlib_linalg_constants
  use stdlib_linalg_blas_aux
  implicit none

interface 
     pure real(sp) module function stdlib_sasum(n,sx,incx)
           integer(ilp), intent(in) :: incx, n
           real(sp), intent(in) :: sx(*)
     end function stdlib_sasum

     pure real(dp) module function stdlib_dasum(n,dx,incx)
           integer(ilp), intent(in) :: incx, n
           real(dp), intent(in) :: dx(*)
     end function stdlib_dasum


end interface 


interface 
     pure real(sp) module function stdlib_scasum(n,cx,incx)
           integer(ilp), intent(in) :: incx, n
           complex(sp), intent(in) :: cx(*)
     end function stdlib_scasum

end interface 


interface 
     pure real(dp) module function stdlib_dzasum(n,zx,incx)
           integer(ilp), intent(in) :: incx, n
           complex(dp), intent(in) :: zx(*)
     end function stdlib_dzasum


end interface 


interface 
     pure module subroutine stdlib_saxpy(n,sa,sx,incx,sy,incy)
           real(sp), intent(in) :: sa
           integer(ilp), intent(in) :: incx, incy, n
           real(sp), intent(in) :: sx(*)
           real(sp), intent(inout) :: sy(*)
     end subroutine stdlib_saxpy

     pure module subroutine stdlib_daxpy(n,da,dx,incx,dy,incy)
           real(dp), intent(in) :: da
           integer(ilp), intent(in) :: incx, incy, n
           real(dp), intent(in) :: dx(*)
           real(dp), intent(inout) :: dy(*)
     end subroutine stdlib_daxpy


     pure module subroutine stdlib_caxpy(n,ca,cx,incx,cy,incy)
           complex(sp), intent(in) :: ca
           integer(ilp), intent(in) :: incx, incy, n
           complex(sp), intent(in) :: cx(*)
           complex(sp), intent(inout) :: cy(*)
     end subroutine stdlib_caxpy

     pure module subroutine stdlib_zaxpy(n,za,zx,incx,zy,incy)
           complex(dp), intent(in) :: za
           integer(ilp), intent(in) :: incx, incy, n
           complex(dp), intent(in) :: zx(*)
           complex(dp), intent(inout) :: zy(*)
     end subroutine stdlib_zaxpy


end interface 


interface 
     pure module subroutine stdlib_scopy(n,sx,incx,sy,incy)
           integer(ilp), intent(in) :: incx, incy, n
           real(sp), intent(in) :: sx(*)
           real(sp), intent(out) :: sy(*)
     end subroutine stdlib_scopy

     pure module subroutine stdlib_dcopy(n,dx,incx,dy,incy)
           integer(ilp), intent(in) :: incx, incy, n
           real(dp), intent(in) :: dx(*)
           real(dp), intent(out) :: dy(*)
     end subroutine stdlib_dcopy


     pure module subroutine stdlib_ccopy(n,cx,incx,cy,incy)
           integer(ilp), intent(in) :: incx, incy, n
           complex(sp), intent(in) :: cx(*)
           complex(sp), intent(out) :: cy(*)
     end subroutine stdlib_ccopy

     pure module subroutine stdlib_zcopy(n,zx,incx,zy,incy)
           integer(ilp), intent(in) :: incx, incy, n
           complex(dp), intent(in) :: zx(*)
           complex(dp), intent(out) :: zy(*)
     end subroutine stdlib_zcopy


end interface 


interface 
     pure real(sp) module function stdlib_sdot(n,sx,incx,sy,incy)
           integer(ilp), intent(in) :: incx, incy, n
           real(sp), intent(in) :: sx(*), sy(*)
     end function stdlib_sdot

     pure real(dp) module function stdlib_ddot(n,dx,incx,dy,incy)
           integer(ilp), intent(in) :: incx, incy, n
           real(dp), intent(in) :: dx(*), dy(*)
     end function stdlib_ddot


end interface 


interface 
     pure real(dp) module function stdlib_dsdot(n,sx,incx,sy,incy)
           integer(ilp), intent(in) :: incx, incy, n
           real(sp), intent(in) :: sx(*), sy(*)
     end function stdlib_dsdot


end interface 


interface 
     pure complex(sp) module function stdlib_cdotc(n,cx,incx,cy,incy)
           integer(ilp), intent(in) :: incx, incy, n
           complex(sp), intent(in) :: cx(*), cy(*)
     end function stdlib_cdotc

     pure complex(dp) module function stdlib_zdotc(n,zx,incx,zy,incy)
           integer(ilp), intent(in) :: incx, incy, n
           complex(dp), intent(in) :: zx(*), zy(*)
     end function stdlib_zdotc


end interface 


interface 
     pure complex(sp) module function stdlib_cdotu(n,cx,incx,cy,incy)
           integer(ilp), intent(in) :: incx, incy, n
           complex(sp), intent(in) :: cx(*), cy(*)
     end function stdlib_cdotu

     pure complex(dp) module function stdlib_zdotu(n,zx,incx,zy,incy)
           integer(ilp), intent(in) :: incx, incy, n
           complex(dp), intent(in) :: zx(*), zy(*)
     end function stdlib_zdotu


end interface 


interface 
     pure real(sp) module function stdlib_snrm2( n, x, incx )
        integer(ilp), intent(in) :: incx, n
        real(sp), intent(in) :: x(*)
     end function stdlib_snrm2

     pure real(dp) module function stdlib_dnrm2( n, x, incx )
        integer(ilp), intent(in) :: incx, n
        real(dp), intent(in) :: x(*)
     end function stdlib_dnrm2


end interface 


interface 
     pure real(sp) module function stdlib_scnrm2( n, x, incx )
        integer(ilp), intent(in) :: incx, n
        complex(sp), intent(in) :: x(*)
     end function stdlib_scnrm2

end interface 


interface 
     pure real(dp) module function stdlib_dznrm2( n, x, incx )
        integer(ilp), intent(in) :: incx, n
        complex(dp), intent(in) :: x(*)
     end function stdlib_dznrm2


end interface 


interface 
     pure module subroutine stdlib_srot(n,sx,incx,sy,incy,c,s)
           real(sp), intent(in) :: c, s
           integer(ilp), intent(in) :: incx, incy, n
           real(sp), intent(inout) :: sx(*), sy(*)
     end subroutine stdlib_srot

     pure module subroutine stdlib_drot(n,dx,incx,dy,incy,c,s)
           real(dp), intent(in) :: c, s
           integer(ilp), intent(in) :: incx, incy, n
           real(dp), intent(inout) :: dx(*), dy(*)
     end subroutine stdlib_drot


end interface 


interface 
     pure module subroutine stdlib_zdrot( n, zx, incx, zy, incy, c, s )
           integer(ilp), intent(in) :: incx, incy, n
           real(dp), intent(in) :: c, s
           complex(dp), intent(inout) :: zx(*), zy(*)
     end subroutine stdlib_zdrot


end interface 


interface 
     pure module subroutine stdlib_srotg( a, b, c, s )
        real(sp), intent(inout) :: a, b
        real(sp), intent(out) :: c, s
     end subroutine stdlib_srotg

     pure module subroutine stdlib_drotg( a, b, c, s )
        real(dp), intent(inout) :: a, b
        real(dp), intent(out) :: c, s
     end subroutine stdlib_drotg


     pure module subroutine stdlib_crotg( a, b, c, s )
        real(sp), intent(out) :: c
        complex(sp), intent(inout) :: a
        complex(sp), intent(in) :: b
        complex(sp), intent(out) :: s
     end subroutine stdlib_crotg

     pure module subroutine stdlib_zrotg( a, b, c, s )
        real(dp), intent(out) :: c
        complex(dp), intent(inout) :: a
        complex(dp), intent(in) :: b
        complex(dp), intent(out) :: s
     end subroutine stdlib_zrotg


end interface 


interface 
     pure module subroutine stdlib_srotm(n,sx,incx,sy,incy,sparam)
           integer(ilp), intent(in) :: incx, incy, n
           real(sp), intent(in) :: sparam(5)
           real(sp), intent(inout) :: sx(*), sy(*)
     end subroutine stdlib_srotm

     pure module subroutine stdlib_drotm(n,dx,incx,dy,incy,dparam)    
           integer(ilp), intent(in) :: incx, incy, n
           real(dp), intent(in) :: dparam(5)
           real(dp), intent(inout) :: dx(*), dy(*)
     end subroutine stdlib_drotm


end interface 


interface 
     pure module subroutine stdlib_srotmg(sd1,sd2,sx1,sy1,sparam)
           real(sp), intent(inout) :: sd1, sd2, sx1
           real(sp), intent(in) :: sy1
           real(sp), intent(out) :: sparam(5)
     end subroutine stdlib_srotmg

     pure module subroutine stdlib_drotmg(dd1,dd2,dx1,dy1,dparam)
           real(dp), intent(inout) :: dd1, dd2, dx1
           real(dp), intent(in) :: dy1
           real(dp), intent(out) :: dparam(5)
     end subroutine stdlib_drotmg


end interface 


interface 
     pure module subroutine stdlib_csrot( n, cx, incx, cy, incy, c, s )
           integer(ilp), intent(in) :: incx, incy, n
           real(sp), intent(in) :: c, s
           complex(sp), intent(inout) :: cx(*), cy(*)
     end subroutine stdlib_csrot

end interface 


interface 
     pure module subroutine stdlib_sscal(n,sa,sx,incx)
           real(sp), intent(in) :: sa
           integer(ilp), intent(in) :: incx, n
           real(sp), intent(inout) :: sx(*)
     end subroutine stdlib_sscal

     pure module subroutine stdlib_dscal(n,da,dx,incx)
           real(dp), intent(in) :: da
           integer(ilp), intent(in) :: incx, n
           real(dp), intent(inout) :: dx(*)
     end subroutine stdlib_dscal


     pure module subroutine stdlib_cscal(n,ca,cx,incx)
           complex(sp), intent(in) :: ca
           integer(ilp), intent(in) :: incx, n
           complex(sp), intent(inout) :: cx(*)
     end subroutine stdlib_cscal

     pure module subroutine stdlib_zscal(n,za,zx,incx)
           complex(dp), intent(in) :: za
           integer(ilp), intent(in) :: incx, n
           complex(dp), intent(inout) :: zx(*)
     end subroutine stdlib_zscal


end interface 


interface 
     pure module subroutine stdlib_csscal(n,sa,cx,incx)
           real(sp), intent(in) :: sa
           integer(ilp), intent(in) :: incx, n
           complex(sp), intent(inout) :: cx(*)
     end subroutine stdlib_csscal

end interface 


interface 
     pure module subroutine stdlib_zdscal(n,da,zx,incx)
           real(dp), intent(in) :: da
           integer(ilp), intent(in) :: incx, n
           complex(dp), intent(inout) :: zx(*)
     end subroutine stdlib_zdscal


end interface 


interface 
     pure module subroutine stdlib_sswap(n,sx,incx,sy,incy)
           integer(ilp), intent(in) :: incx, incy, n
           real(sp), intent(inout) :: sx(*), sy(*)
     end subroutine stdlib_sswap

     pure module subroutine stdlib_dswap(n,dx,incx,dy,incy)
           integer(ilp), intent(in) :: incx, incy, n
           real(dp), intent(inout) :: dx(*), dy(*)
     end subroutine stdlib_dswap


     pure module subroutine stdlib_cswap(n,cx,incx,cy,incy)
           integer(ilp), intent(in) :: incx, incy, n
           complex(sp), intent(inout) :: cx(*), cy(*)
     end subroutine stdlib_cswap

     pure module subroutine stdlib_zswap(n,zx,incx,zy,incy)
           integer(ilp), intent(in) :: incx, incy, n
           complex(dp), intent(inout) :: zx(*), zy(*)
     end subroutine stdlib_zswap


end interface 


interface 
     pure module subroutine stdlib_sgemv(trans,m,n,alpha,a,lda,x,incx,beta,y,incy)
           real(sp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, lda, m, n
           character, intent(in) :: trans
           real(sp), intent(in) :: a(lda,*), x(*)
           real(sp), intent(inout) :: y(*)
     end subroutine stdlib_sgemv

     pure module subroutine stdlib_dgemv(trans,m,n,alpha,a,lda,x,incx,beta,y,incy)
           real(dp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, lda, m, n
           character, intent(in) :: trans
           real(dp), intent(in) :: a(lda,*), x(*)
           real(dp), intent(inout) :: y(*)
     end subroutine stdlib_dgemv


     pure module subroutine stdlib_cgemv(trans,m,n,alpha,a,lda,x,incx,beta,y,incy)
           complex(sp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, lda, m, n
           character, intent(in) :: trans
           complex(sp), intent(in) :: a(lda,*), x(*)
           complex(sp), intent(inout) :: y(*)
     end subroutine stdlib_cgemv

     pure module subroutine stdlib_zgemv(trans,m,n,alpha,a,lda,x,incx,beta,y,incy)
           complex(dp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, lda, m, n
           character, intent(in) :: trans
           complex(dp), intent(in) :: a(lda,*), x(*)
           complex(dp), intent(inout) :: y(*)
     end subroutine stdlib_zgemv


end interface 


interface 
     pure module subroutine stdlib_sger(m,n,alpha,x,incx,y,incy,a,lda)
           real(sp), intent(in) :: alpha
           integer(ilp), intent(in) :: incx, incy, lda, m, n
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(in) :: x(*), y(*)
     end subroutine stdlib_sger

     pure module subroutine stdlib_dger(m,n,alpha,x,incx,y,incy,a,lda)
           real(dp), intent(in) :: alpha
           integer(ilp), intent(in) :: incx, incy, lda, m, n
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(in) :: x(*), y(*)
     end subroutine stdlib_dger


end interface 


interface 
     pure module subroutine stdlib_cgerc(m,n,alpha,x,incx,y,incy,a,lda)
           complex(sp), intent(in) :: alpha
           integer(ilp), intent(in) :: incx, incy, lda, m, n
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(in) :: x(*), y(*)
     end subroutine stdlib_cgerc

     pure module subroutine stdlib_zgerc(m,n,alpha,x,incx,y,incy,a,lda)
           complex(dp), intent(in) :: alpha
           integer(ilp), intent(in) :: incx, incy, lda, m, n
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(in) :: x(*), y(*)
     end subroutine stdlib_zgerc


end interface 


interface 
     pure module subroutine stdlib_cgeru(m,n,alpha,x,incx,y,incy,a,lda)
           complex(sp), intent(in) :: alpha
           integer(ilp), intent(in) :: incx, incy, lda, m, n
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(in) :: x(*), y(*)
     end subroutine stdlib_cgeru

     pure module subroutine stdlib_zgeru(m,n,alpha,x,incx,y,incy,a,lda)
           complex(dp), intent(in) :: alpha
           integer(ilp), intent(in) :: incx, incy, lda, m, n
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(in) :: x(*), y(*)
     end subroutine stdlib_zgeru


end interface 


interface 
     pure module subroutine stdlib_cher(uplo,n,alpha,x,incx,a,lda)
           real(sp), intent(in) :: alpha
           integer(ilp), intent(in) :: incx, lda, n
           character, intent(in) :: uplo
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(in) :: x(*)
     end subroutine stdlib_cher

     pure module subroutine stdlib_zher(uplo,n,alpha,x,incx,a,lda)
           real(dp), intent(in) :: alpha
           integer(ilp), intent(in) :: incx, lda, n
           character, intent(in) :: uplo
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(in) :: x(*)
     end subroutine stdlib_zher


end interface 


interface 
     pure module subroutine stdlib_cher2(uplo,n,alpha,x,incx,y,incy,a,lda)
           complex(sp), intent(in) :: alpha
           integer(ilp), intent(in) :: incx, incy, lda, n
           character, intent(in) :: uplo
           complex(sp), intent(inout) :: a(lda,*)
           complex(sp), intent(in) :: x(*), y(*)
     end subroutine stdlib_cher2

     pure module subroutine stdlib_zher2(uplo,n,alpha,x,incx,y,incy,a,lda)
           complex(dp), intent(in) :: alpha
           integer(ilp), intent(in) :: incx, incy, lda, n
           character, intent(in) :: uplo
           complex(dp), intent(inout) :: a(lda,*)
           complex(dp), intent(in) :: x(*), y(*)
     end subroutine stdlib_zher2


end interface 


interface 
     pure module subroutine stdlib_chemv(uplo,n,alpha,a,lda,x,incx,beta,y,incy)
           complex(sp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, lda, n
           character, intent(in) :: uplo
           complex(sp), intent(in) :: a(lda,*), x(*)
           complex(sp), intent(inout) :: y(*)
     end subroutine stdlib_chemv

     pure module subroutine stdlib_zhemv(uplo,n,alpha,a,lda,x,incx,beta,y,incy)
           complex(dp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, lda, n
           character, intent(in) :: uplo
           complex(dp), intent(in) :: a(lda,*), x(*)
           complex(dp), intent(inout) :: y(*)
     end subroutine stdlib_zhemv


end interface 


interface 
     pure module subroutine stdlib_sgbmv(trans,m,n,kl,ku,alpha,a,lda,x,incx,beta,y,incy)
           real(sp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, kl, ku, lda, m, n
           character, intent(in) :: trans
           real(sp), intent(in) :: a(lda,*), x(*)
           real(sp), intent(inout) :: y(*)
     end subroutine stdlib_sgbmv

     pure module subroutine stdlib_dgbmv(trans,m,n,kl,ku,alpha,a,lda,x,incx,beta,y,incy)
           real(dp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, kl, ku, lda, m, n
           character, intent(in) :: trans
           real(dp), intent(in) :: a(lda,*), x(*)
           real(dp), intent(inout) :: y(*)
     end subroutine stdlib_dgbmv


     pure module subroutine stdlib_cgbmv(trans,m,n,kl,ku,alpha,a,lda,x,incx,beta,y,incy)
           complex(sp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, kl, ku, lda, m, n
           character, intent(in) :: trans
           complex(sp), intent(in) :: a(lda,*), x(*)
           complex(sp), intent(inout) :: y(*)
     end subroutine stdlib_cgbmv

     pure module subroutine stdlib_zgbmv(trans,m,n,kl,ku,alpha,a,lda,x,incx,beta,y,incy)
           complex(dp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, kl, ku, lda, m, n
           character, intent(in) :: trans
           complex(dp), intent(in) :: a(lda,*), x(*)
           complex(dp), intent(inout) :: y(*)
     end subroutine stdlib_zgbmv


end interface 


interface 
     pure module subroutine stdlib_chbmv(uplo,n,k,alpha,a,lda,x,incx,beta,y,incy)
           complex(sp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, k, lda, n
           character, intent(in) :: uplo
           complex(sp), intent(in) :: a(lda,*), x(*)
           complex(sp), intent(inout) :: y(*)
     end subroutine stdlib_chbmv

     pure module subroutine stdlib_zhbmv(uplo,n,k,alpha,a,lda,x,incx,beta,y,incy)
           complex(dp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, k, lda, n
           character, intent(in) :: uplo
           complex(dp), intent(in) :: a(lda,*), x(*)
           complex(dp), intent(inout) :: y(*)
     end subroutine stdlib_zhbmv


end interface 


interface 
     pure module subroutine stdlib_ssymv(uplo,n,alpha,a,lda,x,incx,beta,y,incy)
           real(sp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, lda, n
           character, intent(in) :: uplo
           real(sp), intent(in) :: a(lda,*), x(*)
           real(sp), intent(inout) :: y(*)
     end subroutine stdlib_ssymv

     pure module subroutine stdlib_dsymv(uplo,n,alpha,a,lda,x,incx,beta,y,incy)
           real(dp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, lda, n
           character, intent(in) :: uplo
           real(dp), intent(in) :: a(lda,*), x(*)
           real(dp), intent(inout) :: y(*)
     end subroutine stdlib_dsymv


end interface 


interface 
     pure module subroutine stdlib_ssyr(uplo,n,alpha,x,incx,a,lda)
           real(sp), intent(in) :: alpha
           integer(ilp), intent(in) :: incx, lda, n
           character, intent(in) :: uplo
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(in) :: x(*)
     end subroutine stdlib_ssyr

     pure module subroutine stdlib_dsyr(uplo,n,alpha,x,incx,a,lda)
           real(dp), intent(in) :: alpha
           integer(ilp), intent(in) :: incx, lda, n
           character, intent(in) :: uplo
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(in) :: x(*)
     end subroutine stdlib_dsyr


end interface 


interface 
     pure module subroutine stdlib_ssyr2(uplo,n,alpha,x,incx,y,incy,a,lda)
           real(sp), intent(in) :: alpha
           integer(ilp), intent(in) :: incx, incy, lda, n
           character, intent(in) :: uplo
           real(sp), intent(inout) :: a(lda,*)
           real(sp), intent(in) :: x(*), y(*)
     end subroutine stdlib_ssyr2

     pure module subroutine stdlib_dsyr2(uplo,n,alpha,x,incx,y,incy,a,lda)
           real(dp), intent(in) :: alpha
           integer(ilp), intent(in) :: incx, incy, lda, n
           character, intent(in) :: uplo
           real(dp), intent(inout) :: a(lda,*)
           real(dp), intent(in) :: x(*), y(*)
     end subroutine stdlib_dsyr2


end interface 


interface 
     pure module subroutine stdlib_sspmv(uplo,n,alpha,ap,x,incx,beta,y,incy)
           real(sp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, n
           character, intent(in) :: uplo
           real(sp), intent(in) :: ap(*), x(*)
           real(sp), intent(inout) :: y(*)
     end subroutine stdlib_sspmv

     pure module subroutine stdlib_dspmv(uplo,n,alpha,ap,x,incx,beta,y,incy)
           real(dp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, n
           character, intent(in) :: uplo
           real(dp), intent(in) :: ap(*), x(*)
           real(dp), intent(inout) :: y(*)
     end subroutine stdlib_dspmv


end interface 


interface 
     pure module subroutine stdlib_ssbmv(uplo,n,k,alpha,a,lda,x,incx,beta,y,incy)
           real(sp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, k, lda, n
           character, intent(in) :: uplo
           real(sp), intent(in) :: a(lda,*), x(*)
           real(sp), intent(inout) :: y(*)
     end subroutine stdlib_ssbmv

     pure module subroutine stdlib_dsbmv(uplo,n,k,alpha,a,lda,x,incx,beta,y,incy)
           real(dp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, k, lda, n
           character, intent(in) :: uplo
           real(dp), intent(in) :: a(lda,*), x(*)
           real(dp), intent(inout) :: y(*)
     end subroutine stdlib_dsbmv


end interface 


interface 
     pure module subroutine stdlib_sspr(uplo,n,alpha,x,incx,ap)
           real(sp), intent(in) :: alpha
           integer(ilp), intent(in) :: incx, n
           character, intent(in) :: uplo
           real(sp), intent(inout) :: ap(*)
           real(sp), intent(in) :: x(*)
     end subroutine stdlib_sspr

     pure module subroutine stdlib_dspr(uplo,n,alpha,x,incx,ap)
           real(dp), intent(in) :: alpha
           integer(ilp), intent(in) :: incx, n
           character, intent(in) :: uplo
           real(dp), intent(inout) :: ap(*)
           real(dp), intent(in) :: x(*)
     end subroutine stdlib_dspr


end interface 


interface 
     pure module subroutine stdlib_sspr2(uplo,n,alpha,x,incx,y,incy,ap)
           real(sp), intent(in) :: alpha
           integer(ilp), intent(in) :: incx, incy, n
           character, intent(in) :: uplo
           real(sp), intent(inout) :: ap(*)
           real(sp), intent(in) :: x(*), y(*)
     end subroutine stdlib_sspr2

     pure module subroutine stdlib_dspr2(uplo,n,alpha,x,incx,y,incy,ap)
           real(dp), intent(in) :: alpha
           integer(ilp), intent(in) :: incx, incy, n
           character, intent(in) :: uplo
           real(dp), intent(inout) :: ap(*)
           real(dp), intent(in) :: x(*), y(*)
     end subroutine stdlib_dspr2


end interface 


interface 
     pure module subroutine stdlib_chpmv(uplo,n,alpha,ap,x,incx,beta,y,incy)
           complex(sp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, n
           character, intent(in) :: uplo
           complex(sp), intent(in) :: ap(*), x(*)
           complex(sp), intent(inout) :: y(*)
     end subroutine stdlib_chpmv

     pure module subroutine stdlib_zhpmv(uplo,n,alpha,ap,x,incx,beta,y,incy)
           complex(dp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, n
           character, intent(in) :: uplo
           complex(dp), intent(in) :: ap(*), x(*)
           complex(dp), intent(inout) :: y(*)
     end subroutine stdlib_zhpmv


end interface 


interface 
     pure module subroutine stdlib_chpr(uplo,n,alpha,x,incx,ap)
           real(sp), intent(in) :: alpha
           integer(ilp), intent(in) :: incx, n
           character, intent(in) :: uplo
           complex(sp), intent(inout) :: ap(*)
           complex(sp), intent(in) :: x(*)
     end subroutine stdlib_chpr

     pure module subroutine stdlib_zhpr(uplo,n,alpha,x,incx,ap)
           real(dp), intent(in) :: alpha
           integer(ilp), intent(in) :: incx, n
           character, intent(in) :: uplo
           complex(dp), intent(inout) :: ap(*)
           complex(dp), intent(in) :: x(*)
     end subroutine stdlib_zhpr


end interface 


interface 
     pure module subroutine stdlib_chpr2(uplo,n,alpha,x,incx,y,incy,ap)
           complex(sp), intent(in) :: alpha
           integer(ilp), intent(in) :: incx, incy, n
           character, intent(in) :: uplo
           complex(sp), intent(inout) :: ap(*)
           complex(sp), intent(in) :: x(*), y(*)
     end subroutine stdlib_chpr2

     pure module subroutine stdlib_zhpr2(uplo,n,alpha,x,incx,y,incy,ap)
           complex(dp), intent(in) :: alpha
           integer(ilp), intent(in) :: incx, incy, n
           character, intent(in) :: uplo
           complex(dp), intent(inout) :: ap(*)
           complex(dp), intent(in) :: x(*), y(*)
     end subroutine stdlib_zhpr2


end interface 


interface 
     pure module subroutine stdlib_strmv(uplo,trans,diag,n,a,lda,x,incx)
           integer(ilp), intent(in) :: incx, lda, n
           character, intent(in) :: diag, trans, uplo
           real(sp), intent(in) :: a(lda,*)
           real(sp), intent(inout) :: x(*)
     end subroutine stdlib_strmv

     pure module subroutine stdlib_dtrmv(uplo,trans,diag,n,a,lda,x,incx)
           integer(ilp), intent(in) :: incx, lda, n
           character, intent(in) :: diag, trans, uplo
           real(dp), intent(in) :: a(lda,*)
           real(dp), intent(inout) :: x(*)
     end subroutine stdlib_dtrmv


     pure module subroutine stdlib_ctrmv(uplo,trans,diag,n,a,lda,x,incx)
           integer(ilp), intent(in) :: incx, lda, n
           character, intent(in) :: diag, trans, uplo
           complex(sp), intent(in) :: a(lda,*)
           complex(sp), intent(inout) :: x(*)
     end subroutine stdlib_ctrmv

     pure module subroutine stdlib_ztrmv(uplo,trans,diag,n,a,lda,x,incx)
           integer(ilp), intent(in) :: incx, lda, n
           character, intent(in) :: diag, trans, uplo
           complex(dp), intent(in) :: a(lda,*)
           complex(dp), intent(inout) :: x(*)
     end subroutine stdlib_ztrmv


end interface 


interface 
     pure module subroutine stdlib_stbmv(uplo,trans,diag,n,k,a,lda,x,incx)
           integer(ilp), intent(in) :: incx, k, lda, n
           character, intent(in) :: diag, trans, uplo
           real(sp), intent(in) :: a(lda,*)
           real(sp), intent(inout) :: x(*)
     end subroutine stdlib_stbmv

     pure module subroutine stdlib_dtbmv(uplo,trans,diag,n,k,a,lda,x,incx)
           integer(ilp), intent(in) :: incx, k, lda, n
           character, intent(in) :: diag, trans, uplo
           real(dp), intent(in) :: a(lda,*)
           real(dp), intent(inout) :: x(*)
     end subroutine stdlib_dtbmv


     pure module subroutine stdlib_ctbmv(uplo,trans,diag,n,k,a,lda,x,incx)
           integer(ilp), intent(in) :: incx, k, lda, n
           character, intent(in) :: diag, trans, uplo
           complex(sp), intent(in) :: a(lda,*)
           complex(sp), intent(inout) :: x(*)
     end subroutine stdlib_ctbmv

     pure module subroutine stdlib_ztbmv(uplo,trans,diag,n,k,a,lda,x,incx)
           integer(ilp), intent(in) :: incx, k, lda, n
           character, intent(in) :: diag, trans, uplo
           complex(dp), intent(in) :: a(lda,*)
           complex(dp), intent(inout) :: x(*)
     end subroutine stdlib_ztbmv


end interface 


interface 
     pure module subroutine stdlib_stpmv(uplo,trans,diag,n,ap,x,incx)
           integer(ilp), intent(in) :: incx, n
           character, intent(in) :: diag, trans, uplo
           real(sp), intent(in) :: ap(*)
           real(sp), intent(inout) :: x(*)
     end subroutine stdlib_stpmv

     pure module subroutine stdlib_dtpmv(uplo,trans,diag,n,ap,x,incx)
           integer(ilp), intent(in) :: incx, n
           character, intent(in) :: diag, trans, uplo
           real(dp), intent(in) :: ap(*)
           real(dp), intent(inout) :: x(*)
     end subroutine stdlib_dtpmv


     pure module subroutine stdlib_ctpmv(uplo,trans,diag,n,ap,x,incx)
           integer(ilp), intent(in) :: incx, n
           character, intent(in) :: diag, trans, uplo
           complex(sp), intent(in) :: ap(*)
           complex(sp), intent(inout) :: x(*)
     end subroutine stdlib_ctpmv

     pure module subroutine stdlib_ztpmv(uplo,trans,diag,n,ap,x,incx)
           integer(ilp), intent(in) :: incx, n
           character, intent(in) :: diag, trans, uplo
           complex(dp), intent(in) :: ap(*)
           complex(dp), intent(inout) :: x(*)
     end subroutine stdlib_ztpmv


end interface 


interface 
     pure module subroutine stdlib_strsv(uplo,trans,diag,n,a,lda,x,incx)
           integer(ilp), intent(in) :: incx, lda, n
           character, intent(in) :: diag, trans, uplo
           real(sp), intent(in) :: a(lda,*)
           real(sp), intent(inout) :: x(*)
     end subroutine stdlib_strsv

     pure module subroutine stdlib_dtrsv(uplo,trans,diag,n,a,lda,x,incx)
           integer(ilp), intent(in) :: incx, lda, n
           character, intent(in) :: diag, trans, uplo
           real(dp), intent(in) :: a(lda,*)
           real(dp), intent(inout) :: x(*)
     end subroutine stdlib_dtrsv


     pure module subroutine stdlib_ctrsv(uplo,trans,diag,n,a,lda,x,incx)
           integer(ilp), intent(in) :: incx, lda, n
           character, intent(in) :: diag, trans, uplo
           complex(sp), intent(in) :: a(lda,*)
           complex(sp), intent(inout) :: x(*)
     end subroutine stdlib_ctrsv

     pure module subroutine stdlib_ztrsv(uplo,trans,diag,n,a,lda,x,incx)
           integer(ilp), intent(in) :: incx, lda, n
           character, intent(in) :: diag, trans, uplo
           complex(dp), intent(in) :: a(lda,*)
           complex(dp), intent(inout) :: x(*)
     end subroutine stdlib_ztrsv


end interface 


interface 
     pure module subroutine stdlib_stbsv(uplo,trans,diag,n,k,a,lda,x,incx)
           integer(ilp), intent(in) :: incx, k, lda, n
           character, intent(in) :: diag, trans, uplo
           real(sp), intent(in) :: a(lda,*)
           real(sp), intent(inout) :: x(*)
     end subroutine stdlib_stbsv

     pure module subroutine stdlib_dtbsv(uplo,trans,diag,n,k,a,lda,x,incx)
           integer(ilp), intent(in) :: incx, k, lda, n
           character, intent(in) :: diag, trans, uplo
           real(dp), intent(in) :: a(lda,*)
           real(dp), intent(inout) :: x(*)
     end subroutine stdlib_dtbsv


     pure module subroutine stdlib_ctbsv(uplo,trans,diag,n,k,a,lda,x,incx)
           integer(ilp), intent(in) :: incx, k, lda, n
           character, intent(in) :: diag, trans, uplo
           complex(sp), intent(in) :: a(lda,*)
           complex(sp), intent(inout) :: x(*)
     end subroutine stdlib_ctbsv

     pure module subroutine stdlib_ztbsv(uplo,trans,diag,n,k,a,lda,x,incx)
           integer(ilp), intent(in) :: incx, k, lda, n
           character, intent(in) :: diag, trans, uplo
           complex(dp), intent(in) :: a(lda,*)
           complex(dp), intent(inout) :: x(*)
     end subroutine stdlib_ztbsv


end interface 


interface 
     pure module subroutine stdlib_stpsv(uplo,trans,diag,n,ap,x,incx)
           integer(ilp), intent(in) :: incx, n
           character, intent(in) :: diag, trans, uplo
           real(sp), intent(in) :: ap(*)
           real(sp), intent(inout) :: x(*)
     end subroutine stdlib_stpsv

     pure module subroutine stdlib_dtpsv(uplo,trans,diag,n,ap,x,incx)
           integer(ilp), intent(in) :: incx, n
           character, intent(in) :: diag, trans, uplo
           real(dp), intent(in) :: ap(*)
           real(dp), intent(inout) :: x(*)
     end subroutine stdlib_dtpsv


     pure module subroutine stdlib_ctpsv(uplo,trans,diag,n,ap,x,incx)
           integer(ilp), intent(in) :: incx, n
           character, intent(in) :: diag, trans, uplo
           complex(sp), intent(in) :: ap(*)
           complex(sp), intent(inout) :: x(*)
     end subroutine stdlib_ctpsv

     pure module subroutine stdlib_ztpsv(uplo,trans,diag,n,ap,x,incx)
           integer(ilp), intent(in) :: incx, n
           character, intent(in) :: diag, trans, uplo
           complex(dp), intent(in) :: ap(*)
           complex(dp), intent(inout) :: x(*)
     end subroutine stdlib_ztpsv


end interface 


interface 
     pure module subroutine stdlib_sgemm(transa,transb,m,n,k,alpha,a,lda,b,ldb,beta,c,ldc)
           real(sp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: k, lda, ldb, ldc, m, n
           character, intent(in) :: transa, transb
           real(sp), intent(in) :: a(lda,*), b(ldb,*)
           real(sp), intent(inout) :: c(ldc,*)
     end subroutine stdlib_sgemm

     pure module subroutine stdlib_dgemm(transa,transb,m,n,k,alpha,a,lda,b,ldb,beta,c,ldc)
           real(dp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: k, lda, ldb, ldc, m, n
           character, intent(in) :: transa, transb
           real(dp), intent(in) :: a(lda,*), b(ldb,*)
           real(dp), intent(inout) :: c(ldc,*)
     end subroutine stdlib_dgemm


     pure module subroutine stdlib_cgemm(transa,transb,m,n,k,alpha,a,lda,b,ldb,beta,c,ldc)
           complex(sp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: k, lda, ldb, ldc, m, n
           character, intent(in) :: transa, transb
           complex(sp), intent(in) :: a(lda,*), b(ldb,*)
           complex(sp), intent(inout) :: c(ldc,*)
     end subroutine stdlib_cgemm

     pure module subroutine stdlib_zgemm(transa,transb,m,n,k,alpha,a,lda,b,ldb,beta,c,ldc)
           complex(dp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: k, lda, ldb, ldc, m, n
           character, intent(in) :: transa, transb
           complex(dp), intent(in) :: a(lda,*), b(ldb,*)
           complex(dp), intent(inout) :: c(ldc,*)
     end subroutine stdlib_zgemm


end interface 


interface 
     pure module subroutine stdlib_chemm(side,uplo,m,n,alpha,a,lda,b,ldb,beta,c,ldc)
           complex(sp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: lda, ldb, ldc, m, n
           character, intent(in) :: side, uplo
           complex(sp), intent(in) :: a(lda,*), b(ldb,*)
           complex(sp), intent(inout) :: c(ldc,*)
     end subroutine stdlib_chemm

     pure module subroutine stdlib_zhemm(side,uplo,m,n,alpha,a,lda,b,ldb,beta,c,ldc)
           complex(dp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: lda, ldb, ldc, m, n
           character, intent(in) :: side, uplo
           complex(dp), intent(in) :: a(lda,*), b(ldb,*)
           complex(dp), intent(inout) :: c(ldc,*)
     end subroutine stdlib_zhemm


end interface 


interface 
     pure module subroutine stdlib_cherk(uplo,trans,n,k,alpha,a,lda,beta,c,ldc)
           real(sp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: k, lda, ldc, n
           character, intent(in) :: trans, uplo
           complex(sp), intent(in) :: a(lda,*)
           complex(sp), intent(inout) :: c(ldc,*)
     end subroutine stdlib_cherk

     pure module subroutine stdlib_zherk(uplo,trans,n,k,alpha,a,lda,beta,c,ldc)
           real(dp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: k, lda, ldc, n
           character, intent(in) :: trans, uplo
           complex(dp), intent(in) :: a(lda,*)
           complex(dp), intent(inout) :: c(ldc,*)
     end subroutine stdlib_zherk


end interface 


interface 
     pure module subroutine stdlib_cher2k(uplo,trans,n,k,alpha,a,lda,b,ldb,beta,c,ldc)
           complex(sp), intent(in) :: alpha
           real(sp), intent(in) :: beta
           integer(ilp), intent(in) :: k, lda, ldb, ldc, n
           character, intent(in) :: trans, uplo
           complex(sp), intent(in) :: a(lda,*), b(ldb,*)
           complex(sp), intent(inout) :: c(ldc,*)
     end subroutine stdlib_cher2k

     pure module subroutine stdlib_zher2k(uplo,trans,n,k,alpha,a,lda,b,ldb,beta,c,ldc)
           complex(dp), intent(in) :: alpha
           real(dp), intent(in) :: beta
           integer(ilp), intent(in) :: k, lda, ldb, ldc, n
           character, intent(in) :: trans, uplo
           complex(dp), intent(in) :: a(lda,*), b(ldb,*)
           complex(dp), intent(inout) :: c(ldc,*)
     end subroutine stdlib_zher2k


end interface 


interface 
     pure module subroutine stdlib_ssyrk(uplo,trans,n,k,alpha,a,lda,beta,c,ldc)
           real(sp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: k, lda, ldc, n
           character, intent(in) :: trans, uplo
           real(sp), intent(in) :: a(lda,*)
           real(sp), intent(inout) :: c(ldc,*)
     end subroutine stdlib_ssyrk

     pure module subroutine stdlib_dsyrk(uplo,trans,n,k,alpha,a,lda,beta,c,ldc)
           real(dp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: k, lda, ldc, n
           character, intent(in) :: trans, uplo
           real(dp), intent(in) :: a(lda,*)
           real(dp), intent(inout) :: c(ldc,*)
     end subroutine stdlib_dsyrk


     pure module subroutine stdlib_csyrk(uplo,trans,n,k,alpha,a,lda,beta,c,ldc)
           complex(sp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: k, lda, ldc, n
           character, intent(in) :: trans, uplo
           complex(sp), intent(in) :: a(lda,*)
           complex(sp), intent(inout) :: c(ldc,*)
     end subroutine stdlib_csyrk

     pure module subroutine stdlib_zsyrk(uplo,trans,n,k,alpha,a,lda,beta,c,ldc)
           complex(dp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: k, lda, ldc, n
           character, intent(in) :: trans, uplo
           complex(dp), intent(in) :: a(lda,*)
           complex(dp), intent(inout) :: c(ldc,*)
     end subroutine stdlib_zsyrk


end interface 


interface 
     pure module subroutine stdlib_ssyr2k(uplo,trans,n,k,alpha,a,lda,b,ldb,beta,c,ldc)
           real(sp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: k, lda, ldb, ldc, n
           character, intent(in) :: trans, uplo
           real(sp), intent(in) :: a(lda,*), b(ldb,*)
           real(sp), intent(inout) :: c(ldc,*)
     end subroutine stdlib_ssyr2k

     pure module subroutine stdlib_dsyr2k(uplo,trans,n,k,alpha,a,lda,b,ldb,beta,c,ldc)
           real(dp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: k, lda, ldb, ldc, n
           character, intent(in) :: trans, uplo
           real(dp), intent(in) :: a(lda,*), b(ldb,*)
           real(dp), intent(inout) :: c(ldc,*)
     end subroutine stdlib_dsyr2k


     pure module subroutine stdlib_csyr2k(uplo,trans,n,k,alpha,a,lda,b,ldb,beta,c,ldc)
           complex(sp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: k, lda, ldb, ldc, n
           character, intent(in) :: trans, uplo
           complex(sp), intent(in) :: a(lda,*), b(ldb,*)
           complex(sp), intent(inout) :: c(ldc,*)
     end subroutine stdlib_csyr2k

     pure module subroutine stdlib_zsyr2k(uplo,trans,n,k,alpha,a,lda,b,ldb,beta,c,ldc)
           complex(dp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: k, lda, ldb, ldc, n
           character, intent(in) :: trans, uplo
           complex(dp), intent(in) :: a(lda,*), b(ldb,*)
           complex(dp), intent(inout) :: c(ldc,*)
     end subroutine stdlib_zsyr2k


end interface 


interface 
     pure module subroutine stdlib_ssymm(side,uplo,m,n,alpha,a,lda,b,ldb,beta,c,ldc)
           real(sp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: lda, ldb, ldc, m, n
           character, intent(in) :: side, uplo
           real(sp), intent(in) :: a(lda,*), b(ldb,*)
           real(sp), intent(inout) :: c(ldc,*)
     end subroutine stdlib_ssymm

     pure module subroutine stdlib_dsymm(side,uplo,m,n,alpha,a,lda,b,ldb,beta,c,ldc)
           real(dp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: lda, ldb, ldc, m, n
           character, intent(in) :: side, uplo
           real(dp), intent(in) :: a(lda,*), b(ldb,*)
           real(dp), intent(inout) :: c(ldc,*)
     end subroutine stdlib_dsymm


     pure module subroutine stdlib_csymm(side,uplo,m,n,alpha,a,lda,b,ldb,beta,c,ldc)
           complex(sp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: lda, ldb, ldc, m, n
           character, intent(in) :: side, uplo
           complex(sp), intent(in) :: a(lda,*), b(ldb,*)
           complex(sp), intent(inout) :: c(ldc,*)
     end subroutine stdlib_csymm

     pure module subroutine stdlib_zsymm(side,uplo,m,n,alpha,a,lda,b,ldb,beta,c,ldc)
           complex(dp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: lda, ldb, ldc, m, n
           character, intent(in) :: side, uplo
           complex(dp), intent(in) :: a(lda,*), b(ldb,*)
           complex(dp), intent(inout) :: c(ldc,*)
     end subroutine stdlib_zsymm


end interface 


interface 
     pure module subroutine stdlib_strmm(side,uplo,transa,diag,m,n,alpha,a,lda,b,ldb)
           real(sp), intent(in) :: alpha
           integer(ilp), intent(in) :: lda, ldb, m, n
           character, intent(in) :: diag, side, transa, uplo
           real(sp), intent(in) :: a(lda,*)
           real(sp), intent(inout) :: b(ldb,*)
     end subroutine stdlib_strmm

     pure module subroutine stdlib_dtrmm(side,uplo,transa,diag,m,n,alpha,a,lda,b,ldb)
           real(dp), intent(in) :: alpha
           integer(ilp), intent(in) :: lda, ldb, m, n
           character, intent(in) :: diag, side, transa, uplo
           real(dp), intent(in) :: a(lda,*)
           real(dp), intent(inout) :: b(ldb,*)
     end subroutine stdlib_dtrmm


     pure module subroutine stdlib_ctrmm(side,uplo,transa,diag,m,n,alpha,a,lda,b,ldb)
           complex(sp), intent(in) :: alpha
           integer(ilp), intent(in) :: lda, ldb, m, n
           character, intent(in) :: diag, side, transa, uplo
           complex(sp), intent(in) :: a(lda,*)
           complex(sp), intent(inout) :: b(ldb,*)
     end subroutine stdlib_ctrmm

     pure module subroutine stdlib_ztrmm(side,uplo,transa,diag,m,n,alpha,a,lda,b,ldb)
           complex(dp), intent(in) :: alpha
           integer(ilp), intent(in) :: lda, ldb, m, n
           character, intent(in) :: diag, side, transa, uplo
           complex(dp), intent(in) :: a(lda,*)
           complex(dp), intent(inout) :: b(ldb,*)
     end subroutine stdlib_ztrmm


end interface 


interface 
     pure module subroutine stdlib_strsm(side,uplo,transa,diag,m,n,alpha,a,lda,b,ldb)
           real(sp), intent(in) :: alpha
           integer(ilp), intent(in) :: lda, ldb, m, n
           character, intent(in) :: diag, side, transa, uplo
           real(sp), intent(in) :: a(lda,*)
           real(sp), intent(inout) :: b(ldb,*)
     end subroutine stdlib_strsm

     pure module subroutine stdlib_dtrsm(side,uplo,transa,diag,m,n,alpha,a,lda,b,ldb)
           real(dp), intent(in) :: alpha
           integer(ilp), intent(in) :: lda, ldb, m, n
           character, intent(in) :: diag, side, transa, uplo
           real(dp), intent(in) :: a(lda,*)
           real(dp), intent(inout) :: b(ldb,*)
     end subroutine stdlib_dtrsm


     pure module subroutine stdlib_ctrsm(side,uplo,transa,diag,m,n,alpha,a,lda,b,ldb)
           complex(sp), intent(in) :: alpha
           integer(ilp), intent(in) :: lda, ldb, m, n
           character, intent(in) :: diag, side, transa, uplo
           complex(sp), intent(in) :: a(lda,*)
           complex(sp), intent(inout) :: b(ldb,*)
     end subroutine stdlib_ctrsm

     pure module subroutine stdlib_ztrsm(side,uplo,transa,diag,m,n,alpha,a,lda,b,ldb)
           complex(dp), intent(in) :: alpha
           integer(ilp), intent(in) :: lda, ldb, m, n
           character, intent(in) :: diag, side, transa, uplo
           complex(dp), intent(in) :: a(lda,*)
           complex(dp), intent(inout) :: b(ldb,*)
     end subroutine stdlib_ztrsm


end interface 

end module stdlib_blas
