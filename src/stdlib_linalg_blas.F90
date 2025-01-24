module stdlib_linalg_blas
     use stdlib_linalg_constants
     use stdlib_linalg_blas_aux
     use stdlib_blas
     
     implicit none
     public     
         
          interface asum
          !! ASUM takes the sum of the absolute values.
#ifdef STDLIB_EXTERNAL_BLAS
               pure real(dp) function dasum( n, x, incx )
                    import sp,dp,qp,ilp,lk
                    implicit none
                    integer(ilp), intent(in) :: incx,n
                    real(dp), intent(in) :: x(*)
               end function dasum
#else
               module procedure stdlib_dasum
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure real(dp) function dzasum( n, x, incx )
                    import sp,dp,qp,ilp,lk
                    implicit none
                    integer(ilp), intent(in) :: incx,n
                    complex(dp), intent(in) :: x(*)
               end function dzasum
#else
               module procedure stdlib_dzasum
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure real(sp) function sasum( n, x, incx )
                    import sp,dp,qp,ilp,lk
                    implicit none
                    integer(ilp), intent(in) :: incx,n
                    real(sp), intent(in) :: x(*)
               end function sasum
#else
               module procedure stdlib_sasum
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure real(sp) function scasum( n, x, incx )
                    import sp,dp,qp,ilp,lk
                    implicit none
                    integer(ilp), intent(in) :: incx,n
                    complex(sp), intent(in) :: x(*)
               end function scasum
#else
               module procedure stdlib_scasum
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure real(dp) function dasum( n, x, incx )
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    integer(ilp64), intent(in) :: incx,n
                    real(dp), intent(in) :: x(*)
               end function dasum
#else
               module procedure stdlib_I64_dasum
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure real(dp) function dzasum( n, x, incx )
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    integer(ilp64), intent(in) :: incx,n
                    complex(dp), intent(in) :: x(*)
               end function dzasum
#else
               module procedure stdlib_I64_dzasum
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure real(sp) function sasum( n, x, incx )
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    integer(ilp64), intent(in) :: incx,n
                    real(sp), intent(in) :: x(*)
               end function sasum
#else
               module procedure stdlib_I64_sasum
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure real(sp) function scasum( n, x, incx )
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    integer(ilp64), intent(in) :: incx,n
                    complex(sp), intent(in) :: x(*)
               end function scasum
#else
               module procedure stdlib_I64_scasum
#endif
          end interface asum

          interface axpy
          !! AXPY constant times a vector plus a vector.
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine caxpy(n,ca,cx,incx,cy,incy)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    complex(sp), intent(in) :: ca,cx(*)
                    integer(ilp), intent(in) :: incx,incy,n
                    complex(sp), intent(inout) :: cy(*)
               end subroutine caxpy
#else
               module procedure stdlib_caxpy
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine daxpy(n,da,dx,incx,dy,incy)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    real(dp), intent(in) :: da,dx(*)
                    integer(ilp), intent(in) :: incx,incy,n
                    real(dp), intent(inout) :: dy(*)
               end subroutine daxpy
#else
               module procedure stdlib_daxpy
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine saxpy(n,sa,sx,incx,sy,incy)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    real(sp), intent(in) :: sa,sx(*)
                    integer(ilp), intent(in) :: incx,incy,n
                    real(sp), intent(inout) :: sy(*)
               end subroutine saxpy
#else
               module procedure stdlib_saxpy
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine zaxpy(n,za,zx,incx,zy,incy)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    complex(dp), intent(in) :: za,zx(*)
                    integer(ilp), intent(in) :: incx,incy,n
                    complex(dp), intent(inout) :: zy(*)
               end subroutine zaxpy
#else
               module procedure stdlib_zaxpy
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine caxpy(n,ca,cx,incx,cy,incy)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    complex(sp), intent(in) :: ca,cx(*)
                    integer(ilp64), intent(in) :: incx,incy,n
                    complex(sp), intent(inout) :: cy(*)
               end subroutine caxpy
#else
               module procedure stdlib_I64_caxpy
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine daxpy(n,da,dx,incx,dy,incy)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    real(dp), intent(in) :: da,dx(*)
                    integer(ilp64), intent(in) :: incx,incy,n
                    real(dp), intent(inout) :: dy(*)
               end subroutine daxpy
#else
               module procedure stdlib_I64_daxpy
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine saxpy(n,sa,sx,incx,sy,incy)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    real(sp), intent(in) :: sa,sx(*)
                    integer(ilp64), intent(in) :: incx,incy,n
                    real(sp), intent(inout) :: sy(*)
               end subroutine saxpy
#else
               module procedure stdlib_I64_saxpy
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine zaxpy(n,za,zx,incx,zy,incy)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    complex(dp), intent(in) :: za,zx(*)
                    integer(ilp64), intent(in) :: incx,incy,n
                    complex(dp), intent(inout) :: zy(*)
               end subroutine zaxpy
#else
               module procedure stdlib_I64_zaxpy
#endif
          end interface axpy

          interface copy
          !! COPY copies a vector x to a vector y.
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine ccopy(n,cx,incx,cy,incy)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    integer(ilp), intent(in) :: incx,incy,n
                    complex(sp), intent(in) :: cx(*)
                    complex(sp), intent(out) :: cy(*)
               end subroutine ccopy
#else
               module procedure stdlib_ccopy
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine dcopy(n,dx,incx,dy,incy)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    integer(ilp), intent(in) :: incx,incy,n
                    real(dp), intent(in) :: dx(*)
                    real(dp), intent(out) :: dy(*)
               end subroutine dcopy
#else
               module procedure stdlib_dcopy
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine scopy(n,sx,incx,sy,incy)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    integer(ilp), intent(in) :: incx,incy,n
                    real(sp), intent(in) :: sx(*)
                    real(sp), intent(out) :: sy(*)
               end subroutine scopy
#else
               module procedure stdlib_scopy
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine zcopy(n,zx,incx,zy,incy)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    integer(ilp), intent(in) :: incx,incy,n
                    complex(dp), intent(in) :: zx(*)
                    complex(dp), intent(out) :: zy(*)
               end subroutine zcopy
#else
               module procedure stdlib_zcopy
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine ccopy(n,cx,incx,cy,incy)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    integer(ilp64), intent(in) :: incx,incy,n
                    complex(sp), intent(in) :: cx(*)
                    complex(sp), intent(out) :: cy(*)
               end subroutine ccopy
#else
               module procedure stdlib_I64_ccopy
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine dcopy(n,dx,incx,dy,incy)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    integer(ilp64), intent(in) :: incx,incy,n
                    real(dp), intent(in) :: dx(*)
                    real(dp), intent(out) :: dy(*)
               end subroutine dcopy
#else
               module procedure stdlib_I64_dcopy
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine scopy(n,sx,incx,sy,incy)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    integer(ilp64), intent(in) :: incx,incy,n
                    real(sp), intent(in) :: sx(*)
                    real(sp), intent(out) :: sy(*)
               end subroutine scopy
#else
               module procedure stdlib_I64_scopy
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine zcopy(n,zx,incx,zy,incy)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    integer(ilp64), intent(in) :: incx,incy,n
                    complex(dp), intent(in) :: zx(*)
                    complex(dp), intent(out) :: zy(*)
               end subroutine zcopy
#else
               module procedure stdlib_I64_zcopy
#endif
          end interface copy

          interface dot
          !! DOT forms the dot product of two vectors.
          !! uses unrolled loops for increments equal to one.
#ifdef STDLIB_EXTERNAL_BLAS
               pure real(dp) function ddot(n,dx,incx,dy,incy)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    integer(ilp), intent(in) :: incx,incy,n
                    real(dp), intent(in) :: dx(*),dy(*)
               end function ddot
#else
               module procedure stdlib_ddot
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure real(sp) function sdot(n,sx,incx,sy,incy)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    integer(ilp), intent(in) :: incx,incy,n
                    real(sp), intent(in) :: sx(*),sy(*)
               end function sdot
#else
               module procedure stdlib_sdot
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure real(dp) function ddot(n,dx,incx,dy,incy)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    integer(ilp64), intent(in) :: incx,incy,n
                    real(dp), intent(in) :: dx(*),dy(*)
               end function ddot
#else
               module procedure stdlib_I64_ddot
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure real(sp) function sdot(n,sx,incx,sy,incy)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    integer(ilp64), intent(in) :: incx,incy,n
                    real(sp), intent(in) :: sx(*),sy(*)
               end function sdot
#else
               module procedure stdlib_I64_sdot
#endif
          end interface dot

          interface dotc
          !! DOTC forms the dot product of two complex vectors
          !! DOTC = X^H * Y
#ifdef STDLIB_EXTERNAL_BLAS
               pure complex(sp) function cdotc(n,cx,incx,cy,incy)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    integer(ilp), intent(in) :: incx,incy,n
                    complex(sp), intent(in) :: cx(*),cy(*)
               end function cdotc
#else
               module procedure stdlib_cdotc
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure complex(dp) function zdotc(n,zx,incx,zy,incy)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    integer(ilp), intent(in) :: incx,incy,n
                    complex(dp), intent(in) :: zx(*),zy(*)
               end function zdotc
#else
               module procedure stdlib_zdotc
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure complex(sp) function cdotc(n,cx,incx,cy,incy)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    integer(ilp64), intent(in) :: incx,incy,n
                    complex(sp), intent(in) :: cx(*),cy(*)
               end function cdotc
#else
               module procedure stdlib_I64_cdotc
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure complex(dp) function zdotc(n,zx,incx,zy,incy)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    integer(ilp64), intent(in) :: incx,incy,n
                    complex(dp), intent(in) :: zx(*),zy(*)
               end function zdotc
#else
               module procedure stdlib_I64_zdotc
#endif
          end interface dotc

          interface dotu
          !! DOTU forms the dot product of two complex vectors
          !! DOTU = X^T * Y
#ifdef STDLIB_EXTERNAL_BLAS
               pure complex(sp) function cdotu(n,cx,incx,cy,incy)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    integer(ilp), intent(in) :: incx,incy,n
                    complex(sp), intent(in) :: cx(*),cy(*)
               end function cdotu
#else
               module procedure stdlib_cdotu
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure complex(dp) function zdotu(n,zx,incx,zy,incy)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    integer(ilp), intent(in) :: incx,incy,n
                    complex(dp), intent(in) :: zx(*),zy(*)
               end function zdotu
#else
               module procedure stdlib_zdotu
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure complex(sp) function cdotu(n,cx,incx,cy,incy)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    integer(ilp64), intent(in) :: incx,incy,n
                    complex(sp), intent(in) :: cx(*),cy(*)
               end function cdotu
#else
               module procedure stdlib_I64_cdotu
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure complex(dp) function zdotu(n,zx,incx,zy,incy)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    integer(ilp64), intent(in) :: incx,incy,n
                    complex(dp), intent(in) :: zx(*),zy(*)
               end function zdotu
#else
               module procedure stdlib_I64_zdotu
#endif
          end interface dotu

          interface gbmv
          !! GBMV performs one of the matrix-vector operations
          !! y := alpha*A*x + beta*y,   or   y := alpha*A**T*x + beta*y,   or
          !! y := alpha*A**H*x + beta*y,
          !! where alpha and beta are scalars, x and y are vectors and A is an
          !! m by n band matrix, with kl sub-diagonals and ku super-diagonals.
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine cgbmv(trans,m,n,kl,ku,alpha,a,lda,x,incx,beta,y,incy)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    complex(sp), intent(in) :: alpha,beta,a(lda,*),x(*)
                    integer(ilp), intent(in) :: incx,incy,kl,ku,lda,m,n
                    character, intent(in) :: trans
                    complex(sp), intent(inout) :: y(*)
               end subroutine cgbmv
#else
               module procedure stdlib_cgbmv
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine dgbmv(trans,m,n,kl,ku,alpha,a,lda,x,incx,beta,y,incy)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    real(dp), intent(in) :: alpha,beta,a(lda,*),x(*)
                    integer(ilp), intent(in) :: incx,incy,kl,ku,lda,m,n
                    character, intent(in) :: trans
                    real(dp), intent(inout) :: y(*)
               end subroutine dgbmv
#else
               module procedure stdlib_dgbmv
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine sgbmv(trans,m,n,kl,ku,alpha,a,lda,x,incx,beta,y,incy)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    real(sp), intent(in) :: alpha,beta,a(lda,*),x(*)
                    integer(ilp), intent(in) :: incx,incy,kl,ku,lda,m,n
                    character, intent(in) :: trans
                    real(sp), intent(inout) :: y(*)
               end subroutine sgbmv
#else
               module procedure stdlib_sgbmv
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine zgbmv(trans,m,n,kl,ku,alpha,a,lda,x,incx,beta,y,incy)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    complex(dp), intent(in) :: alpha,beta,a(lda,*),x(*)
                    integer(ilp), intent(in) :: incx,incy,kl,ku,lda,m,n
                    character, intent(in) :: trans
                    complex(dp), intent(inout) :: y(*)
               end subroutine zgbmv
#else
               module procedure stdlib_zgbmv
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine cgbmv(trans,m,n,kl,ku,alpha,a,lda,x,incx,beta,y,incy)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    complex(sp), intent(in) :: alpha,beta,a(lda,*),x(*)
                    integer(ilp64), intent(in) :: incx,incy,kl,ku,lda,m,n
                    character, intent(in) :: trans
                    complex(sp), intent(inout) :: y(*)
               end subroutine cgbmv
#else
               module procedure stdlib_I64_cgbmv
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine dgbmv(trans,m,n,kl,ku,alpha,a,lda,x,incx,beta,y,incy)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    real(dp), intent(in) :: alpha,beta,a(lda,*),x(*)
                    integer(ilp64), intent(in) :: incx,incy,kl,ku,lda,m,n
                    character, intent(in) :: trans
                    real(dp), intent(inout) :: y(*)
               end subroutine dgbmv
#else
               module procedure stdlib_I64_dgbmv
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine sgbmv(trans,m,n,kl,ku,alpha,a,lda,x,incx,beta,y,incy)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    real(sp), intent(in) :: alpha,beta,a(lda,*),x(*)
                    integer(ilp64), intent(in) :: incx,incy,kl,ku,lda,m,n
                    character, intent(in) :: trans
                    real(sp), intent(inout) :: y(*)
               end subroutine sgbmv
#else
               module procedure stdlib_I64_sgbmv
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine zgbmv(trans,m,n,kl,ku,alpha,a,lda,x,incx,beta,y,incy)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    complex(dp), intent(in) :: alpha,beta,a(lda,*),x(*)
                    integer(ilp64), intent(in) :: incx,incy,kl,ku,lda,m,n
                    character, intent(in) :: trans
                    complex(dp), intent(inout) :: y(*)
               end subroutine zgbmv
#else
               module procedure stdlib_I64_zgbmv
#endif
          end interface gbmv

          interface gemm
          !! GEMM performs one of the matrix-matrix operations
          !! C := alpha*op( A )*op( B ) + beta*C,
          !! where  op( X ) is one of
          !! op( X ) = X   or   op( X ) = X**T   or   op( X ) = X**H,
          !! alpha and beta are scalars, and A, B and C are matrices, with op( A )
          !! an m by k matrix,  op( B )  a  k by n matrix and  C an m by n matrix.
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine cgemm(transa,transb,m,n,k,alpha,a,lda,b,ldb,beta,c,ldc)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    complex(sp), intent(in) :: alpha,beta,a(lda,*),b(ldb,*)
                    integer(ilp), intent(in) :: k,lda,ldb,ldc,m,n
                    character, intent(in) :: transa,transb
                    complex(sp), intent(inout) :: c(ldc,*)
               end subroutine cgemm
#else
               module procedure stdlib_cgemm
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine dgemm(transa,transb,m,n,k,alpha,a,lda,b,ldb,beta,c,ldc)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    real(dp), intent(in) :: alpha,beta,a(lda,*),b(ldb,*)
                    integer(ilp), intent(in) :: k,lda,ldb,ldc,m,n
                    character, intent(in) :: transa,transb
                    real(dp), intent(inout) :: c(ldc,*)
               end subroutine dgemm
#else
               module procedure stdlib_dgemm
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine sgemm(transa,transb,m,n,k,alpha,a,lda,b,ldb,beta,c,ldc)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    real(sp), intent(in) :: alpha,beta,a(lda,*),b(ldb,*)
                    integer(ilp), intent(in) :: k,lda,ldb,ldc,m,n
                    character, intent(in) :: transa,transb
                    real(sp), intent(inout) :: c(ldc,*)
               end subroutine sgemm
#else
               module procedure stdlib_sgemm
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine zgemm(transa,transb,m,n,k,alpha,a,lda,b,ldb,beta,c,ldc)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    complex(dp), intent(in) :: alpha,beta,a(lda,*),b(ldb,*)
                    integer(ilp), intent(in) :: k,lda,ldb,ldc,m,n
                    character, intent(in) :: transa,transb
                    complex(dp), intent(inout) :: c(ldc,*)
               end subroutine zgemm
#else
               module procedure stdlib_zgemm
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine cgemm(transa,transb,m,n,k,alpha,a,lda,b,ldb,beta,c,ldc)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    complex(sp), intent(in) :: alpha,beta,a(lda,*),b(ldb,*)
                    integer(ilp64), intent(in) :: k,lda,ldb,ldc,m,n
                    character, intent(in) :: transa,transb
                    complex(sp), intent(inout) :: c(ldc,*)
               end subroutine cgemm
#else
               module procedure stdlib_I64_cgemm
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine dgemm(transa,transb,m,n,k,alpha,a,lda,b,ldb,beta,c,ldc)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    real(dp), intent(in) :: alpha,beta,a(lda,*),b(ldb,*)
                    integer(ilp64), intent(in) :: k,lda,ldb,ldc,m,n
                    character, intent(in) :: transa,transb
                    real(dp), intent(inout) :: c(ldc,*)
               end subroutine dgemm
#else
               module procedure stdlib_I64_dgemm
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine sgemm(transa,transb,m,n,k,alpha,a,lda,b,ldb,beta,c,ldc)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    real(sp), intent(in) :: alpha,beta,a(lda,*),b(ldb,*)
                    integer(ilp64), intent(in) :: k,lda,ldb,ldc,m,n
                    character, intent(in) :: transa,transb
                    real(sp), intent(inout) :: c(ldc,*)
               end subroutine sgemm
#else
               module procedure stdlib_I64_sgemm
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine zgemm(transa,transb,m,n,k,alpha,a,lda,b,ldb,beta,c,ldc)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    complex(dp), intent(in) :: alpha,beta,a(lda,*),b(ldb,*)
                    integer(ilp64), intent(in) :: k,lda,ldb,ldc,m,n
                    character, intent(in) :: transa,transb
                    complex(dp), intent(inout) :: c(ldc,*)
               end subroutine zgemm
#else
               module procedure stdlib_I64_zgemm
#endif
          end interface gemm

          interface gemv
          !! GEMV performs one of the matrix-vector operations
          !! y := alpha*A*x + beta*y,   or   y := alpha*A**T*x + beta*y,   or
          !! y := alpha*A**H*x + beta*y,
          !! where alpha and beta are scalars, x and y are vectors and A is an
          !! m by n matrix.
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine cgemv(trans,m,n,alpha,a,lda,x,incx,beta,y,incy)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    complex(sp), intent(in) :: alpha,beta,a(lda,*),x(*)
                    integer(ilp), intent(in) :: incx,incy,lda,m,n
                    character, intent(in) :: trans
                    complex(sp), intent(inout) :: y(*)
               end subroutine cgemv
#else
               module procedure stdlib_cgemv
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine dgemv(trans,m,n,alpha,a,lda,x,incx,beta,y,incy)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    real(dp), intent(in) :: alpha,beta,a(lda,*),x(*)
                    integer(ilp), intent(in) :: incx,incy,lda,m,n
                    character, intent(in) :: trans
                    real(dp), intent(inout) :: y(*)
               end subroutine dgemv
#else
               module procedure stdlib_dgemv
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine sgemv(trans,m,n,alpha,a,lda,x,incx,beta,y,incy)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    real(sp), intent(in) :: alpha,beta,a(lda,*),x(*)
                    integer(ilp), intent(in) :: incx,incy,lda,m,n
                    character, intent(in) :: trans
                    real(sp), intent(inout) :: y(*)
               end subroutine sgemv
#else
               module procedure stdlib_sgemv
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine zgemv(trans,m,n,alpha,a,lda,x,incx,beta,y,incy)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    complex(dp), intent(in) :: alpha,beta,a(lda,*),x(*)
                    integer(ilp), intent(in) :: incx,incy,lda,m,n
                    character, intent(in) :: trans
                    complex(dp), intent(inout) :: y(*)
               end subroutine zgemv
#else
               module procedure stdlib_zgemv
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine cgemv(trans,m,n,alpha,a,lda,x,incx,beta,y,incy)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    complex(sp), intent(in) :: alpha,beta,a(lda,*),x(*)
                    integer(ilp64), intent(in) :: incx,incy,lda,m,n
                    character, intent(in) :: trans
                    complex(sp), intent(inout) :: y(*)
               end subroutine cgemv
#else
               module procedure stdlib_I64_cgemv
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine dgemv(trans,m,n,alpha,a,lda,x,incx,beta,y,incy)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    real(dp), intent(in) :: alpha,beta,a(lda,*),x(*)
                    integer(ilp64), intent(in) :: incx,incy,lda,m,n
                    character, intent(in) :: trans
                    real(dp), intent(inout) :: y(*)
               end subroutine dgemv
#else
               module procedure stdlib_I64_dgemv
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine sgemv(trans,m,n,alpha,a,lda,x,incx,beta,y,incy)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    real(sp), intent(in) :: alpha,beta,a(lda,*),x(*)
                    integer(ilp64), intent(in) :: incx,incy,lda,m,n
                    character, intent(in) :: trans
                    real(sp), intent(inout) :: y(*)
               end subroutine sgemv
#else
               module procedure stdlib_I64_sgemv
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine zgemv(trans,m,n,alpha,a,lda,x,incx,beta,y,incy)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    complex(dp), intent(in) :: alpha,beta,a(lda,*),x(*)
                    integer(ilp64), intent(in) :: incx,incy,lda,m,n
                    character, intent(in) :: trans
                    complex(dp), intent(inout) :: y(*)
               end subroutine zgemv
#else
               module procedure stdlib_I64_zgemv
#endif
          end interface gemv

          interface ger
          !! GER performs the rank 1 operation
          !! A := alpha*x*y**T + A,
          !! where alpha is a scalar, x is an m element vector, y is an n element
          !! vector and A is an m by n matrix.
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine dger(m,n,alpha,x,incx,y,incy,a,lda)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    real(dp), intent(in) :: alpha,x(*),y(*)
                    integer(ilp), intent(in) :: incx,incy,lda,m,n
                    real(dp), intent(inout) :: a(lda,*)
               end subroutine dger
#else
               module procedure stdlib_dger
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine sger(m,n,alpha,x,incx,y,incy,a,lda)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    real(sp), intent(in) :: alpha,x(*),y(*)
                    integer(ilp), intent(in) :: incx,incy,lda,m,n
                    real(sp), intent(inout) :: a(lda,*)
               end subroutine sger
#else
               module procedure stdlib_sger
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine dger(m,n,alpha,x,incx,y,incy,a,lda)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    real(dp), intent(in) :: alpha,x(*),y(*)
                    integer(ilp64), intent(in) :: incx,incy,lda,m,n
                    real(dp), intent(inout) :: a(lda,*)
               end subroutine dger
#else
               module procedure stdlib_I64_dger
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine sger(m,n,alpha,x,incx,y,incy,a,lda)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    real(sp), intent(in) :: alpha,x(*),y(*)
                    integer(ilp64), intent(in) :: incx,incy,lda,m,n
                    real(sp), intent(inout) :: a(lda,*)
               end subroutine sger
#else
               module procedure stdlib_I64_sger
#endif
          end interface ger

          interface gerc
          !! GERC performs the rank 1 operation
          !! A := alpha*x*y**H + A,
          !! where alpha is a scalar, x is an m element vector, y is an n element
          !! vector and A is an m by n matrix.
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine cgerc(m,n,alpha,x,incx,y,incy,a,lda)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    complex(sp), intent(in) :: alpha,x(*),y(*)
                    integer(ilp), intent(in) :: incx,incy,lda,m,n
                    complex(sp), intent(inout) :: a(lda,*)
               end subroutine cgerc
#else
               module procedure stdlib_cgerc
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine zgerc(m,n,alpha,x,incx,y,incy,a,lda)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    complex(dp), intent(in) :: alpha,x(*),y(*)
                    integer(ilp), intent(in) :: incx,incy,lda,m,n
                    complex(dp), intent(inout) :: a(lda,*)
               end subroutine zgerc
#else
               module procedure stdlib_zgerc
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine cgerc(m,n,alpha,x,incx,y,incy,a,lda)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    complex(sp), intent(in) :: alpha,x(*),y(*)
                    integer(ilp64), intent(in) :: incx,incy,lda,m,n
                    complex(sp), intent(inout) :: a(lda,*)
               end subroutine cgerc
#else
               module procedure stdlib_I64_cgerc
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine zgerc(m,n,alpha,x,incx,y,incy,a,lda)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    complex(dp), intent(in) :: alpha,x(*),y(*)
                    integer(ilp64), intent(in) :: incx,incy,lda,m,n
                    complex(dp), intent(inout) :: a(lda,*)
               end subroutine zgerc
#else
               module procedure stdlib_I64_zgerc
#endif
          end interface gerc

          interface geru
          !! GERU performs the rank 1 operation
          !! A := alpha*x*y**T + A,
          !! where alpha is a scalar, x is an m element vector, y is an n element
          !! vector and A is an m by n matrix.
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine cgeru(m,n,alpha,x,incx,y,incy,a,lda)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    complex(sp), intent(in) :: alpha,x(*),y(*)
                    integer(ilp), intent(in) :: incx,incy,lda,m,n
                    complex(sp), intent(inout) :: a(lda,*)
               end subroutine cgeru
#else
               module procedure stdlib_cgeru
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine zgeru(m,n,alpha,x,incx,y,incy,a,lda)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    complex(dp), intent(in) :: alpha,x(*),y(*)
                    integer(ilp), intent(in) :: incx,incy,lda,m,n
                    complex(dp), intent(inout) :: a(lda,*)
               end subroutine zgeru
#else
               module procedure stdlib_zgeru
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine cgeru(m,n,alpha,x,incx,y,incy,a,lda)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    complex(sp), intent(in) :: alpha,x(*),y(*)
                    integer(ilp64), intent(in) :: incx,incy,lda,m,n
                    complex(sp), intent(inout) :: a(lda,*)
               end subroutine cgeru
#else
               module procedure stdlib_I64_cgeru
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine zgeru(m,n,alpha,x,incx,y,incy,a,lda)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    complex(dp), intent(in) :: alpha,x(*),y(*)
                    integer(ilp64), intent(in) :: incx,incy,lda,m,n
                    complex(dp), intent(inout) :: a(lda,*)
               end subroutine zgeru
#else
               module procedure stdlib_I64_zgeru
#endif
          end interface geru

          interface hbmv
          !! HBMV performs the matrix-vector  operation
          !! y := alpha*A*x + beta*y,
          !! where alpha and beta are scalars, x and y are n element vectors and
          !! A is an n by n hermitian band matrix, with k super-diagonals.
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine chbmv(uplo,n,k,alpha,a,lda,x,incx,beta,y,incy)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    complex(sp), intent(in) :: alpha,beta,a(lda,*),x(*)
                    integer(ilp), intent(in) :: incx,incy,k,lda,n
                    character, intent(in) :: uplo
                    complex(sp), intent(inout) :: y(*)
               end subroutine chbmv
#else
               module procedure stdlib_chbmv
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine zhbmv(uplo,n,k,alpha,a,lda,x,incx,beta,y,incy)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    complex(dp), intent(in) :: alpha,beta,a(lda,*),x(*)
                    integer(ilp), intent(in) :: incx,incy,k,lda,n
                    character, intent(in) :: uplo
                    complex(dp), intent(inout) :: y(*)
               end subroutine zhbmv
#else
               module procedure stdlib_zhbmv
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine chbmv(uplo,n,k,alpha,a,lda,x,incx,beta,y,incy)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    complex(sp), intent(in) :: alpha,beta,a(lda,*),x(*)
                    integer(ilp64), intent(in) :: incx,incy,k,lda,n
                    character, intent(in) :: uplo
                    complex(sp), intent(inout) :: y(*)
               end subroutine chbmv
#else
               module procedure stdlib_I64_chbmv
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine zhbmv(uplo,n,k,alpha,a,lda,x,incx,beta,y,incy)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    complex(dp), intent(in) :: alpha,beta,a(lda,*),x(*)
                    integer(ilp64), intent(in) :: incx,incy,k,lda,n
                    character, intent(in) :: uplo
                    complex(dp), intent(inout) :: y(*)
               end subroutine zhbmv
#else
               module procedure stdlib_I64_zhbmv
#endif
          end interface hbmv

          interface hemm
          !! HEMM performs one of the matrix-matrix operations
          !! C := alpha*A*B + beta*C,
          !! or
          !! C := alpha*B*A + beta*C,
          !! where alpha and beta are scalars, A is an hermitian matrix and  B and
          !! C are m by n matrices.
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine chemm(side,uplo,m,n,alpha,a,lda,b,ldb,beta,c,ldc)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    complex(sp), intent(in) :: alpha,beta,a(lda,*),b(ldb,*)
                    integer(ilp), intent(in) :: lda,ldb,ldc,m,n
                    character, intent(in) :: side,uplo
                    complex(sp), intent(inout) :: c(ldc,*)
               end subroutine chemm
#else
               module procedure stdlib_chemm
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine zhemm(side,uplo,m,n,alpha,a,lda,b,ldb,beta,c,ldc)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    complex(dp), intent(in) :: alpha,beta,a(lda,*),b(ldb,*)
                    integer(ilp), intent(in) :: lda,ldb,ldc,m,n
                    character, intent(in) :: side,uplo
                    complex(dp), intent(inout) :: c(ldc,*)
               end subroutine zhemm
#else
               module procedure stdlib_zhemm
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine chemm(side,uplo,m,n,alpha,a,lda,b,ldb,beta,c,ldc)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    complex(sp), intent(in) :: alpha,beta,a(lda,*),b(ldb,*)
                    integer(ilp64), intent(in) :: lda,ldb,ldc,m,n
                    character, intent(in) :: side,uplo
                    complex(sp), intent(inout) :: c(ldc,*)
               end subroutine chemm
#else
               module procedure stdlib_I64_chemm
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine zhemm(side,uplo,m,n,alpha,a,lda,b,ldb,beta,c,ldc)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    complex(dp), intent(in) :: alpha,beta,a(lda,*),b(ldb,*)
                    integer(ilp64), intent(in) :: lda,ldb,ldc,m,n
                    character, intent(in) :: side,uplo
                    complex(dp), intent(inout) :: c(ldc,*)
               end subroutine zhemm
#else
               module procedure stdlib_I64_zhemm
#endif
          end interface hemm

          interface hemv
          !! HEMV performs the matrix-vector  operation
          !! y := alpha*A*x + beta*y,
          !! where alpha and beta are scalars, x and y are n element vectors and
          !! A is an n by n hermitian matrix.
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine chemv(uplo,n,alpha,a,lda,x,incx,beta,y,incy)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    complex(sp), intent(in) :: alpha,beta,a(lda,*),x(*)
                    integer(ilp), intent(in) :: incx,incy,lda,n
                    character, intent(in) :: uplo
                    complex(sp), intent(inout) :: y(*)
               end subroutine chemv
#else
               module procedure stdlib_chemv
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine zhemv(uplo,n,alpha,a,lda,x,incx,beta,y,incy)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    complex(dp), intent(in) :: alpha,beta,a(lda,*),x(*)
                    integer(ilp), intent(in) :: incx,incy,lda,n
                    character, intent(in) :: uplo
                    complex(dp), intent(inout) :: y(*)
               end subroutine zhemv
#else
               module procedure stdlib_zhemv
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine chemv(uplo,n,alpha,a,lda,x,incx,beta,y,incy)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    complex(sp), intent(in) :: alpha,beta,a(lda,*),x(*)
                    integer(ilp64), intent(in) :: incx,incy,lda,n
                    character, intent(in) :: uplo
                    complex(sp), intent(inout) :: y(*)
               end subroutine chemv
#else
               module procedure stdlib_I64_chemv
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine zhemv(uplo,n,alpha,a,lda,x,incx,beta,y,incy)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    complex(dp), intent(in) :: alpha,beta,a(lda,*),x(*)
                    integer(ilp64), intent(in) :: incx,incy,lda,n
                    character, intent(in) :: uplo
                    complex(dp), intent(inout) :: y(*)
               end subroutine zhemv
#else
               module procedure stdlib_I64_zhemv
#endif
          end interface hemv

          interface her
          !! HER performs the hermitian rank 1 operation
          !! A := alpha*x*x**H + A,
          !! where alpha is a real scalar, x is an n element vector and A is an
          !! n by n hermitian matrix.
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine cher(uplo,n,alpha,x,incx,a,lda)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    real(sp), intent(in) :: alpha
                    integer(ilp), intent(in) :: incx,lda,n
                    character, intent(in) :: uplo
                    complex(sp), intent(inout) :: a(lda,*)
                    complex(sp), intent(in) :: x(*)
               end subroutine cher
#else
               module procedure stdlib_cher
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine zher(uplo,n,alpha,x,incx,a,lda)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    real(dp), intent(in) :: alpha
                    integer(ilp), intent(in) :: incx,lda,n
                    character, intent(in) :: uplo
                    complex(dp), intent(inout) :: a(lda,*)
                    complex(dp), intent(in) :: x(*)
               end subroutine zher
#else
               module procedure stdlib_zher
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine cher(uplo,n,alpha,x,incx,a,lda)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    real(sp), intent(in) :: alpha
                    integer(ilp64), intent(in) :: incx,lda,n
                    character, intent(in) :: uplo
                    complex(sp), intent(inout) :: a(lda,*)
                    complex(sp), intent(in) :: x(*)
               end subroutine cher
#else
               module procedure stdlib_I64_cher
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine zher(uplo,n,alpha,x,incx,a,lda)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    real(dp), intent(in) :: alpha
                    integer(ilp64), intent(in) :: incx,lda,n
                    character, intent(in) :: uplo
                    complex(dp), intent(inout) :: a(lda,*)
                    complex(dp), intent(in) :: x(*)
               end subroutine zher
#else
               module procedure stdlib_I64_zher
#endif
          end interface her

          interface her2
          !! HER2 performs the hermitian rank 2 operation
          !! A := alpha*x*y**H + conjg( alpha )*y*x**H + A,
          !! where alpha is a scalar, x and y are n element vectors and A is an n
          !! by n hermitian matrix.
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine cher2(uplo,n,alpha,x,incx,y,incy,a,lda)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    complex(sp), intent(in) :: alpha,x(*),y(*)
                    integer(ilp), intent(in) :: incx,incy,lda,n
                    character, intent(in) :: uplo
                    complex(sp), intent(inout) :: a(lda,*)
               end subroutine cher2
#else
               module procedure stdlib_cher2
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine zher2(uplo,n,alpha,x,incx,y,incy,a,lda)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    complex(dp), intent(in) :: alpha,x(*),y(*)
                    integer(ilp), intent(in) :: incx,incy,lda,n
                    character, intent(in) :: uplo
                    complex(dp), intent(inout) :: a(lda,*)
               end subroutine zher2
#else
               module procedure stdlib_zher2
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine cher2(uplo,n,alpha,x,incx,y,incy,a,lda)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    complex(sp), intent(in) :: alpha,x(*),y(*)
                    integer(ilp64), intent(in) :: incx,incy,lda,n
                    character, intent(in) :: uplo
                    complex(sp), intent(inout) :: a(lda,*)
               end subroutine cher2
#else
               module procedure stdlib_I64_cher2
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine zher2(uplo,n,alpha,x,incx,y,incy,a,lda)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    complex(dp), intent(in) :: alpha,x(*),y(*)
                    integer(ilp64), intent(in) :: incx,incy,lda,n
                    character, intent(in) :: uplo
                    complex(dp), intent(inout) :: a(lda,*)
               end subroutine zher2
#else
               module procedure stdlib_I64_zher2
#endif
          end interface her2

          interface her2k
          !! HER2K performs one of the hermitian rank 2k operations
          !! C := alpha*A*B**H + conjg( alpha )*B*A**H + beta*C,
          !! or
          !! C := alpha*A**H*B + conjg( alpha )*B**H*A + beta*C,
          !! where  alpha and beta  are scalars with  beta  real,  C is an  n by n
          !! hermitian matrix and  A and B  are  n by k matrices in the first case
          !! and  k by n  matrices in the second case.
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine cher2k(uplo,trans,n,k,alpha,a,lda,b,ldb,beta,c,ldc)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    complex(sp), intent(in) :: alpha,a(lda,*),b(ldb,*)
                    real(sp), intent(in) :: beta
                    integer(ilp), intent(in) :: k,lda,ldb,ldc,n
                    character, intent(in) :: trans,uplo
                    complex(sp), intent(inout) :: c(ldc,*)
               end subroutine cher2k
#else
               module procedure stdlib_cher2k
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine zher2k(uplo,trans,n,k,alpha,a,lda,b,ldb,beta,c,ldc)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    complex(dp), intent(in) :: alpha,a(lda,*),b(ldb,*)
                    real(dp), intent(in) :: beta
                    integer(ilp), intent(in) :: k,lda,ldb,ldc,n
                    character, intent(in) :: trans,uplo
                    complex(dp), intent(inout) :: c(ldc,*)
               end subroutine zher2k
#else
               module procedure stdlib_zher2k
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine cher2k(uplo,trans,n,k,alpha,a,lda,b,ldb,beta,c,ldc)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    complex(sp), intent(in) :: alpha,a(lda,*),b(ldb,*)
                    real(sp), intent(in) :: beta
                    integer(ilp64), intent(in) :: k,lda,ldb,ldc,n
                    character, intent(in) :: trans,uplo
                    complex(sp), intent(inout) :: c(ldc,*)
               end subroutine cher2k
#else
               module procedure stdlib_I64_cher2k
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine zher2k(uplo,trans,n,k,alpha,a,lda,b,ldb,beta,c,ldc)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    complex(dp), intent(in) :: alpha,a(lda,*),b(ldb,*)
                    real(dp), intent(in) :: beta
                    integer(ilp64), intent(in) :: k,lda,ldb,ldc,n
                    character, intent(in) :: trans,uplo
                    complex(dp), intent(inout) :: c(ldc,*)
               end subroutine zher2k
#else
               module procedure stdlib_I64_zher2k
#endif
          end interface her2k

          interface herk
          !! HERK performs one of the hermitian rank k operations
          !! C := alpha*A*A**H + beta*C,
          !! or
          !! C := alpha*A**H*A + beta*C,
          !! where  alpha and beta  are  real scalars,  C is an  n by n  hermitian
          !! matrix and  A  is an  n by k  matrix in the  first case and a  k by n
          !! matrix in the second case.
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine cherk(uplo,trans,n,k,alpha,a,lda,beta,c,ldc)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    real(sp), intent(in) :: alpha,beta
                    integer(ilp), intent(in) :: k,lda,ldc,n
                    character, intent(in) :: trans,uplo
                    complex(sp), intent(in) :: a(lda,*)
                    complex(sp), intent(inout) :: c(ldc,*)
               end subroutine cherk
#else
               module procedure stdlib_cherk
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine zherk(uplo,trans,n,k,alpha,a,lda,beta,c,ldc)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    real(dp), intent(in) :: alpha,beta
                    integer(ilp), intent(in) :: k,lda,ldc,n
                    character, intent(in) :: trans,uplo
                    complex(dp), intent(in) :: a(lda,*)
                    complex(dp), intent(inout) :: c(ldc,*)
               end subroutine zherk
#else
               module procedure stdlib_zherk
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine cherk(uplo,trans,n,k,alpha,a,lda,beta,c,ldc)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    real(sp), intent(in) :: alpha,beta
                    integer(ilp64), intent(in) :: k,lda,ldc,n
                    character, intent(in) :: trans,uplo
                    complex(sp), intent(in) :: a(lda,*)
                    complex(sp), intent(inout) :: c(ldc,*)
               end subroutine cherk
#else
               module procedure stdlib_I64_cherk
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine zherk(uplo,trans,n,k,alpha,a,lda,beta,c,ldc)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    real(dp), intent(in) :: alpha,beta
                    integer(ilp64), intent(in) :: k,lda,ldc,n
                    character, intent(in) :: trans,uplo
                    complex(dp), intent(in) :: a(lda,*)
                    complex(dp), intent(inout) :: c(ldc,*)
               end subroutine zherk
#else
               module procedure stdlib_I64_zherk
#endif
          end interface herk

          interface hpmv
          !! HPMV performs the matrix-vector operation
          !! y := alpha*A*x + beta*y,
          !! where alpha and beta are scalars, x and y are n element vectors and
          !! A is an n by n hermitian matrix, supplied in packed form.
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine chpmv(uplo,n,alpha,ap,x,incx,beta,y,incy)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    complex(sp), intent(in) :: alpha,beta,ap(*),x(*)
                    integer(ilp), intent(in) :: incx,incy,n
                    character, intent(in) :: uplo
                    complex(sp), intent(inout) :: y(*)
               end subroutine chpmv
#else
               module procedure stdlib_chpmv
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine zhpmv(uplo,n,alpha,ap,x,incx,beta,y,incy)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    complex(dp), intent(in) :: alpha,beta,ap(*),x(*)
                    integer(ilp), intent(in) :: incx,incy,n
                    character, intent(in) :: uplo
                    complex(dp), intent(inout) :: y(*)
               end subroutine zhpmv
#else
               module procedure stdlib_zhpmv
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine chpmv(uplo,n,alpha,ap,x,incx,beta,y,incy)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    complex(sp), intent(in) :: alpha,beta,ap(*),x(*)
                    integer(ilp64), intent(in) :: incx,incy,n
                    character, intent(in) :: uplo
                    complex(sp), intent(inout) :: y(*)
               end subroutine chpmv
#else
               module procedure stdlib_I64_chpmv
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine zhpmv(uplo,n,alpha,ap,x,incx,beta,y,incy)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    complex(dp), intent(in) :: alpha,beta,ap(*),x(*)
                    integer(ilp64), intent(in) :: incx,incy,n
                    character, intent(in) :: uplo
                    complex(dp), intent(inout) :: y(*)
               end subroutine zhpmv
#else
               module procedure stdlib_I64_zhpmv
#endif
          end interface hpmv

          interface hpr
          !! HPR performs the hermitian rank 1 operation
          !! A := alpha*x*x**H + A,
          !! where alpha is a real scalar, x is an n element vector and A is an
          !! n by n hermitian matrix, supplied in packed form.
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine chpr(uplo,n,alpha,x,incx,ap)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    real(sp), intent(in) :: alpha
                    integer(ilp), intent(in) :: incx,n
                    character, intent(in) :: uplo
                    complex(sp), intent(inout) :: ap(*)
                    complex(sp), intent(in) :: x(*)
               end subroutine chpr
#else
               module procedure stdlib_chpr
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine zhpr(uplo,n,alpha,x,incx,ap)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    real(dp), intent(in) :: alpha
                    integer(ilp), intent(in) :: incx,n
                    character, intent(in) :: uplo
                    complex(dp), intent(inout) :: ap(*)
                    complex(dp), intent(in) :: x(*)
               end subroutine zhpr
#else
               module procedure stdlib_zhpr
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine chpr(uplo,n,alpha,x,incx,ap)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    real(sp), intent(in) :: alpha
                    integer(ilp64), intent(in) :: incx,n
                    character, intent(in) :: uplo
                    complex(sp), intent(inout) :: ap(*)
                    complex(sp), intent(in) :: x(*)
               end subroutine chpr
#else
               module procedure stdlib_I64_chpr
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine zhpr(uplo,n,alpha,x,incx,ap)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    real(dp), intent(in) :: alpha
                    integer(ilp64), intent(in) :: incx,n
                    character, intent(in) :: uplo
                    complex(dp), intent(inout) :: ap(*)
                    complex(dp), intent(in) :: x(*)
               end subroutine zhpr
#else
               module procedure stdlib_I64_zhpr
#endif
          end interface hpr

          interface hpr2
          !! HPR2 performs the hermitian rank 2 operation
          !! A := alpha*x*y**H + conjg( alpha )*y*x**H + A,
          !! where alpha is a scalar, x and y are n element vectors and A is an
          !! n by n hermitian matrix, supplied in packed form.
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine chpr2(uplo,n,alpha,x,incx,y,incy,ap)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    complex(sp), intent(in) :: alpha,x(*),y(*)
                    integer(ilp), intent(in) :: incx,incy,n
                    character, intent(in) :: uplo
                    complex(sp), intent(inout) :: ap(*)
               end subroutine chpr2
#else
               module procedure stdlib_chpr2
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine zhpr2(uplo,n,alpha,x,incx,y,incy,ap)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    complex(dp), intent(in) :: alpha,x(*),y(*)
                    integer(ilp), intent(in) :: incx,incy,n
                    character, intent(in) :: uplo
                    complex(dp), intent(inout) :: ap(*)
               end subroutine zhpr2
#else
               module procedure stdlib_zhpr2
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine chpr2(uplo,n,alpha,x,incx,y,incy,ap)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    complex(sp), intent(in) :: alpha,x(*),y(*)
                    integer(ilp64), intent(in) :: incx,incy,n
                    character, intent(in) :: uplo
                    complex(sp), intent(inout) :: ap(*)
               end subroutine chpr2
#else
               module procedure stdlib_I64_chpr2
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine zhpr2(uplo,n,alpha,x,incx,y,incy,ap)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    complex(dp), intent(in) :: alpha,x(*),y(*)
                    integer(ilp64), intent(in) :: incx,incy,n
                    character, intent(in) :: uplo
                    complex(dp), intent(inout) :: ap(*)
               end subroutine zhpr2
#else
               module procedure stdlib_I64_zhpr2
#endif
          end interface hpr2

          interface nrm2
          !! NRM2 returns the euclidean norm of a vector via the function
          !! name, so that
          !! NRM2 := sqrt( x'*x )
#ifdef STDLIB_EXTERNAL_BLAS
               pure real(dp) function dnrm2( n, x, incx )
                    import sp,dp,qp,ilp,lk
                    implicit none
                    integer(ilp), intent(in) :: incx,n
                    real(dp), intent(in) :: x(*)
               end function dnrm2
#else
               module procedure stdlib_dnrm2
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure real(dp) function dznrm2( n, x, incx )
                    import sp,dp,qp,ilp,lk
                    implicit none
                    integer(ilp), intent(in) :: incx,n
                    complex(dp), intent(in) :: x(*)
               end function dznrm2
#else
               module procedure stdlib_dznrm2
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure real(sp) function snrm2( n, x, incx )
                    import sp,dp,qp,ilp,lk
                    implicit none
                    integer(ilp), intent(in) :: incx,n
                    real(sp), intent(in) :: x(*)
               end function snrm2
#else
               module procedure stdlib_snrm2
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure real(sp) function scnrm2( n, x, incx )
                    import sp,dp,qp,ilp,lk
                    implicit none
                    integer(ilp), intent(in) :: incx,n
                    complex(sp), intent(in) :: x(*)
               end function scnrm2
#else
               module procedure stdlib_scnrm2
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure real(dp) function dnrm2( n, x, incx )
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    integer(ilp64), intent(in) :: incx,n
                    real(dp), intent(in) :: x(*)
               end function dnrm2
#else
               module procedure stdlib_I64_dnrm2
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure real(dp) function dznrm2( n, x, incx )
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    integer(ilp64), intent(in) :: incx,n
                    complex(dp), intent(in) :: x(*)
               end function dznrm2
#else
               module procedure stdlib_I64_dznrm2
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure real(sp) function snrm2( n, x, incx )
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    integer(ilp64), intent(in) :: incx,n
                    real(sp), intent(in) :: x(*)
               end function snrm2
#else
               module procedure stdlib_I64_snrm2
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure real(sp) function scnrm2( n, x, incx )
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    integer(ilp64), intent(in) :: incx,n
                    complex(sp), intent(in) :: x(*)
               end function scnrm2
#else
               module procedure stdlib_I64_scnrm2
#endif
          end interface nrm2

          interface rot
          !! ROT applies a plane rotation.
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine drot(n,dx,incx,dy,incy,c,s)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    real(dp), intent(in) :: c,s
                    integer(ilp), intent(in) :: incx,incy,n
                    real(dp), intent(inout) :: dx(*),dy(*)
               end subroutine drot
#else
               module procedure stdlib_drot
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine srot(n,sx,incx,sy,incy,c,s)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    real(sp), intent(in) :: c,s
                    integer(ilp), intent(in) :: incx,incy,n
                    real(sp), intent(inout) :: sx(*),sy(*)
               end subroutine srot
#else
               module procedure stdlib_srot
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine drot(n,dx,incx,dy,incy,c,s)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    real(dp), intent(in) :: c,s
                    integer(ilp64), intent(in) :: incx,incy,n
                    real(dp), intent(inout) :: dx(*),dy(*)
               end subroutine drot
#else
               module procedure stdlib_I64_drot
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine srot(n,sx,incx,sy,incy,c,s)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    real(sp), intent(in) :: c,s
                    integer(ilp64), intent(in) :: incx,incy,n
                    real(sp), intent(inout) :: sx(*),sy(*)
               end subroutine srot
#else
               module procedure stdlib_I64_srot
#endif
          end interface rot

          interface rotg
          !! The computation uses the formulas
          !! |x| = sqrt( Re(x)**2 + Im(x)**2 )
          !! sgn(x) = x / |x|  if x /= 0
          !! = 1        if x  = 0
          !! c = |a| / sqrt(|a|**2 + |b|**2)
          !! s = sgn(a) * conjg(b) / sqrt(|a|**2 + |b|**2)
          !! When a and b are real and r /= 0, the formulas simplify to
          !! r = sgn(a)*sqrt(|a|**2 + |b|**2)
          !! c = a / r
          !! s = b / r
          !! the same as in SROTG when |a| > |b|.  When |b| >= |a|, the
          !! sign of c and s will be different from those computed by SROTG
          !! if the signs of a and b are not the same.
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine crotg( a, b, c, s )
                    import sp,dp,qp,ilp,lk
                    implicit none
                    real(sp), intent(out) :: c
                    complex(sp), intent(inout) :: a
                    complex(sp), intent(in) :: b
                    complex(sp), intent(out) :: s
               end subroutine crotg
#else
               module procedure stdlib_crotg
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine drotg( a, b, c, s )
                    import sp,dp,qp,ilp,lk
                    implicit none
                    real(dp), intent(inout) :: a,b
                    real(dp), intent(out) :: c,s
               end subroutine drotg
#else
               module procedure stdlib_drotg
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine srotg( a, b, c, s )
                    import sp,dp,qp,ilp,lk
                    implicit none
                    real(sp), intent(inout) :: a,b
                    real(sp), intent(out) :: c,s
               end subroutine srotg
#else
               module procedure stdlib_srotg
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine zrotg( a, b, c, s )
                    import sp,dp,qp,ilp,lk
                    implicit none
                    real(dp), intent(out) :: c
                    complex(dp), intent(inout) :: a
                    complex(dp), intent(in) :: b
                    complex(dp), intent(out) :: s
               end subroutine zrotg
#else
               module procedure stdlib_zrotg
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine crotg( a, b, c, s )
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    real(sp), intent(out) :: c
                    complex(sp), intent(inout) :: a
                    complex(sp), intent(in) :: b
                    complex(sp), intent(out) :: s
               end subroutine crotg
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine drotg( a, b, c, s )
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    real(dp), intent(inout) :: a,b
                    real(dp), intent(out) :: c,s
               end subroutine drotg
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine srotg( a, b, c, s )
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    real(sp), intent(inout) :: a,b
                    real(sp), intent(out) :: c,s
               end subroutine srotg
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine zrotg( a, b, c, s )
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    real(dp), intent(out) :: c
                    complex(dp), intent(inout) :: a
                    complex(dp), intent(in) :: b
                    complex(dp), intent(out) :: s
               end subroutine zrotg
#endif
          end interface rotg

          interface rotm
          !! ROTM applies the modified Givens transformation, \(H\), to the 2-by-N matrix
          !! $$ \left[ \begin{array}{c}DX^T\\DY^T\\ \end{array} \right], $$
          !! where \(^T\) indicates transpose. The elements of \(DX\) are in
          !! DX(LX+I*INCX), I = 0:N-1, where LX = 1 if INCX >= 0, else LX = (-INCX)*N,
          !! and similarly for DY using LY and INCY.
          !! With DPARAM(1)=DFLAG, \(H\) has one of the following forms:
          !! $$ H=\underbrace{\begin{bmatrix}DH_{11} & DH_{12}\\DH_{21} & DH_{22}\end{bmatrix}}_{DFLAG=-1},
          !!      \underbrace{\begin{bmatrix}1 & DH_{12}\\DH_{21} & 1\end{bmatrix}}_{DFLAG=0},
          !!      \underbrace{\begin{bmatrix}DH_{11} & 1\\-1 & DH_{22}\end{bmatrix}}_{DFLAG=1},
          !!      \underbrace{\begin{bmatrix}1 & 0\\0 & 1\end{bmatrix}}_{DFLAG=-2}. $$
          !! See ROTMG for a description of data storage in DPARAM.
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine drotm(n,dx,incx,dy,incy,dparam)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    integer(ilp), intent(in) :: incx,incy,n
                    real(dp), intent(in) :: dparam(5)
                    real(dp), intent(inout) :: dx(*),dy(*)
               end subroutine drotm
#else
               module procedure stdlib_drotm
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine srotm(n,sx,incx,sy,incy,sparam)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    integer(ilp), intent(in) :: incx,incy,n
                    real(sp), intent(in) :: sparam(5)
                    real(sp), intent(inout) :: sx(*),sy(*)
               end subroutine srotm
#else
               module procedure stdlib_srotm
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine drotm(n,dx,incx,dy,incy,dparam)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    integer(ilp64), intent(in) :: incx,incy,n
                    real(dp), intent(in) :: dparam(5)
                    real(dp), intent(inout) :: dx(*),dy(*)
               end subroutine drotm
#else
               module procedure stdlib_I64_drotm
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine srotm(n,sx,incx,sy,incy,sparam)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    integer(ilp64), intent(in) :: incx,incy,n
                    real(sp), intent(in) :: sparam(5)
                    real(sp), intent(inout) :: sx(*),sy(*)
               end subroutine srotm
#else
               module procedure stdlib_I64_srotm
#endif
          end interface rotm

          interface rotmg
          !! ROTMG Constructs the modified Givens transformation matrix \(H\) which zeros the
          !! second component of the 2-vector
          !! $$ \left[ {\sqrt{DD_1}\cdot DX_1,\sqrt{DD_2}\cdot DY_2} \right]^T. $$
          !! With DPARAM(1)=DFLAG, \(H\) has one of the following forms:
          !! $$ H=\underbrace{\begin{bmatrix}DH_{11} & DH_{12}\\DH_{21} & DH_{22}\end{bmatrix}}_{DFLAG=-1},
          !!      \underbrace{\begin{bmatrix}1 & DH_{12}\\DH_{21} & 1\end{bmatrix}}_{DFLAG=0},
          !!      \underbrace{\begin{bmatrix}DH_{11} & 1\\-1 & DH_{22}\end{bmatrix}}_{DFLAG=1},
          !!      \underbrace{\begin{bmatrix}1 & 0\\0 & 1\end{bmatrix}}_{DFLAG=-2}. $$
          !! Locations 2-4 of DPARAM contain DH11, DH21, DH12 and DH22 respectively.
          !! (Values of 1.0, -1.0, or 0.0 implied by the value of DPARAM(1) are not stored in DPARAM.)
          !! The values of parameters GAMSQ and RGAMSQ may be inexact. This is OK as they are only
          !! used for testing the size of DD1 and DD2. All actual scaling of data is done using GAM.
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine drotmg(dd1,dd2,dx1,dy1,dparam)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    real(dp), intent(inout) :: dd1,dd2,dx1
                    real(dp), intent(in) :: dy1
                    real(dp), intent(out) :: dparam(5)
               end subroutine drotmg
#else
               module procedure stdlib_drotmg
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine srotmg(sd1,sd2,sx1,sy1,sparam)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    real(sp), intent(inout) :: sd1,sd2,sx1
                    real(sp), intent(in) :: sy1
                    real(sp), intent(out) :: sparam(5)
               end subroutine srotmg
#else
               module procedure stdlib_srotmg
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine drotmg(dd1,dd2,dx1,dy1,dparam)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    real(dp), intent(inout) :: dd1,dd2,dx1
                    real(dp), intent(in) :: dy1
                    real(dp), intent(out) :: dparam(5)
               end subroutine drotmg
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine srotmg(sd1,sd2,sx1,sy1,sparam)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    real(sp), intent(inout) :: sd1,sd2,sx1
                    real(sp), intent(in) :: sy1
                    real(sp), intent(out) :: sparam(5)
               end subroutine srotmg
#endif
          end interface rotmg

          interface sbmv
          !! SBMV performs the matrix-vector  operation
          !! y := alpha*A*x + beta*y,
          !! where alpha and beta are scalars, x and y are n element vectors and
          !! A is an n by n symmetric band matrix, with k super-diagonals.
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine dsbmv(uplo,n,k,alpha,a,lda,x,incx,beta,y,incy)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    real(dp), intent(in) :: alpha,beta,a(lda,*),x(*)
                    integer(ilp), intent(in) :: incx,incy,k,lda,n
                    character, intent(in) :: uplo
                    real(dp), intent(inout) :: y(*)
               end subroutine dsbmv
#else
               module procedure stdlib_dsbmv
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine ssbmv(uplo,n,k,alpha,a,lda,x,incx,beta,y,incy)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    real(sp), intent(in) :: alpha,beta,a(lda,*),x(*)
                    integer(ilp), intent(in) :: incx,incy,k,lda,n
                    character, intent(in) :: uplo
                    real(sp), intent(inout) :: y(*)
               end subroutine ssbmv
#else
               module procedure stdlib_ssbmv
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine dsbmv(uplo,n,k,alpha,a,lda,x,incx,beta,y,incy)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    real(dp), intent(in) :: alpha,beta,a(lda,*),x(*)
                    integer(ilp64), intent(in) :: incx,incy,k,lda,n
                    character, intent(in) :: uplo
                    real(dp), intent(inout) :: y(*)
               end subroutine dsbmv
#else
               module procedure stdlib_I64_dsbmv
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine ssbmv(uplo,n,k,alpha,a,lda,x,incx,beta,y,incy)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    real(sp), intent(in) :: alpha,beta,a(lda,*),x(*)
                    integer(ilp64), intent(in) :: incx,incy,k,lda,n
                    character, intent(in) :: uplo
                    real(sp), intent(inout) :: y(*)
               end subroutine ssbmv
#else
               module procedure stdlib_I64_ssbmv
#endif
          end interface sbmv

          interface scal
          !! SCAL scales a vector by a constant.
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine cscal(n,ca,cx,incx)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    complex(sp), intent(in) :: ca
                    integer(ilp), intent(in) :: incx,n
                    complex(sp), intent(inout) :: cx(*)
               end subroutine cscal
#else
               module procedure stdlib_cscal
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine dscal(n,da,dx,incx)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    real(dp), intent(in) :: da
                    integer(ilp), intent(in) :: incx,n
                    real(dp), intent(inout) :: dx(*)
               end subroutine dscal
#else
               module procedure stdlib_dscal
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine sscal(n,sa,sx,incx)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    real(sp), intent(in) :: sa
                    integer(ilp), intent(in) :: incx,n
                    real(sp), intent(inout) :: sx(*)
               end subroutine sscal
#else
               module procedure stdlib_sscal
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine zscal(n,za,zx,incx)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    complex(dp), intent(in) :: za
                    integer(ilp), intent(in) :: incx,n
                    complex(dp), intent(inout) :: zx(*)
               end subroutine zscal
#else
               module procedure stdlib_zscal
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine cscal(n,ca,cx,incx)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    complex(sp), intent(in) :: ca
                    integer(ilp64), intent(in) :: incx,n
                    complex(sp), intent(inout) :: cx(*)
               end subroutine cscal
#else
               module procedure stdlib_I64_cscal
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine dscal(n,da,dx,incx)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    real(dp), intent(in) :: da
                    integer(ilp64), intent(in) :: incx,n
                    real(dp), intent(inout) :: dx(*)
               end subroutine dscal
#else
               module procedure stdlib_I64_dscal
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine sscal(n,sa,sx,incx)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    real(sp), intent(in) :: sa
                    integer(ilp64), intent(in) :: incx,n
                    real(sp), intent(inout) :: sx(*)
               end subroutine sscal
#else
               module procedure stdlib_I64_sscal
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine zscal(n,za,zx,incx)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    complex(dp), intent(in) :: za
                    integer(ilp64), intent(in) :: incx,n
                    complex(dp), intent(inout) :: zx(*)
               end subroutine zscal
#else
               module procedure stdlib_I64_zscal
#endif
          end interface scal

          interface sdot
          !! Compute the inner product of two vectors with extended
          !! precision accumulation and result.
          !! Returns D.P. dot product accumulated in D.P., for S.P. SX and SY
          !! SDOT = sum for I = 0 to N-1 of  SX(LX+I*INCX) * SY(LY+I*INCY),
          !! where LX = 1 if INCX >= 0, else LX = 1+(1-N)*INCX, and LY is
          !! defined in a similar way using INCY.
#ifdef STDLIB_EXTERNAL_BLAS
               pure real(dp) function dsdot(n,sx,incx,sy,incy)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    integer(ilp), intent(in) :: incx,incy,n
                    real(sp), intent(in) :: sx(*),sy(*)
               end function dsdot
#else
               module procedure stdlib_dsdot
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure real(dp) function dsdot(n,sx,incx,sy,incy)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    integer(ilp64), intent(in) :: incx,incy,n
                    real(sp), intent(in) :: sx(*),sy(*)
               end function dsdot
#else
               module procedure stdlib_I64_dsdot
#endif
          end interface sdot

          interface spmv
          !! SPMV performs the matrix-vector operation
          !! y := alpha*A*x + beta*y,
          !! where alpha and beta are scalars, x and y are n element vectors and
          !! A is an n by n symmetric matrix, supplied in packed form.
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine dspmv(uplo,n,alpha,ap,x,incx,beta,y,incy)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    real(dp), intent(in) :: alpha,beta,ap(*),x(*)
                    integer(ilp), intent(in) :: incx,incy,n
                    character, intent(in) :: uplo
                    real(dp), intent(inout) :: y(*)
               end subroutine dspmv
#else
               module procedure stdlib_dspmv
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine sspmv(uplo,n,alpha,ap,x,incx,beta,y,incy)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    real(sp), intent(in) :: alpha,beta,ap(*),x(*)
                    integer(ilp), intent(in) :: incx,incy,n
                    character, intent(in) :: uplo
                    real(sp), intent(inout) :: y(*)
               end subroutine sspmv
#else
               module procedure stdlib_sspmv
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine dspmv(uplo,n,alpha,ap,x,incx,beta,y,incy)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    real(dp), intent(in) :: alpha,beta,ap(*),x(*)
                    integer(ilp64), intent(in) :: incx,incy,n
                    character, intent(in) :: uplo
                    real(dp), intent(inout) :: y(*)
               end subroutine dspmv
#else
               module procedure stdlib_I64_dspmv
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine sspmv(uplo,n,alpha,ap,x,incx,beta,y,incy)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    real(sp), intent(in) :: alpha,beta,ap(*),x(*)
                    integer(ilp64), intent(in) :: incx,incy,n
                    character, intent(in) :: uplo
                    real(sp), intent(inout) :: y(*)
               end subroutine sspmv
#else
               module procedure stdlib_I64_sspmv
#endif
          end interface spmv

          interface spr
          !! SPR performs the symmetric rank 1 operation
          !! A := alpha*x*x**T + A,
          !! where alpha is a real scalar, x is an n element vector and A is an
          !! n by n symmetric matrix, supplied in packed form.
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine dspr(uplo,n,alpha,x,incx,ap)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    real(dp), intent(in) :: alpha,x(*)
                    integer(ilp), intent(in) :: incx,n
                    character, intent(in) :: uplo
                    real(dp), intent(inout) :: ap(*)
               end subroutine dspr
#else
               module procedure stdlib_dspr
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine sspr(uplo,n,alpha,x,incx,ap)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    real(sp), intent(in) :: alpha,x(*)
                    integer(ilp), intent(in) :: incx,n
                    character, intent(in) :: uplo
                    real(sp), intent(inout) :: ap(*)
               end subroutine sspr
#else
               module procedure stdlib_sspr
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine dspr(uplo,n,alpha,x,incx,ap)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    real(dp), intent(in) :: alpha,x(*)
                    integer(ilp64), intent(in) :: incx,n
                    character, intent(in) :: uplo
                    real(dp), intent(inout) :: ap(*)
               end subroutine dspr
#else
               module procedure stdlib_I64_dspr
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine sspr(uplo,n,alpha,x,incx,ap)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    real(sp), intent(in) :: alpha,x(*)
                    integer(ilp64), intent(in) :: incx,n
                    character, intent(in) :: uplo
                    real(sp), intent(inout) :: ap(*)
               end subroutine sspr
#else
               module procedure stdlib_I64_sspr
#endif
          end interface spr

          interface spr2
          !! SPR2 performs the symmetric rank 2 operation
          !! A := alpha*x*y**T + alpha*y*x**T + A,
          !! where alpha is a scalar, x and y are n element vectors and A is an
          !! n by n symmetric matrix, supplied in packed form.
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine dspr2(uplo,n,alpha,x,incx,y,incy,ap)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    real(dp), intent(in) :: alpha,x(*),y(*)
                    integer(ilp), intent(in) :: incx,incy,n
                    character, intent(in) :: uplo
                    real(dp), intent(inout) :: ap(*)
               end subroutine dspr2
#else
               module procedure stdlib_dspr2
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine sspr2(uplo,n,alpha,x,incx,y,incy,ap)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    real(sp), intent(in) :: alpha,x(*),y(*)
                    integer(ilp), intent(in) :: incx,incy,n
                    character, intent(in) :: uplo
                    real(sp), intent(inout) :: ap(*)
               end subroutine sspr2
#else
               module procedure stdlib_sspr2
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine dspr2(uplo,n,alpha,x,incx,y,incy,ap)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    real(dp), intent(in) :: alpha,x(*),y(*)
                    integer(ilp64), intent(in) :: incx,incy,n
                    character, intent(in) :: uplo
                    real(dp), intent(inout) :: ap(*)
               end subroutine dspr2
#else
               module procedure stdlib_I64_dspr2
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine sspr2(uplo,n,alpha,x,incx,y,incy,ap)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    real(sp), intent(in) :: alpha,x(*),y(*)
                    integer(ilp64), intent(in) :: incx,incy,n
                    character, intent(in) :: uplo
                    real(sp), intent(inout) :: ap(*)
               end subroutine sspr2
#else
               module procedure stdlib_I64_sspr2
#endif
          end interface spr2

          interface srot
          !! SROT applies a plane rotation, where the cos and sin (c and s) are real
          !! and the vectors cx and cy are complex.
          !! jack dongarra, linpack, 3/11/78.
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine csrot( n, cx, incx, cy, incy, c, s )
                    import sp,dp,qp,ilp,lk
                    implicit none
                    integer(ilp), intent(in) :: incx,incy,n
                    real(sp), intent(in) :: c,s
                    complex(sp), intent(inout) :: cx(*),cy(*)
               end subroutine csrot
#else
               module procedure stdlib_csrot
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine csrot( n, cx, incx, cy, incy, c, s )
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    integer(ilp64), intent(in) :: incx,incy,n
                    real(sp), intent(in) :: c,s
                    complex(sp), intent(inout) :: cx(*),cy(*)
               end subroutine csrot
#else
               module procedure stdlib_I64_csrot
#endif
          end interface srot

          interface sscal
          !! SSCAL scales a complex vector by a real constant.
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine csscal(n,sa,cx,incx)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    real(sp), intent(in) :: sa
                    integer(ilp), intent(in) :: incx,n
                    complex(sp), intent(inout) :: cx(*)
               end subroutine csscal
#else
               module procedure stdlib_csscal
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine csscal(n,sa,cx,incx)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    real(sp), intent(in) :: sa
                    integer(ilp64), intent(in) :: incx,n
                    complex(sp), intent(inout) :: cx(*)
               end subroutine csscal
#else
               module procedure stdlib_I64_csscal
#endif
          end interface sscal

          interface swap
          !! SWAP interchanges two vectors.
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine cswap(n,cx,incx,cy,incy)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    integer(ilp), intent(in) :: incx,incy,n
                    complex(sp), intent(inout) :: cx(*),cy(*)
               end subroutine cswap
#else
               module procedure stdlib_cswap
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine dswap(n,dx,incx,dy,incy)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    integer(ilp), intent(in) :: incx,incy,n
                    real(dp), intent(inout) :: dx(*),dy(*)
               end subroutine dswap
#else
               module procedure stdlib_dswap
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine sswap(n,sx,incx,sy,incy)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    integer(ilp), intent(in) :: incx,incy,n
                    real(sp), intent(inout) :: sx(*),sy(*)
               end subroutine sswap
#else
               module procedure stdlib_sswap
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine zswap(n,zx,incx,zy,incy)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    integer(ilp), intent(in) :: incx,incy,n
                    complex(dp), intent(inout) :: zx(*),zy(*)
               end subroutine zswap
#else
               module procedure stdlib_zswap
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine cswap(n,cx,incx,cy,incy)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    integer(ilp64), intent(in) :: incx,incy,n
                    complex(sp), intent(inout) :: cx(*),cy(*)
               end subroutine cswap
#else
               module procedure stdlib_I64_cswap
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine dswap(n,dx,incx,dy,incy)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    integer(ilp64), intent(in) :: incx,incy,n
                    real(dp), intent(inout) :: dx(*),dy(*)
               end subroutine dswap
#else
               module procedure stdlib_I64_dswap
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine sswap(n,sx,incx,sy,incy)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    integer(ilp64), intent(in) :: incx,incy,n
                    real(sp), intent(inout) :: sx(*),sy(*)
               end subroutine sswap
#else
               module procedure stdlib_I64_sswap
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine zswap(n,zx,incx,zy,incy)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    integer(ilp64), intent(in) :: incx,incy,n
                    complex(dp), intent(inout) :: zx(*),zy(*)
               end subroutine zswap
#else
               module procedure stdlib_I64_zswap
#endif
          end interface swap

          interface symm
          !! SYMM performs one of the matrix-matrix operations
          !! C := alpha*A*B + beta*C,
          !! or
          !! C := alpha*B*A + beta*C,
          !! where  alpha and beta are scalars, A is a symmetric matrix and  B and
          !! C are m by n matrices.
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine csymm(side,uplo,m,n,alpha,a,lda,b,ldb,beta,c,ldc)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    complex(sp), intent(in) :: alpha,beta,a(lda,*),b(ldb,*)
                    integer(ilp), intent(in) :: lda,ldb,ldc,m,n
                    character, intent(in) :: side,uplo
                    complex(sp), intent(inout) :: c(ldc,*)
               end subroutine csymm
#else
               module procedure stdlib_csymm
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine dsymm(side,uplo,m,n,alpha,a,lda,b,ldb,beta,c,ldc)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    real(dp), intent(in) :: alpha,beta,a(lda,*),b(ldb,*)
                    integer(ilp), intent(in) :: lda,ldb,ldc,m,n
                    character, intent(in) :: side,uplo
                    real(dp), intent(inout) :: c(ldc,*)
               end subroutine dsymm
#else
               module procedure stdlib_dsymm
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine ssymm(side,uplo,m,n,alpha,a,lda,b,ldb,beta,c,ldc)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    real(sp), intent(in) :: alpha,beta,a(lda,*),b(ldb,*)
                    integer(ilp), intent(in) :: lda,ldb,ldc,m,n
                    character, intent(in) :: side,uplo
                    real(sp), intent(inout) :: c(ldc,*)
               end subroutine ssymm
#else
               module procedure stdlib_ssymm
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine zsymm(side,uplo,m,n,alpha,a,lda,b,ldb,beta,c,ldc)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    complex(dp), intent(in) :: alpha,beta,a(lda,*),b(ldb,*)
                    integer(ilp), intent(in) :: lda,ldb,ldc,m,n
                    character, intent(in) :: side,uplo
                    complex(dp), intent(inout) :: c(ldc,*)
               end subroutine zsymm
#else
               module procedure stdlib_zsymm
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine csymm(side,uplo,m,n,alpha,a,lda,b,ldb,beta,c,ldc)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    complex(sp), intent(in) :: alpha,beta,a(lda,*),b(ldb,*)
                    integer(ilp64), intent(in) :: lda,ldb,ldc,m,n
                    character, intent(in) :: side,uplo
                    complex(sp), intent(inout) :: c(ldc,*)
               end subroutine csymm
#else
               module procedure stdlib_I64_csymm
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine dsymm(side,uplo,m,n,alpha,a,lda,b,ldb,beta,c,ldc)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    real(dp), intent(in) :: alpha,beta,a(lda,*),b(ldb,*)
                    integer(ilp64), intent(in) :: lda,ldb,ldc,m,n
                    character, intent(in) :: side,uplo
                    real(dp), intent(inout) :: c(ldc,*)
               end subroutine dsymm
#else
               module procedure stdlib_I64_dsymm
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine ssymm(side,uplo,m,n,alpha,a,lda,b,ldb,beta,c,ldc)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    real(sp), intent(in) :: alpha,beta,a(lda,*),b(ldb,*)
                    integer(ilp64), intent(in) :: lda,ldb,ldc,m,n
                    character, intent(in) :: side,uplo
                    real(sp), intent(inout) :: c(ldc,*)
               end subroutine ssymm
#else
               module procedure stdlib_I64_ssymm
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine zsymm(side,uplo,m,n,alpha,a,lda,b,ldb,beta,c,ldc)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    complex(dp), intent(in) :: alpha,beta,a(lda,*),b(ldb,*)
                    integer(ilp64), intent(in) :: lda,ldb,ldc,m,n
                    character, intent(in) :: side,uplo
                    complex(dp), intent(inout) :: c(ldc,*)
               end subroutine zsymm
#else
               module procedure stdlib_I64_zsymm
#endif
          end interface symm

          interface symv
          !! SYMV performs the matrix-vector  operation
          !! y := alpha*A*x + beta*y,
          !! where alpha and beta are scalars, x and y are n element vectors and
          !! A is an n by n symmetric matrix.
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine dsymv(uplo,n,alpha,a,lda,x,incx,beta,y,incy)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    real(dp), intent(in) :: alpha,beta,a(lda,*),x(*)
                    integer(ilp), intent(in) :: incx,incy,lda,n
                    character, intent(in) :: uplo
                    real(dp), intent(inout) :: y(*)
               end subroutine dsymv
#else
               module procedure stdlib_dsymv
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine ssymv(uplo,n,alpha,a,lda,x,incx,beta,y,incy)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    real(sp), intent(in) :: alpha,beta,a(lda,*),x(*)
                    integer(ilp), intent(in) :: incx,incy,lda,n
                    character, intent(in) :: uplo
                    real(sp), intent(inout) :: y(*)
               end subroutine ssymv
#else
               module procedure stdlib_ssymv
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine dsymv(uplo,n,alpha,a,lda,x,incx,beta,y,incy)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    real(dp), intent(in) :: alpha,beta,a(lda,*),x(*)
                    integer(ilp64), intent(in) :: incx,incy,lda,n
                    character, intent(in) :: uplo
                    real(dp), intent(inout) :: y(*)
               end subroutine dsymv
#else
               module procedure stdlib_I64_dsymv
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine ssymv(uplo,n,alpha,a,lda,x,incx,beta,y,incy)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    real(sp), intent(in) :: alpha,beta,a(lda,*),x(*)
                    integer(ilp64), intent(in) :: incx,incy,lda,n
                    character, intent(in) :: uplo
                    real(sp), intent(inout) :: y(*)
               end subroutine ssymv
#else
               module procedure stdlib_I64_ssymv
#endif
          end interface symv

          interface syr
          !! SYR performs the symmetric rank 1 operation
          !! A := alpha*x*x**T + A,
          !! where alpha is a real scalar, x is an n element vector and A is an
          !! n by n symmetric matrix.
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine dsyr(uplo,n,alpha,x,incx,a,lda)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    real(dp), intent(in) :: alpha,x(*)
                    integer(ilp), intent(in) :: incx,lda,n
                    character, intent(in) :: uplo
                    real(dp), intent(inout) :: a(lda,*)
               end subroutine dsyr
#else
               module procedure stdlib_dsyr
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine ssyr(uplo,n,alpha,x,incx,a,lda)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    real(sp), intent(in) :: alpha,x(*)
                    integer(ilp), intent(in) :: incx,lda,n
                    character, intent(in) :: uplo
                    real(sp), intent(inout) :: a(lda,*)
               end subroutine ssyr
#else
               module procedure stdlib_ssyr
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine dsyr(uplo,n,alpha,x,incx,a,lda)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    real(dp), intent(in) :: alpha,x(*)
                    integer(ilp64), intent(in) :: incx,lda,n
                    character, intent(in) :: uplo
                    real(dp), intent(inout) :: a(lda,*)
               end subroutine dsyr
#else
               module procedure stdlib_I64_dsyr
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine ssyr(uplo,n,alpha,x,incx,a,lda)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    real(sp), intent(in) :: alpha,x(*)
                    integer(ilp64), intent(in) :: incx,lda,n
                    character, intent(in) :: uplo
                    real(sp), intent(inout) :: a(lda,*)
               end subroutine ssyr
#else
               module procedure stdlib_I64_ssyr
#endif
          end interface syr

          interface syr2
          !! SYR2 performs the symmetric rank 2 operation
          !! A := alpha*x*y**T + alpha*y*x**T + A,
          !! where alpha is a scalar, x and y are n element vectors and A is an n
          !! by n symmetric matrix.
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine dsyr2(uplo,n,alpha,x,incx,y,incy,a,lda)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    real(dp), intent(in) :: alpha,x(*),y(*)
                    integer(ilp), intent(in) :: incx,incy,lda,n
                    character, intent(in) :: uplo
                    real(dp), intent(inout) :: a(lda,*)
               end subroutine dsyr2
#else
               module procedure stdlib_dsyr2
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine ssyr2(uplo,n,alpha,x,incx,y,incy,a,lda)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    real(sp), intent(in) :: alpha,x(*),y(*)
                    integer(ilp), intent(in) :: incx,incy,lda,n
                    character, intent(in) :: uplo
                    real(sp), intent(inout) :: a(lda,*)
               end subroutine ssyr2
#else
               module procedure stdlib_ssyr2
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine dsyr2(uplo,n,alpha,x,incx,y,incy,a,lda)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    real(dp), intent(in) :: alpha,x(*),y(*)
                    integer(ilp64), intent(in) :: incx,incy,lda,n
                    character, intent(in) :: uplo
                    real(dp), intent(inout) :: a(lda,*)
               end subroutine dsyr2
#else
               module procedure stdlib_I64_dsyr2
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine ssyr2(uplo,n,alpha,x,incx,y,incy,a,lda)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    real(sp), intent(in) :: alpha,x(*),y(*)
                    integer(ilp64), intent(in) :: incx,incy,lda,n
                    character, intent(in) :: uplo
                    real(sp), intent(inout) :: a(lda,*)
               end subroutine ssyr2
#else
               module procedure stdlib_I64_ssyr2
#endif
          end interface syr2

          interface syr2k
          !! SYR2K performs one of the symmetric rank 2k operations
          !! C := alpha*A*B**T + alpha*B*A**T + beta*C,
          !! or
          !! C := alpha*A**T*B + alpha*B**T*A + beta*C,
          !! where  alpha and beta  are scalars,  C is an  n by n symmetric matrix
          !! and  A and B  are  n by k  matrices  in the  first  case  and  k by n
          !! matrices in the second case.
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine csyr2k(uplo,trans,n,k,alpha,a,lda,b,ldb,beta,c,ldc)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    complex(sp), intent(in) :: alpha,beta,a(lda,*),b(ldb,*)
                    integer(ilp), intent(in) :: k,lda,ldb,ldc,n
                    character, intent(in) :: trans,uplo
                    complex(sp), intent(inout) :: c(ldc,*)
               end subroutine csyr2k
#else
               module procedure stdlib_csyr2k
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine dsyr2k(uplo,trans,n,k,alpha,a,lda,b,ldb,beta,c,ldc)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    real(dp), intent(in) :: alpha,beta,a(lda,*),b(ldb,*)
                    integer(ilp), intent(in) :: k,lda,ldb,ldc,n
                    character, intent(in) :: trans,uplo
                    real(dp), intent(inout) :: c(ldc,*)
               end subroutine dsyr2k
#else
               module procedure stdlib_dsyr2k
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine ssyr2k(uplo,trans,n,k,alpha,a,lda,b,ldb,beta,c,ldc)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    real(sp), intent(in) :: alpha,beta,a(lda,*),b(ldb,*)
                    integer(ilp), intent(in) :: k,lda,ldb,ldc,n
                    character, intent(in) :: trans,uplo
                    real(sp), intent(inout) :: c(ldc,*)
               end subroutine ssyr2k
#else
               module procedure stdlib_ssyr2k
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine zsyr2k(uplo,trans,n,k,alpha,a,lda,b,ldb,beta,c,ldc)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    complex(dp), intent(in) :: alpha,beta,a(lda,*),b(ldb,*)
                    integer(ilp), intent(in) :: k,lda,ldb,ldc,n
                    character, intent(in) :: trans,uplo
                    complex(dp), intent(inout) :: c(ldc,*)
               end subroutine zsyr2k
#else
               module procedure stdlib_zsyr2k
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine csyr2k(uplo,trans,n,k,alpha,a,lda,b,ldb,beta,c,ldc)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    complex(sp), intent(in) :: alpha,beta,a(lda,*),b(ldb,*)
                    integer(ilp64), intent(in) :: k,lda,ldb,ldc,n
                    character, intent(in) :: trans,uplo
                    complex(sp), intent(inout) :: c(ldc,*)
               end subroutine csyr2k
#else
               module procedure stdlib_I64_csyr2k
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine dsyr2k(uplo,trans,n,k,alpha,a,lda,b,ldb,beta,c,ldc)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    real(dp), intent(in) :: alpha,beta,a(lda,*),b(ldb,*)
                    integer(ilp64), intent(in) :: k,lda,ldb,ldc,n
                    character, intent(in) :: trans,uplo
                    real(dp), intent(inout) :: c(ldc,*)
               end subroutine dsyr2k
#else
               module procedure stdlib_I64_dsyr2k
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine ssyr2k(uplo,trans,n,k,alpha,a,lda,b,ldb,beta,c,ldc)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    real(sp), intent(in) :: alpha,beta,a(lda,*),b(ldb,*)
                    integer(ilp64), intent(in) :: k,lda,ldb,ldc,n
                    character, intent(in) :: trans,uplo
                    real(sp), intent(inout) :: c(ldc,*)
               end subroutine ssyr2k
#else
               module procedure stdlib_I64_ssyr2k
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine zsyr2k(uplo,trans,n,k,alpha,a,lda,b,ldb,beta,c,ldc)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    complex(dp), intent(in) :: alpha,beta,a(lda,*),b(ldb,*)
                    integer(ilp64), intent(in) :: k,lda,ldb,ldc,n
                    character, intent(in) :: trans,uplo
                    complex(dp), intent(inout) :: c(ldc,*)
               end subroutine zsyr2k
#else
               module procedure stdlib_I64_zsyr2k
#endif
          end interface syr2k

          interface syrk
          !! SYRK performs one of the symmetric rank k operations
          !! C := alpha*A*A**T + beta*C,
          !! or
          !! C := alpha*A**T*A + beta*C,
          !! where  alpha and beta  are scalars,  C is an  n by n symmetric matrix
          !! and  A  is an  n by k  matrix in the first case and a  k by n  matrix
          !! in the second case.
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine csyrk(uplo,trans,n,k,alpha,a,lda,beta,c,ldc)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    complex(sp), intent(in) :: alpha,beta,a(lda,*)
                    integer(ilp), intent(in) :: k,lda,ldc,n
                    character, intent(in) :: trans,uplo
                    complex(sp), intent(inout) :: c(ldc,*)
               end subroutine csyrk
#else
               module procedure stdlib_csyrk
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine dsyrk(uplo,trans,n,k,alpha,a,lda,beta,c,ldc)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    real(dp), intent(in) :: alpha,beta,a(lda,*)
                    integer(ilp), intent(in) :: k,lda,ldc,n
                    character, intent(in) :: trans,uplo
                    real(dp), intent(inout) :: c(ldc,*)
               end subroutine dsyrk
#else
               module procedure stdlib_dsyrk
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine ssyrk(uplo,trans,n,k,alpha,a,lda,beta,c,ldc)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    real(sp), intent(in) :: alpha,beta,a(lda,*)
                    integer(ilp), intent(in) :: k,lda,ldc,n
                    character, intent(in) :: trans,uplo
                    real(sp), intent(inout) :: c(ldc,*)
               end subroutine ssyrk
#else
               module procedure stdlib_ssyrk
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine zsyrk(uplo,trans,n,k,alpha,a,lda,beta,c,ldc)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    complex(dp), intent(in) :: alpha,beta,a(lda,*)
                    integer(ilp), intent(in) :: k,lda,ldc,n
                    character, intent(in) :: trans,uplo
                    complex(dp), intent(inout) :: c(ldc,*)
               end subroutine zsyrk
#else
               module procedure stdlib_zsyrk
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine csyrk(uplo,trans,n,k,alpha,a,lda,beta,c,ldc)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    complex(sp), intent(in) :: alpha,beta,a(lda,*)
                    integer(ilp64), intent(in) :: k,lda,ldc,n
                    character, intent(in) :: trans,uplo
                    complex(sp), intent(inout) :: c(ldc,*)
               end subroutine csyrk
#else
               module procedure stdlib_I64_csyrk
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine dsyrk(uplo,trans,n,k,alpha,a,lda,beta,c,ldc)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    real(dp), intent(in) :: alpha,beta,a(lda,*)
                    integer(ilp64), intent(in) :: k,lda,ldc,n
                    character, intent(in) :: trans,uplo
                    real(dp), intent(inout) :: c(ldc,*)
               end subroutine dsyrk
#else
               module procedure stdlib_I64_dsyrk
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine ssyrk(uplo,trans,n,k,alpha,a,lda,beta,c,ldc)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    real(sp), intent(in) :: alpha,beta,a(lda,*)
                    integer(ilp64), intent(in) :: k,lda,ldc,n
                    character, intent(in) :: trans,uplo
                    real(sp), intent(inout) :: c(ldc,*)
               end subroutine ssyrk
#else
               module procedure stdlib_I64_ssyrk
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine zsyrk(uplo,trans,n,k,alpha,a,lda,beta,c,ldc)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    complex(dp), intent(in) :: alpha,beta,a(lda,*)
                    integer(ilp64), intent(in) :: k,lda,ldc,n
                    character, intent(in) :: trans,uplo
                    complex(dp), intent(inout) :: c(ldc,*)
               end subroutine zsyrk
#else
               module procedure stdlib_I64_zsyrk
#endif
          end interface syrk

          interface tbmv
          !! TBMV performs one of the matrix-vector operations
          !! x := A*x,   or   x := A**T*x,   or   x := A**H*x,
          !! where x is an n element vector and  A is an n by n unit, or non-unit,
          !! upper or lower triangular band matrix, with ( k + 1 ) diagonals.
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine ctbmv(uplo,trans,diag,n,k,a,lda,x,incx)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    integer(ilp), intent(in) :: incx,k,lda,n
                    character, intent(in) :: diag,trans,uplo
                    complex(sp), intent(in) :: a(lda,*)
                    complex(sp), intent(inout) :: x(*)
               end subroutine ctbmv
#else
               module procedure stdlib_ctbmv
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine dtbmv(uplo,trans,diag,n,k,a,lda,x,incx)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    integer(ilp), intent(in) :: incx,k,lda,n
                    character, intent(in) :: diag,trans,uplo
                    real(dp), intent(in) :: a(lda,*)
                    real(dp), intent(inout) :: x(*)
               end subroutine dtbmv
#else
               module procedure stdlib_dtbmv
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine stbmv(uplo,trans,diag,n,k,a,lda,x,incx)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    integer(ilp), intent(in) :: incx,k,lda,n
                    character, intent(in) :: diag,trans,uplo
                    real(sp), intent(in) :: a(lda,*)
                    real(sp), intent(inout) :: x(*)
               end subroutine stbmv
#else
               module procedure stdlib_stbmv
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine ztbmv(uplo,trans,diag,n,k,a,lda,x,incx)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    integer(ilp), intent(in) :: incx,k,lda,n
                    character, intent(in) :: diag,trans,uplo
                    complex(dp), intent(in) :: a(lda,*)
                    complex(dp), intent(inout) :: x(*)
               end subroutine ztbmv
#else
               module procedure stdlib_ztbmv
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine ctbmv(uplo,trans,diag,n,k,a,lda,x,incx)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    integer(ilp64), intent(in) :: incx,k,lda,n
                    character, intent(in) :: diag,trans,uplo
                    complex(sp), intent(in) :: a(lda,*)
                    complex(sp), intent(inout) :: x(*)
               end subroutine ctbmv
#else
               module procedure stdlib_I64_ctbmv
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine dtbmv(uplo,trans,diag,n,k,a,lda,x,incx)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    integer(ilp64), intent(in) :: incx,k,lda,n
                    character, intent(in) :: diag,trans,uplo
                    real(dp), intent(in) :: a(lda,*)
                    real(dp), intent(inout) :: x(*)
               end subroutine dtbmv
#else
               module procedure stdlib_I64_dtbmv
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine stbmv(uplo,trans,diag,n,k,a,lda,x,incx)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    integer(ilp64), intent(in) :: incx,k,lda,n
                    character, intent(in) :: diag,trans,uplo
                    real(sp), intent(in) :: a(lda,*)
                    real(sp), intent(inout) :: x(*)
               end subroutine stbmv
#else
               module procedure stdlib_I64_stbmv
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine ztbmv(uplo,trans,diag,n,k,a,lda,x,incx)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    integer(ilp64), intent(in) :: incx,k,lda,n
                    character, intent(in) :: diag,trans,uplo
                    complex(dp), intent(in) :: a(lda,*)
                    complex(dp), intent(inout) :: x(*)
               end subroutine ztbmv
#else
               module procedure stdlib_I64_ztbmv
#endif
          end interface tbmv

          interface tbsv
          !! TBSV solves one of the systems of equations
          !! A*x = b,   or   A**T*x = b,   or   A**H*x = b,
          !! where b and x are n element vectors and A is an n by n unit, or
          !! non-unit, upper or lower triangular band matrix, with ( k + 1 )
          !! diagonals.
          !! No test for singularity or near-singularity is included in this
          !! routine. Such tests must be performed before calling this routine.
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine ctbsv(uplo,trans,diag,n,k,a,lda,x,incx)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    integer(ilp), intent(in) :: incx,k,lda,n
                    character, intent(in) :: diag,trans,uplo
                    complex(sp), intent(in) :: a(lda,*)
                    complex(sp), intent(inout) :: x(*)
               end subroutine ctbsv
#else
               module procedure stdlib_ctbsv
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine dtbsv(uplo,trans,diag,n,k,a,lda,x,incx)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    integer(ilp), intent(in) :: incx,k,lda,n
                    character, intent(in) :: diag,trans,uplo
                    real(dp), intent(in) :: a(lda,*)
                    real(dp), intent(inout) :: x(*)
               end subroutine dtbsv
#else
               module procedure stdlib_dtbsv
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine stbsv(uplo,trans,diag,n,k,a,lda,x,incx)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    integer(ilp), intent(in) :: incx,k,lda,n
                    character, intent(in) :: diag,trans,uplo
                    real(sp), intent(in) :: a(lda,*)
                    real(sp), intent(inout) :: x(*)
               end subroutine stbsv
#else
               module procedure stdlib_stbsv
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine ztbsv(uplo,trans,diag,n,k,a,lda,x,incx)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    integer(ilp), intent(in) :: incx,k,lda,n
                    character, intent(in) :: diag,trans,uplo
                    complex(dp), intent(in) :: a(lda,*)
                    complex(dp), intent(inout) :: x(*)
               end subroutine ztbsv
#else
               module procedure stdlib_ztbsv
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine ctbsv(uplo,trans,diag,n,k,a,lda,x,incx)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    integer(ilp64), intent(in) :: incx,k,lda,n
                    character, intent(in) :: diag,trans,uplo
                    complex(sp), intent(in) :: a(lda,*)
                    complex(sp), intent(inout) :: x(*)
               end subroutine ctbsv
#else
               module procedure stdlib_I64_ctbsv
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine dtbsv(uplo,trans,diag,n,k,a,lda,x,incx)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    integer(ilp64), intent(in) :: incx,k,lda,n
                    character, intent(in) :: diag,trans,uplo
                    real(dp), intent(in) :: a(lda,*)
                    real(dp), intent(inout) :: x(*)
               end subroutine dtbsv
#else
               module procedure stdlib_I64_dtbsv
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine stbsv(uplo,trans,diag,n,k,a,lda,x,incx)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    integer(ilp64), intent(in) :: incx,k,lda,n
                    character, intent(in) :: diag,trans,uplo
                    real(sp), intent(in) :: a(lda,*)
                    real(sp), intent(inout) :: x(*)
               end subroutine stbsv
#else
               module procedure stdlib_I64_stbsv
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine ztbsv(uplo,trans,diag,n,k,a,lda,x,incx)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    integer(ilp64), intent(in) :: incx,k,lda,n
                    character, intent(in) :: diag,trans,uplo
                    complex(dp), intent(in) :: a(lda,*)
                    complex(dp), intent(inout) :: x(*)
               end subroutine ztbsv
#else
               module procedure stdlib_I64_ztbsv
#endif
          end interface tbsv

          interface tpmv
          !! TPMV performs one of the matrix-vector operations
          !! x := A*x,   or   x := A**T*x,   or   x := A**H*x,
          !! where x is an n element vector and  A is an n by n unit, or non-unit,
          !! upper or lower triangular matrix, supplied in packed form.
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine ctpmv(uplo,trans,diag,n,ap,x,incx)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    integer(ilp), intent(in) :: incx,n
                    character, intent(in) :: diag,trans,uplo
                    complex(sp), intent(in) :: ap(*)
                    complex(sp), intent(inout) :: x(*)
               end subroutine ctpmv
#else
               module procedure stdlib_ctpmv
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine dtpmv(uplo,trans,diag,n,ap,x,incx)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    integer(ilp), intent(in) :: incx,n
                    character, intent(in) :: diag,trans,uplo
                    real(dp), intent(in) :: ap(*)
                    real(dp), intent(inout) :: x(*)
               end subroutine dtpmv
#else
               module procedure stdlib_dtpmv
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine stpmv(uplo,trans,diag,n,ap,x,incx)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    integer(ilp), intent(in) :: incx,n
                    character, intent(in) :: diag,trans,uplo
                    real(sp), intent(in) :: ap(*)
                    real(sp), intent(inout) :: x(*)
               end subroutine stpmv
#else
               module procedure stdlib_stpmv
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine ztpmv(uplo,trans,diag,n,ap,x,incx)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    integer(ilp), intent(in) :: incx,n
                    character, intent(in) :: diag,trans,uplo
                    complex(dp), intent(in) :: ap(*)
                    complex(dp), intent(inout) :: x(*)
               end subroutine ztpmv
#else
               module procedure stdlib_ztpmv
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine ctpmv(uplo,trans,diag,n,ap,x,incx)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    integer(ilp64), intent(in) :: incx,n
                    character, intent(in) :: diag,trans,uplo
                    complex(sp), intent(in) :: ap(*)
                    complex(sp), intent(inout) :: x(*)
               end subroutine ctpmv
#else
               module procedure stdlib_I64_ctpmv
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine dtpmv(uplo,trans,diag,n,ap,x,incx)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    integer(ilp64), intent(in) :: incx,n
                    character, intent(in) :: diag,trans,uplo
                    real(dp), intent(in) :: ap(*)
                    real(dp), intent(inout) :: x(*)
               end subroutine dtpmv
#else
               module procedure stdlib_I64_dtpmv
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine stpmv(uplo,trans,diag,n,ap,x,incx)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    integer(ilp64), intent(in) :: incx,n
                    character, intent(in) :: diag,trans,uplo
                    real(sp), intent(in) :: ap(*)
                    real(sp), intent(inout) :: x(*)
               end subroutine stpmv
#else
               module procedure stdlib_I64_stpmv
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine ztpmv(uplo,trans,diag,n,ap,x,incx)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    integer(ilp64), intent(in) :: incx,n
                    character, intent(in) :: diag,trans,uplo
                    complex(dp), intent(in) :: ap(*)
                    complex(dp), intent(inout) :: x(*)
               end subroutine ztpmv
#else
               module procedure stdlib_I64_ztpmv
#endif
          end interface tpmv

          interface tpsv
          !! TPSV solves one of the systems of equations
          !! A*x = b,   or   A**T*x = b,   or   A**H*x = b,
          !! where b and x are n element vectors and A is an n by n unit, or
          !! non-unit, upper or lower triangular matrix, supplied in packed form.
          !! No test for singularity or near-singularity is included in this
          !! routine. Such tests must be performed before calling this routine.
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine ctpsv(uplo,trans,diag,n,ap,x,incx)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    integer(ilp), intent(in) :: incx,n
                    character, intent(in) :: diag,trans,uplo
                    complex(sp), intent(in) :: ap(*)
                    complex(sp), intent(inout) :: x(*)
               end subroutine ctpsv
#else
               module procedure stdlib_ctpsv
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine dtpsv(uplo,trans,diag,n,ap,x,incx)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    integer(ilp), intent(in) :: incx,n
                    character, intent(in) :: diag,trans,uplo
                    real(dp), intent(in) :: ap(*)
                    real(dp), intent(inout) :: x(*)
               end subroutine dtpsv
#else
               module procedure stdlib_dtpsv
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine stpsv(uplo,trans,diag,n,ap,x,incx)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    integer(ilp), intent(in) :: incx,n
                    character, intent(in) :: diag,trans,uplo
                    real(sp), intent(in) :: ap(*)
                    real(sp), intent(inout) :: x(*)
               end subroutine stpsv
#else
               module procedure stdlib_stpsv
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine ztpsv(uplo,trans,diag,n,ap,x,incx)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    integer(ilp), intent(in) :: incx,n
                    character, intent(in) :: diag,trans,uplo
                    complex(dp), intent(in) :: ap(*)
                    complex(dp), intent(inout) :: x(*)
               end subroutine ztpsv
#else
               module procedure stdlib_ztpsv
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine ctpsv(uplo,trans,diag,n,ap,x,incx)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    integer(ilp64), intent(in) :: incx,n
                    character, intent(in) :: diag,trans,uplo
                    complex(sp), intent(in) :: ap(*)
                    complex(sp), intent(inout) :: x(*)
               end subroutine ctpsv
#else
               module procedure stdlib_I64_ctpsv
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine dtpsv(uplo,trans,diag,n,ap,x,incx)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    integer(ilp64), intent(in) :: incx,n
                    character, intent(in) :: diag,trans,uplo
                    real(dp), intent(in) :: ap(*)
                    real(dp), intent(inout) :: x(*)
               end subroutine dtpsv
#else
               module procedure stdlib_I64_dtpsv
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine stpsv(uplo,trans,diag,n,ap,x,incx)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    integer(ilp64), intent(in) :: incx,n
                    character, intent(in) :: diag,trans,uplo
                    real(sp), intent(in) :: ap(*)
                    real(sp), intent(inout) :: x(*)
               end subroutine stpsv
#else
               module procedure stdlib_I64_stpsv
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine ztpsv(uplo,trans,diag,n,ap,x,incx)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    integer(ilp64), intent(in) :: incx,n
                    character, intent(in) :: diag,trans,uplo
                    complex(dp), intent(in) :: ap(*)
                    complex(dp), intent(inout) :: x(*)
               end subroutine ztpsv
#else
               module procedure stdlib_I64_ztpsv
#endif
          end interface tpsv

          interface trmm
          !! TRMM performs one of the matrix-matrix operations
          !! B := alpha*op( A )*B,   or   B := alpha*B*op( A )
          !! where  alpha  is a scalar,  B  is an m by n matrix,  A  is a unit, or
          !! non-unit,  upper or lower triangular matrix  and  op( A )  is one  of
          !! op( A ) = A   or   op( A ) = A**T   or   op( A ) = A**H.
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine ctrmm(side,uplo,transa,diag,m,n,alpha,a,lda,b,ldb)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    complex(sp), intent(in) :: alpha,a(lda,*)
                    integer(ilp), intent(in) :: lda,ldb,m,n
                    character, intent(in) :: diag,side,transa,uplo
                    complex(sp), intent(inout) :: b(ldb,*)
               end subroutine ctrmm
#else
               module procedure stdlib_ctrmm
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine dtrmm(side,uplo,transa,diag,m,n,alpha,a,lda,b,ldb)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    real(dp), intent(in) :: alpha,a(lda,*)
                    integer(ilp), intent(in) :: lda,ldb,m,n
                    character, intent(in) :: diag,side,transa,uplo
                    real(dp), intent(inout) :: b(ldb,*)
               end subroutine dtrmm
#else
               module procedure stdlib_dtrmm
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine strmm(side,uplo,transa,diag,m,n,alpha,a,lda,b,ldb)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    real(sp), intent(in) :: alpha,a(lda,*)
                    integer(ilp), intent(in) :: lda,ldb,m,n
                    character, intent(in) :: diag,side,transa,uplo
                    real(sp), intent(inout) :: b(ldb,*)
               end subroutine strmm
#else
               module procedure stdlib_strmm
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine ztrmm(side,uplo,transa,diag,m,n,alpha,a,lda,b,ldb)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    complex(dp), intent(in) :: alpha,a(lda,*)
                    integer(ilp), intent(in) :: lda,ldb,m,n
                    character, intent(in) :: diag,side,transa,uplo
                    complex(dp), intent(inout) :: b(ldb,*)
               end subroutine ztrmm
#else
               module procedure stdlib_ztrmm
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine ctrmm(side,uplo,transa,diag,m,n,alpha,a,lda,b,ldb)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    complex(sp), intent(in) :: alpha,a(lda,*)
                    integer(ilp64), intent(in) :: lda,ldb,m,n
                    character, intent(in) :: diag,side,transa,uplo
                    complex(sp), intent(inout) :: b(ldb,*)
               end subroutine ctrmm
#else
               module procedure stdlib_I64_ctrmm
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine dtrmm(side,uplo,transa,diag,m,n,alpha,a,lda,b,ldb)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    real(dp), intent(in) :: alpha,a(lda,*)
                    integer(ilp64), intent(in) :: lda,ldb,m,n
                    character, intent(in) :: diag,side,transa,uplo
                    real(dp), intent(inout) :: b(ldb,*)
               end subroutine dtrmm
#else
               module procedure stdlib_I64_dtrmm
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine strmm(side,uplo,transa,diag,m,n,alpha,a,lda,b,ldb)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    real(sp), intent(in) :: alpha,a(lda,*)
                    integer(ilp64), intent(in) :: lda,ldb,m,n
                    character, intent(in) :: diag,side,transa,uplo
                    real(sp), intent(inout) :: b(ldb,*)
               end subroutine strmm
#else
               module procedure stdlib_I64_strmm
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine ztrmm(side,uplo,transa,diag,m,n,alpha,a,lda,b,ldb)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    complex(dp), intent(in) :: alpha,a(lda,*)
                    integer(ilp64), intent(in) :: lda,ldb,m,n
                    character, intent(in) :: diag,side,transa,uplo
                    complex(dp), intent(inout) :: b(ldb,*)
               end subroutine ztrmm
#else
               module procedure stdlib_I64_ztrmm
#endif
          end interface trmm

          interface trmv
          !! TRMV performs one of the matrix-vector operations
          !! x := A*x,   or   x := A**T*x,   or   x := A**H*x,
          !! where x is an n element vector and  A is an n by n unit, or non-unit,
          !! upper or lower triangular matrix.
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine ctrmv(uplo,trans,diag,n,a,lda,x,incx)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    integer(ilp), intent(in) :: incx,lda,n
                    character, intent(in) :: diag,trans,uplo
                    complex(sp), intent(in) :: a(lda,*)
                    complex(sp), intent(inout) :: x(*)
               end subroutine ctrmv
#else
               module procedure stdlib_ctrmv
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine dtrmv(uplo,trans,diag,n,a,lda,x,incx)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    integer(ilp), intent(in) :: incx,lda,n
                    character, intent(in) :: diag,trans,uplo
                    real(dp), intent(in) :: a(lda,*)
                    real(dp), intent(inout) :: x(*)
               end subroutine dtrmv
#else
               module procedure stdlib_dtrmv
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine strmv(uplo,trans,diag,n,a,lda,x,incx)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    integer(ilp), intent(in) :: incx,lda,n
                    character, intent(in) :: diag,trans,uplo
                    real(sp), intent(in) :: a(lda,*)
                    real(sp), intent(inout) :: x(*)
               end subroutine strmv
#else
               module procedure stdlib_strmv
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine ztrmv(uplo,trans,diag,n,a,lda,x,incx)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    integer(ilp), intent(in) :: incx,lda,n
                    character, intent(in) :: diag,trans,uplo
                    complex(dp), intent(in) :: a(lda,*)
                    complex(dp), intent(inout) :: x(*)
               end subroutine ztrmv
#else
               module procedure stdlib_ztrmv
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine ctrmv(uplo,trans,diag,n,a,lda,x,incx)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    integer(ilp64), intent(in) :: incx,lda,n
                    character, intent(in) :: diag,trans,uplo
                    complex(sp), intent(in) :: a(lda,*)
                    complex(sp), intent(inout) :: x(*)
               end subroutine ctrmv
#else
               module procedure stdlib_I64_ctrmv
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine dtrmv(uplo,trans,diag,n,a,lda,x,incx)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    integer(ilp64), intent(in) :: incx,lda,n
                    character, intent(in) :: diag,trans,uplo
                    real(dp), intent(in) :: a(lda,*)
                    real(dp), intent(inout) :: x(*)
               end subroutine dtrmv
#else
               module procedure stdlib_I64_dtrmv
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine strmv(uplo,trans,diag,n,a,lda,x,incx)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    integer(ilp64), intent(in) :: incx,lda,n
                    character, intent(in) :: diag,trans,uplo
                    real(sp), intent(in) :: a(lda,*)
                    real(sp), intent(inout) :: x(*)
               end subroutine strmv
#else
               module procedure stdlib_I64_strmv
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine ztrmv(uplo,trans,diag,n,a,lda,x,incx)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    integer(ilp64), intent(in) :: incx,lda,n
                    character, intent(in) :: diag,trans,uplo
                    complex(dp), intent(in) :: a(lda,*)
                    complex(dp), intent(inout) :: x(*)
               end subroutine ztrmv
#else
               module procedure stdlib_I64_ztrmv
#endif
          end interface trmv

          interface trsm
          !! TRSM solves one of the matrix equations
          !! op( A )*X = alpha*B,   or   X*op( A ) = alpha*B,
          !! where alpha is a scalar, X and B are m by n matrices, A is a unit, or
          !! non-unit,  upper or lower triangular matrix  and  op( A )  is one  of
          !! op( A ) = A   or   op( A ) = A**T   or   op( A ) = A**H.
          !! The matrix X is overwritten on B.
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine ctrsm(side,uplo,transa,diag,m,n,alpha,a,lda,b,ldb)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    complex(sp), intent(in) :: alpha,a(lda,*)
                    integer(ilp), intent(in) :: lda,ldb,m,n
                    character, intent(in) :: diag,side,transa,uplo
                    complex(sp), intent(inout) :: b(ldb,*)
               end subroutine ctrsm
#else
               module procedure stdlib_ctrsm
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine dtrsm(side,uplo,transa,diag,m,n,alpha,a,lda,b,ldb)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    real(dp), intent(in) :: alpha,a(lda,*)
                    integer(ilp), intent(in) :: lda,ldb,m,n
                    character, intent(in) :: diag,side,transa,uplo
                    real(dp), intent(inout) :: b(ldb,*)
               end subroutine dtrsm
#else
               module procedure stdlib_dtrsm
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine strsm(side,uplo,transa,diag,m,n,alpha,a,lda,b,ldb)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    real(sp), intent(in) :: alpha,a(lda,*)
                    integer(ilp), intent(in) :: lda,ldb,m,n
                    character, intent(in) :: diag,side,transa,uplo
                    real(sp), intent(inout) :: b(ldb,*)
               end subroutine strsm
#else
               module procedure stdlib_strsm
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine ztrsm(side,uplo,transa,diag,m,n,alpha,a,lda,b,ldb)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    complex(dp), intent(in) :: alpha,a(lda,*)
                    integer(ilp), intent(in) :: lda,ldb,m,n
                    character, intent(in) :: diag,side,transa,uplo
                    complex(dp), intent(inout) :: b(ldb,*)
               end subroutine ztrsm
#else
               module procedure stdlib_ztrsm
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine ctrsm(side,uplo,transa,diag,m,n,alpha,a,lda,b,ldb)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    complex(sp), intent(in) :: alpha,a(lda,*)
                    integer(ilp64), intent(in) :: lda,ldb,m,n
                    character, intent(in) :: diag,side,transa,uplo
                    complex(sp), intent(inout) :: b(ldb,*)
               end subroutine ctrsm
#else
               module procedure stdlib_I64_ctrsm
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine dtrsm(side,uplo,transa,diag,m,n,alpha,a,lda,b,ldb)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    real(dp), intent(in) :: alpha,a(lda,*)
                    integer(ilp64), intent(in) :: lda,ldb,m,n
                    character, intent(in) :: diag,side,transa,uplo
                    real(dp), intent(inout) :: b(ldb,*)
               end subroutine dtrsm
#else
               module procedure stdlib_I64_dtrsm
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine strsm(side,uplo,transa,diag,m,n,alpha,a,lda,b,ldb)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    real(sp), intent(in) :: alpha,a(lda,*)
                    integer(ilp64), intent(in) :: lda,ldb,m,n
                    character, intent(in) :: diag,side,transa,uplo
                    real(sp), intent(inout) :: b(ldb,*)
               end subroutine strsm
#else
               module procedure stdlib_I64_strsm
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine ztrsm(side,uplo,transa,diag,m,n,alpha,a,lda,b,ldb)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    complex(dp), intent(in) :: alpha,a(lda,*)
                    integer(ilp64), intent(in) :: lda,ldb,m,n
                    character, intent(in) :: diag,side,transa,uplo
                    complex(dp), intent(inout) :: b(ldb,*)
               end subroutine ztrsm
#else
               module procedure stdlib_I64_ztrsm
#endif
          end interface trsm

          interface trsv
          !! TRSV solves one of the systems of equations
          !! A*x = b,   or   A**T*x = b,   or   A**H*x = b,
          !! where b and x are n element vectors and A is an n by n unit, or
          !! non-unit, upper or lower triangular matrix.
          !! No test for singularity or near-singularity is included in this
          !! routine. Such tests must be performed before calling this routine.
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine ctrsv(uplo,trans,diag,n,a,lda,x,incx)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    integer(ilp), intent(in) :: incx,lda,n
                    character, intent(in) :: diag,trans,uplo
                    complex(sp), intent(in) :: a(lda,*)
                    complex(sp), intent(inout) :: x(*)
               end subroutine ctrsv
#else
               module procedure stdlib_ctrsv
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine dtrsv(uplo,trans,diag,n,a,lda,x,incx)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    integer(ilp), intent(in) :: incx,lda,n
                    character, intent(in) :: diag,trans,uplo
                    real(dp), intent(in) :: a(lda,*)
                    real(dp), intent(inout) :: x(*)
               end subroutine dtrsv
#else
               module procedure stdlib_dtrsv
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine strsv(uplo,trans,diag,n,a,lda,x,incx)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    integer(ilp), intent(in) :: incx,lda,n
                    character, intent(in) :: diag,trans,uplo
                    real(sp), intent(in) :: a(lda,*)
                    real(sp), intent(inout) :: x(*)
               end subroutine strsv
#else
               module procedure stdlib_strsv
#endif
#ifdef STDLIB_EXTERNAL_BLAS
               pure subroutine ztrsv(uplo,trans,diag,n,a,lda,x,incx)
                    import sp,dp,qp,ilp,lk
                    implicit none
                    integer(ilp), intent(in) :: incx,lda,n
                    character, intent(in) :: diag,trans,uplo
                    complex(dp), intent(in) :: a(lda,*)
                    complex(dp), intent(inout) :: x(*)
               end subroutine ztrsv
#else
               module procedure stdlib_ztrsv
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine ctrsv(uplo,trans,diag,n,a,lda,x,incx)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    integer(ilp64), intent(in) :: incx,lda,n
                    character, intent(in) :: diag,trans,uplo
                    complex(sp), intent(in) :: a(lda,*)
                    complex(sp), intent(inout) :: x(*)
               end subroutine ctrsv
#else
               module procedure stdlib_I64_ctrsv
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine dtrsv(uplo,trans,diag,n,a,lda,x,incx)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    integer(ilp64), intent(in) :: incx,lda,n
                    character, intent(in) :: diag,trans,uplo
                    real(dp), intent(in) :: a(lda,*)
                    real(dp), intent(inout) :: x(*)
               end subroutine dtrsv
#else
               module procedure stdlib_I64_dtrsv
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine strsv(uplo,trans,diag,n,a,lda,x,incx)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    integer(ilp64), intent(in) :: incx,lda,n
                    character, intent(in) :: diag,trans,uplo
                    real(sp), intent(in) :: a(lda,*)
                    real(sp), intent(inout) :: x(*)
               end subroutine strsv
#else
               module procedure stdlib_I64_strsv
#endif
#ifdef STDLIB_EXTERNAL_BLAS_I64
               pure subroutine ztrsv(uplo,trans,diag,n,a,lda,x,incx)
                    import sp,dp,qp,ilp64,lk
                    implicit none
                    integer(ilp64), intent(in) :: incx,lda,n
                    character, intent(in) :: diag,trans,uplo
                    complex(dp), intent(in) :: a(lda,*)
                    complex(dp), intent(inout) :: x(*)
               end subroutine ztrsv
#else
               module procedure stdlib_I64_ztrsv
#endif
          end interface trsv

end module stdlib_linalg_blas
