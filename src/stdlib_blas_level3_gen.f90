submodule(stdlib_blas) stdlib_blas_level3_gen
  implicit none


  contains

     pure module subroutine stdlib_sgemm(transa,transb,m,n,k,alpha,a,lda,b,ldb,beta,c,ldc)
     use stdlib_blas_constants_sp
     !! SGEMM performs one of the matrix-matrix operations
     !! C := alpha*op( A )*op( B ) + beta*C,
     !! where  op( X ) is one of
     !! op( X ) = X   or   op( X ) = X**T,
     !! alpha and beta are scalars, and A, B and C are matrices, with op( A )
     !! an m by k matrix,  op( B )  a  k by n matrix and  C an m by n matrix.
        ! -- reference blas level3 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           real(sp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: k, lda, ldb, ldc, m, n
           character, intent(in) :: transa, transb
           ! Array Arguments 
           real(sp), intent(in) :: a(lda,*), b(ldb,*)
           real(sp), intent(inout) :: c(ldc,*)
        ! =====================================================================
           ! Intrinsic Functions 
           intrinsic :: max
           ! Local Scalars 
           real(sp) :: temp
           integer(ilp) :: i, info, j, l, nrowa, nrowb
           logical(lk) :: nota, notb
           
           ! set  nota  and  notb  as  true if  a  and  b  respectively are not
           ! transposed and set  nrowa and nrowb  as the number of rows of  a
           ! and  b  respectively.
           nota = stdlib_lsame(transa,'N')
           notb = stdlib_lsame(transb,'N')
           if (nota) then
               nrowa = m
           else
               nrowa = k
           end if
           if (notb) then
               nrowb = k
           else
               nrowb = n
           end if
           ! test the input parameters.
           info = 0
           if ((.not.nota) .and. (.not.stdlib_lsame(transa,'C')) .and.(.not.stdlib_lsame(transa,&
                     'T'))) then
               info = 1
           else if ((.not.notb) .and. (.not.stdlib_lsame(transb,'C')) .and.(.not.stdlib_lsame(&
                     transb,'T'))) then
               info = 2
           else if (m<0) then
               info = 3
           else if (n<0) then
               info = 4
           else if (k<0) then
               info = 5
           else if (lda<max(1,nrowa)) then
               info = 8
           else if (ldb<max(1,nrowb)) then
               info = 10
           else if (ldc<max(1,m)) then
               info = 13
           end if
           if (info/=0) then
               call stdlib_xerbla('SGEMM ',info)
               return
           end if
           ! quick return if possible.
           if ((m==0) .or. (n==0) .or.(((alpha==zero).or. (k==0)).and. (beta==one))) &
                     return
           ! and if  alpha.eq.zero.
           if (alpha==zero) then
               if (beta==zero) then
                   do j = 1,n
                       do i = 1,m
                           c(i,j) = zero
                       end do
                   end do
               else
                   do j = 1,n
                       do i = 1,m
                           c(i,j) = beta*c(i,j)
                       end do
                   end do
               end if
               return
           end if
           ! start the operations.
           if (notb) then
               if (nota) then
                 ! form  c := alpha*a*b + beta*c.
                   do j = 1,n
                       if (beta==zero) then
                           do i = 1,m
                               c(i,j) = zero
                           end do
                       else if (beta/=one) then
                           do i = 1,m
                               c(i,j) = beta*c(i,j)
                           end do
                       end if
                       do l = 1,k
                           temp = alpha*b(l,j)
                           do i = 1,m
                               c(i,j) = c(i,j) + temp*a(i,l)
                           end do
                       end do
                   end do
               else
                 ! form  c := alpha*a**t*b + beta*c
                   do j = 1,n
                       do i = 1,m
                           temp = zero
                           do l = 1,k
                               temp = temp + a(l,i)*b(l,j)
                           end do
                           if (beta==zero) then
                               c(i,j) = alpha*temp
                           else
                               c(i,j) = alpha*temp + beta*c(i,j)
                           end if
                       end do
                   end do
               end if
           else
               if (nota) then
                 ! form  c := alpha*a*b**t + beta*c
                   do j = 1,n
                       if (beta==zero) then
                           do i = 1,m
                               c(i,j) = zero
                           end do
                       else if (beta/=one) then
                           do i = 1,m
                               c(i,j) = beta*c(i,j)
                           end do
                       end if
                       do l = 1,k
                           temp = alpha*b(j,l)
                           do i = 1,m
                               c(i,j) = c(i,j) + temp*a(i,l)
                           end do
                       end do
                   end do
               else
                 ! form  c := alpha*a**t*b**t + beta*c
                   do j = 1,n
                       do i = 1,m
                           temp = zero
                           do l = 1,k
                               temp = temp + a(l,i)*b(j,l)
                           end do
                           if (beta==zero) then
                               c(i,j) = alpha*temp
                           else
                               c(i,j) = alpha*temp + beta*c(i,j)
                           end if
                       end do
                   end do
               end if
           end if
           return
     end subroutine stdlib_sgemm

     pure module subroutine stdlib_dgemm(transa,transb,m,n,k,alpha,a,lda,b,ldb,beta,c,ldc)
     use stdlib_blas_constants_dp
     !! DGEMM performs one of the matrix-matrix operations
     !! C := alpha*op( A )*op( B ) + beta*C,
     !! where  op( X ) is one of
     !! op( X ) = X   or   op( X ) = X**T,
     !! alpha and beta are scalars, and A, B and C are matrices, with op( A )
     !! an m by k matrix,  op( B )  a  k by n matrix and  C an m by n matrix.
        ! -- reference blas level3 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           real(dp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: k, lda, ldb, ldc, m, n
           character, intent(in) :: transa, transb
           ! Array Arguments 
           real(dp), intent(in) :: a(lda,*), b(ldb,*)
           real(dp), intent(inout) :: c(ldc,*)
        ! =====================================================================
           ! Intrinsic Functions 
           intrinsic :: max
           ! Local Scalars 
           real(dp) :: temp
           integer(ilp) :: i, info, j, l, nrowa, nrowb
           logical(lk) :: nota, notb
           
           ! set  nota  and  notb  as  true if  a  and  b  respectively are not
           ! transposed and set  nrowa and nrowb  as the number of rows of  a
           ! and  b  respectively.
           nota = stdlib_lsame(transa,'N')
           notb = stdlib_lsame(transb,'N')
           if (nota) then
               nrowa = m
           else
               nrowa = k
           end if
           if (notb) then
               nrowb = k
           else
               nrowb = n
           end if
           ! test the input parameters.
           info = 0
           if ((.not.nota) .and. (.not.stdlib_lsame(transa,'C')) .and.(.not.stdlib_lsame(transa,&
                     'T'))) then
               info = 1
           else if ((.not.notb) .and. (.not.stdlib_lsame(transb,'C')) .and.(.not.stdlib_lsame(&
                     transb,'T'))) then
               info = 2
           else if (m<0) then
               info = 3
           else if (n<0) then
               info = 4
           else if (k<0) then
               info = 5
           else if (lda<max(1,nrowa)) then
               info = 8
           else if (ldb<max(1,nrowb)) then
               info = 10
           else if (ldc<max(1,m)) then
               info = 13
           end if
           if (info/=0) then
               call stdlib_xerbla('DGEMM ',info)
               return
           end if
           ! quick return if possible.
           if ((m==0) .or. (n==0) .or.(((alpha==zero).or. (k==0)).and. (beta==one))) &
                     return
           ! and if  alpha.eq.zero.
           if (alpha==zero) then
               if (beta==zero) then
                   do j = 1,n
                       do i = 1,m
                           c(i,j) = zero
                       end do
                   end do
               else
                   do j = 1,n
                       do i = 1,m
                           c(i,j) = beta*c(i,j)
                       end do
                   end do
               end if
               return
           end if
           ! start the operations.
           if (notb) then
               if (nota) then
                 ! form  c := alpha*a*b + beta*c.
                   do j = 1,n
                       if (beta==zero) then
                           do i = 1,m
                               c(i,j) = zero
                           end do
                       else if (beta/=one) then
                           do i = 1,m
                               c(i,j) = beta*c(i,j)
                           end do
                       end if
                       do l = 1,k
                           temp = alpha*b(l,j)
                           do i = 1,m
                               c(i,j) = c(i,j) + temp*a(i,l)
                           end do
                       end do
                   end do
               else
                 ! form  c := alpha*a**t*b + beta*c
                   do j = 1,n
                       do i = 1,m
                           temp = zero
                           do l = 1,k
                               temp = temp + a(l,i)*b(l,j)
                           end do
                           if (beta==zero) then
                               c(i,j) = alpha*temp
                           else
                               c(i,j) = alpha*temp + beta*c(i,j)
                           end if
                       end do
                   end do
               end if
           else
               if (nota) then
                 ! form  c := alpha*a*b**t + beta*c
                   do j = 1,n
                       if (beta==zero) then
                           do i = 1,m
                               c(i,j) = zero
                           end do
                       else if (beta/=one) then
                           do i = 1,m
                               c(i,j) = beta*c(i,j)
                           end do
                       end if
                       do l = 1,k
                           temp = alpha*b(j,l)
                           do i = 1,m
                               c(i,j) = c(i,j) + temp*a(i,l)
                           end do
                       end do
                   end do
               else
                 ! form  c := alpha*a**t*b**t + beta*c
                   do j = 1,n
                       do i = 1,m
                           temp = zero
                           do l = 1,k
                               temp = temp + a(l,i)*b(j,l)
                           end do
                           if (beta==zero) then
                               c(i,j) = alpha*temp
                           else
                               c(i,j) = alpha*temp + beta*c(i,j)
                           end if
                       end do
                   end do
               end if
           end if
           return
     end subroutine stdlib_dgemm


     pure module subroutine stdlib_cgemm(transa,transb,m,n,k,alpha,a,lda,b,ldb,beta,c,ldc)
     use stdlib_blas_constants_sp
     !! CGEMM performs one of the matrix-matrix operations
     !! C := alpha*op( A )*op( B ) + beta*C,
     !! where  op( X ) is one of
     !! op( X ) = X   or   op( X ) = X**T   or   op( X ) = X**H,
     !! alpha and beta are scalars, and A, B and C are matrices, with op( A )
     !! an m by k matrix,  op( B )  a  k by n matrix and  C an m by n matrix.
        ! -- reference blas level3 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           complex(sp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: k, lda, ldb, ldc, m, n
           character, intent(in) :: transa, transb
           ! Array Arguments 
           complex(sp), intent(in) :: a(lda,*), b(ldb,*)
           complex(sp), intent(inout) :: c(ldc,*)
        ! =====================================================================
           ! Intrinsic Functions 
           intrinsic :: conjg,max
           ! Local Scalars 
           complex(sp) :: temp
           integer(ilp) :: i, info, j, l, nrowa, nrowb
           logical(lk) :: conja, conjb, nota, notb
           
           
           ! set  nota  and  notb  as  true if  a  and  b  respectively are not
           ! conjugated or transposed, set  conja and conjb  as true if  a  and
           ! b  respectively are to be  transposed but  not conjugated  and set
           ! nrowa and  nrowb  as the number of rows of  a  and  b  respectively.
           nota = stdlib_lsame(transa,'N')
           notb = stdlib_lsame(transb,'N')
           conja = stdlib_lsame(transa,'C')
           conjb = stdlib_lsame(transb,'C')
           if (nota) then
               nrowa = m
           else
               nrowa = k
           end if
           if (notb) then
               nrowb = k
           else
               nrowb = n
           end if
           ! test the input parameters.
           info = 0
           if ((.not.nota) .and. (.not.conja) .and.(.not.stdlib_lsame(transa,'T'))) then
               info = 1
           else if ((.not.notb) .and. (.not.conjb) .and.(.not.stdlib_lsame(transb,'T'))) &
                     then
               info = 2
           else if (m<0) then
               info = 3
           else if (n<0) then
               info = 4
           else if (k<0) then
               info = 5
           else if (lda<max(1,nrowa)) then
               info = 8
           else if (ldb<max(1,nrowb)) then
               info = 10
           else if (ldc<max(1,m)) then
               info = 13
           end if
           if (info/=0) then
               call stdlib_xerbla('CGEMM ',info)
               return
           end if
           ! quick return if possible.
           if ((m==0) .or. (n==0) .or.(((alpha==czero).or. (k==0)).and. (beta==cone))) &
                     return
           ! and when  alpha.eq.czero.
           if (alpha==czero) then
               if (beta==czero) then
                   do j = 1,n
                       do i = 1,m
                           c(i,j) = czero
                       end do
                   end do
               else
                   do j = 1,n
                       do i = 1,m
                           c(i,j) = beta*c(i,j)
                       end do
                   end do
               end if
               return
           end if
           ! start the operations.
           if (notb) then
               if (nota) then
                 ! form  c := alpha*a*b + beta*c.
                   do j = 1,n
                       if (beta==czero) then
                           do i = 1,m
                               c(i,j) = czero
                           end do
                       else if (beta/=cone) then
                           do i = 1,m
                               c(i,j) = beta*c(i,j)
                           end do
                       end if
                       do l = 1,k
                           temp = alpha*b(l,j)
                           do i = 1,m
                               c(i,j) = c(i,j) + temp*a(i,l)
                           end do
                       end do
                   end do
               else if (conja) then
                 ! form  c := alpha*a**h*b + beta*c.
                   do j = 1,n
                       do i = 1,m
                           temp = czero
                           do l = 1,k
                               temp = temp + conjg(a(l,i))*b(l,j)
                           end do
                           if (beta==czero) then
                               c(i,j) = alpha*temp
                           else
                               c(i,j) = alpha*temp + beta*c(i,j)
                           end if
                       end do
                   end do
               else
                 ! form  c := alpha*a**t*b + beta*c
                   do j = 1,n
                       do i = 1,m
                           temp = czero
                           do l = 1,k
                               temp = temp + a(l,i)*b(l,j)
                           end do
                           if (beta==czero) then
                               c(i,j) = alpha*temp
                           else
                               c(i,j) = alpha*temp + beta*c(i,j)
                           end if
                       end do
                   end do
               end if
           else if (nota) then
               if (conjb) then
                 ! form  c := alpha*a*b**h + beta*c.
                   do j = 1,n
                       if (beta==czero) then
                           do i = 1,m
                               c(i,j) = czero
                           end do
                       else if (beta/=cone) then
                           do i = 1,m
                               c(i,j) = beta*c(i,j)
                           end do
                       end if
                       do l = 1,k
                           temp = alpha*conjg(b(j,l))
                           do i = 1,m
                               c(i,j) = c(i,j) + temp*a(i,l)
                           end do
                       end do
                   end do
               else
                 ! form  c := alpha*a*b**t + beta*c
                   do j = 1,n
                       if (beta==czero) then
                           do i = 1,m
                               c(i,j) = czero
                           end do
                       else if (beta/=cone) then
                           do i = 1,m
                               c(i,j) = beta*c(i,j)
                           end do
                       end if
                       do l = 1,k
                           temp = alpha*b(j,l)
                           do i = 1,m
                               c(i,j) = c(i,j) + temp*a(i,l)
                           end do
                       end do
                   end do
               end if
           else if (conja) then
               if (conjb) then
                 ! form  c := alpha*a**h*b**h + beta*c.
                   do j = 1,n
                       do i = 1,m
                           temp = czero
                           do l = 1,k
                               temp = temp + conjg(a(l,i))*conjg(b(j,l))
                           end do
                           if (beta==czero) then
                               c(i,j) = alpha*temp
                           else
                               c(i,j) = alpha*temp + beta*c(i,j)
                           end if
                       end do
                   end do
               else
                 ! form  c := alpha*a**h*b**t + beta*c
                   do j = 1,n
                       do i = 1,m
                           temp = czero
                           do l = 1,k
                               temp = temp + conjg(a(l,i))*b(j,l)
                           end do
                           if (beta==czero) then
                               c(i,j) = alpha*temp
                           else
                               c(i,j) = alpha*temp + beta*c(i,j)
                           end if
                       end do
                   end do
               end if
           else
               if (conjb) then
                 ! form  c := alpha*a**t*b**h + beta*c
                   do j = 1,n
                       do i = 1,m
                           temp = czero
                           do l = 1,k
                               temp = temp + a(l,i)*conjg(b(j,l))
                           end do
                           if (beta==czero) then
                               c(i,j) = alpha*temp
                           else
                               c(i,j) = alpha*temp + beta*c(i,j)
                           end if
                       end do
                   end do
               else
                 ! form  c := alpha*a**t*b**t + beta*c
                   do j = 1,n
                       do i = 1,m
                           temp = czero
                           do l = 1,k
                               temp = temp + a(l,i)*b(j,l)
                           end do
                           if (beta==czero) then
                               c(i,j) = alpha*temp
                           else
                               c(i,j) = alpha*temp + beta*c(i,j)
                           end if
                       end do
                   end do
               end if
           end if
           return
     end subroutine stdlib_cgemm

     pure module subroutine stdlib_zgemm(transa,transb,m,n,k,alpha,a,lda,b,ldb,beta,c,ldc)
     use stdlib_blas_constants_dp
     !! ZGEMM performs one of the matrix-matrix operations
     !! C := alpha*op( A )*op( B ) + beta*C,
     !! where  op( X ) is one of
     !! op( X ) = X   or   op( X ) = X**T   or   op( X ) = X**H,
     !! alpha and beta are scalars, and A, B and C are matrices, with op( A )
     !! an m by k matrix,  op( B )  a  k by n matrix and  C an m by n matrix.
        ! -- reference blas level3 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           complex(dp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: k, lda, ldb, ldc, m, n
           character, intent(in) :: transa, transb
           ! Array Arguments 
           complex(dp), intent(in) :: a(lda,*), b(ldb,*)
           complex(dp), intent(inout) :: c(ldc,*)
        ! =====================================================================
           ! Intrinsic Functions 
           intrinsic :: conjg,max
           ! Local Scalars 
           complex(dp) :: temp
           integer(ilp) :: i, info, j, l, nrowa, nrowb
           logical(lk) :: conja, conjb, nota, notb
           
           
           ! set  nota  and  notb  as  true if  a  and  b  respectively are not
           ! conjugated or transposed, set  conja and conjb  as true if  a  and
           ! b  respectively are to be  transposed but  not conjugated  and set
           ! nrowa and nrowb  as the number of rows  of  a  and  b  respectively.
           nota = stdlib_lsame(transa,'N')
           notb = stdlib_lsame(transb,'N')
           conja = stdlib_lsame(transa,'C')
           conjb = stdlib_lsame(transb,'C')
           if (nota) then
               nrowa = m
           else
               nrowa = k
           end if
           if (notb) then
               nrowb = k
           else
               nrowb = n
           end if
           ! test the input parameters.
           info = 0
           if ((.not.nota) .and. (.not.conja) .and.(.not.stdlib_lsame(transa,'T'))) then
               info = 1
           else if ((.not.notb) .and. (.not.conjb) .and.(.not.stdlib_lsame(transb,'T'))) &
                     then
               info = 2
           else if (m<0) then
               info = 3
           else if (n<0) then
               info = 4
           else if (k<0) then
               info = 5
           else if (lda<max(1,nrowa)) then
               info = 8
           else if (ldb<max(1,nrowb)) then
               info = 10
           else if (ldc<max(1,m)) then
               info = 13
           end if
           if (info/=0) then
               call stdlib_xerbla('ZGEMM ',info)
               return
           end if
           ! quick return if possible.
           if ((m==0) .or. (n==0) .or.(((alpha==czero).or. (k==0)).and. (beta==cone))) &
                     return
           ! and when  alpha.eq.czero.
           if (alpha==czero) then
               if (beta==czero) then
                   do j = 1,n
                       do i = 1,m
                           c(i,j) = czero
                       end do
                   end do
               else
                   do j = 1,n
                       do i = 1,m
                           c(i,j) = beta*c(i,j)
                       end do
                   end do
               end if
               return
           end if
           ! start the operations.
           if (notb) then
               if (nota) then
                 ! form  c := alpha*a*b + beta*c.
                   do j = 1,n
                       if (beta==czero) then
                           do i = 1,m
                               c(i,j) = czero
                           end do
                       else if (beta/=cone) then
                           do i = 1,m
                               c(i,j) = beta*c(i,j)
                           end do
                       end if
                       do l = 1,k
                           temp = alpha*b(l,j)
                           do i = 1,m
                               c(i,j) = c(i,j) + temp*a(i,l)
                           end do
                       end do
                   end do
               else if (conja) then
                 ! form  c := alpha*a**h*b + beta*c.
                   do j = 1,n
                       do i = 1,m
                           temp = czero
                           do l = 1,k
                               temp = temp + conjg(a(l,i))*b(l,j)
                           end do
                           if (beta==czero) then
                               c(i,j) = alpha*temp
                           else
                               c(i,j) = alpha*temp + beta*c(i,j)
                           end if
                       end do
                   end do
               else
                 ! form  c := alpha*a**t*b + beta*c
                   do j = 1,n
                       do i = 1,m
                           temp = czero
                           do l = 1,k
                               temp = temp + a(l,i)*b(l,j)
                           end do
                           if (beta==czero) then
                               c(i,j) = alpha*temp
                           else
                               c(i,j) = alpha*temp + beta*c(i,j)
                           end if
                       end do
                   end do
               end if
           else if (nota) then
               if (conjb) then
                 ! form  c := alpha*a*b**h + beta*c.
                   do j = 1,n
                       if (beta==czero) then
                           do i = 1,m
                               c(i,j) = czero
                           end do
                       else if (beta/=cone) then
                           do i = 1,m
                               c(i,j) = beta*c(i,j)
                           end do
                       end if
                       do l = 1,k
                           temp = alpha*conjg(b(j,l))
                           do i = 1,m
                               c(i,j) = c(i,j) + temp*a(i,l)
                           end do
                       end do
                   end do
               else
                 ! form  c := alpha*a*b**t + beta*c
                   do j = 1,n
                       if (beta==czero) then
                           do i = 1,m
                               c(i,j) = czero
                           end do
                       else if (beta/=cone) then
                           do i = 1,m
                               c(i,j) = beta*c(i,j)
                           end do
                       end if
                       do l = 1,k
                           temp = alpha*b(j,l)
                           do i = 1,m
                               c(i,j) = c(i,j) + temp*a(i,l)
                           end do
                       end do
                   end do
               end if
           else if (conja) then
               if (conjb) then
                 ! form  c := alpha*a**h*b**h + beta*c.
                   do j = 1,n
                       do i = 1,m
                           temp = czero
                           do l = 1,k
                               temp = temp + conjg(a(l,i))*conjg(b(j,l))
                           end do
                           if (beta==czero) then
                               c(i,j) = alpha*temp
                           else
                               c(i,j) = alpha*temp + beta*c(i,j)
                           end if
                       end do
                   end do
               else
                 ! form  c := alpha*a**h*b**t + beta*c
                   do j = 1,n
                       do i = 1,m
                           temp = czero
                           do l = 1,k
                               temp = temp + conjg(a(l,i))*b(j,l)
                           end do
                           if (beta==czero) then
                               c(i,j) = alpha*temp
                           else
                               c(i,j) = alpha*temp + beta*c(i,j)
                           end if
                       end do
                   end do
               end if
           else
               if (conjb) then
                 ! form  c := alpha*a**t*b**h + beta*c
                   do j = 1,n
                       do i = 1,m
                           temp = czero
                           do l = 1,k
                               temp = temp + a(l,i)*conjg(b(j,l))
                           end do
                           if (beta==czero) then
                               c(i,j) = alpha*temp
                           else
                               c(i,j) = alpha*temp + beta*c(i,j)
                           end if
                       end do
                   end do
               else
                 ! form  c := alpha*a**t*b**t + beta*c
                   do j = 1,n
                       do i = 1,m
                           temp = czero
                           do l = 1,k
                               temp = temp + a(l,i)*b(j,l)
                           end do
                           if (beta==czero) then
                               c(i,j) = alpha*temp
                           else
                               c(i,j) = alpha*temp + beta*c(i,j)
                           end if
                       end do
                   end do
               end if
           end if
           return
     end subroutine stdlib_zgemm




     pure module subroutine stdlib_chemm(side,uplo,m,n,alpha,a,lda,b,ldb,beta,c,ldc)
     use stdlib_blas_constants_sp
     !! CHEMM performs one of the matrix-matrix operations
     !! C := alpha*A*B + beta*C,
     !! or
     !! C := alpha*B*A + beta*C,
     !! where alpha and beta are scalars, A is an hermitian matrix and  B and
     !! C are m by n matrices.
        ! -- reference blas level3 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           complex(sp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: lda, ldb, ldc, m, n
           character, intent(in) :: side, uplo
           ! Array Arguments 
           complex(sp), intent(in) :: a(lda,*), b(ldb,*)
           complex(sp), intent(inout) :: c(ldc,*)
        ! =====================================================================
           ! Intrinsic Functions 
           intrinsic :: conjg,max,real
           ! Local Scalars 
           complex(sp) :: temp1, temp2
           integer(ilp) :: i, info, j, k, nrowa
           logical(lk) :: upper
           
           
           ! set nrowa as the number of rows of a.
           if (stdlib_lsame(side,'L')) then
               nrowa = m
           else
               nrowa = n
           end if
           upper = stdlib_lsame(uplo,'U')
           ! test the input parameters.
           info = 0
           if ((.not.stdlib_lsame(side,'L')) .and. (.not.stdlib_lsame(side,'R'))) then
               info = 1
           else if ((.not.upper) .and. (.not.stdlib_lsame(uplo,'L'))) then
               info = 2
           else if (m<0) then
               info = 3
           else if (n<0) then
               info = 4
           else if (lda<max(1,nrowa)) then
               info = 7
           else if (ldb<max(1,m)) then
               info = 9
           else if (ldc<max(1,m)) then
               info = 12
           end if
           if (info/=0) then
               call stdlib_xerbla('CHEMM ',info)
               return
           end if
           ! quick return if possible.
           if ((m==0) .or. (n==0) .or.((alpha==czero).and. (beta==cone))) return
           ! and when  alpha.eq.czero.
           if (alpha==czero) then
               if (beta==czero) then
                   do j = 1,n
                       do i = 1,m
                           c(i,j) = czero
                       end do
                   end do
               else
                   do j = 1,n
                       do i = 1,m
                           c(i,j) = beta*c(i,j)
                       end do
                   end do
               end if
               return
           end if
           ! start the operations.
           if (stdlib_lsame(side,'L')) then
              ! form  c := alpha*a*b + beta*c.
               if (upper) then
                   do j = 1,n
                       do i = 1,m
                           temp1 = alpha*b(i,j)
                           temp2 = czero
                           do k = 1,i - 1
                               c(k,j) = c(k,j) + temp1*a(k,i)
                               temp2 = temp2 + b(k,j)*conjg(a(k,i))
                           end do
                           if (beta==czero) then
                               c(i,j) = temp1*real(a(i,i),KIND=sp) + alpha*temp2
                           else
                               c(i,j) = beta*c(i,j) + temp1*real(a(i,i),KIND=sp) +&
                                         alpha*temp2
                           end if
                       end do
                   end do
               else
                   do j = 1,n
                       do i = m,1,-1
                           temp1 = alpha*b(i,j)
                           temp2 = czero
                           do k = i + 1,m
                               c(k,j) = c(k,j) + temp1*a(k,i)
                               temp2 = temp2 + b(k,j)*conjg(a(k,i))
                           end do
                           if (beta==czero) then
                               c(i,j) = temp1*real(a(i,i),KIND=sp) + alpha*temp2
                           else
                               c(i,j) = beta*c(i,j) + temp1*real(a(i,i),KIND=sp) +&
                                         alpha*temp2
                           end if
                       end do
                   end do
               end if
           else
              ! form  c := alpha*b*a + beta*c.
               loop_170: do j = 1,n
                   temp1 = alpha*real(a(j,j),KIND=sp)
                   if (beta==czero) then
                       do i = 1,m
                           c(i,j) = temp1*b(i,j)
                       end do
                   else
                       do i = 1,m
                           c(i,j) = beta*c(i,j) + temp1*b(i,j)
                       end do
                   end if
                   do k = 1,j - 1
                       if (upper) then
                           temp1 = alpha*a(k,j)
                       else
                           temp1 = alpha*conjg(a(j,k))
                       end if
                       do i = 1,m
                           c(i,j) = c(i,j) + temp1*b(i,k)
                       end do
                   end do
                   do k = j + 1,n
                       if (upper) then
                           temp1 = alpha*conjg(a(j,k))
                       else
                           temp1 = alpha*a(k,j)
                       end if
                       do i = 1,m
                           c(i,j) = c(i,j) + temp1*b(i,k)
                       end do
                   end do
               end do loop_170
           end if
           return
     end subroutine stdlib_chemm

     pure module subroutine stdlib_zhemm(side,uplo,m,n,alpha,a,lda,b,ldb,beta,c,ldc)
     use stdlib_blas_constants_dp
     !! ZHEMM performs one of the matrix-matrix operations
     !! C := alpha*A*B + beta*C,
     !! or
     !! C := alpha*B*A + beta*C,
     !! where alpha and beta are scalars, A is an hermitian matrix and  B and
     !! C are m by n matrices.
        ! -- reference blas level3 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           complex(dp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: lda, ldb, ldc, m, n
           character, intent(in) :: side, uplo
           ! Array Arguments 
           complex(dp), intent(in) :: a(lda,*), b(ldb,*)
           complex(dp), intent(inout) :: c(ldc,*)
        ! =====================================================================
           ! Intrinsic Functions 
           intrinsic :: real,conjg,max
           ! Local Scalars 
           complex(dp) :: temp1, temp2
           integer(ilp) :: i, info, j, k, nrowa
           logical(lk) :: upper
           
           
           ! set nrowa as the number of rows of a.
           if (stdlib_lsame(side,'L')) then
               nrowa = m
           else
               nrowa = n
           end if
           upper = stdlib_lsame(uplo,'U')
           ! test the input parameters.
           info = 0
           if ((.not.stdlib_lsame(side,'L')) .and. (.not.stdlib_lsame(side,'R'))) then
               info = 1
           else if ((.not.upper) .and. (.not.stdlib_lsame(uplo,'L'))) then
               info = 2
           else if (m<0) then
               info = 3
           else if (n<0) then
               info = 4
           else if (lda<max(1,nrowa)) then
               info = 7
           else if (ldb<max(1,m)) then
               info = 9
           else if (ldc<max(1,m)) then
               info = 12
           end if
           if (info/=0) then
               call stdlib_xerbla('ZHEMM ',info)
               return
           end if
           ! quick return if possible.
           if ((m==0) .or. (n==0) .or.((alpha==czero).and. (beta==cone))) return
           ! and when  alpha.eq.czero.
           if (alpha==czero) then
               if (beta==czero) then
                   do j = 1,n
                       do i = 1,m
                           c(i,j) = czero
                       end do
                   end do
               else
                   do j = 1,n
                       do i = 1,m
                           c(i,j) = beta*c(i,j)
                       end do
                   end do
               end if
               return
           end if
           ! start the operations.
           if (stdlib_lsame(side,'L')) then
              ! form  c := alpha*a*b + beta*c.
               if (upper) then
                   do j = 1,n
                       do i = 1,m
                           temp1 = alpha*b(i,j)
                           temp2 = czero
                           do k = 1,i - 1
                               c(k,j) = c(k,j) + temp1*a(k,i)
                               temp2 = temp2 + b(k,j)*conjg(a(k,i))
                           end do
                           if (beta==czero) then
                               c(i,j) = temp1*real(a(i,i),KIND=dp) + alpha*temp2
                           else
                               c(i,j) = beta*c(i,j) + temp1*real(a(i,i),KIND=dp) +&
                                         alpha*temp2
                           end if
                       end do
                   end do
               else
                   do j = 1,n
                       do i = m,1,-1
                           temp1 = alpha*b(i,j)
                           temp2 = czero
                           do k = i + 1,m
                               c(k,j) = c(k,j) + temp1*a(k,i)
                               temp2 = temp2 + b(k,j)*conjg(a(k,i))
                           end do
                           if (beta==czero) then
                               c(i,j) = temp1*real(a(i,i),KIND=dp) + alpha*temp2
                           else
                               c(i,j) = beta*c(i,j) + temp1*real(a(i,i),KIND=dp) +&
                                         alpha*temp2
                           end if
                       end do
                   end do
               end if
           else
              ! form  c := alpha*b*a + beta*c.
               loop_170: do j = 1,n
                   temp1 = alpha*real(a(j,j),KIND=dp)
                   if (beta==czero) then
                       do i = 1,m
                           c(i,j) = temp1*b(i,j)
                       end do
                   else
                       do i = 1,m
                           c(i,j) = beta*c(i,j) + temp1*b(i,j)
                       end do
                   end if
                   do k = 1,j - 1
                       if (upper) then
                           temp1 = alpha*a(k,j)
                       else
                           temp1 = alpha*conjg(a(j,k))
                       end if
                       do i = 1,m
                           c(i,j) = c(i,j) + temp1*b(i,k)
                       end do
                   end do
                   do k = j + 1,n
                       if (upper) then
                           temp1 = alpha*conjg(a(j,k))
                       else
                           temp1 = alpha*a(k,j)
                       end if
                       do i = 1,m
                           c(i,j) = c(i,j) + temp1*b(i,k)
                       end do
                   end do
               end do loop_170
           end if
           return
     end subroutine stdlib_zhemm




     pure module subroutine stdlib_cherk(uplo,trans,n,k,alpha,a,lda,beta,c,ldc)
     use stdlib_blas_constants_sp
     !! CHERK performs one of the hermitian rank k operations
     !! C := alpha*A*A**H + beta*C,
     !! or
     !! C := alpha*A**H*A + beta*C,
     !! where  alpha and beta  are  real scalars,  C is an  n by n  hermitian
     !! matrix and  A  is an  n by k  matrix in the  first case and a  k by n
     !! matrix in the second case.
        ! -- reference blas level3 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           real(sp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: k, lda, ldc, n
           character, intent(in) :: trans, uplo
           ! Array Arguments 
           complex(sp), intent(in) :: a(lda,*)
           complex(sp), intent(inout) :: c(ldc,*)
        ! =====================================================================
           ! Intrinsic Functions 
           intrinsic :: cmplx,conjg,max,real
           ! Local Scalars 
           complex(sp) :: temp
           real(sp) :: rtemp
           integer(ilp) :: i, info, j, l, nrowa
           logical(lk) :: upper
           
           ! test the input parameters.
           if (stdlib_lsame(trans,'N')) then
               nrowa = n
           else
               nrowa = k
           end if
           upper = stdlib_lsame(uplo,'U')
           info = 0
           if ((.not.upper) .and. (.not.stdlib_lsame(uplo,'L'))) then
               info = 1
           else if ((.not.stdlib_lsame(trans,'N')) .and.(.not.stdlib_lsame(trans,'C'))) &
                     then
               info = 2
           else if (n<0) then
               info = 3
           else if (k<0) then
               info = 4
           else if (lda<max(1,nrowa)) then
               info = 7
           else if (ldc<max(1,n)) then
               info = 10
           end if
           if (info/=0) then
               call stdlib_xerbla('CHERK ',info)
               return
           end if
           ! quick return if possible.
           if ((n==0) .or. (((alpha==zero).or.(k==0)).and. (beta==one))) return
           ! and when  alpha.eq.zero.
           if (alpha==zero) then
               if (upper) then
                   if (beta==zero) then
                       do j = 1,n
                           do i = 1,j
                               c(i,j) = zero
                           end do
                       end do
                   else
                       do j = 1,n
                           do i = 1,j - 1
                               c(i,j) = beta*c(i,j)
                           end do
                           c(j,j) = beta*real(c(j,j),KIND=sp)
                       end do
                   end if
               else
                   if (beta==zero) then
                       do j = 1,n
                           do i = j,n
                               c(i,j) = zero
                           end do
                       end do
                   else
                       do j = 1,n
                           c(j,j) = beta*real(c(j,j),KIND=sp)
                           do i = j + 1,n
                               c(i,j) = beta*c(i,j)
                           end do
                       end do
                   end if
               end if
               return
           end if
           ! start the operations.
           if (stdlib_lsame(trans,'N')) then
              ! form  c := alpha*a*a**h + beta*c.
               if (upper) then
                   do j = 1,n
                       if (beta==zero) then
                           do i = 1,j
                               c(i,j) = zero
                           end do
                       else if (beta/=one) then
                           do i = 1,j - 1
                               c(i,j) = beta*c(i,j)
                           end do
                           c(j,j) = beta*real(c(j,j),KIND=sp)
                       else
                           c(j,j) = real(c(j,j),KIND=sp)
                       end if
                       do l = 1,k
                           if (a(j,l)/=cmplx(zero,KIND=sp)) then
                               temp = alpha*conjg(a(j,l))
                               do i = 1,j - 1
                                   c(i,j) = c(i,j) + temp*a(i,l)
                               end do
                               c(j,j) = real(c(j,j),KIND=sp) + real(temp*a(i,l),KIND=sp)
                           end if
                       end do
                   end do
               else
                   do j = 1,n
                       if (beta==zero) then
                           do i = j,n
                               c(i,j) = zero
                           end do
                       else if (beta/=one) then
                           c(j,j) = beta*real(c(j,j),KIND=sp)
                           do i = j + 1,n
                               c(i,j) = beta*c(i,j)
                           end do
                       else
                           c(j,j) = real(c(j,j),KIND=sp)
                       end if
                       do l = 1,k
                           if (a(j,l)/=cmplx(zero,KIND=sp)) then
                               temp = alpha*conjg(a(j,l))
                               c(j,j) = real(c(j,j),KIND=sp) + real(temp*a(j,l),KIND=sp)
                               do i = j + 1,n
                                   c(i,j) = c(i,j) + temp*a(i,l)
                               end do
                           end if
                       end do
                   end do
               end if
           else
              ! form  c := alpha*a**h*a + beta*c.
               if (upper) then
                   do j = 1,n
                       do i = 1,j - 1
                           temp = zero
                           do l = 1,k
                               temp = temp + conjg(a(l,i))*a(l,j)
                           end do
                           if (beta==zero) then
                               c(i,j) = alpha*temp
                           else
                               c(i,j) = alpha*temp + beta*c(i,j)
                           end if
                       end do
                       rtemp = zero
                       do l = 1,k
                           rtemp = rtemp + conjg(a(l,j))*a(l,j)
                       end do
                       if (beta==zero) then
                           c(j,j) = alpha*rtemp
                       else
                           c(j,j) = alpha*rtemp + beta*real(c(j,j),KIND=sp)
                       end if
                   end do
               else
                   do j = 1,n
                       rtemp = zero
                       do l = 1,k
                           rtemp = rtemp + conjg(a(l,j))*a(l,j)
                       end do
                       if (beta==zero) then
                           c(j,j) = alpha*rtemp
                       else
                           c(j,j) = alpha*rtemp + beta*real(c(j,j),KIND=sp)
                       end if
                       do i = j + 1,n
                           temp = zero
                           do l = 1,k
                               temp = temp + conjg(a(l,i))*a(l,j)
                           end do
                           if (beta==zero) then
                               c(i,j) = alpha*temp
                           else
                               c(i,j) = alpha*temp + beta*c(i,j)
                           end if
                       end do
                   end do
               end if
           end if
           return
     end subroutine stdlib_cherk

     pure module subroutine stdlib_zherk(uplo,trans,n,k,alpha,a,lda,beta,c,ldc)
     use stdlib_blas_constants_dp
     !! ZHERK performs one of the hermitian rank k operations
     !! C := alpha*A*A**H + beta*C,
     !! or
     !! C := alpha*A**H*A + beta*C,
     !! where  alpha and beta  are  real scalars,  C is an  n by n  hermitian
     !! matrix and  A  is an  n by k  matrix in the  first case and a  k by n
     !! matrix in the second case.
        ! -- reference blas level3 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           real(dp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: k, lda, ldc, n
           character, intent(in) :: trans, uplo
           ! Array Arguments 
           complex(dp), intent(in) :: a(lda,*)
           complex(dp), intent(inout) :: c(ldc,*)
        ! =====================================================================
           ! Intrinsic Functions 
           intrinsic :: real,cmplx,conjg,max
           ! Local Scalars 
           complex(dp) :: temp
           real(dp) :: rtemp
           integer(ilp) :: i, info, j, l, nrowa
           logical(lk) :: upper
           
           ! test the input parameters.
           if (stdlib_lsame(trans,'N')) then
               nrowa = n
           else
               nrowa = k
           end if
           upper = stdlib_lsame(uplo,'U')
           info = 0
           if ((.not.upper) .and. (.not.stdlib_lsame(uplo,'L'))) then
               info = 1
           else if ((.not.stdlib_lsame(trans,'N')) .and.(.not.stdlib_lsame(trans,'C'))) &
                     then
               info = 2
           else if (n<0) then
               info = 3
           else if (k<0) then
               info = 4
           else if (lda<max(1,nrowa)) then
               info = 7
           else if (ldc<max(1,n)) then
               info = 10
           end if
           if (info/=0) then
               call stdlib_xerbla('ZHERK ',info)
               return
           end if
           ! quick return if possible.
           if ((n==0) .or. (((alpha==zero).or.(k==0)).and. (beta==one))) return
           ! and when  alpha.eq.zero.
           if (alpha==zero) then
               if (upper) then
                   if (beta==zero) then
                       do j = 1,n
                           do i = 1,j
                               c(i,j) = zero
                           end do
                       end do
                   else
                       do j = 1,n
                           do i = 1,j - 1
                               c(i,j) = beta*c(i,j)
                           end do
                           c(j,j) = beta*real(c(j,j),KIND=dp)
                       end do
                   end if
               else
                   if (beta==zero) then
                       do j = 1,n
                           do i = j,n
                               c(i,j) = zero
                           end do
                       end do
                   else
                       do j = 1,n
                           c(j,j) = beta*real(c(j,j),KIND=dp)
                           do i = j + 1,n
                               c(i,j) = beta*c(i,j)
                           end do
                       end do
                   end if
               end if
               return
           end if
           ! start the operations.
           if (stdlib_lsame(trans,'N')) then
              ! form  c := alpha*a*a**h + beta*c.
               if (upper) then
                   do j = 1,n
                       if (beta==zero) then
                           do i = 1,j
                               c(i,j) = zero
                           end do
                       else if (beta/=one) then
                           do i = 1,j - 1
                               c(i,j) = beta*c(i,j)
                           end do
                           c(j,j) = beta*real(c(j,j),KIND=dp)
                       else
                           c(j,j) = real(c(j,j),KIND=dp)
                       end if
                       do l = 1,k
                           if (a(j,l)/=cmplx(zero,KIND=dp)) then
                               temp = alpha*conjg(a(j,l))
                               do i = 1,j - 1
                                   c(i,j) = c(i,j) + temp*a(i,l)
                               end do
                               c(j,j) = real(c(j,j),KIND=dp) + real(temp*a(i,l),KIND=dp)
                           end if
                       end do
                   end do
               else
                   do j = 1,n
                       if (beta==zero) then
                           do i = j,n
                               c(i,j) = zero
                           end do
                       else if (beta/=one) then
                           c(j,j) = beta*real(c(j,j),KIND=dp)
                           do i = j + 1,n
                               c(i,j) = beta*c(i,j)
                           end do
                       else
                           c(j,j) = real(c(j,j),KIND=dp)
                       end if
                       do l = 1,k
                           if (a(j,l)/=cmplx(zero,KIND=dp)) then
                               temp = alpha*conjg(a(j,l))
                               c(j,j) = real(c(j,j),KIND=dp) + real(temp*a(j,l),KIND=dp)
                               do i = j + 1,n
                                   c(i,j) = c(i,j) + temp*a(i,l)
                               end do
                           end if
                       end do
                   end do
               end if
           else
              ! form  c := alpha*a**h*a + beta*c.
               if (upper) then
                   do j = 1,n
                       do i = 1,j - 1
                           temp = zero
                           do l = 1,k
                               temp = temp + conjg(a(l,i))*a(l,j)
                           end do
                           if (beta==zero) then
                               c(i,j) = alpha*temp
                           else
                               c(i,j) = alpha*temp + beta*c(i,j)
                           end if
                       end do
                       rtemp = zero
                       do l = 1,k
                           rtemp = rtemp + conjg(a(l,j))*a(l,j)
                       end do
                       if (beta==zero) then
                           c(j,j) = alpha*rtemp
                       else
                           c(j,j) = alpha*rtemp + beta*real(c(j,j),KIND=dp)
                       end if
                   end do
               else
                   do j = 1,n
                       rtemp = zero
                       do l = 1,k
                           rtemp = rtemp + conjg(a(l,j))*a(l,j)
                       end do
                       if (beta==zero) then
                           c(j,j) = alpha*rtemp
                       else
                           c(j,j) = alpha*rtemp + beta*real(c(j,j),KIND=dp)
                       end if
                       do i = j + 1,n
                           temp = zero
                           do l = 1,k
                               temp = temp + conjg(a(l,i))*a(l,j)
                           end do
                           if (beta==zero) then
                               c(i,j) = alpha*temp
                           else
                               c(i,j) = alpha*temp + beta*c(i,j)
                           end if
                       end do
                   end do
               end if
           end if
           return
     end subroutine stdlib_zherk




     pure module subroutine stdlib_cher2k(uplo,trans,n,k,alpha,a,lda,b,ldb,beta,c,ldc)
     use stdlib_blas_constants_sp
     !! CHER2K performs one of the hermitian rank 2k operations
     !! C := alpha*A*B**H + conjg( alpha )*B*A**H + beta*C,
     !! or
     !! C := alpha*A**H*B + conjg( alpha )*B**H*A + beta*C,
     !! where  alpha and beta  are scalars with  beta  real,  C is an  n by n
     !! hermitian matrix and  A and B  are  n by k matrices in the first case
     !! and  k by n  matrices in the second case.
        ! -- reference blas level3 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           complex(sp), intent(in) :: alpha
           real(sp), intent(in) :: beta
           integer(ilp), intent(in) :: k, lda, ldb, ldc, n
           character, intent(in) :: trans, uplo
           ! Array Arguments 
           complex(sp), intent(in) :: a(lda,*), b(ldb,*)
           complex(sp), intent(inout) :: c(ldc,*)
        ! =====================================================================
           ! Intrinsic Functions 
           intrinsic :: conjg,max,real
           ! Local Scalars 
           complex(sp) :: temp1, temp2
           integer(ilp) :: i, info, j, l, nrowa
           logical(lk) :: upper
           
           
           ! test the input parameters.
           if (stdlib_lsame(trans,'N')) then
               nrowa = n
           else
               nrowa = k
           end if
           upper = stdlib_lsame(uplo,'U')
           info = 0
           if ((.not.upper) .and. (.not.stdlib_lsame(uplo,'L'))) then
               info = 1
           else if ((.not.stdlib_lsame(trans,'N')) .and.(.not.stdlib_lsame(trans,'C'))) &
                     then
               info = 2
           else if (n<0) then
               info = 3
           else if (k<0) then
               info = 4
           else if (lda<max(1,nrowa)) then
               info = 7
           else if (ldb<max(1,nrowa)) then
               info = 9
           else if (ldc<max(1,n)) then
               info = 12
           end if
           if (info/=0) then
               call stdlib_xerbla('CHER2K',info)
               return
           end if
           ! quick return if possible.
           if ((n==0) .or. (((alpha==czero).or.(k==0)).and. (beta==one))) return
           ! and when  alpha.eq.czero.
           if (alpha==czero) then
               if (upper) then
                   if (beta==real(czero,KIND=sp)) then
                       do j = 1,n
                           do i = 1,j
                               c(i,j) = czero
                           end do
                       end do
                   else
                       do j = 1,n
                           do i = 1,j - 1
                               c(i,j) = beta*c(i,j)
                           end do
                           c(j,j) = beta*real(c(j,j),KIND=sp)
                       end do
                   end if
               else
                   if (beta==real(czero,KIND=sp)) then
                       do j = 1,n
                           do i = j,n
                               c(i,j) = czero
                           end do
                       end do
                   else
                       do j = 1,n
                           c(j,j) = beta*real(c(j,j),KIND=sp)
                           do i = j + 1,n
                               c(i,j) = beta*c(i,j)
                           end do
                       end do
                   end if
               end if
               return
           end if
           ! start the operations.
           if (stdlib_lsame(trans,'N')) then
              ! form  c := alpha*a*b**h + conjg( alpha )*b*a**h +
                         ! c.
               if (upper) then
                   do j = 1,n
                       if (beta==real(czero,KIND=sp)) then
                           do i = 1,j
                               c(i,j) = czero
                           end do
                       else if (beta/=one) then
                           do i = 1,j - 1
                               c(i,j) = beta*c(i,j)
                           end do
                           c(j,j) = beta*real(c(j,j),KIND=sp)
                       else
                           c(j,j) = real(c(j,j),KIND=sp)
                       end if
                       do l = 1,k
                           if ((a(j,l)/=czero) .or. (b(j,l)/=czero)) then
                               temp1 = alpha*conjg(b(j,l))
                               temp2 = conjg(alpha*a(j,l))
                               do i = 1,j - 1
                                   c(i,j) = c(i,j) + a(i,l)*temp1 +b(i,l)*temp2
                               end do
                               c(j,j) = real(c(j,j),KIND=sp) +real(a(j,l)*temp1+b(j,l)*temp2,&
                                         KIND=sp)
                           end if
                       end do
                   end do
               else
                   do j = 1,n
                       if (beta==real(czero,KIND=sp)) then
                           do i = j,n
                               c(i,j) = czero
                           end do
                       else if (beta/=one) then
                           do i = j + 1,n
                               c(i,j) = beta*c(i,j)
                           end do
                           c(j,j) = beta*real(c(j,j),KIND=sp)
                       else
                           c(j,j) = real(c(j,j),KIND=sp)
                       end if
                       do l = 1,k
                           if ((a(j,l)/=czero) .or. (b(j,l)/=czero)) then
                               temp1 = alpha*conjg(b(j,l))
                               temp2 = conjg(alpha*a(j,l))
                               do i = j + 1,n
                                   c(i,j) = c(i,j) + a(i,l)*temp1 +b(i,l)*temp2
                               end do
                               c(j,j) = real(c(j,j),KIND=sp) +real(a(j,l)*temp1+b(j,l)*temp2,&
                                         KIND=sp)
                           end if
                       end do
                   end do
               end if
           else
              ! form  c := alpha*a**h*b + conjg( alpha )*b**h*a +
                         ! c.
               if (upper) then
                   do j = 1,n
                       do i = 1,j
                           temp1 = czero
                           temp2 = czero
                           do l = 1,k
                               temp1 = temp1 + conjg(a(l,i))*b(l,j)
                               temp2 = temp2 + conjg(b(l,i))*a(l,j)
                           end do
                           if (i==j) then
                               if (beta==real(czero,KIND=sp)) then
                                   c(j,j) = real(alpha*temp1+conjg(alpha)*temp2,KIND=sp)
                               else
                                   c(j,j) = beta*real(c(j,j),KIND=sp) +real(alpha*temp1+conjg(&
                                             alpha)*temp2,KIND=sp)
                               end if
                           else
                               if (beta==real(czero,KIND=sp)) then
                                   c(i,j) = alpha*temp1 + conjg(alpha)*temp2
                               else
                                   c(i,j) = beta*c(i,j) + alpha*temp1 +conjg(alpha)*temp2
                               end if
                           end if
                       end do
                   end do
               else
                   do j = 1,n
                       do i = j,n
                           temp1 = czero
                           temp2 = czero
                           do l = 1,k
                               temp1 = temp1 + conjg(a(l,i))*b(l,j)
                               temp2 = temp2 + conjg(b(l,i))*a(l,j)
                           end do
                           if (i==j) then
                               if (beta==real(czero,KIND=sp)) then
                                   c(j,j) = real(alpha*temp1+conjg(alpha)*temp2,KIND=sp)
                               else
                                   c(j,j) = beta*real(c(j,j),KIND=sp) +real(alpha*temp1+conjg(&
                                             alpha)*temp2,KIND=sp)
                               end if
                           else
                               if (beta==real(czero,KIND=sp)) then
                                   c(i,j) = alpha*temp1 + conjg(alpha)*temp2
                               else
                                   c(i,j) = beta*c(i,j) + alpha*temp1 +conjg(alpha)*temp2
                               end if
                           end if
                       end do
                   end do
               end if
           end if
           return
     end subroutine stdlib_cher2k

     pure module subroutine stdlib_zher2k(uplo,trans,n,k,alpha,a,lda,b,ldb,beta,c,ldc)
     use stdlib_blas_constants_dp
     !! ZHER2K performs one of the hermitian rank 2k operations
     !! C := alpha*A*B**H + conjg( alpha )*B*A**H + beta*C,
     !! or
     !! C := alpha*A**H*B + conjg( alpha )*B**H*A + beta*C,
     !! where  alpha and beta  are scalars with  beta  real,  C is an  n by n
     !! hermitian matrix and  A and B  are  n by k matrices in the first case
     !! and  k by n  matrices in the second case.
        ! -- reference blas level3 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           complex(dp), intent(in) :: alpha
           real(dp), intent(in) :: beta
           integer(ilp), intent(in) :: k, lda, ldb, ldc, n
           character, intent(in) :: trans, uplo
           ! Array Arguments 
           complex(dp), intent(in) :: a(lda,*), b(ldb,*)
           complex(dp), intent(inout) :: c(ldc,*)
        ! =====================================================================
           ! Intrinsic Functions 
           intrinsic :: real,conjg,max
           ! Local Scalars 
           complex(dp) :: temp1, temp2
           integer(ilp) :: i, info, j, l, nrowa
           logical(lk) :: upper
           
           
           ! test the input parameters.
           if (stdlib_lsame(trans,'N')) then
               nrowa = n
           else
               nrowa = k
           end if
           upper = stdlib_lsame(uplo,'U')
           info = 0
           if ((.not.upper) .and. (.not.stdlib_lsame(uplo,'L'))) then
               info = 1
           else if ((.not.stdlib_lsame(trans,'N')) .and.(.not.stdlib_lsame(trans,'C'))) &
                     then
               info = 2
           else if (n<0) then
               info = 3
           else if (k<0) then
               info = 4
           else if (lda<max(1,nrowa)) then
               info = 7
           else if (ldb<max(1,nrowa)) then
               info = 9
           else if (ldc<max(1,n)) then
               info = 12
           end if
           if (info/=0) then
               call stdlib_xerbla('ZHER2K',info)
               return
           end if
           ! quick return if possible.
           if ((n==0) .or. (((alpha==czero).or.(k==0)).and. (beta==one))) return
           ! and when  alpha.eq.czero.
           if (alpha==czero) then
               if (upper) then
                   if (beta==real(czero,KIND=dp)) then
                       do j = 1,n
                           do i = 1,j
                               c(i,j) = czero
                           end do
                       end do
                   else
                       do j = 1,n
                           do i = 1,j - 1
                               c(i,j) = beta*c(i,j)
                           end do
                           c(j,j) = beta*real(c(j,j),KIND=dp)
                       end do
                   end if
               else
                   if (beta==real(czero,KIND=dp)) then
                       do j = 1,n
                           do i = j,n
                               c(i,j) = czero
                           end do
                       end do
                   else
                       do j = 1,n
                           c(j,j) = beta*real(c(j,j),KIND=dp)
                           do i = j + 1,n
                               c(i,j) = beta*c(i,j)
                           end do
                       end do
                   end if
               end if
               return
           end if
           ! start the operations.
           if (stdlib_lsame(trans,'N')) then
              ! form  c := alpha*a*b**h + conjg( alpha )*b*a**h +
                         ! c.
               if (upper) then
                   do j = 1,n
                       if (beta==real(czero,KIND=dp)) then
                           do i = 1,j
                               c(i,j) = czero
                           end do
                       else if (beta/=one) then
                           do i = 1,j - 1
                               c(i,j) = beta*c(i,j)
                           end do
                           c(j,j) = beta*real(c(j,j),KIND=dp)
                       else
                           c(j,j) = real(c(j,j),KIND=dp)
                       end if
                       do l = 1,k
                           if ((a(j,l)/=czero) .or. (b(j,l)/=czero)) then
                               temp1 = alpha*conjg(b(j,l))
                               temp2 = conjg(alpha*a(j,l))
                               do i = 1,j - 1
                                   c(i,j) = c(i,j) + a(i,l)*temp1 +b(i,l)*temp2
                               end do
                               c(j,j) = real(c(j,j),KIND=dp) +real(a(j,l)*temp1+b(j,l)*temp2,&
                                         KIND=dp)
                           end if
                       end do
                   end do
               else
                   do j = 1,n
                       if (beta==real(czero,KIND=dp)) then
                           do i = j,n
                               c(i,j) = czero
                           end do
                       else if (beta/=one) then
                           do i = j + 1,n
                               c(i,j) = beta*c(i,j)
                           end do
                           c(j,j) = beta*real(c(j,j),KIND=dp)
                       else
                           c(j,j) = real(c(j,j),KIND=dp)
                       end if
                       do l = 1,k
                           if ((a(j,l)/=czero) .or. (b(j,l)/=czero)) then
                               temp1 = alpha*conjg(b(j,l))
                               temp2 = conjg(alpha*a(j,l))
                               do i = j + 1,n
                                   c(i,j) = c(i,j) + a(i,l)*temp1 +b(i,l)*temp2
                               end do
                               c(j,j) = real(c(j,j),KIND=dp) +real(a(j,l)*temp1+b(j,l)*temp2,&
                                         KIND=dp)
                           end if
                       end do
                   end do
               end if
           else
              ! form  c := alpha*a**h*b + conjg( alpha )*b**h*a +
                         ! c.
               if (upper) then
                   do j = 1,n
                       do i = 1,j
                           temp1 = czero
                           temp2 = czero
                           do l = 1,k
                               temp1 = temp1 + conjg(a(l,i))*b(l,j)
                               temp2 = temp2 + conjg(b(l,i))*a(l,j)
                           end do
                           if (i==j) then
                               if (beta==real(czero,KIND=dp)) then
                                   c(j,j) = real(alpha*temp1+conjg(alpha)*temp2,KIND=dp)
                               else
                                   c(j,j) = beta*real(c(j,j),KIND=dp) +real(alpha*temp1+conjg(&
                                             alpha)*temp2,KIND=dp)
                               end if
                           else
                               if (beta==real(czero,KIND=dp)) then
                                   c(i,j) = alpha*temp1 + conjg(alpha)*temp2
                               else
                                   c(i,j) = beta*c(i,j) + alpha*temp1 +conjg(alpha)*temp2
                               end if
                           end if
                       end do
                   end do
               else
                   do j = 1,n
                       do i = j,n
                           temp1 = czero
                           temp2 = czero
                           do l = 1,k
                               temp1 = temp1 + conjg(a(l,i))*b(l,j)
                               temp2 = temp2 + conjg(b(l,i))*a(l,j)
                           end do
                           if (i==j) then
                               if (beta==real(czero,KIND=dp)) then
                                   c(j,j) = real(alpha*temp1+conjg(alpha)*temp2,KIND=dp)
                               else
                                   c(j,j) = beta*real(c(j,j),KIND=dp) +real(alpha*temp1+conjg(&
                                             alpha)*temp2,KIND=dp)
                               end if
                           else
                               if (beta==real(czero,KIND=dp)) then
                                   c(i,j) = alpha*temp1 + conjg(alpha)*temp2
                               else
                                   c(i,j) = beta*c(i,j) + alpha*temp1 +conjg(alpha)*temp2
                               end if
                           end if
                       end do
                   end do
               end if
           end if
           return
     end subroutine stdlib_zher2k




     pure module subroutine stdlib_I64_sgemm(transa,transb,m,n,k,alpha,a,lda,b,ldb,beta,c,ldc)
     use stdlib_blas_constants_sp
     !! SGEMM performs one of the matrix-matrix operations
     !! C := alpha*op( A )*op( B ) + beta*C,
     !! where  op( X ) is one of
     !! op( X ) = X   or   op( X ) = X**T,
     !! alpha and beta are scalars, and A, B and C are matrices, with op( A )
     !! an m by k matrix,  op( B )  a  k by n matrix and  C an m by n matrix.
        ! -- reference blas level3 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           real(sp), intent(in) :: alpha, beta
           integer(ilp64), intent(in) :: k, lda, ldb, ldc, m, n
           character, intent(in) :: transa, transb
           ! Array Arguments 
           real(sp), intent(in) :: a(lda,*), b(ldb,*)
           real(sp), intent(inout) :: c(ldc,*)
        ! =====================================================================
           ! Intrinsic Functions 
           intrinsic :: max
           ! Local Scalars 
           real(sp) :: temp
           integer(ilp64) :: i, info, j, l, nrowa, nrowb
           logical(lk) :: nota, notb
           
           ! set  nota  and  notb  as  true if  a  and  b  respectively are not
           ! transposed and set  nrowa and nrowb  as the number of rows of  a
           ! and  b  respectively.
           nota = stdlib_lsame(transa,'N')
           notb = stdlib_lsame(transb,'N')
           if (nota) then
               nrowa = m
           else
               nrowa = k
           end if
           if (notb) then
               nrowb = k
           else
               nrowb = n
           end if
           ! test the input parameters.
           info = 0
           if ((.not.nota) .and. (.not.stdlib_lsame(transa,'C')) .and.(.not.stdlib_lsame(transa,&
                     'T'))) then
               info = 1
           else if ((.not.notb) .and. (.not.stdlib_lsame(transb,'C')) .and.(.not.stdlib_lsame(&
                     transb,'T'))) then
               info = 2
           else if (m<0) then
               info = 3
           else if (n<0) then
               info = 4
           else if (k<0) then
               info = 5
           else if (lda<max(1,nrowa)) then
               info = 8
           else if (ldb<max(1,nrowb)) then
               info = 10
           else if (ldc<max(1,m)) then
               info = 13
           end if
           if (info/=0) then
               call stdlib_I64_xerbla('SGEMM ',info)
               return
           end if
           ! quick return if possible.
           if ((m==0) .or. (n==0) .or.(((alpha==zero).or. (k==0)).and. (beta==one))) &
                     return
           ! and if  alpha.eq.zero.
           if (alpha==zero) then
               if (beta==zero) then
                   do j = 1,n
                       do i = 1,m
                           c(i,j) = zero
                       end do
                   end do
               else
                   do j = 1,n
                       do i = 1,m
                           c(i,j) = beta*c(i,j)
                       end do
                   end do
               end if
               return
           end if
           ! start the operations.
           if (notb) then
               if (nota) then
                 ! form  c := alpha*a*b + beta*c.
                   do j = 1,n
                       if (beta==zero) then
                           do i = 1,m
                               c(i,j) = zero
                           end do
                       else if (beta/=one) then
                           do i = 1,m
                               c(i,j) = beta*c(i,j)
                           end do
                       end if
                       do l = 1,k
                           temp = alpha*b(l,j)
                           do i = 1,m
                               c(i,j) = c(i,j) + temp*a(i,l)
                           end do
                       end do
                   end do
               else
                 ! form  c := alpha*a**t*b + beta*c
                   do j = 1,n
                       do i = 1,m
                           temp = zero
                           do l = 1,k
                               temp = temp + a(l,i)*b(l,j)
                           end do
                           if (beta==zero) then
                               c(i,j) = alpha*temp
                           else
                               c(i,j) = alpha*temp + beta*c(i,j)
                           end if
                       end do
                   end do
               end if
           else
               if (nota) then
                 ! form  c := alpha*a*b**t + beta*c
                   do j = 1,n
                       if (beta==zero) then
                           do i = 1,m
                               c(i,j) = zero
                           end do
                       else if (beta/=one) then
                           do i = 1,m
                               c(i,j) = beta*c(i,j)
                           end do
                       end if
                       do l = 1,k
                           temp = alpha*b(j,l)
                           do i = 1,m
                               c(i,j) = c(i,j) + temp*a(i,l)
                           end do
                       end do
                   end do
               else
                 ! form  c := alpha*a**t*b**t + beta*c
                   do j = 1,n
                       do i = 1,m
                           temp = zero
                           do l = 1,k
                               temp = temp + a(l,i)*b(j,l)
                           end do
                           if (beta==zero) then
                               c(i,j) = alpha*temp
                           else
                               c(i,j) = alpha*temp + beta*c(i,j)
                           end if
                       end do
                   end do
               end if
           end if
           return
     end subroutine stdlib_I64_sgemm

     pure module subroutine stdlib_I64_dgemm(transa,transb,m,n,k,alpha,a,lda,b,ldb,beta,c,ldc)
     use stdlib_blas_constants_dp
     !! DGEMM performs one of the matrix-matrix operations
     !! C := alpha*op( A )*op( B ) + beta*C,
     !! where  op( X ) is one of
     !! op( X ) = X   or   op( X ) = X**T,
     !! alpha and beta are scalars, and A, B and C are matrices, with op( A )
     !! an m by k matrix,  op( B )  a  k by n matrix and  C an m by n matrix.
        ! -- reference blas level3 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           real(dp), intent(in) :: alpha, beta
           integer(ilp64), intent(in) :: k, lda, ldb, ldc, m, n
           character, intent(in) :: transa, transb
           ! Array Arguments 
           real(dp), intent(in) :: a(lda,*), b(ldb,*)
           real(dp), intent(inout) :: c(ldc,*)
        ! =====================================================================
           ! Intrinsic Functions 
           intrinsic :: max
           ! Local Scalars 
           real(dp) :: temp
           integer(ilp64) :: i, info, j, l, nrowa, nrowb
           logical(lk) :: nota, notb
           
           ! set  nota  and  notb  as  true if  a  and  b  respectively are not
           ! transposed and set  nrowa and nrowb  as the number of rows of  a
           ! and  b  respectively.
           nota = stdlib_lsame(transa,'N')
           notb = stdlib_lsame(transb,'N')
           if (nota) then
               nrowa = m
           else
               nrowa = k
           end if
           if (notb) then
               nrowb = k
           else
               nrowb = n
           end if
           ! test the input parameters.
           info = 0
           if ((.not.nota) .and. (.not.stdlib_lsame(transa,'C')) .and.(.not.stdlib_lsame(transa,&
                     'T'))) then
               info = 1
           else if ((.not.notb) .and. (.not.stdlib_lsame(transb,'C')) .and.(.not.stdlib_lsame(&
                     transb,'T'))) then
               info = 2
           else if (m<0) then
               info = 3
           else if (n<0) then
               info = 4
           else if (k<0) then
               info = 5
           else if (lda<max(1,nrowa)) then
               info = 8
           else if (ldb<max(1,nrowb)) then
               info = 10
           else if (ldc<max(1,m)) then
               info = 13
           end if
           if (info/=0) then
               call stdlib_I64_xerbla('DGEMM ',info)
               return
           end if
           ! quick return if possible.
           if ((m==0) .or. (n==0) .or.(((alpha==zero).or. (k==0)).and. (beta==one))) &
                     return
           ! and if  alpha.eq.zero.
           if (alpha==zero) then
               if (beta==zero) then
                   do j = 1,n
                       do i = 1,m
                           c(i,j) = zero
                       end do
                   end do
               else
                   do j = 1,n
                       do i = 1,m
                           c(i,j) = beta*c(i,j)
                       end do
                   end do
               end if
               return
           end if
           ! start the operations.
           if (notb) then
               if (nota) then
                 ! form  c := alpha*a*b + beta*c.
                   do j = 1,n
                       if (beta==zero) then
                           do i = 1,m
                               c(i,j) = zero
                           end do
                       else if (beta/=one) then
                           do i = 1,m
                               c(i,j) = beta*c(i,j)
                           end do
                       end if
                       do l = 1,k
                           temp = alpha*b(l,j)
                           do i = 1,m
                               c(i,j) = c(i,j) + temp*a(i,l)
                           end do
                       end do
                   end do
               else
                 ! form  c := alpha*a**t*b + beta*c
                   do j = 1,n
                       do i = 1,m
                           temp = zero
                           do l = 1,k
                               temp = temp + a(l,i)*b(l,j)
                           end do
                           if (beta==zero) then
                               c(i,j) = alpha*temp
                           else
                               c(i,j) = alpha*temp + beta*c(i,j)
                           end if
                       end do
                   end do
               end if
           else
               if (nota) then
                 ! form  c := alpha*a*b**t + beta*c
                   do j = 1,n
                       if (beta==zero) then
                           do i = 1,m
                               c(i,j) = zero
                           end do
                       else if (beta/=one) then
                           do i = 1,m
                               c(i,j) = beta*c(i,j)
                           end do
                       end if
                       do l = 1,k
                           temp = alpha*b(j,l)
                           do i = 1,m
                               c(i,j) = c(i,j) + temp*a(i,l)
                           end do
                       end do
                   end do
               else
                 ! form  c := alpha*a**t*b**t + beta*c
                   do j = 1,n
                       do i = 1,m
                           temp = zero
                           do l = 1,k
                               temp = temp + a(l,i)*b(j,l)
                           end do
                           if (beta==zero) then
                               c(i,j) = alpha*temp
                           else
                               c(i,j) = alpha*temp + beta*c(i,j)
                           end if
                       end do
                   end do
               end if
           end if
           return
     end subroutine stdlib_I64_dgemm


     pure module subroutine stdlib_I64_cgemm(transa,transb,m,n,k,alpha,a,lda,b,ldb,beta,c,ldc)
     use stdlib_blas_constants_sp
     !! CGEMM performs one of the matrix-matrix operations
     !! C := alpha*op( A )*op( B ) + beta*C,
     !! where  op( X ) is one of
     !! op( X ) = X   or   op( X ) = X**T   or   op( X ) = X**H,
     !! alpha and beta are scalars, and A, B and C are matrices, with op( A )
     !! an m by k matrix,  op( B )  a  k by n matrix and  C an m by n matrix.
        ! -- reference blas level3 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           complex(sp), intent(in) :: alpha, beta
           integer(ilp64), intent(in) :: k, lda, ldb, ldc, m, n
           character, intent(in) :: transa, transb
           ! Array Arguments 
           complex(sp), intent(in) :: a(lda,*), b(ldb,*)
           complex(sp), intent(inout) :: c(ldc,*)
        ! =====================================================================
           ! Intrinsic Functions 
           intrinsic :: conjg,max
           ! Local Scalars 
           complex(sp) :: temp
           integer(ilp64) :: i, info, j, l, nrowa, nrowb
           logical(lk) :: conja, conjb, nota, notb
           
           
           ! set  nota  and  notb  as  true if  a  and  b  respectively are not
           ! conjugated or transposed, set  conja and conjb  as true if  a  and
           ! b  respectively are to be  transposed but  not conjugated  and set
           ! nrowa and  nrowb  as the number of rows of  a  and  b  respectively.
           nota = stdlib_lsame(transa,'N')
           notb = stdlib_lsame(transb,'N')
           conja = stdlib_lsame(transa,'C')
           conjb = stdlib_lsame(transb,'C')
           if (nota) then
               nrowa = m
           else
               nrowa = k
           end if
           if (notb) then
               nrowb = k
           else
               nrowb = n
           end if
           ! test the input parameters.
           info = 0
           if ((.not.nota) .and. (.not.conja) .and.(.not.stdlib_lsame(transa,'T'))) then
               info = 1
           else if ((.not.notb) .and. (.not.conjb) .and.(.not.stdlib_lsame(transb,'T'))) &
                     then
               info = 2
           else if (m<0) then
               info = 3
           else if (n<0) then
               info = 4
           else if (k<0) then
               info = 5
           else if (lda<max(1,nrowa)) then
               info = 8
           else if (ldb<max(1,nrowb)) then
               info = 10
           else if (ldc<max(1,m)) then
               info = 13
           end if
           if (info/=0) then
               call stdlib_I64_xerbla('CGEMM ',info)
               return
           end if
           ! quick return if possible.
           if ((m==0) .or. (n==0) .or.(((alpha==czero).or. (k==0)).and. (beta==cone))) &
                     return
           ! and when  alpha.eq.czero.
           if (alpha==czero) then
               if (beta==czero) then
                   do j = 1,n
                       do i = 1,m
                           c(i,j) = czero
                       end do
                   end do
               else
                   do j = 1,n
                       do i = 1,m
                           c(i,j) = beta*c(i,j)
                       end do
                   end do
               end if
               return
           end if
           ! start the operations.
           if (notb) then
               if (nota) then
                 ! form  c := alpha*a*b + beta*c.
                   do j = 1,n
                       if (beta==czero) then
                           do i = 1,m
                               c(i,j) = czero
                           end do
                       else if (beta/=cone) then
                           do i = 1,m
                               c(i,j) = beta*c(i,j)
                           end do
                       end if
                       do l = 1,k
                           temp = alpha*b(l,j)
                           do i = 1,m
                               c(i,j) = c(i,j) + temp*a(i,l)
                           end do
                       end do
                   end do
               else if (conja) then
                 ! form  c := alpha*a**h*b + beta*c.
                   do j = 1,n
                       do i = 1,m
                           temp = czero
                           do l = 1,k
                               temp = temp + conjg(a(l,i))*b(l,j)
                           end do
                           if (beta==czero) then
                               c(i,j) = alpha*temp
                           else
                               c(i,j) = alpha*temp + beta*c(i,j)
                           end if
                       end do
                   end do
               else
                 ! form  c := alpha*a**t*b + beta*c
                   do j = 1,n
                       do i = 1,m
                           temp = czero
                           do l = 1,k
                               temp = temp + a(l,i)*b(l,j)
                           end do
                           if (beta==czero) then
                               c(i,j) = alpha*temp
                           else
                               c(i,j) = alpha*temp + beta*c(i,j)
                           end if
                       end do
                   end do
               end if
           else if (nota) then
               if (conjb) then
                 ! form  c := alpha*a*b**h + beta*c.
                   do j = 1,n
                       if (beta==czero) then
                           do i = 1,m
                               c(i,j) = czero
                           end do
                       else if (beta/=cone) then
                           do i = 1,m
                               c(i,j) = beta*c(i,j)
                           end do
                       end if
                       do l = 1,k
                           temp = alpha*conjg(b(j,l))
                           do i = 1,m
                               c(i,j) = c(i,j) + temp*a(i,l)
                           end do
                       end do
                   end do
               else
                 ! form  c := alpha*a*b**t + beta*c
                   do j = 1,n
                       if (beta==czero) then
                           do i = 1,m
                               c(i,j) = czero
                           end do
                       else if (beta/=cone) then
                           do i = 1,m
                               c(i,j) = beta*c(i,j)
                           end do
                       end if
                       do l = 1,k
                           temp = alpha*b(j,l)
                           do i = 1,m
                               c(i,j) = c(i,j) + temp*a(i,l)
                           end do
                       end do
                   end do
               end if
           else if (conja) then
               if (conjb) then
                 ! form  c := alpha*a**h*b**h + beta*c.
                   do j = 1,n
                       do i = 1,m
                           temp = czero
                           do l = 1,k
                               temp = temp + conjg(a(l,i))*conjg(b(j,l))
                           end do
                           if (beta==czero) then
                               c(i,j) = alpha*temp
                           else
                               c(i,j) = alpha*temp + beta*c(i,j)
                           end if
                       end do
                   end do
               else
                 ! form  c := alpha*a**h*b**t + beta*c
                   do j = 1,n
                       do i = 1,m
                           temp = czero
                           do l = 1,k
                               temp = temp + conjg(a(l,i))*b(j,l)
                           end do
                           if (beta==czero) then
                               c(i,j) = alpha*temp
                           else
                               c(i,j) = alpha*temp + beta*c(i,j)
                           end if
                       end do
                   end do
               end if
           else
               if (conjb) then
                 ! form  c := alpha*a**t*b**h + beta*c
                   do j = 1,n
                       do i = 1,m
                           temp = czero
                           do l = 1,k
                               temp = temp + a(l,i)*conjg(b(j,l))
                           end do
                           if (beta==czero) then
                               c(i,j) = alpha*temp
                           else
                               c(i,j) = alpha*temp + beta*c(i,j)
                           end if
                       end do
                   end do
               else
                 ! form  c := alpha*a**t*b**t + beta*c
                   do j = 1,n
                       do i = 1,m
                           temp = czero
                           do l = 1,k
                               temp = temp + a(l,i)*b(j,l)
                           end do
                           if (beta==czero) then
                               c(i,j) = alpha*temp
                           else
                               c(i,j) = alpha*temp + beta*c(i,j)
                           end if
                       end do
                   end do
               end if
           end if
           return
     end subroutine stdlib_I64_cgemm

     pure module subroutine stdlib_I64_zgemm(transa,transb,m,n,k,alpha,a,lda,b,ldb,beta,c,ldc)
     use stdlib_blas_constants_dp
     !! ZGEMM performs one of the matrix-matrix operations
     !! C := alpha*op( A )*op( B ) + beta*C,
     !! where  op( X ) is one of
     !! op( X ) = X   or   op( X ) = X**T   or   op( X ) = X**H,
     !! alpha and beta are scalars, and A, B and C are matrices, with op( A )
     !! an m by k matrix,  op( B )  a  k by n matrix and  C an m by n matrix.
        ! -- reference blas level3 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           complex(dp), intent(in) :: alpha, beta
           integer(ilp64), intent(in) :: k, lda, ldb, ldc, m, n
           character, intent(in) :: transa, transb
           ! Array Arguments 
           complex(dp), intent(in) :: a(lda,*), b(ldb,*)
           complex(dp), intent(inout) :: c(ldc,*)
        ! =====================================================================
           ! Intrinsic Functions 
           intrinsic :: conjg,max
           ! Local Scalars 
           complex(dp) :: temp
           integer(ilp64) :: i, info, j, l, nrowa, nrowb
           logical(lk) :: conja, conjb, nota, notb
           
           
           ! set  nota  and  notb  as  true if  a  and  b  respectively are not
           ! conjugated or transposed, set  conja and conjb  as true if  a  and
           ! b  respectively are to be  transposed but  not conjugated  and set
           ! nrowa and nrowb  as the number of rows  of  a  and  b  respectively.
           nota = stdlib_lsame(transa,'N')
           notb = stdlib_lsame(transb,'N')
           conja = stdlib_lsame(transa,'C')
           conjb = stdlib_lsame(transb,'C')
           if (nota) then
               nrowa = m
           else
               nrowa = k
           end if
           if (notb) then
               nrowb = k
           else
               nrowb = n
           end if
           ! test the input parameters.
           info = 0
           if ((.not.nota) .and. (.not.conja) .and.(.not.stdlib_lsame(transa,'T'))) then
               info = 1
           else if ((.not.notb) .and. (.not.conjb) .and.(.not.stdlib_lsame(transb,'T'))) &
                     then
               info = 2
           else if (m<0) then
               info = 3
           else if (n<0) then
               info = 4
           else if (k<0) then
               info = 5
           else if (lda<max(1,nrowa)) then
               info = 8
           else if (ldb<max(1,nrowb)) then
               info = 10
           else if (ldc<max(1,m)) then
               info = 13
           end if
           if (info/=0) then
               call stdlib_I64_xerbla('ZGEMM ',info)
               return
           end if
           ! quick return if possible.
           if ((m==0) .or. (n==0) .or.(((alpha==czero).or. (k==0)).and. (beta==cone))) &
                     return
           ! and when  alpha.eq.czero.
           if (alpha==czero) then
               if (beta==czero) then
                   do j = 1,n
                       do i = 1,m
                           c(i,j) = czero
                       end do
                   end do
               else
                   do j = 1,n
                       do i = 1,m
                           c(i,j) = beta*c(i,j)
                       end do
                   end do
               end if
               return
           end if
           ! start the operations.
           if (notb) then
               if (nota) then
                 ! form  c := alpha*a*b + beta*c.
                   do j = 1,n
                       if (beta==czero) then
                           do i = 1,m
                               c(i,j) = czero
                           end do
                       else if (beta/=cone) then
                           do i = 1,m
                               c(i,j) = beta*c(i,j)
                           end do
                       end if
                       do l = 1,k
                           temp = alpha*b(l,j)
                           do i = 1,m
                               c(i,j) = c(i,j) + temp*a(i,l)
                           end do
                       end do
                   end do
               else if (conja) then
                 ! form  c := alpha*a**h*b + beta*c.
                   do j = 1,n
                       do i = 1,m
                           temp = czero
                           do l = 1,k
                               temp = temp + conjg(a(l,i))*b(l,j)
                           end do
                           if (beta==czero) then
                               c(i,j) = alpha*temp
                           else
                               c(i,j) = alpha*temp + beta*c(i,j)
                           end if
                       end do
                   end do
               else
                 ! form  c := alpha*a**t*b + beta*c
                   do j = 1,n
                       do i = 1,m
                           temp = czero
                           do l = 1,k
                               temp = temp + a(l,i)*b(l,j)
                           end do
                           if (beta==czero) then
                               c(i,j) = alpha*temp
                           else
                               c(i,j) = alpha*temp + beta*c(i,j)
                           end if
                       end do
                   end do
               end if
           else if (nota) then
               if (conjb) then
                 ! form  c := alpha*a*b**h + beta*c.
                   do j = 1,n
                       if (beta==czero) then
                           do i = 1,m
                               c(i,j) = czero
                           end do
                       else if (beta/=cone) then
                           do i = 1,m
                               c(i,j) = beta*c(i,j)
                           end do
                       end if
                       do l = 1,k
                           temp = alpha*conjg(b(j,l))
                           do i = 1,m
                               c(i,j) = c(i,j) + temp*a(i,l)
                           end do
                       end do
                   end do
               else
                 ! form  c := alpha*a*b**t + beta*c
                   do j = 1,n
                       if (beta==czero) then
                           do i = 1,m
                               c(i,j) = czero
                           end do
                       else if (beta/=cone) then
                           do i = 1,m
                               c(i,j) = beta*c(i,j)
                           end do
                       end if
                       do l = 1,k
                           temp = alpha*b(j,l)
                           do i = 1,m
                               c(i,j) = c(i,j) + temp*a(i,l)
                           end do
                       end do
                   end do
               end if
           else if (conja) then
               if (conjb) then
                 ! form  c := alpha*a**h*b**h + beta*c.
                   do j = 1,n
                       do i = 1,m
                           temp = czero
                           do l = 1,k
                               temp = temp + conjg(a(l,i))*conjg(b(j,l))
                           end do
                           if (beta==czero) then
                               c(i,j) = alpha*temp
                           else
                               c(i,j) = alpha*temp + beta*c(i,j)
                           end if
                       end do
                   end do
               else
                 ! form  c := alpha*a**h*b**t + beta*c
                   do j = 1,n
                       do i = 1,m
                           temp = czero
                           do l = 1,k
                               temp = temp + conjg(a(l,i))*b(j,l)
                           end do
                           if (beta==czero) then
                               c(i,j) = alpha*temp
                           else
                               c(i,j) = alpha*temp + beta*c(i,j)
                           end if
                       end do
                   end do
               end if
           else
               if (conjb) then
                 ! form  c := alpha*a**t*b**h + beta*c
                   do j = 1,n
                       do i = 1,m
                           temp = czero
                           do l = 1,k
                               temp = temp + a(l,i)*conjg(b(j,l))
                           end do
                           if (beta==czero) then
                               c(i,j) = alpha*temp
                           else
                               c(i,j) = alpha*temp + beta*c(i,j)
                           end if
                       end do
                   end do
               else
                 ! form  c := alpha*a**t*b**t + beta*c
                   do j = 1,n
                       do i = 1,m
                           temp = czero
                           do l = 1,k
                               temp = temp + a(l,i)*b(j,l)
                           end do
                           if (beta==czero) then
                               c(i,j) = alpha*temp
                           else
                               c(i,j) = alpha*temp + beta*c(i,j)
                           end if
                       end do
                   end do
               end if
           end if
           return
     end subroutine stdlib_I64_zgemm




     pure module subroutine stdlib_I64_chemm(side,uplo,m,n,alpha,a,lda,b,ldb,beta,c,ldc)
     use stdlib_blas_constants_sp
     !! CHEMM performs one of the matrix-matrix operations
     !! C := alpha*A*B + beta*C,
     !! or
     !! C := alpha*B*A + beta*C,
     !! where alpha and beta are scalars, A is an hermitian matrix and  B and
     !! C are m by n matrices.
        ! -- reference blas level3 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           complex(sp), intent(in) :: alpha, beta
           integer(ilp64), intent(in) :: lda, ldb, ldc, m, n
           character, intent(in) :: side, uplo
           ! Array Arguments 
           complex(sp), intent(in) :: a(lda,*), b(ldb,*)
           complex(sp), intent(inout) :: c(ldc,*)
        ! =====================================================================
           ! Intrinsic Functions 
           intrinsic :: conjg,max,real
           ! Local Scalars 
           complex(sp) :: temp1, temp2
           integer(ilp64) :: i, info, j, k, nrowa
           logical(lk) :: upper
           
           
           ! set nrowa as the number of rows of a.
           if (stdlib_lsame(side,'L')) then
               nrowa = m
           else
               nrowa = n
           end if
           upper = stdlib_lsame(uplo,'U')
           ! test the input parameters.
           info = 0
           if ((.not.stdlib_lsame(side,'L')) .and. (.not.stdlib_lsame(side,'R'))) then
               info = 1
           else if ((.not.upper) .and. (.not.stdlib_lsame(uplo,'L'))) then
               info = 2
           else if (m<0) then
               info = 3
           else if (n<0) then
               info = 4
           else if (lda<max(1,nrowa)) then
               info = 7
           else if (ldb<max(1,m)) then
               info = 9
           else if (ldc<max(1,m)) then
               info = 12
           end if
           if (info/=0) then
               call stdlib_I64_xerbla('CHEMM ',info)
               return
           end if
           ! quick return if possible.
           if ((m==0) .or. (n==0) .or.((alpha==czero).and. (beta==cone))) return
           ! and when  alpha.eq.czero.
           if (alpha==czero) then
               if (beta==czero) then
                   do j = 1,n
                       do i = 1,m
                           c(i,j) = czero
                       end do
                   end do
               else
                   do j = 1,n
                       do i = 1,m
                           c(i,j) = beta*c(i,j)
                       end do
                   end do
               end if
               return
           end if
           ! start the operations.
           if (stdlib_lsame(side,'L')) then
              ! form  c := alpha*a*b + beta*c.
               if (upper) then
                   do j = 1,n
                       do i = 1,m
                           temp1 = alpha*b(i,j)
                           temp2 = czero
                           do k = 1,i - 1
                               c(k,j) = c(k,j) + temp1*a(k,i)
                               temp2 = temp2 + b(k,j)*conjg(a(k,i))
                           end do
                           if (beta==czero) then
                               c(i,j) = temp1*real(a(i,i),KIND=sp) + alpha*temp2
                           else
                               c(i,j) = beta*c(i,j) + temp1*real(a(i,i),KIND=sp) +&
                                         alpha*temp2
                           end if
                       end do
                   end do
               else
                   do j = 1,n
                       do i = m,1,-1
                           temp1 = alpha*b(i,j)
                           temp2 = czero
                           do k = i + 1,m
                               c(k,j) = c(k,j) + temp1*a(k,i)
                               temp2 = temp2 + b(k,j)*conjg(a(k,i))
                           end do
                           if (beta==czero) then
                               c(i,j) = temp1*real(a(i,i),KIND=sp) + alpha*temp2
                           else
                               c(i,j) = beta*c(i,j) + temp1*real(a(i,i),KIND=sp) +&
                                         alpha*temp2
                           end if
                       end do
                   end do
               end if
           else
              ! form  c := alpha*b*a + beta*c.
               loop_170: do j = 1,n
                   temp1 = alpha*real(a(j,j),KIND=sp)
                   if (beta==czero) then
                       do i = 1,m
                           c(i,j) = temp1*b(i,j)
                       end do
                   else
                       do i = 1,m
                           c(i,j) = beta*c(i,j) + temp1*b(i,j)
                       end do
                   end if
                   do k = 1,j - 1
                       if (upper) then
                           temp1 = alpha*a(k,j)
                       else
                           temp1 = alpha*conjg(a(j,k))
                       end if
                       do i = 1,m
                           c(i,j) = c(i,j) + temp1*b(i,k)
                       end do
                   end do
                   do k = j + 1,n
                       if (upper) then
                           temp1 = alpha*conjg(a(j,k))
                       else
                           temp1 = alpha*a(k,j)
                       end if
                       do i = 1,m
                           c(i,j) = c(i,j) + temp1*b(i,k)
                       end do
                   end do
               end do loop_170
           end if
           return
     end subroutine stdlib_I64_chemm

     pure module subroutine stdlib_I64_zhemm(side,uplo,m,n,alpha,a,lda,b,ldb,beta,c,ldc)
     use stdlib_blas_constants_dp
     !! ZHEMM performs one of the matrix-matrix operations
     !! C := alpha*A*B + beta*C,
     !! or
     !! C := alpha*B*A + beta*C,
     !! where alpha and beta are scalars, A is an hermitian matrix and  B and
     !! C are m by n matrices.
        ! -- reference blas level3 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           complex(dp), intent(in) :: alpha, beta
           integer(ilp64), intent(in) :: lda, ldb, ldc, m, n
           character, intent(in) :: side, uplo
           ! Array Arguments 
           complex(dp), intent(in) :: a(lda,*), b(ldb,*)
           complex(dp), intent(inout) :: c(ldc,*)
        ! =====================================================================
           ! Intrinsic Functions 
           intrinsic :: real,conjg,max
           ! Local Scalars 
           complex(dp) :: temp1, temp2
           integer(ilp64) :: i, info, j, k, nrowa
           logical(lk) :: upper
           
           
           ! set nrowa as the number of rows of a.
           if (stdlib_lsame(side,'L')) then
               nrowa = m
           else
               nrowa = n
           end if
           upper = stdlib_lsame(uplo,'U')
           ! test the input parameters.
           info = 0
           if ((.not.stdlib_lsame(side,'L')) .and. (.not.stdlib_lsame(side,'R'))) then
               info = 1
           else if ((.not.upper) .and. (.not.stdlib_lsame(uplo,'L'))) then
               info = 2
           else if (m<0) then
               info = 3
           else if (n<0) then
               info = 4
           else if (lda<max(1,nrowa)) then
               info = 7
           else if (ldb<max(1,m)) then
               info = 9
           else if (ldc<max(1,m)) then
               info = 12
           end if
           if (info/=0) then
               call stdlib_I64_xerbla('ZHEMM ',info)
               return
           end if
           ! quick return if possible.
           if ((m==0) .or. (n==0) .or.((alpha==czero).and. (beta==cone))) return
           ! and when  alpha.eq.czero.
           if (alpha==czero) then
               if (beta==czero) then
                   do j = 1,n
                       do i = 1,m
                           c(i,j) = czero
                       end do
                   end do
               else
                   do j = 1,n
                       do i = 1,m
                           c(i,j) = beta*c(i,j)
                       end do
                   end do
               end if
               return
           end if
           ! start the operations.
           if (stdlib_lsame(side,'L')) then
              ! form  c := alpha*a*b + beta*c.
               if (upper) then
                   do j = 1,n
                       do i = 1,m
                           temp1 = alpha*b(i,j)
                           temp2 = czero
                           do k = 1,i - 1
                               c(k,j) = c(k,j) + temp1*a(k,i)
                               temp2 = temp2 + b(k,j)*conjg(a(k,i))
                           end do
                           if (beta==czero) then
                               c(i,j) = temp1*real(a(i,i),KIND=dp) + alpha*temp2
                           else
                               c(i,j) = beta*c(i,j) + temp1*real(a(i,i),KIND=dp) +&
                                         alpha*temp2
                           end if
                       end do
                   end do
               else
                   do j = 1,n
                       do i = m,1,-1
                           temp1 = alpha*b(i,j)
                           temp2 = czero
                           do k = i + 1,m
                               c(k,j) = c(k,j) + temp1*a(k,i)
                               temp2 = temp2 + b(k,j)*conjg(a(k,i))
                           end do
                           if (beta==czero) then
                               c(i,j) = temp1*real(a(i,i),KIND=dp) + alpha*temp2
                           else
                               c(i,j) = beta*c(i,j) + temp1*real(a(i,i),KIND=dp) +&
                                         alpha*temp2
                           end if
                       end do
                   end do
               end if
           else
              ! form  c := alpha*b*a + beta*c.
               loop_170: do j = 1,n
                   temp1 = alpha*real(a(j,j),KIND=dp)
                   if (beta==czero) then
                       do i = 1,m
                           c(i,j) = temp1*b(i,j)
                       end do
                   else
                       do i = 1,m
                           c(i,j) = beta*c(i,j) + temp1*b(i,j)
                       end do
                   end if
                   do k = 1,j - 1
                       if (upper) then
                           temp1 = alpha*a(k,j)
                       else
                           temp1 = alpha*conjg(a(j,k))
                       end if
                       do i = 1,m
                           c(i,j) = c(i,j) + temp1*b(i,k)
                       end do
                   end do
                   do k = j + 1,n
                       if (upper) then
                           temp1 = alpha*conjg(a(j,k))
                       else
                           temp1 = alpha*a(k,j)
                       end if
                       do i = 1,m
                           c(i,j) = c(i,j) + temp1*b(i,k)
                       end do
                   end do
               end do loop_170
           end if
           return
     end subroutine stdlib_I64_zhemm




     pure module subroutine stdlib_I64_cherk(uplo,trans,n,k,alpha,a,lda,beta,c,ldc)
     use stdlib_blas_constants_sp
     !! CHERK performs one of the hermitian rank k operations
     !! C := alpha*A*A**H + beta*C,
     !! or
     !! C := alpha*A**H*A + beta*C,
     !! where  alpha and beta  are  real scalars,  C is an  n by n  hermitian
     !! matrix and  A  is an  n by k  matrix in the  first case and a  k by n
     !! matrix in the second case.
        ! -- reference blas level3 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           real(sp), intent(in) :: alpha, beta
           integer(ilp64), intent(in) :: k, lda, ldc, n
           character, intent(in) :: trans, uplo
           ! Array Arguments 
           complex(sp), intent(in) :: a(lda,*)
           complex(sp), intent(inout) :: c(ldc,*)
        ! =====================================================================
           ! Intrinsic Functions 
           intrinsic :: cmplx,conjg,max,real
           ! Local Scalars 
           complex(sp) :: temp
           real(sp) :: rtemp
           integer(ilp64) :: i, info, j, l, nrowa
           logical(lk) :: upper
           
           ! test the input parameters.
           if (stdlib_lsame(trans,'N')) then
               nrowa = n
           else
               nrowa = k
           end if
           upper = stdlib_lsame(uplo,'U')
           info = 0
           if ((.not.upper) .and. (.not.stdlib_lsame(uplo,'L'))) then
               info = 1
           else if ((.not.stdlib_lsame(trans,'N')) .and.(.not.stdlib_lsame(trans,'C'))) &
                     then
               info = 2
           else if (n<0) then
               info = 3
           else if (k<0) then
               info = 4
           else if (lda<max(1,nrowa)) then
               info = 7
           else if (ldc<max(1,n)) then
               info = 10
           end if
           if (info/=0) then
               call stdlib_I64_xerbla('CHERK ',info)
               return
           end if
           ! quick return if possible.
           if ((n==0) .or. (((alpha==zero).or.(k==0)).and. (beta==one))) return
           ! and when  alpha.eq.zero.
           if (alpha==zero) then
               if (upper) then
                   if (beta==zero) then
                       do j = 1,n
                           do i = 1,j
                               c(i,j) = zero
                           end do
                       end do
                   else
                       do j = 1,n
                           do i = 1,j - 1
                               c(i,j) = beta*c(i,j)
                           end do
                           c(j,j) = beta*real(c(j,j),KIND=sp)
                       end do
                   end if
               else
                   if (beta==zero) then
                       do j = 1,n
                           do i = j,n
                               c(i,j) = zero
                           end do
                       end do
                   else
                       do j = 1,n
                           c(j,j) = beta*real(c(j,j),KIND=sp)
                           do i = j + 1,n
                               c(i,j) = beta*c(i,j)
                           end do
                       end do
                   end if
               end if
               return
           end if
           ! start the operations.
           if (stdlib_lsame(trans,'N')) then
              ! form  c := alpha*a*a**h + beta*c.
               if (upper) then
                   do j = 1,n
                       if (beta==zero) then
                           do i = 1,j
                               c(i,j) = zero
                           end do
                       else if (beta/=one) then
                           do i = 1,j - 1
                               c(i,j) = beta*c(i,j)
                           end do
                           c(j,j) = beta*real(c(j,j),KIND=sp)
                       else
                           c(j,j) = real(c(j,j),KIND=sp)
                       end if
                       do l = 1,k
                           if (a(j,l)/=cmplx(zero,KIND=sp)) then
                               temp = alpha*conjg(a(j,l))
                               do i = 1,j - 1
                                   c(i,j) = c(i,j) + temp*a(i,l)
                               end do
                               c(j,j) = real(c(j,j),KIND=sp) + real(temp*a(i,l),KIND=sp)
                           end if
                       end do
                   end do
               else
                   do j = 1,n
                       if (beta==zero) then
                           do i = j,n
                               c(i,j) = zero
                           end do
                       else if (beta/=one) then
                           c(j,j) = beta*real(c(j,j),KIND=sp)
                           do i = j + 1,n
                               c(i,j) = beta*c(i,j)
                           end do
                       else
                           c(j,j) = real(c(j,j),KIND=sp)
                       end if
                       do l = 1,k
                           if (a(j,l)/=cmplx(zero,KIND=sp)) then
                               temp = alpha*conjg(a(j,l))
                               c(j,j) = real(c(j,j),KIND=sp) + real(temp*a(j,l),KIND=sp)
                               do i = j + 1,n
                                   c(i,j) = c(i,j) + temp*a(i,l)
                               end do
                           end if
                       end do
                   end do
               end if
           else
              ! form  c := alpha*a**h*a + beta*c.
               if (upper) then
                   do j = 1,n
                       do i = 1,j - 1
                           temp = zero
                           do l = 1,k
                               temp = temp + conjg(a(l,i))*a(l,j)
                           end do
                           if (beta==zero) then
                               c(i,j) = alpha*temp
                           else
                               c(i,j) = alpha*temp + beta*c(i,j)
                           end if
                       end do
                       rtemp = zero
                       do l = 1,k
                           rtemp = rtemp + conjg(a(l,j))*a(l,j)
                       end do
                       if (beta==zero) then
                           c(j,j) = alpha*rtemp
                       else
                           c(j,j) = alpha*rtemp + beta*real(c(j,j),KIND=sp)
                       end if
                   end do
               else
                   do j = 1,n
                       rtemp = zero
                       do l = 1,k
                           rtemp = rtemp + conjg(a(l,j))*a(l,j)
                       end do
                       if (beta==zero) then
                           c(j,j) = alpha*rtemp
                       else
                           c(j,j) = alpha*rtemp + beta*real(c(j,j),KIND=sp)
                       end if
                       do i = j + 1,n
                           temp = zero
                           do l = 1,k
                               temp = temp + conjg(a(l,i))*a(l,j)
                           end do
                           if (beta==zero) then
                               c(i,j) = alpha*temp
                           else
                               c(i,j) = alpha*temp + beta*c(i,j)
                           end if
                       end do
                   end do
               end if
           end if
           return
     end subroutine stdlib_I64_cherk

     pure module subroutine stdlib_I64_zherk(uplo,trans,n,k,alpha,a,lda,beta,c,ldc)
     use stdlib_blas_constants_dp
     !! ZHERK performs one of the hermitian rank k operations
     !! C := alpha*A*A**H + beta*C,
     !! or
     !! C := alpha*A**H*A + beta*C,
     !! where  alpha and beta  are  real scalars,  C is an  n by n  hermitian
     !! matrix and  A  is an  n by k  matrix in the  first case and a  k by n
     !! matrix in the second case.
        ! -- reference blas level3 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           real(dp), intent(in) :: alpha, beta
           integer(ilp64), intent(in) :: k, lda, ldc, n
           character, intent(in) :: trans, uplo
           ! Array Arguments 
           complex(dp), intent(in) :: a(lda,*)
           complex(dp), intent(inout) :: c(ldc,*)
        ! =====================================================================
           ! Intrinsic Functions 
           intrinsic :: real,cmplx,conjg,max
           ! Local Scalars 
           complex(dp) :: temp
           real(dp) :: rtemp
           integer(ilp64) :: i, info, j, l, nrowa
           logical(lk) :: upper
           
           ! test the input parameters.
           if (stdlib_lsame(trans,'N')) then
               nrowa = n
           else
               nrowa = k
           end if
           upper = stdlib_lsame(uplo,'U')
           info = 0
           if ((.not.upper) .and. (.not.stdlib_lsame(uplo,'L'))) then
               info = 1
           else if ((.not.stdlib_lsame(trans,'N')) .and.(.not.stdlib_lsame(trans,'C'))) &
                     then
               info = 2
           else if (n<0) then
               info = 3
           else if (k<0) then
               info = 4
           else if (lda<max(1,nrowa)) then
               info = 7
           else if (ldc<max(1,n)) then
               info = 10
           end if
           if (info/=0) then
               call stdlib_I64_xerbla('ZHERK ',info)
               return
           end if
           ! quick return if possible.
           if ((n==0) .or. (((alpha==zero).or.(k==0)).and. (beta==one))) return
           ! and when  alpha.eq.zero.
           if (alpha==zero) then
               if (upper) then
                   if (beta==zero) then
                       do j = 1,n
                           do i = 1,j
                               c(i,j) = zero
                           end do
                       end do
                   else
                       do j = 1,n
                           do i = 1,j - 1
                               c(i,j) = beta*c(i,j)
                           end do
                           c(j,j) = beta*real(c(j,j),KIND=dp)
                       end do
                   end if
               else
                   if (beta==zero) then
                       do j = 1,n
                           do i = j,n
                               c(i,j) = zero
                           end do
                       end do
                   else
                       do j = 1,n
                           c(j,j) = beta*real(c(j,j),KIND=dp)
                           do i = j + 1,n
                               c(i,j) = beta*c(i,j)
                           end do
                       end do
                   end if
               end if
               return
           end if
           ! start the operations.
           if (stdlib_lsame(trans,'N')) then
              ! form  c := alpha*a*a**h + beta*c.
               if (upper) then
                   do j = 1,n
                       if (beta==zero) then
                           do i = 1,j
                               c(i,j) = zero
                           end do
                       else if (beta/=one) then
                           do i = 1,j - 1
                               c(i,j) = beta*c(i,j)
                           end do
                           c(j,j) = beta*real(c(j,j),KIND=dp)
                       else
                           c(j,j) = real(c(j,j),KIND=dp)
                       end if
                       do l = 1,k
                           if (a(j,l)/=cmplx(zero,KIND=dp)) then
                               temp = alpha*conjg(a(j,l))
                               do i = 1,j - 1
                                   c(i,j) = c(i,j) + temp*a(i,l)
                               end do
                               c(j,j) = real(c(j,j),KIND=dp) + real(temp*a(i,l),KIND=dp)
                           end if
                       end do
                   end do
               else
                   do j = 1,n
                       if (beta==zero) then
                           do i = j,n
                               c(i,j) = zero
                           end do
                       else if (beta/=one) then
                           c(j,j) = beta*real(c(j,j),KIND=dp)
                           do i = j + 1,n
                               c(i,j) = beta*c(i,j)
                           end do
                       else
                           c(j,j) = real(c(j,j),KIND=dp)
                       end if
                       do l = 1,k
                           if (a(j,l)/=cmplx(zero,KIND=dp)) then
                               temp = alpha*conjg(a(j,l))
                               c(j,j) = real(c(j,j),KIND=dp) + real(temp*a(j,l),KIND=dp)
                               do i = j + 1,n
                                   c(i,j) = c(i,j) + temp*a(i,l)
                               end do
                           end if
                       end do
                   end do
               end if
           else
              ! form  c := alpha*a**h*a + beta*c.
               if (upper) then
                   do j = 1,n
                       do i = 1,j - 1
                           temp = zero
                           do l = 1,k
                               temp = temp + conjg(a(l,i))*a(l,j)
                           end do
                           if (beta==zero) then
                               c(i,j) = alpha*temp
                           else
                               c(i,j) = alpha*temp + beta*c(i,j)
                           end if
                       end do
                       rtemp = zero
                       do l = 1,k
                           rtemp = rtemp + conjg(a(l,j))*a(l,j)
                       end do
                       if (beta==zero) then
                           c(j,j) = alpha*rtemp
                       else
                           c(j,j) = alpha*rtemp + beta*real(c(j,j),KIND=dp)
                       end if
                   end do
               else
                   do j = 1,n
                       rtemp = zero
                       do l = 1,k
                           rtemp = rtemp + conjg(a(l,j))*a(l,j)
                       end do
                       if (beta==zero) then
                           c(j,j) = alpha*rtemp
                       else
                           c(j,j) = alpha*rtemp + beta*real(c(j,j),KIND=dp)
                       end if
                       do i = j + 1,n
                           temp = zero
                           do l = 1,k
                               temp = temp + conjg(a(l,i))*a(l,j)
                           end do
                           if (beta==zero) then
                               c(i,j) = alpha*temp
                           else
                               c(i,j) = alpha*temp + beta*c(i,j)
                           end if
                       end do
                   end do
               end if
           end if
           return
     end subroutine stdlib_I64_zherk




     pure module subroutine stdlib_I64_cher2k(uplo,trans,n,k,alpha,a,lda,b,ldb,beta,c,ldc)
     use stdlib_blas_constants_sp
     !! CHER2K performs one of the hermitian rank 2k operations
     !! C := alpha*A*B**H + conjg( alpha )*B*A**H + beta*C,
     !! or
     !! C := alpha*A**H*B + conjg( alpha )*B**H*A + beta*C,
     !! where  alpha and beta  are scalars with  beta  real,  C is an  n by n
     !! hermitian matrix and  A and B  are  n by k matrices in the first case
     !! and  k by n  matrices in the second case.
        ! -- reference blas level3 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           complex(sp), intent(in) :: alpha
           real(sp), intent(in) :: beta
           integer(ilp64), intent(in) :: k, lda, ldb, ldc, n
           character, intent(in) :: trans, uplo
           ! Array Arguments 
           complex(sp), intent(in) :: a(lda,*), b(ldb,*)
           complex(sp), intent(inout) :: c(ldc,*)
        ! =====================================================================
           ! Intrinsic Functions 
           intrinsic :: conjg,max,real
           ! Local Scalars 
           complex(sp) :: temp1, temp2
           integer(ilp64) :: i, info, j, l, nrowa
           logical(lk) :: upper
           
           
           ! test the input parameters.
           if (stdlib_lsame(trans,'N')) then
               nrowa = n
           else
               nrowa = k
           end if
           upper = stdlib_lsame(uplo,'U')
           info = 0
           if ((.not.upper) .and. (.not.stdlib_lsame(uplo,'L'))) then
               info = 1
           else if ((.not.stdlib_lsame(trans,'N')) .and.(.not.stdlib_lsame(trans,'C'))) &
                     then
               info = 2
           else if (n<0) then
               info = 3
           else if (k<0) then
               info = 4
           else if (lda<max(1,nrowa)) then
               info = 7
           else if (ldb<max(1,nrowa)) then
               info = 9
           else if (ldc<max(1,n)) then
               info = 12
           end if
           if (info/=0) then
               call stdlib_I64_xerbla('CHER2K',info)
               return
           end if
           ! quick return if possible.
           if ((n==0) .or. (((alpha==czero).or.(k==0)).and. (beta==one))) return
           ! and when  alpha.eq.czero.
           if (alpha==czero) then
               if (upper) then
                   if (beta==real(czero,KIND=sp)) then
                       do j = 1,n
                           do i = 1,j
                               c(i,j) = czero
                           end do
                       end do
                   else
                       do j = 1,n
                           do i = 1,j - 1
                               c(i,j) = beta*c(i,j)
                           end do
                           c(j,j) = beta*real(c(j,j),KIND=sp)
                       end do
                   end if
               else
                   if (beta==real(czero,KIND=sp)) then
                       do j = 1,n
                           do i = j,n
                               c(i,j) = czero
                           end do
                       end do
                   else
                       do j = 1,n
                           c(j,j) = beta*real(c(j,j),KIND=sp)
                           do i = j + 1,n
                               c(i,j) = beta*c(i,j)
                           end do
                       end do
                   end if
               end if
               return
           end if
           ! start the operations.
           if (stdlib_lsame(trans,'N')) then
              ! form  c := alpha*a*b**h + conjg( alpha )*b*a**h +
                         ! c.
               if (upper) then
                   do j = 1,n
                       if (beta==real(czero,KIND=sp)) then
                           do i = 1,j
                               c(i,j) = czero
                           end do
                       else if (beta/=one) then
                           do i = 1,j - 1
                               c(i,j) = beta*c(i,j)
                           end do
                           c(j,j) = beta*real(c(j,j),KIND=sp)
                       else
                           c(j,j) = real(c(j,j),KIND=sp)
                       end if
                       do l = 1,k
                           if ((a(j,l)/=czero) .or. (b(j,l)/=czero)) then
                               temp1 = alpha*conjg(b(j,l))
                               temp2 = conjg(alpha*a(j,l))
                               do i = 1,j - 1
                                   c(i,j) = c(i,j) + a(i,l)*temp1 +b(i,l)*temp2
                               end do
                               c(j,j) = real(c(j,j),KIND=sp) +real(a(j,l)*temp1+b(j,l)*temp2,&
                                         KIND=sp)
                           end if
                       end do
                   end do
               else
                   do j = 1,n
                       if (beta==real(czero,KIND=sp)) then
                           do i = j,n
                               c(i,j) = czero
                           end do
                       else if (beta/=one) then
                           do i = j + 1,n
                               c(i,j) = beta*c(i,j)
                           end do
                           c(j,j) = beta*real(c(j,j),KIND=sp)
                       else
                           c(j,j) = real(c(j,j),KIND=sp)
                       end if
                       do l = 1,k
                           if ((a(j,l)/=czero) .or. (b(j,l)/=czero)) then
                               temp1 = alpha*conjg(b(j,l))
                               temp2 = conjg(alpha*a(j,l))
                               do i = j + 1,n
                                   c(i,j) = c(i,j) + a(i,l)*temp1 +b(i,l)*temp2
                               end do
                               c(j,j) = real(c(j,j),KIND=sp) +real(a(j,l)*temp1+b(j,l)*temp2,&
                                         KIND=sp)
                           end if
                       end do
                   end do
               end if
           else
              ! form  c := alpha*a**h*b + conjg( alpha )*b**h*a +
                         ! c.
               if (upper) then
                   do j = 1,n
                       do i = 1,j
                           temp1 = czero
                           temp2 = czero
                           do l = 1,k
                               temp1 = temp1 + conjg(a(l,i))*b(l,j)
                               temp2 = temp2 + conjg(b(l,i))*a(l,j)
                           end do
                           if (i==j) then
                               if (beta==real(czero,KIND=sp)) then
                                   c(j,j) = real(alpha*temp1+conjg(alpha)*temp2,KIND=sp)
                               else
                                   c(j,j) = beta*real(c(j,j),KIND=sp) +real(alpha*temp1+conjg(&
                                             alpha)*temp2,KIND=sp)
                               end if
                           else
                               if (beta==real(czero,KIND=sp)) then
                                   c(i,j) = alpha*temp1 + conjg(alpha)*temp2
                               else
                                   c(i,j) = beta*c(i,j) + alpha*temp1 +conjg(alpha)*temp2
                               end if
                           end if
                       end do
                   end do
               else
                   do j = 1,n
                       do i = j,n
                           temp1 = czero
                           temp2 = czero
                           do l = 1,k
                               temp1 = temp1 + conjg(a(l,i))*b(l,j)
                               temp2 = temp2 + conjg(b(l,i))*a(l,j)
                           end do
                           if (i==j) then
                               if (beta==real(czero,KIND=sp)) then
                                   c(j,j) = real(alpha*temp1+conjg(alpha)*temp2,KIND=sp)
                               else
                                   c(j,j) = beta*real(c(j,j),KIND=sp) +real(alpha*temp1+conjg(&
                                             alpha)*temp2,KIND=sp)
                               end if
                           else
                               if (beta==real(czero,KIND=sp)) then
                                   c(i,j) = alpha*temp1 + conjg(alpha)*temp2
                               else
                                   c(i,j) = beta*c(i,j) + alpha*temp1 +conjg(alpha)*temp2
                               end if
                           end if
                       end do
                   end do
               end if
           end if
           return
     end subroutine stdlib_I64_cher2k

     pure module subroutine stdlib_I64_zher2k(uplo,trans,n,k,alpha,a,lda,b,ldb,beta,c,ldc)
     use stdlib_blas_constants_dp
     !! ZHER2K performs one of the hermitian rank 2k operations
     !! C := alpha*A*B**H + conjg( alpha )*B*A**H + beta*C,
     !! or
     !! C := alpha*A**H*B + conjg( alpha )*B**H*A + beta*C,
     !! where  alpha and beta  are scalars with  beta  real,  C is an  n by n
     !! hermitian matrix and  A and B  are  n by k matrices in the first case
     !! and  k by n  matrices in the second case.
        ! -- reference blas level3 routine --
        ! -- reference blas is a software package provided by univ. of tennessee,    --
        ! -- univ. of california berkeley, univ. of colorado denver and nag ltd..--
           ! Scalar Arguments 
           complex(dp), intent(in) :: alpha
           real(dp), intent(in) :: beta
           integer(ilp64), intent(in) :: k, lda, ldb, ldc, n
           character, intent(in) :: trans, uplo
           ! Array Arguments 
           complex(dp), intent(in) :: a(lda,*), b(ldb,*)
           complex(dp), intent(inout) :: c(ldc,*)
        ! =====================================================================
           ! Intrinsic Functions 
           intrinsic :: real,conjg,max
           ! Local Scalars 
           complex(dp) :: temp1, temp2
           integer(ilp64) :: i, info, j, l, nrowa
           logical(lk) :: upper
           
           
           ! test the input parameters.
           if (stdlib_lsame(trans,'N')) then
               nrowa = n
           else
               nrowa = k
           end if
           upper = stdlib_lsame(uplo,'U')
           info = 0
           if ((.not.upper) .and. (.not.stdlib_lsame(uplo,'L'))) then
               info = 1
           else if ((.not.stdlib_lsame(trans,'N')) .and.(.not.stdlib_lsame(trans,'C'))) &
                     then
               info = 2
           else if (n<0) then
               info = 3
           else if (k<0) then
               info = 4
           else if (lda<max(1,nrowa)) then
               info = 7
           else if (ldb<max(1,nrowa)) then
               info = 9
           else if (ldc<max(1,n)) then
               info = 12
           end if
           if (info/=0) then
               call stdlib_I64_xerbla('ZHER2K',info)
               return
           end if
           ! quick return if possible.
           if ((n==0) .or. (((alpha==czero).or.(k==0)).and. (beta==one))) return
           ! and when  alpha.eq.czero.
           if (alpha==czero) then
               if (upper) then
                   if (beta==real(czero,KIND=dp)) then
                       do j = 1,n
                           do i = 1,j
                               c(i,j) = czero
                           end do
                       end do
                   else
                       do j = 1,n
                           do i = 1,j - 1
                               c(i,j) = beta*c(i,j)
                           end do
                           c(j,j) = beta*real(c(j,j),KIND=dp)
                       end do
                   end if
               else
                   if (beta==real(czero,KIND=dp)) then
                       do j = 1,n
                           do i = j,n
                               c(i,j) = czero
                           end do
                       end do
                   else
                       do j = 1,n
                           c(j,j) = beta*real(c(j,j),KIND=dp)
                           do i = j + 1,n
                               c(i,j) = beta*c(i,j)
                           end do
                       end do
                   end if
               end if
               return
           end if
           ! start the operations.
           if (stdlib_lsame(trans,'N')) then
              ! form  c := alpha*a*b**h + conjg( alpha )*b*a**h +
                         ! c.
               if (upper) then
                   do j = 1,n
                       if (beta==real(czero,KIND=dp)) then
                           do i = 1,j
                               c(i,j) = czero
                           end do
                       else if (beta/=one) then
                           do i = 1,j - 1
                               c(i,j) = beta*c(i,j)
                           end do
                           c(j,j) = beta*real(c(j,j),KIND=dp)
                       else
                           c(j,j) = real(c(j,j),KIND=dp)
                       end if
                       do l = 1,k
                           if ((a(j,l)/=czero) .or. (b(j,l)/=czero)) then
                               temp1 = alpha*conjg(b(j,l))
                               temp2 = conjg(alpha*a(j,l))
                               do i = 1,j - 1
                                   c(i,j) = c(i,j) + a(i,l)*temp1 +b(i,l)*temp2
                               end do
                               c(j,j) = real(c(j,j),KIND=dp) +real(a(j,l)*temp1+b(j,l)*temp2,&
                                         KIND=dp)
                           end if
                       end do
                   end do
               else
                   do j = 1,n
                       if (beta==real(czero,KIND=dp)) then
                           do i = j,n
                               c(i,j) = czero
                           end do
                       else if (beta/=one) then
                           do i = j + 1,n
                               c(i,j) = beta*c(i,j)
                           end do
                           c(j,j) = beta*real(c(j,j),KIND=dp)
                       else
                           c(j,j) = real(c(j,j),KIND=dp)
                       end if
                       do l = 1,k
                           if ((a(j,l)/=czero) .or. (b(j,l)/=czero)) then
                               temp1 = alpha*conjg(b(j,l))
                               temp2 = conjg(alpha*a(j,l))
                               do i = j + 1,n
                                   c(i,j) = c(i,j) + a(i,l)*temp1 +b(i,l)*temp2
                               end do
                               c(j,j) = real(c(j,j),KIND=dp) +real(a(j,l)*temp1+b(j,l)*temp2,&
                                         KIND=dp)
                           end if
                       end do
                   end do
               end if
           else
              ! form  c := alpha*a**h*b + conjg( alpha )*b**h*a +
                         ! c.
               if (upper) then
                   do j = 1,n
                       do i = 1,j
                           temp1 = czero
                           temp2 = czero
                           do l = 1,k
                               temp1 = temp1 + conjg(a(l,i))*b(l,j)
                               temp2 = temp2 + conjg(b(l,i))*a(l,j)
                           end do
                           if (i==j) then
                               if (beta==real(czero,KIND=dp)) then
                                   c(j,j) = real(alpha*temp1+conjg(alpha)*temp2,KIND=dp)
                               else
                                   c(j,j) = beta*real(c(j,j),KIND=dp) +real(alpha*temp1+conjg(&
                                             alpha)*temp2,KIND=dp)
                               end if
                           else
                               if (beta==real(czero,KIND=dp)) then
                                   c(i,j) = alpha*temp1 + conjg(alpha)*temp2
                               else
                                   c(i,j) = beta*c(i,j) + alpha*temp1 +conjg(alpha)*temp2
                               end if
                           end if
                       end do
                   end do
               else
                   do j = 1,n
                       do i = j,n
                           temp1 = czero
                           temp2 = czero
                           do l = 1,k
                               temp1 = temp1 + conjg(a(l,i))*b(l,j)
                               temp2 = temp2 + conjg(b(l,i))*a(l,j)
                           end do
                           if (i==j) then
                               if (beta==real(czero,KIND=dp)) then
                                   c(j,j) = real(alpha*temp1+conjg(alpha)*temp2,KIND=dp)
                               else
                                   c(j,j) = beta*real(c(j,j),KIND=dp) +real(alpha*temp1+conjg(&
                                             alpha)*temp2,KIND=dp)
                               end if
                           else
                               if (beta==real(czero,KIND=dp)) then
                                   c(i,j) = alpha*temp1 + conjg(alpha)*temp2
                               else
                                   c(i,j) = beta*c(i,j) + alpha*temp1 +conjg(alpha)*temp2
                               end if
                           end if
                       end do
                   end do
               end if
           end if
           return
     end subroutine stdlib_I64_zher2k



end submodule stdlib_blas_level3_gen
