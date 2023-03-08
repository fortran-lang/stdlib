submodule (stdlib_linalg) stdlib_linalg_kronecker

  implicit none

contains

    pure module function kronecker_product_rsp(A, B) result(C)
      real(sp), intent(in) :: A(:,:), B(:,:)
      real(sp) :: C(size(A,dim=1)*size(B,dim=1),size(A,dim=2)*size(B,dim=2))
      integer :: m1, n1, maxM1, maxN1, maxM2, maxN2
      
      maxM1 = size(A, dim=1)
      maxN1 = size(A, dim=2)
      maxM2 = size(B, dim=1)
      maxN2 = size(B, dim=2)
      

      do n1 = 1, maxN1
         do m1 = 1, maxM1
            ! We use the Wikipedia convention for ordering of the matrix elements
	    ! https://en.wikipedia.org/wiki/Kronecker_product
            C((m1-1)*maxM2+1:m1*maxM2, (n1-1)*maxN2+1:n1*maxN2) = A(m1, n1) * B(:,:)
         end do
      end do
    end function kronecker_product_rsp
    pure module function kronecker_product_rdp(A, B) result(C)
      real(dp), intent(in) :: A(:,:), B(:,:)
      real(dp) :: C(size(A,dim=1)*size(B,dim=1),size(A,dim=2)*size(B,dim=2))
      integer :: m1, n1, maxM1, maxN1, maxM2, maxN2
      
      maxM1 = size(A, dim=1)
      maxN1 = size(A, dim=2)
      maxM2 = size(B, dim=1)
      maxN2 = size(B, dim=2)
      

      do n1 = 1, maxN1
         do m1 = 1, maxM1
            ! We use the Wikipedia convention for ordering of the matrix elements
	    ! https://en.wikipedia.org/wiki/Kronecker_product
            C((m1-1)*maxM2+1:m1*maxM2, (n1-1)*maxN2+1:n1*maxN2) = A(m1, n1) * B(:,:)
         end do
      end do
    end function kronecker_product_rdp
    pure module function kronecker_product_csp(A, B) result(C)
      complex(sp), intent(in) :: A(:,:), B(:,:)
      complex(sp) :: C(size(A,dim=1)*size(B,dim=1),size(A,dim=2)*size(B,dim=2))
      integer :: m1, n1, maxM1, maxN1, maxM2, maxN2
      
      maxM1 = size(A, dim=1)
      maxN1 = size(A, dim=2)
      maxM2 = size(B, dim=1)
      maxN2 = size(B, dim=2)
      

      do n1 = 1, maxN1
         do m1 = 1, maxM1
            ! We use the Wikipedia convention for ordering of the matrix elements
	    ! https://en.wikipedia.org/wiki/Kronecker_product
            C((m1-1)*maxM2+1:m1*maxM2, (n1-1)*maxN2+1:n1*maxN2) = A(m1, n1) * B(:,:)
         end do
      end do
    end function kronecker_product_csp
    pure module function kronecker_product_cdp(A, B) result(C)
      complex(dp), intent(in) :: A(:,:), B(:,:)
      complex(dp) :: C(size(A,dim=1)*size(B,dim=1),size(A,dim=2)*size(B,dim=2))
      integer :: m1, n1, maxM1, maxN1, maxM2, maxN2
      
      maxM1 = size(A, dim=1)
      maxN1 = size(A, dim=2)
      maxM2 = size(B, dim=1)
      maxN2 = size(B, dim=2)
      

      do n1 = 1, maxN1
         do m1 = 1, maxM1
            ! We use the Wikipedia convention for ordering of the matrix elements
	    ! https://en.wikipedia.org/wiki/Kronecker_product
            C((m1-1)*maxM2+1:m1*maxM2, (n1-1)*maxN2+1:n1*maxN2) = A(m1, n1) * B(:,:)
         end do
      end do
    end function kronecker_product_cdp
    pure module function kronecker_product_iint8(A, B) result(C)
      integer(int8), intent(in) :: A(:,:), B(:,:)
      integer(int8) :: C(size(A,dim=1)*size(B,dim=1),size(A,dim=2)*size(B,dim=2))
      integer :: m1, n1, maxM1, maxN1, maxM2, maxN2
      
      maxM1 = size(A, dim=1)
      maxN1 = size(A, dim=2)
      maxM2 = size(B, dim=1)
      maxN2 = size(B, dim=2)
      

      do n1 = 1, maxN1
         do m1 = 1, maxM1
            ! We use the Wikipedia convention for ordering of the matrix elements
	    ! https://en.wikipedia.org/wiki/Kronecker_product
            C((m1-1)*maxM2+1:m1*maxM2, (n1-1)*maxN2+1:n1*maxN2) = A(m1, n1) * B(:,:)
         end do
      end do
    end function kronecker_product_iint8
    pure module function kronecker_product_iint16(A, B) result(C)
      integer(int16), intent(in) :: A(:,:), B(:,:)
      integer(int16) :: C(size(A,dim=1)*size(B,dim=1),size(A,dim=2)*size(B,dim=2))
      integer :: m1, n1, maxM1, maxN1, maxM2, maxN2
      
      maxM1 = size(A, dim=1)
      maxN1 = size(A, dim=2)
      maxM2 = size(B, dim=1)
      maxN2 = size(B, dim=2)
      

      do n1 = 1, maxN1
         do m1 = 1, maxM1
            ! We use the Wikipedia convention for ordering of the matrix elements
	    ! https://en.wikipedia.org/wiki/Kronecker_product
            C((m1-1)*maxM2+1:m1*maxM2, (n1-1)*maxN2+1:n1*maxN2) = A(m1, n1) * B(:,:)
         end do
      end do
    end function kronecker_product_iint16
    pure module function kronecker_product_iint32(A, B) result(C)
      integer(int32), intent(in) :: A(:,:), B(:,:)
      integer(int32) :: C(size(A,dim=1)*size(B,dim=1),size(A,dim=2)*size(B,dim=2))
      integer :: m1, n1, maxM1, maxN1, maxM2, maxN2
      
      maxM1 = size(A, dim=1)
      maxN1 = size(A, dim=2)
      maxM2 = size(B, dim=1)
      maxN2 = size(B, dim=2)
      

      do n1 = 1, maxN1
         do m1 = 1, maxM1
            ! We use the Wikipedia convention for ordering of the matrix elements
	    ! https://en.wikipedia.org/wiki/Kronecker_product
            C((m1-1)*maxM2+1:m1*maxM2, (n1-1)*maxN2+1:n1*maxN2) = A(m1, n1) * B(:,:)
         end do
      end do
    end function kronecker_product_iint32
    pure module function kronecker_product_iint64(A, B) result(C)
      integer(int64), intent(in) :: A(:,:), B(:,:)
      integer(int64) :: C(size(A,dim=1)*size(B,dim=1),size(A,dim=2)*size(B,dim=2))
      integer :: m1, n1, maxM1, maxN1, maxM2, maxN2
      
      maxM1 = size(A, dim=1)
      maxN1 = size(A, dim=2)
      maxM2 = size(B, dim=1)
      maxN2 = size(B, dim=2)
      

      do n1 = 1, maxN1
         do m1 = 1, maxM1
            ! We use the Wikipedia convention for ordering of the matrix elements
	    ! https://en.wikipedia.org/wiki/Kronecker_product
            C((m1-1)*maxM2+1:m1*maxM2, (n1-1)*maxN2+1:n1*maxN2) = A(m1, n1) * B(:,:)
         end do
      end do
    end function kronecker_product_iint64
end submodule stdlib_linalg_kronecker
