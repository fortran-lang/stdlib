program example_kronecker_product
  use stdlib_linalg, only: kronecker_product
  implicit none
  integer, parameter :: m1 = 1, n1 = 2, m2 = 2, n2 = 3
  integer :: i, j
  real :: A(m1, n1), B(m2,n2)
  real, allocatable :: C(:,:)

  do j = 1, n1
     do i = 1, m1
        A(i,j) = i*j ! A = [1, 2]
     end do
  end do
  
  do j = 1, n2
     do i = 1, m2      ! B = [ 1, 2, 3 ]
        B(i,j) = i*j !     [ 2, 4, 6 ]
     end do
  end do
  
  C = kronecker_product(A, B)
  ! C =     [ a(1,1) * B(:,:) | a(1,2) * B(:,:) ]
  ! or in other words, 
  ! C =     [  1.00      2.00      3.00      2.00      4.00      6.00  ]
  !         [  2.00      4.00      6.00      4.00      8.00     12.00  ]
end program example_kronecker_product
