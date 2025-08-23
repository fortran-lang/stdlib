program example_sparse_data_accessors
   use stdlib_linalg_constants, only: dp
   use stdlib_sparse
   implicit none

   real(dp) :: mat(2, 2)
   real(dp), allocatable :: dense_matrix(:, :)
   type(CSR_dp_type) :: CSR
   type(COO_dp_type) :: COO
   integer :: i, j, locdof(2)

   ! Initial data
   mat(:, 1) = [1._dp, 2._dp]
   mat(:, 2) = [2._dp, 1._dp]
   allocate (dense_matrix(5, 5), source=0._dp)
   do i = 0, 3
      dense_matrix(1 + i:2 + i, 1 + i:2 + i) = dense_matrix(1 + i:2 + i, 1 + i:2 + i) + mat
   end do

   print *, 'Original Matrix'
   do j = 1, 5
      print '(5f8.1)', dense_matrix(j, :)
   end do

   ! Initialize CSR data and reset dense reference matrix
   call dense2coo(dense_matrix, COO)
   call coo2csr(COO, CSR)
   CSR%data = 0._dp
   dense_matrix = 0._dp

   ! Iteratively add blocks of data
   do i = 0, 3
      locdof(1:2) = [1 + i, 2 + i]
      call CSR%add(locdof, locdof, mat)
      ! lets print a dense view of every step
      call csr2dense(CSR, dense_matrix)
      print '(A,I2)', 'Add block :', i + 1
      do j = 1, 5
         print '(5f8.1)', dense_matrix(j, :)
      end do
   end do

   ! Request values from the matrix
   print *, ''
   print *, 'within sparse pattern  :', CSR%at(2, 1)
   print *, 'outside sparse pattern :', CSR%at(5, 2)
   print *, 'outside matrix pattern :', CSR%at(7, 7)

end program example_sparse_data_accessors
