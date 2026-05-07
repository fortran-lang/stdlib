program example_expm
   use stdlib_linalg, only: matrix_exp
   implicit none
   real :: A(3, 3), E(3, 3)
   integer :: i

   print *, "Matrix A :"
   do i = 1, 3
      print *, A(i, :)
   end do

   A = reshape([1, 2, 3, 4, 5, 6, 7, 8, 9], [3, 3])
   call matrix_exp(A) ! In-place computation.
   ! For out-of-place, use call matrix_exp(A, E).

   print *, "Matrix exponential E = exp(A):"
   do i = 1, 3
      print *, E(i, :)
   end do
end program example_expm
