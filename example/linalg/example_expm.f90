program example_expm
   use stdlib_linalg, only: expm
   implicit none
   real :: A(3, 3), E(3, 3)
   A = reshape([1, 2, 3, 4, 5, 6, 7, 8, 9], [3, 3])
   E = expm(A)
end program example_expm
