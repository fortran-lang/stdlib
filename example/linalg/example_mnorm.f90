program example_mnorm
    use stdlib_linalg, only: mnorm
    use stdlib_kinds, only: sp
    implicit none
    real(sp) :: a(3,3), na
    real(sp) :: b(3,3,4), nb(4)  ! Array of 4 3x3 matrices
    integer :: i
    
    ! Initialize example matrix
    a = reshape([1, 2, 3, 4, 5, 6, 7, 8, 9], [3, 3])
    
    ! Compute Euclidean norm of single matrix
    na = mnorm(a, 'Euclidean')
    print *, "Euclidean norm of matrix a:", na
    
    ! Initialize array of matrices
    b(:,:,1) = a
    b(:,:,2) = 2*a
    b(:,:,3) = 3*a
    b(:,:,4) = 4*a
    
    ! Use rank-2 slices to support builds with maximum rank 2.
    do i = 1, size(b,3)
        nb(i) = mnorm(b(:,:,i), 'inf')
    end do

    ! 18.0000000       36.0000000       54.0000000       72.0000000
    print *, "Infinity norms of matrices in b:", nb
end program example_mnorm
