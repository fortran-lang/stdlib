program example_sum
    use stdlib_kinds, only: sp
    use stdlib_intrinsics, only: stdlib_sum, stdlib_sum_kahan
    implicit none

    real(sp), allocatable :: x(:)
    real(sp) :: total_sum(3)
    real :: P(2,3), Q(2,3), res(2,2), tmp(3)
    integer :: i, j

    allocate( x(1000) )
    call random_number(x)

    total_sum(1) = sum(x)       !> compiler intrinsic
    total_sum(2) = stdlib_sum(x)      !> chunked summation
    total_sum(3) = stdlib_sum_kahan(x)!> chunked kahan summation
    print *, total_sum(1:3)
    
    print*, "my programme: "
    P(:,1) = [1,2]
    P(:,2) = [-1,3]
    P(:,3) = [4,2]
    Q(:,1) = [3,0]
    Q(:,2) = [5,4]
    Q(:,3) = [0,-1]
    res = matmul(P, transpose(Q))
    print*, ""
    do i = 1, 2
        print*, res(i, :)
    end do


    do j = 1, 2
        do i = 1, 2
            tmp(:) = P(i,:) * Q(j,:)
            res(i,j) = stdlib_sum_kahan(tmp)
        end do
    end do
    print*, ""
    do i = 1, 2
        print*, res(i, :)
    end do

end program example_sum