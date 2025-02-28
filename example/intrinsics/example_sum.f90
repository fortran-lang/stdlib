program example_sum
    use stdlib_kinds, only: sp
    use stdlib_intrinsics, only: stdlib_sum, stdlib_sum_kahan
    implicit none

    real(sp), allocatable :: x(:)
    real(sp) :: total_sum(3)

    allocate( x(1000) )
    call random_number(x)

    total_sum(1) = sum(x)       !> compiler intrinsic
    total_sum(2) = stdlib_sum(x)      !> chunked summation
    total_sum(3) = stdlib_sum_kahan(x)!> chunked kahan summation
    print *, total_sum(1:3)
    
end program example_sum