program example_dot_product
    use stdlib_kinds, only: sp
    use stdlib_intrinsics, only: stdlib_dot_product, stdlib_dot_product_kahan
    implicit none

    real(sp), allocatable :: x(:), y(:)
    real(sp) :: total_prod(3)

    allocate( x(1000), y(1000) )
    call random_number(x)
    call random_number(y)

    total_prod(1) = dot_product(x,y) !> compiler intrinsic
    total_prod(2) = stdlib_dot_product(x,y)       !> chunked summation over inner product
    total_prod(3) = stdlib_dot_product_kahan(x,y) !> chunked kahan summation over inner product
    print *, total_prod(1:3)
    
end program example_dot_product