
module test_intrinsics
    use testdrive, only : new_unittest, unittest_type, error_type, check, skip_test
    use stdlib_kinds, only: sp, dp, xdp, qp, int8, int16, int32, int64
    use stdlib_intrinsics
    use stdlib_math, only: swap
    implicit none
    
contains

!> Collect all exported unit tests
subroutine collect_suite(testsuite)
    !> Collection of tests
    type(unittest_type), allocatable, intent(out) :: testsuite(:)

    testsuite = [ &
        new_unittest('sum', test_sum), &
        new_unittest('dot_product', test_dot_product) &
    ]
end subroutine

subroutine test_sum(error)
    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    !> Internal parameters and variables
    integer, parameter :: n = 1e3, ncalc = 3
    real(sp) :: u
    integer :: iter, i, j
    !====================================================================================
    block
        integer(int32), allocatable :: x(:)
        integer(int32), parameter :: total_sum = 0_int32
        integer(int32) :: xsum(ncalc), err(ncalc)
        logical, allocatable :: mask(:), nmask(:)

        allocate(x(n+1))
        do i = 1, n+1
            x(i) = i - n/2 - 1
        end do
        allocate(mask(n+1),source=.false.); mask(1:n+1:2) = .true.
        allocate(nmask(n+1)); nmask = .not.mask
        ! scramble array
        do i = 1, n+1
            call random_number(u) 
            j = 1 + floor(n*u)
            call swap( x(i), x(j) )
            call swap( mask(i), mask(j) )
            call swap( nmask(i), nmask(j) )
        end do
        
        xsum(1) = sum(x)        ! compiler intrinsic
        xsum(2) = stdlib_sum(x) ! chunked summation
        err(1:2) = abs(total_sum-xsum(1:2))
        
        call check(error, all(err(1:2)==0_int32) , "real sum is not accurate" )
        if (allocated(error)) return

        xsum(1) = sum(x,mask)+sum(x,nmask) ! compiler intrinsic
        xsum(2) = stdlib_sum(x,mask)+stdlib_sum(x,nmask) ! chunked summation
        err(1:2) = abs(total_sum-xsum(1:2))
        
        call check(error, all(err(1:2)==0_int32) , "masked real sum is not accurate" )
        if (allocated(error)) return
    end block
    block
        integer(int64), allocatable :: x(:)
        integer(int64), parameter :: total_sum = 0_int64
        integer(int64) :: xsum(ncalc), err(ncalc)
        logical, allocatable :: mask(:), nmask(:)

        allocate(x(n+1))
        do i = 1, n+1
            x(i) = i - n/2 - 1
        end do
        allocate(mask(n+1),source=.false.); mask(1:n+1:2) = .true.
        allocate(nmask(n+1)); nmask = .not.mask
        ! scramble array
        do i = 1, n+1
            call random_number(u) 
            j = 1 + floor(n*u)
            call swap( x(i), x(j) )
            call swap( mask(i), mask(j) )
            call swap( nmask(i), nmask(j) )
        end do
        
        xsum(1) = sum(x)        ! compiler intrinsic
        xsum(2) = stdlib_sum(x) ! chunked summation
        err(1:2) = abs(total_sum-xsum(1:2))
        
        call check(error, all(err(1:2)==0_int64) , "real sum is not accurate" )
        if (allocated(error)) return

        xsum(1) = sum(x,mask)+sum(x,nmask) ! compiler intrinsic
        xsum(2) = stdlib_sum(x,mask)+stdlib_sum(x,nmask) ! chunked summation
        err(1:2) = abs(total_sum-xsum(1:2))
        
        call check(error, all(err(1:2)==0_int64) , "masked real sum is not accurate" )
        if (allocated(error)) return
    end block

    block
        real(sp), allocatable :: x(:)
        real(sp), parameter :: total_sum = 4*atan(1._sp), tolerance = epsilon(1._sp)*100
        real(sp) :: xsum(ncalc), err(ncalc)
        logical, allocatable :: mask(:), nmask(:)

        allocate(x(n))
        do i = 1, n 
            x(i) = 8*atan(1._sp)*(real(i,kind=sp)-0.5_sp)/real(n,kind=sp)**2
        end do
        allocate(mask(n),source=.false.); mask(1:n:2) = .true.
        allocate(nmask(n)); nmask = .not.mask
        ! scramble array
        do i = 1, n
            call random_number(u) 
            j = 1 + floor(n*u)
            call swap( x(i), x(j) )
            call swap( mask(i), mask(j) )
            call swap( nmask(i), nmask(j) )
        end do
        
        xsum(1) = sum(x)        ! compiler intrinsic
        xsum(2) = stdlib_sum_kahan(x) ! chunked Kahan summation
        xsum(3) = stdlib_sum(x)       ! chunked summation
        err(1:ncalc) = abs(1._sp-xsum(1:ncalc)/total_sum)
        
        call check(error, all(err(:)<tolerance) , "real sum is not accurate" )
        if (allocated(error)) return

        xsum(1) = sum(x,mask)+sum(x,nmask) ! compiler intrinsic
        xsum(2) = stdlib_sum_kahan(x,mask)+stdlib_sum_kahan(x,nmask) ! chunked Kahan summation
        xsum(3) = stdlib_sum(x,mask)+stdlib_sum(x,nmask) ! chunked summation
        err(1:ncalc) = abs(1._sp-xsum(1:ncalc)/total_sum)
        
        call check(error, all(err(:)<tolerance) , "masked real sum is not accurate" )
        if (allocated(error)) return
    end block
    block
        real(dp), allocatable :: x(:)
        real(dp), parameter :: total_sum = 4*atan(1._dp), tolerance = epsilon(1._dp)*100
        real(dp) :: xsum(ncalc), err(ncalc)
        logical, allocatable :: mask(:), nmask(:)

        allocate(x(n))
        do i = 1, n 
            x(i) = 8*atan(1._dp)*(real(i,kind=dp)-0.5_dp)/real(n,kind=dp)**2
        end do
        allocate(mask(n),source=.false.); mask(1:n:2) = .true.
        allocate(nmask(n)); nmask = .not.mask
        ! scramble array
        do i = 1, n
            call random_number(u) 
            j = 1 + floor(n*u)
            call swap( x(i), x(j) )
            call swap( mask(i), mask(j) )
            call swap( nmask(i), nmask(j) )
        end do
        
        xsum(1) = sum(x)        ! compiler intrinsic
        xsum(2) = stdlib_sum_kahan(x) ! chunked Kahan summation
        xsum(3) = stdlib_sum(x)       ! chunked summation
        err(1:ncalc) = abs(1._dp-xsum(1:ncalc)/total_sum)
        
        call check(error, all(err(:)<tolerance) , "real sum is not accurate" )
        if (allocated(error)) return

        xsum(1) = sum(x,mask)+sum(x,nmask) ! compiler intrinsic
        xsum(2) = stdlib_sum_kahan(x,mask)+stdlib_sum_kahan(x,nmask) ! chunked Kahan summation
        xsum(3) = stdlib_sum(x,mask)+stdlib_sum(x,nmask) ! chunked summation
        err(1:ncalc) = abs(1._dp-xsum(1:ncalc)/total_sum)
        
        call check(error, all(err(:)<tolerance) , "masked real sum is not accurate" )
        if (allocated(error)) return
    end block

    block
        complex(sp), allocatable :: x(:)
        real(sp), parameter :: total_sum = 4*atan(1._sp), tolerance = epsilon(1._sp)*100
        real(sp) :: err(ncalc)
        complex(sp) :: xsum(ncalc)
        logical, allocatable :: mask(:), nmask(:)

        allocate(x(n))
        do i = 1, n
            x(i) = (8*atan(1._sp)*(real(i,kind=sp)-0.5_sp)/n**2)*cmplx(1._sp,1._sp)
        end do
        
        allocate(mask(n),source=.false.); mask(1:n:2) = .true.
        allocate(nmask(n)); nmask = .not.mask
        ! scramble array
        do i = 1, n
            call random_number(u) 
            j = 1 + floor(n*u)
            call swap( x(i), x(j) )
            call swap( mask(i), mask(j) )
            call swap( nmask(i), nmask(j) )
        end do
        
        xsum(1) = sum(x)        ! compiler intrinsic
        xsum(2) = stdlib_sum_kahan(x) ! chunked Kahan summation
        xsum(3) = stdlib_sum(x)       ! chunked summation
        err(1:ncalc) = abs(1._sp-(xsum(1:ncalc)%re)/total_sum)
        
        
        call check(error, all(err(:)<tolerance) , "complex sum is not accurate" )
        if (allocated(error)) return

        xsum(1) = sum(x,mask)+sum(x,nmask) ! compiler intrinsic
        xsum(2) = stdlib_sum_kahan(x,mask)+stdlib_sum_kahan(x,nmask) ! chunked Kahan summation
        xsum(3) = stdlib_sum(x,mask)+stdlib_sum(x,nmask) ! chunked summation
        err(1:ncalc) = abs(1._sp-(xsum(1:ncalc)%re)/total_sum)
        
        call check(error, all(err(:)<tolerance) , "complex masked sum is not accurate" )
        if (allocated(error)) return
    end block
    block
        complex(dp), allocatable :: x(:)
        real(dp), parameter :: total_sum = 4*atan(1._dp), tolerance = epsilon(1._dp)*100
        real(dp) :: err(ncalc)
        complex(dp) :: xsum(ncalc)
        logical, allocatable :: mask(:), nmask(:)

        allocate(x(n))
        do i = 1, n
            x(i) = (8*atan(1._dp)*(real(i,kind=dp)-0.5_dp)/n**2)*cmplx(1._dp,1._dp)
        end do
        
        allocate(mask(n),source=.false.); mask(1:n:2) = .true.
        allocate(nmask(n)); nmask = .not.mask
        ! scramble array
        do i = 1, n
            call random_number(u) 
            j = 1 + floor(n*u)
            call swap( x(i), x(j) )
            call swap( mask(i), mask(j) )
            call swap( nmask(i), nmask(j) )
        end do
        
        xsum(1) = sum(x)        ! compiler intrinsic
        xsum(2) = stdlib_sum_kahan(x) ! chunked Kahan summation
        xsum(3) = stdlib_sum(x)       ! chunked summation
        err(1:ncalc) = abs(1._dp-(xsum(1:ncalc)%re)/total_sum)
        
        
        call check(error, all(err(:)<tolerance) , "complex sum is not accurate" )
        if (allocated(error)) return

        xsum(1) = sum(x,mask)+sum(x,nmask) ! compiler intrinsic
        xsum(2) = stdlib_sum_kahan(x,mask)+stdlib_sum_kahan(x,nmask) ! chunked Kahan summation
        xsum(3) = stdlib_sum(x,mask)+stdlib_sum(x,nmask) ! chunked summation
        err(1:ncalc) = abs(1._dp-(xsum(1:ncalc)%re)/total_sum)
        
        call check(error, all(err(:)<tolerance) , "complex masked sum is not accurate" )
        if (allocated(error)) return
    end block

    ndarray : block
        use stdlib_strings, only: to_string
        real(sp), allocatable :: x(:,:,:)
        real(sp), parameter :: tolerance = epsilon(1._sp)*100
        integer :: i 

        allocate(x(100,100,10))
        call random_number(x)
        !> sum all elements
        call check(error, abs( sum(x) - stdlib_sum(x) )<tolerance*size(x) , "KO: full ndarray stdlib_sum" )
        if (allocated(error)) return

        call check(error, abs( sum(x) - stdlib_sum_kahan(x) )<tolerance*size(x) , "KO: full ndarray stdlib_sum_kahan" )
        if (allocated(error)) return

        !> sum over specific rank dim
        do i = 1, rank(x)
            call check(error, norm2( sum(x,dim=i) - stdlib_sum(x,dim=i) )<tolerance*size(x) ,&
                        "KO: ndarray stdlib_sum over dim "//to_string(i) )
            if (allocated(error)) return

            call check(error, norm2( sum(x,dim=i) - stdlib_sum_kahan(x,dim=i) )<tolerance*size(x) ,&
                        "KO: ndarray stdlib_sum_kahan over dim "//to_string(i) )
            if (allocated(error)) return
        end do
    end block ndarray

end subroutine

subroutine test_dot_product(error)
    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    !> Internal parameters and variables
    integer, parameter :: n = 1e3, ncalc = 3
    real(sp) :: u
    integer :: iter, i, j
    !====================================================================================
    block
        real(sp), allocatable :: x(:)
        real(sp), parameter :: total_sum = 4*atan(1._sp), tolerance = epsilon(1._sp)*100
        real(sp) :: xsum(ncalc), err(ncalc)

        allocate(x(n))
        do i = 1, n 
            x(i) = 2*sqrt( 2*atan(1._sp)*(real(i,kind=sp)-0.5_sp) )/n
        end do
        ! scramble array
        do i = 1, n
            call random_number(u) 
            j = 1 + floor(n*u)
            call swap( x(i), x(j) )
        end do
        
        xsum(1) = dot_product(x,x) ! compiler intrinsic
        xsum(2) = stdlib_dot_product_kahan(x,x) ! chunked Kahan summation
        xsum(3) = stdlib_dot_product(x,x)       ! chunked summation
        err(1:ncalc) = abs(1._sp-xsum(1:ncalc)/total_sum)
        
        call check(error, all(err(:)<tolerance) , "real dot_product is not accurate" )
        if (allocated(error)) return
    end block
    block
        real(dp), allocatable :: x(:)
        real(dp), parameter :: total_sum = 4*atan(1._dp), tolerance = epsilon(1._dp)*100
        real(dp) :: xsum(ncalc), err(ncalc)

        allocate(x(n))
        do i = 1, n 
            x(i) = 2*sqrt( 2*atan(1._dp)*(real(i,kind=dp)-0.5_dp) )/n
        end do
        ! scramble array
        do i = 1, n
            call random_number(u) 
            j = 1 + floor(n*u)
            call swap( x(i), x(j) )
        end do
        
        xsum(1) = dot_product(x,x) ! compiler intrinsic
        xsum(2) = stdlib_dot_product_kahan(x,x) ! chunked Kahan summation
        xsum(3) = stdlib_dot_product(x,x)       ! chunked summation
        err(1:ncalc) = abs(1._dp-xsum(1:ncalc)/total_sum)
        
        call check(error, all(err(:)<tolerance) , "real dot_product is not accurate" )
        if (allocated(error)) return
    end block

    block
        complex(sp), allocatable :: x(:)
        real(sp), parameter :: total_sum = 4*atan(1._sp), tolerance = epsilon(1._sp)*100
        real(sp) :: err(ncalc)
        complex(sp) :: xsum(ncalc)

        allocate(x(n))
        do i = 1, n
            x(i) = ( 2*sqrt( 2*atan(1._sp)*(real(i,kind=sp)-0.5_sp) ) / n )*cmplx(1._sp,1._sp)
        end do
        ! scramble array
        do i = 1, n
            call random_number(u) 
            j = 1 + floor(n*u)
            call swap( x(i), x(j) )
        end do
        
        xsum(1) = dot_product(x,x) ! compiler intrinsic
        xsum(2) = stdlib_dot_product_kahan(x,x) ! chunked Kahan dot_product
        xsum(3) = stdlib_dot_product(x,x)       ! chunked dot_product
        err(1:ncalc) = abs(1._sp-xsum(1:ncalc)%re/(2*total_sum))
         
        call check(error, all(err(:)<tolerance) , "complex dot_product is not accurate" )
        if (allocated(error)) return
    end block

    block ! test for https://github.com/fortran-lang/stdlib/issues/1016
        complex(sp) :: x(128), y(128)
        real(sp) :: z(128,2)
        real(sp), parameter :: tolerance = epsilon(1._sp)*100000
        real(sp) :: err(2)
        complex(sp) :: p(3)

        call random_number(z)
        x%re = z(:, 1); x%im = z(:, 2)
        call random_number(z)
        y%re = z(:, 1); y%im = z(:, 2)
        
        p(1) = dot_product(x,y) ! compiler intrinsic
        p(2) = stdlib_dot_product_kahan(x,y) ! chunked Kahan dot_product
        p(3) = stdlib_dot_product(x,y)       ! chunked dot_product
        err(1:2) = sqrt((p(2:3)%re - p(1)%re)**2 + (p(2:3)%im - p(1)%im)**2)
        
        call check(error, all(err(:)<tolerance) , "complex dot_product does not conform to the standard" )
        if (allocated(error)) return
    end block
    block
        complex(dp), allocatable :: x(:)
        real(dp), parameter :: total_sum = 4*atan(1._dp), tolerance = epsilon(1._dp)*100
        real(dp) :: err(ncalc)
        complex(dp) :: xsum(ncalc)

        allocate(x(n))
        do i = 1, n
            x(i) = ( 2*sqrt( 2*atan(1._dp)*(real(i,kind=dp)-0.5_dp) ) / n )*cmplx(1._dp,1._dp)
        end do
        ! scramble array
        do i = 1, n
            call random_number(u) 
            j = 1 + floor(n*u)
            call swap( x(i), x(j) )
        end do
        
        xsum(1) = dot_product(x,x) ! compiler intrinsic
        xsum(2) = stdlib_dot_product_kahan(x,x) ! chunked Kahan dot_product
        xsum(3) = stdlib_dot_product(x,x)       ! chunked dot_product
        err(1:ncalc) = abs(1._dp-xsum(1:ncalc)%re/(2*total_sum))
         
        call check(error, all(err(:)<tolerance) , "complex dot_product is not accurate" )
        if (allocated(error)) return
    end block

    block ! test for https://github.com/fortran-lang/stdlib/issues/1016
        complex(dp) :: x(128), y(128)
        real(dp) :: z(128,2)
        real(dp), parameter :: tolerance = epsilon(1._dp)*100000
        real(dp) :: err(2)
        complex(dp) :: p(3)

        call random_number(z)
        x%re = z(:, 1); x%im = z(:, 2)
        call random_number(z)
        y%re = z(:, 1); y%im = z(:, 2)
        
        p(1) = dot_product(x,y) ! compiler intrinsic
        p(2) = stdlib_dot_product_kahan(x,y) ! chunked Kahan dot_product
        p(3) = stdlib_dot_product(x,y)       ! chunked dot_product
        err(1:2) = sqrt((p(2:3)%re - p(1)%re)**2 + (p(2:3)%im - p(1)%im)**2)
        
        call check(error, all(err(:)<tolerance) , "complex dot_product does not conform to the standard" )
        if (allocated(error)) return
    end block

end subroutine
    
end module test_intrinsics

program tester
    use, intrinsic :: iso_fortran_env, only : error_unit
    use testdrive, only : run_testsuite, new_testsuite, testsuite_type
    use test_intrinsics, only : collect_suite
    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
        new_testsuite("sparse", collect_suite) &
        ]

    do is = 1, size(testsuites)
        write(error_unit, fmt) "Testing:", testsuites(is)%name
        call run_testsuite(testsuites(is)%collect, error_unit, stat)
    end do

    if (stat > 0) then
        write(error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
        error stop
    end if
end program