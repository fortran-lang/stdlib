program test_median
    use stdlib_error, only: check
    use stdlib_kinds, only: int32, sp, dp
    use stdlib_stats, only: median
    implicit none
    real(sp), parameter :: sptol = 1000 * epsilon(1._sp)
    real(dp), parameter :: dptol = 2000 * epsilon(1._dp)

    call test_median_int32()
    call test_median_sp()

contains
subroutine test_median_int32()
    integer(int32), allocatable :: d1(:), d2(:,:), d3(:,:,:)

    !even number 
    d1 = [10, 2, -3, -4, 6, -6, 7, -8, 9, 0, 1, 20]
    call check(mod(size(d1), 2) == 0, 'should be an even number')
    call check( abs(median(d1) - 1.5_dp) < dptol, 'int32 median(d1), even')

    d2 = reshape(d1, [3, 4])
    call check(mod(size(d2), 2) == 0, 'should be an even number')
    call check( abs(median(d2) - 1.5_dp) < dptol, 'int32 median(d2), even')

    d3 = reshape(d1, [2, 3, 2])
    call check(mod(size(d3), 2) == 0, 'should be an even number')
    call check( abs(median(d3) - 1.5_dp) < dptol, 'int32 median(d3), even')

    !following dimension
    call check( abs(median(d1, 1) - 1.5_dp) < dptol, 'int32 median(d1, 1), even')

    call check( sum(abs(median(d2, 1) - [2._dp, -4._dp, 7._dp, 1._dp])) < dptol, &
                 'int32 median(d2, 1)')
    call check( sum(abs(median(d2, 2) - [3.5_dp, 1.5_dp, 3._dp])) < dptol,            &
                 'int32 median(d2, 2)')




    !odd number
    d1 = [d1, 20]
    call check(mod(size(d1), 2) == 1, 'should be an odd number')
    call check( abs(median(d1) - 2._dp) < dptol, 'int32 median(d1), odd')

    d2 = reshape(d1, [3, 5], pad = [0_int32])
    call check(mod(size(d2), 2) == 1, 'should be an odd number')
    call check( abs(median(d2) - 1._dp) < dptol, 'int32 median(d2), odd')

    d3 = reshape(d1, [1, 3, 5], pad = [0_int32])
    call check(mod(size(d3), 2) == 1, 'should be an odd number')
    call check( abs(median(d3) - 1._dp) < dptol, 'int32 median(d3), odd')

    !following dimension
    call check( abs(median(d1, 1) - 2._dp) < dptol, 'int32 median(d1, 1)')

    call check( sum(abs(median(d2, 1) - [2._dp, -4._dp, 7._dp, 1._dp, 0._dp])) < dptol, &
                 'int32 median(d2, 1), odd')
    call check( sum(abs(median(d2, 2) - [7._dp, 1._dp, 0._dp])) < dptol,                &
                 'int32 median(d2, 2), odd')

end subroutine

subroutine test_median_sp()
    real(sp), allocatable :: d1(:), d2(:,:), d3(:,:,:)

    !even number 
    d1 = [10., 2., -3., -4., 6., -6., 7., -8., 9., 0., 1., 20.]
    call check(mod(size(d1), 2) == 0, 'should be an even number')
    call check( abs(median(d1) - 1.5_sp) < sptol, 'sp median(d1), even')

    d2 = reshape(d1, [3, 4])
    call check(mod(size(d2), 2) == 0, 'should be an even number')
    call check( abs(median(d2) - 1.5_sp) < sptol, 'sp median(d2), even')

    d3 = reshape(d1, [2, 3, 2])
    call check(mod(size(d3), 2) == 0, 'should be an even number')
    call check( abs(median(d3) - 1.5_sp) < sptol, 'sp median(d3), even')


    !following dimension
    call check( abs(median(d1, 1) - 1.5_sp) < sptol, 'sp median(d1, 1), even')

    call check( sum(abs(median(d2, 1) - [2._sp, -4._sp, 7._sp, 1._sp])) < sptol, &
                 'sp median(d2, 1)')
    call check( sum(abs(median(d2, 2) - [3.5_sp, 1.5_sp, 3._sp])) < sptol,            &
                 'sp median(d2, 2)')


    !odd number
    d1 = [d1, 20.]
    call check(mod(size(d1), 2) == 1, 'should be an odd number')
    call check( abs(median(d1) - 2._sp) < sptol, 'sp median(d1), odd')

    d2 = reshape(d1, [3, 5], pad = [0._sp])
    call check(mod(size(d2), 2) == 1, 'should be an odd number')
    call check( abs(median(d2) - 1._sp) < sptol, 'sp median(d2), odd')

    d3 = reshape(d1, [1, 3, 5], pad = [0._sp])
    call check(mod(size(d3), 2) == 1, 'should be an odd number')
    call check( abs(median(d3) - 1._sp) < sptol, 'sp median(d3), odd')

    !following dimension
    call check( abs(median(d1, 1) - 2._sp) < sptol, 'sp median(d1, 1)')

    call check( sum(abs(median(d2, 1) - [2._sp, -4._sp, 7._sp, 1._sp, 0._sp])) < sptol, &
                 'sp median(d2, 1), odd')
    call check( sum(abs(median(d2, 2) - [7._sp, 1._sp, 0._sp])) < sptol,            &
                 'sp median(d2, 2), odd')


end subroutine

end program
