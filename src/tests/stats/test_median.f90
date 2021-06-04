program test_median
    use stdlib_error, only: check
    use stdlib_kinds, only: int32, sp, dp
    use stdlib_stats, only: median, mean
    use,intrinsic :: ieee_arithmetic, only : ieee_is_nan
    implicit none
    real(sp), parameter :: sptol = 1000 * epsilon(1._sp)
    real(dp), parameter :: dptol = 2000 * epsilon(1._dp)

    call test_median_int32()
    call test_median_sp()

contains
subroutine test_median_int32()
    integer(int32), allocatable :: d0(:), d1(:), d2(:,:), d3(:,:,:)

    allocate(d0(0))
    call check(size(d0) == 0, 'should be of size 0')

    d1 = [10, 2, -3, -4, 6, -6, 7, -8, 9, 0, 1, 20]
    call check(mod(size(d1), 2) == 0, 'should be an even number')

    d2 = reshape(d1, [3, 4])
    call check(mod(size(d2), 2) == 0, 'should be an even number')

    d3 = reshape(d1, [2, 3, 2])
    call check(mod(size(d3), 2) == 0, 'should be an even number')

    !median_all
    call check( ieee_is_nan(median(d0)), 'int32 median(d0)' )
    call check( ieee_is_nan(median(d0, .false.)), 'int32 median(d0, .false.)' )
    call check( ieee_is_nan(median(d1, .false.)), 'int32 median(d1, .false.)' )
    call check( ieee_is_nan(median(d2, .false.)), 'int32 median(d2, .false.)' )
    call check( ieee_is_nan(median(d3, .false.)), 'int32 median(d3, .false.)' )

    call check( abs(median(d1) - 1.5_dp) < dptol, 'int32 median(d1), even')
    call check( abs(median(d2) - 1.5_dp) < dptol, 'int32 median(d2), even')
    call check( abs(median(d3) - 1.5_dp) < dptol, 'int32 median(d3), even')

    !median
    call check( ieee_is_nan(median(d0, 1)), 'int32 median(d0, 1)' )
    call check( ieee_is_nan(median(d0, 1, .false.)), 'int32 median(d0, 1, .false.)' )
    call check( ieee_is_nan(median(d1, 1, .false.)), 'int32 median(d1, 1, .false.)' )
    call check( any(ieee_is_nan(median(d2, 1, .false.))), 'int32 median(d2, 1, .false.)' )
    call check( any(ieee_is_nan(median(d2, 2, .false.))), 'int32 median(d2, 2, .false.)' )
    call check( any(ieee_is_nan(median(d3, 1, .false.))), 'int32 median(d3, 1, .false.)' )
    call check( any(ieee_is_nan(median(d3, 2, .false.))), 'int32 median(d3, 2, .false.)' )
    call check( any(ieee_is_nan(median(d3, 3, .false.))), 'int32 median(d3, 3, .false.)' )

    call check( abs(median(d1, 1) - 1.5_dp) < dptol, 'int32 median(d1, 1), even')
    call check( sum(abs(median(d2, 1) - [2._dp, -4._dp, 7._dp, 1._dp])) < dptol, &
                 'int32 median(d2, 1)')
    call check( sum(abs(median(d2, 2) - [3.5_dp, 1.5_dp, 3._dp])) < dptol,       &
                 'int32 median(d2, 2)')

    !median_mask_all
    call check( ieee_is_nan(median(d0, d0 > 0)), 'int32 median(d0, d0 > 0)' )
    call check( ieee_is_nan(median(d1, d1 > huge(d1))), 'int32 median(d1, d1 > huge(d1))' )
    call check( ieee_is_nan(median(d2, d2 > huge(d2))), 'int32 median(d2, d2 > huge(d2))' )
    call check( ieee_is_nan(median(d3, d3 > huge(d3))), 'int32 median(d3, d3 > huge(d3))' )
    call check( (median(d1, d1 > 0) - 7._dp) < dptol, 'int32 median(d1, d1 > 0)' ) 
    call check( (median(d2, d2 > 0) - 7._dp) < dptol, 'int32 median(d2, d2 > 0)' ) 
    call check( (median(d3, d3 > 0) - 7._dp) < dptol, 'int32 median(d3, d3 > 0)' ) 

    !median mask
    call check( ieee_is_nan(median(d0, 1, d0 > 0)), 'int32 median(d0, 1, d0 > 0)' )
    call check( ieee_is_nan(median(d1, 1, d1 > huge(d1))), 'int32 median(d1, 1, d1 > huge(d1))' )
    call check( any(ieee_is_nan(median(d2, 1, d2 > huge(d2)))), 'int32 median(d2, 1, d2 > huge(d2))' )
    call check( any(ieee_is_nan(median(d3, 1, d3 > huge(d3)))), 'int32 median(d3, 1, d3 > huge(d3))' )
    call check( (median(d1, 1, d1 > 0) - 7._dp) < dptol, 'int32 median(d1, 1, d1 >0)') 
    call check( sum(abs( (median(d2, 1, d2 > 0) - [ 6._dp, 6._dp, 8._dp, 10.5_dp] )  )) &
                 < dptol, 'int32 median(d2, 1, d2 > 0 )') 
    call check( sum(abs( (median(d2, 2, d2 > 0) - [ 8.5_dp, 2._dp, 14.5_dp] )  ))       &
                 < dptol, 'int32 median(d2, 2, d2 > 0 )') 
    call check( any(ieee_is_nan(median(d3, 1, d3 > 0))), 'int32 median(d3, 1, d3 > 0)')

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

    call check( abs(median(d1, 1) - 2._dp) < dptol, 'int32 median(d1, 1)')
    call check( sum(abs(median(d2, 1) - [2._dp, -4._dp, 7._dp, 1._dp, 0._dp])) < dptol, &
                 'int32 median(d2, 1), odd')
    call check( sum(abs(median(d2, 2) - [7._dp, 1._dp, 0._dp])) < dptol,                &
                 'int32 median(d2, 2), odd')

end subroutine

subroutine test_median_sp()
    real(sp), allocatable :: d0(:), d1(:), d2(:,:), d3(:,:,:)

    allocate(d0(0))
    call check(size(d0) == 0, 'should be of size 0')

    d1 = [10., 2., -3., -4., 6., -6., 7., -8., 9., 0., 1., 20.]
    call check(mod(size(d1), 2) == 0, 'should be an even number')

    d2 = reshape(d1, [3, 4])
    call check(mod(size(d2), 2) == 0, 'should be an even number')

    d3 = reshape(d1, [2, 3, 2])
    call check(mod(size(d3), 2) == 0, 'should be an even number')

    !median_all
    call check( ieee_is_nan(median(d0)), 'sp median(d0)' )
    call check( ieee_is_nan(median(d0, .false.)), 'sp median(d0, .false.)' )
    call check( ieee_is_nan(median(d1, .false.)), 'sp median(d1, .false.)' )
    call check( ieee_is_nan(median(d2, .false.)), 'sp median(d2, .false.)' )
    call check( ieee_is_nan(median(d3, .false.)), 'sp median(d3, .false.)' )

    call check( abs(median(d1) - 1.5_sp) < sptol, 'sp median(d1), even')
    call check( abs(median(d2) - 1.5_sp) < sptol, 'sp median(d2), even')
    call check( abs(median(d3) - 1.5_sp) < sptol, 'sp median(d3), even')

    !median
    call check( ieee_is_nan(median(d0, 1)), 'sp median(d0, 1)' )
    call check( ieee_is_nan(median(d0, 1, .false.)), 'sp median(d0, 1, .false.)' )
    call check( ieee_is_nan(median(d1, 1, .false.)), 'sp median(d1, 1, .false.)' )
    call check( any(ieee_is_nan(median(d2, 1, .false.))), 'sp median(d2, 1, .false.)' )
    call check( any(ieee_is_nan(median(d2, 2, .false.))), 'sp median(d2, 2, .false.)' )
    call check( any(ieee_is_nan(median(d3, 1, .false.))), 'sp median(d3, 1, .false.)' )
    call check( any(ieee_is_nan(median(d3, 2, .false.))), 'sp median(d3, 2, .false.)' )
    call check( any(ieee_is_nan(median(d3, 3, .false.))), 'sp median(d3, 3, .false.)' )

    call check( abs(median(d1, 1) - 1.5_sp) < sptol, 'sp median(d1, 1), even')
    call check( sum(abs(median(d2, 1) - [2._sp, -4._sp, 7._sp, 1._sp])) < sptol, &
                 'sp median(d2, 1)')
    call check( sum(abs(median(d2, 2) - [3.5_sp, 1.5_sp, 3._sp])) < sptol,       &
                 'sp median(d2, 2)')

    !median_mask_all
    call check( ieee_is_nan(median(d0, d0 > 0)), 'sp median(d0, d0 > 0)' )
    call check( ieee_is_nan(median(d1, d1 > huge(d1))), 'sp median(d1, d1 > huge(d1))' )
    call check( ieee_is_nan(median(d2, d2 > huge(d2))), 'sp median(d2, d2 > huge(d2))' )
    call check( ieee_is_nan(median(d3, d3 > huge(d3))), 'sp median(d3, d3 > huge(d3))' )
    call check( (median(d1, d1 > 0) - 7._sp) < sptol, 'sp median(d1, d1 > 0)' ) 
    call check( (median(d2, d2 > 0) - 7._sp) < sptol, 'sp median(d2, d2 > 0)' ) 
    call check( (median(d3, d3 > 0) - 7._sp) < sptol, 'sp median(d3, d3 > 0)' ) 

    !median mask
    call check( ieee_is_nan(median(d0, 1, d0 > 0)), 'sp median(d0, 1, d0 > 0)' )
    call check( ieee_is_nan(median(d1, 1, d1 > huge(d1))), 'sp median(d1, 1, d1 > huge(d1))' )
    call check( any(ieee_is_nan(median(d2, 1, d2 > huge(d2)))), 'sp median(d2, 1, d2 > huge(d2))' )
    call check( any(ieee_is_nan(median(d3, 1, d3 > huge(d3)))), 'sp median(d3, 1, d3 > huge(d3))' )
    call check( (median(d1, 1, d1 > 0) - 7._sp) < sptol, 'sp median(d1, 1, d1 >0)') 
    call check( sum(abs( (median(d2, 1, d2 > 0) - [ 6._sp, 6._sp, 8._sp, 10.5_sp] )  )) &
                 < sptol, 'sp median(d2, 1, d2 > 0 )') 
    call check( sum(abs( (median(d2, 2, d2 > 0) - [ 8.5_sp, 2._sp, 14.5_sp] )  ))       &
                 < sptol, 'sp median(d2, 2, d2 > 0 )') 
    call check( any(ieee_is_nan(median(d3, 1, d3 > 0))), 'sp median(d3, 1, d3 > 0)')

    !odd number
    d1 = [d1, 20._sp]
    call check(mod(size(d1), 2) == 1, 'should be an odd number')

    call check( abs(median(d1) - 2._sp) < sptol, 'sp median(d1), odd')

    d2 = reshape(d1, [3, 5], pad = [0._sp])
    call check(mod(size(d2), 2) == 1, 'should be an odd number')
    call check( abs(median(d2) - 1._sp) < sptol, 'sp median(d2), odd')

    d3 = reshape(d1, [1, 3, 5], pad = [0._sp])
    call check(mod(size(d3), 2) == 1, 'should be an odd number')
    call check( abs(median(d3) - 1._sp) < sptol, 'sp median(d3), odd')

    call check( abs(median(d1, 1) - 2._sp) < sptol, 'sp median(d1, 1)')
    call check( sum(abs(median(d2, 1) - [2._sp, -4._sp, 7._sp, 1._sp, 0._sp])) < sptol, &
                 'sp median(d2, 1), odd')
    call check( sum(abs(median(d2, 2) - [7._sp, 1._sp, 0._sp])) < sptol,                &
                 'sp median(d2, 2), odd')

end subroutine




end program
