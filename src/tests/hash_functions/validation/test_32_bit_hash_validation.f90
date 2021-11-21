program test_32_bit_hash_validation
!! Compares the output of Fortran versions of 64 bit hash procedures
!! withe the original C/C++ versions

    use, intrinsic :: iso_fortran_env, only: int8, int32, int64, real64
    use, intrinsic :: iso_c_binding, only : c_loc, c_long
    use stdlib_32_bit_hash_functions, only: &
        nmhash32,                           &
        new_nmhash32_seed,                  &
        nmhash32x,                          &
        new_nmhash32x_seed,                 &
        water_hash,                         &
        new_water_hash_seed
    use nmhash_wrapper, only: c_nmhash32, c_nmhash32x
    use waterhash_wrapper, only: c_waterhash

    implicit none

    integer(int32) :: nmhash32_code, c_nmhash32_code, &
        nmhash32x_code, c_nmhash32x_code, water_hash_code, c_waterhash_code
    integer(int32) :: nmhash32_seed, nmhash32x_seed
    integer(int64) :: waterhash_seed
    integer(int8)  :: test_array(512)
    real(real64)   :: rand(128)
    integer(int32) :: dummy(128)
    integer        :: i

! Create test array
    call random_number( rand )
    do i=1, 128
        dummy(i) = floor( rand(i) * 2_int64**32 - 2_int64**31, kind=int32 )
    end do
    test_array = transfer( dummy, 0_int8, 512 )

    waterhash_seed = 0
    call new_water_hash_seed( waterhash_seed )

    do i=0, 512
        water_hash_code = water_hash( test_array(1:i), waterhash_seed )
        c_waterhash_code = c_waterhash( test_array(1:i), waterhash_seed )
        if ( .not. ( water_hash_code == c_waterhash_code ) ) then
            write(*,*) "WATER_HASH failed for INT8 array size = ", i
            write(*,*) "WATERHASH_SEED = ", waterhash_seed
            write(*,*) 'WATER_HASH_CODE = ', water_hash_code
            write(*,*) 'C_WATERHASH_CODE = ', c_waterhash_code
            write(*,*) "Array = ", test_array(1:i)
            stop "Hash failure"
        end if
    end do
    write(*,*) "WATER_HASH passed validation test."

    nmhash32_seed = 0
!    call new_nmhash32_seed( nmhash32_seed )

    do i=0, 512
        nmhash32_code = nmhash32( test_array(1:i), nmhash32_seed )
        c_nmhash32_code = c_nmhash32( test_array(1:i), nmhash32_seed )
        if ( .not. ( nmhash32_code == c_nmhash32_code ) ) then
            write(*,*) "NMHASH32 failed for INT8 array size = ", i
            write(*,*) "NMHASH32_SEED = ", nmhash32_seed
            write(*,*) 'NMHASH32_CODE = ', nmhash32_code
            write(*,*) 'C_NMHASH32_CODE = ', c_nmhash32_code
            write(*,*) "Array = ", test_array(1:i)
            stop "Hash failure"
        end if
    end do
    write(*,*) "NMHASH32 passed validation test."

    nmhash32x_seed = 0
!    call new_nmhash32x_seed( nmhash32x_seed )

    do i=0, 512
        nmhash32x_code = nmhash32x( test_array(1:i), nmhash32_seed )
        c_nmhash32x_code = c_nmhash32x( test_array(1:i), nmhash32_seed )
        if ( .not. ( nmhash32x_code == c_nmhash32x_code ) ) then
            write(*,*) "NMHASH32X failed for INT8 array size = ", i
            write(*,*) "NMHASH32X_SEED = ", nmhash32x_seed
            write(*,*) 'NMHASH32X_CODE = ', nmhash32x_code
            write(*,*) 'C_NMHASH32X_CODE = ', c_nmhash32x_code
            write(*,*) "Array = ", test_array(1:i)
            stop "Hash failure"
        end if
    end do
    write(*,*) "NMHASH32X passed validation test."

end program test_32_bit_hash_validation
