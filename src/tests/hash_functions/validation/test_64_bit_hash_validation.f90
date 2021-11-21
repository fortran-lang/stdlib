program test_64_bit_hash_validation
!! Compares the output of Fortran versions of 64 bit hash procedures
!! withe the original C/C++ versions

    use, intrinsic :: iso_fortran_env, only: int8, int32, int64, real64
    use, intrinsic :: iso_c_binding, only : c_loc, c_long
    use stdlib_64_bit_hash_functions, only: &
        pengy_hash,                         &
        new_pengy_hash_seed,                &
        spooky_hash,                        &
        new_spooky_hash_seed
    use pengy_wrapper, only: c_pengyhash
    use spookyv2_wrapper, only: c_spooky128

    implicit none

    integer(int64) :: pengy_hash_code, c_pengy_hash_code, &
        spooky_seed(2), spooky_hash_code(2), c_spooky_hash_code(2)
    integer(int32) ::  pengy_seed
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

    pengy_seed = 0_int64
    call new_pengy_hash_seed( pengy_seed )

    do i=0, 512
        pengy_hash_code = pengy_hash( test_array(1:i), pengy_seed )
        c_pengy_hash_code = c_pengyhash( test_array(1:i), pengy_seed )
        if ( .not. ( pengy_hash_code == c_pengy_hash_code ) ) then
            write(*,*) "PENGY_HASH failed for INT8 array size = ", i
            write(*,*) 'PENGY_HASH_CODE = ', pengy_hash_code
            write(*,*) 'C_PENGY_HASH_CODE = ', c_pengy_hash_code
            write(*,*) "Array = ", test_array(1:i)
            stop "Hash failure"
        end if
    end do
    write(*,*) "PENGY_HASH passed validation test."

    spooky_seed = [ 0_int64, 0_int64 ]
    call new_spooky_hash_seed( spooky_seed )

    do i=0, 512
        spooky_hash_code = spooky_hash( test_array(1:i), spooky_seed )
        c_spooky_hash_code = c_spooky128( test_array(1:i), spooky_seed )
        if ( .not. all( spooky_hash_code == c_spooky_hash_code ) ) then
            write(*,*) "SPOOKY_HASH failed for INT8 array size = ", i
            write(*,*) 'SPOOKY_HASH_CODE = ', spooky_hash_code
            write(*,*) 'C_SPOOKY_HASH_CODE = ', c_spooky_hash_code
            write(*,*) "Array = ", test_array(1:i)
            stop "Hash failure"
        end if
    end do
    write(*,*) "SPOOKY_HASH passed validation test."

end program test_64_bit_hash_validation
