!! HASH_VALIDITY_TEST processes a vector of eight bit integers,
!! extracting subvectors of length 0, 1, 2, ... 2048 from the beginning
!! hashing each subvector and comparing the resulting hash with the
!! corresponding hash produced by the original C/C++ code, stopping if
!! they are different. As the original C/C++ code was typically developed
!! for Little-Endian machines the testing should only be cone on such
!! machones. The Fortran codes also assume two's complement integers.
!! The code set assume that C's int32_t and int64_t have the same
!! representation as Firtrans int32 and int64 respectively.

program hash_validity_test

    use, intrinsic :: iso_fortran_env, only: int8, int32, int64, real64
    use stdlib_32_bit_hash_functions, only: &
        little_endian, &
        nmhash32,      &
        nmhash32x,     &
        water_hash
    use stdlib_64_bit_hash_functions, only: &
        pengy_hash, &
        spooky_hash

    integer(int32), parameter :: nm_seed = int( z'deadbeef', int32 )
    integer(int64), parameter :: water_seed = int( z'deadbeef1eadbeef', int64 )
    integer(int32), parameter :: pengy_seed = int( z'deadbeef', int32 )
    integer(int64), parameter :: spooky_seed(2) = [ water_seed, water_seed ]
    integer        :: index
    integer        :: lun
    integer(int8)  :: key_array(2048)
    integer(int32) :: c_nmhash32(0:2048)
    integer(int32) :: c_nmhash32x(0:2048)
    integer(int32) :: c_water_hash(0:2048)
    integer(int64) :: c_pengy_hash(0:2048)
    integer(int64) :: c_spooky_hash(0:1, 0:2048)


    ! Test for endianness
    if ( .not. little_endian ) then
        stop "The processor is not Little-Endian"
    end if

    ! Read key array used to generate hash array
    open(newunit=lun, file="key_array.bin", form="unformatted", &
        access="stream", status="old", action="read")
    read(lun) key_array
    close(lun)

    ! Read hash array generated from key array by the C version of nmhash32
    open(newunit=lun, file="c_nmhash32_array.bin", form="unformatted", &
        access="stream", status="old", action="read")
    read(lun) c_nmhash32
    close(lun)

    do index=0, 2048
        if ( c_nmhash32(index) /= nmhash32(key_array(1:index), nm_seed) ) then
            write(*,'("NMHASH32 failed for KEY_ARRAY(1:", I0, ")")') index
            stop "NMHASH32 is invalid."
        end if
    end do
    write(*,*) "NMHASH32 is valid."

    ! Read hash array generated from key array by the C version of nmhash32x
    open(newunit=lun, file="c_nmhash32x_array.bin", form="unformatted", &
        access="stream", status="old", action="read")
    read(lun) c_nmhash32x
    close(lun)

    do index=0, 2048
        if ( c_nmhash32x(index) /= nmhash32x(key_array(1:index), nm_seed) ) then
            write(*,'("NMHASH32X failed for KEY_ARRAY(1:", I0, ")")') index
            stop "NMHASH32X is invalid."
        end if
    end do
    write(*,*) "NMHASH32X is valid."

    ! Read hash array generated from key array by the C version of water hash
    open(newunit=lun, file="c_water_hash_array.bin", form="unformatted", &
        access="stream", status="old", action="read")
    read(lun) c_water_hash
    close(lun)

    do index=0, 2048
        if ( c_water_hash(index) /= &
             water_hash(key_array(1:index), water_seed) ) then
            write(*,'("WATER_HASH failed for KEY_ARRAY(1:", I0, ")")') index
            stop "WATER_HASH is invalid."
        end if
    end do
    write(*,*) "WATER_HASH is valid."

    ! Read hash array generated from key array by the C version of pengy hash
    open(newunit=lun, file="c_pengy_hash_array.bin", form="unformatted", &
        access="stream", status="old", action="read")
    read(lun) c_pengy_hash
    close(lun)

    do index=0, 2048
        if ( c_pengy_hash(index) /= &
             pengy_hash(key_array(1:index), pengy_seed) ) then
            write(*,'("PENGY_HASH failed for KEY_ARRAY(1:", I0, ")")') index
            stop "PENGY_HASH is invalid."
        end if
    end do
    write(*,*) "PENGY_HASH is valid."

    ! Read hash array generated from key array by the C version of Spooky hash
    open(newunit=lun, file="c_spooky_hash_array.bin", form="unformatted", &
        access="stream", status="old", action="read")
    do index=0, 2048
        read(lun) c_spooky_hash(:, index)
    end do
    close(lun)

    do index=0, 2048
        if ( .not. all( c_spooky_hash(:,index) == &
                        spooky_hash(key_array(1:index), spooky_seed) ) ) then
            write(*,'("SPOOKY_HASH failed for KEY_ARRAY(:,1:", I0, ")")') index
            stop "SPOOKY_HASH is invalid."
        end if
    end do
    write(*,*) "SPOOKY_HASH is valid."

end program hash_validity_test
