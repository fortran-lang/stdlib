module test_hash_functions
    use testdrive, only : new_unittest, unittest_type, error_type, check, &
        skip_test
    use stdlib_kinds, only: int8, int32, int64, dp
    use stdlib_hash_32bit, only: little_endian &
        , nmhash32 &
        , nmhash32x &
        , water_hash
    use stdlib_hash_64bit, only: pengy_hash, spooky_hash
 
    implicit none
    private
    public :: collect_hash_functions
    public :: generate_key_array

    integer, parameter :: size_key_array = 2048

    integer(int32), parameter :: nm_seed = int( z'deadbeef', int32 )
    integer(int64), parameter :: water_seed = int( z'deadbeef1eadbeef', int64 )
    integer(int32), parameter :: pengy_seed = int( z'deadbeef', int32 )
    integer(int64), parameter :: spooky_seed(2) = [ water_seed, water_seed ]

    interface read_array
        module procedure read_array_int8
        module procedure read_array_int32
        module procedure read_array_int64
        module procedure read_2darray_int64
    end interface

contains

    !> Collect all exported unit tests
    subroutine collect_hash_functions(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("little_endian", test_little_endian) &
            , new_unittest("nmhash32", test_nmhash32) &
            , new_unittest("nmhash32x", test_nmhash32x) &
            , new_unittest("water_hash", test_water_hash) &
            , new_unittest("pengy_hash", test_pengy_hash) &
            , new_unittest("spooky_hash", test_spooky_hash) &
            , new_unittest("hash_determinism", test_hash_determinism) &
            , new_unittest("hash_distribution", test_hash_distribution) &
            ]

    end subroutine collect_hash_functions

    subroutine test_little_endian(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
 
        ! Skip test on big-endian systems instead of failing
        if (.not. little_endian) then
            call skip_test(error, "The processor is not Little-Endian (skipping)")
            return
        end if

    end subroutine test_little_endian

    subroutine test_nmhash32(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer :: index
        integer(int8) :: key_array(size_key_array)
        integer(int32) :: c_hash(0:size_key_array)

        call read_array("key_array.bin", key_array )

        ! Read hash array generated from key array by the C version of nmhash32
        call read_array("c_nmhash32_array.bin", c_hash) 

        do index=0, size_key_array
            call check(error, c_hash(index) == nmhash32(key_array(1:index), nm_seed) &
                , "NMHASH32 failed")
            if (allocated(error)) return
         end do

    end subroutine test_nmhash32

    subroutine test_nmhash32x(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer :: index
        integer(int8) :: key_array(size_key_array)
        integer(int32) :: c_hash(0:size_key_array)

        call read_array("key_array.bin", key_array )

        ! Read hash array generated from key array by the C version of nmhash32x
        call read_array("c_nmhash32x_array.bin", c_hash) 

        do index=0, size_key_array
            call check(error, c_hash(index) == nmhash32x(key_array(1:index), nm_seed) &
                , "NMHASH32X failed")
            if (allocated(error)) return
         end do

    end subroutine test_nmhash32x

    subroutine test_water_hash(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer :: index
        integer(int8) :: key_array(size_key_array)
        integer(int32) :: c_hash(0:size_key_array)

        call read_array("key_array.bin", key_array )

        ! Read hash array generated from key array by the C version of water_hash
        call read_array("c_water_hash_array.bin", c_hash) 

        do index=0, size_key_array
            call check(error, c_hash(index) == water_hash(key_array(1:index), water_seed) &
                , "WATER_HASH failed")
            if (allocated(error)) return
         end do

    end subroutine test_water_hash

    subroutine test_pengy_hash(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer :: index
        integer(int8) :: key_array(size_key_array)
        integer(int64) :: c_hash(0:size_key_array)

        call read_array("key_array.bin", key_array )

        ! Read hash array generated from key array by the C version of pengy_hash
        call read_array("c_pengy_hash_array.bin", c_hash) 

        do index=0, size_key_array
            call check(error, c_hash(index) == pengy_hash(key_array(1:index), pengy_seed) &
                , "PENGY_HASH failed")
            if (allocated(error)) return
         end do

    end subroutine test_pengy_hash

    subroutine test_spooky_hash(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer :: index
        integer(int8) :: key_array(size_key_array)
        integer(int64) :: c_hash(0:1, 0:size_key_array)

        call read_array("key_array.bin", key_array )

        ! Read hash array generated from key array by the C version of spooky_hash
        call read_array("c_spooky_hash_array.bin", c_hash) 

        do index=0, size_key_array
            call check(error, all(c_hash(:, index) == spooky_hash(key_array(1:index), spooky_seed)) &
                , "SPOOKY_HASH failed")
            if (allocated(error)) return
         end do

    end subroutine test_spooky_hash


    !> Test that all hash functions produce deterministic results
    !> This test runs on ALL platforms (LE and BE)
    subroutine test_hash_determinism(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer(int8) :: key(8)
        integer(int32) :: h32_a, h32_b
        integer(int64) :: h64_a, h64_b
        integer(int64) :: h128_a(2), h128_b(2)

        key = [1_int8, 2_int8, 3_int8, 4_int8, &
               5_int8, 6_int8, 7_int8, 8_int8]

        ! nmhash32 determinism
        h32_a = nmhash32(key, nm_seed)
        h32_b = nmhash32(key, nm_seed)
        call check(error, h32_a == h32_b, "NMHASH32 not deterministic")
        if (allocated(error)) return

        ! nmhash32x determinism
        h32_a = nmhash32x(key, nm_seed)
        h32_b = nmhash32x(key, nm_seed)
        call check(error, h32_a == h32_b, "NMHASH32X not deterministic")
        if (allocated(error)) return

        ! water_hash determinism
        h32_a = water_hash(key, water_seed)
        h32_b = water_hash(key, water_seed)
        call check(error, h32_a == h32_b, "WATER_HASH not deterministic")
        if (allocated(error)) return

        ! pengy_hash determinism
        h64_a = pengy_hash(key, pengy_seed)
        h64_b = pengy_hash(key, pengy_seed)
        call check(error, h64_a == h64_b, "PENGY_HASH not deterministic")
        if (allocated(error)) return

        ! spooky_hash determinism
        h128_a = spooky_hash(key, spooky_seed)
        h128_b = spooky_hash(key, spooky_seed)
        call check(error, all(h128_a == h128_b), &
            "SPOOKY_HASH not deterministic")
        if (allocated(error)) return

    end subroutine test_hash_determinism

    !> Collision sanity check: verify distinct inputs produce distinct hashes.
    !> For these well-tested hash functions with fixed deterministic inputs,
    !> an accidental collision probability is ~2^-32 (32-bit) or ~2^-64
    !> (64-bit), making this effectively deterministic.
    !> This test runs on ALL platforms (LE and BE).
    subroutine test_hash_distribution(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer(int8) :: key_a(8), key_b(8)
        integer(int32) :: h32_a, h32_b
        integer(int64) :: h64_a, h64_b
        integer(int64) :: h128_a(2), h128_b(2)

        key_a = [1_int8, 2_int8, 3_int8, 4_int8, &
                 5_int8, 6_int8, 7_int8, 8_int8]
        key_b = [1_int8, 2_int8, 3_int8, 4_int8, &
                 5_int8, 6_int8, 7_int8, 9_int8]  ! differs in last byte

        ! nmhash32 collision check
        h32_a = nmhash32(key_a, nm_seed)
        h32_b = nmhash32(key_b, nm_seed)
        call check(error, h32_a /= h32_b, &
            "NMHASH32 same hash for different inputs")
        if (allocated(error)) return

        ! nmhash32x collision check
        h32_a = nmhash32x(key_a, nm_seed)
        h32_b = nmhash32x(key_b, nm_seed)
        call check(error, h32_a /= h32_b, &
            "NMHASH32X same hash for different inputs")
        if (allocated(error)) return

        ! water_hash collision check
        h32_a = water_hash(key_a, water_seed)
        h32_b = water_hash(key_b, water_seed)
        call check(error, h32_a /= h32_b, &
            "WATER_HASH same hash for different inputs")
        if (allocated(error)) return

        ! pengy_hash collision check
        h64_a = pengy_hash(key_a, pengy_seed)
        h64_b = pengy_hash(key_b, pengy_seed)
        call check(error, h64_a /= h64_b, &
            "PENGY_HASH same hash for different inputs")
        if (allocated(error)) return

        ! spooky_hash collision check
        h128_a = spooky_hash(key_a, spooky_seed)
        h128_b = spooky_hash(key_b, spooky_seed)
        call check(error, any(h128_a /= h128_b), &
            "SPOOKY_HASH same hash for different inputs")
        if (allocated(error)) return

    end subroutine test_hash_distribution


    subroutine generate_key_array()
    
        integer        :: i, lun
        integer(int8)  :: key_array(size_key_array)
        integer(int32) :: dummy(size_key_array/4)
        real(dp)   :: rand(size_key_array/4)
    
        ! Create key array
        call random_number( rand )
        do i=1, size_key_array/4
            dummy(i) = floor( rand(i) * 2_int64**32 - 2_int64**31, kind=int32 )
        end do
        key_array = transfer( dummy, 0_int8, size_key_array )
    
        open(newunit=lun, file="key_array.bin", form="unformatted", &
            access="stream", status="replace", action="write")
        write(lun) key_array
        close(lun)
    
    end subroutine generate_key_array


    subroutine read_array_int8(filename, res)
        character(*), intent(in) :: filename
        integer(int8), intent(out) :: res(:)

        integer :: lun

        open(newunit=lun, file=filename, form="unformatted", &
            access="stream", status="old", action="read", err = 9908)
        read(lun) res
        close(lun)

        return

9908    res =  0

    end subroutine read_array_int8

    subroutine read_array_int32(filename, res)
        character(*), intent(in) :: filename
        integer(int32), intent(out) :: res(:)

        integer :: lun

        open(newunit=lun, file=filename, form="unformatted", &
            access="stream", status="old", action="read", err = 9908)
        read(lun) res
        close(lun)

        return

9908    res =  0

    end subroutine read_array_int32

    subroutine read_array_int64(filename, res)
        character(*), intent(in) :: filename
        integer(int64), intent(out) :: res(:)

        integer :: lun

        open(newunit=lun, file=filename, form="unformatted", &
            access="stream", status="old", action="read", err = 9908)
        read(lun) res
        close(lun)

        return

9908    res =  0

    end subroutine read_array_int64

    subroutine read_2darray_int64(filename, res)
        character(*), intent(in) :: filename
        integer(int64), intent(out) :: res(:,:)

        integer :: lun

        open(newunit=lun, file=filename, form="unformatted", &
            access="stream", status="old", action="read", err = 9908)
        read(lun) res
        close(lun)

        return

9908    res =  0

    end subroutine read_2darray_int64

end module

module modchash
 use, intrinsic :: ISO_C_Binding
 implicit none
 private
 public :: generate_all_c_hash

 interface
  function generate_all_c_hash() result(error) bind(C,name = "generate_all_c_hash")
    import C_int
    integer(C_int) :: error
  end function
 end interface

end module

program tester
    use, intrinsic :: iso_fortran_env, only : error_unit
    use, intrinsic :: ISO_C_Binding, only : C_int
    use testdrive, only : run_testsuite, new_testsuite, testsuite_type
    use test_hash_functions, only : collect_hash_functions, generate_key_array
    use modchash, only: generate_all_c_hash
    implicit none
    integer(C_int) :: error
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'


    call generate_key_array()

    error = generate_all_c_hash()

    stat = 0

    testsuites = [ &
        new_testsuite("hash_functions", collect_hash_functions) &
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
