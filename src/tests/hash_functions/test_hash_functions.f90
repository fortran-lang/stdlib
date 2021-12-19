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
            ]

    end subroutine collect_hash_functions

    subroutine test_little_endian(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
 
        ! Test for endianness

        call check(error, little_endian, "The processor is not Little-Endian")
        if (allocated(error)) return

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
