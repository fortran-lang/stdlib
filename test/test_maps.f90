module test_stdlib_chaining_maps
!! Test various aspects of the runtime system.
!! Running this program may require increasing the stack size to above 48 MBytes
!! or decreasing rand_power to 20 or less
    use testdrive, only : new_unittest, unittest_type, error_type, check
    use :: stdlib_kinds, only : dp, int8, int32
    use stdlib_hashmaps, only : chaining_hashmap_type, int_depth, int_index
    use stdlib_hashmap_wrappers

    implicit none
    private

    type dummy_type
        integer(int8), allocatable :: value(:)
    end type dummy_type

    integer(int32), parameter :: huge32 = huge(0_int32)
    real(dp), parameter       :: hugep1 = real(huge32, dp) + 1.0_dp
    integer, parameter        :: rand_power = 18
    integer, parameter        :: rand_size = 2**rand_power
    integer, parameter        :: test_size = rand_size*4
    integer, parameter        :: test_16 = 2**4
    integer, parameter        :: test_256 = 2**8

    public :: collect_stdlib_chaining_maps

contains

    !> Collect all exported unit tests
    subroutine collect_stdlib_chaining_maps(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("chaining-maps-fnv_1_hasher-16-byte-words", test_fnv_1_hasher_16_byte_words) &
                , new_unittest("chaining-maps-fnv_1_hasher-16-byte-words", test_fnv_1_hasher_16_byte_words) &
                , new_unittest("chaining-maps-fnv_1_hasher-256-byte-words", test_fnv_1_hasher_256_byte_words) &
                , new_unittest("chaining-maps-fnv_1a_hasher-16-byte-words", test_fnv_1a_hasher_16_byte_words) &
                , new_unittest("chaining-maps-fnv_1a_hasher-256-byte-words", test_fnv_1a_hasher_256_byte_words) &
                , new_unittest("chaining-maps-seeded_nmhash32_hasher-16-byte-words", test_seeded_nmhash32_hasher_16_byte_words) &
                , new_unittest("chaining-maps-seeded_nmhash32_hasher-256-byte-words", test_seeded_nmhash32_hasher_256_byte_words) &
                , new_unittest("chaining-maps-seeded_nmhash32x_hasher-16-byte-words", test_seeded_nmhash32x_hasher_16_byte_words) &
                , new_unittest("chaining-maps-seeded_nmhash32x_hasher-256-byte-words", test_seeded_nmhash32x_hasher_256_byte_words)&
                    & &
                , new_unittest("chaining-maps-seeded_water_hasher-16-byte-words", test_seeded_water_hasher_16_byte_words) &
                , new_unittest("chaining-maps-seeded_water_hasher-256-byte-words", test_seeded_water_hasher_256_byte_words) &
            , new_unittest("chaining-maps-removal-spec", test_removal_spec) &
            ]

    end subroutine collect_stdlib_chaining_maps

        subroutine test_fnv_1_hasher_16_byte_words(error)
            !> Error handling
            type(error_type), allocatable, intent(out) :: error

            type(chaining_hashmap_type)   :: map
            integer(int8)             :: test_8_bits(test_size)

            call generate_vector(test_8_bits)

            call map % init( fnv_1_hasher, slots_bits=10 )

            call test_input_random_data(error, map, test_8_bits, test_16)
            if (allocated(error)) return

            call test_inquire_data(error, map, test_8_bits, test_16)
            if (allocated(error)) return

            call test_get_data(error, map, test_8_bits, test_16)
            if (allocated(error)) return

            call test_removal(error, map, test_8_bits, test_16)
            if (allocated(error)) return

        end subroutine
        subroutine test_fnv_1_hasher_256_byte_words(error)
            !> Error handling
            type(error_type), allocatable, intent(out) :: error

            type(chaining_hashmap_type)   :: map
            integer(int8)             :: test_8_bits(test_size)

            call generate_vector(test_8_bits)

            call map % init( fnv_1_hasher, slots_bits=10 )

            call test_input_random_data(error, map, test_8_bits, test_256)
            if (allocated(error)) return

            call test_inquire_data(error, map, test_8_bits, test_256)
            if (allocated(error)) return

            call test_get_data(error, map, test_8_bits, test_256)
            if (allocated(error)) return

            call test_removal(error, map, test_8_bits, test_256)
            if (allocated(error)) return

        end subroutine
        subroutine test_fnv_1a_hasher_16_byte_words(error)
            !> Error handling
            type(error_type), allocatable, intent(out) :: error

            type(chaining_hashmap_type)   :: map
            integer(int8)             :: test_8_bits(test_size)

            call generate_vector(test_8_bits)

            call map % init( fnv_1a_hasher, slots_bits=10 )

            call test_input_random_data(error, map, test_8_bits, test_16)
            if (allocated(error)) return

            call test_inquire_data(error, map, test_8_bits, test_16)
            if (allocated(error)) return

            call test_get_data(error, map, test_8_bits, test_16)
            if (allocated(error)) return

            call test_removal(error, map, test_8_bits, test_16)
            if (allocated(error)) return

        end subroutine
        subroutine test_fnv_1a_hasher_256_byte_words(error)
            !> Error handling
            type(error_type), allocatable, intent(out) :: error

            type(chaining_hashmap_type)   :: map
            integer(int8)             :: test_8_bits(test_size)

            call generate_vector(test_8_bits)

            call map % init( fnv_1a_hasher, slots_bits=10 )

            call test_input_random_data(error, map, test_8_bits, test_256)
            if (allocated(error)) return

            call test_inquire_data(error, map, test_8_bits, test_256)
            if (allocated(error)) return

            call test_get_data(error, map, test_8_bits, test_256)
            if (allocated(error)) return

            call test_removal(error, map, test_8_bits, test_256)
            if (allocated(error)) return

        end subroutine
        subroutine test_seeded_nmhash32_hasher_16_byte_words(error)
            !> Error handling
            type(error_type), allocatable, intent(out) :: error

            type(chaining_hashmap_type)   :: map
            integer(int8)             :: test_8_bits(test_size)

            call generate_vector(test_8_bits)

            call map % init( seeded_nmhash32_hasher, slots_bits=10 )

            call test_input_random_data(error, map, test_8_bits, test_16)
            if (allocated(error)) return

            call test_inquire_data(error, map, test_8_bits, test_16)
            if (allocated(error)) return

            call test_get_data(error, map, test_8_bits, test_16)
            if (allocated(error)) return

            call test_removal(error, map, test_8_bits, test_16)
            if (allocated(error)) return

        end subroutine
        subroutine test_seeded_nmhash32_hasher_256_byte_words(error)
            !> Error handling
            type(error_type), allocatable, intent(out) :: error

            type(chaining_hashmap_type)   :: map
            integer(int8)             :: test_8_bits(test_size)

            call generate_vector(test_8_bits)

            call map % init( seeded_nmhash32_hasher, slots_bits=10 )

            call test_input_random_data(error, map, test_8_bits, test_256)
            if (allocated(error)) return

            call test_inquire_data(error, map, test_8_bits, test_256)
            if (allocated(error)) return

            call test_get_data(error, map, test_8_bits, test_256)
            if (allocated(error)) return

            call test_removal(error, map, test_8_bits, test_256)
            if (allocated(error)) return

        end subroutine
        subroutine test_seeded_nmhash32x_hasher_16_byte_words(error)
            !> Error handling
            type(error_type), allocatable, intent(out) :: error

            type(chaining_hashmap_type)   :: map
            integer(int8)             :: test_8_bits(test_size)

            call generate_vector(test_8_bits)

            call map % init( seeded_nmhash32x_hasher, slots_bits=10 )

            call test_input_random_data(error, map, test_8_bits, test_16)
            if (allocated(error)) return

            call test_inquire_data(error, map, test_8_bits, test_16)
            if (allocated(error)) return

            call test_get_data(error, map, test_8_bits, test_16)
            if (allocated(error)) return

            call test_removal(error, map, test_8_bits, test_16)
            if (allocated(error)) return

        end subroutine
        subroutine test_seeded_nmhash32x_hasher_256_byte_words(error)
            !> Error handling
            type(error_type), allocatable, intent(out) :: error

            type(chaining_hashmap_type)   :: map
            integer(int8)             :: test_8_bits(test_size)

            call generate_vector(test_8_bits)

            call map % init( seeded_nmhash32x_hasher, slots_bits=10 )

            call test_input_random_data(error, map, test_8_bits, test_256)
            if (allocated(error)) return

            call test_inquire_data(error, map, test_8_bits, test_256)
            if (allocated(error)) return

            call test_get_data(error, map, test_8_bits, test_256)
            if (allocated(error)) return

            call test_removal(error, map, test_8_bits, test_256)
            if (allocated(error)) return

        end subroutine
        subroutine test_seeded_water_hasher_16_byte_words(error)
            !> Error handling
            type(error_type), allocatable, intent(out) :: error

            type(chaining_hashmap_type)   :: map
            integer(int8)             :: test_8_bits(test_size)

            call generate_vector(test_8_bits)

            call map % init( seeded_water_hasher, slots_bits=10 )

            call test_input_random_data(error, map, test_8_bits, test_16)
            if (allocated(error)) return

            call test_inquire_data(error, map, test_8_bits, test_16)
            if (allocated(error)) return

            call test_get_data(error, map, test_8_bits, test_16)
            if (allocated(error)) return

            call test_removal(error, map, test_8_bits, test_16)
            if (allocated(error)) return

        end subroutine
        subroutine test_seeded_water_hasher_256_byte_words(error)
            !> Error handling
            type(error_type), allocatable, intent(out) :: error

            type(chaining_hashmap_type)   :: map
            integer(int8)             :: test_8_bits(test_size)

            call generate_vector(test_8_bits)

            call map % init( seeded_water_hasher, slots_bits=10 )

            call test_input_random_data(error, map, test_8_bits, test_256)
            if (allocated(error)) return

            call test_inquire_data(error, map, test_8_bits, test_256)
            if (allocated(error)) return

            call test_get_data(error, map, test_8_bits, test_256)
            if (allocated(error)) return

            call test_removal(error, map, test_8_bits, test_256)
            if (allocated(error)) return

        end subroutine


    subroutine generate_vector(test_8_bits)
        integer(int8), intent(out) :: test_8_bits(test_size)

        integer                   :: index
        real(dp)                  :: rand2(2)
        integer(int32)            :: rand_object(rand_size)

        do index=1, rand_size
            call random_number(rand2)
            if (rand2(1) < 0.5_dp) then
                rand_object(index) = ceiling(-rand2(2)*hugep1, int32) - 1
            else
                rand_object(index) = floor(rand2(2)*hugep1, int32)
            end if
        end do

        test_8_bits(:) = transfer( rand_object, 0_int8, test_size )

    end subroutine

    subroutine test_input_random_data(error, map, test_8_bits, test_block)
        type(error_type), allocatable, intent(out) :: error
        type(chaining_hashmap_type), intent(inout) :: map
        integer(int8), intent(in) :: test_8_bits(test_size)
        integer(int_index), intent(in) :: test_block
        class(*), allocatable :: dummy
        type(dummy_type) :: dummy_val
        integer :: index2
        type(key_type) :: key
        type(other_type) :: other
        logical :: conflict

        do index2=1, size(test_8_bits), test_block
            call set( key, test_8_bits( index2:index2+test_block-1 ) )
            if (allocated(dummy)) deallocate(dummy)
            dummy_val % value = test_8_bits( index2:index2+test_block-1 )
            allocate( dummy, source=dummy_val )
            call set ( other, dummy )
            call map % map_entry( key, other, conflict )
            call check(error, .not.conflict, "Unable to map entry because of a key conflict.")
            if (allocated(error)) return
        end do

    end subroutine

    subroutine test_inquire_data(error, map, test_8_bits, test_block)
        type(error_type), allocatable, intent(out) :: error
        type(chaining_hashmap_type), intent(inout)  :: map
        integer(int8), intent(in)               :: test_8_bits(test_size)
        integer(int_index), intent(in)          :: test_block
        integer :: index2
        logical :: present
        type(key_type) :: key

        do index2=1, size(test_8_bits), test_block
            call set( key, test_8_bits( index2:index2+test_block-1 ) )
            call map % key_test( key, present )
            call check(error, present, "KEY not found in map KEY_TEST.")
            if (allocated(error)) return
        end do

    end subroutine

    subroutine test_get_data(error, map, test_8_bits, test_block)
        type(error_type), allocatable, intent(out) :: error
        type(chaining_hashmap_type), intent(inout)  :: map
        integer(int8), intent(in)               :: test_8_bits(test_size)
        integer(int_index), intent(in)          :: test_block
        integer :: index2
        type(key_type) :: key
        type(other_type) :: other
        logical :: exists

        do index2=1, size(test_8_bits), test_block
            call set( key, test_8_bits( index2:index2+test_block-1 ) )
            call map % get_other_data( key, other, exists )
            call check(error, exists, "Unable to get data because key not found in map.")
        end do

    end subroutine

    subroutine test_removal(error, map, test_8_bits, test_block)
        type(error_type), allocatable, intent(out) :: error
        type(chaining_hashmap_type), intent(inout)  :: map
        integer(int8), intent(in)               :: test_8_bits(test_size)
        integer(int_index), intent(in)          :: test_block
        type(key_type) :: key
        integer(int_index) :: index2
        logical :: existed

        do index2=1, size(test_8_bits), test_block
            call set( key, test_8_bits( index2:index2+test_block-1 ) )
            call map % remove(key, existed)
            call check(error, existed,  "Key not found in entry removal.")
        end do

    end subroutine

    subroutine test_removal_spec(error)
        !! Test following code provided by @jannisteunissen
        !! https://github.com/fortran-lang/stdlib/issues/785
        type(error_type), allocatable, intent(out) :: error

        type(chaining_hashmap_type) :: map
        type(key_type) :: key
        integer, parameter :: n_max = 500
        integer :: n
        integer, allocatable :: key_counts(:)
        integer, allocatable :: seed(:)
        integer(int8) :: int32_int8(4)
        integer(int32) :: keys(n_max)
        real(dp) :: r_uniform(n_max)
        logical :: existed, present

        call random_seed(size = n)
        allocate(seed(n), source = 123456)
        call random_seed(put = seed)

        call random_number(r_uniform)
        keys = nint(r_uniform * n_max * 0.25_dp)

        call map%init(fnv_1_hasher, slots_bits=10)

        do n = 1, n_max
            call set(key, transfer(keys(n), int32_int8))
            call map%key_test(key, present)
            if (present) then
                call map%remove(key, existed)
                call check(error, existed, "chaining-removal-spec: Key not found in entry removal.")
                return
            else
                call map%map_entry(key)
            end if
        end do

        ! Count number of keys that occur an odd number of times
        allocate(key_counts(minval(keys):maxval(keys)), source = 0)
        do n = 1, n_max
            key_counts(keys(n)) = key_counts(keys(n)) + 1
        end do
        n = sum(iand(key_counts, 1))

        call check(error, map%entries(), n, & 
                   "chaining-removal-spec: Number of expected keys and entries are different.")
        return

    end subroutine

end module

module test_stdlib_open_maps
!! Test various aspects of the runtime system.
!! Running this program may require increasing the stack size to above 48 MBytes
!! or decreasing rand_power to 20 or less
    use testdrive, only : new_unittest, unittest_type, error_type, check
    use :: stdlib_kinds, only : dp, int8, int32
    use stdlib_hashmaps, only : open_hashmap_type, int_depth, int_index
    use stdlib_hashmap_wrappers

    implicit none
    private

    type dummy_type
        integer(int8), allocatable :: value(:)
    end type dummy_type

    integer(int32), parameter :: huge32 = huge(0_int32)
    real(dp), parameter       :: hugep1 = real(huge32, dp) + 1.0_dp
    integer, parameter        :: rand_power = 18
    integer, parameter        :: rand_size = 2**rand_power
    integer, parameter        :: test_size = rand_size*4
    integer, parameter        :: test_16 = 2**4
    integer, parameter        :: test_256 = 2**8

    public :: collect_stdlib_open_maps

contains

    !> Collect all exported unit tests
    subroutine collect_stdlib_open_maps(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("open-maps-fnv_1_hasher-16-byte-words", test_fnv_1_hasher_16_byte_words) &
                , new_unittest("open-maps-fnv_1_hasher-16-byte-words", test_fnv_1_hasher_16_byte_words) &
                , new_unittest("open-maps-fnv_1_hasher-256-byte-words", test_fnv_1_hasher_256_byte_words) &
                , new_unittest("open-maps-fnv_1a_hasher-16-byte-words", test_fnv_1a_hasher_16_byte_words) &
                , new_unittest("open-maps-fnv_1a_hasher-256-byte-words", test_fnv_1a_hasher_256_byte_words) &
                , new_unittest("open-maps-seeded_nmhash32_hasher-16-byte-words", test_seeded_nmhash32_hasher_16_byte_words) &
                , new_unittest("open-maps-seeded_nmhash32_hasher-256-byte-words", test_seeded_nmhash32_hasher_256_byte_words) &
                , new_unittest("open-maps-seeded_nmhash32x_hasher-16-byte-words", test_seeded_nmhash32x_hasher_16_byte_words) &
                , new_unittest("open-maps-seeded_nmhash32x_hasher-256-byte-words", test_seeded_nmhash32x_hasher_256_byte_words) &
                , new_unittest("open-maps-seeded_water_hasher-16-byte-words", test_seeded_water_hasher_16_byte_words) &
                , new_unittest("open-maps-seeded_water_hasher-256-byte-words", test_seeded_water_hasher_256_byte_words) &
            , new_unittest("open-maps-removal-spec", test_removal_spec) &
            ]

    end subroutine collect_stdlib_open_maps

        subroutine test_fnv_1_hasher_16_byte_words(error)
            !> Error handling
            type(error_type), allocatable, intent(out) :: error

            type(open_hashmap_type)   :: map
            integer(int8)             :: test_8_bits(test_size)

            call generate_vector(test_8_bits)

            call map % init( fnv_1_hasher, slots_bits=10 )

            call test_input_random_data(error, map, test_8_bits, test_16)
            if (allocated(error)) return

            call test_inquire_data(error, map, test_8_bits, test_16)
            if (allocated(error)) return

            call test_get_data(error, map, test_8_bits, test_16)
            if (allocated(error)) return

            call test_removal(error, map, test_8_bits, test_16)
            if (allocated(error)) return

        end subroutine
        subroutine test_fnv_1_hasher_256_byte_words(error)
            !> Error handling
            type(error_type), allocatable, intent(out) :: error

            type(open_hashmap_type)   :: map
            integer(int8)             :: test_8_bits(test_size)

            call generate_vector(test_8_bits)

            call map % init( fnv_1_hasher, slots_bits=10 )

            call test_input_random_data(error, map, test_8_bits, test_256)
            if (allocated(error)) return

            call test_inquire_data(error, map, test_8_bits, test_256)
            if (allocated(error)) return

            call test_get_data(error, map, test_8_bits, test_256)
            if (allocated(error)) return

            call test_removal(error, map, test_8_bits, test_256)
            if (allocated(error)) return

        end subroutine
        subroutine test_fnv_1a_hasher_16_byte_words(error)
            !> Error handling
            type(error_type), allocatable, intent(out) :: error

            type(open_hashmap_type)   :: map
            integer(int8)             :: test_8_bits(test_size)

            call generate_vector(test_8_bits)

            call map % init( fnv_1a_hasher, slots_bits=10 )

            call test_input_random_data(error, map, test_8_bits, test_16)
            if (allocated(error)) return

            call test_inquire_data(error, map, test_8_bits, test_16)
            if (allocated(error)) return

            call test_get_data(error, map, test_8_bits, test_16)
            if (allocated(error)) return

            call test_removal(error, map, test_8_bits, test_16)
            if (allocated(error)) return

        end subroutine
        subroutine test_fnv_1a_hasher_256_byte_words(error)
            !> Error handling
            type(error_type), allocatable, intent(out) :: error

            type(open_hashmap_type)   :: map
            integer(int8)             :: test_8_bits(test_size)

            call generate_vector(test_8_bits)

            call map % init( fnv_1a_hasher, slots_bits=10 )

            call test_input_random_data(error, map, test_8_bits, test_256)
            if (allocated(error)) return

            call test_inquire_data(error, map, test_8_bits, test_256)
            if (allocated(error)) return

            call test_get_data(error, map, test_8_bits, test_256)
            if (allocated(error)) return

            call test_removal(error, map, test_8_bits, test_256)
            if (allocated(error)) return

        end subroutine
        subroutine test_seeded_nmhash32_hasher_16_byte_words(error)
            !> Error handling
            type(error_type), allocatable, intent(out) :: error

            type(open_hashmap_type)   :: map
            integer(int8)             :: test_8_bits(test_size)

            call generate_vector(test_8_bits)

            call map % init( seeded_nmhash32_hasher, slots_bits=10 )

            call test_input_random_data(error, map, test_8_bits, test_16)
            if (allocated(error)) return

            call test_inquire_data(error, map, test_8_bits, test_16)
            if (allocated(error)) return

            call test_get_data(error, map, test_8_bits, test_16)
            if (allocated(error)) return

            call test_removal(error, map, test_8_bits, test_16)
            if (allocated(error)) return

        end subroutine
        subroutine test_seeded_nmhash32_hasher_256_byte_words(error)
            !> Error handling
            type(error_type), allocatable, intent(out) :: error

            type(open_hashmap_type)   :: map
            integer(int8)             :: test_8_bits(test_size)

            call generate_vector(test_8_bits)

            call map % init( seeded_nmhash32_hasher, slots_bits=10 )

            call test_input_random_data(error, map, test_8_bits, test_256)
            if (allocated(error)) return

            call test_inquire_data(error, map, test_8_bits, test_256)
            if (allocated(error)) return

            call test_get_data(error, map, test_8_bits, test_256)
            if (allocated(error)) return

            call test_removal(error, map, test_8_bits, test_256)
            if (allocated(error)) return

        end subroutine
        subroutine test_seeded_nmhash32x_hasher_16_byte_words(error)
            !> Error handling
            type(error_type), allocatable, intent(out) :: error

            type(open_hashmap_type)   :: map
            integer(int8)             :: test_8_bits(test_size)

            call generate_vector(test_8_bits)

            call map % init( seeded_nmhash32x_hasher, slots_bits=10 )

            call test_input_random_data(error, map, test_8_bits, test_16)
            if (allocated(error)) return

            call test_inquire_data(error, map, test_8_bits, test_16)
            if (allocated(error)) return

            call test_get_data(error, map, test_8_bits, test_16)
            if (allocated(error)) return

            call test_removal(error, map, test_8_bits, test_16)
            if (allocated(error)) return

        end subroutine
        subroutine test_seeded_nmhash32x_hasher_256_byte_words(error)
            !> Error handling
            type(error_type), allocatable, intent(out) :: error

            type(open_hashmap_type)   :: map
            integer(int8)             :: test_8_bits(test_size)

            call generate_vector(test_8_bits)

            call map % init( seeded_nmhash32x_hasher, slots_bits=10 )

            call test_input_random_data(error, map, test_8_bits, test_256)
            if (allocated(error)) return

            call test_inquire_data(error, map, test_8_bits, test_256)
            if (allocated(error)) return

            call test_get_data(error, map, test_8_bits, test_256)
            if (allocated(error)) return

            call test_removal(error, map, test_8_bits, test_256)
            if (allocated(error)) return

        end subroutine
        subroutine test_seeded_water_hasher_16_byte_words(error)
            !> Error handling
            type(error_type), allocatable, intent(out) :: error

            type(open_hashmap_type)   :: map
            integer(int8)             :: test_8_bits(test_size)

            call generate_vector(test_8_bits)

            call map % init( seeded_water_hasher, slots_bits=10 )

            call test_input_random_data(error, map, test_8_bits, test_16)
            if (allocated(error)) return

            call test_inquire_data(error, map, test_8_bits, test_16)
            if (allocated(error)) return

            call test_get_data(error, map, test_8_bits, test_16)
            if (allocated(error)) return

            call test_removal(error, map, test_8_bits, test_16)
            if (allocated(error)) return

        end subroutine
        subroutine test_seeded_water_hasher_256_byte_words(error)
            !> Error handling
            type(error_type), allocatable, intent(out) :: error

            type(open_hashmap_type)   :: map
            integer(int8)             :: test_8_bits(test_size)

            call generate_vector(test_8_bits)

            call map % init( seeded_water_hasher, slots_bits=10 )

            call test_input_random_data(error, map, test_8_bits, test_256)
            if (allocated(error)) return

            call test_inquire_data(error, map, test_8_bits, test_256)
            if (allocated(error)) return

            call test_get_data(error, map, test_8_bits, test_256)
            if (allocated(error)) return

            call test_removal(error, map, test_8_bits, test_256)
            if (allocated(error)) return

        end subroutine


    subroutine generate_vector(test_8_bits)
        integer(int8), intent(out) :: test_8_bits(test_size)

        integer                   :: index
        real(dp)                  :: rand2(2)
        integer(int32)            :: rand_object(rand_size)

        do index=1, rand_size
            call random_number(rand2)
            if (rand2(1) < 0.5_dp) then
                rand_object(index) = ceiling(-rand2(2)*hugep1, int32) - 1
            else
                rand_object(index) = floor(rand2(2)*hugep1, int32)
            end if
        end do

        test_8_bits(:) = transfer( rand_object, 0_int8, test_size )

    end subroutine

    subroutine test_input_random_data(error, map, test_8_bits, test_block)
        type(error_type), allocatable, intent(out) :: error
        type(open_hashmap_type), intent(inout) :: map
        integer(int8), intent(in) :: test_8_bits(test_size)
        integer(int_index), intent(in) :: test_block
        class(*), allocatable :: dummy
        type(dummy_type) :: dummy_val
        integer :: index2
        type(key_type) :: key
        type(other_type) :: other
        logical :: conflict

        do index2=1, size(test_8_bits), test_block
            call set( key, test_8_bits( index2:index2+test_block-1 ) )
            if (allocated(dummy)) deallocate(dummy)
            dummy_val % value = test_8_bits( index2:index2+test_block-1 )
            allocate( dummy, source=dummy_val )
            call set ( other, dummy )
            call map % map_entry( key, other, conflict )
            call check(error, .not.conflict, "Unable to map entry because of a key conflict.")
            if (allocated(error)) return
        end do

    end subroutine

    subroutine test_inquire_data(error, map, test_8_bits, test_block)
        type(error_type), allocatable, intent(out) :: error
        type(open_hashmap_type), intent(inout)  :: map
        integer(int8), intent(in)               :: test_8_bits(test_size)
        integer(int_index), intent(in)          :: test_block
        integer :: index2
        logical :: present
        type(key_type) :: key

        do index2=1, size(test_8_bits), test_block
            call set( key, test_8_bits( index2:index2+test_block-1 ) )
            call map % key_test( key, present )
            call check(error, present, "KEY not found in map KEY_TEST.")
            if (allocated(error)) return
        end do

    end subroutine

    subroutine test_get_data(error, map, test_8_bits, test_block)
        type(error_type), allocatable, intent(out) :: error
        type(open_hashmap_type), intent(inout)  :: map
        integer(int8), intent(in)               :: test_8_bits(test_size)
        integer(int_index), intent(in)          :: test_block
        integer :: index2
        type(key_type) :: key
        type(other_type) :: other
        logical :: exists

        do index2=1, size(test_8_bits), test_block
            call set( key, test_8_bits( index2:index2+test_block-1 ) )
            call map % get_other_data( key, other, exists )
            call check(error, exists, "Unable to get data because key not found in map.")
        end do

    end subroutine

    subroutine test_removal(error, map, test_8_bits, test_block)
        type(error_type), allocatable, intent(out) :: error
        type(open_hashmap_type), intent(inout)  :: map
        integer(int8), intent(in)               :: test_8_bits(test_size)
        integer(int_index), intent(in)          :: test_block
        type(key_type) :: key
        integer(int_index) :: index2
        logical :: existed

        do index2=1, size(test_8_bits), test_block
            call set( key, test_8_bits( index2:index2+test_block-1 ) )
            call map % remove(key, existed)
            call check(error, existed,  "Key not found in entry removal.")
        end do

    end subroutine

    subroutine test_removal_spec(error)
        !! Test following code provided by @jannisteunissen
        !! https://github.com/fortran-lang/stdlib/issues/785
        type(error_type), allocatable, intent(out) :: error

        type(open_hashmap_type) :: map
        type(key_type) :: key
        integer, parameter :: n_max = 500
        integer :: n
        integer, allocatable :: key_counts(:)
        integer, allocatable :: seed(:)
        integer(int8) :: int32_int8(4)
        integer(int32) :: keys(n_max)
        real(dp) :: r_uniform(n_max)
        logical :: existed, present

        call random_seed(size = n)
        allocate(seed(n), source = 123456)
        call random_seed(put = seed)

        call random_number(r_uniform)
        keys = nint(r_uniform * n_max * 0.25_dp)

        call map%init(fnv_1_hasher, slots_bits=10)

        do n = 1, n_max
            call set(key, transfer(keys(n), int32_int8))
            call map%key_test(key, present)
            if (present) then
                call map%remove(key, existed)
                call check(error, existed, "open-removal-spec: Key not found in entry removal.")
                return
            else
                call map%map_entry(key)
            end if
        end do

        ! Count number of keys that occur an odd number of times
        allocate(key_counts(minval(keys):maxval(keys)), source = 0)
        do n = 1, n_max
            key_counts(keys(n)) = key_counts(keys(n)) + 1
        end do
        n = sum(iand(key_counts, 1))

        call check(error, map%entries(), n, & 
                   "open-removal-spec: Number of expected keys and entries are different.")
        return

    end subroutine

end module


program tester
    use, intrinsic :: iso_fortran_env, only : error_unit
    use testdrive, only : run_testsuite, new_testsuite, testsuite_type
    use test_stdlib_open_maps, only : collect_stdlib_open_maps
    use test_stdlib_chaining_maps, only : collect_stdlib_chaining_maps
    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
        new_testsuite("stdlib-open-maps", collect_stdlib_open_maps) &
        , new_testsuite("stdlib-chaining-maps", collect_stdlib_chaining_maps) &
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
