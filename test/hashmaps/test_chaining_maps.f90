program test_chaining_maps
!! Test various aspects of the runtime system.
!! Running this program may require increasing the stack size to above 48 MBytes
!! or decreasing rand_power to 20 or less

    use stdlib_kinds, only: &
        dp,                 &
        int8,               &
        int32
 
    use stdlib_hashmaps, only : chaining_hashmap_type, int_depth, int_index
    use stdlib_hashmap_wrappers

    implicit none

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

    integer                     :: index
    integer                     :: lun
    type(chaining_hashmap_type) :: map
    real(dp)                    :: rand2(2)
    integer(int32)              :: rand_object(rand_size)
    integer(int8)               :: test_8_bits(test_size)

    open( newunit=lun, file="test_chaining_maps.txt", access="sequential", &
        action="write", form="formatted", position="rewind" )
    write(lun, '("| ", a17, " | ", a12, " | ", a15, " | ", a10, " |")') &
        'Algorithm', 'Process', 'Data Set', 'Time (s)'

    do index=1, rand_size
        call random_number(rand2)
        if (rand2(1) < 0.5_dp) then
            rand_object(index) = ceiling(-rand2(2)*hugep1, int32) - 1
        else
            rand_object(index) = floor(rand2(2)*hugep1, int32)
        end if
    end do

    test_8_bits(:) = transfer( rand_object, 0_int8, test_size )

    ! Test implicit initalization by skipping init call for first test.
    call input_random_data( map, test_16, 'FNV-1', "16 byte words" )
    call test_inquire_data( map, test_16, 'FNV-1', "16 byte words" )
    call test_get_all_keys( map, test_16, 'FNV-1', '16 byte words' )
    call test_get_data( map, test_16, 'FNV-1', '16 byte words' )
    call report_rehash_times( map, fnv_1_hasher, 'FNV-1', '16 byte words' )
    call report_hash_statistics( map, 'FNV-1', '16 byte words' )
    call report_removal_times( map, test_16, 'FNV-1', '16 byte words' )

    call map % init()   ! Test default options
    call input_random_data( map, test_256, 'FNV-1', "256 byte words" )
    call test_inquire_data( map, test_256, 'FNV-1', "256 byte words" )
    call test_get_data( map, test_256, 'FNV-1', '256 byte words' )
    call test_get_all_keys( map, test_256, 'FNV-1', '256 byte words' )
    call report_rehash_times( map, fnv_1_hasher, 'FNV-1', '256 byte words' )
    call report_hash_statistics( map, 'FNV-1', '256 byte words' )
    call report_removal_times( map, test_256, 'FNV-1', '256 byte words' )

    call map % init( fnv_1a_hasher, slots_bits=10 )
    call input_random_data( map, test_16, 'FNV-1A', "16 byte words" )
    call test_inquire_data( map, test_16, 'FNV-1A', "16 byte words" )
    call test_get_data( map, test_16, 'FNV-1A', '16 byte words' )
    call test_get_all_keys( map, test_16, 'FNV-1A', '16 byte words' )
    call report_rehash_times( map, fnv_1a_hasher, 'FNV-1', '16 byte words' )
    call report_hash_statistics( map, 'FNV-1A', '16 byte words' )
    call report_removal_times( map, test_16, 'FNV-1a', '16 byte words' )

    call map % init( fnv_1a_hasher, slots_bits=10 )
    call input_random_data( map, test_256, 'FNV-1A', "256 byte words" )
    call test_inquire_data( map, test_256, 'FNV-1A', "256 byte words" )
    call test_get_data( map, test_256, 'FNV-1A', '256 byte words' )
    call test_get_all_keys( map, test_256, 'FNV-1A', '256 byte words' )
    call report_rehash_times( map, fnv_1_hasher, 'FNV-1A', '256 byte words' )
    call report_hash_statistics( map, 'FNV-1A', '256 byte words' )
    call report_removal_times( map, test_256, 'FNV-1A', '256 byte words' )

    call map % init( seeded_nmhash32_hasher, slots_bits=10 )
    call input_random_data( map, test_16, 'Seeded_Nmhash32', "16 byte words" )
    call test_inquire_data( map, test_16, 'Seeded_Nmhash32', "16 byte words" )
    call test_get_data( map, test_16, 'Seeded_Nmhash32', '16 byte words' )
    call test_get_all_keys( map, test_16, 'Seeded_Nmhash32', '16 byte words' )
    call report_rehash_times( map, seeded_nmhash32_hasher, 'Seeded_Nmhash32', &
        '16 byte words' )
    call report_hash_statistics( map, 'Seeded_Nmhash32', '16 byte words' )
    call report_removal_times( map, test_16, 'Seeded_Nmhash32', &
        '16 byte words' )

    call map % init( seeded_nmhash32_hasher, slots_bits=10 )
    call input_random_data( map, test_256, 'Seeded_Nmhash32', "256 byte words" )
    call test_inquire_data( map, test_256, 'Seeded_Nmhash32', "256 byte words" )
    call test_get_data( map, test_256, 'Seeded_Nmhash32', '256 byte words' )
    call test_get_all_keys( map, test_256, 'Seeded_Nmhash32', '256 byte words' )
    call report_rehash_times( map, seeded_nmhash32_hasher, 'Seeded_Nmhash32', &
        '256 byte words' )
    call report_hash_statistics( map, 'Seeded_Nmhash32', '256 byte words' )
    call report_removal_times( map, test_256, 'Seeded_Nmhash32', &
        '256 byte words' )

    call map % init( seeded_nmhash32x_hasher, slots_bits=10 )
    call input_random_data( map, test_16, 'Seeded_Nmhash32x', "16 byte words" )
    call test_inquire_data( map, test_16, 'Seeded_Nmhash32x', "16 byte words" )
    call test_get_data( map, test_16, 'Seeded_Nmhash32x', '16 byte words' )
    call test_get_all_keys( map, test_16, 'Seeded_Nmhash32x', '16 byte words' )
    call report_rehash_times( map, seeded_nmhash32x_hasher, &
        'Seeded_Nmhash32x', '16 byte words' )
    call report_hash_statistics( map, 'Seeded_Nmhash32x', '16 byte words' )
    call report_removal_times( map, test_16, 'Seeded_Nmhash32x', &
        '16 byte words' )

    call map % init( seeded_nmhash32x_hasher, slots_bits=10 )
    call input_random_data( map, test_256, 'Seeded_Nmhash32x', &
        "256 byte words" )
    call test_inquire_data( map, test_256, 'Seeded_Nmhash32x', &
        "256 byte words" )
    call test_get_data( map, test_256, 'Seeded_Nmhash32x', '256 byte words' )
    call test_get_all_keys( map, test_256, 'Seeded_Nmhash32x', '256 byte words' )
    call report_rehash_times( map, seeded_nmhash32x_hasher, &
        'Seeded_Nmhash32x', '256 byte words' )
    call report_hash_statistics( map, 'Seeded_Nmhash32x', '256 byte words' )
    call report_removal_times( map, test_256, 'Seeded_Nmhash32x', &
        '256 byte words' )

    call map % init( seeded_water_hasher, slots_bits=10 )
    call input_random_data( map, test_16, 'Seeded_Water', "16 byte words" )
    call test_inquire_data( map, test_16, 'Seeded_Water', "16 byte words" )
    call test_get_data( map, test_16, 'Seeded_Water', '16 byte words' )
    call test_get_all_keys( map, test_16, 'Seeded_Water', '16 byte words' )
    call report_rehash_times( map, seeded_water_hasher, &
        'Seeded_Water', '16 byte words' )
    call report_hash_statistics( map, 'Seeded_Water', '16 byte words' )
    call report_removal_times( map, test_16, 'Seeded_Water', &
        '16 byte words' )

    call map % init( seeded_water_hasher, slots_bits=10 )
    call input_random_data( map, test_256, 'Seeded_Water', &
        "256 byte words" )
    call test_inquire_data( map, test_256, 'Seeded_Water', &
        "256 byte words" )
    call test_get_data( map, test_256, 'Seeded_Water', '256 byte words' )
    call test_get_all_keys( map, test_256, 'Seeded_Water', '256 byte words' )
    call report_rehash_times( map, seeded_water_hasher, &
        'Seeded_Water', '256 byte words' )
    call report_hash_statistics( map, 'Seeded_Water', '256 byte words' )
    call report_removal_times( map, test_256, 'Seeded_Water', &
        '256 byte words' )

contains

    subroutine input_random_data( map, test_block, hash_name, size_name )
        type(chaining_hashmap_type), intent(inout) :: map
        integer(int_index), intent(in) :: test_block
        character(*), intent(in) :: hash_name
        character(*), intent(in) :: size_name
        type(dummy_type) :: dummy_val
        integer :: index2
        type(key_type) :: key
        real :: t1, t2, tdiff
        logical :: conflict

        call cpu_time(t1)
        do index2=1, size(test_8_bits), test_block
            call set( key, test_8_bits( index2:index2+test_block-1 ) )
            dummy_val % value = test_8_bits( index2:index2+test_block-1 )
            call map % map_entry( key, dummy_val, conflict )
            if (conflict) &
                error stop "Unable to map entry because of a key conflict."
        end do
        call cpu_time(t2)
        tdiff = t2-t1
        write(lun, '("|", a18, " | ", a12, " | ", a15, " | ", f10.5, " |")') &
            trim(hash_name), 'Enter data', size_name, tdiff

    end subroutine input_random_data


    subroutine test_inquire_data( map, test_block, hash_name, size_name )
        type(chaining_hashmap_type), intent(inout) :: map
        integer(int_index), intent(in)          :: test_block
        character(*), intent(in)                :: hash_name, size_name
        integer :: index2
        logical :: present
        type(key_type) :: key
        real :: t1, t2, tdiff

        call cpu_time(t1)
        do index2=1, size(test_8_bits), test_block
            call set( key, test_8_bits( index2:index2+test_block-1 ) )
            call map % key_test( key, present )
            if (.not. present) &
                error stop "KEY not found in map KEY_TEST."
        end do
        call cpu_time(t2)
        tdiff = t2-t1
        write(lun, '("|", a18, " | ", a12, " | ", a15, " | ", f10.5, " |")') &
            trim(hash_name), 'Inquire data', size_name, tdiff

    end subroutine test_inquire_data


    subroutine test_get_data( map, test_block, hash_name, size_name )
        type(chaining_hashmap_type), intent(inout) :: map
        integer(int_index), intent(in)          :: test_block
        character(*), intent(in)                :: hash_name, size_name
        integer :: index2
        type(key_type) :: key
        class(*), allocatable :: data
        logical :: exists
        real :: t1, t2, tdiff

        call cpu_time(t1)
        do index2=1, size(test_8_bits), test_block
            call set( key, test_8_bits( index2:index2+test_block-1 ) )
            call map % get_other_data( key, data, exists )
            if (.not. exists) &
                error stop "Unable to get data because key not found in map."
        end do
        call cpu_time(t2)
        tdiff = t2-t1
        write(lun, '("|", a18, " | ", a12, " | ", a15, " | ", f10.5, " |")') &
            trim(hash_name), 'Get data', size_name, tdiff

    end subroutine test_get_data


    subroutine test_get_all_keys( map, test_block, hash_name, size_name )
        type(chaining_hashmap_type), intent(inout) :: map
        integer(int_index), intent(in)          :: test_block
        character(*), intent(in)                :: hash_name, size_name
        integer :: index2, key_idx
        type(key_type) :: key
        type(key_type), allocatable :: all_keys(:)
        real :: t1, t2, tdiff

        call cpu_time(t1)
        call map % get_all_keys(all_keys)
        call cpu_time(t2)
        tdiff = t2-t1

        if (size( all_keys ) /= size( test_8_bits )/test_block) &
            error stop "Number of keys is different from that of keys in a map."

        do index2=1, size(test_8_bits), test_block
            call set( key, test_8_bits( index2:index2+test_block-1 ) )

            key_idx = ( index2/test_block ) + 1
            if (.not. ( all_keys(key_idx) == key )) &
                error stop "Invalid value of a key."
        end do

        write(lun, '("|", a18, " | ", a12, " | ", a15, " | ", f10.5, " |")') &
            trim(hash_name), 'Get all keys', size_name, tdiff

    end subroutine test_get_all_keys


    subroutine report_rehash_times( map, hasher, hash_name, size_name )
        type(chaining_hashmap_type), intent(inout) :: map
        procedure(hasher_fun)                   :: hasher
        character(*), intent(in)                :: hash_name, size_name
        real :: t1, t2, tdiff

        call cpu_time(t1)
        call map % rehash( hasher )
        call cpu_time(t2)
        tdiff = t2-t1

        write(lun, '("|", a18, " | ", a12, " | ", a15, " | ", f10.5, " |")') &
            trim(hash_name), 'Rehash data', size_name, tdiff

    end subroutine report_rehash_times


    subroutine report_removal_times( map, test_block, hash_name, size_name )
        type(chaining_hashmap_type), intent(inout) :: map
        integer(int_index), intent(in)          :: test_block
        character(*), intent(in)                :: hash_name, size_name
        real :: t1, t2, tdiff
        type(key_type) :: key
        integer(int_index) :: index2
        logical :: existed

        call cpu_time(t1)
        do index2=1, size(test_8_bits), test_block
            call set( key, test_8_bits( index2:index2+test_block-1 ) )
            call map % remove(key, existed)
            if ( .not. existed ) &
                error stop "Key not found in entry removal."
        end do
        call cpu_time(t2)
        tdiff = t2-t1

        write(lun, '("|", a18, " | ", a12, " | ", a15, " | ", f10.5, " |")') &
            trim(hash_name), 'Remove data', size_name, tdiff
        flush(lun)

    end subroutine report_removal_times


    subroutine report_hash_statistics( map, hash_name, size_name )
        type(chaining_hashmap_type), intent(inout) :: map
        character(*), intent(in)                :: hash_name, size_name
        integer(int_depth) :: depth

        write(lun, *)
        write(lun, '("Statistics for chaining hash table with ",' // &
              'A, " hasher on ", A, ".")' ) hash_name, size_name
        write(lun, '("Slots = ", I0)' ) map % num_slots()
        write(lun, '("Calls = ", I0)' ) map % calls()
        write(lun, '("Entries = ", I0)' ) map % entries()
        write(lun, '("Total probes = ", I0)' ) map % map_probes()
        write(lun, '("Loading = ", ES10.3)' ) map % loading()
        depth = map % total_depth()
        write(lun, '("Total depth = ", I0)' ) depth
        write(lun, '("Relative depth = ", ES10.3)') &
            real( depth ) / real( map % entries() )

    end subroutine report_hash_statistics


end program test_chaining_maps
