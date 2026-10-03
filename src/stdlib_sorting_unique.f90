#include "macros.inc"

submodule(stdlib_sorting) stdlib_sorting_unique
#if STDLIB_HASHMAPS
    use stdlib_hashmaps, only: chaining_hashmap_type
    use stdlib_hashmap_wrappers, only: key_type, set
#endif
    use stdlib_constants
    use stdlib_string_type, only: string_type, char_string => char, operator(==), operator(/=)
    implicit none

contains
    module subroutine int8_unique(array, output, sorted_output &
                                        )
        integer(int8), intent(in) :: array(:)
        integer(int8), allocatable, intent(inout) :: output(:)
        logical, optional, intent(in) :: sorted_output

        logical :: sorted_output_
        integer(int8), allocatable :: temp(:)

        if(size(array) == 0) then
            if (allocated(output)) then
                deallocate(output)
            end if
            allocate(output(0))
            return
        end if

        sorted_output_ = optval(sorted_output, .false.)

        if(sorted_output_) then
            allocate(temp, source=array)
            call sort_unique(temp, output)
            deallocate(temp)
        else
#if !STDLIB_HASHMAPS
                error stop "unsorted version requires STDLIB_HASHMAPS"
#endif
            call unsorted_unique(array, output)
        end if
    end subroutine
    module subroutine int16_unique(array, output, sorted_output &
                                        )
        integer(int16), intent(in) :: array(:)
        integer(int16), allocatable, intent(inout) :: output(:)
        logical, optional, intent(in) :: sorted_output

        logical :: sorted_output_
        integer(int16), allocatable :: temp(:)

        if(size(array) == 0) then
            if (allocated(output)) then
                deallocate(output)
            end if
            allocate(output(0))
            return
        end if

        sorted_output_ = optval(sorted_output, .false.)

        if(sorted_output_) then
            allocate(temp, source=array)
            call sort_unique(temp, output)
            deallocate(temp)
        else
#if !STDLIB_HASHMAPS
                error stop "unsorted version requires STDLIB_HASHMAPS"
#endif
            call unsorted_unique(array, output)
        end if
    end subroutine
    module subroutine int32_unique(array, output, sorted_output &
                                        )
        integer(int32), intent(in) :: array(:)
        integer(int32), allocatable, intent(inout) :: output(:)
        logical, optional, intent(in) :: sorted_output

        logical :: sorted_output_
        integer(int32), allocatable :: temp(:)

        if(size(array) == 0) then
            if (allocated(output)) then
                deallocate(output)
            end if
            allocate(output(0))
            return
        end if

        sorted_output_ = optval(sorted_output, .false.)

        if(sorted_output_) then
            allocate(temp, source=array)
            call sort_unique(temp, output)
            deallocate(temp)
        else
#if !STDLIB_HASHMAPS
                error stop "unsorted version requires STDLIB_HASHMAPS"
#endif
            call unsorted_unique(array, output)
        end if
    end subroutine
    module subroutine int64_unique(array, output, sorted_output &
                                        )
        integer(int64), intent(in) :: array(:)
        integer(int64), allocatable, intent(inout) :: output(:)
        logical, optional, intent(in) :: sorted_output

        logical :: sorted_output_
        integer(int64), allocatable :: temp(:)

        if(size(array) == 0) then
            if (allocated(output)) then
                deallocate(output)
            end if
            allocate(output(0))
            return
        end if

        sorted_output_ = optval(sorted_output, .false.)

        if(sorted_output_) then
            allocate(temp, source=array)
            call sort_unique(temp, output)
            deallocate(temp)
        else
#if !STDLIB_HASHMAPS
                error stop "unsorted version requires STDLIB_HASHMAPS"
#endif
            call unsorted_unique(array, output)
        end if
    end subroutine
    module subroutine sp_unique(array, output, sorted_output &
                                        , tolerance)
        real(sp), intent(in) :: array(:)
        real(sp), allocatable, intent(inout) :: output(:)
        logical, optional, intent(in) :: sorted_output
        real(sp), optional, intent(in) :: tolerance

        real(sp) :: tolerance_
        logical :: sorted_output_
        real(sp), allocatable :: temp(:)

        if(size(array) == 0) then
            if (allocated(output)) then
                deallocate(output)
            end if
            allocate(output(0))
            return
        end if

        sorted_output_ = optval(sorted_output, .false.)

        if (.not. sorted_output_ .and. present(tolerance)) then
            error stop "tolerance requires sorted_output=.true."
        end if
        tolerance_ = optval(tolerance, 0.0_sp)
        if(tolerance_ < 0.0_sp) error stop "tolerance must be non-negative"
        if(sorted_output_) then
            allocate(temp, source=array)
            call sort_unique(temp, output, tolerance_)
            deallocate(temp)
        else
#if !STDLIB_HASHMAPS
                error stop "unsorted version requires STDLIB_HASHMAPS"
#endif
            call unsorted_unique(array, output)
        end if
    end subroutine
    module subroutine dp_unique(array, output, sorted_output &
                                        , tolerance)
        real(dp), intent(in) :: array(:)
        real(dp), allocatable, intent(inout) :: output(:)
        logical, optional, intent(in) :: sorted_output
        real(dp), optional, intent(in) :: tolerance

        real(dp) :: tolerance_
        logical :: sorted_output_
        real(dp), allocatable :: temp(:)

        if(size(array) == 0) then
            if (allocated(output)) then
                deallocate(output)
            end if
            allocate(output(0))
            return
        end if

        sorted_output_ = optval(sorted_output, .false.)

        if (.not. sorted_output_ .and. present(tolerance)) then
            error stop "tolerance requires sorted_output=.true."
        end if
        tolerance_ = optval(tolerance, 0.0_dp)
        if(tolerance_ < 0.0_dp) error stop "tolerance must be non-negative"
        if(sorted_output_) then
            allocate(temp, source=array)
            call sort_unique(temp, output, tolerance_)
            deallocate(temp)
        else
#if !STDLIB_HASHMAPS
                error stop "unsorted version requires STDLIB_HASHMAPS"
#endif
            call unsorted_unique(array, output)
        end if
    end subroutine
    module subroutine char_unique(array, output, sorted_output &
                                        )
        character(len=*), intent(in) :: array(:)
        character(len=len(array)), allocatable, intent(inout) :: output(:)
        logical, optional, intent(in) :: sorted_output

        logical :: sorted_output_
        character(len=len(array)), allocatable :: temp(:)

        if(size(array) == 0) then
            if (allocated(output)) then
                deallocate(output)
            end if
            allocate(output(0))
            return
        end if

        sorted_output_ = optval(sorted_output, .false.)

        if(sorted_output_) then
            allocate(temp, source=array)
            call sort_unique(temp, output)
            deallocate(temp)
        else
#if !STDLIB_HASHMAPS
                error stop "unsorted version requires STDLIB_HASHMAPS"
#endif
            call unsorted_unique(array, output)
        end if
    end subroutine
    module subroutine string_type_unique(array, output, sorted_output &
                                        )
        type(string_type), intent(in) :: array(:)
        type(string_type), allocatable, intent(inout) :: output(:)
        logical, optional, intent(in) :: sorted_output

        logical :: sorted_output_
        type(string_type), allocatable :: temp(:)

        if(size(array) == 0) then
            if (allocated(output)) then
                deallocate(output)
            end if
            allocate(output(0))
            return
        end if

        sorted_output_ = optval(sorted_output, .false.)

        if(sorted_output_) then
            allocate(temp, source=array)
            call sort_unique(temp, output)
            deallocate(temp)
        else
#if !STDLIB_HASHMAPS
                error stop "unsorted version requires STDLIB_HASHMAPS"
#endif
            call unsorted_unique(array, output)
        end if
    end subroutine
    module subroutine csp_unique(array, output &
                                        )
        complex(sp), intent(in) :: array(:)
        complex(sp), allocatable, intent(inout) :: output(:)


        if(size(array) == 0) then
            if (allocated(output)) then
                deallocate(output)
            end if
            allocate(output(0))
            return
        end if


#if !STDLIB_HASHMAPS
                error stop "unsorted version requires STDLIB_HASHMAPS"
#endif
            call unsorted_unique(array, output)
    end subroutine
    module subroutine cdp_unique(array, output &
                                        )
        complex(dp), intent(in) :: array(:)
        complex(dp), allocatable, intent(inout) :: output(:)


        if(size(array) == 0) then
            if (allocated(output)) then
                deallocate(output)
            end if
            allocate(output(0))
            return
        end if


#if !STDLIB_HASHMAPS
                error stop "unsorted version requires STDLIB_HASHMAPS"
#endif
            call unsorted_unique(array, output)
    end subroutine

    module subroutine int8_sort_unique(array, output)
        integer(int8), intent(inout) :: array(:)
        integer(int8), allocatable, intent(inout) :: output(:)

        logical, allocatable :: mask(:)
        integer :: i

        allocate(mask(size(array)))
        mask(1) = .true.
        call sort(array)
        do i = 2, size(array)
            mask(i) = array(i) /= array(i-1)
        end do
        if (.not. allocated(output)) then
            allocate(output(size(array)))
        else if (size(output) < size(array)) then
            deallocate(output)
            allocate(output(size(array)))
        end if
        output = pack(array, mask)
        deallocate(mask)
    end subroutine
    module subroutine int16_sort_unique(array, output)
        integer(int16), intent(inout) :: array(:)
        integer(int16), allocatable, intent(inout) :: output(:)

        logical, allocatable :: mask(:)
        integer :: i

        allocate(mask(size(array)))
        mask(1) = .true.
        call sort(array)
        do i = 2, size(array)
            mask(i) = array(i) /= array(i-1)
        end do
        if (.not. allocated(output)) then
            allocate(output(size(array)))
        else if (size(output) < size(array)) then
            deallocate(output)
            allocate(output(size(array)))
        end if
        output = pack(array, mask)
        deallocate(mask)
    end subroutine
    module subroutine int32_sort_unique(array, output)
        integer(int32), intent(inout) :: array(:)
        integer(int32), allocatable, intent(inout) :: output(:)

        logical, allocatable :: mask(:)
        integer :: i

        allocate(mask(size(array)))
        mask(1) = .true.
        call sort(array)
        do i = 2, size(array)
            mask(i) = array(i) /= array(i-1)
        end do
        if (.not. allocated(output)) then
            allocate(output(size(array)))
        else if (size(output) < size(array)) then
            deallocate(output)
            allocate(output(size(array)))
        end if
        output = pack(array, mask)
        deallocate(mask)
    end subroutine
    module subroutine int64_sort_unique(array, output)
        integer(int64), intent(inout) :: array(:)
        integer(int64), allocatable, intent(inout) :: output(:)

        logical, allocatable :: mask(:)
        integer :: i

        allocate(mask(size(array)))
        mask(1) = .true.
        call sort(array)
        do i = 2, size(array)
            mask(i) = array(i) /= array(i-1)
        end do
        if (.not. allocated(output)) then
            allocate(output(size(array)))
        else if (size(output) < size(array)) then
            deallocate(output)
            allocate(output(size(array)))
        end if
        output = pack(array, mask)
        deallocate(mask)
    end subroutine
    module subroutine sp_sort_unique(array, output, tolerance)
        real(sp), intent(inout) :: array(:)
        real(sp), allocatable, intent(inout) :: output(:)
        real(sp), intent(in) :: tolerance

        logical, allocatable :: mask(:)
        integer :: i
        real(sp) :: last_unique

        allocate(mask(size(array)))
        mask(1) = .true.
        call sort(array)
        last_unique = array(1)
        do i = 2, size(array)
            mask(i) = abs(array(i)-last_unique) > tolerance
            if(mask(i)) last_unique = array(i)
        end do
        if (.not. allocated(output)) then
            allocate(output(size(array)))
        else if (size(output) < size(array)) then
            deallocate(output)
            allocate(output(size(array)))
        end if
        output = pack(array, mask)
        deallocate(mask)
    end subroutine
    module subroutine dp_sort_unique(array, output, tolerance)
        real(dp), intent(inout) :: array(:)
        real(dp), allocatable, intent(inout) :: output(:)
        real(dp), intent(in) :: tolerance

        logical, allocatable :: mask(:)
        integer :: i
        real(dp) :: last_unique

        allocate(mask(size(array)))
        mask(1) = .true.
        call sort(array)
        last_unique = array(1)
        do i = 2, size(array)
            mask(i) = abs(array(i)-last_unique) > tolerance
            if(mask(i)) last_unique = array(i)
        end do
        if (.not. allocated(output)) then
            allocate(output(size(array)))
        else if (size(output) < size(array)) then
            deallocate(output)
            allocate(output(size(array)))
        end if
        output = pack(array, mask)
        deallocate(mask)
    end subroutine
    module subroutine char_sort_unique(array, output)
        character(len=*), intent(inout) :: array(:)
        character(len=len(array)), allocatable, intent(inout) :: output(:)

        logical, allocatable :: mask(:)
        integer :: i

        allocate(mask(size(array)))
        mask(1) = .true.
        call sort(array)
        do i = 2, size(array)
            mask(i) = array(i) /= array(i-1)
        end do
        if (.not. allocated(output)) then
            allocate(output(size(array)))
        else if (size(output) < size(array)) then
            deallocate(output)
            allocate(output(size(array)))
        end if
        output = pack(array, mask)
        deallocate(mask)
    end subroutine
    module subroutine string_type_sort_unique(array, output)
        type(string_type), intent(inout) :: array(:)
        type(string_type), allocatable, intent(inout) :: output(:)

        logical, allocatable :: mask(:)
        integer :: i

        allocate(mask(size(array)))
        mask(1) = .true.
        call sort(array)
        do i = 2, size(array)
            mask(i) = array(i) /= array(i-1)
        end do
        if (.not. allocated(output)) then
            allocate(output(size(array)))
        else if (size(output) < size(array)) then
            deallocate(output)
            allocate(output(size(array)))
        end if
        output = pack(array, mask)
        deallocate(mask)
    end subroutine

#if STDLIB_HASHMAPS
    module subroutine int8_unsorted_unique(array, output)
        integer(int8), intent(in) :: array(:)
        integer(int8), allocatable, intent(inout) :: output(:)

        type(chaining_hashmap_type) :: map
        logical, allocatable :: mask(:)
        logical :: present
        integer :: i
        integer(int8) :: key(storage_size(array(1))/8)

        call map%init()
        allocate(mask(size(array)))
        do i = 1, size(array)
            key = transfer(array(i), key)
            call map%key_test(key, present)
            if (.not. present) then
                call map%map_entry(key)
                mask(i) = .true.
            else
                mask(i) = .false.
            end if
        end do
        if (.not. allocated(output)) then
            allocate(output(size(array)))
        else if (size(output) < size(array)) then
            deallocate(output)
            allocate(output(size(array)))
        end if
        output = pack(array, mask)
        deallocate(mask)
    end subroutine
    module subroutine int16_unsorted_unique(array, output)
        integer(int16), intent(in) :: array(:)
        integer(int16), allocatable, intent(inout) :: output(:)

        type(chaining_hashmap_type) :: map
        logical, allocatable :: mask(:)
        logical :: present
        integer :: i
        integer(int8) :: key(storage_size(array(1))/8)

        call map%init()
        allocate(mask(size(array)))
        do i = 1, size(array)
            key = transfer(array(i), key)
            call map%key_test(key, present)
            if (.not. present) then
                call map%map_entry(key)
                mask(i) = .true.
            else
                mask(i) = .false.
            end if
        end do
        if (.not. allocated(output)) then
            allocate(output(size(array)))
        else if (size(output) < size(array)) then
            deallocate(output)
            allocate(output(size(array)))
        end if
        output = pack(array, mask)
        deallocate(mask)
    end subroutine
    module subroutine int32_unsorted_unique(array, output)
        integer(int32), intent(in) :: array(:)
        integer(int32), allocatable, intent(inout) :: output(:)

        type(chaining_hashmap_type) :: map
        logical, allocatable :: mask(:)
        logical :: present
        integer :: i
        integer(int8) :: key(storage_size(array(1))/8)

        call map%init()
        allocate(mask(size(array)))
        do i = 1, size(array)
            key = transfer(array(i), key)
            call map%key_test(key, present)
            if (.not. present) then
                call map%map_entry(key)
                mask(i) = .true.
            else
                mask(i) = .false.
            end if
        end do
        if (.not. allocated(output)) then
            allocate(output(size(array)))
        else if (size(output) < size(array)) then
            deallocate(output)
            allocate(output(size(array)))
        end if
        output = pack(array, mask)
        deallocate(mask)
    end subroutine
    module subroutine int64_unsorted_unique(array, output)
        integer(int64), intent(in) :: array(:)
        integer(int64), allocatable, intent(inout) :: output(:)

        type(chaining_hashmap_type) :: map
        logical, allocatable :: mask(:)
        logical :: present
        integer :: i
        integer(int8) :: key(storage_size(array(1))/8)

        call map%init()
        allocate(mask(size(array)))
        do i = 1, size(array)
            key = transfer(array(i), key)
            call map%key_test(key, present)
            if (.not. present) then
                call map%map_entry(key)
                mask(i) = .true.
            else
                mask(i) = .false.
            end if
        end do
        if (.not. allocated(output)) then
            allocate(output(size(array)))
        else if (size(output) < size(array)) then
            deallocate(output)
            allocate(output(size(array)))
        end if
        output = pack(array, mask)
        deallocate(mask)
    end subroutine
    module subroutine sp_unsorted_unique(array, output)
        real(sp), intent(in) :: array(:)
        real(sp), allocatable, intent(inout) :: output(:)

        type(chaining_hashmap_type) :: map
        logical, allocatable :: mask(:)
        logical :: present
        integer :: i
        integer(int8) :: key(storage_size(array(1))/8)

        call map%init()
        allocate(mask(size(array)))
        do i = 1, size(array)
            key = transfer(array(i), key)
            call map%key_test(key, present)
            if (.not. present) then
                call map%map_entry(key)
                mask(i) = .true.
            else
                mask(i) = .false.
            end if
        end do
        if (.not. allocated(output)) then
            allocate(output(size(array)))
        else if (size(output) < size(array)) then
            deallocate(output)
            allocate(output(size(array)))
        end if
        output = pack(array, mask)
        deallocate(mask)
    end subroutine
    module subroutine dp_unsorted_unique(array, output)
        real(dp), intent(in) :: array(:)
        real(dp), allocatable, intent(inout) :: output(:)

        type(chaining_hashmap_type) :: map
        logical, allocatable :: mask(:)
        logical :: present
        integer :: i
        integer(int8) :: key(storage_size(array(1))/8)

        call map%init()
        allocate(mask(size(array)))
        do i = 1, size(array)
            key = transfer(array(i), key)
            call map%key_test(key, present)
            if (.not. present) then
                call map%map_entry(key)
                mask(i) = .true.
            else
                mask(i) = .false.
            end if
        end do
        if (.not. allocated(output)) then
            allocate(output(size(array)))
        else if (size(output) < size(array)) then
            deallocate(output)
            allocate(output(size(array)))
        end if
        output = pack(array, mask)
        deallocate(mask)
    end subroutine
    module subroutine char_unsorted_unique(array, output)
        character(len=*), intent(in) :: array(:)
        character(len=len(array)), allocatable, intent(inout) :: output(:)

        type(chaining_hashmap_type) :: map
        logical, allocatable :: mask(:)
        logical :: present
        integer :: i
        type(key_type) :: key

        call map%init()
        allocate(mask(size(array)))
        do i = 1, size(array)
            call set(key, array(i))
            call map%key_test(key, present)
            if (.not. present) then
                call map%map_entry(key)
                mask(i) = .true.
            else
                mask(i) = .false.
            end if
        end do
        if (.not. allocated(output)) then
            allocate(output(size(array)))
        else if (size(output) < size(array)) then
            deallocate(output)
            allocate(output(size(array)))
        end if
        output = pack(array, mask)
        deallocate(mask)
    end subroutine
    module subroutine string_type_unsorted_unique(array, output)
        type(string_type), intent(in) :: array(:)
        type(string_type), allocatable, intent(inout) :: output(:)

        type(chaining_hashmap_type) :: map
        logical, allocatable :: mask(:)
        logical :: present
        integer :: i
        type(key_type) :: key

        call map%init()
        allocate(mask(size(array)))
        do i = 1, size(array)
            call set(key, char_string(array(i)))
            call map%key_test(key, present)
            if (.not. present) then
                call map%map_entry(key)
                mask(i) = .true.
            else
                mask(i) = .false.
            end if
        end do
        if (.not. allocated(output)) then
            allocate(output(size(array)))
        else if (size(output) < size(array)) then
            deallocate(output)
            allocate(output(size(array)))
        end if
        output = pack(array, mask)
        deallocate(mask)
    end subroutine
    module subroutine csp_unsorted_unique(array, output)
        complex(sp), intent(in) :: array(:)
        complex(sp), allocatable, intent(inout) :: output(:)

        type(chaining_hashmap_type) :: map
        logical, allocatable :: mask(:)
        logical :: present
        integer :: i
        integer(int8) :: key(storage_size(array(1))/8)

        call map%init()
        allocate(mask(size(array)))
        do i = 1, size(array)
            key = transfer(array(i), key)
            call map%key_test(key, present)
            if (.not. present) then
                call map%map_entry(key)
                mask(i) = .true.
            else
                mask(i) = .false.
            end if
        end do
        if (.not. allocated(output)) then
            allocate(output(size(array)))
        else if (size(output) < size(array)) then
            deallocate(output)
            allocate(output(size(array)))
        end if
        output = pack(array, mask)
        deallocate(mask)
    end subroutine
    module subroutine cdp_unsorted_unique(array, output)
        complex(dp), intent(in) :: array(:)
        complex(dp), allocatable, intent(inout) :: output(:)

        type(chaining_hashmap_type) :: map
        logical, allocatable :: mask(:)
        logical :: present
        integer :: i
        integer(int8) :: key(storage_size(array(1))/8)

        call map%init()
        allocate(mask(size(array)))
        do i = 1, size(array)
            key = transfer(array(i), key)
            call map%key_test(key, present)
            if (.not. present) then
                call map%map_entry(key)
                mask(i) = .true.
            else
                mask(i) = .false.
            end if
        end do
        if (.not. allocated(output)) then
            allocate(output(size(array)))
        else if (size(output) < size(array)) then
            deallocate(output)
            allocate(output(size(array)))
        end if
        output = pack(array, mask)
        deallocate(mask)
    end subroutine
#endif

end submodule stdlib_sorting_unique