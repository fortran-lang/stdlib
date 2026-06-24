program example_unique
  use stdlib_kinds, only: xdp, int8
  use stdlib_constants, only: zero_xdp
  use stdlib_hashmaps, only: chaining_hashmap_type
  use stdlib_hashmap_wrappers, only: key_type, set
  implicit none

  ! integer, parameter :: xdp = selected_real_kind(18)
  ! real(xdp), parameter :: zero_xdp = 0._xdp
  block
    real(xdp), allocatable :: array(:), result(:)
    integer(int8), allocatable :: bytes(:)

    allocate(array(5), source=zero_xdp)
    array = [5.0_xdp, 4.0_xdp, 3.0_xdp, -1.0_xdp, 3.0_xdp, 5.0_xdp]
    result = unique(array)

    !-----------------------------------------!
    ! print lines to see the byte structure of array
    allocate(bytes(storage_size(array(1))/8))
    bytes = transfer(array(1), bytes)
    print*, "array(1): ", bytes
    bytes = transfer(array(2), bytes)
    print*, "array(2): ", bytes
    bytes = transfer(array(3), bytes)
    print*, "array(3): ", bytes
    bytes = transfer(array(4), bytes)
    print*, "array(4): ", bytes
    !-----------------------------------------!

    print*, "array: ", array
    print*, "result: ", result
    deallocate(array)
  end block
  block
    real(xdp), allocatable :: array(:), result(:)
    integer(int8), allocatable :: bytes(:)

    allocate(array(4), source=zero_xdp)
    array = [4.0_xdp, 4.0_xdp, 4.0_xdp, 4.0_xdp]
    result = unique(array)

    !-----------------------------------------!
    ! print lines to see the byte structure of array
    print*
    bytes = transfer(array(1), bytes)
    print*, "array(1): ", bytes
    bytes = transfer(array(2), bytes)
    print*, "array(2): ", bytes
    bytes = transfer(array(3), bytes)
    print*, "array(3): ", bytes
    bytes = transfer(array(4), bytes)
    print*, "array(4): ", bytes
    !-----------------------------------------!

    print*, "array: ", array
    print*, "result: ", result
    deallocate(array)
  end block

contains

  function unique(A) result(output)
    implicit none
    real(xdp), intent(in) :: A(:)
    real(xdp), allocatable :: output(:)

    real(xdp), allocatable:: temp(:)

    if(size(A) == 0) then
        allocate(output(0))
        return
    end if
    allocate(temp, source=A)
    output = xdp_stable_unique(temp)
  end function unique

  function xdp_stable_unique(temp) result(output)
    real(xdp), intent(in) :: temp(:)
    real(xdp), allocatable :: output(:)

    type(chaining_hashmap_type) :: map
    logical, allocatable :: mask(:)
    logical :: present
    integer :: i
    type(key_type) :: key
    integer(int8) :: bytes(storage_size(temp(1))/8)

    call map%init(slots_bits=10)
    allocate(mask(size(temp)))
    do i = 1, size(temp)
        bytes = 0
        bytes = transfer(temp(i), bytes)
        call set(key, bytes)
        call map%key_test(key, present)
        if (.not. present) then
            call map%map_entry(key)
            mask(i) = .true.
        else
            mask(i) = .false.
        end if
    end do
    output = pack(temp, mask)
    deallocate(mask)
  end function
end program example_unique
