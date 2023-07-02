program example_sort_bitset
  use stdlib_kinds, only: int32
  use stdlib_sorting, only: sort
  use stdlib_bitsets, only: bitset_large
  implicit none
  type(bitset_large), allocatable :: array(:)
  integer(int32) :: i

  array = [bitset_l("0101"), & ! 5
           bitset_l("0100"), & ! 4
           bitset_l("0011"), & ! 3
           bitset_l("0001"), & ! 1
           bitset_l("1010"), & ! 10
           bitset_l("0100"), & ! 4
           bitset_l("1001")]   ! 9

  call sort(array)

  do i = 1, size(array)
    print *, to_string(array(i))
    ! 0001
    ! 0011
    ! 0100
    ! 0100
    ! 0101
    ! 1001
    ! 1010
  end do

  deallocate(array)
contains
    function bitset_l(str) result(new_bitsetl)
      character(*), intent(in) :: str
      type(bitset_large) :: new_bitsetl

      call new_bitsetl%from_string(str)
    end function bitset_l

    function to_string(bitset) result(str)
      type(bitset_large), intent(in) :: bitset
      character(:), allocatable :: str

      call bitset%to_string(str)
    end function to_string
end program example_sort_bitset
