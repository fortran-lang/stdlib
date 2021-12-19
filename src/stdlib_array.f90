! SPDX-Identifier: MIT

!> Module for index manipulation and general array handling
module stdlib_array
  implicit none
  private

  public :: trueloc, falseloc

contains

  !> Return the positions of the true elements in array
  pure function trueloc(array, lbound) result(loc)
    !> Mask of logicals
    logical, intent(in) :: array(:)
    !> Lower bound of array to index
    integer, intent(in), optional :: lbound
    !> Locations of true elements
    integer :: loc(count(array))

    loc = logicalloc(array, .true., lbound)
  end function trueloc

  !> Return the positions of the false elements in array
  pure function falseloc(array, lbound) result(loc)
    !> Mask of logicals
    logical, intent(in) :: array(:)
    !> Lower bound of array to index
    integer, intent(in), optional :: lbound
    !> Locations of false elements
    integer :: loc(count(.not.array))

    loc = logicalloc(array, .false., lbound)
  end function falseloc

  !> Return the positions of the truthy elements in array
  pure function logicalloc(array, truth, lbound) result(loc)
    !> Mask of logicals
    logical, intent(in) :: array(:)
    !> Truthy value
    logical, intent(in) :: truth
    !> Lower bound of array to index
    integer, intent(in), optional :: lbound
    !> Locations of truthy elements
    integer :: loc(count(array.eqv.truth))
    integer :: i, pos, offset

    offset = 0
    if (present(lbound)) offset = lbound - 1

    i = 0
    do pos = 1, size(array)
      if (array(pos).eqv.truth) then
        i = i + 1
        loc(i) = pos + offset
      end if
    end do
  end function logicalloc

end module stdlib_array
