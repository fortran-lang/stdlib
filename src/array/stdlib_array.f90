! SPDX-Identifier: MIT

!> Module for index manipulation and general array handling
!>
!> Both public procedures are experimental.

module stdlib_array
  implicit none
  private

  public :: trueloc, falseloc

contains

  !> Version: experimental
  !>
  !> Return the indices of the true elements in the logical mask.
  !> Indices are returned in array order. The result is empty if no elements match.


  pure function trueloc(array, lbound) result(loc)
    !> Rank-one array of default logical kind.
    logical, intent(in) :: array(:)
    !> Index to use for the first element of ``array``. Defaults to 1.
    !> For an array with a different lower bound, pass that bound here.
    integer, intent(in), optional :: lbound
    !> Array of default integers with one index for each true element.
    integer :: loc(count(array))

    call logicalloc(loc, array, .true., lbound)
  end function trueloc

  !> Version: experimental
  !>
  !> Return the indices of the false elements in the logical mask.
  !> Indices are returned in array order. The result is empty if no elements match.
  pure function falseloc(array, lbound) result(loc)
    !> Rank-one array of default logical kind.
    logical, intent(in) :: array(:)
    !> Index to use for the first element of ``array``. Defaults to 1.
    !> For an array with a different lower bound, pass that bound here.
    integer, intent(in), optional :: lbound
    !> Array of default integers with one index for each false element.
    integer :: loc(count(.not.array))

    call logicalloc(loc, array, .false., lbound)
  end function falseloc

  !> Return the positions of the truthy elements in array
  pure subroutine logicalloc(loc, array, truth, lbound)
    !> Locations of truthy elements
    integer, intent(out) :: loc(:)
    !> Mask of logicals
    logical, intent(in) :: array(:)
    !> Truthy value
    logical, intent(in) :: truth
    !> Lower bound of array to index
    integer, intent(in), optional :: lbound
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
  end subroutine logicalloc

end module stdlib_array
