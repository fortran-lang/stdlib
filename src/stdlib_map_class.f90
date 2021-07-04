! SPDX-Identifier: MIT

!> Abstract base class of a map.
!>
!> The specification of this module is available [here](../page/specs/stdlib_map_class.html).
module stdlib_map_class
    use stdlib_container, only : container_type
    implicit none
    private

    public :: map_class, len


    !> Definition of an abstract base class of a map
    !>
    !> Version: experimental
    type, abstract :: map_class
    contains
      !> Get length of the map
      procedure(get_length), deferred :: get_length
      !> Insert a new key into the map
      procedure(insert), deferred :: insert
      !> Get an existing key
      procedure(get), deferred :: get
      !> Drop a key from the map
      procedure(drop), deferred :: drop
      !> Clear the map
      procedure(clear), deferred :: clear
    end type map_class

    !> Get number of entries in the map
    !>
    !> Version: experimental
    interface len
        module procedure :: len
    end interface len

    abstract interface
        !> Get current number of occupied nest in the map
        pure function get_length(self) result(length)
            import :: map_class
            !> Instance of the map
            class(map_class), intent(in) :: self
            !> Number of allocated values
            integer :: length
        end function get_length

        !> Get a pointer to the nest with the corresponding character key
        subroutine get(self, key, ptr)
            import :: map_class, container_type
            !> Instance of the map
            class(map_class), target, intent(inout) :: self
            !> Character key
            character(len=*), intent(in) :: key
            !> Pointer to the occupied nest
            type(container_type), pointer, intent(out) :: ptr
        end subroutine get

        !> Insert a new key into the map
        recursive subroutine insert(self, key, ptr)
            import :: map_class, container_type
            !> Instance of the map
            class(map_class), target, intent(inout) :: self
            !> Character key to insert
            character(len=*), intent(in) :: key
            !> Pointer for the newly occupied nest
            type(container_type), pointer, intent(out) :: ptr
        end subroutine insert

        !> Clear the map
        subroutine clear(self)
            import :: map_class
            !> Instance of the map
            class(map_class), intent(inout) :: self
        end subroutine clear

        !> Drop a key from the map
        subroutine drop(self, key)
            import :: map_class
            !> Instance of the map
            class(map_class), intent(inout) :: self
            !> Character key
            character(len=*), intent(in) :: key
        end subroutine drop
    end interface

contains

    !> Get current number of keys in the map
    pure function len(self) result(length)
        !> Instance of the map
        class(map_class), intent(in) :: self
        !> Number of allocated values
        integer :: length

        length = self%get_length()

    end function len

end module stdlib_map_class
