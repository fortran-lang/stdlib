! SPDX-Identifier: MIT

!> Proxy module to provide access to the map data types
!>
!> The specification of this module is available [here](../page/specs/stdlib_map.html).
module stdlib_map
    use stdlib_map_class, only : map_class, len
    use stdlib_map_cuckoohash, only : cuckoo_hash, new_cuckoo_hash
    implicit none
    private

    public :: map_class, len

contains

    !> Generic constructor to create a map with one of the available implementations
    !>
    !> Version: experimental
    subroutine new_map(self)
        !> Instance of a map class
        class(map_class), allocatable, intent(out) :: self

        block
            type(cuckoo_hash), allocatable :: map
            allocate(map)
            call new_cuckoo_hash(map)
            call move_alloc(map, self)
        end block

    end subroutine new_map

end module stdlib_map
