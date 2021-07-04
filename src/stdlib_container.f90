! SPDX-Identifier: MIT

!> Definition of a data container. The data container is used to define higher-level
!> data structures based on instances of the container. Users of higher-level data
!> structures can transparently interact with the data inside the container.
!>
!> The specification of this module is available [here](../page/specs/stdlib_container.html).
module stdlib_container
    implicit none
    private

    public :: container_type


    !> Simple data container
    !>
    !> Version: experimental
    type :: container_type
        !> Scalar polymorphic value
        class(*), allocatable :: val
    end type container_type


end module stdlib_container
