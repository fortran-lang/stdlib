! SPDX-Identifier: MIT

!> Hash map implementation using cuckoo hashing.
!>
!> The specification of this module is available [here](../page/specs/stdlib_map_cuckoohash.html).
module stdlib_map_cuckoohash
    use, intrinsic :: iso_fortran_env, only : stderr => error_unit
    use stdlib_container, only : container_type
    use stdlib_hash, only : hash
    use stdlib_map_class, only : map_class, len
    use stdlib_optval, only : optval
    implicit none
    private

    public :: cuckoo_hash, new_cuckoo_hash, len

    !> Long length for integers
    integer, parameter :: i8 = selected_int_kind(18)

    !> Number of nest rows
    integer, parameter :: nest_rows = 3

    !> Initial seed
    integer(i8), parameter :: default_seed(nest_rows) = [2946901_i8, 5039_i8, 433494437_i8]

    !> Initial size
    integer, parameter :: initial_capacity = 10


    !> Wrapper for the key-value type
    type :: cuckoo_nest
        !> Hashed key of the nest
        character(len=:), allocatable :: key
        !> Data payload
        type(container_type), allocatable :: val
    end type cuckoo_nest


    !> Cuckoo hash map
    !>
    !> Version: experimental
    type, extends(map_class) :: cuckoo_hash
        !> Capacity of the map
        integer :: capacity
        !> Seed for the hash functions
        integer(i8) :: seed(nest_rows)
        !> Maximum tries for claiming a nest before rehashing the map
        integer :: max_try
        !> Allocated values in the dictionary
        integer :: nalloc
        !> Possible nests for cuckoos
        type(cuckoo_nest), allocatable :: nest(:, :)
    contains
        !> Get length of the map
        procedure :: get_length
        !> Insert a new key into the map
        procedure :: insert
        !> Insert a new key into the map
        procedure, private :: insert_nest
        !> Get an existing key
        procedure :: get
        !> Get an existing nest
        procedure, private :: get_nest
        !> Drop a key from the map
        procedure :: drop
        !> Clear the map
        procedure :: clear
    end type cuckoo_hash


    !> Initialize the cuckoo hash map
    interface cuckoo_hash
        module procedure :: create_cuckoo_hash
    end interface cuckoo_hash

    !> Overload operator for cuckoo nest
    interface operator(==)
        module procedure :: equals
    end interface


contains


    !> Compare a cuckoo nest against a key
    elemental function equals(lhs, rhs)
        !> Cuckoo nest
        type(cuckoo_nest), intent(in) :: lhs
        !> Character key
        character(len=*), intent(in) :: rhs
        !> Cuckoo key matches key
        logical :: equals

        if (allocated(lhs%key)) then
            equals = lhs%key == rhs
        else
            equals = .false.
        end if
    end function equals


    !> Initialize a new map
    subroutine new_cuckoo_hash(self, capacity, seed)
        !> Instance of the map
        type(cuckoo_hash), intent(out) :: self
        !> Capacity of this map
        integer, intent(in), optional :: capacity
        !> Seeds for the hashing function, all must be unique
        integer(i8), intent(in), optional :: seed(nest_rows)

        self%nalloc = 0
        self%capacity = optval(capacity, initial_capacity)/nest_rows+1
        self%max_try = ceiling(nest_rows*log(real(nest_rows*self%capacity)))
        allocate(self%nest(1:self%capacity, nest_rows))

        self%seed(:) = default_seed
        if (present(seed)) then
            if (nest_rows == count(spread(seed, 1, nest_rows) == spread(seed, 2, nest_rows))) then
                self%seed(:) = seed
            end if
        end if
    end subroutine new_cuckoo_hash


    function create_cuckoo_hash(capacity, seed) result(new)
        !> Capacity of this map
        integer, intent(in) :: capacity
        !> Seeds for the hashing function, all must be unique
        integer(i8), intent(in), optional :: seed(nest_rows)
        !> Instance of the map
        type(cuckoo_hash) :: new

        call new_cuckoo_hash(new, capacity, seed)
    end function create_cuckoo_hash


    !> Get current number of occupied nest in the map
    pure function get_length(self) result(length)
        !> Instance of the map
    class(cuckoo_hash), intent(in) :: self
        !> Number of allocated values
        integer :: length

        length = self%nalloc
    end function get_length


    !> Hash function of the map
    pure function get_hash(self, key, is) result(pos)
        !> Instance of the map
        class(cuckoo_hash), intent(in) :: self
        !> Character key to be hashed
        character(len=*), intent(in) :: key
        !> Selected seed
        integer, intent(in) :: is
        !> Position in the map
        integer :: pos

        pos = modulo(hash(key, self%seed(is)), self%capacity)+1
    end function get_hash


    !> Get a pointer to the nest with the corresponding character key
    subroutine get_nest(self, key, ptr)
        !> Instance of the map
        class(cuckoo_hash), target, intent(in) :: self
        !> Character key
        character(len=*), intent(in) :: key
        !> Pointer to the occupied nest
        type(cuckoo_nest), pointer, intent(out) :: ptr

        integer :: ih, is

        ptr => null()
        do is = 1, nest_rows
            ih = get_hash(self, key, is)
            if (self%nest(ih, is) == key) then
                ptr => self%nest(ih, is)
                exit
            end if
        end do
    end subroutine get_nest


    !> Get a pointer to the nest with the corresponding character key
    subroutine get(self, key, ptr)
        !> Source of the error
        character(len=*), parameter :: source = "cuckoo_hash%get"
        !> Instance of the map
        class(cuckoo_hash), target, intent(inout) :: self
        !> Character key
        character(len=*), intent(in) :: key
        !> Pointer to the occupied nest
        type(container_type), pointer, intent(out) :: ptr

        type(cuckoo_nest), pointer :: nest
        integer :: ii

        call self%get_nest(key, nest)
        if (associated(nest)) then
            if (.not.allocated(nest%val)) then
                write(stderr, '("[FATAL]",1x,a,":",1x,a)') source, &
                    & "Key '"//key//"' present, but value not allocated"
                ptr => null()
            end if
            ptr => nest%val
        else
            ptr => null()
        end if
    end subroutine get


    !> Insert a new key into the map
    recursive subroutine insert(self, key, ptr)
        !> Source of the error
        character(len=*), parameter :: source = "cuckoo_hash%insert"
        !> Instance of the map
        class(cuckoo_hash), target, intent(inout) :: self
        !> Character key to insert
        character(len=*), intent(in) :: key
        !> Pointer for the newly occupied nest
        type(container_type), pointer, intent(out) :: ptr

        ! Pointer for the newly occupied nest
        type(cuckoo_nest), pointer :: nest

        call self%insert_nest(key, nest)
        if (allocated(nest%val)) then
            ptr => nest%val
        else
            write(stderr, '("[FATAL]",1x,a,":",1x,a)') source, &
                & "Key '"//key//"' present, but value not allocated"
            ptr => null()
        end if
    end subroutine insert


    !> Insert a new key into the map
    recursive subroutine insert_nest(self, key, ptr)
        !> Instance of the map
        class(cuckoo_hash), intent(inout) :: self
        !> Character key to insert
        character(len=*), intent(in) :: key
        !> Pointer for the newly occupied nest
        type(cuckoo_nest), pointer, intent(out) :: ptr

        ! Pointer for the newly occupied nest
        type(cuckoo_nest), pointer :: tmp
        ! Temporary cuckoo searching for a new nest
        type(cuckoo_nest) :: cuckoo

        integer :: ih, is, try

        call self%get_nest(key, ptr)
        if (.not.associated(ptr)) then
            cuckoo%key = key
            allocate(cuckoo%val)
            find: do try = 1, self%max_try
                do is = 1, nest_rows
                    ih = get_hash(self, cuckoo%key, is)
                    if (allocated(self%nest(ih, is)%key)) then
                        call swap(self%nest(ih, is), cuckoo)
                    else
                        call move(cuckoo, self%nest(ih, is))
                        self%nalloc = self%nalloc + 1
                        exit find
                    end if
                end do
            end do find
            if (allocated(cuckoo%key)) then
                call rehash(self)
                call self%insert_nest(cuckoo%key, tmp)
            end if
            call self%get_nest(key, ptr)
        end if
    end subroutine insert_nest


    !> Rehash the map
    recursive subroutine rehash(self)
        !> Instance of the map
        class(cuckoo_hash), intent(inout) :: self
        !> Old cuckoo nests
        type(cuckoo_nest), allocatable :: nest(:, :)
        !> Cuckoo for reassigning the value pointer
        type(cuckoo_nest), pointer :: cuckoo

        character(len=:), allocatable :: key
        integer :: ii, jj
        integer :: capacity
        integer :: nalloc

        call move_alloc(self%nest, nest)

        capacity = self%capacity
        self%capacity = capacity + capacity/2 + 1
        allocate(self%nest(1:self%capacity, nest_rows))
        self%max_try = ceiling(nest_rows*log(real(2*self%capacity)))

        do ii = 1, nest_rows
            do jj = 1, capacity
                if (allocated(nest(jj, ii)%key)) then
                    self%nalloc = self%nalloc - 1
                    call move_alloc(nest(jj, ii)%key, key)
                    call self%insert_nest(key, cuckoo)
                    call move_alloc(nest(jj, ii)%val, cuckoo%val)
                end if
            end do
        end do

        deallocate(nest)
    end subroutine rehash


    !> Clear the map
    subroutine clear(self)
        !> Source of the error
        character(len=*), parameter :: source = "cuckoo_hash%clear"
        !> Instance of the map
        class(cuckoo_hash), intent(inout) :: self

        integer :: ii, jj

        do ii = 1, nest_rows
            do jj = 1, self%capacity
                if (allocated(self%nest(jj, ii)%key)) then
                    call self%drop(self%nest(jj, ii)%key)
                end if
            end do
        end do

        if (self%nalloc /= 0) then
            write(stderr, '("[FATAL]",1x,a,":",1x,a,1x,"(",i0,"/",i0,")")') source, &
                & "Memory leak in cuckoo hash map implementation", self%nalloc, self%get_length()
        end if
    end subroutine clear


    !> Swap two cuckoos
    elemental subroutine swap(lhs, rhs)
        !> Cuckoo currently occupying a nest
        type(cuckoo_nest), intent(inout) :: lhs
        !> New cuckoo resident
        type(cuckoo_nest), intent(inout) :: rhs

        type(cuckoo_nest) :: tmp

        call move(lhs, tmp)
        call move(rhs, lhs)
        call move(tmp, rhs)
    end subroutine swap


    !> Move content from one nest to another
    elemental subroutine move(from, to)
        !> New cuckoo resident
        type(cuckoo_nest), intent(inout) :: from
        !> Empty cuckoo nest
        type(cuckoo_nest), intent(inout) :: to

        call move_alloc(from%key, to%key)
        call move_alloc(from%val, to%val)
    end subroutine move


    !> Drop a cuckoo from the map
    subroutine drop(self, key)
        !> Instance of the map
        class(cuckoo_hash), intent(inout) :: self
        !> Character key
        character(len=*), intent(in) :: key
        !> Pointer to the nest to be cleared
        type(cuckoo_nest), pointer :: nest

        call self%get_nest(key, nest)

        if (associated(nest)) then
            self%nalloc = self%nalloc - 1
            if (allocated(nest%val)) then
                deallocate(nest%val)
            end if
            if (allocated(nest%key)) then
                deallocate(nest%key)
            end if
        end if
    end subroutine drop

end module stdlib_map_cuckoohash
