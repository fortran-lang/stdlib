program demo_copy_other
use stdlib_hashmap_wrappers, only: &
copy_other, get, other_type, set
use iso_fortran_env, only: int8
implicit none
type(other_type) :: other_in, other_out
integer(int8) :: i
class(*), allocatable :: dummy
type dummy_type
integer(int8) :: value(15)
end type
type(dummy_type) :: dummy_val
do i=1, 15
dummy_val % value(i) = i
end do
allocate(other_in % value, source=dummy_val)
call copy_other( other_in, other_out )
select type(other_out)
typeis (dummy_type)
print *, "other_in == other_out = ", &
all( other_in % value == other_out % value )
end select
end program demo_copy_other
