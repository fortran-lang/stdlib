program demo_free_other
use stdlib_hashmap_wrappers, only: &
copy_other, free_other, other_type, set
use iso_fortran_env, only: int8
implicit none
type dummy_type
integer(int8) :: value(15)
end type dummy_type
typer(dummy_type) :: dummy_val
type(other_type), allocatable :: other_in, other_out
integer(int_8) :: i
do i=1, 15
dummy_val % value(i) = i
end do
allocate(other_in, source=dummy_val)
call copy_other( other_in, other_out )
call free_other( other_out )
end program demo_free_other
