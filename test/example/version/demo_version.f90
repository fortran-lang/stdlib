program demo_version
use stdlib_version, only : get_stdlib_version
implicit none
character(len=:), allocatable :: version
call get_stdlib_version(string=version)
print '(a)', version
end program demo_version
