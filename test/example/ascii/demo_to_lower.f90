program demo_to_lower
use stdlib_ascii, only : to_lower
implicit none
print'(a)', to_lower("HELLo!") ! returns "hello!"
end program demo_to_lower
