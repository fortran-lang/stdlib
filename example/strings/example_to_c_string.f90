program example_to_c_string
  use stdlib_strings, only: to_c_string
  use stdlib_string_type, only: string_type
  use stdlib_kinds, only: c_char
  implicit none
  
  character(kind=c_char), allocatable :: cstr(:),cstr2(:)
  character(*), parameter :: hello = "Hello, World!"
  
  ! Convert character array
  cstr = to_c_string(hello)
  
  ! Convert string type  
  cstr2 = to_c_string(string_type(hello))
    
  if (size(cstr)==size(cstr2) .and. all(cstr==cstr2)) then 
     stop 0
  else
     error stop 'String conversion error'
  end if
  
end program example_to_c_string
