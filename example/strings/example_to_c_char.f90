program example_to_c_char
  use stdlib_strings, only: to_c_char
  use stdlib_string_type, only: string_type
  use stdlib_kinds, only: c_char
  implicit none
  
  character(kind=c_char), allocatable :: cstr(:),cstr2(:)
  character(*), parameter :: hello = "Hello, World!"
  
  ! Convert character array
  cstr = to_c_char(hello)
  
  ! Convert string type  
  cstr2 = to_c_char(string_type(hello))
    
  if (size(cstr)/=size(cstr2) .or. .not.all(cstr==cstr2)) then 
     error stop 'String conversion error'
  end if
  
end program example_to_c_char
