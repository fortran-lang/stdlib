program demo_constructor_character
    use stdlib_string_type
    implicit none
    type(string_type) :: string
! len(string) == 0
    string = "Sequence"
! len(string) == 8
end program demo_constructor_character
