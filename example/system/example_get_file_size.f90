program example_get_file_size
    use stdlib_system, only: get_file_size
    implicit none
    
    character(*), parameter :: path = "src"

    print *, get_file_size(path)

end program example_get_file_size

