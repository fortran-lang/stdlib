! Usage of basename
program example_path_basename
    use stdlib_system, only: basename, ISWIN
    character(len=:), allocatable :: p1

    if( ISWIN ) then
        p1 = 'C:\Users'
    else
        p1 = '/home'
    endif 
    
    print *, 'basename of '// p1 // ' -> ' // basename(p1) 
    ! basename of C:\Users -> Users
    ! OR
    ! basename of /home -> home
end program example_path_basename
