! Usage of base_name
program example_path_base_name
    use stdlib_system, only: base_name, ISWIN
    character(len=:), allocatable :: p1

    if( ISWIN ) then
        p1 = 'C:\Users'
    else
        p1 = '/home'
    endif 
    
    print *, 'base name of '// p1 // ' -> ' // base_name(p1) 
    ! base name of C:\Users -> Users
    ! OR
    ! base name of /home -> home
end program example_path_base_name
