! Usage of dir_name
program example_path_dir_name
    use stdlib_system, only: dir_name, OS_TYPE, OS_WINDOWS
    character(len=:), allocatable :: p1, head, tail

    if(OS_TYPE() == OS_WINDOWS) then
        p1 = 'C:\Users' ! C:\Users
    else
        p1 = '/home' ! /home
    endif 

    print *, 'dir_name of '// p1 // ' -> ' // dir_name(p1) 
    ! dir_name of C:\Users -> C:\
    ! OR
    ! dir_name of /home -> /
end program example_path_dir_name
