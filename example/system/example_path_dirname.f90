! Usage of splitpath, dirname, basename
program example_path_splitpath
    use stdlib_system, only: dirname, ISWIN
    character(len=:), allocatable :: p1, head, tail

    if( ISWIN ) then
        p1 = 'C:\Users' ! C:\Users
    else
        p1 = '/home' ! /home
    endif 

    print *, 'dirname of '// p1 // ' -> ' // dirname(p1) 
    ! dirname of C:\Users -> C:\
    ! OR
    ! dirname of /home -> /
end program example_path_splitpath
