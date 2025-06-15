! Usage of splitpath, dirname, basename
program example_path_splitpath
    use stdlib_system, only: joinpath, splitpath, ISWIN
    character(len=:), allocatable :: p1, head, tail

    if( ISWIN ) then
        p1 = joinpath('C:\Users\User1', 'Desktop') ! C:\Users\User1\Desktop
    else
        p1 = joinpath('/home/User1', 'Desktop') ! /home/User1/Desktop
    endif 
    
    call splitpath(p1, head, tail)
    ! head = /home/User1 OR C:\Users\User1, tail = Desktop
    print *, p1 // " -> " // head // " + " // tail 
    ! C:\Users\User1\Desktop -> C:\Users\User1 + Desktop
    ! OR
    ! /home/User1/Desktop -> /home/User1 + Desktop

    call splitpath(head, p1, tail)
    ! p1 = /home OR C:\Users, tail = User1
    print *, head // " -> " // p1 // " + " // tail 
    ! C:\Users\User1 -> C:\Users + User1
    ! OR
    ! /home/User1 -> /home + User1
end program example_path_splitpath
