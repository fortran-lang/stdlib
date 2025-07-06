! Usage of split_path
program example_path_split_path
    use stdlib_system, only: join_path, split_path, OS_TYPE, OS_WINDOWS
    character(len=:), allocatable :: p1, head, tail

    if(OS_TYPE() == OS_WINDOWS) then
        p1 = join_path('C:\Users\User1', 'Desktop') ! C:\Users\User1\Desktop
    else
        p1 = join_path('/home/User1', 'Desktop') ! /home/User1/Desktop
    endif 
    
    call split_path(p1, head, tail)
    ! head = /home/User1 OR C:\Users\User1, tail = Desktop
    print *, p1 // " -> " // head // " + " // tail 
    ! C:\Users\User1\Desktop -> C:\Users\User1 + Desktop
    ! OR
    ! /home/User1/Desktop -> /home/User1 + Desktop

    call split_path(head, p1, tail)
    ! p1 = /home OR C:\Users, tail = User1
    print *, head // " -> " // p1 // " + " // tail 
    ! C:\Users\User1 -> C:\Users + User1
    ! OR
    ! /home/User1 -> /home + User1
end program example_path_split_path
