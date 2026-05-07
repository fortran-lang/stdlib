! Usage of join_path, operator(/)
program example_path_join
    use stdlib_system, only: join_path, operator(/), OS_TYPE, OS_WINDOWS
    character(len=:), allocatable :: p1, p2, p3
    character(len=20) :: parr(4)

    if(OS_TYPE() == OS_WINDOWS) then
        p1 = 'C:'/'Users'/'User1'/'Desktop'
        p2 = join_path('C:\Users\User1', 'Desktop')
        parr = [character(len=20) :: 'C:', 'Users', 'User1', 'Desktop']
        p3 = join_path(parr)
    else
        p1 = ''/'home'/'User1'/'Desktop'
        p2 = join_path('/home/User1', 'Desktop')
        parr = [character(len=20) :: '', 'home', 'User1', 'Desktop']
        p3 = join_path(parr)
    end if

    ! (p1 == p2 == p3) = '/home/User1/Desktop' OR 'C:\Users\User1\Desktop'
    print *, p1 ! /home/User1/Desktop OR 'C:\Users\User1\Desktop'
    print *, "p1 == p2: ", p1 == p2 ! T
    print *, "p2 == p3: ", p2 == p3 ! T
end program example_path_join
