! Usage of joinpath, operator(/)
program example_path_join
    use stdlib_system, only: joinpath, operator(/), ISWIN
    character(len=:), allocatable :: p1, p2, p3
    character(len=20) :: parr(4)

    if( ISWIN ) then
        p1 = 'C:'/'Users'/'User1'/'Desktop'
        p2 = joinpath('C:\Users\User1', 'Desktop')
        parr = [character(len=20) :: 'C:', 'Users', 'User1', 'Desktop']
        p3 = joinpath(parr)
    else
        p1 = ''/'home'/'User1'/'Desktop'
        p2 = joinpath('/home/User1', 'Desktop')
        parr = [character(len=20) :: '', 'home', 'User1', 'Desktop']
        p3 = joinpath(parr)
    end if

    ! (p1 == p2 == p3) = '/home/User1/Desktop' OR 'C:\Users\User1\Desktop'
    print *, p1 ! /home/User1/Desktop OR 'C:\Users\User1\Desktop'
    print *, "p1 == p2: ", p1 == p2 ! T
    print *, "p2 == p3: ", p2 == p3 ! T
end program example_path_join
