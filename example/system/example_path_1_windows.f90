! Usage of joinpath, operator(/)
! Only Windows
program example_join_windows
    use stdlib_system, only: joinpath, operator(/)
    character(len=:), allocatable :: p1, p2, p3
    character(len=20) :: parr(4)

    p1 = 'C:'/'Users'/'User1'/'Desktop'
    p2 = joinpath('C:\Users\User1', 'Desktop')

    parr = [character(len=20) :: 'C:', 'Users', 'User1', 'Desktop']
    p3 = joinpath(parr)

    ! (p1 == p2 == p3) = 'C:\Users\User1\Desktop'
    print *, p1 ! C:\Users\User1\Desktop
    print *, "p1 == p2: ", p1 == p2 ! T
    print *, "p2 == p3: ", p2 == p3 ! T
end program example_join_windows
