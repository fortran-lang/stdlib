program example_path
    use stdlib_system, only: joinpath, operator(/), splitpath, ISWIN, dirname, basename
    character(len=:), allocatable :: p1, p2, head, tail
    character(len=20) :: parr(4)

    if (ISWIN) then
        p1 = 'C:'/'Users'/'User1'/'Desktop'
        parr = [character(len=20) :: 'C:', 'Users', 'User1', 'Desktop']
        p2 = joinpath(parr)

        ! p1 == p2 = 'C:\Users\User1\Desktop'
        print *, p1
        print *, "p1 == p2: ", p1 == p2

        call splitpath(p1, head, tail)
        print *, p1 // " -> " // head // " + " // tail

        call splitpath(head, p1, tail)
        print *, head // " -> " // p1 // " + " // tail

        print *, 'dirname of '// p1 // ' -> ' // dirname(p1)
        print *, 'basename of '// p1 // ' -> ' // basename(p1)
    else
        p1 = ''/'home'/'User1'/'Desktop'
        parr = [character(len=20) :: '', 'home', 'User1', 'Desktop']
        p2 = joinpath(parr)

        ! p1 == p2 = '/home/User1/Desktop'
        print *, p1
        print *, "p1 == p2: ", p1 == p2

        call splitpath(p1, head, tail)
        print *, p1 // " -> " // head // " + " // tail

        call splitpath(head, p1, tail)
        print *, head // " -> " // p1 // " + " // tail

        print *, 'dirname of '// p1 // ' -> ' // dirname(p1)
        print *, 'basename of '// p1 // ' -> ' // basename(p1)
    end if
end program example_path
