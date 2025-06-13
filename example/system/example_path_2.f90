! Usage of splitpath, dirname, basename
program example_splitpath
    use stdlib_system, only: joinpath, splitpath, dirname, basename
    character(len=:), allocatable :: p1, head, tail

    p1 = joinpath('/home/User1', 'Desktop') ! /home/User1/Desktop

    call splitpath(p1, head, tail)
    ! head = /home/User1, tail = Desktop
    print *, p1 // " -> " // head // " + " // tail ! /home/User1/Desktop -> /home/User1 + Desktop

    call splitpath(head, p1, tail)
    ! p1 = /home, tail = User1
    print *, head // " -> " // p1 // " + " // tail ! /home/User1 -> /home + User1

    print *, 'dirname of '// p1 // ' -> ' // dirname(p1) ! dirname of /home -> /
    print *, 'basename of '// p1 // ' -> ' // basename(p1) ! basename of /home -> home
end program example_splitpath
