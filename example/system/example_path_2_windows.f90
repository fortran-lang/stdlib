! Usage of splitpath, dirname, basename
! Only Windows
program example_splitpath_windows
    use stdlib_system, only: joinpath, splitpath, dirname, basename
    character(len=:), allocatable :: p1, head, tail

    p1 = joinpath('C:\Users\User1', 'Desktop') ! C:\Users\User1\Desktop

    call splitpath(p1, head, tail)
    ! head = C:\Users\User1, tail = Desktop
    print *, p1 // " -> " // head // " + " // tail ! C:\Users\User1\Desktop -> C:\Users\User1 + Desktop

    call splitpath(head, p1, tail)
    ! p1 = C:\Users, tail = User1
    print *, head // " -> " // p1 // " + " // tail ! C:\Users\User1 -> C:\Users + User1

    print *, 'dirname of '// p1 // ' -> ' // dirname(p1) ! dirname of C:\Users -> C:\
    print *, 'basename of '// p1 // ' -> ' // basename(p1) ! basename of C:\Users -> Users
end program example_splitpath_windows
