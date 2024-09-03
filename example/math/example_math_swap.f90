program example_math_swap
    use stdlib_math, only: swap
    implicit none
    
    block
        integer :: x, y
        x = 9
        y = 18
        call swap(x,y)
    end block

    block
        real :: x, y
        x = 4.0
        y = 8.0
        call swap(x,y)
    end block

    block
        real :: x(3), y(3)
        x = [1.0,2.0,3.0]
        y = [4.0,5.0,6.0]
        call swap(x,y)
    end block

    block
        character(5) :: x, y
        x = 'abcde'
        y = 'fghij'
        call swap(x,y)
    end block

end program example_math_swap  