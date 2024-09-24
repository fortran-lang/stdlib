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
        character(4) :: x
        character(6) :: y
        x = 'abcd'
        y = 'efghij'
        call swap(x,y)      ! x=efgh,  y=abcd

        x = 'abcd'
        y = 'efghij'
        call swap(x,y(1:4)) ! x=efgh,  y=abcdij
    end block

    block
        use stdlib_string_type
        type(string_type) :: x, y
        x = 'abcde'
        y = 'fghij'
        call swap(x,y)
    end block

    block
        use stdlib_bitsets
        type(bitset_64) :: x, y
        call x%from_string('0000')
        call y%from_string('1111')
        call swap(x,y)
    end block

end program example_math_swap  