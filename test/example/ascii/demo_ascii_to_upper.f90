program demo_to_upper
    use stdlib_ascii, only: to_upper
    implicit none
    print'(a)', to_upper("hello!") ! returns "HELLO!"
end program demo_to_upper
