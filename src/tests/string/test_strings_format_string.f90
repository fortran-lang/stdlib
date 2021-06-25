program test_strings_format_string
    use, non_intrinsic :: stdlib_strings, only: format_string
    implicit none
    print *, 'format_string(complex) : '
        print *, format_string((1, 1))
        print *, format_string((1, 1), '(F6.2)')
        print *, format_string((1, 1), '(F6.2)'), format_string((2, 2), '(F7.3)')
    print *, 'format_string(integer) : '
        print *, format_string(100)
        print *, format_string(100, '(I6)')
        print *, format_string(100, '(I6)'), format_string(1000, '(I7)')
    print *, 'format_string(real) : '
        print *, format_string(100.)
        print *, format_string(100., '(F6.2)')
        print *, format_string(100., '(F6.2)'), &
                    format_string(1000., '(F7.3)'), format_string(1000, '(F7.3)')
                        !! Wrong demonstration
    print *, 'format_string(logical) : '
        print *, format_string(.true.)
        print *, format_string(.true., '(L2)')
        print *, format_string(.false., '(L2)'), format_string(.true., '(L5)'), &
                    format_string(.false., '(I5)')
                        !! Wrong demonstration
end program test_strings_format_string