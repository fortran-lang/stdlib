! SPDX-Identifier: MIT
module test_strip_chomp
    use stdlib_ascii, only : TAB, VT, NUL, LF, CR, FF
    use stdlib_error, only : check
    use stdlib_strings, only : strip, chomp
    use stdlib_string_type, only : string_type, operator(==), operator(//)
    implicit none

contains

    subroutine test_strip_char
        call check(strip("   hello   ") == "hello")
        call check(strip(TAB//"goodbye"//CR//LF) == "goodbye")
        call check(strip(NUL//TAB//LF//VT//FF//CR) == NUL)
        call check(strip(" "//TAB//LF//VT//FF//CR) == "")
        call check(strip("  !  ")//"!" == "!!")
        call check(strip("Hello") == "Hello")
    end subroutine test_strip_char

    subroutine test_strip_string
        call check(strip(string_type("   hello   ")) == "hello")
        call check(strip(string_type(TAB//"goodbye"//CR//LF)) == "goodbye")
        call check(strip(string_type(NUL//TAB//LF//VT//FF//CR)) == NUL)
        call check(strip(string_type(" "//TAB//LF//VT//FF//CR)) == "")
        call check(strip(string_type("  !  "))//"!" == "!!")
        call check(strip(string_type("Hello")) == "Hello")
    end subroutine test_strip_string

    subroutine test_chomp_char
        call check(chomp("hello") == "hello")
        call check(chomp("hello"//LF) == "hello", "1")
        call check(chomp("hello"//CR//LF) == "hello", "2")
        call check(chomp("hello"//LF//CR) == "hello", "3")
        call check(chomp("hello"//CR) == "hello", "4")
        call check(chomp("hello "//LF//" there") == "hello "//LF//" there")
        call check(chomp("hello"//CR//LF//CR//LF) == "hello")
        call check(chomp("hello"//CR//LF//CR//CR//LF) == "hello")
        call check(chomp(NUL//TAB//LF//VT//FF//CR) == NUL)
        call check(chomp(" "//TAB//LF//VT//FF//CR) == "")
        call check(chomp("  !  ")//"!" == "  !!")
    end subroutine test_chomp_char

    subroutine test_chomp_string
        call check(chomp(string_type("hello")) == "hello")
        call check(chomp(string_type("hello"//LF)) == "hello")
        call check(chomp(string_type("hello"//CR//LF)) == "hello")
        call check(chomp(string_type("hello"//LF//CR)) == "hello")
        call check(chomp(string_type("hello"//CR)) == "hello")
        call check(chomp(string_type("hello "//LF//" there")) == "hello "//LF//" there")
        call check(chomp(string_type("hello"//CR//LF//CR//LF)) == "hello")
        call check(chomp(string_type("hello"//CR//LF//CR//CR//LF)) == "hello")
        call check(chomp(string_type(NUL//TAB//LF//VT//FF//CR)) == NUL)
        call check(chomp(string_type(" "//TAB//LF//VT//FF//CR)) == "")
        call check(chomp(string_type("  !  "))//"!" == "  !!")
    end subroutine test_chomp_string

    subroutine test_chomp_set_char
        call check(chomp("hello", ["l", "o"]) == "he")
        call check(chomp("hello", set=["l", "o"]) == "he")
    end subroutine test_chomp_set_char

    subroutine test_chomp_set_string
        call check(chomp(string_type("hello"), ["l", "o"]) == "he")
        call check(chomp(string_type("hello"), set=["l", "o"]) == "he")
        call check(chomp("hellooooo", ["o", "o"]) == "hell")
        call check(chomp("hellooooo", set=["o", "o"]) == "hell")
    end subroutine test_chomp_set_string

    subroutine test_chomp_substring_char
        call check(chomp("hello", "") == "hello")
        call check(chomp("hello", substring="") == "hello")
        call check(chomp("hello", "lo") == "hel")
        call check(chomp("hello", substring="lo") == "hel")
        call check(chomp("hellooooo", "oo") == "hello")
        call check(chomp("hellooooo", substring="oo") == "hello")
    end subroutine test_chomp_substring_char

    subroutine test_chomp_substring_string
        call check(chomp(string_type("hello"), "") == "hello")
        call check(chomp(string_type("hello"), substring="") == "hello")
        call check(chomp(string_type("hello"), "lo") == "hel")
        call check(chomp(string_type("hello"), substring="lo") == "hel")
        call check(chomp("hello", string_type("lo")) == "hel")
        call check(chomp("hello", substring=string_type("lo")) == "hel")
        call check(chomp(string_type("hello"), string_type("lo")) == "hel")
        call check(chomp(string_type("hello"), substring=string_type("lo")) == "hel")
        call check(chomp(string_type("hellooooo"), "oo") == "hello")
        call check(chomp(string_type("hellooooo"), substring="oo") == "hello")
        call check(chomp("hellooooo", string_type("oo")) == "hello")
        call check(chomp("hellooooo", substring=string_type("oo")) == "hello")
        call check(chomp(string_type("hellooooo"), string_type("oo")) == "hello")
        call check(chomp(string_type("hellooooo"), substring=string_type("oo")) == "hello")
    end subroutine test_chomp_substring_string

end module test_strip_chomp

program tester
    use test_strip_chomp
    implicit none

    call test_strip_char
    call test_strip_string
    call test_chomp_char
    call test_chomp_string
    call test_chomp_set_char
    call test_chomp_set_string
    call test_chomp_substring_char
    call test_chomp_substring_string

end program tester
