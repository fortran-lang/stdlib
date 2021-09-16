program test_parse_mode
use stdlib_io, only: parse_mode
use stdlib_error, only: check
implicit none

call test_parse_mode_expected_order()

call test_parse_mode_reverse_order()

call test_parse_mode_random_order()

!call test_parse_mode_always_fail()

contains

    subroutine test_parse_mode_expected_order()
    character(3) :: m
    m = parse_mode("")
    call check(m == "r t")

    m = parse_mode("r")
    call check(m == "r t")
    m = parse_mode("w")
    call check(m == "w t")
    m = parse_mode("a")
    call check(m == "a t")
    m = parse_mode("x")
    call check(m == "x t")

    m = parse_mode("rt")
    call check(m == "r t")
    m = parse_mode("wt")
    call check(m == "w t")
    m = parse_mode("at")
    call check(m == "a t")
    m = parse_mode("xt")
    call check(m == "x t")

    m = parse_mode("rb")
    call check(m == "r b")
    m = parse_mode("wb")
    call check(m == "w b")
    m = parse_mode("ab")
    call check(m == "a b")
    m = parse_mode("xb")
    call check(m == "x b")

    m = parse_mode("r+")
    call check(m == "r+t")
    m = parse_mode("w+")
    call check(m == "w+t")
    m = parse_mode("a+")
    call check(m == "a+t")
    m = parse_mode("x+")
    call check(m == "x+t")

    m = parse_mode("r+t")
    call check(m == "r+t")
    m = parse_mode("w+t")
    call check(m == "w+t")
    m = parse_mode("a+t")
    call check(m == "a+t")
    m = parse_mode("x+t")
    call check(m == "x+t")

    m = parse_mode("r+b")
    call check(m == "r+b")
    m = parse_mode("w+b")
    call check(m == "w+b")
    m = parse_mode("a+b")
    call check(m == "a+b")
    m = parse_mode("x+b")
    call check(m == "x+b")

    end subroutine

    subroutine test_parse_mode_reverse_order()
    character(3) :: m
    m = parse_mode("")
    call check(m == "r t")

    m = parse_mode("tr")
    call check(m == "r t")
    m = parse_mode("tw")
    call check(m == "w t")
    m = parse_mode("ta")
    call check(m == "a t")
    m = parse_mode("tx")
    call check(m == "x t")

    m = parse_mode("br")
    call check(m == "r b")
    m = parse_mode("bw")
    call check(m == "w b")
    m = parse_mode("ba")
    call check(m == "a b")
    m = parse_mode("bx")
    call check(m == "x b")

    m = parse_mode("+r")
    call check(m == "r+t")
    m = parse_mode("+w")
    call check(m == "w+t")
    m = parse_mode("+a")
    call check(m == "a+t")
    m = parse_mode("+x")
    call check(m == "x+t")

    m = parse_mode("t+r")
    call check(m == "r+t")
    m = parse_mode("t+w")
    call check(m == "w+t")
    m = parse_mode("t+a")
    call check(m == "a+t")
    m = parse_mode("t+x")
    call check(m == "x+t")

    m = parse_mode("b+r")
    call check(m == "r+b")
    m = parse_mode("b+w")
    call check(m == "w+b")
    m = parse_mode("b+a")
    call check(m == "a+b")
    m = parse_mode("x+b")
    call check(m == "x+b")

    end subroutine

    subroutine test_parse_mode_random_order()
    character(3) :: m
    m = parse_mode("")
    call check(m == "r t")

    m = parse_mode("t r")
    call check(m == "r t")
    m = parse_mode(" tw ")
    call check(m == "w t")
    m = parse_mode("ta  ")
    call check(m == "a t")
    m = parse_mode("  t   x   ")
    call check(m == "x t")

    m = parse_mode("+ r ")
    call check(m == "r+t")
    m = parse_mode("w   +")
    call check(m == "w+t")
    m = parse_mode(" a+")
    call check(m == "a+t")
    m = parse_mode(" x+   t  ")
    call check(m == "x+t")

    m = parse_mode("tr+ ")
    call check(m == "r+t")
    m = parse_mode("wt + ")
    call check(m == "w+t")
    m = parse_mode("a + t")
    call check(m == "a+t")
    m = parse_mode(" xt + ")
    call check(m == "x+t")

    m = parse_mode(" + t")
    call check(m == "r+t")
    m = parse_mode(" +w  b")
    call check(m == "w+b")
    m = parse_mode("a + b")
    call check(m == "a+b")
    m = parse_mode(" b + x  ")
    call check(m == "x+b")

    end subroutine

    subroutine test_parse_mode_always_fail()
    character(3) :: m

    m = parse_mode("r+w")
    call check(m /= "r t")

    m = parse_mode("tt")
    call check(m /= "r t")

    m = parse_mode("bt")
    call check(m /= "r t")

    end subroutine


end program
