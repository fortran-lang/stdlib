program test_open
use stdlib_experimental_io, only: parse_mode
use stdlib_experimental_error, only: assert
implicit none

call test_parse_mode()

call test_parse_mode_reverse_order()

call test_parse_mode_random_order()

contains

    subroutine test_parse_mode()
    character(3) :: m
    m = parse_mode("")
    call assert(m == "r t")

    m = parse_mode("r")
    call assert(m == "r t")
    m = parse_mode("w")
    call assert(m == "w t")
    m = parse_mode("a")
    call assert(m == "a t")
    m = parse_mode("x")
    call assert(m == "x t")

    m = parse_mode("rt")
    call assert(m == "r t")
    m = parse_mode("wt")
    call assert(m == "w t")
    m = parse_mode("at")
    call assert(m == "a t")
    m = parse_mode("xt")
    call assert(m == "x t")

    m = parse_mode("rb")
    call assert(m == "r b")
    m = parse_mode("wb")
    call assert(m == "w b")
    m = parse_mode("ab")
    call assert(m == "a b")
    m = parse_mode("xb")
    call assert(m == "x b")

    m = parse_mode("r+")
    call assert(m == "r+t")
    m = parse_mode("w+")
    call assert(m == "w+t")
    m = parse_mode("a+")
    call assert(m == "a+t")
    m = parse_mode("x+")
    call assert(m == "x+t")

    m = parse_mode("r+t")
    call assert(m == "r+t")
    m = parse_mode("w+t")
    call assert(m == "w+t")
    m = parse_mode("a+t")
    call assert(m == "a+t")
    m = parse_mode("x+t")
    call assert(m == "x+t")

    m = parse_mode("r+b")
    call assert(m == "r+b")
    m = parse_mode("w+b")
    call assert(m == "w+b")
    m = parse_mode("a+b")
    call assert(m == "a+b")
    m = parse_mode("x+b")
    call assert(m == "x+b")

    end subroutine

    subroutine test_parse_mode_reverse_order()
    character(3) :: m
    m = parse_mode("")
    call assert(m == "r t")

    m = parse_mode("tr")
    call assert(m == "r t")
    m = parse_mode("tw")
    call assert(m == "w t")
    m = parse_mode("ta")
    call assert(m == "a t")
    m = parse_mode("tx")
    call assert(m == "x t")

    m = parse_mode("br")
    call assert(m == "r b")
    m = parse_mode("bw")
    call assert(m == "w b")
    m = parse_mode("ba")
    call assert(m == "a b")
    m = parse_mode("bx")
    call assert(m == "x b")

    m = parse_mode("+r")
    call assert(m == "r+t")
    m = parse_mode("+w")
    call assert(m == "w+t")
    m = parse_mode("+a")
    call assert(m == "a+t")
    m = parse_mode("+x")
    call assert(m == "x+t")

    m = parse_mode("t+r")
    call assert(m == "r+t")
    m = parse_mode("t+w")
    call assert(m == "w+t")
    m = parse_mode("t+a")
    call assert(m == "a+t")
    m = parse_mode("t+x")
    call assert(m == "x+t")

    m = parse_mode("b+r")
    call assert(m == "r+b")
    m = parse_mode("b+w")
    call assert(m == "w+b")
    m = parse_mode("b+a")
    call assert(m == "a+b")
    m = parse_mode("x+b")
    call assert(m == "x+b")

    end subroutine

    subroutine test_parse_mode_random_order()
    character(3) :: m
    m = parse_mode("")
    call assert(m == "r t")

    m = parse_mode("t r")
    call assert(m == "r t")
    m = parse_mode(" tw ")
    call assert(m == "w t")
    m = parse_mode("ta  ")
    call assert(m == "a t")
    m = parse_mode("  t   x   ")
    call assert(m == "x t")

    m = parse_mode("+ r ")
    call assert(m == "r+t")
    m = parse_mode("w   +")
    call assert(m == "w+t")
    m = parse_mode(" a+")
    call assert(m == "a+t")
    m = parse_mode(" x+   t  ")
    call assert(m == "x+t")

    m = parse_mode("tr+ ")
    call assert(m == "r+t")
    m = parse_mode("wtt + ")
    call assert(m == "w+t")
    m = parse_mode("a + t")
    call assert(m == "a+t")
    m = parse_mode(" xt + ")
    call assert(m == "x+t")

    m = parse_mode("t + t")
    call assert(m == "r+t")
    m = parse_mode(" ww + b")
    call assert(m == "w+b")
    m = parse_mode("a + b")
    call assert(m == "a+b")
    m = parse_mode(" b + x  ")
    call assert(m == "x+b")

    end subroutine


end program
