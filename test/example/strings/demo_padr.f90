program demo_padr
    use stdlib_string_type, only: string_type, assignment(=)
    use stdlib_strings, only: padr
    implicit none
    string_type :: string

    string = "right pad this string"
! string <-- "right pad this string"

    print *, padr(string, 25, "$") ! "right pad this string$$$$"

    string = padr(string, 25)
! string <-- "right pad this string    "

end program demo_padr
