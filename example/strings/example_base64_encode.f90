program example_base64_encode
    use stdlib_base64, only: base64_encode
    use stdlib_kinds, only: int8
    implicit none

    character(len=:), allocatable :: encoded
    integer(int8) :: bytes(3)

    bytes = [77_int8, 97_int8, 110_int8]
    encoded = base64_encode(bytes)

    print '(a)', encoded
end program example_base64_encode
