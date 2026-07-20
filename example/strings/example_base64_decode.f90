program example_base64_decode
    use stdlib_base64, only: base64_decode
    implicit none

    character(len=:), allocatable :: decoded

    decoded = base64_decode("TWFu")

    print '(a)', decoded
end program example_base64_decode
