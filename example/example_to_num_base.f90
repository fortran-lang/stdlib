program example_to_num_base
    use stdlib_kinds, only: int8, int32
    use stdlib_str2num, only: to_num_base
    implicit none
    character(*), parameter :: text = " 123 rest"
    integer(int32) :: value
    integer(int8) :: position, status

    call to_num_base(text, value, position, status)
    print *, "value:", value
    print *, "next character:", text(position:position)
    print *, "status:", status
end program example_to_num_base
