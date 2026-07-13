program example_base64_encode_into
    use stdlib_base64, only: base64_encode_into
    use stdlib_kinds, only: int8
    use stdlib_error, only: state_type
    implicit none

    integer(int8) :: bytes(3)
    character(len=4) :: str
    integer :: encoded_len
    type(state_type) :: err_state

    bytes = [77_int8, 97_int8, 110_int8]
    call base64_encode_into(bytes, str, encoded_len, err_state)

    if (err_state%ok()) then
        print '(a)', str
    else
        print '(a)', "Encoding failed"
    end if
end program example_base64_encode_into
