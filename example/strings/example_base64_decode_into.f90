program example_base64_decode_into
    use stdlib_base64, only: base64_decode_into
    use stdlib_error, only: state_type
    implicit none

    character(len=4) :: str = "TWFu"
    character(len=3) :: res
    integer :: decoded_len
    type(state_type) :: err_state

    call base64_decode_into(str, res, decoded_len, err_state)

    if (err_state%ok()) then
        print '(a)', res
    else
        print '(a)', "Decoding failed"
    end if
end program example_base64_decode_into
