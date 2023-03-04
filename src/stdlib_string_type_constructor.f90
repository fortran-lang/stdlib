submodule(stdlib_string_type) stdlib_string_type_constructor

    use stdlib_strings, only: to_string

contains

    !> Constructor for new string instances from a scalar character value.
    elemental module function new_string(string) result(new)
        character(len=*), intent(in), optional :: string
        type(string_type) :: new
        if (present(string)) then
            new%raw = string
        end if
    end function new_string

    !> Constructor for new string instances from an integer of kind int8.
    elemental module function new_string_from_integer_int8(val) result(new)
        integer(int8), intent(in) :: val
        type(string_type) :: new
        new%raw = to_string(val)
    end function new_string_from_integer_int8
    !> Constructor for new string instances from an integer of kind int16.
    elemental module function new_string_from_integer_int16(val) result(new)
        integer(int16), intent(in) :: val
        type(string_type) :: new
        new%raw = to_string(val)
    end function new_string_from_integer_int16
    !> Constructor for new string instances from an integer of kind int32.
    elemental module function new_string_from_integer_int32(val) result(new)
        integer(int32), intent(in) :: val
        type(string_type) :: new
        new%raw = to_string(val)
    end function new_string_from_integer_int32
    !> Constructor for new string instances from an integer of kind int64.
    elemental module function new_string_from_integer_int64(val) result(new)
        integer(int64), intent(in) :: val
        type(string_type) :: new
        new%raw = to_string(val)
    end function new_string_from_integer_int64

    !> Constructor for new string instances from a logical of kind lk.
    elemental module function new_string_from_logical_lk(val) result(new)
        logical(lk), intent(in) :: val
        type(string_type) :: new
        new%raw = to_string(val)
    end function new_string_from_logical_lk

end submodule stdlib_string_type_constructor