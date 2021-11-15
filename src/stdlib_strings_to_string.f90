submodule(stdlib_strings) stdlib_strings_to_string

    integer, parameter :: buffer_len = 128
    character(len=*), parameter :: err_sym = "[*]"
        !!TODO: [*]?

contains

    !> Format or transfer a real(sp) scalar as a string.
    pure module function to_string_r_sp(value, format) result(string)
        real(sp), intent(in) :: value
        character(len=*), intent(in), optional :: format
        character(len=:), allocatable :: string
        
        character(len=buffer_len) :: buffer
        integer :: stat

        write(buffer, '(' // optval(format, "g0") // ')', iostat=stat) value
        if (stat == 0) then
            string = trim(buffer)
        else
            string = err_sym
        end if

    end function to_string_r_sp
    !> Format or transfer a real(dp) scalar as a string.
    pure module function to_string_r_dp(value, format) result(string)
        real(dp), intent(in) :: value
        character(len=*), intent(in), optional :: format
        character(len=:), allocatable :: string
        
        character(len=buffer_len) :: buffer
        integer :: stat

        write(buffer, '(' // optval(format, "g0") // ')', iostat=stat) value
        if (stat == 0) then
            string = trim(buffer)
        else
            string = err_sym
        end if

    end function to_string_r_dp

    !> Format or transfer a complex(sp) scalar as a string.
    pure module function to_string_c_sp(value, format) result(string)
        complex(sp), intent(in) :: value
        character(len=*), intent(in), optional :: format
        character(len=:), allocatable :: string

        string = '(' // to_string_r_sp(value%re, format) // ',' // &
                      & to_string_r_sp(value%im, format) // ')'

    end function to_string_c_sp
    !> Format or transfer a complex(dp) scalar as a string.
    pure module function to_string_c_dp(value, format) result(string)
        complex(dp), intent(in) :: value
        character(len=*), intent(in), optional :: format
        character(len=:), allocatable :: string

        string = '(' // to_string_r_dp(value%re, format) // ',' // &
                      & to_string_r_dp(value%im, format) // ')'

    end function to_string_c_dp

    !> Represent an integer of kind int8 as character sequence.
    pure module function to_string_1_i_int8(value) result(string)
        integer(int8), intent(in) :: value
        character(len=:), allocatable :: string
        integer, parameter :: buffer_len = range(value)+2
        character(len=buffer_len) :: buffer
        integer :: pos
        integer(int8) :: n
        character(len=1), parameter :: numbers(0:9) = &
            ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]

        if (value == 0_int8) then
            string = numbers(0)
            return
        end if

        n = abs(value)
        buffer = ""

        pos = buffer_len + 1
        do while (n > 0_int8)
            pos = pos - 1
            buffer(pos:pos) = numbers(mod(n, 10_int8))
            n = n/10_int8
        end do
        if (value < 0_int8) then
            pos = pos - 1
            buffer(pos:pos) = '-'
        end if

        string = buffer(pos:)
    end function to_string_1_i_int8

    pure module function to_string_2_i_int8(value, format) result(string)
        integer(int8), intent(in) :: value
        character(len=*), intent(in) :: format
        character(len=:), allocatable :: string
        
        character(len=buffer_len) :: buffer
        integer :: stat

        write(buffer, "(" // format // ")", iostat=stat) value
        if (stat == 0) then
            string = trim(buffer)
        else
            string = err_sym
        end if

    end function to_string_2_i_int8
    !> Represent an integer of kind int16 as character sequence.
    pure module function to_string_1_i_int16(value) result(string)
        integer(int16), intent(in) :: value
        character(len=:), allocatable :: string
        integer, parameter :: buffer_len = range(value)+2
        character(len=buffer_len) :: buffer
        integer :: pos
        integer(int16) :: n
        character(len=1), parameter :: numbers(0:9) = &
            ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]

        if (value == 0_int16) then
            string = numbers(0)
            return
        end if

        n = abs(value)
        buffer = ""

        pos = buffer_len + 1
        do while (n > 0_int16)
            pos = pos - 1
            buffer(pos:pos) = numbers(mod(n, 10_int16))
            n = n/10_int16
        end do
        if (value < 0_int16) then
            pos = pos - 1
            buffer(pos:pos) = '-'
        end if

        string = buffer(pos:)
    end function to_string_1_i_int16

    pure module function to_string_2_i_int16(value, format) result(string)
        integer(int16), intent(in) :: value
        character(len=*), intent(in) :: format
        character(len=:), allocatable :: string
        
        character(len=buffer_len) :: buffer
        integer :: stat

        write(buffer, "(" // format // ")", iostat=stat) value
        if (stat == 0) then
            string = trim(buffer)
        else
            string = err_sym
        end if

    end function to_string_2_i_int16
    !> Represent an integer of kind int32 as character sequence.
    pure module function to_string_1_i_int32(value) result(string)
        integer(int32), intent(in) :: value
        character(len=:), allocatable :: string
        integer, parameter :: buffer_len = range(value)+2
        character(len=buffer_len) :: buffer
        integer :: pos
        integer(int32) :: n
        character(len=1), parameter :: numbers(0:9) = &
            ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]

        if (value == 0_int32) then
            string = numbers(0)
            return
        end if

        n = abs(value)
        buffer = ""

        pos = buffer_len + 1
        do while (n > 0_int32)
            pos = pos - 1
            buffer(pos:pos) = numbers(mod(n, 10_int32))
            n = n/10_int32
        end do
        if (value < 0_int32) then
            pos = pos - 1
            buffer(pos:pos) = '-'
        end if

        string = buffer(pos:)
    end function to_string_1_i_int32

    pure module function to_string_2_i_int32(value, format) result(string)
        integer(int32), intent(in) :: value
        character(len=*), intent(in) :: format
        character(len=:), allocatable :: string
        
        character(len=buffer_len) :: buffer
        integer :: stat

        write(buffer, "(" // format // ")", iostat=stat) value
        if (stat == 0) then
            string = trim(buffer)
        else
            string = err_sym
        end if

    end function to_string_2_i_int32
    !> Represent an integer of kind int64 as character sequence.
    pure module function to_string_1_i_int64(value) result(string)
        integer(int64), intent(in) :: value
        character(len=:), allocatable :: string
        integer, parameter :: buffer_len = range(value)+2
        character(len=buffer_len) :: buffer
        integer :: pos
        integer(int64) :: n
        character(len=1), parameter :: numbers(0:9) = &
            ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]

        if (value == 0_int64) then
            string = numbers(0)
            return
        end if

        n = abs(value)
        buffer = ""

        pos = buffer_len + 1
        do while (n > 0_int64)
            pos = pos - 1
            buffer(pos:pos) = numbers(mod(n, 10_int64))
            n = n/10_int64
        end do
        if (value < 0_int64) then
            pos = pos - 1
            buffer(pos:pos) = '-'
        end if

        string = buffer(pos:)
    end function to_string_1_i_int64

    pure module function to_string_2_i_int64(value, format) result(string)
        integer(int64), intent(in) :: value
        character(len=*), intent(in) :: format
        character(len=:), allocatable :: string
        
        character(len=buffer_len) :: buffer
        integer :: stat

        write(buffer, "(" // format // ")", iostat=stat) value
        if (stat == 0) then
            string = trim(buffer)
        else
            string = err_sym
        end if

    end function to_string_2_i_int64

    !> Represent an logical of kind lk as character sequence.
    pure module function to_string_1_l_lk(value) result(string)
        logical(lk), intent(in) :: value
        character(len=1) :: string

        string = merge("T", "F", value)

    end function to_string_1_l_lk

    pure module function to_string_2_l_lk(value, format) result(string)
        logical(lk), intent(in) :: value
        character(len=*), intent(in) :: format
        character(len=:), allocatable :: string
        
        character(len=buffer_len) :: buffer
        integer :: stat

        write(buffer, "(" // format // ")", iostat=stat) value
        if (stat == 0) then
            string = trim(buffer)
        else
            string = err_sym
        end if

    end function to_string_2_l_lk
    !> Represent an logical of kind c_bool as character sequence.
    pure module function to_string_1_l_c_bool(value) result(string)
        logical(c_bool), intent(in) :: value
        character(len=1) :: string

        string = merge("T", "F", value)

    end function to_string_1_l_c_bool

    pure module function to_string_2_l_c_bool(value, format) result(string)
        logical(c_bool), intent(in) :: value
        character(len=*), intent(in) :: format
        character(len=:), allocatable :: string
        
        character(len=buffer_len) :: buffer
        integer :: stat

        write(buffer, "(" // format // ")", iostat=stat) value
        if (stat == 0) then
            string = trim(buffer)
        else
            string = err_sym
        end if

    end function to_string_2_l_c_bool

end submodule stdlib_strings_to_string
