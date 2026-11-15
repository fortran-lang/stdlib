submodule(stdlib_system) stdlib_system_path
    use stdlib_strings, only: join
    use stdlib_string_type, only: string_type, char, move
contains
    module function join2_char_char(p1, p2) result(path)
        character(:), allocatable :: path
        character(*), intent(in) :: p1, p2

        path = trim(p1) // path_sep() // trim(p2)
    end function join2_char_char

    module function join2_char_string(p1, p2) result(path)
        character(:), allocatable :: path
        character(*), intent(in) :: p1
        type(string_type), intent(in) :: p2

        path = join_path(p1, char(p2))
    end function join2_char_string

    module function join2_string_char(p1, p2) result(path)
        type(string_type) :: path
        type(string_type), intent(in) :: p1
        character(*), intent(in) :: p2
        character(:), allocatable :: join_char

        join_char = join_path(char(p1), p2)

        call move(join_char, path)
    end function join2_string_char

    module function join2_string_string(p1, p2) result(path)
        type(string_type) :: path
        type(string_type), intent(in) :: p1, p2
        character(:), allocatable :: join_char

        join_char = join_path(char(p1), char(p2))

        call move(join_char, path)
    end function join2_string_string

    module function joinarr_char(p) result(path)
        character(:), allocatable :: path
        character(*), intent(in) :: p(:)

        path = join(p, path_sep())
    end function joinarr_char

    module function joinarr_string(p) result(path)
        type(string_type) :: path
        type(string_type), intent(in) :: p(:)

        path = join(p, path_sep())
    end function joinarr_string

    module function join_op_char_char(p1, p2) result(path)
        character(:), allocatable :: path
        character(*), intent(in) :: p1, p2

        path = join_path(p1, p2)
    end function join_op_char_char

    module function join_op_char_string(p1, p2) result(path)
        character(:), allocatable :: path
        character(*), intent(in) :: p1
        type(string_type), intent(in) :: p2

        path = join_path(p1, p2)
    end function join_op_char_string

    module function join_op_string_char(p1, p2) result(path)
        type(string_type) :: path
        type(string_type), intent(in) :: p1
        character(*), intent(in) :: p2

        path = join_path(p1, p2)
    end function join_op_string_char

    module function join_op_string_string(p1, p2) result(path)
        type(string_type) :: path
        type(string_type), intent(in) :: p1, p2

        path = join_path(p1, p2)
    end function join_op_string_string

    module subroutine split_path_char(p, head, tail)
        character(*), intent(in) :: p
        character(:), allocatable, intent(out) :: head, tail
        character(:), allocatable :: temp, sep_chars
        integer :: n, i
        logical :: is_windows
        character(len=1) :: sep
        sep = path_sep()
        is_windows = OS_TYPE() == OS_WINDOWS

        ! sep: the separator used when building results (OS native)
        ! sep_chars: every character accepted as a separator in the input
        !            (both `\` and `/` on Windows)
        if (is_windows) then
            sep_chars = '/\'
        else
            sep_chars = sep
        end if

        ! Empty string, return (.,'')
        if (len_trim(p) == 0) then
            head = '.'
            tail = ''
            return
        end if

        ! Remove trailing path separators
        n = verify(p(:len_trim(p)), sep_chars, back=.true.)

        if (n == 0) then
            head = sep
            tail = ''
            return
        end if

        temp = p(:n)
        i = scan(temp, sep_chars, back=.true.)

        if (i == 0) then
            if (is_windows .and. n < len_trim(p) .and. n == 2) then
                ! drive root such as `C:\`
                if (temp(2:2) == ':') then
                    head = temp // sep
                    tail = ''
                    return
                end if
            end if
            ! no separator at all, so it is just a file name
            head = ''
            tail = temp
            return
        end if

        head = temp(:i-1)

        ! child of a root directory
        if (scan(head, sep_chars) == 0) then
            head = head // sep
        end if

        tail = temp(i+1:)
    end subroutine split_path_char

    module subroutine split_path_string(p, head, tail)
        type(string_type), intent(in) :: p
        type(string_type), intent(out) :: head, tail

        character(:), allocatable :: head_char, tail_char

        call split_path(char(p), head_char, tail_char)

        call move(head_char, head)
        call move(tail_char, tail)
    end subroutine split_path_string

    module function base_name_char(p) result(base)
        character(:), allocatable :: base, temp
        character(*), intent(in) :: p

        call split_path(p, temp, base)
    end function base_name_char

    module function base_name_string(p) result(base)
        type(string_type) :: base
        type(string_type), intent(in) :: p
        type(string_type) :: temp

        call split_path(p, temp, base)
    end function base_name_string

    module function dir_name_char(p) result(dir)
        character(:), allocatable :: dir, temp
        character(*), intent(in) :: p

        call split_path(p, dir, temp)
    end function dir_name_char

    module function dir_name_string(p) result(dir)
        type(string_type) :: dir
        type(string_type), intent(in) :: p
        type(string_type) :: temp

        call split_path(p, dir, temp)
    end function dir_name_string
end submodule stdlib_system_path
