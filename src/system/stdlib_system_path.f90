submodule(stdlib_system) stdlib_system_path
    use stdlib_ascii, only: reverse
    use stdlib_strings, only: chomp, join, starts_with
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
        character(:), allocatable :: temp
        integer :: i
        character(len=1) :: sep
        sep = path_sep()

        ! Empty string, return (.,'')
        if (trim(p) == '') then
            head = '.'
            tail = ''
            return
        end if

        ! Remove trailing path separators
        temp = trim(chomp(trim(p), sep))

        if (temp == '') then
            head = sep
            tail = ''
            return
        end if

        i = find(reverse(temp), sep)

        ! if no `pathsep`, then it probably was a root dir like `C:\`
        if (i == 0) then
            head = temp // sep
            tail = ''
            return
        end if

        head = temp(:len(temp)-i)

        ! child of a root directory
        if (find(head, sep) == 0) then
            head = head // sep
        end if

        tail = temp(len(temp)-i+2:)
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

    module logical function is_abs_path_char(p)
        character(len=*), intent(in) :: p
        character(len=1) :: sep

        sep = path_sep()

        if (sep == '/') then
            ! should start with '/'
            is_abs_path_char = starts_with(p, sep)
        else
            ! should be either an UNC path like '\\server\host...'
            ! or should be starting with a drive letter like 'C:\Users\...'
            is_abs_path_char = starts_with(p(2:), ':\') .or. starts_with(p, '\\')
        end if
    end function is_abs_path_char

    module logical function is_abs_path_string(p)
        type(string_type), intent(in) :: p

        is_abs_path_string = is_abs_path(char(p))
    end function is_abs_path_string

    module function abs_path_char(p, err) result(abs_p)
        character(len=*), intent(in) :: p
        type(state_type), optional, intent(out) :: err
        character(len=:), allocatable :: abs_p

        type(state_type) :: err0
        character(:), allocatable :: cwd

        ! get the current working directory
        call get_cwd(cwd, err0)

        if (err0%error()) then
            abs_p = ''
            call err0%handle(err)
        end if

        ! join the cwd and path
        abs_p = cwd / p
    end function abs_path_char

    module function abs_path_string(p, err) result(abs_p)
        type(string_type), intent(in) :: p
        type(state_type), optional, intent(out) :: err
        type(string_type) :: abs_p

        character(len=:), allocatable :: res

        res = abs_path(char(p), err)

        call move(res, abs_p)
    end function abs_path_string
end submodule stdlib_system_path
