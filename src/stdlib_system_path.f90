submodule(stdlib_system) stdlib_system_path
    use stdlib_ascii, only: reverse
    use stdlib_strings, only: chomp, find, join
contains
    module function join2(p1, p2) result(path)
        character(:), allocatable :: path
        character(*), intent(in) :: p1, p2

        path = trim(p1) // path_sep() // trim(p2)
    end function join2

    module function joinarr(p) result(path)
        character(:), allocatable :: path
        character(*), intent(in) :: p(:)

        path = join(p, path_sep())
    end function joinarr

    module function join_op(p1, p2) result(path)
        character(:), allocatable :: path
        character(*), intent(in) :: p1, p2

        path = join_path(p1, p2)
    end function join_op

    module subroutine split_path(p, head, tail)
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
    end subroutine split_path

    module function base_name(p) result(base)
        character(:), allocatable :: base, temp
        character(*), intent(in) :: p

        call split_path(p, temp, base)
    end function base_name

    module function dir_name(p) result(dir)
        character(:), allocatable :: dir, temp
        character(*), intent(in) :: p

        call split_path(p, dir, temp)
    end function dir_name
end submodule stdlib_system_path
