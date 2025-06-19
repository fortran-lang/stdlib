submodule(stdlib_system) stdlib_system_path
    use stdlib_ascii, only: reverse
    use stdlib_strings, only: chomp, find, join
contains
    module pure function join2(p1, p2) result(path)
        character(:), allocatable :: path
        character(*), intent(in) :: p1, p2

        path = trim(p1) // pathsep // trim(p2)
    end function join2

    module pure function joinarr(p) result(path)
        character(:), allocatable :: path
        character(*), intent(in) :: p(:)

        path = join(p, pathsep)
    end function joinarr

    module pure function join_op(p1, p2) result(path)
        character(:), allocatable :: path
        character(*), intent(in) :: p1, p2

        path = join_path(p1, p2)
    end function join_op

    module subroutine split_path(p, head, tail)
        character(*), intent(in) :: p
        character(:), allocatable, intent(out) :: head, tail
        character(:), allocatable :: temp
        integer :: i

        ! Empty string, return (.,'')
        if (trim(p) == '') then
            head = '.'
            tail = ''
            return
        end if

        ! Remove trailing path separators
        temp = trim(chomp(trim(p), pathsep))

        if (temp == '') then
            head = pathsep
            tail = ''
            return
        end if

        i = find(reverse(temp), pathsep)

        ! if no `pathsep`, then it probably was a root dir like `C:\`
        if (i == 0) then
            head = temp // pathsep
            tail = ''
            return
        end if

        head = temp(:len(temp)-i)

        ! child of a root directory
        if (find(head, pathsep) == 0) then
            head = head // pathsep
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
