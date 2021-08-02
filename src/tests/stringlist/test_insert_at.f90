! SPDX-Identifier: MIT
module test_insert_at
    use stdlib_error, only: check
    use stdlib_string_type, only: string_type, operator(//), operator(==)
    use stdlib_stringlist, only: stringlist_type, stringlist_index_type, fidx, bidx, list_head, &
                            & list_tail, operator(//), operator(==), operator(/=)
    use stdlib_ascii, only: to_string
    implicit none

contains

    subroutine test_insert_at_1
        type(stringlist_type)           :: work_list
        integer                         :: i, current_length
        character(len=:), allocatable   :: string
        integer, parameter              :: first = -5
        integer, parameter              :: last = 1

        call check( work_list%to_current_idxn( list_tail ) == 0, "test_insert_at_1: list_tail == 0")
        call check( work_list%to_current_idxn( list_head ) == 1, "test_insert_at_1: list_head == 1")

        write (*,*) "test_insert_at_1: Starting test case 1!"
        current_length = 0
        do i = first, last
            string = to_string( i )
            call work_list%insert_at( fidx(i), string )
            current_length = current_length + 1

            call check( work_list%get( fidx(1) ) == string, "test_insert_at_1:&
                                    & get fidx(1) " // string )
            call check( work_list%get( list_head ) == string, "test_insert_at_1:&
                                    & get list_head " // string )
            call check( work_list%get( bidx(current_length) ) == string, "test_insert_at_1: get&
                                    & bidx(current_length) " // string )
            call check( work_list%get( list_tail ) == to_string(first), "test_insert_at_1: get&
                                    & list_tail " // string )

            call check( work_list%to_current_idxn( list_head ) == 1, "test_insert_at_1:&
                                    & to_current_idxn( list_head ) " // to_string( current_length ) )
            call check( work_list%to_current_idxn( list_tail ) == current_length, "test_insert_at_1:&
                                    & to_current_idxn( list_tail ) "  // to_string( current_length ) )
            call check( work_list%len() == current_length, "test_insert_at_1: length check "&
                                    & // to_string( current_length ) )

        end do

        ! compare work_list with [1, 0, -1, -2, -3, -4, -5]
        call compare_list( work_list, last, first - 1, 1)

        call work_list%destroy()
        current_length = 0

        write (*,*) "test_insert_at_1: Starting test case 2!"
        do i = first, last
            string = to_string( i )
            call work_list%insert_at( bidx(i), string )
            current_length = current_length + 1

            call check( work_list%get( bidx(1) ) == string, "test_insert_at_1:&
                                    & get bidx(1) " // string )
            call check( work_list%get( list_tail ) == string, "test_insert_at_1:&
                                    & get list_tail " // string )
            call check( work_list%get( fidx(current_length) ) == string, "test_insert_at_1: get&
                                    & fidx(current_length) " // string )
            call check( work_list%get( list_head ) == to_string(first), "test_insert_at_1: get&
                                    & list_head " // string )

            call check( work_list%to_current_idxn( list_head ) == 1, "test_insert_at_1:&
                                    & to_current_idxn( list_head ) " // to_string( current_length ) )
            call check( work_list%to_current_idxn( list_tail ) == current_length, "test_insert_at_1:&
                                    & to_current_idxn( list_tail ) "  // to_string( current_length ) )
            call check( work_list%len() == current_length, "test_insert_at_1: length check "&
                                    & // to_string( current_length ) )

        end do

        ! compare work_list with [-5, -4, -3, -2, -1, 0, 1]
        call compare_list( work_list, first, last + 1, 2)

    end subroutine test_insert_at_1

    subroutine test_insert_at_2
        type(stringlist_type)           :: work_list
        integer                         :: i, current_length
        character(len=:), allocatable   :: string
        integer, parameter              :: first = 2
        integer, parameter              :: last = 20

        write (*,*) "test_insert_at_2: Starting test case 1!"

        current_length = 0
        do i = first, last, 2
            string = to_string( i )
            call work_list%insert_at( fidx(i), string )
            current_length = current_length + 1

            call check( work_list%get( fidx(current_length) ) == string, "test_insert_at_2:&
                                    & get fidx(current_length) " // string )
            call check( work_list%get( fidx(1) ) == to_string(first), "test_insert_at_2:&
                                    & get fidx(1) " // string )
            call check( work_list%get( list_head ) == to_string(first), "test_insert_at_2:&
                                    & get list_head " // string )
            call check( work_list%get( bidx(1) ) == string, "test_insert_at_2:&
                                    & get bidx(1) " // string )
            call check( work_list%get( bidx(current_length) ) == to_string(first), "test_insert_at_2: get&
                                    & bidx(current_length) " // string )
            call check( work_list%get( list_tail ) == string, "test_insert_at_2: get&
                                    & list_tail " // string )

            call check( work_list%to_current_idxn( list_head ) == 1, "test_insert_at_2:&
                                    & to_current_idxn( list_head ) " // to_string( current_length ) )
            call check( work_list%to_current_idxn( list_tail ) == current_length, "test_insert_at_2:&
                                    & to_current_idxn( list_tail ) "  // to_string( current_length ) )
            call check( work_list%len() == current_length, "test_insert_at_2: length check "&
                                    & // to_string( current_length ) )

        end do

        write (*,*) "test_insert_at_2: Starting test case 2!"

        do i = first - 1, last - 1, 2
            string = to_string( i )
            call work_list%insert_at( fidx(i), string )
            current_length = current_length + 1

            call check( work_list%get( fidx(i) ) == string, "test_insert_at_2:&
                                    & get fidx(current_length) " // string )
            call check( work_list%get( fidx(1) ) == to_string(first - 1), "test_insert_at_2:&
                                    & get fidx(1) " // string )
            call check( work_list%get( list_head ) == to_string(first - 1), "test_insert_at_2:&
                                    & get list_head " // string )
            call check( work_list%get( bidx(1) ) == to_string(last), "test_insert_at_2:&
                                    & get bidx(1) " // string )
            call check( work_list%get( bidx(current_length) ) == to_string(first - 1), "test_insert_at_2: get&
                                    & bidx(current_length) " // string )
            call check( work_list%get( list_tail ) == to_string(last), "test_insert_at_2: get&
                                    & list_tail " // string )

            call check( work_list%to_current_idxn( list_head ) == 1, "test_insert_at_2:&
                                    & to_current_idxn( list_head ) " // to_string( current_length ) )
            call check( work_list%to_current_idxn( list_tail ) == current_length, "test_insert_at_2:&
                                    & to_current_idxn( list_tail ) "  // to_string( current_length ) )
            call check( work_list%len() == current_length, "test_insert_at_2: length check "&
                                    & // to_string( current_length ) )

        end do

        ! compare work_list with [1, 2, ..., ..., 19, 20]
        call compare_list( work_list, first - 1, last + 1, 3 )

    end subroutine test_insert_at_2

    subroutine test_insert_at_3
        type(stringlist_type)           :: work_list
        integer                         :: i, current_length
        character(len=:), allocatable   :: string
        integer, parameter              :: first = 2
        integer, parameter              :: last = 20

        write (*,*) "test_insert_at_3: Starting test case 1!"

        current_length = 0
        do i = first, last, 2
            string = to_string( i )
            call work_list%insert_at( bidx(i), string )
            current_length = current_length + 1

            call check( work_list%get( bidx(current_length) ) == string, "test_insert_at_3:&
                                    & get bidx(current_length) " // string )
            call check( work_list%get( bidx(1) ) == to_string(first), "test_insert_at_3:&
                                    & get bidx(1) " // string )
            call check( work_list%get( list_tail ) == to_string(first), "test_insert_at_3:&
                                    & get list_tail " // string )
            call check( work_list%get( fidx(1) ) == string, "test_insert_at_3:&
                                    & get fidx(1) " // string )
            call check( work_list%get( fidx(current_length) ) == to_string(first), "test_insert_at_3: get&
                                    & fidx(current_length) " // string )
            call check( work_list%get( list_head ) == string, "test_insert_at_3: get&
                                    & list_head " // string )

            call check( work_list%to_current_idxn( list_head ) == 1, "test_insert_at_3:&
                                    & to_current_idxn( list_head ) " // to_string( current_length ) )
            call check( work_list%to_current_idxn( list_tail ) == current_length, "test_insert_at_3:&
                                    & to_current_idxn( list_tail ) "  // to_string( current_length ) )
            call check( work_list%len() == current_length, "test_insert_at_3: length check "&
                                    & // to_string( current_length ) )

        end do

        write (*,*) "test_insert_at_3: Starting test case 2!"

        do i = first - 1, last - 1, 2
            string = to_string( i )
            call work_list%insert_at( bidx(i), string )
            current_length = current_length + 1

            call check( work_list%get( bidx(i) ) == string, "test_insert_at_3:&
                                    & get bidx(current_length) " // string )
            call check( work_list%get( bidx(1) ) == to_string(first - 1), "test_insert_at_3:&
                                    & get bidx(1) " // string )
            call check( work_list%get( list_tail ) == to_string(first - 1), "test_insert_at_3:&
                                    & get list_tail " // string )
            call check( work_list%get( fidx(1) ) == to_string(last), "test_insert_at_3:&
                                    & get fidx(1) " // string )
            call check( work_list%get( fidx(current_length) ) == to_string(first - 1), "test_insert_at_3: get&
                                    & fidx(current_length) " // string )
            call check( work_list%get( list_head ) == to_string(last), "test_insert_at_3: get&
                                    & list_head " // string )

            call check( work_list%to_current_idxn( list_head ) == 1, "test_insert_at_3:&
                                    & to_current_idxn( list_head ) " // to_string( current_length ) )
            call check( work_list%to_current_idxn( list_tail ) == current_length, "test_insert_at_3:&
                                    & to_current_idxn( list_tail ) "  // to_string( current_length ) )
            call check( work_list%len() == current_length, "test_insert_at_3: length check "&
                                    & // to_string( current_length ) )

        end do

        ! compare work_list with [20, 19, ..., ..., 2, 1]
        call compare_list( work_list, last, first - 2, 4 )

    end subroutine test_insert_at_3

    ! compares input stringlist 'list' with an array of consecutive integers
    ! array is 'first' inclusive and 'last' exclusive
    subroutine compare_list(list, first, last, call_number)
        type(stringlist_type), intent(in)   :: list
        integer, intent(in)                 :: first, last, call_number
        integer                             :: i, j

        call check( abs( last - first ) == list%len(), "compare_list: length mis-match&
                                        & call_number " // to_string( call_number ) )

        j = merge(-1, 1, last < first)
        do i = 1, list%len()
            call check( list%get( fidx(i) ) == to_string( first + ( ( i - 1 ) * j ) ), &
                                    & "compare_list: call_number " // to_string( call_number ) &
                                    & // " fidx( " // to_string( i ) // " )")
            call check( list%get( bidx(i) ) == to_string( last - ( i * j ) ), &
                                    & "compare_list: call_number " // to_string( call_number ) &
                                    & // " bidx( " // to_string( i ) // " )")
        end do

    end subroutine compare_list

end module test_insert_at


program tester
    use test_insert_at
    implicit none

    call test_insert_at_1
    call test_insert_at_2
    call test_insert_at_3

end program tester
