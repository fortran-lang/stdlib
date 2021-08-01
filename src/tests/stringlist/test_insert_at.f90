! SPDX-Identifier: MIT
module test_insert_at
    use stdlib_error, only: check
    use stdlib_string_type, only: string_type, char, operator(//), operator(==)
    use stdlib_stringlist, only: stringlist_type, stringlist_index_type, fidx, bidx, list_head, &
                            & list_tail, operator(==), operator(/=)
    use stdlib_ascii, only: to_string
    implicit none

contains

    subroutine test_insert_at_1
        type(stringlist_type)           :: first_list
        integer                         :: i, current_length
        character(len=:), allocatable   :: string

        call check( first_list%to_current_idxn( list_tail ) == 0, "test_insert_at_1: list_tail == 0")
        call check( first_list%to_current_idxn( list_head ) == 1, "test_insert_at_1: list_head == 1")

        current_length = 0
        do i = -5, 1
            string = to_string( i )
            call first_list%insert_at( fidx(i), string )
            current_length = current_length + 1

            call check( first_list%get( fidx(1) ) == string, "test_insert_at_1: get check failed &
                                        & for forward index " // string )
            call check( first_list%get( list_head ) == string, "test_insert_at_1: get list_head check &
                                        & failed for " // string )
            call check( first_list%get( bidx(current_length) ) == string, "test_insert_at_1: get &
                                        & list_head check failed for backward index " // string )
            call check( first_list%get( list_tail ) == to_string(-5), "test_insert_at_1: get list_tail &
                                        & check failed for " // string )
            call check( first_list%to_current_idxn( list_head ) == 1, "")
            call check( first_list%to_current_idxn( list_tail ) == current_length, "" )
            call check( first_list%len() == current_length, "test_insert_at_1: length check &
                                        & failed for " // to_string( current_length ) )

        end do

    end subroutine test_insert_at_1

end module test_insert_at


program tester
    use test_insert_at
    implicit none

    call test_insert_at_1

end program tester
